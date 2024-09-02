use ast::cursor_node;
use clap::Parser;
use std::{collections::HashMap, io::Read, iter::Peekable, process::Stdio, sync::Arc};
use tempfile::NamedTempFile;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    sync::Mutex,
    task::JoinHandle,
};
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use log::info;

pub mod ast;
mod timing {
    use log::info;
    use std::time::Instant;
    use tracing::span::{Attributes, Id};
    use tracing::Subscriber;
    use tracing_subscriber::layer::{Context, Layer};
    use tracing_subscriber::registry::LookupSpan;

    struct Timing {
        started_at: Instant,
    }

    pub struct CustomLayer;

    impl<S> Layer<S> for CustomLayer
    where
        S: Subscriber,
        S: for<'lookup> LookupSpan<'lookup>,
    {
        fn on_new_span(&self, _attrs: &Attributes<'_>, id: &Id, ctx: Context<'_, S>) {
            let span = ctx.span(id).unwrap();

            span.extensions_mut().insert(Timing {
                started_at: Instant::now(),
            });
        }

        fn on_close(&self, id: Id, ctx: Context<'_, S>) {
            let span = ctx.span(&id).unwrap();

            let started_at = span.extensions().get::<Timing>().unwrap().started_at;

            info!(
                "span {} took {}µs",
                span.metadata().name(),
                (Instant::now() - started_at).as_micros(),
            );
        }
    }
}
use tower_lsp::{
    jsonrpc::Result as JResult,
    lsp_types::{
        Diagnostic, DiagnosticOptions, DiagnosticServerCapabilities, DidChangeTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams,
        DocumentDiagnosticReportResult, Hover, HoverParams, HoverProviderCapability,
        InitializeParams, InitializeResult, InitializedParams, MessageType, Position,
        PositionEncodingKind, Range, ServerCapabilities, TextDocumentSyncCapability,
        TextDocumentSyncKind, WorkDoneProgressOptions,
    },
    Client, LanguageServer, LspService, Server,
};

#[derive(Clone, Copy, Debug)]
struct Options {
    java: &'static str,
    gobra: &'static str,
}

#[derive(Clone, Debug)]
struct DiagnosticItem {
    line: u32,
    col: u32,
    message: String,
}

impl DiagnosticItem {
    fn from_line<'a>(s: &str, lines: &mut Peekable<impl Iterator<Item = &'a str>>) -> Option<Self> {
        let (_, s) = s.split_once("Error at: <")?;
        let (_, s) = s.split_once(':')?;
        let (line, s) = s.split_once(':')?;
        let (col, err) = s.split_once('>')?;
        let more_info = lines.peek()?;
        let line = line.parse().ok()?;
        let col = col.parse().ok()?;

        Some(DiagnosticItem {
            line,
            col,
            message: format!("{err} {more_info}"),
        })
    }
}

async fn compute_diagnostics(options: Options, contents: &str) -> Vec<DiagnosticItem> {
    let mut f = NamedTempFile::new().expect("could create temp file");
    let path = f.path().to_owned();
    {
        let mut f = tokio::fs::File::from_std(f.reopen().expect("could reopen temp file"));
        f.write_all(contents.as_bytes())
            .await
            .expect("could write contents");
        let mut s = String::new();
        f.read_to_string(&mut s).await.unwrap();
        info!("re {s}");
    }

    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    info!("{s}");

    info!("started");

    let mut cmd = tokio::process::Command::new(options.java);
    cmd.args(["-Xss1g", "-Xmx4g", "-jar", options.gobra])
        .args(["--backend", "SILICON"])
        .args(["--chop", "1"])
        .args(["--cacheFile", "/tmp/gobracache"])
        // .arg("--onlyFilesWithHeader")
        .arg("--assumeInjectivityOnInhale")
        .arg("--checkConsistency")
        .arg("--mceMode=od")
        .arg("--requireTriggers")
        // .arg("--unsafeWildcardOptimization")
        .args(["--moreJoins", "off"])
        .args(["-g", "/tmp/"])
        .arg("-i")
        .arg(path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    info!("{cmd:#?}");

    let cmd = cmd.spawn().unwrap();
    let o = cmd.wait_with_output().await.unwrap();
    let stdout = std::str::from_utf8(&o.stdout).unwrap();

    let lines = &mut (stdout.lines()).peekable();
    let mut diagnostics = vec![];
    while let Some(line) = lines.next() {
        let Some(diag) = DiagnosticItem::from_line(line, lines) else {
            continue;
        };

        diagnostics.push(diag);
    }
    info!("{:#?}", diagnostics);
    drop(f);
    diagnostics
}

#[derive(Debug, Default)]
struct FileInfo {
    contents: String,
    analysis_handle: Option<JoinHandle<()>>,
    queued: bool,
    diagnostics: Vec<DiagnosticItem>,
    tree: Option<tree_sitter::Tree>,
}

impl FileInfo {
    async fn compute_diagnostics(&mut self, options: Options, entry: &Arc<Mutex<Self>>) {
        if self.analysis_handle.is_none() {
            let contents = self.contents.clone();
            let entry = Arc::clone(entry);
            // TODO: centralized worker task that automatically dedupes requests
            // send(timestamp, contents, file-id)
            let handle = tokio::spawn(async move {
                let contents = contents;
                let diagnostics = compute_diagnostics(options, &contents).await;
                let mut entry_l = entry.lock().await;
                entry_l.diagnostics = diagnostics;
                entry_l.analysis_handle = None;
            });
            self.analysis_handle = Some(handle);
        } else {
            self.queued = true
        }
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,
    files: Arc<Mutex<HashMap<String, Arc<Mutex<FileInfo>>>>>,
    options: Options,
}

impl Backend {
    async fn update_file(&self, path: &str, contents: &str) {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_gobra::language())
            .expect("Error loading gobra grammar");

        let mut files = self.files.lock().await;
        let entry = files.entry(path.to_owned()).or_default();
        let mut entry_l = entry.lock().await;
        entry_l.contents = contents.to_owned();
        let parsed_tree = parser.parse(&entry_l.contents, None);
        entry_l.tree = parsed_tree;

        self.client
            .log_message(MessageType::LOG, "document updated!")
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    #[tracing::instrument(skip(self))]
    async fn initialize(&self, _: InitializeParams) -> JResult<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: None,
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                position_encoding: Some(PositionEncodingKind::UTF16),
                ..ServerCapabilities::default()
            },
            ..InitializeResult::default()
        })
    }

    #[tracing::instrument(skip(self))]
    async fn initialized(&self, _: InitializedParams) {
        info!("init'd");
        self.client
            .log_message(MessageType::WARNING, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> JResult<()> {
        Ok(())
    }

    #[tracing::instrument(skip(self))]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        info!("changed: {params:#?}");

        let file = params.text_document.uri.path().to_owned();
        let contents = params
            .content_changes
            .first()
            .map(|x| x.text.as_str())
            .unwrap_or_default();

        self.update_file(&file, contents).await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let file = params.text_document.uri.path().to_owned();
        let contents = &params.text_document.text;
        self.update_file(&file, contents).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let file = params.text_document.uri.path().to_owned();
        let files = self.files.lock().await;

        let Some(entry) = files.get(&file) else {
            return;
        };

        entry
            .lock()
            .await
            .compute_diagnostics(self.options, entry)
            .await;
    }

    #[tracing::instrument(skip(self))]
    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> JResult<DocumentDiagnosticReportResult> {
        let file = self.files.lock().await;
        let Some(file) = file.get(params.text_document.uri.path()) else {
            return Ok(DocumentDiagnosticReportResult::Report(
                tower_lsp::lsp_types::DocumentDiagnosticReport::Full(
                    tower_lsp::lsp_types::RelatedFullDocumentDiagnosticReport {
                        related_documents: None,
                        full_document_diagnostic_report:
                            tower_lsp::lsp_types::FullDocumentDiagnosticReport {
                                result_id: None,
                                items: vec![],
                            },
                    },
                ),
            ));
        };
        let entry = file.lock().await;
        let items = entry
            .diagnostics
            .iter()
            .map(|diag| {
                let pos = Position::new(diag.line.saturating_sub(1), diag.col.saturating_sub(1));
                let range = match &entry.tree {
                    None => Range::new(
                        Position::new(diag.line.saturating_sub(1), diag.col.saturating_sub(1)),
                        Position::new(diag.line.saturating_sub(1), diag.col),
                    ),
                    Some(t) => {
                        let node = cursor_node(t, pos);
                        Range::new(
                            Position::new(
                                node.range().start_point.row as u32,
                                node.range().start_point.column as u32,
                            ),
                            Position::new(
                                node.range().end_point.row as u32,
                                node.range().end_point.column as u32,
                            ),
                        )
                    }
                };
                info!("range is {range:?}");
                Diagnostic {
                    source: Some(env!("CARGO_PKG_NAME").to_string()),
                    range,
                    message: diag.message.clone(),
                    ..Default::default()
                }
            })
            .collect();
        log::info!("{:#?}", params);
        Ok(DocumentDiagnosticReportResult::Report(
            tower_lsp::lsp_types::DocumentDiagnosticReport::Full(
                tower_lsp::lsp_types::RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report:
                        tower_lsp::lsp_types::FullDocumentDiagnosticReport {
                            result_id: None,
                            items,
                        },
                },
            ),
        ))
    }

    #[tracing::instrument(skip(self, params))]
    async fn hover(&self, params: HoverParams) -> JResult<Option<Hover>> {
        let file = params
            .text_document_position_params
            .text_document
            .uri
            .path()
            .to_owned();
        let cursor = params.text_document_position_params.position;
        let now = std::time::Instant::now();
        let files = self.files.lock().await;
        info!("{file:#?}");
        let Some(entry) = files.get(&file) else {
            return Ok(None);
        };
        let entry_l = entry.lock().await;
        info!("{entry_l:#?} {:?}", now.elapsed());

        let Some(tree) = &entry_l.tree else {
            return Ok(None);
        };
        let node = ast::cursor_node(tree, cursor);

        Ok(Some(Hover {
            contents: tower_lsp::lsp_types::HoverContents::Markup(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::PlainText,
                    value: format!(
                        r#"{cursor:#?}
{node:#?}
                    "#
                    ),
                },
            ),
            range: None,
        }))
    }
}

#[derive(clap::Parser)]
struct App {
    #[clap(short, long)]
    java: Option<String>,
    #[clap(short, long)]
    gobra: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app = App::parse();

    // let logfile = FileAppender::builder()
    //     .encoder(Box::new(PatternEncoder::new("{l} - {m}\n")))
    //     .build("/tmp/gobrapls.log")?;
    //
    // let config = Config::builder()
    //     .appender(Appender::builder().build("logfile", Box::new(logfile)))
    //     .build(Root::builder().appender("logfile").build(LevelFilter::Info))?;

    let f = std::fs::OpenOptions::new()
        .append(true)
        .open("/tmp/gobrapls.log")?;

    tracing_subscriber::registry::Registry::default()
        .with(LevelFilter::DEBUG)
        .with(timing::CustomLayer)
        .with(tracing_subscriber::fmt::layer().with_writer(f))
        .init();

    // log4rs::init_config(config)?;

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let options = Options {
        java: app.java.map(|x| &*x.leak()).unwrap_or("java"),
        gobra: app.gobra.leak(),
    };

    let (service, socket) = LspService::new(|client| Backend {
        client,
        files: Arc::new(Mutex::new(HashMap::new())),
        options,
    });
    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}
