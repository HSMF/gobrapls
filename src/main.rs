use clap::Parser;
use std::{collections::HashMap, io::Read, iter::Peekable, process::Stdio, sync::Arc};
use tempfile::NamedTempFile;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    sync::Mutex,
    task::JoinHandle,
};

const FILE_NOT_LOADED: i64 = 18;

use log::{info, LevelFilter};
use log4rs::{
    append::file::FileAppender,
    config::{Appender, Root},
    encode::pattern::PatternEncoder,
    Config,
};
use tower_lsp::{
    jsonrpc::{self, Result as JResult},
    lsp_types::{
        Diagnostic, DiagnosticOptions, DiagnosticServerCapabilities, DidChangeTextDocumentParams,
        DocumentDiagnosticParams, DocumentDiagnosticReportResult, InitializeParams,
        InitializeResult, InitializedParams, MessageType, Position, Range, ServerCapabilities,
        TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions,
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
    fn type1_from_string(line: &str) -> Option<Self> {
        let (_, line) = line.split_once("Error at: <")?;
        info!("{}", line);
        None
    }

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
    diagnostics: Vec<DiagnosticItem>,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    files: Arc<Mutex<HashMap<String, Arc<Mutex<FileInfo>>>>>,
    options: Options,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
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
                ..ServerCapabilities::default()
            },
            ..InitializeResult::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("init'd");
        self.client
            .log_message(MessageType::WARNING, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> JResult<()> {
        Ok(())
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        info!("changed: {params:#?}");
        let file = params.text_document.uri.path().to_owned();
        let mut files = self.files.lock().await;
        let entry = files.entry(file).or_default();
        let mut entry_l = entry.lock().await;
        entry_l.contents = params
            .content_changes
            .first()
            .map(|x| x.text.clone())
            .unwrap_or_default();

        if entry_l.analysis_handle.is_none() {
            let contents = entry_l.contents.clone();
            let options = self.options;
            let entry = Arc::clone(entry);
            let handle = tokio::spawn(async move {
                let contents = contents;
                let mut entry_l = entry.lock().await;
                entry_l.diagnostics = compute_diagnostics(options, &contents).await;
                entry_l.analysis_handle = None;
            });
            entry_l.analysis_handle = Some(handle);
        }

        self.client
            .log_message(MessageType::LOG, "document changed!")
            .await;
    }

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
        let items = file
            .lock()
            .await
            .diagnostics
            .iter()
            .map(|diag| Diagnostic {
                source: Some(env!("CARGO_PKG_NAME").to_string()),
                range: Range::new(
                    Position::new(diag.line, diag.col),
                    Position::new(diag.line, diag.col + 10),
                ),
                message: diag.message.clone(),
                ..Default::default()
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

    let logfile = FileAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{l} - {m}\n")))
        .build("/tmp/gobrapls.log")?;

    let config = Config::builder()
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        .build(Root::builder().appender("logfile").build(LevelFilter::Info))?;

    log4rs::init_config(config)?;

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
