use std::fmt::Display;

use itertools::Itertools;
use log::{error, info};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Range};
use tree_sitter::{Node, Query, QueryCursor, QueryMatch, Tree};

use crate::ast::lsp_position;

trait Lint {
    type Diagnostic: Display;
    fn query(&self) -> &str;
    fn lint(&self, problem: &str, query: &Query, mtch: QueryMatch) -> Option<Self::Diagnostic>;
}

struct SliceIndexTrigger;
struct SliceIndexTriggerLint {
    index: String,
}

impl Display for SliceIndexTriggerLint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Bad trigger {}. use &{} instead", self.index, self.index)
    }
}

impl Lint for SliceIndexTrigger {
    type Diagnostic = SliceIndexTriggerLint;
    fn query(&self) -> &str {
        include_str!("./queries/slice_index_trigger.scm")
    }

    fn lint(&self, problem: &str, _: &Query, _: QueryMatch) -> Option<Self::Diagnostic> {
        Some(SliceIndexTriggerLint {
            index: problem.to_owned(),
        })
    }
}

struct ContractOrder;
struct ContractOrderLint;

impl Display for ContractOrderLint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "place preconditions before postconditions")
    }
}

impl Lint for ContractOrder {
    type Diagnostic = ContractOrderLint;

    fn query(&self) -> &str {
        include_str!("./queries/contract_order.scm")
    }

    fn lint(&self, _: &str, query: &Query, mtch: QueryMatch) -> Option<Self::Diagnostic> {
        #[derive(Copy, Clone, Debug)]
        #[repr(u32)]
        enum ContractItem {
            Requires = 0,
            Preserves = 1,
            Ensures = 2,
        }
        let fdecl = query
            .capture_index_for_name("fdecl")
            .expect("query did not define @fdecl");
        let fdecl = mtch.nodes_for_capture_index(fdecl).next()?;
        let mut cursor = fdecl.walk();

        let mut now_item = ContractItem::Requires;
        for node in fdecl.children(&mut cursor) {
            let this_item = match node.kind() {
                "pre_condition" => ContractItem::Requires,
                "preserves_condition" => ContractItem::Preserves,
                "post_condition" => ContractItem::Ensures,
                _ => {
                    continue;
                }
            };

            if this_item as u32 >= now_item as u32 {
                now_item = this_item;
            } else {
                return Some(ContractOrderLint);
            }
        }

        None
    }
}

fn diagnose_lint<L>(lint: L, tree: Node, src: &[u8]) -> Vec<Diagnostic>
where
    L: Lint,
{
    let query = Query::new(&tree_sitter_gobra::language(), lint.query()).expect("valid query");
    let problem = query
        .capture_index_for_name("problem")
        .expect("query did not define @problem");

    QueryCursor::new()
        .matches(&query, tree, src)
        .filter_map(|mtch| {
            let problem = mtch.nodes_for_capture_index(problem).next()?;
            let range = Range::new(
                lsp_position(problem.start_position()),
                lsp_position(problem.end_position()),
            );
            let hint = problem.utf8_text(src).expect("invalid utf in source");
            let lint = lint.lint(hint, &query, mtch)?;
            Some(Diagnostic {
                range,
                message: lint.to_string(),
                severity: Some(DiagnosticSeverity::WARNING),
                ..Default::default()
            })
        })
        .collect_vec()
}

pub fn diagnose(tree: &Tree, src: &str) -> Vec<Diagnostic> {
    let src = src.as_bytes();

    let mut problems = vec![];

    problems.extend_from_slice(&diagnose_lint(SliceIndexTrigger, tree.root_node(), src));
    problems.extend_from_slice(&diagnose_lint(ContractOrder, tree.root_node(), src));

    problems
}
