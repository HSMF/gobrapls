use tower_lsp::lsp_types::Position;
use tree_sitter::{Node, Point, Range, Tree, TreeCursor};

pub fn lsp_position(x: Point) -> Position {
    Position::new(x.row as u32, x.column as u32)
}

/// retrieves the smallest node that is positioned around the cursor in the tree
pub fn cursor_node(t: &Tree, cursor: Position) -> Node {
    let node = t.root_node();

    smallest_node_around_cursor_node(node, cursor, &mut t.walk())
}

fn is_inside(pos: Position, range: Range) -> bool {
    if !(range.start_point.row..=range.end_point.row).contains(&(pos.line as usize)) {
        return false;
    }
    if range.start_point.row == pos.line as usize
        && range.start_point.column > pos.character as usize
    {
        return false;
    }
    if range.end_point.row == pos.line as usize && range.end_point.column < pos.character as usize {
        return false;
    }
    true
}

#[tracing::instrument(skip(cur))]
fn smallest_node_around_cursor_node<'a>(
    n: Node<'a>,
    cursor: Position,
    cur: &mut TreeCursor<'a>,
) -> Node<'a> {
    let c = n.children(cur).find(|x| is_inside(cursor, x.range()));

    if let Some(child) = c {
        smallest_node_around_cursor_node(child, cursor, cur)
    } else {
        n
    }
}
