/// returns index of the @s and end of comment
fn find_block_comment(b: &[u8]) -> Option<(usize, usize, usize)> {
    let &[b'/', b'*', ref b @ ..] = b else {
        return None;
    };

    let (end, _) = b
        .windows(2)
        .enumerate()
        .find(|(_, a)| a[0] == b'*' && a[1] == b'/')?;

    let (first_at, _) = b[..end].iter().enumerate().find(|(_, ch)| **ch == b'@')?;
    let (second_at, _) = b[..end]
        .iter()
        .enumerate()
        .rev()
        .find(|(_, ch)| **ch == b'@')?;

    if first_at == second_at {
        return None;
    }

    Some((first_at + 2, second_at + 2, end + 3))
}

/// returns index of the @ and end of comment
fn find_line_comment(b: &[u8]) -> (Option<usize>, usize) {
    let &[b'/', b'/', ref b @ ..] = b else {
        return (None, 0);
    };

    let end = b
        .iter()
        .enumerate()
        .find(|(_, x)| **x == b'\n')
        .map(|(i, _)| i)
        .unwrap_or(b.len());
    let Some((at, _)) = &b[..end]
        .iter()
        .enumerate()
        .take_while(|(_, x)| x.is_ascii_whitespace() || **x == b'@')
        .find(|(_, ch)| **ch == b'@')
    else {
        return (None, end + 2);
    };

    (Some(at + 2), end + 2)
}

pub fn preprocess_in_place(before: String) -> String {
    let mut bytes = before.into_bytes();

    let i = bytes
        .iter()
        .enumerate()
        .find(|&(_, &u)| u == b'/')
        .map(|(i, _)| i)
        .unwrap_or(bytes.len());
    let mut view = &mut bytes[i..];

    while !view.is_empty() {
        let Some(i) = view
            .iter()
            .enumerate()
            .find(|&(_, &u)| u == b'/')
            .map(|(i, _)| i)
        else {
            break;
        };
        view = &mut view[i..];

        let (at, end) = find_line_comment(view);
        if let Some(at) = at {
            view[0] = b' ';
            view[1] = b' ';
            view[at] = b' ';
        }
        view = &mut view[end..];
        if end != 0 {
            continue;
        }

        if let Some((at1, at2, end)) = find_block_comment(view) {
            view[0] = b' ';
            view[1] = b' ';
            view[at1] = b' ';
            view[at2] = b' ';
            view[end - 1] = b' ';
            view[end] = b' ';
            view = &mut view[end..];
        } else {
            view = &mut view[1..];
        }
    }

    String::from_utf8(bytes).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! preprocess_go_in_place {
        ($name:ident, $a:expr, $b:expr $(,)?) => {
            #[test]
            fn $name() {
                let a = preprocess_in_place($a.to_owned());
                assert_eq!(a, $b);
            }
        };
    }

    preprocess_go_in_place!(in_place_1, "// hello", "// hello");
    preprocess_go_in_place!(in_place_2, "// @ hello", "     hello");
    preprocess_go_in_place!(in_place_3, "/*@hello@*/", "   hello   ");
    preprocess_go_in_place!(
        in_place_4,
        r#"
// asd is commented
// @ requires acc(Bytes(x))
    "#,
        r#"
// asd is commented
     requires acc(Bytes(x))
    "#
    );

    preprocess_go_in_place! {in_place_5, r#"
/* @
pred Bytes(foo int) {
 	foo == 1
 }
@ */ "#,
r#"
    
pred Bytes(foo int) {
 	foo == 1
 }
     "#
    }

    preprocess_go_in_place! {in_place_6, r#"
package main

// +gobra

// does bytes stuff
/* @
pred Bytes(foo int) {
 	foo == 1
 }
@ */

"#,
r#"
package main

// +gobra

// does bytes stuff
    
pred Bytes(foo int) {
 	foo == 1
 }
    

"#
    }

    preprocess_go_in_place!(
        in_place_7,
        r"package main

// +gobra

// does bytes stuff
/* @
pred Bytes(foo int) {
 	foo == 1
 }
@ */",
        r"package main

// +gobra

// does bytes stuff
    
pred Bytes(foo int) {
 	foo == 1
 }
    ",
    );

    preprocess_go_in_place!(in_place_8, r#"/* @ @ *//*@"#, r#"         /*@"#,);
    preprocess_go_in_place!(in_place_9, r#"// a @"#, r#"// a @"#,);
    preprocess_go_in_place!(in_place_10, "// a /* @ @ */", "// a /* @ @ */");
}
