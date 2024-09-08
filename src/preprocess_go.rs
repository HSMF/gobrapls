fn consume_line_comment(s: &str) -> (&str, &str) {
    let orig = s;
    let Some(s) = s.strip_prefix("//") else {
        return ("", s);
    };
    let eol = s.find('\n').unwrap_or(s.len());
    let Some(at) = s[..eol].find('@') else {
        return (&orig[..eol + 2], &s[eol..]);
    };
    (&s[at + 1..eol], &s[eol..])
}

fn consume_block_comment(s: &str) -> (&str, &str) {
    let orig = s;
    let Some(s) = s.strip_prefix("/*") else {
        return ("", s);
    };

    let Some(eoc) = s.find("*/") else {
        return (orig, "");
    };
    let eoc = eoc + 2;
    let Some(first_at) = s[..eoc].find('@') else {
        return (&orig[..eoc + 2], &s[eoc..]);
    };
    let Some(second_at) = s[first_at + 1..eoc].find('@') else {
        return (&orig[..eoc + 2], &s[eoc..]);
    };

    (&s[first_at + 1..first_at + 1 + second_at], &s[eoc..])
}

pub fn preprocess_go(mut before: &str) -> String {
    let mut out = String::with_capacity(before.len());

    let mut len_before = before.len();
    while !before.is_empty() {
        let s = before;

        let sl = s.find('/').unwrap_or(s.len());
        out.push_str(&s[..sl]);
        let s = &s[sl..];

        let (a, s) = dbg!(consume_line_comment(s));
        out.push_str(a);
        let (b, s) = dbg!(consume_block_comment(s));
        out.push_str(b);

        let s = if a.len() + b.len() == 0 {
            // did not make progress,
            let i = s.len().min(1);
            out.push_str(&s[..i]);
            &s[i..]
        } else {
            s
        };

        before = s;
        assert_ne!(before.len(), len_before);
        len_before = before.len();
    }

    out
}

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

    macro_rules! preprocess_go_test {
        ($name:ident, $src:expr, $expected:expr$(,)?) => {
            #[test]
            fn $name() {
                let src = $src;
                let expected = $expected;

                assert_eq!(preprocess_go(src), expected);
            }
        };
    }

    preprocess_go_test!(
        preprocess_go_1,
        r#"
        // @ ensures res > 0
        func foo(x int) (res int) {
            return x + 1
        }
        "#,
        r#"
         ensures res > 0
        func foo(x int) (res int) {
            return x + 1
        }
        "#
    );

    preprocess_go_test!(
        preprocess_go_2,
        r#"
        /* @ ensures res > 0 @ */
        func foo(x int) (res int) {
            return x + 1
        }
        "#,
        r#"
         ensures res > 0 
        func foo(x int) (res int) {
            return x + 1
        }
        "#
    );

    preprocess_go_test!(
        preprocess_go_3,
        r#"
        /* @ ensures res > 0  */
        func foo(x int) (res int) {
            return x + 1
        }
        "#,
        r#"
        /* @ ensures res > 0  */
        func foo(x int) (res int) {
            return x + 1
        }
        "#
    );

    preprocess_go_test!(
        preprocess_go_improper_1,
        r#"
        /* @ ensures res > 0
        func foo(x int) (res int) {
            return x + 1
        }
        "#,
        r#"
        /* @ ensures res > 0
        func foo(x int) (res int) {
            return x + 1
        }
        "#
    );

    preprocess_go_test!(preprocess_go_improper_2, r#"/**//"#, r#"/**//"#,);

    preprocess_go_test!(preprocess_go_unexpected_plus, "// +gobra", "// +gobra");
    preprocess_go_test!(
        preprocess_go_unexpected_plus_2,
        "// +gobra\n",
        "// +gobra\n"
    );

    preprocess_go_test!(preprocess_go_4, "// /* */\n", "// /* */\n");
    preprocess_go_test!(preprocess_go_5, "// /* */", "// /* */");

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
