use std::iter::Peekable;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1},
    combinator::fail,
    sequence::tuple,
    IResult, Parser,
};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum Type {
    VerificationFailure,
    Exception,
    Other,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct DiagnosticItem {
    pub(crate) line: u32,
    pub(crate) col: u32,
    pub(crate) message: String,
    pub(crate) typ: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum LogLevel {
    Error,
    Info,
    Warn,
    Debug,
}

fn is_ident_start(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}
fn is_ident_continue(ch: char) -> bool {
    is_ident_start(ch) || ch.is_ascii_digit()
}

fn log_level(s: &str) -> IResult<&str, LogLevel> {
    alt((
        tag("ERROR").map(|_| LogLevel::Error),
        tag("INFO").map(|_| LogLevel::Info),
        tag("WARN").map(|_| LogLevel::Warn),
        tag("DEBUG").map(|_| LogLevel::Debug),
    ))(s)
}

fn log_source<'a, E: nom::error::ParseError<&'a str>>(s: &'a str) -> IResult<&'a str, &'a str, E> {
    let orig = s;
    let (mut s, _) = take_while(is_ident_continue)(s)?;

    loop {
        let Ok((s2, _)) = char::<_, E>('.')(s) else {
            break;
        };
        let Ok((s2, _)) = take_while::<_, _, E>(is_ident_continue)(s2) else {
            break;
        };
        s = s2;
    }

    Ok((s, &orig[..orig.len() - s.len()]))
}

fn log(s: &str) -> IResult<&str, (LogLevel, &str)> {
    let (s, _) = tuple((
        digit1,
        char(':'),
        digit1,
        char(':'),
        digit1,
        char('.'),
        digit1,
        take_while(|x: char| x.is_whitespace()),
        char('['),
        take_until("]"),
        char(']'),
        take_while(|x: char| x.is_whitespace()),
    ))(s)?;
    let (s, log_level) = log_level(s)?;
    let (s, _) = take_while(|x: char| x.is_whitespace())(s)?;
    let (s, src) = log_source(s)?;
    Ok((s, (log_level, src)))
}

fn verification_failure(s: &str) -> IResult<&str, (&str, u32, u32)> {
    let (s, _) = log(s)?;
    let (s, _) = take_until("Error at: <")(s)?;
    let (s, _) = tag("Error at: <")(s)?;
    let (s, (file, _, line, _, col)) =
        tuple((take_until(":"), char(':'), digit1, char(':'), digit1))(s)?;
    let (s, _) = char('>')(s)?;
    let (s, _) = take_while(|x: char| x.is_whitespace())(s)?;
    let line = line.parse().unwrap();
    let col = col.parse().unwrap();
    Ok((s, (file, line, col)))
}

fn exception(s: &str) -> IResult<&str, &str> {
    if !s.contains("Exception") {
        return fail(s);
    }
    let orig = s;

    let (s, a) = take_until("Exception")(s)?;
    let (s, _) = tag("Exception")(s)?;
    let (s, _) = char(':')(s)?;
    let (s, _) = take_while(|x: char| x.is_whitespace())(s)?;

    Ok((s, &orig[..a.len() + "Exception".len()]))
}

fn other(s: &str) -> IResult<&str, ()> {
    let (s, (lvl, _)) = log(s)?;
    if lvl != LogLevel::Error {
        return fail(s);
    }

    Ok((s, ()))
}

impl DiagnosticItem {
    fn more_info<'a>(lines: &mut Peekable<impl Iterator<Item = &'a str>>) -> String {
        let mut more_info = String::new();
        while let Some(next) = lines.peek() {
            if log(next).is_ok() {
                break;
            }
            if !more_info.is_empty() {
                more_info.push('\n');
            }
            more_info += next;
            lines.next();
        }
        more_info
    }

    pub fn verfication_failure<'a>(
        s: &str,
        lines: &mut Peekable<impl Iterator<Item = &'a str>>,
    ) -> Option<Self> {
        let (err, (_, line, col)) = verification_failure(s).ok()?;
        let more_info = Self::more_info(lines);

        Some(DiagnosticItem {
            line,
            col,
            message: format!("{err} {more_info}"),
            typ: Type::VerificationFailure,
        })
    }

    pub fn exception<'a>(
        s: &str,
        lines: &mut Peekable<impl Iterator<Item = &'a str>>,
    ) -> Option<Self> {
        let (err, exception) = exception(s).ok()?;
        let more_info = Self::more_info(lines);

        Some(DiagnosticItem {
            line: 1,
            col: 1,
            message: format!(
                "encountered exception {exception} in gobra. report this. {err}\n{more_info}"
            ),
            typ: Type::Exception,
        })
    }

    pub fn other<'a>(
        s: &str,
        _lines: &mut Peekable<impl Iterator<Item = &'a str>>,
    ) -> Option<Self> {
        let (err, ()) = other(s).ok()?;
        // let more_info = Self::more_info(lines);

        if err.contains("Gobra has found") {
            return None;
        }

        Some(DiagnosticItem {
            line: 1,
            col: 1,
            message: err.to_string(),
            typ: Type::Other,
        })
    }
}

pub fn from_lines<'a>(lines: impl IntoIterator<Item = &'a str>) -> Vec<DiagnosticItem> {
    let lines = &mut (lines.into_iter()).peekable();
    let mut diagnostics = vec![];
    while let Some(line) = lines.next() {
        let Some(diag) = DiagnosticItem::verfication_failure(line, lines)
            .or_else(|| DiagnosticItem::exception(line, lines))
            .or_else(|| DiagnosticItem::other(line, lines))
        else {
            continue;
        };
        diagnostics.push(diag);
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    // #[ignore = "reason"]
    fn t1() {
        let s = r#"
15:00:00.233 [main] INFO viper.gobra.Gobra - Verifying package /home/hyde/projects/gobrapls - main [15:00:00]
15:00:00.719 [main] ERROR viper.gobra.GobraRunner$ - An assumption was violated during execution.
15:00:00.720 [main] ERROR viper.gobra.GobraRunner$ - Logic error: Unexpected constant perm expression: 1.
viper.gobra.util.Violation$LogicException: Logic error: Unexpected constant perm expression: 1.
        at viper.gobra.util.Violation$.violation(Violation.scala:27)
        at viper.gobra.frontend.info.implementation.property.ConstantEvaluation.$anonfun$permConstantEval$1(ConstantEvaluation.scala:179)
        at org.bitbucket.inkytonik.kiama.attribution.AttributionCore$CachedAttribute.liftedTree1$1(AttributionCore.scala:58)
        at org.bitbucket.inkytonik.kiama.attribution.AttributionCore$CachedAttribute.apply(AttributionCore.scala:56)
15:00:00.739 [Thread-0] INFO viper.gobra.Gobra - Writing report to .gobra/stats.json
15:00:00.739 [Thread-0] ERROR viper.gobra.Gobra - Could not write to the file .gobra/stats.json. Check whether the permissions to the file allow writing to it.
"#;
        let parsed = from_lines(s.lines());
        use Type::*;

        assert_eq!(parsed, [
            DiagnosticItem {
                line: 1,
                col: 1,
                message: "$ - An assumption was violated during execution.".into(),
                typ: Other,
            },
            DiagnosticItem {
                line: 1,
                col: 1,
                message: "$ - Logic error: Unexpected constant perm expression: 1.".into(),
                typ: Other,
            },
            DiagnosticItem {
                line: 1,
                col: 1,
                message: "encountered exception viper.gobra.util.Violation$LogicException in gobra. report this. Logic error: Unexpected constant perm expression: 1.\n        at viper.gobra.util.Violation$.violation(Violation.scala:27)\n        at viper.gobra.frontend.info.implementation.property.ConstantEvaluation.$anonfun$permConstantEval$1(ConstantEvaluation.scala:179)\n        at org.bitbucket.inkytonik.kiama.attribution.AttributionCore$CachedAttribute.liftedTree1$1(AttributionCore.scala:58)\n        at org.bitbucket.inkytonik.kiama.attribution.AttributionCore$CachedAttribute.apply(AttributionCore.scala:56)".into(),
                typ: Exception,
            },
            DiagnosticItem {
                line: 1,
                col: 1,
                message: " - Could not write to the file .gobra/stats.json. Check whether the permissions to the file allow writing to it.".into(),
                typ: Other,
            },

        ]);
    }

    #[test]
    fn t2() {
        let s = r#"
Gobra 1.1-SNAPSHOT (1b11869e@(detached))
(c) Copyright ETH Zurich 2012 - 2024
15:49:01.656 [main] INFO viper.gobra.Gobra - Verifying package /home/hyde/projects/gobrapls - main [15:49:01]
15:49:08.997 [ForkJoinPool-3-worker-19] ERROR viper.gobra.reporting.FileWriterReporter - Error at: <foo.go:21:9> Postcondition might not hold.
Assertion res > 0 might not hold.
Assertion res > 0 might not hold.
15:49:08.997 [ForkJoinPool-3-worker-19] ERROR viper.gobra.reporting.FileWriterReporter - Error at: <foo.go:21:9> Postcondition might not hold.
Assertion res > 0 might not hold.
Assertion res > 0 might not hold.
15:49:09.150 [thread-14] ERROR viper.gobra.Gobra - Gobra has found 1 error(s) in package /home/hyde/projects/gobrapls - main
15:49:09.150 [main] INFO viper.gobra.Gobra - Gobra has found 1 error(s)
15:49:09.173 [Thread-0] INFO viper.gobra.Gobra - Writing report to .gobra/stats.json
15:49:09.173 [Thread-0] ERROR viper.gobra.Gobra - Could not write to the file .gobra/stats.json. Check whether the permissions to the file allow writing to it.
"#;
        let parsed = from_lines(s.lines());
        dbg!(&parsed);

        assert_eq!(parsed, [
            DiagnosticItem {
                line: 21,
                col: 9,
                message: "Postcondition might not hold. Assertion res > 0 might not hold.\nAssertion res > 0 might not hold.".into(),
                typ: Type::VerificationFailure
            },
            DiagnosticItem {
                line: 21,
                col: 9,
                message: "Postcondition might not hold. Assertion res > 0 might not hold.\nAssertion res > 0 might not hold.".into(),
                typ: Type::VerificationFailure
            },
            DiagnosticItem {
                line: 1,
                col: 1,
                message: " - Could not write to the file .gobra/stats.json. Check whether the permissions to the file allow writing to it.".into(),
                typ: Type::Other,
            },
        ])
    }
}
