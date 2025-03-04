use ariadne::*;
use chumsky::error::Rich;

use crate::{Span, lexer::Token};

pub struct Error {
    pub span: Span,
    pub msg: String,
}

impl Error {
    pub fn new(span: Span, msg: impl ToString) -> Self {
        Self {
            span,
            msg: msg.to_string(),
        }
    }
}

pub fn print_errors<'src>(
    filename: &str,
    src: &str,
    lex_errors: Vec<Rich<'src, char>>,
    parse_errs: Vec<Rich<'src, Token<'src>>>,
) {
    lex_errors
        .into_iter()
        .map(|x| x.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| format!("{tok}"))),
        )
        .for_each(|e| {
            Report::build(
                ReportKind::Error,
                (filename, e.span().start..e.span().start),
            )
            .with_message(e.to_string())
            .with_label(
                Label::new((filename, e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new((filename, span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .print((filename, Source::from(src)))
            .unwrap()
        });
}
