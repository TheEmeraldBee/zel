use crate::lexer::Token;
use crate::value::Value;
use crate::{Span, Spanned};
use chumsky::input::ValueInput;
use chumsky::prelude::*;

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    Or,
    And,
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Error,
    Value(Value<'src>),
    List(Vec<Spanned<Self>>),
    Local(&'src str),

    Var(&'src str, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Const(&'src str, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Set(&'src str, Box<Spanned<Self>>, Box<Spanned<Self>>),

    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),

    Block(Box<Spanned<Self>>, Box<Spanned<Self>>),

    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),

    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),

    Func(Vec<&'src str>, Box<Spanned<Self>>),
}

pub fn parser<'src, I>(
) -> impl Parser<'src, I, Spanned<Expr<'src>>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|expr| {
        let block = expr
            .clone()
            .or_not()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            .map_with(|a, e| a.unwrap_or_else(|| (Expr::Value(Value::Null), e.span())))
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Expr::Error, span),
            )));

        let if_ = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_.clone()))
                        .or_not(),
                )
                .map_with(|((cond, a), b), e| {
                    (
                        Expr::If(
                            Box::new(cond),
                            Box::new(a),
                            Box::new(b.unwrap_or_else(|| (Expr::Value(Value::Null), e.span()))),
                        ),
                        e.span(),
                    )
                })
        });

        let block_expr = block.clone().or(if_.clone());

        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Bool(x) => Expr::Value(Value::Bool(x)),
                Token::Num(x) => Expr::Value(Value::Num(x)),
                Token::Str(x) => Expr::Value(Value::Str(x.to_string())),
            }
            .labelled("value");

            let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing()
                .collect::<Vec<_>>();

            let var = just(Token::Var)
                .ignore_then(ident)
                .then_ignore(just(Token::Op("=")))
                .then(block_expr.clone().or(inline_expr.clone()))
                .then_ignore(just(Token::Ctrl(';')))
                .then(expr.clone().or_not())
                .map_with(|((name, val), body), e| {
                    Expr::Var(
                        name,
                        Box::new(val),
                        Box::new(body.unwrap_or_else(|| (Expr::Value(Value::Null), e.span()))),
                    )
                });

            let const_ = just(Token::Const)
                .ignore_then(ident)
                .then_ignore(just(Token::Op("=")))
                .then(block_expr.clone().or(inline_expr.clone()))
                .then_ignore(just(Token::Ctrl(';')))
                .then(expr.clone().or_not())
                .map_with(|((name, val), body), e| {
                    Expr::Const(
                        name,
                        Box::new(val),
                        Box::new(body.unwrap_or_else(|| (Expr::Value(Value::Null), e.span()))),
                    )
                });

            let set = ident
                .then_ignore(just(Token::Op("=")))
                .then(block_expr.clone().or(inline_expr.clone()))
                .then_ignore(just(Token::Ctrl(';')))
                .then(expr.clone().or_not())
                .map_with(|((name, val), body), e| {
                    Expr::Set(
                        name,
                        Box::new(val),
                        Box::new(body.unwrap_or_else(|| (Expr::Value(Value::Null), e.span()))),
                    )
                });

            let list = items
                .clone()
                .map(Expr::List)
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')));

            let args = ident
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .labelled("function args");

            let func = just(Token::Fn)
                .ignore_then(args)
                .then(block.clone())
                .map(|(args, body)| Expr::Func(args, Box::new(body)));

            let atom = choice([
                val.boxed(),
                set.boxed(),
                var.boxed(),
                const_.boxed(),
                list.boxed(),
                func.boxed(),
                ident.map(Expr::Local).boxed(),
            ])
            .map_with(|expr, e| (expr, e.span()))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Expr::Error, span),
            )))
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('['),
                Token::Ctrl(']'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| (Expr::Error, span),
            )))
            .boxed();

            let call = atom.foldl_with(
                items
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                    .map_with(|args, e| (args, e.span()))
                    .repeated(),
                |f, args, e| (Expr::Call(Box::new(f), args), e.span()),
            );

            let ops = [
                just(Token::Op("*"))
                    .to(BinaryOp::Mul)
                    .or(just(Token::Op("/")).to(BinaryOp::Div))
                    .boxed(),
                just(Token::Op("+"))
                    .to(BinaryOp::Add)
                    .or(just(Token::Op("-")).to(BinaryOp::Sub))
                    .boxed(),
                choice([
                    just(Token::Op("==")).to(BinaryOp::Eq),
                    just(Token::Op("!=")).to(BinaryOp::NotEq),
                    just(Token::Op("<")).to(BinaryOp::Less),
                    just(Token::Op("<=")).to(BinaryOp::LessEq),
                    just(Token::Op(">")).to(BinaryOp::Greater),
                    just(Token::Op(">=")).to(BinaryOp::GreaterEq),
                ])
                .boxed(),
                just(Token::Op("||")).to(BinaryOp::Or).boxed(),
                just(Token::Op("&&")).to(BinaryOp::And).boxed(),
            ];

            let mut bin_op = call.clone().boxed();

            for op in ops {
                bin_op = bin_op
                    .clone()
                    .foldl_with(op.then(bin_op).repeated(), |a, (op, b), e| {
                        (Expr::Binary(Box::new(a), op, Box::new(b)), e.span())
                    })
                    .boxed();
            }

            bin_op.labelled("expression").as_context()
        });

        let block_chain = block_expr
            .clone()
            .foldl_with(block_expr.clone().repeated(), |a, b, e| {
                (Expr::Then(Box::new(a), Box::new(b)), e.span())
            });

        let block_recovery = nested_delimiters(
            Token::Ctrl('{'),
            Token::Ctrl('}'),
            [
                (Token::Ctrl('('), Token::Ctrl(')')),
                (Token::Ctrl('['), Token::Ctrl(']')),
            ],
            |span| (Expr::Error, span),
        );

        block_chain
            .labelled("block")
            .or(inline_expr.clone())
            .recover_with(skip_then_retry_until(
                block_recovery.ignored(),
                one_of([
                    Token::Ctrl(';'),
                    Token::Ctrl('}'),
                    Token::Ctrl(')'),
                    Token::Ctrl(']'),
                ])
                .ignored(),
            ))
            .foldl_with(
                just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated(),
                |a, b, e| {
                    let span: Span = e.span();
                    (
                        Expr::Then(
                            Box::new(a),
                            Box::new(
                                b.unwrap_or_else(|| (Expr::Value(Value::Null), span.to_end())),
                            ),
                        ),
                        span,
                    )
                },
            )
            .boxed()
    })
}
