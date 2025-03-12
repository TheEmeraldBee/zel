#![allow(clippy::result_unit_err)]

use anyhow::anyhow;

use crate::{ast::expr::Expr, token::Token};

#[macro_export]
macro_rules! err {
    ($msg:expr, $expected:expr, $toks:expr) => {
        Err(anyhow!(
            $msg,
            $expected,
            $toks
                .iter()
                .map(|x| x.to_string())
                .reduce(|lhs, rhs| format!("{lhs}, {rhs}"))
                .unwrap_or_else(|| "`unknown`".to_string())
        ))
    };
}

#[macro_export]
macro_rules! any {
    ($($par:expr),* $(,)?) => {
        any([
            $(($par).boxed()),*
        ])
    }
}

#[macro_export]
macro_rules! matched {
    ($pat:pat => $expr:expr, $err:expr) => {{
        let method = |inp, parser: &mut Parser| {
            let tok = parser.get().clone();
            parser.adv();
            match tok {
                $pat => Ok((inp, $expr)),
                _ => Err(()),
            }
        };
        (method, || $err.to_string())
    }};
}

pub struct Parser<'src> {
    cur: usize,
    tokens: &'src [Token],

    span_start: usize,
}

fn expr_<I: Clone + Sync + Send + 'static>() -> impl Parse<I, Expr> {
    any![const_()]
}

fn const_<I: Clone + Sync + Send + 'static>() -> impl Parse<I, Expr> {
    just(Token::Const)
        .ignore_then(matched!(Token::Ident(name) => name, "ident"))
        .then_ignore(just(Token::Op("=".to_string())))
        .then(expr_())
        .map(|((_, l), body)| Expr::Const {
            name: l,
            body: Box::new(body),
        })
        .labelled("expression")
}

impl<'src> Parser<'src> {
    pub fn parse(tokens: &'src [Token]) -> anyhow::Result<Expr> {
        let mut parser = Parser {
            cur: 0,
            tokens,
            span_start: 0,
        };

        let parse = expr_().repeated();

        match parse.parse((), &mut parser) {
            Ok(t) => Ok(t
                .into_iter()
                .reduce(|lhs, rhs| Expr::Then {
                    first: Box::new(lhs),
                    next: Box::new(rhs),
                })
                .unwrap_or(Expr::Null)),
            Err(_) => {
                err!(
                    "Expected One Of: {}\nFound: {}",
                    parse.err_msg(),
                    parser.tok_span()
                )
            }
        }
    }

    pub fn adv(&mut self) {
        self.cur += 1;
    }

    pub fn get(&self) -> &Token {
        &self.tokens[self.cur]
    }

    pub fn is(&self, token: &Token) -> bool {
        &self.tokens[self.cur] == token
    }

    pub fn is_end(&self) -> bool {
        self.cur >= self.tokens.len() - 1
    }

    pub fn start_span(&mut self) {
        self.span_start = self.cur;
    }

    pub fn tok_span(&self) -> &[Token] {
        &self.tokens[self.span_start..=self.cur]
    }
}

pub trait Boxed<I: Clone, R: Clone> {
    fn boxed(self) -> Box<dyn Parse<I, R>>;
}

impl<I: Clone, R: Clone, P: Parse<I, R> + 'static> Boxed<I, R> for P {
    fn boxed(self) -> Box<dyn Parse<I, R>> {
        Box::new(self)
    }
}

pub trait Parse<I: Send + Sync, R: Send + Sync> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<R, ()>;
    fn err_msg(&self) -> String;
}

pub trait ParseExt<I: Send + Sync, R: Send + Sync>: Parse<I, R> {
    fn then<T: Send + Sync>(self, then: impl Parse<R, T> + 'static) -> Then<I, R, T>;
    fn ignore_then<T: Send + Sync>(self, then: impl Parse<I, T> + 'static) -> IgnoreThen<I, R, T>;
    fn then_ignore<T: Send + Sync>(self, then: impl Parse<I, T> + 'static) -> ThenIgnore<I, R, T>;
    fn ignore(self) -> Ignore<I, R>;
    fn repeated(self) -> Repeated<I, R>;
    fn maybe(self) -> Maybe<I, R>;
    fn map<T: Send + Sync>(self, mapper: impl Fn(R) -> T + 'static) -> Map<I, R, T>;
    fn rest(self) -> Rest<I, R>;
    fn labelled(self, label: impl ToString) -> Labeled<I, R>;
}

impl<I: Send + Sync + 'static, R: Send + Sync + 'static, P: Parse<I, R> + 'static> ParseExt<I, R>
    for P
{
    fn then<T: Send + Sync>(self, then: impl Parse<R, T> + 'static) -> Then<I, R, T> {
        Then(Box::new(self), Box::new(then))
    }

    fn ignore_then<T: Send + Sync>(self, then: impl Parse<I, T> + 'static) -> IgnoreThen<I, R, T> {
        IgnoreThen(Box::new(self), Box::new(then))
    }

    fn then_ignore<T: Send + Sync>(self, then: impl Parse<I, T> + 'static) -> ThenIgnore<I, R, T> {
        ThenIgnore(Box::new(self), Box::new(then))
    }

    fn ignore(self) -> Ignore<I, R> {
        Ignore(Box::new(self))
    }

    fn repeated(self) -> Repeated<I, R> {
        Repeated(Box::new(self))
    }

    fn maybe(self) -> Maybe<I, R> {
        Maybe(Box::new(self))
    }

    fn map<T: Send + Sync>(self, mapper: impl Fn(R) -> T + 'static) -> Map<I, R, T> {
        Map(Box::new(self), Box::new(mapper))
    }

    fn rest(self) -> Rest<I, R> {
        Rest(Box::new(self))
    }

    fn labelled(self, label: impl ToString) -> Labeled<I, R> {
        Labeled(Box::new(self), label.to_string())
    }
}

pub struct Just(Token);
impl<I: Send + Sync> Parse<I, I> for Just {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<I, ()> {
        parser.start_span();
        match parser.is(&self.0) {
            true => (),
            false => return Err(()),
        };
        parser.adv();

        Ok(last)
    }
    fn err_msg(&self) -> String {
        format!("{}", self.0)
    }
}

pub fn just(token: Token) -> Just {
    Just(token)
}

pub struct Then<I, R, T>(Box<dyn Parse<I, R>>, Box<dyn Parse<R, T>>);
impl<I, R: Clone, T> Parse<I, (R, T)> for Then<I, R, T> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<(R, T), ()> {
        let res = self.0.parse(last, parser)?;

        Ok((res.clone(), self.1.parse(res, parser)?))
    }
    fn err_msg(&self) -> String {
        format!("{} -> {}", self.0.err_msg(), self.1.err_msg())
    }
}

pub struct IgnoreThen<I, R, T>(Box<dyn Parse<I, R>>, Box<dyn Parse<I, T>>);
impl<I: Clone + Send + Sync, R: Send + Sync, T: Send + Sync> Parse<I, T> for IgnoreThen<I, R, T> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<T, ()> {
        self.0.parse(last.clone(), parser)?;

        self.1.parse(last, parser)
    }
    fn err_msg(&self) -> String {
        format!("{} -> {}", self.0.err_msg(), self.1.err_msg())
    }
}

pub struct ThenIgnore<I, R, T>(Box<dyn Parse<I, R>>, Box<dyn Parse<I, T>>);
impl<I: Clone + Send + Sync, R: Send + Sync, T: Send + Sync> Parse<I, R> for ThenIgnore<I, R, T> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<R, ()> {
        let res = self.0.parse(last.clone(), parser)?;

        self.1.parse(last, parser)?;

        Ok(res)
    }
    fn err_msg(&self) -> String {
        format!("{} -> {}", self.0.err_msg(), self.1.err_msg())
    }
}

pub struct Ignore<I, R>(Box<dyn Parse<I, R>>);
impl<I: Clone + Send + Sync, R: Send + Sync> Parse<I, I> for Ignore<I, R> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<I, ()> {
        self.0.parse(last.clone(), parser)?;
        Ok(last)
    }
    fn err_msg(&self) -> String {
        self.0.err_msg()
    }
}

pub struct Any<I, R>(Vec<Box<dyn Parse<I, R>>>);
impl<I: Clone + Send + Sync, R: Send + Sync> Parse<I, R> for Any<I, R> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<R, ()> {
        let start = parser.cur;
        for parse in &self.0 {
            if let Ok(r) = parse.parse(last.clone(), parser) {
                return Ok(r);
            } else {
                parser.cur = start;
            }
        }

        Err(())
    }
    fn err_msg(&self) -> String {
        self.0
            .iter()
            .map(|x| x.err_msg())
            .reduce(|lhs, rhs| format!("{}, {}", lhs, rhs))
            .unwrap_or_else(|| "".to_string())
    }
}

pub fn any<I, R>(parsers: impl IntoIterator<Item = Box<dyn Parse<I, R>>>) -> Any<I, R> {
    Any(parsers.into_iter().collect())
}

impl<I: Send + Sync, R: Send + Sync, T, E> Parse<I, R> for (T, E)
where
    T: Fn(I, &mut Parser) -> Result<R, ()>,
    E: Fn() -> String,
{
    fn parse(&self, last: I, parser: &mut Parser) -> Result<R, ()> {
        self.0(last, parser)
    }
    fn err_msg(&self) -> String {
        self.1().to_string()
    }
}

pub struct Repeated<I, R>(Box<dyn Parse<I, R>>);
impl<I: Clone + Send + Sync, R: Send + Sync> Parse<I, Vec<R>> for Repeated<I, R> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<Vec<R>, ()> {
        let mut res = vec![];
        while let Ok(x) = self.0.parse(last.clone(), parser) {
            res.push(x);
        }
        Ok(res)
    }
    fn err_msg(&self) -> String {
        self.0.err_msg()
    }
}

pub struct Rest<I, R>(Box<dyn Parse<I, R>>);
impl<I: Send + Sync, R: Send + Sync> Parse<I, R> for Rest<I, R> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<R, ()> {
        let res = self.0.parse(last, parser)?;
        if parser.is_end() { Ok(res) } else { Err(()) }
    }
    fn err_msg(&self) -> String {
        format!("EOF, {}", self.0.err_msg())
    }
}

pub struct Labeled<I, R>(Box<dyn Parse<I, R>>, String);
impl<I: Send + Sync, R: Send + Sync> Parse<I, R> for Labeled<I, R> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<R, ()> {
        self.0.parse(last, parser)
    }
    fn err_msg(&self) -> String {
        self.1.clone()
    }
}

pub struct Maybe<I, R>(Box<dyn Parse<I, R>>);
impl<I: Send + Sync, R: Send + Sync> Parse<I, Option<R>> for Maybe<I, R> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<Option<R>, ()> {
        match self.0.parse(last, parser) {
            Ok(t) => Ok(Some(t)),
            Err(_) => Ok(None),
        }
    }
    fn err_msg(&self) -> String {
        self.0.err_msg()
    }
}

pub struct Map<I, R, P>(Box<dyn Parse<I, R>>, Box<dyn Fn(R) -> P>);
impl<I: Send + Sync, R: Send + Sync, P: Send + Sync> Parse<I, P> for Map<I, R, P> {
    fn parse(&self, last: I, parser: &mut Parser) -> Result<P, ()> {
        let res = self.0.parse(last, parser)?;
        Ok(self.1(res))
    }
    fn err_msg(&self) -> String {
        "".to_string()
    }
}
