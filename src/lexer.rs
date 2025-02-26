use chumsky::prelude::*;

use crate::{Span, Spanned};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Bool(bool),
    Num(f64),
    Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),

    Fn,
    Var,
    Const,

    This,

    For,

    If,
    Else,
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Num)
        .boxed();

    let escape = just('\\')
        .then(choice((
            just('\\'),
            just('\\'),
            just('/'),
            just('"'),
            just('b').to('\x08'),
            just('f').to('\x0C'),
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
            just('u').ignore_then(text::digits(16).exactly(4).to_slice().validate(
                |digits, e, emitter| {
                    char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(|| {
                        emitter.emit(Rich::custom(e.span(), "invalid unicode character"));
                        '\u{FFFD}' // Unicode Replacement Character
                    })
                },
            )),
        )))
        .ignored()
        .boxed();

    let string = none_of("\\\"")
        .ignored()
        .or(escape)
        .repeated()
        .to_slice()
        .map(Token::Str)
        .delimited_by(just('"'), just('"'))
        .boxed();

    let op = one_of("+-*/!?%=><")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    let ctrl = one_of("{}[]();,").map(Token::Ctrl);

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "false" => Token::Bool(false),
        "true" => Token::Bool(true),

        "fn" => Token::Fn,

        "var" => Token::Var,
        "const" => Token::Const,

        "for" => Token::For,

        "self" => Token::This,

        "if" => Token::If,
        "else" => Token::Else,

        _ => Token::Ident(ident),
    });

    let token = num.or(string).or(op).or(ctrl).or(ident);

    let line_comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();
    let block_comment = just("//*")
        .then(any().and_is(just("*//").not()).repeated())
        .padded();

    let comment = line_comment.or(block_comment).ignored();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_if() {
        let lexer = lexer();

        let parsed = lexer.parse("if x == 5.05").unwrap();
        assert_eq!(Token::If, parsed[0].0);
        assert_eq!(Token::Ident("x"), parsed[1].0);
        assert_eq!(Token::Op("=="), parsed[2].0);
        assert_eq!(Token::Num(5.05), parsed[3].0);
    }

    #[test]
    fn test_lex_type() {
        let lexer = lexer();

        let parsed = lexer.parse("!MyType").unwrap();
        assert_eq!(Token::Op("!"), parsed[0].0);
        assert_eq!(Token::Ident("MyType"), parsed[1].0);

        let parsed = lexer.parse("?MyType").unwrap();
        assert_eq!(Token::Op("?"), parsed[0].0);
        assert_eq!(Token::Ident("MyType"), parsed[1].0);
    }

    #[test]
    fn test_ctrl_ident() {
        let lexer = lexer();

        assert_eq!(Token::Ident("me"), lexer.parse("me").unwrap()[0].0);
        assert_eq!(Token::Fn, lexer.parse("fn").unwrap()[0].0);
        assert_eq!(Token::Var, lexer.parse("var").unwrap()[0].0);
        assert_eq!(Token::Const, lexer.parse("const").unwrap()[0].0);
    }

    #[test]
    fn test_ctrl_lex() {
        let lexer = lexer();

        assert_eq!(Token::Ctrl('{'), lexer.parse("{").unwrap()[0].0);
        assert_eq!(Token::Ctrl(']'), lexer.parse("]").unwrap()[0].0);
        assert_eq!(Token::Ctrl('('), lexer.parse("(").unwrap()[0].0);
    }

    #[test]
    fn test_op_lex() {
        let lexer = lexer();

        assert_eq!(Token::Op("+"), lexer.parse("+").unwrap()[0].0);
        assert_eq!(Token::Op("+="), lexer.parse("+=").unwrap()[0].0);
        assert_eq!(Token::Op("=>"), lexer.parse("=>").unwrap()[0].0);
    }

    #[test]
    fn test_string_lex() {
        let lexer = lexer();

        assert_eq!(
            Token::Str("hello, world!"),
            lexer.parse("\"hello, world!\"").unwrap()[0].0
        );
        assert_eq!(
            Token::Str("This is a \n"),
            lexer.parse("\"This is a \n\"").unwrap()[0].0
        );
    }

    #[test]
    fn test_num_lex() {
        let lexer = lexer();

        assert_eq!(Token::Num(5.0), lexer.parse("5").unwrap()[0].0);
        assert_eq!(Token::Num(5.6), lexer.parse("5.6").unwrap()[0].0);
        assert_eq!(Token::Num(5.057), lexer.parse("5.057").unwrap()[0].0);
    }
}
