// src/parser.rs

use thiserror::Error;

use crate::{
    ast::{expr::Expr, ops::BinaryOp},
    token::Token,
    types::Type,
};

#[derive(Error, Clone, Debug)]
pub enum ParseError {
    #[error("Expected `{0}`, found `{1}`")]
    Expected(String, String),

    #[error("expected `{0}`, found EOF")]
    ExpectedFoundEof(String),
}

pub struct Parser {
    cur: isize,
    toks: Vec<Token>,
}

impl Parser {
    pub fn parse(input: Vec<Token>) -> Result<Expr, ParseError> {
        let mut parser = Parser {
            cur: -1,
            toks: input,
        };

        let mut exprs = vec![];

        while let Some(expr) = parser.next()? {
            exprs.push(expr);
        }

        Ok(exprs
            .into_iter()
            .reduce(|a, v| Expr::Then {
                first: Box::new(a),
                next: Box::new(v),
            })
            .unwrap_or_default())
    }

    fn advance(&mut self) -> bool {
        self.cur += 1;
        self.cur < self.toks.len() as isize
    }

    fn expect_advance(&mut self, err_val: impl ToString) -> Result<(), ParseError> {
        if self.advance() {
            Ok(())
        } else {
            Err(ParseError::ExpectedFoundEof(err_val.to_string()))
        }
    }

    fn expect(&self, token: &Token) -> Result<(), ParseError> {
        match self.is(token) {
            true => Ok(()),
            false => Err(ParseError::Expected(
                token.to_string(),
                self.get().to_string(),
            )),
        }
    }

    fn get(&self) -> &Token {
        &self.toks[self.cur as usize]
    }

    fn peek(&self) -> Option<&Token> {
        self.toks.get(self.cur as usize + 1)
    }

    fn is(&self, tok: &Token) -> bool {
        self.toks[self.cur as usize] == *tok
    }

    fn then(&mut self, tok: &Token) -> bool {
        if (self.cur + 1) as usize >= self.toks.len() {
            return false;
        }
        self.toks[(self.cur + 1) as usize] == *tok
    }

    fn skip(&mut self, tok: &Token, after: impl ToString) -> Result<(), ParseError> {
        self.expect_advance(tok)?;

        self.expect(tok)?;

        self.expect_advance(after)?;

        Ok(())
    }

    fn next(&mut self) -> Result<Option<Expr>, ParseError> {
        if !self.advance() {
            return Ok(None);
        }

        Ok(Some(self.expr()?))
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        Ok(match self.get() {
            Token::Let => {
                self.expect_advance("mut or ident")?;

                let mut_ = self.is(&Token::Mut);

                if mut_ {
                    self.expect_advance("ident")?;
                }

                let Token::Ident(ident) = self.get().clone() else {
                    return Err(ParseError::Expected(
                        "ident".to_string(),
                        format!("{}", self.get()),
                    ));
                };

                let mut type_annotation = None;
                if self.then(&Token::Ctrl(':')) {
                    self.advance();
                    self.expect_advance("type")?;
                    type_annotation = Some(Box::new(self.type_expr()?));
                }

                self.skip(&Token::Op("=".to_string()), "expr")?;

                let body = self.expr()?;

                Expr::Let {
                    mutable: mut_,
                    name: ident,
                    type_annotation,
                    body: Box::new(body),
                }
            }
            Token::If => {
                self.expect_advance("cond")?;
                let cond = self.expr()?;

                self.expect_advance("block")?;
                let body = self.block()?;

                let mut else_ = None;

                if self.then(&Token::Else) {
                    self.advance();
                    self.expect_advance("if | block")?;
                    if self.is(&Token::If) {
                        else_ = Some(self.expr()?);
                    } else {
                        else_ = Some(self.block()?);
                    }
                }

                Expr::If {
                    cond: Box::new(cond),
                    body: Box::new(body),
                    else_: else_.map(Box::new),
                }
            }

            Token::For => {
                self.expect_advance("cond")?;
                let first = self.expr()?;

                if self.then(&Token::Ctrl(';')) {
                    self.advance();

                    self.expect_advance("cond")?;
                    let cond = self.expr()?;

                    self.skip(&Token::Ctrl(';'), "each")?;

                    let each = self.expr()?;

                    self.expect_advance("body")?;

                    let body = self.block()?;

                    Expr::For {
                        first: Box::new(first),
                        cond: Box::new(cond),
                        each: Box::new(each),
                        body: Box::new(body),
                    }
                } else {
                    self.expect_advance("body")?;

                    let body = self.block()?;

                    Expr::While {
                        cond: Box::new(first),
                        body: Box::new(body),
                    }
                }
            }
            Token::Extern => {
                self.skip(&Token::Fn, "ident")?;

                let Token::Ident(ident) = self.get().clone() else {
                    return Err(ParseError::Expected(
                        "ident".to_string(),
                        self.get().to_string(),
                    ));
                };

                self.expect_advance("args")?;
                self.expect(&Token::Ctrl('('))?;

                let mut args = vec![];

                while !self.then(&Token::Ctrl(')')) {
                    self.expect_advance("argument or )")?;

                    let Token::Ident(t) = self.get().clone() else {
                        return Err(ParseError::Expected(
                            "ident".to_string(),
                            self.get().to_string(),
                        ));
                    };

                    self.skip(&Token::Ctrl(':'), "type")?;
                    let type_ = self.type_expr()?;
                    args.push((t, type_));

                    if self.then(&Token::Ctrl(',')) {
                        self.advance();
                    } else {
                        self.expect_advance(")")?;
                        break;
                    }
                }

                if self.is(&Token::Ctrl(',')) {
                    self.expect_advance(")")?;
                }

                self.expect(&Token::Ctrl(')'))?;

                let mut return_type = Expr::Type(Type::Null);

                if self.then(&Token::Op("->".to_string())) {
                    self.advance();
                    self.expect_advance("expr")?;
                    return_type = self.expr()?;
                }

                Expr::Extern {
                    name: ident,
                    args,
                    return_type: Box::new(return_type),
                }
            }
            _ => self.binary_op(0)?,
        })
    }

    fn type_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.unary()?;

        loop {
            if self.then(&Token::Ctrl('(')) {
                self.advance();
                let (_, args) = self.list(
                    &Token::Ctrl('('),
                    &Token::Ctrl(')'),
                    &Token::Ctrl(','),
                    Self::expr,
                    "expr".to_string(),
                )?;
                lhs = Expr::Call {
                    func: Box::new(lhs),
                    args,
                };
            } else if self.then(&Token::Op(".".to_string())) {
                self.advance();
                self.expect_advance("ident")?;
                let Token::Ident(field) = self.get().clone() else {
                    return Err(ParseError::Expected(
                        "ident".to_string(),
                        self.get().to_string(),
                    ));
                };
                lhs = Expr::Access {
                    val: Box::new(lhs),
                    field,
                };
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn binary_op(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.unary()?;

        while let Some(tok) = self.peek() {
            if let Token::Op(op) = tok {
                if op == "=" {
                    if min_precedence > 0 {
                        break;
                    }

                    match lhs {
                        Expr::Local(_)
                        | Expr::Index { .. }
                        | Expr::Access { .. }
                        | Expr::Deref(_) => {}
                        _ => {
                            return Err(ParseError::Expected(
                                "assignable expression (variable, index, access, or deref)"
                                    .to_string(),
                                format!("{:?}", lhs),
                            ));
                        }
                    }

                    self.advance();
                    self.expect_advance("expression for assignment")?;

                    let rhs = self.binary_op(0)?;

                    lhs = Expr::Set {
                        target: Box::new(lhs),
                        body: Box::new(rhs),
                    };

                    continue;
                }
                if let Some(op) = BinaryOp::parsed(op) {
                    let precedence = op.precedence();
                    if precedence < min_precedence {
                        break;
                    }

                    self.advance();

                    self.expect_advance("binary op | atom")?;

                    let rhs = self.binary_op(precedence)?;

                    lhs = Expr::Binary {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    };
                } else if op == "." {
                    self.advance();
                    self.expect_advance("ident")?;

                    let Token::Ident(field) = self.get().clone() else {
                        return Err(ParseError::Expected(
                            "ident".to_string(),
                            self.get().to_string(),
                        ));
                    };

                    lhs = Expr::Access {
                        val: Box::new(lhs),
                        field,
                    };
                } else {
                    return Err(ParseError::Expected(
                        "+, -, *, /, ==, !=, <=, >=, >, or <".to_string(),
                        op.to_string(),
                    ));
                }
            } else if tok == &Token::Ctrl('(') {
                self.advance();
                let (_, args) = self.list(
                    &Token::Ctrl('('),
                    &Token::Ctrl(')'),
                    &Token::Ctrl(','),
                    Self::expr,
                    "expr".to_string(),
                )?;

                lhs = Expr::Call {
                    func: Box::new(lhs),
                    args,
                };
            } else if tok == &Token::Ctrl('[') {
                self.advance();

                self.advance();
                let next = self.peek().cloned();
                self.cur -= 1;

                if next == Some(Token::Ctrl(':')) {
                    let (_, fields) = self.list(
                        &Token::Ctrl('['),
                        &Token::Ctrl(']'),
                        &Token::Ctrl(','),
                        |p| {
                            let Token::Ident(i) = p.get().clone() else {
                                return Err(ParseError::Expected(
                                    "ident".to_string(),
                                    p.get().to_string(),
                                ));
                            };

                            p.skip(&Token::Ctrl(':'), "expr")?;

                            let expr = p.expr()?;

                            Ok((i.clone(), expr))
                        },
                        "field".to_string(),
                    )?;

                    lhs = Expr::InitStruct {
                        fields,
                        struct_: Box::new(lhs),
                    };
                } else {
                    let (trailing, fields) = self.list(
                        &Token::Ctrl('['),
                        &Token::Ctrl(']'),
                        &Token::Ctrl(','),
                        Self::expr,
                        "field".to_string(),
                    )?;

                    if fields.len() == 1 && !trailing {
                        lhs = Expr::Index {
                            value: Box::new(lhs),
                            index: Box::new(fields[0].clone()),
                        }
                    } else {
                        lhs = Expr::ArrayInit {
                            type_: Box::new(lhs),
                            values: fields,
                        };
                    }
                }
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.is(&Token::Op("&".to_string())) {
            self.expect_advance("expression after '&'")?;
            let expr = self.unary()?;
            Ok(Expr::AddressOf(Box::new(expr)))
        } else if self.is(&Token::Op("*".to_string())) {
            self.expect_advance("expression after '*'")?;
            let expr = self.unary()?;
            Ok(Expr::Deref(Box::new(expr)))
        } else {
            self.atom()
        }
    }

    fn atom(&mut self) -> Result<Expr, ParseError> {
        Ok(match self.get().clone() {
            Token::Literal(l) => Expr::Literal(l.clone()),

            Token::Ident(l) => Expr::Local(l.clone()),

            Token::Fn => {
                self.expect_advance("args")?;
                let (_, args) = self.list(
                    &Token::Ctrl('('),
                    &Token::Ctrl(')'),
                    &Token::Ctrl(','),
                    |p| {
                        let Token::Ident(t) = p.get().clone() else {
                            return Err(ParseError::Expected(
                                "ident".to_string(),
                                p.get().to_string(),
                            ));
                        };

                        p.skip(&Token::Ctrl(':'), "type")?;

                        let type_ = p.type_expr()?;

                        Ok((t, type_))
                    },
                    "argument".to_string(),
                )?;

                self.expect_advance("block or ->")?;

                let mut return_type = Expr::Type(Type::Null);

                if self.is(&Token::Op("->".to_string())) {
                    self.expect_advance("expr")?;
                    return_type = self.expr()?;
                    self.expect_advance("block")?;
                }

                let body = self.block()?;

                Expr::Func {
                    args,
                    body: Box::new(body),
                    return_type: Box::new(return_type),
                }
            }

            Token::Ctrl('{') => self.block()?,
            Token::Ctrl('(') => {
                self.expect_advance("expr")?;

                let body = self.expr()?;

                self.expect_advance(")")?;

                if !self.is(&Token::Ctrl(')')) {
                    return Err(ParseError::Expected(
                        Token::Ctrl(')').to_string(),
                        self.get().to_string(),
                    ));
                }

                body
            }

            Token::Ctrl('[') => {
                self.expect_advance("array content")?;
                let first_elem = self.expr()?;

                if self.then(&Token::Ctrl(';')) {
                    self.advance();
                    self.expect_advance("array size")?;
                    let size = self.expr()?;
                    self.expect_advance("]")?;
                    self.expect(&Token::Ctrl(']'))?;
                    Expr::ArrayFill {
                        value: Box::new(first_elem),
                        size: Box::new(size),
                    }
                } else {
                    let mut values = vec![first_elem];
                    while self.then(&Token::Ctrl(',')) {
                        self.advance();
                        if self.then(&Token::Ctrl(']')) {
                            break;
                        }
                        self.expect_advance("array element")?;
                        values.push(self.expr()?);
                    }
                    self.expect_advance("]")?;
                    self.expect(&Token::Ctrl(']'))?;
                    Expr::ArrayLiteral { values }
                }
            }

            Token::Struct => {
                self.expect_advance("fields")?;
                let (_, fields) = self.list(
                    &Token::Ctrl('{'),
                    &Token::Ctrl('}'),
                    &Token::Ctrl(','),
                    |p| {
                        let Token::Ident(i) = p.get().clone() else {
                            return Err(ParseError::Expected(
                                "ident".to_string(),
                                p.get().to_string(),
                            ));
                        };

                        p.skip(&Token::Ctrl(':'), "expr")?;

                        let expr = p.expr()?;

                        Ok((i.clone(), expr))
                    },
                    "field".to_string(),
                )?;

                Expr::Struct { fields }
            }

            t => todo!("{t}"),
        })
    }

    fn list<T>(
        &mut self,
        left: &Token,
        right: &Token,
        separator: &Token,
        each: impl Fn(&mut Self) -> Result<T, ParseError>,
        item_msg: String,
    ) -> Result<(bool, Vec<T>), ParseError> {
        self.expect(left)?;

        let mut res = vec![];
        let mut trailing = false;

        if self.then(right) {
            self.advance();
            return Ok((trailing, res));
        }

        // Parse first item
        self.expect_advance(item_msg.to_string())?;
        res.push(each(self)?);

        // Parse subsequent items
        while self.then(separator) {
            self.advance(); // consume separator

            if self.then(right) {
                // trailing separator
                trailing = true;
                break;
            }

            self.expect_advance(item_msg.to_string())?;
            res.push(each(self)?);
        }

        self.expect_advance(right)?;
        self.expect(right)?;

        Ok((trailing, res))
    }

    fn block(&mut self) -> Result<Expr, ParseError> {
        let (trailing, mut res) = self.list(
            &Token::Ctrl('{'),
            &Token::Ctrl('}'),
            &Token::Ctrl(';'),
            Self::expr,
            "expr".to_string(),
        )?;

        if trailing {
            res.push(Expr::Null);
        }

        Ok(res
            .into_iter()
            .reduce(|a, v| Expr::Then {
                first: Box::new(a),
                next: Box::new(v),
            })
            .unwrap_or_default())
    }
}
