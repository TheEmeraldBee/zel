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
    ignore_sep: bool,
}

impl Parser {
    pub fn parse(input: Vec<Token>) -> Result<Expr, ParseError> {
        let mut parser = Parser {
            cur: -1,
            toks: input,
            ignore_sep: false,
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

                self.ignore_sep = true;

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
                    // c-style `for` loop expected now!
                    self.advance();

                    self.expect_advance("cond")?;
                    let cond = self.expr()?;

                    self.skip(&Token::Ctrl(';'), "each")?;

                    let each = self.expr()?;

                    self.expect_advance("body")?;

                    let body = self.block()?;
                    self.ignore_sep = true;

                    Expr::For {
                        first: Box::new(first),
                        cond: Box::new(cond),
                        each: Box::new(each),
                        body: Box::new(body),
                    }
                } else {
                    // Basic while loop!
                    self.expect_advance("body")?;

                    let body = self.block()?;
                    self.ignore_sep = true;

                    Expr::While {
                        cond: Box::new(first),
                        body: Box::new(body),
                    }
                }
            }
            _ => self.binary_op(0)?,
        })
    }

    fn binary_op(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.atom()?;

        while let Some(tok) = self.peek() {
            if let Token::Op(op) = tok {
                if op == "=" {
                    // Assignment has the lowest precedence (e.g., 0) and is right-associative
                    if min_precedence > 0 {
                        break;
                    }

                    // Check if the LHS is a valid assignment target (l-value)
                    match lhs {
                        Expr::Local(_) | Expr::Index { .. } | Expr::Access { .. } => {
                            // This is a valid target
                        }
                        _ => {
                            // You can't assign to a literal like `5 = x`
                            return Err(ParseError::Expected(
                                "assignable expression (variable, index, or access)".to_string(),
                                format!("{:?}", lhs),
                            ));
                        }
                    }

                    self.advance(); // consume '='
                    self.expect_advance("expression for assignment")?;

                    // Parse the right-hand side recursively
                    let rhs = self.binary_op(0)?; // 0 for right-associativity

                    // Create a single `Set` expression
                    lhs = Expr::Set {
                        target: Box::new(lhs),
                        body: Box::new(rhs),
                    };

                    continue; // Continue parsing after the assignment
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

                // Peek 2 elements ahead
                self.advance();
                let next = self.peek().cloned();

                // Go back to the beginning
                self.cur -= 1;

                if next == Some(Token::Ctrl(':')) {
                    // Field initializer! it must be a struct
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
                    // It doesn't have a field separator, so it has to be an array.
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
                        lhs = Expr::Array {
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

    fn atom(&mut self) -> Result<Expr, ParseError> {
        Ok(match self.get().clone() {
            Token::Literal(l) => Expr::Literal(l.clone()),

            // This could be either a set or a local
            Token::Ident(l) => Expr::Local(l.clone()),

            Token::Let => {
                self.expect_advance("ident")?;

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

                self.skip(&Token::Op("=".to_string()), "expr")?;

                let body = self.expr()?;

                Expr::Let {
                    mutable: mut_,
                    name: ident,
                    body: Box::new(body),
                }
            }

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

                        let type_ = p.expr()?;

                        Ok((t, type_))
                    },
                    "argument".to_string(),
                )?;

                self.expect_advance("block")?;

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

            Token::Ctrl('{') => Expr::Block {
                body: Box::new(self.block()?),
            },
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

    fn list<T: Default>(
        &mut self,
        left: &Token,
        right: &Token,

        separator: &Token,
        each: impl Fn(&mut Self) -> Result<T, ParseError>,
        item_msg: String,
    ) -> Result<(bool, Vec<T>), ParseError> {
        self.ignore_sep = false;
        self.expect(left)?;

        let mut res = vec![];

        let mut any = false;

        let mut just_skipped = false;
        while !self.then(right) {
            any = true;

            if !just_skipped {
                self.expect_advance(item_msg.to_string())?;
            } else {
                // We should be on-top of the token
                if self.is(right) {
                    break;
                }
            }
            just_skipped = false;

            let expr = each(self)?;

            res.push(expr);

            if self.ignore_sep {
                self.expect_advance(format!("{} or {}", item_msg.to_string(), separator))?;
                just_skipped = true;
                self.ignore_sep = false;
                continue;
            }

            self.expect_advance(separator)?;

            if !self.is(separator) {
                break;
            }
        }

        let mut trailing = false;

        if self.is(separator) {
            trailing = true;
        }

        if self.is(separator) || !any {
            self.expect_advance(right)?;
        }

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
