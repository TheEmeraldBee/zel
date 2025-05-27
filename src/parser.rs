use thiserror::Error;

use crate::{
    ast::{expr::Expr, ops::BinaryOp},
    token::Token,
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

        while let Some(expr) = parser.expr()? {
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

    fn expect(res: bool, err_func: impl FnOnce() -> ParseError) -> Result<(), ParseError> {
        match res {
            true => Ok(()),
            false => Err(err_func()),
        }
    }

    fn get(&mut self) -> &Token {
        &self.toks[self.cur as usize]
    }

    fn peek(&mut self) -> Option<&Token> {
        self.toks.get(self.cur as usize + 1)
    }

    fn is(&mut self, tok: &Token) -> bool {
        self.toks[self.cur as usize] == *tok
    }

    fn then(&mut self, tok: &Token) -> bool {
        if (self.cur + 1) as usize >= self.toks.len() {
            return false;
        }
        self.toks[(self.cur + 1) as usize] == *tok
    }

    fn skip(&mut self, tok: &Token) -> Result<(), ParseError> {
        Self::expect(self.advance(), || {
            ParseError::ExpectedFoundEof(format!("{}", tok))
        })?;

        Self::expect(self.is(tok), || {
            ParseError::Expected(tok.to_string(), self.get().to_string())
        })?;

        Ok(())
    }

    fn expr(&mut self) -> Result<Option<Expr>, ParseError> {
        if !self.advance() {
            return Ok(None);
        }

        Ok(Some(self.binary_op(0)?))
    }

    fn binary_op(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.atom()?;

        if let Some(Token::Ctrl('(')) = self.peek() {
            self.advance();
            let (_, args) = self.list(
                &Token::Ctrl('('),
                &Token::Ctrl(')'),
                &Token::Ctrl(','),
                Self::expr,
                "expr".to_string(),
            )?;

            return Ok(Expr::Call {
                func: Box::new(lhs),
                args,
            });
        }

        while let Some(Token::Op(op)) = self.peek() {
            if let Some(op) = BinaryOp::parsed(op) {
                let precedence = op.precedence();
                if precedence < min_precedence {
                    break;
                }

                self.advance();

                if !self.advance() {
                    return Err(ParseError::ExpectedFoundEof("binary op | atom".to_string()));
                }

                let rhs = self.binary_op(precedence)?;

                lhs = Expr::Binary {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                };
            } else {
                return Err(ParseError::Expected(
                    "+, -, *, or /".to_string(),
                    op.to_string(),
                ));
            }
        }

        Ok(lhs)
    }

    fn atom(&mut self) -> Result<Expr, ParseError> {
        Ok(match self.get().clone() {
            Token::Literal(l) => Expr::Literal(l.clone()),

            // This could be either a set or a local
            Token::Ident(l) => {
                if self.then(&Token::Op("=".to_string())) {
                    Self::expect(self.advance(), || {
                        ParseError::ExpectedFoundEof("expr".to_string())
                    })?;

                    let Some(body) = self.expr()? else {
                        return Err(ParseError::ExpectedFoundEof("expr".to_string()));
                    };
                    Expr::Set {
                        name: l.clone(),
                        body: Box::new(body),
                    }
                } else {
                    Expr::Local(l.clone())
                }
            }
            Token::Let => {
                Self::expect(self.advance(), || {
                    ParseError::ExpectedFoundEof("ident".to_string())
                })?;

                let mut_ = self.is(&Token::Mut);

                if mut_ {
                    Self::expect(self.advance(), || {
                        ParseError::ExpectedFoundEof("ident".to_string())
                    })?;
                }

                let Token::Ident(ident) = self.get().clone() else {
                    return Err(ParseError::Expected(
                        "ident".to_string(),
                        format!("{}", self.get()),
                    ));
                };

                self.skip(&Token::Op("=".to_string()))?;

                let body = self.expr()?;
                let Some(body) = body else {
                    return Err(ParseError::ExpectedFoundEof("expr".to_string()));
                };

                Expr::Let {
                    mutable: mut_,
                    name: ident,
                    body: Box::new(body),
                }
            }
            Token::Const => {
                Self::expect(self.advance(), || {
                    ParseError::ExpectedFoundEof("ident".to_string())
                })?;
                let Token::Ident(ident) = self.get().clone() else {
                    return Err(ParseError::Expected(
                        "ident".to_string(),
                        format!("{}", self.get()),
                    ));
                };

                self.skip(&Token::Op("=".to_string()))?;

                let body = self.expr()?;
                let Some(body) = body else {
                    return Err(ParseError::ExpectedFoundEof("expr".to_string()));
                };

                Expr::Const {
                    name: ident,
                    body: Box::new(body),
                }
            }
            Token::Fn => {
                Self::expect(self.advance(), || {
                    ParseError::ExpectedFoundEof("args".to_string())
                })?;
                let (_, args) = self.list(
                    &Token::Ctrl('('),
                    &Token::Ctrl(')'),
                    &Token::Ctrl(','),
                    |p| {
                        Self::expect(p.advance(), || {
                            ParseError::ExpectedFoundEof("ident".to_string())
                        })?;

                        let Token::Ident(t) = p.get() else {
                            return Err(ParseError::Expected(
                                "ident".to_string(),
                                p.get().to_string(),
                            ));
                        };
                        Ok(Some(t.clone()))
                    },
                    "argument".to_string(),
                )?;

                Self::expect(self.advance(), || {
                    ParseError::ExpectedFoundEof("block".to_string())
                })?;

                let body = self.block()?;

                Expr::Func {
                    args,
                    body: Box::new(body),
                }
            }
            Token::Ctrl('{') => Expr::Block {
                body: Box::new(self.block()?),
            },
            Token::Ctrl('(') => {
                let Some(body) = self.expr()? else {
                    return Err(ParseError::ExpectedFoundEof("expr".to_string()));
                };

                Self::expect(self.advance(), || {
                    ParseError::ExpectedFoundEof(Token::Ctrl(')').to_string())
                })?;

                if !self.is(&Token::Ctrl(')')) {
                    return Err(ParseError::Expected(
                        Token::Ctrl(')').to_string(),
                        self.get().to_string(),
                    ));
                }

                body
            }

            t => todo!("{t}"),
        })
    }

    fn list<T: Default>(
        &mut self,
        left: &Token,
        right: &Token,
        separator: &Token,
        each: impl Fn(&mut Self) -> Result<Option<T>, ParseError>,
        item_msg: String,
    ) -> Result<(bool, Vec<T>), ParseError> {
        Self::expect(self.is(left), || {
            ParseError::Expected(left.to_string(), self.get().to_string())
        })?;

        let mut res = vec![];

        let mut any = false;
        while !self.then(right) {
            any = true;
            let Some(expr) = each(self)? else {
                return Err(ParseError::ExpectedFoundEof(item_msg.clone()));
            };
            res.push(expr);

            Self::expect(self.advance(), || {
                ParseError::ExpectedFoundEof(separator.to_string())
            })?;

            if !self.is(separator) {
                break;
            }
        }

        let mut trailing = false;

        if self.is(separator) {
            trailing = true;
        }

        if self.is(separator) || !any {
            Self::expect(self.advance(), || {
                ParseError::ExpectedFoundEof(right.to_string())
            })?;
        }

        Self::expect(self.is(right), || {
            ParseError::Expected(right.to_string(), self.get().to_string())
        })?;

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
