use crate::{
    ast::{Declaration, Expression, Function, Identifier, Statement},
    lexer::token::Token,
    parser::{ParseResult, Parser},
};

#[derive(Debug, PartialEq)]
enum FunctionKind {
    Declaration,
    Expression,
}

impl<'src> Parser<'src> {
    /// https://tc39.es/ecma262/#prod-FunctionDeclaration
    pub(super) fn parse_function_statement(&mut self) -> ParseResult<Statement> {
        Ok(Statement::Declaration(Declaration::FunctionDeclaration(
            self.parse_function(FunctionKind::Declaration)?,
        )))
    }

    /// https://tc39.es/ecma262/#prod-FunctionExpression
    pub(super) fn parse_function_expression(&mut self) -> ParseResult<Expression> {
        Ok(Expression::FunctionExpression(Box::new(
            self.parse_function(FunctionKind::Expression)?,
        )))
    }

    fn parse_function(&mut self, kind: FunctionKind) -> ParseResult<Function> {
        self.expect_current(Token::Function)?;

        let id = if matches!(self.current_token, Token::LeftParen) {
            match kind {
                FunctionKind::Declaration => return Err(()),
                FunctionKind::Expression => None,
            }
        } else {
            let Token::Identifier(identifier) = &self.current_token else {
                return Err(());
            };

            let name = identifier.to_string();
            self.next_token();

            Some(Identifier { name })
        };

        self.expect_current(Token::LeftParen)?;
        // TODO: parse function params
        self.expect_current(Token::RightParen)?;

        let body = self.parse_block_statement()?;
        let Statement::BlockStatement(body) = body else {
            return Err(());
        };

        Ok(Function {
            id,
            params: Vec::new(),
            body,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            BlockStatement, BooleanLiteral, Literal, Pattern, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator,
        },
        lexer::Lexer,
    };

    #[test]
    fn parse_function() {
        let input = r#"
        function noParams() {
            var a = true;
        }

        var noParams2 = function someName() {
            var a = true;
        }

        var noParams3 = function() {
            var a = true;
        }
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        macro_rules! literal_expr {
            (true) => {
                Expression::Literal(Literal::BooleanLiteral(BooleanLiteral { value: true }))
            };

            (false) => {
                Expression::Literal(Literal::BooleanLiteral(BooleanLiteral { value: false }))
            };

            (null) => {
                Expression::Literal(Literal::NullLiteral)
            };

            ($lit:literal) => {
                match $lit.to_string().parse::<f64>() {
                    Ok(num) => {
                        Expression::Literal(Literal::NumberLiteral(NumberLiteral { value: num }))
                    }
                    _ => Expression::Literal(Literal::StringLiteral(StringLiteral {
                        value: $lit.to_string(),
                    })),
                }
            };
        }

        assert_eq!(
            program.body,
            vec![
                Statement::Declaration(Declaration::FunctionDeclaration(Function {
                    id: Some(Identifier {
                        name: "noParams".to_string()
                    }),
                    params: vec![],
                    body: BlockStatement {
                        body: vec![Statement::Declaration(Declaration::VariableDeclaration(
                            VariableDeclaration {
                                kind: VariableDeclarationKind::Var,
                                declarations: vec![VariableDeclarator {
                                    id: Pattern::Identifier(Identifier {
                                        name: "a".to_string()
                                    }),
                                    init: Some(literal_expr!(true))
                                }]
                            }
                        ))]
                    }
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "noParams2".to_string()
                        }),
                        init: Some(Expression::FunctionExpression(Box::new(Function {
                            id: Some(Identifier {
                                name: "someName".to_string()
                            }),
                            params: vec![],
                            body: BlockStatement {
                                body: vec![Statement::Declaration(
                                    Declaration::VariableDeclaration(VariableDeclaration {
                                        kind: VariableDeclarationKind::Var,
                                        declarations: vec![VariableDeclarator {
                                            id: Pattern::Identifier(Identifier {
                                                name: "a".to_string()
                                            }),
                                            init: Some(literal_expr!(true))
                                        }]
                                    })
                                )]
                            }
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "noParams3".to_string()
                        }),
                        init: Some(Expression::FunctionExpression(Box::new(Function {
                            id: None,
                            params: vec![],
                            body: BlockStatement {
                                body: vec![Statement::Declaration(
                                    Declaration::VariableDeclaration(VariableDeclaration {
                                        kind: VariableDeclarationKind::Var,
                                        declarations: vec![VariableDeclarator {
                                            id: Pattern::Identifier(Identifier {
                                                name: "a".to_string()
                                            }),
                                            init: Some(literal_expr!(true))
                                        }]
                                    })
                                )]
                            }
                        })))
                    }]
                }))
            ]
        );
    }
}
