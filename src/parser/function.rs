use crate::{
    ast::{Declaration, Expression, Function, Pattern, Statement},
    lexer::token::Token,
    parser::{ParseResult, Parser},
};

#[derive(Debug, PartialEq)]
pub(super) enum FunctionKind {
    Declaration,
    Expression,
}

impl<'src> Parser<'src> {
    /// https://tc39.es/ecma262/#prod-FunctionDeclaration
    pub(super) fn parse_function_statement(&mut self) -> ParseResult<Statement> {
        self.expect_current(Token::Function)?;
        Ok(Statement::Declaration(Declaration::FunctionDeclaration(
            self.parse_function(FunctionKind::Declaration)?,
        )))
    }

    /// https://tc39.es/ecma262/#prod-FunctionExpression
    pub(super) fn parse_function_expression(&mut self) -> ParseResult<Expression> {
        self.expect_current(Token::Function)?;
        Ok(Expression::FunctionExpression(Box::new(
            self.parse_function(FunctionKind::Expression)?,
        )))
    }

    pub(super) fn parse_function(&mut self, kind: FunctionKind) -> ParseResult<Function> {
        let is_generator = if matches!(self.current_token, Token::Multiply) {
            self.next_token();
            true
        } else {
            false
        };

        let id = if matches!(self.current_token, Token::LeftParen) {
            match kind {
                FunctionKind::Declaration => return Err(()),
                FunctionKind::Expression => None,
            }
        } else {
            Some(self.parse_identifier()?)
        };

        let params = self.parse_function_params()?;

        let body = self.parse_block_statement()?;

        Ok(Function {
            id,
            params,
            body,
            generator: is_generator,
        })
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<Pattern>> {
        self.expect_current(Token::LeftParen)?;

        let mut params = Vec::new();

        loop {
            if matches!(self.current_token, Token::RightParen) {
                break;
            }

            let identifier = self.parse_identifier()?;

            params.push(Pattern::Identifier(identifier));

            if matches!(self.current_token, Token::Comma) {
                self.next_token();
            } else {
                break;
            }
        }

        self.expect_current(Token::RightParen)?;

        Ok(params)
    }
}

#[cfg(test)]
mod tests {
    use test_utils::{binary_expr, ident, ident_expr, ident_pattern, literal_expr};

    use super::*;
    use crate::{
        ast::{
            BinaryExpression, BinaryOperator, BlockStatement, BooleanLiteral, ExpressionStatement,
            Identifier, Literal, NumberLiteral, Pattern, StringLiteral, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator, YieldExpression,
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

        function basicParams(a, b, c) {
            var d = 45 ** 7;
        }

        function* myGenerator(a, b) {
            var c = a / b % 4 & 27;
            yield c;
            yield* delegated;
        }
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![
                Statement::Declaration(Declaration::FunctionDeclaration(Function {
                    id: Some(ident!("noParams")),
                    params: vec![],
                    generator: false,
                    body: BlockStatement {
                        body: vec![Statement::Declaration(Declaration::VariableDeclaration(
                            VariableDeclaration {
                                kind: VariableDeclarationKind::Var,
                                declarations: vec![VariableDeclarator {
                                    id: ident_pattern!("a"),
                                    init: Some(literal_expr!(true))
                                }]
                            }
                        ))]
                    }
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: ident_pattern!("noParams2"),
                        init: Some(Expression::FunctionExpression(Box::new(Function {
                            id: Some(ident!("someName")),
                            params: vec![],
                            generator: false,
                            body: BlockStatement {
                                body: vec![Statement::Declaration(
                                    Declaration::VariableDeclaration(VariableDeclaration {
                                        kind: VariableDeclarationKind::Var,
                                        declarations: vec![VariableDeclarator {
                                            id: ident_pattern!("a"),
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
                        id: ident_pattern!("noParams3"),
                        init: Some(Expression::FunctionExpression(Box::new(Function {
                            id: None,
                            params: vec![],
                            generator: false,
                            body: BlockStatement {
                                body: vec![Statement::Declaration(
                                    Declaration::VariableDeclaration(VariableDeclaration {
                                        kind: VariableDeclarationKind::Var,
                                        declarations: vec![VariableDeclarator {
                                            id: ident_pattern!("a"),
                                            init: Some(literal_expr!(true))
                                        }]
                                    })
                                )]
                            }
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::FunctionDeclaration(Function {
                    id: Some(ident!("basicParams")),
                    params: vec![
                        ident_pattern!("a"),
                        ident_pattern!("b"),
                        ident_pattern!("c"),
                    ],
                    generator: false,
                    body: BlockStatement {
                        body: vec![Statement::Declaration(Declaration::VariableDeclaration(
                            VariableDeclaration {
                                kind: VariableDeclarationKind::Var,
                                declarations: vec![VariableDeclarator {
                                    id: ident_pattern!("d"),
                                    init: Some(binary_expr!(
                                        literal_expr!(45),
                                        literal_expr!(7),
                                        Exponentiation
                                    ))
                                }]
                            }
                        ))]
                    }
                })),
                Statement::Declaration(Declaration::FunctionDeclaration(Function {
                    id: Some(ident!("myGenerator")),
                    params: vec![ident_pattern!("a"), ident_pattern!("b")],
                    generator: true,
                    body: BlockStatement {
                        body: vec![
                            Statement::Declaration(Declaration::VariableDeclaration(
                                VariableDeclaration {
                                    kind: VariableDeclarationKind::Var,
                                    declarations: vec![VariableDeclarator {
                                        id: ident_pattern!("c"),
                                        init: Some(binary_expr!(
                                            binary_expr!(
                                                binary_expr!(
                                                    ident_expr!("a"),
                                                    ident_expr!("b"),
                                                    Divide
                                                ),
                                                literal_expr!(4),
                                                Remainder
                                            ),
                                            literal_expr!(27),
                                            BitwiseAnd
                                        ))
                                    }]
                                }
                            )),
                            Statement::ExpressionStatement(ExpressionStatement {
                                expression: Expression::YieldExpression(Box::new(
                                    YieldExpression {
                                        argument: Some(ident_expr!("c")),
                                        delegate: false
                                    }
                                ))
                            }),
                            Statement::ExpressionStatement(ExpressionStatement {
                                expression: Expression::YieldExpression(Box::new(
                                    YieldExpression {
                                        argument: Some(ident_expr!("delegated")),
                                        delegate: true
                                    }
                                ))
                            })
                        ]
                    }
                })),
            ]
        );
    }
}
