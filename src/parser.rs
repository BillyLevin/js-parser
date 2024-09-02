use crate::{
    ast::{
        BlockStatement, BooleanLiteral, BreakStatement, ContinueStatement, Declaration, Expression,
        Identifier, Literal, NumberLiteral, Pattern, Program, RegExp, RegExpLiteral, Statement,
        StringLiteral, VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
    },
    lexer::{token::Token, Lexer},
};

pub struct Parser<'src> {
    lexer: Lexer<'src>,

    current_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            errors: Vec::new(),
        };

        // populate `current_token` and `next_token`
        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program: Program = Program::default();

        loop {
            if self.current_token == Token::Eof {
                break;
            }

            if let Some(statement) = self.parse_statement() {
                program.push_statement(statement);
            } else {
                break;
            }
        }

        program
    }

    /// https://tc39.es/ecma262/#prod-StatementListItem
    /// includes statements and declarations
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Var => self.parse_variable_statement(VariableDeclarationKind::Var),
            Token::Debugger => self.parse_debugger_statement(),
            Token::Semicolon => self.parse_empty_statement(),
            Token::Break => self.parse_break_statement(),
            Token::Continue => self.parse_continue_statement(),
            Token::LeftBrace => self.parse_block_statement(),
            _ => None,
        }
    }

    fn parse_variable_statement(&mut self, kind: VariableDeclarationKind) -> Option<Statement> {
        self.next_token();

        let Token::Identifier(ref identifier) = self.current_token else {
            self.errors
                .push(format!("expected identifier, got {}", self.peek_token));
            return None;
        };

        let identifier = identifier.to_string();

        self.next_token();

        let initializer = if self.current_token == Token::Equal {
            self.next_token();
            Some(self.parse_assignment_expression()?)
        } else {
            None
        };

        let declarator = VariableDeclarator {
            id: Pattern::Identifier(Identifier { name: identifier }),
            init: initializer,
        };

        self.eat_or_insert_semicolon();

        Some(Statement::Declaration(Declaration::VariableDeclaration(
            VariableDeclaration {
                kind,
                declarations: vec![declarator],
            },
        )))
    }

    /// note that the [`AssignmentExpression`](https://tc39.es/ecma262/#prod-AssignmentExpression) from the ECMAScript spec is more broad than the
    /// [`AssignmentExpression`](https://github.com/estree/estree/blob/master/es5.md#assignmentexpression) in the estree spec.
    ///
    /// this function handles the ECMAScript version (which includes the estree version as a
    /// subset)
    fn parse_assignment_expression(&mut self) -> Option<Expression> {
        self.parse_conditional_expression()
    }

    /// https://tc39.es/ecma262/#prod-ConditionalExpression
    fn parse_conditional_expression(&mut self) -> Option<Expression> {
        let lhs = self.parse_prefix_expression()?;

        Some(lhs)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let lhs = match &self.current_token {
            Token::String(value) => {
                Some(Expression::Literal(Literal::StringLiteral(StringLiteral {
                    value: value.to_string(),
                })))
            }
            Token::Decimal(value)
            | Token::Binary(value)
            | Token::Octal(value)
            | Token::Hex(value)
            | Token::LegacyOctal(value) => {
                Some(Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                    value: *value,
                })))
            }
            Token::True => Some(Expression::Literal(Literal::BooleanLiteral(
                BooleanLiteral { value: true },
            ))),
            Token::False => Some(Expression::Literal(Literal::BooleanLiteral(
                BooleanLiteral { value: false },
            ))),
            Token::Null => Some(Expression::Literal(Literal::NullLiteral)),
            Token::RegularExpression { body, flags } => {
                Some(Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
                    regex: RegExp {
                        pattern: body.to_string(),
                        flags: flags.clone(),
                    },
                })))
            }
            _ => None,
        };

        self.next_token();

        lhs
    }

    fn parse_debugger_statement(&mut self) -> Option<Statement> {
        self.next_token();
        self.eat_or_insert_semicolon();

        Some(Statement::DebuggerStatement)
    }

    fn parse_empty_statement(&mut self) -> Option<Statement> {
        self.next_token();
        Some(Statement::EmptyStatement)
    }

    fn parse_break_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let identifier = if let Token::Identifier(name) = &self.current_token {
            Some(Identifier {
                name: name.to_string(),
            })
        } else {
            None
        };

        if identifier.is_some() {
            self.next_token();
        }

        self.eat_or_insert_semicolon();

        Some(Statement::BreakStatement(BreakStatement {
            label: identifier,
        }))
    }

    fn parse_continue_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let identifier = if let Token::Identifier(name) = &self.current_token {
            Some(Identifier {
                name: name.to_string(),
            })
        } else {
            None
        };

        if identifier.is_some() {
            self.next_token();
        }

        self.eat_or_insert_semicolon();

        Some(Statement::ContinueStatement(ContinueStatement {
            label: identifier,
        }))
    }

    /// https://tc39.es/ecma262/#prod-BlockStatement
    fn parse_block_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let mut statement_list = Vec::new();

        loop {
            if self.current_token == Token::RightBrace || self.current_token == Token::Eof {
                break;
            }

            if let Some(statement) = self.parse_statement() {
                statement_list.push(statement);
            } else {
                break;
            }
        }

        if self.expect_current(Token::RightBrace).is_err() {
            return None;
        }

        Some(Statement::BlockStatement(BlockStatement {
            body: statement_list,
        }))
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_current(&mut self, token: Token) -> Result<(), ()> {
        if self.current_token == token {
            self.next_token();
            Ok(())
        } else {
            self.errors
                .push(format!("expected {}, got {}", token, self.current_token));
            Err(())
        }
    }

    /// [Automatic Semicolon Insertion](https://tc39.es/ecma262/#sec-automatic-semicolon-insertion)
    /// TODO: check if allowed to insert semicolon and return a `Result`
    fn eat_or_insert_semicolon(&mut self) {
        if self.current_token == Token::Semicolon {
            self.next_token();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        BlockStatement, BooleanLiteral, BreakStatement, ContinueStatement, Declaration, Expression,
        Identifier, Literal, NumberLiteral, Pattern, Statement, StringLiteral, VariableDeclaration,
        VariableDeclarationKind, VariableDeclarator,
    };

    use super::*;

    #[test]
    fn parse_variable_statement() {
        // TODO: add these back when they can be parsed
        // var b, c
        // var myRegex = /[hello](.*)world[0-9]$/gmi;
        let input = r#"
            var x = 5;
            var y = "hello";
            var z = false;
            var a;
            var d = null;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "x".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                            value: 5.0
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "y".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::StringLiteral(StringLiteral {
                            value: "hello".to_string()
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "z".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::BooleanLiteral(
                            BooleanLiteral { value: false }
                        )))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: None
                    }]
                })),
                // Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                //     kind: VariableDeclarationKind::Var,
                //     declarations: vec![
                //         VariableDeclarator {
                //             id: Pattern::Identifier(Identifier {
                //                 name: "b".to_string()
                //             }),
                //             init: None
                //         },
                //         VariableDeclarator {
                //             id: Pattern::Identifier(Identifier {
                //                 name: "c".to_string()
                //             }),
                //             init: None
                //         }
                //     ]
                // })),
                // Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                //     kind: VariableDeclarationKind::Var,
                //     declarations: vec![VariableDeclarator {
                //         id: Pattern::Identifier(Identifier {
                //             name: "myRegex".to_string()
                //         }),
                //         init: Some(Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
                //             regex: RegExp {
                //                 pattern: "[hello](.*)world[0-9]$".to_string(),
                //                 flags: RegularExpressionFlags::new("gmi")
                //             }
                //         })))
                //     },]
                // })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "d".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::NullLiteral))
                    }]
                })),
            ]
        );
    }

    #[test]
    fn parse_basic_statements() {
        let input = "
            debugger;debugger
            debugger
            ;;
            break;
            break
            break someLabel;
            continue;
            continue
            continue someLabel;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![
                Statement::DebuggerStatement,
                Statement::DebuggerStatement,
                Statement::DebuggerStatement,
                Statement::EmptyStatement,
                Statement::BreakStatement(BreakStatement { label: None }),
                Statement::BreakStatement(BreakStatement { label: None }),
                Statement::BreakStatement(BreakStatement {
                    label: Some(Identifier {
                        name: "someLabel".to_string()
                    })
                }),
                Statement::ContinueStatement(ContinueStatement { label: None }),
                Statement::ContinueStatement(ContinueStatement { label: None }),
                Statement::ContinueStatement(ContinueStatement {
                    label: Some(Identifier {
                        name: "someLabel".to_string()
                    })
                }),
            ]
        );
    }

    #[test]
    fn parse_block_statements() {
        let input = r#"
            {
                debugger;debugger
                debugger
                ;;
            }
            {
                break;
                break
                break someLabel;
            }

            {
                continue;
                continue
                continue someLabel;
            }

            var a = "thing";
            { var a = "another thing"; var b = 54 }
            {}
            { var c = "hello";
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![
                Statement::BlockStatement(BlockStatement {
                    body: vec![
                        Statement::DebuggerStatement,
                        Statement::DebuggerStatement,
                        Statement::DebuggerStatement,
                        Statement::EmptyStatement,
                    ]
                }),
                Statement::BlockStatement(BlockStatement {
                    body: vec![
                        Statement::BreakStatement(BreakStatement { label: None }),
                        Statement::BreakStatement(BreakStatement { label: None }),
                        Statement::BreakStatement(BreakStatement {
                            label: Some(Identifier {
                                name: "someLabel".to_string()
                            })
                        }),
                    ]
                }),
                Statement::BlockStatement(BlockStatement {
                    body: vec![
                        Statement::ContinueStatement(ContinueStatement { label: None }),
                        Statement::ContinueStatement(ContinueStatement { label: None }),
                        Statement::ContinueStatement(ContinueStatement {
                            label: Some(Identifier {
                                name: "someLabel".to_string()
                            })
                        }),
                    ]
                }),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::StringLiteral(StringLiteral {
                            value: "thing".to_string()
                        })))
                    }]
                })),
                Statement::BlockStatement(BlockStatement {
                    body: vec![
                        Statement::Declaration(Declaration::VariableDeclaration(
                            VariableDeclaration {
                                kind: VariableDeclarationKind::Var,
                                declarations: vec![VariableDeclarator {
                                    id: Pattern::Identifier(Identifier {
                                        name: "a".to_string()
                                    }),
                                    init: Some(Expression::Literal(Literal::StringLiteral(
                                        StringLiteral {
                                            value: "another thing".to_string()
                                        }
                                    )))
                                }]
                            }
                        )),
                        Statement::Declaration(Declaration::VariableDeclaration(
                            VariableDeclaration {
                                kind: VariableDeclarationKind::Var,
                                declarations: vec![VariableDeclarator {
                                    id: Pattern::Identifier(Identifier {
                                        name: "b".to_string()
                                    }),
                                    init: Some(Expression::Literal(Literal::NumberLiteral(
                                        NumberLiteral { value: 54.0 }
                                    )))
                                }]
                            }
                        ))
                    ]
                }),
                Statement::BlockStatement(BlockStatement { body: vec![] })
            ]
        );

        assert_eq!(parser.errors, vec!["expected }, got EOF"]);
    }
}
