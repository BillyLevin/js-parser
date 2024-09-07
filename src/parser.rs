use crate::{
    ast::{
        BinaryExpression, BinaryOperator, BlockStatement, BooleanLiteral, BreakStatement,
        ContinueStatement, Declaration, Expression, Identifier, LabeledStatement, Literal,
        NumberLiteral, Pattern, Program, RegExp, RegExpLiteral, Statement, StringLiteral,
        VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
    },
    lexer::{token::Token, Lexer},
    precedence::Precedence,
};

pub struct Parser<'src> {
    lexer: Lexer<'src>,

    current_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

type ParseResult<T> = Result<T, ()>;

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            errors: Vec::new(),
        };

        // populate `current_token` and `peek_token`
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program: Program = Program::default();

        loop {
            if self.current_token == Token::Eof {
                break;
            }

            if let Ok(statement) = self.parse_statement() {
                program.push_statement(statement);
            } else {
                break;
            }
        }

        program
    }

    /// https://tc39.es/ecma262/#prod-StatementListItem
    /// includes statements and declarations
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.current_token {
            Token::Var => self.parse_variable_statement(VariableDeclarationKind::Var),
            Token::Debugger => self.parse_debugger_statement(),
            Token::Semicolon => self.parse_empty_statement(),
            Token::Break => self.parse_break_statement(),
            Token::Continue => self.parse_continue_statement(),
            Token::LeftBrace => self.parse_block_statement(),
            Token::Identifier(_) if self.peek_token == Token::Colon => {
                self.parse_labeled_statement()
            }
            _ => Err(()),
        }
    }

    fn parse_variable_statement(
        &mut self,
        kind: VariableDeclarationKind,
    ) -> ParseResult<Statement> {
        self.next_token();

        let mut declaration_list = Vec::new();

        loop {
            declaration_list.push(self.parse_variable_declarator()?);

            if self.current_token == Token::Comma {
                self.next_token();
                continue;
            }

            break;
        }

        self.eat_or_insert_semicolon();

        Ok(Statement::Declaration(Declaration::VariableDeclaration(
            VariableDeclaration {
                kind,
                declarations: declaration_list,
            },
        )))
    }

    fn parse_variable_declarator(&mut self) -> ParseResult<VariableDeclarator> {
        let Token::Identifier(ref identifier) = self.current_token else {
            self.errors
                .push(format!("expected identifier, got {}", self.current_token));
            return Err(());
        };

        let identifier = identifier.to_string();

        self.next_token();

        let initializer = if self.current_token == Token::Equal {
            self.next_token();
            Some(self.parse_assignment_expression()?)
        } else {
            None
        };

        Ok(VariableDeclarator {
            id: Pattern::Identifier(Identifier { name: identifier }),
            init: initializer,
        })
    }

    /// note that the [`AssignmentExpression`](https://tc39.es/ecma262/#prod-AssignmentExpression) from the ECMAScript spec is more broad than the
    /// [`AssignmentExpression`](https://github.com/estree/estree/blob/master/es5.md#assignmentexpression) in the estree spec.
    ///
    /// this function handles the ECMAScript version (which includes the estree version as a subset)
    fn parse_assignment_expression(&mut self) -> ParseResult<Expression> {
        self.parse_binary_expression(Precedence::Comma)
    }

    fn parse_binary_expression(&mut self, min_precedence: Precedence) -> ParseResult<Expression> {
        let mut lhs = self.parse_primary_expression()?;

        loop {
            let Ok(precedence) = Precedence::try_from(&self.current_token) else {
                break;
            };

            if precedence.is_right_associative() {
                if precedence < min_precedence {
                    break;
                }
            } else if precedence <= min_precedence {
                break;
            }

            let operator = BinaryOperator::from(&self.current_token);

            self.next_token();

            let rhs = self.parse_binary_expression(precedence)?;

            lhs = Expression::BinaryExpression(Box::new(BinaryExpression {
                left: lhs,
                right: rhs,
                operator,
            }));
        }

        Ok(lhs)
    }

    fn parse_primary_expression(&mut self) -> ParseResult<Expression> {
        let lhs = match &self.current_token {
            Token::String(value) => {
                Ok(Expression::Literal(Literal::StringLiteral(StringLiteral {
                    value: value.to_string(),
                })))
            }
            Token::Decimal(value)
            | Token::Binary(value)
            | Token::Octal(value)
            | Token::Hex(value)
            | Token::LegacyOctal(value) => {
                Ok(Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                    value: *value,
                })))
            }
            Token::True => Ok(Expression::Literal(Literal::BooleanLiteral(
                BooleanLiteral { value: true },
            ))),
            Token::False => Ok(Expression::Literal(Literal::BooleanLiteral(
                BooleanLiteral { value: false },
            ))),
            Token::Null => Ok(Expression::Literal(Literal::NullLiteral)),
            Token::RegularExpression { body, flags } => {
                Ok(Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
                    regex: RegExp {
                        pattern: body.to_string(),
                        flags: flags.clone(),
                    },
                })))
            }
            Token::Divide | Token::DivideEqual => self.parse_regular_expression_literal(),
            Token::Identifier(_) => self.parse_identifier_expression(),
            _ => Err(()),
        };

        self.next_token();

        lhs
    }

    fn parse_identifier_expression(&self) -> ParseResult<Expression> {
        let Token::Identifier(ref identifier) = self.current_token else {
            return Err(());
        };

        Ok(Expression::Identifier(Identifier {
            name: identifier.to_string(),
        }))
    }

    fn parse_debugger_statement(&mut self) -> ParseResult<Statement> {
        self.next_token();
        self.eat_or_insert_semicolon();

        Ok(Statement::DebuggerStatement)
    }

    fn parse_empty_statement(&mut self) -> ParseResult<Statement> {
        self.next_token();
        Ok(Statement::EmptyStatement)
    }

    fn parse_break_statement(&mut self) -> ParseResult<Statement> {
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

        Ok(Statement::BreakStatement(BreakStatement {
            label: identifier,
        }))
    }

    fn parse_continue_statement(&mut self) -> ParseResult<Statement> {
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

        Ok(Statement::ContinueStatement(ContinueStatement {
            label: identifier,
        }))
    }

    /// https://tc39.es/ecma262/#prod-BlockStatement
    fn parse_block_statement(&mut self) -> ParseResult<Statement> {
        self.next_token();

        let mut statement_list = Vec::new();

        loop {
            if self.current_token == Token::RightBrace || self.current_token == Token::Eof {
                break;
            }

            if let Ok(statement) = self.parse_statement() {
                statement_list.push(statement);
            } else {
                break;
            }
        }

        self.expect_current(Token::RightBrace)?;

        Ok(Statement::BlockStatement(BlockStatement {
            body: statement_list,
        }))
    }

    fn parse_labeled_statement(&mut self) -> ParseResult<Statement> {
        let Token::Identifier(ref identifier) = self.current_token else {
            self.errors
                .push(format!("expected identifier, got {}", self.current_token));
            return Err(());
        };

        let identifier = identifier.to_string();

        self.next_token();
        self.expect_current(Token::Colon)?;

        let statement = self.parse_statement()?;

        Ok(Statement::LabeledStatement(Box::new(LabeledStatement {
            label: Identifier { name: identifier },
            body: statement,
        })))
    }

    fn parse_regular_expression_literal(&mut self) -> ParseResult<Expression> {
        let regex = self.lexer.read_regex(self.current_token.clone());

        let Token::RegularExpression { body, flags } = regex else {
            return Err(());
        };

        Ok(Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
            regex: RegExp {
                pattern: body,
                flags,
            },
        })))
    }

    fn next_token(&mut self) {
        self.current_token = self.lexer.next_token();
        self.peek_token = self.lexer.clone().next_token();
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
    use crate::{
        ast::{
            BinaryExpression, BinaryOperator, BlockStatement, BooleanLiteral, BreakStatement,
            ContinueStatement, Declaration, Expression, Identifier, LabeledStatement, Literal,
            NumberLiteral, Pattern, Statement, StringLiteral, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator,
        },
        lexer::RegularExpressionFlags,
    };

    use super::*;

    #[test]
    fn parse_variable_statement() {
        let input = r#"
            var x = 5;
            var y = "hello";
            var z = false;
            var a;
            var d = null;
            var myRegex = /[hello](.*)world[0-9]$/gmi;
            var myRegex2 = /=start[a-z]\/with(.*)equals/yu;
            var b, c = 4;
            var hello = "world", bool = false;
            var sum = 4 + 5;
            var product = 27 * 8;
            var precedence = 4 + 27 * 8;
            var precedence2 = 4 * 27 + 8;
            var lotsOfOperations = 4 * 27 / 45 + 4 - 7.5 * 2 + 67.45 / 3;
            var exp = 4 ** 3 ** 2 + 34 * 4;
            var exp2 = 45 * 7 ** 3;
            var remainder = 45 % 5 + 3;
            var remainder2 = 45 + 5 % 3;
            var leftShift = 45 << 5 * 3;
            var leftShift2 = 45 * 5 << 3;
            var rightShift = 45 >> 5 * 3;
            var rightShift2 = 45 * 5 >> 3;
            var unsignedRightShift = 45 >>> 5 * 3;
            var unsignedRightShift2 = 45 * 5 >>> 3;
            var lessThan = 34 < 7 + 2;
            var lessThan2 = 34 + 7 < 2;
            var lessThanEqual = 34 <= 7 + 2;
            var lessThanEqual2 = 34 + 7 <= 2;
            var greaterThan = 34 > 7 + 2;
            var greaterThan2 = 34 + 7 > 2;
            var greaterThanEqual = 34 >= 7 + 2;
            var greaterThanEqual2 = 34 + 7 >= 2;
            var inOperator = "property" in y;
            var instanceofOperator = x instanceof y;
            var doubleEquals = x == 4;
            var doubleEquals2 = "hello" == false;
            var notDoubleEquals = 4 != x;
            var notDoubleEquals2 = false != "hello";
            var tripleEquals = x === 4;
            var tripleEquals2 = "hello" === false;
            var notTripleEquals = 4 !== x;
            var notTripleEquals2 = false !== "hello";
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
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "d".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::NullLiteral))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "myRegex".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
                            regex: RegExp {
                                pattern: "[hello](.*)world[0-9]$".to_string(),
                                flags: RegularExpressionFlags::new("gmi")
                            }
                        })))
                    },]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "myRegex2".to_string()
                        }),
                        init: Some(Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
                            regex: RegExp {
                                pattern: "=start[a-z]\\/with(.*)equals".to_string(),
                                flags: RegularExpressionFlags::new("yu")
                            }
                        })))
                    },]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![
                        VariableDeclarator {
                            id: Pattern::Identifier(Identifier {
                                name: "b".to_string()
                            }),
                            init: None
                        },
                        VariableDeclarator {
                            id: Pattern::Identifier(Identifier {
                                name: "c".to_string()
                            }),
                            init: Some(Expression::Literal(Literal::NumberLiteral(
                                NumberLiteral { value: 4.0 }
                            )))
                        }
                    ]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![
                        VariableDeclarator {
                            id: Pattern::Identifier(Identifier {
                                name: "hello".to_string()
                            }),
                            init: Some(Expression::Literal(Literal::StringLiteral(
                                StringLiteral {
                                    value: "world".to_string()
                                }
                            )))
                        },
                        VariableDeclarator {
                            id: Pattern::Identifier(Identifier {
                                name: "bool".to_string()
                            }),
                            init: Some(Expression::Literal(Literal::BooleanLiteral(
                                BooleanLiteral { value: false }
                            )))
                        }
                    ]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "sum".to_string()
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 4.0
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 5.0
                            })),
                            operator: BinaryOperator::Plus
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "product".to_string()
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 27.0
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 8.0
                            })),
                            operator: BinaryOperator::Multiply
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "precedence".to_string()
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 4.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 27.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 8.0
                                })),
                                operator: BinaryOperator::Multiply
                            })),
                            operator: BinaryOperator::Plus
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "precedence2".to_string()
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 4.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 27.0
                                })),
                                operator: BinaryOperator::Multiply
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 8.0
                            })),
                            operator: BinaryOperator::Plus
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lotsOfOperations".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                    left: Expression::BinaryExpression(Box::new(
                                        BinaryExpression {
                                            left: Expression::BinaryExpression(Box::new(
                                                BinaryExpression {
                                                    left: Expression::Literal(
                                                        Literal::NumberLiteral(NumberLiteral {
                                                            value: 4.0
                                                        })
                                                    ),
                                                    right: Expression::Literal(
                                                        Literal::NumberLiteral(NumberLiteral {
                                                            value: 27.0
                                                        })
                                                    ),
                                                    operator: BinaryOperator::Multiply
                                                }
                                            )),
                                            right: Expression::Literal(Literal::NumberLiteral(
                                                NumberLiteral { value: 45.0 }
                                            )),
                                            operator: BinaryOperator::Divide
                                        }
                                    )),
                                    right: Expression::Literal(Literal::NumberLiteral(
                                        NumberLiteral { value: 4.0 }
                                    )),
                                    operator: BinaryOperator::Plus
                                })),
                                right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                    left: Expression::Literal(Literal::NumberLiteral(
                                        NumberLiteral { value: 7.5 }
                                    )),
                                    right: Expression::Literal(Literal::NumberLiteral(
                                        NumberLiteral { value: 2.0 }
                                    )),
                                    operator: BinaryOperator::Multiply
                                })),
                                operator: BinaryOperator::Minus
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 67.45
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 3.0
                                })),
                                operator: BinaryOperator::Divide
                            })),
                            operator: BinaryOperator::Plus
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "exp".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 4.0
                                })),
                                right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                    left: Expression::Literal(Literal::NumberLiteral(
                                        NumberLiteral { value: 3.0 }
                                    )),
                                    right: Expression::Literal(Literal::NumberLiteral(
                                        NumberLiteral { value: 2.0 }
                                    )),
                                    operator: BinaryOperator::Exponentiation
                                })),
                                operator: BinaryOperator::Exponentiation
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 34.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 4.0
                                })),
                                operator: BinaryOperator::Multiply,
                            })),
                            operator: BinaryOperator::Plus
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "exp2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 45.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 3.0
                                })),
                                operator: BinaryOperator::Exponentiation,
                            })),
                            operator: BinaryOperator::Multiply
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "remainder".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 45.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                operator: BinaryOperator::Remainder,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 3.0
                            })),
                            operator: BinaryOperator::Plus
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "remainder2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 45.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 3.0
                                })),
                                operator: BinaryOperator::Remainder,
                            })),
                            operator: BinaryOperator::Plus
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "leftShift".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 45.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 3.0
                                })),
                                operator: BinaryOperator::Multiply,
                            })),
                            operator: BinaryOperator::LeftShift
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "leftShift2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 45.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                operator: BinaryOperator::Multiply,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 3.0
                            })),
                            operator: BinaryOperator::LeftShift
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "rightShift".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 45.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 3.0
                                })),
                                operator: BinaryOperator::Multiply,
                            })),
                            operator: BinaryOperator::RightShift
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "rightShift2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 45.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                operator: BinaryOperator::Multiply,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 3.0
                            })),
                            operator: BinaryOperator::RightShift
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "unsignedRightShift".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 45.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 3.0
                                })),
                                operator: BinaryOperator::Multiply,
                            })),
                            operator: BinaryOperator::UnsignedRightShift
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "unsignedRightShift2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 45.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 5.0
                                })),
                                operator: BinaryOperator::Multiply,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 3.0
                            })),
                            operator: BinaryOperator::UnsignedRightShift
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThan".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 34.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 2.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            operator: BinaryOperator::LessThan
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThan2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 34.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 2.0
                            })),
                            operator: BinaryOperator::LessThan
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThanEqual".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 34.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 2.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            operator: BinaryOperator::LessThanEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThanEqual2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 34.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 2.0
                            })),
                            operator: BinaryOperator::LessThanEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThan".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 34.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 2.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            operator: BinaryOperator::GreaterThan
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThan2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 34.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 2.0
                            })),
                            operator: BinaryOperator::GreaterThan
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThanEqual".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 34.0
                            })),
                            right: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 2.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            operator: BinaryOperator::GreaterThanEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThanEqual2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::BinaryExpression(Box::new(BinaryExpression {
                                left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 34.0
                                })),
                                right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                    value: 7.0
                                })),
                                operator: BinaryOperator::Plus,
                            })),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 2.0
                            })),
                            operator: BinaryOperator::GreaterThanEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "inOperator".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::StringLiteral(StringLiteral {
                                value: "property".to_string(),
                            })),
                            right: Expression::Identifier(Identifier {
                                name: "y".to_string()
                            }),
                            operator: BinaryOperator::In
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "instanceofOperator".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Identifier(Identifier {
                                name: "x".to_string()
                            }),
                            right: Expression::Identifier(Identifier {
                                name: "y".to_string()
                            }),
                            operator: BinaryOperator::Instanceof
                        })))
                    }]
                })),
                // var tripleEquals = x === 4;
                // var tripleEquals2 = "hello" === false;
                // var notTripleEquals = 4 !== x;
                // var notTripleEquals2 = false !== "hello";
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "doubleEquals".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Identifier(Identifier {
                                name: "x".to_string()
                            }),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 4.0
                            })),
                            operator: BinaryOperator::DoubleEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "doubleEquals2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::StringLiteral(StringLiteral {
                                value: "hello".to_string()
                            })),
                            right: Expression::Literal(Literal::BooleanLiteral(BooleanLiteral {
                                value: false
                            })),
                            operator: BinaryOperator::DoubleEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notDoubleEquals".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 4.0
                            })),
                            right: Expression::Identifier(Identifier {
                                name: "x".to_string()
                            }),
                            operator: BinaryOperator::NotDoubleEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notDoubleEquals2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::BooleanLiteral(BooleanLiteral {
                                value: false
                            })),
                            right: Expression::Literal(Literal::StringLiteral(StringLiteral {
                                value: "hello".to_string()
                            })),
                            operator: BinaryOperator::NotDoubleEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "tripleEquals".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Identifier(Identifier {
                                name: "x".to_string()
                            }),
                            right: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 4.0
                            })),
                            operator: BinaryOperator::TripleEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "tripleEquals2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::StringLiteral(StringLiteral {
                                value: "hello".to_string()
                            })),
                            right: Expression::Literal(Literal::BooleanLiteral(BooleanLiteral {
                                value: false
                            })),
                            operator: BinaryOperator::TripleEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notTripleEquals".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                                value: 4.0
                            })),
                            right: Expression::Identifier(Identifier {
                                name: "x".to_string()
                            }),
                            operator: BinaryOperator::NotTripleEqual
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notTripleEquals2".to_string(),
                        }),
                        init: Some(Expression::BinaryExpression(Box::new(BinaryExpression {
                            left: Expression::Literal(Literal::BooleanLiteral(BooleanLiteral {
                                value: false
                            })),
                            right: Expression::Literal(Literal::StringLiteral(StringLiteral {
                                value: "hello".to_string()
                            })),
                            operator: BinaryOperator::NotTripleEqual
                        })))
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

    #[test]
    fn parse_labeled_statements() {
        let input = r#"
            myLabel123: var a = 4;
            $label_456: var b = "hello";
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![
                Statement::LabeledStatement(Box::new(LabeledStatement {
                    label: Identifier {
                        name: "myLabel123".to_string()
                    },
                    body: Statement::Declaration(Declaration::VariableDeclaration(
                        VariableDeclaration {
                            kind: VariableDeclarationKind::Var,
                            declarations: vec![VariableDeclarator {
                                id: Pattern::Identifier(Identifier {
                                    name: "a".to_string()
                                }),
                                init: Some(Expression::Literal(Literal::NumberLiteral(
                                    NumberLiteral { value: 4.0 }
                                )))
                            }]
                        }
                    ))
                })),
                Statement::LabeledStatement(Box::new(LabeledStatement {
                    label: Identifier {
                        name: "$label_456".to_string()
                    },
                    body: Statement::Declaration(Declaration::VariableDeclaration(
                        VariableDeclaration {
                            kind: VariableDeclarationKind::Var,
                            declarations: vec![VariableDeclarator {
                                id: Pattern::Identifier(Identifier {
                                    name: "b".to_string()
                                }),
                                init: Some(Expression::Literal(Literal::StringLiteral(
                                    StringLiteral {
                                        value: "hello".to_string()
                                    }
                                )))
                            }]
                        }
                    ))
                }))
            ]
        );
    }
}
