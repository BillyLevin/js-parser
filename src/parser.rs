use crate::{
    ast::{
        ArrayElement, ArrayExpression, AssignmentExpression, AssignmentOperator, BinaryExpression,
        BinaryOperator, BlockStatement, BooleanLiteral, BreakStatement, ContinueStatement,
        Declaration, Expression, Identifier, LabeledStatement, Literal, LogicalExpression,
        LogicalOperator, NumberLiteral, Operator, Pattern, Program, RegExp, RegExpLiteral,
        SpreadElement, Statement, StringLiteral, ThisExpression, UnaryExpression, UnaryOperator,
        UpdateExpression, UpdateOperator, VariableDeclaration, VariableDeclarationKind,
        VariableDeclarator,
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
            } else {
                break;
            }
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
        let mut lhs = self.parse_unary_expression()?;

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

            let operator = if self.current_token.is_logical_operator() {
                Operator::Logical(LogicalOperator::from(&self.current_token))
            } else if self.current_token.is_assignment_operator() {
                Operator::Assignment(AssignmentOperator::from(&self.current_token))
            } else {
                Operator::Binary(BinaryOperator::from(&self.current_token))
            };

            self.next_token();

            let rhs = self.parse_binary_expression(precedence)?;

            lhs = match operator {
                Operator::Binary(op) => Expression::BinaryExpression(Box::new(BinaryExpression {
                    left: lhs,
                    right: rhs,
                    operator: op,
                })),
                Operator::Logical(op) => {
                    Expression::LogicalExpression(Box::new(LogicalExpression {
                        left: lhs,
                        right: rhs,
                        operator: op,
                    }))
                }
                Operator::Assignment(op) => {
                    Expression::AssignmentExpression(Box::new(AssignmentExpression {
                        left: lhs,
                        right: rhs,
                        operator: op,
                    }))
                }
            };
        }

        Ok(lhs)
    }

    /// https://tc39.es/ecma262/#prod-UnaryExpression
    fn parse_unary_expression(&mut self) -> ParseResult<Expression> {
        if self.current_token.is_unary_operator() {
            let operator = UnaryOperator::from(&self.current_token);
            self.next_token();
            let argument = self.parse_unary_expression()?;
            Ok(Expression::UnaryExpression(Box::new(UnaryExpression {
                argument,
                operator,
                prefix: true,
            })))
        } else {
            self.parse_update_expression()
        }
    }

    /// https://tc39.es/ecma262/#prod-UpdateExpression
    fn parse_update_expression(&mut self) -> ParseResult<Expression> {
        if self.current_token.is_update_operator() {
            let operator = UpdateOperator::from(&self.current_token);
            self.next_token();
            let argument = self.parse_unary_expression()?;
            Ok(Expression::UpdateExpression(Box::new(UpdateExpression {
                argument,
                operator,
                prefix: true,
            })))
        } else {
            let lhs = self.parse_primary_expression()?;

            if self.current_token.is_update_operator() {
                let operator = UpdateOperator::from(&self.current_token);
                self.next_token();
                Ok(Expression::UpdateExpression(Box::new(UpdateExpression {
                    argument: lhs,
                    operator,
                    prefix: false,
                })))
            } else {
                Ok(lhs)
            }
        }
    }

    fn parse_primary_expression(&mut self) -> ParseResult<Expression> {
        match &self.current_token {
            Token::String(_) => self.parse_string_literal(),
            Token::Decimal(_)
            | Token::Binary(_)
            | Token::Octal(_)
            | Token::Hex(_)
            | Token::LegacyOctal(_) => self.parse_number_literal(),
            Token::True | Token::False => self.parse_boolean_literal(),
            Token::Null => self.parse_null_literal(),
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
            Token::This => self.parse_this_expression(),
            Token::LeftBracket => self.parse_array_expression(),
            _ => Err(()),
        }
    }

    fn parse_string_literal(&mut self) -> ParseResult<Expression> {
        let Token::String(ref value) = self.current_token else {
            return Err(());
        };

        let value = value.to_string();

        self.next_token();

        Ok(Expression::Literal(Literal::StringLiteral(StringLiteral {
            value,
        })))
    }

    fn parse_number_literal(&mut self) -> ParseResult<Expression> {
        let (Token::Decimal(value)
        | Token::Binary(value)
        | Token::Octal(value)
        | Token::Hex(value)
        | Token::LegacyOctal(value)) = self.current_token
        else {
            return Err(());
        };

        self.next_token();

        Ok(Expression::Literal(Literal::NumberLiteral(NumberLiteral {
            value,
        })))
    }

    fn parse_boolean_literal(&mut self) -> ParseResult<Expression> {
        let value = match self.current_token {
            Token::True => true,
            Token::False => false,
            _ => return Err(()),
        };

        self.next_token();

        Ok(Expression::Literal(Literal::BooleanLiteral(
            BooleanLiteral { value },
        )))
    }

    /// https://tc39.es/ecma262/#prod-ArrayLiteral
    fn parse_array_expression(&mut self) -> ParseResult<Expression> {
        self.expect_current(Token::LeftBracket)?;

        let mut elements = Vec::new();

        loop {
            if matches!(self.current_token, Token::RightBracket) {
                break;
            }

            let mut is_spread = false;

            if matches!(self.current_token, Token::DotDotDot) {
                is_spread = true;
                self.next_token();
            }

            let expr = self.parse_assignment_expression()?;

            let array_element = if is_spread {
                ArrayElement::SpreadElement(SpreadElement { argument: expr })
            } else {
                ArrayElement::Expression(expr)
            };

            elements.push(Some(array_element));

            if matches!(self.current_token, Token::Comma) {
                self.next_token();
            } else {
                break;
            }
        }

        self.expect_current(Token::RightBracket)?;

        Ok(Expression::ArrayExpression(Box::new(ArrayExpression {
            elements,
        })))
    }

    fn parse_identifier_expression(&mut self) -> ParseResult<Expression> {
        let Token::Identifier(identifier) = &self.current_token else {
            return Err(());
        };

        let name = identifier.to_string();

        self.next_token();

        Ok(Expression::Identifier(Identifier { name }))
    }

    fn parse_this_expression(&mut self) -> ParseResult<Expression> {
        self.expect_current(Token::This)?;

        Ok(Expression::ThisExpression(ThisExpression))
    }

    fn parse_null_literal(&mut self) -> ParseResult<Expression> {
        self.expect_current(Token::Null)?;

        Ok(Expression::Literal(Literal::NullLiteral))
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
            if matches!(self.current_token, Token::RightBrace | Token::Eof) {
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

        let Token::RegularExpression {
            ref body,
            ref flags,
        } = regex
        else {
            return Err(());
        };

        self.current_token = regex.clone();
        self.next_token();

        Ok(Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
            regex: RegExp {
                pattern: body.to_string(),
                flags: flags.clone(),
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
            ArrayElement, ArrayExpression, AssignmentExpression, AssignmentOperator,
            BinaryExpression, BinaryOperator, BlockStatement, BooleanLiteral, BreakStatement,
            ContinueStatement, Declaration, Expression, Identifier, LabeledStatement, Literal,
            LogicalExpression, LogicalOperator, NumberLiteral, Pattern, SpreadElement, Statement,
            StringLiteral, ThisExpression, UnaryExpression, UnaryOperator, UpdateExpression,
            UpdateOperator, VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
        },
        lexer::RegularExpressionFlags,
    };

    use super::*;

    macro_rules! binary_expr {
        ($left:expr, $right:expr, $op:ident) => {
            Expression::BinaryExpression(Box::new(BinaryExpression {
                left: $left,
                right: $right,
                operator: BinaryOperator::$op,
            }))
        };
    }

    macro_rules! logical_expr {
        ($left:expr, $right:expr, $op:ident) => {
            Expression::LogicalExpression(Box::new(LogicalExpression {
                left: $left,
                right: $right,
                operator: LogicalOperator::$op,
            }))
        };
    }

    macro_rules! assign_expr {
        ($left:expr, $right:expr, $op:ident) => {
            Expression::AssignmentExpression(Box::new(AssignmentExpression {
                left: $left,
                right: $right,
                operator: AssignmentOperator::$op,
            }))
        };
    }

    macro_rules! unary_expr {
        ($arg:expr, $op:ident) => {
            Expression::UnaryExpression(Box::new(UnaryExpression {
                argument: $arg,
                operator: UnaryOperator::$op,
                prefix: true,
            }))
        };
    }

    macro_rules! update_expr {
        ($arg:expr, $op:ident, $prefix:literal) => {
            Expression::UpdateExpression(Box::new(UpdateExpression {
                argument: $arg,
                operator: UpdateOperator::$op,
                prefix: $prefix,
            }))
        };
    }

    macro_rules! ident_expr {
        ($ident:expr) => {
            Expression::Identifier(Identifier {
                name: $ident.to_string(),
            })
        };
    }

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

    macro_rules! array_expr_element {
        ($expr:expr) => {
            Some(ArrayElement::Expression($expr))
        };
    }

    macro_rules! array_spread_element {
        ($expr:expr) => {
            Some(ArrayElement::SpreadElement(SpreadElement {
                argument: $expr,
            }))
        };
    }

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
            var bitwise = 45 ^ 3 & 23 | 14 + 7;
            var logical = true === false && 4 > 5 || 3;
            var logical2 = true || false > 4 && 5 === 3;
            var logical3 = thing ?? "fallback";
            var a = b = 4 > 5;
            var a = b *= c /= 3;
            var a = b -= c += d *= 4;
            var a = b %= c %= d;
            var a = b <<= c >>= d >>>= 27;
            var a = b |= c &= d ^= 34;
            var a = b **= c ||= d &&= e ??= "hello";
            var a = -b;
            var a = +b;
            var a = !b;
            var a = ~b;
            var a = typeof b;
            var a = void b;
            var a = delete b;
            var a = b + -c * ~d === typeof e;
            var a = !b / +c - -d > void e;
            var a = +b > -c && ~d <= !e;
            var a = ++b + --c * ++d;
            var a = b-- / c++ - d--;
            var a = typeof b++ >= ~--c % d--;
            var a = this;
            var arr = [];
            var arr = [
              1,
              2,
              3,
              true,
              "hello",
              6 * 7,
              ~22,
              thing / 4,
              false && "fallback",
              -7 % 3,
              2 ** 4,
              a++,
              --b,
              null,
              undefined
            ];
            var arr = [
              ...spread1,
              2,
              "hello",
              false,
              ...spread2,
              ...spread3,
              4 ** 7,
              ...[1, 2, ...["hello", false, 2 + ~2]]
            ];
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
                        init: Some(literal_expr!(5))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "y".to_string()
                        }),
                        init: Some(literal_expr!("hello"))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "z".to_string()
                        }),
                        init: Some(literal_expr!(false))
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
                        init: Some(literal_expr!(null))
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
                            init: Some(literal_expr!(4))
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
                            init: Some(literal_expr!("world"))
                        },
                        VariableDeclarator {
                            id: Pattern::Identifier(Identifier {
                                name: "bool".to_string()
                            }),
                            init: Some(literal_expr!(false))
                        }
                    ]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "sum".to_string()
                        }),
                        init: Some(binary_expr!(literal_expr!(4), literal_expr!(5), Plus))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "product".to_string()
                        }),
                        init: Some(binary_expr!(literal_expr!(27), literal_expr!(8), Multiply))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "precedence".to_string()
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(4),
                            binary_expr!(literal_expr!(27), literal_expr!(8), Multiply),
                            Plus
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "precedence2".to_string()
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(4), literal_expr!(27), Multiply),
                            literal_expr!(8),
                            Plus
                        )),
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lotsOfOperations".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(
                                binary_expr!(
                                    binary_expr!(
                                        binary_expr!(literal_expr!(4), literal_expr!(27), Multiply),
                                        literal_expr!(45),
                                        Divide
                                    ),
                                    literal_expr!(4),
                                    Plus
                                ),
                                binary_expr!(literal_expr!(7.5), literal_expr!(2), Multiply),
                                Minus
                            ),
                            binary_expr!(literal_expr!(67.45), literal_expr!(3), Divide),
                            Plus
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "exp".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(
                                literal_expr!(4),
                                binary_expr!(literal_expr!(3), literal_expr!(2), Exponentiation),
                                Exponentiation
                            ),
                            binary_expr!(literal_expr!(34), literal_expr!(4), Multiply),
                            Plus
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "exp2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(45),
                            binary_expr!(literal_expr!(7), literal_expr!(3), Exponentiation),
                            Multiply
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "remainder".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(45), literal_expr!(5), Remainder),
                            literal_expr!(3),
                            Plus
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "remainder2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(45),
                            binary_expr!(literal_expr!(5), literal_expr!(3), Remainder),
                            Plus
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "leftShift".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(45),
                            binary_expr!(literal_expr!(5), literal_expr!(3), Multiply),
                            LeftShift
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "leftShift2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(45), literal_expr!(5), Multiply),
                            literal_expr!(3),
                            LeftShift
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "rightShift".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(45),
                            binary_expr!(literal_expr!(5), literal_expr!(3), Multiply),
                            RightShift
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "rightShift2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(45), literal_expr!(5), Multiply),
                            literal_expr!(3),
                            RightShift
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "unsignedRightShift".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(45),
                            binary_expr!(literal_expr!(5), literal_expr!(3), Multiply),
                            UnsignedRightShift
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "unsignedRightShift2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(45), literal_expr!(5), Multiply),
                            literal_expr!(3),
                            UnsignedRightShift
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThan".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(34),
                            binary_expr!(literal_expr!(7), literal_expr!(2), Plus),
                            LessThan
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThan2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(34), literal_expr!(7), Plus),
                            literal_expr!(2),
                            LessThan
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThanEqual".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(34),
                            binary_expr!(literal_expr!(7), literal_expr!(2), Plus),
                            LessThanEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "lessThanEqual2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(34), literal_expr!(7), Plus),
                            literal_expr!(2),
                            LessThanEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThan".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(34),
                            binary_expr!(literal_expr!(7), literal_expr!(2), Plus),
                            GreaterThan
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThan2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(34), literal_expr!(7), Plus),
                            literal_expr!(2),
                            GreaterThan
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThanEqual".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(34),
                            binary_expr!(literal_expr!(7), literal_expr!(2), Plus),
                            GreaterThanEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "greaterThanEqual2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(literal_expr!(34), literal_expr!(7), Plus),
                            literal_expr!(2),
                            GreaterThanEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "inOperator".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!("property"),
                            ident_expr!("y"),
                            In
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "instanceofOperator".to_string(),
                        }),
                        init: Some(binary_expr!(ident_expr!("x"), ident_expr!("y"), Instanceof))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "doubleEquals".to_string(),
                        }),
                        init: Some(binary_expr!(
                            ident_expr!("x"),
                            literal_expr!(4),
                            DoubleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "doubleEquals2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!("hello"),
                            literal_expr!(false),
                            DoubleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notDoubleEquals".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(4),
                            ident_expr!("x"),
                            NotDoubleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notDoubleEquals2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(false),
                            literal_expr!("hello"),
                            NotDoubleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "tripleEquals".to_string(),
                        }),
                        init: Some(binary_expr!(
                            ident_expr!("x"),
                            literal_expr!(4),
                            TripleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "tripleEquals2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!("hello"),
                            literal_expr!(false),
                            TripleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notTripleEquals".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(4),
                            ident_expr!("x"),
                            NotTripleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "notTripleEquals2".to_string(),
                        }),
                        init: Some(binary_expr!(
                            literal_expr!(false),
                            literal_expr!("hello"),
                            NotTripleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "bitwise".to_string(),
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(
                                literal_expr!(45),
                                binary_expr!(literal_expr!(3), literal_expr!(23), BitwiseAnd),
                                BitwiseXor
                            ),
                            binary_expr!(literal_expr!(14), literal_expr!(7), Plus),
                            BitwiseOr
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "logical".to_string()
                        }),
                        init: Some(logical_expr!(
                            logical_expr!(
                                binary_expr!(
                                    literal_expr!(true),
                                    literal_expr!(false),
                                    TripleEqual
                                ),
                                binary_expr!(literal_expr!(4), literal_expr!(5), GreaterThan),
                                And
                            ),
                            literal_expr!(3),
                            Or
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "logical2".to_string()
                        }),
                        init: Some(logical_expr!(
                            literal_expr!(true),
                            logical_expr!(
                                binary_expr!(literal_expr!(false), literal_expr!(4), GreaterThan),
                                binary_expr!(literal_expr!(5), literal_expr!(3), TripleEqual),
                                And
                            ),
                            Or
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "logical3".to_string()
                        }),
                        init: Some(logical_expr!(
                            ident_expr!("thing"),
                            literal_expr!("fallback"),
                            NullishCoalescing
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(assign_expr!(
                            ident_expr!("b"),
                            binary_expr!(literal_expr!(4), literal_expr!(5), GreaterThan),
                            Assign
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(assign_expr!(
                            ident_expr!("b"),
                            assign_expr!(ident_expr!("c"), literal_expr!(3), Divide),
                            Multiply
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(assign_expr!(
                            ident_expr!("b"),
                            assign_expr!(
                                ident_expr!("c"),
                                assign_expr!(ident_expr!("d"), literal_expr!(4), Multiply),
                                Plus
                            ),
                            Minus
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(assign_expr!(
                            ident_expr!("b"),
                            assign_expr!(ident_expr!("c"), ident_expr!("d"), Remainder),
                            Remainder
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(assign_expr!(
                            ident_expr!("b"),
                            assign_expr!(
                                ident_expr!("c"),
                                assign_expr!(
                                    ident_expr!("d"),
                                    literal_expr!(27),
                                    UnsignedRightShift
                                ),
                                RightShift
                            ),
                            LeftShift
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(assign_expr!(
                            ident_expr!("b"),
                            assign_expr!(
                                ident_expr!("c"),
                                assign_expr!(ident_expr!("d"), literal_expr!(34), BitwiseXor),
                                BitwiseAnd
                            ),
                            BitwiseOr
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(assign_expr!(
                            ident_expr!("b"),
                            assign_expr!(
                                ident_expr!("c"),
                                assign_expr!(
                                    ident_expr!("d"),
                                    assign_expr!(
                                        ident_expr!("e"),
                                        literal_expr!("hello"),
                                        NullishCoalescing
                                    ),
                                    LogicalAnd
                                ),
                                LogicalOr
                            ),
                            Exponentiation
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(unary_expr!(ident_expr!("b"), Minus))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(unary_expr!(ident_expr!("b"), Plus))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(unary_expr!(ident_expr!("b"), LogicalNot))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(unary_expr!(ident_expr!("b"), BitwiseNot))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(unary_expr!(ident_expr!("b"), Typeof))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(unary_expr!(ident_expr!("b"), Void))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(unary_expr!(ident_expr!("b"), Delete))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(
                                ident_expr!("b"),
                                binary_expr!(
                                    unary_expr!(ident_expr!("c"), Minus),
                                    unary_expr!(ident_expr!("d"), BitwiseNot),
                                    Multiply
                                ),
                                Plus
                            ),
                            unary_expr!(ident_expr!("e"), Typeof),
                            TripleEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(
                                binary_expr!(
                                    unary_expr!(ident_expr!("b"), LogicalNot),
                                    unary_expr!(ident_expr!("c"), Plus),
                                    Divide
                                ),
                                unary_expr!(ident_expr!("d"), Minus),
                                Minus
                            ),
                            unary_expr!(ident_expr!("e"), Void),
                            GreaterThan
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(logical_expr!(
                            binary_expr!(
                                unary_expr!(ident_expr!("b"), Plus),
                                unary_expr!(ident_expr!("c"), Minus),
                                GreaterThan
                            ),
                            binary_expr!(
                                unary_expr!(ident_expr!("d"), BitwiseNot),
                                unary_expr!(ident_expr!("e"), LogicalNot),
                                LessThanEqual
                            ),
                            And
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(binary_expr!(
                            update_expr!(ident_expr!("b"), Increment, true),
                            binary_expr!(
                                update_expr!(ident_expr!("c"), Decrement, true),
                                update_expr!(ident_expr!("d"), Increment, true),
                                Multiply
                            ),
                            Plus
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(binary_expr!(
                            binary_expr!(
                                update_expr!(ident_expr!("b"), Decrement, false),
                                update_expr!(ident_expr!("c"), Increment, false),
                                Divide
                            ),
                            update_expr!(ident_expr!("d"), Decrement, false),
                            Minus
                        ))
                    }]
                })),
                // var a = typeof b++ >= ~--c % d--;
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(binary_expr!(
                            unary_expr!(update_expr!(ident_expr!("b"), Increment, false), Typeof),
                            binary_expr!(
                                unary_expr!(
                                    update_expr!(ident_expr!("c"), Decrement, true),
                                    BitwiseNot
                                ),
                                update_expr!(ident_expr!("d"), Decrement, false),
                                Remainder
                            ),
                            GreaterThanEqual
                        ))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "a".to_string()
                        }),
                        init: Some(Expression::ThisExpression(ThisExpression))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "arr".to_string()
                        }),
                        init: Some(Expression::ArrayExpression(Box::new(ArrayExpression {
                            elements: vec![]
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "arr".to_string()
                        }),
                        init: Some(Expression::ArrayExpression(Box::new(ArrayExpression {
                            elements: vec![
                                array_expr_element!(literal_expr!(1)),
                                array_expr_element!(literal_expr!(2)),
                                array_expr_element!(literal_expr!(3)),
                                array_expr_element!(literal_expr!(true)),
                                array_expr_element!(literal_expr!("hello")),
                                array_expr_element!(binary_expr!(
                                    literal_expr!(6),
                                    literal_expr!(7),
                                    Multiply
                                )),
                                array_expr_element!(unary_expr!(literal_expr!(22), BitwiseNot)),
                                array_expr_element!(binary_expr!(
                                    ident_expr!("thing"),
                                    literal_expr!(4),
                                    Divide
                                )),
                                array_expr_element!(logical_expr!(
                                    literal_expr!(false),
                                    literal_expr!("fallback"),
                                    And
                                )),
                                array_expr_element!(binary_expr!(
                                    unary_expr!(literal_expr!(7), Minus),
                                    literal_expr!(3),
                                    Remainder
                                )),
                                array_expr_element!(binary_expr!(
                                    literal_expr!(2),
                                    literal_expr!(4),
                                    Exponentiation
                                )),
                                array_expr_element!(update_expr!(
                                    ident_expr!("a"),
                                    Increment,
                                    false
                                )),
                                array_expr_element!(update_expr!(
                                    ident_expr!("b"),
                                    Decrement,
                                    true
                                )),
                                array_expr_element!(literal_expr!(null)),
                                array_expr_element!(ident_expr!("undefined")),
                            ]
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::Identifier(Identifier {
                            name: "arr".to_string()
                        }),
                        init: Some(Expression::ArrayExpression(Box::new(ArrayExpression {
                            elements: vec![
                                array_spread_element!(ident_expr!("spread1")),
                                array_expr_element!(literal_expr!(2)),
                                array_expr_element!(literal_expr!("hello")),
                                array_expr_element!(literal_expr!(false)),
                                array_spread_element!(ident_expr!("spread2")),
                                array_spread_element!(ident_expr!("spread3")),
                                array_expr_element!(binary_expr!(
                                    literal_expr!(4),
                                    literal_expr!(7),
                                    Exponentiation
                                )),
                                array_spread_element!(Expression::ArrayExpression(Box::new(
                                    ArrayExpression {
                                        elements: vec![
                                            array_expr_element!(literal_expr!(1)),
                                            array_expr_element!(literal_expr!(2)),
                                            array_spread_element!(Expression::ArrayExpression(
                                                Box::new(ArrayExpression {
                                                    elements: vec![
                                                        array_expr_element!(literal_expr!("hello")),
                                                        array_expr_element!(literal_expr!(false)),
                                                        array_expr_element!(binary_expr!(
                                                            literal_expr!(2),
                                                            unary_expr!(
                                                                literal_expr!(2),
                                                                BitwiseNot
                                                            ),
                                                            Plus
                                                        )),
                                                    ]
                                                })
                                            ))
                                        ]
                                    }
                                )))
                            ]
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
                        init: Some(literal_expr!("thing"))
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
                                    init: Some(literal_expr!("another thing"))
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
                                    init: Some(literal_expr!(54))
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
                                init: Some(literal_expr!(4))
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
                                init: Some(literal_expr!("hello"))
                            }]
                        }
                    ))
                }))
            ]
        );
    }
}
