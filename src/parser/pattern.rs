use crate::{
    ast::{ArrayPattern, Identifier, Pattern, RestElement},
    lexer::token::Token,
    parser::{ParseResult, Parser},
};

impl<'src> Parser<'src> {
    pub(super) fn parse_binding_pattern(&mut self) -> ParseResult<Pattern> {
        match &self.current_token {
            Token::LeftBracket => self.parse_array_pattern(),
            Token::Identifier(identifier) => {
                let pattern = Pattern::Identifier(Identifier {
                    name: identifier.to_string(),
                });
                self.next_token();
                Ok(pattern)
            }
            _ => todo!(),
        }
    }

    fn parse_array_pattern(&mut self) -> ParseResult<Pattern> {
        self.expect_current(Token::LeftBracket)?;

        let mut elements = Vec::new();

        loop {
            if matches!(self.current_token, Token::RightBracket) {
                break;
            }

            let mut is_rest = false;

            if matches!(self.current_token, Token::DotDotDot) {
                is_rest = true;
                self.next_token();
            }

            let binding_pattern = self.parse_binding_pattern()?;

            let element = if is_rest {
                Pattern::RestElement(Box::new(RestElement {
                    argument: binding_pattern,
                }))
            } else {
                binding_pattern
            };

            elements.push(Some(element));

            if matches!(self.current_token, Token::Comma) {
                self.next_token();
            } else {
                break;
            }
        }

        self.expect_current(Token::RightBracket)?;

        Ok(Pattern::ArrayPattern(Box::new(ArrayPattern { elements })))
    }
}

#[cfg(test)]
mod tests {
    use test_utils::{
        array_expr_element, array_spread_element, ident_expr, ident_pattern, literal_expr,
        rest_pattern,
    };

    use super::*;
    use crate::{ast::*, lexer::Lexer};

    #[test]
    fn array_pattern() {
        let input = r#"
            var [a, b, c] = thing;
            var [a, b, c, ...d] = [1, 2, 3, [4, 5]];
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
                        id: Pattern::ArrayPattern(Box::new(ArrayPattern {
                            elements: vec![
                                Some(ident_pattern!("a")),
                                Some(ident_pattern!("b")),
                                Some(ident_pattern!("c")),
                            ]
                        })),
                        init: Some(ident_expr!("thing"))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::ArrayPattern(Box::new(ArrayPattern {
                            elements: vec![
                                Some(ident_pattern!("a")),
                                Some(ident_pattern!("b")),
                                Some(ident_pattern!("c")),
                                Some(rest_pattern!(ident_pattern!("d"))),
                            ]
                        })),
                        init: Some(Expression::ArrayExpression(Box::new(ArrayExpression {
                            elements: vec![
                                array_expr_element!(literal_expr!(1)),
                                array_expr_element!(literal_expr!(2)),
                                array_expr_element!(literal_expr!(3)),
                                array_expr_element!(Expression::ArrayExpression(Box::new(
                                    ArrayExpression {
                                        elements: vec![
                                            array_expr_element!(literal_expr!(4)),
                                            array_expr_element!(literal_expr!(5)),
                                        ]
                                    }
                                ))),
                            ]
                        })))
                    }]
                }))
            ]
        );
    }
}
