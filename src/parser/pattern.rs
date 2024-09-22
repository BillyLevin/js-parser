use crate::{
    ast::{ArrayPattern, Identifier, Pattern},
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

            elements.push(Some(self.parse_binding_pattern()?));

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
    use super::*;
    use crate::{
        ast::{
            ArrayPattern, Declaration, Expression, Identifier, Statement, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator,
        },
        lexer::Lexer,
    };

    #[test]
    fn array_pattern() {
        let input = r#"
            var [a, b, c] = thing;
            // var [a, b, c, ...d] = [1, 2, 3, [4, 5]];
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![Statement::Declaration(Declaration::VariableDeclaration(
                VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::ArrayPattern(Box::new(ArrayPattern {
                            elements: vec![
                                Some(Pattern::Identifier(Identifier {
                                    name: "a".to_string()
                                })),
                                Some(Pattern::Identifier(Identifier {
                                    name: "b".to_string()
                                })),
                                Some(Pattern::Identifier(Identifier {
                                    name: "c".to_string()
                                }))
                            ]
                        })),
                        init: Some(Expression::Identifier(Identifier {
                            name: "thing".to_string()
                        }))
                    }]
                }
            ))]
        );
    }
}
