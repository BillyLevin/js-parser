use crate::{
    ast::{Class, ClassBody, Expression, Identifier},
    lexer::token::Token,
    parser::{ParseResult, Parser},
};

impl<'src> Parser<'src> {
    pub(super) fn parse_class_expression(&mut self) -> ParseResult<Expression> {
        self.expect_current(Token::Class)?;

        let id = match &self.current_token {
            Token::Identifier(identifier) => {
                let name = identifier.to_string();
                self.next_token();
                Some(Identifier { name })
            }
            _ => None,
        };

        self.expect_current(Token::LeftBrace)?;
        // TODO: class body
        self.expect_current(Token::RightBrace)?;

        Ok(Expression::ClassExpression(Box::new(Class {
            id,
            super_class: None,
            body: ClassBody { body: vec![] },
        })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::ident_pattern;

    use crate::{
        ast::{
            Class, ClassBody, Declaration, Identifier, Pattern, Statement, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator,
        },
        lexer::Lexer,
    };

    #[test]
    fn class_expressions() {
        let input = r#"
            var emptyClass = class {};
            var emptyNamedClass = class NamedClass {};
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
                        id: ident_pattern!("emptyClass"),
                        init: Some(Expression::ClassExpression(Box::new(Class {
                            id: None,
                            super_class: None,
                            body: ClassBody { body: vec![] }
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: ident_pattern!("emptyNamedClass"),
                        init: Some(Expression::ClassExpression(Box::new(Class {
                            id: Some(Identifier {
                                name: "NamedClass".to_string()
                            }),
                            super_class: None,
                            body: ClassBody { body: vec![] }
                        })))
                    }]
                }))
            ]
        );
    }
}
