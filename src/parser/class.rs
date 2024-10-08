use crate::{
    ast::{
        Class, ClassBody, ClassElement, Expression, Identifier, MethodDefinition,
        MethodDefinitionKind,
    },
    lexer::token::Token,
    parser::{function::FunctionKind, ParseResult, Parser},
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

        let body = self.parse_class_body()?;

        Ok(Expression::ClassExpression(Box::new(Class {
            id,
            super_class: None,
            body,
        })))
    }

    fn parse_class_body(&mut self) -> ParseResult<ClassBody> {
        self.expect_current(Token::LeftBrace)?;

        let mut body = Vec::new();

        loop {
            if matches!(self.current_token, Token::RightBrace | Token::Eof) {
                break;
            }

            body.push(self.parse_class_element()?);
        }

        self.expect_current(Token::RightBrace)?;

        Ok(ClassBody { body })
    }

    fn parse_class_element(&mut self) -> ParseResult<ClassElement> {
        let is_static = if matches!(self.current_token, Token::Static) {
            self.next_token();
            true
        } else {
            false
        };

        let mut kind = match self.current_token {
            Token::Get => {
                self.next_token();
                MethodDefinitionKind::Get
            }
            Token::Set => {
                self.next_token();
                MethodDefinitionKind::Set
            }
            _ => MethodDefinitionKind::Method,
        };

        if matches!(self.current_token, Token::LeftBrace) {
            todo!("parse static block")
        } else {
            let (element_name, is_computed) = self.parse_class_element_name()?;

            if let Expression::Identifier(Identifier { name }) = &element_name {
                if name == "constructor" {
                    kind = MethodDefinitionKind::Constructor;
                }
            }

            if matches!(self.current_token, Token::LeftParen) {
                let function = self.parse_function(FunctionKind::Expression)?;

                Ok(ClassElement::MethodDefinition(MethodDefinition {
                    key: element_name,
                    value: function,
                    kind,
                    computed: is_computed,
                    r#static: is_static,
                }))
            } else {
                Err(())
            }
        }
    }

    /// https://tc39.es/ecma262/#prod-ClassElementName
    fn parse_class_element_name(&mut self) -> ParseResult<(Expression, bool)> {
        match self.current_token {
            Token::PrivateIdentifier(_) => {
                let _identifier = self.parse_private_identifier()?;
                todo!("figure out best way to handle private elements/identifiers (also used in member and binary expressions)")
            }
            _ => self.parse_property_name(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::{
        assign_expr, binary_expr, ident, ident_expr, ident_pattern, literal_expr,
        static_member_expr,
    };

    use crate::{
        ast::{
            AssignmentExpression, AssignmentOperator, BinaryExpression, BinaryOperator,
            BlockStatement, Class, ClassBody, ClassElement, Declaration, ExpressionStatement,
            Function, Identifier, Literal, MemberExpression, MethodDefinition,
            MethodDefinitionKind, NumberLiteral, Pattern, ReturnStatement, Statement,
            StaticMemberExpression, StringLiteral, ThisExpression, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator,
        },
        lexer::Lexer,
    };

    #[test]
    fn class_expressions() {
        let input = r#"
            var emptyClass = class {};
            var emptyNamedClass = class NamedClass {};
            var testClass = class {
                constructor(value) {
                    this.value = value;
                }

                static get value() {
                    return this.value;
                }

                set value(num) {
                    this.value = num;
                }

                doCalculation() {
                    return this.value ** 4;
                }
            }
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
                            id: Some(ident!("NamedClass")),
                            super_class: None,
                            body: ClassBody { body: vec![] }
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: ident_pattern!("testClass"),
                        init: Some(Expression::ClassExpression(Box::new(Class {
                            id: None,
                            super_class: None,
                            body: ClassBody {
                                body: vec![
                                    ClassElement::MethodDefinition(MethodDefinition {
                                        key: ident_expr!("constructor"),
                                        value: Function {
                                            id: None,
                                            params: vec![ident_pattern!("value")],
                                            body: BlockStatement {
                                                body: vec![Statement::ExpressionStatement(
                                                    ExpressionStatement {
                                                        expression: assign_expr!(
                                                            static_member_expr!(
                                                                Expression::ThisExpression(
                                                                    ThisExpression
                                                                ),
                                                                ident!("value")
                                                            ),
                                                            ident_expr!("value"),
                                                            Assign
                                                        ),
                                                    }
                                                )]
                                            }
                                        },
                                        kind: MethodDefinitionKind::Constructor,
                                        computed: false,
                                        r#static: false,
                                    }),
                                    ClassElement::MethodDefinition(MethodDefinition {
                                        key: ident_expr!("value"),
                                        value: Function {
                                            id: None,
                                            params: vec![],
                                            body: BlockStatement {
                                                body: vec![Statement::ReturnStatement(
                                                    ReturnStatement {
                                                        argument: Some(static_member_expr!(
                                                            Expression::ThisExpression(
                                                                ThisExpression
                                                            ),
                                                            ident!("value")
                                                        ))
                                                    }
                                                )]
                                            }
                                        },
                                        kind: MethodDefinitionKind::Get,
                                        computed: false,
                                        r#static: true,
                                    }),
                                    ClassElement::MethodDefinition(MethodDefinition {
                                        key: ident_expr!("value"),
                                        value: Function {
                                            id: None,
                                            params: vec![ident_pattern!("num")],
                                            body: BlockStatement {
                                                body: vec![Statement::ExpressionStatement(
                                                    ExpressionStatement {
                                                        expression: assign_expr!(
                                                            static_member_expr!(
                                                                Expression::ThisExpression(
                                                                    ThisExpression
                                                                ),
                                                                ident!("value")
                                                            ),
                                                            ident_expr!("num"),
                                                            Assign
                                                        ),
                                                    }
                                                )]
                                            }
                                        },
                                        kind: MethodDefinitionKind::Set,
                                        computed: false,
                                        r#static: false,
                                    }),
                                    ClassElement::MethodDefinition(MethodDefinition {
                                        key: ident_expr!("doCalculation"),
                                        value: Function {
                                            id: None,
                                            params: vec![],
                                            body: BlockStatement {
                                                body: vec![Statement::ReturnStatement(
                                                    ReturnStatement {
                                                        argument: Some(binary_expr!(
                                                            static_member_expr!(
                                                                Expression::ThisExpression(
                                                                    ThisExpression
                                                                ),
                                                                ident!("value")
                                                            ),
                                                            literal_expr!(4),
                                                            Exponentiation
                                                        ))
                                                    }
                                                )]
                                            }
                                        },
                                        kind: MethodDefinitionKind::Method,
                                        computed: false,
                                        r#static: false,
                                    })
                                ]
                            }
                        })))
                    }]
                }))
            ]
        );
    }
}
