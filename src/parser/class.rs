use crate::{
    ast::{
        Class, ClassBody, ClassElement, Expression, Identifier, MethodDefinition,
        MethodDefinitionKind, PropertyDefinition,
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

        if matches!(self.current_token, Token::LeftBrace) && is_static {
            Ok(ClassElement::StaticBlock(self.parse_block_statement()?))
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
                let field_value = if self.current_token == Token::Equal {
                    self.next_token();
                    Some(self.parse_assignment_expression()?)
                } else {
                    None
                };

                self.eat_or_insert_semicolon();

                Ok(ClassElement::PropertyDefinition(PropertyDefinition {
                    key: element_name,
                    value: field_value,
                    computed: is_computed,
                    r#static: is_static,
                }))
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
            MethodDefinitionKind, NumberLiteral, Pattern, PropertyDefinition, ReturnStatement,
            Statement, StaticMemberExpression, StringLiteral, ThisExpression, VariableDeclaration,
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
                publicValue = "hello";
                static staticPublicValue = 20 * 4;
                emptyPublicValue;
                static emptyStaticPublicValue;

                ["computed" + "PublicValue"] = "something"
                static ["computed" + "StaticPublicValue"] = 4 % 7 / 3;
                ["computed" + "EmptyPublicValue"];
                static ["computed" + "EmptyStaticPublicValue"];

                static {
                    var a = 4;
                    this.value2 = a;
                    this.value3 = a ** 3;
                }

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
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: ident_expr!("publicValue"),
                                        value: Some(literal_expr!("hello")),
                                        computed: false,
                                        r#static: false,
                                    }),
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: ident_expr!("staticPublicValue"),
                                        value: Some(binary_expr!(
                                            literal_expr!(20),
                                            literal_expr!(4),
                                            Multiply
                                        )),
                                        computed: false,
                                        r#static: true,
                                    }),
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: ident_expr!("emptyPublicValue"),
                                        value: None,
                                        computed: false,
                                        r#static: false,
                                    }),
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: ident_expr!("emptyStaticPublicValue"),
                                        value: None,
                                        computed: false,
                                        r#static: true,
                                    }),
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: binary_expr!(
                                            literal_expr!("computed"),
                                            literal_expr!("PublicValue"),
                                            Plus
                                        ),
                                        value: Some(literal_expr!("something")),
                                        computed: true,
                                        r#static: false,
                                    }),
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: binary_expr!(
                                            literal_expr!("computed"),
                                            literal_expr!("StaticPublicValue"),
                                            Plus
                                        ),
                                        value: Some(binary_expr!(
                                            binary_expr!(
                                                literal_expr!(4),
                                                literal_expr!(7),
                                                Remainder
                                            ),
                                            literal_expr!(3),
                                            Divide
                                        )),
                                        computed: true,
                                        r#static: true,
                                    }),
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: binary_expr!(
                                            literal_expr!("computed"),
                                            literal_expr!("EmptyPublicValue"),
                                            Plus
                                        ),
                                        value: None,
                                        computed: true,
                                        r#static: false,
                                    }),
                                    ClassElement::PropertyDefinition(PropertyDefinition {
                                        key: binary_expr!(
                                            literal_expr!("computed"),
                                            literal_expr!("EmptyStaticPublicValue"),
                                            Plus
                                        ),
                                        value: None,
                                        computed: true,
                                        r#static: true,
                                    }),
                                    ClassElement::StaticBlock(BlockStatement {
                                        body: vec![
                                            Statement::Declaration(
                                                Declaration::VariableDeclaration(
                                                    VariableDeclaration {
                                                        kind: VariableDeclarationKind::Var,
                                                        declarations: vec![VariableDeclarator {
                                                            id: ident_pattern!("a"),
                                                            init: Some(literal_expr!(4))
                                                        }]
                                                    }
                                                ),
                                            ),
                                            Statement::ExpressionStatement(ExpressionStatement {
                                                expression: assign_expr!(
                                                    static_member_expr!(
                                                        Expression::ThisExpression(ThisExpression),
                                                        ident!("value2")
                                                    ),
                                                    ident_expr!("a"),
                                                    Assign
                                                )
                                            }),
                                            Statement::ExpressionStatement(ExpressionStatement {
                                                expression: assign_expr!(
                                                    static_member_expr!(
                                                        Expression::ThisExpression(ThisExpression),
                                                        ident!("value3")
                                                    ),
                                                    binary_expr!(
                                                        ident_expr!("a"),
                                                        literal_expr!(3),
                                                        Exponentiation
                                                    ),
                                                    Assign
                                                )
                                            })
                                        ]
                                    }),
                                    ClassElement::MethodDefinition(MethodDefinition {
                                        key: ident_expr!("constructor"),
                                        value: Function {
                                            id: None,
                                            params: vec![ident_pattern!("value")],
                                            generator: false,
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
                                            generator: false,
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
                                            generator: false,
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
                                            generator: false,
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
