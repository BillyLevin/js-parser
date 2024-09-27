use crate::{
    ast::{
        ArrayPattern, AssignmentProperty, Expression, Identifier, ObjectPattern,
        ObjectPatternProperty, Pattern, RestElement,
    },
    lexer::token::Token,
    parser::{ParseResult, Parser},
};

impl<'src> Parser<'src> {
    pub(super) fn parse_binding_pattern(&mut self) -> ParseResult<Pattern> {
        match &self.current_token {
            Token::LeftBracket => self.parse_array_pattern(),
            Token::LeftBrace => self.parse_object_pattern(),
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

    /// https://tc39.es/ecma262/#prod-ArrayBindingPattern
    fn parse_array_pattern(&mut self) -> ParseResult<Pattern> {
        self.expect_current(Token::LeftBracket)?;

        let mut elements = Vec::new();

        loop {
            if matches!(self.current_token, Token::RightBracket) {
                break;
            }

            if matches!(self.current_token, Token::Comma) {
                elements.push(None);
                self.next_token();
                continue;
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

    /// https://tc39.es/ecma262/#prod-ObjectBindingPattern
    fn parse_object_pattern(&mut self) -> ParseResult<Pattern> {
        self.expect_current(Token::LeftBrace)?;

        let mut properties = Vec::new();

        loop {
            if matches!(self.current_token, Token::RightBrace) {
                break;
            }

            let is_rest = matches!(self.current_token, Token::DotDotDot);

            let property = if is_rest {
                self.parse_rest_element()?
            } else {
                let (key, computed) = self.parse_object_property_name()?;
                let is_shorthand = !computed && !matches!(self.current_token, Token::Colon);

                if is_shorthand {
                    let Expression::Identifier(Identifier { name }) = &key else {
                        return Err(());
                    };

                    let value = Pattern::Identifier(Identifier {
                        name: name.to_string(),
                    });

                    ObjectPatternProperty::Property(AssignmentProperty {
                        key,
                        value,
                        shorthand: true,
                        computed,
                    })
                } else {
                    self.expect_current(Token::Colon)?;
                    let value = self.parse_binding_pattern()?;
                    ObjectPatternProperty::Property(AssignmentProperty {
                        key,
                        value,
                        shorthand: false,
                        computed,
                    })
                }
            };

            properties.push(property);

            if matches!(self.current_token, Token::Comma) {
                self.next_token();
            } else {
                break;
            }
        }

        self.expect_current(Token::RightBrace)?;

        Ok(Pattern::ObjectPattern(Box::new(ObjectPattern {
            properties,
        })))
    }

    fn parse_rest_element(&mut self) -> ParseResult<ObjectPatternProperty> {
        self.expect_current(Token::DotDotDot)?;

        let argument = self.parse_binding_pattern()?;

        if !matches!(argument, Pattern::Identifier(_)) {
            return Err(());
        }

        Ok(ObjectPatternProperty::RestElement(RestElement { argument }))
    }
}

#[cfg(test)]
mod tests {
    use test_utils::{array_expr_element, ident_expr, ident_pattern, literal_expr, rest_pattern};

    use super::*;
    use crate::{ast::*, lexer::Lexer};

    #[test]
    fn array_pattern() {
        let input = r#"
            var [a, b, c] = thing;
            var [a, b, c, ...d] = [1, 2, 3, [4, 5]];
            var [a, b, c, [d, e]] = [1, 2, 3, [4, 5]];
            var [a, b, c, [d, e], ...f] = [1, 2, 3, [4, 5], []];
            var [a,,,b] = [1, 2, 3, 4];
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
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::ArrayPattern(Box::new(ArrayPattern {
                            elements: vec![
                                Some(ident_pattern!("a")),
                                Some(ident_pattern!("b")),
                                Some(ident_pattern!("c")),
                                Some(Pattern::ArrayPattern(Box::new(ArrayPattern {
                                    elements: vec![
                                        Some(ident_pattern!("d")),
                                        Some(ident_pattern!("e"))
                                    ]
                                }))),
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
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::ArrayPattern(Box::new(ArrayPattern {
                            elements: vec![
                                Some(ident_pattern!("a")),
                                Some(ident_pattern!("b")),
                                Some(ident_pattern!("c")),
                                Some(Pattern::ArrayPattern(Box::new(ArrayPattern {
                                    elements: vec![
                                        Some(ident_pattern!("d")),
                                        Some(ident_pattern!("e"))
                                    ]
                                }))),
                                Some(rest_pattern!(ident_pattern!("f")))
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
                                array_expr_element!(Expression::ArrayExpression(Box::new(
                                    ArrayExpression { elements: vec![] }
                                ))),
                            ]
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::ArrayPattern(Box::new(ArrayPattern {
                            elements: vec![
                                Some(ident_pattern!("a")),
                                None,
                                None,
                                Some(ident_pattern!("b")),
                            ]
                        })),
                        init: Some(Expression::ArrayExpression(Box::new(ArrayExpression {
                            elements: vec![
                                array_expr_element!(literal_expr!(1)),
                                array_expr_element!(literal_expr!(2)),
                                array_expr_element!(literal_expr!(3)),
                                array_expr_element!(literal_expr!(4)),
                            ]
                        })))
                    }]
                })),
            ]
        );
    }

    #[test]
    fn object_pattern() {
        let input = r#"
            var { a, b, c: renamedC } = thing;
            var {
              a,
              b: renamedB,
              c,
              d: { e: renamedE, f },
              ...gAndH
            } = { a: 1, b: 2, c: 3, d: { e: 4, f: 5 }, g: 6, h: 7 };
            var { a, b: renamedB, ["computed-thing"]: computedThing, ...rest } = thing;
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
                        id: Pattern::ObjectPattern(Box::new(ObjectPattern {
                            properties: vec![
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("a"),
                                    value: ident_pattern!("a"),
                                    shorthand: true,
                                    computed: false
                                }),
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("b"),
                                    value: ident_pattern!("b"),
                                    shorthand: true,
                                    computed: false
                                }),
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("c"),
                                    value: ident_pattern!("renamedC"),
                                    shorthand: false,
                                    computed: false
                                })
                            ]
                        })),
                        init: Some(ident_expr!("thing"))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::ObjectPattern(Box::new(ObjectPattern {
                            properties: vec![
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("a"),
                                    value: ident_pattern!("a"),
                                    shorthand: true,
                                    computed: false
                                }),
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("b"),
                                    value: ident_pattern!("renamedB"),
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("c"),
                                    value: ident_pattern!("c"),
                                    shorthand: true,
                                    computed: false
                                }),
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("d"),
                                    value: Pattern::ObjectPattern(Box::new(ObjectPattern {
                                        properties: vec![
                                            ObjectPatternProperty::Property(AssignmentProperty {
                                                key: ident_expr!("e"),
                                                value: ident_pattern!("renamedE"),
                                                shorthand: false,
                                                computed: false,
                                            }),
                                            ObjectPatternProperty::Property(AssignmentProperty {
                                                key: ident_expr!("f"),
                                                value: ident_pattern!("f"),
                                                shorthand: true,
                                                computed: false,
                                            })
                                        ]
                                    })),
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectPatternProperty::RestElement(RestElement {
                                    argument: ident_pattern!("gAndH")
                                })
                            ]
                        })),
                        init: Some(Expression::ObjectExpression(Box::new(ObjectExpression {
                            properties: vec![
                                ObjectProperty::Property(Property {
                                    key: ident_expr!("a"),
                                    value: literal_expr!(1),
                                    kind: PropertyKind::Init,
                                    method: false,
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectProperty::Property(Property {
                                    key: ident_expr!("b"),
                                    value: literal_expr!(2),
                                    kind: PropertyKind::Init,
                                    method: false,
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectProperty::Property(Property {
                                    key: ident_expr!("c"),
                                    value: literal_expr!(3),
                                    kind: PropertyKind::Init,
                                    method: false,
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectProperty::Property(Property {
                                    key: ident_expr!("d"),
                                    value: Expression::ObjectExpression(Box::new(
                                        ObjectExpression {
                                            properties: vec![
                                                ObjectProperty::Property(Property {
                                                    key: ident_expr!("e"),
                                                    value: literal_expr!(4),
                                                    kind: PropertyKind::Init,
                                                    method: false,
                                                    shorthand: false,
                                                    computed: false,
                                                }),
                                                ObjectProperty::Property(Property {
                                                    key: ident_expr!("f"),
                                                    value: literal_expr!(5),
                                                    kind: PropertyKind::Init,
                                                    method: false,
                                                    shorthand: false,
                                                    computed: false,
                                                })
                                            ]
                                        }
                                    )),
                                    kind: PropertyKind::Init,
                                    method: false,
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectProperty::Property(Property {
                                    key: ident_expr!("g"),
                                    value: literal_expr!(6),
                                    kind: PropertyKind::Init,
                                    method: false,
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectProperty::Property(Property {
                                    key: ident_expr!("h"),
                                    value: literal_expr!(7),
                                    kind: PropertyKind::Init,
                                    method: false,
                                    shorthand: false,
                                    computed: false
                                }),
                            ]
                        })))
                    }]
                })),
                Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    declarations: vec![VariableDeclarator {
                        id: Pattern::ObjectPattern(Box::new(ObjectPattern {
                            properties: vec![
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("a"),
                                    value: ident_pattern!("a"),
                                    shorthand: true,
                                    computed: false
                                }),
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: ident_expr!("b"),
                                    value: ident_pattern!("renamedB"),
                                    shorthand: false,
                                    computed: false
                                }),
                                ObjectPatternProperty::Property(AssignmentProperty {
                                    key: literal_expr!("computed-thing"),
                                    value: ident_pattern!("computedThing"),
                                    shorthand: false,
                                    computed: true
                                }),
                                ObjectPatternProperty::RestElement(RestElement {
                                    argument: ident_pattern!("rest")
                                })
                            ]
                        })),
                        init: Some(ident_expr!("thing"))
                    }]
                })),
            ]
        );
    }
}
