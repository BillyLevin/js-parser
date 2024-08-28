use crate::{ast::Program, lexer::Lexer};

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    errors: Vec<&'static str>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        Self {
            lexer,
            errors: Vec::new(),
        }
    }

    fn parse_program(&mut self) -> Program {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

    #[test]
    fn parse_var_statements() {
        let input = r#"
            var x = 5;
            var y = "hello";
            var z = false;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![Statement::VariableStatement, Statement::VariableStatement]
        )
    }
}
