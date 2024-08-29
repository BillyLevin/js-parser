use crate::{
    ast::{Program, Statement},
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

    fn parse_program(&mut self) -> Program {
        let mut program: Program = Program::new();

        loop {
            if self.current_token == Token::Eof {
                break;
            }

            let maybe_statement = match self.current_token {
                Token::Var => self.parse_variable_statement(),
                _ => None,
            };

            if let Some(statement) = maybe_statement {
                program.push_statement(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_variable_statement(&mut self) -> Option<Statement> {
        let Token::Identifier(identifier) = &self.peek_token else {
            self.errors
                .push(format!("expected identifier, got {}", self.peek_token));
            return None;
        };

        todo!()
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
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
            var a;
            var b, c
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            program.body,
            vec![
                Statement::VariableStatement,
                Statement::VariableStatement,
                Statement::VariableStatement,
                Statement::VariableStatement,
                Statement::VariableStatement,
                Statement::VariableStatement
            ]
        );
    }
}
