use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Eof,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            input: source_code,
            chars: source_code.chars(),
        }
    }

    fn next_token(&mut self) -> Token {
        if let Some(ch) = self.chars.next() {
            match ch {
                _ => Token::Eof,
            }
        } else {
            Token::Eof
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let input = "const thing;";

        let mut lexer = Lexer::new(input);

        let expected: Vec<Token> = vec![
            // Token::Const,
            // Token::Identifier(String::from("thing")),
            // Token::Semicolon,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
