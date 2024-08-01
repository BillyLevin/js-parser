mod token;

use std::str::Chars;

use self::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            input: source_code,
            chars: source_code.chars(),
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(ch) = self.chars.next() {
            let token = match ch {
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                '.' => {
                    let mut token = Token::Dot;
                    if let Some('.') = self.peek_char() {
                        self.chars.next();

                        if let Some('.') = self.peek_char() {
                            self.chars.next();
                            token = Token::DotDotDot
                        }
                    }
                    token
                }
                ';' => Token::Semicolon,
                ',' => Token::Comma,
                '<' => match self.peek_char() {
                    Some('=') => {
                        self.chars.next();
                        Token::LessThanEqual
                    }
                    Some('<') => {
                        self.chars.next();

                        match self.peek_char() {
                            Some('=') => {
                                self.chars.next();
                                Token::LeftShiftEqual
                            }
                            _ => Token::LeftShift,
                        }
                    }
                    _ => Token::LessThan,
                },
                '>' => match self.peek_char() {
                    Some('=') => {
                        self.chars.next();
                        Token::GreaterThanEqual
                    }
                    Some('>') => {
                        self.chars.next();

                        match self.peek_char() {
                            Some('>') => {
                                self.chars.next();

                                match self.peek_char() {
                                    Some('=') => Token::UnsignedRightShiftEqual,
                                    _ => Token::UnsignedRightShift,
                                }
                            }
                            Some('=') => {
                                self.chars.next();
                                Token::RightShiftEqual
                            }
                            _ => Token::RightShift,
                        }
                    }
                    _ => Token::GreaterThan,
                },
                '=' => match self.peek_char() {
                    Some('=') => {
                        self.chars.next();

                        match self.peek_char() {
                            Some('=') => {
                                self.chars.next();
                                Token::TripleEqual
                            }
                            _ => Token::DoubleEqual,
                        }
                    }
                    Some('>') => {
                        self.chars.next();
                        Token::Arrow
                    }
                    _ => Token::Equal,
                },
                '!' => {
                    let mut token = Token::Bang;

                    if let Some('=') = self.peek_char() {
                        self.chars.next();

                        if let Some('=') = self.peek_char() {
                            self.chars.next();
                            token = Token::NotTripleEqual;
                        } else {
                            token = Token::NotDoubleEqual;
                        }
                    }

                    token
                }
                '+' => match self.peek_char() {
                    Some('+') => {
                        self.chars.next();
                        Token::PlusPlus
                    }
                    Some('=') => {
                        self.chars.next();
                        Token::PlusEqual
                    }
                    _ => Token::Plus,
                },
                '-' => match self.peek_char() {
                    Some('-') => {
                        self.chars.next();
                        Token::MinusMinus
                    }
                    Some('=') => {
                        self.chars.next();
                        Token::MinusEqual
                    }
                    _ => Token::Minus,
                },
                '*' => match self.peek_char() {
                    Some('*') => {
                        self.chars.next();

                        match self.peek_char() {
                            Some('=') => {
                                self.chars.next();
                                Token::ExponentiationEqual
                            }
                            _ => Token::Exponentiation,
                        }
                    }
                    Some('=') => {
                        self.chars.next();
                        Token::MultiplyEqual
                    }
                    _ => Token::Multiply,
                },
                '/' => match self.peek_char() {
                    Some('=') => {
                        self.chars.next();
                        Token::DivisionEqual
                    }
                    _ => Token::Division,
                },

                '%' => match self.peek_char() {
                    Some('=') => {
                        self.chars.next();
                        Token::RemainderEqual
                    }
                    _ => Token::Percent,
                },
                '&' => match self.peek_char() {
                    Some('&') => {
                        self.chars.next();

                        match self.peek_char() {
                            Some('=') => {
                                self.chars.next();
                                Token::LogicalAndEqual
                            }
                            _ => Token::LogicalAnd,
                        }
                    }
                    Some('=') => {
                        self.chars.next();
                        Token::BitwiseAndEqual
                    }
                    _ => Token::BitwiseAnd,
                },
                '|' => match self.peek_char() {
                    Some('|') => {
                        self.chars.next();
                        match self.peek_char() {
                            Some('=') => {
                                self.chars.next();
                                Token::LogicalOrEqual
                            }
                            _ => Token::LogicalOr,
                        }
                    }
                    Some('=') => {
                        self.chars.next();
                        Token::BitwiseOrEqual
                    }
                    _ => Token::BitwiseOr,
                },
                '^' => match self.peek_char() {
                    Some('=') => {
                        self.chars.next();
                        Token::BitwiseXorEqual
                    }
                    _ => Token::BitwiseXor,
                },
                '~' => Token::BitwiseNot,
                '?' => match self.peek_char() {
                    Some('?') => {
                        self.chars.next();
                        match self.peek_char() {
                            Some('=') => {
                                self.chars.next();
                                Token::NullishCoalescingEqual
                            }
                            _ => Token::NullishCoalescing,
                        }
                    }
                    _ => Token::QuestionMark,
                },
                ':' => Token::Colon,

                _ => Token::Eof,
            };

            self.chars.next();

            token
        } else {
            Token::Eof
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn skip_whitespace(&mut self) {
        // TODO: this is probably not spec-compliant with all the unicode stuff, will fix later
        while let Some(' ' | '\t' | '\n' | '\r') = self.peek_char() {
            self.chars.next();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let input =
            "{ } ( ) [ ] . ... ; , < > <= >= = == ! != === !== + - * / % ** ++ -- << >> >>> / % & | ^ ~ ? : && || ?? += -= *= /= %= => **= <<= >>= >>>= &= |= ^= &&= ||= ??=";

        let mut lexer = Lexer::new(input);

        let expected: Vec<Token> = vec![
            Token::LeftBrace,
            Token::RightBrace,
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBracket,
            Token::RightBracket,
            Token::Dot,
            Token::DotDotDot,
            Token::Semicolon,
            Token::Comma,
            Token::LessThan,
            Token::GreaterThan,
            Token::LessThanEqual,
            Token::GreaterThanEqual,
            Token::Equal,
            Token::DoubleEqual,
            Token::Bang,
            Token::NotDoubleEqual,
            Token::TripleEqual,
            Token::NotTripleEqual,
            Token::Plus,
            Token::Minus,
            Token::Multiply,
            Token::Division,
            Token::Percent,
            Token::Exponentiation,
            Token::PlusPlus,
            Token::MinusMinus,
            Token::LeftShift,
            Token::RightShift,
            Token::UnsignedRightShift,
            Token::Division,
            Token::Percent,
            Token::BitwiseAnd,
            Token::BitwiseOr,
            Token::BitwiseXor,
            Token::BitwiseNot,
            Token::QuestionMark,
            Token::Colon,
            Token::LogicalAnd,
            Token::LogicalOr,
            Token::NullishCoalescing,
            Token::PlusEqual,
            Token::MinusEqual,
            Token::MultiplyEqual,
            Token::DivisionEqual,
            Token::RemainderEqual,
            Token::Arrow,
            Token::ExponentiationEqual,
            Token::LeftShiftEqual,
            Token::RightShiftEqual,
            Token::UnsignedRightShiftEqual,
            Token::BitwiseAndEqual,
            Token::BitwiseOrEqual,
            Token::BitwiseXorEqual,
            Token::LogicalAndEqual,
            Token::LogicalOrEqual,
            Token::NullishCoalescingEqual,
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
