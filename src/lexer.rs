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
                'a'..='z' | 'A'..='Z' | '$' | '_' => return self.read_identifier(ch),
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

    fn read_identifier(&mut self, start_char: char) -> Token {
        // TODO: this won't be performant. should just use start/end pointers and get the slice
        // from source code
        let mut identifier_name = String::new();
        identifier_name.push(start_char);

        for ch in self.chars.by_ref() {
            if matches!(ch, 'a'..='z' | 'A'..='Z' | '$' | '_') {
                identifier_name.push(ch);
            } else {
                break;
            }
        }

        match identifier_name.as_str() {
            "await" => Token::Await,
            "break" => Token::Break,
            "case" => Token::Case,
            "catch" => Token::Catch,
            "class" => Token::Class,
            "const" => Token::Const,
            "continue" => Token::Continue,
            "debugger" => Token::Debugger,
            "default" => Token::Default,
            "delete" => Token::Delete,
            "do" => Token::Do,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "export" => Token::Export,
            "extends" => Token::Extends,
            "false" => Token::False,
            "finally" => Token::Finally,
            "for" => Token::For,
            "function" => Token::Function,
            "if" => Token::If,
            "import" => Token::Import,
            "in" => Token::In,
            "instanceof" => Token::Instanceof,
            "new" => Token::New,
            "null" => Token::Null,
            "return" => Token::Return,
            "super" => Token::Super,
            "switch" => Token::Switch,
            "this" => Token::This,
            "throw" => Token::Throw,
            "true" => Token::True,
            "try" => Token::Try,
            "typeof" => Token::Typeof,
            "var" => Token::Var,
            "void" => Token::Void,
            "while" => Token::While,
            "with" => Token::With,
            "yield" => Token::Yield,
            _ => Token::Identifier(identifier_name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input =
            "{ } ( ) [ ] . ... ; , < > <= >= = == ! != === !== + - * / % ** ++ -- << >> >>> / % & | ^ ~ ? : && || ?? += -= *= /= %= => **= <<= >>= >>>= &= |= ^= &&= ||= ??= await break case catch class const continue debugger default delete do else enum export extends false finally for function if import in instanceof new null return super switch this throw true try typeof var void while with yield hello";

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
            Token::Await,
            Token::Break,
            Token::Case,
            Token::Catch,
            Token::Class,
            Token::Const,
            Token::Continue,
            Token::Debugger,
            Token::Default,
            Token::Delete,
            Token::Do,
            Token::Else,
            Token::Enum,
            Token::Export,
            Token::Extends,
            Token::False,
            Token::Finally,
            Token::For,
            Token::Function,
            Token::If,
            Token::Import,
            Token::In,
            Token::Instanceof,
            Token::New,
            Token::Null,
            Token::Return,
            Token::Super,
            Token::Switch,
            Token::This,
            Token::Throw,
            Token::True,
            Token::Try,
            Token::Typeof,
            Token::Var,
            Token::Void,
            Token::While,
            Token::With,
            Token::Yield,
            Token::Identifier("hello".to_string()),
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
