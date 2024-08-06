mod token;

use self::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    current_position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            input: source_code,
            current_position: 0,
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(ch) = self.next_char() {
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
                        self.next_char();

                        if let Some('.') = self.peek_char() {
                            self.next_char();
                            token = Token::DotDotDot
                        }
                    }
                    token
                }
                ';' => Token::Semicolon,
                ',' => Token::Comma,
                '<' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::LessThanEqual
                    }
                    Some('<') => {
                        self.next_char();

                        match self.peek_char() {
                            Some('=') => {
                                self.next_char();
                                Token::LeftShiftEqual
                            }
                            _ => Token::LeftShift,
                        }
                    }
                    _ => Token::LessThan,
                },
                '>' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::GreaterThanEqual
                    }
                    Some('>') => {
                        self.next_char();

                        match self.peek_char() {
                            Some('>') => {
                                self.next_char();

                                match self.peek_char() {
                                    Some('=') => Token::UnsignedRightShiftEqual,
                                    _ => Token::UnsignedRightShift,
                                }
                            }
                            Some('=') => {
                                self.next_char();
                                Token::RightShiftEqual
                            }
                            _ => Token::RightShift,
                        }
                    }
                    _ => Token::GreaterThan,
                },
                '=' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();

                        match self.peek_char() {
                            Some('=') => {
                                self.next_char();
                                Token::TripleEqual
                            }
                            _ => Token::DoubleEqual,
                        }
                    }
                    Some('>') => {
                        self.next_char();
                        Token::Arrow
                    }
                    _ => Token::Equal,
                },
                '!' => {
                    let mut token = Token::Bang;

                    if let Some('=') = self.peek_char() {
                        self.next_char();

                        if let Some('=') = self.peek_char() {
                            self.next_char();
                            token = Token::NotTripleEqual;
                        } else {
                            token = Token::NotDoubleEqual;
                        }
                    }

                    token
                }
                '+' => match self.peek_char() {
                    Some('+') => {
                        self.next_char();
                        Token::PlusPlus
                    }
                    Some('=') => {
                        self.next_char();
                        Token::PlusEqual
                    }
                    _ => Token::Plus,
                },
                '-' => match self.peek_char() {
                    Some('-') => {
                        self.next_char();
                        Token::MinusMinus
                    }
                    Some('=') => {
                        self.next_char();
                        Token::MinusEqual
                    }
                    _ => Token::Minus,
                },
                '*' => match self.peek_char() {
                    Some('*') => {
                        self.next_char();

                        match self.peek_char() {
                            Some('=') => {
                                self.next_char();
                                Token::ExponentiationEqual
                            }
                            _ => Token::Exponentiation,
                        }
                    }
                    Some('=') => {
                        self.next_char();
                        Token::MultiplyEqual
                    }
                    _ => Token::Multiply,
                },
                '/' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::DivisionEqual
                    }
                    Some('/') => {
                        self.next_char();
                        return self.read_single_line_comment();
                    }
                    Some('*') => {
                        self.next_char();
                        return self.read_multi_line_comment();
                    }
                    _ => Token::Division,
                },

                '%' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::RemainderEqual
                    }
                    _ => Token::Percent,
                },
                '&' => match self.peek_char() {
                    Some('&') => {
                        self.next_char();

                        match self.peek_char() {
                            Some('=') => {
                                self.next_char();
                                Token::LogicalAndEqual
                            }
                            _ => Token::LogicalAnd,
                        }
                    }
                    Some('=') => {
                        self.next_char();
                        Token::BitwiseAndEqual
                    }
                    _ => Token::BitwiseAnd,
                },
                '|' => match self.peek_char() {
                    Some('|') => {
                        self.next_char();
                        match self.peek_char() {
                            Some('=') => {
                                self.next_char();
                                Token::LogicalOrEqual
                            }
                            _ => Token::LogicalOr,
                        }
                    }
                    Some('=') => {
                        self.next_char();
                        Token::BitwiseOrEqual
                    }
                    _ => Token::BitwiseOr,
                },
                '^' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::BitwiseXorEqual
                    }
                    _ => Token::BitwiseXor,
                },
                '~' => Token::BitwiseNot,
                '?' => match self.peek_char() {
                    Some('?') => {
                        self.next_char();
                        match self.peek_char() {
                            Some('=') => {
                                self.next_char();
                                Token::NullishCoalescingEqual
                            }
                            _ => Token::NullishCoalescing,
                        }
                    }
                    _ => Token::QuestionMark,
                },
                ':' => Token::Colon,
                // TODO: allow unicode start per spec https://tc39.es/ecma262/#prod-IdentifierStart
                'a'..='z' | 'A'..='Z' | '$' | '_' => return self.read_identifier(ch),
                '#' => match self.peek_char() {
                    Some('!') => {
                        if self.current_position == 1 {
                            self.read_hashbang_comment()
                        } else {
                            Token::Invalid
                        }
                    }
                    _ => return self.read_private_identifier(),
                },
                '\'' => return self.read_single_quote_string(),
                '"' => return self.read_double_quote_string(),
                '1'..='9' => return self.read_decimal(ch, true),
                '0' => return self.read_number_starting_with_zero(),
                _ => Token::Eof,
            };

            self.next_char();

            token
        } else {
            Token::Eof
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if self.current_position >= self.input.len() {
            None
        } else {
            self.current_position += 1;
            Some(self.input.as_bytes()[self.current_position - 1] as char)
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.current_position >= self.input.len() {
            None
        } else {
            Some(self.input.as_bytes()[self.current_position] as char)
        }
    }

    fn skip_whitespace(&mut self) {
        // TODO: this is probably not spec-compliant with all the unicode stuff, will fix later
        while let Some(' ' | '\t' | '\n' | '\r') = self.peek_char() {
            self.next_char();
        }
    }

    fn read_identifier(&mut self, start_char: char) -> Token {
        // TODO: this won't be performant. should just use start/end pointers and get the slice
        // from source code
        let mut identifier_name = String::new();
        identifier_name.push(start_char);

        while let Some(ch) = self.next_char() {
            // TODO: allow unicode continue per spec https://tc39.es/ecma262/#prod-IdentifierPartChar
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
            "let" => Token::Let,
            "static" => Token::Static,
            "implements" => Token::Implements,
            "interface" => Token::Interface,
            "package" => Token::Package,
            "private" => Token::Private,
            "protected" => Token::Protected,
            "public" => Token::Public,
            "as" => Token::As,
            "async" => Token::Async,
            "from" => Token::From,
            "get" => Token::Get,
            "meta" => Token::Meta,
            "of" => Token::Of,
            "set" => Token::Set,
            "target" => Token::Target,
            _ => Token::Identifier(identifier_name),
        }
    }

    fn read_private_identifier(&mut self) -> Token {
        // TODO: this won't be performant. should just use start/end pointers and get the slice
        // from source code
        let mut identifier_name = String::new();

        while let Some(ch) = self.next_char() {
            // TODO: allow unicode continue per spec https://tc39.es/ecma262/#prod-IdentifierPartChar
            if matches!(ch, 'a'..='z' | 'A'..='Z' | '$' | '_') {
                identifier_name.push(ch);
            } else {
                break;
            }
        }

        Token::PrivateIdentifier(identifier_name)
    }

    fn read_hashbang_comment(&mut self) -> Token {
        while let Some(ch) = self.peek_char() {
            if ch == '\n' || ch == '\r' {
                break;
            }

            self.next_char();
        }

        Token::HashbangComment
    }

    fn read_single_line_comment(&mut self) -> Token {
        while let Some(ch) = self.peek_char() {
            if ch == '\n' || ch == '\r' {
                break;
            }

            self.next_char();
        }

        Token::SingleLineComment
    }

    fn read_multi_line_comment(&mut self) -> Token {
        while let Some(ch) = self.peek_char() {
            self.next_char();

            if ch == '*' {
                if let Some('/') = self.peek_char() {
                    self.next_char();
                    break;
                }
            }
        }

        Token::MultiLineComment
    }

    fn read_single_quote_string(&mut self) -> Token {
        let mut string_literal = String::new();

        while let Some(ch) = self.next_char() {
            match ch {
                '\'' => break,
                _ => string_literal.push(ch),
            };
        }

        Token::String(string_literal)
    }

    fn read_double_quote_string(&mut self) -> Token {
        let mut string_literal = String::new();

        while let Some(ch) = self.next_char() {
            match ch {
                '"' => break,
                _ => string_literal.push(ch),
            };
        }

        Token::String(string_literal)
    }

    fn read_decimal(&mut self, start_char: char, underscores_allowed: bool) -> Token {
        let mut number_literal = String::new();
        number_literal.push(start_char);

        let mut has_decimal_point = false;

        let mut has_exponent = false;

        while let Some(ch) = self.next_char() {
            match ch {
                '0'..='9' => number_literal.push(ch),
                '.' => {
                    if has_decimal_point {
                        break;
                    } else {
                        has_decimal_point = true;
                        number_literal.push('.');
                    }
                }
                'e' | 'E' => {
                    has_exponent = true;
                    break;
                }
                '_' => {
                    if underscores_allowed {
                        continue;
                    } else {
                        break;
                    }
                }
                _ => break,
            };
        }

        if has_exponent {
            let mut exponent_literal = String::from("e");

            while let Some(ch) = self.next_char() {
                match ch {
                    '+' | '-' => {
                        if exponent_literal.len() == 1 {
                            exponent_literal.push(ch);
                        } else {
                            return Token::Invalid;
                        }
                    }
                    '0'..='9' => {
                        exponent_literal.push(ch);
                    }
                    '_' => continue,
                    _ => break,
                }
            }

            number_literal.push_str(&exponent_literal);
        }

        match number_literal.parse() {
            Ok(num) => Token::Decimal(num),
            Err(_) => Token::Invalid,
        }
    }

    fn read_number_starting_with_zero(&mut self) -> Token {
        match self.peek_char() {
            Some('b' | 'B') => {
                self.next_char();
                self.read_binary()
            }
            Some('o' | 'O') => {
                self.next_char();
                self.read_octal(true)
            }
            Some('x' | 'X') => {
                self.next_char();
                self.read_hex()
            }
            Some('0'..='9') => self.read_legacy_octal_or_decimal(),
            Some('.') => {
                self.next_char();
                self.read_after_decimal_point()
            }
            // from the spec: "The `SourceCharacter` immediately following a `NumericLiteral` must
            // not be an `IdentifierStart` or `DecimalDigit`"
            Some(ch) => {
                if matches!(ch, 'a'..='z' | 'A'..='Z' | '$' | '_') {
                    Token::Invalid
                } else {
                    Token::Decimal(0.0)
                }
            }
            None => Token::Decimal(0.0),
        }
    }

    fn read_binary(&mut self) -> Token {
        let mut numeric_value = 0u64;

        while let Some(ch) = self.next_char() {
            match ch {
                '0' => numeric_value *= 2,
                '1' => numeric_value = numeric_value * 2 + 1,
                '_' => continue,
                _ => break,
            }
        }

        Token::Binary(numeric_value)
    }

    fn read_octal(&mut self, underscores_allowed: bool) -> Token {
        let mut numeric_value = 0u64;

        while let Some(ch) = self.next_char() {
            match ch {
                '0'..='7' => numeric_value = numeric_value * 8 + (ch.to_digit(10).unwrap() as u64),
                '_' => {
                    if underscores_allowed {
                        continue;
                    } else {
                        break;
                    }
                }
                _ => break,
            };
        }

        Token::Octal(numeric_value)
    }

    fn read_hex(&mut self) -> Token {
        let mut numeric_value = 0u64;

        while let Some(ch) = self.next_char() {
            match ch {
                '0'..='9' => numeric_value = numeric_value * 16 + (ch.to_digit(10).unwrap() as u64),
                'a'..='f' => numeric_value = numeric_value * 16 + (ch as u64 - 'a' as u64 + 10),
                'A'..='F' => numeric_value = numeric_value * 16 + (ch as u64 - 'A' as u64 + 10),
                '_' => continue,
                _ => break,
            };
        }

        Token::Hex(numeric_value)
    }

    fn read_legacy_octal_or_decimal(&mut self) -> Token {
        let mut is_decimal = false;

        let start_position = self.current_position;

        while let Some(ch) = self.next_char() {
            match ch {
                '0'..='7' => (),
                '8' | '9' => is_decimal = true,
                _ => break,
            };
        }

        self.current_position = start_position;

        if is_decimal {
            let start_ch = self.next_char().unwrap();
            self.read_decimal(start_ch, false)
        } else {
            let result = self.read_octal(false);

            if let Token::Octal(value) = result {
                Token::LegacyOctal(value)
            } else {
                Token::Invalid
            }
        }
    }

    fn read_after_decimal_point(&mut self) -> Token {
        let mut has_exponent = false;
        let mut number_literal = String::from("0.");

        while let Some(ch) = self.next_char() {
            match ch {
                '0'..='9' => number_literal.push(ch),
                'e' | 'E' => {
                    has_exponent = true;
                    break;
                }
                '_' => continue,
                _ => break,
            };
        }

        if has_exponent {
            let mut exponent_literal = String::from("e");

            while let Some(ch) = self.next_char() {
                match ch {
                    '+' | '-' => {
                        if exponent_literal.len() == 1 {
                            exponent_literal.push(ch);
                        } else {
                            return Token::Invalid;
                        }
                    }
                    '0'..='9' => {
                        exponent_literal.push(ch);
                    }
                    '_' => continue,
                    _ => break,
                }
            }

            number_literal.push_str(&exponent_literal);
        }

        match number_literal.parse() {
            Ok(num) => Token::Decimal(num),
            Err(_) => Token::Invalid,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input =
            "{ } ( ) [ ] . ... ; , < > <= >= = == ! != === !== + - * / % ** ++ -- << >> >>> / % & | ^ ~ ? : && || ?? += -= *= /= %= => **= <<= >>= >>>= &= |= ^= &&= ||= ??= await break case catch class const continue debugger default delete do else enum export extends false finally for function if import in instanceof new null return super switch this throw true try typeof var void while with yield hello #thisisprivate hi // single line comment *
var
/*
hello
this is all ignored + * > <<
*/const
let static implements interface package private protected public
as async from get meta of set target
'this is a string 123'
\"this is another string 456\"
123 123.456 456_789 123e2 123e+2 123e-2 456E2 456E+2 456E-2
0b10101 0b10_11 0B10101 0B10_11
0o1234 0o5_67 0O12_34 0O567
0x456F3d 0x09abcF 0X45_6F3d 0x09ab_cF
01236745
089234.6
078
087e2
0
0.34
0.2_3e+2
";

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
            Token::PrivateIdentifier("thisisprivate".to_string()),
            Token::Identifier("hi".to_string()),
            Token::SingleLineComment,
            Token::Var,
            Token::MultiLineComment,
            Token::Const,
            Token::Let,
            Token::Static,
            Token::Implements,
            Token::Interface,
            Token::Package,
            Token::Private,
            Token::Protected,
            Token::Public,
            Token::As,
            Token::Async,
            Token::From,
            Token::Get,
            Token::Meta,
            Token::Of,
            Token::Set,
            Token::Target,
            Token::String("this is a string 123".to_string()),
            Token::String("this is another string 456".to_string()),
            Token::Decimal(123.0),
            Token::Decimal(123.456),
            Token::Decimal(456789.0),
            Token::Decimal(12300.0),
            Token::Decimal(12300.0),
            Token::Decimal(1.23),
            Token::Decimal(45600.0),
            Token::Decimal(45600.0),
            Token::Decimal(4.56),
            Token::Binary(21),
            Token::Binary(11),
            Token::Binary(21),
            Token::Binary(11),
            Token::Octal(668),
            Token::Octal(375),
            Token::Octal(668),
            Token::Octal(375),
            Token::Hex(4550461),
            Token::Hex(633807),
            Token::Hex(4550461),
            Token::Hex(633807),
            Token::LegacyOctal(343525),
            Token::Decimal(89234.6),
            Token::Decimal(78.0),
            Token::Decimal(8700.0),
            Token::Decimal(0.0),
            Token::Decimal(0.34),
            Token::Decimal(23.0),
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }

    #[test]
    fn test_valid_hashbang_comment() {
        let input = "#! this is a hashbang comment and nothing on this line becomes a token += * /
var { ]
";

        let mut lexer = Lexer::new(input);

        let expected: Vec<Token> = vec![
            Token::HashbangComment,
            Token::Var,
            Token::LeftBrace,
            Token::RightBracket,
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }

    #[test]
    fn test_invalid_hashbang_comment_leading_whitespace() {
        let input = "   #! oops";

        let mut lexer = Lexer::new(input);

        let expected: Vec<Token> = vec![
            Token::Invalid,
            Token::Identifier("oops".to_string()),
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }

    #[test]
    fn test_invalid_hashbang_comment_not_first_line() {
        let input = "var
#! oops";

        let mut lexer = Lexer::new(input);

        let expected: Vec<Token> = vec![
            Token::Var,
            Token::Invalid,
            Token::Identifier("oops".to_string()),
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
