mod token;
mod whitespace;

use crate::lexer::whitespace::is_line_terminator;

use self::{
    token::Token,
    whitespace::{CR, FF, LF, TAB, VT},
};

pub struct Lexer<'a> {
    input: &'a str,
    read_position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            input: source_code,
            read_position: 1,
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(ch) = self.current_char() {
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
                                    Some('=') => {
                                        self.next_char();
                                        Token::UnsignedRightShiftEqual
                                    }
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
                        if self.read_position == 1 {
                            self.next_char();
                            self.read_hashbang_comment()
                        } else {
                            self.next_char();
                            Token::Invalid
                        }
                    }
                    _ => return self.read_private_identifier(),
                },
                '\'' => return self.read_single_quote_string(),
                '"' => return self.read_double_quote_string(),
                '1'..='9' => return self.read_decimal(ch, true),
                '0' => return self.read_number_starting_with_zero(),
                '`' => return self.read_template(),
                _ => Token::Eof,
            };

            self.next_char();

            token
        } else {
            Token::Eof
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.read_position += 1;
            Some(self.input.as_bytes()[self.read_position - 1] as char)
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.as_bytes()[self.read_position] as char)
        }
    }

    fn current_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.as_bytes()[self.read_position - 1] as char)
        }
    }

    fn skip_whitespace(&mut self) {
        // TODO: this is probably not spec-compliant with all the unicode stuff, will fix later
        while let Some(' ' | '\t' | '\n' | '\r') = self.current_char() {
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
            if matches!(ch, 'a'..='z' | 'A'..='Z' | '$' | '_' | '0'..='9') {
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
        while let Some(ch) = self.current_char() {
            if ch == '\n' || ch == '\r' {
                break;
            }

            self.next_char();
        }

        Token::SingleLineComment
    }

    fn read_multi_line_comment(&mut self) -> Token {
        while let Some(ch) = self.current_char() {
            self.next_char();

            if ch == '*' {
                if let Some('/') = self.current_char() {
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
            if is_line_terminator(ch) {
                return Token::Invalid;
            }

            match ch {
                '\'' => {
                    self.next_char();
                    return Token::String(string_literal);
                }
                '\\' => {
                    let escape_sequence = match self.read_escape_sequence() {
                        Ok(s) => s,
                        Err(_) => return Token::Invalid,
                    };

                    string_literal.push_str(&escape_sequence);
                }
                _ => string_literal.push(ch),
            };
        }

        // string was never closed
        Token::Invalid
    }

    fn read_double_quote_string(&mut self) -> Token {
        let mut string_literal = String::new();

        while let Some(ch) = self.next_char() {
            if is_line_terminator(ch) {
                return Token::Invalid;
            }

            match ch {
                '"' => {
                    self.next_char();
                    return Token::String(string_literal);
                }
                '\\' => {
                    let escape_sequence = match self.read_escape_sequence() {
                        Ok(s) => s,
                        Err(_) => return Token::Invalid,
                    };

                    string_literal.push_str(&escape_sequence);
                }
                _ => string_literal.push(ch),
            };
        }

        // string was never closed
        Token::Invalid
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
        self.next_char();

        match self.current_char() {
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

        while let Some(ch) = self.current_char() {
            self.next_char();

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

        while let Some(ch) = self.current_char() {
            self.next_char();

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

        while let Some(ch) = self.current_char() {
            self.next_char();

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

        let start_position = self.read_position;

        while let Some(ch) = self.current_char() {
            self.next_char();

            match ch {
                '0'..='7' => (),
                '8' | '9' => is_decimal = true,
                _ => break,
            };
        }

        self.read_position = start_position;

        if is_decimal {
            let start_ch = self.current_char().unwrap();
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

        while let Some(ch) = self.current_char() {
            self.next_char();

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

    fn read_regex(&mut self, current_token: Token) -> Token {
        // the start token already gets read, so we need to backtrack to the beginning of
        // the regular expression
        let start_offset = match current_token {
            Token::Division => 1,
            Token::DivisionEqual => 2,
            _ => unreachable!(),
        };

        self.read_position -= start_offset;
        debug_assert!(self.read_position > 0);

        let mut body = String::new();

        let mut is_escaping = false;
        let mut is_in_class = false;

        loop {
            match self.next_char() {
                Some(ch) => {
                    if is_line_terminator(ch) {
                        return Token::Invalid;
                    }

                    if is_escaping {
                        is_escaping = false;
                        body.push(ch);
                        continue;
                    }

                    // unescaped forward slash is allowed in regex class, for example `/[hel/lo]/`
                    // is a valid regular expression in JS
                    if ch == '/' && !is_in_class {
                        break;
                    }

                    body.push(ch);

                    match ch {
                        '\\' => is_escaping = true,
                        '[' => is_in_class = true,
                        ']' => is_in_class = false,
                        _ => (),
                    };
                }
                None => return Token::Invalid,
            };
        }

        let mut flags = RegularExpressionFlags::default();

        while let Some(ch) = self.peek_char() {
            // TODO: allow unicode continue per spec https://tc39.es/ecma262/#prod-IdentifierPartChar
            if !matches!(ch, 'a'..='z' | 'A'..='Z' | '$' | '_' | '0'..='9') {
                break;
            }

            self.next_char();

            if let Ok(flag) = RegularExpressionFlag::try_from(ch) {
                flags.add_flag(flag);
            } else {
                break;
            }
        }

        Token::RegularExpression { body, flags }
    }

    /// https://tc39.es/ecma262/#prod-EscapeSequence
    fn read_escape_sequence(&mut self) -> Result<String, ()> {
        let mut escape_sequence = String::new();

        match self.next_char() {
            Some(ch) => match ch {
                // https://tc39.es/ecma262/#table-string-single-character-escape-sequences
                '\'' => {
                    escape_sequence.push('\'');
                }
                '"' => {
                    escape_sequence.push('"');
                }
                '\\' => {
                    escape_sequence.push('\\');
                }
                'b' => {
                    // backspace (see table at above link)
                    escape_sequence.push('\u{0008}');
                }
                'f' => {
                    escape_sequence.push(FF);
                }
                'n' => {
                    escape_sequence.push(LF);
                }
                'r' => {
                    escape_sequence.push(CR);
                }
                't' => {
                    escape_sequence.push(TAB);
                }
                'v' => {
                    escape_sequence.push(VT);
                }
                // TODO: there is probably a less weird syntax for writing this, but don't want to
                // match on '0' directly because other productions also begin with '0'
                // 0 [lookahead ∉ DecimalDigit]
                '0' if !matches!(self.peek_char(), Some(lookahead) if lookahead.is_ascii_digit()) => {
                    escape_sequence.push('\0')
                }
                '0'..='7' => {
                    let mut sequence_string = String::from(ch);

                    match ch {
                        // ZeroToThree OctalDigit [lookahead ∉ OctalDigit]
                        // ZeroToThree OctalDigit OctalDigit
                        '0'..='3' => {
                            if let Some(next_ch @ '0'..='7') = self.peek_char() {
                                sequence_string.push(next_ch);
                                self.next_char();

                                if let Some(lookahead @ '0'..='7') = self.peek_char() {
                                    sequence_string.push(lookahead);
                                    self.next_char();
                                }
                            }
                        }
                        // FourToSeven OctalDigit
                        '4'..='7' => {
                            if let Some(next_ch @ '0'..='7') = self.peek_char() {
                                sequence_string.push(next_ch);
                                self.next_char();
                            }
                        }
                        _ => unreachable!(),
                    }

                    let sequence_u32 = u32::from_str_radix(&sequence_string, 8)
                        .expect("implementation must be incorrect");
                    let escape_char =
                        char::try_from(sequence_u32).expect("implementation must be incorrect");
                    escape_sequence.push(escape_char);
                }

                // https://tc39.es/ecma262/#prod-HexEscapeSequence
                // x HexDigit HexDigit
                'x' => {
                    let mut sequence_string = String::new();

                    match self.next_char() {
                        Some(next_ch @ ('0'..='9' | 'a'..='f' | 'A'..='F')) => {
                            sequence_string.push(next_ch);

                            match self.next_char() {
                                Some(next_next_ch @ ('0'..='9' | 'a'..='f' | 'A'..='F')) => {
                                    sequence_string.push(next_next_ch);
                                }
                                _ => return Err(()),
                            }
                        }
                        _ => return Err(()),
                    }

                    let sequence_u32 = u32::from_str_radix(&sequence_string, 16)
                        .expect("implementation must be incorrect");
                    let escape_char =
                        char::try_from(sequence_u32).expect("implementation must be incorrect");
                    escape_sequence.push(escape_char);
                }
                // https://tc39.es/ecma262/#prod-UnicodeEscapeSequence
                // u Hex4Digits
                // u{ CodePoint }
                'u' => match self.next_char() {
                    Some('{') => {
                        // CodePoint ::
                        //   HexDigits[~Sep] but only if the MV of HexDigits ≤ 0x10FFFF
                        let start_char = match self.peek_char() {
                            Some(c @ ('0'..='9' | 'a'..='f' | 'A'..='F')) => c,
                            _ => return Err(()),
                        };

                        self.next_char();

                        let mut hex_string = String::from(start_char);

                        while let Some(next_ch) = self.peek_char() {
                            if matches!(next_ch, '0'..='9' | 'a'..='f' | 'A'..='F') {
                                // TODO: need to do the `> 0x10FFFF` check somewhere, maybe in the
                                // parser? depends if it makes sense to still treat it as a
                                // `Token::String` or if it should be `Token::Invalid`
                                self.next_char();
                                hex_string.push(next_ch);
                            } else {
                                break;
                            }
                        }

                        if self.peek_char() != Some('}') {
                            return Err(());
                        }

                        self.next_char();

                        let sequence_u32 = u32::from_str_radix(&hex_string, 16)
                            .expect("implementation must be incorrect");

                        let escape_char = match char::try_from(sequence_u32) {
                            Ok(ch) => ch,
                            Err(_) => {
                                escape_sequence.push('\\');
                                escape_sequence.push('u');
                                escape_sequence.push_str(&hex_string);
                                return Ok(escape_sequence);
                            }
                        };

                        escape_sequence.push(escape_char);
                    }
                    Some(next_ch @ ('0'..='9' | 'a'..='f' | 'A'..='F')) => {
                        let mut sequence_string = String::new();
                        sequence_string.push(next_ch);

                        for _ in 0..3 {
                            if let Some(c @ ('0'..='9' | 'a'..='f' | 'A'..='F')) = self.next_char()
                            {
                                sequence_string.push(c);
                            } else {
                                return Err(());
                            }
                        }

                        let sequence_u32 = u32::from_str_radix(&sequence_string, 16)
                            .expect("implementation must be incorrect");

                        // https://tc39.es/ecma262/#surrogate-pair
                        let is_leading_surrogate = (0xD800..=0xDBFF).contains(&sequence_u32);

                        if is_leading_surrogate {
                            let mut is_surrogate_pair = false;

                            if self.peek_char() == Some('\\') {
                                self.next_char();

                                if self.peek_char() == Some('u') {
                                    self.next_char();
                                    is_surrogate_pair = true;
                                } else {
                                    self.read_position -= 1;
                                }
                            }

                            if is_surrogate_pair {
                                let mut trailing_surrogate = String::new();

                                for _ in 0..4 {
                                    if let Some(c @ ('0'..='9' | 'a'..='f' | 'A'..='F')) =
                                        self.next_char()
                                    {
                                        trailing_surrogate.push(c);
                                    } else {
                                        return Err(());
                                    }
                                }

                                let trailing_surrogate_u32 =
                                    u32::from_str_radix(&trailing_surrogate, 16)
                                        .expect("implementation must be incorrect");

                                // valid surrogate pair
                                if (0xDC00..=0xDFFF).contains(&trailing_surrogate_u32) {
                                    let surrogate_pair = (sequence_u32 - 0xD800) * 0x400
                                        + (trailing_surrogate_u32 - 0xDC00)
                                        + 0x10000;

                                    let surrogate_pair_char = char::try_from(surrogate_pair)
                                        .expect("implementation must be incorrect");
                                    escape_sequence.push(surrogate_pair_char);
                                    return Ok(escape_sequence);
                                }
                            }
                        }

                        let escape_char = match char::try_from(sequence_u32) {
                            Ok(ch) => ch,
                            Err(_) => {
                                escape_sequence.push('\\');
                                escape_sequence.push('u');
                                escape_sequence.push_str(&sequence_string);
                                return Ok(escape_sequence);
                            }
                        };

                        escape_sequence.push(escape_char);
                    }
                    _ => return Err(()),
                },
                _ => escape_sequence.push(ch),
            },

            None => return Err(()),
        };

        Ok(escape_sequence)
    }

    fn read_template(&mut self) -> Token {
        let mut template_literal = String::new();

        while let Some(ch) = self.next_char() {
            match ch {
                '`' => break,
                '\\' => {
                    let escape_sequence = match self.read_escape_sequence() {
                        Ok(sequence) => sequence,
                        Err(_) => return Token::Invalid,
                    };

                    template_literal.push_str(&escape_sequence);
                }
                '$' => {
                    if self.peek_char() == Some('{') {
                        todo!("templates with substitutions are a lot more complex, will handle them separately (maybe in the parser like with regex)");
                    } else {
                        template_literal.push('$');
                    }
                }
                _ => template_literal.push(ch),
            };
        }

        self.next_char();

        Token::TemplateNoSubstitution(template_literal)
    }
}

/// each possible regex flag, see item 5 in this section:
/// https://tc39.es/ecma262/#sec-regexpinitialize
#[derive(Debug)]
enum RegularExpressionFlag {
    /// HasIndices
    D = 0b00000001,
    /// Global
    G = 0b00000010,
    /// IgnoreCase
    I = 0b00000100,
    /// Multiline
    M = 0b00001000,
    /// DotAll
    S = 0b00010000,
    /// Unicode
    U = 0b00100000,
    /// UnicodeSets
    V = 0b01000000,
    /// Sticky
    Y = 0b10000000,
}

impl TryFrom<char> for RegularExpressionFlag {
    type Error = String;

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        match ch {
            'd' => Ok(Self::D),
            'g' => Ok(Self::G),
            'i' => Ok(Self::I),
            'm' => Ok(Self::M),
            's' => Ok(Self::S),
            'u' => Ok(Self::U),
            'v' => Ok(Self::V),
            'y' => Ok(Self::Y),
            _ => Err(format!("{ch} is not a valid regular expression flag")),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct RegularExpressionFlags {
    value: u8,
}

impl RegularExpressionFlags {
    fn default() -> Self {
        Self { value: 0 }
    }

    fn add_flag(&mut self, flag: RegularExpressionFlag) {
        self.value |= flag as u8;
    }
}

impl PartialEq<u8> for RegularExpressionFlags {
    fn eq(&self, other: &u8) -> bool {
        self.value == *other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input =
            "{} () [ ] . ... ; , < > <= >= = == ! != === !== + - * / % ** ++ -- << >> >>> / % & | ^ ~ ? : && || ?? += -= *= /= %= => **= <<= >>= >>>= &= |= ^= &&= ||= ??= await break case catch class const continue debugger default delete do else enum export extends false finally for function if import in instanceof new null return super switch this throw true try typeof var void while with yield hello #thisisprivate hi // single line comment *
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
`plain template with no substitutions`
`multiline template
with no substitutions`
`escaped template with no substitutions \\u2692`
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
            Token::TemplateNoSubstitution("plain template with no substitutions".to_string()),
            Token::TemplateNoSubstitution("multiline template\nwith no substitutions".to_string()),
            Token::TemplateNoSubstitution("escaped template with no substitutions ⚒".to_string()),
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

    #[test]
    fn test_read_regex() {
        // we only know to tokenise a regex based on context in the parser, so we're testing the
        // regex functionality separately from the main test
        let input = "/this\\/[0-9]+is(a.*)[/test\\]]\\[regex$/gid";

        let mut lexer = Lexer::new(input);

        let original_token = lexer.next_token();

        assert_eq!(original_token, Token::Division);
        let regex_token = lexer.read_regex(original_token);

        if let Token::RegularExpression { body, flags } = regex_token {
            assert_eq!(body, "this\\/[0-9]+is(a.*)[/test\\]]\\[regex$".to_string());
            assert_eq!(
                flags,
                RegularExpressionFlag::G as u8
                    | RegularExpressionFlag::I as u8
                    | RegularExpressionFlag::D as u8
            );
        } else {
            panic!("Expected `RegularExpression` token");
        }
    }

    #[test]
    fn test_strings() {
        let input = "
            'hello\\n \\' \\\\ \\08'
            'hi\\8 \\9'
            'hey\\47 \\48 \\4 \\147'
            'another \" \\\" 123'
            'hex \\x8F'
            'unicode \\u2692'
            'surrogate pair \\uD83D\\uDE43'
            'invalid unicode \\uD83D'
            'codepoint \\u{2692}'
            'long codepoint \\u{1F976}'
            'invalid codepoint \\u{D83D}'

            \"hello\\n \\\" \\\\ \\08\"
            \"hi\\8 \\9\"
            \"hey\\47 \\48 \\4 \\147\"
            \"another \' \\' 123\"
            \"hex \\xc4\"
            \"unicode \\u265e\"
            \"surrogate pair \\uD83D\\uDE1C\"
            \"invalid unicode \\udE1c\"
            \"codepoint \\u{265e}\"
            \"long codepoint \\u{1f921}\"
            \"invalid codepoint \\u{dE1c}\"
            ";

        let mut lexer = Lexer::new(input);

        let expected: Vec<Token> = vec![
            // single quotes
            Token::String("hello\n ' \\ \08".to_string()),
            Token::String("hi8 9".to_string()),
            Token::String("hey\u{27} \u{4}8 \u{4} \u{67}".to_string()),
            Token::String("another \" \" 123".to_string()),
            Token::String("hex \u{8f}".to_string()),
            Token::String("unicode ⚒".to_string()),
            Token::String("surrogate pair 🙃".to_string()),
            Token::String("invalid unicode \\uD83D".to_string()),
            Token::String("codepoint ⚒".to_string()),
            Token::String("long codepoint 🥶".to_string()),
            Token::String("invalid codepoint \\uD83D".to_string()),
            // double quotes
            Token::String("hello\n \" \\ \08".to_string()),
            Token::String("hi8 9".to_string()),
            Token::String("hey\u{27} \u{4}8 \u{4} \u{67}".to_string()),
            Token::String("another ' ' 123".to_string()),
            Token::String("hex \u{c4}".to_string()),
            Token::String("unicode ♞".to_string()),
            Token::String("surrogate pair 😜".to_string()),
            Token::String("invalid unicode \\udE1c".to_string()),
            Token::String("codepoint ♞".to_string()),
            Token::String("long codepoint 🤡".to_string()),
            Token::String("invalid codepoint \\udE1c".to_string()),
            Token::Eof,
        ];

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
