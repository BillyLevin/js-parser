use std::fmt::Display;

use super::RegularExpressionFlags;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Invalid,
    Eof,

    // identifiers https://tc39.es/ecma262/#sec-names-and-keywords
    Identifier(String),
    PrivateIdentifier(String),

    // comments https://tc39.es/ecma262/#sec-comments
    SingleLineComment,
    MultiLineComment,

    // hashbang comments https://tc39.es/ecma262/#sec-hashbang
    HashbangComment,

    // reserved words https://tc39.es/ecma262/#prod-ReservedWord
    Await,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Extends,
    False,
    Finally,
    For,
    Function,
    If,
    Import,
    In,
    Instanceof,
    New,
    Null,
    Return,
    Super,
    Switch,
    This,
    Throw,
    True,
    Try,
    Typeof,
    Var,
    Void,
    While,
    With,
    Yield,

    // contextually disallowed identifiers / future reserved keywords (grouped together as there's
    // some overlap) https://tc39.es/ecma262/#sec-keywords-and-reserved-words
    Let,
    Static,
    Implements,
    Interface,
    Package,
    Private,
    Protected,
    Public,

    // contextual keywords. these are always allowed as identifiers, but are sometimes also keywords in
    // productions where `Identifier` is not allowed
    As,
    Async,
    From,
    Get,
    Meta,
    Of,
    Set,
    Target,

    // punctuators https://tc39.es/ecma262/#sec-punctuators
    LeftBrace,               // `{`
    RightBrace,              // `}`
    LeftParen,               // `(`
    RightParen,              // `)`
    LeftBracket,             // `[`
    RightBracket,            // `]`
    Dot,                     // `.`
    DotDotDot,               // `...`
    Semicolon,               // `;`
    Comma,                   // `,`
    LessThan,                // `<`
    GreaterThan,             // `>`
    LessThanEqual,           // `<=`
    GreaterThanEqual,        // `>=`
    DoubleEqual,             // `==`
    NotDoubleEqual,          // `!=`
    TripleEqual,             // `===`
    NotTripleEqual,          // `!==`
    Plus,                    // `+`
    Minus,                   // `-`
    Multiply,                // `*`
    Divide,                  // `/`
    Percent,                 // `%`
    Exponentiation,          // `**`
    PlusPlus,                // `++`
    MinusMinus,              // `--`
    LeftShift,               // `<<`
    RightShift,              // `>>`
    UnsignedRightShift,      // `>>>`
    BitwiseAnd,              // `&`
    BitwiseOr,               // `|`
    BitwiseXor,              // `^`
    Bang,                    // `!`
    BitwiseNot,              // `~`
    LogicalAnd,              // `&&`
    LogicalOr,               // `||`
    NullishCoalescing,       // `??`
    QuestionMark,            // `?`
    Colon,                   // `:`
    Equal,                   // `=`
    PlusEqual,               // `+=`
    MinusEqual,              // `-=`
    MultiplyEqual,           // `*=`
    DivideEqual,             // `/=`
    RemainderEqual,          // `%=`
    ExponentiationEqual,     // `**=`
    LeftShiftEqual,          // `<<=`
    RightShiftEqual,         // `>>=`
    UnsignedRightShiftEqual, // `>>>=`
    BitwiseAndEqual,         // `&=`
    BitwiseOrEqual,          // `|=`
    BitwiseXorEqual,         // `^=`
    LogicalAndEqual,         // `&&=`
    LogicalOrEqual,          // `||=`
    NullishCoalescingEqual,  // `??=`
    Arrow,                   // `=>`

    // string literals https://tc39.es/ecma262/#sec-literals-string-literals
    String(String),

    // numeric literals https://tc39.es/ecma262/#sec-literals-numeric-literals
    Decimal(f64),
    Binary(f64),
    Octal(f64),
    Hex(f64),
    LegacyOctal(f64),

    // regular expression literals https://tc39.es/ecma262/#sec-literals-regular-expression-literals
    RegularExpression {
        body: String,
        flags: RegularExpressionFlags,
    },

    // template literals https://tc39.es/ecma262/#sec-template-literal-lexical-components
    TemplateNoSubstitution(String),
    TemplateHead(String),
    TemplateMiddle(String),
    TemplateTail(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Invalid => write!(f, "invalid"),
            Token::Eof => write!(f, "EOF"),
            Token::Identifier(identifier) => write!(f, "Identifier: {}", identifier),
            Token::PrivateIdentifier(identifier) => write!(f, "Private Identifier: {}", identifier),
            Token::SingleLineComment => write!(f, "Comment"),
            Token::MultiLineComment => write!(f, "Comment"),
            Token::HashbangComment => write!(f, "Hashbang Comment"),
            Token::Await => write!(f, "await"),
            Token::Break => write!(f, "break"),
            Token::Case => write!(f, "case"),
            Token::Catch => write!(f, "catch"),
            Token::Class => write!(f, "class"),
            Token::Const => write!(f, "const"),
            Token::Continue => write!(f, "continue"),
            Token::Debugger => write!(f, "debugger"),
            Token::Default => write!(f, "default"),
            Token::Delete => write!(f, "delete"),
            Token::Do => write!(f, "do"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::Export => write!(f, "export"),
            Token::Extends => write!(f, "extends"),
            Token::False => write!(f, "false"),
            Token::Finally => write!(f, "finally"),
            Token::For => write!(f, "for"),
            Token::Function => write!(f, "function"),
            Token::If => write!(f, "if"),
            Token::Import => write!(f, "import"),
            Token::In => write!(f, "in"),
            Token::Instanceof => write!(f, "instanceof"),
            Token::New => write!(f, "new"),
            Token::Null => write!(f, "null"),
            Token::Return => write!(f, "return"),
            Token::Super => write!(f, "super"),
            Token::Switch => write!(f, "switch"),
            Token::This => write!(f, "this"),
            Token::Throw => write!(f, "throw"),
            Token::True => write!(f, "true"),
            Token::Try => write!(f, "try"),
            Token::Typeof => write!(f, "typeof"),
            Token::Var => write!(f, "var"),
            Token::Void => write!(f, "void"),
            Token::While => write!(f, "while"),
            Token::With => write!(f, "with"),
            Token::Yield => write!(f, "yield"),
            Token::Let => write!(f, "let"),
            Token::Static => write!(f, "static"),
            Token::Implements => write!(f, "implements"),
            Token::Interface => write!(f, "interface"),
            Token::Package => write!(f, "package"),
            Token::Private => write!(f, "private"),
            Token::Protected => write!(f, "protected"),
            Token::Public => write!(f, "public"),
            Token::As => write!(f, "as"),
            Token::Async => write!(f, "async"),
            Token::From => write!(f, "from"),
            Token::Get => write!(f, "get"),
            Token::Meta => write!(f, "meta"),
            Token::Of => write!(f, "of"),
            Token::Set => write!(f, "set"),
            Token::Target => write!(f, "target"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Dot => write!(f, "."),
            Token::DotDotDot => write!(f, "..."),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::LessThanEqual => write!(f, "<="),
            Token::GreaterThanEqual => write!(f, ">="),
            Token::DoubleEqual => write!(f, "=="),
            Token::NotDoubleEqual => write!(f, "!="),
            Token::TripleEqual => write!(f, "==="),
            Token::NotTripleEqual => write!(f, "!=="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Exponentiation => write!(f, "**"),
            Token::PlusPlus => write!(f, "++"),
            Token::MinusMinus => write!(f, "--"),
            Token::LeftShift => write!(f, "<<"),
            Token::RightShift => write!(f, ">>"),
            Token::UnsignedRightShift => write!(f, ">>>"),
            Token::BitwiseAnd => write!(f, "&"),
            Token::BitwiseOr => write!(f, "|"),
            Token::BitwiseXor => write!(f, "^"),
            Token::Bang => write!(f, "!"),
            Token::BitwiseNot => write!(f, "~"),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::NullishCoalescing => write!(f, "??"),
            Token::QuestionMark => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::Equal => write!(f, "="),
            Token::PlusEqual => write!(f, "+="),
            Token::MinusEqual => write!(f, "-="),
            Token::MultiplyEqual => write!(f, "*="),
            Token::DivideEqual => write!(f, "/="),
            Token::RemainderEqual => write!(f, "%="),
            Token::ExponentiationEqual => write!(f, "**="),
            Token::LeftShiftEqual => write!(f, "<<="),
            Token::RightShiftEqual => write!(f, ">>="),
            Token::UnsignedRightShiftEqual => write!(f, ">>>="),
            Token::BitwiseAndEqual => write!(f, "&="),
            Token::BitwiseOrEqual => write!(f, "|="),
            Token::BitwiseXorEqual => write!(f, "^="),
            Token::LogicalAndEqual => write!(f, "&&="),
            Token::LogicalOrEqual => write!(f, "||="),
            Token::NullishCoalescingEqual => write!(f, "??="),
            Token::Arrow => write!(f, "=>"),
            Token::String(value) => write!(f, "\"{}\"", value),
            Token::Decimal(value) => write!(f, "{}", value),
            Token::Binary(value) => write!(f, "{:#b}", *value as u64),
            Token::Octal(value) => write!(f, "{:#o}", *value as u64),
            Token::Hex(value) => write!(f, "{:#x}", *value as u64),
            Token::LegacyOctal(value) => write!(f, "{:#o}", *value as u64),
            Token::RegularExpression { body, flags } => write!(f, "/{}/{}", body, flags),
            Token::TemplateNoSubstitution(value) => write!(f, "`{}`", value),
            Token::TemplateHead(value) => write!(f, "`{}${{", value),
            Token::TemplateMiddle(value) => write!(f, "}}{}${{", value),
            Token::TemplateTail(value) => write!(f, "}}{}`", value),
        }
    }
}
