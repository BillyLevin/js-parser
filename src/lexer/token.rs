#[derive(Debug, PartialEq)]
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
    Division,                // `/`
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
    DivisionEqual,           // `/=`
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
}
