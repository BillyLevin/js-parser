use crate::lexer::token::Token;

/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence#table
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Comma,
    Assignment,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseAnd,
    Equality,
    Relational,
    BitwiseShift,
    Addition,
    Multiplication,
    Exponentiation,
    Prefix,
    Postfix,
    New,
    Call,
    Member,
    Grouping,
}

impl TryFrom<&Token> for Precedence {
    type Error = ();

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus | Token::Minus => Ok(Precedence::Addition),
            Token::Multiply | Token::Divide => Ok(Precedence::Multiplication),
            _ => Err(()),
        }
    }
}
