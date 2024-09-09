use crate::lexer::token::Token;

/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence#table
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Comma,
    Assignment,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
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

impl Precedence {
    pub fn is_right_associative(&self) -> bool {
        matches!(self, Precedence::Assignment | Precedence::Exponentiation)
    }
}

impl TryFrom<&Token> for Precedence {
    type Error = ();

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus | Token::Minus => Ok(Precedence::Addition),
            Token::Multiply | Token::Divide | Token::Percent => Ok(Precedence::Multiplication),
            Token::Exponentiation => Ok(Precedence::Exponentiation),

            Token::LeftShift | Token::RightShift | Token::UnsignedRightShift => {
                Ok(Precedence::BitwiseShift)
            }

            Token::LessThan
            | Token::LessThanEqual
            | Token::GreaterThan
            | Token::GreaterThanEqual
            | Token::In
            | Token::Instanceof => Ok(Precedence::Relational),

            Token::DoubleEqual
            | Token::NotDoubleEqual
            | Token::TripleEqual
            | Token::NotTripleEqual => Ok(Precedence::Equality),

            Token::BitwiseAnd => Ok(Precedence::BitwiseAnd),
            Token::BitwiseXor => Ok(Precedence::BitwiseXor),
            Token::BitwiseOr => Ok(Precedence::BitwiseOr),
            _ => Err(()),
        }
    }
}
