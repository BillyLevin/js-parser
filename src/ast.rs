use crate::lexer::{token::Token, RegularExpressionFlags};

#[derive(Debug, Default)]
pub struct Program {
    pub body: Vec<Statement>,
}

impl Program {
    pub fn push_statement(&mut self, statement: Statement) {
        self.body.push(statement);
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    // TODO: check if this stuff is all in estree. might be differences like with variable
    // declarations
    BlockStatement(BlockStatement),
    EmptyStatement,
    ExpressionStatement,
    IfStatement,
    BreakableStatement,
    ContinueStatement(ContinueStatement),
    BreakStatement(BreakStatement),
    ReturnStatement,
    WithStatement,
    LabeledStatement(Box<LabeledStatement>),
    ThrowStatement,
    TryStatement,
    DebuggerStatement,

    /// while ECMAScript specifies a [`VariableStatement`](https://tc39.es/ecma262/#prod-VariableStatement),
    /// estree instead specifies a [`VariableDeclaration`](https://github.com/estree/estree/blob/master/es5.md#variabledeclaration)
    /// that handles all of var, let, and const
    Declaration(Declaration),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
}

/// [ES5 `var`](https://github.com/estree/estree/blob/master/es5.md#variabledeclaration)
/// [ES2015 `let`, `const`](https://github.com/estree/estree/blob/master/es2015.md#variabledeclaration)
#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub kind: VariableDeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, PartialEq)]
pub enum VariableDeclarationKind {
    Var,
    Let,
    Const,
}

/// https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
#[derive(Debug, PartialEq)]
pub struct VariableDeclarator {
    pub id: Pattern,
    pub init: Option<Expression>,
}

/// https://github.com/estree/estree/blob/master/es5.md#patterns
/// https://github.com/estree/estree/blob/master/es2015.md#patterns
#[derive(Debug, PartialEq)]
pub enum Pattern {
    Identifier(Identifier),
    ObjectPattern,
    ArrayPattern,
    RestElement,
    AssignmentPattern,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}

/// https://github.com/estree/estree/blob/master/es5.md#expressions
#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    BinaryExpression(Box<BinaryExpression>),
}

/// https://github.com/estree/estree/blob/master/es5.md#literal
#[derive(Debug, PartialEq)]
pub enum Literal {
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    NullLiteral,
    NumberLiteral(NumberLiteral),
    RegExpLiteral(RegExpLiteral),
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral {
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug, PartialEq)]
pub struct NumberLiteral {
    pub value: f64,
}

#[derive(Debug, PartialEq)]
pub struct RegExpLiteral {
    pub regex: RegExp,
}

#[derive(Debug, PartialEq)]
pub struct RegExp {
    pub pattern: String,
    pub flags: RegularExpressionFlags,
}

/// https://github.com/estree/estree/blob/master/es5.md#breakstatement
#[derive(Debug, PartialEq)]
pub struct BreakStatement {
    pub label: Option<Identifier>,
}

/// https://github.com/estree/estree/blob/master/es5.md#continuestatement
#[derive(Debug, PartialEq)]
pub struct ContinueStatement {
    pub label: Option<Identifier>,
}

/// https://github.com/estree/estree/blob/master/es5.md#blockstatement
#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

/// https://github.com/estree/estree/blob/master/es5.md#labeledstatement
#[derive(Debug, PartialEq)]
pub struct LabeledStatement {
    pub label: Identifier,
    pub body: Statement,
}

/// https://github.com/estree/estree/blob/master/es5.md#binaryexpression
#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
    pub left: Expression,
    pub right: Expression,
    pub operator: BinaryOperator,
}

/// https://github.com/estree/estree/blob/master/es5.md#binaryoperator
#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    /// `==`
    DoubleEqual,
    /// `!=`
    NotDoubleEqual,
    /// `===`
    TripleEqual,
    /// `!==`
    NotTripleEqual,
    /// `<`
    LessThan,
    /// `<=`
    LessThanEqual,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanEqual,
    /// `<<`
    LeftShift,
    /// `>>`
    RightShift,
    /// `>>>`
    UnsignedRightShift,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Remainder,
    /// `|`
    BitwiseOr,
    /// `^`
    BitwiseXor,
    /// `&`
    BitwiseAnd,
    /// `in`
    In,
    /// `instanceof`
    Instanceof,
    /// `**`
    Exponentiation,
}

impl From<&Token> for BinaryOperator {
    fn from(token: &Token) -> Self {
        match token {
            Token::Plus => BinaryOperator::Plus,
            Token::Minus => BinaryOperator::Minus,
            Token::Multiply => BinaryOperator::Multiply,
            Token::Divide => BinaryOperator::Divide,
            Token::Exponentiation => BinaryOperator::Exponentiation,
            Token::Percent => BinaryOperator::Remainder,
            Token::LeftShift => BinaryOperator::LeftShift,
            Token::RightShift => BinaryOperator::RightShift,
            Token::UnsignedRightShift => BinaryOperator::UnsignedRightShift,
            _ => unreachable!("this function should only be called with tokens that can be mapped to a binary operation"),
        }
    }
}
