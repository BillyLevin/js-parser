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
    ExpressionStatement(ExpressionStatement),
    IfStatement,
    BreakableStatement,
    ContinueStatement(ContinueStatement),
    BreakStatement(BreakStatement),
    ReturnStatement(ReturnStatement),
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
    FunctionDeclaration(Function),
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
    ObjectPattern(Box<ObjectPattern>),
    ArrayPattern(Box<ArrayPattern>),
    AssignmentPattern(Box<AssignmentPattern>),
    RestElement(Box<RestElement>),
}

/// https://github.com/estree/estree/blob/master/es2015.md#arraypattern
#[derive(Debug, PartialEq)]
pub struct ArrayPattern {
    pub elements: Vec<Option<Pattern>>,
}

/// https://github.com/estree/estree/blob/master/es2015.md#objectpattern
#[derive(Debug, PartialEq)]
pub struct ObjectPattern {
    pub properties: Vec<ObjectPatternProperty>,
}

#[derive(Debug, PartialEq)]
pub enum ObjectPatternProperty {
    Property(AssignmentProperty),
    RestElement(RestElement),
}

#[derive(Debug, PartialEq)]
pub struct AssignmentProperty {
    pub key: Expression,
    pub value: Pattern,
    pub shorthand: bool,
    pub computed: bool,
}

/// https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
#[derive(Debug, PartialEq)]
pub struct AssignmentPattern {
    pub left: Pattern,
    pub right: Expression,
}

/// https://github.com/estree/estree/blob/master/es2015.md#restelement
#[derive(Debug, PartialEq)]
pub struct RestElement {
    pub argument: Pattern,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct PrivateIdentifier {
    pub name: String,
}

/// https://github.com/estree/estree/blob/master/es5.md#expressions
#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    BinaryExpression(Box<BinaryExpression>),
    LogicalExpression(Box<LogicalExpression>),
    AssignmentExpression(Box<AssignmentExpression>),
    UnaryExpression(Box<UnaryExpression>),
    UpdateExpression(Box<UpdateExpression>),
    MemberExpression(Box<MemberExpression>),
    Identifier(Identifier),
    ThisExpression(ThisExpression),
    ArrayExpression(Box<ArrayExpression>),
    ObjectExpression(Box<ObjectExpression>),
    FunctionExpression(Box<Function>),
    ClassExpression(Box<Class>),
    SequenceExpression(Box<SequenceExpression>),
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

/// https://github.com/estree/estree/blob/master/es5.md#returnstatement
#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub argument: Option<Expression>,
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

/// https://github.com/estree/estree/blob/master/es5.md#expressionstatement
#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

/// https://github.com/estree/estree/blob/master/es5.md#labeledstatement
#[derive(Debug, PartialEq)]
pub struct LabeledStatement {
    pub label: Identifier,
    pub body: Statement,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Binary(BinaryOperator),
    Logical(LogicalOperator),
    Assignment(AssignmentOperator),
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
            Token::LessThan => BinaryOperator::LessThan,
            Token::LessThanEqual => BinaryOperator::LessThanEqual,
            Token::GreaterThan => BinaryOperator::GreaterThan,
            Token::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
            Token::In => BinaryOperator::In,
            Token::Instanceof => BinaryOperator::Instanceof,
            Token::DoubleEqual => BinaryOperator::DoubleEqual,
            Token::NotDoubleEqual => BinaryOperator::NotDoubleEqual,
            Token::TripleEqual => BinaryOperator::TripleEqual,
            Token::NotTripleEqual => BinaryOperator::NotTripleEqual,
            Token::BitwiseAnd => BinaryOperator::BitwiseAnd,
            Token::BitwiseXor => BinaryOperator::BitwiseXor,
            Token::BitwiseOr => BinaryOperator::BitwiseOr,
            _ => unreachable!("this function should only be called with tokens that can be mapped to a binary operation"),
        }
    }
}

/// https://github.com/estree/estree/blob/master/es5.md#logicalexpression
#[derive(Debug, PartialEq)]
pub struct LogicalExpression {
    pub left: Expression,
    pub right: Expression,
    pub operator: LogicalOperator,
}

/// https://github.com/estree/estree/blob/master/es5.md#logicaloperator
#[derive(Debug, PartialEq)]
pub enum LogicalOperator {
    /// `&&`
    And,
    /// `||`
    Or,
    /// `??`
    NullishCoalescing,
}

impl From<&Token> for LogicalOperator {
    fn from(token: &Token) -> Self {
        match token {
            Token::LogicalAnd => LogicalOperator::And,
            Token::LogicalOr => LogicalOperator::Or,
            Token::NullishCoalescing => LogicalOperator::NullishCoalescing,
            _ => unreachable!("this function should only be called with tokens that can be mapped to a logical operation"),
        }
    }
}

/// https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
#[derive(Debug, PartialEq)]
pub struct AssignmentExpression {
    pub left: Expression,
    pub right: Expression,
    pub operator: AssignmentOperator,
}

/// https://github.com/estree/estree/blob/master/es5.md#assignmentoperator
#[derive(Debug, PartialEq)]
pub enum AssignmentOperator {
    /// `=`
    Assign,
    /// `+=`
    Plus,
    /// `-=`
    Minus,
    /// `*=`
    Multiply,
    /// `/=`
    Divide,
    /// `%=`
    Remainder,
    /// `<<=`
    LeftShift,
    /// `>>=`
    RightShift,
    /// `>>>=`
    UnsignedRightShift,
    /// `|=`
    BitwiseOr,
    /// `^=`
    BitwiseXor,
    /// `&=`
    BitwiseAnd,
    /// `**=`
    Exponentiation,
    /// `||=`
    LogicalOr,
    /// `&&=`
    LogicalAnd,
    /// `??=`
    NullishCoalescing,
}

impl From<&Token> for AssignmentOperator {
    fn from(token: &Token) -> Self {
        match token {
            Token::Equal => AssignmentOperator::Assign,
            Token::PlusEqual => AssignmentOperator::Plus,
            Token::MinusEqual => AssignmentOperator::Minus,
            Token::MultiplyEqual => AssignmentOperator::Multiply,
            Token::DivideEqual => AssignmentOperator::Divide,
            Token::RemainderEqual => AssignmentOperator::Remainder,
            Token::LeftShiftEqual => AssignmentOperator::LeftShift,
            Token::RightShiftEqual => AssignmentOperator::RightShift,
            Token::UnsignedRightShiftEqual => AssignmentOperator::UnsignedRightShift,
            Token::BitwiseOrEqual => AssignmentOperator::BitwiseOr,
            Token::BitwiseXorEqual => AssignmentOperator::BitwiseXor,
            Token::BitwiseAndEqual => AssignmentOperator::BitwiseAnd,
            Token::ExponentiationEqual => AssignmentOperator::Exponentiation,
            Token::LogicalOrEqual => AssignmentOperator::LogicalOr,
            Token::LogicalAndEqual => AssignmentOperator::LogicalAnd,
            Token::NullishCoalescingEqual => AssignmentOperator::NullishCoalescing,
            _ => unreachable!("this function should only be called with tokens that can be mapped to an assignment operation"),
        }
    }
}

/// https://github.com/estree/estree/blob/master/es5.md#unaryexpression
#[derive(Debug, PartialEq)]
pub struct UnaryExpression {
    pub argument: Expression,
    pub operator: UnaryOperator,
    // TODO: estree spec contains this field, but as far as i can tell the unary operator is always
    // a prefix. find out if this is needed
    pub prefix: bool,
}

/// https://github.com/estree/estree/blob/master/es5.md#unaryoperator
#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `!`
    LogicalNot,
    /// `~`
    BitwiseNot,
    /// `typeof`
    Typeof,
    /// `void`
    Void,
    /// `delete`
    Delete,
}

impl From<&Token> for UnaryOperator {
    fn from(token: &Token) -> Self {
        match token {
           Token::Minus => UnaryOperator::Minus,
           Token::Plus => UnaryOperator::Plus,
           Token::Bang => UnaryOperator::LogicalNot,
           Token::BitwiseNot => UnaryOperator::BitwiseNot,
           Token::Typeof => UnaryOperator::Typeof,
           Token::Void => UnaryOperator::Void,
           Token::Delete => UnaryOperator::Delete,
            _ => unreachable!("this function should only be called with tokens that can be mapped to an unary operation"),
        }
    }
}

/// https://github.com/estree/estree/blob/master/es5.md#updateexpression
#[derive(Debug, PartialEq)]
pub struct UpdateExpression {
    pub argument: Expression,
    pub operator: UpdateOperator,
    pub prefix: bool,
}

/// https://github.com/estree/estree/blob/master/es5.md#updateoperator
#[derive(Debug, PartialEq)]
pub enum UpdateOperator {
    /// `++`
    Increment,
    /// `--`
    Decrement,
}

impl From<&Token> for UpdateOperator {
    fn from(token: &Token) -> Self {
        match token {
            Token::PlusPlus => UpdateOperator::Increment,
            Token::MinusMinus => UpdateOperator::Decrement,
            _ => unreachable!("this function should only be called with tokens that can be mapped to an update operation"),
        }
    }
}

/// https://github.com/estree/estree/blob/master/es5.md#thisexpression
#[derive(Debug, PartialEq)]
pub struct ThisExpression;

/// https://github.com/estree/estree/blob/master/es5.md#arrayexpression
#[derive(Debug, PartialEq)]
pub struct ArrayExpression {
    pub elements: Vec<Option<ArrayElement>>,
}

/// ES2015 introduced `SpreadElement` as an additional array element variant: https://github.com/estree/estree/blob/master/es2015.md#expressions
#[derive(Debug, PartialEq)]
pub enum ArrayElement {
    Expression(Expression),
    SpreadElement(SpreadElement),
}

/// https://github.com/estree/estree/blob/master/es2015.md#expressions
#[derive(Debug, PartialEq)]
pub struct SpreadElement {
    pub argument: Expression,
}

/// https://github.com/estree/estree/blob/master/es5.md#objectexpression
#[derive(Debug, PartialEq)]
pub struct ObjectExpression {
    pub properties: Vec<ObjectProperty>,
}

/// https://github.com/estree/estree/blob/master/es2018.md#expressions
#[derive(Debug, PartialEq)]
pub enum ObjectProperty {
    Property(Property),
    SpreadElement(SpreadElement),
}

/// https://github.com/estree/estree/blob/master/es5.md#property
#[derive(Debug, PartialEq)]
pub struct Property {
    pub key: Expression,
    pub value: Expression,
    pub kind: PropertyKind,
    pub method: bool,
    pub shorthand: bool,
    pub computed: bool,
}

#[derive(Debug, PartialEq)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

/// https://github.com/estree/estree/blob/master/es5.md#functions
#[derive(Debug, PartialEq)]
pub struct Function {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: BlockStatement,
}

/// https://github.com/estree/estree/blob/master/es2015.md#classes
#[derive(Debug, PartialEq)]
pub struct Class {
    pub id: Option<Identifier>,
    pub super_class: Option<Expression>,
    pub body: ClassBody,
}

/// https://github.com/estree/estree/blob/master/es2015.md#classbody
#[derive(Debug, PartialEq)]
pub struct ClassBody {
    pub body: Vec<ClassElement>,
}

/// https://github.com/estree/estree/blob/master/es2022.md#classbody
#[derive(Debug, PartialEq)]
pub enum ClassElement {
    MethodDefinition(MethodDefinition),
    PropertyDefinition,
    StaticBlock,
}

/// https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
#[derive(Debug, PartialEq)]
pub struct MethodDefinition {
    pub key: Expression,
    pub value: Function,
    pub kind: MethodDefinitionKind,
    pub computed: bool,
    pub r#static: bool,
}

#[derive(Debug, PartialEq)]
pub enum MethodDefinitionKind {
    Constructor,
    Method,
    Get,
    Set,
}

/// https://github.com/estree/estree/blob/master/es5.md#memberexpression
#[derive(Debug, PartialEq)]
pub enum MemberExpression {
    Static(StaticMemberExpression),
    Computed(ComputedMemberExpression),
}

#[derive(Debug, PartialEq)]
pub struct StaticMemberExpression {
    pub object: Expression,
    pub property: Identifier,
}

#[derive(Debug, PartialEq)]
pub struct ComputedMemberExpression {
    pub object: Expression,
    pub property: Expression,
}

/// https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
#[derive(Debug, PartialEq)]
pub struct SequenceExpression {
    pub expressions: Vec<Expression>,
}
