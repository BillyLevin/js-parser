pub struct Program {
    pub body: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { body: Vec::new() }
    }

    pub fn push_statement(&mut self, statement: Statement) {
        self.body.push(statement);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    BlockStatement,
    VariableStatement,
    EmptyStatement,
    ExpressionStatement,
    IfStatement,
    BreakableStatement,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
    WithStatement,
    LabelledStatement,
    ThrowStatement,
    TryStatement,
    DebuggerStatement,
}
