#[macro_export]
macro_rules! binary_expr {
    ($left:expr, $right:expr, $op:ident) => {
        Expression::BinaryExpression(Box::new(BinaryExpression {
            left: $left,
            right: $right,
            operator: BinaryOperator::$op,
        }))
    };
}

#[macro_export]
macro_rules! logical_expr {
    ($left:expr, $right:expr, $op:ident) => {
        Expression::LogicalExpression(Box::new(LogicalExpression {
            left: $left,
            right: $right,
            operator: LogicalOperator::$op,
        }))
    };
}

#[macro_export]
macro_rules! assign_expr {
    ($left:expr, $right:expr, $op:ident) => {
        Expression::AssignmentExpression(Box::new(AssignmentExpression {
            left: $left,
            right: $right,
            operator: AssignmentOperator::$op,
        }))
    };
}

#[macro_export]
macro_rules! unary_expr {
    ($arg:expr, $op:ident) => {
        Expression::UnaryExpression(Box::new(UnaryExpression {
            argument: $arg,
            operator: UnaryOperator::$op,
            prefix: true,
        }))
    };
}

#[macro_export]
macro_rules! update_expr {
    ($arg:expr, $op:ident, $prefix:literal) => {
        Expression::UpdateExpression(Box::new(UpdateExpression {
            argument: $arg,
            operator: UpdateOperator::$op,
            prefix: $prefix,
        }))
    };
}

#[macro_export]
macro_rules! ident_expr {
    ($ident:expr) => {
        Expression::Identifier(Identifier {
            name: $ident.to_string(),
        })
    };
}

#[macro_export]
macro_rules! literal_expr {
    (true) => {
        Expression::Literal(Literal::BooleanLiteral(BooleanLiteral { value: true }))
    };

    (false) => {
        Expression::Literal(Literal::BooleanLiteral(BooleanLiteral { value: false }))
    };

    (null) => {
        Expression::Literal(Literal::NullLiteral)
    };

    ($lit:literal) => {
        match $lit.to_string().parse::<f64>() {
            Ok(num) => Expression::Literal(Literal::NumberLiteral(NumberLiteral { value: num })),
            _ => Expression::Literal(Literal::StringLiteral(StringLiteral {
                value: $lit.to_string(),
            })),
        }
    };
}

#[macro_export]
macro_rules! array_expr_element {
    ($expr:expr) => {
        Some(ArrayElement::Expression($expr))
    };
}

#[macro_export]
macro_rules! array_spread_element {
    ($expr:expr) => {
        Some(ArrayElement::SpreadElement(SpreadElement {
            argument: $expr,
        }))
    };
}

#[macro_export]
macro_rules! ident_pattern {
    ($ident:expr) => {
        Pattern::Identifier(Identifier {
            name: $ident.to_string(),
        })
    };
}

#[macro_export]
macro_rules! rest_pattern {
    ($arg:expr) => {
        Pattern::RestElement(Box::new(RestElement { argument: $arg }))
    };
}
