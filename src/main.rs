use js_parser::{lexer::Lexer, parser::Parser};

fn main() {
    let input = r#"
            var arr = [1, 2];
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    dbg!(program.body);
}
