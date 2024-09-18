use js_parser::{lexer::Lexer, parser::Parser};

fn main() {
    let input = r#"
            var obj = {
                [computed]: true
            };
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    dbg!(program.body);
}
