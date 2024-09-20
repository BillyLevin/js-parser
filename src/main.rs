use js_parser::{lexer::Lexer, parser::Parser};

fn main() {
    let input = r#"
            var func = function testFunction() {
                var a = true;
                var b = false, c = 4 ** 7 / 3;
            };
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    dbg!(program.body);
}
