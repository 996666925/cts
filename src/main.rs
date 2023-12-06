use cts_compiler::compiler::Compiler;

fn main() {
    let cts = r#"
let a=123;
let b=123;
    "#;

    let compiler = Compiler::new(cts);
    let code = compiler.compile_module();

    println!("{code}");
}
