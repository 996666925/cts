use std::borrow::Cow;

use boa_ast::declaration::Binding;
use boa_ast::declaration::LexicalDeclaration;
use boa_ast::declaration::Variable;
use boa_ast::declaration::VariableList;
use boa_ast::expression::literal::Literal;
use boa_ast::expression::Identifier;
use boa_ast::Declaration;
use boa_ast::Expression;
use boa_ast::Module;
use boa_ast::ModuleItem;
use boa_ast::StatementListItem;
use boa_interner::Interner;
use boa_parser::{Parser, Source};

pub struct Compiler {
    module: Module,
    interner: Interner,
}

impl Compiler {
    pub fn new(source: &str) -> Self {
        let mut interner = Default::default();
        let module = Parser::new(Source::from_bytes(source))
            .parse_module(&mut interner)
            .expect("解析代码失败");
        Self { module, interner }
    }

    pub fn compile_module(&self) -> String {
        let mut code = String::new();
        for item in self.module.items().items() {
            code += &match item {
                ModuleItem::ImportDeclaration(_) => todo!(),
                ModuleItem::ExportDeclaration(_) => todo!(),
                ModuleItem::StatementListItem(statement) => self.compile_statement(statement),
            };
        }

        code
    }

    pub fn compile_statement(&self, statement: &StatementListItem) -> String {
        match statement {
            StatementListItem::Statement(_) => todo!(),
            StatementListItem::Declaration(declare) => self.compile_declare(declare),
        }
    }

    pub fn compile_declare(&self, declare: &Declaration) -> String {
        match declare {
            Declaration::Function(_) => todo!(),
            Declaration::Generator(_) => todo!(),
            Declaration::AsyncFunction(_) => todo!(),
            Declaration::AsyncGenerator(_) => todo!(),
            Declaration::Class(_) => todo!(),
            Declaration::Lexical(lexical) => self.compile_lexical(lexical),
        }
    }

    pub fn compile_lexical(&self, lexical: &LexicalDeclaration) -> String {
        match lexical {
            LexicalDeclaration::Const(_) => todo!(),
            LexicalDeclaration::Let(variable_list) => self.compile_variable_list(variable_list),
        }
    }

    pub fn compile_variable_list(&self, variable_list: &VariableList) -> String {
        let mut code = String::new();
        for variable in variable_list.as_ref() {
            code += &self.compile_variable(variable);
        }
        code
    }

    pub fn compile_variable(&self, variable: &Variable) -> String {
        let mut code = String::new();

        let binding = self.compile_binding(variable.binding());

        if let Some(init) = variable.init() {
            let (_type, value) = self.compile_expression(init);
            code = format!("{_type} {binding} = {value};")
        } else {
            code = format!("{{{binding}::type}} {binding};")
        }
        code
    }

    pub fn compile_expression(&self, expr: &Expression) -> (&str, String) {
        match expr {
            Expression::This => todo!(),
            Expression::Identifier(_) => todo!(),
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::ArrayLiteral(_) => todo!(),
            Expression::ObjectLiteral(_) => todo!(),
            Expression::Spread(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::ArrowFunction(_) => todo!(),
            Expression::AsyncArrowFunction(_) => todo!(),
            Expression::Generator(_) => todo!(),
            Expression::AsyncFunction(_) => todo!(),
            Expression::AsyncGenerator(_) => todo!(),
            Expression::Class(_) => todo!(),
            Expression::TemplateLiteral(_) => todo!(),
            Expression::PropertyAccess(_) => todo!(),
            Expression::New(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::SuperCall(_) => todo!(),
            Expression::ImportCall(_) => todo!(),
            Expression::Optional(_) => todo!(),
            Expression::TaggedTemplate(_) => todo!(),
            Expression::NewTarget => todo!(),
            Expression::ImportMeta => todo!(),
            Expression::Assign(_) => todo!(),
            Expression::Unary(_) => todo!(),
            Expression::Update(_) => todo!(),
            Expression::Binary(_) => todo!(),
            Expression::BinaryInPrivate(_) => todo!(),
            Expression::Conditional(_) => todo!(),
            Expression::Await(_) => todo!(),
            Expression::Yield(_) => todo!(),
            Expression::Parenthesized(_) => todo!(),
            _ => todo!(),
        }
    }

    pub fn compile_literal(&self, literal: &Literal) -> (&str, String) {
        match literal {
            Literal::String(_) => todo!(),
            Literal::Num(_) => todo!(),
            Literal::Int(int) => ("int", int.to_string()),
            Literal::BigInt(_) => todo!(),
            Literal::Bool(_) => todo!(),
            Literal::Null => todo!(),
            Literal::Undefined => todo!(),
        }
    }

    pub fn compile_binding(&self, binding: &Binding) -> String {
        match binding {
            Binding::Identifier(identifier) => self.compile_identifier(identifier),
            Binding::Pattern(_) => todo!(),
        }
    }

    pub fn compile_identifier(&self, identifier: &Identifier) -> String {
        if let Some(name) = self.interner.resolve(identifier.sym()) {
            return name.to_string();
        }

        panic!("声明语句必须有变量名")
    }
}
