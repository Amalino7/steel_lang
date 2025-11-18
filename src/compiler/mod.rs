use crate::parser::ast::{Expr, Literal, Stmt};
use crate::token::TokenType;
use crate::vm::bytecode::{Chunk, Opcode};
use crate::vm::value::Value;
use std::rc::Rc;

struct Compiler {
    chunk: Chunk,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
        }
    }
    pub fn compile(mut self, statements: &[Stmt]) -> Chunk {
        for stmt in statements {
            self.compile_stmt(stmt);
        }
        self.chunk
    }
    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(e) => {
                self.compile_expr(e);
                self.emit_op(Opcode::Pop, e.get_line());
            }
            Stmt::Let {
                identifier,
                value,
                type_info,
                scope,
                index,
            } => {
                self.compile_expr(value);
                let scope = scope.expect("Scope should be written");
                let index = index.expect("Index should be written");
                if scope == 0 {
                    self.emit_op(Opcode::SetGlobal, identifier.line);
                    self.emit_byte(scope as u8, identifier.line);
                } else {
                    self.emit_op(Opcode::SetLocal, identifier.line);
                    self.emit_byte(index as u8, identifier.line); // ??? TODO rework logic
                }
            }
            Stmt::Block(statements) => {
                for stmt in statements {
                    self.compile_stmt(stmt);
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let line = condition.get_line();
                let then_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.compile_stmt(then_branch);
                self.emit_jump_back(then_jump, then_branch.get_line());
                self.patch_jump(then_jump);

                let else_jump = self.emit_jump(Opcode::Jump, line);
                self.emit_op(Opcode::Pop, line);

                if let Some(else_branch) = else_branch {
                    self.compile_stmt(else_branch);
                }

                self.patch_jump(else_jump);
            }
            Stmt::While { condition, body } => {
                let line = condition.get_line();

                let loop_start = self.chunk.instructions.len();
                self.compile_expr(condition);

                let exit_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.compile_stmt(body);
                self.emit_jump_back(loop_start, body.get_line());

                self.patch_jump(exit_jump);
                self.emit_op(Opcode::Pop, body.get_line());
            }
            Stmt::Function { .. } => {
                todo!()
            }
            Stmt::Return(val) => {
                self.compile_expr(val);
                self.emit_op(Opcode::Return, val.get_line());
            }
        }
    }

    fn emit_jump(&mut self, op: Opcode, line: usize) -> usize {
        self.emit_op(op, line);
        self.emit_byte(0xff, line); // Placeholder
        self.emit_byte(0xff, line); // Placeholder
        self.chunk.instructions.len() - 2
    }
    fn emit_jump_back(&mut self, start: usize, line: usize) {
        self.emit_op(Opcode::JumpBack, line);
        let offset = self.chunk.instructions.len() - start + 2;
        if offset > u16::MAX as usize {
            panic!("Loop body too large!");
        }
        self.emit_byte(((offset >> 8) & 0xff) as u8, line);
        self.emit_byte((offset & 0xff) as u8, line);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.chunk.instructions.len() - offset - 2;
        if jump > u16::MAX as usize {
            panic!("Too much code to jump over!");
        }
        self.chunk.instructions[offset] = ((jump >> 8) & 0xff) as u8;
        self.chunk.instructions[offset + 1] = (jump & 0xff) as u8;
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary {
                left,
                right,
                operator,
            } => {
                self.compile_expr(left);
                self.compile_expr(right);
                let line = operator.line;
                match operator.token_type {
                    TokenType::Plus => self.emit_op(Opcode::Add, line),
                    TokenType::Minus => self.emit_op(Opcode::Subtract, line),
                    TokenType::Star => self.emit_op(Opcode::Multiply, line),
                    TokenType::Slash => self.emit_op(Opcode::Divide, line),
                    TokenType::EqualEqual => self.emit_op(Opcode::Equal, line),
                    TokenType::BangEqual => {
                        self.emit_op(Opcode::Equal, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    TokenType::Greater => self.emit_op(Opcode::Greater, line),
                    TokenType::GreaterEqual => {
                        self.emit_op(Opcode::Less, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    TokenType::Less => self.emit_op(Opcode::Less, line),
                    TokenType::LessEqual => {
                        self.emit_op(Opcode::Greater, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    _ => panic!("Unknown binary operator"),
                }
            }
            Expr::Grouping { expression } => {
                self.compile_expr(expression);
            }
            Expr::Literal { literal, line } => match literal {
                Literal::Number(n) => self.chunk.write_constant(Value::Number(*n), *line),
                Literal::String(s) => self
                    .chunk
                    .write_constant(Value::String(Rc::new(s.to_string())), *line),
                Literal::Boolean(b) => {
                    self.chunk.write_constant(Value::Boolean(*b), *line);
                }
                Literal::Void => {
                    self.chunk.write_constant(Value::Nil, *line);
                }
            },

            Expr::Unary {
                operator,
                expression,
            } => {
                self.compile_expr(expression);
                match operator.token_type {
                    TokenType::Minus => self.emit_op(Opcode::Negate, operator.line),
                    TokenType::Bang => self.emit_op(Opcode::Not, operator.line),
                    _ => panic!("Unknown unary operator"),
                }
            }
            Expr::Variable { name, scope, index } => {
                let scope = scope.expect("Scope should be written");
                let index = index.expect("Index should be written");
                if scope == 0 {
                    self.emit_op(Opcode::GetGlobal, name.line);
                    self.emit_byte(index as u8, name.line);
                } else {
                    self.emit_op(Opcode::GetLocal, name.line);
                    self.emit_byte(index as u8, name.line);
                }
            }
            Expr::Assignment { identifier, value } => {
                self.compile_expr(value);
                todo!()
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                self.compile_expr(left);
                if operator.token_type == TokenType::Or {
                    // TODO Add jump if true for better performance
                    let else_jump = self.emit_jump(Opcode::JumpIfFalse, operator.line);
                    let end_jump = self.emit_jump(Opcode::Jump, operator.line);
                    self.patch_jump(else_jump);
                    self.emit_op(Opcode::Pop, operator.line);
                    self.compile_expr(right);
                    self.patch_jump(end_jump);
                } else if operator.token_type == TokenType::And {
                    let short_circuit = self.emit_jump(Opcode::JumpIfFalse, operator.line);
                    self.emit_op(Opcode::Pop, operator.line);
                    self.compile_expr(right);
                    self.patch_jump(short_circuit);
                } else {
                    panic!("Unknown logical operator");
                }
            }
            Expr::Call { callee, arguments } => {
                todo!()
            }
        }
    }

    fn emit_op(&mut self, op: Opcode, line: usize) {
        self.chunk.write_op(op as u8, line);
    }
    fn emit_byte(&mut self, byte: u8, line: usize) {
        self.chunk.write_op(byte, line);
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::typechecker::TypeChecker;
    use crate::vm::value::Value;
    use crate::vm::VM;

    #[test]
    fn test_expressions() {
        let src = "7 + 3 * 2 == 1;";
        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        typecheker.check(&mut ast).expect("Failed to typecheck");
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&ast);
        let mut vm = VM::new();
        vm.chunk = chunk;
        let val = vm.run();
        assert_eq!(val, Value::Boolean(false));
    }

    #[test]
    fn test_cmp() {
        let src = "7 >= 1;";
        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        typecheker.check(&mut ast).expect("Failed to typecheck");
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&ast);
        let mut vm = VM::new();
        vm.chunk = chunk;
        let val = vm.run();
        assert_eq!(val, Value::Boolean(true));
    }
    #[test]
    fn test_complex() {
        let src = "let a = 1;
        if a < 10 {
           a;
        }
        else {
            a + 10;
        }
        ";

        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        typecheker.check(&mut ast).expect("Failed to typecheck");
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&ast);
        let mut vm = VM::new();
        vm.chunk = chunk;
        let val = vm.run();
    }
}
