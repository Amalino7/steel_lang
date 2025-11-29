pub mod analysis;

use crate::compiler::analysis::{AnalysisInfo, ResolvedVar};
use crate::parser::ast::{Expr, Literal, Stmt};
use crate::token::TokenType;
use crate::vm::bytecode::{Chunk, Opcode};
use crate::vm::gc::GarbageCollector;
use crate::vm::value::{Function, Value};

pub struct Compiler<'a> {
    function: Function,
    analysis_info: &'a AnalysisInfo,
    gc: &'a mut GarbageCollector,
}

impl<'a> Compiler<'a> {
    pub fn new(
        analysis_info: &'a AnalysisInfo,
        name: String,
        gc: &'a mut GarbageCollector,
    ) -> Self {
        Self {
            function: Function::new(name, 0, Chunk::new()),
            analysis_info,
            gc,
        }
    }
    fn chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    pub fn compile(mut self, statements: &[Stmt]) -> Function {
        // Compile functions first
        for stmt in statements {
            if let Stmt::Function { .. } = stmt {
                self.compile_stmt(stmt);
            }
        }

        for stmt in statements {
            if let Stmt::Function { .. } = stmt {
                continue;
            }
            self.compile_stmt(stmt);
        }

        self.chunk().write_constant(Value::Nil, 0);
        self.emit_op(Opcode::Return, 0);
        self.function
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
                id,
                ..
            } => {
                self.compile_expr(value);

                let var_ctx = self
                    .analysis_info
                    .resolved_vars
                    .get(id)
                    .expect("Variable not found");

                match var_ctx {
                    ResolvedVar::Local(_) => {
                        // there is no need to set the variable it is already in the right slot
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, identifier.line);
                        self.emit_byte(*idx as u8, identifier.line);

                        self.emit_op(Opcode::Pop, identifier.line);
                    }
                    ResolvedVar::Closure(idx) => {
                        self.emit_op(Opcode::SetCapture, identifier.line);
                        self.emit_byte(*idx as u8, identifier.line);
                        self.emit_op(Opcode::Pop, identifier.line);
                    }
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
                self.compile_expr(condition);
                let then_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.compile_stmt(then_branch);

                let else_jump = self.emit_jump(Opcode::Jump, line);
                self.patch_jump(then_jump);

                self.emit_op(Opcode::Pop, line);

                if let Some(else_branch) = else_branch {
                    self.compile_stmt(else_branch);
                }

                self.patch_jump(else_jump);
            }
            Stmt::While { condition, body } => {
                let line = condition.get_line();

                let loop_start = self.chunk().instructions.len();
                self.compile_expr(condition);

                let exit_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.compile_stmt(body);
                self.emit_jump_back(loop_start, body.get_line());

                self.patch_jump(exit_jump);
                self.emit_op(Opcode::Pop, body.get_line());
            }
            Stmt::Function {
                name,
                params,
                body,
                type_: _type_,
                id,
            } => {
                let mut func_compiler =
                    Compiler::new(self.analysis_info, name.lexeme.to_string(), self.gc);
                func_compiler.function.arity = params.len();

                let compiled_fn = func_compiler.compile(body);
                let constant = Value::Function(self.gc.alloc(compiled_fn));
                self.chunk().write_constant(constant, name.line);

                let var_ctx = self
                    .analysis_info
                    .resolved_vars
                    .get(id)
                    .expect("Variable not found");

                // Add captures
                if let Some(captures) = self.analysis_info.captures.get(id) {
                    // Reverse captures to make sure they are in the right order
                    for capture in captures.iter().rev() {
                        self.emit_var_access(capture, name.line)
                    }
                    self.emit_op(Opcode::MakeClosure, name.line);
                    self.emit_byte(captures.len() as u8, name.line);
                }

                match var_ctx {
                    ResolvedVar::Local(_) => {
                        // there is no need to set the variable it is already in the right slot
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, name.line);
                        self.emit_byte(*idx as u8, name.line);

                        self.emit_op(Opcode::Pop, name.line);
                    }
                    ResolvedVar::Closure(idx) => {
                        self.emit_op(Opcode::SetCapture, name.line);
                        self.emit_byte(*idx as u8, name.line);
                        self.emit_op(Opcode::Pop, name.line);
                    }
                }
            }
            Stmt::Return(val) => {
                self.compile_expr(val);
                self.emit_op(Opcode::Return, val.get_line());
            }
        }
    }

    fn emit_var_access(&mut self, var_ctx: &ResolvedVar, line: usize) {
        match var_ctx {
            ResolvedVar::Local(idx) => {
                self.emit_op(Opcode::GetLocal, line);
                self.emit_byte(*idx as u8, line);
            }
            ResolvedVar::Global(idx) => {
                self.emit_op(Opcode::GetGlobal, line);
                self.emit_byte(*idx as u8, line);
            }
            ResolvedVar::Closure(idx) => {
                self.emit_op(Opcode::GetCapture, line);
                self.emit_byte(*idx as u8, line);
            }
        }
    }

    fn emit_jump(&mut self, op: Opcode, line: usize) -> usize {
        self.emit_op(op, line);
        self.emit_byte(0xff, line); // Placeholder
        self.emit_byte(0xff, line); // Placeholder
        self.chunk().instructions.len() - 2
    }
    fn emit_jump_back(&mut self, start: usize, line: usize) {
        self.emit_op(Opcode::JumpBack, line);
        let offset = self.chunk().instructions.len() - start + 2;
        if offset > u16::MAX as usize {
            panic!("Loop body too large!");
        }
        self.emit_byte(((offset >> 8) & 0xff) as u8, line);
        self.emit_byte((offset & 0xff) as u8, line);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.chunk().instructions.len() - offset - 2;
        if jump > u16::MAX as usize {
            panic!("Too much code to jump over!");
        }
        self.chunk().instructions[offset] = ((jump >> 8) & 0xff) as u8;
        self.chunk().instructions[offset + 1] = (jump & 0xff) as u8;
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
                    TokenType::Concat => {
                        self.emit_op(Opcode::Concat, line);
                    }
                    _ => panic!("Unknown binary operator"),
                }
            }
            Expr::Grouping { expression } => {
                self.compile_expr(expression);
            }
            Expr::Literal { literal, line } => match literal {
                Literal::Number(n) => self.chunk().write_constant(Value::Number(*n), *line),
                Literal::String(s) => {
                    let str = self.gc.alloc(s.to_string());
                    self.chunk().write_constant(Value::String(str), *line)
                }
                Literal::Boolean(b) => {
                    self.chunk().write_constant(Value::Boolean(*b), *line);
                }
                Literal::Void => {
                    self.chunk().write_constant(Value::Nil, *line);
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
            Expr::Variable { name, id } => {
                let var_ctx = self
                    .analysis_info
                    .resolved_vars
                    .get(id)
                    .expect("Variable not found");

                self.emit_var_access(var_ctx, name.line);
            }
            Expr::Assignment {
                identifier,
                value,
                id,
            } => {
                self.compile_expr(value);
                let var_ctx = self
                    .analysis_info
                    .resolved_vars
                    .get(id)
                    .expect("Variable not found");

                match var_ctx {
                    ResolvedVar::Local(idx) => {
                        self.emit_op(Opcode::SetLocal, identifier.line);
                        self.emit_byte(*idx as u8, identifier.line);
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, identifier.line);
                        self.emit_byte(*idx as u8, identifier.line);
                    }
                    ResolvedVar::Closure(idx) => {
                        self.emit_op(Opcode::SetCapture, identifier.line);
                        self.emit_byte(*idx as u8, identifier.line);
                    }
                }
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
                self.compile_expr(callee);
                for arg in arguments {
                    self.compile_expr(arg);
                }
                self.emit_op(Opcode::Call, callee.get_line());
                self.emit_byte(
                    arguments.len() as u8,
                    arguments
                        .last()
                        .map(|e| e.get_line())
                        .unwrap_or(callee.get_line()),
                );
            }
        }
    }

    fn emit_op(&mut self, op: Opcode, line: usize) {
        self.chunk().write_op(op as u8, line);
    }
    fn emit_byte(&mut self, byte: u8, line: usize) {
        self.chunk().write_op(byte, line);
    }
}
