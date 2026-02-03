pub mod analysis;

use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Literal;
use crate::typechecker::type_ast::{
    BinaryOp, ExprKind, LogicalOp, MatchCase, StmtKind, TypedBinding, TypedExpr, TypedStmt, UnaryOp,
};
use crate::vm::bytecode::{Chunk, Opcode};
use crate::vm::gc::GarbageCollector;
use crate::vm::value::{Function, Value};

pub struct Compiler<'a> {
    function: Function,
    gc: &'a mut GarbageCollector,
}

impl<'a> Compiler<'a> {
    pub fn new(name: String, gc: &'a mut GarbageCollector) -> Self {
        Self {
            function: Function::new(name, Chunk::new()),
            gc,
        }
    }
    fn chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    pub fn compile(mut self, typed_ast: &TypedStmt) -> Function {
        self.compile_stmt(typed_ast);

        self.chunk().write_constant(Value::Nil, 0);
        self.emit_op(Opcode::Return, 0);
        self.function
    }
    fn compile_stmt(&mut self, stmt: &TypedStmt) {
        match &stmt.kind {
            StmtKind::Expression(e) => {
                self.compile_expr(e);
                self.emit_op(Opcode::Pop, stmt.span.line);
            }
            StmtKind::Let { binding, value } => {
                self.compile_expr(value);

                self.compile_binding(binding, stmt.span.line);
            }
            StmtKind::Blank => {}
            StmtKind::Block {
                body: stmts,
                reserved,
            } => {
                if *reserved != 0 {
                    self.emit_op(Opcode::Reserve, stmt.span.line);
                    self.emit_byte(*reserved as u8, stmt.span.line);
                }
                for stmt in stmts {
                    self.compile_stmt(stmt);
                }
            }
            StmtKind::If {
                condition,
                then_branch,
                else_branch,
                typed_refinements,
            } => {
                let line = stmt.span.line;
                self.compile_expr(condition);
                let then_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.emit_refinement(&typed_refinements.true_path, line);
                self.compile_stmt(then_branch);

                let else_jump = self.emit_jump(Opcode::Jump, line);
                self.patch_jump(then_jump);

                self.emit_op(Opcode::Pop, line);

                if let Some(else_branch) = else_branch {
                    self.emit_refinement(&typed_refinements.else_path, line);
                    self.compile_stmt(else_branch);
                }

                self.patch_jump(else_jump);
                self.emit_refinement(&typed_refinements.after_path, line);
            }
            StmtKind::Match { value, cases } => {
                self.compile_expr(value);

                let mut end_jumps = Vec::new();

                for case in cases.iter() {
                    match case {
                        MatchCase::Variable { binding, body } => {
                            self.compile_binding(binding, body.span.line);
                            self.compile_stmt(body);
                            end_jumps.push(self.emit_jump(Opcode::Jump, stmt.span.line));
                        }
                        MatchCase::Named {
                            variant_idx,
                            binding,
                            body,
                        } => {
                            // Check if the tag matches the case variant
                            self.emit_op(Opcode::Dup, stmt.span.line);
                            self.emit_op(Opcode::CheckEnumTag, stmt.span.line);
                            self.emit_byte(*variant_idx as u8, stmt.span.line);

                            // Similar to if
                            let next_case_jump =
                                self.emit_jump(Opcode::JumpIfFalse, stmt.span.line);
                            self.emit_op(Opcode::Pop, stmt.span.line);

                            self.emit_op(Opcode::DestructureEnum, stmt.span.line);
                            self.compile_binding(binding, stmt.span.line);

                            self.compile_stmt(body);
                            end_jumps.push(self.emit_jump(Opcode::Jump, stmt.span.line));

                            self.patch_jump(next_case_jump);
                            self.emit_op(Opcode::Pop, stmt.span.line);
                        }
                    }
                }

                // Pop original enum
                self.emit_op(Opcode::Pop, stmt.span.line);
                for jump in end_jumps {
                    self.patch_jump(jump);
                }
            }
            StmtKind::While { condition, body } => {
                let line = stmt.span.line;

                let loop_start = self.chunk().instructions.len();
                self.compile_expr(condition);

                let exit_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                self.emit_op(Opcode::Pop, line);

                self.compile_stmt(body);
                self.emit_jump_back(loop_start, body.span.line);

                self.patch_jump(exit_jump);
                self.emit_op(Opcode::Pop, body.span.line);
            }
            StmtKind::Impl { methods, vtables } => {
                for method in methods {
                    self.compile_stmt(method);
                }

                for vtable in vtables.iter() {
                    for method_loc in vtable.iter().rev() {
                        self.emit_var_access(method_loc, stmt.span.line);
                    }

                    self.emit_op(Opcode::MakeVTable, stmt.span.line);
                    self.emit_byte(vtable.len() as u8, stmt.span.line);
                }
            }
            StmtKind::Function {
                target,
                name,
                body,
                captures,
            } => {
                let line = stmt.span.line;
                let func_compiler = Compiler::new(name.to_string(), self.gc);

                let compiled_fn = func_compiler.compile(body);
                let constant = Value::Function(self.gc.alloc(compiled_fn));
                self.chunk().write_constant(constant, line as usize);

                // Add captures
                // Reverse captures to make sure they are in the right order
                for capture in captures.iter().rev() {
                    self.emit_var_access(capture, line)
                }
                if !captures.is_empty() {
                    self.emit_op(Opcode::MakeClosure, line);
                    self.emit_byte(captures.len() as u8, line);
                }

                match target {
                    ResolvedVar::Local(idx) => {
                        self.emit_op(Opcode::SetLocal, stmt.span.line);
                        self.emit_byte(*idx, stmt.span.line);

                        self.emit_op(Opcode::Pop, stmt.span.line);
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, line);
                        self.emit_byte(*idx as u8, line);

                        self.emit_op(Opcode::Pop, line);
                    }
                    ResolvedVar::Closure(_) => {
                        unreachable!("Closures shouldn't be assigned to")
                    }
                }
            }
            StmtKind::Return(val) => {
                self.compile_expr(val);
                self.emit_op(Opcode::Return, stmt.span.line);
            }
            StmtKind::Global {
                stmts, reserved, ..
            } => {
                if *reserved != 0 {
                    self.emit_op(Opcode::Reserve, stmt.span.line);
                    self.emit_byte(*reserved as u8, stmt.span.line);
                }
                for s in stmts {
                    match &s.kind {
                        StmtKind::Function { .. } => self.compile_stmt(s),
                        StmtKind::Impl { .. } => {
                            self.compile_stmt(s);
                        }
                        _ => {}
                    }
                }

                // Second: compile the rest
                for s in stmts {
                    match &s.kind {
                        StmtKind::Function { .. } | StmtKind::Impl { .. } => continue,
                        _ => self.compile_stmt(s),
                    }
                }
            }
            StmtKind::EnumDecl { .. } => {}
            StmtKind::StructDecl { .. } => {}
        }
    }

    fn emit_refinement(&mut self, typed_refinements: &[(ResolvedVar, ResolvedVar)], line: u32) {
        for refinement in typed_refinements {
            self.emit_var_access(&refinement.0, line);
            self.emit_op(Opcode::DestructureEnum, line);
            self.emit_set_var(&refinement.1, line);
            self.emit_op(Opcode::Pop, line);
        }
    }

    fn compile_binding(&mut self, binding: &TypedBinding, line: u32) {
        match binding {
            TypedBinding::Variable(var) => {
                self.emit_set_var(var, line);
                self.emit_op(Opcode::Pop, line);
            }
            TypedBinding::Ignored => {
                self.emit_op(Opcode::Pop, line);
            }
            TypedBinding::Tuple(bindings) => {
                for (idx, binding) in bindings.iter().enumerate() {
                    self.emit_op(Opcode::Dup, line);
                    self.emit_op(Opcode::GetField, line);
                    self.emit_byte(idx as u8, line);
                    self.compile_binding(binding, line);
                }
                self.emit_op(Opcode::Pop, line);
            }
            TypedBinding::Struct(bindings) => {
                for (idx, binding) in bindings {
                    self.emit_op(Opcode::Dup, line);
                    self.emit_op(Opcode::GetField, line);
                    self.emit_byte(*idx, line);
                    self.compile_binding(binding, line);
                }
                self.emit_op(Opcode::Pop, line);
            }
        }
    }
    fn emit_set_var(&mut self, var_ctx: &ResolvedVar, line: u32) {
        match var_ctx {
            ResolvedVar::Local(idx) => {
                self.emit_op(Opcode::SetLocal, line);
                self.emit_byte(*idx, line);
            }
            ResolvedVar::Global(idx) => {
                self.emit_op(Opcode::SetGlobal, line);
                self.emit_byte(*idx as u8, line);
            }
            ResolvedVar::Closure(_) => {
                unreachable!("Cannot set capture")
            }
        }
    }

    fn emit_var_access(&mut self, var_ctx: &ResolvedVar, line: u32) {
        match var_ctx {
            ResolvedVar::Local(idx) => {
                self.emit_op(Opcode::GetLocal, line);
                self.emit_byte(*idx, line);
            }
            ResolvedVar::Global(idx) => {
                self.emit_op(Opcode::GetGlobal, line);
                self.emit_byte(*idx as u8, line);
            }
            ResolvedVar::Closure(idx) => {
                self.emit_op(Opcode::GetCapture, line);
                self.emit_byte(*idx, line);
            }
        }
    }

    fn emit_jump(&mut self, op: Opcode, line: u32) -> usize {
        self.emit_op(op, line);
        self.emit_byte(0xff, line); // Placeholder
        self.emit_byte(0xff, line); // Placeholder
        self.chunk().instructions.len() - 2
    }
    fn emit_jump_back(&mut self, start: usize, line: u32) {
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

    fn compile_expr(&mut self, expr: &TypedExpr) {
        let line = expr.span.line;
        match &expr.kind {
            ExprKind::Binary {
                left,
                right,
                operator,
            } => {
                self.compile_expr(left);
                self.compile_expr(right);
                match operator {
                    BinaryOp::Concat => self.emit_op(Opcode::Concat, line),
                    BinaryOp::Add => self.emit_op(Opcode::Add, line),
                    BinaryOp::Subtract => self.emit_op(Opcode::Subtract, line),
                    BinaryOp::Multiply => self.emit_op(Opcode::Multiply, line),
                    BinaryOp::Divide => self.emit_op(Opcode::Divide, line),
                    BinaryOp::LessString => self.emit_op(Opcode::LessString, line),
                    BinaryOp::LessNumber => self.emit_op(Opcode::LessNumber, line),
                    BinaryOp::GreaterString => self.emit_op(Opcode::GreaterString, line),
                    BinaryOp::GreaterNumber => self.emit_op(Opcode::GreaterNumber, line),
                    BinaryOp::GreaterEqualString => {
                        self.emit_op(Opcode::LessString, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::GreaterEqualNumber => {
                        self.emit_op(Opcode::LessNumber, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::LessEqualNumber => {
                        self.emit_op(Opcode::GreaterNumber, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::LessEqualString => {
                        self.emit_op(Opcode::GreaterString, line);
                        self.emit_op(Opcode::Not, line);
                    }
                    BinaryOp::EqualEqualNumber => self.emit_op(Opcode::EqualNumber, line),
                    BinaryOp::EqualEqualString => self.emit_op(Opcode::EqualString, line),
                    BinaryOp::EqualEqual => self.emit_op(Opcode::Equal, line),
                }
            }
            ExprKind::Is {
                target,
                variant_idx,
                ..
            } => {
                self.compile_expr(target);
                self.emit_op(Opcode::CheckEnumTag, expr.span.line);
                self.emit_byte(*variant_idx as u8, expr.span.line);
                // target !!
            }
            ExprKind::Literal(literal) => match literal {
                Literal::Number(n) => self
                    .chunk()
                    .write_constant(Value::Number(*n), line as usize),
                Literal::String(s) => {
                    let str = self.gc.alloc(s.to_string());
                    self.chunk()
                        .write_constant(Value::String(str), line as usize)
                }
                Literal::Boolean(b) => {
                    self.chunk()
                        .write_constant(Value::Boolean(*b), line as usize);
                }
                Literal::Void => {
                    self.chunk().write_constant(Value::Nil, line as usize);
                }
                Literal::Nil => self.emit_op(Opcode::Nil, line),
            },
            ExprKind::Try { operand } => {
                self.compile_expr(operand);
                self.emit_op(Opcode::Dup, expr.span.line);
                self.emit_op(Opcode::CheckEnumTag, expr.span.line);
                self.emit_byte(0, expr.span.line);
                let jump = self.emit_jump(Opcode::JumpIfFalse, expr.span.line);
                // if ok destructure
                self.emit_op(Opcode::Pop, expr.span.line);
                self.emit_op(Opcode::DestructureEnum, expr.span.line);
                let exit_jump = self.emit_jump(Opcode::Jump, expr.span.line);
                self.patch_jump(jump);
                self.emit_op(Opcode::Pop, expr.span.line); // condition
                self.emit_op(Opcode::Return, expr.span.line); // return enum
                self.patch_jump(exit_jump);
            }
            ExprKind::Unary { operator, operand } => {
                self.compile_expr(operand);
                match operator {
                    UnaryOp::Negate => self.emit_op(Opcode::Negate, line),
                    UnaryOp::Not => self.emit_op(Opcode::Not, line),
                    UnaryOp::Unwrap => self.emit_op(Opcode::Unwrap, line),
                }
            }
            ExprKind::GetVar(resolved, _) => self.emit_var_access(resolved, line),
            ExprKind::Assign { target, value } => {
                self.compile_expr(value);

                match target {
                    ResolvedVar::Local(idx) => {
                        self.emit_op(Opcode::SetLocal, line);
                        self.emit_byte(*idx, line);
                    }
                    ResolvedVar::Global(idx) => {
                        self.emit_op(Opcode::SetGlobal, line);
                        self.emit_byte(*idx as u8, line);
                    }
                    ResolvedVar::Closure(_) => {
                        unreachable!("Closures shouldn't be assigned to")
                    }
                }
            }
            ExprKind::Logical {
                left,
                operator,
                right,
                typed_refinements,
            } => {
                self.compile_expr(left);
                match operator {
                    LogicalOp::Or => {
                        // TODO Add jump if true for better performance
                        let else_jump = self.emit_jump(Opcode::JumpIfFalse, line);
                        let end_jump = self.emit_jump(Opcode::Jump, line);
                        self.patch_jump(else_jump);
                        self.emit_op(Opcode::Pop, line);
                        self.emit_refinement(typed_refinements, line);
                        self.compile_expr(right);
                        self.patch_jump(end_jump);
                    }
                    LogicalOp::And => {
                        let short_circuit = self.emit_jump(Opcode::JumpIfFalse, line);
                        self.emit_refinement(typed_refinements, line);
                        self.emit_op(Opcode::Pop, line);
                        self.compile_expr(right);
                        self.patch_jump(short_circuit);
                    }
                    LogicalOp::Coalesce => {
                        let jump = self.emit_jump(Opcode::JumpIfNotNil, line);
                        self.emit_op(Opcode::Pop, line);
                        self.compile_expr(right);
                        self.patch_jump(jump)
                    }
                }
            }
            ExprKind::Call {
                callee,
                arguments,
                safe,
            } => {
                if let ExprKind::MethodGet {
                    object,
                    method,
                    safe,
                } = &callee.kind
                {
                    self.emit_var_access(method, line);
                    self.compile_expr(object);

                    let jump = if *safe {
                        self.emit_jump(Opcode::JumpIfNil, line)
                    } else {
                        0
                    };

                    for arg in arguments {
                        self.compile_expr(arg);
                    }

                    self.emit_op(Opcode::Call, line);
                    self.emit_byte((arguments.len() + 1) as u8, line);
                    if *safe {
                        let escape_jump = self.emit_jump(Opcode::Jump, line);
                        self.patch_jump(jump);
                        self.emit_op(Opcode::Pop, line);
                        self.emit_op(Opcode::Pop, line);
                        self.emit_op(Opcode::Nil, line);
                        self.patch_jump(escape_jump);
                    }
                } else if let ExprKind::InterfaceMethodGet {
                    object,
                    method_index,
                    safe,
                } = &callee.kind
                {
                    self.compile_expr(object);
                    let jump = if *safe {
                        self.emit_jump(Opcode::JumpIfNil, line)
                    } else {
                        0
                    };
                    self.emit_op(Opcode::GetInterfaceMethod, line);
                    self.emit_byte(*method_index, line);

                    for arg in arguments {
                        self.compile_expr(arg);
                    }
                    self.emit_op(Opcode::Call, line);
                    self.emit_byte((arguments.len() + 1) as u8, line);

                    if *safe {
                        self.patch_jump(jump);
                    }
                } else {
                    self.compile_expr(callee);
                    let jump = if *safe {
                        self.emit_jump(Opcode::JumpIfNil, line)
                    } else {
                        0
                    };

                    for arg in arguments {
                        self.compile_expr(arg);
                    }
                    self.emit_op(Opcode::Call, callee.span.line);
                    self.emit_byte(
                        arguments.len() as u8,
                        arguments
                            .last()
                            .map(|e| e.span.line)
                            .unwrap_or(callee.span.line),
                    );

                    if *safe {
                        self.patch_jump(jump);
                    }
                }
            }
            ExprKind::StructInit { args, name } => {
                let str_name = self.gc.alloc(name.to_string());

                self.chunk()
                    .write_constant(Value::String(str_name), line as usize);
                for arg in args.iter().rev() {
                    self.compile_expr(arg);
                }
                self.emit_op(Opcode::StructAlloc, line);
                self.emit_byte(args.len() as u8, line);
            }

            ExprKind::GetField {
                object,
                index,
                safe,
            } => {
                self.compile_expr(object);
                if *safe {
                    let jump = self.emit_jump(Opcode::JumpIfNil, line);
                    self.emit_op(Opcode::GetField, line);
                    self.emit_byte(*index, line);
                    self.patch_jump(jump);
                } else {
                    self.emit_op(Opcode::GetField, line);
                    self.emit_byte(*index, line);
                }
            }
            ExprKind::SetField {
                object,
                index,
                value,
                safe,
            } => {
                self.compile_expr(value);
                if *safe {
                    self.compile_expr(object);
                    let jump_nil = self.emit_jump(Opcode::JumpIfNil, line);
                    self.emit_op(Opcode::SetField, line);
                    self.emit_byte(*index, line);
                    let escape_jump = self.emit_jump(Opcode::Jump, line);

                    self.patch_jump(jump_nil);
                    self.emit_op(Opcode::Pop, line); // Pop object
                    self.patch_jump(escape_jump);
                } else {
                    self.compile_expr(object);
                    self.emit_op(Opcode::SetField, line);
                    self.emit_byte(*index, line);
                }
            }
            ExprKind::MethodGet {
                object,
                method,
                safe,
            } => {
                self.compile_expr(object);
                if *safe {
                    let jump = self.emit_jump(Opcode::JumpIfNil, line);
                    self.emit_var_access(method, line);
                    self.emit_op(Opcode::BindMethod, line);
                    self.patch_jump(jump);
                } else {
                    self.emit_var_access(method, line);
                    self.emit_op(Opcode::BindMethod, line);
                }
            }

            ExprKind::InterfaceUpcast {
                expr: inner,
                vtable_idx,
                ..
            } => {
                self.compile_expr(inner);
                self.emit_op(Opcode::MakeInterfaceObj, line);
                self.emit_byte(*vtable_idx as u8, line);
            }

            ExprKind::InterfaceMethodGet {
                object,
                method_index,
                safe,
            } => {
                self.compile_expr(object);
                if *safe {
                    let jump = self.emit_jump(Opcode::JumpIfNil, line);
                    self.emit_op(Opcode::InterfaceBindMethod, line);
                    self.emit_byte(*method_index, line);
                    self.patch_jump(jump);
                } else {
                    self.emit_op(Opcode::InterfaceBindMethod, line);
                    self.emit_byte(*method_index, line);
                }
            }
            ExprKind::EnumInit {
                enum_name,
                variant_idx,
                value,
            } => {
                let enum_name = self.gc.alloc(enum_name.to_string());
                self.chunk()
                    .write_constant(Value::String(enum_name), line as usize);
                self.compile_expr(value); // Simple like 8 or complex like struct alloc.
                self.emit_op(Opcode::EnumAlloc, line);
                self.emit_byte(*variant_idx as u8, line);
            }
            ExprKind::Tuple { elements } => {
                let str_name = self.gc.alloc(format!("Tuple{}", elements.len()));

                self.chunk()
                    .write_constant(Value::String(str_name), line as usize);
                for element in elements.iter().rev() {
                    self.compile_expr(element);
                }
                self.emit_op(Opcode::StructAlloc, line);
                self.emit_byte(elements.len() as u8, line);
            }
            ExprKind::List { elements } => {
                for element in elements.iter().rev() {
                    self.compile_expr(element);
                }
                self.emit_op(Opcode::MakeList, line);
                self.emit_byte(elements.len() as u8, line);
            }
            ExprKind::GetIndex {
                object,
                index,
                safe,
            } => {
                self.compile_expr(object);
                if *safe {
                    let jump = self.emit_jump(Opcode::JumpIfNil, line);
                    self.compile_expr(index);
                    self.emit_op(Opcode::GetIndex, line);
                    self.patch_jump(jump);
                } else {
                    self.compile_expr(index);
                    self.emit_op(Opcode::GetIndex, line);
                }
            }
            ExprKind::SetIndex {
                object,
                index,
                value,
                safe,
            } => {
                self.compile_expr(value);
                if *safe {
                    self.compile_expr(object);
                    let jump_nil = self.emit_jump(Opcode::JumpIfNil, line);
                    self.compile_expr(index);
                    self.emit_op(Opcode::SetIndex, line);
                    let escape_jump = self.emit_jump(Opcode::Jump, line);

                    self.patch_jump(jump_nil);
                    self.emit_op(Opcode::Pop, line); // Pop object
                    self.patch_jump(escape_jump);
                } else {
                    self.compile_expr(object);
                    self.compile_expr(index);
                    self.emit_op(Opcode::SetIndex, line);
                }
            }
        }
    }

    fn emit_op(&mut self, op: Opcode, line: u32) {
        self.chunk().write_op(op as u8, line as usize);
    }
    fn emit_byte(&mut self, byte: u8, line: u32) {
        self.chunk().write_op(byte, line as usize);
    }
}
