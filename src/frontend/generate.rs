//! Main module for generating IR from AST (frontend)

use super::ast::*;
use super::builtin::setup_builtins;
use super::env::Environment;
use super::eval::Eval;
use super::loops::*;
use super::symbol::*;
use super::AstError;
use crate::utils::namer::{global_ident, normal_ident};
use koopa::ir::builder::*;
use koopa::ir::{BinaryOp, FunctionData, Type, TypeKind, Value};

/// Trait which should be implemented by AST types to generate IR
///
/// Return a value of type `T` if needed
pub trait GenerateIr {
    type IrTarget;

    /// Generate IR on the given env
    ///
    /// # Errors
    /// Returns an [`AstError`] if the generation fails
    fn generate_on(&self, env: &mut Environment) -> Result<Self::IrTarget, AstError>;
}

impl GenerateIr for CompUnit {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        setup_builtins(&mut env.ctx);

        for comp_item in &self.comp_items {
            match comp_item {
                CompItem::FuncDef(func_def) => func_def.generate_on(env)?,
                CompItem::Decl(decl) => decl.generate_on(env)?,
            }
        }
        Ok(())
    }
}

impl GenerateIr for FuncDef {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        env.table.enter_scope();

        let mut p_types = vec![];
        let mut params = vec![];
        for param in &self.params {
            let symbol = env.table.new_symbol(param)?;
            let ty = symbol.get_type(param.b_type.into())?;
            p_types.push(ty.clone());
            params.push((Some(global_ident(symbol.ident())), ty));
        }

        let func = FunctionData::with_param_names(
            global_ident(&self.ident),
            params,
            self.func_type.into(),
        );

        let func = env.ctx.program.new_func(func);
        env.ctx.func(func);
        env.ctx.create_block(Some("%entry".into()));

        // Add parameters to the symbol table
        for (i, param) in self.params.iter().enumerate() {
            let value = env.ctx.func_data().params()[i];
            let alloc = env.ctx.alloc_and_store(value, p_types[i].clone());
            env.ctx.set_value_name(alloc, normal_ident(&param.ident));
            env.table.set_alloc(&param.ident, alloc)?;
        }
        self.block.generate_on(env)?;
        env.table.exit_scope();

        let ret = env.ctx.local_val().ret(None);
        env.ctx.add_inst(ret);
        env.ctx.block = None;
        env.ctx.func = None;
        Ok(())
    }
}

impl GenerateIr for Decl {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(env),
            Decl::VarDecl(var_decl) => var_decl.generate_on(env),
        }
    }
}

impl GenerateIr for Block {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        if self.block_items.is_empty() {
            return Ok(());
        }

        env.ctx.new_block(None, true);
        env.table.enter_scope();

        for block_item in &self.block_items {
            match block_item {
                BlockItem::Stmt(stmt) => {
                    stmt.generate_on(env)?;
                    // end block if the block item is a
                    // return/break/continue statement
                    if matches!(stmt, Stmt::Return(_) | Stmt::Break(_) | Stmt::Continue(_)) {
                        break;
                    }
                }
                BlockItem::Decl(decl) => decl.generate_on(env)?,
            };
        }
        env.table.exit_scope();
        env.ctx.new_block(None, true);
        Ok(())
    }
}

impl GenerateIr for Stmt {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        match self {
            Stmt::Empty => Ok(()),
            Stmt::Exp(exp) => exp.generate_on(env).map(|_| ()),
            Stmt::Block(stmt) => stmt.generate_on(env),
            Stmt::Assign(stmt) => stmt.generate_on(env),
            Stmt::If(stmt) => stmt.generate_on(env),
            Stmt::While(stmt) => stmt.generate_on(env),
            Stmt::Return(stmt) => stmt.generate_on(env),
            Stmt::Break(stmt) => stmt.generate_on(env),
            Stmt::Continue(stmt) => stmt.generate_on(env),
        }
    }
}

impl GenerateIr for LVal {
    type IrTarget = Value;

    /// Returns the address of the left value
    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        let mut indexes = vec![];
        for exp in &self.array_index {
            indexes.push(exp.generate_on(env)?.value);
        }

        let symbol = env.table.lookup_or(&self.ident)?;

        if !symbol.is_single() {
            let addr = symbol.index(&indexes, &mut env.ctx)?;
            Ok(addr)
        } else if !indexes.is_empty() {
            return Err(AstError::TypeError("Cannot index a single value!".into()));
        } else {
            return symbol.get_alloc();
        }
    }
}

impl GenerateIr for Return {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(env)?.value),
            None => None,
        };
        let ret = env.ctx.local_val().ret(ret);
        env.ctx.add_inst(ret);
        Ok(())
    }
}

impl GenerateIr for Assign {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let alloc = self.l_val.generate_on(env)?;
        let value = self.exp.generate_on(env)?.value;
        let store = env.ctx.local_val().store(value, alloc);
        env.ctx.add_inst(store);
        Ok(())
    }
}

impl GenerateIr for If {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let cond = self.cond.generate_on(env)?.value;
        let base_bb = env.ctx.block.unwrap();

        let true_bb = env.ctx.new_block(Some("%then".into()), false);
        self.stmt.generate_on(env)?;
        // always create a new ending block for the then statement
        // even if it is not a block statement
        let true_end_bb = env.ctx.new_block(Some("%then_end".into()), true);

        if let Some(else_stmt) = &self.else_stmt {
            let false_bb = env.ctx.new_block(Some("%else".into()), false);
            else_stmt.generate_on(env)?;
            let end_bb = env.ctx.new_block(None, true);

            // branch to true/false
            let branch = env.ctx.local_val().branch(cond, true_bb, false_bb);
            // true jump to end
            let jump = env.ctx.local_val().jump(end_bb);
            add_inst!(env.ctx.func_data(), base_bb, branch);
            add_inst!(env.ctx.func_data(), true_end_bb, jump);
        } else {
            let end_bb = env.ctx.new_block(None, true);
            // branch to true/end
            let branch = env.ctx.local_val().branch(cond, true_bb, end_bb);
            add_inst!(env.ctx.func_data(), base_bb, branch);
        };

        Ok(())
    }
}

impl GenerateIr for While {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let block = env.ctx.block.unwrap();
        let cond_bb = if env.ctx.in_entry() {
            // The entry basic block is not allowed to have predecessors
            // so create a new block for the condition
            let cond_bb = env.ctx.new_block(Some("%cond".into()), false);
            let jump = env.ctx.local_val().jump(cond_bb);
            add_inst!(env.ctx.func_data(), block, jump);
            cond_bb
        } else {
            env.ctx.new_block(Some("%cond".into()), true)
        };

        let cond = self.cond.generate_on(env)?.value;
        let cond_end_bb = env.ctx.block.unwrap();
        // use a temporary end block to serve for "break"
        let temp_end_bb = env.ctx.new_block(None, false);

        env.loops.push(Loop {
            cond_bb,
            end_bb: temp_end_bb,
        });
        let body_bb = env.ctx.new_block(Some("%body".into()), false);
        self.stmt.generate_on(env)?;

        // jump back to the condition
        if !env.ctx.if_block_ended(&env.ctx.block.unwrap()) {
            let jump = env.ctx.local_val().jump(cond_bb);
            env.ctx.add_inst(jump);
        }

        let end_bb = env.ctx.new_block(None, true);
        // branch to body/end
        let branch = env.ctx.local_val().branch(cond, body_bb, end_bb);
        // temp end block
        let jump = env.ctx.local_val().jump(end_bb);

        add_inst!(env.ctx.func_data(), cond_end_bb, branch);
        add_inst!(env.ctx.func_data(), temp_end_bb, jump);

        env.loops.pop();
        Ok(())
    }
}

impl GenerateIr for Break {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let end_bb = env
            .loops
            .last()
            .ok_or(AstError::LoopStackError("break".into()))?
            .end_bb;
        let jump = env.ctx.local_val().jump(end_bb);
        env.ctx.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr for Continue {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        let cond_bb = env
            .loops
            .last()
            .ok_or(AstError::LoopStackError("continue".into()))?
            .cond_bb;
        let jump = env.ctx.local_val().jump(cond_bb);
        env.ctx.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr for ConstDecl {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        for const_def in &self.const_defs {
            let symbol = env.table.new_symbol(const_def)?;
            let init = Some(Init::Const(&const_def.const_init_val));
            if !symbol.is_const() {
                panic!("ConstDecl::generate_on: variable should be in VarDecl");
            }
            symbol.gen_value(self.b_type.into(), init, env)?;
        }
        Ok(())
    }
}

impl GenerateIr for VarDecl {
    type IrTarget = ();

    fn generate_on(&self, env: &mut Environment) -> Result<(), AstError> {
        for var_def in &self.var_defs {
            let symbol = env.table.new_symbol(var_def)?;
            let init = var_def.init_val.as_ref().map(|init| Init::Var(init));
            if symbol.is_const() {
                panic!("VarDecl::generate_on: const variable should be in ConstDecl");
            }
            symbol.gen_value(self.b_type.into(), init, env)?;
        }
        Ok(())
    }
}

impl GenerateIr for LValAssign {
    type IrTarget = Value;

    fn generate_on(&self, env: &mut Environment) -> Result<Value, AstError> {
        let symbol = env.table.lookup_or(&self.ident)?;

        if symbol.is_const() {
            return Err(AstError::AssignError(self.ident.clone()));
        }

        let l_val = LVal {
            ident: self.ident.clone(),
            array_index: self.array_index.clone(),
        };
        l_val.generate_on(env)
    }
}

pub struct ExpResult {
    value: Value,
    side_effect: bool,
}

impl ExpResult {
    pub fn new(value: Value, side_effect: bool) -> Self {
        Self { value, side_effect }
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn has_effect(&self) -> bool {
        self.side_effect
    }

    pub fn safe(value: Value) -> Self {
        Self::new(value, false)
    }

    pub fn with_effect(value: Value) -> Self {
        Self::new(value, true)
    }

    pub fn are_safe(results: Vec<Self>) -> bool {
        results.iter().all(|result| !result.side_effect)
    }

    pub fn merge(value: Value, results: Vec<Self>) -> Self {
        Self::new(value, !Self::are_safe(results))
    }
}

impl GenerateIr for Exp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        // try to evaluate the expression at compile time
        if let Ok(value) = self.eval(&env.table) {
            return value.generate_on(env);
        }

        match self {
            Exp::LOrExp(l_or_exp) => l_or_exp.generate_on(env),
        }
    }
}

impl GenerateIr for ConstExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        // Const expressions must be evaluated at compile time
        self.eval(&env.table)?.generate_on(env)
    }
}

impl GenerateIr for Cond {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            Cond::LOrExp(exp) => exp.generate_on(env),
        }
    }
}

impl GenerateIr for LOrExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.generate_on(env),
            LOrExp::LOrOpExp(op_exp) => {
                // short circuit

                // base block
                let zero = env.ctx.local_val().integer(0);
                let lhs = op_exp.l_or_exp.generate_on(env)?;
                let base_bb = env.ctx.block.unwrap();

                // false block
                let false_bb = env.ctx.new_block(Some("%or_else".into()), false);
                let rhs = op_exp.l_and_exp.generate_on(env)?;

                if !rhs.has_effect() {
                    // if rhs has no side effect, we can use a simpler implementation
                    let value = env
                        .ctx
                        .local_val()
                        .binary(BinaryOp::Or, lhs.value, rhs.value);
                    env.ctx.add_inst(value);
                    let value = env.ctx.local_val().binary(BinaryOp::NotEq, value, zero);
                    env.ctx.add_inst(value);
                    let jump = env.ctx.local_val().jump(false_bb);
                    add_inst!(env.ctx.func_data(), base_bb, jump);
                    return Ok(ExpResult::merge(value, vec![lhs, rhs]));
                }

                let cur_bb = env.ctx.block.unwrap();
                env.ctx.block(base_bb);
                let l_binary = env.ctx.local_val().binary(BinaryOp::NotEq, lhs.value, zero);
                env.ctx.add_inst(l_binary);
                let result = env.ctx.alloc_and_store(l_binary, Type::get_i32());
                env.ctx.block(cur_bb);

                let r_binary = env.ctx.local_val().binary(BinaryOp::NotEq, zero, rhs.value);
                let cover = env.ctx.local_val().store(r_binary, result);
                env.ctx.add_inst(r_binary);
                env.ctx.add_inst(cover);

                // end block
                let end_bb = env.ctx.new_block(None, true);
                let branch = env.ctx.local_val().branch(l_binary, end_bb, false_bb);
                add_inst!(env.ctx.func_data(), base_bb, branch);

                let result = env.ctx.local_val().load(result);
                env.ctx.add_inst(result);
                Ok(ExpResult::with_effect(result))
            }
        }
    }
}

impl GenerateIr for LAndExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate_on(env),
            LAndExp::LAndOpExp(op_exp) => {
                // short circuit

                // base block
                let zero = env.ctx.local_val().integer(0);
                let lhs = op_exp.l_and_exp.generate_on(env)?;
                let base_bb = env.ctx.block.unwrap();

                // true block
                let true_bb = env.ctx.new_block(Some("%and_else".into()), false);
                let rhs = op_exp.eq_exp.generate_on(env)?;

                if !rhs.has_effect() {
                    // if rhs has no side effect, we can use a simpler implementation
                    let lv = env.ctx.local_val().binary(BinaryOp::NotEq, lhs.value, zero);
                    env.ctx.add_inst(lv);
                    let rv = env.ctx.local_val().binary(BinaryOp::NotEq, rhs.value, zero);
                    env.ctx.add_inst(rv);
                    let value = env.ctx.local_val().binary(BinaryOp::And, lv, rv);
                    env.ctx.add_inst(value);
                    let jump = env.ctx.local_val().jump(true_bb);
                    add_inst!(env.ctx.func_data(), base_bb, jump);
                    return Ok(ExpResult::merge(value, vec![lhs, rhs]));
                }

                let cur_bb = env.ctx.block.unwrap();
                env.ctx.block(base_bb);
                let l_binary = env.ctx.local_val().binary(BinaryOp::NotEq, lhs.value, zero);
                env.ctx.add_inst(l_binary);
                let result = env.ctx.alloc_and_store(l_binary, Type::get_i32());
                env.ctx.block(cur_bb);

                let r_binary = env.ctx.local_val().binary(BinaryOp::NotEq, rhs.value, zero);
                let cover = env.ctx.local_val().store(r_binary, result);
                env.ctx.add_inst(r_binary);
                env.ctx.add_inst(cover);

                // end block
                let end_bb = env.ctx.new_block(None, true);
                let branch = env.ctx.local_val().branch(l_binary, true_bb, end_bb);
                add_inst!(env.ctx.func_data(), base_bb, branch);

                let result = env.ctx.local_val().load(result);
                env.ctx.add_inst(result);
                Ok(ExpResult::with_effect(result))
            }
        }
    }
}

impl GenerateIr for EqExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate_on(env),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.generate_on(env)?;
                let rhs = op_exp.rel_exp.generate_on(env)?;
                let op = op_exp.eq_op.into();
                let value = env.ctx.local_val().binary(op, lhs.value, rhs.value);
                env.ctx.add_inst(value);
                Ok(ExpResult::merge(value, vec![lhs, rhs]))
            }
        }
    }
}

impl GenerateIr for RelExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate_on(env),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.generate_on(env)?;
                let rhs = op_exp.add_exp.generate_on(env)?;
                let op = op_exp.rel_op.into();
                let value = env.ctx.local_val().binary(op, lhs.value, rhs.value);
                env.ctx.add_inst(value);
                Ok(ExpResult::merge(value, vec![lhs, rhs]))
            }
        }
    }
}

impl GenerateIr for AddExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(env),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.generate_on(env)?;
                let rhs = op_exp.mul_exp.generate_on(env)?;
                let op = op_exp.add_op.into();
                let value = env.ctx.local_val().binary(op, lhs.value, rhs.value);
                env.ctx.add_inst(value);
                Ok(ExpResult::merge(value, vec![lhs, rhs]))
            }
        }
    }
}

impl GenerateIr for MulExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(env),
            MulExp::MulOpExp(mul_op_exp) => {
                let lhs = mul_op_exp.mul_exp.generate_on(env)?;
                let rhs = mul_op_exp.unary_exp.generate_on(env)?;
                let op = mul_op_exp.mul_op.into();
                let value = env.ctx.local_val().binary(op, lhs.value, rhs.value);
                env.ctx.add_inst(value);
                Ok(ExpResult::merge(value, vec![lhs, rhs]))
            }
        }
    }
}

impl GenerateIr for UnaryExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(env),
            UnaryExp::UnaryOpExp(unary_op_exp) => {
                let exp = unary_op_exp.unary_exp.generate_on(env)?;
                let zero = env.ctx.local_val().integer(0);

                let exp_v = exp.value;
                let value = match unary_op_exp.unary_op {
                    UnaryOp::Pos => return Ok(exp),
                    UnaryOp::Neg => env.ctx.local_val().binary(BinaryOp::Sub, zero, exp_v),
                    UnaryOp::Not => env.ctx.local_val().binary(BinaryOp::Eq, exp_v, zero),
                };
                env.ctx.add_inst(value);
                Ok(ExpResult::merge(value, vec![exp]))
            }

            UnaryExp::FuncCall(func_call) => {
                let mut callee = None;
                for (&func, data) in env.ctx.program.funcs() {
                    if data.name() == global_ident(&func_call.ident).as_str() {
                        callee = Some(func);
                        break;
                    }
                }
                let callee =
                    callee.ok_or(AstError::FunctionNotFoundError(func_call.ident.clone()))?;

                let mut args = vec![];
                for exp in &func_call.args {
                    let arg = exp.generate_on(env)?;
                    args.push(arg.value);
                }

                let call = env.ctx.local_val().call(callee, args);
                env.ctx.add_inst(call);
                Ok(ExpResult::with_effect(call))
            }
        }
    }
}

impl GenerateIr for PrimaryExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            PrimaryExp::Number(number) => number.generate_on(env),
            PrimaryExp::LValExp(l_val) => l_val.generate_on(env),
            PrimaryExp::Exp(exp) => exp.generate_on(env),
        }
    }
}

impl GenerateIr for LValExp {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        let symbol = env.table.lookup_or(&self.ident)?;

        // if the symbol is a const, return the value
        if symbol.is_const() && symbol.is_single() {
            return env
                .table
                .lookup_const_val(&self.ident)
                .unwrap()
                .generate_on(env);
        }

        // if not a const, load the value
        let lval = LVal {
            ident: self.ident.clone(),
            array_index: self.array_index.clone(),
        };
        let alloc = lval.generate_on(env)?;
        let kind = env.ctx.value_type(alloc).kind().clone();
        let value = match kind {
            TypeKind::Pointer(ty) => match ty.kind() {
                TypeKind::Array(_, _) => {
                    let zero = env.ctx.local_val().integer(0);
                    env.ctx.local_val().get_elem_ptr(alloc, zero)
                }
                _ => env.ctx.local_val().load(alloc),
            },
            _ => unreachable!("LValExp::generate_on: not a pointer type"),
        };
        env.ctx.add_inst(value);
        Ok(ExpResult::safe(value))
    }
}

impl GenerateIr for Number {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        match self {
            Number::Int(int) => int.generate_on(env),
        }
    }
}

impl GenerateIr for i32 {
    type IrTarget = ExpResult;

    fn generate_on(&self, env: &mut Environment) -> Result<ExpResult, AstError> {
        let int = env.ctx.local_val().integer(*self);
        Ok(ExpResult::safe(int))
    }
}
