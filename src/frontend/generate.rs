//! Generate IR from AST

use super::ast::*;
use super::eval::*;
use super::symbol::*;
use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::ValueKind;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Value};
use std::io;

pub fn build_ir(ast: CompUnit) -> Result<Program, AstError> {
    let mut context = Context::default();
    ast.generate_on(&mut context)?;
    Ok(context.program)
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), io::Error> {
    KoopaGenerator::new(output).generate_on(program)
}

/*********************  Structs  *********************/

/// Context for current generating
#[derive(Default)]
pub struct Context {
    pub program: Program,
    pub syb_table: SymbolTable,
    pub func: Option<Function>,
    pub block: Option<BasicBlock>,
}

impl Context {
    pub fn func(&mut self, func: Function) {
        self.func = Some(func);
    }

    pub fn block(&mut self, block: BasicBlock) {
        self.block = Some(block);
    }
}

#[derive(Debug)]
pub enum AstError {
    FunctionNotFoundError(String),
    IllegalConstExpError(String),
    UndefinedConstError(String),
    UndefinedVarError(String),
    AssignError(String),
    UnknownError(String),
}

/*********************  Traits  *********************/

/// Trait which should be implemented by AST types to generate IR
///
/// Return a value of type `T` if needed
trait GenerateIr<T> {
    /// Generate IR on the given context
    ///
    /// # Errors
    /// Returns an [`ParseError`] if the generation fails
    fn generate_on(&self, context: &mut Context) -> Result<T, AstError>;
}

/// Create a new [`BasicBlock`] in [`Function`]
macro_rules! new_bb {
    ($func:expr) => {
        $func.dfg_mut().new_bb()
    };
}

/// Create a new [`Value`] in [`Function`]
macro_rules! new_value {
    ($func:expr) => {
        $func.dfg_mut().new_value()
    };
}

/// Add a [`BasicBlock`] to the layout of [`Function`]
macro_rules! add_bb {
    ($func:expr, $bb:expr) => {
        $func.layout_mut().bbs_mut().push_key_back($bb).unwrap()
    };
}

/// Add an [`Value`] to a [`BasicBlock`] in [`Function`]
macro_rules! add_inst {
    ($func:expr, $bb:expr, $inst:expr) => {
        $func
            .layout_mut()
            .bb_mut($bb)
            .insts_mut()
            .push_key_back($inst)
            .unwrap()
    };
}

/// Set the name of a [`Value`] in the [`FunctionData`]
macro_rules! set_value_name {
    ($func:expr, $value:expr, $name:expr) => {
        $func.dfg_mut().set_value_name($value, Some($name))
    };
}

/// Generate a global identifier in Koopa IR
macro_rules! global_ident {
    ($def:expr) => {
        format!("@{}", &$def.ident)
    };
}

/// Generate a normal identifier in Koopa IR
macro_rules! normal_ident {
    ($def:expr, $id:expr) => {
        format!("%{}_{}", $def.ident, $id)
    };
}

/*********************  Implementations  *********************/

impl Context {
    pub fn func_data(&mut self) -> &mut FunctionData {
        self.program.func_mut(self.func.unwrap())
    }

    /// Create a new block and set it as the current block
    pub fn create_block(&mut self, name: Option<String>) {
        let block = new_bb!(self.func_data()).basic_block(name);
        add_bb!(self.func_data(), block);
        self.block(block);
    }

    /// Generate a new block and append it to the layout
    /// If enabled `auto_link`, It will automatically add a jump to the next block if needed
    pub fn new_block(&mut self, name: Option<String>, auto_link: bool) -> BasicBlock {
        let block = self.block.unwrap();
        let func_data = self.func_data();
        let block_node = func_data.layout_mut().bb_mut(block);
        if !block_node.insts().is_empty() || !auto_link {
            let last = block_node.insts().back_key().copied();

            // If the last block is empty, or forced not to link, create a new block
            let this = new_bb!(func_data).basic_block(name);
            add_bb!(func_data, this);

            // If the last instruction is not a jump, branch or return, add a jump to the next block
            if auto_link {
                let kind = func_data.dfg().value(last.unwrap()).kind();
                match kind {
                    ValueKind::Jump(_) => {}
                    ValueKind::Branch(_) => {}
                    ValueKind::Return(_) => {}
                    _ => {
                        let jump = new_value!(func_data).jump(this);
                        add_inst!(func_data, block, jump);
                    }
                }
            }
            self.block(this);
        }
        self.block.unwrap()
    }

    pub fn add_inst(&mut self, inst: Value) {
        let func_data = self.program.func_mut(self.func.unwrap());
        let block = self.block.unwrap();
        add_inst!(func_data, block, inst);
    }
}

impl GenerateIr<()> for CompUnit {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        for comp_item in &self.comp_items {
            match comp_item {
                CompItem::FuncDef(func_def) => {
                    func_def.generate_on(context)?;
                }
                CompItem::Decl(decl) => {
                    decl.generate_on(context)?;
                }
            }
        }
        Ok(())
    }
}

impl GenerateIr<()> for FuncDef {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let func = FunctionData::new_decl(
            global_ident!(self),
            self.params
                .iter()
                .map(|param| param.b_type.into())
                .collect(),
            self.func_type.into(),
        );

        let func = context.program.new_func(func);
        context.func(func);
        context.create_block(Some("%entry".into()));

        context.syb_table.enter_scope();
        self.block.generate_on(context)?;
        context.syb_table.exit_scope();
        assert!(context.syb_table.items.is_empty());

        // context.func_data().layout_mut().bbs_mut().pop_back();
        context.block = None;
        Ok(())
    }
}

impl GenerateIr<()> for Decl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(context),
            Decl::VarDecl(var_decl) => var_decl.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Block {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        if self.block_items.is_empty() {
            return Ok(());
        }

        context.new_block(None, true);
        context.syb_table.enter_scope();

        for block_item in &self.block_items {
            match block_item {
                BlockItem::Stmt(stmt) => {
                    stmt.generate_on(context)?;
                    match stmt {
                        Stmt::Return(_) => {
                            break;
                        }
                        Stmt::Break => {
                            break;
                        }
                        Stmt::Continue => {
                            break;
                        }
                        _ => {}
                    }
                }
                BlockItem::Decl(decl) => decl.generate_on(context)?,
            };
        }
        context.syb_table.exit_scope();
        context.new_block(None, true);
        Ok(())
    }
}

impl GenerateIr<()> for Stmt {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        match self {
            Stmt::Empty => Ok(()),
            Stmt::Exp(exp) => {
                exp.generate_on(context)?;
                Ok(())
            }
            Stmt::Block(stmt) => stmt.generate_on(context),
            Stmt::Assign(stmt) => stmt.generate_on(context),
            Stmt::If(stmt) => stmt.generate_on(context),
            Stmt::Return(stmt) => stmt.generate_on(context),
            _ => todo!(),
        }
    }
}

impl GenerateIr<()> for Return {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(context)?),
            None => None,
        };

        let func_data = context.func_data();
        let ret = new_value!(func_data).ret(ret);
        context.add_inst(ret);
        Ok(())
    }
}

impl GenerateIr<()> for Assign {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let alloc = match context.syb_table.lookup_var(&self.l_val.ident) {
            Some((&_, &value)) => value,
            None => return Err(AstError::UndefinedVarError(self.l_val.ident.clone())),
        };
        let exp = self.exp.generate_on(context)?;

        // Store the value to the variable
        let func_data = context.func_data();
        let store = new_value!(func_data).store(exp, alloc);
        context.add_inst(store);
        Ok(())
    }
}

impl GenerateIr<()> for If {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let cond = match &self.cond {
            Cond::LOrExp(exp) => exp.generate_on(context)?,
        };
        let base_bb = context.block.unwrap();

        let true_bb = context.new_block(Some("%then".into()), false);
        self.stmt.generate_on(context)?;
         // always create a new ending block for the then statement
        // even if it is not a block statement       
        let true_end_bb = context.new_block(Some("%then_end".into()), true);

        if let Some(else_stmt) = &self.else_stmt {
            let false_bb = context.new_block(Some("%else".into()), false);
            else_stmt.generate_on(context)?;
            let end_bb = context.new_block(None, true);

            let func_data = context.func_data();
            // branch to true/false
            let branch = new_value!(func_data).branch(cond, true_bb, false_bb);
            add_inst!(func_data, base_bb, branch);
            // true jump to end
            let jump = new_value!(func_data).jump(end_bb);
            add_inst!(func_data, true_end_bb, jump);
        } else {
            let end_bb = context.new_block(None, true);
            // branch to true/end
            let func_data = context.func_data();
            let branch = new_value!(func_data).branch(cond, true_bb, end_bb);
            add_inst!(func_data, base_bb, branch);
        };

        Ok(())
        // todo!()
    }
}

impl GenerateIr<()> for ConstDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        for const_def in self.const_defs.clone() {
            context.syb_table.add_const(const_def.to_symbol(context)?);
        }
        Ok(())
    }
}

impl GenerateIr<()> for VarDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        for var_def in self.var_defs.clone() {
            // Initialize the variable if needed
            let is_single = var_def.is_single();
            let mut init = None;

            if let Some(value) = &var_def.init_val {
                match value {
                    InitVal::Exp(exp) => {
                        if !is_single {
                            return Err(AstError::AssignError(var_def.ident.clone()));
                        }
                        init = Some(exp.generate_on(context)?);
                    }
                    InitVal::InitVals(_) => todo!(),
                }
            }

            let func_data = context.func_data();

            // Allocate memory for the variable
            let alloc = if is_single {
                new_value!(func_data).alloc(self.b_type.into())
            } else {
                todo!()
            };
            context.add_inst(alloc);

            if let Some(init) = init {
                // Store the initial value to the variable
                let store = new_value!(context.func_data()).store(init, alloc);
                context.add_inst(store);
            }

            context
                .syb_table
                .add_var(var_def.to_symbol(context)?, alloc);
        }
        Ok(())
    }
}

impl GenerateIr<Value> for Exp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        if let Ok(value) = self.eval(context) {
            return value.generate_on(context);
        }

        match self {
            Exp::LOrExp(l_or_exp) => l_or_exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for ConstExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        // Const expressions must be evaluated at compile time
        self.eval(context)?.generate_on(context)
    }
}

impl GenerateIr<Value> for LOrExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.generate_on(context),
            LOrExp::LOrOpExp(op_exp) => {
                let lhs = op_exp.l_or_exp.generate_on(context)?;
                let rhs = op_exp.l_and_exp.generate_on(context)?;
                let func_data = context.func_data();

                // a || b = (a | b) != 0
                let zero = new_value!(func_data).integer(0);
                let temp = new_value!(func_data).binary(BinaryOp::Or, lhs, rhs);
                let value = new_value!(func_data).binary(BinaryOp::NotEq, temp, zero);
                context.add_inst(temp);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for LAndExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate_on(context),
            LAndExp::LAndOpExp(op_exp) => {
                let lhs = op_exp.l_and_exp.generate_on(context)?;
                let rhs = op_exp.eq_exp.generate_on(context)?;
                let func_data = context.func_data();

                // a && b = (a != 0) & (b != 0)
                let zero = new_value!(func_data).integer(0);
                let lhs = new_value!(func_data).binary(BinaryOp::NotEq, lhs, zero);
                let rhs = new_value!(func_data).binary(BinaryOp::NotEq, rhs, zero);
                let value = new_value!(func_data).binary(BinaryOp::And, lhs, rhs);
                context.add_inst(lhs);
                context.add_inst(rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for EqExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate_on(context),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.generate_on(context)?;
                let rhs = op_exp.rel_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(op_exp.eq_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for RelExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate_on(context),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.generate_on(context)?;
                let rhs = op_exp.add_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(op_exp.rel_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for AddExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(context),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.generate_on(context)?;
                let rhs = op_exp.mul_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(op_exp.add_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for MulExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(context),
            MulExp::MulOpExp(mul_op_exp) => {
                let lhs = mul_op_exp.mul_exp.generate_on(context)?;
                let rhs = mul_op_exp.unary_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(mul_op_exp.mul_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for UnaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(context),

            UnaryExp::UnaryOpExp(unary_op_exp) => {
                let exp = unary_op_exp.unary_exp.generate_on(context)?;
                let func_data = context.func_data();
                let zero = new_value!(func_data).integer(0);
                let value = match unary_op_exp.unary_op {
                    UnaryOp::Pos => return Ok(exp),
                    UnaryOp::Neg => new_value!(func_data).binary(BinaryOp::Sub, zero, exp),
                    UnaryOp::Not => new_value!(func_data).binary(BinaryOp::Eq, exp, zero),
                };
                context.add_inst(value);
                Ok(value)
            }

            UnaryExp::FuncCall(func_call) => {
                let mut callee = None;
                let mut args = vec![];
                for arg in &func_call.args {
                    args.push(arg.generate_on(context)?);
                }
                for (&func, data) in context.program.funcs() {
                    if data.name() == global_ident!(func_call).as_str() {
                        callee = Some(func);
                        break;
                    }
                }
                if callee.is_none() {
                    return Err(AstError::FunctionNotFoundError(func_call.ident.clone()));
                }

                let func_data = context.func_data();
                let call = new_value!(func_data).call(callee.unwrap(), args);
                context.add_inst(call);
                Ok(call)
            }
        }
    }
}

impl GenerateIr<Value> for PrimaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            PrimaryExp::Number(number) => number.generate_on(context),
            PrimaryExp::LVal(l_val) => l_val.generate_on(context),
            PrimaryExp::Exp(exp) => exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LVal {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match context.syb_table.lookup(&self.ident) {
            Some(item) => match &item.symbol {
                Symbol::Const(symbol) => symbol.value.clone().generate_on(context),
                Symbol::ConstArray(_) => todo!(),
                Symbol::Var(_) => {
                    let alloc = item
                        .alloc
                        .ok_or(AstError::UndefinedVarError(self.ident.clone()))?;
                    // load the value from the variable
                    let load = new_value!(context.func_data()).load(alloc);
                    context.add_inst(load);
                    Ok(load)
                }
                Symbol::VarArray(_) => todo!(),
                _ => unreachable!(),
            },
            None => Err(AstError::UndefinedVarError(self.ident.clone())),
        }
    }
}

impl GenerateIr<Value> for Number {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            Number::Int(int) => int.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for i32 {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        let int = new_value!(context.func_data()).integer(*self);
        Ok(int)
    }
}
