//! Generate IR from AST

use super::ast::*;
use super::eval::*;
use super::symbol::*;
use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Value};
use std::io;

pub fn build_ir(ast: CompUnit) -> Result<Program, ParseError> {
    let program = Program::new();
    let mut context = Context {
        program,
        syb_table: SymbolTable::default(),
        func: None,
        block: None,
    };
    ast.generate_on(&mut context)?;
    Ok(context.program)
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), io::Error> {
    KoopaGenerator::new(output).generate_on(program)
}

/*********************  Structs  *********************/

/// Context for current generating
pub struct Context {
    pub program: Program,
    pub syb_table: SymbolTable,
    pub func: Option<Function>,
    pub block: Option<BasicBlock>,
}

impl Context {
    pub fn func(&mut self, func: Function) -> &mut Self {
        self.func = Some(func);
        self
    }

    pub fn block(&mut self, block: BasicBlock) -> &mut Self {
        self.block = Some(block);
        self
    }
}

#[derive(Debug)]
pub enum ParseError {
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
    fn generate_on(&self, context: &mut Context) -> Result<T, ParseError>;
}

/*********************  Macros  *********************/

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

/// Get the current [`FunctionData`] in the [`Context`]
macro_rules! func_data {
    ($context:expr) => {
        $context.program.func_mut($context.func.unwrap())
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
    ($def:expr) => {
        format!("%{}", $def.ident)
    };
}

/*********************  Implementations  *********************/

impl GenerateIr<()> for CompUnit {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
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
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        let func = FunctionData::new_decl(
            global_ident!(self),
            self.params
                .iter()
                .map(|param| param.b_type.into())
                .collect(),
            self.func_type.into(),
        );
        let func = context.program.new_func(func);
        context.syb_table.enter_scope();
        self.block.generate_on(context.func(func))?;
        context.syb_table.exit_scope();
        Ok(())
    }
}

impl GenerateIr<()> for Decl {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(context),
            Decl::VarDecl(var_decl) => var_decl.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Block {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        let func_data = func_data!(context);
        let entry = new_bb!(func_data).basic_block(Some("%entry".into()));
        add_bb!(func_data, entry);
        context.block(entry);

        context.syb_table.enter_scope();
        for block_item in &self.block_items {
            block_item.generate_on(context)?;
        }
        context.syb_table.exit_scope();

        Ok(())
    }
}

impl GenerateIr<()> for BlockItem {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        match self {
            BlockItem::Stmt(stmt) => stmt.generate_on(context),
            BlockItem::Decl(decl) => decl.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Stmt {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        match self {
            Stmt::Empty => Ok(()),
            Stmt::Exp(exp) => {
                exp.generate_on(context)?;
                Ok(())
            }
            Stmt::Block(block) => block.generate_on(context),
            Stmt::Assign(assign) => assign.generate_on(context),
            Stmt::Return(stmt) => stmt.generate_on(context),
            _ => todo!(),
        }
    }
}

impl GenerateIr<()> for Return {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(context)?),
            None => None,
        };

        let func_data = func_data!(context);
        let ret = new_value!(func_data).ret(ret);
        add_inst!(func_data, context.block.unwrap(), ret);
        Ok(())
    }
}

impl GenerateIr<()> for Assign {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        let alloc = match context.syb_table.lookup_var(&self.l_val.ident) {
            Some((&_, &value)) => value,
            None => return Err(ParseError::UndefinedVarError(self.l_val.ident.clone())),
        };
        let exp = self.exp.generate_on(context)?;

        // Store the value to the variable
        let func_data = func_data!(context);
        let store = new_value!(func_data).store(exp, alloc);
        add_inst!(func_data, context.block.unwrap(), store);
        Ok(())
    }
}

impl GenerateIr<()> for ConstDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        for const_def in self.const_defs.clone() {
            context.syb_table.add_const(const_def.to_symbol(context)?);
        }
        Ok(())
    }
}

impl GenerateIr<()> for VarDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), ParseError> {
        for var_def in self.var_defs.clone() {
            // Initialize the variable if needed
            let is_single = var_def.is_single();
            let mut init = None;

            if let Some(value) = &var_def.init_val {
                match value {
                    InitVal::Exp(exp) => {
                        if !is_single {
                            return Err(ParseError::AssignError(var_def.ident.clone()));
                        }
                        init = Some(exp.generate_on(context)?);
                    }
                    InitVal::InitVals(init_vals) => todo!(),
                }
            }

            let func_data = func_data!(context);

            // Allocate memory for the variable
            let alloc = if is_single {
                new_value!(func_data).alloc(self.b_type.into())
            } else {
                todo!()
            };
            set_value_name!(func_data, alloc, global_ident!(var_def));
            add_inst!(func_data, context.block.unwrap(), alloc);

            if let Some(init) = init {
                // Store the initial value to the variable
                let store = new_value!(func_data).store(init, alloc);
                add_inst!(func_data, context.block.unwrap(), store);
            }

            context
                .syb_table
                .add_var(var_def.to_symbol(context)?, alloc);
        }
        Ok(())
    }
}

impl GenerateIr<Value> for Exp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        if let Ok(value) = self.eval(context) {
            return Number::Int(value).generate_on(context);
        }

        match self {
            Exp::LOrExp(l_or_exp) => l_or_exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for ConstExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        // Const expressions must be evaluated at compile time
        Number::Int(self.eval(context)?).generate_on(context)
    }
}

impl GenerateIr<Value> for LOrExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.generate_on(context),
            LOrExp::LOrOpExp(op_exp) => {
                let lhs = op_exp.l_or_exp.generate_on(context)?;
                let rhs = op_exp.l_and_exp.generate_on(context)?;
                let zero = new_value!(func_data!(context)).integer(0);
                let func_data = func_data!(context);

                // a || b = (a | b) != 0
                let value = new_value!(func_data).binary(BinaryOp::Or, lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                let value = new_value!(func_data).binary(BinaryOp::NotEq, value, zero);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for LAndExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate_on(context),
            LAndExp::LAndOpExp(op_exp) => {
                let lhs = op_exp.l_and_exp.generate_on(context)?;
                let rhs = op_exp.eq_exp.generate_on(context)?;
                let zero = new_value!(func_data!(context)).integer(0);
                let func_data = func_data!(context);

                // a && b = (a != 0) & (b != 0)
                let lhs = new_value!(func_data).binary(BinaryOp::NotEq, lhs, zero);
                add_inst!(func_data, context.block.unwrap(), lhs);
                let rhs = new_value!(func_data).binary(BinaryOp::NotEq, rhs, zero);
                add_inst!(func_data, context.block.unwrap(), rhs);
                let value = new_value!(func_data).binary(BinaryOp::And, lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for EqExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate_on(context),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.generate_on(context)?;
                let rhs = op_exp.rel_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(op_exp.eq_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for RelExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate_on(context),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.generate_on(context)?;
                let rhs = op_exp.add_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(op_exp.rel_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for AddExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(context),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.generate_on(context)?;
                let rhs = op_exp.mul_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(op_exp.add_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for MulExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(context),
            MulExp::MulOpExp(mul_op_exp) => {
                let lhs = mul_op_exp.mul_exp.generate_on(context)?;
                let rhs = mul_op_exp.unary_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let value = new_value!(func_data).binary(mul_op_exp.mul_op.into(), lhs, rhs);
                add_inst!(func_data, context.block.unwrap(), value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for UnaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(context),

            UnaryExp::UnaryOpExp(unary_op_exp) => {
                let exp = unary_op_exp.unary_exp.generate_on(context)?;
                let func_data = func_data!(context);
                let zero = new_value!(func_data).integer(0);
                let value = match unary_op_exp.unary_op {
                    UnaryOp::Pos => return Ok(exp),
                    UnaryOp::Neg => new_value!(func_data).binary(BinaryOp::Sub, zero, exp),
                    UnaryOp::Not => new_value!(func_data).binary(BinaryOp::Eq, exp, zero),
                };
                add_inst!(func_data, context.block.unwrap(), value);
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
                    return Err(ParseError::FunctionNotFoundError(func_call.ident.clone()));
                }

                let func_data = func_data!(context);
                let call = new_value!(func_data).call(callee.unwrap(), args);
                add_inst!(func_data, context.block.unwrap(), call);
                Ok(call)
            }
        }
    }
}

impl GenerateIr<Value> for PrimaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            PrimaryExp::Number(number) => number.generate_on(context),
            PrimaryExp::LVal(l_val) => l_val.generate_on(context),
            PrimaryExp::Exp(exp) => exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LVal {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match context.syb_table.lookup(&self.ident) {
            Some(item) => match &item.symbol {
                Symbol::Const(symbol) => Number::Int(symbol.value).generate_on(context),
                Symbol::ConstArray(symbol) => todo!(),
                Symbol::Var(symbol) => {
                    let alloc = item.alloc.unwrap();
                    // load the value from the variable
                    let func_data = func_data!(context);
                    let value = new_value!(func_data).load(alloc);
                    add_inst!(func_data, context.block.unwrap(), value);
                    Ok(value)
                }
                Symbol::VarArray(symbol) => todo!(),
                _ => unreachable!(),
            },
            None => Err(ParseError::UndefinedVarError(self.ident.clone())),
        }
    }
}

impl GenerateIr<Value> for Number {
    fn generate_on(&self, context: &mut Context) -> Result<Value, ParseError> {
        match self {
            Number::Int(int) => {
                let int = new_value!(func_data!(context)).integer(*int);
                Ok(int)
            }
        }
    }
}
