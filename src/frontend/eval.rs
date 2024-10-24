use super::ast::*;
use super::symbol::{SymbolItem, SymbolTable};
use super::AstError;

pub trait Eval<T> {
    fn eval(&self, table: &SymbolTable) -> Result<T, AstError>;
}

impl Eval<i32> for Exp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            Exp::LOrExp(exp) => exp.eval(table),
        }
    }
}

impl Eval<i32> for ConstExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            ConstExp::LOrExp(exp) => exp.eval(table),
        }
    }
}

impl Eval<i32> for LOrExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            LOrExp::LAndExp(exp) => exp.eval(table),
            LOrExp::LOrOpExp(op_exp) => {
                let lhs = op_exp.l_or_exp.eval(table)?;
                let rhs = op_exp.l_and_exp.eval(table)?;
                Ok(if lhs != 0 || rhs != 0 { 1 } else { 0 })
            }
        }
    }
}

impl Eval<i32> for LAndExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            LAndExp::EqExp(exp) => exp.eval(table),
            LAndExp::LAndOpExp(op_exp) => {
                let lhs = op_exp.l_and_exp.eval(table)?;
                let rhs = op_exp.eq_exp.eval(table)?;
                Ok(if lhs != 0 && rhs != 0 { 1 } else { 0 })
            }
        }
    }
}

impl Eval<i32> for EqExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            EqExp::RelExp(exp) => exp.eval(table),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.eval(table)?;
                let rhs = op_exp.rel_exp.eval(table)?;
                match op_exp.eq_op {
                    EqOp::Eq => Ok(if lhs == rhs { 1 } else { 0 }),
                    EqOp::Ne => Ok(if lhs != rhs { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Eval<i32> for RelExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            RelExp::AddExp(exp) => exp.eval(table),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.eval(table)?;
                let rhs = op_exp.add_exp.eval(table)?;
                match op_exp.rel_op {
                    RelOp::Lt => Ok(if lhs < rhs { 1 } else { 0 }),
                    RelOp::Le => Ok(if lhs <= rhs { 1 } else { 0 }),
                    RelOp::Gt => Ok(if lhs > rhs { 1 } else { 0 }),
                    RelOp::Ge => Ok(if lhs >= rhs { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Eval<i32> for AddExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            AddExp::MulExp(exp) => exp.eval(table),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.eval(table)?;
                let rhs = op_exp.mul_exp.eval(table)?;
                match op_exp.add_op {
                    AddOp::Add => Ok(lhs + rhs),
                    AddOp::Sub => Ok(lhs - rhs),
                }
            }
        }
    }
}

impl Eval<i32> for MulExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            MulExp::UnaryExp(exp) => exp.eval(table),
            MulExp::MulOpExp(op_exp) => {
                let lhs = op_exp.mul_exp.eval(table)?;
                let rhs = op_exp.unary_exp.eval(table)?;
                match op_exp.mul_op {
                    MulOp::Mul => Ok(lhs * rhs),
                    MulOp::Div => Ok(lhs / rhs),
                    MulOp::Mod => Ok(lhs % rhs),
                }
            }
        }
    }
}

impl Eval<i32> for UnaryExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            UnaryExp::PrimaryExp(exp) => exp.eval(table),
            UnaryExp::FuncCall(_) => Err(AstError::IllegalConstExpError(String::from(
                "Function Call",
            ))),
            UnaryExp::UnaryOpExp(op_exp) => {
                let exp = op_exp.unary_exp.eval(table)?;
                match op_exp.unary_op {
                    UnaryOp::Pos => Ok(exp),
                    UnaryOp::Neg => Ok(-exp),
                    UnaryOp::Not => Ok(if exp == 0 { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Eval<i32> for PrimaryExp {
    fn eval(&self, table: &SymbolTable) -> Result<i32, AstError> {
        match self {
            PrimaryExp::Exp(exp) => exp.eval(table),
            PrimaryExp::LValExp(l_val) => {
                let ident = l_val.ident.clone();
                let item = table
                    .lookup_item(&ident)
                    .ok_or(AstError::SymbolNotFoundError(ident.clone()))?;
                match item {
                    SymbolItem::Const(symbol) => Ok(symbol.value),
                    _ => Err(AstError::IllegalConstExpError(ident.clone())),
                }
            }
            PrimaryExp::Number(num) => match num {
                Number::Int(i) => Ok(*i),
            },
        }
    }
}
