//! Evaluation of AST nodes

use super::ast::*;
use super::symbol::SymbolTable;
use super::{AstError, AstResult};

pub trait Eval<T> {
    fn eval(&self, table: &SymbolTable) -> AstResult<T>;
}

impl Eval<i32> for Exp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            Exp::LOrExp(exp) => exp.eval(table),
        }
    }
}

impl Eval<i32> for ConstExp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            ConstExp::LOrExp(exp) => exp.eval(table),
        }
    }
}

impl Eval<i32> for LOrExp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            LOrExp::LAndExp(exp) => exp.eval(table),
            LOrExp::LOrOpExp(op_exp) => {
                let lhs = op_exp.l_or_exp.eval(table)?;
                let rhs = op_exp.l_and_exp.eval(table)?;
                Ok((lhs != 0 || rhs != 0) as i32)
            }
        }
    }
}

impl Eval<i32> for LAndExp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            LAndExp::EqExp(exp) => exp.eval(table),
            LAndExp::LAndOpExp(op_exp) => {
                let lhs = op_exp.l_and_exp.eval(table)?;
                let rhs = op_exp.eq_exp.eval(table)?;
                Ok((lhs != 0 && rhs != 0) as i32)
            }
        }
    }
}

impl Eval<i32> for EqExp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            EqExp::RelExp(exp) => exp.eval(table),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.eval(table)?;
                let rhs = op_exp.rel_exp.eval(table)?;
                match op_exp.eq_op {
                    EqOp::Eq => Ok((lhs == rhs) as i32),
                    EqOp::Ne => Ok((lhs != rhs) as i32),
                }
            }
        }
    }
}

impl Eval<i32> for RelExp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            RelExp::AddExp(exp) => exp.eval(table),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.eval(table)?;
                let rhs = op_exp.add_exp.eval(table)?;
                match op_exp.rel_op {
                    RelOp::Lt => Ok((lhs < rhs) as i32),
                    RelOp::Le => Ok((lhs <= rhs) as i32),
                    RelOp::Gt => Ok((lhs > rhs) as i32),
                    RelOp::Ge => Ok((lhs >= rhs) as i32),
                }
            }
        }
    }
}

impl Eval<i32> for AddExp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
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
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
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
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            UnaryExp::PrimaryExp(exp) => exp.eval(table),
            UnaryExp::FuncCall(_) => Err(AstError::IllegalConstExpError("Function Call".into())),
            UnaryExp::UnaryOpExp(op_exp) => {
                let exp = op_exp.unary_exp.eval(table)?;
                match op_exp.unary_op {
                    UnaryOp::Pos => Ok(exp),
                    UnaryOp::Neg => Ok(-exp),
                    UnaryOp::Not => Ok((exp == 0) as i32),
                }
            }
        }
    }
}

impl Eval<i32> for PrimaryExp {
    fn eval(&self, table: &SymbolTable) -> AstResult<i32> {
        match self {
            PrimaryExp::Exp(exp) => exp.eval(table),
            PrimaryExp::LValExp(l_val) => {
                let ident = l_val.ident.clone();
                table
                    .lookup_or(&ident)?
                    .borrow()
                    .const_value()
                    .ok_or(AstError::IllegalConstExpError(ident.clone()))
            }
            PrimaryExp::Number(num) => match num {
                Number::Int(i) => Ok(*i),
            },
        }
    }
}
