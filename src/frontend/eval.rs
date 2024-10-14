use super::ast::*;
use super::ParseError;

pub trait Eval<T> {
    fn eval(&self) -> Result<T, ParseError>;
}

impl Eval<i32> for Exp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            Exp::LOrExp(exp) => exp.eval(),
        }
    }
}

impl Eval<i32> for ConstExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            ConstExp::LOrExp(exp) => exp.eval(),
        }
    }
}

impl Eval<i32> for LOrExp {
    fn eval(&self) -> Result<i32, ParseError> {
        todo!()
    }
}

impl Eval<i32> for AddExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            AddExp::MulExp(exp) => exp.eval(),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.eval()?;
                let rhs = op_exp.mul_exp.eval()?;
                match op_exp.add_op {
                    AddOp::Add => Ok(lhs + rhs),
                    AddOp::Sub => Ok(lhs - rhs),
                }
            }
        }
    }
}

impl Eval<i32> for MulExp {
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            MulExp::UnaryExp(exp) => exp.eval(),
            MulExp::MulOpExp(op_exp) => {
                let lhs = op_exp.mul_exp.eval()?;
                let rhs = op_exp.unary_exp.eval()?;
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
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            UnaryExp::PrimaryExp(exp) => exp.eval(),
            UnaryExp::FuncCall(_) => Err(ParseError::IllegalConstExpError(String::from(
                "Function Call",
            ))),
            UnaryExp::UnaryOpExp(op_exp) => {
                let exp = op_exp.unary_exp.eval()?;
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
    fn eval(&self) -> Result<i32, ParseError> {
        match self {
            PrimaryExp::Exp(exp) => exp.eval(),
            PrimaryExp::LVal(l_val) => {
                Err(ParseError::IllegalConstExpError(format!("{:?}", l_val)))
            }
            PrimaryExp::Number(num) => match num {
                Number::Int(i) => Ok(*i),
            },
        }
    }
}

pub fn eval_array_size(array_index: &Vec<ConstExp>) -> Result<i32, ParseError> {
    let mut length = 1;
    for exp in array_index {
        length *= exp.eval()?;
    }
    Ok(length)
}
