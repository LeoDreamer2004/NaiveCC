use koopa::ir::{Function, FunctionData};
use koopa::opt::FunctionPass;

#[derive(Default)]
pub struct CommonSubexpression;

impl FunctionPass for CommonSubexpression {
    fn run_on(&mut self, _: Function, _: &mut FunctionData) {
        // TODO
    }
}
