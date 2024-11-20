use koopa::ir::{Function, FunctionData};
use koopa::opt::FunctionPass;

use crate::opt::flow::FunctionFlowGraph;

#[derive(Default)]
pub struct CommonSubexpression;

impl FunctionPass for CommonSubexpression {
    fn run_on(&mut self, _: Function, data: &mut FunctionData) {
        if data.layout().entry_bb().is_none() {
            return;
        }
        let fg = FunctionFlowGraph::build(data);
    }
}
