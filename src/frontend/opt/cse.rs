use koopa::ir::{Function, FunctionData};
use koopa::opt::FunctionPass;

use crate::frontend::dataflow::FunctionFlowGraph;

#[derive(Default)]
pub struct CommonSubexpression;

impl FunctionPass for CommonSubexpression {
    fn run_on(&mut self, _: Function, func_data: &mut FunctionData) {
        // if func_data.layout().entry_bb().is_none() {
        //     // the function is a declaration
        //     return;
        // }
        // let mut graph = FunctionFlowGraph::default();
        // graph.build(func_data);
    }
}
