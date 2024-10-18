use koopa::ir::BasicBlock;

#[derive(Debug)]
pub struct Loop {
    pub cond_bb: BasicBlock,
    pub end_bb: BasicBlock,
}

pub type LoopStack = Vec<Loop>;
