use koopa::ir::BasicBlock;

/// Loop structure.
#[derive(Debug)]
pub struct Loop {
    pub cond_bb: BasicBlock,
    pub end_bb: BasicBlock,
}

/// A stack of loops.
pub type LoopStack = Vec<Loop>;
