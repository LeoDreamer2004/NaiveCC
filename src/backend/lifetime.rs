use koopa::ir::Program;

pub struct IrLifeTimeParser<'a> {
    program: &'a Program,
}

impl<'a> IrLifeTimeParser<'a> {
    pub fn new(program: &'a Program) -> Self {
        IrLifeTimeParser { program }
    }

    pub fn parse(&self) {
        // Parse the lifetime of the IR.
    }
}
