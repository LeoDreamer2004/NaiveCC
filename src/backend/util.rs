use koopa::ir::Type;

pub trait AsmType {
    fn size(&self) -> usize;
}

impl AsmType for Type {
    fn size(&self) -> usize {
        if self.is_i32() {
            return 4;
        }
        todo!()
    }
}
