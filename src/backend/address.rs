use super::registers::Register;

pub type Label = String;

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Stack {
    pub base: Register,
    pub offset: i32,
}

/// Data address, which is in the .data section.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Data {
    pub label: Label,
    pub offset: i32,
}

/// Address descriptor of the data.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Descriptor {
    /// The data is in the register.
    Register(Register),
    /// The data is in the stack.
    Stack(Stack),
    /// The data is in the data section.
    Data(Data),
}

impl Into<Descriptor> for Register {
    fn into(self) -> Descriptor {
        Descriptor::Register(self)
    }
}

impl Into<Descriptor> for Stack {
    fn into(self) -> Descriptor {
        Descriptor::Stack(self)
    }
}

impl Into<Descriptor> for Data {
    fn into(self) -> Descriptor {
        Descriptor::Data(self)
    }
}
