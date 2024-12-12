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

pub trait ToDescriptor {
    fn to_desc(self) -> Descriptor;
}

impl ToDescriptor for Register {
    fn to_desc(self) -> Descriptor {
        Descriptor::Register(self)
    }
}

impl ToDescriptor for Stack {
    fn to_desc(self) -> Descriptor {
        Descriptor::Stack(self)
    }
}

impl ToDescriptor for Data {
    fn to_desc(self) -> Descriptor {
        Descriptor::Data(self)
    }
}
