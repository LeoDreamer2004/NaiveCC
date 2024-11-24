use koopa::ir::{entities::ValueData, ValueKind};

use super::registers::Register;

pub type Pointer = *const ValueData;

/// Basic element in an instruction, including immediate value and register.
pub enum AsmElement {
    Local(Pointer),
    Imm(i32),
}

impl AsmElement {
    pub fn from(data: Pointer) -> Self {
        unsafe {
            match data.as_ref().unwrap().kind() {
                ValueKind::Integer(int) => AsmElement::Imm(int.value()),
                ValueKind::ZeroInit(_) => AsmElement::Imm(0),
                _ => AsmElement::Local(data),
            }
        }
    }
}

pub type Label = String;

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Stack {
    pub base: Register,
    pub offset: i32,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Data {
    pub label: Label,
    pub offset: i32,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]

pub enum Location {
    /// The data is in the register.
    Register(Register),
    /// The data is in the stack.
    Stack(Stack),
    /// The data is in the data section.
    Data(Data),
}

pub trait ToLocation {
    fn to_loc(self) -> Location;
}

impl ToLocation for Register {
    fn to_loc(self) -> Location {
        Location::Register(self)
    }
}

impl ToLocation for Stack {
    fn to_loc(self) -> Location {
        Location::Stack(self)
    }
}

impl ToLocation for Data {
    fn to_loc(self) -> Location {
        Location::Data(self)
    }
}
