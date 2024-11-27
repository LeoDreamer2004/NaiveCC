pub const MAX_STACK_SIZE: usize = 1 << 22;
pub const MAX_PARAM_REG: usize = 8;
pub const INT_SIZE: usize = 4;
pub const IMM12_BOUND: usize = 1 << 11;

pub type Imm32 = i32;
pub type Imm12 = i32;
pub fn is_imm12(imm: i32) -> bool {
    imm >= -(IMM12_BOUND as i32) && imm <= (IMM12_BOUND - 1) as i32
}
