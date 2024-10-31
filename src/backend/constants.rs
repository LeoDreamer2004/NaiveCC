pub const MAX_STACK_SIZE: usize = 1 << 22;
pub const MAX_PARAM_REG: usize = 8;
pub const INT_SIZE: usize = 4;

pub fn is_imm12(imm: i32) -> bool {
    imm >= -2048 && imm <= 2047
}
