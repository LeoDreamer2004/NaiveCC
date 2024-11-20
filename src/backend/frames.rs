use super::registers::*;
use super::{instruction::*, AsmError};
use super::{INT_SIZE, MAX_PARAM_REG, MAX_STACK_SIZE};
use koopa::ir::{FunctionData, ValueKind};
use std::cmp::max;
use std::collections::LinkedList;

#[derive(Debug, Default)]
pub struct Frame {
    pub var_size: usize,
    pub has_call: bool,
    pub param_bias: usize,
}

#[derive(Debug, Default)]
pub struct FrameHelper {
    pub frames: LinkedList<Frame>,
}

impl FrameHelper {
    const SP_IN: u8 = 0;
    const SP_OUT: u8 = 1;
    const SAVE_RA: u8 = 2;
    const RECOVER_RA: u8 = 3;
    const SAVE_S0: u8 = 4;
    const RECOVER_S0: u8 = 5;

    fn need_save_ra(&self) -> bool {
        self.frames.back().map_or(false, |frame| frame.has_call)
    }

    fn need_use_s0(&self) -> bool {
        self.frames
            .back()
            .map_or(false, |frame| frame.param_bias > 0)
    }

    /// Start a new frame.
    pub fn prologue(&mut self, func_data: &FunctionData, asm: &mut AsmProgram) {
        asm.push(Inst::Comment("-- prologue".to_string()));
        // add a placeholder here, waiting for update when the frame size is known.
        asm.push(Inst::Placeholder(Self::SP_IN));

        let mut frame = Frame::default();

        // calculate the parameter bias
        for (_, node) in func_data.layout().bbs() {
            for inst in node.insts().keys() {
                let data = func_data.dfg().value(*inst);
                if let ValueKind::Call(call) = data.kind() {
                    frame.has_call = true;
                    let p_num = call.args().len();
                    if p_num > MAX_PARAM_REG {
                        frame.param_bias =
                            max(frame.param_bias, INT_SIZE * (p_num - MAX_PARAM_REG));
                    }
                }
            }
        }
        self.frames.push_back(frame);

        if self.need_save_ra() {
            asm.push(Inst::Placeholder(Self::SAVE_RA));
        }
        if self.need_use_s0() {
            asm.push(Inst::Placeholder(Self::SAVE_S0));
            asm.push(Inst::Mv(Mv(S0, SP)));
        }
    }


    /// Get the current frame.
    pub fn current_frame_mut(&mut self) -> Result<&mut Frame, AsmError> {
        self.frames.back_mut().ok_or(AsmError::InvalidStackFrame)
    }


    /// Mark an exit of the frame.
    pub fn epilogue(&mut self, asm: &mut AsmProgram) -> Result<(), AsmError> {
        // read all "call" instructions in the function
        asm.push(Inst::Comment("-- epilogue".to_string()));
        if self.need_save_ra() {
            asm.push(Inst::Placeholder(Self::RECOVER_RA));
        }
        if self.need_use_s0() {
            asm.push(Inst::Placeholder(Self::RECOVER_S0));
        }
        asm.push(Inst::Placeholder(Self::SP_OUT));
        Ok(())
    }

    /// End the frame.
    pub fn end(&mut self, asm: &mut AsmProgram) -> Result<(), AsmError> {
        let mut size = 0;
        if self.need_save_ra() {
            size += INT_SIZE;
        }
        if self.need_use_s0() {
            size += INT_SIZE;
        }
        let frame = self.frames.pop_back().ok_or(AsmError::InvalidStackFrame)?;
        size += frame.var_size + frame.param_bias;

        // align to 16
        let size = ((size + 15) & !15) as i32;
        if size > MAX_STACK_SIZE as i32 {
            return Err(AsmError::StackOverflow);
        }

        // recover the place holder
        let mut flag = false;
        for inst in asm.iter_mut().rev() {
            if let Inst::Placeholder(p) = inst {
                match *p {
                    Self::SP_IN => {
                        flag = true;
                        if size == 0 {
                            *inst = Inst::Nop;
                        } else {
                            *inst = Inst::Addi(Addi(SP, SP, -size));
                        }
                        break;
                    }
                    Self::SP_OUT => {
                        *inst = if size == 0 {
                            Inst::Nop
                        } else {
                            Inst::Addi(Addi(SP, SP, size))
                        }
                    }
                    Self::SAVE_RA => {
                        *inst = Inst::Sw(Sw(RA, SP, size - INT_SIZE as i32));
                    }
                    Self::RECOVER_RA => {
                        *inst = Inst::Lw(Lw(RA, SP, size - INT_SIZE as i32));
                    }
                    Self::SAVE_S0 => {
                        *inst = Inst::Sw(Sw(S0, SP, size - 2 * INT_SIZE as i32));
                    }
                    Self::RECOVER_S0 => {
                        *inst = Inst::Lw(Lw(S0, SP, size - 2 * INT_SIZE as i32));
                    }
                    _ => unreachable!("Unknown placeholder"),
                }
            }
        }
        if !flag {
            return Err(AsmError::InvalidStackFrame);
        }
        Ok(())
    }
}
