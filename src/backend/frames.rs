use super::instruction::*;
use super::location::Stack;
use super::program::{AsmGlobal, AsmLocal};
use super::registers::*;
use super::{AsmError, INT_SIZE, MAX_PARAM_REG, MAX_STACK_SIZE};
use koopa::ir::{FunctionData, ValueKind};
use std::cmp::max;
use std::collections::LinkedList;

#[derive(Debug, Default)]
pub struct Frame {
    pub var_size: usize,
    pub has_call: bool,
    pub param_bias: usize,
}

/// Easy frame manager for function stack.
/// Automatically generate the prologue and epilogue of the function.
#[derive(Debug, Default)]
pub struct FrameStack {
    pub frames: LinkedList<Frame>,
}

impl FrameStack {
    // Identifiers for placeholders in the prologue and epilogue.
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

    fn current_frame_mut(&mut self) -> Result<&mut Frame, AsmError> {
        self.frames.back_mut().ok_or(AsmError::InvalidStackFrame)
    }

    fn tot_size(&self) -> usize {
        let mut size = 0;
        if self.need_save_ra() {
            size += INT_SIZE;
        }
        if self.need_use_s0() {
            size += INT_SIZE;
        }
        let frame = self.frames.back().unwrap();
        size += frame.var_size + frame.param_bias;

        // align to 16
        (size + 15) & !15
    }

    /// Allocate a new memory in the stack.
    pub fn malloc(&mut self, size: usize) -> Result<Stack, AsmError> {
        let frame = self.current_frame_mut()?;
        let offset = (frame.var_size + frame.param_bias) as i32;
        let address = Stack { base: SP, offset };
        frame.var_size += size;
        Ok(address)
    }

    /// Start a new frame and build the prologue.
    ///
    /// Note: The prologue is not complete until the end_fill is called.
    pub fn build_prologue(&mut self, func_data: &FunctionData, asm: &mut AsmLocal) {
        let insts = asm.insts_mut();

        // add a placeholder here, waiting for update when the frame size is known.
        insts.push(Inst::Placeholder(Self::SP_IN));

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
            insts.push(Inst::Placeholder(Self::SAVE_RA));
        }
        if self.need_use_s0() {
            insts.push(Inst::Placeholder(Self::SAVE_S0));
            insts.push(Inst::Mv(S0, SP));
        }
    }

    /// Mark an exit of the frame and build the epilogue.
    /// In this compilation, there maybe not only one epilogue.
    ///
    /// Note: The epilogue is not complete until the end_fill is called.
    pub fn build_epilogue(&mut self, asm: &mut AsmLocal) -> Result<(), AsmError> {
        // read all "call" instructions in the function
        let insts = asm.insts_mut();
        insts.push(Inst::Comment("-- epilogue".to_string()));
        if self.need_save_ra() {
            insts.push(Inst::Placeholder(Self::RECOVER_RA));
        }
        if self.need_use_s0() {
            insts.push(Inst::Placeholder(Self::RECOVER_S0));
        }
        insts.push(Inst::Placeholder(Self::SP_OUT));
        Ok(())
    }

    /// End the frame and fill the prologue and epilogue.
    ///
    /// When this function is called, the frame is closed and the stack is ready to use.
    pub fn end_fill(&mut self, asm: &mut AsmGlobal) -> Result<(), AsmError> {
        let size = self.tot_size() as i32;
        self.frames.pop_back().ok_or(AsmError::InvalidStackFrame)?;
        if size > MAX_STACK_SIZE as i32 {
            return Err(AsmError::StackOverflow);
        }

        // recover the place holder
        for local in asm.locals_mut().iter_mut().rev() {
            for inst in local.insts_mut().iter_mut().rev() {
                if let Inst::Placeholder(p) = inst {
                    match *p {
                        Self::SP_IN => {
                            if size == 0 {
                                *inst = Inst::Nop;
                            } else {
                                *inst = Inst::Addi(SP, SP, -size);
                            }
                            return Ok(());
                        }
                        Self::SP_OUT => {
                            *inst = if size == 0 {
                                Inst::Nop
                            } else {
                                Inst::Addi(SP, SP, size)
                            }
                        }
                        Self::SAVE_RA => {
                            *inst = Inst::Sw(RA, SP, size - INT_SIZE as i32);
                        }
                        Self::RECOVER_RA => {
                            *inst = Inst::Lw(RA, SP, size - INT_SIZE as i32);
                        }
                        Self::SAVE_S0 => {
                            *inst = Inst::Sw(S0, SP, size - 2 * INT_SIZE as i32);
                        }
                        Self::RECOVER_S0 => {
                            *inst = Inst::Lw(S0, SP, size - 2 * INT_SIZE as i32);
                        }
                        _ => unreachable!("Unknown placeholder"),
                    }
                }
            }
        }
        Err(AsmError::InvalidStackFrame)
    }
}
