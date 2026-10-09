use super::env::Context;
use koopa::ir::Type;

pub fn setup_builtins(ctx: &mut Context) {
    let program = &mut ctx.program;
    program.new_func_decl("@getint".into(), vec![], Type::get_i32());
    program.new_func_decl("@getch".into(), vec![], Type::get_i32());
    program.new_func_decl(
        "@getarray".into(),
        vec![Type::get_pointer(Type::get_i32())],
        Type::get_i32(),
    );
    program.new_func_decl("@putint".into(), vec![Type::get_i32()], Type::get_unit());
    program.new_func_decl("@putch".into(), vec![Type::get_i32()], Type::get_unit());
    program.new_func_decl(
        "@putarray".into(),
        vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
        Type::get_unit(),
    );
    program.new_func_decl("@starttime".into(), vec![], Type::get_i32());
    program.new_func_decl("@stoptime".into(), vec![], Type::get_i32());
}
