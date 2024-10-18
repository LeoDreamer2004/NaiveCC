use koopa::ir::{FunctionData, Type};

pub fn builtin_functions() -> Vec<FunctionData> {
    let get_int = FunctionData::new_decl("@getint".into(), vec![], Type::get_i32());
    let get_ch = FunctionData::new_decl("@getch".into(), vec![], Type::get_i32());
    let get_array = FunctionData::new_decl(
        "@getarray".into(),
        vec![Type::get_pointer(Type::get_i32())],
        Type::get_i32(),
    );
    let put_int = FunctionData::new_decl("@putint".into(), vec![Type::get_i32()], Type::get_unit());
    let put_ch = FunctionData::new_decl("@putch".into(), vec![Type::get_i32()], Type::get_unit());
    let put_array = FunctionData::new_decl(
        "@putarray".into(),
        vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
        Type::get_unit(),
    );
    let start_time = FunctionData::new_decl("@starttime".into(), vec![], Type::get_i32());
    let end_time = FunctionData::new_decl("@stoptime".into(), vec![], Type::get_i32());
    vec![
        get_int, get_ch, get_array, put_int, put_ch, put_array, start_time, end_time,
    ]
}
