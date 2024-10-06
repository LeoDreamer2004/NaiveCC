pub struct CompUnit {
    pub comp_items: Vec<CompItem>,
}

pub enum CompItem {
    FuncDef(FuncDef),
    Decl(Decl),
}

pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

pub enum BType {
    Int,
}

pub struct ArrayIndex {
    pub usize: Vec<ConstExp>,
}

pub struct ConstDef {
    pub ident: String,
    pub array_size: Option<ArrayIndex>,
    pub const_init_val: ConstInitVal,
}

pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstInitVals(Vec<Box<ConstInitVal>>),
}

pub struct ConstExp {
    pub add_exp: AddExp,
}

pub struct VarDecl {
    b_type: BType,
    var_defs: Vec<VarDef>,
}

pub struct VarDef {
    ident: String,
    array_size: Option<ArrayIndex>,
    init_val: Option<InitVal>,
}

pub enum InitVal {
    Exp(Exp),
    InitVals(Vec<Box<InitVal>>),
}

pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub params: FuncFParams,
    pub block: Block,
}

pub enum FuncType {
    Void,
    Int,
}

pub struct FuncFParams {
    pub params: Vec<FuncFParam>,
}

pub struct FuncFParam {
    pub b_type: BType,
    pub ident: String,
    // must be as a[][5][6]
    pub array_size: Option<ArrayIndex>,
}

pub struct Block {
    pub block_items: Vec<BlockItem>,
}

pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

pub enum Stmt {
    Assign(Assign),
    Exp(Exp),
    Block(Block),
    If(If),
    While(While),
    Break,
    Continue,
    Return(Return),
    Empty,
}

pub struct Assign {
    pub l_val: LVal,
    pub exp: Exp,
}

pub struct If {
    pub cond: Cond,
    pub true_stmt: Box<Stmt>,
    pub false_stmt: Option<Box<Stmt>>,
}

pub struct While {
    pub cond: Cond,
    pub stmt: Box<Stmt>,
}

pub struct Return {
    pub exp: Option<Exp>,
}

pub struct Exp {
    pub add_exp: AddExp,
}

pub struct Cond {
    pub l_or_exp: LOrExp,
}

pub struct LVal {
    pub ident: String,
    pub exp: Option<ArrayIndex>,
}

pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
}

pub struct Number {
    pub value: i32,
}

pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    FuncCall(FuncCall),
    UnaryOpExp(UnaryOpExp),
}

pub struct FuncCall {
    pub ident: String,
    pub args: FuncRParams,
}

pub struct UnaryOpExp {
    pub unary_op: UnaryOp,
    pub unary_exp: Box<UnaryExp>,
}

pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

pub struct FuncRParams {
    pub exps: Vec<Exp>,
}

pub enum MulExp {
    UnaryExp(UnaryExp),
    MulOpExp(MulOpExp),
}

pub struct MulOpExp {
    pub mul_exp: Box<MulExp>,
    pub mul_op: MulOp,
    pub unary_exp: Box<UnaryExp>,
}

pub enum MulOp {
    Mul,
    Div,
    Mod,
}

pub enum AddExp {
    MulExp(MulExp),
    AddOpExp(AddOpExp),
}

pub struct AddOpExp {
    pub add_exp: Box<AddExp>,
    pub add_op: AddOp,
    pub mul_exp: MulExp,
}

pub enum AddOp {
    Plus,
    Minus,
}

pub enum RelExp {
    AddExp(AddExp),
    RelOpExp(RelOpExp),
}

pub struct RelOpExp {
    pub rel_exp: Box<RelExp>,
    pub rel_op: RelOp,
    pub add_exp: AddExp,
}

pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

pub enum EqExp {
    RelExp(RelExp),
    EqOpExp(EqOpExp),
}

pub struct EqOpExp {
    pub eq_exp: Box<EqExp>,
    pub eq_op: EqOp,
    pub rel_exp: RelExp,
}

pub enum EqOp {
    Eq,
    Ne,
}

pub enum LAndExp {
    EqExp(EqExp),
    LAndOpExp(LAndOpExp),
}

pub struct LAndOpExp {
    pub l_and_exp: Box<LAndExp>,
    pub l_and_op: LAndOp,
    pub eq_exp: EqExp,
}

pub enum LAndOp {
    And,
}

pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOpExp(LOrOpExp),
}

pub struct LOrOpExp {
    pub l_or_exp: Box<LOrExp>,
    pub l_or_op: LOrOp,
    pub l_and_exp: LAndExp,
}

pub enum LOrOp {
    Or,
}