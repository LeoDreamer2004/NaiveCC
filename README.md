# NaiveCC

## 简介

**NaiveCC** (Naive C Language Compiler) 是基于 Rust 实现的高效的玩具 SysY 编译器，用于北大编译原理课程。

中间代码源于来自课程中的 [Koopa IR](https://github.com/pku-minic/koopa)。目标代码为 RISC-V 汇编。

## 特点

- **前后端分离**：前端将源代码转换为 IR，后端将 IR 转换为汇编代码，实现代码的分层设计和解耦。
- **高效**：实现了图着色寄存器分配算法、引入窥孔优化、强度削弱、常量传播等优化策略，同时通过数据流分析进一步做了更深层的优化。在最终性能测试中，可在 200s 内完成所有任务。

## 使用

```shell
cargo run -- -koopa <input> -o <output>     # 生成中间代码
cargo run -- -riscv <input> -o <output>     # 生成 RISC-V 汇编代码
```

当 `<output>` 为 `-debug` 时，输出到标准输出。

`helper` 目录下有一些辅助脚本，请进入 Docker 环境后运行：

- `RaiseADocker.sh`：启动课程所用 Docker 容器
- `ItsMyRust.sh`：生成中间代码
- `AveMujippa.sh`：利用 koopac（基于 LLVM），检测生成的中间代码是否正确
- `Morfnicasm.sh`：生成 RISC-V 汇编代码
- `PoppinTest.sh`：与 gcc 比较，检测生成的汇编代码是否正确
- `BanGTest.sh`：运行 autotest 测试某个 lvl

## 已知问题

在关闭所有优化时（只需在 `main.rs` 注释掉 `opt_ir` 和 `opt_asm` 即可），lv8: 20_many_params2：WA；另外优化可能潜在问题，未经过详细测试。

## 更多信息

- 课程文档[链接](https://pku-minic.github.io/online-doc/)
- 关于具体实现，详见[报告](./report/report.md)
