cargo run -- -riscv ./tests/hello.c -o ./tests/hello.S
clang ./tests/hello.S -c -o ./tests/hello.o -target riscv32-unknown-linux-elf -march=rv32im -mabi=ilp32
ld.lld ./tests/hello.o -L$CDE_LIBRARY_PATH/riscv32 -lsysy -o ./tests/hello
qemu-riscv32-static ./tests/hello
echo "My exit code: $?"

gcc ./tests/hello.c -o ./tests/hello
./tests/hello
echo "Real exit code: $?"