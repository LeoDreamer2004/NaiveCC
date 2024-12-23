export RUST_BACKTRACE=1
cargo run -- -riscv ./tests/hello.c -o ./tests/hello.S
# cargo run -- -riscv ./tests/hello.c -o -debug

