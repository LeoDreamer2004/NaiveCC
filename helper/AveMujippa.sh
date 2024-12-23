cargo run -- -koopa ./tests/hello.c -o ./tests/hello.koopa
koopac ./tests/hello.koopa | llc --filetype=obj -o ./tests/hello.o
clang ./tests/hello.o -L$CDE_LIBRARY_PATH/native -lsysy -o ./tests/hello
./tests/hello
echo $?