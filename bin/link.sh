$(clang ./bin/vec.c -S -emit-llvm -o $1)
printf '%s\n%s\n' "let as_llvm_ir = {|" "$(cat $1)" > $1
echo "|}" >> $1