$(clang ./bin/builtins.c -S -O3 -emit-llvm -o $1)
cat <<-EOF > $1
$(echo "let as_llvm_ir = {|")
$(cat $1)
EOF
echo "|}" >> $1
