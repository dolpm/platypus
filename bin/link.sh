$(clang ./bin/builtins.c -S -O3 -emit-llvm -o $1)
cat <<-EOF > $1
$(echo "let as_llvm_ir = {|")
$(cat $1)
EOF
echo "|}" >> $1
# grep -v '^target ' $1 > tmpfile.ml && mv tmpfile.ml $1