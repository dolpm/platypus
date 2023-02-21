#!/bin/bash
positive_files=($(ls -d ./test_cases/pos_*.ppus))
positive_asts=($(ls -d ./test_cases/ast_*.txt))

for i in "${!positive_files[@]}"; do
    diff ${positive_files[$i]} ${positive_asts[$i]}
done