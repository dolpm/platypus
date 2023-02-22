#!/bin/bash

# TODO: fixed this scuffed way of matching .ppus with .txt
positive_files=($(ls -d ./test_cases/pos_*.ppus))
positive_asts=($(ls -d ./test_cases/ast_*.txt))

for i in "${!positive_files[@]}"; do
    output=$(../platypus -a ${positive_files[$i]} | diff - ${positive_asts[$i]})

    if [ "$output" != "" ]; then
        echo "Test ${positive_files[$i]} failed..."
    else
        echo "Test ${positive_files[$i]} passed!"
    fi

done

negative_files=($(ls -d ./test_cases/neg_*.ppus))

for i in "${!negative_files[@]}"; do
    output=$(../platypus -a ${negative_files[$i]} 2> /dev/null)

    # We want negative tests to throw errors
    if [ "$?" -ne 0 ]; then
        echo "Test ${negative_files[$i]} passed!"
    else
        echo "Test ${negative_files[$i]} failed..."
    fi

done