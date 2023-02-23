#!/bin/bash

positive_files=($(ls -d ./test_cases/pos_*.ppus))
positive_expected_output=($(ls -d ./test_cases/pos_*.out))

negative_files=($(ls -d ./test_cases/neg_*.ppus))
negative_expected_output=($(ls -d ./test_cases/neg_*.out))

num_tests=$(( ${#positive_files[@]} + ${#negative_files[@]} ))
echo "Running $num_tests tests..."

#${#a[@]}

for i in "${!positive_files[@]}"; do
    output=$(../platypus -a ${positive_files[$i]} | diff - ${positive_expected_output[$i]})

    if [ "$output" != "" ]; then
        echo "Test ${positive_files[$i]} failed..."
    else
        echo "Test ${positive_files[$i]} passed!"
    fi

done

for i in "${!negative_files[@]}"; do
    output=$(../platypus -a ${negative_files[$i]} 2>&1)
    output=$(echo ${output} | diff - ${negative_expected_output[$i]})

    # We want negative tests to throw errors
    if [ "$output" != "" ]; then
        echo "Test ${negative_files[$i]} failed..."
    else
        echo "Test ${negative_files[$i]} passed!"
    fi

done