#!/bin/bash

# Terminal output colors
Color_Off='\033[0m'
BGreen='\033[1;32m'
BRed='\033[1;31m'

test_dirs=("ast" "sast")
test_flags=("-a" "-s")

for test in "${!test_dirs[@]}"; do
    test_dir=${test_dirs[$test]}
    test_flag=${test_flags[$test]}

    
    positive_files=($(ls -d ./test_cases/${test_dir}/pos_*.ppus))
    positive_expected_output=($(ls -d ./test_cases/${test_dir}/pos_*.out))

    negative_files=($(ls -d ./test_cases/${test_dir}/neg_*.ppus))
    negative_expected_output=($(ls -d ./test_cases/${test_dir}/neg_*.out))

    num_tests=$(( ${#positive_files[@]} + ${#negative_files[@]} ))
    echo "Running $num_tests ${test_dir} tests..."

    #${#a[@]}

    for i in "${!positive_files[@]}"; do
        output=$(../platypus ${test_flag} ${positive_files[$i]} | diff - ${positive_expected_output[$i]})

        if [ "$output" != "" ]; then
            echo -e "Test ${positive_files[$i]} ${BRed}failed...${Color_Off}"
        else
            echo -e "Test ${positive_files[$i]} ${BGreen}passed!${Color_Off}"
        fi

    done

    for i in "${!negative_files[@]}"; do
        output=$(../platypus ${test_flag} ${negative_files[$i]} 2>&1)
        output=$(echo ${output} | diff - ${negative_expected_output[$i]})

        # We want negative tests to throw errors
        if [ "$output" != "" ]; then
            echo -e "Test ${negative_files[$i]} ${BRed}failed...${Color_Off}"
        else
            echo -e "Test ${negative_files[$i]} ${BGreen}passed!${Color_Off}"
        fi

    done

done