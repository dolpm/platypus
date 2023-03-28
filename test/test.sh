#!/bin/bash

# Terminal output colors
Color_Off='\033[0m'
BGreen='\033[1;32m'
BRed='\033[1;31m'

test_dirs=("ast" "sast" "codegen" "compile")
test_flags=("-a" "-s" "-l" "-e")

# Bash is my religion.
run_test () {
    in_file=$1
    out_file=$2
    tflag=$3

    output=""

    ../platypus ${tflag} ${in_file} &> /dev/null

    if [ $? -ne 0 ]
    then # This is a negative test, capture stderr
        output=$(../platypus ${tflag} ${in_file} 2>&1)
        output=$(echo ${output} | diff - ${out_file})
    else # This is a postive test, capture stdout
        output=$(../platypus ${tflag} ${in_file} | 
            diff - ${out_file})
    fi

    if [ "$output" != "" ]; then
        echo -e "Test ${in_file} ${BRed}failed...${Color_Off}"
    else
        echo -e "Test ${in_file} ${BGreen}passed!${Color_Off}"
    fi
}

# Map from type of test to needed compiler flag
declare flag_map=( ["ast"]="-a" ["sast"]="-s" ["codegen"]="-l" ["compile"]="-e")

if [ ! -z $ttype ]
then
    test_dirs=($ttype)
    test_flags=("${flag_map[$ttype]}")

    # Just run the single specified test
    if [ ! -z $tname ]
    then
        in_file="./test_cases/${ttype}/${tname}.ppus"
        out_file="./test_cases/${ttype}/${tname}.out"

        run_test $in_file $out_file ${flag_map[$ttype]}
        exit 0
    fi
fi

for test in "${!test_dirs[@]}"; do
    test_dir=${test_dirs[$test]}
    test_flag=${test_flags[$test]}

    
    positive_files=($(ls -d ./test_cases/${test_dir}/pos_*.ppus 2>/dev/null))
    positive_expected_output=($(ls -d ./test_cases/${test_dir}/pos_*.out 2>/dev/null))

    negative_files=($(ls -d ./test_cases/${test_dir}/neg_*.ppus 2>/dev/null))
    negative_expected_output=($(ls -d ./test_cases/${test_dir}/neg_*.out 2>/dev/null))

    num_tests=$(( ${#positive_files[@]} + ${#negative_files[@]} ))
    echo "Running $num_tests ${test_dir} tests..."

    #${#a[@]}

    for i in "${!positive_files[@]}"; do
        run_test ${positive_files[$i]} ${positive_expected_output[$i]} \
            ${test_flag}

    done

    for i in "${!negative_files[@]}"; do
        run_test ${negative_files[$i]} ${negative_expected_output[$i]} \
            ${test_flag}
    done

done