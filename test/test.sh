#!/bin/bash

# Terminal output colors
Color_Off='\033[0m'
BGreen='\033[1;32m'
BRed='\033[1;31m'
BYellowUnderlined='\033[4;33m'

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
        echo -e "${in_file} ${BRed}failed...${Color_Off}"
    else
        echo -e "${in_file} ${BGreen}passed!${Color_Off}"
        if [ ! -z $tmemcheck ] && [ "$tflag" = "-e" ]
        then
            # compile program
            $(../platypus -c ${in_file} 2>&1)
            # get executable name
            exec_name=$(echo $(basename $in_file) | sed "s/\..*//")
            # run on valgrind
            if valgrind --log-fd=1 ./${exec_name} | grep -q "All heap blocks were freed";
            then
            	echo -e "${in_file} ${BGreen}passed mem check!${Color_Off}"
            else
            	echo -e "${in_file} ${BRed}failed mem check...${Color_Off}"
            fi
            # clean up exec
            $(rm -rf ${exec_name})
        fi  
    fi
}

test_dirs=("ast" "sast" "compile")
test_flags=("-a" "-s" "-e")

flag_map=(
    "ast|-a"
    "sast|-s"
    "compile|-e"
)


if [ ! -z $ttype ]
then
    test_dirs=($ttype)

    # get the correct flags
    test_flags=()
    for item in "${flag_map[@]}"
    do
        ttyp=$(echo "${item}"|awk -F "|" '{print $1}')
        flags=$(echo "${item}"|awk -F "|" '{print $2}')
        if [ "$ttype" = "$ttyp" ]
        then 
            test_flags=($flags)
        fi
    done

    # Just run the single specified test
    if [ ! -z $tname ]
    then
        in_file="./test_cases/${ttype}/${tname}.ppus"
        out_file="./test_cases/${ttype}/${tname}.out"

        run_test $in_file $out_file ${test_flags[0]}
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
    echo -e "${BYellowUnderlined}Running $num_tests ${test_dir} tests...${Color_Off}"

    for i in "${!positive_files[@]}"; do
        run_test ${positive_files[$i]} ${positive_expected_output[$i]} \
            ${test_flag}

    done

    for i in "${!negative_files[@]}"; do
        run_test ${negative_files[$i]} ${negative_expected_output[$i]} \
            ${test_flag}
    done

done