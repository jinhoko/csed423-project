#!/bin/bash

echo ""
echo "##############################"
echo "### Result comparison tool ###"
echo "##############################"
echo "(comparing semant output)"
echo ""

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`
verbosity=0
sort=0

REF_SEMANT=$2
USER_SEMANT=$3
LEXER=$4
PARSER=$5
VERBOSE=$6
SORT=$7

if [ $# -lt '5' ]; then
    echo "Error occured : wrong inputs!"; exit 1;
fi

if [ $6 == '-v' -o $7 == '-v' ]; then
    ((verbosity=1))
    echo "Verbose option ON";
fi

if [ $6 == '-s' -o $7 == '-s' ]; then
    ((sort=1))
    echo "Sort option ON";
fi

INPUT_FILE=$1

input_list=()
while IFS= read -r line; do
  if [[ "$line" == "" ]]; then continue; fi;
  input_list+=($line)
done < $INPUT_FILE

input_size=${#input_list[@]}
pass_cnt=0

for input in ${input_list[@]}; do
  $LEXER $input | $PARSER | $REF_SEMANT  > _ref_out 2>&1 # delete 2>&1 to remove verbosity
  $LEXER $input | $PARSER | $USER_SEMANT > _user_out 2>&1 

  if [ $sort == 1 ]; then
    diff <(sort _ref_out) <(sort _user_out) > /dev/null 2>&1 
  else
    diff _ref_out _user_out > /dev/null 2>&1
  fi
  
  if [ $? == 0 ]; then
   echo "[${green}PASS${reset}] $input"
   ((pass_cnt += 1))
   if [ $verbosity -eq 1 ]; then
     echo "${green}$(cat _ref_out)${reset}" | head -n 10 && echo "..."
     echo "${red}$(cat _user_out)${reset}" | head -n 10 && echo "..."
   fi
  else
   echo "[${red}FAIL${reset}] $input"
   if [ $verbosity -eq 1 ]; then
     echo "${green}$(cat _ref_out)${reset}" | head -n 10 && echo "..."
     echo "${red}$(cat _user_out)${reset}" | head -n 10 && echo "..."
   fi
  fi
  rm _ref_out
  rm _user_out
done

echo ""
echo "${pass_cnt}/${input_size} test(s) passed."
echo ""
