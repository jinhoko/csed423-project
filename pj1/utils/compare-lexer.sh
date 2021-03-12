#!/bin/bash

echo ""
echo "##############################"
echo "### Result comparison tool ###"
echo "##############################"
echo "(comparing lexer output)"
echo ""

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

REF_LEXER=$2
USER_LEXER=$3

if [ $# -ne 3 ]; then
    echo "Error occured : wrong inputs!"; exit 1;
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
  $REF_LEXER $input > _ref_out
  $USER_LEXER $input > _user_out

  diff _ref_out _user_out > /dev/null 2>&1
  if [ $? == 0 ]; then
   echo "[${green}PASS${reset}] $input"
   ((pass_cnt += 1))
  else
   echo "[${red}FAIL${reset}] $input"
  fi
  rm _ref_out
  rm _user_out
done

echo ""
echo "${pass_cnt}/${input_size} test(s) passed."
echo ""
