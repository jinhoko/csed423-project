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

INPUT_FILE=$1

input_list=()
while IFS= read -r line; do
  if [[ "$line" == "" ]]; then continue; fi;
  input_list+=($line)
done < $INPUT_FILE

for input in ${input_list[@]}; do
  $REF_LEXER $input > _ref_out
  $USER_LEXER $input > _user_out

  diff _ref_out _user_out > /dev/null 2>&1
  if [ $? == 0 ]; then
   echo "[${green}PASS${reset}] $input"
  else
   echo "[${red}FAIL${reset}] $input"
  fi
  rm _ref_out
  rm _user_out
done

echo ""
