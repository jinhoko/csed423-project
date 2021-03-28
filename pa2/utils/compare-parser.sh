#!/bin/bash

echo ""
echo "##############################"
echo "### Result comparison tool ###"
echo "##############################"
echo "(comparing parser output)"
echo ""

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`
verbosity=0

REF_PARSER=$2
USER_PARSER=$3
LEXER=$4
VERBOSE=$5

if [ $# -lt '4' ]; then
    echo "Error occured : wrong inputs!"; exit 1;
fi

if [ $5 == '-v' ]; then
    ((verbosity=1))
    echo "Verbose option ON";
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
  $LEXER $input | $REF_PARSER > _ref_out 2>&1 # delete 2>&1 to remove verbosity
  $LEXER $input | $USER_PARSER > _user_out 2>&1 

  diff _ref_out _user_out > /dev/null 2>&1 
  if [ $? == 0 ]; then
   echo "[${green}PASS${reset}] $input"
   ((pass_cnt += 1))
  else
   echo "[${red}FAIL${reset}] $input"
   if [ $verbosity -eq 1 ]; then
     echo "${green}$(cat _ref_out)${reset}" | head -n 5 && echo "..."
     echo "${red}$(cat _user_out)${reset}" | head -n 5 && echo "..."
   fi
  fi
  rm _ref_out
  rm _user_out
done

echo ""
echo "${pass_cnt}/${input_size} test(s) passed."
echo ""
