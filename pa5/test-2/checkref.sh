#! /bin/bash

# init
if [ -f "result.txt" ]; then
  rm result.txt
fi
make clean

# logic
for f in *.cl
do
  if [ -f "${f%.cl}.in" ]; then
    make "${f%.cl}.out" < "${f%.cl}.in" >/dev/null 2>&1
  else
    make "${f%.cl}.out" >/dev/null 2>&1
  fi
  if [ "$?" -ne 0 ]; then
    echo "FAILED    ${f%}"
    continue
  fi

  diff "${f%.cl}.out" "${f%.cl}.stdout" >/dev/null 2>&1
  if [ "$?" -eq 0 ]; then
    echo "OK        ${f%}"
  else 
    echo "WRONG     ${f%}"
  fi
done
