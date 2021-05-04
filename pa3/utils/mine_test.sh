cd ../src/ && make >/dev/null 2>&1
cd ../utils/
../ref-lexer/lexer ./testfile.cl | ../ref-parser/parser | ../src/semant
