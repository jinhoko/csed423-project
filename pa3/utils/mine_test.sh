cd ../src && make clean && make && cd ../utils > /dev/null 2>&1
../ref-lexer/lexer ./foo.cl | ../ref-parser/parser | ../src/semant