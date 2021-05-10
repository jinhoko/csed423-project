./mine_test.sh > _user_out 2>&1
./ref_test.sh > _ref_out 2>&1
diff <(sort _ref_out) <(sort _user_out)
rm _user_out _ref_out
