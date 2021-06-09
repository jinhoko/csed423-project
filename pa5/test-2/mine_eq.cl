-- typecase

class Main inherits IO
{
   main() : Object { {

    if 3 = 3 then out_int(0) else out_int(9) fi;                    -- 0
    if 0 = new Int then out_int(1) else out_int(2) fi;              -- 1
    if 1 = new Int then out_int(3) else out_int(4) fi;              -- 4
    if true = new Bool then out_int(5) else out_int(6) fi;          -- 6
    if new String = new String then out_int(7) else out_int(8) fi;  -- 7
    if "foo" = new String then out_int(9) else out_int(10) fi ;     -- 10
    if "bar" = "barrr".substr(0,3) then out_int(7) else out_int(8) fi; -- 7
    if new Main = self then out_int(3) else out_int(4) fi;          -- 4
    if self = self then out_int(5) else out_int(6) fi;              -- 5
    
   } };
};