class Main {
  main(): Int {
    {
        if true then 1 else 2 fi;
        if 0 = 1 then true else true fi;
        if 0 = 1 then true else false fi;
        let a: Int <- 3 in {
            a <- a+3;
            if a = 6 then 1 else 3+4 fi;
        };
    }
  };
};
