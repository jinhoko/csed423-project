class Main {
  main(): Int {
    {
        let c: Int <- 1 in
            {
                while c = 1 loop c <- 1 pool;
                2;
            }
        ;
    }
  };
};
