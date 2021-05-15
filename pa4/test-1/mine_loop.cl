class Main {
  main(): Int {
    {


        let b: Bool <- false in
            {
                while b = false loop b <- not b pool;
                1;
            }
        ;

        let a: Int <- 0 in
            {
                while a < 3 loop a <- a + 1 pool;
                a;
            }
        ;
    }
  };
};
