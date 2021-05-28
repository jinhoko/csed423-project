class Main {
  main(): Int {
    {
        let result:Int, a:Int in {
            let a:Int <- 1000 in {
                a <- a+200;
                let b:Int in {
                    a <- a+ b + 30;
                    result <- result + a;
                };
            };
            a <- 30;
            let a:Int <- 1 in
                let b:Int <- 2 in {
                    a <- a + b;
                    result <- result + a;
                }
            ;
            result <- result + a;
            result;
        };
    }
  };
};

-- 1263