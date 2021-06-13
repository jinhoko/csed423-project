class Main {
  main(): Int {
    {
        let a : Bool <- true in 
            let b: Bool <- false in 
                    b <- not a = not b = b
        ;        

        let a : Int in 
            let b: Int in 
                {
                    a <- 3;
                    b <- 5;
                    a <- 3+5+7;
                    a + b - a + a;
                }    
        ;
    }
  };
};
