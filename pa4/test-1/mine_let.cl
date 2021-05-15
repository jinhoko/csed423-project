class Main {
  main(): Int {
    {
        let a : Int in 
            let b: Int in 
                1
        ;

        let d : Int <- 3 in
            d
        ;

        let a : Int <- 1+3/2 in 
            let b: Int in 
                a + b
        ;

        let a : Int <- ~1+3 in 
            let b: Int <- ~1+3 in 
                a + b  
        ;

        let a : Int <- ~1+3 in 
            let b: Bool <- not true in 
                {
                    b;
                    a;
                }
        ;
    }
  };
};
