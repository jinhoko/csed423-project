class Main {
  main(): Int {
    {
        let a : Int <- 3 in 
            let b: Int <- 5 in 
                a/b
        ;

        let c : Int <- 3 in 
            let d: Int <- 0 in 
                c/d
        ;

        let e: Int <- 0 in
            e/0
        ;

    }
  };
};
