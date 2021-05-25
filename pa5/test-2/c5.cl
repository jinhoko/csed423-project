class Main {
  main(): Int {{


    let x:Int <- 8 in {
      while x < 10 loop x <- x + 1 pool;
      x;
    };


    let x:Int <- 8 in {
      while { 1+1; x < 10; }  loop { x <- x + 1; new Main; } pool;
      x;
    };

    let x:Main <- (new Main) in {
      while { x = x; 1+1; true; }  loop { x = x; new Main; } pool;
      x;
    1;
    }; -- infinite loop

  }};
};


-- checking loop