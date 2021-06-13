class Main inherits IO {
  main(): Int {{


    let x:Int <- 5 in {
      while x < 10 loop { x <- x + 1; out_int(x); } pool;
      x;
    };


    let x:Int <- new Int in {
      while { 1+1; x < 5; }  loop { x <- x + 1; new Main; out_int(x); } pool;
      x;
    };

--    let x:Main <- (new Main) in {
--      while { x = x; 1+1; true; }  loop { x = x; new Main; } pool;
--      x;
--    1;
--    }; -- infinite loop

  }};
};


-- checking loop