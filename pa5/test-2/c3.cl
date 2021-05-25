class Main inherits IO
{
   main() : Int {{
                let x:Main2 in 1;
                let x:Main2 <- (new Main2) in 1;
                let x:Main <- (new Main3) in 1;
                let x:Int in 1;
                let x:Int <- 1 in 1;
                let x:Bool in 1;
                let x:Bool <- true in 1;
                let x:String in 1;
                let x:String <- "hey" in 1;
		        1+1;
        }};
   
};
class Main2 {
    x:Int;
    y:Int <- 1;
    z:Main3;
    w:Main3 <- (new Main3);
    ww:Main <- (new Main3);
    a:String;
    b:String <- "hey";
};

class Main3 inherits Main {
    x: Int;
};

-- checking all lets and inits