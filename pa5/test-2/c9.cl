-- conform

class Main inherits IO
{
    x:Object;
    y:Int <- new Int;
   main() : Int {{
            x <- new Int;
            main22();
		    1;
        }};
   main2( x:Object): Int { x <- 3 };
   main22(): Int {{ out_int(y); 1; }};
};
