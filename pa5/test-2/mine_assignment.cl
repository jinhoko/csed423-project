-- Simple assignment

class Main inherits IO
{
   main() : Int {{
                main2( 1 );
                self@Main.main2(1);
                self@Main.main2(false);
                self@Main.main2( "hey" );
                self@Main.main2( "heyyy" );
                main2(new Object);
                self@Main.main22();
                self@Main.main22();
                new Main2;
                new Object;
                new Int;
                true = isvoid 1;
                (new Main4).main3();
		         1;
        }};
   main2( x:Object): Int { x <- 3 };
   main22(): Int { 1 };
};
class Main2 {
   main2(): Int { 1 };
};
class Main3 {
   main3(): SELF_TYPE { self };
};
class Main4 inherits Main3 {
   main44():Int { 1};
};
