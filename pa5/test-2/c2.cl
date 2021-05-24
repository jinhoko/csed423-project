-- Simple assignment

class Main inherits IO
{
   main() : Int {{
                -- main2( 1 );
                -- main2(new Object);
                self@Main.main22();
                                self@Main.main22();

                -- (new Main4).main3();
		         1;
        }};
   main2( x:Object): Int { 1 };
   main22(): Int { 1};
};
class Main2 {
   main2(): Int { 1};
};
class Main3 {
   main3(): SELF_TYPE { self};
};
class Main4 inherits Main3 {
   main44():Int { 1};
};
