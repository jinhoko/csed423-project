-- dispach

class Main inherits IO
{
   main() : Object { {

      (new C)@A.print();   -- A
      (new B)@A.print();   -- A
      (new C).print();     -- C
      (new E).print();     -- C
      (new D).print();     -- D
      (new D).copy_print();   -- D            
      out_string( self@IO.type_name() );  -- Main
    
   } };
};

class A inherits IO {
    print(): Object {
        out_string("A")
    };
};

class B inherits A {
    print(): Object {
        out_string("B")
    };
};

class C inherits B {
    print(): Object {
        out_string("C")
    };
};

class E inherits C {
    printttt(): Object {
        out_string("Not me!")
    };
};

class D inherits IO{
    print(): Object {
        out_string( self@Object.type_name() )
    };
    copy_print(): Object{
       out_string( self@Object.copy().type_name() )
    };
};

