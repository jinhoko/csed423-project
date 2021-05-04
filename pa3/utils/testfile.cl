class Ma inherits IO {
    main() : SELF_TYPE {
	{
	    out_string((new Object).type_name().substr(4,1)).
	    out_string((isvoid self).type_name().substr(1,3));
	    out_string("\n");
	}
    };
};

class A inherits B {
   main() : Int {
         1
   };
};

class B inherits C {
   main() : Int {
         1
   };
};

class C inherits A {
   main() : Int {
         1
   };
};

class AA inherits BB {
   main() : Int {
         1
   };
};

class BB inherits CC {
   main() : Int {
         1
   };
};

class CC inherits AA {
   main() : Int {
         1
   };
};


