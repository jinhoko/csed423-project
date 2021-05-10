class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};


class Main3 inherits IO {
    main() : SELF_TYPE {
	{
	    out_string((new Object).type_name().substr(4,1)).
	    out_string((isvoid self).type_name().substr(1,3));
	    out_string("\n");
	}
    };
};

class Main2 inherits IO {
    main() : Int {
        1
    };
};

class Mainnn inherits Main1 {
    main( a:Int, b:Int ) : SELF_TYPE {
        {
            out_string((new Object).type_name().substr(4,1)).
            out_string((isvoid self).type_name().substr(1,3));
            out_string("\n");
        }
    };

};

class Mainnnnnnnn inherits Mainnn {
    main( a:Bool  ) : SELF_TYPE {
        {
            out_string((new Object).type_name().substr(4,1)).
            out_string((isvoid self).type_name().substr(1,3));
            out_string("\n");
        }
    };

};


class Main1 inherits IO {
    main( a:Bool ) : SELF_TYPE {
        {
            out_string((new Object).type_name().substr(4,1)).
            out_string((isvoid self).type_name().substr(1,3));
            out_string("\n");
        }
    };
    main(): String { 1  };
    main(): Bool { 1  };
    main: Int;
    main: Bool;
    main: Bool <- 1;
    main: Booooool <- 1;
    self: Int;
    self(): Bool { true };
    self(): Bool { true };
};

class MyClass {
      o1():SELF_TYPE { 5 }; 
      o2():SELF_TYPE {new Int};
      o3():Object {new SELF_TYPE};
      o4():SELF_TYPE {new Object} ;
      o5():Object {new Object} ; 
      o6():SELF_TYPE {new SELF_TYPE} ;
      o7():MyClass {new MyClass} ;
      o8():MyClassChild {new MyClass} ;
      o9():MyClass {new MyClassChild} ;
      o10():YourClass {new Myclass} ;
      o11():MyClass {new YourClass} ;
      o12():MyClass { new SELF_TYPE };
      o13():SELF_TYPE { new MyClass };
      o14():SELF_TYPE { new YourClass };
      o15():YourClass { new SELF_TYPE };
      o16():MyClassChild { new YourClass };
};

class MyClassChild inherits MyClass  {
      o17():SELF_TYPE { new MyClass };
      o18():SELF_TYPE { new MyClassChild };
      o19():MyClass { new SELF_TYPE };
      o20():MyClassChild { new SELF_TYPE };
      o21():MyClass { new MyClassChild };
      o22():MyClassChild { new MyClassChild };
      o23():MyClass { new MyClass };
      o24():SELF_TYPE { new SELF_TYPE };
      o25():SELF_TYPE { new YourClass };
      o26():SELF_TYPE { new Int };
      o27():SELF_TYPE { new String };

};

class MyClassChildObj inherits MyClass  {
      o17:SELF_TYPE <- new MyClass ;
      o18:SELF_TYPE <- new MyClassChildObj ;
      o19:MyClass <- new SELF_TYPE ;
      o20:MyClassChild <- new SELF_TYPE ;
      o21:MyClass <- new MyClassChildObj ;
      o22:MyClassChild <- new MyClassChildObj ;
      o23:MyClass <- new MyClass ;
      o24:SELF_TYPE <- new SELF_TYPE ;
      o25:SELF_TYPE <- new YourClass ;
      o26:SELF_TYPE <- new Int ;
      o27:SELF_TYPE <- new String ;

};

class YourClass  {
      do(arg:Object) : Object { 1};
      o100(x: SELF_TYPE) : SELF_TYPE { x <- new SELF_TYPE };
      o101(x: SELF_TYPE) : SELF_TYPE { x <- new YourClass };
      o102(x: SELF_TYPE) : YourClass { x <- new SELF_TYPE };
      o103(x: YourClass) : SELF_TYPE { x <- new SELF_TYPE };
      o104(x: YourClass) : YourClass { x <- new SELF_TYPE };
      o105(x: SELF_TYPE) : YourClass { x <- new YourClass };
      o106(x: YourClass) : SELF_TYPE { x <- new YourClass };
      o107(x: YourClass) : YourClass { x <- new YourClass };
};

class MyClass33 {
  do(arg:Object) : Object {
      {     let o3:SELF_TYPE <- 5 in 1; 
            let o3:SELF_TYPE <- new Int in 1;
            let o3:Object <- new SELF_TYPE in 1;
            let o3:SELF_TYPE <- new Object in 1;
            let o3:Object <- new Object in 1; 
            let o3:SELF_TYPE <- new SELF_TYPE in 1;
            let o3:MyClass33 <- new MyClass33 in 1;
            let o3:MyClassChild3 <- new MyClass33 in 1;
            let o3:MyClass33 <- new MyClassChild3 in 1;
            let o3:YourClass3 <- new MyClass33 in 1;
            let o3:MyClass33 <- new YourClass3 in 1;
      }
  };
};

class MyClassChild3 inherits MyClas33  {
      do(arg:Object) : Object { 1};
};

class YourClass3  {
      do(arg:Object) : Object { 1};
};