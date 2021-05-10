class Main { main() : Int {0 }; };

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