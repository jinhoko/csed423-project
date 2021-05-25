class Main {
  main(): Int {{
    if true then 1 else 0 fi;
    if true then 1 else (new Object) fi; -- OBject
    if true then (new A) else (new B) fi; -- A
    if true then (new C) else (new D) fi; -- C
    if true then (new A) else (new D) fi; -- A
    if true then (new B) else (new D) fi; -- A
    if true then (new Object) else (new C) fi; -- Object
    if true then (new D) else (new Object) fi; -- Object
    if true then (new Object) else (new Object) fi; -- Object
    1;
  }};
};

-- for lub checking ( A - B / A - C - D )
class A {
    a:Int;
};
class B inherits A {
    b: Int;
};
class C inherits A {
    c: Int;
};
class D inherits C {
    d: Int;
};


-- check if