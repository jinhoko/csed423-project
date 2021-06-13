class Main inherits IO{
  main(): Int {{
    out_int( if true then 1 else 0 fi );                                      -- 1
    out_string( (if true then 1 else (new Object) fi).type_name() );          -- Int
    out_string( (if true then (new A) else (new B) fi).type_name() );         -- A
    out_string( (if true then (new C) else (new D) fi).type_name() );         -- C
    out_string( (if true then (new A) else (new D) fi).type_name() );         -- A
    out_string( (if true then (new B) else (new D) fi).type_name() );         -- B
    out_string( (if true then (new Object) else (new C) fi).type_name() );    -- Object
    out_string( (if true then (new D) else (new Object) fi).type_name() );    -- D
    out_string( (if true then (new Object) else (new Object) fi).type_name() ); -- Object
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