
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;


Class Wrongblock {
  f():Type{       {{{{ (1+1 - ~5);};};};{1;};   };
};

Class Wrongblock2 {
  f():Type{ {{{{ (1+1 - ~5);};};};{1};}  };
};


Class WrongFormal {
  f(x:Int,y:int):Type { 1 };
};

Class WrongForma2 {
  f(x:Inty:int):Type { 1 };
};

Class WrongForma3 {
  f(x:,y:int):Type { 1 };
};


Class Wrongdispatch1 {
  f(x:Int,y:int):Type { a(,) };
};

Class Wrongdispatch2 {
  f(x:Int,y:int):Type { a(,1,2,); };
};

Class Wrongif {
  f():T { 
     if 1 then 1 else fi;
  };
};

Class Wrongwhile {
  f():T {
    while 1 loop pool;
  };
};

Class Wrongwhile2 {
  f():T {
    while 1 pool pool;
  };
};

Class Wrongcase {
  f():T {
    case 1 esac;
  };
};

Class Wrongassoc {
  f():T {
     1<2<3;
  };
};

Class Wrongassoc2 {
  f():T {
     1<=2=3;
  };
};

Clas Wrongassoc3 {
  f():T {
     1<2<3;
  };
};

Class Wrongoper {
  f():T {
     ~+3;
  };
};

Class Wrongoper2 {
  f():T {
     a <- Type;
  };
};

Class Multierrors {
  f():T {
     a():Object{};
a():Object{};
a():Object{};
a():Object{};
a():Object{};
  };
};

Class Wrongfeature {
  f():smalltype {
     1;
  };
};
