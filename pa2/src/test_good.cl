class A {
};

Class BB__ inherits A {
};


class Letprec {
  a():Int {
      let a:Type in ctxt
      +
      let b:Typee in ctxtt
  };  

};


class LetprecTwo {
  a():Int {
      let a:Type in ctxt
      +
      not b
  };

};

class LetAssoc {
  a() : Int {
    let a:Type in let b:Type <- "hehe" in let c:Type in 1
  };
};

class Operator {

  a(): Int {
     ~ ( 3 - 7 ) / 2 * 5 - true + a + false

  };

};

class Block {
  a(): Int {
    {{{{ (1+1 - ~5);};};};{1;};}


  };
};

class OperAssoc {
  a() : Int {
    {
      id <- id1 <- id2 <- id3;
      NOT 1 < NOT 3;
      3 = 3;
      3 + (1 - 3) / ISVOID a * 3 / 3 / 1 - 3 * 2 / 3 - 1 - 3 - 4;
    }
  };

};

class Allexpr {
  a(): Int {
    {
      a <- 1;
      a@Type.idee( "hehe", "hoho");
      idd( 1,2,3,4,5);
      if iffff then thennn else elssss fi;
      while whileeee loop loooop pool;    
      { blockimneeda; nadoyo; };
      let me:Person <- tired, my:People <- sleepy in coolcool;
      case caseeee of mdddddd:Type => "hi"; wada:Typetoo => ~arggghhhh; esac;
      new Assignmentnono;
      isvoid myhead;
      1+1;
      1-1;
      1*1;
      1*1;
      1/1;
      ~1;
      1<1;
      1<=1;
      1=1;
      not 1;
      (1);
      ideeeeeeeeeeee;
      1;
      "he";
      true;
      false;
    }
  };
};



class MultipleWhile {
 a(): Int {  while 1 loop while 1 loop 1 pool pool };
};

class Main inherits IO {
   main(): SELF_TYPE {
        out_string("Hello, World.\n")
   };
};

(*
 *  The Main class shows how to use the List class. It creates a small
 *  list and then repeatedly prints out its elements and takes off the
 *  first element of the list.
 *)

class Main inherits IO {

   mylist : List;

   -- Print all elements of the list. Calls itself recursively with
   -- the tail of the list, until the end of the list is reached.

   print_list(l : List) : Object {
      if l.isNil() then out_string("\n")
                   else {
                           out_int(l.head());
                           out_string(" ");
                           print_list(l.tail());
                        }
      fi
   };

   -- Note how the dynamic dispatch mechanism is responsible to end
   -- the while loop. As long as mylist is bound to an object of
   -- dynamic type Cons, the dispatch to isNil calls the isNil method of
   -- the Cons class, which returns false. However when we reach the
   -- end of the list, mylist gets bound to the object that was
   -- created by the (new List) expression. This object is of dynamic type
   -- List, and thus the method isNil in the List class is called and
   -- returns true.

   main() : Object {
      {
         mylist <- new List.cons(1).cons(2).cons(3).cons(4).cons(5);
         while (not mylist.isNil()) loop
            {
               print_list(mylist);
               mylist <- mylist.tail();
            }
         pool;
      }
   };

};


