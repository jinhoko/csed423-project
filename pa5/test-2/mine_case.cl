-- typecase

class Main inherits IO
{
   main() : Object { {
       -- 1
       case new Int of
            a:Int => out_int(1);
            b:Object => out_int(2);
        esac;

        -- 1
        case 1 of
            b:Object => out_int(2);  -- closest one should be chosen
            a:Int => out_int(a);     -- so choose this branch
        esac;

        -- B
        case new B of 
            c:C => c.print();
            b:B => b.print();
            a:A => a.print();
        esac;

        -- C (!!)
        case new C of
            a:A => a.print();
            b:B => b.print();
        esac;

        -- this one aborts
--        case new Int of
--            a:Bool => out_int(1);     -- no matching branch so aborted
--        esac;

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
