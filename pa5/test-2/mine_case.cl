-- typecase

class Main inherits IO
{
   main() : Object { {

       case new Int of
            a:Int => out_int(1);
            b:Object => out_int(2);
        esac;

        case 1 of
            b:Object => out_int(2);  -- closest one should be chosen
            a:Int => out_int(a);     -- so choose this branch
        esac;

--        case new Int of
--            a:Bool => out_int(1);     -- no matching branch so aborted
--        esac;

         case abort() of
            b:Object => out_int(2);  -- closest one should be chosen
            a:Int => 1+a;     -- so choose this branch
            esac;

   } };
};