class Main inherits IO {
    main( a:Bool, b:String ) : SELF_TYPE {
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

class Mainnn inherits Main {
    main( a:Int, b:Int ) : SELF_TYPE {
        {
            out_string((new Object).type_name().substr(4,1)).
            out_string((isvoid self).type_name().substr(1,3));
            out_string("\n");
        }
    };

};