class Main inherits IO {
    main( ) : SELF_TYPE {
        {
            out_string((new Object).type_name().substr(4,1)).
            out_string((isvoid self).type_name().substr(1,3));
            out_string("\n");
        }
    };
    second( a:Bool, b:InvalidType ): InvalidType {  b+1  };
    third( a:Bool, b:SELF_TYPE ): InvalidType {  b+1  };
    four( a:Bool, self:Int ): SELF_TYPE {  self  };
    five( a:Bool, a:Int ): Int {  a  };
};

