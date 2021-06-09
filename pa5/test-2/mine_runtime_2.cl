-- runtime check

class Main inherits IO
{
   main() : Int {{
        -- string-String_length / out-int
        -- out_int( "test".length() ); -- 4
        -- let x:Int <- "".length() in out_int(x); -- 0
        -- let x:Int <- "test".length() in out_int(x); -- 4
        let x:Int in out_string( "abcde".substr( 1, 2 )) ; -- bc
        let x:Int in out_string( "abcde".substr( "1".length(), "11".length() )) ; -- bc
        1;
    }};
};
