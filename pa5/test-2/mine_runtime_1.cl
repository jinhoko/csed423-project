-- runtime check

class Main inherits IO
{
   x_glob:Int <- 1;
   main() : Int {{
        -- abort
        -- abort();
        -- new
        new String;
        new IO;
        new Object;
        new Int;
        new Bool;
        -- substr
        let x:String <- "test".substr( x_glob,1) in out_string(x); -- e
        let x:String <- "123456".substr(0,2) in out_string(x);  -- 12
        let x:String <- "123456".substr(2,4) in out_string(x);  -- 3456
        let x:String <- "123456".substr(3,2) in out_string(x);  -- 45

        -- substr - err
        -- let x:String <- "".substr(0,0) in out_string(x);
        -- let x:String <- "".substr(0,1) in out_string(x);
        -- let x:String <- "1".substr(1,0) in out_string(x);
        -- let x:String <- "123456".substr(2,5) in out_string(x);
        -- let x:String <- "123456".substr(3,~1) in out_string(x);
        -- let x:String <- "123456".substr(3,~10) in out_string(x);
        -- let x:String <- "123456".substr(~1,1) in out_string(x);

        -- string-String_length / out-int
        out_int( "test".length() ); -- 4
        let x:Int <- "test".length() in out_int(x); -- 4
        let x:Int <- "".length() in out_int(x); -- 0

        -- string-String_concat
        let x:String <- "haha".concat("hoho") in out_string(x); -- hahahoho
        let x:String in {
            let y:String <- "no".concat(x) in out_string(y); -- "no"
        }; -- 0

        -- method call
        out_int(1);  -- 1
        out_int( sum(1+2, 3+4) ); -- 10
        let x:Int <- 1 in
        {
            let y:Int <- 1 in out_int( sum(x, y) );
        }; -- 2
        1;
    }};
    sum(x:Int, y:Int ): Int {
        x+y
    };
};
