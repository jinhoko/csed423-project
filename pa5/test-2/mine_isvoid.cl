-- isvoid

class Main inherits IO
{
   main() : Object { {

       if isvoid 1 then out_int(1) else out_int(0) fi;
       if isvoid false then out_int(1) else out_int(0) fi;
       if isvoid new Int then out_int(1) else out_int(0) fi;
       if isvoid new String then out_int(1) else out_int(0) fi;
       if isvoid new Main then out_int(1) else out_int(0) fi;
       if isvoid self then out_int(1) else out_int(0) fi;

   } };
};

-- 000000