class Main inherits IO {
    main() : SELF_TYPE {
	{
	    out_string((new Object).type_name().substr(4,1)).
	    out_string((isvoid self).type_name().substr(1,3));
	    out_string("\n");
	}
    };
};

class B inherits SELF_TYPE {
    main() : Int {
        1
    };
};

class C inherits Int {
    main() : Int {
        1
    };
};

class D inherits Bool {
    main() : Int {
        1
    };
};

class E inherits Object {
    main() : Int {
        1
    };
};

class F inherits IO {
    main() : Int {
        1
    };
};

class G inherits A {
    main() : Int {
        1
    };
};