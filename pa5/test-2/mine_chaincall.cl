-- chaincall

class Main inherits IO
{
   main() : Object { {

	out_int( (new C).get() );
        out_int( (new C).plus(3).get() );
	out_int( (new C).plus(3).plus(5).get() );
    
   } };
};

class C {

	value: Int;
	plus( i: Int ):C {{
		value <- value + i;
		self;
	}};
	get( ): Int {
		value
	};
};