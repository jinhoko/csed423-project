Class Main inherits IO {
	s : String <- "_";
	main() : Object {
	 while not (s = "") loop
	  {
	    out_string("Type in a line\n");
	    s <- in_string();
	    out_string("Entered: ").out_string(s).out_string("\n");
	    out_string("Length: ").out_int(s.length()).out_string("\n");
	  }
	 pool
	};
};



