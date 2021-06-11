class Main inherits IO {
	main():SELF_TYPE {
	{
	    self.out_string("Enter an integer: ");
	    let i : Int <- self.in_int() in
	     {
		self.out_string("Read: ");
		self.out_int(i);
		self.out_string("\n");
	     };
	}
        };
};

