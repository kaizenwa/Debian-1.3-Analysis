	struct {
	    char c;
	    int i:8;
	} s;
	
	main()
	{
	    int &ir = s.i;
	    int *ip = &s.i;
	    ir = 10;
	}
