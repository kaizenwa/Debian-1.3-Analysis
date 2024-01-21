time.optdate time.c time.err time.o : time.m \
	mercury_builtin.int

time.date : time.m \
	mercury_builtin.int3

time.dir/time_000.o: time.m
	rm -rf time.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) time.m
