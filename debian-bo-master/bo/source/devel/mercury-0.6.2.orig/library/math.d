math.optdate math.c math.err math.o : math.m \
	float.int \
	mercury_builtin.int

math.date : math.m \
	float.int3 \
	mercury_builtin.int3

math.dir/math_000.o: math.m
	rm -rf math.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) math.m
