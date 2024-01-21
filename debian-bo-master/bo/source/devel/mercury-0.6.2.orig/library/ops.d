ops.optdate ops.c ops.err ops.o : ops.m \
	mercury_builtin.int

ops.date : ops.m \
	mercury_builtin.int3

ops.dir/ops_000.o: ops.m
	rm -rf ops.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) ops.m
