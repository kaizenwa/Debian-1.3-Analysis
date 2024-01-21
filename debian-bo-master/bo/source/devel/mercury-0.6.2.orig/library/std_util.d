std_util.optdate std_util.c std_util.err std_util.o : std_util.m \
	list.int \
	mercury_builtin.int \
	require.int \
	set.int \
	bool.int2

std_util.date : std_util.m \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	bool.int3

std_util.dir/std_util_000.o: std_util.m
	rm -rf std_util.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) std_util.m
