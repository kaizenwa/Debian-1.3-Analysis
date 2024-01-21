prof_debug.optdate prof_debug.c prof_debug.err prof_debug.o : prof_debug.m \
	assoc_list.int \
	io.int \
	list.int \
	mercury_builtin.int \
	set.int \
	std_util.int \
	string.int \
	bool.int2 \
	char.int2 \
	float.int2 \
	int.int2 \
	ops.int2 \
	require.int2

prof_debug.date : prof_debug.m \
	assoc_list.int3 \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	int.int3 \
	ops.int3 \
	require.int3

prof_debug.dir/prof_debug_000.o: prof_debug.m
	rm -rf prof_debug.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) prof_debug.m
