stack.optdate stack.c stack.err stack.o : stack.m \
	int.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	float.int2 \
	set.int2

stack.date : stack.m \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

stack.dir/stack_000.o: stack.m
	rm -rf stack.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) stack.m
