random.optdate random.c random.err random.o : random.m \
	int.int \
	list.int \
	mercury_builtin.int \
	require.int \
	float.int2 \
	set.int2 \
	std_util.int2

random.date : random.m \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	float.int3 \
	set.int3 \
	std_util.int3

random.dir/random_000.o: random.m
	rm -rf random.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) random.m
