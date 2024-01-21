mercury_builtin.optdate mercury_builtin.c mercury_builtin.err mercury_builtin.o : mercury_builtin.m \
	char.int \
	float.int \
	int.int \
	list.int \
	require.int \
	std_util.int \
	string.int \
	set.int2

mercury_builtin.date : mercury_builtin.m \
	char.int3 \
	float.int3 \
	int.int3 \
	list.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	set.int3

mercury_builtin.dir/mercury_builtin_000.o: mercury_builtin.m
	rm -rf mercury_builtin.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) mercury_builtin.m
