list.optdate list.c list.err list.o : list.m \
	bintree_set.int \
	int.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	float.int2 \
	set.int2

list.date : list.m \
	bintree_set.int3 \
	int.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

list.dir/list_000.o: list.m
	rm -rf list.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) list.m
