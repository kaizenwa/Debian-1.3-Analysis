tree234.optdate tree234.c tree234.err tree234.o : tree234.m \
	assoc_list.int \
	int.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	float.int2 \
	set.int2

tree234.date : tree234.m \
	assoc_list.int3 \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

tree234.dir/tree234_000.o: tree234.m
	rm -rf tree234.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) tree234.m
