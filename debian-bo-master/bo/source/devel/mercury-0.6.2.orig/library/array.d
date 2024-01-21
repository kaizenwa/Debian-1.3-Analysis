array.optdate array.c array.err array.o : array.m \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	assoc_list.int2 \
	float.int2 \
	set.int2 \
	std_util.int2 \
	tree234.int2

array.date : array.m \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	assoc_list.int3 \
	float.int3 \
	set.int3 \
	std_util.int3 \
	tree234.int3

array.dir/array_000.o: array.m
	rm -rf array.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) array.m
