propagate.optdate propagate.c propagate.err propagate.o : propagate.m \
	float.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	multi_map.int \
	prof_info.int \
	relation.int \
	require.int \
	set_bbbtree.int \
	std_util.int \
	string.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	ops.int2 \
	set.int2 \
	tree234.int2

propagate.date : propagate.m \
	float.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	multi_map.int3 \
	prof_info.int3 \
	relation.int3 \
	require.int3 \
	set_bbbtree.int3 \
	std_util.int3 \
	string.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	ops.int3 \
	set.int3 \
	tree234.int3

propagate.dir/propagate_000.o: propagate.m
	rm -rf propagate.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) propagate.m
