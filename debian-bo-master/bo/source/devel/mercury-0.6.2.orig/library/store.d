store.optdate store.c store.err store.o : store.m \
	int.int \
	map.int \
	mercury_builtin.int \
	assoc_list.int2 \
	float.int2 \
	list.int2 \
	require.int2 \
	set.int2 \
	std_util.int2 \
	tree234.int2

store.date : store.m \
	int.int3 \
	map.int3 \
	mercury_builtin.int3 \
	assoc_list.int3 \
	float.int3 \
	list.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	tree234.int3

store.dir/store_000.o: store.m
	rm -rf store.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) store.m
