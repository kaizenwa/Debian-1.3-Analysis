bag.optdate bag.c bag.err bag.o : bag.m \
	int.int \
	map.int \
	mercury_builtin.int \
	require.int \
	assoc_list.int2 \
	float.int2 \
	list.int2 \
	set.int2 \
	std_util.int2 \
	tree234.int2

bag.date : bag.m \
	int.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	assoc_list.int3 \
	float.int3 \
	list.int3 \
	set.int3 \
	std_util.int3 \
	tree234.int3

bag.dir/bag_000.o: bag.m
	rm -rf bag.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) bag.m
