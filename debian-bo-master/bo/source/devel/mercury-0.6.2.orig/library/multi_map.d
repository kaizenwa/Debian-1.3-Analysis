multi_map.optdate multi_map.c multi_map.err multi_map.o : multi_map.m \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	std_util.int \
	assoc_list.int2 \
	float.int2 \
	require.int2 \
	set.int2 \
	tree234.int2

multi_map.date : multi_map.m \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	assoc_list.int3 \
	float.int3 \
	require.int3 \
	set.int3 \
	tree234.int3

multi_map.dir/multi_map_000.o: multi_map.m
	rm -rf multi_map.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) multi_map.m
