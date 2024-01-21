graph.optdate graph.c graph.err graph.o : graph.m \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	assoc_list.int2 \
	bool.int2 \
	float.int2 \
	tree234.int2

graph.date : graph.m \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	assoc_list.int3 \
	bool.int3 \
	float.int3 \
	tree234.int3

graph.dir/graph_000.o: graph.m
	rm -rf graph.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) graph.m
