call_graph.optdate call_graph.c call_graph.err call_graph.o : call_graph.m \
	bool.int \
	globals.int \
	io.int \
	list.int \
	mercury_builtin.int \
	options.int \
	read.int \
	relation.int \
	require.int \
	std_util.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	int.int2 \
	map.int2 \
	ops.int2 \
	set.int2 \
	set_bbbtree.int2 \
	string.int2 \
	tree234.int2

call_graph.date : call_graph.m \
	bool.int3 \
	globals.int3 \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	options.int3 \
	read.int3 \
	relation.int3 \
	require.int3 \
	std_util.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	int.int3 \
	map.int3 \
	ops.int3 \
	set.int3 \
	set_bbbtree.int3 \
	string.int3 \
	tree234.int3

call_graph.dir/call_graph_000.o: call_graph.m
	rm -rf call_graph.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) call_graph.m
