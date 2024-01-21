graph_colour.optdate graph_colour.c graph_colour.err graph_colour.o : graph_colour.m \
	list.int \
	mercury_builtin.int \
	require.int \
	set.int \
	bool.int2 \
	std_util.int2

graph_colour.date : graph_colour.m \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	bool.int3 \
	std_util.int3

graph_colour.dir/graph_colour_000.o: graph_colour.m
	rm -rf graph_colour.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) graph_colour.m
