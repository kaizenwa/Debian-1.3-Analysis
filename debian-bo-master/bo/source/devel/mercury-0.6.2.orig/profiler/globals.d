globals.optdate globals.c globals.err globals.o : globals.m \
	getopt.int \
	io.int \
	map.int \
	mercury_builtin.int \
	options.int \
	require.int \
	std_util.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	int.int2 \
	list.int2 \
	ops.int2 \
	set.int2 \
	string.int2 \
	tree234.int2

globals.date : globals.m \
	getopt.int3 \
	io.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	require.int3 \
	std_util.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	int.int3 \
	list.int3 \
	ops.int3 \
	set.int3 \
	string.int3 \
	tree234.int3

globals.dir/globals_000.o: globals.m
	rm -rf globals.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) globals.m
