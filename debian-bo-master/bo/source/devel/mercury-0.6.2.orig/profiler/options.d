options.optdate options.c options.err options.o : options.m \
	bool.int \
	getopt.int \
	int.int \
	io.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	string.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	list.int2 \
	map.int2 \
	ops.int2 \
	set.int2 \
	tree234.int2

options.date : options.m \
	bool.int3 \
	getopt.int3 \
	int.int3 \
	io.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	list.int3 \
	map.int3 \
	ops.int3 \
	set.int3 \
	tree234.int3

options.dir/options_000.o: options.m
	rm -rf options.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) options.m
