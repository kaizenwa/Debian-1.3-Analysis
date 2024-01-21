options.optdate options.c options.err options.o : options.m \
	assoc_list.int \
	bool.int \
	getopt.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	char.int2 \
	float.int2 \
	ops.int2 \
	set.int2 \
	string.int2 \
	tree234.int2

options.date : options.m \
	assoc_list.int3 \
	bool.int3 \
	getopt.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	char.int3 \
	float.int3 \
	ops.int3 \
	set.int3 \
	string.int3 \
	tree234.int3

options.dir/options_000.o: options.m
	rm -rf options.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) options.m
