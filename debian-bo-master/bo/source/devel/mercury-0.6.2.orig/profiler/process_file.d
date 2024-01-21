process_file.optdate process_file.c process_file.err process_file.o : process_file.m \
	bool.int \
	globals.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	options.int \
	prof_info.int \
	read.int \
	relation.int \
	require.int \
	std_util.int \
	string.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	ops.int2 \
	set.int2 \
	set_bbbtree.int2 \
	tree234.int2

process_file.date : process_file.m \
	bool.int3 \
	globals.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	prof_info.int3 \
	read.int3 \
	relation.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	ops.int3 \
	set.int3 \
	set_bbbtree.int3 \
	tree234.int3

process_file.dir/process_file_000.o: process_file.m
	rm -rf process_file.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) process_file.m
