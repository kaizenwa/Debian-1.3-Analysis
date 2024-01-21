generate_output.optdate generate_output.c generate_output.err generate_output.o : generate_output.m \
	bool.int \
	float.int \
	globals.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	options.int \
	output_prof_info.int \
	prof_info.int \
	rbtree.int \
	relation.int \
	string.int \
	assoc_list.int2 \
	char.int2 \
	getopt.int2 \
	ops.int2 \
	require.int2 \
	set.int2 \
	set_bbbtree.int2 \
	std_util.int2 \
	tree234.int2

generate_output.date : generate_output.m \
	bool.int3 \
	float.int3 \
	globals.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	output_prof_info.int3 \
	prof_info.int3 \
	rbtree.int3 \
	relation.int3 \
	string.int3 \
	assoc_list.int3 \
	char.int3 \
	getopt.int3 \
	ops.int3 \
	require.int3 \
	set.int3 \
	set_bbbtree.int3 \
	std_util.int3 \
	tree234.int3

generate_output.dir/generate_output_000.o: generate_output.m
	rm -rf generate_output.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) generate_output.m
