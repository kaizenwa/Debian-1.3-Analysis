output.optdate output.c output.err output.o : output.m \
	bool.int \
	float.int \
	generate_output.int \
	globals.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	options.int \
	output_prof_info.int \
	require.int \
	std_util.int \
	string.int \
	assoc_list.int2 \
	char.int2 \
	getopt.int2 \
	ops.int2 \
	prof_info.int2 \
	set.int2 \
	tree234.int2

output.date : output.m \
	bool.int3 \
	float.int3 \
	generate_output.int3 \
	globals.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	output_prof_info.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	assoc_list.int3 \
	char.int3 \
	getopt.int3 \
	ops.int3 \
	prof_info.int3 \
	set.int3 \
	tree234.int3

output.dir/output_000.o: output.m
	rm -rf output.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) output.m
