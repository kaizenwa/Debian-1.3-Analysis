output_prof_info.optdate output_prof_info.c output_prof_info.err output_prof_info.o : output_prof_info.m \
	float.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	string.int \
	assoc_list.int2 \
	char.int2 \
	require.int2 \
	set.int2 \
	std_util.int2 \
	tree234.int2

output_prof_info.date : output_prof_info.m \
	float.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	string.int3 \
	assoc_list.int3 \
	char.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	tree234.int3

output_prof_info.dir/output_prof_info_000.o: output_prof_info.m
	rm -rf output_prof_info.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) output_prof_info.m
