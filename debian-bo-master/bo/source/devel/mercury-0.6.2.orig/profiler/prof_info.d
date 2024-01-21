prof_info.optdate prof_info.c prof_info.err prof_info.o : prof_info.m \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	assoc_list.int2 \
	set.int2 \
	tree234.int2

prof_info.date : prof_info.m \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	assoc_list.int3 \
	set.int3 \
	tree234.int3

prof_info.dir/prof_info_000.o: prof_info.m
	rm -rf prof_info.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) prof_info.m
