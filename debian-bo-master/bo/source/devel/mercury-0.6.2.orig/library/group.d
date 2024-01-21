group.optdate group.c group.err group.o : group.m \
	assoc_list.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	bool.int2 \
	float.int2 \
	tree234.int2

group.date : group.m \
	assoc_list.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	bool.int3 \
	float.int3 \
	tree234.int3

group.dir/group_000.o: group.m
	rm -rf group.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) group.m
