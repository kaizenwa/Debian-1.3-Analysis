map.optdate map.c map.err map.o : map.m \
	assoc_list.int \
	list.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	tree234.int \
	bool.int2

map.date : map.m \
	assoc_list.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	tree234.int3 \
	bool.int3

map.dir/map_000.o: map.m
	rm -rf map.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) map.m
