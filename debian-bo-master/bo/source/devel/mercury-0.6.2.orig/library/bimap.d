bimap.optdate bimap.c bimap.err bimap.o : bimap.m \
	assoc_list.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	std_util.int \
	float.int2 \
	require.int2 \
	set.int2 \
	tree234.int2

bimap.date : bimap.m \
	assoc_list.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	float.int3 \
	require.int3 \
	set.int3 \
	tree234.int3

bimap.dir/bimap_000.o: bimap.m
	rm -rf bimap.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) bimap.m
