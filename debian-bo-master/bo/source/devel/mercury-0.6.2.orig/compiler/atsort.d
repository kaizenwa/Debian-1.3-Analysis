atsort.optdate atsort.c atsort.err atsort.o : atsort.m \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	assoc_list.int2 \
	set.int2 \
	std_util.int2 \
	tree234.int2

atsort.date : atsort.m \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	assoc_list.int3 \
	set.int3 \
	std_util.int3 \
	tree234.int3

atsort.dir/atsort_000.o: atsort.m
	rm -rf atsort.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) atsort.m
