assoc_list.optdate assoc_list.c assoc_list.err assoc_list.o : assoc_list.m \
	list.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	bool.int2

assoc_list.date : assoc_list.m \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	bool.int3

assoc_list.dir/assoc_list_000.o: assoc_list.m
	rm -rf assoc_list.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) assoc_list.m
