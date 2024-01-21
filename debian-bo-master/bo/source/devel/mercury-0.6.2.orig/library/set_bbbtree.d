set_bbbtree.optdate set_bbbtree.c set_bbbtree.err set_bbbtree.o : set_bbbtree.m \
	bool.int \
	int.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	float.int2 \
	set.int2

set_bbbtree.date : set_bbbtree.m \
	bool.int3 \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

set_bbbtree.dir/set_bbbtree_000.o: set_bbbtree.m
	rm -rf set_bbbtree.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) set_bbbtree.m
