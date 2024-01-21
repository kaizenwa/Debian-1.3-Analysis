rbtree.optdate rbtree.c rbtree.err rbtree.o : rbtree.m \
	assoc_list.int \
	int.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	float.int2 \
	set.int2

rbtree.date : rbtree.m \
	assoc_list.int3 \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

rbtree.dir/rbtree_000.o: rbtree.m
	rm -rf rbtree.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) rbtree.m
