relation.optdate relation.c relation.err relation.o : relation.m \
	assoc_list.int \
	bimap.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	queue.int \
	require.int \
	set.int \
	set_bbbtree.int \
	stack.int \
	std_util.int \
	bool.int2 \
	float.int2 \
	tree234.int2

relation.date : relation.m \
	assoc_list.int3 \
	bimap.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	queue.int3 \
	require.int3 \
	set.int3 \
	set_bbbtree.int3 \
	stack.int3 \
	std_util.int3 \
	bool.int3 \
	float.int3 \
	tree234.int3

relation.dir/relation_000.o: relation.m
	rm -rf relation.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) relation.m
