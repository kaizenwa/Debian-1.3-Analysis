bintree.optdate bintree.c bintree.err bintree.o : bintree.m \
	assoc_list.int \
	int.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	float.int2 \
	set.int2

bintree.date : bintree.m \
	assoc_list.int3 \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

bintree.dir/bintree_000.o: bintree.m
	rm -rf bintree.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) bintree.m
