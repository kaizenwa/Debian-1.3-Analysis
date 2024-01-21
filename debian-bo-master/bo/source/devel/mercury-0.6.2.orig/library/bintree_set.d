bintree_set.optdate bintree_set.c bintree_set.err bintree_set.o : bintree_set.m \
	assoc_list.int \
	bintree.int \
	list.int \
	mercury_builtin.int \
	std_util.int \
	set.int2

bintree_set.date : bintree_set.m \
	assoc_list.int3 \
	bintree.int3 \
	list.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	set.int3

bintree_set.dir/bintree_set_000.o: bintree_set.m
	rm -rf bintree_set.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) bintree_set.m
