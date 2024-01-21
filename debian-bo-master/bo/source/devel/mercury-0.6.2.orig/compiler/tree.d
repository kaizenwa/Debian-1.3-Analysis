tree.optdate tree.c tree.err tree.o : tree.m \
	list.int \
	mercury_builtin.int \
	set.int2 \
	std_util.int2

tree.date : tree.m \
	list.int3 \
	mercury_builtin.int3 \
	set.int3 \
	std_util.int3

tree.dir/tree_000.o: tree.m
	rm -rf tree.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) tree.m
