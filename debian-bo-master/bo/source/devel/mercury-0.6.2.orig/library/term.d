term.optdate term.c term.err term.o : term.m \
	float.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	string.int \
	assoc_list.int2 \
	char.int2 \
	set.int2 \
	tree234.int2

term.date : term.m \
	float.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	assoc_list.int3 \
	char.int3 \
	set.int3 \
	tree234.int3

term.dir/term_000.o: term.m
	rm -rf term.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) term.m
