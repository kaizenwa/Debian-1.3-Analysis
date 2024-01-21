term_io.optdate term_io.c term_io.err term_io.o : term_io.m \
	char.int \
	float.int \
	int.int \
	io.int \
	list.int \
	mercury_builtin.int \
	ops.int \
	parser.int \
	require.int \
	std_util.int \
	string.int \
	term.int \
	varset.int \
	assoc_list.int2 \
	map.int2 \
	set.int2 \
	tree234.int2

term_io.date : term_io.m \
	char.int3 \
	float.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	ops.int3 \
	parser.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	varset.int3 \
	assoc_list.int3 \
	map.int3 \
	set.int3 \
	tree234.int3

term_io.dir/term_io_000.o: term_io.m
	rm -rf term_io.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) term_io.m
