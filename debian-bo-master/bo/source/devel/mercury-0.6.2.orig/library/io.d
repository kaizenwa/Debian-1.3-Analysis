io.optdate io.c io.err io.o : io.m \
	char.int \
	dir.int \
	float.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	ops.int \
	require.int \
	std_util.int \
	string.int \
	term_io.int \
	time.int \
	varset.int \
	assoc_list.int2 \
	set.int2 \
	term.int2 \
	tree234.int2

io.date : io.m \
	char.int3 \
	dir.int3 \
	float.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	ops.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	term_io.int3 \
	time.int3 \
	varset.int3 \
	assoc_list.int3 \
	set.int3 \
	term.int3 \
	tree234.int3

io.dir/io_000.o: io.m
	rm -rf io.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) io.m
