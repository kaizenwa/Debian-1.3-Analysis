parser.optdate parser.c parser.err parser.o : parser.m \
	bool.int \
	char.int \
	float.int \
	int.int \
	io.int \
	lexer.int \
	list.int \
	map.int \
	mercury_builtin.int \
	ops.int \
	require.int \
	std_util.int \
	string.int \
	term.int \
	term_io.int \
	varset.int \
	assoc_list.int2 \
	set.int2 \
	tree234.int2

parser.date : parser.m \
	bool.int3 \
	char.int3 \
	float.int3 \
	int.int3 \
	io.int3 \
	lexer.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	ops.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	term_io.int3 \
	varset.int3 \
	assoc_list.int3 \
	set.int3 \
	tree234.int3

parser.dir/parser_000.o: parser.m
	rm -rf parser.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) parser.m
