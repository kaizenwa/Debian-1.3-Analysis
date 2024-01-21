lexer.optdate lexer.c lexer.err lexer.o : lexer.m \
	char.int \
	float.int \
	int.int \
	io.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	string.int \
	ops.int2 \
	set.int2

lexer.date : lexer.m \
	char.int3 \
	float.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	ops.int3 \
	set.int3

lexer.dir/lexer_000.o: lexer.m
	rm -rf lexer.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) lexer.m
