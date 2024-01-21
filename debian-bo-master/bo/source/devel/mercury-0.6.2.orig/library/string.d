string.optdate string.c string.err string.o : string.m \
	bool.int \
	char.int \
	float.int \
	int.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	set.int2

string.date : string.m \
	bool.int3 \
	char.int3 \
	float.int3 \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	set.int3

string.dir/string_000.o: string.m
	rm -rf string.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) string.m
