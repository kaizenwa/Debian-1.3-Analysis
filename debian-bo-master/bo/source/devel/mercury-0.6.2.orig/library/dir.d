dir.optdate dir.c dir.err dir.o : dir.m \
	int.int \
	mercury_builtin.int \
	require.int \
	string.int \
	char.int2 \
	float.int2 \
	list.int2 \
	set.int2 \
	std_util.int2

dir.date : dir.m \
	int.int3 \
	mercury_builtin.int3 \
	require.int3 \
	string.int3 \
	char.int3 \
	float.int3 \
	list.int3 \
	set.int3 \
	std_util.int3

dir.dir/dir_000.o: dir.m
	rm -rf dir.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) dir.m
