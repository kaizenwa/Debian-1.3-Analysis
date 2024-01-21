read.optdate read.c read.err read.o : read.m \
	int.int \
	io.int \
	list.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	string.int \
	char.int2 \
	float.int2 \
	ops.int2 \
	set.int2

read.date : read.m \
	int.int3 \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	char.int3 \
	float.int3 \
	ops.int3 \
	set.int3

read.dir/read_000.o: read.m
	rm -rf read.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) read.m
