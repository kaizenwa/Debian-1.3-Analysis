getopt.optdate getopt.c getopt.err getopt.o : getopt.m \
	bool.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	string.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	set.int2 \
	tree234.int2

getopt.date : getopt.m \
	bool.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	set.int3 \
	tree234.int3

getopt.dir/getopt_000.o: getopt.m
	rm -rf getopt.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) getopt.m
