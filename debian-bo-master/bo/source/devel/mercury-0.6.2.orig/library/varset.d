varset.optdate varset.c varset.err varset.o : varset.m \
	assoc_list.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	term.int \
	bool.int2 \
	char.int2 \
	float.int2 \
	tree234.int2

varset.date : varset.m \
	assoc_list.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	tree234.int3

varset.dir/varset_000.o: varset.m
	rm -rf varset.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) varset.m
