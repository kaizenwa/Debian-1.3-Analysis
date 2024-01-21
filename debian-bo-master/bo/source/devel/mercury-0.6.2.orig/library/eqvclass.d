eqvclass.optdate eqvclass.c eqvclass.err eqvclass.o : eqvclass.m \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	assoc_list.int2 \
	bool.int2 \
	float.int2 \
	tree234.int2

eqvclass.date : eqvclass.m \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	assoc_list.int3 \
	bool.int3 \
	float.int3 \
	tree234.int3

eqvclass.dir/eqvclass_000.o: eqvclass.m
	rm -rf eqvclass.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) eqvclass.m
