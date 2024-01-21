set.optdate set.c set.err set.o : set.m \
	bool.int \
	list.int \
	mercury_builtin.int \
	require.int \
	set_ordlist.int \
	set_unordlist.int \
	std_util.int2

set.date : set.m \
	bool.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set_ordlist.int3 \
	set_unordlist.int3 \
	std_util.int3

set.dir/set_000.o: set.m
	rm -rf set.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) set.m
