require.optdate require.c require.err require.o : require.m \
	mercury_builtin.int

require.date : require.m \
	mercury_builtin.int3

require.dir/require_000.o: require.m
	rm -rf require.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) require.m
