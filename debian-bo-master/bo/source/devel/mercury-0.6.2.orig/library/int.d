int.optdate int.c int.err int.o : int.m \
	float.int \
	mercury_builtin.int \
	require.int

int.date : int.m \
	float.int3 \
	mercury_builtin.int3 \
	require.int3

int.dir/int_000.o: int.m
	rm -rf int.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) int.m
