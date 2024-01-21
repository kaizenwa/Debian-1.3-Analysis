float.optdate float.c float.err float.o : float.m \
	int.int \
	mercury_builtin.int \
	require.int

float.date : float.m \
	int.int3 \
	mercury_builtin.int3 \
	require.int3

float.dir/float_000.o: float.m
	rm -rf float.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) float.m
