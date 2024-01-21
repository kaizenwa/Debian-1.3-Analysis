uniq_array.optdate uniq_array.c uniq_array.err uniq_array.o : uniq_array.m \
	int.int \
	list.int \
	mercury_builtin.int \
	std_util.int \
	float.int2 \
	set.int2

uniq_array.date : uniq_array.m \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

uniq_array.dir/uniq_array_000.o: uniq_array.m
	rm -rf uniq_array.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) uniq_array.m
