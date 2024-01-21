char.optdate char.c char.err char.o : char.m \
	list.int \
	mercury_builtin.int \
	set.int2 \
	std_util.int2

char.date : char.m \
	list.int3 \
	mercury_builtin.int3 \
	set.int3 \
	std_util.int3

char.dir/char_000.o: char.m
	rm -rf char.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) char.m
