bool.optdate bool.c bool.err bool.o : bool.m \
	list.int \
	mercury_builtin.int \
	set.int2 \
	std_util.int2

bool.date : bool.m \
	list.int3 \
	mercury_builtin.int3 \
	set.int3 \
	std_util.int3

bool.dir/bool_000.o: bool.m
	rm -rf bool.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) bool.m
