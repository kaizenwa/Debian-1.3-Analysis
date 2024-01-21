set_unordlist.optdate set_unordlist.c set_unordlist.err set_unordlist.o : set_unordlist.m \
	bool.int \
	list.int \
	mercury_builtin.int \
	std_util.int \
	set.int2

set_unordlist.date : set_unordlist.m \
	bool.int3 \
	list.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	set.int3

set_unordlist.dir/set_unordlist_000.o: set_unordlist.m
	rm -rf set_unordlist.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) set_unordlist.m
