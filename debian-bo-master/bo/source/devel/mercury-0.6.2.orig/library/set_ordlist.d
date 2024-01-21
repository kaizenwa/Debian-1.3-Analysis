set_ordlist.optdate set_ordlist.c set_ordlist.err set_ordlist.o : set_ordlist.m \
	bool.int \
	list.int \
	mercury_builtin.int \
	std_util.int \
	set.int2

set_ordlist.date : set_ordlist.m \
	bool.int3 \
	list.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	set.int3

set_ordlist.dir/set_ordlist_000.o: set_ordlist.m
	rm -rf set_ordlist.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) set_ordlist.m
