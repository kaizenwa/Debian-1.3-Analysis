pqueue.optdate pqueue.c pqueue.err pqueue.o : pqueue.m \
	assoc_list.int \
	int.int \
	list.int \
	mercury_builtin.int \
	std_util.int \
	float.int2 \
	set.int2

pqueue.date : pqueue.m \
	assoc_list.int3 \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

pqueue.dir/pqueue_000.o: pqueue.m
	rm -rf pqueue.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) pqueue.m
