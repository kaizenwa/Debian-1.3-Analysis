queue.optdate queue.c queue.err queue.o : queue.m \
	int.int \
	list.int \
	mercury_builtin.int \
	std_util.int \
	float.int2 \
	set.int2

queue.date : queue.m \
	int.int3 \
	list.int3 \
	mercury_builtin.int3 \
	std_util.int3 \
	float.int3 \
	set.int3

queue.dir/queue_000.o: queue.m
	rm -rf queue.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) queue.m
