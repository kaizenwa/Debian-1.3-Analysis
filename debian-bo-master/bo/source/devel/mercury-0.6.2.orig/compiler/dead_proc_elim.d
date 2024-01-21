dead_proc_elim.optdate dead_proc_elim.c dead_proc_elim.err dead_proc_elim.o : dead_proc_elim.m \
	bool.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	options.int \
	passes_aux.int \
	prog_data.int \
	queue.int \
	require.int \
	set.int \
	std_util.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	instmap.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

dead_proc_elim.date : dead_proc_elim.m \
	bool.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	passes_aux.int3 \
	prog_data.int3 \
	queue.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	instmap.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

dead_proc_elim.dir/dead_proc_elim_000.o: dead_proc_elim.m
	rm -rf dead_proc_elim.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) dead_proc_elim.m
