store_alloc.optdate store_alloc.c store_alloc.err store_alloc.o : store_alloc.m \
	assoc_list.int \
	bool.int \
	follow_vars.int \
	globals.int \
	goal_util.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	instmap.int \
	int.int \
	list.int \
	liveness.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	options.int \
	require.int \
	set.int \
	std_util.int \
	char.int2 \
	delay_info.int2 \
	float.int2 \
	getopt.int2 \
	hlds_data.int2 \
	io.int2 \
	mode_errors.int2 \
	mode_info.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

store_alloc.date : store_alloc.m \
	assoc_list.int3 \
	bool.int3 \
	follow_vars.int3 \
	globals.int3 \
	goal_util.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	list.int3 \
	liveness.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	options.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	char.int3 \
	delay_info.int3 \
	float.int3 \
	getopt.int3 \
	hlds_data.int3 \
	io.int3 \
	mode_errors.int3 \
	mode_info.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

store_alloc.dir/store_alloc_000.o: store_alloc.m
	rm -rf store_alloc.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) store_alloc.m
