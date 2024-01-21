saved_vars.optdate saved_vars.c saved_vars.err saved_vars.o : saved_vars.m \
	bool.int \
	goal_util.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	list.int \
	map.int \
	mercury_builtin.int \
	quantification.int \
	set.int \
	std_util.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	require.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

saved_vars.date : saved_vars.m \
	bool.int3 \
	goal_util.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	quantification.int3 \
	set.int3 \
	std_util.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	require.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

saved_vars.dir/saved_vars_000.o: saved_vars.m
	rm -rf saved_vars.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) saved_vars.m
