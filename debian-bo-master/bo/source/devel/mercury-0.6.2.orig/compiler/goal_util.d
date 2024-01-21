goal_util.optdate goal_util.c goal_util.err goal_util.o : goal_util.m \
	assoc_list.int \
	bool.int \
	code_aux.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_pred.int \
	instmap.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	prog_data.int \
	require.int \
	set.int \
	std_util.int \
	term.int \
	varset.int \
	char.int2 \
	code_info.int2 \
	delay_info.int2 \
	float.int2 \
	globals.int2 \
	hlds_module.int2 \
	io.int2 \
	llds.int2 \
	mode_errors.int2 \
	mode_info.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

goal_util.date : goal_util.m \
	assoc_list.int3 \
	bool.int3 \
	code_aux.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	term.int3 \
	varset.int3 \
	char.int3 \
	code_info.int3 \
	delay_info.int3 \
	float.int3 \
	globals.int3 \
	hlds_module.int3 \
	io.int3 \
	llds.int3 \
	mode_errors.int3 \
	mode_info.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

goal_util.dir/goal_util_000.o: goal_util.m
	rm -rf goal_util.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) goal_util.m
