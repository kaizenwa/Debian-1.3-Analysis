hlds_goal.optdate hlds_goal.c hlds_goal.err hlds_goal.o : hlds_goal.m \
	bool.int \
	hlds_data.int \
	instmap.int \
	list.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	require.int \
	set.int \
	std_util.int \
	assoc_list.int2 \
	char.int2 \
	delay_info.int2 \
	float.int2 \
	globals.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	int.int2 \
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
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

hlds_goal.date : hlds_goal.m \
	bool.int3 \
	hlds_data.int3 \
	instmap.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	assoc_list.int3 \
	char.int3 \
	delay_info.int3 \
	float.int3 \
	globals.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
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
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

hlds_goal.dir/hlds_goal_000.o: hlds_goal.m
	rm -rf hlds_goal.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) hlds_goal.m
