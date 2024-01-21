modecheck_call.optdate modecheck_call.c modecheck_call.err modecheck_call.o : modecheck_call.m \
	bool.int \
	clause_to_proc.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	inst_match.int \
	instmap.int \
	list.int \
	make_hlds.int \
	map.int \
	mercury_builtin.int \
	mode_debug.int \
	mode_errors.int \
	mode_info.int \
	mode_util.int \
	modes.int \
	prog_data.int \
	set.int \
	std_util.int \
	assoc_list.int2 \
	char.int2 \
	delay_info.int2 \
	equiv_type.int2 \
	float.int2 \
	globals.int2 \
	int.int2 \
	io.int2 \
	llds.int2 \
	ops.int2 \
	relation.int2 \
	require.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

modecheck_call.date : modecheck_call.m \
	bool.int3 \
	clause_to_proc.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	inst_match.int3 \
	instmap.int3 \
	list.int3 \
	make_hlds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_debug.int3 \
	mode_errors.int3 \
	mode_info.int3 \
	mode_util.int3 \
	modes.int3 \
	prog_data.int3 \
	set.int3 \
	std_util.int3 \
	assoc_list.int3 \
	char.int3 \
	delay_info.int3 \
	equiv_type.int3 \
	float.int3 \
	globals.int3 \
	int.int3 \
	io.int3 \
	llds.int3 \
	ops.int3 \
	relation.int3 \
	require.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

modecheck_call.dir/modecheck_call_000.o: modecheck_call.m
	rm -rf modecheck_call.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) modecheck_call.m
