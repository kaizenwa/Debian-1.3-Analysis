mode_debug.optdate mode_debug.c mode_debug.err mode_debug.o : mode_debug.m \
	assoc_list.int \
	bool.int \
	globals.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	instmap.int \
	io.int \
	map.int \
	mercury_builtin.int \
	mercury_to_mercury.int \
	mode_info.int \
	modes.int \
	options.int \
	passes_aux.int \
	std_util.int \
	char.int2 \
	clause_to_proc.int2 \
	delay_info.int2 \
	float.int2 \
	getopt.int2 \
	hlds_data.int2 \
	int.int2 \
	list.int2 \
	llds.int2 \
	mode_errors.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	require.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

mode_debug.date : mode_debug.m \
	assoc_list.int3 \
	bool.int3 \
	globals.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mercury_to_mercury.int3 \
	mode_info.int3 \
	modes.int3 \
	options.int3 \
	passes_aux.int3 \
	std_util.int3 \
	char.int3 \
	clause_to_proc.int3 \
	delay_info.int3 \
	float.int3 \
	getopt.int3 \
	hlds_data.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	mode_errors.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	require.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

mode_debug.dir/mode_debug_000.o: mode_debug.m
	rm -rf mode_debug.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) mode_debug.m
