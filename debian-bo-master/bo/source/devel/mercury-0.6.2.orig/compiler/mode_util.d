mode_util.optdate mode_util.c mode_util.err mode_util.o : mode_util.m \
	assoc_list.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	inst_match.int \
	instmap.int \
	list.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	prog_util.int \
	require.int \
	set.int \
	std_util.int \
	term.int \
	type_util.int \
	bool.int2 \
	char.int2 \
	delay_info.int2 \
	float.int2 \
	globals.int2 \
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
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

mode_util.date : mode_util.m \
	assoc_list.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	inst_match.int3 \
	instmap.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	prog_util.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	term.int3 \
	type_util.int3 \
	bool.int3 \
	char.int3 \
	delay_info.int3 \
	float.int3 \
	globals.int3 \
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
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

mode_util.dir/mode_util_000.o: mode_util.m
	rm -rf mode_util.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) mode_util.m
