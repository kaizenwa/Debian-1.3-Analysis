mode_info.optdate mode_info.c mode_info.err mode_info.o : mode_info.m \
	bool.int \
	delay_info.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	instmap.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mode_errors.int \
	prog_data.int \
	require.int \
	set.int \
	std_util.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	int.int2 \
	io.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

mode_info.date : mode_info.m \
	bool.int3 \
	delay_info.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_errors.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	int.int3 \
	io.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

mode_info.dir/mode_info_000.o: mode_info.m
	rm -rf mode_info.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) mode_info.m
