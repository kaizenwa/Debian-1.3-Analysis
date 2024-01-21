hlds_pred.optdate hlds_pred.c hlds_pred.err hlds_pred.o : hlds_pred.m \
	assoc_list.int \
	bool.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	instmap.int \
	int.int \
	list.int \
	make_hlds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	prog_data.int \
	prog_util.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	type_util.int \
	varset.int \
	char.int2 \
	delay_info.int2 \
	equiv_type.int2 \
	float.int2 \
	globals.int2 \
	io.int2 \
	llds.int2 \
	mode_errors.int2 \
	mode_info.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

hlds_pred.date : hlds_pred.m \
	assoc_list.int3 \
	bool.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	instmap.int3 \
	int.int3 \
	list.int3 \
	make_hlds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	prog_data.int3 \
	prog_util.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	type_util.int3 \
	varset.int3 \
	char.int3 \
	delay_info.int3 \
	equiv_type.int3 \
	float.int3 \
	globals.int3 \
	io.int3 \
	llds.int3 \
	mode_errors.int3 \
	mode_info.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

hlds_pred.dir/hlds_pred_000.o: hlds_pred.m
	rm -rf hlds_pred.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) hlds_pred.m
