transform.optdate transform.c transform.err transform.o : transform.m \
	code_aux.int \
	delay_info.int \
	hlds_goal.int \
	instmap.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	mode_info.int \
	mode_util.int \
	prog_data.int \
	require.int \
	set.int \
	std_util.int \
	term.int \
	varset.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	code_info.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	int.int2 \
	io.int2 \
	mode_errors.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

transform.date : transform.m \
	code_aux.int3 \
	delay_info.int3 \
	hlds_goal.int3 \
	instmap.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_info.int3 \
	mode_util.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	term.int3 \
	varset.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	code_info.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	mode_errors.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

transform.dir/transform_000.o: transform.m
	rm -rf transform.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) transform.m
