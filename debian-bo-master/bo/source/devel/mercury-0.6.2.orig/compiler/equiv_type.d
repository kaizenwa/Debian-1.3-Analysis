equiv_type.optdate equiv_type.c equiv_type.err equiv_type.o : equiv_type.m \
	bool.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	prog_out.int \
	prog_util.int \
	require.int \
	std_util.int \
	term.int \
	type_util.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	int.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

equiv_type.date : equiv_type.m \
	bool.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	prog_out.int3 \
	prog_util.int3 \
	require.int3 \
	std_util.int3 \
	term.int3 \
	type_util.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

equiv_type.dir/equiv_type_000.o: equiv_type.m
	rm -rf equiv_type.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) equiv_type.m
