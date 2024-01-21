code_util.optdate code_util.c code_util.err code_util.o : code_util.m \
	bool.int \
	char.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	require.int \
	special_pred.int \
	std_util.int \
	string.int \
	type_util.int \
	varset.int \
	assoc_list.int2 \
	float.int2 \
	globals.int2 \
	instmap.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

code_util.date : code_util.m \
	bool.int3 \
	char.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	require.int3 \
	special_pred.int3 \
	std_util.int3 \
	string.int3 \
	type_util.int3 \
	varset.int3 \
	assoc_list.int3 \
	float.int3 \
	globals.int3 \
	instmap.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

code_util.dir/code_util_000.o: code_util.m
	rm -rf code_util.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) code_util.m
