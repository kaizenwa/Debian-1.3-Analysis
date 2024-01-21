code_aux.optdate code_aux.c code_aux.err code_aux.o : code_aux.m \
	assoc_list.int \
	bool.int \
	code_info.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	list.int \
	llds.int \
	llds_out.int \
	map.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	term.int \
	type_util.int \
	varset.int \
	char.int2 \
	code_util.int2 \
	float.int2 \
	globals.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

code_aux.date : code_aux.m \
	assoc_list.int3 \
	bool.int3 \
	code_info.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	list.int3 \
	llds.int3 \
	llds_out.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	type_util.int3 \
	varset.int3 \
	char.int3 \
	code_util.int3 \
	float.int3 \
	globals.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

code_aux.dir/code_aux_000.o: code_aux.m
	rm -rf code_aux.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) code_aux.m
