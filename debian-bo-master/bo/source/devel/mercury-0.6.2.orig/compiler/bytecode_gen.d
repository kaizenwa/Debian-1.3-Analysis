bytecode_gen.optdate bytecode_gen.c bytecode_gen.err bytecode_gen.o : bytecode_gen.m \
	assoc_list.int \
	bool.int \
	bytecode.int \
	call_gen.int \
	code_util.int \
	goal_util.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	passes_aux.int \
	prog_data.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	tree.int \
	varset.int \
	char.int2 \
	code_info.int2 \
	float.int2 \
	globals.int2 \
	instmap.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	term.int2 \
	tree234.int2 \
	unify_proc.int2

bytecode_gen.date : bytecode_gen.m \
	assoc_list.int3 \
	bool.int3 \
	bytecode.int3 \
	call_gen.int3 \
	code_util.int3 \
	goal_util.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	passes_aux.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	tree.int3 \
	varset.int3 \
	char.int3 \
	code_info.int3 \
	float.int3 \
	globals.int3 \
	instmap.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	term.int3 \
	tree234.int3 \
	unify_proc.int3

bytecode_gen.dir/bytecode_gen_000.o: bytecode_gen.m
	rm -rf bytecode_gen.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) bytecode_gen.m
