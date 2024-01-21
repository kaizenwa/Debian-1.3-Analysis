unify_gen.optdate unify_gen.c unify_gen.err unify_gen.o : unify_gen.m \
	bool.int \
	code_aux.int \
	code_info.int \
	code_util.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_out.int \
	hlds_pred.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	prog_data.int \
	require.int \
	std_util.int \
	string.int \
	term.int \
	tree.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	instmap.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

unify_gen.date : unify_gen.m \
	bool.int3 \
	code_aux.int3 \
	code_info.int3 \
	code_util.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_out.int3 \
	hlds_pred.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	prog_data.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	instmap.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

unify_gen.dir/unify_gen_000.o: unify_gen.m
	rm -rf unify_gen.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) unify_gen.m
