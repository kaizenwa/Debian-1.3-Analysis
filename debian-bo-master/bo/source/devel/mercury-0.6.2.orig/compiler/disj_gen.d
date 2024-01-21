disj_gen.optdate disj_gen.c disj_gen.err disj_gen.o : disj_gen.m \
	bool.int \
	code_gen.int \
	code_info.int \
	code_util.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	options.int \
	require.int \
	set.int \
	std_util.int \
	tree.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_module.int2 \
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
	string.int2 \
	term.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

disj_gen.date : disj_gen.m \
	bool.int3 \
	code_gen.int3 \
	code_info.int3 \
	code_util.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	tree.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_module.int3 \
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
	string.int3 \
	term.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

disj_gen.dir/disj_gen_000.o: disj_gen.m
	rm -rf disj_gen.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) disj_gen.m
