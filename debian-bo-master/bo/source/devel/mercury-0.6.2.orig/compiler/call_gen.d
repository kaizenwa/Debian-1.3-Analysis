call_gen.optdate call_gen.c call_gen.err call_gen.o : call_gen.m \
	arg_info.int \
	assoc_list.int \
	bool.int \
	code_info.int \
	code_util.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	prog_data.int \
	require.int \
	set.int \
	shapes.int \
	std_util.int \
	tree.int \
	type_util.int \
	unify_proc.int \
	char.int2 \
	float.int2 \
	getopt.int2 \
	instmap.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	options.int2 \
	relation.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree234.int2 \
	varset.int2

call_gen.date : call_gen.m \
	arg_info.int3 \
	assoc_list.int3 \
	bool.int3 \
	code_info.int3 \
	code_util.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	shapes.int3 \
	std_util.int3 \
	tree.int3 \
	type_util.int3 \
	unify_proc.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	instmap.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	options.int3 \
	relation.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree234.int3 \
	varset.int3

call_gen.dir/call_gen_000.o: call_gen.m
	rm -rf call_gen.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) call_gen.m
