code_exprn.optdate code_exprn.c code_exprn.err code_exprn.o : code_exprn.m \
	assoc_list.int \
	bag.int \
	bool.int \
	exprn_aux.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	options.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	term.int \
	tree.int \
	varset.int \
	char.int2 \
	float.int2 \
	getopt.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree234.int2 \
	unify_proc.int2

code_exprn.date : code_exprn.m \
	assoc_list.int3 \
	bag.int3 \
	bool.int3 \
	exprn_aux.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	varset.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree234.int3 \
	unify_proc.int3

code_exprn.dir/code_exprn_000.o: code_exprn.m
	rm -rf code_exprn.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) code_exprn.m
