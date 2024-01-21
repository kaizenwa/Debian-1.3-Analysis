opt_util.optdate opt_util.c opt_util.err opt_util.o : opt_util.m \
	bool.int \
	exprn_aux.int \
	int.int \
	list.int \
	llds.int \
	llds_out.int \
	map.int \
	mercury_builtin.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	assoc_list.int2 \
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
	options.int2 \
	prog_data.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

opt_util.date : opt_util.m \
	bool.int3 \
	exprn_aux.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	llds_out.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	assoc_list.int3 \
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
	options.int3 \
	prog_data.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

opt_util.dir/opt_util_000.o: opt_util.m
	rm -rf opt_util.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) opt_util.m
