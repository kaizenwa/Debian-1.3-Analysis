exprn_aux.optdate exprn_aux.c exprn_aux.err exprn_aux.o : exprn_aux.m \
	assoc_list.int \
	bool.int \
	getopt.int \
	int.int \
	list.int \
	llds.int \
	mercury_builtin.int \
	options.int \
	require.int \
	std_util.int \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	io.int2 \
	map.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

exprn_aux.date : exprn_aux.m \
	assoc_list.int3 \
	bool.int3 \
	getopt.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	mercury_builtin.int3 \
	options.int3 \
	require.int3 \
	std_util.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	map.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

exprn_aux.dir/exprn_aux_000.o: exprn_aux.m
	rm -rf exprn_aux.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) exprn_aux.m
