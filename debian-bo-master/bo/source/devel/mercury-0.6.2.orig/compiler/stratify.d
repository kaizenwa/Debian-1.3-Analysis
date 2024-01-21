stratify.optdate stratify.c stratify.err stratify.o : stratify.m \
	bool.int \
	dependency_graph.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	options.int \
	passes_aux.int \
	prog_data.int \
	prog_out.int \
	relation.int \
	require.int \
	set.int \
	std_util.int \
	type_util.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	instmap.int2 \
	int.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	set_bbbtree.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

stratify.date : stratify.m \
	bool.int3 \
	dependency_graph.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	options.int3 \
	passes_aux.int3 \
	prog_data.int3 \
	prog_out.int3 \
	relation.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	type_util.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	instmap.int3 \
	int.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	set_bbbtree.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

stratify.dir/stratify_000.o: stratify.m
	rm -rf stratify.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) stratify.m
