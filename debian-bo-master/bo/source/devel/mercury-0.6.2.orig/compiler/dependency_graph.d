dependency_graph.optdate dependency_graph.c dependency_graph.err dependency_graph.o : dependency_graph.m \
	bool.int \
	code_util.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	llds.int \
	llds_out.int \
	map.int \
	mercury_builtin.int \
	mercury_to_mercury.int \
	mode_util.int \
	options.int \
	prog_data.int \
	relation.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	term.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	instmap.int2 \
	modes.int2 \
	ops.int2 \
	set_bbbtree.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

dependency_graph.date : dependency_graph.m \
	bool.int3 \
	code_util.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	llds.int3 \
	llds_out.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mercury_to_mercury.int3 \
	mode_util.int3 \
	options.int3 \
	prog_data.int3 \
	relation.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	instmap.int3 \
	modes.int3 \
	ops.int3 \
	set_bbbtree.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

dependency_graph.dir/dependency_graph_000.o: dependency_graph.m
	rm -rf dependency_graph.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) dependency_graph.m
