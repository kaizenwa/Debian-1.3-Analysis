hlds_module.optdate hlds_module.c hlds_module.err hlds_module.o : hlds_module.m \
	globals.int \
	hlds_data.int \
	hlds_out.int \
	hlds_pred.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	relation.int \
	require.int \
	set.int \
	shapes.int \
	special_pred.int \
	std_util.int \
	string.int \
	unify_proc.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_goal.int2 \
	instmap.int2 \
	io.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	options.int2 \
	set_bbbtree.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	varset.int2

hlds_module.date : hlds_module.m \
	globals.int3 \
	hlds_data.int3 \
	hlds_out.int3 \
	hlds_pred.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	relation.int3 \
	require.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	std_util.int3 \
	string.int3 \
	unify_proc.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_goal.int3 \
	instmap.int3 \
	io.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	options.int3 \
	set_bbbtree.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	varset.int3

hlds_module.dir/hlds_module_000.o: hlds_module.m
	rm -rf hlds_module.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) hlds_module.m
