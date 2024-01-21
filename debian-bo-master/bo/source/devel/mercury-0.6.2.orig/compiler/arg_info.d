arg_info.optdate arg_info.c arg_info.err arg_info.o : arg_info.m \
	globals.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	require.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	instmap.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	options.int2 \
	prog_data.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	std_util.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

arg_info.date : arg_info.m \
	globals.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	require.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	instmap.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	options.int3 \
	prog_data.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

arg_info.dir/arg_info_000.o: arg_info.m
	rm -rf arg_info.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) arg_info.m
