vn_filter.optdate vn_filter.c vn_filter.err vn_filter.o : vn_filter.m \
	list.int \
	llds.int \
	mercury_builtin.int \
	opt_util.int \
	require.int \
	std_util.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	int.int2 \
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

vn_filter.date : vn_filter.m \
	list.int3 \
	llds.int3 \
	mercury_builtin.int3 \
	opt_util.int3 \
	require.int3 \
	std_util.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
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

vn_filter.dir/vn_filter_000.o: vn_filter.m
	rm -rf vn_filter.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) vn_filter.m
