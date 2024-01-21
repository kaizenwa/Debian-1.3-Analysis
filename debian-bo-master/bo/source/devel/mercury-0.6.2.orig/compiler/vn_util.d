vn_util.optdate vn_util.c vn_util.err vn_util.o : vn_util.m \
	bool.int \
	float.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	opt_util.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	vn_table.int \
	vn_type.int \
	assoc_list.int2 \
	char.int2 \
	getopt.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	io.int2 \
	livemap.int2 \
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

vn_util.date : vn_util.m \
	bool.int3 \
	float.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	opt_util.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	vn_table.int3 \
	vn_type.int3 \
	assoc_list.int3 \
	char.int3 \
	getopt.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	livemap.int3 \
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

vn_util.dir/vn_util_000.o: vn_util.m
	rm -rf vn_util.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) vn_util.m
