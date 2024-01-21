vn_debug.optdate vn_debug.c vn_debug.err vn_debug.o : vn_debug.m \
	atsort.int \
	bool.int \
	globals.int \
	int.int \
	io.int \
	list.int \
	livemap.int \
	llds.int \
	llds_out.int \
	map.int \
	mercury_builtin.int \
	opt_debug.int \
	options.int \
	std_util.int \
	string.int \
	vn_table.int \
	vn_type.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	require.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

vn_debug.date : vn_debug.m \
	atsort.int3 \
	bool.int3 \
	globals.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	livemap.int3 \
	llds.int3 \
	llds_out.int3 \
	map.int3 \
	mercury_builtin.int3 \
	opt_debug.int3 \
	options.int3 \
	std_util.int3 \
	string.int3 \
	vn_table.int3 \
	vn_type.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	require.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

vn_debug.dir/vn_debug_000.o: vn_debug.m
	rm -rf vn_debug.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) vn_debug.m
