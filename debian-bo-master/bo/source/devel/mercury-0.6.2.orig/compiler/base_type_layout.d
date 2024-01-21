base_type_layout.optdate base_type_layout.c base_type_layout.err base_type_layout.o : base_type_layout.m \
	assoc_list.int \
	bool.int \
	code_util.int \
	globals.int \
	hlds_data.int \
	hlds_module.int \
	hlds_out.int \
	hlds_pred.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	options.int \
	prog_data.int \
	prog_util.int \
	require.int \
	special_pred.int \
	std_util.int \
	string.int \
	type_util.int \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_goal.int2 \
	instmap.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

base_type_layout.date : base_type_layout.m \
	assoc_list.int3 \
	bool.int3 \
	code_util.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_module.int3 \
	hlds_out.int3 \
	hlds_pred.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	prog_data.int3 \
	prog_util.int3 \
	require.int3 \
	special_pred.int3 \
	std_util.int3 \
	string.int3 \
	type_util.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_goal.int3 \
	instmap.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

base_type_layout.dir/base_type_layout_000.o: base_type_layout.m
	rm -rf base_type_layout.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) base_type_layout.m
