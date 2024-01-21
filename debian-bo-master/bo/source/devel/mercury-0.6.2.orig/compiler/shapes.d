shapes.optdate shapes.c shapes.err shapes.o : shapes.m \
	assoc_list.int \
	hlds_data.int \
	hlds_module.int \
	int.int \
	io.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	require.int \
	std_util.int \
	term.int \
	type_util.int \
	bool.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_goal.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

shapes.date : shapes.m \
	assoc_list.int3 \
	hlds_data.int3 \
	hlds_module.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	require.int3 \
	std_util.int3 \
	term.int3 \
	type_util.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_goal.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

shapes.dir/shapes_000.o: shapes.m
	rm -rf shapes.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) shapes.m
