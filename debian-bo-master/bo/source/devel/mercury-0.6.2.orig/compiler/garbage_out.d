garbage_out.optdate garbage_out.c garbage_out.err garbage_out.o : garbage_out.m \
	assoc_list.int \
	hlds_module.int \
	io.int \
	list.int \
	llds.int \
	llds_out.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	require.int \
	shapes.int \
	std_util.int \
	string.int \
	term.int \
	term_io.int \
	type_util.int \
	varset.int \
	bool.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	int.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

garbage_out.date : garbage_out.m \
	assoc_list.int3 \
	hlds_module.int3 \
	io.int3 \
	list.int3 \
	llds.int3 \
	llds_out.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	require.int3 \
	shapes.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	term_io.int3 \
	type_util.int3 \
	varset.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

garbage_out.dir/garbage_out_000.o: garbage_out.m
	rm -rf garbage_out.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) garbage_out.m
