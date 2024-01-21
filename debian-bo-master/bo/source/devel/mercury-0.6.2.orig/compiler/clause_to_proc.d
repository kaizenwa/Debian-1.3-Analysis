clause_to_proc.optdate clause_to_proc.c clause_to_proc.err clause_to_proc.o : clause_to_proc.m \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	list.int \
	make_hlds.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	set.int \
	std_util.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	equiv_type.int2 \
	float.int2 \
	globals.int2 \
	instmap.int2 \
	io.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	require.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

clause_to_proc.date : clause_to_proc.m \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	list.int3 \
	make_hlds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	set.int3 \
	std_util.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	equiv_type.int3 \
	float.int3 \
	globals.int3 \
	instmap.int3 \
	io.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	require.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

clause_to_proc.dir/clause_to_proc_000.o: clause_to_proc.m
	rm -rf clause_to_proc.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) clause_to_proc.m
