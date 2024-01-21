passes_aux.optdate passes_aux.c passes_aux.err passes_aux.o : passes_aux.m \
	bool.int \
	globals.int \
	hlds_module.int \
	hlds_out.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mercury_to_mercury.int \
	mode_util.int \
	options.int \
	prog_out.int \
	require.int \
	std_util.int \
	tree234.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	instmap.int2 \
	llds.int2 \
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
	unify_proc.int2

passes_aux.date : passes_aux.m \
	bool.int3 \
	globals.int3 \
	hlds_module.int3 \
	hlds_out.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mercury_to_mercury.int3 \
	mode_util.int3 \
	options.int3 \
	prog_out.int3 \
	require.int3 \
	std_util.int3 \
	tree234.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	instmap.int3 \
	llds.int3 \
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
	unify_proc.int3

passes_aux.dir/passes_aux_000.o: passes_aux.m
	rm -rf passes_aux.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) passes_aux.m
