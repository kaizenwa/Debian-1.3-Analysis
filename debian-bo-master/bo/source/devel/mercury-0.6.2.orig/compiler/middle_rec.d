middle_rec.optdate middle_rec.c middle_rec.err middle_rec.o : middle_rec.m \
	assoc_list.int \
	bool.int \
	code_aux.int \
	code_gen.int \
	code_info.int \
	code_util.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	int.int \
	list.int \
	llds.int \
	mercury_builtin.int \
	opt_util.int \
	require.int \
	set.int \
	std_util.int \
	tree.int \
	unify_gen.int \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	io.int2 \
	map.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

middle_rec.date : middle_rec.m \
	assoc_list.int3 \
	bool.int3 \
	code_aux.int3 \
	code_gen.int3 \
	code_info.int3 \
	code_util.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	mercury_builtin.int3 \
	opt_util.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	tree.int3 \
	unify_gen.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	map.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

middle_rec.dir/middle_rec_000.o: middle_rec.m
	rm -rf middle_rec.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) middle_rec.m
