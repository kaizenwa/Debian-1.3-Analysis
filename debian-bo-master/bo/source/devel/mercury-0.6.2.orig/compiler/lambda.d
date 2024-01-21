lambda.optdate lambda.c lambda.err lambda.o : lambda.m \
	bool.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	inst_match.int \
	list.int \
	llds.int \
	make_hlds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	prog_data.int \
	prog_util.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	term.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	equiv_type.int2 \
	float.int2 \
	globals.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

lambda.date : lambda.m \
	bool.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	inst_match.int3 \
	list.int3 \
	llds.int3 \
	make_hlds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	prog_data.int3 \
	prog_util.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	equiv_type.int3 \
	float.int3 \
	globals.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

lambda.dir/lambda_000.o: lambda.m
	rm -rf lambda.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) lambda.m
