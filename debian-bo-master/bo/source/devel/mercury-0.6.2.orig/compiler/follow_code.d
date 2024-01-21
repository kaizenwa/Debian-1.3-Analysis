follow_code.optdate follow_code.c follow_code.err follow_code.o : follow_code.m \
	bool.int \
	det_analysis.int \
	globals.int \
	goal_util.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	options.int \
	quantification.int \
	require.int \
	set.int \
	std_util.int \
	term.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

follow_code.date : follow_code.m \
	bool.int3 \
	det_analysis.int3 \
	globals.int3 \
	goal_util.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	options.int3 \
	quantification.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	term.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

follow_code.dir/follow_code_000.o: follow_code.m
	rm -rf follow_code.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) follow_code.m
