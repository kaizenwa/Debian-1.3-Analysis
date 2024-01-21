common.optdate common.c common.err common.o : common.m \
	eqvclass.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	options.int \
	quantification.int \
	require.int \
	set.int \
	std_util.int \
	term.int \
	type_util.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	llds.int2 \
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

common.date : common.m \
	eqvclass.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	options.int3 \
	quantification.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	term.int3 \
	type_util.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	llds.int3 \
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

common.dir/common_000.o: common.m
	rm -rf common.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) common.m
