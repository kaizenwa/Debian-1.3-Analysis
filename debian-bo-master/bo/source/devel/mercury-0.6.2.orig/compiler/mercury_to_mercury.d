mercury_to_mercury.optdate mercury_to_mercury.c mercury_to_mercury.err mercury_to_mercury.o : mercury_to_mercury.m \
	bool.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	mercury_builtin.int \
	options.int \
	prog_data.int \
	prog_out.int \
	prog_util.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	term.int \
	term_io.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_module.int2 \
	instmap.int2 \
	llds.int2 \
	map.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

mercury_to_mercury.date : mercury_to_mercury.m \
	bool.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	options.int3 \
	prog_data.int3 \
	prog_out.int3 \
	prog_util.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	term_io.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_module.int3 \
	instmap.int3 \
	llds.int3 \
	map.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

mercury_to_mercury.dir/mercury_to_mercury_000.o: mercury_to_mercury.m
	rm -rf mercury_to_mercury.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) mercury_to_mercury.m
