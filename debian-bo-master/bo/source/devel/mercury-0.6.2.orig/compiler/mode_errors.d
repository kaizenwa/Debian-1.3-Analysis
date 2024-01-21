mode_errors.optdate mode_errors.c mode_errors.err mode_errors.o : mode_errors.m \
	assoc_list.int \
	bool.int \
	globals.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_out.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mercury_to_mercury.int \
	mode_info.int \
	mode_util.int \
	options.int \
	prog_data.int \
	prog_out.int \
	require.int \
	set.int \
	std_util.int \
	term.int \
	term_io.int \
	varset.int \
	char.int2 \
	delay_info.int2 \
	float.int2 \
	getopt.int2 \
	hlds_data.int2 \
	instmap.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

mode_errors.date : mode_errors.m \
	assoc_list.int3 \
	bool.int3 \
	globals.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_out.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mercury_to_mercury.int3 \
	mode_info.int3 \
	mode_util.int3 \
	options.int3 \
	prog_data.int3 \
	prog_out.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	term.int3 \
	term_io.int3 \
	varset.int3 \
	char.int3 \
	delay_info.int3 \
	float.int3 \
	getopt.int3 \
	hlds_data.int3 \
	instmap.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

mode_errors.dir/mode_errors_000.o: mode_errors.m
	rm -rf mode_errors.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) mode_errors.m
