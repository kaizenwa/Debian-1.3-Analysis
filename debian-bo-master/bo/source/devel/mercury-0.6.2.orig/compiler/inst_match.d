inst_match.optdate inst_match.c inst_match.err inst_match.o : inst_match.m \
	hlds_data.int \
	hlds_module.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	prog_data.int \
	require.int \
	set.int \
	std_util.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_goal.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

inst_match.date : inst_match.m \
	hlds_data.int3 \
	hlds_module.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_goal.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

inst_match.dir/inst_match_000.o: inst_match.m
	rm -rf inst_match.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) inst_match.m
