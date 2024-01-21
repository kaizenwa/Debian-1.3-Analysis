prog_out.optdate prog_out.c prog_out.err prog_out.o : prog_out.m \
	io.int \
	list.int \
	mercury_builtin.int \
	prog_data.int \
	std_util.int \
	string.int \
	term.int \
	term_io.int \
	varset.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	int.int2 \
	llds.int2 \
	map.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	require.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

prog_out.date : prog_out.m \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	term_io.int3 \
	varset.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	llds.int3 \
	map.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	require.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

prog_out.dir/prog_out_000.o: prog_out.m
	rm -rf prog_out.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) prog_out.m
