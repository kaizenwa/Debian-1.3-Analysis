llds.optdate llds.c llds.err llds.o : llds.m \
	bool.int \
	list.int \
	mercury_builtin.int \
	require.int \
	set.int \
	shapes.int \
	std_util.int \
	term.int \
	tree.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	map.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	special_pred.int2 \
	string.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

llds.date : llds.m \
	bool.int3 \
	list.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	shapes.int3 \
	std_util.int3 \
	term.int3 \
	tree.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	map.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	special_pred.int3 \
	string.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

llds.dir/llds_000.o: llds.m
	rm -rf llds.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) llds.m
