hlds_data.optdate hlds_data.c hlds_data.err hlds_data.o : hlds_data.m \
	bool.int \
	hlds_pred.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	require.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	std_util.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

hlds_data.date : hlds_data.m \
	bool.int3 \
	hlds_pred.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	require.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

hlds_data.dir/hlds_data_000.o: hlds_data.m
	rm -rf hlds_data.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) hlds_data.m
