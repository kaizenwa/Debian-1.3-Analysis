type_util.optdate type_util.c type_util.err type_util.o : type_util.m \
	bool.int \
	hlds_data.int \
	hlds_module.int \
	hlds_pred.int \
	list.int \
	map.int \
	mercury_builtin.int \
	prog_data.int \
	prog_io.int \
	prog_util.int \
	require.int \
	std_util.int \
	term.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	globals.int2 \
	hlds_goal.int2 \
	instmap.int2 \
	int.int2 \
	io.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

type_util.date : type_util.m \
	bool.int3 \
	hlds_data.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	prog_data.int3 \
	prog_io.int3 \
	prog_util.int3 \
	require.int3 \
	std_util.int3 \
	term.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	globals.int3 \
	hlds_goal.int3 \
	instmap.int3 \
	int.int3 \
	io.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

type_util.dir/type_util_000.o: type_util.m
	rm -rf type_util.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) type_util.m
