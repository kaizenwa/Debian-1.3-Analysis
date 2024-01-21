make_tags.optdate make_tags.c make_tags.err make_tags.o : make_tags.m \
	bool.int \
	globals.int \
	hlds_data.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	options.int \
	prog_data.int \
	prog_util.int \
	require.int \
	std_util.int \
	type_util.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	io.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

make_tags.date : make_tags.m \
	bool.int3 \
	globals.int3 \
	hlds_data.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	prog_data.int3 \
	prog_util.int3 \
	require.int3 \
	std_util.int3 \
	type_util.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

make_tags.dir/make_tags_000.o: make_tags.m
	rm -rf make_tags.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) make_tags.m
