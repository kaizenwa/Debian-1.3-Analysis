module_qual.optdate module_qual.c module_qual.err module_qual.o : module_qual.m \
	bool.int \
	globals.int \
	hlds_data.int \
	hlds_module.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mercury_to_mercury.int \
	options.int \
	prog_data.int \
	prog_out.int \
	prog_util.int \
	require.int \
	set.int \
	std_util.int \
	string.int \
	term.int \
	type_util.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_goal.int2 \
	instmap.int2 \
	llds.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

module_qual.date : module_qual.m \
	bool.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mercury_to_mercury.int3 \
	options.int3 \
	prog_data.int3 \
	prog_out.int3 \
	prog_util.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	type_util.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_goal.int3 \
	instmap.int3 \
	llds.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

module_qual.dir/module_qual_000.o: module_qual.m
	rm -rf module_qual.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) module_qual.m
