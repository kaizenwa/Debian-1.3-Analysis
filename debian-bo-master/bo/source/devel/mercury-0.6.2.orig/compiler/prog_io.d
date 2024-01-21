prog_io.optdate prog_io.c prog_io.err prog_io.o : prog_io.m \
	bool.int \
	dir.int \
	globals.int \
	hlds_data.int \
	hlds_pred.int \
	int.int \
	io.int \
	list.int \
	mercury_builtin.int \
	options.int \
	parser.int \
	prog_data.int \
	prog_util.int \
	require.int \
	std_util.int \
	string.int \
	term.int \
	term_io.int \
	varset.int \
	assoc_list.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	instmap.int2 \
	llds.int2 \
	map.int2 \
	modes.int2 \
	ops.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2

prog_io.date : prog_io.m \
	bool.int3 \
	dir.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_pred.int3 \
	int.int3 \
	io.int3 \
	list.int3 \
	mercury_builtin.int3 \
	options.int3 \
	parser.int3 \
	prog_data.int3 \
	prog_util.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	term_io.int3 \
	varset.int3 \
	assoc_list.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	instmap.int3 \
	llds.int3 \
	map.int3 \
	modes.int3 \
	ops.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3

prog_io.dir/prog_io_000.o: prog_io.m
	rm -rf prog_io.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) prog_io.m
