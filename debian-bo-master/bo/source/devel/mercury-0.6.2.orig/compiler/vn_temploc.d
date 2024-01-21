vn_temploc.optdate vn_temploc.c vn_temploc.err vn_temploc.o : vn_temploc.m \
	int.int \
	list.int \
	llds.int \
	mercury_builtin.int \
	require.int \
	set.int \
	vn_table.int \
	vn_type.int \
	assoc_list.int2 \
	bool.int2 \
	char.int2 \
	float.int2 \
	getopt.int2 \
	globals.int2 \
	hlds_data.int2 \
	hlds_goal.int2 \
	hlds_module.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	io.int2 \
	livemap.int2 \
	map.int2 \
	modes.int2 \
	ops.int2 \
	options.int2 \
	prog_data.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	std_util.int2 \
	string.int2 \
	term.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

vn_temploc.date : vn_temploc.m \
	int.int3 \
	list.int3 \
	llds.int3 \
	mercury_builtin.int3 \
	require.int3 \
	set.int3 \
	vn_table.int3 \
	vn_type.int3 \
	assoc_list.int3 \
	bool.int3 \
	char.int3 \
	float.int3 \
	getopt.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	livemap.int3 \
	map.int3 \
	modes.int3 \
	ops.int3 \
	options.int3 \
	prog_data.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	std_util.int3 \
	string.int3 \
	term.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

vn_temploc.dir/vn_temploc_000.o: vn_temploc.m
	rm -rf vn_temploc.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) vn_temploc.m
