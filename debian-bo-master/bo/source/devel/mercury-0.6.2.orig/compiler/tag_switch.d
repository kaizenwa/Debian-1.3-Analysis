tag_switch.optdate tag_switch.c tag_switch.err tag_switch.o : tag_switch.m \
	assoc_list.int \
	code_gen.int \
	code_info.int \
	globals.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	options.int \
	require.int \
	std_util.int \
	switch_gen.int \
	tree.int \
	type_util.int \
	bool.int2 \
	char.int2 \
	code_util.int2 \
	float.int2 \
	getopt.int2 \
	hlds_pred.int2 \
	instmap.int2 \
	io.int2 \
	modes.int2 \
	ops.int2 \
	prog_data.int2 \
	relation.int2 \
	set.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	term.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

tag_switch.date : tag_switch.m \
	assoc_list.int3 \
	code_gen.int3 \
	code_info.int3 \
	globals.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	options.int3 \
	require.int3 \
	std_util.int3 \
	switch_gen.int3 \
	tree.int3 \
	type_util.int3 \
	bool.int3 \
	char.int3 \
	code_util.int3 \
	float.int3 \
	getopt.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	io.int3 \
	modes.int3 \
	ops.int3 \
	prog_data.int3 \
	relation.int3 \
	set.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	term.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

tag_switch.dir/tag_switch_000.o: tag_switch.m
	rm -rf tag_switch.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) tag_switch.m
