string_switch.optdate string_switch.c string_switch.err string_switch.o : string_switch.m \
	assoc_list.int \
	bool.int \
	code_gen.int \
	code_info.int \
	hlds_data.int \
	hlds_goal.int \
	int.int \
	list.int \
	llds.int \
	map.int \
	mercury_builtin.int \
	require.int \
	std_util.int \
	string.int \
	switch_gen.int \
	tree.int \
	char.int2 \
	code_util.int2 \
	float.int2 \
	globals.int2 \
	hlds_module.int2 \
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
	term.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

string_switch.date : string_switch.m \
	assoc_list.int3 \
	bool.int3 \
	code_gen.int3 \
	code_info.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	int.int3 \
	list.int3 \
	llds.int3 \
	map.int3 \
	mercury_builtin.int3 \
	require.int3 \
	std_util.int3 \
	string.int3 \
	switch_gen.int3 \
	tree.int3 \
	char.int3 \
	code_util.int3 \
	float.int3 \
	globals.int3 \
	hlds_module.int3 \
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
	term.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

string_switch.dir/string_switch_000.o: string_switch.m
	rm -rf string_switch.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) string_switch.m
