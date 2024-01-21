switch_detection.optdate switch_detection.c switch_detection.err switch_detection.o : switch_detection.m \
	assoc_list.int \
	det_util.int \
	hlds_data.int \
	hlds_goal.int \
	hlds_module.int \
	hlds_pred.int \
	instmap.int \
	int.int \
	list.int \
	map.int \
	mercury_builtin.int \
	mode_util.int \
	modes.int \
	prog_data.int \
	require.int \
	set.int \
	std_util.int \
	term.int \
	type_util.int \
	bool.int2 \
	char.int2 \
	clause_to_proc.int2 \
	delay_info.int2 \
	float.int2 \
	globals.int2 \
	io.int2 \
	llds.int2 \
	mode_errors.int2 \
	mode_info.int2 \
	ops.int2 \
	relation.int2 \
	shapes.int2 \
	special_pred.int2 \
	string.int2 \
	tree.int2 \
	tree234.int2 \
	unify_proc.int2 \
	varset.int2

switch_detection.date : switch_detection.m \
	assoc_list.int3 \
	det_util.int3 \
	hlds_data.int3 \
	hlds_goal.int3 \
	hlds_module.int3 \
	hlds_pred.int3 \
	instmap.int3 \
	int.int3 \
	list.int3 \
	map.int3 \
	mercury_builtin.int3 \
	mode_util.int3 \
	modes.int3 \
	prog_data.int3 \
	require.int3 \
	set.int3 \
	std_util.int3 \
	term.int3 \
	type_util.int3 \
	bool.int3 \
	char.int3 \
	clause_to_proc.int3 \
	delay_info.int3 \
	float.int3 \
	globals.int3 \
	io.int3 \
	llds.int3 \
	mode_errors.int3 \
	mode_info.int3 \
	ops.int3 \
	relation.int3 \
	shapes.int3 \
	special_pred.int3 \
	string.int3 \
	tree.int3 \
	tree234.int3 \
	unify_proc.int3 \
	varset.int3

switch_detection.dir/switch_detection_000.o: switch_detection.m
	rm -rf switch_detection.dir
	$(MCS) -s$(GRADE) $(MCSFLAGS) switch_detection.m
