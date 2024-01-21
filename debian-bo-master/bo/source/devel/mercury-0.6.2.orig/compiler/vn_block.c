/*
** Automatically generated from `vn_block.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__vn_block__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__vn_block__divide_into_blocks_3_0);
Define_extern_entry(mercury__vn_block__build_block_info_9_0);
Declare_label(mercury__vn_block__build_block_info_9_0_i2);
Declare_label(mercury__vn_block__build_block_info_9_0_i3);
Declare_label(mercury__vn_block__build_block_info_9_0_i4);
Declare_label(mercury__vn_block__build_block_info_9_0_i5);
Declare_label(mercury__vn_block__build_block_info_9_0_i6);
Declare_label(mercury__vn_block__build_block_info_9_0_i7);
Define_extern_entry(mercury__vn_block__split_at_next_ctrl_instr_4_0);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i9);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i11);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1009);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i27);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i28);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i6);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i29);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i4);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i30);
Declare_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1008);
Declare_static(mercury__vn_block__divide_into_blocks_2_5_0);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i6);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i7);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i5);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i11);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i13);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i16);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i9);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i3);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i21);
Declare_label(mercury__vn_block__divide_into_blocks_2_5_0_i25);
Declare_static(mercury__vn_block__build_from_parallel_3_0);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i4);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i7);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i8);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i9);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i10);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i6);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i5);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i11);
Declare_label(mercury__vn_block__build_from_parallel_3_0_i1003);
Declare_static(mercury__vn_block__real_fake_parentries_3_0);
Declare_label(mercury__vn_block__real_fake_parentries_3_0_i7);
Declare_label(mercury__vn_block__real_fake_parentries_3_0_i8);
Declare_label(mercury__vn_block__real_fake_parentries_3_0_i3);
Declare_label(mercury__vn_block__real_fake_parentries_3_0_i6);
Declare_label(mercury__vn_block__real_fake_parentries_3_0_i1);
Declare_static(mercury__vn_block__build_from_fake_rval_4_0);
Declare_label(mercury__vn_block__build_from_fake_rval_4_0_i6);
Declare_label(mercury__vn_block__build_from_fake_rval_4_0_i8);
Declare_label(mercury__vn_block__build_from_fake_rval_4_0_i9);
Declare_label(mercury__vn_block__build_from_fake_rval_4_0_i5);
Declare_label(mercury__vn_block__build_from_fake_rval_4_0_i1003);
Declare_static(mercury__vn_block__handle_instrs_11_0);
Declare_label(mercury__vn_block__handle_instrs_11_0_i4);
Declare_label(mercury__vn_block__handle_instrs_11_0_i1002);
Declare_static(mercury__vn_block__handle_instr_11_0);
Declare_label(mercury__vn_block__handle_instr_11_0_i1018);
Declare_label(mercury__vn_block__handle_instr_11_0_i1017);
Declare_label(mercury__vn_block__handle_instr_11_0_i1016);
Declare_label(mercury__vn_block__handle_instr_11_0_i1015);
Declare_label(mercury__vn_block__handle_instr_11_0_i1014);
Declare_label(mercury__vn_block__handle_instr_11_0_i1013);
Declare_label(mercury__vn_block__handle_instr_11_0_i1012);
Declare_label(mercury__vn_block__handle_instr_11_0_i1011);
Declare_label(mercury__vn_block__handle_instr_11_0_i1010);
Declare_label(mercury__vn_block__handle_instr_11_0_i1009);
Declare_label(mercury__vn_block__handle_instr_11_0_i1008);
Declare_label(mercury__vn_block__handle_instr_11_0_i1007);
Declare_label(mercury__vn_block__handle_instr_11_0_i1006);
Declare_label(mercury__vn_block__handle_instr_11_0_i1005);
Declare_label(mercury__vn_block__handle_instr_11_0_i1004);
Declare_label(mercury__vn_block__handle_instr_11_0_i1003);
Declare_label(mercury__vn_block__handle_instr_11_0_i1002);
Declare_label(mercury__vn_block__handle_instr_11_0_i1001);
Declare_label(mercury__vn_block__handle_instr_11_0_i5);
Declare_label(mercury__vn_block__handle_instr_11_0_i6);
Declare_label(mercury__vn_block__handle_instr_11_0_i7);
Declare_label(mercury__vn_block__handle_instr_11_0_i8);
Declare_label(mercury__vn_block__handle_instr_11_0_i9);
Declare_label(mercury__vn_block__handle_instr_11_0_i10);
Declare_label(mercury__vn_block__handle_instr_11_0_i11);
Declare_label(mercury__vn_block__handle_instr_11_0_i12);
Declare_label(mercury__vn_block__handle_instr_11_0_i14);
Declare_label(mercury__vn_block__handle_instr_11_0_i15);
Declare_label(mercury__vn_block__handle_instr_11_0_i16);
Declare_label(mercury__vn_block__handle_instr_11_0_i18);
Declare_label(mercury__vn_block__handle_instr_11_0_i20);
Declare_label(mercury__vn_block__handle_instr_11_0_i22);
Declare_label(mercury__vn_block__handle_instr_11_0_i24);
Declare_label(mercury__vn_block__handle_instr_11_0_i25);
Declare_label(mercury__vn_block__handle_instr_11_0_i27);
Declare_label(mercury__vn_block__handle_instr_11_0_i29);
Declare_label(mercury__vn_block__handle_instr_11_0_i30);
Declare_label(mercury__vn_block__handle_instr_11_0_i32);
Declare_label(mercury__vn_block__handle_instr_11_0_i34);
Declare_label(mercury__vn_block__handle_instr_11_0_i33);
Declare_label(mercury__vn_block__handle_instr_11_0_i35);
Declare_label(mercury__vn_block__handle_instr_11_0_i36);
Declare_label(mercury__vn_block__handle_instr_11_0_i38);
Declare_label(mercury__vn_block__handle_instr_11_0_i39);
Declare_label(mercury__vn_block__handle_instr_11_0_i41);
Declare_label(mercury__vn_block__handle_instr_11_0_i42);
Declare_label(mercury__vn_block__handle_instr_11_0_i44);
Declare_label(mercury__vn_block__handle_instr_11_0_i45);
Declare_label(mercury__vn_block__handle_instr_11_0_i47);
Declare_label(mercury__vn_block__handle_instr_11_0_i48);
Declare_label(mercury__vn_block__handle_instr_11_0_i50);
Declare_label(mercury__vn_block__handle_instr_11_0_i52);
Declare_label(mercury__vn_block__handle_instr_11_0_i54);
Declare_label(mercury__vn_block__handle_instr_11_0_i1000);
Declare_label(mercury__vn_block__handle_instr_11_0_i56);
Declare_label(mercury__vn_block__handle_instr_11_0_i58);
Declare_label(mercury__vn_block__handle_instr_11_0_i59);
Declare_static(mercury__vn_block__new_ctrl_node_9_0);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i2);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i6);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i8);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i12);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i14);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i10);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i9);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i15);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i17);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i19);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i21);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i22);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i23);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i24);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i26);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i29);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i28);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i31);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i32);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i33);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i5);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i38);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i40);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i41);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i39);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i42);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i3);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i43);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i44);
Declare_label(mercury__vn_block__new_ctrl_node_9_0_i45);
Declare_static(mercury__vn_block__new_if_node_14_0);
Declare_label(mercury__vn_block__new_if_node_14_0_i5);
Declare_label(mercury__vn_block__new_if_node_14_0_i7);
Declare_label(mercury__vn_block__new_if_node_14_0_i8);
Declare_label(mercury__vn_block__new_if_node_14_0_i9);
Declare_label(mercury__vn_block__new_if_node_14_0_i10);
Declare_label(mercury__vn_block__new_if_node_14_0_i11);
Declare_label(mercury__vn_block__new_if_node_14_0_i14);
Declare_label(mercury__vn_block__new_if_node_14_0_i17);
Declare_label(mercury__vn_block__new_if_node_14_0_i18);
Declare_label(mercury__vn_block__new_if_node_14_0_i13);
Declare_label(mercury__vn_block__new_if_node_14_0_i3);
Declare_label(mercury__vn_block__new_if_node_14_0_i2);
Declare_label(mercury__vn_block__new_if_node_14_0_i20);
Declare_label(mercury__vn_block__new_if_node_14_0_i26);
Declare_label(mercury__vn_block__new_if_node_14_0_i27);
Declare_label(mercury__vn_block__new_if_node_14_0_i23);
Declare_label(mercury__vn_block__new_if_node_14_0_i28);
Declare_label(mercury__vn_block__new_if_node_14_0_i29);
Declare_static(mercury__vn_block__record_at_call_6_0);
Declare_label(mercury__vn_block__record_at_call_6_0_i2);
Declare_label(mercury__vn_block__record_at_call_6_0_i3);
Declare_label(mercury__vn_block__record_at_call_6_0_i4);
Declare_label(mercury__vn_block__record_at_call_6_0_i5);
Declare_static(mercury__vn_block__record_several_labels_12_0);
Declare_label(mercury__vn_block__record_several_labels_12_0_i2);
Declare_label(mercury__vn_block__record_several_labels_12_0_i3);
Declare_label(mercury__vn_block__record_several_labels_12_0_i4);
Declare_static(mercury__vn_block__record_one_label_12_0);
Declare_label(mercury__vn_block__record_one_label_12_0_i2);
Declare_label(mercury__vn_block__record_one_label_12_0_i3);
Declare_label(mercury__vn_block__record_one_label_12_0_i4);
Declare_static(mercury__vn_block__record_labels_12_0);
Declare_label(mercury__vn_block__record_labels_12_0_i4);
Declare_label(mercury__vn_block__record_labels_12_0_i5);
Declare_label(mercury__vn_block__record_labels_12_0_i6);
Declare_label(mercury__vn_block__record_labels_12_0_i1002);
Declare_static(mercury__vn_block__record_label_12_0);
Declare_label(mercury__vn_block__record_label_12_0_i4);
Declare_label(mercury__vn_block__record_label_12_0_i6);
Declare_label(mercury__vn_block__record_label_12_0_i7);
Declare_label(mercury__vn_block__record_label_12_0_i8);
Declare_label(mercury__vn_block__record_label_12_0_i11);
Declare_label(mercury__vn_block__record_label_12_0_i3);
Declare_label(mercury__vn_block__record_label_12_0_i16);
Declare_label(mercury__vn_block__record_label_12_0_i17);
Declare_static(mercury__vn_block__record_livevals_9_0);
Declare_label(mercury__vn_block__record_livevals_9_0_i4);
Declare_label(mercury__vn_block__record_livevals_9_0_i9);
Declare_label(mercury__vn_block__record_livevals_9_0_i11);
Declare_label(mercury__vn_block__record_livevals_9_0_i12);
Declare_label(mercury__vn_block__record_livevals_9_0_i13);
Declare_label(mercury__vn_block__record_livevals_9_0_i14);
Declare_label(mercury__vn_block__record_livevals_9_0_i8);
Declare_label(mercury__vn_block__record_livevals_9_0_i18);
Declare_label(mercury__vn_block__record_livevals_9_0_i19);
Declare_label(mercury__vn_block__record_livevals_9_0_i20);
Declare_label(mercury__vn_block__record_livevals_9_0_i21);
Declare_label(mercury__vn_block__record_livevals_9_0_i6);
Declare_label(mercury__vn_block__record_livevals_9_0_i5);
Declare_label(mercury__vn_block__record_livevals_9_0_i22);
Declare_label(mercury__vn_block__record_livevals_9_0_i23);
Declare_label(mercury__vn_block__record_livevals_9_0_i1006);
Declare_static(mercury__vn_block__record_livevnlvals_7_0);
Declare_label(mercury__vn_block__record_livevnlvals_7_0_i6);
Declare_label(mercury__vn_block__record_livevnlvals_7_0_i5);
Declare_label(mercury__vn_block__record_livevnlvals_7_0_i8);
Declare_label(mercury__vn_block__record_livevnlvals_7_0_i9);
Declare_label(mercury__vn_block__record_livevnlvals_7_0_i10);
Declare_label(mercury__vn_block__record_livevnlvals_7_0_i11);
Declare_label(mercury__vn_block__record_livevnlvals_7_0_i1003);
Declare_static(mercury__vn_block__record_compulsory_lval_list_5_0);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1005);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i9);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1002);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i6);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i16);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i17);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i4);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1000);
Declare_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1001);
Declare_static(mercury__vn_block__find_cheaper_copies_2_5_0);
Declare_label(mercury__vn_block__find_cheaper_copies_2_5_0_i6);
Declare_label(mercury__vn_block__find_cheaper_copies_2_5_0_i9);
Declare_label(mercury__vn_block__find_cheaper_copies_2_5_0_i11);
Declare_label(mercury__vn_block__find_cheaper_copies_2_5_0_i8);
Declare_label(mercury__vn_block__find_cheaper_copies_2_5_0_i1008);
Declare_label(mercury__vn_block__find_cheaper_copies_2_5_0_i1010);

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_vn_type__base_type_info_vnlval_0[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_vn_block__common_0[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_vn_type__base_type_info_vnlval_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
extern Word * mercury_data_vn_type__base_type_info_parallel_0[];
Word * mercury_data_vn_block__common_1[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data_vn_type__base_type_info_parallel_0
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_llds__base_type_info_instr_0[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_vn_block__common_2[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_instr_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word * mercury_data_vn_block__common_3[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_2)
};

Word mercury_data_vn_block__common_4[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_vn_block__common_5[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_4)
};

Word mercury_data_vn_block__common_6[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 2)))
};

Word * mercury_data_vn_block__common_7[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_6)
};

Word mercury_data_vn_block__common_8[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 3)))
};

extern Word * mercury_data_set__base_type_info_set_1[];
extern Word * mercury_data_llds__base_type_info_lval_0[];
Word * mercury_data_vn_block__common_9[] = {
	(Word *) (Integer) mercury_data_set__base_type_info_set_1,
	(Word *) (Integer) mercury_data_llds__base_type_info_lval_0
};

Word * mercury_data_vn_block__common_10[] = {
	(Word *) string_const(" in Livemap", 11),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

extern Word * mercury_data_llds__base_type_info_rval_0[];
Word * mercury_data_vn_block__common_11[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data_llds__base_type_info_rval_0
};

Word * mercury_data_vn_block__common_12[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_lval_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_11)
};

BEGIN_MODULE(mercury__vn_block_module0)
	init_entry(mercury__vn_block__divide_into_blocks_3_0);
BEGIN_CODE

/* code for predicate 'vn_block__divide_into_blocks'/3 in mode 0 */
Define_entry(mercury__vn_block__divide_into_blocks_3_0);
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__vn_block__divide_into_blocks_2_5_0),
		ENTRY(mercury__vn_block__divide_into_blocks_3_0));
END_MODULE

BEGIN_MODULE(mercury__vn_block_module1)
	init_entry(mercury__vn_block__build_block_info_9_0);
	init_label(mercury__vn_block__build_block_info_9_0_i2);
	init_label(mercury__vn_block__build_block_info_9_0_i3);
	init_label(mercury__vn_block__build_block_info_9_0_i4);
	init_label(mercury__vn_block__build_block_info_9_0_i5);
	init_label(mercury__vn_block__build_block_info_9_0_i6);
	init_label(mercury__vn_block__build_block_info_9_0_i7);
BEGIN_CODE

/* code for predicate 'vn_block__build_block_info'/9 in mode 0 */
Define_entry(mercury__vn_block__build_block_info_9_0);
	incr_sp_push_msg(9, "vn_block__build_block_info");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	{
	Declare_entry(mercury__vn_table__init_tables_1_0);
	call_localret(ENTRY(mercury__vn_table__init_tables_1_0),
		mercury__vn_block__build_block_info_9_0_i2,
		ENTRY(mercury__vn_block__build_block_info_9_0));
	}
Define_label(mercury__vn_block__build_block_info_9_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_block__build_block_info_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__vn_block__build_from_parallel_3_0),
		mercury__vn_block__build_block_info_9_0_i3,
		ENTRY(mercury__vn_block__build_block_info_9_0));
Define_label(mercury__vn_block__build_block_info_9_0_i3);
	update_prof_current_proc(LABEL(mercury__vn_block__build_block_info_9_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__vn_block__build_block_info_9_0_i4,
		ENTRY(mercury__vn_block__build_block_info_9_0));
	}
Define_label(mercury__vn_block__build_block_info_9_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__build_block_info_9_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__vn_block__build_block_info_9_0_i5,
		ENTRY(mercury__vn_block__build_block_info_9_0));
	}
Define_label(mercury__vn_block__build_block_info_9_0_i5);
	update_prof_current_proc(LABEL(mercury__vn_block__build_block_info_9_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_0);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__vn_block__build_block_info_9_0_i6,
		ENTRY(mercury__vn_block__build_block_info_9_0));
	}
Define_label(mercury__vn_block__build_block_info_9_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__build_block_info_9_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_1);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__vn_block__build_block_info_9_0_i7,
		ENTRY(mercury__vn_block__build_block_info_9_0));
	}
Define_label(mercury__vn_block__build_block_info_9_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_block__build_block_info_9_0));
	tag_incr_hp(r7, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r7, ((Integer) 4)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(6);
	r6 = ((Integer) 1);
	field(mktag(0), (Integer) r7, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r7, ((Integer) 2)) = (Integer) detstackvar(8);
	field(mktag(0), (Integer) r7, ((Integer) 0)) = ((Integer) 0);
	field(mktag(0), (Integer) r7, ((Integer) 1)) = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__vn_block__handle_instrs_11_0),
		ENTRY(mercury__vn_block__build_block_info_9_0));
END_MODULE

BEGIN_MODULE(mercury__vn_block_module2)
	init_entry(mercury__vn_block__split_at_next_ctrl_instr_4_0);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i9);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i11);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1009);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i27);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i28);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i6);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i29);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i4);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i30);
	init_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1008);
BEGIN_CODE

/* code for predicate 'vn_block__split_at_next_ctrl_instr'/4 in mode 0 */
Define_entry(mercury__vn_block__split_at_next_ctrl_instr_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1008);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r3 = (Integer) r2;
	if ((tag((Integer) r4) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1009);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r4, ((Integer) 0)),
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025) AND
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027));
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1027);
	incr_sp_push_msg(2, "vn_block__split_at_next_ctrl_instr");
	detstackvar(2) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i9);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1025);
	incr_sp_push_msg(2, "vn_block__split_at_next_ctrl_instr");
	detstackvar(2) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i11);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i9);
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = ((Integer) 1);
	GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i6);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i11);
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = ((Integer) 0);
	GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i6);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1009);
	incr_sp_push_msg(2, "vn_block__split_at_next_ctrl_instr");
	detstackvar(2) = (Integer) succip;
	if ((tag((Integer) r4) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i27);
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = ((Integer) 0);
	GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i6);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i27);
	if ((tag((Integer) r4) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i28);
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = ((Integer) 1);
	GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i6);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i28);
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = ((Integer) 0);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i6);
	if ((((Integer) 0) == (Integer) r1))
		GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i29);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r1;
	GOTO_LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i4);
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i29);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i4);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) r2;
	localcall(mercury__vn_block__split_at_next_ctrl_instr_4_0,
		LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0_i30),
		ENTRY(mercury__vn_block__split_at_next_ctrl_instr_4_0));
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i30);
	update_prof_current_proc(LABEL(mercury__vn_block__split_at_next_ctrl_instr_4_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
	}
Define_label(mercury__vn_block__split_at_next_ctrl_instr_4_0_i1008);
	r1 = string_const("could not find next ctrl instr", 30);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__vn_block__split_at_next_ctrl_instr_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__vn_block_module3)
	init_entry(mercury__vn_block__divide_into_blocks_2_5_0);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i6);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i7);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i5);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i11);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i13);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i16);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i9);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i3);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i21);
	init_label(mercury__vn_block__divide_into_blocks_2_5_0_i25);
BEGIN_CODE

/* code for predicate 'vn_block__divide_into_blocks_2'/5 in mode 0 */
Define_static(mercury__vn_block__divide_into_blocks_2_5_0);
	incr_sp_push_msg(7, "vn_block__divide_into_blocks_2");
	detstackvar(7) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__divide_into_blocks_2_5_0_i3);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) tempr1;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(6) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__opt_util__can_instr_fall_through_2_0);
	call_localret(ENTRY(mercury__opt_util__can_instr_fall_through_2_0),
		mercury__vn_block__divide_into_blocks_2_5_0_i6,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
	}
	}
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__divide_into_blocks_2_5_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__vn_block__divide_into_blocks_2_5_0_i5);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_2);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__vn_block__divide_into_blocks_2_5_0_i7,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
	}
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_block__divide_into_blocks_2_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(3);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r5;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__vn_block__divide_into_blocks_2_5_0,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i5);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(6);
	if ((tag((Integer) tempr1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_block__divide_into_blocks_2_5_0_i9);
	if (((Integer) field(mktag(3), (Integer) tempr1, ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury__vn_block__divide_into_blocks_2_5_0_i9);
	r2 = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 1));
	detstackvar(6) = (Integer) r2;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__vn_block__divide_into_blocks_2_5_0_i11,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
	}
	}
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_block__divide_into_blocks_2_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__divide_into_blocks_2_5_0_i9);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	if (((Integer) detstackvar(1) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__divide_into_blocks_2_5_0_i13);
	r4 = (Integer) detstackvar(3);
	r1 = (Integer) detstackvar(5);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__vn_block__divide_into_blocks_2_5_0,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i13);
	r2 = (Integer) detstackvar(6);
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_2);
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	tag_incr_hp(r6, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r6, ((Integer) 0)) = ((Integer) 6);
	tag_incr_hp(r3, mktag(1), ((Integer) 1));
	field(mktag(3), (Integer) r6, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) r6;
	field(mktag(0), (Integer) r5, ((Integer) 1)) = string_const("", 0);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__vn_block__divide_into_blocks_2_5_0_i16,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
	}
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_block__divide_into_blocks_2_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(3);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r5;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__vn_block__divide_into_blocks_2_5_0,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i9);
	r4 = (Integer) detstackvar(3);
	r1 = (Integer) detstackvar(5);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__vn_block__divide_into_blocks_2_5_0,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i3);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__divide_into_blocks_2_5_0_i21);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_3);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury__list__reverse_2_0);
	tailcall(ENTRY(mercury__list__reverse_2_0),
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
	}
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i21);
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_2);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__vn_block__divide_into_blocks_2_5_0_i25,
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
	}
Define_label(mercury__vn_block__divide_into_blocks_2_5_0_i25);
	update_prof_current_proc(LABEL(mercury__vn_block__divide_into_blocks_2_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_3);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury__list__reverse_2_0);
	tailcall(ENTRY(mercury__list__reverse_2_0),
		STATIC(mercury__vn_block__divide_into_blocks_2_5_0));
	}
END_MODULE

BEGIN_MODULE(mercury__vn_block_module4)
	init_entry(mercury__vn_block__build_from_parallel_3_0);
	init_label(mercury__vn_block__build_from_parallel_3_0_i4);
	init_label(mercury__vn_block__build_from_parallel_3_0_i7);
	init_label(mercury__vn_block__build_from_parallel_3_0_i8);
	init_label(mercury__vn_block__build_from_parallel_3_0_i9);
	init_label(mercury__vn_block__build_from_parallel_3_0_i10);
	init_label(mercury__vn_block__build_from_parallel_3_0_i6);
	init_label(mercury__vn_block__build_from_parallel_3_0_i5);
	init_label(mercury__vn_block__build_from_parallel_3_0_i11);
	init_label(mercury__vn_block__build_from_parallel_3_0_i1003);
BEGIN_CODE

/* code for predicate 'vn_block__build_from_parallel'/3 in mode 0 */
Define_static(mercury__vn_block__build_from_parallel_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__build_from_parallel_3_0_i1003);
	incr_sp_push_msg(6, "vn_block__build_from_parallel");
	detstackvar(6) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	call_localret(STATIC(mercury__vn_block__real_fake_parentries_3_0),
		mercury__vn_block__build_from_parallel_3_0_i4,
		STATIC(mercury__vn_block__build_from_parallel_3_0));
Define_label(mercury__vn_block__build_from_parallel_3_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_parallel_3_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__build_from_parallel_3_0_i6);
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__vn_util__rval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__rval_to_vn_4_0),
		mercury__vn_block__build_from_parallel_3_0_i7,
		STATIC(mercury__vn_block__build_from_parallel_3_0));
	}
Define_label(mercury__vn_block__build_from_parallel_3_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_parallel_3_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__vn_util__lval_to_vnlval_4_0);
	call_localret(ENTRY(mercury__vn_util__lval_to_vnlval_4_0),
		mercury__vn_block__build_from_parallel_3_0_i8,
		STATIC(mercury__vn_block__build_from_parallel_3_0));
	}
Define_label(mercury__vn_block__build_from_parallel_3_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_parallel_3_0));
	detstackvar(5) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__vn_table__set_desired_value_4_0);
	call_localret(ENTRY(mercury__vn_table__set_desired_value_4_0),
		mercury__vn_block__build_from_parallel_3_0_i9,
		STATIC(mercury__vn_block__build_from_parallel_3_0));
	}
Define_label(mercury__vn_block__build_from_parallel_3_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_parallel_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__vn_table__set_current_value_4_0);
	call_localret(ENTRY(mercury__vn_table__set_current_value_4_0),
		mercury__vn_block__build_from_parallel_3_0_i10,
		STATIC(mercury__vn_block__build_from_parallel_3_0));
	}
Define_label(mercury__vn_block__build_from_parallel_3_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_parallel_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__vn_block__build_from_parallel_3_0_i5);
Define_label(mercury__vn_block__build_from_parallel_3_0_i6);
	r1 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
Define_label(mercury__vn_block__build_from_parallel_3_0_i5);
	detstackvar(3) = (Integer) r4;
	call_localret(STATIC(mercury__vn_block__build_from_fake_rval_4_0),
		mercury__vn_block__build_from_parallel_3_0_i11,
		STATIC(mercury__vn_block__build_from_parallel_3_0));
Define_label(mercury__vn_block__build_from_parallel_3_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_parallel_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__vn_block__build_from_parallel_3_0,
		STATIC(mercury__vn_block__build_from_parallel_3_0));
Define_label(mercury__vn_block__build_from_parallel_3_0_i1003);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module5)
	init_entry(mercury__vn_block__real_fake_parentries_3_0);
	init_label(mercury__vn_block__real_fake_parentries_3_0_i7);
	init_label(mercury__vn_block__real_fake_parentries_3_0_i8);
	init_label(mercury__vn_block__real_fake_parentries_3_0_i3);
	init_label(mercury__vn_block__real_fake_parentries_3_0_i6);
	init_label(mercury__vn_block__real_fake_parentries_3_0_i1);
BEGIN_CODE

/* code for predicate 'vn_block__real_fake_parentries'/3 in mode 0 */
Define_static(mercury__vn_block__real_fake_parentries_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__real_fake_parentries_3_0_i1);
	r4 = (Integer) sp;
Define_label(mercury__vn_block__real_fake_parentries_3_0_i7);
	while (1) {
	incr_sp_push_msg(1, "vn_block__real_fake_parentries");
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__vn_block__real_fake_parentries_3_0_i8);
	if ((tag((Integer) detstackvar(1)) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_block__real_fake_parentries_3_0_i3);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 0));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	GOTO_LABEL(mercury__vn_block__real_fake_parentries_3_0_i6);
Define_label(mercury__vn_block__real_fake_parentries_3_0_i3);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
Define_label(mercury__vn_block__real_fake_parentries_3_0_i6);
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r4))
		GOTO_LABEL(mercury__vn_block__real_fake_parentries_3_0_i8);
	proceed();
Define_label(mercury__vn_block__real_fake_parentries_3_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module6)
	init_entry(mercury__vn_block__build_from_fake_rval_4_0);
	init_label(mercury__vn_block__build_from_fake_rval_4_0_i6);
	init_label(mercury__vn_block__build_from_fake_rval_4_0_i8);
	init_label(mercury__vn_block__build_from_fake_rval_4_0_i9);
	init_label(mercury__vn_block__build_from_fake_rval_4_0_i5);
	init_label(mercury__vn_block__build_from_fake_rval_4_0_i1003);
BEGIN_CODE

/* code for predicate 'vn_block__build_from_fake_rval'/4 in mode 0 */
Define_static(mercury__vn_block__build_from_fake_rval_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__build_from_fake_rval_4_0_i1003);
	incr_sp_push_msg(5, "vn_block__build_from_fake_rval");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__vn_util__no_access_lval_to_vnlval_2_0);
	call_localret(ENTRY(mercury__vn_util__no_access_lval_to_vnlval_2_0),
		mercury__vn_block__build_from_fake_rval_4_0_i6,
		STATIC(mercury__vn_block__build_from_fake_rval_4_0));
	}
Define_label(mercury__vn_block__build_from_fake_rval_4_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_fake_rval_4_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__build_from_fake_rval_4_0_i5);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__vn_util__lval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__lval_to_vn_4_0),
		mercury__vn_block__build_from_fake_rval_4_0_i8,
		STATIC(mercury__vn_block__build_from_fake_rval_4_0));
	}
Define_label(mercury__vn_block__build_from_fake_rval_4_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_fake_rval_4_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__vn_table__set_parallel_value_4_0);
	call_localret(ENTRY(mercury__vn_table__set_parallel_value_4_0),
		mercury__vn_block__build_from_fake_rval_4_0_i9,
		STATIC(mercury__vn_block__build_from_fake_rval_4_0));
	}
Define_label(mercury__vn_block__build_from_fake_rval_4_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_block__build_from_fake_rval_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__vn_block__build_from_fake_rval_4_0,
		STATIC(mercury__vn_block__build_from_fake_rval_4_0));
Define_label(mercury__vn_block__build_from_fake_rval_4_0_i5);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__vn_block__build_from_fake_rval_4_0,
		STATIC(mercury__vn_block__build_from_fake_rval_4_0));
Define_label(mercury__vn_block__build_from_fake_rval_4_0_i1003);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module7)
	init_entry(mercury__vn_block__handle_instrs_11_0);
	init_label(mercury__vn_block__handle_instrs_11_0_i4);
	init_label(mercury__vn_block__handle_instrs_11_0_i1002);
BEGIN_CODE

/* code for predicate 'vn_block__handle_instrs'/11 in mode 0 */
Define_static(mercury__vn_block__handle_instrs_11_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__handle_instrs_11_0_i1002);
	incr_sp_push_msg(4, "vn_block__handle_instrs");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	call_localret(STATIC(mercury__vn_block__handle_instr_11_0),
		mercury__vn_block__handle_instrs_11_0_i4,
		STATIC(mercury__vn_block__handle_instrs_11_0));
Define_label(mercury__vn_block__handle_instrs_11_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instrs_11_0));
	r7 = (Integer) r4;
	r4 = (Integer) r1;
	r5 = (Integer) r2;
	r6 = (Integer) r3;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__vn_block__handle_instrs_11_0,
		STATIC(mercury__vn_block__handle_instrs_11_0));
Define_label(mercury__vn_block__handle_instrs_11_0_i1002);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	r3 = (Integer) r6;
	r4 = (Integer) r7;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module8)
	init_entry(mercury__vn_block__handle_instr_11_0);
	init_label(mercury__vn_block__handle_instr_11_0_i1018);
	init_label(mercury__vn_block__handle_instr_11_0_i1017);
	init_label(mercury__vn_block__handle_instr_11_0_i1016);
	init_label(mercury__vn_block__handle_instr_11_0_i1015);
	init_label(mercury__vn_block__handle_instr_11_0_i1014);
	init_label(mercury__vn_block__handle_instr_11_0_i1013);
	init_label(mercury__vn_block__handle_instr_11_0_i1012);
	init_label(mercury__vn_block__handle_instr_11_0_i1011);
	init_label(mercury__vn_block__handle_instr_11_0_i1010);
	init_label(mercury__vn_block__handle_instr_11_0_i1009);
	init_label(mercury__vn_block__handle_instr_11_0_i1008);
	init_label(mercury__vn_block__handle_instr_11_0_i1007);
	init_label(mercury__vn_block__handle_instr_11_0_i1006);
	init_label(mercury__vn_block__handle_instr_11_0_i1005);
	init_label(mercury__vn_block__handle_instr_11_0_i1004);
	init_label(mercury__vn_block__handle_instr_11_0_i1003);
	init_label(mercury__vn_block__handle_instr_11_0_i1002);
	init_label(mercury__vn_block__handle_instr_11_0_i1001);
	init_label(mercury__vn_block__handle_instr_11_0_i5);
	init_label(mercury__vn_block__handle_instr_11_0_i6);
	init_label(mercury__vn_block__handle_instr_11_0_i7);
	init_label(mercury__vn_block__handle_instr_11_0_i8);
	init_label(mercury__vn_block__handle_instr_11_0_i9);
	init_label(mercury__vn_block__handle_instr_11_0_i10);
	init_label(mercury__vn_block__handle_instr_11_0_i11);
	init_label(mercury__vn_block__handle_instr_11_0_i12);
	init_label(mercury__vn_block__handle_instr_11_0_i14);
	init_label(mercury__vn_block__handle_instr_11_0_i15);
	init_label(mercury__vn_block__handle_instr_11_0_i16);
	init_label(mercury__vn_block__handle_instr_11_0_i18);
	init_label(mercury__vn_block__handle_instr_11_0_i20);
	init_label(mercury__vn_block__handle_instr_11_0_i22);
	init_label(mercury__vn_block__handle_instr_11_0_i24);
	init_label(mercury__vn_block__handle_instr_11_0_i25);
	init_label(mercury__vn_block__handle_instr_11_0_i27);
	init_label(mercury__vn_block__handle_instr_11_0_i29);
	init_label(mercury__vn_block__handle_instr_11_0_i30);
	init_label(mercury__vn_block__handle_instr_11_0_i32);
	init_label(mercury__vn_block__handle_instr_11_0_i34);
	init_label(mercury__vn_block__handle_instr_11_0_i33);
	init_label(mercury__vn_block__handle_instr_11_0_i35);
	init_label(mercury__vn_block__handle_instr_11_0_i36);
	init_label(mercury__vn_block__handle_instr_11_0_i38);
	init_label(mercury__vn_block__handle_instr_11_0_i39);
	init_label(mercury__vn_block__handle_instr_11_0_i41);
	init_label(mercury__vn_block__handle_instr_11_0_i42);
	init_label(mercury__vn_block__handle_instr_11_0_i44);
	init_label(mercury__vn_block__handle_instr_11_0_i45);
	init_label(mercury__vn_block__handle_instr_11_0_i47);
	init_label(mercury__vn_block__handle_instr_11_0_i48);
	init_label(mercury__vn_block__handle_instr_11_0_i50);
	init_label(mercury__vn_block__handle_instr_11_0_i52);
	init_label(mercury__vn_block__handle_instr_11_0_i54);
	init_label(mercury__vn_block__handle_instr_11_0_i1000);
	init_label(mercury__vn_block__handle_instr_11_0_i56);
	init_label(mercury__vn_block__handle_instr_11_0_i58);
	init_label(mercury__vn_block__handle_instr_11_0_i59);
BEGIN_CODE

/* code for predicate 'vn_block__handle_instr'/11 in mode 0 */
Define_static(mercury__vn_block__handle_instr_11_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i1000);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__vn_block__handle_instr_11_0_i1018) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1017) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1016) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1015) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1014) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1013) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1012) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1011) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1010) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1009) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1008) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1007) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1006) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1005) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1004) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1003) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1002) AND
		LABEL(mercury__vn_block__handle_instr_11_0_i1001));
Define_label(mercury__vn_block__handle_instr_11_0_i1018);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i5);
Define_label(mercury__vn_block__handle_instr_11_0_i1017);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i7);
Define_label(mercury__vn_block__handle_instr_11_0_i1016);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	tag_incr_hp(r1, mktag(2), ((Integer) 4));
	field(mktag(2), (Integer) r1, ((Integer) 3)) = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 4));
	field(mktag(2), (Integer) r1, ((Integer) 2)) = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 3));
	field(mktag(2), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 1));
	field(mktag(2), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 2));
	detstackvar(3) = (Integer) r6;
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i1015);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i15);
Define_label(mercury__vn_block__handle_instr_11_0_i1014);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i18);
Define_label(mercury__vn_block__handle_instr_11_0_i1013);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i20);
Define_label(mercury__vn_block__handle_instr_11_0_i1012);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i22);
Define_label(mercury__vn_block__handle_instr_11_0_i1011);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i24);
Define_label(mercury__vn_block__handle_instr_11_0_i1010);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i27);
Define_label(mercury__vn_block__handle_instr_11_0_i1009);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i29);
Define_label(mercury__vn_block__handle_instr_11_0_i1008);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i32);
Define_label(mercury__vn_block__handle_instr_11_0_i1007);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i38);
Define_label(mercury__vn_block__handle_instr_11_0_i1006);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i41);
Define_label(mercury__vn_block__handle_instr_11_0_i1005);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i44);
Define_label(mercury__vn_block__handle_instr_11_0_i1004);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i47);
Define_label(mercury__vn_block__handle_instr_11_0_i1003);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i50);
Define_label(mercury__vn_block__handle_instr_11_0_i1002);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i52);
Define_label(mercury__vn_block__handle_instr_11_0_i1001);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i54);
Define_label(mercury__vn_block__handle_instr_11_0_i5);
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	r1 = string_const("block should not be found in vn_block__handle_instr", 51);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_block__handle_instr_11_0_i6,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__vn_block__handle_instr_11_0_i7);
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__vn_util__rval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__rval_to_vn_4_0),
		mercury__vn_block__handle_instr_11_0_i8,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__vn_util__lval_to_vnlval_4_0);
	call_localret(ENTRY(mercury__vn_util__lval_to_vnlval_4_0),
		mercury__vn_block__handle_instr_11_0_i9,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__vn_table__set_desired_value_4_0);
	call_localret(ENTRY(mercury__vn_table__set_desired_value_4_0),
		mercury__vn_block__handle_instr_11_0_i10,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_util__find_specials_2_0);
	call_localret(ENTRY(mercury__vn_util__find_specials_2_0),
		mercury__vn_block__handle_instr_11_0_i11,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__insert_list_3_0);
	call_localret(ENTRY(mercury__set__insert_list_3_0),
		mercury__vn_block__handle_instr_11_0_i12,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__vn_block__handle_instr_11_0_i14);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r4 = (Integer) r3;
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__vn_block__handle_instr_11_0_i15);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r6;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r8 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 1));
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(4);
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i16,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(3), (Integer) mercury_data_vn_block__common_5);
	tag_incr_hp(r5, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r5, ((Integer) 0)) = ((Integer) 1);
	r6 = (Integer) detstackvar(3);
	r7 = (Integer) r3;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r5;
	field(mktag(3), (Integer) r5, ((Integer) 1)) = (Integer) tempr1;
	r5 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	decr_sp_pop_msg(8);
	localtailcall(mercury__vn_block__handle_instr_11_0,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i18);
	r8 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(3), (Integer) mercury_data_vn_block__common_7);
	tag_incr_hp(r9, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r9, ((Integer) 0)) = ((Integer) 1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r9, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 1));
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r9;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__vn_block__handle_instr_11_0,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i20);
	detstackvar(3) = (Integer) r6;
	r8 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 1));
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i22);
	detstackvar(3) = (Integer) r6;
	r8 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 1));
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i24);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__vn_util__rval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__rval_to_vn_4_0),
		mercury__vn_block__handle_instr_11_0_i25,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i25);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	r4 = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i27);
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	r1 = string_const("c_code should not be found in handle_instr", 42);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_block__handle_instr_11_0_i6,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i29);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__vn_util__rval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__rval_to_vn_4_0),
		mercury__vn_block__handle_instr_11_0_i30,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i30);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	r4 = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 4);
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i32);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 2)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i34);
	r6 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r11 = (Integer) r1;
	r1 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	r5 = (Integer) r7;
	tag_incr_hp(r7, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r7, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r7, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r11, ((Integer) 1));
	{
	Word tempr1, tempr2;
	tag_incr_hp(tempr2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr2, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) field(mktag(3), (Integer) r11, ((Integer) 2)), ((Integer) 0));
	field(mktag(3), (Integer) tempr2, ((Integer) 2)) = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_8);
	field(mktag(3), (Integer) tempr2, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r7, ((Integer) 2)) = (Integer) tempr2;
	GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i33);
	}
Define_label(mercury__vn_block__handle_instr_11_0_i34);
	r6 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r11 = (Integer) r1;
	r1 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	r5 = (Integer) r7;
	tag_incr_hp(r7, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r7, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r7, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r11, ((Integer) 1));
	field(mktag(3), (Integer) r7, ((Integer) 2)) = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_8);
Define_label(mercury__vn_block__handle_instr_11_0_i33);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	{
	Declare_entry(mercury__vn_type__bytes_per_word_2_0);
	call_localret(ENTRY(mercury__vn_type__bytes_per_word_2_0),
		mercury__vn_block__handle_instr_11_0_i35,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i35);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r2 = (Integer) detstackvar(3);
	tag_incr_hp(r3, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 3)));
	detstackvar(3) = (Integer) r3;
	tag_incr_hp(r3, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = ((Integer) 0);
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_8);
	tag_incr_hp(r4, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r4, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r4, ((Integer) 1)) = ((Integer) 2);
	field(mktag(3), (Integer) r4, ((Integer) 2)) = (Integer) detstackvar(6);
	tag_incr_hp(r5, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r5, ((Integer) 0)) = ((Integer) 1);
	r6 = ((Integer) 0);
	r7 = (Integer) detstackvar(5);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	field(mktag(3), (Integer) detstackvar(3), ((Integer) 2)) = (Integer) r3;
	field(mktag(3), (Integer) r3, ((Integer) 3)) = (Integer) r4;
	field(mktag(3), (Integer) r4, ((Integer) 3)) = (Integer) r5;
	field(mktag(3), (Integer) r5, ((Integer) 1)) = (Integer) tempr1;
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	localcall(mercury__vn_block__handle_instr_11_0,
		LABEL(mercury__vn_block__handle_instr_11_0_i36),
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i36);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r7 = (Integer) r4;
	r4 = (Integer) r1;
	r5 = (Integer) r2;
	r6 = (Integer) r3;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__vn_block__handle_instr_11_0,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i38);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__vn_util__lval_to_vnlval_4_0);
	call_localret(ENTRY(mercury__vn_util__lval_to_vnlval_4_0),
		mercury__vn_block__handle_instr_11_0_i39,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i39);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	r4 = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i41);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__vn_util__rval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__rval_to_vn_4_0),
		mercury__vn_block__handle_instr_11_0_i42,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i42);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	r4 = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 6);
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i44);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__vn_util__lval_to_vnlval_4_0);
	call_localret(ENTRY(mercury__vn_util__lval_to_vnlval_4_0),
		mercury__vn_block__handle_instr_11_0_i45,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i45);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	r4 = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 7);
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i47);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__vn_util__rval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__rval_to_vn_4_0),
		mercury__vn_block__handle_instr_11_0_i48,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i48);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	r4 = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 8);
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i50);
	detstackvar(3) = (Integer) r6;
	r8 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 9);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 1));
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 2));
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i52);
	detstackvar(3) = (Integer) r6;
	r8 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 10);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 1));
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i54);
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	r1 = string_const("value numbering not supported for pragma_c", 42);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_block__handle_instr_11_0_i6,
		STATIC(mercury__vn_block__handle_instr_11_0));
	}
Define_label(mercury__vn_block__handle_instr_11_0_i1000);
	incr_sp_push_msg(8, "vn_block__handle_instr");
	detstackvar(8) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i56);
	detstackvar(3) = (Integer) r6;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i14,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i56);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_block__handle_instr_11_0_i58);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	r3 = (Integer) r6;
	r4 = (Integer) r7;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__vn_block__handle_instr_11_0_i58);
	detstackvar(3) = (Integer) r6;
	r8 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(2), (Integer) r8, ((Integer) 0));
	r6 = (Integer) r7;
	call_localret(STATIC(mercury__vn_block__new_ctrl_node_9_0),
		mercury__vn_block__handle_instr_11_0_i59,
		STATIC(mercury__vn_block__handle_instr_11_0));
Define_label(mercury__vn_block__handle_instr_11_0_i59);
	update_prof_current_proc(LABEL(mercury__vn_block__handle_instr_11_0));
	r4 = (Integer) r3;
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module9)
	init_entry(mercury__vn_block__new_ctrl_node_9_0);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i2);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i6);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i8);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i12);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i14);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i10);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i9);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i15);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i17);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i19);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i21);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i22);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i23);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i24);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i26);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i29);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i28);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i31);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i32);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i33);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i5);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i38);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i40);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i41);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i39);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i42);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i3);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i43);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i44);
	init_label(mercury__vn_block__new_ctrl_node_9_0_i45);
BEGIN_CODE

/* code for predicate 'vn_block__new_ctrl_node'/9 in mode 0 */
Define_static(mercury__vn_block__new_ctrl_node_9_0);
	incr_sp_push_msg(17, "vn_block__new_ctrl_node");
	detstackvar(17) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r6, ((Integer) 0));
	detstackvar(7) = (Integer) field(mktag(0), (Integer) r6, ((Integer) 1));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) r6, ((Integer) 2));
	detstackvar(9) = (Integer) field(mktag(0), (Integer) r6, ((Integer) 3));
	detstackvar(10) = (Integer) field(mktag(0), (Integer) r6, ((Integer) 4));
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__vn_block__new_ctrl_node_9_0_i2,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r2 = (Integer) detstackvar(1);
	r3 = tag((Integer) r2);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i5);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)),
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i6) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i6) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i8) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i17) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i19) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i21) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i6) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i26) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i6) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i6) AND
		LABEL(mercury__vn_block__new_ctrl_node_9_0_i6));
Define_label(mercury__vn_block__new_ctrl_node_9_0_i6);
	r5 = (Integer) r2;
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(10);
	r10 = ((Integer) r4 + ((Integer) 1));
	r11 = (Integer) detstackvar(9);
	r12 = (Integer) r1;
	r13 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i3);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i8);
	r3 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i9);
	detstackvar(15) = (Integer) r1;
	detstackvar(11) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_9);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(11);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__vn_block__new_ctrl_node_9_0_i12,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i10);
	r1 = (Integer) detstackvar(11);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(15);
	r7 = (Integer) detstackvar(9);
	call_localret(STATIC(mercury__vn_block__record_one_label_12_0),
		mercury__vn_block__new_ctrl_node_9_0_i14,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
Define_label(mercury__vn_block__new_ctrl_node_9_0_i14);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r6 = (Integer) r1;
	r7 = (Integer) r2;
	r11 = (Integer) r4;
	r12 = (Integer) r3;
	r13 = (Integer) r5;
	r4 = (Integer) detstackvar(6);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	r3 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(1);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(10);
	r10 = ((Integer) r4 + ((Integer) 1));
	GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i3);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i10);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(15);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i9);
	detstackvar(1) = (Integer) r2;
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__vn_block__record_at_call_6_0),
		mercury__vn_block__new_ctrl_node_9_0_i15,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
Define_label(mercury__vn_block__new_ctrl_node_9_0_i15);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r6 = (Integer) r1;
	r7 = (Integer) r2;
	r12 = (Integer) r3;
	r4 = (Integer) detstackvar(6);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	r3 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(1);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(10);
	r10 = ((Integer) r4 + ((Integer) 1));
	r11 = (Integer) detstackvar(9);
	r13 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i3);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i17);
	r6 = (Integer) r1;
	r1 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 2));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(9);
	call_localret(STATIC(mercury__vn_block__record_several_labels_12_0),
		mercury__vn_block__new_ctrl_node_9_0_i14,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
Define_label(mercury__vn_block__new_ctrl_node_9_0_i19);
	r8 = (Integer) r1;
	r1 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 2));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(6);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r9 = (Integer) detstackvar(9);
	call_localret(STATIC(mercury__vn_block__new_if_node_14_0),
		mercury__vn_block__new_ctrl_node_9_0_i14,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
Define_label(mercury__vn_block__new_ctrl_node_9_0_i21);
	detstackvar(15) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_8);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__vn_util__rval_to_vn_4_0);
	call_localret(ENTRY(mercury__vn_util__rval_to_vn_4_0),
		mercury__vn_block__new_ctrl_node_9_0_i22,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i22);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__vn_table__set_desired_value_4_0);
	call_localret(ENTRY(mercury__vn_table__set_desired_value_4_0),
		mercury__vn_block__new_ctrl_node_9_0_i23,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i23);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_block__new_ctrl_node_9_0_i24,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i24);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) r1;
	r4 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(10);
	r10 = ((Integer) r4 + ((Integer) 1));
	r11 = (Integer) detstackvar(9);
	r12 = (Integer) detstackvar(15);
	r13 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i3);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i26);
	detstackvar(15) = (Integer) r1;
	r1 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	detstackvar(16) = (Integer) r1;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__vn_table__search_desired_value_3_0);
	call_localret(ENTRY(mercury__vn_table__search_desired_value_3_0),
		mercury__vn_block__new_ctrl_node_9_0_i29,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i29);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i28);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = (Integer) detstackvar(7);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(9);
	r10 = (Integer) detstackvar(10);
	r11 = (Integer) detstackvar(15);
	r1 = (Integer) detstackvar(16);
	r3 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i32);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i28);
	r1 = (Integer) detstackvar(16);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__vn_table__record_first_vnlval_4_0);
	call_localret(ENTRY(mercury__vn_table__record_first_vnlval_4_0),
		mercury__vn_block__new_ctrl_node_9_0_i31,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i31);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(16);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = (Integer) detstackvar(7);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(9);
	r10 = (Integer) detstackvar(10);
	r11 = (Integer) detstackvar(15);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i32);
	detstackvar(1) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	detstackvar(8) = (Integer) r8;
	detstackvar(9) = (Integer) r9;
	detstackvar(10) = (Integer) r10;
	detstackvar(15) = (Integer) r11;
	detstackvar(16) = (Integer) r1;
	{
	Declare_entry(mercury__vn_table__set_desired_value_4_0);
	call_localret(ENTRY(mercury__vn_table__set_desired_value_4_0),
		mercury__vn_block__new_ctrl_node_9_0_i33,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i33);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(16);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_block__new_ctrl_node_9_0_i24,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i5);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i38);
	r5 = (Integer) r2;
	r12 = (Integer) r1;
	r4 = (Integer) detstackvar(6);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	r3 = (Integer) detstackvar(7);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(10);
	r10 = ((Integer) r4 + ((Integer) 1));
	r11 = (Integer) detstackvar(9);
	r13 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i3);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i38);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i39);
	detstackvar(15) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__vn_block__new_ctrl_node_9_0_i40,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i40);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__vn_util__convert_to_vnlval_and_insert_3_0);
	call_localret(ENTRY(mercury__vn_util__convert_to_vnlval_and_insert_3_0),
		mercury__vn_block__new_ctrl_node_9_0_i41,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i41);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) r1;
	r4 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(10);
	r10 = ((Integer) r4 + ((Integer) 1));
	r11 = (Integer) detstackvar(9);
	r12 = (Integer) detstackvar(15);
	r13 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	GOTO_LABEL(mercury__vn_block__new_ctrl_node_9_0_i3);
Define_label(mercury__vn_block__new_ctrl_node_9_0_i39);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__vn_block__record_at_call_6_0),
		mercury__vn_block__new_ctrl_node_9_0_i42,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
Define_label(mercury__vn_block__new_ctrl_node_9_0_i42);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r6 = (Integer) r1;
	r7 = (Integer) r2;
	r12 = (Integer) r3;
	r4 = (Integer) detstackvar(6);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	r3 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(1);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(10);
	r10 = ((Integer) r4 + ((Integer) 1));
	r11 = (Integer) detstackvar(9);
	r13 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
Define_label(mercury__vn_block__new_ctrl_node_9_0_i3);
	detstackvar(2) = (Integer) r6;
	detstackvar(3) = (Integer) r7;
	detstackvar(6) = (Integer) r4;
	detstackvar(8) = (Integer) r8;
	detstackvar(10) = (Integer) r9;
	detstackvar(11) = (Integer) r10;
	detstackvar(14) = (Integer) r11;
	detstackvar(12) = (Integer) r12;
	detstackvar(13) = (Integer) r13;
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__vn_block__new_ctrl_node_9_0_i43,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i43);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r5 = (Integer) detstackvar(12);
	detstackvar(12) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_0);
	r3 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__vn_block__new_ctrl_node_9_0_i44,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i44);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	r5 = (Integer) detstackvar(13);
	detstackvar(13) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_1);
	r3 = (Integer) detstackvar(10);
	r4 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__vn_block__new_ctrl_node_9_0_i45,
		STATIC(mercury__vn_block__new_ctrl_node_9_0));
	}
Define_label(mercury__vn_block__new_ctrl_node_9_0_i45);
	update_prof_current_proc(LABEL(mercury__vn_block__new_ctrl_node_9_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r3, ((Integer) 4)) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(14);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(13);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(12);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(11);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(17);
	decr_sp_pop_msg(17);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module10)
	init_entry(mercury__vn_block__new_if_node_14_0);
	init_label(mercury__vn_block__new_if_node_14_0_i5);
	init_label(mercury__vn_block__new_if_node_14_0_i7);
	init_label(mercury__vn_block__new_if_node_14_0_i8);
	init_label(mercury__vn_block__new_if_node_14_0_i9);
	init_label(mercury__vn_block__new_if_node_14_0_i10);
	init_label(mercury__vn_block__new_if_node_14_0_i11);
	init_label(mercury__vn_block__new_if_node_14_0_i14);
	init_label(mercury__vn_block__new_if_node_14_0_i17);
	init_label(mercury__vn_block__new_if_node_14_0_i18);
	init_label(mercury__vn_block__new_if_node_14_0_i13);
	init_label(mercury__vn_block__new_if_node_14_0_i3);
	init_label(mercury__vn_block__new_if_node_14_0_i2);
	init_label(mercury__vn_block__new_if_node_14_0_i20);
	init_label(mercury__vn_block__new_if_node_14_0_i26);
	init_label(mercury__vn_block__new_if_node_14_0_i27);
	init_label(mercury__vn_block__new_if_node_14_0_i23);
	init_label(mercury__vn_block__new_if_node_14_0_i28);
	init_label(mercury__vn_block__new_if_node_14_0_i29);
BEGIN_CODE

/* code for predicate 'vn_block__new_if_node'/14 in mode 0 */
Define_static(mercury__vn_block__new_if_node_14_0);
	incr_sp_push_msg(13, "vn_block__new_if_node");
	detstackvar(13) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_block__new_if_node_14_0_i2);
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(10) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_9);
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	detstackvar(8) = (Integer) r8;
	detstackvar(9) = (Integer) r9;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__vn_block__new_if_node_14_0_i5,
		STATIC(mercury__vn_block__new_if_node_14_0));
	}
Define_label(mercury__vn_block__new_if_node_14_0_i5);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__new_if_node_14_0_i3);
	r1 = (Integer) detstackvar(10);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(7);
	r6 = (Integer) detstackvar(8);
	r7 = (Integer) detstackvar(9);
	call_localret(STATIC(mercury__vn_block__record_label_12_0),
		mercury__vn_block__new_if_node_14_0_i7,
		STATIC(mercury__vn_block__new_if_node_14_0));
Define_label(mercury__vn_block__new_if_node_14_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(11) = (Integer) r4;
	detstackvar(12) = (Integer) r5;
	detstackvar(3) = (Integer) r2;
	detstackvar(10) = (Integer) r3;
	{
	Declare_entry(mercury__vn_table__get_vnlval_vn_list_2_0);
	call_localret(ENTRY(mercury__vn_table__get_vnlval_vn_list_2_0),
		mercury__vn_block__new_if_node_14_0_i8,
		STATIC(mercury__vn_block__new_if_node_14_0));
	}
Define_label(mercury__vn_block__new_if_node_14_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(10);
	call_localret(STATIC(mercury__vn_block__record_compulsory_lval_list_5_0),
		mercury__vn_block__new_if_node_14_0_i9,
		STATIC(mercury__vn_block__new_if_node_14_0));
Define_label(mercury__vn_block__new_if_node_14_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	detstackvar(3) = (Integer) r1;
	detstackvar(10) = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__vn_table__get_vnlval_vn_list_2_0);
	call_localret(ENTRY(mercury__vn_table__get_vnlval_vn_list_2_0),
		mercury__vn_block__new_if_node_14_0_i10,
		STATIC(mercury__vn_block__new_if_node_14_0));
	}
Define_label(mercury__vn_block__new_if_node_14_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(10);
	call_localret(STATIC(mercury__vn_block__record_compulsory_lval_list_5_0),
		mercury__vn_block__new_if_node_14_0_i11,
		STATIC(mercury__vn_block__new_if_node_14_0));
Define_label(mercury__vn_block__new_if_node_14_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	detstackvar(3) = (Integer) r1;
	r3 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_vn_type__base_type_info_vn_instr_0[];
	r2 = (Integer) mercury_data_vn_type__base_type_info_vn_instr_0;
	}
	r4 = ((Integer) detstackvar(5) - ((Integer) 1));
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__vn_block__new_if_node_14_0_i14,
		STATIC(mercury__vn_block__new_if_node_14_0));
	}
Define_label(mercury__vn_block__new_if_node_14_0_i14);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__new_if_node_14_0_i13);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_block__new_if_node_14_0_i13);
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__vn_block__new_if_node_14_0_i17,
		STATIC(mercury__vn_block__new_if_node_14_0));
	}
Define_label(mercury__vn_block__new_if_node_14_0_i17);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__vn_block__record_livevnlvals_7_0),
		mercury__vn_block__new_if_node_14_0_i18,
		STATIC(mercury__vn_block__new_if_node_14_0));
Define_label(mercury__vn_block__new_if_node_14_0_i18);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r4 = (Integer) detstackvar(9);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__vn_block__new_if_node_14_0_i13);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(11);
	r5 = (Integer) detstackvar(12);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__vn_block__new_if_node_14_0_i3);
	r1 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(6);
	r7 = (Integer) detstackvar(7);
	r8 = (Integer) detstackvar(8);
	r9 = (Integer) detstackvar(9);
Define_label(mercury__vn_block__new_if_node_14_0_i2);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury__vn_block__new_if_node_14_0_i20);
	r1 = (Integer) r6;
	r2 = (Integer) r7;
	r3 = (Integer) r8;
	r4 = (Integer) r9;
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__vn_block__new_if_node_14_0_i20);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury__vn_block__new_if_node_14_0_i23);
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	detstackvar(8) = (Integer) r8;
	detstackvar(9) = (Integer) r9;
	r1 = (Integer) r6;
	{
	Declare_entry(mercury__vn_table__get_vnlval_vn_list_2_0);
	call_localret(ENTRY(mercury__vn_table__get_vnlval_vn_list_2_0),
		mercury__vn_block__new_if_node_14_0_i26,
		STATIC(mercury__vn_block__new_if_node_14_0));
	}
Define_label(mercury__vn_block__new_if_node_14_0_i26);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__vn_block__record_compulsory_lval_list_5_0),
		mercury__vn_block__new_if_node_14_0_i27,
		STATIC(mercury__vn_block__new_if_node_14_0));
Define_label(mercury__vn_block__new_if_node_14_0_i27);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(9);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__vn_block__new_if_node_14_0_i23);
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	detstackvar(8) = (Integer) r8;
	detstackvar(9) = (Integer) r9;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) r7;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__vn_block__new_if_node_14_0_i28,
		STATIC(mercury__vn_block__new_if_node_14_0));
	}
Define_label(mercury__vn_block__new_if_node_14_0_i28);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	r4 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__vn_block__record_livevnlvals_7_0),
		mercury__vn_block__new_if_node_14_0_i29,
		STATIC(mercury__vn_block__new_if_node_14_0));
Define_label(mercury__vn_block__new_if_node_14_0_i29);
	update_prof_current_proc(LABEL(mercury__vn_block__new_if_node_14_0));
	r4 = (Integer) detstackvar(9);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module11)
	init_entry(mercury__vn_block__record_at_call_6_0);
	init_label(mercury__vn_block__record_at_call_6_0_i2);
	init_label(mercury__vn_block__record_at_call_6_0_i3);
	init_label(mercury__vn_block__record_at_call_6_0_i4);
	init_label(mercury__vn_block__record_at_call_6_0_i5);
BEGIN_CODE

/* code for predicate 'vn_block__record_at_call'/6 in mode 0 */
Define_static(mercury__vn_block__record_at_call_6_0);
	incr_sp_push_msg(4, "vn_block__record_at_call");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__vn_block__record_at_call_6_0_i2,
		STATIC(mercury__vn_block__record_at_call_6_0));
	}
Define_label(mercury__vn_block__record_at_call_6_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_block__record_at_call_6_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__vn_block__record_livevnlvals_7_0),
		mercury__vn_block__record_at_call_6_0_i3,
		STATIC(mercury__vn_block__record_at_call_6_0));
Define_label(mercury__vn_block__record_at_call_6_0_i3);
	update_prof_current_proc(LABEL(mercury__vn_block__record_at_call_6_0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__vn_table__get_vnlval_vn_list_2_0);
	call_localret(ENTRY(mercury__vn_table__get_vnlval_vn_list_2_0),
		mercury__vn_block__record_at_call_6_0_i4,
		STATIC(mercury__vn_block__record_at_call_6_0));
	}
Define_label(mercury__vn_block__record_at_call_6_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__record_at_call_6_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__vn_block__record_compulsory_lval_list_5_0),
		mercury__vn_block__record_at_call_6_0_i5,
		STATIC(mercury__vn_block__record_at_call_6_0));
Define_label(mercury__vn_block__record_at_call_6_0_i5);
	update_prof_current_proc(LABEL(mercury__vn_block__record_at_call_6_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module12)
	init_entry(mercury__vn_block__record_several_labels_12_0);
	init_label(mercury__vn_block__record_several_labels_12_0_i2);
	init_label(mercury__vn_block__record_several_labels_12_0_i3);
	init_label(mercury__vn_block__record_several_labels_12_0_i4);
BEGIN_CODE

/* code for predicate 'vn_block__record_several_labels'/12 in mode 0 */
Define_static(mercury__vn_block__record_several_labels_12_0);
	incr_sp_push_msg(6, "vn_block__record_several_labels");
	detstackvar(6) = (Integer) succip;
	call_localret(STATIC(mercury__vn_block__record_labels_12_0),
		mercury__vn_block__record_several_labels_12_0_i2,
		STATIC(mercury__vn_block__record_several_labels_12_0));
Define_label(mercury__vn_block__record_several_labels_12_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_block__record_several_labels_12_0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r2;
	detstackvar(5) = (Integer) r3;
	{
	Declare_entry(mercury__vn_table__get_vnlval_vn_list_2_0);
	call_localret(ENTRY(mercury__vn_table__get_vnlval_vn_list_2_0),
		mercury__vn_block__record_several_labels_12_0_i3,
		STATIC(mercury__vn_block__record_several_labels_12_0));
	}
Define_label(mercury__vn_block__record_several_labels_12_0_i3);
	update_prof_current_proc(LABEL(mercury__vn_block__record_several_labels_12_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__vn_block__record_compulsory_lval_list_5_0),
		mercury__vn_block__record_several_labels_12_0_i4,
		STATIC(mercury__vn_block__record_several_labels_12_0));
Define_label(mercury__vn_block__record_several_labels_12_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__record_several_labels_12_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module13)
	init_entry(mercury__vn_block__record_one_label_12_0);
	init_label(mercury__vn_block__record_one_label_12_0_i2);
	init_label(mercury__vn_block__record_one_label_12_0_i3);
	init_label(mercury__vn_block__record_one_label_12_0_i4);
BEGIN_CODE

/* code for predicate 'vn_block__record_one_label'/12 in mode 0 */
Define_static(mercury__vn_block__record_one_label_12_0);
	incr_sp_push_msg(6, "vn_block__record_one_label");
	detstackvar(6) = (Integer) succip;
	call_localret(STATIC(mercury__vn_block__record_label_12_0),
		mercury__vn_block__record_one_label_12_0_i2,
		STATIC(mercury__vn_block__record_one_label_12_0));
Define_label(mercury__vn_block__record_one_label_12_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_block__record_one_label_12_0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r2;
	detstackvar(5) = (Integer) r3;
	{
	Declare_entry(mercury__vn_table__get_vnlval_vn_list_2_0);
	call_localret(ENTRY(mercury__vn_table__get_vnlval_vn_list_2_0),
		mercury__vn_block__record_one_label_12_0_i3,
		STATIC(mercury__vn_block__record_one_label_12_0));
	}
Define_label(mercury__vn_block__record_one_label_12_0_i3);
	update_prof_current_proc(LABEL(mercury__vn_block__record_one_label_12_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__vn_block__record_compulsory_lval_list_5_0),
		mercury__vn_block__record_one_label_12_0_i4,
		STATIC(mercury__vn_block__record_one_label_12_0));
Define_label(mercury__vn_block__record_one_label_12_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__record_one_label_12_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module14)
	init_entry(mercury__vn_block__record_labels_12_0);
	init_label(mercury__vn_block__record_labels_12_0_i4);
	init_label(mercury__vn_block__record_labels_12_0_i5);
	init_label(mercury__vn_block__record_labels_12_0_i6);
	init_label(mercury__vn_block__record_labels_12_0_i1002);
BEGIN_CODE

/* code for predicate 'vn_block__record_labels'/12 in mode 0 */
Define_static(mercury__vn_block__record_labels_12_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__record_labels_12_0_i1002);
	incr_sp_push_msg(5, "vn_block__record_labels");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__vn_block__record_label_12_0),
		mercury__vn_block__record_labels_12_0_i4,
		STATIC(mercury__vn_block__record_labels_12_0));
Define_label(mercury__vn_block__record_labels_12_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__record_labels_12_0));
	r7 = (Integer) r4;
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r5;
	r5 = (Integer) tempr1;
	r4 = (Integer) r1;
	r6 = (Integer) r3;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__vn_block__record_labels_12_0,
		LABEL(mercury__vn_block__record_labels_12_0_i5),
		STATIC(mercury__vn_block__record_labels_12_0));
	}
Define_label(mercury__vn_block__record_labels_12_0_i5);
	update_prof_current_proc(LABEL(mercury__vn_block__record_labels_12_0));
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mercury_data_vn_type__base_type_info_parallel_0;
	r3 = (Integer) r5;
	detstackvar(4) = (Integer) r4;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__vn_block__record_labels_12_0_i6,
		STATIC(mercury__vn_block__record_labels_12_0));
	}
Define_label(mercury__vn_block__record_labels_12_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__record_labels_12_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__vn_block__record_labels_12_0_i1002);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	r3 = (Integer) r6;
	r4 = (Integer) r7;
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module15)
	init_entry(mercury__vn_block__record_label_12_0);
	init_label(mercury__vn_block__record_label_12_0_i4);
	init_label(mercury__vn_block__record_label_12_0_i6);
	init_label(mercury__vn_block__record_label_12_0_i7);
	init_label(mercury__vn_block__record_label_12_0_i8);
	init_label(mercury__vn_block__record_label_12_0_i11);
	init_label(mercury__vn_block__record_label_12_0_i3);
	init_label(mercury__vn_block__record_label_12_0_i16);
	init_label(mercury__vn_block__record_label_12_0_i17);
BEGIN_CODE

/* code for predicate 'vn_block__record_label'/12 in mode 0 */
Define_static(mercury__vn_block__record_label_12_0);
	incr_sp_push_msg(7, "vn_block__record_label");
	detstackvar(7) = (Integer) succip;
	detstackvar(3) = (Integer) r4;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r2;
	r4 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_9);
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__vn_block__record_label_12_0_i4,
		STATIC(mercury__vn_block__record_label_12_0));
	}
Define_label(mercury__vn_block__record_label_12_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__record_label_12_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__record_label_12_0_i3);
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__vn_block__record_label_12_0_i6,
		STATIC(mercury__vn_block__record_label_12_0));
	}
Define_label(mercury__vn_block__record_label_12_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__record_label_12_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__vn_block__record_livevals_9_0),
		mercury__vn_block__record_label_12_0_i7,
		STATIC(mercury__vn_block__record_label_12_0));
Define_label(mercury__vn_block__record_label_12_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_block__record_label_12_0));
	if (((Integer) r4 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__record_label_12_0_i8);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__vn_block__record_label_12_0_i8);
	if ((tag((Integer) detstackvar(1)) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_block__record_label_12_0_i11);
	r6 = (Integer) r4;
	r7 = ((Integer) detstackvar(6) + ((Integer) 1));
	r4 = (Integer) r7;
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	tag_incr_hp(r8, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r8, ((Integer) 0)) = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r8, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 0));
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r8;
	field(mktag(0), (Integer) r8, ((Integer) 2)) = (Integer) r6;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r7;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
	}
Define_label(mercury__vn_block__record_label_12_0_i11);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__vn_block__record_label_12_0_i3);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__opt_debug__dump_label_2_0);
	call_localret(ENTRY(mercury__opt_debug__dump_label_2_0),
		mercury__vn_block__record_label_12_0_i16,
		STATIC(mercury__vn_block__record_label_12_0));
	}
Define_label(mercury__vn_block__record_label_12_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_block__record_label_12_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("cannot find label ", 18);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_vn_block__common_10);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__vn_block__record_label_12_0_i17,
		STATIC(mercury__vn_block__record_label_12_0));
	}
Define_label(mercury__vn_block__record_label_12_0_i17);
	update_prof_current_proc(LABEL(mercury__vn_block__record_label_12_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__vn_block__record_label_12_0));
	}
END_MODULE

BEGIN_MODULE(mercury__vn_block_module16)
	init_entry(mercury__vn_block__record_livevals_9_0);
	init_label(mercury__vn_block__record_livevals_9_0_i4);
	init_label(mercury__vn_block__record_livevals_9_0_i9);
	init_label(mercury__vn_block__record_livevals_9_0_i11);
	init_label(mercury__vn_block__record_livevals_9_0_i12);
	init_label(mercury__vn_block__record_livevals_9_0_i13);
	init_label(mercury__vn_block__record_livevals_9_0_i14);
	init_label(mercury__vn_block__record_livevals_9_0_i8);
	init_label(mercury__vn_block__record_livevals_9_0_i18);
	init_label(mercury__vn_block__record_livevals_9_0_i19);
	init_label(mercury__vn_block__record_livevals_9_0_i20);
	init_label(mercury__vn_block__record_livevals_9_0_i21);
	init_label(mercury__vn_block__record_livevals_9_0_i6);
	init_label(mercury__vn_block__record_livevals_9_0_i5);
	init_label(mercury__vn_block__record_livevals_9_0_i22);
	init_label(mercury__vn_block__record_livevals_9_0_i23);
	init_label(mercury__vn_block__record_livevals_9_0_i1006);
BEGIN_CODE

/* code for predicate 'vn_block__record_livevals'/9 in mode 0 */
Define_static(mercury__vn_block__record_livevals_9_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__record_livevals_9_0_i1006);
	incr_sp_push_msg(11, "vn_block__record_livevals");
	detstackvar(11) = (Integer) succip;
	detstackvar(8) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(6) = (Integer) r5;
	{
	Declare_entry(mercury__vn_util__no_access_lval_to_vnlval_2_0);
	call_localret(ENTRY(mercury__vn_util__no_access_lval_to_vnlval_2_0),
		mercury__vn_block__record_livevals_9_0_i4,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__record_livevals_9_0_i6);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(9) = (Integer) r1;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__vn_table__search_desired_value_3_0);
	call_localret(ENTRY(mercury__vn_table__search_desired_value_3_0),
		mercury__vn_block__record_livevals_9_0_i9,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__record_livevals_9_0_i8);
	detstackvar(5) = (Integer) r2;
	detstackvar(7) = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__vn_cost__lval_cost_3_0);
	call_localret(ENTRY(mercury__vn_cost__lval_cost_3_0),
		mercury__vn_block__record_livevals_9_0_i11,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__vn_table__get_vnlval_vn_list_2_0);
	call_localret(ENTRY(mercury__vn_table__get_vnlval_vn_list_2_0),
		mercury__vn_block__record_livevals_9_0_i12,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(10);
	r4 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__vn_block__find_cheaper_copies_2_5_0),
		mercury__vn_block__record_livevals_9_0_i13,
		STATIC(mercury__vn_block__record_livevals_9_0));
Define_label(mercury__vn_block__record_livevals_9_0_i13);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__record_livevals_9_0_i14);
	r6 = (Integer) detstackvar(1);
	r7 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(9);
	r5 = (Integer) detstackvar(5);
	r9 = (Integer) detstackvar(2);
	r10 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	GOTO_LABEL(mercury__vn_block__record_livevals_9_0_i19);
Define_label(mercury__vn_block__record_livevals_9_0_i14);
	r6 = (Integer) detstackvar(1);
	r7 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(9);
	r5 = (Integer) detstackvar(5);
	r9 = (Integer) detstackvar(2);
	tag_incr_hp(r10, mktag(1), ((Integer) 2));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	field(mktag(1), (Integer) r10, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r10, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	GOTO_LABEL(mercury__vn_block__record_livevals_9_0_i19);
Define_label(mercury__vn_block__record_livevals_9_0_i8);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__vn_table__record_first_vnlval_4_0);
	call_localret(ENTRY(mercury__vn_table__record_first_vnlval_4_0),
		mercury__vn_block__record_livevals_9_0_i18,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i18);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	r6 = (Integer) detstackvar(1);
	r7 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(9);
	r5 = (Integer) r1;
	r9 = (Integer) r2;
	r10 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
Define_label(mercury__vn_block__record_livevals_9_0_i19);
	detstackvar(1) = (Integer) r6;
	detstackvar(4) = (Integer) r7;
	detstackvar(8) = (Integer) r8;
	detstackvar(9) = (Integer) r4;
	detstackvar(7) = (Integer) r9;
	detstackvar(3) = (Integer) r10;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__vn_block__record_livevals_9_0_i20,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i20);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_block__record_livevals_9_0_i21,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i21);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__vn_block__record_livevals_9_0_i5);
Define_label(mercury__vn_block__record_livevals_9_0_i6);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	r6 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r5 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(4);
Define_label(mercury__vn_block__record_livevals_9_0_i5);
	detstackvar(3) = (Integer) r6;
	localcall(mercury__vn_block__record_livevals_9_0,
		LABEL(mercury__vn_block__record_livevals_9_0_i22),
		STATIC(mercury__vn_block__record_livevals_9_0));
Define_label(mercury__vn_block__record_livevals_9_0_i22);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	detstackvar(5) = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	detstackvar(7) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_block__common_12);
	r3 = (Integer) r4;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__vn_block__record_livevals_9_0_i23,
		STATIC(mercury__vn_block__record_livevals_9_0));
	}
Define_label(mercury__vn_block__record_livevals_9_0_i23);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevals_9_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury__vn_block__record_livevals_9_0_i1006);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module17)
	init_entry(mercury__vn_block__record_livevnlvals_7_0);
	init_label(mercury__vn_block__record_livevnlvals_7_0_i6);
	init_label(mercury__vn_block__record_livevnlvals_7_0_i5);
	init_label(mercury__vn_block__record_livevnlvals_7_0_i8);
	init_label(mercury__vn_block__record_livevnlvals_7_0_i9);
	init_label(mercury__vn_block__record_livevnlvals_7_0_i10);
	init_label(mercury__vn_block__record_livevnlvals_7_0_i11);
	init_label(mercury__vn_block__record_livevnlvals_7_0_i1003);
BEGIN_CODE

/* code for predicate 'vn_block__record_livevnlvals'/7 in mode 0 */
Define_static(mercury__vn_block__record_livevnlvals_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__record_livevnlvals_7_0_i1003);
	incr_sp_push_msg(8, "vn_block__record_livevnlvals");
	detstackvar(8) = (Integer) succip;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__vn_table__search_desired_value_3_0);
	call_localret(ENTRY(mercury__vn_table__search_desired_value_3_0),
		mercury__vn_block__record_livevnlvals_7_0_i6,
		STATIC(mercury__vn_block__record_livevnlvals_7_0));
	}
Define_label(mercury__vn_block__record_livevnlvals_7_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevnlvals_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_block__record_livevnlvals_7_0_i5);
	r6 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r5 = (Integer) r2;
	r8 = (Integer) detstackvar(1);
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	GOTO_LABEL(mercury__vn_block__record_livevnlvals_7_0_i9);
Define_label(mercury__vn_block__record_livevnlvals_7_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__vn_table__record_first_vnlval_4_0);
	call_localret(ENTRY(mercury__vn_table__record_first_vnlval_4_0),
		mercury__vn_block__record_livevnlvals_7_0_i8,
		STATIC(mercury__vn_block__record_livevnlvals_7_0));
	}
Define_label(mercury__vn_block__record_livevnlvals_7_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevnlvals_7_0));
	r6 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r5 = (Integer) r1;
	r8 = (Integer) r2;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
Define_label(mercury__vn_block__record_livevnlvals_7_0_i9);
	detstackvar(2) = (Integer) r6;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) r8;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__vn_block__record_livevnlvals_7_0_i10,
		STATIC(mercury__vn_block__record_livevnlvals_7_0));
	}
Define_label(mercury__vn_block__record_livevnlvals_7_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevnlvals_7_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_block__record_livevnlvals_7_0_i11,
		STATIC(mercury__vn_block__record_livevnlvals_7_0));
	}
Define_label(mercury__vn_block__record_livevnlvals_7_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_block__record_livevnlvals_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__vn_block__record_livevnlvals_7_0,
		STATIC(mercury__vn_block__record_livevnlvals_7_0));
Define_label(mercury__vn_block__record_livevnlvals_7_0_i1003);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module18)
	init_entry(mercury__vn_block__record_compulsory_lval_list_5_0);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1005);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i9);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1002);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i6);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i16);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i17);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i4);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1000);
	init_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1001);
BEGIN_CODE

/* code for predicate 'vn_block__record_compulsory_lval_list'/5 in mode 0 */
Define_static(mercury__vn_block__record_compulsory_lval_list_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1001);
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r5 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r5) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1002);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r5, ((Integer) 0)),
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1005) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1000) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1000) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1005) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1000) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1005) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i1000));
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1005);
	incr_sp_push_msg(4, "vn_block__record_compulsory_lval_list");
	detstackvar(4) = (Integer) succip;
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i9);
	r4 = (Integer) r5;
	r5 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	GOTO_LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i6);
	}
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1002);
	incr_sp_push_msg(4, "vn_block__record_compulsory_lval_list");
	detstackvar(4) = (Integer) succip;
	if ((tag((Integer) r5) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i4);
	COMPUTED_GOTO(unmkbody((Integer) r5),
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i9) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i9) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i9) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i4) AND
		LABEL(mercury__vn_block__record_compulsory_lval_list_5_0_i4));
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i6);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__vn_block__record_compulsory_lval_list_5_0_i16,
		STATIC(mercury__vn_block__record_compulsory_lval_list_5_0));
	}
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_block__record_compulsory_lval_list_5_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_block__record_compulsory_lval_list_5_0_i17,
		STATIC(mercury__vn_block__record_compulsory_lval_list_5_0));
	}
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i17);
	update_prof_current_proc(LABEL(mercury__vn_block__record_compulsory_lval_list_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__vn_block__record_compulsory_lval_list_5_0,
		STATIC(mercury__vn_block__record_compulsory_lval_list_5_0));
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i4);
	r1 = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__vn_block__record_compulsory_lval_list_5_0,
		STATIC(mercury__vn_block__record_compulsory_lval_list_5_0));
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1000);
	r1 = (Integer) r4;
	localtailcall(mercury__vn_block__record_compulsory_lval_list_5_0,
		STATIC(mercury__vn_block__record_compulsory_lval_list_5_0));
Define_label(mercury__vn_block__record_compulsory_lval_list_5_0_i1001);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_block_module19)
	init_entry(mercury__vn_block__find_cheaper_copies_2_5_0);
	init_label(mercury__vn_block__find_cheaper_copies_2_5_0_i6);
	init_label(mercury__vn_block__find_cheaper_copies_2_5_0_i9);
	init_label(mercury__vn_block__find_cheaper_copies_2_5_0_i11);
	init_label(mercury__vn_block__find_cheaper_copies_2_5_0_i8);
	init_label(mercury__vn_block__find_cheaper_copies_2_5_0_i1008);
	init_label(mercury__vn_block__find_cheaper_copies_2_5_0_i1010);
BEGIN_CODE

/* code for predicate 'vn_block__find_cheaper_copies_2'/5 in mode 0 */
Define_static(mercury__vn_block__find_cheaper_copies_2_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__find_cheaper_copies_2_5_0_i1008);
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1)) != (Integer) r2))
		GOTO_LABEL(mercury__vn_block__find_cheaper_copies_2_5_0_i1010);
	incr_sp_push_msg(4, "vn_block__find_cheaper_copies_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) r5;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	localcall(mercury__vn_block__find_cheaper_copies_2_5_0,
		LABEL(mercury__vn_block__find_cheaper_copies_2_5_0_i6),
		STATIC(mercury__vn_block__find_cheaper_copies_2_5_0));
	}
Define_label(mercury__vn_block__find_cheaper_copies_2_5_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_block__find_cheaper_copies_2_5_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_util__no_access_vnlval_to_lval_2_0);
	call_localret(ENTRY(mercury__vn_util__no_access_vnlval_to_lval_2_0),
		mercury__vn_block__find_cheaper_copies_2_5_0_i9,
		STATIC(mercury__vn_block__find_cheaper_copies_2_5_0));
	}
Define_label(mercury__vn_block__find_cheaper_copies_2_5_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_block__find_cheaper_copies_2_5_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_block__find_cheaper_copies_2_5_0_i8);
	r2 = (Integer) detstackvar(2);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__vn_cost__lval_cost_3_0);
	call_localret(ENTRY(mercury__vn_cost__lval_cost_3_0),
		mercury__vn_block__find_cheaper_copies_2_5_0_i11,
		STATIC(mercury__vn_block__find_cheaper_copies_2_5_0));
	}
Define_label(mercury__vn_block__find_cheaper_copies_2_5_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_block__find_cheaper_copies_2_5_0));
	if (((Integer) r1 >= (Integer) detstackvar(1)))
		GOTO_LABEL(mercury__vn_block__find_cheaper_copies_2_5_0_i8);
	if ((tag((Integer) detstackvar(2)) == mktag(((Integer) 2))))
		GOTO_LABEL(mercury__vn_block__find_cheaper_copies_2_5_0_i8);
	if (((Integer) detstackvar(2) == (Integer) mkword(mktag(0), mkbody(((Integer) 3)))))
		GOTO_LABEL(mercury__vn_block__find_cheaper_copies_2_5_0_i8);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__vn_block__find_cheaper_copies_2_5_0_i8);
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__vn_block__find_cheaper_copies_2_5_0_i1008);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__vn_block__find_cheaper_copies_2_5_0_i1010);
	r1 = (Integer) r5;
	localtailcall(mercury__vn_block__find_cheaper_copies_2_5_0,
		STATIC(mercury__vn_block__find_cheaper_copies_2_5_0));
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__vn_block_bunch_0(void)
{
	mercury__vn_block_module0();
	mercury__vn_block_module1();
	mercury__vn_block_module2();
	mercury__vn_block_module3();
	mercury__vn_block_module4();
	mercury__vn_block_module5();
	mercury__vn_block_module6();
	mercury__vn_block_module7();
	mercury__vn_block_module8();
	mercury__vn_block_module9();
	mercury__vn_block_module10();
	mercury__vn_block_module11();
	mercury__vn_block_module12();
	mercury__vn_block_module13();
	mercury__vn_block_module14();
	mercury__vn_block_module15();
	mercury__vn_block_module16();
	mercury__vn_block_module17();
	mercury__vn_block_module18();
	mercury__vn_block_module19();
}

#endif

void mercury__vn_block__init(void); /* suppress gcc warning */
void mercury__vn_block__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__vn_block_bunch_0();
#endif
}
