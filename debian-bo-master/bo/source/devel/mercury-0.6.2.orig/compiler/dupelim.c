/*
** Automatically generated from `dupelim.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__dupelim__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__dupelim__main_2_0);
Declare_label(mercury__dupelim__main_2_0_i2);
Declare_label(mercury__dupelim__main_2_0_i3);
Declare_label(mercury__dupelim__main_2_0_i4);
Declare_label(mercury__dupelim__main_2_0_i5);
Declare_label(mercury__dupelim__main_2_0_i6);
Declare_label(mercury__dupelim__main_2_0_i7);
Declare_label(mercury__dupelim__main_2_0_i8);
Declare_static(mercury__dupelim__make_blocks_2_0);
Declare_label(mercury__dupelim__make_blocks_2_0_i1001);
Declare_label(mercury__dupelim__make_blocks_2_0_i9);
Declare_label(mercury__dupelim__make_blocks_2_0_i10);
Declare_label(mercury__dupelim__make_blocks_2_0_i5);
Declare_static(mercury__dupelim__build_maps_5_0);
Declare_label(mercury__dupelim__build_maps_5_0_i6);
Declare_label(mercury__dupelim__build_maps_5_0_i7);
Declare_label(mercury__dupelim__build_maps_5_0_i9);
Declare_label(mercury__dupelim__build_maps_5_0_i5);
Declare_label(mercury__dupelim__build_maps_5_0_i10);
Declare_label(mercury__dupelim__build_maps_5_0_i13);
Declare_label(mercury__dupelim__build_maps_5_0_i17);
Declare_label(mercury__dupelim__build_maps_5_0_i16);
Declare_label(mercury__dupelim__build_maps_5_0_i12);
Declare_label(mercury__dupelim__build_maps_5_0_i20);
Declare_label(mercury__dupelim__build_maps_5_0_i19);
Declare_label(mercury__dupelim__build_maps_5_0_i1007);
Declare_static(mercury__dupelim__replace_labels_3_0);
Declare_label(mercury__dupelim__replace_labels_3_0_i6);
Declare_label(mercury__dupelim__replace_labels_3_0_i5);
Declare_label(mercury__dupelim__replace_labels_3_0_i9);
Declare_label(mercury__dupelim__replace_labels_3_0_i10);
Declare_label(mercury__dupelim__replace_labels_3_0_i1004);
Declare_static(mercury__dupelim__replace_labels_instr_list_3_0);
Declare_label(mercury__dupelim__replace_labels_instr_list_3_0_i4);
Declare_label(mercury__dupelim__replace_labels_instr_list_3_0_i5);
Declare_label(mercury__dupelim__replace_labels_instr_list_3_0_i1003);
Declare_static(mercury__dupelim__replace_labels_instr_3_0);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1041);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1040);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1039);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1038);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1037);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1036);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1035);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1034);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1033);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1032);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1031);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1030);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1029);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i5);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i6);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i7);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i8);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i9);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i10);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i11);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i12);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i13);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i14);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i15);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i18);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i19);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i20);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i21);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i22);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i24);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i25);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i26);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i27);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i28);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i29);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i30);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i31);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i32);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i33);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i34);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i35);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i36);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i37);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1028);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1024);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1025);
Declare_label(mercury__dupelim__replace_labels_instr_3_0_i1027);
Declare_static(mercury__dupelim__replace_labels_lval_3_0);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i1021);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i1020);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i1019);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i1018);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i1017);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i7);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i8);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i9);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i10);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i11);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i12);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i13);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i14);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i15);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i16);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i17);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i1016);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i20);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i21);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i22);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i23);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i24);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i1008);
Declare_label(mercury__dupelim__replace_labels_lval_3_0_i2);
Declare_static(mercury__dupelim__replace_labels_rval_3_0);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i6);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i5);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i11);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i13);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i1019);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i10);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i14);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i7);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i18);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i17);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i19);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i20);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i1022);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i22);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i2);
Declare_label(mercury__dupelim__replace_labels_rval_3_0_i1020);
Declare_static(mercury__dupelim__replace_labels_code_addr_3_0);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i1004);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i1005);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i1006);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i1007);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i1008);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i1009);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i1017);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i12);
Declare_label(mercury__dupelim__replace_labels_code_addr_3_0_i11);
Declare_static(mercury__dupelim__replace_labels_label_list_3_0);
Declare_label(mercury__dupelim__replace_labels_label_list_3_0_i4);
Declare_label(mercury__dupelim__replace_labels_label_list_3_0_i5);
Declare_label(mercury__dupelim__replace_labels_label_list_3_0_i1002);
Declare_static(mercury__dupelim__replace_labels_label_3_0);
Declare_label(mercury__dupelim__replace_labels_label_3_0_i4);
Declare_label(mercury__dupelim__replace_labels_label_3_0_i3);
Declare_static(mercury__dupelim__condense_2_0);
Declare_label(mercury__dupelim__condense_2_0_i4);
Declare_label(mercury__dupelim__condense_2_0_i1004);

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_dupelim__base_type_layout_block_0[];
Word * mercury_data_dupelim__base_type_info_block_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_dupelim__base_type_layout_block_0
};

extern Word * mercury_data_dupelim__common_5[];
Word * mercury_data_dupelim__base_type_layout_block_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dupelim__common_5),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dupelim__common_5),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dupelim__common_5),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dupelim__common_5)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_llds__base_type_info_instr_0[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_dupelim__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_instr_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_dupelim__common_1[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_0)
};

Word mercury_data_dupelim__common_2[] = {
	((Integer) 1),
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word mercury_data_dupelim__common_3[] = {
	((Integer) 1),
	(Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_llds__base_type_info_label_0[];
Word * mercury_data_dupelim__common_4[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_label_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_1)
};

Word * mercury_data_dupelim__common_5[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_4)
};

BEGIN_MODULE(mercury__dupelim_module0)
	init_entry(mercury__dupelim__main_2_0);
	init_label(mercury__dupelim__main_2_0_i2);
	init_label(mercury__dupelim__main_2_0_i3);
	init_label(mercury__dupelim__main_2_0_i4);
	init_label(mercury__dupelim__main_2_0_i5);
	init_label(mercury__dupelim__main_2_0_i6);
	init_label(mercury__dupelim__main_2_0_i7);
	init_label(mercury__dupelim__main_2_0_i8);
BEGIN_CODE

/* code for predicate 'dupelim__main'/2 in mode 0 */
Define_entry(mercury__dupelim__main_2_0);
	incr_sp_push_msg(4, "dupelim__main");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_1);
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__dupelim__main_2_0_i2,
		ENTRY(mercury__dupelim__main_2_0));
	}
Define_label(mercury__dupelim__main_2_0_i2);
	update_prof_current_proc(LABEL(mercury__dupelim__main_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__dupelim__main_2_0_i3,
		ENTRY(mercury__dupelim__main_2_0));
	}
Define_label(mercury__dupelim__main_2_0_i3);
	update_prof_current_proc(LABEL(mercury__dupelim__main_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__opt_util__skip_to_next_label_3_0);
	call_localret(ENTRY(mercury__opt_util__skip_to_next_label_3_0),
		mercury__dupelim__main_2_0_i4,
		ENTRY(mercury__dupelim__main_2_0));
	}
Define_label(mercury__dupelim__main_2_0_i4);
	update_prof_current_proc(LABEL(mercury__dupelim__main_2_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__dupelim__make_blocks_2_0),
		mercury__dupelim__main_2_0_i5,
		ENTRY(mercury__dupelim__main_2_0));
Define_label(mercury__dupelim__main_2_0_i5);
	update_prof_current_proc(LABEL(mercury__dupelim__main_2_0));
	r4 = (Integer) detstackvar(1);
	r2 = ((Integer) 0);
	r3 = (Integer) detstackvar(2);
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__dupelim__build_maps_5_0),
		mercury__dupelim__main_2_0_i6,
		ENTRY(mercury__dupelim__main_2_0));
Define_label(mercury__dupelim__main_2_0_i6);
	update_prof_current_proc(LABEL(mercury__dupelim__main_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__dupelim__replace_labels_3_0),
		mercury__dupelim__main_2_0_i7,
		ENTRY(mercury__dupelim__main_2_0));
Define_label(mercury__dupelim__main_2_0_i7);
	update_prof_current_proc(LABEL(mercury__dupelim__main_2_0));
	call_localret(STATIC(mercury__dupelim__condense_2_0),
		mercury__dupelim__main_2_0_i8,
		ENTRY(mercury__dupelim__main_2_0));
Define_label(mercury__dupelim__main_2_0_i8);
	update_prof_current_proc(LABEL(mercury__dupelim__main_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_0);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		ENTRY(mercury__dupelim__main_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dupelim_module1)
	init_entry(mercury__dupelim__make_blocks_2_0);
	init_label(mercury__dupelim__make_blocks_2_0_i1001);
	init_label(mercury__dupelim__make_blocks_2_0_i9);
	init_label(mercury__dupelim__make_blocks_2_0_i10);
	init_label(mercury__dupelim__make_blocks_2_0_i5);
BEGIN_CODE

/* code for predicate 'dupelim__make_blocks'/2 in mode 0 */
Define_static(mercury__dupelim__make_blocks_2_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__make_blocks_2_0_i1001);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__dupelim__make_blocks_2_0_i1001);
	incr_sp_push_msg(3, "dupelim__make_blocks");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__make_blocks_2_0_i5);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dupelim__make_blocks_2_0_i5);
	r3 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 5)))
		GOTO_LABEL(mercury__dupelim__make_blocks_2_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__opt_util__skip_to_next_label_3_0);
	call_localret(ENTRY(mercury__opt_util__skip_to_next_label_3_0),
		mercury__dupelim__make_blocks_2_0_i9,
		STATIC(mercury__dupelim__make_blocks_2_0));
	}
Define_label(mercury__dupelim__make_blocks_2_0_i9);
	update_prof_current_proc(LABEL(mercury__dupelim__make_blocks_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__dupelim__make_blocks_2_0,
		LABEL(mercury__dupelim__make_blocks_2_0_i10),
		STATIC(mercury__dupelim__make_blocks_2_0));
Define_label(mercury__dupelim__make_blocks_2_0_i10);
	update_prof_current_proc(LABEL(mercury__dupelim__make_blocks_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__dupelim__make_blocks_2_0_i5);
	r1 = string_const("instruction other than label in dupelim__make_blocks", 52);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__dupelim__make_blocks_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dupelim_module2)
	init_entry(mercury__dupelim__build_maps_5_0);
	init_label(mercury__dupelim__build_maps_5_0_i6);
	init_label(mercury__dupelim__build_maps_5_0_i7);
	init_label(mercury__dupelim__build_maps_5_0_i9);
	init_label(mercury__dupelim__build_maps_5_0_i5);
	init_label(mercury__dupelim__build_maps_5_0_i10);
	init_label(mercury__dupelim__build_maps_5_0_i13);
	init_label(mercury__dupelim__build_maps_5_0_i17);
	init_label(mercury__dupelim__build_maps_5_0_i16);
	init_label(mercury__dupelim__build_maps_5_0_i12);
	init_label(mercury__dupelim__build_maps_5_0_i20);
	init_label(mercury__dupelim__build_maps_5_0_i19);
	init_label(mercury__dupelim__build_maps_5_0_i1007);
BEGIN_CODE

/* code for predicate 'dupelim__build_maps'/5 in mode 0 */
Define_static(mercury__dupelim__build_maps_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__build_maps_5_0_i1007);
	incr_sp_push_msg(9, "dupelim__build_maps");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(5) = (Integer) r2;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_0);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__dupelim__build_maps_5_0_i6,
		STATIC(mercury__dupelim__build_maps_5_0));
	}
	}
Define_label(mercury__dupelim__build_maps_5_0_i6);
	update_prof_current_proc(LABEL(mercury__dupelim__build_maps_5_0));
	{
	Declare_entry(mercury__opt_util__skip_comments_livevals_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_livevals_2_0),
		mercury__dupelim__build_maps_5_0_i7,
		STATIC(mercury__dupelim__build_maps_5_0));
	}
Define_label(mercury__dupelim__build_maps_5_0_i7);
	update_prof_current_proc(LABEL(mercury__dupelim__build_maps_5_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__build_maps_5_0_i5);
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	{
	Declare_entry(mercury__opt_util__can_instr_fall_through_2_0);
	call_localret(ENTRY(mercury__opt_util__can_instr_fall_through_2_0),
		mercury__dupelim__build_maps_5_0_i9,
		STATIC(mercury__dupelim__build_maps_5_0));
	}
Define_label(mercury__dupelim__build_maps_5_0_i9);
	update_prof_current_proc(LABEL(mercury__dupelim__build_maps_5_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__dupelim__build_maps_5_0_i5);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = ((Integer) 1);
	GOTO_LABEL(mercury__dupelim__build_maps_5_0_i10);
Define_label(mercury__dupelim__build_maps_5_0_i5);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = ((Integer) 0);
Define_label(mercury__dupelim__build_maps_5_0_i10);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r2;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r4;
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_1);
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__dupelim__build_maps_5_0_i13,
		STATIC(mercury__dupelim__build_maps_5_0));
	}
Define_label(mercury__dupelim__build_maps_5_0_i13);
	update_prof_current_proc(LABEL(mercury__dupelim__build_maps_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dupelim__build_maps_5_0_i12);
	if (((Integer) detstackvar(1) == ((Integer) 0)))
		GOTO_LABEL(mercury__dupelim__build_maps_5_0_i16);
	r5 = (Integer) r2;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	detstackvar(8) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__dupelim__build_maps_5_0_i17,
		STATIC(mercury__dupelim__build_maps_5_0));
	}
Define_label(mercury__dupelim__build_maps_5_0_i17);
	update_prof_current_proc(LABEL(mercury__dupelim__build_maps_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__dupelim__build_maps_5_0,
		STATIC(mercury__dupelim__build_maps_5_0));
Define_label(mercury__dupelim__build_maps_5_0_i16);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__dupelim__build_maps_5_0,
		STATIC(mercury__dupelim__build_maps_5_0));
Define_label(mercury__dupelim__build_maps_5_0_i12);
	if (((Integer) detstackvar(7) == ((Integer) 0)))
		GOTO_LABEL(mercury__dupelim__build_maps_5_0_i19);
	detstackvar(8) = (Integer) detstackvar(3);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_1);
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__dupelim__build_maps_5_0_i20,
		STATIC(mercury__dupelim__build_maps_5_0));
	}
Define_label(mercury__dupelim__build_maps_5_0_i20);
	update_prof_current_proc(LABEL(mercury__dupelim__build_maps_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(7);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__dupelim__build_maps_5_0,
		STATIC(mercury__dupelim__build_maps_5_0));
Define_label(mercury__dupelim__build_maps_5_0_i19);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__dupelim__build_maps_5_0,
		STATIC(mercury__dupelim__build_maps_5_0));
Define_label(mercury__dupelim__build_maps_5_0_i1007);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module3)
	init_entry(mercury__dupelim__replace_labels_3_0);
	init_label(mercury__dupelim__replace_labels_3_0_i6);
	init_label(mercury__dupelim__replace_labels_3_0_i5);
	init_label(mercury__dupelim__replace_labels_3_0_i9);
	init_label(mercury__dupelim__replace_labels_3_0_i10);
	init_label(mercury__dupelim__replace_labels_3_0_i1004);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__replace_labels_3_0_i1004);
	incr_sp_push_msg(5, "dupelim__replace_labels");
	detstackvar(5) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(2) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__dupelim__replace_labels_3_0_i6,
		STATIC(mercury__dupelim__replace_labels_3_0));
	}
	}
Define_label(mercury__dupelim__replace_labels_3_0_i6);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dupelim__replace_labels_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__dupelim__replace_labels_3_0,
		STATIC(mercury__dupelim__replace_labels_3_0));
Define_label(mercury__dupelim__replace_labels_3_0_i5);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__dupelim__replace_labels_instr_list_3_0),
		mercury__dupelim__replace_labels_3_0_i9,
		STATIC(mercury__dupelim__replace_labels_3_0));
Define_label(mercury__dupelim__replace_labels_3_0_i9);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	localcall(mercury__dupelim__replace_labels_3_0,
		LABEL(mercury__dupelim__replace_labels_3_0_i10),
		STATIC(mercury__dupelim__replace_labels_3_0));
Define_label(mercury__dupelim__replace_labels_3_0_i10);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__dupelim__replace_labels_3_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module4)
	init_entry(mercury__dupelim__replace_labels_instr_list_3_0);
	init_label(mercury__dupelim__replace_labels_instr_list_3_0_i4);
	init_label(mercury__dupelim__replace_labels_instr_list_3_0_i5);
	init_label(mercury__dupelim__replace_labels_instr_list_3_0_i1003);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels_instr_list'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_instr_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__replace_labels_instr_list_3_0_i1003);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr_list");
	detstackvar(4) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	call_localret(STATIC(mercury__dupelim__replace_labels_instr_3_0),
		mercury__dupelim__replace_labels_instr_list_3_0_i4,
		STATIC(mercury__dupelim__replace_labels_instr_list_3_0));
Define_label(mercury__dupelim__replace_labels_instr_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_list_3_0));
	r2 = (Integer) detstackvar(1);
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	localcall(mercury__dupelim__replace_labels_instr_list_3_0,
		LABEL(mercury__dupelim__replace_labels_instr_list_3_0_i5),
		STATIC(mercury__dupelim__replace_labels_instr_list_3_0));
Define_label(mercury__dupelim__replace_labels_instr_list_3_0_i5);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_list_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_list_3_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module5)
	init_entry(mercury__dupelim__replace_labels_instr_3_0);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1041);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1040);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1039);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1038);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1037);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1036);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1035);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1034);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1033);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1032);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1031);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1030);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1029);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i5);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i6);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i7);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i8);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i9);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i10);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i11);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i12);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i13);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i14);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i15);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i18);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i19);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i20);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i21);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i22);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i24);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i25);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i26);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i27);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i28);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i29);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i30);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i31);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i32);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i33);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i34);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i35);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i36);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i37);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1028);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1024);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1025);
	init_label(mercury__dupelim__replace_labels_instr_3_0_i1027);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels_instr'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_instr_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i1028);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1041) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1040) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1039) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1038) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1037) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1025) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1036) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1035) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1024) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1034) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1033) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1032) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1031) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1030) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1029) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1024) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1024) AND
		LABEL(mercury__dupelim__replace_labels_instr_3_0_i1024));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1041);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i5);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1040);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i7);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1039);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i10);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1038);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i12);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1037);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i14);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1036);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i18);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1035);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i20);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1034);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i24);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1033);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i27);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1032);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i30);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1031);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i32);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1030);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i34);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1029);
	incr_sp_push_msg(4, "dupelim__replace_labels_instr");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i36);
Define_label(mercury__dupelim__replace_labels_instr_3_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	call_localret(STATIC(mercury__dupelim__replace_labels_instr_list_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i6,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i6);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i7);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_lval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i8,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i8);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i9,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i9);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i10);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	call_localret(STATIC(mercury__dupelim__replace_labels_code_addr_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i11,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i11);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 5));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 4)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i12);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	call_localret(STATIC(mercury__dupelim__replace_labels_code_addr_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i13,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i13);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i14);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_code_addr_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i15,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i15);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i18);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_code_addr_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i19,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i19);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 6);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i20);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i21,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i21);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__dupelim__replace_labels_label_list_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i22,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i22);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 7);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i24);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i25,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i25);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__dupelim__replace_labels_code_addr_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i26,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i26);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 9);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i27);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_lval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i28,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i28);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i29,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i29);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 10);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i30);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_lval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i31,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i31);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 11);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i32);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i33,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i33);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 12);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i34);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_lval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i35,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i35);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 13);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i36);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_instr_3_0_i37,
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
Define_label(mercury__dupelim__replace_labels_instr_3_0_i37);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_instr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 14);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1028);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dupelim__replace_labels_instr_3_0_i1027);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1024);
	proceed();
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1025);
	r1 = string_const("found label in dupelim__replace_labels_instr", 44);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__dupelim__replace_labels_instr_3_0));
	}
Define_label(mercury__dupelim__replace_labels_instr_3_0_i1027);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module6)
	init_entry(mercury__dupelim__replace_labels_lval_3_0);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i1021);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i1020);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i1019);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i1018);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i1017);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i7);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i8);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i9);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i10);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i11);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i12);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i13);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i14);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i15);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i16);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i17);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i1016);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i20);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i21);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i22);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i23);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i24);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i1008);
	init_label(mercury__dupelim__replace_labels_lval_3_0_i2);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels_lval'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_lval_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dupelim__replace_labels_lval_3_0_i1016);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1008) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1008) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1021) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1020) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1019) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1018) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1017) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i1008));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i1021);
	incr_sp_push_msg(4, "dupelim__replace_labels_lval");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_lval_3_0_i7);
Define_label(mercury__dupelim__replace_labels_lval_3_0_i1020);
	incr_sp_push_msg(4, "dupelim__replace_labels_lval");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_lval_3_0_i9);
Define_label(mercury__dupelim__replace_labels_lval_3_0_i1019);
	incr_sp_push_msg(4, "dupelim__replace_labels_lval");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_lval_3_0_i11);
Define_label(mercury__dupelim__replace_labels_lval_3_0_i1018);
	incr_sp_push_msg(4, "dupelim__replace_labels_lval");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_lval_3_0_i13);
Define_label(mercury__dupelim__replace_labels_lval_3_0_i1017);
	incr_sp_push_msg(4, "dupelim__replace_labels_lval");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dupelim__replace_labels_lval_3_0_i15);
Define_label(mercury__dupelim__replace_labels_lval_3_0_i7);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_lval_3_0_i8,
		STATIC(mercury__dupelim__replace_labels_lval_3_0));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i8);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_lval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i9);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_lval_3_0_i10,
		STATIC(mercury__dupelim__replace_labels_lval_3_0));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i10);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_lval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i11);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_lval_3_0_i12,
		STATIC(mercury__dupelim__replace_labels_lval_3_0));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i12);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_lval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i13);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_lval_3_0_i14,
		STATIC(mercury__dupelim__replace_labels_lval_3_0));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i14);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_lval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i15);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_lval_3_0_i16,
		STATIC(mercury__dupelim__replace_labels_lval_3_0));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i16);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_lval_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__dupelim__replace_labels_rval_3_0),
		mercury__dupelim__replace_labels_lval_3_0_i17,
		STATIC(mercury__dupelim__replace_labels_lval_3_0));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i17);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_lval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 6);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i1016);
	incr_sp_push_msg(4, "dupelim__replace_labels_lval");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dupelim__replace_labels_lval_3_0_i2);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i20) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i21) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i22) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i23) AND
		LABEL(mercury__dupelim__replace_labels_lval_3_0_i24));
Define_label(mercury__dupelim__replace_labels_lval_3_0_i20);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i21);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i22);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 2)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i23);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 3)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i24);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 4)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i1008);
	proceed();
Define_label(mercury__dupelim__replace_labels_lval_3_0_i2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module7)
	init_entry(mercury__dupelim__replace_labels_rval_3_0);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i6);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i5);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i11);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i13);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i1019);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i10);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i14);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i7);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i18);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i17);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i19);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i20);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i1022);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i22);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i2);
	init_label(mercury__dupelim__replace_labels_rval_3_0_i1020);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels_rval'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_rval_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i1022);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 0));
	incr_sp_push_msg(4, "dupelim__replace_labels_rval");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__dupelim__replace_labels_rval_3_0,
		LABEL(mercury__dupelim__replace_labels_rval_3_0_i6),
		STATIC(mercury__dupelim__replace_labels_rval_3_0));
Define_label(mercury__dupelim__replace_labels_rval_3_0_i6);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_rval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i5);
	if (((Integer) r3 != ((Integer) 1)))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i7);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i10);
	r4 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 0));
	if (((Integer) r4 != ((Integer) 0)))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i11);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i11);
	if (((Integer) r4 != ((Integer) 1)))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i1019);
	r1 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	call_localret(STATIC(mercury__dupelim__replace_labels_code_addr_3_0),
		mercury__dupelim__replace_labels_rval_3_0_i13,
		STATIC(mercury__dupelim__replace_labels_rval_3_0));
Define_label(mercury__dupelim__replace_labels_rval_3_0_i13);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_rval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	tag_incr_hp(r3, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i1019);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i10);
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i14);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((unmkbody((Integer) r3) != ((Integer) 0)))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i1020);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_dupelim__common_2);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i14);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i1019);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i7);
	if (((Integer) r3 != ((Integer) 2)))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i17);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__dupelim__replace_labels_rval_3_0,
		LABEL(mercury__dupelim__replace_labels_rval_3_0_i18),
		STATIC(mercury__dupelim__replace_labels_rval_3_0));
Define_label(mercury__dupelim__replace_labels_rval_3_0_i18);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_rval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i17);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__dupelim__replace_labels_rval_3_0,
		LABEL(mercury__dupelim__replace_labels_rval_3_0_i19),
		STATIC(mercury__dupelim__replace_labels_rval_3_0));
Define_label(mercury__dupelim__replace_labels_rval_3_0_i19);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_rval_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	localcall(mercury__dupelim__replace_labels_rval_3_0,
		LABEL(mercury__dupelim__replace_labels_rval_3_0_i20),
		STATIC(mercury__dupelim__replace_labels_rval_3_0));
Define_label(mercury__dupelim__replace_labels_rval_3_0_i20);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_rval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i1022);
	incr_sp_push_msg(4, "dupelim__replace_labels_rval");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dupelim__replace_labels_rval_3_0_i2);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__dupelim__replace_labels_lval_3_0),
		mercury__dupelim__replace_labels_rval_3_0_i22,
		STATIC(mercury__dupelim__replace_labels_rval_3_0));
Define_label(mercury__dupelim__replace_labels_rval_3_0_i22);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_rval_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dupelim__replace_labels_rval_3_0_i1020);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_dupelim__common_3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module8)
	init_entry(mercury__dupelim__replace_labels_code_addr_3_0);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i1004);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i1005);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i1006);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i1007);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i1008);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i1009);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i1017);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i12);
	init_label(mercury__dupelim__replace_labels_code_addr_3_0_i11);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels_code_addr'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_code_addr_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i1017);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i1004) AND
		LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i1005) AND
		LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i1006) AND
		LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i1007) AND
		LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i1008) AND
		LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i1009));
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i1005);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	proceed();
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i1006);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 2)));
	proceed();
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i1007);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 3)));
	proceed();
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i1008);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 4)));
	proceed();
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i1009);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 5)));
	proceed();
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i1017);
	incr_sp_push_msg(1, "dupelim__replace_labels_code_addr");
	detstackvar(1) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__dupelim__replace_labels_code_addr_3_0_i11);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__dupelim__replace_labels_label_3_0),
		mercury__dupelim__replace_labels_code_addr_3_0_i12,
		STATIC(mercury__dupelim__replace_labels_code_addr_3_0));
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i12);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_code_addr_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__dupelim__replace_labels_code_addr_3_0_i11);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module9)
	init_entry(mercury__dupelim__replace_labels_label_list_3_0);
	init_label(mercury__dupelim__replace_labels_label_list_3_0_i4);
	init_label(mercury__dupelim__replace_labels_label_list_3_0_i5);
	init_label(mercury__dupelim__replace_labels_label_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels_label_list'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_label_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__replace_labels_label_list_3_0_i1002);
	incr_sp_push_msg(3, "dupelim__replace_labels_label_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__dupelim__replace_labels_label_3_0),
		mercury__dupelim__replace_labels_label_list_3_0_i4,
		STATIC(mercury__dupelim__replace_labels_label_list_3_0));
Define_label(mercury__dupelim__replace_labels_label_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_label_list_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__dupelim__replace_labels_label_list_3_0,
		LABEL(mercury__dupelim__replace_labels_label_list_3_0_i5),
		STATIC(mercury__dupelim__replace_labels_label_list_3_0));
Define_label(mercury__dupelim__replace_labels_label_list_3_0_i5);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_label_list_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__dupelim__replace_labels_label_list_3_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module10)
	init_entry(mercury__dupelim__replace_labels_label_3_0);
	init_label(mercury__dupelim__replace_labels_label_3_0_i4);
	init_label(mercury__dupelim__replace_labels_label_3_0_i3);
BEGIN_CODE

/* code for predicate 'dupelim__replace_labels_label'/3 in mode 0 */
Define_static(mercury__dupelim__replace_labels_label_3_0);
	r3 = (Integer) r2;
	r4 = (Integer) r1;
	incr_sp_push_msg(2, "dupelim__replace_labels_label");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__dupelim__replace_labels_label_3_0_i4,
		STATIC(mercury__dupelim__replace_labels_label_3_0));
	}
Define_label(mercury__dupelim__replace_labels_label_3_0_i4);
	update_prof_current_proc(LABEL(mercury__dupelim__replace_labels_label_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dupelim__replace_labels_label_3_0_i3);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__dupelim__replace_labels_label_3_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dupelim_module11)
	init_entry(mercury__dupelim__condense_2_0);
	init_label(mercury__dupelim__condense_2_0_i4);
	init_label(mercury__dupelim__condense_2_0_i1004);
BEGIN_CODE

/* code for predicate 'dupelim__condense'/2 in mode 0 */
Define_static(mercury__dupelim__condense_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dupelim__condense_2_0_i1004);
	incr_sp_push_msg(3, "dupelim__condense");
	detstackvar(3) = (Integer) succip;
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	localcall(mercury__dupelim__condense_2_0,
		LABEL(mercury__dupelim__condense_2_0_i4),
		STATIC(mercury__dupelim__condense_2_0));
Define_label(mercury__dupelim__condense_2_0_i4);
	update_prof_current_proc(LABEL(mercury__dupelim__condense_2_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_dupelim__common_0);
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = string_const("", 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__dupelim__condense_2_0));
	}
	}
Define_label(mercury__dupelim__condense_2_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__dupelim_bunch_0(void)
{
	mercury__dupelim_module0();
	mercury__dupelim_module1();
	mercury__dupelim_module2();
	mercury__dupelim_module3();
	mercury__dupelim_module4();
	mercury__dupelim_module5();
	mercury__dupelim_module6();
	mercury__dupelim_module7();
	mercury__dupelim_module8();
	mercury__dupelim_module9();
	mercury__dupelim_module10();
	mercury__dupelim_module11();
}

#endif

void mercury__dupelim__init(void); /* suppress gcc warning */
void mercury__dupelim__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__dupelim_bunch_0();
#endif
}
