/*
** Automatically generated from `vn_verify.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__vn_verify__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__vn_verify__ok_11_0);
Declare_label(mercury__vn_verify__ok_11_0_i4);
Declare_label(mercury__vn_verify__ok_11_0_i1002);
Declare_label(mercury__vn_verify__ok_11_0_i7);
Declare_label(mercury__vn_verify__ok_11_0_i8);
Declare_label(mercury__vn_verify__ok_11_0_i9);
Declare_label(mercury__vn_verify__ok_11_0_i10);
Declare_label(mercury__vn_verify__ok_11_0_i11);
Declare_label(mercury__vn_verify__ok_11_0_i12);
Declare_label(mercury__vn_verify__ok_11_0_i13);
Declare_label(mercury__vn_verify__ok_11_0_i14);
Declare_label(mercury__vn_verify__ok_11_0_i15);
Declare_label(mercury__vn_verify__ok_11_0_i16);
Declare_label(mercury__vn_verify__ok_11_0_i17);
Declare_label(mercury__vn_verify__ok_11_0_i20);
Declare_label(mercury__vn_verify__ok_11_0_i23);
Declare_label(mercury__vn_verify__ok_11_0_i24);
Declare_label(mercury__vn_verify__ok_11_0_i25);
Declare_label(mercury__vn_verify__ok_11_0_i26);
Declare_label(mercury__vn_verify__ok_11_0_i27);
Declare_label(mercury__vn_verify__ok_11_0_i29);
Declare_label(mercury__vn_verify__ok_11_0_i30);
Declare_label(mercury__vn_verify__ok_11_0_i6);
Declare_label(mercury__vn_verify__ok_11_0_i34);
Declare_label(mercury__vn_verify__ok_11_0_i35);
Declare_label(mercury__vn_verify__ok_11_0_i36);
Declare_label(mercury__vn_verify__ok_11_0_i37);
Declare_label(mercury__vn_verify__ok_11_0_i33);
Declare_label(mercury__vn_verify__ok_11_0_i39);
Declare_static(mercury__vn_verify__correspondence_4_0);
Declare_label(mercury__vn_verify__correspondence_4_0_i6);
Declare_label(mercury__vn_verify__correspondence_4_0_i8);
Declare_label(mercury__vn_verify__correspondence_4_0_i12);
Declare_label(mercury__vn_verify__correspondence_4_0_i11);
Declare_label(mercury__vn_verify__correspondence_4_0_i15);
Declare_label(mercury__vn_verify__correspondence_4_0_i16);
Declare_label(mercury__vn_verify__correspondence_4_0_i5);
Declare_label(mercury__vn_verify__correspondence_4_0_i22);
Declare_label(mercury__vn_verify__correspondence_4_0_i18);
Declare_label(mercury__vn_verify__correspondence_4_0_i25);
Declare_label(mercury__vn_verify__correspondence_4_0_i1000);
Declare_static(mercury__vn_verify__make_verify_map_2_5_0);
Declare_label(mercury__vn_verify__make_verify_map_2_5_0_i4);
Declare_label(mercury__vn_verify__make_verify_map_2_5_0_i7);
Declare_label(mercury__vn_verify__make_verify_map_2_5_0_i9);
Declare_label(mercury__vn_verify__make_verify_map_2_5_0_i10);
Declare_label(mercury__vn_verify__make_verify_map_2_5_0_i6);
Declare_label(mercury__vn_verify__make_verify_map_2_5_0_i1003);
Declare_static(mercury__vn_verify__make_verify_map_specials_3_0);
Declare_label(mercury__vn_verify__make_verify_map_specials_3_0_i7);
Declare_label(mercury__vn_verify__make_verify_map_specials_3_0_i8);
Declare_label(mercury__vn_verify__make_verify_map_specials_3_0_i1003);
Declare_label(mercury__vn_verify__make_verify_map_specials_3_0_i1005);
Declare_static(mercury__vn_verify__lval_3_0);
Declare_label(mercury__vn_verify__lval_3_0_i2);
Declare_label(mercury__vn_verify__lval_3_0_i3);
Declare_label(mercury__vn_verify__lval_3_0_i9);
Declare_label(mercury__vn_verify__lval_3_0_i11);
Declare_label(mercury__vn_verify__lval_3_0_i14);
Declare_label(mercury__vn_verify__lval_3_0_i17);
Declare_label(mercury__vn_verify__lval_3_0_i20);
Declare_label(mercury__vn_verify__lval_3_0_i23);
Declare_label(mercury__vn_verify__lval_3_0_i27);
Declare_label(mercury__vn_verify__lval_3_0_i8);
Declare_label(mercury__vn_verify__lval_3_0_i30);
Declare_label(mercury__vn_verify__lval_3_0_i32);
Declare_label(mercury__vn_verify__lval_3_0_i34);
Declare_label(mercury__vn_verify__lval_3_0_i36);
Declare_label(mercury__vn_verify__lval_3_0_i38);
Declare_label(mercury__vn_verify__lval_3_0_i29);
Declare_label(mercury__vn_verify__lval_3_0_i40);
Declare_label(mercury__vn_verify__lval_3_0_i4);
Declare_label(mercury__vn_verify__lval_3_0_i1000);
Declare_static(mercury__vn_verify__values_3_0);
Declare_label(mercury__vn_verify__values_3_0_i4);
Declare_label(mercury__vn_verify__values_3_0_i5);
Declare_label(mercury__vn_verify__values_3_0_i1002);
Declare_static(mercury__vn_verify__value_3_0);
Declare_label(mercury__vn_verify__value_3_0_i2);
Declare_label(mercury__vn_verify__value_3_0_i3);
Declare_label(mercury__vn_verify__value_3_0_i4);
Declare_label(mercury__vn_verify__value_3_0_i7);
Declare_label(mercury__vn_verify__value_3_0_i1000);
Declare_static(mercury__vn_verify__subst_sub_vns_4_0);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i1039);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i7);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i1038);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i1035);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i13);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i15);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i1022);
Declare_label(mercury__vn_verify__subst_sub_vns_4_0_i1);
Declare_static(mercury__vn_verify__tags_2_3_0);
Declare_label(mercury__vn_verify__tags_2_3_0_i1014);
Declare_label(mercury__vn_verify__tags_2_3_0_i1013);
Declare_label(mercury__vn_verify__tags_2_3_0_i1012);
Declare_label(mercury__vn_verify__tags_2_3_0_i1011);
Declare_label(mercury__vn_verify__tags_2_3_0_i1010);
Declare_label(mercury__vn_verify__tags_2_3_0_i1009);
Declare_label(mercury__vn_verify__tags_2_3_0_i1008);
Declare_label(mercury__vn_verify__tags_2_3_0_i1005);
Declare_label(mercury__vn_verify__tags_2_3_0_i7);
Declare_label(mercury__vn_verify__tags_2_3_0_i8);
Declare_label(mercury__vn_verify__tags_2_3_0_i9);
Declare_label(mercury__vn_verify__tags_2_3_0_i10);
Declare_label(mercury__vn_verify__tags_2_3_0_i14);
Declare_label(mercury__vn_verify__tags_2_3_0_i16);
Declare_label(mercury__vn_verify__tags_2_3_0_i18);
Declare_label(mercury__vn_verify__tags_2_3_0_i13);
Declare_label(mercury__vn_verify__tags_2_3_0_i19);
Declare_label(mercury__vn_verify__tags_2_3_0_i22);
Declare_label(mercury__vn_verify__tags_2_3_0_i24);
Declare_label(mercury__vn_verify__tags_2_3_0_i21);
Declare_label(mercury__vn_verify__tags_2_3_0_i26);
Declare_label(mercury__vn_verify__tags_2_3_0_i27);
Declare_label(mercury__vn_verify__tags_2_3_0_i28);
Declare_label(mercury__vn_verify__tags_2_3_0_i36);
Declare_label(mercury__vn_verify__tags_2_3_0_i38);
Declare_label(mercury__vn_verify__tags_2_3_0_i39);
Declare_label(mercury__vn_verify__tags_2_3_0_i41);
Declare_label(mercury__vn_verify__tags_2_3_0_i42);
Declare_label(mercury__vn_verify__tags_2_3_0_i44);
Declare_label(mercury__vn_verify__tags_2_3_0_i46);
Declare_label(mercury__vn_verify__tags_2_3_0_i49);
Declare_label(mercury__vn_verify__tags_2_3_0_i60);
Declare_label(mercury__vn_verify__tags_2_3_0_i1004);
Declare_label(mercury__vn_verify__tags_2_3_0_i62);
Declare_label(mercury__vn_verify__tags_2_3_0_i1);
Declare_label(mercury__vn_verify__tags_2_3_0_i1000);
Declare_label(mercury__vn_verify__tags_2_3_0_i1001);
Declare_label(mercury__vn_verify__tags_2_3_0_i1002);
Declare_static(mercury__vn_verify__tags_lval_2_0);
Declare_label(mercury__vn_verify__tags_lval_2_0_i1013);
Declare_label(mercury__vn_verify__tags_lval_2_0_i1012);
Declare_label(mercury__vn_verify__tags_lval_2_0_i19);
Declare_label(mercury__vn_verify__tags_lval_2_0_i22);
Declare_label(mercury__vn_verify__tags_lval_2_0_i24);
Declare_label(mercury__vn_verify__tags_lval_2_0_i28);
Declare_label(mercury__vn_verify__tags_lval_2_0_i1004);
Declare_label(mercury__vn_verify__tags_lval_2_0_i2);
Declare_label(mercury__vn_verify__tags_lval_2_0_i1);
Declare_label(mercury__vn_verify__tags_lval_2_0_i1008);
Declare_static(mercury__vn_verify__tags_rval_2_0);
Declare_label(mercury__vn_verify__tags_rval_2_0_i1012);
Declare_label(mercury__vn_verify__tags_rval_2_0_i9);
Declare_label(mercury__vn_verify__tags_rval_2_0_i12);
Declare_label(mercury__vn_verify__tags_rval_2_0_i1011);
Declare_label(mercury__vn_verify__tags_rval_2_0_i16);
Declare_label(mercury__vn_verify__tags_rval_2_0_i2);
Declare_label(mercury__vn_verify__tags_rval_2_0_i1);
Declare_static(mercury__vn_verify__tags_cond_5_0);
Declare_label(mercury__vn_verify__tags_cond_5_0_i8);
Declare_label(mercury__vn_verify__tags_cond_5_0_i10);
Declare_label(mercury__vn_verify__tags_cond_5_0_i7);
Declare_label(mercury__vn_verify__tags_cond_5_0_i11);
Declare_label(mercury__vn_verify__tags_cond_5_0_i13);
Declare_label(mercury__vn_verify__tags_cond_5_0_i16);
Declare_label(mercury__vn_verify__tags_cond_5_0_i18);
Declare_label(mercury__vn_verify__tags_cond_5_0_i15);
Declare_label(mercury__vn_verify__tags_cond_5_0_i19);
Declare_label(mercury__vn_verify__tags_cond_5_0_i1002);
Declare_label(mercury__vn_verify__tags_cond_5_0_i2);
Declare_label(mercury__vn_verify__tags_cond_5_0_i26);
Declare_label(mercury__vn_verify__tags_cond_5_0_i25);
Declare_label(mercury__vn_verify__tags_cond_5_0_i28);
Declare_label(mercury__vn_verify__tags_cond_5_0_i30);
Declare_label(mercury__vn_verify__tags_cond_5_0_i22);
Declare_label(mercury__vn_verify__tags_cond_5_0_i32);
Declare_label(mercury__vn_verify__tags_cond_5_0_i41);
Declare_label(mercury__vn_verify__tags_cond_5_0_i43);
Declare_label(mercury__vn_verify__tags_cond_5_0_i38);
Declare_label(mercury__vn_verify__tags_cond_5_0_i44);
Declare_label(mercury__vn_verify__tags_cond_5_0_i1);
Declare_label(mercury__vn_verify__tags_cond_5_0_i1000);
Declare_static(mercury__vn_verify__tags_is_base_2_0);
Declare_label(mercury__vn_verify__tags_is_base_2_0_i3);
Declare_label(mercury__vn_verify__tags_is_base_2_0_i1);

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_llds__base_type_info_instr_0[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_vn_verify__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_instr_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

BEGIN_MODULE(mercury__vn_verify_module0)
	init_entry(mercury__vn_verify__ok_11_0);
	init_label(mercury__vn_verify__ok_11_0_i4);
	init_label(mercury__vn_verify__ok_11_0_i1002);
	init_label(mercury__vn_verify__ok_11_0_i7);
	init_label(mercury__vn_verify__ok_11_0_i8);
	init_label(mercury__vn_verify__ok_11_0_i9);
	init_label(mercury__vn_verify__ok_11_0_i10);
	init_label(mercury__vn_verify__ok_11_0_i11);
	init_label(mercury__vn_verify__ok_11_0_i12);
	init_label(mercury__vn_verify__ok_11_0_i13);
	init_label(mercury__vn_verify__ok_11_0_i14);
	init_label(mercury__vn_verify__ok_11_0_i15);
	init_label(mercury__vn_verify__ok_11_0_i16);
	init_label(mercury__vn_verify__ok_11_0_i17);
	init_label(mercury__vn_verify__ok_11_0_i20);
	init_label(mercury__vn_verify__ok_11_0_i23);
	init_label(mercury__vn_verify__ok_11_0_i24);
	init_label(mercury__vn_verify__ok_11_0_i25);
	init_label(mercury__vn_verify__ok_11_0_i26);
	init_label(mercury__vn_verify__ok_11_0_i27);
	init_label(mercury__vn_verify__ok_11_0_i29);
	init_label(mercury__vn_verify__ok_11_0_i30);
	init_label(mercury__vn_verify__ok_11_0_i6);
	init_label(mercury__vn_verify__ok_11_0_i34);
	init_label(mercury__vn_verify__ok_11_0_i35);
	init_label(mercury__vn_verify__ok_11_0_i36);
	init_label(mercury__vn_verify__ok_11_0_i37);
	init_label(mercury__vn_verify__ok_11_0_i33);
	init_label(mercury__vn_verify__ok_11_0_i39);
BEGIN_CODE

/* code for predicate 'vn_verify__ok'/11 in mode 0 */
Define_entry(mercury__vn_verify__ok_11_0);
	if (((Integer) r3 == (Integer) r4))
		GOTO_LABEL(mercury__vn_verify__ok_11_0_i1002);
	r1 = (Integer) r2;
	r2 = string_const("disagreement on SeenIncr", 24);
	r3 = (Integer) r9;
	incr_sp_push_msg(9, "vn_verify__ok");
	detstackvar(9) = (Integer) succip;
	{
	Declare_entry(mercury__vn_debug__failure_msg_4_0);
	call_localret(ENTRY(mercury__vn_debug__failure_msg_4_0),
		mercury__vn_verify__ok_11_0_i4,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__vn_verify__ok_11_0_i1002);
	incr_sp_push_msg(9, "vn_verify__ok");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) r8;
	detstackvar(7) = (Integer) r9;
	{
	extern Word * mercury_data_vn_type__base_type_info_vnlval_0[];
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	}
	r2 = (Integer) r5;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__vn_verify__ok_11_0_i7,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(3) = (Integer) r1;
	{
	extern Word * mercury_data_vn_type__base_type_info_vnlval_0[];
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	}
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__vn_verify__ok_11_0_i8,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(4) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__vn_verify__ok_11_0_i9,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__vn_table__get_all_vnrvals_2_0);
	call_localret(ENTRY(mercury__vn_table__get_all_vnrvals_2_0),
		mercury__vn_verify__ok_11_0_i10,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__vn_verify__make_verify_map_specials_3_0),
		mercury__vn_verify__ok_11_0_i11,
		ENTRY(mercury__vn_verify__ok_11_0));
Define_label(mercury__vn_verify__ok_11_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__vn_verify__make_verify_map_2_5_0),
		mercury__vn_verify__ok_11_0_i12,
		ENTRY(mercury__vn_verify__ok_11_0));
Define_label(mercury__vn_verify__ok_11_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(3) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__vn_verify__ok_11_0_i13,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i13);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__vn_table__get_all_vnrvals_2_0);
	call_localret(ENTRY(mercury__vn_table__get_all_vnrvals_2_0),
		mercury__vn_verify__ok_11_0_i14,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i14);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__vn_verify__make_verify_map_specials_3_0),
		mercury__vn_verify__ok_11_0_i15,
		ENTRY(mercury__vn_verify__ok_11_0));
Define_label(mercury__vn_verify__ok_11_0_i15);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__vn_verify__make_verify_map_2_5_0),
		mercury__vn_verify__ok_11_0_i16,
		ENTRY(mercury__vn_verify__ok_11_0));
Define_label(mercury__vn_verify__ok_11_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	if (((Integer) detstackvar(5) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__ok_11_0_i17);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(7);
	r4 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__vn_verify__ok_11_0_i29);
Define_label(mercury__vn_verify__ok_11_0_i17);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__ok_11_0_i20);
	r4 = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(7);
	GOTO_LABEL(mercury__vn_verify__ok_11_0_i29);
Define_label(mercury__vn_verify__ok_11_0_i20);
	detstackvar(4) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__vn_verify__ok_11_0_i23,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i23);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(5) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__vn_verify__ok_11_0_i24,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i24);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__vn_verify__ok_11_0_i25,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i25);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	Declare_entry(mercury__list__remove_dups_2_0);
	call_localret(ENTRY(mercury__list__remove_dups_2_0),
		mercury__vn_verify__ok_11_0_i26,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i26);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__vn_verify__correspondence_4_0),
		mercury__vn_verify__ok_11_0_i27,
		ENTRY(mercury__vn_verify__ok_11_0));
Define_label(mercury__vn_verify__ok_11_0_i27);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(7);
Define_label(mercury__vn_verify__ok_11_0_i29);
	if (((Integer) r4 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__ok_11_0_i30);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	detstackvar(7) = (Integer) r3;
	GOTO_LABEL(mercury__vn_verify__ok_11_0_i6);
Define_label(mercury__vn_verify__ok_11_0_i30);
	r2 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	{
	Declare_entry(mercury__vn_debug__failure_msg_4_0);
	call_localret(ENTRY(mercury__vn_debug__failure_msg_4_0),
		mercury__vn_verify__ok_11_0_i4,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i6);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_vn_verify__common_0);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__vn_verify__ok_11_0_i34,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i34);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__vn_verify__ok_11_0_i35,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i35);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	detstackvar(3) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__vn_verify__ok_11_0_i36,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i36);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__vn_verify__tags_2_3_0),
		mercury__vn_verify__ok_11_0_i37,
		ENTRY(mercury__vn_verify__ok_11_0));
Define_label(mercury__vn_verify__ok_11_0_i37);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__ok_11_0_i33);
	r1 = ((Integer) 0);
	r2 = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__vn_verify__ok_11_0_i33);
	r1 = (Integer) detstackvar(2);
	r2 = string_const("failure of tag check", 20);
	r3 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__vn_debug__failure_msg_4_0);
	call_localret(ENTRY(mercury__vn_debug__failure_msg_4_0),
		mercury__vn_verify__ok_11_0_i39,
		ENTRY(mercury__vn_verify__ok_11_0));
	}
Define_label(mercury__vn_verify__ok_11_0_i39);
	update_prof_current_proc(LABEL(mercury__vn_verify__ok_11_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module1)
	init_entry(mercury__vn_verify__correspondence_4_0);
	init_label(mercury__vn_verify__correspondence_4_0_i6);
	init_label(mercury__vn_verify__correspondence_4_0_i8);
	init_label(mercury__vn_verify__correspondence_4_0_i12);
	init_label(mercury__vn_verify__correspondence_4_0_i11);
	init_label(mercury__vn_verify__correspondence_4_0_i15);
	init_label(mercury__vn_verify__correspondence_4_0_i16);
	init_label(mercury__vn_verify__correspondence_4_0_i5);
	init_label(mercury__vn_verify__correspondence_4_0_i22);
	init_label(mercury__vn_verify__correspondence_4_0_i18);
	init_label(mercury__vn_verify__correspondence_4_0_i25);
	init_label(mercury__vn_verify__correspondence_4_0_i1000);
BEGIN_CODE

/* code for predicate 'vn_verify__correspondence'/4 in mode 0 */
Define_static(mercury__vn_verify__correspondence_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__correspondence_4_0_i1000);
	incr_sp_push_msg(6, "vn_verify__correspondence");
	detstackvar(6) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__vn_verify__correspondence_4_0_i6,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_verify__correspondence_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__correspondence_4_0_i5);
	detstackvar(5) = (Integer) r2;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__vn_verify__correspondence_4_0_i8,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_verify__correspondence_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__correspondence_4_0_i5);
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury____Unify___llds__rval_0_0);
	call_localret(ENTRY(mercury____Unify___llds__rval_0_0),
		mercury__vn_verify__correspondence_4_0_i12,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_verify__correspondence_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__correspondence_4_0_i11);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__vn_verify__correspondence_4_0,
		STATIC(mercury__vn_verify__correspondence_4_0));
Define_label(mercury__vn_verify__correspondence_4_0_i11);
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__opt_debug__dump_lval_2_0);
	call_localret(ENTRY(mercury__opt_debug__dump_lval_2_0),
		mercury__vn_verify__correspondence_4_0_i15,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i15);
	update_prof_current_proc(LABEL(mercury__vn_verify__correspondence_4_0));
	r2 = (Integer) r1;
	r1 = string_const("disagreement on value of ", 25);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__vn_verify__correspondence_4_0_i16,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_verify__correspondence_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__vn_verify__correspondence_4_0_i5);
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__vn_verify__correspondence_4_0_i22,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i22);
	update_prof_current_proc(LABEL(mercury__vn_verify__correspondence_4_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__vn_verify__correspondence_4_0_i18);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__vn_verify__correspondence_4_0,
		STATIC(mercury__vn_verify__correspondence_4_0));
Define_label(mercury__vn_verify__correspondence_4_0_i18);
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__opt_debug__dump_lval_2_0);
	call_localret(ENTRY(mercury__opt_debug__dump_lval_2_0),
		mercury__vn_verify__correspondence_4_0_i25,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i25);
	update_prof_current_proc(LABEL(mercury__vn_verify__correspondence_4_0));
	r2 = (Integer) r1;
	r1 = string_const("cannot find value of ", 21);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__vn_verify__correspondence_4_0_i16,
		STATIC(mercury__vn_verify__correspondence_4_0));
	}
Define_label(mercury__vn_verify__correspondence_4_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module2)
	init_entry(mercury__vn_verify__make_verify_map_2_5_0);
	init_label(mercury__vn_verify__make_verify_map_2_5_0_i4);
	init_label(mercury__vn_verify__make_verify_map_2_5_0_i7);
	init_label(mercury__vn_verify__make_verify_map_2_5_0_i9);
	init_label(mercury__vn_verify__make_verify_map_2_5_0_i10);
	init_label(mercury__vn_verify__make_verify_map_2_5_0_i6);
	init_label(mercury__vn_verify__make_verify_map_2_5_0_i1003);
BEGIN_CODE

/* code for predicate 'vn_verify__make_verify_map_2'/5 in mode 0 */
Define_static(mercury__vn_verify__make_verify_map_2_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__make_verify_map_2_5_0_i1003);
	incr_sp_push_msg(5, "vn_verify__make_verify_map_2");
	detstackvar(5) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	call_localret(STATIC(mercury__vn_verify__lval_3_0),
		mercury__vn_verify__make_verify_map_2_5_0_i4,
		STATIC(mercury__vn_verify__make_verify_map_2_5_0));
Define_label(mercury__vn_verify__make_verify_map_2_5_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_verify__make_verify_map_2_5_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__vn_table__search_desired_value_3_0);
	call_localret(ENTRY(mercury__vn_table__search_desired_value_3_0),
		mercury__vn_verify__make_verify_map_2_5_0_i7,
		STATIC(mercury__vn_verify__make_verify_map_2_5_0));
	}
Define_label(mercury__vn_verify__make_verify_map_2_5_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_verify__make_verify_map_2_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__make_verify_map_2_5_0_i6);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__vn_verify__value_3_0),
		mercury__vn_verify__make_verify_map_2_5_0_i9,
		STATIC(mercury__vn_verify__make_verify_map_2_5_0));
Define_label(mercury__vn_verify__make_verify_map_2_5_0_i9);
	update_prof_current_proc(LABEL(mercury__vn_verify__make_verify_map_2_5_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__vn_verify__make_verify_map_2_5_0_i10,
		STATIC(mercury__vn_verify__make_verify_map_2_5_0));
	}
Define_label(mercury__vn_verify__make_verify_map_2_5_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_verify__make_verify_map_2_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__vn_verify__make_verify_map_2_5_0,
		STATIC(mercury__vn_verify__make_verify_map_2_5_0));
Define_label(mercury__vn_verify__make_verify_map_2_5_0_i6);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__vn_verify__make_verify_map_2_5_0,
		STATIC(mercury__vn_verify__make_verify_map_2_5_0));
Define_label(mercury__vn_verify__make_verify_map_2_5_0_i1003);
	r1 = (Integer) r3;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module3)
	init_entry(mercury__vn_verify__make_verify_map_specials_3_0);
	init_label(mercury__vn_verify__make_verify_map_specials_3_0_i7);
	init_label(mercury__vn_verify__make_verify_map_specials_3_0_i8);
	init_label(mercury__vn_verify__make_verify_map_specials_3_0_i1003);
	init_label(mercury__vn_verify__make_verify_map_specials_3_0_i1005);
BEGIN_CODE

/* code for predicate 'vn_verify__make_verify_map_specials'/3 in mode 0 */
Define_static(mercury__vn_verify__make_verify_map_specials_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__make_verify_map_specials_3_0_i1003);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) tempr1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_verify__make_verify_map_specials_3_0_i1005);
	incr_sp_push_msg(3, "vn_verify__make_verify_map_specials");
	detstackvar(3) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__vn_util__find_specials_2_0);
	call_localret(ENTRY(mercury__vn_util__find_specials_2_0),
		mercury__vn_verify__make_verify_map_specials_3_0_i7,
		STATIC(mercury__vn_verify__make_verify_map_specials_3_0));
	}
	}
Define_label(mercury__vn_verify__make_verify_map_specials_3_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_verify__make_verify_map_specials_3_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data_vn_type__base_type_info_vnlval_0[];
	r1 = (Integer) mercury_data_vn_type__base_type_info_vnlval_0;
	}
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__vn_verify__make_verify_map_specials_3_0_i8,
		STATIC(mercury__vn_verify__make_verify_map_specials_3_0));
	}
Define_label(mercury__vn_verify__make_verify_map_specials_3_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_verify__make_verify_map_specials_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__vn_verify__make_verify_map_specials_3_0,
		STATIC(mercury__vn_verify__make_verify_map_specials_3_0));
Define_label(mercury__vn_verify__make_verify_map_specials_3_0_i1003);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__vn_verify__make_verify_map_specials_3_0_i1005);
	r1 = (Integer) r3;
	localtailcall(mercury__vn_verify__make_verify_map_specials_3_0,
		STATIC(mercury__vn_verify__make_verify_map_specials_3_0));
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module4)
	init_entry(mercury__vn_verify__lval_3_0);
	init_label(mercury__vn_verify__lval_3_0_i2);
	init_label(mercury__vn_verify__lval_3_0_i3);
	init_label(mercury__vn_verify__lval_3_0_i9);
	init_label(mercury__vn_verify__lval_3_0_i11);
	init_label(mercury__vn_verify__lval_3_0_i14);
	init_label(mercury__vn_verify__lval_3_0_i17);
	init_label(mercury__vn_verify__lval_3_0_i20);
	init_label(mercury__vn_verify__lval_3_0_i23);
	init_label(mercury__vn_verify__lval_3_0_i27);
	init_label(mercury__vn_verify__lval_3_0_i8);
	init_label(mercury__vn_verify__lval_3_0_i30);
	init_label(mercury__vn_verify__lval_3_0_i32);
	init_label(mercury__vn_verify__lval_3_0_i34);
	init_label(mercury__vn_verify__lval_3_0_i36);
	init_label(mercury__vn_verify__lval_3_0_i38);
	init_label(mercury__vn_verify__lval_3_0_i29);
	init_label(mercury__vn_verify__lval_3_0_i40);
	init_label(mercury__vn_verify__lval_3_0_i4);
	init_label(mercury__vn_verify__lval_3_0_i1000);
BEGIN_CODE

/* code for predicate 'vn_verify__lval'/3 in mode 0 */
Define_static(mercury__vn_verify__lval_3_0);
	incr_sp_push_msg(3, "vn_verify__lval");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__vn_util__vnlval_access_vns_2_0);
	call_localret(ENTRY(mercury__vn_util__vnlval_access_vns_2_0),
		mercury__vn_verify__lval_3_0_i2,
		STATIC(mercury__vn_verify__lval_3_0));
	}
Define_label(mercury__vn_verify__lval_3_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_verify__lval_3_0));
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__vn_verify__values_3_0),
		mercury__vn_verify__lval_3_0_i3,
		STATIC(mercury__vn_verify__lval_3_0));
Define_label(mercury__vn_verify__lval_3_0_i3);
	update_prof_current_proc(LABEL(mercury__vn_verify__lval_3_0));
	r2 = (Integer) detstackvar(1);
	r3 = tag((Integer) r2);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i8);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)),
		LABEL(mercury__vn_verify__lval_3_0_i9) AND
		LABEL(mercury__vn_verify__lval_3_0_i11) AND
		LABEL(mercury__vn_verify__lval_3_0_i14) AND
		LABEL(mercury__vn_verify__lval_3_0_i17) AND
		LABEL(mercury__vn_verify__lval_3_0_i20) AND
		LABEL(mercury__vn_verify__lval_3_0_i23) AND
		LABEL(mercury__vn_verify__lval_3_0_i27));
Define_label(mercury__vn_verify__lval_3_0_i9);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i11);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i14);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i17);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i20);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i23);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r1, ((Integer) 1)), ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 6);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r3, ((Integer) 1)), ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i27);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	tag_incr_hp(r1, mktag(2), ((Integer) 1));
	field(mktag(2), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i8);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i29);
	COMPUTED_GOTO(unmkbody((Integer) r2),
		LABEL(mercury__vn_verify__lval_3_0_i30) AND
		LABEL(mercury__vn_verify__lval_3_0_i32) AND
		LABEL(mercury__vn_verify__lval_3_0_i34) AND
		LABEL(mercury__vn_verify__lval_3_0_i36) AND
		LABEL(mercury__vn_verify__lval_3_0_i38));
Define_label(mercury__vn_verify__lval_3_0_i30);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i32);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i34);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 2)));
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i36);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 3)));
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i38);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 4)));
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i29);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i40);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i40);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__lval_3_0_i4);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(2), (Integer) r2, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__lval_3_0_i4);
	r1 = string_const("cannot substitute access vns in vn_verify__lval", 47);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__vn_verify__lval_3_0));
	}
Define_label(mercury__vn_verify__lval_3_0_i1000);
	r1 = string_const("cannot substitute access vns in vn_verify__lval", 47);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__vn_verify__lval_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module5)
	init_entry(mercury__vn_verify__values_3_0);
	init_label(mercury__vn_verify__values_3_0_i4);
	init_label(mercury__vn_verify__values_3_0_i5);
	init_label(mercury__vn_verify__values_3_0_i1002);
BEGIN_CODE

/* code for predicate 'vn_verify__values'/3 in mode 0 */
Define_static(mercury__vn_verify__values_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__values_3_0_i1002);
	incr_sp_push_msg(3, "vn_verify__values");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__vn_verify__value_3_0),
		mercury__vn_verify__values_3_0_i4,
		STATIC(mercury__vn_verify__values_3_0));
Define_label(mercury__vn_verify__values_3_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_verify__values_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__vn_verify__values_3_0,
		LABEL(mercury__vn_verify__values_3_0_i5),
		STATIC(mercury__vn_verify__values_3_0));
Define_label(mercury__vn_verify__values_3_0_i5);
	update_prof_current_proc(LABEL(mercury__vn_verify__values_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__values_3_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module6)
	init_entry(mercury__vn_verify__value_3_0);
	init_label(mercury__vn_verify__value_3_0_i2);
	init_label(mercury__vn_verify__value_3_0_i3);
	init_label(mercury__vn_verify__value_3_0_i4);
	init_label(mercury__vn_verify__value_3_0_i7);
	init_label(mercury__vn_verify__value_3_0_i1000);
BEGIN_CODE

/* code for predicate 'vn_verify__value'/3 in mode 0 */
Define_static(mercury__vn_verify__value_3_0);
	r3 = (Integer) r2;
	incr_sp_push_msg(3, "vn_verify__value");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = string_const("vn_verify__value", 16);
	{
	Declare_entry(mercury__vn_table__lookup_defn_4_0);
	call_localret(ENTRY(mercury__vn_table__lookup_defn_4_0),
		mercury__vn_verify__value_3_0_i2,
		STATIC(mercury__vn_verify__value_3_0));
	}
Define_label(mercury__vn_verify__value_3_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_verify__value_3_0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__vn_util__find_sub_vns_2_0);
	call_localret(ENTRY(mercury__vn_util__find_sub_vns_2_0),
		mercury__vn_verify__value_3_0_i3,
		STATIC(mercury__vn_verify__value_3_0));
	}
Define_label(mercury__vn_verify__value_3_0_i3);
	update_prof_current_proc(LABEL(mercury__vn_verify__value_3_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__vn_verify__values_3_0),
		mercury__vn_verify__value_3_0_i4,
		STATIC(mercury__vn_verify__value_3_0));
Define_label(mercury__vn_verify__value_3_0_i4);
	update_prof_current_proc(LABEL(mercury__vn_verify__value_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__vn_verify__subst_sub_vns_4_0),
		mercury__vn_verify__value_3_0_i7,
		STATIC(mercury__vn_verify__value_3_0));
Define_label(mercury__vn_verify__value_3_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_verify__value_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__value_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__vn_verify__value_3_0_i1000);
	r1 = string_const("cannot substitute sub vns in vn_verify__value", 45);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__vn_verify__value_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module7)
	init_entry(mercury__vn_verify__subst_sub_vns_4_0);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i1039);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i7);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i1038);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i1035);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i13);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i15);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i1022);
	init_label(mercury__vn_verify__subst_sub_vns_4_0_i1);
BEGIN_CODE

/* code for predicate 'vn_verify__subst_sub_vns'/4 in mode 0 */
Define_static(mercury__vn_verify__subst_sub_vns_4_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1038);
	r4 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 0));
	if (((Integer) r4 != ((Integer) 0)))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1039);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1022);
	tag_incr_hp(r2, mktag(2), ((Integer) 4));
	field(mktag(2), (Integer) r2, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	field(mktag(2), (Integer) r2, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	field(mktag(2), (Integer) r2, ((Integer) 2)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	field(mktag(2), (Integer) r2, ((Integer) 3)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = TRUE;
	proceed();
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i1039);
	incr_sp_push_msg(1, "vn_verify__subst_sub_vns");
	detstackvar(1) = (Integer) succip;
	if (((Integer) r4 != ((Integer) 1)))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i7);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	if (((Integer) field(mktag(1), (Integer) r2, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i7);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	if (((Integer) field(mktag(1), (Integer) r2, ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r3, ((Integer) 1)), ((Integer) 0));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i1038);
	incr_sp_push_msg(1, "vn_verify__subst_sub_vns");
	detstackvar(1) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i13);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__vn_verify__lval_3_0),
		mercury__vn_verify__subst_sub_vns_4_0_i1035,
		STATIC(mercury__vn_verify__subst_sub_vns_4_0));
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i1035);
	update_prof_current_proc(LABEL(mercury__vn_verify__subst_sub_vns_4_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i13);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i15);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	if (((Integer) field(mktag(1), (Integer) r2, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i15);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__subst_sub_vns_4_0_i1);
	tag_incr_hp(r2, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i1022);
	r1 = FALSE;
	proceed();
Define_label(mercury__vn_verify__subst_sub_vns_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module8)
	init_entry(mercury__vn_verify__tags_2_3_0);
	init_label(mercury__vn_verify__tags_2_3_0_i1014);
	init_label(mercury__vn_verify__tags_2_3_0_i1013);
	init_label(mercury__vn_verify__tags_2_3_0_i1012);
	init_label(mercury__vn_verify__tags_2_3_0_i1011);
	init_label(mercury__vn_verify__tags_2_3_0_i1010);
	init_label(mercury__vn_verify__tags_2_3_0_i1009);
	init_label(mercury__vn_verify__tags_2_3_0_i1008);
	init_label(mercury__vn_verify__tags_2_3_0_i1005);
	init_label(mercury__vn_verify__tags_2_3_0_i7);
	init_label(mercury__vn_verify__tags_2_3_0_i8);
	init_label(mercury__vn_verify__tags_2_3_0_i9);
	init_label(mercury__vn_verify__tags_2_3_0_i10);
	init_label(mercury__vn_verify__tags_2_3_0_i14);
	init_label(mercury__vn_verify__tags_2_3_0_i16);
	init_label(mercury__vn_verify__tags_2_3_0_i18);
	init_label(mercury__vn_verify__tags_2_3_0_i13);
	init_label(mercury__vn_verify__tags_2_3_0_i19);
	init_label(mercury__vn_verify__tags_2_3_0_i22);
	init_label(mercury__vn_verify__tags_2_3_0_i24);
	init_label(mercury__vn_verify__tags_2_3_0_i21);
	init_label(mercury__vn_verify__tags_2_3_0_i26);
	init_label(mercury__vn_verify__tags_2_3_0_i27);
	init_label(mercury__vn_verify__tags_2_3_0_i28);
	init_label(mercury__vn_verify__tags_2_3_0_i36);
	init_label(mercury__vn_verify__tags_2_3_0_i38);
	init_label(mercury__vn_verify__tags_2_3_0_i39);
	init_label(mercury__vn_verify__tags_2_3_0_i41);
	init_label(mercury__vn_verify__tags_2_3_0_i42);
	init_label(mercury__vn_verify__tags_2_3_0_i44);
	init_label(mercury__vn_verify__tags_2_3_0_i46);
	init_label(mercury__vn_verify__tags_2_3_0_i49);
	init_label(mercury__vn_verify__tags_2_3_0_i60);
	init_label(mercury__vn_verify__tags_2_3_0_i1004);
	init_label(mercury__vn_verify__tags_2_3_0_i62);
	init_label(mercury__vn_verify__tags_2_3_0_i1);
	init_label(mercury__vn_verify__tags_2_3_0_i1000);
	init_label(mercury__vn_verify__tags_2_3_0_i1001);
	init_label(mercury__vn_verify__tags_2_3_0_i1002);
BEGIN_CODE

/* code for predicate 'vn_verify__tags_2'/3 in mode 0 */
Define_static(mercury__vn_verify__tags_2_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1002);
	r4 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r4) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1004);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r4, ((Integer) 0)),
		LABEL(mercury__vn_verify__tags_2_3_0_i1014) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1013) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1012) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1011) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1010) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1009) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1008) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1009) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1008) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1000) AND
		LABEL(mercury__vn_verify__tags_2_3_0_i1005));
Define_label(mercury__vn_verify__tags_2_3_0_i1014);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i7);
Define_label(mercury__vn_verify__tags_2_3_0_i1013);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i9);
Define_label(mercury__vn_verify__tags_2_3_0_i1012);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i36);
Define_label(mercury__vn_verify__tags_2_3_0_i1011);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i38);
Define_label(mercury__vn_verify__tags_2_3_0_i1010);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i41);
Define_label(mercury__vn_verify__tags_2_3_0_i1009);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i46);
Define_label(mercury__vn_verify__tags_2_3_0_i1008);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i49);
Define_label(mercury__vn_verify__tags_2_3_0_i1005);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i60);
Define_label(mercury__vn_verify__tags_2_3_0_i7);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = string_const("found block in vn_verify__tags_instr", 36);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_verify__tags_2_3_0_i8,
		STATIC(mercury__vn_verify__tags_2_3_0));
	}
Define_label(mercury__vn_verify__tags_2_3_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i9);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r4, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r4, ((Integer) 2));
	detstackvar(6) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	call_localret(STATIC(mercury__vn_verify__tags_lval_2_0),
		mercury__vn_verify__tags_2_3_0_i10,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1);
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__vn_verify__tags_2_3_0_i14,
		STATIC(mercury__vn_verify__tags_2_3_0));
	}
Define_label(mercury__vn_verify__tags_2_3_0_i14);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i13);
	r1 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__vn_verify__tags_is_base_2_0),
		mercury__vn_verify__tags_2_3_0_i16,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i13);
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_verify__tags_2_3_0_i18,
		STATIC(mercury__vn_verify__tags_2_3_0));
	}
Define_label(mercury__vn_verify__tags_2_3_0_i18);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(7);
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i19);
Define_label(mercury__vn_verify__tags_2_3_0_i13);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(1);
Define_label(mercury__vn_verify__tags_2_3_0_i19);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	detstackvar(6) = (Integer) r2;
	detstackvar(7) = (Integer) r4;
	detstackvar(8) = (Integer) r5;
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__vn_verify__tags_2_3_0_i22,
		STATIC(mercury__vn_verify__tags_2_3_0));
	}
Define_label(mercury__vn_verify__tags_2_3_0_i22);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i21);
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__vn_verify__tags_cond_5_0),
		mercury__vn_verify__tags_2_3_0_i24,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i24);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) r3;
	r6 = (Integer) detstackvar(7);
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(6);
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i26);
Define_label(mercury__vn_verify__tags_2_3_0_i21);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(6);
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
Define_label(mercury__vn_verify__tags_2_3_0_i26);
	detstackvar(3) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(7) = (Integer) r6;
	{
	Declare_entry(mercury__set__delete_3_0);
	call_localret(ENTRY(mercury__set__delete_3_0),
		mercury__vn_verify__tags_2_3_0_i27,
		STATIC(mercury__vn_verify__tags_2_3_0));
	}
Define_label(mercury__vn_verify__tags_2_3_0_i27);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	r2 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_2_3_0_i28,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i28);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i36);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = string_const("found c_code in vn_verify__tags_instr", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_verify__tags_2_3_0_i8,
		STATIC(mercury__vn_verify__tags_2_3_0));
	}
Define_label(mercury__vn_verify__tags_2_3_0_i38);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r4, ((Integer) 1));
	call_localret(STATIC(mercury__vn_verify__tags_cond_5_0),
		mercury__vn_verify__tags_2_3_0_i39,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i39);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1);
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i41);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r4, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r4, ((Integer) 1));
	call_localret(STATIC(mercury__vn_verify__tags_lval_2_0),
		mercury__vn_verify__tags_2_3_0_i42,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i42);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_2_3_0_i44,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i44);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i46);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r4, ((Integer) 1));
	call_localret(STATIC(mercury__vn_verify__tags_lval_2_0),
		mercury__vn_verify__tags_2_3_0_i44,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i49);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r4, ((Integer) 1));
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_2_3_0_i44,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i60);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = string_const("found c_code in vn_verify__tags_instr", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_verify__tags_2_3_0_i8,
		STATIC(mercury__vn_verify__tags_2_3_0));
	}
Define_label(mercury__vn_verify__tags_2_3_0_i1004);
	incr_sp_push_msg(9, "vn_verify__tags_2");
	detstackvar(9) = (Integer) succip;
	if ((tag((Integer) r4) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i62);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i62);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	if ((tag((Integer) r4) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_verify__tags_2_3_0_i1001);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__vn_verify__tags_2_3_0_i1000);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i1001);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localtailcall(mercury__vn_verify__tags_2_3_0,
		STATIC(mercury__vn_verify__tags_2_3_0));
Define_label(mercury__vn_verify__tags_2_3_0_i1002);
	r1 = TRUE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module9)
	init_entry(mercury__vn_verify__tags_lval_2_0);
	init_label(mercury__vn_verify__tags_lval_2_0_i1013);
	init_label(mercury__vn_verify__tags_lval_2_0_i1012);
	init_label(mercury__vn_verify__tags_lval_2_0_i19);
	init_label(mercury__vn_verify__tags_lval_2_0_i22);
	init_label(mercury__vn_verify__tags_lval_2_0_i24);
	init_label(mercury__vn_verify__tags_lval_2_0_i28);
	init_label(mercury__vn_verify__tags_lval_2_0_i1004);
	init_label(mercury__vn_verify__tags_lval_2_0_i2);
	init_label(mercury__vn_verify__tags_lval_2_0_i1);
	init_label(mercury__vn_verify__tags_lval_2_0_i1008);
BEGIN_CODE

/* code for predicate 'vn_verify__tags_lval'/2 in mode 0 */
Define_static(mercury__vn_verify__tags_lval_2_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__tags_lval_2_0_i1004);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 0));
	COMPUTED_GOTO((Integer) r3,
		LABEL(mercury__vn_verify__tags_lval_2_0_i1004) AND
		LABEL(mercury__vn_verify__tags_lval_2_0_i1004) AND
		LABEL(mercury__vn_verify__tags_lval_2_0_i1008) AND
		LABEL(mercury__vn_verify__tags_lval_2_0_i1008) AND
		LABEL(mercury__vn_verify__tags_lval_2_0_i1008) AND
		LABEL(mercury__vn_verify__tags_lval_2_0_i1008) AND
		LABEL(mercury__vn_verify__tags_lval_2_0_i1013) AND
		LABEL(mercury__vn_verify__tags_lval_2_0_i1012));
Define_label(mercury__vn_verify__tags_lval_2_0_i1013);
	incr_sp_push_msg(4, "vn_verify__tags_lval");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_lval_2_0_i19);
Define_label(mercury__vn_verify__tags_lval_2_0_i1012);
	incr_sp_push_msg(4, "vn_verify__tags_lval");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__vn_verify__tags_lval_2_0_i28);
Define_label(mercury__vn_verify__tags_lval_2_0_i19);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__vn_verify__tags_lval_2_0_i22,
		STATIC(mercury__vn_verify__tags_lval_2_0));
	}
Define_label(mercury__vn_verify__tags_lval_2_0_i22);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_lval_2_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__vn_verify__tags_lval_2_0_i1);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_lval_2_0_i24,
		STATIC(mercury__vn_verify__tags_lval_2_0));
Define_label(mercury__vn_verify__tags_lval_2_0_i24);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_lval_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_lval_2_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__vn_verify__tags_rval_2_0),
		STATIC(mercury__vn_verify__tags_lval_2_0));
Define_label(mercury__vn_verify__tags_lval_2_0_i28);
	r1 = string_const("found lvar in vn_verify__tags_lval", 34);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_verify__tags_lval_2_0_i2,
		STATIC(mercury__vn_verify__tags_lval_2_0));
	}
Define_label(mercury__vn_verify__tags_lval_2_0_i1004);
	r1 = TRUE;
	proceed();
Define_label(mercury__vn_verify__tags_lval_2_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_lval_2_0));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__vn_verify__tags_lval_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__vn_verify__tags_lval_2_0_i1008);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	tailcall(STATIC(mercury__vn_verify__tags_rval_2_0),
		STATIC(mercury__vn_verify__tags_lval_2_0));
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module10)
	init_entry(mercury__vn_verify__tags_rval_2_0);
	init_label(mercury__vn_verify__tags_rval_2_0_i1012);
	init_label(mercury__vn_verify__tags_rval_2_0_i9);
	init_label(mercury__vn_verify__tags_rval_2_0_i12);
	init_label(mercury__vn_verify__tags_rval_2_0_i1011);
	init_label(mercury__vn_verify__tags_rval_2_0_i16);
	init_label(mercury__vn_verify__tags_rval_2_0_i2);
	init_label(mercury__vn_verify__tags_rval_2_0_i1);
BEGIN_CODE

/* code for predicate 'vn_verify__tags_rval'/2 in mode 0 */
Define_static(mercury__vn_verify__tags_rval_2_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__tags_rval_2_0_i1011);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__vn_verify__tags_rval_2_0_i1012);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localtailcall(mercury__vn_verify__tags_rval_2_0,
		STATIC(mercury__vn_verify__tags_rval_2_0));
Define_label(mercury__vn_verify__tags_rval_2_0_i1012);
	incr_sp_push_msg(3, "vn_verify__tags_rval");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 == ((Integer) 1)))
		GOTO_LABEL(mercury__vn_verify__tags_rval_2_0_i2);
	if (((Integer) r3 != ((Integer) 2)))
		GOTO_LABEL(mercury__vn_verify__tags_rval_2_0_i9);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__vn_verify__tags_rval_2_0,
		STATIC(mercury__vn_verify__tags_rval_2_0));
Define_label(mercury__vn_verify__tags_rval_2_0_i9);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__vn_verify__tags_rval_2_0,
		LABEL(mercury__vn_verify__tags_rval_2_0_i12),
		STATIC(mercury__vn_verify__tags_rval_2_0));
Define_label(mercury__vn_verify__tags_rval_2_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_rval_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_rval_2_0_i1);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__vn_verify__tags_rval_2_0,
		STATIC(mercury__vn_verify__tags_rval_2_0));
Define_label(mercury__vn_verify__tags_rval_2_0_i1011);
	incr_sp_push_msg(3, "vn_verify__tags_rval");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_verify__tags_rval_2_0_i16);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__vn_verify__tags_lval_2_0),
		STATIC(mercury__vn_verify__tags_rval_2_0));
Define_label(mercury__vn_verify__tags_rval_2_0_i16);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_verify__tags_rval_2_0_i2);
	r1 = string_const("found var in vn_verify__tags_rval", 33);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_verify__tags_rval_2_0_i2,
		STATIC(mercury__vn_verify__tags_rval_2_0));
	}
Define_label(mercury__vn_verify__tags_rval_2_0_i2);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_rval_2_0));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_verify__tags_rval_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module11)
	init_entry(mercury__vn_verify__tags_cond_5_0);
	init_label(mercury__vn_verify__tags_cond_5_0_i8);
	init_label(mercury__vn_verify__tags_cond_5_0_i10);
	init_label(mercury__vn_verify__tags_cond_5_0_i7);
	init_label(mercury__vn_verify__tags_cond_5_0_i11);
	init_label(mercury__vn_verify__tags_cond_5_0_i13);
	init_label(mercury__vn_verify__tags_cond_5_0_i16);
	init_label(mercury__vn_verify__tags_cond_5_0_i18);
	init_label(mercury__vn_verify__tags_cond_5_0_i15);
	init_label(mercury__vn_verify__tags_cond_5_0_i19);
	init_label(mercury__vn_verify__tags_cond_5_0_i1002);
	init_label(mercury__vn_verify__tags_cond_5_0_i2);
	init_label(mercury__vn_verify__tags_cond_5_0_i26);
	init_label(mercury__vn_verify__tags_cond_5_0_i25);
	init_label(mercury__vn_verify__tags_cond_5_0_i28);
	init_label(mercury__vn_verify__tags_cond_5_0_i30);
	init_label(mercury__vn_verify__tags_cond_5_0_i22);
	init_label(mercury__vn_verify__tags_cond_5_0_i32);
	init_label(mercury__vn_verify__tags_cond_5_0_i41);
	init_label(mercury__vn_verify__tags_cond_5_0_i43);
	init_label(mercury__vn_verify__tags_cond_5_0_i38);
	init_label(mercury__vn_verify__tags_cond_5_0_i44);
	init_label(mercury__vn_verify__tags_cond_5_0_i1);
	init_label(mercury__vn_verify__tags_cond_5_0_i1000);
BEGIN_CODE

/* code for predicate 'vn_verify__tags_cond'/5 in mode 0 */
Define_static(mercury__vn_verify__tags_cond_5_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1002);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)) != ((Integer) 3)))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1002);
	r4 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if (((Unsigned)(((Integer) r4 - ((Integer) 12))) > ((Integer) 12)))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1002);
	incr_sp_push_msg(6, "vn_verify__tags_cond");
	detstackvar(6) = (Integer) succip;
	{
	static const Word mercury_const_1[] = {
		((Integer) 7683)
	};
	if (!(((((Integer) 1) << ((Unsigned)(((Integer) r4 - ((Integer) 12))) % ((Integer) 32))) & (Integer) field(mktag(0), mkword(mktag(0), mercury_const_1), ((Unsigned)(((Integer) r4 - ((Integer) 12))) / ((Integer) 32))))))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i2);
	}
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	call_localret(STATIC(mercury__vn_verify__tags_is_base_2_0),
		mercury__vn_verify__tags_cond_5_0_i8,
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i7);
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_verify__tags_cond_5_0_i10,
		STATIC(mercury__vn_verify__tags_cond_5_0));
	}
Define_label(mercury__vn_verify__tags_cond_5_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i13);
Define_label(mercury__vn_verify__tags_cond_5_0_i7);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_cond_5_0_i11,
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1);
	r2 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
Define_label(mercury__vn_verify__tags_cond_5_0_i13);
	detstackvar(2) = (Integer) r2;
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r3;
	call_localret(STATIC(mercury__vn_verify__tags_is_base_2_0),
		mercury__vn_verify__tags_cond_5_0_i16,
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i16);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i15);
	{
	extern Word * mercury_data_llds__base_type_info_rval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_rval_0;
	}
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_verify__tags_cond_5_0_i18,
		STATIC(mercury__vn_verify__tags_cond_5_0));
	}
Define_label(mercury__vn_verify__tags_cond_5_0_i18);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	r2 = (Integer) r1;
	r3 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__vn_verify__tags_cond_5_0_i15);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_cond_5_0_i19,
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i19);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__vn_verify__tags_cond_5_0_i1002);
	incr_sp_push_msg(6, "vn_verify__tags_cond");
	detstackvar(6) = (Integer) succip;
Define_label(mercury__vn_verify__tags_cond_5_0_i2);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i22);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)) != ((Integer) 3)))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i22);
	r4 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if (((Integer) r4 != ((Integer) 10)))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i26);
	r5 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i25);
Define_label(mercury__vn_verify__tags_cond_5_0_i26);
	if (((Integer) r4 != ((Integer) 11)))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i22);
	r5 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
Define_label(mercury__vn_verify__tags_cond_5_0_i25);
	detstackvar(1) = (Integer) r5;
	localcall(mercury__vn_verify__tags_cond_5_0,
		LABEL(mercury__vn_verify__tags_cond_5_0_i28),
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i28);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1);
	r1 = (Integer) detstackvar(1);
	localcall(mercury__vn_verify__tags_cond_5_0,
		LABEL(mercury__vn_verify__tags_cond_5_0_i30),
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i30);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	if ((Integer) r1)
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1000);
	r1 = FALSE;
	proceed();
Define_label(mercury__vn_verify__tags_cond_5_0_i22);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i32);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)) != ((Integer) 2)))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i32);
	r4 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if (((Integer) r4 != ((Integer) 9)))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i32);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__vn_verify__tags_cond_5_0,
		LABEL(mercury__vn_verify__tags_cond_5_0_i30),
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i32);
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i38);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_cond_5_0_i41,
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i41);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1);
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__vn_verify__tags_cond_5_0_i43,
		STATIC(mercury__vn_verify__tags_cond_5_0));
	}
Define_label(mercury__vn_verify__tags_cond_5_0_i43);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__vn_verify__tags_cond_5_0_i38);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	call_localret(STATIC(mercury__vn_verify__tags_rval_2_0),
		mercury__vn_verify__tags_cond_5_0_i44,
		STATIC(mercury__vn_verify__tags_cond_5_0));
Define_label(mercury__vn_verify__tags_cond_5_0_i44);
	update_prof_current_proc(LABEL(mercury__vn_verify__tags_cond_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__vn_verify__tags_cond_5_0_i1);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__vn_verify__tags_cond_5_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__vn_verify__tags_cond_5_0_i1000);
	r1 = TRUE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_verify_module12)
	init_entry(mercury__vn_verify__tags_is_base_2_0);
	init_label(mercury__vn_verify__tags_is_base_2_0_i3);
	init_label(mercury__vn_verify__tags_is_base_2_0_i1);
BEGIN_CODE

/* code for predicate 'vn_verify__tags_is_base'/2 in mode 0 */
Define_static(mercury__vn_verify__tags_is_base_2_0);
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_verify__tags_is_base_2_0_i3);
	r2 = (Integer) r1;
	r1 = TRUE;
	proceed();
Define_label(mercury__vn_verify__tags_is_base_2_0_i3);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_verify__tags_is_base_2_0_i1);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)) != ((Integer) 2)))
		GOTO_LABEL(mercury__vn_verify__tags_is_base_2_0_i1);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 1)) != ((Integer) 1)))
		GOTO_LABEL(mercury__vn_verify__tags_is_base_2_0_i1);
	r2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = TRUE;
	proceed();
Define_label(mercury__vn_verify__tags_is_base_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__vn_verify_bunch_0(void)
{
	mercury__vn_verify_module0();
	mercury__vn_verify_module1();
	mercury__vn_verify_module2();
	mercury__vn_verify_module3();
	mercury__vn_verify_module4();
	mercury__vn_verify_module5();
	mercury__vn_verify_module6();
	mercury__vn_verify_module7();
	mercury__vn_verify_module8();
	mercury__vn_verify_module9();
	mercury__vn_verify_module10();
	mercury__vn_verify_module11();
	mercury__vn_verify_module12();
}

#endif

void mercury__vn_verify__init(void); /* suppress gcc warning */
void mercury__vn_verify__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__vn_verify_bunch_0();
#endif
}
