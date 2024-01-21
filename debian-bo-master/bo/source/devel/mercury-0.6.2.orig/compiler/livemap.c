/*
** Automatically generated from `livemap.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__livemap__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__livemap__build_2_0);
Declare_label(mercury__livemap__build_2_0_i2);
Declare_label(mercury__livemap__build_2_0_i3);
Declare_static(mercury__livemap__build_2_3_0);
Declare_label(mercury__livemap__build_2_3_0_i2);
Declare_label(mercury__livemap__build_2_3_0_i3);
Declare_label(mercury__livemap__build_2_3_0_i4);
Declare_label(mercury__livemap__build_2_3_0_i9);
Declare_label(mercury__livemap__build_2_3_0_i10);
Declare_label(mercury__livemap__build_2_3_0_i11);
Declare_label(mercury__livemap__build_2_3_0_i13);
Declare_label(mercury__livemap__build_2_3_0_i8);
Declare_static(mercury__livemap__equal_livemaps_keys_3_0);
Declare_label(mercury__livemap__equal_livemaps_keys_3_0_i4);
Declare_label(mercury__livemap__equal_livemaps_keys_3_0_i5);
Declare_label(mercury__livemap__equal_livemaps_keys_3_0_i6);
Declare_label(mercury__livemap__equal_livemaps_keys_3_0_i1003);
Declare_label(mercury__livemap__equal_livemaps_keys_3_0_i1);
Declare_static(mercury__livemap__build_livemap_6_0);
Declare_label(mercury__livemap__build_livemap_6_0_i4);
Declare_label(mercury__livemap__build_livemap_6_0_i1002);
Declare_static(mercury__livemap__build_livemap_instr_9_0);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1014);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1013);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1012);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1011);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1010);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1009);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1008);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1007);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1006);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1005);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1003);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i5);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i6);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i7);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i8);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i9);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i10);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i11);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i12);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i15);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i16);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i17);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i18);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i19);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i20);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i26);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i27);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i23);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i31);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i28);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i33);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i36);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i37);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i38);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i43);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i44);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i45);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i48);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i49);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i52);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i56);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i53);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i51);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i50);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i64);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i65);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i66);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i68);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i69);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i71);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i76);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1002);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i81);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i82);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1000);
Declare_label(mercury__livemap__build_livemap_instr_9_0_i1001);
Declare_static(mercury__livemap__look_for_livevals_7_0);
Declare_label(mercury__livemap__look_for_livevals_7_0_i2);
Declare_label(mercury__livemap__look_for_livevals_7_0_i7);
Declare_label(mercury__livemap__look_for_livevals_7_0_i8);
Declare_label(mercury__livemap__look_for_livevals_7_0_i9);
Declare_label(mercury__livemap__look_for_livevals_7_0_i3);
Declare_label(mercury__livemap__look_for_livevals_7_0_i13);
Declare_label(mercury__livemap__look_for_livevals_7_0_i10);
Declare_static(mercury__livemap__special_code_addr_2_0);
Declare_label(mercury__livemap__special_code_addr_2_0_i5);
Declare_label(mercury__livemap__special_code_addr_2_0_i6);
Declare_label(mercury__livemap__special_code_addr_2_0_i7);
Declare_label(mercury__livemap__special_code_addr_2_0_i4);
Declare_label(mercury__livemap__special_code_addr_2_0_i11);
Declare_label(mercury__livemap__special_code_addr_2_0_i12);
Declare_static(mercury__livemap__make_live_3_0);
Declare_label(mercury__livemap__make_live_3_0_i8);
Declare_label(mercury__livemap__make_live_3_0_i7);
Declare_label(mercury__livemap__make_live_3_0_i9);
Declare_label(mercury__livemap__make_live_3_0_i10);
Declare_label(mercury__livemap__make_live_3_0_i1002);
Declare_label(mercury__livemap__make_live_3_0_i14);
Declare_label(mercury__livemap__make_live_3_0_i17);
Declare_label(mercury__livemap__make_live_3_0_i18);
Declare_label(mercury__livemap__make_live_3_0_i19);
Declare_label(mercury__livemap__make_live_3_0_i13);
Declare_label(mercury__livemap__make_live_3_0_i21);
Declare_label(mercury__livemap__make_live_3_0_i1001);
Declare_static(mercury__livemap__insert_label_livevals_4_0);
Declare_label(mercury__livemap__insert_label_livevals_4_0_i6);
Declare_label(mercury__livemap__insert_label_livevals_4_0_i8);
Declare_label(mercury__livemap__insert_label_livevals_4_0_i9);
Declare_label(mercury__livemap__insert_label_livevals_4_0_i5);
Declare_label(mercury__livemap__insert_label_livevals_4_0_i1003);
Declare_static(mercury__livemap__insert_proper_livevals_3_0);
Declare_label(mercury__livemap__insert_proper_livevals_3_0_i1006);
Declare_label(mercury__livemap__insert_proper_livevals_3_0_i7);
Declare_label(mercury__livemap__insert_proper_livevals_3_0_i1004);
Define_extern_entry(mercury____Unify___livemap__livemap_0_0);
Define_extern_entry(mercury____Index___livemap__livemap_0_0);
Define_extern_entry(mercury____Compare___livemap__livemap_0_0);
Define_extern_entry(mercury____Unify___livemap__lvalset_0_0);
Define_extern_entry(mercury____Index___livemap__lvalset_0_0);
Define_extern_entry(mercury____Compare___livemap__lvalset_0_0);

extern Word * mercury_data_livemap__base_type_layout_livemap_0[];
Word * mercury_data_livemap__base_type_info_livemap_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___livemap__livemap_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___livemap__livemap_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___livemap__livemap_0_0),
	(Word *) (Integer) mercury_data_livemap__base_type_layout_livemap_0
};

extern Word * mercury_data_livemap__base_type_layout_lvalset_0[];
Word * mercury_data_livemap__base_type_info_lvalset_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___livemap__lvalset_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___livemap__lvalset_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___livemap__lvalset_0_0),
	(Word *) (Integer) mercury_data_livemap__base_type_layout_lvalset_0
};

extern Word * mercury_data_livemap__common_9[];
Word * mercury_data_livemap__base_type_layout_lvalset_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_9)
};

extern Word * mercury_data_livemap__common_11[];
Word * mercury_data_livemap__base_type_layout_livemap_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_11)
};

extern Word * mercury_data_set__base_type_info_set_1[];
extern Word * mercury_data_llds__base_type_info_lval_0[];
Word * mercury_data_livemap__common_0[] = {
	(Word *) (Integer) mercury_data_set__base_type_info_set_1,
	(Word *) (Integer) mercury_data_llds__base_type_info_lval_0
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_llds__base_type_info_instr_0[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_livemap__common_1[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_instr_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word mercury_data_livemap__common_2[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word mercury_data_livemap__common_3[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_livemap__common_4[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_3)
};

Word * mercury_data_livemap__common_5[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_4)
};

Word mercury_data_livemap__common_6[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 2)))
};

Word * mercury_data_livemap__common_7[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_6)
};

Word * mercury_data_livemap__common_8[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_livemap__common_7)
};

Word * mercury_data_livemap__common_9[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_llds__base_type_info_label_0[];
Word * mercury_data_livemap__common_10[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_label_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0)
};

Word * mercury_data_livemap__common_11[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_10)
};

BEGIN_MODULE(mercury__livemap_module0)
	init_entry(mercury__livemap__build_2_0);
	init_label(mercury__livemap__build_2_0_i2);
	init_label(mercury__livemap__build_2_0_i3);
BEGIN_CODE

/* code for predicate 'livemap__build'/2 in mode 0 */
Define_entry(mercury__livemap__build_2_0);
	incr_sp_push_msg(2, "livemap__build");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__livemap__build_2_0_i2,
		ENTRY(mercury__livemap__build_2_0));
	}
Define_label(mercury__livemap__build_2_0_i2);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_1);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__livemap__build_2_0_i3,
		ENTRY(mercury__livemap__build_2_0));
	}
Define_label(mercury__livemap__build_2_0_i3);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__livemap__build_2_3_0),
		ENTRY(mercury__livemap__build_2_0));
END_MODULE

BEGIN_MODULE(mercury__livemap_module1)
	init_entry(mercury__livemap__build_2_3_0);
	init_label(mercury__livemap__build_2_3_0_i2);
	init_label(mercury__livemap__build_2_3_0_i3);
	init_label(mercury__livemap__build_2_3_0_i4);
	init_label(mercury__livemap__build_2_3_0_i9);
	init_label(mercury__livemap__build_2_3_0_i10);
	init_label(mercury__livemap__build_2_3_0_i11);
	init_label(mercury__livemap__build_2_3_0_i13);
	init_label(mercury__livemap__build_2_3_0_i8);
BEGIN_CODE

/* code for predicate 'livemap__build_2'/3 in mode 0 */
Define_static(mercury__livemap__build_2_3_0);
	incr_sp_push_msg(5, "livemap__build_2");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__livemap__build_2_3_0_i2,
		STATIC(mercury__livemap__build_2_3_0));
	}
Define_label(mercury__livemap__build_2_3_0_i2);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = ((Integer) 1);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__livemap__build_livemap_6_0),
		mercury__livemap__build_2_3_0_i3,
		STATIC(mercury__livemap__build_2_3_0));
Define_label(mercury__livemap__build_2_3_0_i3);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_3_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__livemap__build_2_3_0_i4);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__livemap__build_2_3_0_i4);
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__livemap__build_2_3_0_i9,
		STATIC(mercury__livemap__build_2_3_0));
	}
Define_label(mercury__livemap__build_2_3_0_i9);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_3_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__livemap__build_2_3_0_i10,
		STATIC(mercury__livemap__build_2_3_0));
	}
Define_label(mercury__livemap__build_2_3_0_i10);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__livemap__build_2_3_0_i11,
		STATIC(mercury__livemap__build_2_3_0));
	}
Define_label(mercury__livemap__build_2_3_0_i11);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__livemap__build_2_3_0_i8);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__livemap__equal_livemaps_keys_3_0),
		mercury__livemap__build_2_3_0_i13,
		STATIC(mercury__livemap__build_2_3_0));
Define_label(mercury__livemap__build_2_3_0_i13);
	update_prof_current_proc(LABEL(mercury__livemap__build_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__livemap__build_2_3_0_i8);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__livemap__build_2_3_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__livemap__build_2_3_0,
		STATIC(mercury__livemap__build_2_3_0));
END_MODULE

BEGIN_MODULE(mercury__livemap_module2)
	init_entry(mercury__livemap__equal_livemaps_keys_3_0);
	init_label(mercury__livemap__equal_livemaps_keys_3_0_i4);
	init_label(mercury__livemap__equal_livemaps_keys_3_0_i5);
	init_label(mercury__livemap__equal_livemaps_keys_3_0_i6);
	init_label(mercury__livemap__equal_livemaps_keys_3_0_i1003);
	init_label(mercury__livemap__equal_livemaps_keys_3_0_i1);
BEGIN_CODE

/* code for predicate 'livemap__equal_livemaps_keys'/3 in mode 0 */
Define_static(mercury__livemap__equal_livemaps_keys_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__livemap__equal_livemaps_keys_3_0_i1003);
	incr_sp_push_msg(5, "livemap__equal_livemaps_keys");
	detstackvar(5) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__livemap__equal_livemaps_keys_3_0_i4,
		STATIC(mercury__livemap__equal_livemaps_keys_3_0));
	}
Define_label(mercury__livemap__equal_livemaps_keys_3_0_i4);
	update_prof_current_proc(LABEL(mercury__livemap__equal_livemaps_keys_3_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__livemap__equal_livemaps_keys_3_0_i5,
		STATIC(mercury__livemap__equal_livemaps_keys_3_0));
	}
Define_label(mercury__livemap__equal_livemaps_keys_3_0_i5);
	update_prof_current_proc(LABEL(mercury__livemap__equal_livemaps_keys_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__equal_2_0);
	call_localret(ENTRY(mercury__set__equal_2_0),
		mercury__livemap__equal_livemaps_keys_3_0_i6,
		STATIC(mercury__livemap__equal_livemaps_keys_3_0));
	}
Define_label(mercury__livemap__equal_livemaps_keys_3_0_i6);
	update_prof_current_proc(LABEL(mercury__livemap__equal_livemaps_keys_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__livemap__equal_livemaps_keys_3_0_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__livemap__equal_livemaps_keys_3_0,
		STATIC(mercury__livemap__equal_livemaps_keys_3_0));
Define_label(mercury__livemap__equal_livemaps_keys_3_0_i1003);
	r1 = TRUE;
	proceed();
Define_label(mercury__livemap__equal_livemaps_keys_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module3)
	init_entry(mercury__livemap__build_livemap_6_0);
	init_label(mercury__livemap__build_livemap_6_0_i4);
	init_label(mercury__livemap__build_livemap_6_0_i1002);
BEGIN_CODE

/* code for predicate 'livemap__build_livemap'/6 in mode 0 */
Define_static(mercury__livemap__build_livemap_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__livemap__build_livemap_6_0_i1002);
	r5 = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	incr_sp_push_msg(1, "livemap__build_livemap");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__livemap__build_livemap_instr_9_0),
		mercury__livemap__build_livemap_6_0_i4,
		STATIC(mercury__livemap__build_livemap_6_0));
Define_label(mercury__livemap__build_livemap_6_0_i4);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_6_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	localtailcall(mercury__livemap__build_livemap_6_0,
		STATIC(mercury__livemap__build_livemap_6_0));
Define_label(mercury__livemap__build_livemap_6_0_i1002);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module4)
	init_entry(mercury__livemap__build_livemap_instr_9_0);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1014);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1013);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1012);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1011);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1010);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1009);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1008);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1007);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1006);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1005);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1003);
	init_label(mercury__livemap__build_livemap_instr_9_0_i5);
	init_label(mercury__livemap__build_livemap_instr_9_0_i6);
	init_label(mercury__livemap__build_livemap_instr_9_0_i7);
	init_label(mercury__livemap__build_livemap_instr_9_0_i8);
	init_label(mercury__livemap__build_livemap_instr_9_0_i9);
	init_label(mercury__livemap__build_livemap_instr_9_0_i10);
	init_label(mercury__livemap__build_livemap_instr_9_0_i11);
	init_label(mercury__livemap__build_livemap_instr_9_0_i12);
	init_label(mercury__livemap__build_livemap_instr_9_0_i15);
	init_label(mercury__livemap__build_livemap_instr_9_0_i16);
	init_label(mercury__livemap__build_livemap_instr_9_0_i17);
	init_label(mercury__livemap__build_livemap_instr_9_0_i18);
	init_label(mercury__livemap__build_livemap_instr_9_0_i19);
	init_label(mercury__livemap__build_livemap_instr_9_0_i20);
	init_label(mercury__livemap__build_livemap_instr_9_0_i26);
	init_label(mercury__livemap__build_livemap_instr_9_0_i27);
	init_label(mercury__livemap__build_livemap_instr_9_0_i23);
	init_label(mercury__livemap__build_livemap_instr_9_0_i31);
	init_label(mercury__livemap__build_livemap_instr_9_0_i28);
	init_label(mercury__livemap__build_livemap_instr_9_0_i33);
	init_label(mercury__livemap__build_livemap_instr_9_0_i36);
	init_label(mercury__livemap__build_livemap_instr_9_0_i37);
	init_label(mercury__livemap__build_livemap_instr_9_0_i38);
	init_label(mercury__livemap__build_livemap_instr_9_0_i43);
	init_label(mercury__livemap__build_livemap_instr_9_0_i44);
	init_label(mercury__livemap__build_livemap_instr_9_0_i45);
	init_label(mercury__livemap__build_livemap_instr_9_0_i48);
	init_label(mercury__livemap__build_livemap_instr_9_0_i49);
	init_label(mercury__livemap__build_livemap_instr_9_0_i52);
	init_label(mercury__livemap__build_livemap_instr_9_0_i56);
	init_label(mercury__livemap__build_livemap_instr_9_0_i53);
	init_label(mercury__livemap__build_livemap_instr_9_0_i51);
	init_label(mercury__livemap__build_livemap_instr_9_0_i50);
	init_label(mercury__livemap__build_livemap_instr_9_0_i64);
	init_label(mercury__livemap__build_livemap_instr_9_0_i65);
	init_label(mercury__livemap__build_livemap_instr_9_0_i66);
	init_label(mercury__livemap__build_livemap_instr_9_0_i68);
	init_label(mercury__livemap__build_livemap_instr_9_0_i69);
	init_label(mercury__livemap__build_livemap_instr_9_0_i71);
	init_label(mercury__livemap__build_livemap_instr_9_0_i76);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1002);
	init_label(mercury__livemap__build_livemap_instr_9_0_i81);
	init_label(mercury__livemap__build_livemap_instr_9_0_i82);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1000);
	init_label(mercury__livemap__build_livemap_instr_9_0_i1001);
BEGIN_CODE

/* code for predicate 'livemap__build_livemap_instr'/9 in mode 0 */
Define_static(mercury__livemap__build_livemap_instr_9_0);
	r6 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r6) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i1002);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r6, ((Integer) 0)),
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1014) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1013) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1012) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1000) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1000) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1011) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1010) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1009) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1001) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1008) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1007) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1006) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1005) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1006) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1003) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1000) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1000) AND
		LABEL(mercury__livemap__build_livemap_instr_9_0_i1001));
Define_label(mercury__livemap__build_livemap_instr_9_0_i1014);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i5);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1013);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i7);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1012);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i11);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1011);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i15);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1010);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i17);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1009);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i43);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1008);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i48);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1007);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i64);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1006);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i68);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1005);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i71);
Define_label(mercury__livemap__build_livemap_instr_9_0_i1003);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i76);
Define_label(mercury__livemap__build_livemap_instr_9_0_i5);
	r1 = string_const("block found in backward scan in build_livemap", 45);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__livemap__build_livemap_instr_9_0_i6,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i6);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i7);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 2));
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__set__delete_3_0);
	call_localret(ENTRY(mercury__set__delete_3_0),
		mercury__livemap__build_livemap_instr_9_0_i8,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i8);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__opt_util__lval_access_rvals_2_0);
	call_localret(ENTRY(mercury__opt_util__lval_access_rvals_2_0),
		mercury__livemap__build_livemap_instr_9_0_i9,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i9);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__livemap__make_live_3_0),
		mercury__livemap__build_livemap_instr_9_0_i10,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i10);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i11);
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = string_const("call", 4);
	r4 = ((Integer) 0);
	call_localret(STATIC(mercury__livemap__look_for_livevals_7_0),
		mercury__livemap__build_livemap_instr_9_0_i12,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i12);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i15);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	r3 = (Integer) r5;
	r4 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	r5 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__livemap__build_livemap_instr_9_0_i16,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i16);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i17);
	r1 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	detstackvar(5) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__opt_util__livevals_addr_2_0);
	call_localret(ENTRY(mercury__opt_util__livevals_addr_2_0),
		mercury__livemap__build_livemap_instr_9_0_i18,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i18);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = string_const("goto", 4);
	call_localret(STATIC(mercury__livemap__look_for_livevals_7_0),
		mercury__livemap__build_livemap_instr_9_0_i19,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i19);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i20);
	r5 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i36);
Define_label(mercury__livemap__build_livemap_instr_9_0_i20);
	if ((tag((Integer) detstackvar(5)) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i23);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) detstackvar(5), ((Integer) 0));
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__livemap__build_livemap_instr_9_0_i26,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i26);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__livemap__insert_label_livevals_4_0),
		mercury__livemap__build_livemap_instr_9_0_i27,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i27);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i36);
Define_label(mercury__livemap__build_livemap_instr_9_0_i23);
	if (((Integer) detstackvar(5) != (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i31);
	r5 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i36);
Define_label(mercury__livemap__build_livemap_instr_9_0_i31);
	if (((Integer) detstackvar(5) != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i28);
	r5 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i36);
Define_label(mercury__livemap__build_livemap_instr_9_0_i28);
	detstackvar(1) = (Integer) r1;
	r1 = string_const("unknown label type in build_livemap", 35);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__livemap__build_livemap_instr_9_0_i33,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i33);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(2);
Define_label(mercury__livemap__build_livemap_instr_9_0_i36);
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	call_localret(STATIC(mercury__livemap__special_code_addr_2_0),
		mercury__livemap__build_livemap_instr_9_0_i37,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i37);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i38);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__livemap__build_livemap_instr_9_0_i10,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i38);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i43);
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__livemap__build_livemap_instr_9_0_i44,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i44);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__livemap__make_live_3_0),
		mercury__livemap__build_livemap_instr_9_0_i45,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i45);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__livemap__insert_label_livevals_4_0),
		mercury__livemap__build_livemap_instr_9_0_i10,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i48);
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 2));
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = string_const("if_val", 6);
	r4 = ((Integer) 1);
	call_localret(STATIC(mercury__livemap__look_for_livevals_7_0),
		mercury__livemap__build_livemap_instr_9_0_i49,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i49);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	if (((Integer) r3 == ((Integer) 0)))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i51);
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__livemap__make_live_3_0),
		mercury__livemap__build_livemap_instr_9_0_i52,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i52);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	if ((tag((Integer) detstackvar(2)) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i53);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) detstackvar(2), ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__livemap__insert_label_livevals_4_0),
		mercury__livemap__build_livemap_instr_9_0_i56,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i56);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i50);
Define_label(mercury__livemap__build_livemap_instr_9_0_i53);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i50);
Define_label(mercury__livemap__build_livemap_instr_9_0_i51);
	r5 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
Define_label(mercury__livemap__build_livemap_instr_9_0_i50);
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	call_localret(STATIC(mercury__livemap__special_code_addr_2_0),
		mercury__livemap__build_livemap_instr_9_0_i37,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i64);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 3));
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__set__delete_3_0);
	call_localret(ENTRY(mercury__set__delete_3_0),
		mercury__livemap__build_livemap_instr_9_0_i65,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i65);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__opt_util__lval_access_rvals_2_0);
	call_localret(ENTRY(mercury__opt_util__lval_access_rvals_2_0),
		mercury__livemap__build_livemap_instr_9_0_i66,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i66);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__livemap__make_live_3_0),
		mercury__livemap__build_livemap_instr_9_0_i10,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i68);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	r1 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	{
	Declare_entry(mercury__opt_util__lval_access_rvals_2_0);
	call_localret(ENTRY(mercury__opt_util__lval_access_rvals_2_0),
		mercury__livemap__build_livemap_instr_9_0_i69,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i69);
	update_prof_current_proc(LABEL(mercury__livemap__build_livemap_instr_9_0));
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__livemap__make_live_3_0),
		mercury__livemap__build_livemap_instr_9_0_i10,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i71);
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__livemap__make_live_3_0),
		mercury__livemap__build_livemap_instr_9_0_i10,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i76);
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__livemap__make_live_3_0),
		mercury__livemap__build_livemap_instr_9_0_i10,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
Define_label(mercury__livemap__build_livemap_instr_9_0_i1002);
	incr_sp_push_msg(6, "livemap__build_livemap_instr");
	detstackvar(6) = (Integer) succip;
	if ((tag((Integer) r6) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i81);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i81);
	if ((tag((Integer) r6) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__livemap__build_livemap_instr_9_0_i82);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i82);
	r1 = string_const("livevals found in backward scan in build_livemap", 48);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__livemap__build_livemap_instr_9_0_i6,
		STATIC(mercury__livemap__build_livemap_instr_9_0));
	}
Define_label(mercury__livemap__build_livemap_instr_9_0_i1000);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	proceed();
Define_label(mercury__livemap__build_livemap_instr_9_0_i1001);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = ((Integer) 0);
	r4 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module5)
	init_entry(mercury__livemap__look_for_livevals_7_0);
	init_label(mercury__livemap__look_for_livevals_7_0_i2);
	init_label(mercury__livemap__look_for_livevals_7_0_i7);
	init_label(mercury__livemap__look_for_livevals_7_0_i8);
	init_label(mercury__livemap__look_for_livevals_7_0_i9);
	init_label(mercury__livemap__look_for_livevals_7_0_i3);
	init_label(mercury__livemap__look_for_livevals_7_0_i13);
	init_label(mercury__livemap__look_for_livevals_7_0_i10);
BEGIN_CODE

/* code for predicate 'livemap__look_for_livevals'/7 in mode 0 */
Define_static(mercury__livemap__look_for_livevals_7_0);
	incr_sp_push_msg(4, "livemap__look_for_livevals");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__opt_util__skip_comments_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_2_0),
		mercury__livemap__look_for_livevals_7_0_i2,
		STATIC(mercury__livemap__look_for_livevals_7_0));
	}
Define_label(mercury__livemap__look_for_livevals_7_0_i2);
	update_prof_current_proc(LABEL(mercury__livemap__look_for_livevals_7_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__livemap__look_for_livevals_7_0_i3);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r2) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__livemap__look_for_livevals_7_0_i3);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	r2 = (Integer) field(mktag(2), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__livemap__look_for_livevals_7_0_i7,
		STATIC(mercury__livemap__look_for_livevals_7_0));
	}
Define_label(mercury__livemap__look_for_livevals_7_0_i7);
	update_prof_current_proc(LABEL(mercury__livemap__look_for_livevals_7_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__livemap__look_for_livevals_7_0_i8,
		STATIC(mercury__livemap__look_for_livevals_7_0));
	}
Define_label(mercury__livemap__look_for_livevals_7_0_i8);
	update_prof_current_proc(LABEL(mercury__livemap__look_for_livevals_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__livemap__insert_proper_livevals_3_0),
		mercury__livemap__look_for_livevals_7_0_i9,
		STATIC(mercury__livemap__look_for_livevals_7_0));
Define_label(mercury__livemap__look_for_livevals_7_0_i9);
	update_prof_current_proc(LABEL(mercury__livemap__look_for_livevals_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__livemap__look_for_livevals_7_0_i3);
	if (((Integer) detstackvar(3) != ((Integer) 0)))
		GOTO_LABEL(mercury__livemap__look_for_livevals_7_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = string_const(" not preceded by livevals", 25);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__livemap__look_for_livevals_7_0_i13,
		STATIC(mercury__livemap__look_for_livevals_7_0));
	}
Define_label(mercury__livemap__look_for_livevals_7_0_i13);
	update_prof_current_proc(LABEL(mercury__livemap__look_for_livevals_7_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__livemap__look_for_livevals_7_0));
	}
Define_label(mercury__livemap__look_for_livevals_7_0_i10);
	r2 = (Integer) detstackvar(1);
	r3 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module6)
	init_entry(mercury__livemap__special_code_addr_2_0);
	init_label(mercury__livemap__special_code_addr_2_0_i5);
	init_label(mercury__livemap__special_code_addr_2_0_i6);
	init_label(mercury__livemap__special_code_addr_2_0_i7);
	init_label(mercury__livemap__special_code_addr_2_0_i4);
	init_label(mercury__livemap__special_code_addr_2_0_i11);
	init_label(mercury__livemap__special_code_addr_2_0_i12);
BEGIN_CODE

/* code for predicate 'livemap__special_code_addr'/2 in mode 0 */
Define_static(mercury__livemap__special_code_addr_2_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__livemap__special_code_addr_2_0_i4);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__livemap__special_code_addr_2_0_i5) AND
		LABEL(mercury__livemap__special_code_addr_2_0_i6) AND
		LABEL(mercury__livemap__special_code_addr_2_0_i7) AND
		LABEL(mercury__livemap__special_code_addr_2_0_i7) AND
		LABEL(mercury__livemap__special_code_addr_2_0_i7) AND
		LABEL(mercury__livemap__special_code_addr_2_0_i7));
Define_label(mercury__livemap__special_code_addr_2_0_i5);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_livemap__common_2);
	proceed();
Define_label(mercury__livemap__special_code_addr_2_0_i6);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_livemap__common_5);
	proceed();
Define_label(mercury__livemap__special_code_addr_2_0_i7);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__livemap__special_code_addr_2_0_i4);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__livemap__special_code_addr_2_0_i11);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__livemap__special_code_addr_2_0_i11);
	if (((Integer) r2 != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__livemap__special_code_addr_2_0_i12);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__livemap__special_code_addr_2_0_i12);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_livemap__common_8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module7)
	init_entry(mercury__livemap__make_live_3_0);
	init_label(mercury__livemap__make_live_3_0_i8);
	init_label(mercury__livemap__make_live_3_0_i7);
	init_label(mercury__livemap__make_live_3_0_i9);
	init_label(mercury__livemap__make_live_3_0_i10);
	init_label(mercury__livemap__make_live_3_0_i1002);
	init_label(mercury__livemap__make_live_3_0_i14);
	init_label(mercury__livemap__make_live_3_0_i17);
	init_label(mercury__livemap__make_live_3_0_i18);
	init_label(mercury__livemap__make_live_3_0_i19);
	init_label(mercury__livemap__make_live_3_0_i13);
	init_label(mercury__livemap__make_live_3_0_i21);
	init_label(mercury__livemap__make_live_3_0_i1001);
BEGIN_CODE

/* code for predicate 'livemap__make_live'/3 in mode 0 */
Define_static(mercury__livemap__make_live_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i1001);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i1002);
	r4 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 0));
	incr_sp_push_msg(3, "livemap__make_live");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r4 != ((Integer) 0)))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r3, ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	localcall(mercury__livemap__make_live_3_0,
		LABEL(mercury__livemap__make_live_3_0_i8),
		STATIC(mercury__livemap__make_live_3_0));
Define_label(mercury__livemap__make_live_3_0_i8);
	update_prof_current_proc(LABEL(mercury__livemap__make_live_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__livemap__make_live_3_0,
		STATIC(mercury__livemap__make_live_3_0));
Define_label(mercury__livemap__make_live_3_0_i7);
	if (((Integer) r4 != ((Integer) 1)))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i9);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__livemap__make_live_3_0,
		STATIC(mercury__livemap__make_live_3_0));
Define_label(mercury__livemap__make_live_3_0_i9);
	if (((Integer) r4 != ((Integer) 2)))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i10);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r3, ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	localcall(mercury__livemap__make_live_3_0,
		LABEL(mercury__livemap__make_live_3_0_i8),
		STATIC(mercury__livemap__make_live_3_0));
Define_label(mercury__livemap__make_live_3_0_i10);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r3, ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r3, ((Integer) 3));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	localcall(mercury__livemap__make_live_3_0,
		LABEL(mercury__livemap__make_live_3_0_i8),
		STATIC(mercury__livemap__make_live_3_0));
	}
Define_label(mercury__livemap__make_live_3_0_i1002);
	incr_sp_push_msg(3, "livemap__make_live");
	detstackvar(3) = (Integer) succip;
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i13);
	r4 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) r4) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i14);
	if (((Integer) field(mktag(3), (Integer) r4, ((Integer) 0)) != ((Integer) 6)))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i14);
	r3 = (Integer) r2;
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	GOTO_LABEL(mercury__livemap__make_live_3_0_i18);
Define_label(mercury__livemap__make_live_3_0_i14);
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r4;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	r3 = (Integer) r4;
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__livemap__make_live_3_0_i17,
		STATIC(mercury__livemap__make_live_3_0));
	}
Define_label(mercury__livemap__make_live_3_0_i17);
	update_prof_current_proc(LABEL(mercury__livemap__make_live_3_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
Define_label(mercury__livemap__make_live_3_0_i18);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__opt_util__lval_access_rvals_2_0);
	call_localret(ENTRY(mercury__opt_util__lval_access_rvals_2_0),
		mercury__livemap__make_live_3_0_i19,
		STATIC(mercury__livemap__make_live_3_0));
	}
Define_label(mercury__livemap__make_live_3_0_i19);
	update_prof_current_proc(LABEL(mercury__livemap__make_live_3_0));
	r2 = (Integer) detstackvar(2);
	localcall(mercury__livemap__make_live_3_0,
		LABEL(mercury__livemap__make_live_3_0_i8),
		STATIC(mercury__livemap__make_live_3_0));
Define_label(mercury__livemap__make_live_3_0_i13);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__livemap__make_live_3_0_i21);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = string_const("var rval should not propagate to the optimizer", 46);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__livemap__make_live_3_0_i8,
		STATIC(mercury__livemap__make_live_3_0));
	}
Define_label(mercury__livemap__make_live_3_0_i21);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__livemap__make_live_3_0,
		STATIC(mercury__livemap__make_live_3_0));
Define_label(mercury__livemap__make_live_3_0_i1001);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module8)
	init_entry(mercury__livemap__insert_label_livevals_4_0);
	init_label(mercury__livemap__insert_label_livevals_4_0_i6);
	init_label(mercury__livemap__insert_label_livevals_4_0_i8);
	init_label(mercury__livemap__insert_label_livevals_4_0_i9);
	init_label(mercury__livemap__insert_label_livevals_4_0_i5);
	init_label(mercury__livemap__insert_label_livevals_4_0_i1003);
BEGIN_CODE

/* code for predicate 'livemap__insert_label_livevals'/4 in mode 0 */
Define_static(mercury__livemap__insert_label_livevals_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__livemap__insert_label_livevals_4_0_i1003);
	incr_sp_push_msg(4, "livemap__insert_label_livevals");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r2;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__livemap__insert_label_livevals_4_0_i6,
		STATIC(mercury__livemap__insert_label_livevals_4_0));
	}
Define_label(mercury__livemap__insert_label_livevals_4_0_i6);
	update_prof_current_proc(LABEL(mercury__livemap__insert_label_livevals_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__livemap__insert_label_livevals_4_0_i5);
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__livemap__insert_label_livevals_4_0_i8,
		STATIC(mercury__livemap__insert_label_livevals_4_0));
	}
Define_label(mercury__livemap__insert_label_livevals_4_0_i8);
	update_prof_current_proc(LABEL(mercury__livemap__insert_label_livevals_4_0));
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__livemap__insert_proper_livevals_3_0),
		mercury__livemap__insert_label_livevals_4_0_i9,
		STATIC(mercury__livemap__insert_label_livevals_4_0));
Define_label(mercury__livemap__insert_label_livevals_4_0_i9);
	update_prof_current_proc(LABEL(mercury__livemap__insert_label_livevals_4_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__livemap__insert_label_livevals_4_0,
		STATIC(mercury__livemap__insert_label_livevals_4_0));
Define_label(mercury__livemap__insert_label_livevals_4_0_i5);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__livemap__insert_label_livevals_4_0,
		STATIC(mercury__livemap__insert_label_livevals_4_0));
Define_label(mercury__livemap__insert_label_livevals_4_0_i1003);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module9)
	init_entry(mercury__livemap__insert_proper_livevals_3_0);
	init_label(mercury__livemap__insert_proper_livevals_3_0_i1006);
	init_label(mercury__livemap__insert_proper_livevals_3_0_i7);
	init_label(mercury__livemap__insert_proper_livevals_3_0_i1004);
BEGIN_CODE

/* code for predicate 'livemap__insert_proper_livevals'/3 in mode 0 */
Define_static(mercury__livemap__insert_proper_livevals_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__livemap__insert_proper_livevals_3_0_i1004);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r4) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__livemap__insert_proper_livevals_3_0_i1006);
	if (((Integer) field(mktag(3), (Integer) r4, ((Integer) 0)) != ((Integer) 6)))
		GOTO_LABEL(mercury__livemap__insert_proper_livevals_3_0_i1006);
	r1 = (Integer) r3;
	localtailcall(mercury__livemap__insert_proper_livevals_3_0,
		STATIC(mercury__livemap__insert_proper_livevals_3_0));
Define_label(mercury__livemap__insert_proper_livevals_3_0_i1006);
	incr_sp_push_msg(2, "livemap__insert_proper_livevals");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	r3 = (Integer) r4;
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__livemap__insert_proper_livevals_3_0_i7,
		STATIC(mercury__livemap__insert_proper_livevals_3_0));
	}
Define_label(mercury__livemap__insert_proper_livevals_3_0_i7);
	update_prof_current_proc(LABEL(mercury__livemap__insert_proper_livevals_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__livemap__insert_proper_livevals_3_0,
		STATIC(mercury__livemap__insert_proper_livevals_3_0));
Define_label(mercury__livemap__insert_proper_livevals_3_0_i1004);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__livemap_module10)
	init_entry(mercury____Unify___livemap__livemap_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___livemap__livemap_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___livemap__livemap_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__livemap_module11)
	init_entry(mercury____Index___livemap__livemap_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___livemap__livemap_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	{
	Declare_entry(mercury____Index___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Index___tree234__tree234_2_0),
		ENTRY(mercury____Index___livemap__livemap_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__livemap_module12)
	init_entry(mercury____Compare___livemap__livemap_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___livemap__livemap_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_livemap__common_0);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___livemap__livemap_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__livemap_module13)
	init_entry(mercury____Unify___livemap__lvalset_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___livemap__lvalset_0_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury____Unify___set__set_1_0);
	tailcall(ENTRY(mercury____Unify___set__set_1_0),
		ENTRY(mercury____Unify___livemap__lvalset_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__livemap_module14)
	init_entry(mercury____Index___livemap__lvalset_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___livemap__lvalset_0_0);
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury____Index___set__set_1_0);
	tailcall(ENTRY(mercury____Index___set__set_1_0),
		ENTRY(mercury____Index___livemap__lvalset_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__livemap_module15)
	init_entry(mercury____Compare___livemap__lvalset_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___livemap__lvalset_0_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_lval_0;
	{
	Declare_entry(mercury____Compare___set__set_1_0);
	tailcall(ENTRY(mercury____Compare___set__set_1_0),
		ENTRY(mercury____Compare___livemap__lvalset_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__livemap_bunch_0(void)
{
	mercury__livemap_module0();
	mercury__livemap_module1();
	mercury__livemap_module2();
	mercury__livemap_module3();
	mercury__livemap_module4();
	mercury__livemap_module5();
	mercury__livemap_module6();
	mercury__livemap_module7();
	mercury__livemap_module8();
	mercury__livemap_module9();
	mercury__livemap_module10();
	mercury__livemap_module11();
	mercury__livemap_module12();
	mercury__livemap_module13();
	mercury__livemap_module14();
	mercury__livemap_module15();
}

#endif

void mercury__livemap__init(void); /* suppress gcc warning */
void mercury__livemap__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__livemap_bunch_0();
#endif
}
