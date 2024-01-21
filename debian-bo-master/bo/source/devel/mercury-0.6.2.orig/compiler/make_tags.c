/*
** Automatically generated from `make_tags.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__make_tags__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__make_tags__assign_constructor_tags_4_0);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i2);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i3);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i6);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i8);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i5);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i11);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i13);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i10);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i18);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i15);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i25);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i26);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i27);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i30);
Declare_label(mercury__make_tags__assign_constructor_tags_4_0_i34);
Declare_static(mercury__make_tags__assign_enum_constants_4_0);
Declare_label(mercury__make_tags__assign_enum_constants_4_0_i4);
Declare_label(mercury__make_tags__assign_enum_constants_4_0_i5);
Declare_label(mercury__make_tags__assign_enum_constants_4_0_i1003);
Declare_static(mercury__make_tags__assign_simple_tags_5_0);
Declare_label(mercury__make_tags__assign_simple_tags_5_0_i4);
Declare_label(mercury__make_tags__assign_simple_tags_5_0_i5);
Declare_label(mercury__make_tags__assign_simple_tags_5_0_i11);
Declare_label(mercury__make_tags__assign_simple_tags_5_0_i1005);
Declare_static(mercury__make_tags__assign_complicated_tags_5_0);
Declare_label(mercury__make_tags__assign_complicated_tags_5_0_i4);
Declare_label(mercury__make_tags__assign_complicated_tags_5_0_i5);
Declare_label(mercury__make_tags__assign_complicated_tags_5_0_i1003);
Declare_static(mercury__make_tags__assign_complicated_constant_tags_5_0);
Declare_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i4);
Declare_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i5);
Declare_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i1003);
Declare_static(mercury__make_tags__ctors_are_all_constants_1_0);
Declare_label(mercury__make_tags__ctors_are_all_constants_1_0_i1003);
Declare_label(mercury__make_tags__ctors_are_all_constants_1_0_i1004);
Declare_static(mercury__make_tags__split_constructors_3_0);
Declare_label(mercury__make_tags__split_constructors_3_0_i7);
Declare_label(mercury__make_tags__split_constructors_3_0_i8);
Declare_label(mercury__make_tags__split_constructors_3_0_i3);
Declare_label(mercury__make_tags__split_constructors_3_0_i6);
Declare_label(mercury__make_tags__split_constructors_3_0_i1);

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data___base_type_info_string_0[];
extern Word * mercury_data_mercury_builtin__base_type_info_term_0[];
Word * mercury_data_make_tags__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data___base_type_info_string_0,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_term_0
};

BEGIN_MODULE(mercury__make_tags_module0)
	init_entry(mercury__make_tags__assign_constructor_tags_4_0);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i2);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i3);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i6);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i8);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i5);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i11);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i13);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i10);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i18);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i15);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i25);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i26);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i27);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i30);
	init_label(mercury__make_tags__assign_constructor_tags_4_0_i34);
BEGIN_CODE

/* code for predicate 'assign_constructor_tags'/4 in mode 0 */
Define_entry(mercury__make_tags__assign_constructor_tags_4_0);
	incr_sp_push_msg(5, "assign_constructor_tags");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 57);
	{
	Declare_entry(mercury__globals__lookup_int_option_3_0);
	call_localret(ENTRY(mercury__globals__lookup_int_option_3_0),
		mercury__make_tags__assign_constructor_tags_4_0_i2,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
	}
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i2);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	detstackvar(2) = (Integer) r1;
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_id_0[];
	r1 = (Integer) mercury_data_hlds_data__base_type_info_cons_id_0;
	}
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_tag_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_cons_tag_0;
	}
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__make_tags__assign_constructor_tags_4_0_i3,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
	}
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i3);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__make_tags__ctors_are_all_constants_1_0),
		mercury__make_tags__assign_constructor_tags_4_0_i6,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i6);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__make_tags__assign_constructor_tags_4_0_i5);
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 0);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__make_tags__assign_enum_constants_4_0),
		mercury__make_tags__assign_constructor_tags_4_0_i8,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i8);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	r2 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i5);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__type_util__type_is_no_tag_type_3_0);
	call_localret(ENTRY(mercury__type_util__type_is_no_tag_type_3_0),
		mercury__make_tags__assign_constructor_tags_4_0_i11,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
	}
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i11);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__make_tags__assign_constructor_tags_4_0_i10);
	detstackvar(4) = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__make_tags__assign_constructor_tags_4_0_i13,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
	}
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i13);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_id_0[];
	r1 = (Integer) mercury_data_hlds_data__base_type_info_cons_id_0;
	}
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_tag_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_cons_tag_0;
	}
	r3 = (Integer) detstackvar(3);
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r5;
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__make_tags__assign_constructor_tags_4_0_i34,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
	}
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i10);
	r1 = (Integer) detstackvar(3);
	if (((Integer) detstackvar(2) != ((Integer) 0)))
		GOTO_LABEL(mercury__make_tags__assign_constructor_tags_4_0_i15);
	if (((Integer) detstackvar(1) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_constructor_tags_4_0_i18);
	r2 = (Integer) field(mktag(1), (Integer) detstackvar(1), ((Integer) 1));
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_constructor_tags_4_0_i18);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 0);
	r3 = ((Integer) 1);
	call_localret(STATIC(mercury__make_tags__assign_simple_tags_5_0),
		mercury__make_tags__assign_constructor_tags_4_0_i34,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i18);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 0);
	r3 = ((Integer) 0);
	call_localret(STATIC(mercury__make_tags__assign_complicated_tags_5_0),
		mercury__make_tags__assign_constructor_tags_4_0_i34,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i15);
	detstackvar(3) = (Integer) r1;
	r1 = ((Integer) 2);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__int__pow_3_0);
	call_localret(ENTRY(mercury__int__pow_3_0),
		mercury__make_tags__assign_constructor_tags_4_0_i25,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
	}
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i25);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = ((Integer) r1 - ((Integer) 1));
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__make_tags__split_constructors_3_0),
		mercury__make_tags__assign_constructor_tags_4_0_i26,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i26);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_constructor_tags_4_0_i27);
	r3 = (Integer) detstackvar(1);
	r1 = (Integer) r2;
	r4 = (Integer) detstackvar(3);
	r2 = ((Integer) 0);
	call_localret(STATIC(mercury__make_tags__assign_simple_tags_5_0),
		mercury__make_tags__assign_constructor_tags_4_0_i34,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i27);
	detstackvar(2) = (Integer) r2;
	r2 = ((Integer) 0);
	r3 = ((Integer) 0);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__make_tags__assign_complicated_constant_tags_5_0),
		mercury__make_tags__assign_constructor_tags_4_0_i30,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i30);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = ((Integer) 1);
	r3 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__make_tags__assign_simple_tags_5_0),
		mercury__make_tags__assign_constructor_tags_4_0_i34,
		ENTRY(mercury__make_tags__assign_constructor_tags_4_0));
Define_label(mercury__make_tags__assign_constructor_tags_4_0_i34);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_constructor_tags_4_0));
	r2 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__make_tags_module1)
	init_entry(mercury__make_tags__assign_enum_constants_4_0);
	init_label(mercury__make_tags__assign_enum_constants_4_0_i4);
	init_label(mercury__make_tags__assign_enum_constants_4_0_i5);
	init_label(mercury__make_tags__assign_enum_constants_4_0_i1003);
BEGIN_CODE

/* code for predicate 'assign_enum_constants'/4 in mode 0 */
Define_static(mercury__make_tags__assign_enum_constants_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_enum_constants_4_0_i1003);
	incr_sp_push_msg(5, "assign_enum_constants");
	detstackvar(5) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_make_tags__common_0);
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__make_tags__assign_enum_constants_4_0_i4,
		STATIC(mercury__make_tags__assign_enum_constants_4_0));
	}
	}
Define_label(mercury__make_tags__assign_enum_constants_4_0_i4);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_enum_constants_4_0));
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_tag_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_cons_tag_0;
	}
	r3 = (Integer) detstackvar(2);
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(3);
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_id_0[];
	r1 = (Integer) mercury_data_hlds_data__base_type_info_cons_id_0;
	}
	tag_incr_hp(r5, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r5, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__make_tags__assign_enum_constants_4_0_i5,
		STATIC(mercury__make_tags__assign_enum_constants_4_0));
	}
Define_label(mercury__make_tags__assign_enum_constants_4_0_i5);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_enum_constants_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = ((Integer) detstackvar(1) + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__make_tags__assign_enum_constants_4_0,
		STATIC(mercury__make_tags__assign_enum_constants_4_0));
Define_label(mercury__make_tags__assign_enum_constants_4_0_i1003);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__make_tags_module2)
	init_entry(mercury__make_tags__assign_simple_tags_5_0);
	init_label(mercury__make_tags__assign_simple_tags_5_0_i4);
	init_label(mercury__make_tags__assign_simple_tags_5_0_i5);
	init_label(mercury__make_tags__assign_simple_tags_5_0_i11);
	init_label(mercury__make_tags__assign_simple_tags_5_0_i1005);
BEGIN_CODE

/* code for predicate 'assign_simple_tags'/5 in mode 0 */
Define_static(mercury__make_tags__assign_simple_tags_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_simple_tags_5_0_i1005);
	incr_sp_push_msg(7, "assign_simple_tags");
	detstackvar(7) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_make_tags__common_0);
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__make_tags__assign_simple_tags_5_0_i4,
		STATIC(mercury__make_tags__assign_simple_tags_5_0));
	}
	}
Define_label(mercury__make_tags__assign_simple_tags_5_0_i4);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_simple_tags_5_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	if (((Integer) detstackvar(2) != (Integer) detstackvar(3)))
		GOTO_LABEL(mercury__make_tags__assign_simple_tags_5_0_i5);
	r3 = (Integer) r2;
	if (((Integer) detstackvar(6) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_simple_tags_5_0_i5);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = ((Integer) 0);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	tailcall(STATIC(mercury__make_tags__assign_complicated_tags_5_0),
		STATIC(mercury__make_tags__assign_simple_tags_5_0));
Define_label(mercury__make_tags__assign_simple_tags_5_0_i5);
	r4 = (Integer) r2;
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_id_0[];
	r1 = (Integer) mercury_data_hlds_data__base_type_info_cons_id_0;
	}
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_tag_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_cons_tag_0;
	}
	r3 = (Integer) detstackvar(4);
	tag_incr_hp(r5, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r5, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__make_tags__assign_simple_tags_5_0_i11,
		STATIC(mercury__make_tags__assign_simple_tags_5_0));
	}
Define_label(mercury__make_tags__assign_simple_tags_5_0_i11);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_simple_tags_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = ((Integer) detstackvar(2) + ((Integer) 1));
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__make_tags__assign_simple_tags_5_0,
		STATIC(mercury__make_tags__assign_simple_tags_5_0));
Define_label(mercury__make_tags__assign_simple_tags_5_0_i1005);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__make_tags_module3)
	init_entry(mercury__make_tags__assign_complicated_tags_5_0);
	init_label(mercury__make_tags__assign_complicated_tags_5_0_i4);
	init_label(mercury__make_tags__assign_complicated_tags_5_0_i5);
	init_label(mercury__make_tags__assign_complicated_tags_5_0_i1003);
BEGIN_CODE

/* code for predicate 'assign_complicated_tags'/5 in mode 0 */
Define_static(mercury__make_tags__assign_complicated_tags_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_complicated_tags_5_0_i1003);
	incr_sp_push_msg(6, "assign_complicated_tags");
	detstackvar(6) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_make_tags__common_0);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__make_tags__assign_complicated_tags_5_0_i4,
		STATIC(mercury__make_tags__assign_complicated_tags_5_0));
	}
	}
Define_label(mercury__make_tags__assign_complicated_tags_5_0_i4);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_complicated_tags_5_0));
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_tag_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_cons_tag_0;
	}
	r3 = (Integer) detstackvar(3);
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(4);
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_id_0[];
	r1 = (Integer) mercury_data_hlds_data__base_type_info_cons_id_0;
	}
	tag_incr_hp(r5, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r5, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r5, ((Integer) 2)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__make_tags__assign_complicated_tags_5_0_i5,
		STATIC(mercury__make_tags__assign_complicated_tags_5_0));
	}
Define_label(mercury__make_tags__assign_complicated_tags_5_0_i5);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_complicated_tags_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = ((Integer) detstackvar(2) + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__make_tags__assign_complicated_tags_5_0,
		STATIC(mercury__make_tags__assign_complicated_tags_5_0));
Define_label(mercury__make_tags__assign_complicated_tags_5_0_i1003);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__make_tags_module4)
	init_entry(mercury__make_tags__assign_complicated_constant_tags_5_0);
	init_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i4);
	init_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i5);
	init_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i1003);
BEGIN_CODE

/* code for predicate 'assign_complicated_constant_tags'/5 in mode 0 */
Define_static(mercury__make_tags__assign_complicated_constant_tags_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__assign_complicated_constant_tags_5_0_i1003);
	incr_sp_push_msg(6, "assign_complicated_constant_tags");
	detstackvar(6) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_make_tags__common_0);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__make_tags__assign_complicated_constant_tags_5_0_i4,
		STATIC(mercury__make_tags__assign_complicated_constant_tags_5_0));
	}
	}
Define_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i4);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_complicated_constant_tags_5_0));
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_tag_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_cons_tag_0;
	}
	r3 = (Integer) detstackvar(3);
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(4);
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_id_0[];
	r1 = (Integer) mercury_data_hlds_data__base_type_info_cons_id_0;
	}
	tag_incr_hp(r5, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r5, ((Integer) 0)) = ((Integer) 6);
	field(mktag(3), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r5, ((Integer) 2)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__make_tags__assign_complicated_constant_tags_5_0_i5,
		STATIC(mercury__make_tags__assign_complicated_constant_tags_5_0));
	}
Define_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i5);
	update_prof_current_proc(LABEL(mercury__make_tags__assign_complicated_constant_tags_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = ((Integer) detstackvar(2) + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__make_tags__assign_complicated_constant_tags_5_0,
		STATIC(mercury__make_tags__assign_complicated_constant_tags_5_0));
Define_label(mercury__make_tags__assign_complicated_constant_tags_5_0_i1003);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__make_tags_module5)
	init_entry(mercury__make_tags__ctors_are_all_constants_1_0);
	init_label(mercury__make_tags__ctors_are_all_constants_1_0_i1003);
	init_label(mercury__make_tags__ctors_are_all_constants_1_0_i1004);
BEGIN_CODE

/* code for predicate 'ctors_are_all_constants'/1 in mode 0 */
Define_static(mercury__make_tags__ctors_are_all_constants_1_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__ctors_are_all_constants_1_0_i1003);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__ctors_are_all_constants_1_0_i1004);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localtailcall(mercury__make_tags__ctors_are_all_constants_1_0,
		STATIC(mercury__make_tags__ctors_are_all_constants_1_0));
Define_label(mercury__make_tags__ctors_are_all_constants_1_0_i1003);
	r1 = TRUE;
	proceed();
Define_label(mercury__make_tags__ctors_are_all_constants_1_0_i1004);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__make_tags_module6)
	init_entry(mercury__make_tags__split_constructors_3_0);
	init_label(mercury__make_tags__split_constructors_3_0_i7);
	init_label(mercury__make_tags__split_constructors_3_0_i8);
	init_label(mercury__make_tags__split_constructors_3_0_i3);
	init_label(mercury__make_tags__split_constructors_3_0_i6);
	init_label(mercury__make_tags__split_constructors_3_0_i1);
BEGIN_CODE

/* code for predicate 'split_constructors'/3 in mode 0 */
Define_static(mercury__make_tags__split_constructors_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__split_constructors_3_0_i1);
	r4 = (Integer) sp;
Define_label(mercury__make_tags__split_constructors_3_0_i7);
	while (1) {
	incr_sp_push_msg(2, "split_constructors");
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__make_tags__split_constructors_3_0_i8);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__make_tags__split_constructors_3_0_i3);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	GOTO_LABEL(mercury__make_tags__split_constructors_3_0_i6);
Define_label(mercury__make_tags__split_constructors_3_0_i3);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
Define_label(mercury__make_tags__split_constructors_3_0_i6);
	decr_sp_pop_msg(2);
	if (((Integer) sp > (Integer) r4))
		GOTO_LABEL(mercury__make_tags__split_constructors_3_0_i8);
	proceed();
Define_label(mercury__make_tags__split_constructors_3_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__make_tags_bunch_0(void)
{
	mercury__make_tags_module0();
	mercury__make_tags_module1();
	mercury__make_tags_module2();
	mercury__make_tags_module3();
	mercury__make_tags_module4();
	mercury__make_tags_module5();
	mercury__make_tags_module6();
}

#endif

void mercury__make_tags__init(void); /* suppress gcc warning */
void mercury__make_tags__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__make_tags_bunch_0();
#endif
}
