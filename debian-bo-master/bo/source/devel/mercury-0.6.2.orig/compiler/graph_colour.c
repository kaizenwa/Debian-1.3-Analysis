/*
** Automatically generated from `graph_colour.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__graph_colour__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__graph_colour__group_elements_2_0);
Declare_label(mercury__graph_colour__group_elements_2_0_i2);
Declare_label(mercury__graph_colour__group_elements_2_0_i3);
Declare_label(mercury__graph_colour__group_elements_2_0_i4);
Declare_label(mercury__graph_colour__group_elements_2_0_i5);
Declare_label(mercury__graph_colour__group_elements_2_0_i6);
Declare_static(mercury__graph_colour__find_all_colours_3_0);
Declare_label(mercury__graph_colour__find_all_colours_3_0_i1000);
Declare_label(mercury__graph_colour__find_all_colours_3_0_i5);
Declare_label(mercury__graph_colour__find_all_colours_3_0_i6);
Declare_label(mercury__graph_colour__find_all_colours_3_0_i7);
Declare_static(mercury__graph_colour__next_colour_4_0);
Declare_label(mercury__graph_colour__next_colour_4_0_i7);
Declare_label(mercury__graph_colour__next_colour_4_0_i11);
Declare_label(mercury__graph_colour__next_colour_4_0_i8);
Declare_label(mercury__graph_colour__next_colour_4_0_i12);
Declare_label(mercury__graph_colour__next_colour_4_0_i13);
Declare_label(mercury__graph_colour__next_colour_4_0_i14);
Declare_label(mercury__graph_colour__next_colour_4_0_i24);
Declare_label(mercury__graph_colour__next_colour_4_0_i26);
Declare_label(mercury__graph_colour__next_colour_4_0_i27);
Declare_label(mercury__graph_colour__next_colour_4_0_i21);
Declare_label(mercury__graph_colour__next_colour_4_0_i28);
Declare_label(mercury__graph_colour__next_colour_4_0_i15);
Declare_label(mercury__graph_colour__next_colour_4_0_i30);
Declare_label(mercury__graph_colour__next_colour_4_0_i31);
Declare_label(mercury__graph_colour__next_colour_4_0_i32);
Declare_label(mercury__graph_colour__next_colour_4_0_i2);
Declare_label(mercury__graph_colour__next_colour_4_0_i33);
Declare_static(mercury__graph_colour__divide_constraints_6_0);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i4);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i7);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i9);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i12);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i11);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i14);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i15);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i6);
Declare_label(mercury__graph_colour__divide_constraints_6_0_i1005);

BEGIN_MODULE(mercury__graph_colour_module0)
	init_entry(mercury__graph_colour__group_elements_2_0);
	init_label(mercury__graph_colour__group_elements_2_0_i2);
	init_label(mercury__graph_colour__group_elements_2_0_i3);
	init_label(mercury__graph_colour__group_elements_2_0_i4);
	init_label(mercury__graph_colour__group_elements_2_0_i5);
	init_label(mercury__graph_colour__group_elements_2_0_i6);
BEGIN_CODE

/* code for predicate 'graph_colour__group_elements'/2 in mode 0 */
Define_entry(mercury__graph_colour__group_elements_2_0);
	incr_sp_push_msg(4, "graph_colour__group_elements");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__set__power_union_2_0);
	call_localret(ENTRY(mercury__set__power_union_2_0),
		mercury__graph_colour__group_elements_2_0_i2,
		ENTRY(mercury__graph_colour__group_elements_2_0));
	}
Define_label(mercury__graph_colour__group_elements_2_0_i2);
	update_prof_current_proc(LABEL(mercury__graph_colour__group_elements_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__graph_colour__group_elements_2_0_i3,
		ENTRY(mercury__graph_colour__group_elements_2_0));
	}
Define_label(mercury__graph_colour__group_elements_2_0_i3);
	update_prof_current_proc(LABEL(mercury__graph_colour__group_elements_2_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	r2 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	{
	extern Word * mercury_data_set__base_type_info_set_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	}
	{
	Declare_entry(mercury__set__delete_3_0);
	call_localret(ENTRY(mercury__set__delete_3_0),
		mercury__graph_colour__group_elements_2_0_i4,
		ENTRY(mercury__graph_colour__group_elements_2_0));
	}
Define_label(mercury__graph_colour__group_elements_2_0_i4);
	update_prof_current_proc(LABEL(mercury__graph_colour__group_elements_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	extern Word * mercury_data_set__base_type_info_set_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	}
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__graph_colour__group_elements_2_0_i5,
		ENTRY(mercury__graph_colour__group_elements_2_0));
	}
Define_label(mercury__graph_colour__group_elements_2_0_i5);
	update_prof_current_proc(LABEL(mercury__graph_colour__group_elements_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__graph_colour__find_all_colours_3_0),
		mercury__graph_colour__group_elements_2_0_i6,
		ENTRY(mercury__graph_colour__group_elements_2_0));
Define_label(mercury__graph_colour__group_elements_2_0_i6);
	update_prof_current_proc(LABEL(mercury__graph_colour__group_elements_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	extern Word * mercury_data_set__base_type_info_set_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	}
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	tailcall(ENTRY(mercury__set__list_to_set_2_0),
		ENTRY(mercury__graph_colour__group_elements_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__graph_colour_module1)
	init_entry(mercury__graph_colour__find_all_colours_3_0);
	init_label(mercury__graph_colour__find_all_colours_3_0_i1000);
	init_label(mercury__graph_colour__find_all_colours_3_0_i5);
	init_label(mercury__graph_colour__find_all_colours_3_0_i6);
	init_label(mercury__graph_colour__find_all_colours_3_0_i7);
BEGIN_CODE

/* code for predicate 'graph_colour__find_all_colours'/3 in mode 0 */
Define_static(mercury__graph_colour__find_all_colours_3_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__graph_colour__find_all_colours_3_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__graph_colour__find_all_colours_3_0_i1000);
	incr_sp_push_msg(4, "graph_colour__find_all_colours");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	detstackvar(3) = (Integer) r1;
	call_localret(STATIC(mercury__graph_colour__next_colour_4_0),
		mercury__graph_colour__find_all_colours_3_0_i5,
		STATIC(mercury__graph_colour__find_all_colours_3_0));
	}
Define_label(mercury__graph_colour__find_all_colours_3_0_i5);
	update_prof_current_proc(LABEL(mercury__graph_colour__find_all_colours_3_0));
	r3 = (Integer) r2;
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__graph_colour__find_all_colours_3_0_i6,
		STATIC(mercury__graph_colour__find_all_colours_3_0));
	}
Define_label(mercury__graph_colour__find_all_colours_3_0_i6);
	update_prof_current_proc(LABEL(mercury__graph_colour__find_all_colours_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	localcall(mercury__graph_colour__find_all_colours_3_0,
		LABEL(mercury__graph_colour__find_all_colours_3_0_i7),
		STATIC(mercury__graph_colour__find_all_colours_3_0));
Define_label(mercury__graph_colour__find_all_colours_3_0_i7);
	update_prof_current_proc(LABEL(mercury__graph_colour__find_all_colours_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__graph_colour_module2)
	init_entry(mercury__graph_colour__next_colour_4_0);
	init_label(mercury__graph_colour__next_colour_4_0_i7);
	init_label(mercury__graph_colour__next_colour_4_0_i11);
	init_label(mercury__graph_colour__next_colour_4_0_i8);
	init_label(mercury__graph_colour__next_colour_4_0_i12);
	init_label(mercury__graph_colour__next_colour_4_0_i13);
	init_label(mercury__graph_colour__next_colour_4_0_i14);
	init_label(mercury__graph_colour__next_colour_4_0_i24);
	init_label(mercury__graph_colour__next_colour_4_0_i26);
	init_label(mercury__graph_colour__next_colour_4_0_i27);
	init_label(mercury__graph_colour__next_colour_4_0_i21);
	init_label(mercury__graph_colour__next_colour_4_0_i28);
	init_label(mercury__graph_colour__next_colour_4_0_i15);
	init_label(mercury__graph_colour__next_colour_4_0_i30);
	init_label(mercury__graph_colour__next_colour_4_0_i31);
	init_label(mercury__graph_colour__next_colour_4_0_i32);
	init_label(mercury__graph_colour__next_colour_4_0_i2);
	init_label(mercury__graph_colour__next_colour_4_0_i33);
BEGIN_CODE

/* code for predicate 'graph_colour__next_colour'/4 in mode 0 */
Define_static(mercury__graph_colour__next_colour_4_0);
	incr_sp_push_msg(6, "graph_colour__next_colour");
	detstackvar(6) = (Integer) succip;
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__graph_colour__next_colour_4_0_i2);
	detstackvar(1) = (Integer) r3;
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__graph_colour__next_colour_4_0_i7,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i7);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__graph_colour__next_colour_4_0_i8);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	call_localret(ENTRY(mercury__set__list_to_set_2_0),
		mercury__graph_colour__next_colour_4_0_i11,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i11);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__graph_colour__next_colour_4_0_i13);
Define_label(mercury__graph_colour__next_colour_4_0_i8);
	r1 = string_const("graph_colour__choose_var: no vars!", 34);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__graph_colour__next_colour_4_0_i12,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i12);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
Define_label(mercury__graph_colour__next_colour_4_0_i13);
	detstackvar(2) = (Integer) r2;
	detstackvar(5) = (Integer) r1;
	call_localret(STATIC(mercury__graph_colour__divide_constraints_6_0),
		mercury__graph_colour__next_colour_4_0_i14,
		STATIC(mercury__graph_colour__next_colour_4_0));
Define_label(mercury__graph_colour__next_colour_4_0_i14);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__graph_colour__next_colour_4_0_i15);
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__set__empty_1_0);
	call_localret(ENTRY(mercury__set__empty_1_0),
		mercury__graph_colour__next_colour_4_0_i24,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i24);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__graph_colour__next_colour_4_0_i21);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	localcall(mercury__graph_colour__next_colour_4_0,
		LABEL(mercury__graph_colour__next_colour_4_0_i26),
		STATIC(mercury__graph_colour__next_colour_4_0));
Define_label(mercury__graph_colour__next_colour_4_0_i26);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__graph_colour__next_colour_4_0_i27,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i27);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	{
	extern Word * mercury_data_set__base_type_info_set_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	}
	GOTO_LABEL(mercury__graph_colour__next_colour_4_0_i31);
Define_label(mercury__graph_colour__next_colour_4_0_i21);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__singleton_set_2_1);
	call_localret(ENTRY(mercury__set__singleton_set_2_1),
		mercury__graph_colour__next_colour_4_0_i28,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i28);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	extern Word * mercury_data_set__base_type_info_set_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	}
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__graph_colour__next_colour_4_0_i31);
Define_label(mercury__graph_colour__next_colour_4_0_i15);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__singleton_set_2_1);
	call_localret(ENTRY(mercury__set__singleton_set_2_1),
		mercury__graph_colour__next_colour_4_0_i30,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i30);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	extern Word * mercury_data_set__base_type_info_set_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	}
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
Define_label(mercury__graph_colour__next_colour_4_0_i31);
	detstackvar(1) = (Integer) r4;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__graph_colour__next_colour_4_0_i32,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i32);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__graph_colour__next_colour_4_0_i2);
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__graph_colour__next_colour_4_0_i33,
		STATIC(mercury__graph_colour__next_colour_4_0));
	}
Define_label(mercury__graph_colour__next_colour_4_0_i33);
	update_prof_current_proc(LABEL(mercury__graph_colour__next_colour_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__graph_colour_module3)
	init_entry(mercury__graph_colour__divide_constraints_6_0);
	init_label(mercury__graph_colour__divide_constraints_6_0_i4);
	init_label(mercury__graph_colour__divide_constraints_6_0_i7);
	init_label(mercury__graph_colour__divide_constraints_6_0_i9);
	init_label(mercury__graph_colour__divide_constraints_6_0_i12);
	init_label(mercury__graph_colour__divide_constraints_6_0_i11);
	init_label(mercury__graph_colour__divide_constraints_6_0_i14);
	init_label(mercury__graph_colour__divide_constraints_6_0_i15);
	init_label(mercury__graph_colour__divide_constraints_6_0_i6);
	init_label(mercury__graph_colour__divide_constraints_6_0_i1005);
BEGIN_CODE

/* code for predicate 'graph_colour__divide_constraints'/6 in mode 0 */
Define_static(mercury__graph_colour__divide_constraints_6_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__graph_colour__divide_constraints_6_0_i1005);
	incr_sp_push_msg(9, "graph_colour__divide_constraints");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(8) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	localcall(mercury__graph_colour__divide_constraints_6_0,
		LABEL(mercury__graph_colour__divide_constraints_6_0_i4),
		STATIC(mercury__graph_colour__divide_constraints_6_0));
Define_label(mercury__graph_colour__divide_constraints_6_0_i4);
	update_prof_current_proc(LABEL(mercury__graph_colour__divide_constraints_6_0));
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	detstackvar(6) = (Integer) r3;
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__graph_colour__divide_constraints_6_0_i7,
		STATIC(mercury__graph_colour__divide_constraints_6_0));
	}
Define_label(mercury__graph_colour__divide_constraints_6_0_i7);
	update_prof_current_proc(LABEL(mercury__graph_colour__divide_constraints_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__graph_colour__divide_constraints_6_0_i6);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__delete_3_0);
	call_localret(ENTRY(mercury__set__delete_3_0),
		mercury__graph_colour__divide_constraints_6_0_i9,
		STATIC(mercury__graph_colour__divide_constraints_6_0));
	}
Define_label(mercury__graph_colour__divide_constraints_6_0_i9);
	update_prof_current_proc(LABEL(mercury__graph_colour__divide_constraints_6_0));
	r2 = (Integer) r1;
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__set__empty_1_0);
	call_localret(ENTRY(mercury__set__empty_1_0),
		mercury__graph_colour__divide_constraints_6_0_i12,
		STATIC(mercury__graph_colour__divide_constraints_6_0));
	}
Define_label(mercury__graph_colour__divide_constraints_6_0_i12);
	update_prof_current_proc(LABEL(mercury__graph_colour__divide_constraints_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__graph_colour__divide_constraints_6_0_i11);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	r1 = (Integer) detstackvar(8);
	GOTO_LABEL(mercury__graph_colour__divide_constraints_6_0_i14);
Define_label(mercury__graph_colour__divide_constraints_6_0_i11);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(5);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) r3;
Define_label(mercury__graph_colour__divide_constraints_6_0_i14);
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__graph_colour__divide_constraints_6_0_i15,
		STATIC(mercury__graph_colour__divide_constraints_6_0));
	}
Define_label(mercury__graph_colour__divide_constraints_6_0_i15);
	update_prof_current_proc(LABEL(mercury__graph_colour__divide_constraints_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__graph_colour__divide_constraints_6_0_i6);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__graph_colour__divide_constraints_6_0_i1005);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) r4;
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__graph_colour_bunch_0(void)
{
	mercury__graph_colour_module0();
	mercury__graph_colour_module1();
	mercury__graph_colour_module2();
	mercury__graph_colour_module3();
}

#endif

void mercury__graph_colour__init(void); /* suppress gcc warning */
void mercury__graph_colour__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__graph_colour_bunch_0();
#endif
}
