/*
** Automatically generated from `multi_map.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__multi_map__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__multi_map__assoc_list_member__ua40000_3_0);
Declare_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i5);
Declare_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i4);
Declare_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i1009);
Declare_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i2);
Declare_static(mercury__multi_map__count_list__ua10000_3_0);
Declare_label(mercury__multi_map__count_list__ua10000_3_0_i3);
Declare_label(mercury__multi_map__count_list__ua10000_3_0_i1);
Define_extern_entry(mercury__multi_map__init_1_0);
Define_extern_entry(mercury__multi_map__is_empty_1_0);
Declare_label(mercury__multi_map__is_empty_1_0_i2);
Define_extern_entry(mercury__multi_map__contains_2_0);
Define_extern_entry(mercury__multi_map__member_3_0);
Declare_label(mercury__multi_map__member_3_0_i1);
Declare_label(mercury__multi_map__member_3_0_i2);
Define_extern_entry(mercury__multi_map__search_3_0);
Declare_label(mercury__multi_map__search_3_0_i2);
Declare_label(mercury__multi_map__search_3_0_i1000);
Define_extern_entry(mercury__multi_map__nondet_search_3_0);
Declare_label(mercury__multi_map__nondet_search_3_0_i1);
Declare_label(mercury__multi_map__nondet_search_3_0_i3);
Define_extern_entry(mercury__multi_map__lookup_3_0);
Define_extern_entry(mercury__multi_map__nondet_lookup_3_0);
Declare_label(mercury__multi_map__nondet_lookup_3_0_i1);
Declare_label(mercury__multi_map__nondet_lookup_3_0_i3);
Define_extern_entry(mercury__multi_map__inverse_search_3_0);
Declare_label(mercury__multi_map__inverse_search_3_0_i1);
Declare_label(mercury__multi_map__inverse_search_3_0_i2);
Define_extern_entry(mercury__multi_map__insert_4_0);
Declare_label(mercury__multi_map__insert_4_0_i2);
Declare_label(mercury__multi_map__insert_4_0_i1000);
Define_extern_entry(mercury__multi_map__det_insert_4_0);
Define_extern_entry(mercury__multi_map__update_4_0);
Declare_label(mercury__multi_map__update_4_0_i2);
Declare_label(mercury__multi_map__update_4_0_i4);
Declare_label(mercury__multi_map__update_4_0_i1);
Declare_label(mercury__multi_map__update_4_0_i1000);
Define_extern_entry(mercury__multi_map__det_update_4_0);
Define_extern_entry(mercury__multi_map__det_replace_4_0);
Define_extern_entry(mercury__multi_map__set_4_0);
Declare_label(mercury__multi_map__set_4_0_i4);
Declare_label(mercury__multi_map__set_4_0_i3);
Define_extern_entry(mercury__multi_map__keys_2_0);
Define_extern_entry(mercury__multi_map__values_2_0);
Declare_label(mercury__multi_map__values_2_0_i2);
Define_extern_entry(mercury__multi_map__to_assoc_list_2_0);
Define_extern_entry(mercury__multi_map__from_assoc_list_2_0);
Define_extern_entry(mercury__multi_map__from_sorted_assoc_list_2_0);
Define_extern_entry(mercury__multi_map__delete_3_0);
Define_extern_entry(mercury__multi_map__delete_4_0);
Declare_label(mercury__multi_map__delete_4_0_i4);
Declare_label(mercury__multi_map__delete_4_0_i6);
Declare_label(mercury__multi_map__delete_4_0_i7);
Declare_label(mercury__multi_map__delete_4_0_i3);
Define_extern_entry(mercury__multi_map__remove_4_0);
Declare_label(mercury__multi_map__remove_4_0_i2);
Declare_label(mercury__multi_map__remove_4_0_i1000);
Define_extern_entry(mercury__multi_map__det_remove_4_0);
Define_extern_entry(mercury__multi_map__count_2_0);
Define_extern_entry(mercury__multi_map__all_count_2_0);
Declare_label(mercury__multi_map__all_count_2_0_i2);
Define_extern_entry(mercury__multi_map__from_corresponding_lists_3_0);
Declare_label(mercury__multi_map__from_corresponding_lists_3_0_i2);
Define_extern_entry(mercury__multi_map__from_corresponding_list_lists_3_0);
Define_extern_entry(mercury__multi_map__merge_3_0);
Declare_label(mercury__multi_map__merge_3_0_i2);
Declare_label(mercury__multi_map__merge_3_0_i3);
Declare_label(mercury__multi_map__merge_3_0_i4);
Define_extern_entry(mercury__multi_map__select_3_0);
Define_extern_entry(mercury__multi_map__apply_to_list_3_0);
Declare_label(mercury__multi_map__apply_to_list_3_0_i4);
Declare_label(mercury__multi_map__apply_to_list_3_0_i1002);
Define_extern_entry(mercury__multi_map__optimize_2_0);
Define_extern_entry(mercury__multi_map__remove_smallest_4_0);
Declare_label(mercury__multi_map__remove_smallest_4_0_i2);
Declare_label(mercury__multi_map__remove_smallest_4_0_i1000);
Declare_static(mercury__multi_map__assoc_list_merge_3_0);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i6);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i9);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i10);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i8);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i12);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i11);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i13);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i1011);
Declare_label(mercury__multi_map__assoc_list_merge_3_0_i1010);
Define_extern_entry(mercury____Unify___multi_map__multi_map_2_0);
Define_extern_entry(mercury____Index___multi_map__multi_map_2_0);
Define_extern_entry(mercury____Compare___multi_map__multi_map_2_0);

extern Word * mercury_data_multi_map__base_type_layout_multi_map_2[];
Word * mercury_data_multi_map__base_type_info_multi_map_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) ENTRY(mercury____Unify___multi_map__multi_map_2_0),
	(Word *) (Integer) ENTRY(mercury____Index___multi_map__multi_map_2_0),
	(Word *) (Integer) ENTRY(mercury____Compare___multi_map__multi_map_2_0),
	(Word *) (Integer) mercury_data_multi_map__base_type_layout_multi_map_2
};

extern Word * mercury_data_multi_map__common_2[];
Word * mercury_data_multi_map__base_type_layout_multi_map_2[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_multi_map__common_2),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_multi_map__common_2),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_multi_map__common_2),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_multi_map__common_2)
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_multi_map__common_0[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) ((Integer) 2)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_multi_map__common_1[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_multi_map__common_0)
};

Word * mercury_data_multi_map__common_2[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_multi_map__common_1)
};

BEGIN_MODULE(mercury__multi_map_module0)
	init_entry(mercury__multi_map__assoc_list_member__ua40000_3_0);
	init_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i5);
	init_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i4);
	init_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i1009);
	init_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i2);
BEGIN_CODE

/* code for predicate 'multi_map__assoc_list_member__ua40000'/3 in mode 0 */
Define_static(mercury__multi_map__assoc_list_member__ua40000_3_0);
	{
	Declare_entry(do_redo);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_redo));
	}
	mkframe("multi_map__assoc_list_member__ua40000/3", 4, LABEL(mercury__multi_map__assoc_list_member__ua40000_3_0_i4));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	framevar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	framevar(1) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	framevar(0) = (Integer) r2;
	framevar(3) = (Integer) r1;
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__multi_map__assoc_list_member__ua40000_3_0_i5,
		STATIC(mercury__multi_map__assoc_list_member__ua40000_3_0));
	}
	}
Define_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i5);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_member__ua40000_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__assoc_list_member__ua40000_3_0_i1009);
	r1 = (Integer) framevar(1);
	succeed();
Define_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_member__ua40000_3_0));
Define_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i1009);
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r1 = (Integer) framevar(3);
	r2 = (Integer) framevar(0);
	r3 = (Integer) framevar(2);
	localcall(mercury__multi_map__assoc_list_member__ua40000_3_0,
		LABEL(mercury__multi_map__assoc_list_member__ua40000_3_0_i2),
		STATIC(mercury__multi_map__assoc_list_member__ua40000_3_0));
Define_label(mercury__multi_map__assoc_list_member__ua40000_3_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_member__ua40000_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module1)
	init_entry(mercury__multi_map__count_list__ua10000_3_0);
	init_label(mercury__multi_map__count_list__ua10000_3_0_i3);
	init_label(mercury__multi_map__count_list__ua10000_3_0_i1);
BEGIN_CODE

/* code for predicate 'multi_map__count_list__ua10000'/3 in mode 0 */
Define_static(mercury__multi_map__count_list__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__multi_map__count_list__ua10000_3_0_i1);
Define_label(mercury__multi_map__count_list__ua10000_3_0_i3);
	while (1) {
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = ((Integer) r2 + ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	break; } /* end while */
Define_label(mercury__multi_map__count_list__ua10000_3_0_i1);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module2)
	init_entry(mercury__multi_map__init_1_0);
BEGIN_CODE

/* code for predicate 'multi_map__init'/1 in mode 0 */
Define_entry(mercury__multi_map__init_1_0);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	{
	Declare_entry(mercury__map__init_1_0);
	tailcall(ENTRY(mercury__map__init_1_0),
		ENTRY(mercury__multi_map__init_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module3)
	init_entry(mercury__multi_map__is_empty_1_0);
	init_label(mercury__multi_map__is_empty_1_0_i2);
BEGIN_CODE

/* code for predicate 'multi_map__is_empty'/1 in mode 0 */
Define_entry(mercury__multi_map__is_empty_1_0);
	incr_sp_push_msg(4, "multi_map__is_empty");
	detstackvar(4) = (Integer) succip;
	detstackvar(3) = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__multi_map__is_empty_1_0_i2,
		ENTRY(mercury__multi_map__is_empty_1_0));
	}
	}
Define_label(mercury__multi_map__is_empty_1_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__is_empty_1_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	r4 = (Integer) r3;
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury__multi_map__is_empty_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module4)
	init_entry(mercury__multi_map__contains_2_0);
BEGIN_CODE

/* code for predicate 'multi_map__contains'/2 in mode 0 */
Define_entry(mercury__multi_map__contains_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__search_3_1);
	tailcall(ENTRY(mercury__map__search_3_1),
		ENTRY(mercury__multi_map__contains_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module5)
	init_entry(mercury__multi_map__member_3_0);
	init_label(mercury__multi_map__member_3_0_i1);
	init_label(mercury__multi_map__member_3_0_i2);
BEGIN_CODE

/* code for predicate 'multi_map__member'/3 in mode 0 */
Define_entry(mercury__multi_map__member_3_0);
	{
	Declare_entry(do_fail);
	mkframe("multi_map__member/3", 2, ENTRY(do_fail));
	}
	framevar(1) = (Integer) r2;
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	{
	Declare_entry(mercury__map__member_3_0);
	call_localret(ENTRY(mercury__map__member_3_0),
		mercury__multi_map__member_3_0_i1,
		ENTRY(mercury__multi_map__member_3_0));
	}
Define_label(mercury__multi_map__member_3_0_i1);
	update_prof_current_proc(LABEL(mercury__multi_map__member_3_0));
	framevar(0) = (Integer) r1;
	r1 = (Integer) framevar(1);
	{
	Declare_entry(mercury__list__member_2_1);
	call_localret(ENTRY(mercury__list__member_2_1),
		mercury__multi_map__member_3_0_i2,
		ENTRY(mercury__multi_map__member_3_0));
	}
Define_label(mercury__multi_map__member_3_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__member_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) framevar(0);
	succeed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module6)
	init_entry(mercury__multi_map__search_3_0);
	init_label(mercury__multi_map__search_3_0_i2);
	init_label(mercury__multi_map__search_3_0_i1000);
BEGIN_CODE

/* code for predicate 'multi_map__search'/3 in mode 0 */
Define_entry(mercury__multi_map__search_3_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	incr_sp_push_msg(1, "multi_map__search");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__multi_map__search_3_0_i2,
		ENTRY(mercury__multi_map__search_3_0));
	}
	}
Define_label(mercury__multi_map__search_3_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__search_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__search_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__multi_map__search_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module7)
	init_entry(mercury__multi_map__nondet_search_3_0);
	init_label(mercury__multi_map__nondet_search_3_0_i1);
	init_label(mercury__multi_map__nondet_search_3_0_i3);
BEGIN_CODE

/* code for predicate 'multi_map__nondet_search'/3 in mode 0 */
Define_entry(mercury__multi_map__nondet_search_3_0);
	{
	Declare_entry(do_fail);
	mkframe("multi_map__nondet_search/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r2;
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r5;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__multi_map__nondet_search_3_0_i1,
		ENTRY(mercury__multi_map__nondet_search_3_0));
	}
Define_label(mercury__multi_map__nondet_search_3_0_i1);
	update_prof_current_proc(LABEL(mercury__multi_map__nondet_search_3_0));
	{
	Declare_entry(do_fail);
	if (!((Integer) r1))
		GOTO(ENTRY(do_fail));
	}
	r1 = (Integer) framevar(0);
	{
	Declare_entry(mercury__list__member_2_1);
	call_localret(ENTRY(mercury__list__member_2_1),
		mercury__multi_map__nondet_search_3_0_i3,
		ENTRY(mercury__multi_map__nondet_search_3_0));
	}
Define_label(mercury__multi_map__nondet_search_3_0_i3);
	update_prof_current_proc(LABEL(mercury__multi_map__nondet_search_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module8)
	init_entry(mercury__multi_map__lookup_3_0);
BEGIN_CODE

/* code for predicate 'multi_map__lookup'/3 in mode 0 */
Define_entry(mercury__multi_map__lookup_3_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__lookup_3_1);
	tailcall(ENTRY(mercury__map__lookup_3_1),
		ENTRY(mercury__multi_map__lookup_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module9)
	init_entry(mercury__multi_map__nondet_lookup_3_0);
	init_label(mercury__multi_map__nondet_lookup_3_0_i1);
	init_label(mercury__multi_map__nondet_lookup_3_0_i3);
BEGIN_CODE

/* code for predicate 'multi_map__nondet_lookup'/3 in mode 0 */
Define_entry(mercury__multi_map__nondet_lookup_3_0);
	{
	Declare_entry(do_fail);
	mkframe("multi_map__nondet_lookup/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r2;
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r5;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__multi_map__nondet_lookup_3_0_i1,
		ENTRY(mercury__multi_map__nondet_lookup_3_0));
	}
Define_label(mercury__multi_map__nondet_lookup_3_0_i1);
	update_prof_current_proc(LABEL(mercury__multi_map__nondet_lookup_3_0));
	{
	Declare_entry(do_fail);
	if (!((Integer) r1))
		GOTO(ENTRY(do_fail));
	}
	r1 = (Integer) framevar(0);
	{
	Declare_entry(mercury__list__member_2_1);
	call_localret(ENTRY(mercury__list__member_2_1),
		mercury__multi_map__nondet_lookup_3_0_i3,
		ENTRY(mercury__multi_map__nondet_lookup_3_0));
	}
Define_label(mercury__multi_map__nondet_lookup_3_0_i3);
	update_prof_current_proc(LABEL(mercury__multi_map__nondet_lookup_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module10)
	init_entry(mercury__multi_map__inverse_search_3_0);
	init_label(mercury__multi_map__inverse_search_3_0_i1);
	init_label(mercury__multi_map__inverse_search_3_0_i2);
BEGIN_CODE

/* code for predicate 'multi_map__inverse_search'/3 in mode 0 */
Define_entry(mercury__multi_map__inverse_search_3_0);
	{
	Declare_entry(do_fail);
	mkframe("multi_map__inverse_search/3", 2, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r4;
	framevar(1) = (Integer) r2;
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r5;
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__to_assoc_list_2_0),
		mercury__multi_map__inverse_search_3_0_i1,
		ENTRY(mercury__multi_map__inverse_search_3_0));
	}
Define_label(mercury__multi_map__inverse_search_3_0_i1);
	update_prof_current_proc(LABEL(mercury__multi_map__inverse_search_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) framevar(1);
	r2 = (Integer) framevar(0);
	call_localret(STATIC(mercury__multi_map__assoc_list_member__ua40000_3_0),
		mercury__multi_map__inverse_search_3_0_i2,
		ENTRY(mercury__multi_map__inverse_search_3_0));
Define_label(mercury__multi_map__inverse_search_3_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__inverse_search_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module11)
	init_entry(mercury__multi_map__insert_4_0);
	init_label(mercury__multi_map__insert_4_0_i2);
	init_label(mercury__multi_map__insert_4_0_i1000);
BEGIN_CODE

/* code for predicate 'multi_map__insert'/4 in mode 0 */
Define_entry(mercury__multi_map__insert_4_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	r6 = (Integer) r5;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	incr_sp_push_msg(1, "multi_map__insert");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__insert_4_0);
	call_localret(ENTRY(mercury__map__insert_4_0),
		mercury__multi_map__insert_4_0_i2,
		ENTRY(mercury__multi_map__insert_4_0));
	}
	}
Define_label(mercury__multi_map__insert_4_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__insert_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__insert_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__multi_map__insert_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module12)
	init_entry(mercury__multi_map__det_insert_4_0);
BEGIN_CODE

/* code for predicate 'multi_map__det_insert'/4 in mode 0 */
Define_entry(mercury__multi_map__det_insert_4_0);
	r6 = (Integer) r5;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	r2 = (Integer) tempr1;
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__det_insert_4_0);
	tailcall(ENTRY(mercury__map__det_insert_4_0),
		ENTRY(mercury__multi_map__det_insert_4_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module13)
	init_entry(mercury__multi_map__update_4_0);
	init_label(mercury__multi_map__update_4_0_i2);
	init_label(mercury__multi_map__update_4_0_i4);
	init_label(mercury__multi_map__update_4_0_i1);
	init_label(mercury__multi_map__update_4_0_i1000);
BEGIN_CODE

/* code for predicate 'multi_map__update'/4 in mode 0 */
Define_entry(mercury__multi_map__update_4_0);
	incr_sp_push_msg(6, "multi_map__update");
	detstackvar(6) = (Integer) succip;
	detstackvar(5) = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__multi_map__update_4_0_i2,
		ENTRY(mercury__multi_map__update_4_0));
	}
	}
Define_label(mercury__multi_map__update_4_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__update_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__update_4_0_i1);
	r1 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(2);
	r6 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) r6;
	{
	Declare_entry(mercury__map__update_4_0);
	call_localret(ENTRY(mercury__map__update_4_0),
		mercury__multi_map__update_4_0_i4,
		ENTRY(mercury__multi_map__update_4_0));
	}
Define_label(mercury__multi_map__update_4_0_i4);
	update_prof_current_proc(LABEL(mercury__multi_map__update_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__update_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__multi_map__update_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__multi_map__update_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module14)
	init_entry(mercury__multi_map__det_update_4_0);
BEGIN_CODE

/* code for predicate 'multi_map__det_update'/4 in mode 0 */
Define_entry(mercury__multi_map__det_update_4_0);
	r6 = (Integer) r5;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	r2 = (Integer) tempr1;
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__det_update_4_0);
	tailcall(ENTRY(mercury__map__det_update_4_0),
		ENTRY(mercury__multi_map__det_update_4_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module15)
	init_entry(mercury__multi_map__det_replace_4_0);
BEGIN_CODE

/* code for predicate 'multi_map__det_replace'/4 in mode 0 */
Define_entry(mercury__multi_map__det_replace_4_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__det_update_4_0);
	tailcall(ENTRY(mercury__map__det_update_4_0),
		ENTRY(mercury__multi_map__det_replace_4_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module16)
	init_entry(mercury__multi_map__set_4_0);
	init_label(mercury__multi_map__set_4_0_i4);
	init_label(mercury__multi_map__set_4_0_i3);
BEGIN_CODE

/* code for predicate 'multi_map__set'/4 in mode 0 */
Define_entry(mercury__multi_map__set_4_0);
	incr_sp_push_msg(6, "multi_map__set");
	detstackvar(6) = (Integer) succip;
	detstackvar(5) = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__multi_map__set_4_0_i4,
		ENTRY(mercury__multi_map__set_4_0));
	}
	}
Define_label(mercury__multi_map__set_4_0_i4);
	update_prof_current_proc(LABEL(mercury__multi_map__set_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__set_4_0_i3);
	r1 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(2);
	r6 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury__map__set_4_1);
	tailcall(ENTRY(mercury__map__set_4_1),
		ENTRY(mercury__multi_map__set_4_0));
	}
Define_label(mercury__multi_map__set_4_0_i3);
	r1 = (Integer) detstackvar(4);
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	tailcall(ENTRY(mercury__map__det_insert_4_0),
		ENTRY(mercury__multi_map__set_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module17)
	init_entry(mercury__multi_map__keys_2_0);
BEGIN_CODE

/* code for predicate 'multi_map__keys'/2 in mode 0 */
Define_entry(mercury__multi_map__keys_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__keys_2_0);
	tailcall(ENTRY(mercury__map__keys_2_0),
		ENTRY(mercury__multi_map__keys_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module18)
	init_entry(mercury__multi_map__values_2_0);
	init_label(mercury__multi_map__values_2_0_i2);
BEGIN_CODE

/* code for predicate 'multi_map__values'/2 in mode 0 */
Define_entry(mercury__multi_map__values_2_0);
	incr_sp_push_msg(2, "multi_map__values");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__values_2_0);
	call_localret(ENTRY(mercury__map__values_2_0),
		mercury__multi_map__values_2_0_i2,
		ENTRY(mercury__multi_map__values_2_0));
	}
	}
Define_label(mercury__multi_map__values_2_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__values_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__condense_2_0);
	tailcall(ENTRY(mercury__list__condense_2_0),
		ENTRY(mercury__multi_map__values_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module19)
	init_entry(mercury__multi_map__to_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'multi_map__to_assoc_list'/2 in mode 0 */
Define_entry(mercury__multi_map__to_assoc_list_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	tailcall(ENTRY(mercury__map__to_assoc_list_2_0),
		ENTRY(mercury__multi_map__to_assoc_list_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module20)
	init_entry(mercury__multi_map__from_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'multi_map__from_assoc_list'/2 in mode 0 */
Define_entry(mercury__multi_map__from_assoc_list_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__from_assoc_list_2_0);
	tailcall(ENTRY(mercury__map__from_assoc_list_2_0),
		ENTRY(mercury__multi_map__from_assoc_list_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module21)
	init_entry(mercury__multi_map__from_sorted_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'multi_map__from_sorted_assoc_list'/2 in mode 0 */
Define_entry(mercury__multi_map__from_sorted_assoc_list_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__from_sorted_assoc_list_2_0);
	tailcall(ENTRY(mercury__map__from_sorted_assoc_list_2_0),
		ENTRY(mercury__multi_map__from_sorted_assoc_list_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module22)
	init_entry(mercury__multi_map__delete_3_0);
BEGIN_CODE

/* code for predicate 'multi_map__delete'/3 in mode 0 */
Define_entry(mercury__multi_map__delete_3_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__delete_3_1);
	tailcall(ENTRY(mercury__map__delete_3_1),
		ENTRY(mercury__multi_map__delete_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module23)
	init_entry(mercury__multi_map__delete_4_0);
	init_label(mercury__multi_map__delete_4_0_i4);
	init_label(mercury__multi_map__delete_4_0_i6);
	init_label(mercury__multi_map__delete_4_0_i7);
	init_label(mercury__multi_map__delete_4_0_i3);
BEGIN_CODE

/* code for predicate 'multi_map__delete'/4 in mode 0 */
Define_entry(mercury__multi_map__delete_4_0);
	incr_sp_push_msg(6, "multi_map__delete");
	detstackvar(6) = (Integer) succip;
	detstackvar(5) = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__multi_map__delete_4_0_i4,
		ENTRY(mercury__multi_map__delete_4_0));
	}
	}
Define_label(mercury__multi_map__delete_4_0_i4);
	update_prof_current_proc(LABEL(mercury__multi_map__delete_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__delete_4_0_i3);
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__delete_all_3_1);
	call_localret(ENTRY(mercury__list__delete_all_3_1),
		mercury__multi_map__delete_4_0_i6,
		ENTRY(mercury__multi_map__delete_4_0));
	}
Define_label(mercury__multi_map__delete_4_0_i6);
	update_prof_current_proc(LABEL(mercury__multi_map__delete_4_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__multi_map__delete_4_0_i7);
	r1 = (Integer) detstackvar(4);
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury__map__delete_3_1);
	tailcall(ENTRY(mercury__map__delete_3_1),
		ENTRY(mercury__multi_map__delete_4_0));
	}
Define_label(mercury__multi_map__delete_4_0_i7);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	r5 = (Integer) r3;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury__map__set_4_1);
	tailcall(ENTRY(mercury__map__set_4_1),
		ENTRY(mercury__multi_map__delete_4_0));
	}
Define_label(mercury__multi_map__delete_4_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module24)
	init_entry(mercury__multi_map__remove_4_0);
	init_label(mercury__multi_map__remove_4_0_i2);
	init_label(mercury__multi_map__remove_4_0_i1000);
BEGIN_CODE

/* code for predicate 'multi_map__remove'/4 in mode 0 */
Define_entry(mercury__multi_map__remove_4_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	incr_sp_push_msg(1, "multi_map__remove");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__remove_4_0);
	call_localret(ENTRY(mercury__map__remove_4_0),
		mercury__multi_map__remove_4_0_i2,
		ENTRY(mercury__multi_map__remove_4_0));
	}
	}
Define_label(mercury__multi_map__remove_4_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__remove_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__remove_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__multi_map__remove_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module25)
	init_entry(mercury__multi_map__det_remove_4_0);
BEGIN_CODE

/* code for predicate 'multi_map__det_remove'/4 in mode 0 */
Define_entry(mercury__multi_map__det_remove_4_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__det_remove_4_0);
	tailcall(ENTRY(mercury__map__det_remove_4_0),
		ENTRY(mercury__multi_map__det_remove_4_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module26)
	init_entry(mercury__multi_map__count_2_0);
BEGIN_CODE

/* code for predicate 'multi_map__count'/2 in mode 0 */
Define_entry(mercury__multi_map__count_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__count_2_0);
	tailcall(ENTRY(mercury__map__count_2_0),
		ENTRY(mercury__multi_map__count_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module27)
	init_entry(mercury__multi_map__all_count_2_0);
	init_label(mercury__multi_map__all_count_2_0_i2);
BEGIN_CODE

/* code for predicate 'multi_map__all_count'/2 in mode 0 */
Define_entry(mercury__multi_map__all_count_2_0);
	incr_sp_push_msg(1, "multi_map__all_count");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__multi_map__values_2_0),
		mercury__multi_map__all_count_2_0_i2,
		ENTRY(mercury__multi_map__all_count_2_0));
	}
Define_label(mercury__multi_map__all_count_2_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__all_count_2_0));
	r2 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__multi_map__count_list__ua10000_3_0),
		ENTRY(mercury__multi_map__all_count_2_0));
END_MODULE

BEGIN_MODULE(mercury__multi_map_module28)
	init_entry(mercury__multi_map__from_corresponding_lists_3_0);
	init_label(mercury__multi_map__from_corresponding_lists_3_0_i2);
BEGIN_CODE

/* code for predicate 'multi_map__from_corresponding_lists'/3 in mode 0 */
Define_entry(mercury__multi_map__from_corresponding_lists_3_0);
	incr_sp_push_msg(4, "multi_map__from_corresponding_lists");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	r3 = ((Integer) 1);
	{
	Declare_entry(mercury__list__chunk_3_0);
	call_localret(ENTRY(mercury__list__chunk_3_0),
		mercury__multi_map__from_corresponding_lists_3_0_i2,
		ENTRY(mercury__multi_map__from_corresponding_lists_3_0));
	}
Define_label(mercury__multi_map__from_corresponding_lists_3_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__from_corresponding_lists_3_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__map__from_corresponding_lists_3_0);
	tailcall(ENTRY(mercury__map__from_corresponding_lists_3_0),
		ENTRY(mercury__multi_map__from_corresponding_lists_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module29)
	init_entry(mercury__multi_map__from_corresponding_list_lists_3_0);
BEGIN_CODE

/* code for predicate 'multi_map__from_corresponding_list_lists'/3 in mode 0 */
Define_entry(mercury__multi_map__from_corresponding_list_lists_3_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__from_corresponding_lists_3_0);
	tailcall(ENTRY(mercury__map__from_corresponding_lists_3_0),
		ENTRY(mercury__multi_map__from_corresponding_list_lists_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module30)
	init_entry(mercury__multi_map__merge_3_0);
	init_label(mercury__multi_map__merge_3_0_i2);
	init_label(mercury__multi_map__merge_3_0_i3);
	init_label(mercury__multi_map__merge_3_0_i4);
BEGIN_CODE

/* code for predicate 'multi_map__merge'/3 in mode 0 */
Define_entry(mercury__multi_map__merge_3_0);
	incr_sp_push_msg(4, "multi_map__merge");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
		call_localret(STATIC(mercury__multi_map__to_assoc_list_2_0),
		mercury__multi_map__merge_3_0_i2,
		ENTRY(mercury__multi_map__merge_3_0));
	}
Define_label(mercury__multi_map__merge_3_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__merge_3_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__multi_map__to_assoc_list_2_0),
		mercury__multi_map__merge_3_0_i3,
		ENTRY(mercury__multi_map__merge_3_0));
	}
Define_label(mercury__multi_map__merge_3_0_i3);
	update_prof_current_proc(LABEL(mercury__multi_map__merge_3_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__multi_map__assoc_list_merge_3_0),
		mercury__multi_map__merge_3_0_i4,
		ENTRY(mercury__multi_map__merge_3_0));
Define_label(mercury__multi_map__merge_3_0_i4);
	update_prof_current_proc(LABEL(mercury__multi_map__merge_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__multi_map__from_sorted_assoc_list_2_0),
		ENTRY(mercury__multi_map__merge_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module31)
	init_entry(mercury__multi_map__select_3_0);
BEGIN_CODE

/* code for predicate 'multi_map__select'/3 in mode 0 */
Define_entry(mercury__multi_map__select_3_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__select_3_0);
	tailcall(ENTRY(mercury__map__select_3_0),
		ENTRY(mercury__multi_map__select_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module32)
	init_entry(mercury__multi_map__apply_to_list_3_0);
	init_label(mercury__multi_map__apply_to_list_3_0_i4);
	init_label(mercury__multi_map__apply_to_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'multi_map__apply_to_list'/3 in mode 0 */
Define_entry(mercury__multi_map__apply_to_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__multi_map__apply_to_list_3_0_i1002);
	incr_sp_push_msg(2, "multi_map__apply_to_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__apply_to_list_3_0);
	call_localret(ENTRY(mercury__map__apply_to_list_3_0),
		mercury__multi_map__apply_to_list_3_0_i4,
		ENTRY(mercury__multi_map__apply_to_list_3_0));
	}
	}
Define_label(mercury__multi_map__apply_to_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__multi_map__apply_to_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__condense_2_0);
	tailcall(ENTRY(mercury__list__condense_2_0),
		ENTRY(mercury__multi_map__apply_to_list_3_0));
	}
Define_label(mercury__multi_map__apply_to_list_3_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module33)
	init_entry(mercury__multi_map__optimize_2_0);
BEGIN_CODE

/* code for predicate 'multi_map__optimize'/2 in mode 0 */
Define_entry(mercury__multi_map__optimize_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury__map__optimize_2_0);
	tailcall(ENTRY(mercury__map__optimize_2_0),
		ENTRY(mercury__multi_map__optimize_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module34)
	init_entry(mercury__multi_map__remove_smallest_4_0);
	init_label(mercury__multi_map__remove_smallest_4_0_i2);
	init_label(mercury__multi_map__remove_smallest_4_0_i1000);
BEGIN_CODE

/* code for predicate 'multi_map__remove_smallest'/4 in mode 0 */
Define_entry(mercury__multi_map__remove_smallest_4_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	incr_sp_push_msg(1, "multi_map__remove_smallest");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__remove_smallest_4_0);
	call_localret(ENTRY(mercury__map__remove_smallest_4_0),
		mercury__multi_map__remove_smallest_4_0_i2,
		ENTRY(mercury__multi_map__remove_smallest_4_0));
	}
	}
Define_label(mercury__multi_map__remove_smallest_4_0_i2);
	update_prof_current_proc(LABEL(mercury__multi_map__remove_smallest_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__multi_map__remove_smallest_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__multi_map__remove_smallest_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module35)
	init_entry(mercury__multi_map__assoc_list_merge_3_0);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i6);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i9);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i10);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i8);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i12);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i11);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i13);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i1011);
	init_label(mercury__multi_map__assoc_list_merge_3_0_i1010);
BEGIN_CODE

/* code for predicate 'multi_map__assoc_list_merge'/3 in mode 0 */
Define_static(mercury__multi_map__assoc_list_merge_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__multi_map__assoc_list_merge_3_0_i1010);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__multi_map__assoc_list_merge_3_0_i1011);
	incr_sp_push_msg(11, "multi_map__assoc_list_merge");
	detstackvar(11) = (Integer) succip;
	{
	Word tempr1, tempr2, tempr3;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	tempr2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(10) = (Integer) r2;
	r2 = (Integer) tempr2;
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) tempr2;
	detstackvar(8) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	tempr3 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) tempr3, ((Integer) 0));
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) tempr3, ((Integer) 1));
	detstackvar(5) = (Integer) r3;
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(2) = (Integer) r4;
	detstackvar(9) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__multi_map__assoc_list_merge_3_0_i6,
		STATIC(mercury__multi_map__assoc_list_merge_3_0));
	}
	}
Define_label(mercury__multi_map__assoc_list_merge_3_0_i6);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_merge_3_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__multi_map__assoc_list_merge_3_0_i8);
	r1 = (Integer) detstackvar(10);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__multi_map__assoc_list_merge_3_0_i9,
		STATIC(mercury__multi_map__assoc_list_merge_3_0));
	}
Define_label(mercury__multi_map__assoc_list_merge_3_0_i9);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_merge_3_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(10);
	r3 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(7);
	localcall(mercury__multi_map__assoc_list_merge_3_0,
		LABEL(mercury__multi_map__assoc_list_merge_3_0_i10),
		STATIC(mercury__multi_map__assoc_list_merge_3_0));
Define_label(mercury__multi_map__assoc_list_merge_3_0_i10);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_merge_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury__multi_map__assoc_list_merge_3_0_i8);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__multi_map__assoc_list_merge_3_0_i11);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(10);
	r3 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__multi_map__assoc_list_merge_3_0,
		LABEL(mercury__multi_map__assoc_list_merge_3_0_i12),
		STATIC(mercury__multi_map__assoc_list_merge_3_0));
Define_label(mercury__multi_map__assoc_list_merge_3_0_i12);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_merge_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury__multi_map__assoc_list_merge_3_0_i11);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(10);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(7);
	localcall(mercury__multi_map__assoc_list_merge_3_0,
		LABEL(mercury__multi_map__assoc_list_merge_3_0_i13),
		STATIC(mercury__multi_map__assoc_list_merge_3_0));
Define_label(mercury__multi_map__assoc_list_merge_3_0_i13);
	update_prof_current_proc(LABEL(mercury__multi_map__assoc_list_merge_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(6);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury__multi_map__assoc_list_merge_3_0_i1011);
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__multi_map__assoc_list_merge_3_0_i1010);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__multi_map_module36)
	init_entry(mercury____Unify___multi_map__multi_map_2_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___multi_map__multi_map_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___multi_map__multi_map_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module37)
	init_entry(mercury____Index___multi_map__multi_map_2_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___multi_map__multi_map_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury____Index___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Index___tree234__tree234_2_0),
		ENTRY(mercury____Index___multi_map__multi_map_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__multi_map_module38)
	init_entry(mercury____Compare___multi_map__multi_map_2_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___multi_map__multi_map_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___multi_map__multi_map_2_0));
	}
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__multi_map_bunch_0(void)
{
	mercury__multi_map_module0();
	mercury__multi_map_module1();
	mercury__multi_map_module2();
	mercury__multi_map_module3();
	mercury__multi_map_module4();
	mercury__multi_map_module5();
	mercury__multi_map_module6();
	mercury__multi_map_module7();
	mercury__multi_map_module8();
	mercury__multi_map_module9();
	mercury__multi_map_module10();
	mercury__multi_map_module11();
	mercury__multi_map_module12();
	mercury__multi_map_module13();
	mercury__multi_map_module14();
	mercury__multi_map_module15();
	mercury__multi_map_module16();
	mercury__multi_map_module17();
	mercury__multi_map_module18();
	mercury__multi_map_module19();
	mercury__multi_map_module20();
	mercury__multi_map_module21();
	mercury__multi_map_module22();
	mercury__multi_map_module23();
	mercury__multi_map_module24();
	mercury__multi_map_module25();
	mercury__multi_map_module26();
	mercury__multi_map_module27();
	mercury__multi_map_module28();
	mercury__multi_map_module29();
	mercury__multi_map_module30();
	mercury__multi_map_module31();
	mercury__multi_map_module32();
	mercury__multi_map_module33();
	mercury__multi_map_module34();
	mercury__multi_map_module35();
	mercury__multi_map_module36();
	mercury__multi_map_module37();
	mercury__multi_map_module38();
}

#endif

void mercury__multi_map__init(void); /* suppress gcc warning */
void mercury__multi_map__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__multi_map_bunch_0();
#endif
}
