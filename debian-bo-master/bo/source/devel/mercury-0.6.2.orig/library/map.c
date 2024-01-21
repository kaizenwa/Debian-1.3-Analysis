/*
** Automatically generated from `map.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__map__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__map__assoc_list_member__ua40001_3_0);
Declare_label(mercury__map__assoc_list_member__ua40001_3_0_i5);
Declare_label(mercury__map__assoc_list_member__ua40001_3_0_i4);
Declare_label(mercury__map__assoc_list_member__ua40001_3_0_i1009);
Declare_label(mercury__map__assoc_list_member__ua40001_3_0_i2);
Declare_static(mercury__map__optimize__ua10000_2_0);
Define_extern_entry(mercury__map__init_1_0);
Define_extern_entry(mercury__map__is_empty_1_0);
Declare_label(mercury__map__is_empty_1_0_i2);
Define_extern_entry(mercury__map__contains_2_0);
Define_extern_entry(mercury__map__member_3_0);
Declare_label(mercury__map__member_3_0_i1);
Define_extern_entry(mercury__map__search_3_0);
Declare_label(mercury__map__search_3_0_i2);
Declare_label(mercury__map__search_3_0_i1);
Define_extern_entry(mercury__map__search_3_1);
Declare_label(mercury__map__search_3_1_i2);
Declare_label(mercury__map__search_3_1_i1000);
Define_extern_entry(mercury__map__lookup_3_0);
Declare_label(mercury__map__lookup_3_0_i4);
Declare_label(mercury__map__lookup_3_0_i3);
Declare_label(mercury__map__lookup_3_0_i9);
Define_extern_entry(mercury__map__lookup_3_1);
Declare_label(mercury__map__lookup_3_1_i4);
Declare_label(mercury__map__lookup_3_1_i1000);
Define_extern_entry(mercury__map__inverse_search_3_0);
Declare_label(mercury__map__inverse_search_3_0_i1);
Declare_label(mercury__map__inverse_search_3_0_i2);
Define_extern_entry(mercury__map__insert_4_0);
Declare_label(mercury__map__insert_4_0_i2);
Declare_label(mercury__map__insert_4_0_i1000);
Define_extern_entry(mercury__map__det_insert_4_0);
Declare_label(mercury__map__det_insert_4_0_i4);
Declare_label(mercury__map__det_insert_4_0_i1000);
Define_extern_entry(mercury__map__det_insert_from_corresponding_lists_4_0);
Declare_label(mercury__map__det_insert_from_corresponding_lists_4_0_i6);
Declare_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1007);
Declare_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1005);
Declare_label(mercury__map__det_insert_from_corresponding_lists_4_0_i8);
Declare_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1006);
Define_extern_entry(mercury__map__update_4_0);
Declare_label(mercury__map__update_4_0_i2);
Declare_label(mercury__map__update_4_0_i1000);
Define_extern_entry(mercury__map__det_update_4_0);
Declare_label(mercury__map__det_update_4_0_i4);
Declare_label(mercury__map__det_update_4_0_i1000);
Define_extern_entry(mercury__map__set_4_0);
Define_extern_entry(mercury__map__set_4_1);
Define_extern_entry(mercury__map__keys_2_0);
Define_extern_entry(mercury__map__values_2_0);
Define_extern_entry(mercury__map__to_assoc_list_2_0);
Define_extern_entry(mercury__map__from_assoc_list_2_0);
Define_extern_entry(mercury__map__from_sorted_assoc_list_2_0);
Define_extern_entry(mercury__map__delete_3_0);
Define_extern_entry(mercury__map__delete_3_1);
Define_extern_entry(mercury__map__delete_list_3_0);
Declare_label(mercury__map__delete_list_3_0_i4);
Declare_label(mercury__map__delete_list_3_0_i1002);
Define_extern_entry(mercury__map__delete_list_3_1);
Declare_label(mercury__map__delete_list_3_1_i4);
Declare_label(mercury__map__delete_list_3_1_i1002);
Define_extern_entry(mercury__map__remove_4_0);
Declare_label(mercury__map__remove_4_0_i2);
Declare_label(mercury__map__remove_4_0_i1000);
Define_extern_entry(mercury__map__det_remove_4_0);
Declare_label(mercury__map__det_remove_4_0_i4);
Declare_label(mercury__map__det_remove_4_0_i1000);
Define_extern_entry(mercury__map__count_2_0);
Define_extern_entry(mercury__map__from_corresponding_lists_3_0);
Declare_label(mercury__map__from_corresponding_lists_3_0_i2);
Define_extern_entry(mercury__map__merge_3_0);
Declare_label(mercury__map__merge_3_0_i2);
Declare_label(mercury__map__merge_3_0_i3);
Declare_label(mercury__map__merge_3_0_i4);
Define_extern_entry(mercury__map__overlay_3_0);
Declare_label(mercury__map__overlay_3_0_i2);
Define_extern_entry(mercury__map__select_3_0);
Declare_label(mercury__map__select_3_0_i2);
Declare_label(mercury__map__select_3_0_i3);
Define_extern_entry(mercury__map__apply_to_list_3_0);
Declare_label(mercury__map__apply_to_list_3_0_i4);
Declare_label(mercury__map__apply_to_list_3_0_i5);
Declare_label(mercury__map__apply_to_list_3_0_i1002);
Define_extern_entry(mercury__map__optimize_2_0);
Define_extern_entry(mercury__map__remove_smallest_4_0);
Declare_label(mercury__map__remove_smallest_4_0_i2);
Declare_label(mercury__map__remove_smallest_4_0_i1000);
Declare_static(mercury__map__overlay_2_3_0);
Declare_label(mercury__map__overlay_2_3_0_i4);
Declare_label(mercury__map__overlay_2_3_0_i1002);
Declare_static(mercury__map__select_2_4_0);
Declare_label(mercury__map__select_2_4_0_i6);
Declare_label(mercury__map__select_2_4_0_i8);
Declare_label(mercury__map__select_2_4_0_i5);
Declare_label(mercury__map__select_2_4_0_i1003);
Define_extern_entry(mercury____Unify___map__map_2_0);
Define_extern_entry(mercury____Index___map__map_2_0);
Define_extern_entry(mercury____Compare___map__map_2_0);

extern Word * mercury_data_map__base_type_layout_map_2[];
Word * mercury_data_map__base_type_info_map_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) ENTRY(mercury____Unify___map__map_2_0),
	(Word *) (Integer) ENTRY(mercury____Index___map__map_2_0),
	(Word *) (Integer) ENTRY(mercury____Compare___map__map_2_0),
	(Word *) (Integer) mercury_data_map__base_type_layout_map_2
};

extern Word * mercury_data_map__common_1[];
Word * mercury_data_map__base_type_layout_map_2[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_map__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_map__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_map__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_map__common_1)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_map__common_0[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2)
};

Word * mercury_data_map__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_map__common_0)
};

BEGIN_MODULE(mercury__map_module0)
	init_entry(mercury__map__assoc_list_member__ua40001_3_0);
	init_label(mercury__map__assoc_list_member__ua40001_3_0_i5);
	init_label(mercury__map__assoc_list_member__ua40001_3_0_i4);
	init_label(mercury__map__assoc_list_member__ua40001_3_0_i1009);
	init_label(mercury__map__assoc_list_member__ua40001_3_0_i2);
BEGIN_CODE

/* code for predicate 'assoc_list_member__ua40001'/3 in mode 0 */
Define_static(mercury__map__assoc_list_member__ua40001_3_0);
	{
	Declare_entry(do_redo);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_redo));
	}
	mkframe("assoc_list_member__ua40001/3", 4, LABEL(mercury__map__assoc_list_member__ua40001_3_0_i4));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	framevar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	framevar(0) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	framevar(1) = (Integer) r2;
	framevar(3) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__map__assoc_list_member__ua40001_3_0_i5,
		STATIC(mercury__map__assoc_list_member__ua40001_3_0));
	}
	}
Define_label(mercury__map__assoc_list_member__ua40001_3_0_i5);
	update_prof_current_proc(LABEL(mercury__map__assoc_list_member__ua40001_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__assoc_list_member__ua40001_3_0_i1009);
	r1 = (Integer) framevar(0);
	succeed();
Define_label(mercury__map__assoc_list_member__ua40001_3_0_i4);
	update_prof_current_proc(LABEL(mercury__map__assoc_list_member__ua40001_3_0));
Define_label(mercury__map__assoc_list_member__ua40001_3_0_i1009);
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r1 = (Integer) framevar(3);
	r2 = (Integer) framevar(1);
	r3 = (Integer) framevar(2);
	localcall(mercury__map__assoc_list_member__ua40001_3_0,
		LABEL(mercury__map__assoc_list_member__ua40001_3_0_i2),
		STATIC(mercury__map__assoc_list_member__ua40001_3_0));
Define_label(mercury__map__assoc_list_member__ua40001_3_0_i2);
	update_prof_current_proc(LABEL(mercury__map__assoc_list_member__ua40001_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__map_module1)
	init_entry(mercury__map__optimize__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'map__optimize__ua10000'/2 in mode 0 */
Define_static(mercury__map__optimize__ua10000_2_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module2)
	init_entry(mercury__map__init_1_0);
BEGIN_CODE

/* code for predicate 'map__init'/1 in mode 0 */
Define_entry(mercury__map__init_1_0);
	{
	Declare_entry(mercury__tree234__init_1_0);
	tailcall(ENTRY(mercury__tree234__init_1_0),
		ENTRY(mercury__map__init_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module3)
	init_entry(mercury__map__is_empty_1_0);
	init_label(mercury__map__is_empty_1_0_i2);
BEGIN_CODE

/* code for predicate 'map__is_empty'/1 in mode 0 */
Define_entry(mercury__map__is_empty_1_0);
	incr_sp_push_msg(4, "map__is_empty");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__tree234__init_1_0);
	call_localret(ENTRY(mercury__tree234__init_1_0),
		mercury__map__is_empty_1_0_i2,
		ENTRY(mercury__map__is_empty_1_0));
	}
Define_label(mercury__map__is_empty_1_0_i2);
	update_prof_current_proc(LABEL(mercury__map__is_empty_1_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury__map__is_empty_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module4)
	init_entry(mercury__map__contains_2_0);
BEGIN_CODE

/* code for predicate 'map__contains'/2 in mode 0 */
Define_entry(mercury__map__contains_2_0);
	{
		tailcall(STATIC(mercury__map__search_3_1),
		ENTRY(mercury__map__contains_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module5)
	init_entry(mercury__map__member_3_0);
	init_label(mercury__map__member_3_0_i1);
BEGIN_CODE

/* code for predicate 'map__member'/3 in mode 0 */
Define_entry(mercury__map__member_3_0);
	{
	Declare_entry(do_fail);
	mkframe("map__member/3", 1, ENTRY(do_fail));
	}
	{
	Declare_entry(mercury__tree234__member_3_0);
	call_localret(ENTRY(mercury__tree234__member_3_0),
		mercury__map__member_3_0_i1,
		ENTRY(mercury__map__member_3_0));
	}
Define_label(mercury__map__member_3_0_i1);
	update_prof_current_proc(LABEL(mercury__map__member_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__map_module6)
	init_entry(mercury__map__search_3_0);
	init_label(mercury__map__search_3_0_i2);
	init_label(mercury__map__search_3_0_i1);
BEGIN_CODE

/* code for predicate 'map__search'/3 in mode 0 */
Define_entry(mercury__map__search_3_0);
	incr_sp_push_msg(3, "map__search");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__tree234__search_3_0);
	call_localret(ENTRY(mercury__tree234__search_3_0),
		mercury__map__search_3_0_i2,
		ENTRY(mercury__map__search_3_0));
	}
Define_label(mercury__map__search_3_0_i2);
	update_prof_current_proc(LABEL(mercury__map__search_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__search_3_0_i1);
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury__map__search_3_0));
	}
Define_label(mercury__map__search_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module7)
	init_entry(mercury__map__search_3_1);
	init_label(mercury__map__search_3_1_i2);
	init_label(mercury__map__search_3_1_i1000);
BEGIN_CODE

/* code for predicate 'map__search'/3 in mode 1 */
Define_entry(mercury__map__search_3_1);
	incr_sp_push_msg(1, "map__search");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__search_3_0);
	call_localret(ENTRY(mercury__tree234__search_3_0),
		mercury__map__search_3_1_i2,
		ENTRY(mercury__map__search_3_1));
	}
Define_label(mercury__map__search_3_1_i2);
	update_prof_current_proc(LABEL(mercury__map__search_3_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__search_3_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__map__search_3_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module8)
	init_entry(mercury__map__lookup_3_0);
	init_label(mercury__map__lookup_3_0_i4);
	init_label(mercury__map__lookup_3_0_i3);
	init_label(mercury__map__lookup_3_0_i9);
BEGIN_CODE

/* code for predicate 'map__lookup'/3 in mode 0 */
Define_entry(mercury__map__lookup_3_0);
	incr_sp_push_msg(3, "map__lookup");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__tree234__search_3_0);
	call_localret(ENTRY(mercury__tree234__search_3_0),
		mercury__map__lookup_3_0_i4,
		ENTRY(mercury__map__lookup_3_0));
	}
Define_label(mercury__map__lookup_3_0_i4);
	update_prof_current_proc(LABEL(mercury__map__lookup_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__lookup_3_0_i3);
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury__map__lookup_3_0));
	}
Define_label(mercury__map__lookup_3_0_i3);
	r1 = string_const("map__lookup: key not found", 26);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__map__lookup_3_0_i9,
		ENTRY(mercury__map__lookup_3_0));
	}
Define_label(mercury__map__lookup_3_0_i9);
	update_prof_current_proc(LABEL(mercury__map__lookup_3_0));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module9)
	init_entry(mercury__map__lookup_3_1);
	init_label(mercury__map__lookup_3_1_i4);
	init_label(mercury__map__lookup_3_1_i1000);
BEGIN_CODE

/* code for predicate 'map__lookup'/3 in mode 1 */
Define_entry(mercury__map__lookup_3_1);
	incr_sp_push_msg(1, "map__lookup");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__search_3_0);
	call_localret(ENTRY(mercury__tree234__search_3_0),
		mercury__map__lookup_3_1_i4,
		ENTRY(mercury__map__lookup_3_1));
	}
Define_label(mercury__map__lookup_3_1_i4);
	update_prof_current_proc(LABEL(mercury__map__lookup_3_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__lookup_3_1_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__map__lookup_3_1_i1000);
	r1 = string_const("map__lookup: key not found", 26);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__map__lookup_3_1));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module10)
	init_entry(mercury__map__inverse_search_3_0);
	init_label(mercury__map__inverse_search_3_0_i1);
	init_label(mercury__map__inverse_search_3_0_i2);
BEGIN_CODE

/* code for predicate 'map__inverse_search'/3 in mode 0 */
Define_entry(mercury__map__inverse_search_3_0);
	{
	Declare_entry(do_fail);
	mkframe("map__inverse_search/3", 2, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r4;
	framevar(1) = (Integer) r2;
	{
	Declare_entry(mercury__tree234__tree234_to_assoc_list_2_0);
	call_localret(ENTRY(mercury__tree234__tree234_to_assoc_list_2_0),
		mercury__map__inverse_search_3_0_i1,
		ENTRY(mercury__map__inverse_search_3_0));
	}
Define_label(mercury__map__inverse_search_3_0_i1);
	update_prof_current_proc(LABEL(mercury__map__inverse_search_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) framevar(1);
	r2 = (Integer) framevar(0);
	call_localret(STATIC(mercury__map__assoc_list_member__ua40001_3_0),
		mercury__map__inverse_search_3_0_i2,
		ENTRY(mercury__map__inverse_search_3_0));
Define_label(mercury__map__inverse_search_3_0_i2);
	update_prof_current_proc(LABEL(mercury__map__inverse_search_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__map_module11)
	init_entry(mercury__map__insert_4_0);
	init_label(mercury__map__insert_4_0_i2);
	init_label(mercury__map__insert_4_0_i1000);
BEGIN_CODE

/* code for predicate 'map__insert'/4 in mode 0 */
Define_entry(mercury__map__insert_4_0);
	incr_sp_push_msg(1, "map__insert");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__insert_4_0);
	call_localret(ENTRY(mercury__tree234__insert_4_0),
		mercury__map__insert_4_0_i2,
		ENTRY(mercury__map__insert_4_0));
	}
Define_label(mercury__map__insert_4_0_i2);
	update_prof_current_proc(LABEL(mercury__map__insert_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__insert_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__map__insert_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module12)
	init_entry(mercury__map__det_insert_4_0);
	init_label(mercury__map__det_insert_4_0_i4);
	init_label(mercury__map__det_insert_4_0_i1000);
BEGIN_CODE

/* code for predicate 'map__det_insert'/4 in mode 0 */
Define_entry(mercury__map__det_insert_4_0);
	incr_sp_push_msg(1, "map__det_insert");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__insert_4_0);
	call_localret(ENTRY(mercury__tree234__insert_4_0),
		mercury__map__det_insert_4_0_i4,
		ENTRY(mercury__map__det_insert_4_0));
	}
Define_label(mercury__map__det_insert_4_0_i4);
	update_prof_current_proc(LABEL(mercury__map__det_insert_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__det_insert_4_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__map__det_insert_4_0_i1000);
	r1 = string_const("map__det_insert: key already present", 36);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__map__det_insert_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module13)
	init_entry(mercury__map__det_insert_from_corresponding_lists_4_0);
	init_label(mercury__map__det_insert_from_corresponding_lists_4_0_i6);
	init_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1007);
	init_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1005);
	init_label(mercury__map__det_insert_from_corresponding_lists_4_0_i8);
	init_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1006);
BEGIN_CODE

/* code for predicate 'map__det_insert_from_corresponding_lists'/4 in mode 0 */
Define_entry(mercury__map__det_insert_from_corresponding_lists_4_0);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__det_insert_from_corresponding_lists_4_0_i1007);
	incr_sp_push_msg(5, "map__det_insert_from_corresponding_lists");
	detstackvar(5) = (Integer) succip;
	if (((Integer) r5 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__det_insert_from_corresponding_lists_4_0_i1005);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r5, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r4 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	r5 = (Integer) field(mktag(1), (Integer) r5, ((Integer) 0));
	{
		call_localret(STATIC(mercury__map__det_insert_4_0),
		mercury__map__det_insert_from_corresponding_lists_4_0_i6,
		ENTRY(mercury__map__det_insert_from_corresponding_lists_4_0));
	}
Define_label(mercury__map__det_insert_from_corresponding_lists_4_0_i6);
	update_prof_current_proc(LABEL(mercury__map__det_insert_from_corresponding_lists_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__map__det_insert_from_corresponding_lists_4_0,
		ENTRY(mercury__map__det_insert_from_corresponding_lists_4_0));
Define_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1007);
	incr_sp_push_msg(5, "map__det_insert_from_corresponding_lists");
	detstackvar(5) = (Integer) succip;
Define_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1005);
	if (((Integer) r4 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__det_insert_from_corresponding_lists_4_0_i8);
	decr_sp_pop_msg(5);
	if (((Integer) r5 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__det_insert_from_corresponding_lists_4_0_i1006);
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__map__det_insert_from_corresponding_lists_4_0_i8);
	r1 = string_const("map__det_insert_from_corresponding_lists - lists do not correspond", 66);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__map__det_insert_from_corresponding_lists_4_0));
	}
Define_label(mercury__map__det_insert_from_corresponding_lists_4_0_i1006);
	r1 = string_const("map__det_insert_from_corresponding_lists - lists do not correspond", 66);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__map__det_insert_from_corresponding_lists_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module14)
	init_entry(mercury__map__update_4_0);
	init_label(mercury__map__update_4_0_i2);
	init_label(mercury__map__update_4_0_i1000);
BEGIN_CODE

/* code for predicate 'map__update'/4 in mode 0 */
Define_entry(mercury__map__update_4_0);
	incr_sp_push_msg(1, "map__update");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__update_4_0);
	call_localret(ENTRY(mercury__tree234__update_4_0),
		mercury__map__update_4_0_i2,
		ENTRY(mercury__map__update_4_0));
	}
Define_label(mercury__map__update_4_0_i2);
	update_prof_current_proc(LABEL(mercury__map__update_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__update_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__map__update_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module15)
	init_entry(mercury__map__det_update_4_0);
	init_label(mercury__map__det_update_4_0_i4);
	init_label(mercury__map__det_update_4_0_i1000);
BEGIN_CODE

/* code for predicate 'map__det_update'/4 in mode 0 */
Define_entry(mercury__map__det_update_4_0);
	incr_sp_push_msg(1, "map__det_update");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__update_4_0);
	call_localret(ENTRY(mercury__tree234__update_4_0),
		mercury__map__det_update_4_0_i4,
		ENTRY(mercury__map__det_update_4_0));
	}
Define_label(mercury__map__det_update_4_0_i4);
	update_prof_current_proc(LABEL(mercury__map__det_update_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__det_update_4_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__map__det_update_4_0_i1000);
	r1 = string_const("map__det_update: key not found", 30);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__map__det_update_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module16)
	init_entry(mercury__map__set_4_0);
BEGIN_CODE

/* code for predicate 'map__set'/4 in mode 0 */
Define_entry(mercury__map__set_4_0);
	{
	Declare_entry(mercury__tree234__set_4_0);
	tailcall(ENTRY(mercury__tree234__set_4_0),
		ENTRY(mercury__map__set_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module17)
	init_entry(mercury__map__set_4_1);
BEGIN_CODE

/* code for predicate 'map__set'/4 in mode 1 */
Define_entry(mercury__map__set_4_1);
	{
	Declare_entry(mercury__tree234__set_4_2);
	tailcall(ENTRY(mercury__tree234__set_4_2),
		ENTRY(mercury__map__set_4_1));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module18)
	init_entry(mercury__map__keys_2_0);
BEGIN_CODE

/* code for predicate 'map__keys'/2 in mode 0 */
Define_entry(mercury__map__keys_2_0);
	{
	Declare_entry(mercury__tree234__keys_2_0);
	tailcall(ENTRY(mercury__tree234__keys_2_0),
		ENTRY(mercury__map__keys_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module19)
	init_entry(mercury__map__values_2_0);
BEGIN_CODE

/* code for predicate 'map__values'/2 in mode 0 */
Define_entry(mercury__map__values_2_0);
	{
	Declare_entry(mercury__tree234__values_2_0);
	tailcall(ENTRY(mercury__tree234__values_2_0),
		ENTRY(mercury__map__values_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module20)
	init_entry(mercury__map__to_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'map__to_assoc_list'/2 in mode 0 */
Define_entry(mercury__map__to_assoc_list_2_0);
	{
	Declare_entry(mercury__tree234__tree234_to_assoc_list_2_0);
	tailcall(ENTRY(mercury__tree234__tree234_to_assoc_list_2_0),
		ENTRY(mercury__map__to_assoc_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module21)
	init_entry(mercury__map__from_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'map__from_assoc_list'/2 in mode 0 */
Define_entry(mercury__map__from_assoc_list_2_0);
	{
	Declare_entry(mercury__tree234__assoc_list_to_tree234_2_0);
	tailcall(ENTRY(mercury__tree234__assoc_list_to_tree234_2_0),
		ENTRY(mercury__map__from_assoc_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module22)
	init_entry(mercury__map__from_sorted_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'map__from_sorted_assoc_list'/2 in mode 0 */
Define_entry(mercury__map__from_sorted_assoc_list_2_0);
	{
	Declare_entry(mercury__tree234__assoc_list_to_tree234_2_0);
	tailcall(ENTRY(mercury__tree234__assoc_list_to_tree234_2_0),
		ENTRY(mercury__map__from_sorted_assoc_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module23)
	init_entry(mercury__map__delete_3_0);
BEGIN_CODE

/* code for predicate 'map__delete'/3 in mode 0 */
Define_entry(mercury__map__delete_3_0);
	{
	Declare_entry(mercury__tree234__delete_3_0);
	tailcall(ENTRY(mercury__tree234__delete_3_0),
		ENTRY(mercury__map__delete_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module24)
	init_entry(mercury__map__delete_3_1);
BEGIN_CODE

/* code for predicate 'map__delete'/3 in mode 1 */
Define_entry(mercury__map__delete_3_1);
	{
	Declare_entry(mercury__tree234__delete_3_2);
	tailcall(ENTRY(mercury__tree234__delete_3_2),
		ENTRY(mercury__map__delete_3_1));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module25)
	init_entry(mercury__map__delete_list_3_0);
	init_label(mercury__map__delete_list_3_0_i4);
	init_label(mercury__map__delete_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'map__delete_list'/3 in mode 0 */
Define_entry(mercury__map__delete_list_3_0);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__delete_list_3_0_i1002);
	incr_sp_push_msg(4, "map__delete_list");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r4 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	{
		call_localret(STATIC(mercury__map__delete_3_0),
		mercury__map__delete_list_3_0_i4,
		ENTRY(mercury__map__delete_list_3_0));
	}
Define_label(mercury__map__delete_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__map__delete_list_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__map__delete_list_3_0,
		ENTRY(mercury__map__delete_list_3_0));
Define_label(mercury__map__delete_list_3_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module26)
	init_entry(mercury__map__delete_list_3_1);
	init_label(mercury__map__delete_list_3_1_i4);
	init_label(mercury__map__delete_list_3_1_i1002);
BEGIN_CODE

/* code for predicate 'map__delete_list'/3 in mode 1 */
Define_entry(mercury__map__delete_list_3_1);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__delete_list_3_1_i1002);
	incr_sp_push_msg(4, "map__delete_list");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r4 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	{
		call_localret(STATIC(mercury__map__delete_3_1),
		mercury__map__delete_list_3_1_i4,
		ENTRY(mercury__map__delete_list_3_1));
	}
Define_label(mercury__map__delete_list_3_1_i4);
	update_prof_current_proc(LABEL(mercury__map__delete_list_3_1));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__map__delete_list_3_1,
		ENTRY(mercury__map__delete_list_3_1));
Define_label(mercury__map__delete_list_3_1_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module27)
	init_entry(mercury__map__remove_4_0);
	init_label(mercury__map__remove_4_0_i2);
	init_label(mercury__map__remove_4_0_i1000);
BEGIN_CODE

/* code for predicate 'map__remove'/4 in mode 0 */
Define_entry(mercury__map__remove_4_0);
	incr_sp_push_msg(1, "map__remove");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__remove_4_2);
	call_localret(ENTRY(mercury__tree234__remove_4_2),
		mercury__map__remove_4_0_i2,
		ENTRY(mercury__map__remove_4_0));
	}
Define_label(mercury__map__remove_4_0_i2);
	update_prof_current_proc(LABEL(mercury__map__remove_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__remove_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__map__remove_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module28)
	init_entry(mercury__map__det_remove_4_0);
	init_label(mercury__map__det_remove_4_0_i4);
	init_label(mercury__map__det_remove_4_0_i1000);
BEGIN_CODE

/* code for predicate 'map__det_remove'/4 in mode 0 */
Define_entry(mercury__map__det_remove_4_0);
	incr_sp_push_msg(1, "map__det_remove");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__remove_4_2);
	call_localret(ENTRY(mercury__tree234__remove_4_2),
		mercury__map__det_remove_4_0_i4,
		ENTRY(mercury__map__det_remove_4_0));
	}
Define_label(mercury__map__det_remove_4_0_i4);
	update_prof_current_proc(LABEL(mercury__map__det_remove_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__det_remove_4_0_i1000);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__map__det_remove_4_0_i1000);
	r1 = string_const("map__det_remove: key not found", 30);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__map__det_remove_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module29)
	init_entry(mercury__map__count_2_0);
BEGIN_CODE

/* code for predicate 'map__count'/2 in mode 0 */
Define_entry(mercury__map__count_2_0);
	{
	Declare_entry(mercury__tree234__count_2_0);
	tailcall(ENTRY(mercury__tree234__count_2_0),
		ENTRY(mercury__map__count_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module30)
	init_entry(mercury__map__from_corresponding_lists_3_0);
	init_label(mercury__map__from_corresponding_lists_3_0_i2);
BEGIN_CODE

/* code for predicate 'map__from_corresponding_lists'/3 in mode 0 */
Define_entry(mercury__map__from_corresponding_lists_3_0);
	incr_sp_push_msg(3, "map__from_corresponding_lists");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__assoc_list__from_corresponding_lists_3_0);
	call_localret(ENTRY(mercury__assoc_list__from_corresponding_lists_3_0),
		mercury__map__from_corresponding_lists_3_0_i2,
		ENTRY(mercury__map__from_corresponding_lists_3_0));
	}
Define_label(mercury__map__from_corresponding_lists_3_0_i2);
	update_prof_current_proc(LABEL(mercury__map__from_corresponding_lists_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__tree234__assoc_list_to_tree234_2_0);
	tailcall(ENTRY(mercury__tree234__assoc_list_to_tree234_2_0),
		ENTRY(mercury__map__from_corresponding_lists_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module31)
	init_entry(mercury__map__merge_3_0);
	init_label(mercury__map__merge_3_0_i2);
	init_label(mercury__map__merge_3_0_i3);
	init_label(mercury__map__merge_3_0_i4);
BEGIN_CODE

/* code for predicate 'map__merge'/3 in mode 0 */
Define_entry(mercury__map__merge_3_0);
	incr_sp_push_msg(4, "map__merge");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
		call_localret(STATIC(mercury__map__to_assoc_list_2_0),
		mercury__map__merge_3_0_i2,
		ENTRY(mercury__map__merge_3_0));
	}
Define_label(mercury__map__merge_3_0_i2);
	update_prof_current_proc(LABEL(mercury__map__merge_3_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__map__to_assoc_list_2_0),
		mercury__map__merge_3_0_i3,
		ENTRY(mercury__map__merge_3_0));
	}
Define_label(mercury__map__merge_3_0_i3);
	update_prof_current_proc(LABEL(mercury__map__merge_3_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	r2 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	{
	extern Word * mercury_data_std_util__base_type_info_pair_2[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	}
	{
	Declare_entry(mercury__list__merge_3_0);
	call_localret(ENTRY(mercury__list__merge_3_0),
		mercury__map__merge_3_0_i4,
		ENTRY(mercury__map__merge_3_0));
	}
Define_label(mercury__map__merge_3_0_i4);
	update_prof_current_proc(LABEL(mercury__map__merge_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__map__from_sorted_assoc_list_2_0),
		ENTRY(mercury__map__merge_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module32)
	init_entry(mercury__map__overlay_3_0);
	init_label(mercury__map__overlay_3_0_i2);
BEGIN_CODE

/* code for predicate 'map__overlay'/3 in mode 0 */
Define_entry(mercury__map__overlay_3_0);
	incr_sp_push_msg(4, "map__overlay");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r3 = (Integer) r4;
	{
		call_localret(STATIC(mercury__map__to_assoc_list_2_0),
		mercury__map__overlay_3_0_i2,
		ENTRY(mercury__map__overlay_3_0));
	}
Define_label(mercury__map__overlay_3_0_i2);
	update_prof_current_proc(LABEL(mercury__map__overlay_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__map__overlay_2_3_0),
		ENTRY(mercury__map__overlay_3_0));
END_MODULE

BEGIN_MODULE(mercury__map_module33)
	init_entry(mercury__map__select_3_0);
	init_label(mercury__map__select_3_0_i2);
	init_label(mercury__map__select_3_0_i3);
BEGIN_CODE

/* code for predicate 'map__select'/3 in mode 0 */
Define_entry(mercury__map__select_3_0);
	incr_sp_push_msg(5, "map__select");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__map__select_3_0_i2,
		ENTRY(mercury__map__select_3_0));
	}
Define_label(mercury__map__select_3_0_i2);
	update_prof_current_proc(LABEL(mercury__map__select_3_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	{
		call_localret(STATIC(mercury__map__init_1_0),
		mercury__map__select_3_0_i3,
		ENTRY(mercury__map__select_3_0));
	}
Define_label(mercury__map__select_3_0_i3);
	update_prof_current_proc(LABEL(mercury__map__select_3_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__map__select_2_4_0),
		ENTRY(mercury__map__select_3_0));
END_MODULE

BEGIN_MODULE(mercury__map_module34)
	init_entry(mercury__map__apply_to_list_3_0);
	init_label(mercury__map__apply_to_list_3_0_i4);
	init_label(mercury__map__apply_to_list_3_0_i5);
	init_label(mercury__map__apply_to_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'map__apply_to_list'/3 in mode 0 */
Define_entry(mercury__map__apply_to_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__apply_to_list_3_0_i1002);
	incr_sp_push_msg(5, "map__apply_to_list");
	detstackvar(5) = (Integer) succip;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) r4;
	{
	Word tempr1;
	tempr1 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	{
		call_localret(STATIC(mercury__map__lookup_3_1),
		mercury__map__apply_to_list_3_0_i4,
		ENTRY(mercury__map__apply_to_list_3_0));
	}
	}
Define_label(mercury__map__apply_to_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__map__apply_to_list_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__map__apply_to_list_3_0,
		LABEL(mercury__map__apply_to_list_3_0_i5),
		ENTRY(mercury__map__apply_to_list_3_0));
Define_label(mercury__map__apply_to_list_3_0_i5);
	update_prof_current_proc(LABEL(mercury__map__apply_to_list_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__map__apply_to_list_3_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module35)
	init_entry(mercury__map__optimize_2_0);
BEGIN_CODE

/* code for predicate 'map__optimize'/2 in mode 0 */
Define_entry(mercury__map__optimize_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__map__optimize__ua10000_2_0),
		ENTRY(mercury__map__optimize_2_0));
END_MODULE

BEGIN_MODULE(mercury__map_module36)
	init_entry(mercury__map__remove_smallest_4_0);
	init_label(mercury__map__remove_smallest_4_0_i2);
	init_label(mercury__map__remove_smallest_4_0_i1000);
BEGIN_CODE

/* code for predicate 'map__remove_smallest'/4 in mode 0 */
Define_entry(mercury__map__remove_smallest_4_0);
	incr_sp_push_msg(1, "map__remove_smallest");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__tree234__remove_smallest_4_2);
	call_localret(ENTRY(mercury__tree234__remove_smallest_4_2),
		mercury__map__remove_smallest_4_0_i2,
		ENTRY(mercury__map__remove_smallest_4_0));
	}
Define_label(mercury__map__remove_smallest_4_0_i2);
	update_prof_current_proc(LABEL(mercury__map__remove_smallest_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__remove_smallest_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__map__remove_smallest_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module37)
	init_entry(mercury__map__overlay_2_3_0);
	init_label(mercury__map__overlay_2_3_0_i4);
	init_label(mercury__map__overlay_2_3_0_i1002);
BEGIN_CODE

/* code for predicate 'map__overlay_2'/3 in mode 0 */
Define_static(mercury__map__overlay_2_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__overlay_2_3_0_i1002);
	incr_sp_push_msg(4, "map__overlay_2");
	detstackvar(4) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) r4;
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r5 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
		call_localret(STATIC(mercury__map__set_4_1),
		mercury__map__overlay_2_3_0_i4,
		STATIC(mercury__map__overlay_2_3_0));
	}
	}
Define_label(mercury__map__overlay_2_3_0_i4);
	update_prof_current_proc(LABEL(mercury__map__overlay_2_3_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__map__overlay_2_3_0,
		STATIC(mercury__map__overlay_2_3_0));
Define_label(mercury__map__overlay_2_3_0_i1002);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module38)
	init_entry(mercury__map__select_2_4_0);
	init_label(mercury__map__select_2_4_0_i6);
	init_label(mercury__map__select_2_4_0_i8);
	init_label(mercury__map__select_2_4_0_i5);
	init_label(mercury__map__select_2_4_0_i1003);
BEGIN_CODE

/* code for predicate 'map__select_2'/4 in mode 0 */
Define_static(mercury__map__select_2_4_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__map__select_2_4_0_i1003);
	incr_sp_push_msg(7, "map__select_2");
	detstackvar(7) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) r4;
	detstackvar(3) = (Integer) tempr1;
	r3 = (Integer) r4;
	r4 = (Integer) tempr1;
	detstackvar(2) = (Integer) r5;
	detstackvar(5) = (Integer) r1;
	detstackvar(6) = (Integer) r2;
	{
		call_localret(STATIC(mercury__map__search_3_1),
		mercury__map__select_2_4_0_i6,
		STATIC(mercury__map__select_2_4_0));
	}
	}
Define_label(mercury__map__select_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__map__select_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__map__select_2_4_0_i5);
	r5 = (Integer) r2;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__map__set_4_1),
		mercury__map__select_2_4_0_i8,
		STATIC(mercury__map__select_2_4_0));
	}
Define_label(mercury__map__select_2_4_0_i8);
	update_prof_current_proc(LABEL(mercury__map__select_2_4_0));
	r4 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__map__select_2_4_0,
		STATIC(mercury__map__select_2_4_0));
Define_label(mercury__map__select_2_4_0_i5);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__map__select_2_4_0,
		STATIC(mercury__map__select_2_4_0));
Define_label(mercury__map__select_2_4_0_i1003);
	r1 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__map_module39)
	init_entry(mercury____Unify___map__map_2_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___map__map_2_0);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___map__map_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module40)
	init_entry(mercury____Index___map__map_2_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___map__map_2_0);
	{
	Declare_entry(mercury____Index___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Index___tree234__tree234_2_0),
		ENTRY(mercury____Index___map__map_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__map_module41)
	init_entry(mercury____Compare___map__map_2_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___map__map_2_0);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___map__map_2_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__map_bunch_0(void)
{
	mercury__map_module0();
	mercury__map_module1();
	mercury__map_module2();
	mercury__map_module3();
	mercury__map_module4();
	mercury__map_module5();
	mercury__map_module6();
	mercury__map_module7();
	mercury__map_module8();
	mercury__map_module9();
	mercury__map_module10();
	mercury__map_module11();
	mercury__map_module12();
	mercury__map_module13();
	mercury__map_module14();
	mercury__map_module15();
	mercury__map_module16();
	mercury__map_module17();
	mercury__map_module18();
	mercury__map_module19();
	mercury__map_module20();
	mercury__map_module21();
	mercury__map_module22();
	mercury__map_module23();
	mercury__map_module24();
	mercury__map_module25();
	mercury__map_module26();
	mercury__map_module27();
	mercury__map_module28();
	mercury__map_module29();
	mercury__map_module30();
	mercury__map_module31();
	mercury__map_module32();
	mercury__map_module33();
	mercury__map_module34();
	mercury__map_module35();
	mercury__map_module36();
	mercury__map_module37();
	mercury__map_module38();
	mercury__map_module39();
	mercury__map_module40();
}

static void mercury__map_bunch_1(void)
{
	mercury__map_module41();
}

#endif

void mercury__map__init(void); /* suppress gcc warning */
void mercury__map__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__map_bunch_0();
	mercury__map_bunch_1();
#endif
}
