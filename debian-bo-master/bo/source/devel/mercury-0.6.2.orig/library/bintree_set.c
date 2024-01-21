/*
** Automatically generated from `bintree_set.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__bintree_set__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__bintree_set__assoc_unit__ua10000_2_0);
Declare_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i3);
Declare_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i4);
Declare_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i1);
Define_extern_entry(mercury__bintree_set__list_to_set_2_0);
Declare_label(mercury__bintree_set__list_to_set_2_0_i2);
Define_extern_entry(mercury__bintree_set__sorted_list_to_set_2_0);
Declare_label(mercury__bintree_set__sorted_list_to_set_2_0_i2);
Define_extern_entry(mercury__bintree_set__to_sorted_list_2_0);
Define_extern_entry(mercury__bintree_set__init_1_0);
Define_extern_entry(mercury__bintree_set__singleton_set_2_0);
Declare_label(mercury__bintree_set__singleton_set_2_0_i2);
Define_extern_entry(mercury__bintree_set__equal_2_0);
Declare_label(mercury__bintree_set__equal_2_0_i2);
Declare_label(mercury__bintree_set__equal_2_0_i3);
Define_extern_entry(mercury__bintree_set__subset_2_0);
Declare_label(mercury__bintree_set__subset_2_0_i2);
Define_extern_entry(mercury__bintree_set__superset_2_0);
Define_extern_entry(mercury__bintree_set__member_2_0);
Declare_label(mercury__bintree_set__member_2_0_i2);
Define_extern_entry(mercury__bintree_set__member_2_1);
Declare_label(mercury__bintree_set__member_2_1_i1);
Declare_label(mercury__bintree_set__member_2_1_i2);
Define_extern_entry(mercury__bintree_set__is_member_2_0);
Define_extern_entry(mercury__bintree_set__insert_3_0);
Define_extern_entry(mercury__bintree_set__insert_3_1);
Define_extern_entry(mercury__bintree_set__insert_list_3_0);
Declare_label(mercury__bintree_set__insert_list_3_0_i4);
Declare_label(mercury__bintree_set__insert_list_3_0_i1002);
Define_extern_entry(mercury__bintree_set__insert_list_3_1);
Declare_label(mercury__bintree_set__insert_list_3_1_i4);
Declare_label(mercury__bintree_set__insert_list_3_1_i1002);
Define_extern_entry(mercury__bintree_set__remove_3_0);
Declare_label(mercury__bintree_set__remove_3_0_i2);
Declare_label(mercury__bintree_set__remove_3_0_i1000);
Define_extern_entry(mercury__bintree_set__remove_list_3_0);
Declare_label(mercury__bintree_set__remove_list_3_0_i4);
Declare_label(mercury__bintree_set__remove_list_3_0_i6);
Declare_label(mercury__bintree_set__remove_list_3_0_i8);
Declare_label(mercury__bintree_set__remove_list_3_0_i1004);
Declare_label(mercury__bintree_set__remove_list_3_0_i1);
Declare_label(mercury__bintree_set__remove_list_3_0_i1006);
Define_extern_entry(mercury__bintree_set__delete_3_0);
Define_extern_entry(mercury__bintree_set__delete_list_3_0);
Declare_label(mercury__bintree_set__delete_list_3_0_i4);
Declare_label(mercury__bintree_set__delete_list_3_0_i1002);
Define_extern_entry(mercury__bintree_set__union_3_0);
Declare_label(mercury__bintree_set__union_3_0_i2);
Declare_label(mercury__bintree_set__union_3_0_i3);
Declare_label(mercury__bintree_set__union_3_0_i4);
Define_extern_entry(mercury__bintree_set__intersect_3_0);
Declare_label(mercury__bintree_set__intersect_3_0_i2);
Declare_static(mercury__bintree_set__contains_list_2_0);
Declare_label(mercury__bintree_set__contains_list_2_0_i3);
Declare_label(mercury__bintree_set__contains_list_2_0_i1003);
Declare_label(mercury__bintree_set__contains_list_2_0_i1);
Define_extern_entry(mercury____Unify___bintree_set__bintree_set_1_0);
Define_extern_entry(mercury____Index___bintree_set__bintree_set_1_0);
Define_extern_entry(mercury____Compare___bintree_set__bintree_set_1_0);

extern Word * mercury_data_bintree_set__base_type_layout_bintree_set_1[];
Word * mercury_data_bintree_set__base_type_info_bintree_set_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___bintree_set__bintree_set_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___bintree_set__bintree_set_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___bintree_set__bintree_set_1_0),
	(Word *) (Integer) mercury_data_bintree_set__base_type_layout_bintree_set_1
};

extern Word * mercury_data_bintree_set__common_1[];
Word * mercury_data_bintree_set__base_type_layout_bintree_set_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bintree_set__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bintree_set__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bintree_set__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bintree_set__common_1)
};

extern Word * mercury_data_bintree__base_type_info_bintree_2[];
extern Word * mercury_data_std_util__base_type_info_unit_0[];
Word * mercury_data_bintree_set__common_0[] = {
	(Word *) (Integer) mercury_data_bintree__base_type_info_bintree_2,
	(Word *) ((Integer) 1),
	(Word *) (Integer) mercury_data_std_util__base_type_info_unit_0
};

Word * mercury_data_bintree_set__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bintree_set__common_0)
};

BEGIN_MODULE(mercury__bintree_set_module0)
	init_entry(mercury__bintree_set__assoc_unit__ua10000_2_0);
	init_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i3);
	init_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i4);
	init_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i1);
BEGIN_CODE

/* code for predicate 'assoc_unit__ua10000'/2 in mode 0 */
Define_static(mercury__bintree_set__assoc_unit__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree_set__assoc_unit__ua10000_2_0_i1);
	r3 = (Integer) sp;
Define_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i3);
	while (1) {
	incr_sp_push_msg(1, "assoc_unit__ua10000");
	tag_incr_hp(detstackvar(1), mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) detstackvar(1), ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	field(mktag(0), (Integer) detstackvar(1), ((Integer) 1)) = ((Integer) 0);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r3))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__bintree_set__assoc_unit__ua10000_2_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module1)
	init_entry(mercury__bintree_set__list_to_set_2_0);
	init_label(mercury__bintree_set__list_to_set_2_0_i2);
BEGIN_CODE

/* code for predicate 'bintree_set__list_to_set'/2 in mode 0 */
Define_entry(mercury__bintree_set__list_to_set_2_0);
	incr_sp_push_msg(2, "bintree_set__list_to_set");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__list__sort_and_remove_dups_2_0);
	call_localret(ENTRY(mercury__list__sort_and_remove_dups_2_0),
		mercury__bintree_set__list_to_set_2_0_i2,
		ENTRY(mercury__bintree_set__list_to_set_2_0));
	}
Define_label(mercury__bintree_set__list_to_set_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__list_to_set_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__bintree_set__sorted_list_to_set_2_0),
		ENTRY(mercury__bintree_set__list_to_set_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module2)
	init_entry(mercury__bintree_set__sorted_list_to_set_2_0);
	init_label(mercury__bintree_set__sorted_list_to_set_2_0_i2);
BEGIN_CODE

/* code for predicate 'bintree_set__sorted_list_to_set'/2 in mode 0 */
Define_entry(mercury__bintree_set__sorted_list_to_set_2_0);
	incr_sp_push_msg(2, "bintree_set__sorted_list_to_set");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__bintree_set__assoc_unit__ua10000_2_0),
		mercury__bintree_set__sorted_list_to_set_2_0_i2,
		ENTRY(mercury__bintree_set__sorted_list_to_set_2_0));
Define_label(mercury__bintree_set__sorted_list_to_set_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__sorted_list_to_set_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__bintree__from_sorted_list_2_0);
	tailcall(ENTRY(mercury__bintree__from_sorted_list_2_0),
		ENTRY(mercury__bintree_set__sorted_list_to_set_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module3)
	init_entry(mercury__bintree_set__to_sorted_list_2_0);
BEGIN_CODE

/* code for predicate 'bintree_set__to_sorted_list'/2 in mode 0 */
Define_entry(mercury__bintree_set__to_sorted_list_2_0);
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__keys_2_0);
	tailcall(ENTRY(mercury__bintree__keys_2_0),
		ENTRY(mercury__bintree_set__to_sorted_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module4)
	init_entry(mercury__bintree_set__init_1_0);
BEGIN_CODE

/* code for predicate 'bintree_set__init'/1 in mode 0 */
Define_entry(mercury__bintree_set__init_1_0);
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__init_1_0);
	tailcall(ENTRY(mercury__bintree__init_1_0),
		ENTRY(mercury__bintree_set__init_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module5)
	init_entry(mercury__bintree_set__singleton_set_2_0);
	init_label(mercury__bintree_set__singleton_set_2_0_i2);
BEGIN_CODE

/* code for predicate 'bintree_set__singleton_set'/2 in mode 0 */
Define_entry(mercury__bintree_set__singleton_set_2_0);
	incr_sp_push_msg(3, "bintree_set__singleton_set");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__init_1_0);
	call_localret(ENTRY(mercury__bintree__init_1_0),
		mercury__bintree_set__singleton_set_2_0_i2,
		ENTRY(mercury__bintree_set__singleton_set_2_0));
	}
Define_label(mercury__bintree_set__singleton_set_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__singleton_set_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	r4 = (Integer) detstackvar(1);
	r5 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__bintree__set_4_1);
	tailcall(ENTRY(mercury__bintree__set_4_1),
		ENTRY(mercury__bintree_set__singleton_set_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module6)
	init_entry(mercury__bintree_set__equal_2_0);
	init_label(mercury__bintree_set__equal_2_0_i2);
	init_label(mercury__bintree_set__equal_2_0_i3);
BEGIN_CODE

/* code for predicate 'bintree_set__equal'/2 in mode 0 */
Define_entry(mercury__bintree_set__equal_2_0);
	incr_sp_push_msg(3, "bintree_set__equal");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__bintree__keys_2_0);
	call_localret(ENTRY(mercury__bintree__keys_2_0),
		mercury__bintree_set__equal_2_0_i2,
		ENTRY(mercury__bintree_set__equal_2_0));
	}
Define_label(mercury__bintree_set__equal_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__equal_2_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__keys_2_0);
	call_localret(ENTRY(mercury__bintree__keys_2_0),
		mercury__bintree_set__equal_2_0_i3,
		ENTRY(mercury__bintree_set__equal_2_0));
	}
Define_label(mercury__bintree_set__equal_2_0_i3);
	update_prof_current_proc(LABEL(mercury__bintree_set__equal_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury__bintree_set__equal_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module7)
	init_entry(mercury__bintree_set__subset_2_0);
	init_label(mercury__bintree_set__subset_2_0_i2);
BEGIN_CODE

/* code for predicate 'bintree_set__subset'/2 in mode 0 */
Define_entry(mercury__bintree_set__subset_2_0);
	incr_sp_push_msg(3, "bintree_set__subset");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__bintree__keys_2_0);
	call_localret(ENTRY(mercury__bintree__keys_2_0),
		mercury__bintree_set__subset_2_0_i2,
		ENTRY(mercury__bintree_set__subset_2_0));
	}
Define_label(mercury__bintree_set__subset_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__subset_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__bintree_set__contains_list_2_0),
		ENTRY(mercury__bintree_set__subset_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module8)
	init_entry(mercury__bintree_set__superset_2_0);
BEGIN_CODE

/* code for predicate 'bintree_set__superset'/2 in mode 0 */
Define_entry(mercury__bintree_set__superset_2_0);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	{
		tailcall(STATIC(mercury__bintree_set__subset_2_0),
		ENTRY(mercury__bintree_set__superset_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module9)
	init_entry(mercury__bintree_set__member_2_0);
	init_label(mercury__bintree_set__member_2_0_i2);
BEGIN_CODE

/* code for predicate 'bintree_set__member'/2 in mode 0 */
Define_entry(mercury__bintree_set__member_2_0);
	incr_sp_push_msg(3, "bintree_set__member");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__keys_2_0);
	call_localret(ENTRY(mercury__bintree__keys_2_0),
		mercury__bintree_set__member_2_0_i2,
		ENTRY(mercury__bintree_set__member_2_0));
	}
Define_label(mercury__bintree_set__member_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__member_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__list__member_2_0);
	tailcall(ENTRY(mercury__list__member_2_0),
		ENTRY(mercury__bintree_set__member_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module10)
	init_entry(mercury__bintree_set__member_2_1);
	init_label(mercury__bintree_set__member_2_1_i1);
	init_label(mercury__bintree_set__member_2_1_i2);
BEGIN_CODE

/* code for predicate 'bintree_set__member'/2 in mode 1 */
Define_entry(mercury__bintree_set__member_2_1);
	{
	Declare_entry(do_fail);
	mkframe("bintree_set__member/2", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__keys_2_0);
	call_localret(ENTRY(mercury__bintree__keys_2_0),
		mercury__bintree_set__member_2_1_i1,
		ENTRY(mercury__bintree_set__member_2_1));
	}
Define_label(mercury__bintree_set__member_2_1_i1);
	update_prof_current_proc(LABEL(mercury__bintree_set__member_2_1));
	r2 = (Integer) r1;
	r1 = (Integer) framevar(0);
	{
	Declare_entry(mercury__list__member_2_1);
	call_localret(ENTRY(mercury__list__member_2_1),
		mercury__bintree_set__member_2_1_i2,
		ENTRY(mercury__bintree_set__member_2_1));
	}
Define_label(mercury__bintree_set__member_2_1_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__member_2_1));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module11)
	init_entry(mercury__bintree_set__is_member_2_0);
BEGIN_CODE

/* code for predicate 'bintree_set__is_member'/2 in mode 0 */
Define_entry(mercury__bintree_set__is_member_2_0);
	r4 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__search_3_1);
	tailcall(ENTRY(mercury__bintree__search_3_1),
		ENTRY(mercury__bintree_set__is_member_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module12)
	init_entry(mercury__bintree_set__insert_3_0);
BEGIN_CODE

/* code for predicate 'bintree_set__insert'/3 in mode 0 */
Define_entry(mercury__bintree_set__insert_3_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	r5 = ((Integer) 0);
	{
	Declare_entry(mercury__bintree__set_4_0);
	tailcall(ENTRY(mercury__bintree__set_4_0),
		ENTRY(mercury__bintree_set__insert_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module13)
	init_entry(mercury__bintree_set__insert_3_1);
BEGIN_CODE

/* code for predicate 'bintree_set__insert'/3 in mode 1 */
Define_entry(mercury__bintree_set__insert_3_1);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	r5 = ((Integer) 0);
	{
	Declare_entry(mercury__bintree__set_4_1);
	tailcall(ENTRY(mercury__bintree__set_4_1),
		ENTRY(mercury__bintree_set__insert_3_1));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module14)
	init_entry(mercury__bintree_set__insert_list_3_0);
	init_label(mercury__bintree_set__insert_list_3_0_i4);
	init_label(mercury__bintree_set__insert_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'bintree_set__insert_list'/3 in mode 0 */
Define_entry(mercury__bintree_set__insert_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree_set__insert_list_3_0_i1002);
	incr_sp_push_msg(3, "bintree_set__insert_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__bintree_set__insert_3_0),
		mercury__bintree_set__insert_list_3_0_i4,
		ENTRY(mercury__bintree_set__insert_list_3_0));
	}
Define_label(mercury__bintree_set__insert_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree_set__insert_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bintree_set__insert_list_3_0,
		ENTRY(mercury__bintree_set__insert_list_3_0));
Define_label(mercury__bintree_set__insert_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module15)
	init_entry(mercury__bintree_set__insert_list_3_1);
	init_label(mercury__bintree_set__insert_list_3_1_i4);
	init_label(mercury__bintree_set__insert_list_3_1_i1002);
BEGIN_CODE

/* code for predicate 'bintree_set__insert_list'/3 in mode 1 */
Define_entry(mercury__bintree_set__insert_list_3_1);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree_set__insert_list_3_1_i1002);
	incr_sp_push_msg(3, "bintree_set__insert_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__bintree_set__insert_3_1),
		mercury__bintree_set__insert_list_3_1_i4,
		ENTRY(mercury__bintree_set__insert_list_3_1));
	}
Define_label(mercury__bintree_set__insert_list_3_1_i4);
	update_prof_current_proc(LABEL(mercury__bintree_set__insert_list_3_1));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bintree_set__insert_list_3_1,
		ENTRY(mercury__bintree_set__insert_list_3_1));
Define_label(mercury__bintree_set__insert_list_3_1_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module16)
	init_entry(mercury__bintree_set__remove_3_0);
	init_label(mercury__bintree_set__remove_3_0_i2);
	init_label(mercury__bintree_set__remove_3_0_i1000);
BEGIN_CODE

/* code for predicate 'bintree_set__remove'/3 in mode 0 */
Define_entry(mercury__bintree_set__remove_3_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	incr_sp_push_msg(1, "bintree_set__remove");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__bintree__remove_4_0);
	call_localret(ENTRY(mercury__bintree__remove_4_0),
		mercury__bintree_set__remove_3_0_i2,
		ENTRY(mercury__bintree_set__remove_3_0));
	}
Define_label(mercury__bintree_set__remove_3_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__remove_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree_set__remove_3_0_i1000);
	r1 = TRUE;
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__bintree_set__remove_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module17)
	init_entry(mercury__bintree_set__remove_list_3_0);
	init_label(mercury__bintree_set__remove_list_3_0_i4);
	init_label(mercury__bintree_set__remove_list_3_0_i6);
	init_label(mercury__bintree_set__remove_list_3_0_i8);
	init_label(mercury__bintree_set__remove_list_3_0_i1004);
	init_label(mercury__bintree_set__remove_list_3_0_i1);
	init_label(mercury__bintree_set__remove_list_3_0_i1006);
BEGIN_CODE

/* code for predicate 'bintree_set__remove_list'/3 in mode 0 */
Define_entry(mercury__bintree_set__remove_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree_set__remove_list_3_0_i1004);
	incr_sp_push_msg(5, "bintree_set__remove_list");
	detstackvar(5) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) tempr1;
	r2 = (Integer) tempr1;
	r3 = (Integer) detstackvar(1);
	detstackvar(4) = (Integer) r1;
	{
		call_localret(STATIC(mercury__bintree_set__member_2_0),
		mercury__bintree_set__remove_list_3_0_i4,
		ENTRY(mercury__bintree_set__remove_list_3_0));
	}
	}
Define_label(mercury__bintree_set__remove_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree_set__remove_list_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree_set__remove_list_3_0_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__bintree_set__remove_3_0),
		mercury__bintree_set__remove_list_3_0_i6,
		ENTRY(mercury__bintree_set__remove_list_3_0));
	}
Define_label(mercury__bintree_set__remove_list_3_0_i6);
	update_prof_current_proc(LABEL(mercury__bintree_set__remove_list_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree_set__remove_list_3_0_i1);
	r1 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(3);
	localcall(mercury__bintree_set__remove_list_3_0,
		LABEL(mercury__bintree_set__remove_list_3_0_i8),
		ENTRY(mercury__bintree_set__remove_list_3_0));
Define_label(mercury__bintree_set__remove_list_3_0_i8);
	update_prof_current_proc(LABEL(mercury__bintree_set__remove_list_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree_set__remove_list_3_0_i1006);
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree_set__remove_list_3_0_i1004);
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree_set__remove_list_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__bintree_set__remove_list_3_0_i1006);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module18)
	init_entry(mercury__bintree_set__delete_3_0);
BEGIN_CODE

/* code for predicate 'bintree_set__delete'/3 in mode 0 */
Define_entry(mercury__bintree_set__delete_3_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__delete_3_0);
	tailcall(ENTRY(mercury__bintree__delete_3_0),
		ENTRY(mercury__bintree_set__delete_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module19)
	init_entry(mercury__bintree_set__delete_list_3_0);
	init_label(mercury__bintree_set__delete_list_3_0_i4);
	init_label(mercury__bintree_set__delete_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'bintree_set__delete_list'/3 in mode 0 */
Define_entry(mercury__bintree_set__delete_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree_set__delete_list_3_0_i1002);
	incr_sp_push_msg(3, "bintree_set__delete_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__bintree_set__delete_3_0),
		mercury__bintree_set__delete_list_3_0_i4,
		ENTRY(mercury__bintree_set__delete_list_3_0));
	}
Define_label(mercury__bintree_set__delete_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree_set__delete_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bintree_set__delete_list_3_0,
		ENTRY(mercury__bintree_set__delete_list_3_0));
Define_label(mercury__bintree_set__delete_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module20)
	init_entry(mercury__bintree_set__union_3_0);
	init_label(mercury__bintree_set__union_3_0_i2);
	init_label(mercury__bintree_set__union_3_0_i3);
	init_label(mercury__bintree_set__union_3_0_i4);
BEGIN_CODE

/* code for predicate 'bintree_set__union'/3 in mode 0 */
Define_entry(mercury__bintree_set__union_3_0);
	incr_sp_push_msg(3, "bintree_set__union");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__bintree__to_list_2_0);
	call_localret(ENTRY(mercury__bintree__to_list_2_0),
		mercury__bintree_set__union_3_0_i2,
		ENTRY(mercury__bintree_set__union_3_0));
	}
Define_label(mercury__bintree_set__union_3_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__union_3_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__to_list_2_0);
	call_localret(ENTRY(mercury__bintree__to_list_2_0),
		mercury__bintree_set__union_3_0_i3,
		ENTRY(mercury__bintree_set__union_3_0));
	}
Define_label(mercury__bintree_set__union_3_0_i3);
	update_prof_current_proc(LABEL(mercury__bintree_set__union_3_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	r2 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) mercury_data_std_util__base_type_info_unit_0;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	{
	extern Word * mercury_data_std_util__base_type_info_pair_2[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	}
	{
	Declare_entry(mercury__list__merge_3_0);
	call_localret(ENTRY(mercury__list__merge_3_0),
		mercury__bintree_set__union_3_0_i4,
		ENTRY(mercury__bintree_set__union_3_0));
	}
Define_label(mercury__bintree_set__union_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree_set__union_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__bintree__from_sorted_list_2_0);
	tailcall(ENTRY(mercury__bintree__from_sorted_list_2_0),
		ENTRY(mercury__bintree_set__union_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module21)
	init_entry(mercury__bintree_set__intersect_3_0);
	init_label(mercury__bintree_set__intersect_3_0_i2);
BEGIN_CODE

/* code for predicate 'bintree_set__intersect'/3 in mode 0 */
Define_entry(mercury__bintree_set__intersect_3_0);
	incr_sp_push_msg(3, "bintree_set__intersect");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury__bintree__keys_2_0);
	call_localret(ENTRY(mercury__bintree__keys_2_0),
		mercury__bintree_set__intersect_3_0_i2,
		ENTRY(mercury__bintree_set__intersect_3_0));
	}
Define_label(mercury__bintree_set__intersect_3_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree_set__intersect_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__bintree_set__delete_list_3_0),
		ENTRY(mercury__bintree_set__intersect_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module22)
	init_entry(mercury__bintree_set__contains_list_2_0);
	init_label(mercury__bintree_set__contains_list_2_0_i3);
	init_label(mercury__bintree_set__contains_list_2_0_i1003);
	init_label(mercury__bintree_set__contains_list_2_0_i1);
BEGIN_CODE

/* code for predicate 'bintree_set__contains_list'/2 in mode 0 */
Define_static(mercury__bintree_set__contains_list_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree_set__contains_list_2_0_i1003);
	incr_sp_push_msg(4, "bintree_set__contains_list");
	detstackvar(4) = (Integer) succip;
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__bintree__search_3_1);
	call_localret(ENTRY(mercury__bintree__search_3_1),
		mercury__bintree_set__contains_list_2_0_i3,
		STATIC(mercury__bintree_set__contains_list_2_0));
	}
Define_label(mercury__bintree_set__contains_list_2_0_i3);
	update_prof_current_proc(LABEL(mercury__bintree_set__contains_list_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree_set__contains_list_2_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__bintree_set__contains_list_2_0,
		STATIC(mercury__bintree_set__contains_list_2_0));
Define_label(mercury__bintree_set__contains_list_2_0_i1003);
	r1 = FALSE;
	proceed();
Define_label(mercury__bintree_set__contains_list_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module23)
	init_entry(mercury____Unify___bintree_set__bintree_set_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___bintree_set__bintree_set_1_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury____Unify___bintree__bintree_2_0);
	tailcall(ENTRY(mercury____Unify___bintree__bintree_2_0),
		ENTRY(mercury____Unify___bintree_set__bintree_set_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module24)
	init_entry(mercury____Index___bintree_set__bintree_set_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___bintree_set__bintree_set_1_0);
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury____Index___bintree__bintree_2_0);
	tailcall(ENTRY(mercury____Index___bintree__bintree_2_0),
		ENTRY(mercury____Index___bintree_set__bintree_set_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_set_module25)
	init_entry(mercury____Compare___bintree_set__bintree_set_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___bintree_set__bintree_set_1_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data_std_util__base_type_info_unit_0;
	{
	Declare_entry(mercury____Compare___bintree__bintree_2_0);
	tailcall(ENTRY(mercury____Compare___bintree__bintree_2_0),
		ENTRY(mercury____Compare___bintree_set__bintree_set_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__bintree_set_bunch_0(void)
{
	mercury__bintree_set_module0();
	mercury__bintree_set_module1();
	mercury__bintree_set_module2();
	mercury__bintree_set_module3();
	mercury__bintree_set_module4();
	mercury__bintree_set_module5();
	mercury__bintree_set_module6();
	mercury__bintree_set_module7();
	mercury__bintree_set_module8();
	mercury__bintree_set_module9();
	mercury__bintree_set_module10();
	mercury__bintree_set_module11();
	mercury__bintree_set_module12();
	mercury__bintree_set_module13();
	mercury__bintree_set_module14();
	mercury__bintree_set_module15();
	mercury__bintree_set_module16();
	mercury__bintree_set_module17();
	mercury__bintree_set_module18();
	mercury__bintree_set_module19();
	mercury__bintree_set_module20();
	mercury__bintree_set_module21();
	mercury__bintree_set_module22();
	mercury__bintree_set_module23();
	mercury__bintree_set_module24();
	mercury__bintree_set_module25();
}

#endif

void mercury__bintree_set__init(void); /* suppress gcc warning */
void mercury__bintree_set__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__bintree_set_bunch_0();
#endif
}
