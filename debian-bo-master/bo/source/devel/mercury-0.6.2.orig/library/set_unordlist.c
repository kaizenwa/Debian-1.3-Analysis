/*
** Automatically generated from `set_unordlist.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__set_unordlist__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__set_unordlist__insert__ua10001_3_0);
Declare_static(mercury__set_unordlist__insert__ua10000_3_0);
Declare_static(mercury__set_unordlist__empty__ua0_1_0);
Declare_label(mercury__set_unordlist__empty__ua0_1_0_i1);
Declare_static(mercury__set_unordlist__singleton_set__ua10001_2_0);
Declare_static(mercury__set_unordlist__singleton_set__ua0_2_0);
Declare_label(mercury__set_unordlist__singleton_set__ua0_2_0_i1);
Declare_static(mercury__set_unordlist__init__ua10000_1_0);
Declare_static(mercury__set_unordlist__sorted_list_to_set__ua10000_2_0);
Declare_static(mercury__set_unordlist__list_to_set__ua10000_2_0);
Define_extern_entry(mercury__set_unordlist__list_to_set_2_0);
Define_extern_entry(mercury__set_unordlist__sorted_list_to_set_2_0);
Define_extern_entry(mercury__set_unordlist__to_sorted_list_2_0);
Define_extern_entry(mercury__set_unordlist__init_1_0);
Define_extern_entry(mercury__set_unordlist__singleton_set_2_0);
Declare_label(mercury__set_unordlist__singleton_set_2_0_i2);
Declare_label(mercury__set_unordlist__singleton_set_2_0_i1000);
Define_extern_entry(mercury__set_unordlist__singleton_set_2_1);
Define_extern_entry(mercury__set_unordlist__equal_2_0);
Declare_label(mercury__set_unordlist__equal_2_0_i2);
Declare_label(mercury__set_unordlist__equal_2_0_i1);
Define_extern_entry(mercury__set_unordlist__empty_1_0);
Define_extern_entry(mercury__set_unordlist__subset_2_0);
Declare_label(mercury__set_unordlist__subset_2_0_i4);
Declare_label(mercury__set_unordlist__subset_2_0_i1003);
Declare_label(mercury__set_unordlist__subset_2_0_i1);
Define_extern_entry(mercury__set_unordlist__superset_2_0);
Define_extern_entry(mercury__set_unordlist__member_2_0);
Define_extern_entry(mercury__set_unordlist__member_2_1);
Declare_label(mercury__set_unordlist__member_2_1_i1);
Define_extern_entry(mercury__set_unordlist__is_member_3_0);
Declare_label(mercury__set_unordlist__is_member_3_0_i4);
Declare_label(mercury__set_unordlist__is_member_3_0_i1000);
Define_extern_entry(mercury__set_unordlist__insert_3_0);
Define_extern_entry(mercury__set_unordlist__insert_3_1);
Define_extern_entry(mercury__set_unordlist__insert_list_3_0);
Define_extern_entry(mercury__set_unordlist__delete_3_0);
Define_extern_entry(mercury__set_unordlist__delete_3_1);
Define_extern_entry(mercury__set_unordlist__delete_list_3_0);
Declare_label(mercury__set_unordlist__delete_list_3_0_i4);
Declare_label(mercury__set_unordlist__delete_list_3_0_i1002);
Define_extern_entry(mercury__set_unordlist__remove_3_0);
Declare_label(mercury__set_unordlist__remove_3_0_i2);
Declare_label(mercury__set_unordlist__remove_3_0_i4);
Declare_label(mercury__set_unordlist__remove_3_0_i1);
Define_extern_entry(mercury__set_unordlist__remove_list_3_0);
Declare_label(mercury__set_unordlist__remove_list_3_0_i4);
Declare_label(mercury__set_unordlist__remove_list_3_0_i6);
Declare_label(mercury__set_unordlist__remove_list_3_0_i1003);
Declare_label(mercury__set_unordlist__remove_list_3_0_i1);
Declare_label(mercury__set_unordlist__remove_list_3_0_i1005);
Define_extern_entry(mercury__set_unordlist__remove_least_3_0);
Declare_label(mercury__set_unordlist__remove_least_3_0_i3);
Declare_label(mercury__set_unordlist__remove_least_3_0_i1003);
Declare_label(mercury__set_unordlist__remove_least_3_0_i1005);
Define_extern_entry(mercury__set_unordlist__union_3_0);
Define_extern_entry(mercury__set_unordlist__power_union_2_0);
Declare_label(mercury__set_unordlist__power_union_2_0_i2);
Declare_label(mercury__set_unordlist__power_union_2_0_i3);
Declare_label(mercury__set_unordlist__power_union_2_0_i4);
Define_extern_entry(mercury__set_unordlist__intersect_3_0);
Define_extern_entry(mercury__set_unordlist__power_intersect_2_0);
Declare_label(mercury__set_unordlist__power_intersect_2_0_i1005);
Declare_label(mercury__set_unordlist__power_intersect_2_0_i7);
Declare_label(mercury__set_unordlist__power_intersect_2_0_i1003);
Define_extern_entry(mercury__set_unordlist__difference_3_0);
Declare_static(mercury__set_unordlist__power_union_2_3_0);
Declare_label(mercury__set_unordlist__power_union_2_3_0_i4);
Declare_label(mercury__set_unordlist__power_union_2_3_0_i1002);
Declare_static(mercury__set_unordlist__intersect_2_4_0);
Declare_label(mercury__set_unordlist__intersect_2_4_0_i6);
Declare_label(mercury__set_unordlist__intersect_2_4_0_i5);
Declare_label(mercury__set_unordlist__intersect_2_4_0_i1003);
Declare_static(mercury__set_unordlist__difference_2_3_0);
Declare_label(mercury__set_unordlist__difference_2_3_0_i4);
Declare_label(mercury__set_unordlist__difference_2_3_0_i1002);
Define_extern_entry(mercury____Unify___set_unordlist__set_unordlist_1_0);
Define_extern_entry(mercury____Index___set_unordlist__set_unordlist_1_0);
Define_extern_entry(mercury____Compare___set_unordlist__set_unordlist_1_0);

extern Word * mercury_data_set_unordlist__base_type_layout_set_unordlist_1[];
Word * mercury_data_set_unordlist__base_type_info_set_unordlist_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___set_unordlist__set_unordlist_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___set_unordlist__set_unordlist_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___set_unordlist__set_unordlist_1_0),
	(Word *) (Integer) mercury_data_set_unordlist__base_type_layout_set_unordlist_1
};

extern Word * mercury_data_set_unordlist__common_1[];
Word * mercury_data_set_unordlist__base_type_layout_set_unordlist_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set_unordlist__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set_unordlist__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set_unordlist__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set_unordlist__common_1)
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_set_unordlist__common_0[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) ((Integer) 1)
};

Word * mercury_data_set_unordlist__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_set_unordlist__common_0)
};

BEGIN_MODULE(mercury__set_unordlist_module0)
	init_entry(mercury__set_unordlist__insert__ua10001_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__insert__ua10001'/3 in mode 0 */
Define_static(mercury__set_unordlist__insert__ua10001_3_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module1)
	init_entry(mercury__set_unordlist__insert__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__insert__ua10000'/3 in mode 0 */
Define_static(mercury__set_unordlist__insert__ua10000_3_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module2)
	init_entry(mercury__set_unordlist__empty__ua0_1_0);
	init_label(mercury__set_unordlist__empty__ua0_1_0_i1);
BEGIN_CODE

/* code for predicate 'set_unordlist__empty__ua0'/1 in mode 0 */
Define_static(mercury__set_unordlist__empty__ua0_1_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__empty__ua0_1_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__set_unordlist__empty__ua0_1_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module3)
	init_entry(mercury__set_unordlist__singleton_set__ua10001_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__singleton_set__ua10001'/2 in mode 0 */
Define_static(mercury__set_unordlist__singleton_set__ua10001_2_0);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module4)
	init_entry(mercury__set_unordlist__singleton_set__ua0_2_0);
	init_label(mercury__set_unordlist__singleton_set__ua0_2_0_i1);
BEGIN_CODE

/* code for predicate 'set_unordlist__singleton_set__ua0'/2 in mode 0 */
Define_static(mercury__set_unordlist__singleton_set__ua0_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__singleton_set__ua0_2_0_i1);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__singleton_set__ua0_2_0_i1);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	proceed();
Define_label(mercury__set_unordlist__singleton_set__ua0_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module5)
	init_entry(mercury__set_unordlist__init__ua10000_1_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__init__ua10000'/1 in mode 0 */
Define_static(mercury__set_unordlist__init__ua10000_1_0);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module6)
	init_entry(mercury__set_unordlist__sorted_list_to_set__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__sorted_list_to_set__ua10000'/2 in mode 0 */
Define_static(mercury__set_unordlist__sorted_list_to_set__ua10000_2_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module7)
	init_entry(mercury__set_unordlist__list_to_set__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__list_to_set__ua10000'/2 in mode 0 */
Define_static(mercury__set_unordlist__list_to_set__ua10000_2_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module8)
	init_entry(mercury__set_unordlist__list_to_set_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__list_to_set'/2 in mode 0 */
Define_entry(mercury__set_unordlist__list_to_set_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__set_unordlist__list_to_set__ua10000_2_0),
		ENTRY(mercury__set_unordlist__list_to_set_2_0));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module9)
	init_entry(mercury__set_unordlist__sorted_list_to_set_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__sorted_list_to_set'/2 in mode 0 */
Define_entry(mercury__set_unordlist__sorted_list_to_set_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__set_unordlist__sorted_list_to_set__ua10000_2_0),
		ENTRY(mercury__set_unordlist__sorted_list_to_set_2_0));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module10)
	init_entry(mercury__set_unordlist__to_sorted_list_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__to_sorted_list'/2 in mode 0 */
Define_entry(mercury__set_unordlist__to_sorted_list_2_0);
	{
	Declare_entry(mercury__list__sort_and_remove_dups_2_0);
	tailcall(ENTRY(mercury__list__sort_and_remove_dups_2_0),
		ENTRY(mercury__set_unordlist__to_sorted_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module11)
	init_entry(mercury__set_unordlist__init_1_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__init'/1 in mode 0 */
Define_entry(mercury__set_unordlist__init_1_0);
	tailcall(STATIC(mercury__set_unordlist__init__ua10000_1_0),
		ENTRY(mercury__set_unordlist__init_1_0));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module12)
	init_entry(mercury__set_unordlist__singleton_set_2_0);
	init_label(mercury__set_unordlist__singleton_set_2_0_i2);
	init_label(mercury__set_unordlist__singleton_set_2_0_i1000);
BEGIN_CODE

/* code for predicate 'set_unordlist__singleton_set'/2 in mode 0 */
Define_entry(mercury__set_unordlist__singleton_set_2_0);
	incr_sp_push_msg(2, "set_unordlist__singleton_set");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__set_unordlist__singleton_set__ua0_2_0),
		mercury__set_unordlist__singleton_set_2_0_i2,
		ENTRY(mercury__set_unordlist__singleton_set_2_0));
Define_label(mercury__set_unordlist__singleton_set_2_0_i2);
	update_prof_current_proc(LABEL(mercury__set_unordlist__singleton_set_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__singleton_set_2_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__set_unordlist__singleton_set_2_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module13)
	init_entry(mercury__set_unordlist__singleton_set_2_1);
BEGIN_CODE

/* code for predicate 'set_unordlist__singleton_set'/2 in mode 1 */
Define_entry(mercury__set_unordlist__singleton_set_2_1);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__set_unordlist__singleton_set__ua10001_2_0),
		ENTRY(mercury__set_unordlist__singleton_set_2_1));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module14)
	init_entry(mercury__set_unordlist__equal_2_0);
	init_label(mercury__set_unordlist__equal_2_0_i2);
	init_label(mercury__set_unordlist__equal_2_0_i1);
BEGIN_CODE

/* code for predicate 'set_unordlist__equal'/2 in mode 0 */
Define_entry(mercury__set_unordlist__equal_2_0);
	incr_sp_push_msg(4, "set_unordlist__equal");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	{
		call_localret(STATIC(mercury__set_unordlist__subset_2_0),
		mercury__set_unordlist__equal_2_0_i2,
		ENTRY(mercury__set_unordlist__equal_2_0));
	}
Define_label(mercury__set_unordlist__equal_2_0_i2);
	update_prof_current_proc(LABEL(mercury__set_unordlist__equal_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__equal_2_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__set_unordlist__subset_2_0),
		ENTRY(mercury__set_unordlist__equal_2_0));
	}
Define_label(mercury__set_unordlist__equal_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module15)
	init_entry(mercury__set_unordlist__empty_1_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__empty'/1 in mode 0 */
Define_entry(mercury__set_unordlist__empty_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__set_unordlist__empty__ua0_1_0),
		ENTRY(mercury__set_unordlist__empty_1_0));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module16)
	init_entry(mercury__set_unordlist__subset_2_0);
	init_label(mercury__set_unordlist__subset_2_0_i4);
	init_label(mercury__set_unordlist__subset_2_0_i1003);
	init_label(mercury__set_unordlist__subset_2_0_i1);
BEGIN_CODE

/* code for predicate 'set_unordlist__subset'/2 in mode 0 */
Define_entry(mercury__set_unordlist__subset_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__subset_2_0_i1003);
	incr_sp_push_msg(4, "set_unordlist__subset");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	{
		call_localret(STATIC(mercury__set_unordlist__member_2_0),
		mercury__set_unordlist__subset_2_0_i4,
		ENTRY(mercury__set_unordlist__subset_2_0));
	}
Define_label(mercury__set_unordlist__subset_2_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__subset_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__subset_2_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__set_unordlist__subset_2_0,
		ENTRY(mercury__set_unordlist__subset_2_0));
Define_label(mercury__set_unordlist__subset_2_0_i1003);
	r1 = TRUE;
	proceed();
Define_label(mercury__set_unordlist__subset_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module17)
	init_entry(mercury__set_unordlist__superset_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__superset'/2 in mode 0 */
Define_entry(mercury__set_unordlist__superset_2_0);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	{
		tailcall(STATIC(mercury__set_unordlist__subset_2_0),
		ENTRY(mercury__set_unordlist__superset_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module18)
	init_entry(mercury__set_unordlist__member_2_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__member'/2 in mode 0 */
Define_entry(mercury__set_unordlist__member_2_0);
	{
	Declare_entry(mercury__list__member_2_0);
	tailcall(ENTRY(mercury__list__member_2_0),
		ENTRY(mercury__set_unordlist__member_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module19)
	init_entry(mercury__set_unordlist__member_2_1);
	init_label(mercury__set_unordlist__member_2_1_i1);
BEGIN_CODE

/* code for predicate 'set_unordlist__member'/2 in mode 1 */
Define_entry(mercury__set_unordlist__member_2_1);
	{
	Declare_entry(do_fail);
	mkframe("set_unordlist__member/2", 1, ENTRY(do_fail));
	}
	{
	Declare_entry(mercury__list__member_2_1);
	call_localret(ENTRY(mercury__list__member_2_1),
		mercury__set_unordlist__member_2_1_i1,
		ENTRY(mercury__set_unordlist__member_2_1));
	}
Define_label(mercury__set_unordlist__member_2_1_i1);
	update_prof_current_proc(LABEL(mercury__set_unordlist__member_2_1));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module20)
	init_entry(mercury__set_unordlist__is_member_3_0);
	init_label(mercury__set_unordlist__is_member_3_0_i4);
	init_label(mercury__set_unordlist__is_member_3_0_i1000);
BEGIN_CODE

/* code for predicate 'set_unordlist__is_member'/3 in mode 0 */
Define_entry(mercury__set_unordlist__is_member_3_0);
	incr_sp_push_msg(1, "set_unordlist__is_member");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__set_unordlist__member_2_0),
		mercury__set_unordlist__is_member_3_0_i4,
		ENTRY(mercury__set_unordlist__is_member_3_0));
	}
Define_label(mercury__set_unordlist__is_member_3_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__is_member_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__is_member_3_0_i1000);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__set_unordlist__is_member_3_0_i1000);
	r1 = ((Integer) 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module21)
	init_entry(mercury__set_unordlist__insert_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__insert'/3 in mode 0 */
Define_entry(mercury__set_unordlist__insert_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__set_unordlist__insert__ua10000_3_0),
		ENTRY(mercury__set_unordlist__insert_3_0));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module22)
	init_entry(mercury__set_unordlist__insert_3_1);
BEGIN_CODE

/* code for predicate 'set_unordlist__insert'/3 in mode 1 */
Define_entry(mercury__set_unordlist__insert_3_1);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__set_unordlist__insert__ua10001_3_0),
		ENTRY(mercury__set_unordlist__insert_3_1));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module23)
	init_entry(mercury__set_unordlist__insert_list_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__insert_list'/3 in mode 0 */
Define_entry(mercury__set_unordlist__insert_list_3_0);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		ENTRY(mercury__set_unordlist__insert_list_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module24)
	init_entry(mercury__set_unordlist__delete_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__delete'/3 in mode 0 */
Define_entry(mercury__set_unordlist__delete_3_0);
	{
	Declare_entry(mercury__list__delete_all_3_0);
	tailcall(ENTRY(mercury__list__delete_all_3_0),
		ENTRY(mercury__set_unordlist__delete_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module25)
	init_entry(mercury__set_unordlist__delete_3_1);
BEGIN_CODE

/* code for predicate 'set_unordlist__delete'/3 in mode 1 */
Define_entry(mercury__set_unordlist__delete_3_1);
	{
	Declare_entry(mercury__list__delete_all_3_1);
	tailcall(ENTRY(mercury__list__delete_all_3_1),
		ENTRY(mercury__set_unordlist__delete_3_1));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module26)
	init_entry(mercury__set_unordlist__delete_list_3_0);
	init_label(mercury__set_unordlist__delete_list_3_0_i4);
	init_label(mercury__set_unordlist__delete_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'set_unordlist__delete_list'/3 in mode 0 */
Define_entry(mercury__set_unordlist__delete_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__delete_list_3_0_i1002);
	incr_sp_push_msg(3, "set_unordlist__delete_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__set_unordlist__delete_3_1),
		mercury__set_unordlist__delete_list_3_0_i4,
		ENTRY(mercury__set_unordlist__delete_list_3_0));
	}
Define_label(mercury__set_unordlist__delete_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__delete_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__set_unordlist__delete_list_3_0,
		ENTRY(mercury__set_unordlist__delete_list_3_0));
Define_label(mercury__set_unordlist__delete_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module27)
	init_entry(mercury__set_unordlist__remove_3_0);
	init_label(mercury__set_unordlist__remove_3_0_i2);
	init_label(mercury__set_unordlist__remove_3_0_i4);
	init_label(mercury__set_unordlist__remove_3_0_i1);
BEGIN_CODE

/* code for predicate 'set_unordlist__remove'/3 in mode 0 */
Define_entry(mercury__set_unordlist__remove_3_0);
	incr_sp_push_msg(4, "set_unordlist__remove");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	r2 = (Integer) r3;
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__set_unordlist__remove_3_0_i2,
		ENTRY(mercury__set_unordlist__remove_3_0));
	}
Define_label(mercury__set_unordlist__remove_3_0_i2);
	update_prof_current_proc(LABEL(mercury__set_unordlist__remove_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__remove_3_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__set_unordlist__delete_3_1),
		mercury__set_unordlist__remove_3_0_i4,
		ENTRY(mercury__set_unordlist__remove_3_0));
	}
Define_label(mercury__set_unordlist__remove_3_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__remove_3_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__set_unordlist__remove_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module28)
	init_entry(mercury__set_unordlist__remove_list_3_0);
	init_label(mercury__set_unordlist__remove_list_3_0_i4);
	init_label(mercury__set_unordlist__remove_list_3_0_i6);
	init_label(mercury__set_unordlist__remove_list_3_0_i1003);
	init_label(mercury__set_unordlist__remove_list_3_0_i1);
	init_label(mercury__set_unordlist__remove_list_3_0_i1005);
BEGIN_CODE

/* code for predicate 'set_unordlist__remove_list'/3 in mode 0 */
Define_entry(mercury__set_unordlist__remove_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__remove_list_3_0_i1003);
	incr_sp_push_msg(3, "set_unordlist__remove_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__set_unordlist__remove_3_0),
		mercury__set_unordlist__remove_list_3_0_i4,
		ENTRY(mercury__set_unordlist__remove_list_3_0));
	}
Define_label(mercury__set_unordlist__remove_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__remove_list_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__remove_list_3_0_i1);
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__set_unordlist__remove_list_3_0,
		LABEL(mercury__set_unordlist__remove_list_3_0_i6),
		ENTRY(mercury__set_unordlist__remove_list_3_0));
Define_label(mercury__set_unordlist__remove_list_3_0_i6);
	update_prof_current_proc(LABEL(mercury__set_unordlist__remove_list_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__remove_list_3_0_i1005);
	r1 = TRUE;
	proceed();
Define_label(mercury__set_unordlist__remove_list_3_0_i1003);
	r1 = TRUE;
	proceed();
Define_label(mercury__set_unordlist__remove_list_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__set_unordlist__remove_list_3_0_i1005);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module29)
	init_entry(mercury__set_unordlist__remove_least_3_0);
	init_label(mercury__set_unordlist__remove_least_3_0_i3);
	init_label(mercury__set_unordlist__remove_least_3_0_i1003);
	init_label(mercury__set_unordlist__remove_least_3_0_i1005);
BEGIN_CODE

/* code for predicate 'set_unordlist__remove_least'/3 in mode 0 */
Define_entry(mercury__set_unordlist__remove_least_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__remove_least_3_0_i1003);
	incr_sp_push_msg(1, "set_unordlist__remove_least");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__set_unordlist__to_sorted_list_2_0),
		mercury__set_unordlist__remove_least_3_0_i3,
		ENTRY(mercury__set_unordlist__remove_least_3_0));
	}
Define_label(mercury__set_unordlist__remove_least_3_0_i3);
	update_prof_current_proc(LABEL(mercury__set_unordlist__remove_least_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__remove_least_3_0_i1005);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__set_unordlist__remove_least_3_0_i1003);
	r1 = FALSE;
	proceed();
Define_label(mercury__set_unordlist__remove_least_3_0_i1005);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module30)
	init_entry(mercury__set_unordlist__union_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__union'/3 in mode 0 */
Define_entry(mercury__set_unordlist__union_3_0);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		ENTRY(mercury__set_unordlist__union_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module31)
	init_entry(mercury__set_unordlist__power_union_2_0);
	init_label(mercury__set_unordlist__power_union_2_0_i2);
	init_label(mercury__set_unordlist__power_union_2_0_i3);
	init_label(mercury__set_unordlist__power_union_2_0_i4);
BEGIN_CODE

/* code for predicate 'set_unordlist__power_union'/2 in mode 0 */
Define_entry(mercury__set_unordlist__power_union_2_0);
	incr_sp_push_msg(3, "set_unordlist__power_union");
	detstackvar(3) = (Integer) succip;
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	{
		call_localret(STATIC(mercury__set_unordlist__to_sorted_list_2_0),
		mercury__set_unordlist__power_union_2_0_i2,
		ENTRY(mercury__set_unordlist__power_union_2_0));
	}
Define_label(mercury__set_unordlist__power_union_2_0_i2);
	update_prof_current_proc(LABEL(mercury__set_unordlist__power_union_2_0));
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__set_unordlist__init__ua10000_1_0),
		mercury__set_unordlist__power_union_2_0_i3,
		ENTRY(mercury__set_unordlist__power_union_2_0));
Define_label(mercury__set_unordlist__power_union_2_0_i3);
	update_prof_current_proc(LABEL(mercury__set_unordlist__power_union_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__set_unordlist__power_union_2_3_0),
		mercury__set_unordlist__power_union_2_0_i4,
		ENTRY(mercury__set_unordlist__power_union_2_0));
Define_label(mercury__set_unordlist__power_union_2_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__power_union_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__list__sort_and_remove_dups_2_0);
	tailcall(ENTRY(mercury__list__sort_and_remove_dups_2_0),
		ENTRY(mercury__set_unordlist__power_union_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module32)
	init_entry(mercury__set_unordlist__intersect_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__intersect'/3 in mode 0 */
Define_entry(mercury__set_unordlist__intersect_3_0);
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__set_unordlist__intersect_2_4_0),
		ENTRY(mercury__set_unordlist__intersect_3_0));
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module33)
	init_entry(mercury__set_unordlist__power_intersect_2_0);
	init_label(mercury__set_unordlist__power_intersect_2_0_i1005);
	init_label(mercury__set_unordlist__power_intersect_2_0_i7);
	init_label(mercury__set_unordlist__power_intersect_2_0_i1003);
BEGIN_CODE

/* code for predicate 'set_unordlist__power_intersect'/2 in mode 0 */
Define_entry(mercury__set_unordlist__power_intersect_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__power_intersect_2_0_i1003);
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__power_intersect_2_0_i1005);
	r1 = (Integer) r4;
	proceed();
Define_label(mercury__set_unordlist__power_intersect_2_0_i1005);
	incr_sp_push_msg(3, "set_unordlist__power_intersect");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r1;
	r2 = (Integer) r3;
	localcall(mercury__set_unordlist__power_intersect_2_0,
		LABEL(mercury__set_unordlist__power_intersect_2_0_i7),
		ENTRY(mercury__set_unordlist__power_intersect_2_0));
Define_label(mercury__set_unordlist__power_intersect_2_0_i7);
	update_prof_current_proc(LABEL(mercury__set_unordlist__power_intersect_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__set_unordlist__intersect_3_0),
		ENTRY(mercury__set_unordlist__power_intersect_2_0));
	}
Define_label(mercury__set_unordlist__power_intersect_2_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module34)
	init_entry(mercury__set_unordlist__difference_3_0);
BEGIN_CODE

/* code for predicate 'set_unordlist__difference'/3 in mode 0 */
Define_entry(mercury__set_unordlist__difference_3_0);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	tailcall(STATIC(mercury__set_unordlist__difference_2_3_0),
		ENTRY(mercury__set_unordlist__difference_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module35)
	init_entry(mercury__set_unordlist__power_union_2_3_0);
	init_label(mercury__set_unordlist__power_union_2_3_0_i4);
	init_label(mercury__set_unordlist__power_union_2_3_0_i1002);
BEGIN_CODE

/* code for predicate 'set_unordlist__power_union_2'/3 in mode 0 */
Define_static(mercury__set_unordlist__power_union_2_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__power_union_2_3_0_i1002);
	incr_sp_push_msg(3, "set_unordlist__power_union_2");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	{
		call_localret(STATIC(mercury__set_unordlist__union_3_0),
		mercury__set_unordlist__power_union_2_3_0_i4,
		STATIC(mercury__set_unordlist__power_union_2_3_0));
	}
	}
Define_label(mercury__set_unordlist__power_union_2_3_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__power_union_2_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__set_unordlist__power_union_2_3_0,
		STATIC(mercury__set_unordlist__power_union_2_3_0));
Define_label(mercury__set_unordlist__power_union_2_3_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module36)
	init_entry(mercury__set_unordlist__intersect_2_4_0);
	init_label(mercury__set_unordlist__intersect_2_4_0_i6);
	init_label(mercury__set_unordlist__intersect_2_4_0_i5);
	init_label(mercury__set_unordlist__intersect_2_4_0_i1003);
BEGIN_CODE

/* code for predicate 'set_unordlist__intersect_2'/4 in mode 0 */
Define_static(mercury__set_unordlist__intersect_2_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__intersect_2_4_0_i1003);
	incr_sp_push_msg(6, "set_unordlist__intersect_2");
	detstackvar(6) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__set_unordlist__intersect_2_4_0_i6,
		STATIC(mercury__set_unordlist__intersect_2_4_0));
	}
Define_label(mercury__set_unordlist__intersect_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__set_unordlist__intersect_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set_unordlist__intersect_2_4_0_i5);
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__set_unordlist__intersect_2_4_0,
		STATIC(mercury__set_unordlist__intersect_2_4_0));
Define_label(mercury__set_unordlist__intersect_2_4_0_i5);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__set_unordlist__intersect_2_4_0,
		STATIC(mercury__set_unordlist__intersect_2_4_0));
Define_label(mercury__set_unordlist__intersect_2_4_0_i1003);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module37)
	init_entry(mercury__set_unordlist__difference_2_3_0);
	init_label(mercury__set_unordlist__difference_2_3_0_i4);
	init_label(mercury__set_unordlist__difference_2_3_0_i1002);
BEGIN_CODE

/* code for predicate 'set_unordlist__difference_2'/3 in mode 0 */
Define_static(mercury__set_unordlist__difference_2_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__set_unordlist__difference_2_3_0_i1002);
	incr_sp_push_msg(3, "set_unordlist__difference_2");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	{
		call_localret(STATIC(mercury__set_unordlist__delete_3_1),
		mercury__set_unordlist__difference_2_3_0_i4,
		STATIC(mercury__set_unordlist__difference_2_3_0));
	}
	}
Define_label(mercury__set_unordlist__difference_2_3_0_i4);
	update_prof_current_proc(LABEL(mercury__set_unordlist__difference_2_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__set_unordlist__difference_2_3_0,
		STATIC(mercury__set_unordlist__difference_2_3_0));
Define_label(mercury__set_unordlist__difference_2_3_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module38)
	init_entry(mercury____Unify___set_unordlist__set_unordlist_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___set_unordlist__set_unordlist_1_0);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury____Unify___set_unordlist__set_unordlist_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module39)
	init_entry(mercury____Index___set_unordlist__set_unordlist_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___set_unordlist__set_unordlist_1_0);
	{
	Declare_entry(mercury____Index___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Index___mercury_builtin__list_1_0),
		ENTRY(mercury____Index___set_unordlist__set_unordlist_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_unordlist_module40)
	init_entry(mercury____Compare___set_unordlist__set_unordlist_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___set_unordlist__set_unordlist_1_0);
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		ENTRY(mercury____Compare___set_unordlist__set_unordlist_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__set_unordlist_bunch_0(void)
{
	mercury__set_unordlist_module0();
	mercury__set_unordlist_module1();
	mercury__set_unordlist_module2();
	mercury__set_unordlist_module3();
	mercury__set_unordlist_module4();
	mercury__set_unordlist_module5();
	mercury__set_unordlist_module6();
	mercury__set_unordlist_module7();
	mercury__set_unordlist_module8();
	mercury__set_unordlist_module9();
	mercury__set_unordlist_module10();
	mercury__set_unordlist_module11();
	mercury__set_unordlist_module12();
	mercury__set_unordlist_module13();
	mercury__set_unordlist_module14();
	mercury__set_unordlist_module15();
	mercury__set_unordlist_module16();
	mercury__set_unordlist_module17();
	mercury__set_unordlist_module18();
	mercury__set_unordlist_module19();
	mercury__set_unordlist_module20();
	mercury__set_unordlist_module21();
	mercury__set_unordlist_module22();
	mercury__set_unordlist_module23();
	mercury__set_unordlist_module24();
	mercury__set_unordlist_module25();
	mercury__set_unordlist_module26();
	mercury__set_unordlist_module27();
	mercury__set_unordlist_module28();
	mercury__set_unordlist_module29();
	mercury__set_unordlist_module30();
	mercury__set_unordlist_module31();
	mercury__set_unordlist_module32();
	mercury__set_unordlist_module33();
	mercury__set_unordlist_module34();
	mercury__set_unordlist_module35();
	mercury__set_unordlist_module36();
	mercury__set_unordlist_module37();
	mercury__set_unordlist_module38();
	mercury__set_unordlist_module39();
	mercury__set_unordlist_module40();
}

#endif

void mercury__set_unordlist__init(void); /* suppress gcc warning */
void mercury__set_unordlist__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__set_unordlist_bunch_0();
#endif
}
