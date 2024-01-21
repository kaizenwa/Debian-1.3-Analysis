/*
** Automatically generated from `bintree.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__bintree__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___bintree_bintree_2__ua10000_2_0);
Declare_label(mercury____Index___bintree_bintree_2__ua10000_2_0_i3);
Declare_static(mercury__bintree__values_2__ua10000_3_0);
Declare_label(mercury__bintree__values_2__ua10000_3_0_i4);
Declare_label(mercury__bintree__values_2__ua10000_3_0_i1002);
Declare_static(mercury__bintree__keys_2__ua10000_3_0);
Declare_label(mercury__bintree__keys_2__ua10000_3_0_i4);
Declare_label(mercury__bintree__keys_2__ua10000_3_0_i1002);
Declare_static(mercury__bintree__to_list_2__ua10000_3_0);
Declare_label(mercury__bintree__to_list_2__ua10000_3_0_i4);
Declare_label(mercury__bintree__to_list_2__ua10000_3_0_i1003);
Declare_static(mercury__bintree__from_corresponding_lists_2__ua0_4_0);
Declare_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1010);
Declare_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i6);
Declare_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i8);
Declare_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1007);
Declare_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1);
Declare_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1009);
Declare_static(mercury__bintree__from_sorted_list_2__ua10000_4_0);
Declare_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i1001);
Declare_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i5);
Declare_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i9);
Declare_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i6);
Declare_static(mercury__bintree__from_list_2__ua10000_3_0);
Declare_label(mercury__bintree__from_list_2__ua10000_3_0_i6);
Declare_label(mercury__bintree__from_list_2__ua10000_3_0_i5);
Declare_label(mercury__bintree__from_list_2__ua10000_3_0_i8);
Declare_label(mercury__bintree__from_list_2__ua10000_3_0_i1003);
Declare_static(mercury__bintree__knock_right__ua10000_4_0);
Declare_label(mercury__bintree__knock_right__ua10000_4_0_i1005);
Declare_label(mercury__bintree__knock_right__ua10000_4_0_i7);
Declare_label(mercury__bintree__knock_right__ua10000_4_0_i1004);
Declare_static(mercury__bintree__knock_left__ua10000_4_0);
Declare_label(mercury__bintree__knock_left__ua10000_4_0_i1005);
Declare_label(mercury__bintree__knock_left__ua10000_4_0_i7);
Declare_label(mercury__bintree__knock_left__ua10000_4_0_i1004);
Declare_static(mercury__bintree__left_depth__ua10000_2_0);
Declare_label(mercury__bintree__left_depth__ua10000_2_0_i3);
Declare_label(mercury__bintree__left_depth__ua10000_2_0_i4);
Declare_label(mercury__bintree__left_depth__ua10000_2_0_i1);
Declare_static(mercury__bintree__right_depth__ua10000_2_0);
Declare_label(mercury__bintree__right_depth__ua10000_2_0_i3);
Declare_label(mercury__bintree__right_depth__ua10000_2_0_i4);
Declare_label(mercury__bintree__right_depth__ua10000_2_0_i1);
Declare_static(mercury__bintree__fixup__ua10000_3_0);
Declare_label(mercury__bintree__fixup__ua10000_3_0_i1001);
Declare_label(mercury__bintree__fixup__ua10000_3_0_i8);
Declare_label(mercury__bintree__fixup__ua10000_3_0_i9);
Declare_label(mercury__bintree__fixup__ua10000_3_0_i12);
Declare_label(mercury__bintree__fixup__ua10000_3_0_i10);
Declare_label(mercury__bintree__fixup__ua10000_3_0_i13);
Declare_label(mercury__bintree__fixup__ua10000_3_0_i16);
Declare_static(mercury__bintree__branching_factor__ua10000_3_0);
Declare_label(mercury__bintree__branching_factor__ua10000_3_0_i1003);
Declare_label(mercury__bintree__branching_factor__ua10000_3_0_i10);
Declare_label(mercury__bintree__branching_factor__ua10000_3_0_i1002);
Declare_label(mercury__bintree__branching_factor__ua10000_3_0_i12);
Declare_label(mercury__bintree__branching_factor__ua10000_3_0_i16);
Declare_label(mercury__bintree__branching_factor__ua10000_3_0_i17);
Declare_label(mercury__bintree__branching_factor__ua10000_3_0_i1001);
Declare_static(mercury__bintree__depth__ua10000_2_0);
Declare_label(mercury__bintree__depth__ua10000_2_0_i4);
Declare_label(mercury__bintree__depth__ua10000_2_0_i5);
Declare_label(mercury__bintree__depth__ua10000_2_0_i6);
Declare_label(mercury__bintree__depth__ua10000_2_0_i1002);
Declare_static(mercury__bintree__count__ua10000_2_0);
Declare_label(mercury__bintree__count__ua10000_2_0_i4);
Declare_label(mercury__bintree__count__ua10000_2_0_i5);
Declare_label(mercury__bintree__count__ua10000_2_0_i1002);
Declare_static(mercury__bintree__to_list__ua10000_2_0);
Declare_static(mercury__bintree__from_corresponding_lists__ua10000_3_0);
Declare_label(mercury__bintree__from_corresponding_lists__ua10000_3_0_i4);
Declare_label(mercury__bintree__from_corresponding_lists__ua10000_3_0_i1000);
Declare_static(mercury__bintree__from_list__ua10000_2_0);
Declare_static(mercury__bintree__values__ua10000_2_0);
Declare_static(mercury__bintree__keys__ua10000_2_0);
Declare_static(mercury__bintree__remove__ua0_4_0);
Declare_label(mercury__bintree__remove__ua0_4_0_i3);
Declare_label(mercury__bintree__remove__ua0_4_0_i7);
Declare_label(mercury__bintree__remove__ua0_4_0_i4);
Declare_label(mercury__bintree__remove__ua0_4_0_i11);
Declare_label(mercury__bintree__remove__ua0_4_0_i8);
Declare_label(mercury__bintree__remove__ua0_4_0_i13);
Declare_label(mercury__bintree__remove__ua0_4_0_i1007);
Declare_label(mercury__bintree__remove__ua0_4_0_i1);
Declare_static(mercury__bintree__delete__ua10000_3_0);
Declare_label(mercury__bintree__delete__ua10000_3_0_i4);
Declare_label(mercury__bintree__delete__ua10000_3_0_i5);
Declare_label(mercury__bintree__delete__ua10000_3_0_i12);
Declare_label(mercury__bintree__delete__ua10000_3_0_i9);
Declare_label(mercury__bintree__delete__ua10000_3_0_i13);
Declare_label(mercury__bintree__delete__ua10000_3_0_i1005);
Declare_static(mercury__bintree__search__ua1_3_0);
Declare_label(mercury__bintree__search__ua1_3_0_i3);
Declare_label(mercury__bintree__search__ua1_3_0_i4);
Declare_label(mercury__bintree__search__ua1_3_0_i10);
Declare_label(mercury__bintree__search__ua1_3_0_i7);
Declare_label(mercury__bintree__search__ua1_3_0_i12);
Declare_label(mercury__bintree__search__ua1_3_0_i1005);
Declare_label(mercury__bintree__search__ua1_3_0_i1);
Declare_label(mercury__bintree__search__ua1_3_0_i1006);
Declare_static(mercury__bintree__set__ua10001_4_0);
Declare_label(mercury__bintree__set__ua10001_4_0_i4);
Declare_label(mercury__bintree__set__ua10001_4_0_i5);
Declare_label(mercury__bintree__set__ua10001_4_0_i11);
Declare_label(mercury__bintree__set__ua10001_4_0_i8);
Declare_label(mercury__bintree__set__ua10001_4_0_i12);
Declare_label(mercury__bintree__set__ua10001_4_0_i1011);
Declare_static(mercury__bintree__set__ua10000_4_0);
Declare_label(mercury__bintree__set__ua10000_4_0_i4);
Declare_label(mercury__bintree__set__ua10000_4_0_i5);
Declare_label(mercury__bintree__set__ua10000_4_0_i11);
Declare_label(mercury__bintree__set__ua10000_4_0_i8);
Declare_label(mercury__bintree__set__ua10000_4_0_i12);
Declare_label(mercury__bintree__set__ua10000_4_0_i1011);
Declare_static(mercury__bintree__update__ua0_4_0);
Declare_label(mercury__bintree__update__ua0_4_0_i1001);
Declare_label(mercury__bintree__update__ua0_4_0_i4);
Declare_label(mercury__bintree__update__ua0_4_0_i5);
Declare_label(mercury__bintree__update__ua0_4_0_i11);
Declare_label(mercury__bintree__update__ua0_4_0_i8);
Declare_label(mercury__bintree__update__ua0_4_0_i13);
Declare_label(mercury__bintree__update__ua0_4_0_i1);
Declare_static(mercury__bintree__insert__ua0_4_0);
Declare_label(mercury__bintree__insert__ua0_4_0_i3);
Declare_label(mercury__bintree__insert__ua0_4_0_i4);
Declare_label(mercury__bintree__insert__ua0_4_0_i11);
Declare_label(mercury__bintree__insert__ua0_4_0_i8);
Declare_label(mercury__bintree__insert__ua0_4_0_i13);
Declare_label(mercury__bintree__insert__ua0_4_0_i1);
Declare_static(mercury__bintree__init__ua10000_1_0);
Define_extern_entry(mercury__bintree__init_1_0);
Define_extern_entry(mercury__bintree__insert_4_0);
Declare_label(mercury__bintree__insert_4_0_i2);
Declare_label(mercury__bintree__insert_4_0_i1000);
Define_extern_entry(mercury__bintree__update_4_0);
Declare_label(mercury__bintree__update_4_0_i2);
Declare_label(mercury__bintree__update_4_0_i1000);
Define_extern_entry(mercury__bintree__set_4_0);
Define_extern_entry(mercury__bintree__set_4_1);
Define_extern_entry(mercury__bintree__search_3_0);
Declare_label(mercury__bintree__search_3_0_i3);
Declare_label(mercury__bintree__search_3_0_i4);
Declare_label(mercury__bintree__search_3_0_i9);
Declare_label(mercury__bintree__search_3_0_i1004);
Define_extern_entry(mercury__bintree__search_3_1);
Declare_label(mercury__bintree__search_3_1_i2);
Declare_label(mercury__bintree__search_3_1_i1000);
Define_extern_entry(mercury__bintree__delete_3_0);
Define_extern_entry(mercury__bintree__remove_4_0);
Declare_label(mercury__bintree__remove_4_0_i2);
Declare_label(mercury__bintree__remove_4_0_i1000);
Define_extern_entry(mercury__bintree__keys_2_0);
Define_extern_entry(mercury__bintree__values_2_0);
Define_extern_entry(mercury__bintree__from_list_2_0);
Define_extern_entry(mercury__bintree__from_sorted_list_2_0);
Declare_label(mercury__bintree__from_sorted_list_2_0_i2);
Define_extern_entry(mercury__bintree__from_corresponding_lists_3_0);
Define_extern_entry(mercury__bintree__to_list_2_0);
Define_extern_entry(mercury__bintree__count_2_0);
Define_extern_entry(mercury__bintree__depth_2_0);
Define_extern_entry(mercury__bintree__branching_factor_3_0);
Define_extern_entry(mercury__bintree__balance_2_0);
Declare_label(mercury__bintree__balance_2_0_i2);
Define_extern_entry(mercury____Unify___bintree__bintree_2_0);
Declare_label(mercury____Unify___bintree__bintree_2_0_i1011);
Declare_label(mercury____Unify___bintree__bintree_2_0_i6);
Declare_label(mercury____Unify___bintree__bintree_2_0_i8);
Declare_label(mercury____Unify___bintree__bintree_2_0_i10);
Declare_label(mercury____Unify___bintree__bintree_2_0_i1008);
Declare_label(mercury____Unify___bintree__bintree_2_0_i1);
Define_extern_entry(mercury____Index___bintree__bintree_2_0);
Define_extern_entry(mercury____Compare___bintree__bintree_2_0);
Declare_label(mercury____Compare___bintree__bintree_2_0_i2);
Declare_label(mercury____Compare___bintree__bintree_2_0_i3);
Declare_label(mercury____Compare___bintree__bintree_2_0_i4);
Declare_label(mercury____Compare___bintree__bintree_2_0_i6);
Declare_label(mercury____Compare___bintree__bintree_2_0_i11);
Declare_label(mercury____Compare___bintree__bintree_2_0_i16);
Declare_label(mercury____Compare___bintree__bintree_2_0_i17);
Declare_label(mercury____Compare___bintree__bintree_2_0_i15);
Declare_label(mercury____Compare___bintree__bintree_2_0_i22);
Declare_label(mercury____Compare___bintree__bintree_2_0_i28);
Declare_label(mercury____Compare___bintree__bintree_2_0_i9);

extern Word * mercury_data_bintree__base_type_layout_bintree_2[];
Word * mercury_data_bintree__base_type_info_bintree_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) ENTRY(mercury____Unify___bintree__bintree_2_0),
	(Word *) (Integer) ENTRY(mercury____Index___bintree__bintree_2_0),
	(Word *) (Integer) ENTRY(mercury____Compare___bintree__bintree_2_0),
	(Word *) (Integer) mercury_data_bintree__base_type_layout_bintree_2
};

extern Word * mercury_data_bintree__common_0[];
extern Word * mercury_data_bintree__common_2[];
Word * mercury_data_bintree__base_type_layout_bintree_2[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bintree__common_0),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_bintree__common_2),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_bintree__common_0[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 1),
	(Word *) string_const("empty", 5)
};

Word * mercury_data_bintree__common_1[] = {
	(Word *) (Integer) mercury_data_bintree__base_type_info_bintree_2,
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2)
};

Word * mercury_data_bintree__common_2[] = {
	(Word *) ((Integer) 4),
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bintree__common_1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bintree__common_1),
	(Word *) string_const("tree", 4)
};

BEGIN_MODULE(mercury__bintree_module0)
	init_entry(mercury____Index___bintree_bintree_2__ua10000_2_0);
	init_label(mercury____Index___bintree_bintree_2__ua10000_2_0_i3);
BEGIN_CODE

/* code for predicate '__Index___bintree_bintree_2__ua10000'/2 in mode 0 */
Define_static(mercury____Index___bintree_bintree_2__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Index___bintree_bintree_2__ua10000_2_0_i3);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___bintree_bintree_2__ua10000_2_0_i3);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module1)
	init_entry(mercury__bintree__values_2__ua10000_3_0);
	init_label(mercury__bintree__values_2__ua10000_3_0_i4);
	init_label(mercury__bintree__values_2__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'bintree__values_2__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__values_2__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__values_2__ua10000_3_0_i1002);
	incr_sp_push_msg(3, "bintree__values_2__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	localcall(mercury__bintree__values_2__ua10000_3_0,
		LABEL(mercury__bintree__values_2__ua10000_3_0_i4),
		STATIC(mercury__bintree__values_2__ua10000_3_0));
Define_label(mercury__bintree__values_2__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__values_2__ua10000_3_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bintree__values_2__ua10000_3_0,
		STATIC(mercury__bintree__values_2__ua10000_3_0));
Define_label(mercury__bintree__values_2__ua10000_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module2)
	init_entry(mercury__bintree__keys_2__ua10000_3_0);
	init_label(mercury__bintree__keys_2__ua10000_3_0_i4);
	init_label(mercury__bintree__keys_2__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'bintree__keys_2__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__keys_2__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__keys_2__ua10000_3_0_i1002);
	incr_sp_push_msg(3, "bintree__keys_2__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	localcall(mercury__bintree__keys_2__ua10000_3_0,
		LABEL(mercury__bintree__keys_2__ua10000_3_0_i4),
		STATIC(mercury__bintree__keys_2__ua10000_3_0));
Define_label(mercury__bintree__keys_2__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__keys_2__ua10000_3_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bintree__keys_2__ua10000_3_0,
		STATIC(mercury__bintree__keys_2__ua10000_3_0));
Define_label(mercury__bintree__keys_2__ua10000_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module3)
	init_entry(mercury__bintree__to_list_2__ua10000_3_0);
	init_label(mercury__bintree__to_list_2__ua10000_3_0_i4);
	init_label(mercury__bintree__to_list_2__ua10000_3_0_i1003);
BEGIN_CODE

/* code for predicate 'bintree__to_list_2__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__to_list_2__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__to_list_2__ua10000_3_0_i1003);
	incr_sp_push_msg(4, "bintree__to_list_2__ua10000");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	localcall(mercury__bintree__to_list_2__ua10000_3_0,
		LABEL(mercury__bintree__to_list_2__ua10000_3_0_i4),
		STATIC(mercury__bintree__to_list_2__ua10000_3_0));
Define_label(mercury__bintree__to_list_2__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__to_list_2__ua10000_3_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__bintree__to_list_2__ua10000_3_0,
		STATIC(mercury__bintree__to_list_2__ua10000_3_0));
	}
Define_label(mercury__bintree__to_list_2__ua10000_3_0_i1003);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module4)
	init_entry(mercury__bintree__from_corresponding_lists_2__ua0_4_0);
	init_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1010);
	init_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i6);
	init_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i8);
	init_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1007);
	init_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1);
	init_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1009);
BEGIN_CODE

/* code for predicate 'bintree__from_corresponding_lists_2__ua0'/4 in mode 0 */
Define_static(mercury__bintree__from_corresponding_lists_2__ua0_4_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1010);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1007);
	r2 = (Integer) r4;
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1010);
	incr_sp_push_msg(4, "bintree__from_corresponding_lists_2__ua0");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	call_localret(STATIC(mercury__bintree__insert__ua0_4_0),
		mercury__bintree__from_corresponding_lists_2__ua0_4_0_i6,
		STATIC(mercury__bintree__from_corresponding_lists_2__ua0_4_0));
	}
Define_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i6);
	update_prof_current_proc(LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1);
	r4 = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__bintree__from_corresponding_lists_2__ua0_4_0,
		LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i8),
		STATIC(mercury__bintree__from_corresponding_lists_2__ua0_4_0));
Define_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i8);
	update_prof_current_proc(LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1009);
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1007);
	r1 = FALSE;
	proceed();
Define_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__bintree__from_corresponding_lists_2__ua0_4_0_i1009);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module5)
	init_entry(mercury__bintree__from_sorted_list_2__ua10000_4_0);
	init_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i1001);
	init_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i5);
	init_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i9);
	init_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i6);
BEGIN_CODE

/* code for predicate 'bintree__from_sorted_list_2__ua10000'/4 in mode 0 */
Define_static(mercury__bintree__from_sorted_list_2__ua10000_4_0);
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__from_sorted_list_2__ua10000_4_0_i1001);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i1001);
	incr_sp_push_msg(4, "bintree__from_sorted_list_2__ua10000");
	detstackvar(4) = (Integer) succip;
	r3 = ((Integer) r1 - ((Integer) 1));
	r1 = ((Integer) r3 / ((Integer) 2));
	detstackvar(1) = ((Integer) r3 - (Integer) r1);
	localcall(mercury__bintree__from_sorted_list_2__ua10000_4_0,
		LABEL(mercury__bintree__from_sorted_list_2__ua10000_4_0_i5),
		STATIC(mercury__bintree__from_sorted_list_2__ua10000_4_0));
Define_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i5);
	update_prof_current_proc(LABEL(mercury__bintree__from_sorted_list_2__ua10000_4_0));
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__from_sorted_list_2__ua10000_4_0_i6);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	localcall(mercury__bintree__from_sorted_list_2__ua10000_4_0,
		LABEL(mercury__bintree__from_sorted_list_2__ua10000_4_0_i9),
		STATIC(mercury__bintree__from_sorted_list_2__ua10000_4_0));
	}
Define_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i9);
	update_prof_current_proc(LABEL(mercury__bintree__from_sorted_list_2__ua10000_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__bintree__from_sorted_list_2__ua10000_4_0_i6);
	r1 = string_const("bintree__from_sorted_list_2", 27);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__bintree__from_sorted_list_2__ua10000_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_module6)
	init_entry(mercury__bintree__from_list_2__ua10000_3_0);
	init_label(mercury__bintree__from_list_2__ua10000_3_0_i6);
	init_label(mercury__bintree__from_list_2__ua10000_3_0_i5);
	init_label(mercury__bintree__from_list_2__ua10000_3_0_i8);
	init_label(mercury__bintree__from_list_2__ua10000_3_0_i1003);
BEGIN_CODE

/* code for predicate 'bintree__from_list_2__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__from_list_2__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__from_list_2__ua10000_3_0_i1003);
	incr_sp_push_msg(3, "bintree__from_list_2__ua10000");
	detstackvar(3) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	call_localret(STATIC(mercury__bintree__insert__ua0_4_0),
		mercury__bintree__from_list_2__ua10000_3_0_i6,
		STATIC(mercury__bintree__from_list_2__ua10000_3_0));
	}
Define_label(mercury__bintree__from_list_2__ua10000_3_0_i6);
	update_prof_current_proc(LABEL(mercury__bintree__from_list_2__ua10000_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__from_list_2__ua10000_3_0_i5);
	r3 = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bintree__from_list_2__ua10000_3_0,
		STATIC(mercury__bintree__from_list_2__ua10000_3_0));
Define_label(mercury__bintree__from_list_2__ua10000_3_0_i5);
	r1 = string_const("bintree__from_list: duplicate key", 33);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__bintree__from_list_2__ua10000_3_0_i8,
		STATIC(mercury__bintree__from_list_2__ua10000_3_0));
	}
Define_label(mercury__bintree__from_list_2__ua10000_3_0_i8);
	update_prof_current_proc(LABEL(mercury__bintree__from_list_2__ua10000_3_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bintree__from_list_2__ua10000_3_0,
		STATIC(mercury__bintree__from_list_2__ua10000_3_0));
Define_label(mercury__bintree__from_list_2__ua10000_3_0_i1003);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module7)
	init_entry(mercury__bintree__knock_right__ua10000_4_0);
	init_label(mercury__bintree__knock_right__ua10000_4_0_i1005);
	init_label(mercury__bintree__knock_right__ua10000_4_0_i7);
	init_label(mercury__bintree__knock_right__ua10000_4_0_i1004);
BEGIN_CODE

/* code for predicate 'bintree__knock_right__ua10000'/4 in mode 0 */
Define_static(mercury__bintree__knock_right__ua10000_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__knock_right__ua10000_4_0_i1004);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__knock_right__ua10000_4_0_i1005);
	r1 = (Integer) r5;
	r3 = (Integer) r2;
	r2 = (Integer) r4;
	proceed();
Define_label(mercury__bintree__knock_right__ua10000_4_0_i1005);
	incr_sp_push_msg(4, "bintree__knock_right__ua10000");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) r3;
	localcall(mercury__bintree__knock_right__ua10000_4_0,
		LABEL(mercury__bintree__knock_right__ua10000_4_0_i7),
		STATIC(mercury__bintree__knock_right__ua10000_4_0));
Define_label(mercury__bintree__knock_right__ua10000_4_0_i7);
	update_prof_current_proc(LABEL(mercury__bintree__knock_right__ua10000_4_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) tempr1, ((Integer) 2)) = (Integer) r3;
	r3 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
	}
Define_label(mercury__bintree__knock_right__ua10000_4_0_i1004);
	r1 = string_const("bintree__knock_right: empty tree", 32);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__bintree__knock_right__ua10000_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_module8)
	init_entry(mercury__bintree__knock_left__ua10000_4_0);
	init_label(mercury__bintree__knock_left__ua10000_4_0_i1005);
	init_label(mercury__bintree__knock_left__ua10000_4_0_i7);
	init_label(mercury__bintree__knock_left__ua10000_4_0_i1004);
BEGIN_CODE

/* code for predicate 'bintree__knock_left__ua10000'/4 in mode 0 */
Define_static(mercury__bintree__knock_left__ua10000_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__knock_left__ua10000_4_0_i1004);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__knock_left__ua10000_4_0_i1005);
	r1 = (Integer) r5;
	r2 = (Integer) r4;
	proceed();
Define_label(mercury__bintree__knock_left__ua10000_4_0_i1005);
	incr_sp_push_msg(4, "bintree__knock_left__ua10000");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r2;
	localcall(mercury__bintree__knock_left__ua10000_4_0,
		LABEL(mercury__bintree__knock_left__ua10000_4_0_i7),
		STATIC(mercury__bintree__knock_left__ua10000_4_0));
Define_label(mercury__bintree__knock_left__ua10000_4_0_i7);
	update_prof_current_proc(LABEL(mercury__bintree__knock_left__ua10000_4_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) tempr1, ((Integer) 3)) = (Integer) r3;
	r3 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
	}
Define_label(mercury__bintree__knock_left__ua10000_4_0_i1004);
	r1 = string_const("bintree__knock_left: empty tree", 31);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__bintree__knock_left__ua10000_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_module9)
	init_entry(mercury__bintree__left_depth__ua10000_2_0);
	init_label(mercury__bintree__left_depth__ua10000_2_0_i3);
	init_label(mercury__bintree__left_depth__ua10000_2_0_i4);
	init_label(mercury__bintree__left_depth__ua10000_2_0_i1);
BEGIN_CODE

/* code for predicate 'bintree__left_depth__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__left_depth__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__left_depth__ua10000_2_0_i1);
	r2 = ((Integer) 0);
Define_label(mercury__bintree__left_depth__ua10000_2_0_i3);
	while (1) {
	r2 = ((Integer) r2 + ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = ((Integer) 0);
	break; } /* end while */
Define_label(mercury__bintree__left_depth__ua10000_2_0_i4);
	while (1) {
	r1 = ((Integer) r1 + ((Integer) 1));
	r2 = ((Integer) r2 - ((Integer) 1));
	if (((Integer) r2 > ((Integer) 0)))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__bintree__left_depth__ua10000_2_0_i1);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module10)
	init_entry(mercury__bintree__right_depth__ua10000_2_0);
	init_label(mercury__bintree__right_depth__ua10000_2_0_i3);
	init_label(mercury__bintree__right_depth__ua10000_2_0_i4);
	init_label(mercury__bintree__right_depth__ua10000_2_0_i1);
BEGIN_CODE

/* code for predicate 'bintree__right_depth__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__right_depth__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__right_depth__ua10000_2_0_i1);
	r2 = ((Integer) 0);
Define_label(mercury__bintree__right_depth__ua10000_2_0_i3);
	while (1) {
	r2 = ((Integer) r2 + ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = ((Integer) 0);
	break; } /* end while */
Define_label(mercury__bintree__right_depth__ua10000_2_0_i4);
	while (1) {
	r1 = ((Integer) r1 + ((Integer) 1));
	r2 = ((Integer) r2 - ((Integer) 1));
	if (((Integer) r2 > ((Integer) 0)))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__bintree__right_depth__ua10000_2_0_i1);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module11)
	init_entry(mercury__bintree__fixup__ua10000_3_0);
	init_label(mercury__bintree__fixup__ua10000_3_0_i1001);
	init_label(mercury__bintree__fixup__ua10000_3_0_i8);
	init_label(mercury__bintree__fixup__ua10000_3_0_i9);
	init_label(mercury__bintree__fixup__ua10000_3_0_i12);
	init_label(mercury__bintree__fixup__ua10000_3_0_i10);
	init_label(mercury__bintree__fixup__ua10000_3_0_i13);
	init_label(mercury__bintree__fixup__ua10000_3_0_i16);
BEGIN_CODE

/* code for predicate 'bintree__fixup__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__fixup__ua10000_3_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__fixup__ua10000_3_0_i1001);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__bintree__fixup__ua10000_3_0_i1001);
	incr_sp_push_msg(4, "bintree__fixup__ua10000");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__fixup__ua10000_3_0_i16);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	call_localret(STATIC(mercury__bintree__right_depth__ua10000_2_0),
		mercury__bintree__fixup__ua10000_3_0_i8,
		STATIC(mercury__bintree__fixup__ua10000_3_0));
Define_label(mercury__bintree__fixup__ua10000_3_0_i8);
	update_prof_current_proc(LABEL(mercury__bintree__fixup__ua10000_3_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__bintree__left_depth__ua10000_2_0),
		mercury__bintree__fixup__ua10000_3_0_i9,
		STATIC(mercury__bintree__fixup__ua10000_3_0));
Define_label(mercury__bintree__fixup__ua10000_3_0_i9);
	update_prof_current_proc(LABEL(mercury__bintree__fixup__ua10000_3_0));
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury__bintree__fixup__ua10000_3_0_i10);
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__bintree__knock_left__ua10000_4_0),
		mercury__bintree__fixup__ua10000_3_0_i12,
		STATIC(mercury__bintree__fixup__ua10000_3_0));
Define_label(mercury__bintree__fixup__ua10000_3_0_i12);
	update_prof_current_proc(LABEL(mercury__bintree__fixup__ua10000_3_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) tempr1, ((Integer) 2)) = (Integer) r3;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
	}
Define_label(mercury__bintree__fixup__ua10000_3_0_i10);
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__bintree__knock_right__ua10000_4_0),
		mercury__bintree__fixup__ua10000_3_0_i13,
		STATIC(mercury__bintree__fixup__ua10000_3_0));
Define_label(mercury__bintree__fixup__ua10000_3_0_i13);
	update_prof_current_proc(LABEL(mercury__bintree__fixup__ua10000_3_0));
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r3;
Define_label(mercury__bintree__fixup__ua10000_3_0_i16);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module12)
	init_entry(mercury__bintree__branching_factor__ua10000_3_0);
	init_label(mercury__bintree__branching_factor__ua10000_3_0_i1003);
	init_label(mercury__bintree__branching_factor__ua10000_3_0_i10);
	init_label(mercury__bintree__branching_factor__ua10000_3_0_i1002);
	init_label(mercury__bintree__branching_factor__ua10000_3_0_i12);
	init_label(mercury__bintree__branching_factor__ua10000_3_0_i16);
	init_label(mercury__bintree__branching_factor__ua10000_3_0_i17);
	init_label(mercury__bintree__branching_factor__ua10000_3_0_i1001);
BEGIN_CODE

/* code for predicate 'bintree__branching_factor__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__branching_factor__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__branching_factor__ua10000_3_0_i1001);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__branching_factor__ua10000_3_0_i1002);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__branching_factor__ua10000_3_0_i1003);
	r1 = ((Integer) 0);
	r2 = ((Integer) 0);
	proceed();
Define_label(mercury__bintree__branching_factor__ua10000_3_0_i1003);
	incr_sp_push_msg(3, "bintree__branching_factor__ua10000");
	detstackvar(3) = (Integer) succip;
	r1 = (Integer) r2;
	localcall(mercury__bintree__branching_factor__ua10000_3_0,
		LABEL(mercury__bintree__branching_factor__ua10000_3_0_i10),
		STATIC(mercury__bintree__branching_factor__ua10000_3_0));
Define_label(mercury__bintree__branching_factor__ua10000_3_0_i10);
	update_prof_current_proc(LABEL(mercury__bintree__branching_factor__ua10000_3_0));
	r1 = ((Integer) r1 + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__bintree__branching_factor__ua10000_3_0_i1002);
	incr_sp_push_msg(3, "bintree__branching_factor__ua10000");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__branching_factor__ua10000_3_0_i12);
	r1 = (Integer) r3;
	localcall(mercury__bintree__branching_factor__ua10000_3_0,
		LABEL(mercury__bintree__branching_factor__ua10000_3_0_i10),
		STATIC(mercury__bintree__branching_factor__ua10000_3_0));
Define_label(mercury__bintree__branching_factor__ua10000_3_0_i12);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) r3;
	localcall(mercury__bintree__branching_factor__ua10000_3_0,
		LABEL(mercury__bintree__branching_factor__ua10000_3_0_i16),
		STATIC(mercury__bintree__branching_factor__ua10000_3_0));
Define_label(mercury__bintree__branching_factor__ua10000_3_0_i16);
	update_prof_current_proc(LABEL(mercury__bintree__branching_factor__ua10000_3_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	localcall(mercury__bintree__branching_factor__ua10000_3_0,
		LABEL(mercury__bintree__branching_factor__ua10000_3_0_i17),
		STATIC(mercury__bintree__branching_factor__ua10000_3_0));
Define_label(mercury__bintree__branching_factor__ua10000_3_0_i17);
	update_prof_current_proc(LABEL(mercury__bintree__branching_factor__ua10000_3_0));
	r1 = ((Integer) detstackvar(1) + (Integer) r1);
	r2 = (((Integer) detstackvar(2) + (Integer) r2) + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__bintree__branching_factor__ua10000_3_0_i1001);
	r1 = ((Integer) 0);
	r2 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module13)
	init_entry(mercury__bintree__depth__ua10000_2_0);
	init_label(mercury__bintree__depth__ua10000_2_0_i4);
	init_label(mercury__bintree__depth__ua10000_2_0_i5);
	init_label(mercury__bintree__depth__ua10000_2_0_i6);
	init_label(mercury__bintree__depth__ua10000_2_0_i1002);
BEGIN_CODE

/* code for predicate 'bintree__depth__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__depth__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__depth__ua10000_2_0_i1002);
	incr_sp_push_msg(2, "bintree__depth__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	localcall(mercury__bintree__depth__ua10000_2_0,
		LABEL(mercury__bintree__depth__ua10000_2_0_i4),
		STATIC(mercury__bintree__depth__ua10000_2_0));
Define_label(mercury__bintree__depth__ua10000_2_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__depth__ua10000_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__bintree__depth__ua10000_2_0,
		LABEL(mercury__bintree__depth__ua10000_2_0_i5),
		STATIC(mercury__bintree__depth__ua10000_2_0));
Define_label(mercury__bintree__depth__ua10000_2_0_i5);
	update_prof_current_proc(LABEL(mercury__bintree__depth__ua10000_2_0));
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__int__max_3_0);
	call_localret(ENTRY(mercury__int__max_3_0),
		mercury__bintree__depth__ua10000_2_0_i6,
		STATIC(mercury__bintree__depth__ua10000_2_0));
	}
Define_label(mercury__bintree__depth__ua10000_2_0_i6);
	update_prof_current_proc(LABEL(mercury__bintree__depth__ua10000_2_0));
	r1 = ((Integer) r1 + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__bintree__depth__ua10000_2_0_i1002);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module14)
	init_entry(mercury__bintree__count__ua10000_2_0);
	init_label(mercury__bintree__count__ua10000_2_0_i4);
	init_label(mercury__bintree__count__ua10000_2_0_i5);
	init_label(mercury__bintree__count__ua10000_2_0_i1002);
BEGIN_CODE

/* code for predicate 'bintree__count__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__count__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__count__ua10000_2_0_i1002);
	incr_sp_push_msg(2, "bintree__count__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	localcall(mercury__bintree__count__ua10000_2_0,
		LABEL(mercury__bintree__count__ua10000_2_0_i4),
		STATIC(mercury__bintree__count__ua10000_2_0));
Define_label(mercury__bintree__count__ua10000_2_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__count__ua10000_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__bintree__count__ua10000_2_0,
		LABEL(mercury__bintree__count__ua10000_2_0_i5),
		STATIC(mercury__bintree__count__ua10000_2_0));
Define_label(mercury__bintree__count__ua10000_2_0_i5);
	update_prof_current_proc(LABEL(mercury__bintree__count__ua10000_2_0));
	r1 = (((Integer) r1 + (Integer) detstackvar(1)) + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__bintree__count__ua10000_2_0_i1002);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module15)
	init_entry(mercury__bintree__to_list__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'bintree__to_list__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__to_list__ua10000_2_0);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__bintree__to_list_2__ua10000_3_0),
		STATIC(mercury__bintree__to_list__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module16)
	init_entry(mercury__bintree__from_corresponding_lists__ua10000_3_0);
	init_label(mercury__bintree__from_corresponding_lists__ua10000_3_0_i4);
	init_label(mercury__bintree__from_corresponding_lists__ua10000_3_0_i1000);
BEGIN_CODE

/* code for predicate 'bintree__from_corresponding_lists__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__from_corresponding_lists__ua10000_3_0);
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	incr_sp_push_msg(1, "bintree__from_corresponding_lists__ua10000");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__bintree__from_corresponding_lists_2__ua0_4_0),
		mercury__bintree__from_corresponding_lists__ua10000_3_0_i4,
		STATIC(mercury__bintree__from_corresponding_lists__ua10000_3_0));
Define_label(mercury__bintree__from_corresponding_lists__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__from_corresponding_lists__ua10000_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__from_corresponding_lists__ua10000_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__bintree__from_corresponding_lists__ua10000_3_0_i1000);
	r1 = string_const("bintree__from_corresponding_lists", 33);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__bintree__from_corresponding_lists__ua10000_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_module17)
	init_entry(mercury__bintree__from_list__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'bintree__from_list__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__from_list__ua10000_2_0);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__bintree__from_list_2__ua10000_3_0),
		STATIC(mercury__bintree__from_list__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module18)
	init_entry(mercury__bintree__values__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'bintree__values__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__values__ua10000_2_0);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__bintree__values_2__ua10000_3_0),
		STATIC(mercury__bintree__values__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module19)
	init_entry(mercury__bintree__keys__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'bintree__keys__ua10000'/2 in mode 0 */
Define_static(mercury__bintree__keys__ua10000_2_0);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__bintree__keys_2__ua10000_3_0),
		STATIC(mercury__bintree__keys__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module20)
	init_entry(mercury__bintree__remove__ua0_4_0);
	init_label(mercury__bintree__remove__ua0_4_0_i3);
	init_label(mercury__bintree__remove__ua0_4_0_i7);
	init_label(mercury__bintree__remove__ua0_4_0_i4);
	init_label(mercury__bintree__remove__ua0_4_0_i11);
	init_label(mercury__bintree__remove__ua0_4_0_i8);
	init_label(mercury__bintree__remove__ua0_4_0_i13);
	init_label(mercury__bintree__remove__ua0_4_0_i1007);
	init_label(mercury__bintree__remove__ua0_4_0_i1);
BEGIN_CODE

/* code for predicate 'bintree__remove__ua0'/4 in mode 0 */
Define_static(mercury__bintree__remove__ua0_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__remove__ua0_4_0_i1007);
	incr_sp_push_msg(7, "bintree__remove__ua0");
	detstackvar(7) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(6) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__bintree__remove__ua0_4_0_i3,
		STATIC(mercury__bintree__remove__ua0_4_0));
	}
Define_label(mercury__bintree__remove__ua0_4_0_i3);
	update_prof_current_proc(LABEL(mercury__bintree__remove__ua0_4_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__remove__ua0_4_0_i4);
	detstackvar(1) = (Integer) detstackvar(3);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__bintree__fixup__ua10000_3_0),
		mercury__bintree__remove__ua0_4_0_i7,
		STATIC(mercury__bintree__remove__ua0_4_0));
Define_label(mercury__bintree__remove__ua0_4_0_i7);
	update_prof_current_proc(LABEL(mercury__bintree__remove__ua0_4_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__bintree__remove__ua0_4_0_i4);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__remove__ua0_4_0_i8);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__bintree__remove__ua0_4_0,
		LABEL(mercury__bintree__remove__ua0_4_0_i11),
		STATIC(mercury__bintree__remove__ua0_4_0));
Define_label(mercury__bintree__remove__ua0_4_0_i11);
	update_prof_current_proc(LABEL(mercury__bintree__remove__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__remove__ua0_4_0_i1);
	r1 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r3, ((Integer) 3)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__bintree__remove__ua0_4_0_i8);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__bintree__remove__ua0_4_0,
		LABEL(mercury__bintree__remove__ua0_4_0_i13),
		STATIC(mercury__bintree__remove__ua0_4_0));
Define_label(mercury__bintree__remove__ua0_4_0_i13);
	update_prof_current_proc(LABEL(mercury__bintree__remove__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__remove__ua0_4_0_i1);
	r1 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r3, ((Integer) 2)) = (Integer) r1;
	field(mktag(1), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(5);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__bintree__remove__ua0_4_0_i1007);
	r1 = FALSE;
	proceed();
Define_label(mercury__bintree__remove__ua0_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module21)
	init_entry(mercury__bintree__delete__ua10000_3_0);
	init_label(mercury__bintree__delete__ua10000_3_0_i4);
	init_label(mercury__bintree__delete__ua10000_3_0_i5);
	init_label(mercury__bintree__delete__ua10000_3_0_i12);
	init_label(mercury__bintree__delete__ua10000_3_0_i9);
	init_label(mercury__bintree__delete__ua10000_3_0_i13);
	init_label(mercury__bintree__delete__ua10000_3_0_i1005);
BEGIN_CODE

/* code for predicate 'bintree__delete__ua10000'/3 in mode 0 */
Define_static(mercury__bintree__delete__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__delete__ua10000_3_0_i1005);
	incr_sp_push_msg(7, "bintree__delete__ua10000");
	detstackvar(7) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(6) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__bintree__delete__ua10000_3_0_i4,
		STATIC(mercury__bintree__delete__ua10000_3_0));
	}
Define_label(mercury__bintree__delete__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__delete__ua10000_3_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__delete__ua10000_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	tailcall(STATIC(mercury__bintree__fixup__ua10000_3_0),
		STATIC(mercury__bintree__delete__ua10000_3_0));
Define_label(mercury__bintree__delete__ua10000_3_0_i5);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__delete__ua10000_3_0_i9);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__bintree__delete__ua10000_3_0,
		LABEL(mercury__bintree__delete__ua10000_3_0_i12),
		STATIC(mercury__bintree__delete__ua10000_3_0));
Define_label(mercury__bintree__delete__ua10000_3_0_i12);
	update_prof_current_proc(LABEL(mercury__bintree__delete__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__bintree__delete__ua10000_3_0_i9);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__bintree__delete__ua10000_3_0,
		LABEL(mercury__bintree__delete__ua10000_3_0_i13),
		STATIC(mercury__bintree__delete__ua10000_3_0));
Define_label(mercury__bintree__delete__ua10000_3_0_i13);
	update_prof_current_proc(LABEL(mercury__bintree__delete__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__bintree__delete__ua10000_3_0_i1005);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module22)
	init_entry(mercury__bintree__search__ua1_3_0);
	init_label(mercury__bintree__search__ua1_3_0_i3);
	init_label(mercury__bintree__search__ua1_3_0_i4);
	init_label(mercury__bintree__search__ua1_3_0_i10);
	init_label(mercury__bintree__search__ua1_3_0_i7);
	init_label(mercury__bintree__search__ua1_3_0_i12);
	init_label(mercury__bintree__search__ua1_3_0_i1005);
	init_label(mercury__bintree__search__ua1_3_0_i1);
	init_label(mercury__bintree__search__ua1_3_0_i1006);
BEGIN_CODE

/* code for predicate 'bintree__search__ua1'/3 in mode 0 */
Define_static(mercury__bintree__search__ua1_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__search__ua1_3_0_i1005);
	incr_sp_push_msg(6, "bintree__search__ua1");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	detstackvar(5) = (Integer) r1;
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__bintree__search__ua1_3_0_i3,
		STATIC(mercury__bintree__search__ua1_3_0));
	}
Define_label(mercury__bintree__search__ua1_3_0_i3);
	update_prof_current_proc(LABEL(mercury__bintree__search__ua1_3_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__search__ua1_3_0_i4);
	r2 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__bintree__search__ua1_3_0_i4);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__search__ua1_3_0_i7);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__bintree__search__ua1_3_0,
		LABEL(mercury__bintree__search__ua1_3_0_i10),
		STATIC(mercury__bintree__search__ua1_3_0));
Define_label(mercury__bintree__search__ua1_3_0_i10);
	update_prof_current_proc(LABEL(mercury__bintree__search__ua1_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	if ((Integer) r1)
		GOTO_LABEL(mercury__bintree__search__ua1_3_0_i1006);
	r1 = FALSE;
	proceed();
Define_label(mercury__bintree__search__ua1_3_0_i7);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__bintree__search__ua1_3_0,
		LABEL(mercury__bintree__search__ua1_3_0_i12),
		STATIC(mercury__bintree__search__ua1_3_0));
Define_label(mercury__bintree__search__ua1_3_0_i12);
	update_prof_current_proc(LABEL(mercury__bintree__search__ua1_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__search__ua1_3_0_i1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__bintree__search__ua1_3_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury__bintree__search__ua1_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__bintree__search__ua1_3_0_i1006);
	r1 = TRUE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module23)
	init_entry(mercury__bintree__set__ua10001_4_0);
	init_label(mercury__bintree__set__ua10001_4_0_i4);
	init_label(mercury__bintree__set__ua10001_4_0_i5);
	init_label(mercury__bintree__set__ua10001_4_0_i11);
	init_label(mercury__bintree__set__ua10001_4_0_i8);
	init_label(mercury__bintree__set__ua10001_4_0_i12);
	init_label(mercury__bintree__set__ua10001_4_0_i1011);
BEGIN_CODE

/* code for predicate 'bintree__set__ua10001'/4 in mode 0 */
Define_static(mercury__bintree__set__ua10001_4_0);
	incr_sp_push_msg(8, "bintree__set__ua10001");
	detstackvar(8) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__set__ua10001_4_0_i1011);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__bintree__set__ua10001_4_0_i4,
		STATIC(mercury__bintree__set__ua10001_4_0));
	}
Define_label(mercury__bintree__set__ua10001_4_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__set__ua10001_4_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__set__ua10001_4_0_i5);
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__set__ua10001_4_0_i5);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__set__ua10001_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__set__ua10001_4_0,
		LABEL(mercury__bintree__set__ua10001_4_0_i11),
		STATIC(mercury__bintree__set__ua10001_4_0));
Define_label(mercury__bintree__set__ua10001_4_0_i11);
	update_prof_current_proc(LABEL(mercury__bintree__set__ua10001_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__set__ua10001_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__set__ua10001_4_0,
		LABEL(mercury__bintree__set__ua10001_4_0_i12),
		STATIC(mercury__bintree__set__ua10001_4_0));
Define_label(mercury__bintree__set__ua10001_4_0_i12);
	update_prof_current_proc(LABEL(mercury__bintree__set__ua10001_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__set__ua10001_4_0_i1011);
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module24)
	init_entry(mercury__bintree__set__ua10000_4_0);
	init_label(mercury__bintree__set__ua10000_4_0_i4);
	init_label(mercury__bintree__set__ua10000_4_0_i5);
	init_label(mercury__bintree__set__ua10000_4_0_i11);
	init_label(mercury__bintree__set__ua10000_4_0_i8);
	init_label(mercury__bintree__set__ua10000_4_0_i12);
	init_label(mercury__bintree__set__ua10000_4_0_i1011);
BEGIN_CODE

/* code for predicate 'bintree__set__ua10000'/4 in mode 0 */
Define_static(mercury__bintree__set__ua10000_4_0);
	incr_sp_push_msg(8, "bintree__set__ua10000");
	detstackvar(8) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__set__ua10000_4_0_i1011);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_0);
	call_localret(ENTRY(mercury__compare_3_0),
		mercury__bintree__set__ua10000_4_0_i4,
		STATIC(mercury__bintree__set__ua10000_4_0));
	}
Define_label(mercury__bintree__set__ua10000_4_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__set__ua10000_4_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__set__ua10000_4_0_i5);
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__set__ua10000_4_0_i5);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__set__ua10000_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__set__ua10000_4_0,
		LABEL(mercury__bintree__set__ua10000_4_0_i11),
		STATIC(mercury__bintree__set__ua10000_4_0));
Define_label(mercury__bintree__set__ua10000_4_0_i11);
	update_prof_current_proc(LABEL(mercury__bintree__set__ua10000_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__set__ua10000_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__set__ua10000_4_0,
		LABEL(mercury__bintree__set__ua10000_4_0_i12),
		STATIC(mercury__bintree__set__ua10000_4_0));
Define_label(mercury__bintree__set__ua10000_4_0_i12);
	update_prof_current_proc(LABEL(mercury__bintree__set__ua10000_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__set__ua10000_4_0_i1011);
	tag_incr_hp(r1, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module25)
	init_entry(mercury__bintree__update__ua0_4_0);
	init_label(mercury__bintree__update__ua0_4_0_i1001);
	init_label(mercury__bintree__update__ua0_4_0_i4);
	init_label(mercury__bintree__update__ua0_4_0_i5);
	init_label(mercury__bintree__update__ua0_4_0_i11);
	init_label(mercury__bintree__update__ua0_4_0_i8);
	init_label(mercury__bintree__update__ua0_4_0_i13);
	init_label(mercury__bintree__update__ua0_4_0_i1);
BEGIN_CODE

/* code for predicate 'bintree__update__ua0'/4 in mode 0 */
Define_static(mercury__bintree__update__ua0_4_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__update__ua0_4_0_i1001);
	r1 = FALSE;
	proceed();
Define_label(mercury__bintree__update__ua0_4_0_i1001);
	incr_sp_push_msg(8, "bintree__update__ua0");
	detstackvar(8) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__bintree__update__ua0_4_0_i4,
		STATIC(mercury__bintree__update__ua0_4_0));
	}
Define_label(mercury__bintree__update__ua0_4_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__update__ua0_4_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__update__ua0_4_0_i5);
	tag_incr_hp(r2, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(6);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__update__ua0_4_0_i5);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__update__ua0_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__update__ua0_4_0,
		LABEL(mercury__bintree__update__ua0_4_0_i11),
		STATIC(mercury__bintree__update__ua0_4_0));
Define_label(mercury__bintree__update__ua0_4_0_i11);
	update_prof_current_proc(LABEL(mercury__bintree__update__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__update__ua0_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r2, ((Integer) 3)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__update__ua0_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__update__ua0_4_0,
		LABEL(mercury__bintree__update__ua0_4_0_i13),
		STATIC(mercury__bintree__update__ua0_4_0));
Define_label(mercury__bintree__update__ua0_4_0_i13);
	update_prof_current_proc(LABEL(mercury__bintree__update__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__update__ua0_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 2)) = (Integer) r1;
	field(mktag(1), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(6);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__update__ua0_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module26)
	init_entry(mercury__bintree__insert__ua0_4_0);
	init_label(mercury__bintree__insert__ua0_4_0_i3);
	init_label(mercury__bintree__insert__ua0_4_0_i4);
	init_label(mercury__bintree__insert__ua0_4_0_i11);
	init_label(mercury__bintree__insert__ua0_4_0_i8);
	init_label(mercury__bintree__insert__ua0_4_0_i13);
	init_label(mercury__bintree__insert__ua0_4_0_i1);
BEGIN_CODE

/* code for predicate 'bintree__insert__ua0'/4 in mode 0 */
Define_static(mercury__bintree__insert__ua0_4_0);
	incr_sp_push_msg(8, "bintree__insert__ua0");
	detstackvar(8) = (Integer) succip;
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__insert__ua0_4_0_i3);
	tag_incr_hp(r2, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r2, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__insert__ua0_4_0_i3);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__bintree__insert__ua0_4_0_i4,
		STATIC(mercury__bintree__insert__ua0_4_0));
	}
Define_label(mercury__bintree__insert__ua0_4_0_i4);
	update_prof_current_proc(LABEL(mercury__bintree__insert__ua0_4_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__insert__ua0_4_0_i1);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__insert__ua0_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__insert__ua0_4_0,
		LABEL(mercury__bintree__insert__ua0_4_0_i11),
		STATIC(mercury__bintree__insert__ua0_4_0));
Define_label(mercury__bintree__insert__ua0_4_0_i11);
	update_prof_current_proc(LABEL(mercury__bintree__insert__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__insert__ua0_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r2, ((Integer) 3)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__insert__ua0_4_0_i8);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__bintree__insert__ua0_4_0,
		LABEL(mercury__bintree__insert__ua0_4_0_i13),
		STATIC(mercury__bintree__insert__ua0_4_0));
Define_label(mercury__bintree__insert__ua0_4_0_i13);
	update_prof_current_proc(LABEL(mercury__bintree__insert__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__insert__ua0_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 2)) = (Integer) r1;
	field(mktag(1), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(6);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__bintree__insert__ua0_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module27)
	init_entry(mercury__bintree__init__ua10000_1_0);
BEGIN_CODE

/* code for predicate 'bintree__init__ua10000'/1 in mode 0 */
Define_static(mercury__bintree__init__ua10000_1_0);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module28)
	init_entry(mercury__bintree__init_1_0);
BEGIN_CODE

/* code for predicate 'bintree__init'/1 in mode 0 */
Define_entry(mercury__bintree__init_1_0);
	tailcall(STATIC(mercury__bintree__init__ua10000_1_0),
		ENTRY(mercury__bintree__init_1_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module29)
	init_entry(mercury__bintree__insert_4_0);
	init_label(mercury__bintree__insert_4_0_i2);
	init_label(mercury__bintree__insert_4_0_i1000);
BEGIN_CODE

/* code for predicate 'bintree__insert'/4 in mode 0 */
Define_entry(mercury__bintree__insert_4_0);
	incr_sp_push_msg(2, "bintree__insert");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	call_localret(STATIC(mercury__bintree__insert__ua0_4_0),
		mercury__bintree__insert_4_0_i2,
		ENTRY(mercury__bintree__insert_4_0));
Define_label(mercury__bintree__insert_4_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree__insert_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__insert_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree__insert_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module30)
	init_entry(mercury__bintree__update_4_0);
	init_label(mercury__bintree__update_4_0_i2);
	init_label(mercury__bintree__update_4_0_i1000);
BEGIN_CODE

/* code for predicate 'bintree__update'/4 in mode 0 */
Define_entry(mercury__bintree__update_4_0);
	incr_sp_push_msg(2, "bintree__update");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	call_localret(STATIC(mercury__bintree__update__ua0_4_0),
		mercury__bintree__update_4_0_i2,
		ENTRY(mercury__bintree__update_4_0));
Define_label(mercury__bintree__update_4_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree__update_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__update_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree__update_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module31)
	init_entry(mercury__bintree__set_4_0);
BEGIN_CODE

/* code for predicate 'bintree__set'/4 in mode 0 */
Define_entry(mercury__bintree__set_4_0);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	tailcall(STATIC(mercury__bintree__set__ua10000_4_0),
		ENTRY(mercury__bintree__set_4_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module32)
	init_entry(mercury__bintree__set_4_1);
BEGIN_CODE

/* code for predicate 'bintree__set'/4 in mode 1 */
Define_entry(mercury__bintree__set_4_1);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	tailcall(STATIC(mercury__bintree__set__ua10001_4_0),
		ENTRY(mercury__bintree__set_4_1));
END_MODULE

BEGIN_MODULE(mercury__bintree_module33)
	init_entry(mercury__bintree__search_3_0);
	init_label(mercury__bintree__search_3_0_i3);
	init_label(mercury__bintree__search_3_0_i4);
	init_label(mercury__bintree__search_3_0_i9);
	init_label(mercury__bintree__search_3_0_i1004);
BEGIN_CODE

/* code for predicate 'bintree__search'/3 in mode 0 */
Define_entry(mercury__bintree__search_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bintree__search_3_0_i1004);
	incr_sp_push_msg(8, "bintree__search");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 3));
	detstackvar(6) = (Integer) r1;
	detstackvar(7) = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r3 = (Integer) r4;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__bintree__search_3_0_i3,
		ENTRY(mercury__bintree__search_3_0));
	}
Define_label(mercury__bintree__search_3_0_i3);
	update_prof_current_proc(LABEL(mercury__bintree__search_3_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__bintree__search_3_0_i4);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury__bintree__search_3_0));
	}
Define_label(mercury__bintree__search_3_0_i4);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__bintree__search_3_0_i9);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__bintree__search_3_0,
		ENTRY(mercury__bintree__search_3_0));
Define_label(mercury__bintree__search_3_0_i9);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__bintree__search_3_0,
		ENTRY(mercury__bintree__search_3_0));
Define_label(mercury__bintree__search_3_0_i1004);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module34)
	init_entry(mercury__bintree__search_3_1);
	init_label(mercury__bintree__search_3_1_i2);
	init_label(mercury__bintree__search_3_1_i1000);
BEGIN_CODE

/* code for predicate 'bintree__search'/3 in mode 1 */
Define_entry(mercury__bintree__search_3_1);
	incr_sp_push_msg(2, "bintree__search");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__bintree__search__ua1_3_0),
		mercury__bintree__search_3_1_i2,
		ENTRY(mercury__bintree__search_3_1));
Define_label(mercury__bintree__search_3_1_i2);
	update_prof_current_proc(LABEL(mercury__bintree__search_3_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__search_3_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree__search_3_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module35)
	init_entry(mercury__bintree__delete_3_0);
BEGIN_CODE

/* code for predicate 'bintree__delete'/3 in mode 0 */
Define_entry(mercury__bintree__delete_3_0);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	tailcall(STATIC(mercury__bintree__delete__ua10000_3_0),
		ENTRY(mercury__bintree__delete_3_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module36)
	init_entry(mercury__bintree__remove_4_0);
	init_label(mercury__bintree__remove_4_0_i2);
	init_label(mercury__bintree__remove_4_0_i1000);
BEGIN_CODE

/* code for predicate 'bintree__remove'/4 in mode 0 */
Define_entry(mercury__bintree__remove_4_0);
	incr_sp_push_msg(2, "bintree__remove");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__bintree__remove__ua0_4_0),
		mercury__bintree__remove_4_0_i2,
		ENTRY(mercury__bintree__remove_4_0));
Define_label(mercury__bintree__remove_4_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree__remove_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bintree__remove_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__bintree__remove_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module37)
	init_entry(mercury__bintree__keys_2_0);
BEGIN_CODE

/* code for predicate 'bintree__keys'/2 in mode 0 */
Define_entry(mercury__bintree__keys_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__bintree__keys__ua10000_2_0),
		ENTRY(mercury__bintree__keys_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module38)
	init_entry(mercury__bintree__values_2_0);
BEGIN_CODE

/* code for predicate 'bintree__values'/2 in mode 0 */
Define_entry(mercury__bintree__values_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__bintree__values__ua10000_2_0),
		ENTRY(mercury__bintree__values_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module39)
	init_entry(mercury__bintree__from_list_2_0);
BEGIN_CODE

/* code for predicate 'bintree__from_list'/2 in mode 0 */
Define_entry(mercury__bintree__from_list_2_0);
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__bintree__from_list__ua10000_2_0),
		ENTRY(mercury__bintree__from_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module40)
	init_entry(mercury__bintree__from_sorted_list_2_0);
	init_label(mercury__bintree__from_sorted_list_2_0_i2);
BEGIN_CODE

/* code for predicate 'bintree__from_sorted_list'/2 in mode 0 */
Define_entry(mercury__bintree__from_sorted_list_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r2;
	r1 = (Integer) tempr1;
	r2 = (Integer) r3;
	incr_sp_push_msg(2, "bintree__from_sorted_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	{
	extern Word * mercury_data_std_util__base_type_info_pair_2[];
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	}
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__bintree__from_sorted_list_2_0_i2,
		ENTRY(mercury__bintree__from_sorted_list_2_0));
	}
	}
Define_label(mercury__bintree__from_sorted_list_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree__from_sorted_list_2_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__bintree__from_sorted_list_2__ua10000_4_0),
		ENTRY(mercury__bintree__from_sorted_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module41)
	init_entry(mercury__bintree__from_corresponding_lists_3_0);
BEGIN_CODE

/* code for predicate 'bintree__from_corresponding_lists'/3 in mode 0 */
Define_entry(mercury__bintree__from_corresponding_lists_3_0);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	tailcall(STATIC(mercury__bintree__from_corresponding_lists__ua10000_3_0),
		ENTRY(mercury__bintree__from_corresponding_lists_3_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module42)
	init_entry(mercury__bintree__to_list_2_0);
BEGIN_CODE

/* code for predicate 'bintree__to_list'/2 in mode 0 */
Define_entry(mercury__bintree__to_list_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__bintree__to_list__ua10000_2_0),
		ENTRY(mercury__bintree__to_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module43)
	init_entry(mercury__bintree__count_2_0);
BEGIN_CODE

/* code for predicate 'bintree__count'/2 in mode 0 */
Define_entry(mercury__bintree__count_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__bintree__count__ua10000_2_0),
		ENTRY(mercury__bintree__count_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module44)
	init_entry(mercury__bintree__depth_2_0);
BEGIN_CODE

/* code for predicate 'bintree__depth'/2 in mode 0 */
Define_entry(mercury__bintree__depth_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__bintree__depth__ua10000_2_0),
		ENTRY(mercury__bintree__depth_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module45)
	init_entry(mercury__bintree__branching_factor_3_0);
BEGIN_CODE

/* code for predicate 'bintree__branching_factor'/3 in mode 0 */
Define_entry(mercury__bintree__branching_factor_3_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__bintree__branching_factor__ua10000_3_0),
		ENTRY(mercury__bintree__branching_factor_3_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module46)
	init_entry(mercury__bintree__balance_2_0);
	init_label(mercury__bintree__balance_2_0_i2);
BEGIN_CODE

/* code for predicate 'bintree__balance'/2 in mode 0 */
Define_entry(mercury__bintree__balance_2_0);
	incr_sp_push_msg(3, "bintree__balance");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__bintree__to_list__ua10000_2_0),
		mercury__bintree__balance_2_0_i2,
		ENTRY(mercury__bintree__balance_2_0));
Define_label(mercury__bintree__balance_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bintree__balance_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__bintree__from_sorted_list_2_0),
		ENTRY(mercury__bintree__balance_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bintree_module47)
	init_entry(mercury____Unify___bintree__bintree_2_0);
	init_label(mercury____Unify___bintree__bintree_2_0_i1011);
	init_label(mercury____Unify___bintree__bintree_2_0_i6);
	init_label(mercury____Unify___bintree__bintree_2_0_i8);
	init_label(mercury____Unify___bintree__bintree_2_0_i10);
	init_label(mercury____Unify___bintree__bintree_2_0_i1008);
	init_label(mercury____Unify___bintree__bintree_2_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___bintree__bintree_2_0);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___bintree__bintree_2_0_i1011);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___bintree__bintree_2_0_i1008);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___bintree__bintree_2_0_i1011);
	incr_sp_push_msg(9, "__Unify__");
	detstackvar(9) = (Integer) succip;
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___bintree__bintree_2_0_i1);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 3));
	detstackvar(7) = (Integer) r1;
	detstackvar(8) = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury____Unify___bintree__bintree_2_0_i6,
		ENTRY(mercury____Unify___bintree__bintree_2_0));
	}
Define_label(mercury____Unify___bintree__bintree_2_0_i6);
	update_prof_current_proc(LABEL(mercury____Unify___bintree__bintree_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___bintree__bintree_2_0_i1);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury____Unify___bintree__bintree_2_0_i8,
		ENTRY(mercury____Unify___bintree__bintree_2_0));
	}
Define_label(mercury____Unify___bintree__bintree_2_0_i8);
	update_prof_current_proc(LABEL(mercury____Unify___bintree__bintree_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___bintree__bintree_2_0_i1);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(5);
	localcall(mercury____Unify___bintree__bintree_2_0,
		LABEL(mercury____Unify___bintree__bintree_2_0_i10),
		ENTRY(mercury____Unify___bintree__bintree_2_0));
Define_label(mercury____Unify___bintree__bintree_2_0_i10);
	update_prof_current_proc(LABEL(mercury____Unify___bintree__bintree_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___bintree__bintree_2_0_i1);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury____Unify___bintree__bintree_2_0,
		ENTRY(mercury____Unify___bintree__bintree_2_0));
Define_label(mercury____Unify___bintree__bintree_2_0_i1008);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___bintree__bintree_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bintree_module48)
	init_entry(mercury____Index___bintree__bintree_2_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___bintree__bintree_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury____Index___bintree_bintree_2__ua10000_2_0),
		ENTRY(mercury____Index___bintree__bintree_2_0));
END_MODULE

BEGIN_MODULE(mercury__bintree_module49)
	init_entry(mercury____Compare___bintree__bintree_2_0);
	init_label(mercury____Compare___bintree__bintree_2_0_i2);
	init_label(mercury____Compare___bintree__bintree_2_0_i3);
	init_label(mercury____Compare___bintree__bintree_2_0_i4);
	init_label(mercury____Compare___bintree__bintree_2_0_i6);
	init_label(mercury____Compare___bintree__bintree_2_0_i11);
	init_label(mercury____Compare___bintree__bintree_2_0_i16);
	init_label(mercury____Compare___bintree__bintree_2_0_i17);
	init_label(mercury____Compare___bintree__bintree_2_0_i15);
	init_label(mercury____Compare___bintree__bintree_2_0_i22);
	init_label(mercury____Compare___bintree__bintree_2_0_i28);
	init_label(mercury____Compare___bintree__bintree_2_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___bintree__bintree_2_0);
	incr_sp_push_msg(9, "__Compare__");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(7) = (Integer) r1;
	detstackvar(8) = (Integer) r2;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury____Index___bintree_bintree_2__ua10000_2_0),
		mercury____Compare___bintree__bintree_2_0_i2,
		ENTRY(mercury____Compare___bintree__bintree_2_0));
Define_label(mercury____Compare___bintree__bintree_2_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___bintree__bintree_2_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury____Index___bintree_bintree_2__ua10000_2_0),
		mercury____Compare___bintree__bintree_2_0_i3,
		ENTRY(mercury____Compare___bintree__bintree_2_0));
Define_label(mercury____Compare___bintree__bintree_2_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___bintree__bintree_2_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury____Compare___bintree__bintree_2_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury____Compare___bintree__bintree_2_0_i6);
	if (((Integer) detstackvar(1) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i11);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury____Compare___bintree__bintree_2_0_i11);
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) detstackvar(2);
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i9);
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 3));
	tempr2 = (Integer) detstackvar(1);
	r2 = (Integer) field(mktag(1), (Integer) tempr2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) tempr2, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) tempr2, ((Integer) 3));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) tempr2, ((Integer) 1));
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury____Compare___bintree__bintree_2_0_i16,
		ENTRY(mercury____Compare___bintree__bintree_2_0));
	}
	}
Define_label(mercury____Compare___bintree__bintree_2_0_i16);
	update_prof_current_proc(LABEL(mercury____Compare___bintree__bintree_2_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i15);
Define_label(mercury____Compare___bintree__bintree_2_0_i17);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury____Compare___bintree__bintree_2_0_i15);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury____Compare___bintree__bintree_2_0_i22,
		ENTRY(mercury____Compare___bintree__bintree_2_0));
	}
Define_label(mercury____Compare___bintree__bintree_2_0_i22);
	update_prof_current_proc(LABEL(mercury____Compare___bintree__bintree_2_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i17);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(5);
	localcall(mercury____Compare___bintree__bintree_2_0,
		LABEL(mercury____Compare___bintree__bintree_2_0_i28),
		ENTRY(mercury____Compare___bintree__bintree_2_0));
Define_label(mercury____Compare___bintree__bintree_2_0_i28);
	update_prof_current_proc(LABEL(mercury____Compare___bintree__bintree_2_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___bintree__bintree_2_0_i17);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury____Compare___bintree__bintree_2_0,
		ENTRY(mercury____Compare___bintree__bintree_2_0));
Define_label(mercury____Compare___bintree__bintree_2_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___bintree__bintree_2_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__bintree_bunch_0(void)
{
	mercury__bintree_module0();
	mercury__bintree_module1();
	mercury__bintree_module2();
	mercury__bintree_module3();
	mercury__bintree_module4();
	mercury__bintree_module5();
	mercury__bintree_module6();
	mercury__bintree_module7();
	mercury__bintree_module8();
	mercury__bintree_module9();
	mercury__bintree_module10();
	mercury__bintree_module11();
	mercury__bintree_module12();
	mercury__bintree_module13();
	mercury__bintree_module14();
	mercury__bintree_module15();
	mercury__bintree_module16();
	mercury__bintree_module17();
	mercury__bintree_module18();
	mercury__bintree_module19();
	mercury__bintree_module20();
	mercury__bintree_module21();
	mercury__bintree_module22();
	mercury__bintree_module23();
	mercury__bintree_module24();
	mercury__bintree_module25();
	mercury__bintree_module26();
	mercury__bintree_module27();
	mercury__bintree_module28();
	mercury__bintree_module29();
	mercury__bintree_module30();
	mercury__bintree_module31();
	mercury__bintree_module32();
	mercury__bintree_module33();
	mercury__bintree_module34();
	mercury__bintree_module35();
	mercury__bintree_module36();
	mercury__bintree_module37();
	mercury__bintree_module38();
	mercury__bintree_module39();
	mercury__bintree_module40();
}

static void mercury__bintree_bunch_1(void)
{
	mercury__bintree_module41();
	mercury__bintree_module42();
	mercury__bintree_module43();
	mercury__bintree_module44();
	mercury__bintree_module45();
	mercury__bintree_module46();
	mercury__bintree_module47();
	mercury__bintree_module48();
	mercury__bintree_module49();
}

#endif

void mercury__bintree__init(void); /* suppress gcc warning */
void mercury__bintree__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__bintree_bunch_0();
	mercury__bintree_bunch_1();
#endif
}
