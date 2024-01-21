/*
** Automatically generated from `propagate.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__propagate__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__propagate__build_cycle_list__ua10000_4_0);
Declare_label(mercury__propagate__build_cycle_list__ua10000_4_0_i4);
Declare_label(mercury__propagate__build_cycle_list__ua10000_4_0_i5);
Declare_label(mercury__propagate__build_cycle_list__ua10000_4_0_i1002);
Declare_static(mercury__propagate__LambdaGoal__1_3_0);
Define_extern_entry(mercury__propagate__counts_5_0);
Declare_label(mercury__propagate__counts_5_0_i2);
Declare_label(mercury__propagate__counts_5_0_i3);
Declare_label(mercury__propagate__counts_5_0_i4);
Declare_label(mercury__propagate__counts_5_0_i5);
Declare_label(mercury__propagate__counts_5_0_i6);
Declare_label(mercury__propagate__counts_5_0_i7);
Declare_label(mercury__propagate__counts_5_0_i8);
Declare_label(mercury__propagate__counts_5_0_i9);
Declare_label(mercury__propagate__counts_5_0_i10);
Declare_label(mercury__propagate__counts_5_0_i11);
Declare_label(mercury__propagate__counts_5_0_i12);
Declare_label(mercury__propagate__counts_5_0_i13);
Declare_label(mercury__propagate__counts_5_0_i14);
Declare_static(mercury__propagate__identify_cycles_2_8_0);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i4);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i5);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i10);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i9);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i8);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i6);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i12);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i13);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i14);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i15);
Declare_label(mercury__propagate__identify_cycles_2_8_0_i1004);
Declare_static(mercury__propagate__add_to_cycle_map_4_0);
Declare_label(mercury__propagate__add_to_cycle_map_4_0_i4);
Declare_label(mercury__propagate__add_to_cycle_map_4_0_i5);
Declare_label(mercury__propagate__add_to_cycle_map_4_0_i1002);
Declare_static(mercury__propagate__update_cycles_2_4_0);
Declare_label(mercury__propagate__update_cycles_2_4_0_i4);
Declare_label(mercury__propagate__update_cycles_2_4_0_i1002);
Declare_static(mercury__propagate__update_cycles_3_5_0);
Declare_label(mercury__propagate__update_cycles_3_5_0_i4);
Declare_label(mercury__propagate__update_cycles_3_5_0_i5);
Declare_label(mercury__propagate__update_cycles_3_5_0_i6);
Declare_label(mercury__propagate__update_cycles_3_5_0_i1002);
Declare_static(mercury__propagate__counts_2_5_0);
Declare_label(mercury__propagate__counts_2_5_0_i6);
Declare_label(mercury__propagate__counts_2_5_0_i8);
Declare_label(mercury__propagate__counts_2_5_0_i9);
Declare_label(mercury__propagate__counts_2_5_0_i12);
Declare_label(mercury__propagate__counts_2_5_0_i16);
Declare_label(mercury__propagate__counts_2_5_0_i17);
Declare_label(mercury__propagate__counts_2_5_0_i15);
Declare_label(mercury__propagate__counts_2_5_0_i18);
Declare_label(mercury__propagate__counts_2_5_0_i14);
Declare_label(mercury__propagate__counts_2_5_0_i19);
Declare_label(mercury__propagate__counts_2_5_0_i20);
Declare_label(mercury__propagate__counts_2_5_0_i21);
Declare_label(mercury__propagate__counts_2_5_0_i22);
Declare_label(mercury__propagate__counts_2_5_0_i23);
Declare_label(mercury__propagate__counts_2_5_0_i24);
Declare_label(mercury__propagate__counts_2_5_0_i25);
Declare_label(mercury__propagate__counts_2_5_0_i26);
Declare_label(mercury__propagate__counts_2_5_0_i27);
Declare_label(mercury__propagate__counts_2_5_0_i28);
Declare_label(mercury__propagate__counts_2_5_0_i29);
Declare_label(mercury__propagate__counts_2_5_0_i30);
Declare_label(mercury__propagate__counts_2_5_0_i31);
Declare_label(mercury__propagate__counts_2_5_0_i11);
Declare_label(mercury__propagate__counts_2_5_0_i5);
Declare_label(mercury__propagate__counts_2_5_0_i35);
Declare_label(mercury__propagate__counts_2_5_0_i36);
Declare_label(mercury__propagate__counts_2_5_0_i37);
Declare_label(mercury__propagate__counts_2_5_0_i38);
Declare_label(mercury__propagate__counts_2_5_0_i39);
Declare_label(mercury__propagate__counts_2_5_0_i40);
Declare_label(mercury__propagate__counts_2_5_0_i41);
Declare_label(mercury__propagate__counts_2_5_0_i42);
Declare_label(mercury__propagate__counts_2_5_0_i1005);
Declare_static(mercury__propagate__sum_self_counts_4_0);
Declare_label(mercury__propagate__sum_self_counts_4_0_i4);
Declare_label(mercury__propagate__sum_self_counts_4_0_i5);
Declare_label(mercury__propagate__sum_self_counts_4_0_i6);
Declare_label(mercury__propagate__sum_self_counts_4_0_i1002);
Declare_static(mercury__propagate__sum_propagated_counts_4_0);
Declare_label(mercury__propagate__sum_propagated_counts_4_0_i4);
Declare_label(mercury__propagate__sum_propagated_counts_4_0_i5);
Declare_label(mercury__propagate__sum_propagated_counts_4_0_i6);
Declare_label(mercury__propagate__sum_propagated_counts_4_0_i1002);
Declare_static(mercury__propagate__counts_3_6_0);
Declare_label(mercury__propagate__counts_3_6_0_i4);
Declare_label(mercury__propagate__counts_3_6_0_i5);
Declare_label(mercury__propagate__counts_3_6_0_i6);
Declare_label(mercury__propagate__counts_3_6_0_i7);
Declare_label(mercury__propagate__counts_3_6_0_i8);
Declare_label(mercury__propagate__counts_3_6_0_i9);
Declare_label(mercury__propagate__counts_3_6_0_i1002);
Declare_static(mercury__propagate__build_parent_map_2_10_0);
Declare_label(mercury__propagate__build_parent_map_2_10_0_i4);
Declare_label(mercury__propagate__build_parent_map_2_10_0_i5);
Declare_label(mercury__propagate__build_parent_map_2_10_0_i6);
Declare_label(mercury__propagate__build_parent_map_2_10_0_i1002);
Declare_static(mercury__propagate__add_to_parent_map_8_0);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i4);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i5);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i11);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i10);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i8);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i7);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i17);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i19);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i16);
Declare_label(mercury__propagate__add_to_parent_map_8_0_i1005);
Declare_static(mercury__propagate__assoc_list_to_pred_info_list_2_0);
Declare_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i4);
Declare_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i5);
Declare_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i1002);

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_propagate__base_type_layout_cycle_info_0[];
Word * mercury_data_propagate__base_type_info_cycle_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_propagate__base_type_layout_cycle_info_0
};

extern Word * mercury_data_propagate__common_4[];
Word * mercury_data_propagate__base_type_layout_cycle_info_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_propagate__common_4),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_propagate__common_4),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_propagate__common_4),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_propagate__common_4)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data___base_type_info_string_0[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_propagate__common_0[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_string_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_propagate__common_1[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word * mercury_data_propagate__common_2[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_propagate__common_1)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
Word * mercury_data_propagate__common_3[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_propagate__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_propagate__common_2)
};

Word * mercury_data_propagate__common_4[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_propagate__common_3)
};

BEGIN_MODULE(mercury__propagate_module0)
	init_entry(mercury__propagate__build_cycle_list__ua10000_4_0);
	init_label(mercury__propagate__build_cycle_list__ua10000_4_0_i4);
	init_label(mercury__propagate__build_cycle_list__ua10000_4_0_i5);
	init_label(mercury__propagate__build_cycle_list__ua10000_4_0_i1002);
BEGIN_CODE

/* code for predicate 'propagate__build_cycle_list__ua10000'/4 in mode 0 */
Define_static(mercury__propagate__build_cycle_list__ua10000_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__build_cycle_list__ua10000_4_0_i1002);
	incr_sp_push_msg(2, "propagate__build_cycle_list__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__propagate__build_cycle_list__ua10000_4_0,
		LABEL(mercury__propagate__build_cycle_list__ua10000_4_0_i4),
		STATIC(mercury__propagate__build_cycle_list__ua10000_4_0));
Define_label(mercury__propagate__build_cycle_list__ua10000_4_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__build_cycle_list__ua10000_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 0);
	{
	Declare_entry(mercury__prof_info__pred_info__init_3_0);
	call_localret(ENTRY(mercury__prof_info__pred_info__init_3_0),
		mercury__propagate__build_cycle_list__ua10000_4_0_i5,
		STATIC(mercury__propagate__build_cycle_list__ua10000_4_0));
	}
Define_label(mercury__propagate__build_cycle_list__ua10000_4_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__build_cycle_list__ua10000_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__propagate__build_cycle_list__ua10000_4_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module1)
	init_entry(mercury__propagate__LambdaGoal__1_3_0);
BEGIN_CODE

/* code for predicate 'propagate__LambdaGoal__1'/3 in mode 0 */
Define_static(mercury__propagate__LambdaGoal__1_3_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__relation__lookup_key_3_0);
	tailcall(ENTRY(mercury__relation__lookup_key_3_0),
		STATIC(mercury__propagate__LambdaGoal__1_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__propagate_module2)
	init_entry(mercury__propagate__counts_5_0);
	init_label(mercury__propagate__counts_5_0_i2);
	init_label(mercury__propagate__counts_5_0_i3);
	init_label(mercury__propagate__counts_5_0_i4);
	init_label(mercury__propagate__counts_5_0_i5);
	init_label(mercury__propagate__counts_5_0_i6);
	init_label(mercury__propagate__counts_5_0_i7);
	init_label(mercury__propagate__counts_5_0_i8);
	init_label(mercury__propagate__counts_5_0_i9);
	init_label(mercury__propagate__counts_5_0_i10);
	init_label(mercury__propagate__counts_5_0_i11);
	init_label(mercury__propagate__counts_5_0_i12);
	init_label(mercury__propagate__counts_5_0_i13);
	init_label(mercury__propagate__counts_5_0_i14);
BEGIN_CODE

/* code for predicate 'propagate__counts'/5 in mode 0 */
Define_entry(mercury__propagate__counts_5_0);
	incr_sp_push_msg(8, "propagate__counts");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__prof_info__prof_get_addrdeclmap_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_get_addrdeclmap_2_0),
		mercury__propagate__counts_5_0_i2,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i2);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__prof_info__prof_get_profnodemap_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_get_profnodemap_2_0),
		mercury__propagate__counts_5_0_i3,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i3);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__relation__dfsrev_2_0);
	call_localret(ENTRY(mercury__relation__dfsrev_2_0),
		mercury__propagate__counts_5_0_i4,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__relation__inverse_2_0);
	call_localret(ENTRY(mercury__relation__inverse_2_0),
		mercury__propagate__counts_5_0_i5,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__propagate__counts_5_0_i6,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i6);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__multi_map__init_1_0);
	call_localret(ENTRY(mercury__multi_map__init_1_0),
		mercury__propagate__counts_5_0_i7,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i7);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(7);
	detstackvar(7) = (Integer) r2;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	{
	extern Word * mercury_data_relation__base_type_info_relation_key_0[];
	r1 = (Integer) mercury_data_relation__base_type_info_relation_key_0;
	}
	{
	Declare_entry(mercury__set_bbbtree__init_1_0);
	call_localret(ENTRY(mercury__set_bbbtree__init_1_0),
		mercury__propagate__counts_5_0_i8,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i8);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 1);
	r3 = (Integer) detstackvar(6);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r6 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__propagate__identify_cycles_2_8_0),
		mercury__propagate__counts_5_0_i9,
		ENTRY(mercury__propagate__counts_5_0));
Define_label(mercury__propagate__counts_5_0_i9);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(1) = (Integer) r1;
	detstackvar(6) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__multi_map__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__multi_map__to_assoc_list_2_0),
		mercury__propagate__counts_5_0_i10,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i10);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__propagate__update_cycles_2_4_0),
		mercury__propagate__counts_5_0_i11,
		ENTRY(mercury__propagate__counts_5_0));
Define_label(mercury__propagate__counts_5_0_i11);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__propagate__counts_2_5_0),
		mercury__propagate__counts_5_0_i12,
		ENTRY(mercury__propagate__counts_5_0));
Define_label(mercury__propagate__counts_5_0_i12);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(6), ((Integer) 0));
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__prof_info__prof_set_cyclemap_3_0);
	call_localret(ENTRY(mercury__prof_info__prof_set_cyclemap_3_0),
		mercury__propagate__counts_5_0_i13,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i13);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__prof_info__prof_set_profnodemap_3_0);
	call_localret(ENTRY(mercury__prof_info__prof_set_profnodemap_3_0),
		mercury__propagate__counts_5_0_i14,
		ENTRY(mercury__propagate__counts_5_0));
	}
Define_label(mercury__propagate__counts_5_0_i14);
	update_prof_current_proc(LABEL(mercury__propagate__counts_5_0));
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module3)
	init_entry(mercury__propagate__identify_cycles_2_8_0);
	init_label(mercury__propagate__identify_cycles_2_8_0_i4);
	init_label(mercury__propagate__identify_cycles_2_8_0_i5);
	init_label(mercury__propagate__identify_cycles_2_8_0_i10);
	init_label(mercury__propagate__identify_cycles_2_8_0_i9);
	init_label(mercury__propagate__identify_cycles_2_8_0_i8);
	init_label(mercury__propagate__identify_cycles_2_8_0_i6);
	init_label(mercury__propagate__identify_cycles_2_8_0_i12);
	init_label(mercury__propagate__identify_cycles_2_8_0_i13);
	init_label(mercury__propagate__identify_cycles_2_8_0_i14);
	init_label(mercury__propagate__identify_cycles_2_8_0_i15);
	init_label(mercury__propagate__identify_cycles_2_8_0_i1004);
BEGIN_CODE

/* code for predicate 'propagate__identify_cycles_2'/8 in mode 0 */
Define_static(mercury__propagate__identify_cycles_2_8_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__identify_cycles_2_8_0_i1004);
	incr_sp_push_msg(12, "propagate__identify_cycles_2");
	detstackvar(12) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data___base_type_info_string_0;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	{
	Declare_entry(mercury__relation__dfsrev_5_0);
	call_localret(ENTRY(mercury__relation__dfsrev_5_0),
		mercury__propagate__identify_cycles_2_8_0_i4,
		STATIC(mercury__propagate__identify_cycles_2_8_0));
	}
Define_label(mercury__propagate__identify_cycles_2_8_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__identify_cycles_2_8_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 3));
	r4 = (Integer) r2;
	detstackvar(6) = (Integer) r1;
	detstackvar(7) = (Integer) r2;
	{
	extern Word * mercury_data_relation__base_type_info_relation_key_0[];
	r1 = (Integer) mercury_data_relation__base_type_info_relation_key_0;
	}
	r2 = (Integer) mercury_data___base_type_info_string_0;
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) STATIC(mercury__propagate__LambdaGoal__1_3_0);
	{
	Declare_entry(mercury__list__map_3_0);
	call_localret(ENTRY(mercury__list__map_3_0),
		mercury__propagate__identify_cycles_2_8_0_i5,
		STATIC(mercury__propagate__identify_cycles_2_8_0));
	}
Define_label(mercury__propagate__identify_cycles_2_8_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__identify_cycles_2_8_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__identify_cycles_2_8_0_i9);
	detstackvar(8) = (Integer) r1;
	r1 = string_const("propagate__identify_cycles_2: empty list\n", 41);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__propagate__identify_cycles_2_8_0_i10,
		STATIC(mercury__propagate__identify_cycles_2_8_0));
	}
Define_label(mercury__propagate__identify_cycles_2_8_0_i10);
	update_prof_current_proc(LABEL(mercury__propagate__identify_cycles_2_8_0));
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	GOTO_LABEL(mercury__propagate__identify_cycles_2_8_0_i8);
Define_label(mercury__propagate__identify_cycles_2_8_0_i9);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__identify_cycles_2_8_0_i6);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
Define_label(mercury__propagate__identify_cycles_2_8_0_i8);
	r9 = (Integer) r5;
	r5 = (Integer) r6;
	r6 = (Integer) r7;
	r7 = (Integer) r8;
	r8 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	GOTO_LABEL(mercury__propagate__identify_cycles_2_8_0_i13);
Define_label(mercury__propagate__identify_cycles_2_8_0_i6);
	r2 = (Integer) r1;
	detstackvar(8) = (Integer) r1;
	r3 = ((Integer) detstackvar(1) + ((Integer) 1));
	r1 = (Integer) detstackvar(4);
	detstackvar(9) = (Integer) r3;
	call_localret(STATIC(mercury__propagate__add_to_cycle_map_4_0),
		mercury__propagate__identify_cycles_2_8_0_i12,
		STATIC(mercury__propagate__identify_cycles_2_8_0));
Define_label(mercury__propagate__identify_cycles_2_8_0_i12);
	update_prof_current_proc(LABEL(mercury__propagate__identify_cycles_2_8_0));
	r4 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r8 = (Integer) detstackvar(9);
	r9 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
Define_label(mercury__propagate__identify_cycles_2_8_0_i13);
	detstackvar(2) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	detstackvar(9) = (Integer) r8;
	detstackvar(10) = (Integer) r9;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__propagate__identify_cycles_2_8_0_i14,
		STATIC(mercury__propagate__identify_cycles_2_8_0));
	}
Define_label(mercury__propagate__identify_cycles_2_8_0_i14);
	update_prof_current_proc(LABEL(mercury__propagate__identify_cycles_2_8_0));
	detstackvar(11) = (Integer) r1;
	{
	extern Word * mercury_data_relation__base_type_info_relation_key_0[];
	r1 = (Integer) mercury_data_relation__base_type_info_relation_key_0;
	}
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__list__delete_elems_3_0);
	call_localret(ENTRY(mercury__list__delete_elems_3_0),
		mercury__propagate__identify_cycles_2_8_0_i15,
		STATIC(mercury__propagate__identify_cycles_2_8_0));
	}
Define_label(mercury__propagate__identify_cycles_2_8_0_i15);
	update_prof_current_proc(LABEL(mercury__propagate__identify_cycles_2_8_0));
	r2 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(11);
	r6 = (Integer) detstackvar(10);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	localtailcall(mercury__propagate__identify_cycles_2_8_0,
		STATIC(mercury__propagate__identify_cycles_2_8_0));
Define_label(mercury__propagate__identify_cycles_2_8_0_i1004);
	r1 = (Integer) r5;
	r2 = (Integer) r6;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module4)
	init_entry(mercury__propagate__add_to_cycle_map_4_0);
	init_label(mercury__propagate__add_to_cycle_map_4_0_i4);
	init_label(mercury__propagate__add_to_cycle_map_4_0_i5);
	init_label(mercury__propagate__add_to_cycle_map_4_0_i1002);
BEGIN_CODE

/* code for predicate 'propagate__add_to_cycle_map'/4 in mode 0 */
Define_static(mercury__propagate__add_to_cycle_map_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__add_to_cycle_map_4_0_i1002);
	incr_sp_push_msg(5, "propagate__add_to_cycle_map");
	detstackvar(5) = (Integer) succip;
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r5 = (Integer) r3;
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__propagate__add_to_cycle_map_4_0_i4,
		STATIC(mercury__propagate__add_to_cycle_map_4_0));
	}
Define_label(mercury__propagate__add_to_cycle_map_4_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__add_to_cycle_map_4_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__multi_map__set_4_0);
	call_localret(ENTRY(mercury__multi_map__set_4_0),
		mercury__propagate__add_to_cycle_map_4_0_i5,
		STATIC(mercury__propagate__add_to_cycle_map_4_0));
	}
Define_label(mercury__propagate__add_to_cycle_map_4_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__add_to_cycle_map_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__propagate__add_to_cycle_map_4_0,
		STATIC(mercury__propagate__add_to_cycle_map_4_0));
Define_label(mercury__propagate__add_to_cycle_map_4_0_i1002);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module5)
	init_entry(mercury__propagate__update_cycles_2_4_0);
	init_label(mercury__propagate__update_cycles_2_4_0_i4);
	init_label(mercury__propagate__update_cycles_2_4_0_i1002);
BEGIN_CODE

/* code for predicate 'propagate__update_cycles_2'/4 in mode 0 */
Define_static(mercury__propagate__update_cycles_2_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__update_cycles_2_4_0_i1002);
	incr_sp_push_msg(3, "propagate__update_cycles_2");
	detstackvar(3) = (Integer) succip;
	r4 = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	call_localret(STATIC(mercury__propagate__update_cycles_3_5_0),
		mercury__propagate__update_cycles_2_4_0_i4,
		STATIC(mercury__propagate__update_cycles_2_4_0));
	}
Define_label(mercury__propagate__update_cycles_2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__update_cycles_2_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__propagate__update_cycles_2_4_0,
		STATIC(mercury__propagate__update_cycles_2_4_0));
Define_label(mercury__propagate__update_cycles_2_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module6)
	init_entry(mercury__propagate__update_cycles_3_5_0);
	init_label(mercury__propagate__update_cycles_3_5_0_i4);
	init_label(mercury__propagate__update_cycles_3_5_0_i5);
	init_label(mercury__propagate__update_cycles_3_5_0_i6);
	init_label(mercury__propagate__update_cycles_3_5_0_i1002);
BEGIN_CODE

/* code for predicate 'propagate__update_cycles_3'/5 in mode 0 */
Define_static(mercury__propagate__update_cycles_3_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__update_cycles_3_5_0_i1002);
	incr_sp_push_msg(6, "propagate__update_cycles_3");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	detstackvar(2) = (Integer) r3;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r1;
	r3 = (Integer) r4;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__prof_info__get_prof_node_4_0);
	call_localret(ENTRY(mercury__prof_info__get_prof_node_4_0),
		mercury__propagate__update_cycles_3_5_0_i4,
		STATIC(mercury__propagate__update_cycles_3_5_0));
	}
Define_label(mercury__propagate__update_cycles_3_5_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__update_cycles_3_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__prof_info__prof_node__set_cycle_num_3_0);
	call_localret(ENTRY(mercury__prof_info__prof_node__set_cycle_num_3_0),
		mercury__propagate__update_cycles_3_5_0_i5,
		STATIC(mercury__propagate__update_cycles_3_5_0));
	}
Define_label(mercury__propagate__update_cycles_3_5_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__update_cycles_3_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__prof_info__update_prof_node_5_0);
	call_localret(ENTRY(mercury__prof_info__update_prof_node_5_0),
		mercury__propagate__update_cycles_3_5_0_i6,
		STATIC(mercury__propagate__update_cycles_3_5_0));
	}
Define_label(mercury__propagate__update_cycles_3_5_0_i6);
	update_prof_current_proc(LABEL(mercury__propagate__update_cycles_3_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__propagate__update_cycles_3_5_0,
		STATIC(mercury__propagate__update_cycles_3_5_0));
Define_label(mercury__propagate__update_cycles_3_5_0_i1002);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module7)
	init_entry(mercury__propagate__counts_2_5_0);
	init_label(mercury__propagate__counts_2_5_0_i6);
	init_label(mercury__propagate__counts_2_5_0_i8);
	init_label(mercury__propagate__counts_2_5_0_i9);
	init_label(mercury__propagate__counts_2_5_0_i12);
	init_label(mercury__propagate__counts_2_5_0_i16);
	init_label(mercury__propagate__counts_2_5_0_i17);
	init_label(mercury__propagate__counts_2_5_0_i15);
	init_label(mercury__propagate__counts_2_5_0_i18);
	init_label(mercury__propagate__counts_2_5_0_i14);
	init_label(mercury__propagate__counts_2_5_0_i19);
	init_label(mercury__propagate__counts_2_5_0_i20);
	init_label(mercury__propagate__counts_2_5_0_i21);
	init_label(mercury__propagate__counts_2_5_0_i22);
	init_label(mercury__propagate__counts_2_5_0_i23);
	init_label(mercury__propagate__counts_2_5_0_i24);
	init_label(mercury__propagate__counts_2_5_0_i25);
	init_label(mercury__propagate__counts_2_5_0_i26);
	init_label(mercury__propagate__counts_2_5_0_i27);
	init_label(mercury__propagate__counts_2_5_0_i28);
	init_label(mercury__propagate__counts_2_5_0_i29);
	init_label(mercury__propagate__counts_2_5_0_i30);
	init_label(mercury__propagate__counts_2_5_0_i31);
	init_label(mercury__propagate__counts_2_5_0_i11);
	init_label(mercury__propagate__counts_2_5_0_i5);
	init_label(mercury__propagate__counts_2_5_0_i35);
	init_label(mercury__propagate__counts_2_5_0_i36);
	init_label(mercury__propagate__counts_2_5_0_i37);
	init_label(mercury__propagate__counts_2_5_0_i38);
	init_label(mercury__propagate__counts_2_5_0_i39);
	init_label(mercury__propagate__counts_2_5_0_i40);
	init_label(mercury__propagate__counts_2_5_0_i41);
	init_label(mercury__propagate__counts_2_5_0_i42);
	init_label(mercury__propagate__counts_2_5_0_i1005);
BEGIN_CODE

/* code for predicate 'propagate__counts_2'/5 in mode 0 */
Define_static(mercury__propagate__counts_2_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__counts_2_5_0_i1005);
	incr_sp_push_msg(14, "propagate__counts_2");
	detstackvar(14) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r4;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__propagate__counts_2_5_0_i6,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i6);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__propagate__counts_2_5_0_i5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) r2;
	detstackvar(6) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__multi_map__lookup_3_0);
	call_localret(ENTRY(mercury__multi_map__lookup_3_0),
		mercury__propagate__counts_2_5_0_i8,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i8);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = (Integer) r1;
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__propagate__counts_2_5_0_i9,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i9);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = ((Integer) r1 - ((Integer) 1));
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__list__drop_3_0);
	call_localret(ENTRY(mercury__list__drop_3_0),
		mercury__propagate__counts_2_5_0_i12,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i12);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__propagate__counts_2_5_0_i11);
	if (((Integer) detstackvar(7) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__counts_2_5_0_i15);
	detstackvar(8) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__propagate__counts_2_5_0_i16,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i16);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r7 = (Integer) r1;
	r2 = (Integer) detstackvar(7);
	r1 = (Integer) r2;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = ((Integer) 0);
	r6 = ((Integer) 0);
	call_localret(STATIC(mercury__propagate__build_parent_map_2_10_0),
		mercury__propagate__counts_2_5_0_i17,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i17);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	r10 = (Integer) r1;
	r11 = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	GOTO_LABEL(mercury__propagate__counts_2_5_0_i14);
Define_label(mercury__propagate__counts_2_5_0_i15);
	detstackvar(8) = (Integer) r2;
	r1 = string_const("build_parent_map: empty cycle list\n", 35);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__propagate__counts_2_5_0_i18,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i18);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	{
	Word tempr1;
	tempr1 = (Integer) r3;
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) tempr1;
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	r10 = (Integer) detstackvar(9);
	r11 = (Integer) detstackvar(10);
	}
Define_label(mercury__propagate__counts_2_5_0_i14);
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r8;
	detstackvar(8) = (Integer) r9;
	detstackvar(9) = (Integer) r10;
	detstackvar(10) = (Integer) r11;
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__to_assoc_list_2_0),
		mercury__propagate__counts_2_5_0_i19,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i19);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	call_localret(STATIC(mercury__propagate__assoc_list_to_pred_info_list_2_0),
		mercury__propagate__counts_2_5_0_i20,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i20);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__string__int_to_string_2_0);
	call_localret(ENTRY(mercury__string__int_to_string_2_0),
		mercury__propagate__counts_2_5_0_i21,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i21);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = (Integer) r1;
	r1 = string_const("< cycle ", 8);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__propagate__counts_2_5_0_i22,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i22);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = string_const(" as a whole >", 13);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__propagate__counts_2_5_0_i23,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i23);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	detstackvar(12) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__propagate__sum_self_counts_4_0),
		mercury__propagate__counts_2_5_0_i24,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i24);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	detstackvar(13) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__propagate__sum_propagated_counts_4_0),
		mercury__propagate__counts_2_5_0_i25,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i25);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = (Integer) detstackvar(7);
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__propagate__build_cycle_list__ua10000_4_0),
		mercury__propagate__counts_2_5_0_i26,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i26);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(12);
	r2 = ((Integer) 0);
	r3 = (Integer) detstackvar(13);
	r4 = (Integer) detstackvar(7);
	r6 = (Integer) detstackvar(9);
	r7 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__prof_info__prof_node__init_cycle_8_0);
	call_localret(ENTRY(mercury__prof_info__prof_node__init_cycle_8_0),
		mercury__propagate__counts_2_5_0_i27,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i27);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_prof_info__base_type_info_prof_node_0[];
	r2 = (Integer) mercury_data_prof_info__base_type_info_prof_node_0;
	}
	r3 = (Integer) detstackvar(3);
	r4 = (((Integer) 0) - (Integer) detstackvar(6));
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__propagate__counts_2_5_0_i28,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i28);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(13);
	{
	Declare_entry(mercury__int__to_float_2_0);
	call_localret(ENTRY(mercury__int__to_float_2_0),
		mercury__propagate__counts_2_5_0_i29,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i29);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = (Integer) detstackvar(9);
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__int__to_float_2_0);
	call_localret(ENTRY(mercury__int__to_float_2_0),
		mercury__propagate__counts_2_5_0_i30,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i30);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(11);
	r2 = float_to_word(word_to_float((Integer) detstackvar(9)) + word_to_float((Integer) detstackvar(7)));
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__propagate__counts_3_6_0),
		mercury__propagate__counts_2_5_0_i31,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i31);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(14);
	decr_sp_pop_msg(14);
	localtailcall(mercury__propagate__counts_2_5_0,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i11);
	r1 = string_const("propagate__counts_2: list_drop failed\n", 38);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(14);
	decr_sp_pop_msg(14);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__prof_info__get_prof_node_4_0);
	call_localret(ENTRY(mercury__prof_info__get_prof_node_4_0),
		mercury__propagate__counts_2_5_0_i35,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i35);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__prof_info__prof_node_get_initial_counts_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_initial_counts_2_0),
		mercury__propagate__counts_2_5_0_i36,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i36);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__prof_info__prof_node_get_propagated_counts_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_propagated_counts_2_0),
		mercury__propagate__counts_2_5_0_i37,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i37);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__prof_info__prof_node_get_parent_list_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_parent_list_2_0),
		mercury__propagate__counts_2_5_0_i38,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i38);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__prof_info__prof_node_get_total_calls_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_total_calls_2_0),
		mercury__propagate__counts_2_5_0_i39,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i39);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = (Integer) detstackvar(6);
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__int__to_float_2_0);
	call_localret(ENTRY(mercury__int__to_float_2_0),
		mercury__propagate__counts_2_5_0_i40,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i40);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r2 = (Integer) detstackvar(6);
	detstackvar(6) = float_to_word(word_to_float((Integer) r1) + word_to_float((Integer) detstackvar(7)));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__int__to_float_2_0);
	call_localret(ENTRY(mercury__int__to_float_2_0),
		mercury__propagate__counts_2_5_0_i41,
		STATIC(mercury__propagate__counts_2_5_0));
	}
Define_label(mercury__propagate__counts_2_5_0_i41);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__propagate__counts_3_6_0),
		mercury__propagate__counts_2_5_0_i42,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i42);
	update_prof_current_proc(LABEL(mercury__propagate__counts_2_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(14);
	decr_sp_pop_msg(14);
	localtailcall(mercury__propagate__counts_2_5_0,
		STATIC(mercury__propagate__counts_2_5_0));
Define_label(mercury__propagate__counts_2_5_0_i1005);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module8)
	init_entry(mercury__propagate__sum_self_counts_4_0);
	init_label(mercury__propagate__sum_self_counts_4_0_i4);
	init_label(mercury__propagate__sum_self_counts_4_0_i5);
	init_label(mercury__propagate__sum_self_counts_4_0_i6);
	init_label(mercury__propagate__sum_self_counts_4_0_i1002);
BEGIN_CODE

/* code for predicate 'propagate__sum_self_counts'/4 in mode 0 */
Define_static(mercury__propagate__sum_self_counts_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__sum_self_counts_4_0_i1002);
	incr_sp_push_msg(4, "propagate__sum_self_counts");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__propagate__sum_self_counts_4_0,
		LABEL(mercury__propagate__sum_self_counts_4_0_i4),
		STATIC(mercury__propagate__sum_self_counts_4_0));
Define_label(mercury__propagate__sum_self_counts_4_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__sum_self_counts_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__prof_info__get_prof_node_4_0);
	call_localret(ENTRY(mercury__prof_info__get_prof_node_4_0),
		mercury__propagate__sum_self_counts_4_0_i5,
		STATIC(mercury__propagate__sum_self_counts_4_0));
	}
Define_label(mercury__propagate__sum_self_counts_4_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__sum_self_counts_4_0));
	{
	Declare_entry(mercury__prof_info__prof_node_get_initial_counts_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_initial_counts_2_0),
		mercury__propagate__sum_self_counts_4_0_i6,
		STATIC(mercury__propagate__sum_self_counts_4_0));
	}
Define_label(mercury__propagate__sum_self_counts_4_0_i6);
	update_prof_current_proc(LABEL(mercury__propagate__sum_self_counts_4_0));
	r1 = ((Integer) detstackvar(1) + (Integer) r1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__propagate__sum_self_counts_4_0_i1002);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module9)
	init_entry(mercury__propagate__sum_propagated_counts_4_0);
	init_label(mercury__propagate__sum_propagated_counts_4_0_i4);
	init_label(mercury__propagate__sum_propagated_counts_4_0_i5);
	init_label(mercury__propagate__sum_propagated_counts_4_0_i6);
	init_label(mercury__propagate__sum_propagated_counts_4_0_i1002);
BEGIN_CODE

/* code for predicate 'propagate__sum_propagated_counts'/4 in mode 0 */
Define_static(mercury__propagate__sum_propagated_counts_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__sum_propagated_counts_4_0_i1002);
	incr_sp_push_msg(4, "propagate__sum_propagated_counts");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__propagate__sum_propagated_counts_4_0,
		LABEL(mercury__propagate__sum_propagated_counts_4_0_i4),
		STATIC(mercury__propagate__sum_propagated_counts_4_0));
Define_label(mercury__propagate__sum_propagated_counts_4_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__sum_propagated_counts_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__prof_info__get_prof_node_4_0);
	call_localret(ENTRY(mercury__prof_info__get_prof_node_4_0),
		mercury__propagate__sum_propagated_counts_4_0_i5,
		STATIC(mercury__propagate__sum_propagated_counts_4_0));
	}
Define_label(mercury__propagate__sum_propagated_counts_4_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__sum_propagated_counts_4_0));
	{
	Declare_entry(mercury__prof_info__prof_node_get_propagated_counts_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_propagated_counts_2_0),
		mercury__propagate__sum_propagated_counts_4_0_i6,
		STATIC(mercury__propagate__sum_propagated_counts_4_0));
	}
Define_label(mercury__propagate__sum_propagated_counts_4_0_i6);
	update_prof_current_proc(LABEL(mercury__propagate__sum_propagated_counts_4_0));
	r1 = float_to_word(word_to_float((Integer) detstackvar(1)) + word_to_float((Integer) r1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__propagate__sum_propagated_counts_4_0_i1002);
	{
	static const Float mercury_float_const_0 = 0;
	r1 = (Word)(&mercury_float_const_0);
	}
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module10)
	init_entry(mercury__propagate__counts_3_6_0);
	init_label(mercury__propagate__counts_3_6_0_i4);
	init_label(mercury__propagate__counts_3_6_0_i5);
	init_label(mercury__propagate__counts_3_6_0_i6);
	init_label(mercury__propagate__counts_3_6_0_i7);
	init_label(mercury__propagate__counts_3_6_0_i8);
	init_label(mercury__propagate__counts_3_6_0_i9);
	init_label(mercury__propagate__counts_3_6_0_i1002);
BEGIN_CODE

/* code for predicate 'propagate__counts_3'/6 in mode 0 */
Define_static(mercury__propagate__counts_3_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__counts_3_6_0_i1002);
	incr_sp_push_msg(9, "propagate__counts_3");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__prof_info__pred_info__get_entire_3_0);
	call_localret(ENTRY(mercury__prof_info__pred_info__get_entire_3_0),
		mercury__propagate__counts_3_6_0_i4,
		STATIC(mercury__propagate__counts_3_6_0));
	}
Define_label(mercury__propagate__counts_3_6_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__counts_3_6_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__int__to_float_2_0);
	call_localret(ENTRY(mercury__int__to_float_2_0),
		mercury__propagate__counts_3_6_0_i5,
		STATIC(mercury__propagate__counts_3_6_0));
	}
Define_label(mercury__propagate__counts_3_6_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__counts_3_6_0));
	detstackvar(7) = float_to_word((word_to_float((Integer) r1) / word_to_float((Integer) detstackvar(2))) * word_to_float((Integer) detstackvar(1)));
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__prof_info__get_prof_node_4_0);
	call_localret(ENTRY(mercury__prof_info__get_prof_node_4_0),
		mercury__propagate__counts_3_6_0_i6,
		STATIC(mercury__propagate__counts_3_6_0));
	}
Define_label(mercury__propagate__counts_3_6_0_i6);
	update_prof_current_proc(LABEL(mercury__propagate__counts_3_6_0));
	detstackvar(8) = (Integer) r1;
	{
	Declare_entry(mercury__prof_info__prof_node_get_propagated_counts_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_propagated_counts_2_0),
		mercury__propagate__counts_3_6_0_i7,
		STATIC(mercury__propagate__counts_3_6_0));
	}
Define_label(mercury__propagate__counts_3_6_0_i7);
	update_prof_current_proc(LABEL(mercury__propagate__counts_3_6_0));
	r1 = float_to_word(word_to_float((Integer) r1) + word_to_float((Integer) detstackvar(7)));
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__prof_info__prof_node_set_propagated_counts_3_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_set_propagated_counts_3_0),
		mercury__propagate__counts_3_6_0_i8,
		STATIC(mercury__propagate__counts_3_6_0));
	}
Define_label(mercury__propagate__counts_3_6_0_i8);
	update_prof_current_proc(LABEL(mercury__propagate__counts_3_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__prof_info__update_prof_node_5_0);
	call_localret(ENTRY(mercury__prof_info__update_prof_node_5_0),
		mercury__propagate__counts_3_6_0_i9,
		STATIC(mercury__propagate__counts_3_6_0));
	}
Define_label(mercury__propagate__counts_3_6_0_i9);
	update_prof_current_proc(LABEL(mercury__propagate__counts_3_6_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__propagate__counts_3_6_0,
		STATIC(mercury__propagate__counts_3_6_0));
Define_label(mercury__propagate__counts_3_6_0_i1002);
	r1 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module11)
	init_entry(mercury__propagate__build_parent_map_2_10_0);
	init_label(mercury__propagate__build_parent_map_2_10_0_i4);
	init_label(mercury__propagate__build_parent_map_2_10_0_i5);
	init_label(mercury__propagate__build_parent_map_2_10_0_i6);
	init_label(mercury__propagate__build_parent_map_2_10_0_i1002);
BEGIN_CODE

/* code for predicate 'build_parent_map_2'/10 in mode 0 */
Define_static(mercury__propagate__build_parent_map_2_10_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__build_parent_map_2_10_0_i1002);
	incr_sp_push_msg(8, "build_parent_map_2");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	{
	Declare_entry(mercury__prof_info__get_prof_node_4_0);
	call_localret(ENTRY(mercury__prof_info__get_prof_node_4_0),
		mercury__propagate__build_parent_map_2_10_0_i4,
		STATIC(mercury__propagate__build_parent_map_2_10_0));
	}
Define_label(mercury__propagate__build_parent_map_2_10_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__build_parent_map_2_10_0));
	{
	Declare_entry(mercury__prof_info__prof_node_get_parent_list_2_0);
	call_localret(ENTRY(mercury__prof_info__prof_node_get_parent_list_2_0),
		mercury__propagate__build_parent_map_2_10_0_i5,
		STATIC(mercury__propagate__build_parent_map_2_10_0));
	}
Define_label(mercury__propagate__build_parent_map_2_10_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__build_parent_map_2_10_0));
	r2 = (Integer) detstackvar(1);
	r3 = ((Integer) 0);
	r4 = ((Integer) 0);
	r5 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__propagate__add_to_parent_map_8_0),
		mercury__propagate__build_parent_map_2_10_0_i6,
		STATIC(mercury__propagate__build_parent_map_2_10_0));
Define_label(mercury__propagate__build_parent_map_2_10_0_i6);
	update_prof_current_proc(LABEL(mercury__propagate__build_parent_map_2_10_0));
	r5 = ((Integer) detstackvar(4) + (Integer) r1);
	r6 = ((Integer) detstackvar(5) + (Integer) r2);
	r7 = (Integer) r3;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__propagate__build_parent_map_2_10_0,
		STATIC(mercury__propagate__build_parent_map_2_10_0));
Define_label(mercury__propagate__build_parent_map_2_10_0_i1002);
	r1 = (Integer) r5;
	r2 = (Integer) r6;
	r3 = (Integer) r7;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module12)
	init_entry(mercury__propagate__add_to_parent_map_8_0);
	init_label(mercury__propagate__add_to_parent_map_8_0_i4);
	init_label(mercury__propagate__add_to_parent_map_8_0_i5);
	init_label(mercury__propagate__add_to_parent_map_8_0_i11);
	init_label(mercury__propagate__add_to_parent_map_8_0_i10);
	init_label(mercury__propagate__add_to_parent_map_8_0_i8);
	init_label(mercury__propagate__add_to_parent_map_8_0_i7);
	init_label(mercury__propagate__add_to_parent_map_8_0_i17);
	init_label(mercury__propagate__add_to_parent_map_8_0_i19);
	init_label(mercury__propagate__add_to_parent_map_8_0_i16);
	init_label(mercury__propagate__add_to_parent_map_8_0_i1005);
BEGIN_CODE

/* code for predicate 'add_to_parent_map'/8 in mode 0 */
Define_static(mercury__propagate__add_to_parent_map_8_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__add_to_parent_map_8_0_i1005);
	incr_sp_push_msg(8, "add_to_parent_map");
	detstackvar(8) = (Integer) succip;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__prof_info__pred_info_get_pred_name_2_0);
	call_localret(ENTRY(mercury__prof_info__pred_info_get_pred_name_2_0),
		mercury__propagate__add_to_parent_map_8_0_i4,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
	}
Define_label(mercury__propagate__add_to_parent_map_8_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__add_to_parent_map_8_0));
	r2 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__prof_info__pred_info_get_counts_2_0);
	call_localret(ENTRY(mercury__prof_info__pred_info_get_counts_2_0),
		mercury__propagate__add_to_parent_map_8_0_i5,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
	}
Define_label(mercury__propagate__add_to_parent_map_8_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__add_to_parent_map_8_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__propagate__add_to_parent_map_8_0_i11,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
	}
Define_label(mercury__propagate__add_to_parent_map_8_0_i11);
	update_prof_current_proc(LABEL(mercury__propagate__add_to_parent_map_8_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__propagate__add_to_parent_map_8_0_i10);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(7);
	GOTO_LABEL(mercury__propagate__add_to_parent_map_8_0_i8);
Define_label(mercury__propagate__add_to_parent_map_8_0_i10);
	if (((Integer) detstackvar(7) != ((Integer) 0)))
		GOTO_LABEL(mercury__propagate__add_to_parent_map_8_0_i7);
	r7 = (Integer) detstackvar(7);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
Define_label(mercury__propagate__add_to_parent_map_8_0_i8);
	r4 = ((Integer) r4 + (Integer) r7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__propagate__add_to_parent_map_8_0,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
Define_label(mercury__propagate__add_to_parent_map_8_0_i7);
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__propagate__add_to_parent_map_8_0_i17,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
	}
Define_label(mercury__propagate__add_to_parent_map_8_0_i17);
	update_prof_current_proc(LABEL(mercury__propagate__add_to_parent_map_8_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__propagate__add_to_parent_map_8_0_i16);
	r5 = ((Integer) r2 + (Integer) detstackvar(7));
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__det_update_4_0);
	call_localret(ENTRY(mercury__map__det_update_4_0),
		mercury__propagate__add_to_parent_map_8_0_i19,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
	}
Define_label(mercury__propagate__add_to_parent_map_8_0_i19);
	update_prof_current_proc(LABEL(mercury__propagate__add_to_parent_map_8_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = ((Integer) detstackvar(2) + (Integer) detstackvar(7));
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__propagate__add_to_parent_map_8_0,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
Define_label(mercury__propagate__add_to_parent_map_8_0_i16);
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__propagate__add_to_parent_map_8_0_i19,
		STATIC(mercury__propagate__add_to_parent_map_8_0));
	}
Define_label(mercury__propagate__add_to_parent_map_8_0_i1005);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__propagate_module13)
	init_entry(mercury__propagate__assoc_list_to_pred_info_list_2_0);
	init_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i4);
	init_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i5);
	init_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i1002);
BEGIN_CODE

/* code for predicate 'assoc_list_to_pred_info_list'/2 in mode 0 */
Define_static(mercury__propagate__assoc_list_to_pred_info_list_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__propagate__assoc_list_to_pred_info_list_2_0_i1002);
	incr_sp_push_msg(3, "assoc_list_to_pred_info_list");
	detstackvar(3) = (Integer) succip;
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	localcall(mercury__propagate__assoc_list_to_pred_info_list_2_0,
		LABEL(mercury__propagate__assoc_list_to_pred_info_list_2_0_i4),
		STATIC(mercury__propagate__assoc_list_to_pred_info_list_2_0));
Define_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i4);
	update_prof_current_proc(LABEL(mercury__propagate__assoc_list_to_pred_info_list_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__prof_info__pred_info__init_3_0);
	call_localret(ENTRY(mercury__prof_info__pred_info__init_3_0),
		mercury__propagate__assoc_list_to_pred_info_list_2_0_i5,
		STATIC(mercury__propagate__assoc_list_to_pred_info_list_2_0));
	}
Define_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i5);
	update_prof_current_proc(LABEL(mercury__propagate__assoc_list_to_pred_info_list_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__propagate__assoc_list_to_pred_info_list_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__propagate_bunch_0(void)
{
	mercury__propagate_module0();
	mercury__propagate_module1();
	mercury__propagate_module2();
	mercury__propagate_module3();
	mercury__propagate_module4();
	mercury__propagate_module5();
	mercury__propagate_module6();
	mercury__propagate_module7();
	mercury__propagate_module8();
	mercury__propagate_module9();
	mercury__propagate_module10();
	mercury__propagate_module11();
	mercury__propagate_module12();
	mercury__propagate_module13();
}

#endif

void mercury__propagate__init(void); /* suppress gcc warning */
void mercury__propagate__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__propagate_bunch_0();
#endif
}
