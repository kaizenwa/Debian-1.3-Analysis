/*
** Automatically generated from `dead_proc_elim.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__dead_proc_elim__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i9);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i8);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i5);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i12);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i16);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i13);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i17);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i18);
Declare_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i1006);
Declare_static(mercury__dead_proc_elim__LambdaGoal__1_4_0);
Declare_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i2);
Declare_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i3);
Declare_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i4);
Define_extern_entry(mercury__dead_proc_elim__dead_proc_elim_4_0);
Declare_label(mercury__dead_proc_elim__dead_proc_elim_4_0_i2);
Define_extern_entry(mercury__dead_proc_elim__analyze_2_0);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i2);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i3);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i4);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i5);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i6);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i7);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i8);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i9);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i10);
Declare_label(mercury__dead_proc_elim__analyze_2_0_i11);
Define_extern_entry(mercury__dead_proc_elim__eliminate_5_0);
Declare_label(mercury__dead_proc_elim__eliminate_5_0_i2);
Declare_label(mercury__dead_proc_elim__eliminate_5_0_i3);
Declare_label(mercury__dead_proc_elim__eliminate_5_0_i4);
Declare_label(mercury__dead_proc_elim__eliminate_5_0_i5);
Declare_label(mercury__dead_proc_elim__eliminate_5_0_i6);
Declare_label(mercury__dead_proc_elim__eliminate_5_0_i7);
Declare_label(mercury__dead_proc_elim__eliminate_5_0_i8);
Declare_static(mercury__dead_proc_elim__initialize_preds_6_0);
Declare_label(mercury__dead_proc_elim__initialize_preds_6_0_i4);
Declare_label(mercury__dead_proc_elim__initialize_preds_6_0_i5);
Declare_label(mercury__dead_proc_elim__initialize_preds_6_0_i6);
Declare_label(mercury__dead_proc_elim__initialize_preds_6_0_i1002);
Declare_static(mercury__dead_proc_elim__initialize_procs_6_0);
Declare_label(mercury__dead_proc_elim__initialize_procs_6_0_i4);
Declare_label(mercury__dead_proc_elim__initialize_procs_6_0_i5);
Declare_label(mercury__dead_proc_elim__initialize_procs_6_0_i1002);
Declare_static(mercury__dead_proc_elim__initialize_pragma_exports_5_0);
Declare_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i4);
Declare_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i5);
Declare_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i1002);
Declare_static(mercury__dead_proc_elim__initialize_base_gen_infos_5_0);
Declare_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1007);
Declare_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i6);
Declare_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i9);
Declare_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i10);
Declare_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i4);
Declare_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1004);
Declare_static(mercury__dead_proc_elim__examine_5_0);
Declare_label(mercury__dead_proc_elim__examine_5_0_i4);
Declare_label(mercury__dead_proc_elim__examine_5_0_i8);
Declare_label(mercury__dead_proc_elim__examine_5_0_i7);
Declare_label(mercury__dead_proc_elim__examine_5_0_i11);
Declare_label(mercury__dead_proc_elim__examine_5_0_i14);
Declare_label(mercury__dead_proc_elim__examine_5_0_i17);
Declare_label(mercury__dead_proc_elim__examine_5_0_i19);
Declare_label(mercury__dead_proc_elim__examine_5_0_i16);
Declare_label(mercury__dead_proc_elim__examine_5_0_i13);
Declare_label(mercury__dead_proc_elim__examine_5_0_i23);
Declare_label(mercury__dead_proc_elim__examine_5_0_i24);
Declare_label(mercury__dead_proc_elim__examine_5_0_i25);
Declare_label(mercury__dead_proc_elim__examine_5_0_i26);
Declare_label(mercury__dead_proc_elim__examine_5_0_i28);
Declare_label(mercury__dead_proc_elim__examine_5_0_i29);
Declare_label(mercury__dead_proc_elim__examine_5_0_i30);
Declare_label(mercury__dead_proc_elim__examine_5_0_i22);
Declare_label(mercury__dead_proc_elim__examine_5_0_i3);
Declare_static(mercury__dead_proc_elim__find_base_gen_info_5_0);
Declare_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1009);
Declare_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i5);
Declare_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1006);
Declare_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1008);
Declare_static(mercury__dead_proc_elim__examine_refs_5_0);
Declare_label(mercury__dead_proc_elim__examine_refs_5_0_i4);
Declare_label(mercury__dead_proc_elim__examine_refs_5_0_i5);
Declare_label(mercury__dead_proc_elim__examine_refs_5_0_i1002);
Declare_static(mercury__dead_proc_elim__examine_goals_6_0);
Declare_label(mercury__dead_proc_elim__examine_goals_6_0_i4);
Declare_label(mercury__dead_proc_elim__examine_goals_6_0_i1002);
Declare_static(mercury__dead_proc_elim__examine_cases_6_0);
Declare_label(mercury__dead_proc_elim__examine_cases_6_0_i4);
Declare_label(mercury__dead_proc_elim__examine_cases_6_0_i1002);
Declare_static(mercury__dead_proc_elim__examine_goal_6_0);
Declare_static(mercury__dead_proc_elim__examine_expr_6_0);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1009);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1008);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1007);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i7);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i14);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i15);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i11);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i17);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i18);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i8);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i26);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i27);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i28);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i30);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i31);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1006);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i33);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i36);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i37);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i41);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i44);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i40);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i46);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i35);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1000);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1002);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1003);
Declare_label(mercury__dead_proc_elim__examine_expr_6_0_i1004);
Declare_static(mercury__dead_proc_elim__eliminate_preds_7_0);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i4);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i5);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i9);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i8);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i11);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i12);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i13);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i14);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i15);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i16);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i17);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i6);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i21);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i22);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i23);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i24);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i25);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i26);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i27);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i31);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i28);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i18);
Declare_label(mercury__dead_proc_elim__eliminate_preds_7_0_i1006);
Declare_static(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0);
Declare_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i4);
Declare_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i7);
Declare_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i6);
Declare_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i9);
Declare_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i1007);
Define_extern_entry(mercury____Unify___dead_proc_elim__entity_0_0);
Declare_label(mercury____Unify___dead_proc_elim__entity_0_0_i3);
Declare_label(mercury____Unify___dead_proc_elim__entity_0_0_i1);
Define_extern_entry(mercury____Index___dead_proc_elim__entity_0_0);
Declare_label(mercury____Index___dead_proc_elim__entity_0_0_i3);
Define_extern_entry(mercury____Compare___dead_proc_elim__entity_0_0);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i2);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i3);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i4);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i6);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i15);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i16);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i14);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i11);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i24);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i30);
Declare_label(mercury____Compare___dead_proc_elim__entity_0_0_i9);
Define_extern_entry(mercury____Unify___dead_proc_elim__needed_map_0_0);
Define_extern_entry(mercury____Index___dead_proc_elim__needed_map_0_0);
Define_extern_entry(mercury____Compare___dead_proc_elim__needed_map_0_0);

extern Word * mercury_data_dead_proc_elim__base_type_layout_entity_0[];
Word * mercury_data_dead_proc_elim__base_type_info_entity_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___dead_proc_elim__entity_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___dead_proc_elim__entity_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___dead_proc_elim__entity_0_0),
	(Word *) (Integer) mercury_data_dead_proc_elim__base_type_layout_entity_0
};

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_dead_proc_elim__base_type_layout_entity_queue_0[];
Word * mercury_data_dead_proc_elim__base_type_info_entity_queue_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_dead_proc_elim__base_type_layout_entity_queue_0
};

extern Word * mercury_data_dead_proc_elim__base_type_layout_examined_set_0[];
Word * mercury_data_dead_proc_elim__base_type_info_examined_set_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_dead_proc_elim__base_type_layout_examined_set_0
};

extern Word * mercury_data_dead_proc_elim__base_type_layout_needed_map_0[];
Word * mercury_data_dead_proc_elim__base_type_info_needed_map_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___dead_proc_elim__needed_map_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___dead_proc_elim__needed_map_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___dead_proc_elim__needed_map_0_0),
	(Word *) (Integer) mercury_data_dead_proc_elim__base_type_layout_needed_map_0
};

extern Word * mercury_data_dead_proc_elim__common_6[];
Word * mercury_data_dead_proc_elim__base_type_layout_needed_map_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_6),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_6),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_6),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_6)
};

extern Word * mercury_data_dead_proc_elim__common_8[];
Word * mercury_data_dead_proc_elim__base_type_layout_examined_set_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_8),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_8),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_8),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_8)
};

extern Word * mercury_data_dead_proc_elim__common_10[];
Word * mercury_data_dead_proc_elim__base_type_layout_entity_queue_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_10),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_10),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_10),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_dead_proc_elim__common_10)
};

extern Word * mercury_data_dead_proc_elim__common_12[];
extern Word * mercury_data_dead_proc_elim__common_14[];
Word * mercury_data_dead_proc_elim__base_type_layout_entity_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_dead_proc_elim__common_12),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_dead_proc_elim__common_14),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_std_util__base_type_info_maybe_1[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_dead_proc_elim__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_maybe_1,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word mercury_data_dead_proc_elim__common_1[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word mercury_data_dead_proc_elim__common_2[] = {
	((Integer) 1)
};

Word mercury_data_dead_proc_elim__common_3[] = {
	((Integer) 0)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
Word * mercury_data_dead_proc_elim__common_4[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0
};

Word * mercury_data_dead_proc_elim__common_5[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0)
};

Word * mercury_data_dead_proc_elim__common_6[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_5)
};

extern Word * mercury_data_set__base_type_info_set_1[];
Word * mercury_data_dead_proc_elim__common_7[] = {
	(Word *) (Integer) mercury_data_set__base_type_info_set_1,
	(Word *) (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0
};

Word * mercury_data_dead_proc_elim__common_8[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_7)
};

extern Word * mercury_data_queue__base_type_info_queue_1[];
Word * mercury_data_dead_proc_elim__common_9[] = {
	(Word *) (Integer) mercury_data_queue__base_type_info_queue_1,
	(Word *) (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0
};

Word * mercury_data_dead_proc_elim__common_10[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_9)
};

Word * mercury_data_dead_proc_elim__common_11[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_dead_proc_elim__common_12[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_11),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_11),
	(Word *) string_const("proc", 4)
};

extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_dead_proc_elim__common_13[] = {
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word * mercury_data_dead_proc_elim__common_14[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_13),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_13),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_11),
	(Word *) string_const("base_gen_info", 13)
};

BEGIN_MODULE(mercury__dead_proc_elim_module0)
	init_entry(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i9);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i8);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i5);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i12);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i16);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i13);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i17);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i18);
	init_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i1006);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__eliminate_procs__ua10000'/11 in mode 0 */
Define_static(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i1006);
	incr_sp_push_msg(9, "dead_proc_elim__eliminate_procs__ua10000");
	detstackvar(9) = (Integer) succip;
	detstackvar(3) = (Integer) r4;
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	detstackvar(7) = (Integer) tempr1;
	detstackvar(8) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i9,
		STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	}
	}
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i9);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0,
		STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i8);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(3);
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i5);
	if (((Integer) detstackvar(7) != (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0))))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i5);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) tempr1;
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0,
		STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i5);
	r1 = ((Integer) 12);
	r2 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i12,
		STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i12);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i13);
	r5 = (Integer) r2;
	r1 = string_const("% Eliminated the dead procedure ", 32);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(7);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__passes_aux__write_proc_progress_message_6_0);
	call_localret(ENTRY(mercury__passes_aux__write_proc_progress_message_6_0),
		mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i16,
		STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i16);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(3);
	r8 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	r10 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	GOTO_LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i17);
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i13);
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(3);
	r8 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	r10 = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i17);
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r6;
	detstackvar(3) = (Integer) r7;
	detstackvar(4) = (Integer) r8;
	detstackvar(8) = (Integer) r9;
	detstackvar(5) = (Integer) r10;
	{
	Declare_entry(mercury__map__delete_3_1);
	call_localret(ENTRY(mercury__map__delete_3_1),
		mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i18,
		STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i18);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0,
		STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0));
Define_label(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0_i1006);
	r1 = (Integer) r6;
	r2 = (Integer) r7;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module1)
	init_entry(mercury__dead_proc_elim__LambdaGoal__1_4_0);
	init_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i2);
	init_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i3);
	init_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i4);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__LambdaGoal__1'/4 in mode 0 */
Define_static(mercury__dead_proc_elim__LambdaGoal__1_4_0);
	incr_sp_push_msg(4, "dead_proc_elim__LambdaGoal__1");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r1;
	r4 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__dead_proc_elim__LambdaGoal__1_4_0_i2,
		STATIC(mercury__dead_proc_elim__LambdaGoal__1_4_0));
	}
Define_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i2);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__LambdaGoal__1_4_0));
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_goal__goal_info_init_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_init_1_0),
		mercury__dead_proc_elim__LambdaGoal__1_4_0_i3,
		STATIC(mercury__dead_proc_elim__LambdaGoal__1_4_0));
	}
Define_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i3);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__LambdaGoal__1_4_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_1);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_goal_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_goal_3_0),
		mercury__dead_proc_elim__LambdaGoal__1_4_0_i4,
		STATIC(mercury__dead_proc_elim__LambdaGoal__1_4_0));
	}
Define_label(mercury__dead_proc_elim__LambdaGoal__1_4_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__LambdaGoal__1_4_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__map__det_update_4_0);
	tailcall(ENTRY(mercury__map__det_update_4_0),
		STATIC(mercury__dead_proc_elim__LambdaGoal__1_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module2)
	init_entry(mercury__dead_proc_elim__dead_proc_elim_4_0);
	init_label(mercury__dead_proc_elim__dead_proc_elim_4_0_i2);
BEGIN_CODE

/* code for predicate 'dead_proc_elim'/4 in mode 0 */
Define_entry(mercury__dead_proc_elim__dead_proc_elim_4_0);
	incr_sp_push_msg(3, "dead_proc_elim");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury__dead_proc_elim__analyze_2_0),
		mercury__dead_proc_elim__dead_proc_elim_4_0_i2,
		ENTRY(mercury__dead_proc_elim__dead_proc_elim_4_0));
	}
Define_label(mercury__dead_proc_elim__dead_proc_elim_4_0_i2);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__dead_proc_elim_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__dead_proc_elim__eliminate_5_0),
		ENTRY(mercury__dead_proc_elim__dead_proc_elim_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module3)
	init_entry(mercury__dead_proc_elim__analyze_2_0);
	init_label(mercury__dead_proc_elim__analyze_2_0_i2);
	init_label(mercury__dead_proc_elim__analyze_2_0_i3);
	init_label(mercury__dead_proc_elim__analyze_2_0_i4);
	init_label(mercury__dead_proc_elim__analyze_2_0_i5);
	init_label(mercury__dead_proc_elim__analyze_2_0_i6);
	init_label(mercury__dead_proc_elim__analyze_2_0_i7);
	init_label(mercury__dead_proc_elim__analyze_2_0_i8);
	init_label(mercury__dead_proc_elim__analyze_2_0_i9);
	init_label(mercury__dead_proc_elim__analyze_2_0_i10);
	init_label(mercury__dead_proc_elim__analyze_2_0_i11);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__analyze'/2 in mode 0 */
Define_entry(mercury__dead_proc_elim__analyze_2_0);
	incr_sp_push_msg(6, "dead_proc_elim__analyze");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__dead_proc_elim__analyze_2_0_i2,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
	}
Define_label(mercury__dead_proc_elim__analyze_2_0_i2);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	{
	Declare_entry(mercury__queue__init_1_0);
	call_localret(ENTRY(mercury__queue__init_1_0),
		mercury__dead_proc_elim__analyze_2_0_i3,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
	}
Define_label(mercury__dead_proc_elim__analyze_2_0_i3);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__dead_proc_elim__analyze_2_0_i4,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
	}
Define_label(mercury__dead_proc_elim__analyze_2_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_module__module_info_predids_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_predids_2_0),
		mercury__dead_proc_elim__analyze_2_0_i5,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
	}
Define_label(mercury__dead_proc_elim__analyze_2_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__dead_proc_elim__analyze_2_0_i6,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
	}
Define_label(mercury__dead_proc_elim__analyze_2_0_i6);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__dead_proc_elim__initialize_preds_6_0),
		mercury__dead_proc_elim__analyze_2_0_i7,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
Define_label(mercury__dead_proc_elim__analyze_2_0_i7);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_module__module_info_get_pragma_exported_procs_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_get_pragma_exported_procs_2_0),
		mercury__dead_proc_elim__analyze_2_0_i8,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
	}
Define_label(mercury__dead_proc_elim__analyze_2_0_i8);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__dead_proc_elim__initialize_pragma_exports_5_0),
		mercury__dead_proc_elim__analyze_2_0_i9,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
Define_label(mercury__dead_proc_elim__analyze_2_0_i9);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_module__module_info_base_gen_infos_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_base_gen_infos_2_0),
		mercury__dead_proc_elim__analyze_2_0_i10,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
	}
Define_label(mercury__dead_proc_elim__analyze_2_0_i10);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__dead_proc_elim__initialize_base_gen_infos_5_0),
		mercury__dead_proc_elim__analyze_2_0_i11,
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
Define_label(mercury__dead_proc_elim__analyze_2_0_i11);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__analyze_2_0));
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__dead_proc_elim__examine_5_0),
		ENTRY(mercury__dead_proc_elim__analyze_2_0));
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module4)
	init_entry(mercury__dead_proc_elim__eliminate_5_0);
	init_label(mercury__dead_proc_elim__eliminate_5_0_i2);
	init_label(mercury__dead_proc_elim__eliminate_5_0_i3);
	init_label(mercury__dead_proc_elim__eliminate_5_0_i4);
	init_label(mercury__dead_proc_elim__eliminate_5_0_i5);
	init_label(mercury__dead_proc_elim__eliminate_5_0_i6);
	init_label(mercury__dead_proc_elim__eliminate_5_0_i7);
	init_label(mercury__dead_proc_elim__eliminate_5_0_i8);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__eliminate'/5 in mode 0 */
Define_entry(mercury__dead_proc_elim__eliminate_5_0);
	incr_sp_push_msg(5, "dead_proc_elim__eliminate");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_predids_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_predids_2_0),
		mercury__dead_proc_elim__eliminate_5_0_i2,
		ENTRY(mercury__dead_proc_elim__eliminate_5_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_5_0_i2);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_5_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__dead_proc_elim__eliminate_5_0_i3,
		ENTRY(mercury__dead_proc_elim__eliminate_5_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_5_0_i3);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__dead_proc_elim__eliminate_preds_7_0),
		mercury__dead_proc_elim__eliminate_5_0_i4,
		ENTRY(mercury__dead_proc_elim__eliminate_5_0));
Define_label(mercury__dead_proc_elim__eliminate_5_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_set_preds_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_set_preds_3_0),
		mercury__dead_proc_elim__eliminate_5_0_i5,
		ENTRY(mercury__dead_proc_elim__eliminate_5_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_5_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_5_0));
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_module__module_info_base_gen_infos_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_base_gen_infos_2_0),
		mercury__dead_proc_elim__eliminate_5_0_i6,
		ENTRY(mercury__dead_proc_elim__eliminate_5_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_5_0_i6);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_5_0));
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0),
		mercury__dead_proc_elim__eliminate_5_0_i7,
		ENTRY(mercury__dead_proc_elim__eliminate_5_0));
Define_label(mercury__dead_proc_elim__eliminate_5_0_i7);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_module__module_info_set_base_gen_infos_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_set_base_gen_infos_3_0),
		mercury__dead_proc_elim__eliminate_5_0_i8,
		ENTRY(mercury__dead_proc_elim__eliminate_5_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_5_0_i8);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_5_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module5)
	init_entry(mercury__dead_proc_elim__initialize_preds_6_0);
	init_label(mercury__dead_proc_elim__initialize_preds_6_0_i4);
	init_label(mercury__dead_proc_elim__initialize_preds_6_0_i5);
	init_label(mercury__dead_proc_elim__initialize_preds_6_0_i6);
	init_label(mercury__dead_proc_elim__initialize_preds_6_0_i1002);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__initialize_preds'/6 in mode 0 */
Define_static(mercury__dead_proc_elim__initialize_preds_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__initialize_preds_6_0_i1002);
	incr_sp_push_msg(6, "dead_proc_elim__initialize_preds");
	detstackvar(6) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__dead_proc_elim__initialize_preds_6_0_i4,
		STATIC(mercury__dead_proc_elim__initialize_preds_6_0));
	}
Define_label(mercury__dead_proc_elim__initialize_preds_6_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_preds_6_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_exported_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_exported_procids_2_0),
		mercury__dead_proc_elim__initialize_preds_6_0_i5,
		STATIC(mercury__dead_proc_elim__initialize_preds_6_0));
	}
Define_label(mercury__dead_proc_elim__initialize_preds_6_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_preds_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__dead_proc_elim__initialize_procs_6_0),
		mercury__dead_proc_elim__initialize_preds_6_0_i6,
		STATIC(mercury__dead_proc_elim__initialize_preds_6_0));
Define_label(mercury__dead_proc_elim__initialize_preds_6_0_i6);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_preds_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__dead_proc_elim__initialize_preds_6_0,
		STATIC(mercury__dead_proc_elim__initialize_preds_6_0));
Define_label(mercury__dead_proc_elim__initialize_preds_6_0_i1002);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module6)
	init_entry(mercury__dead_proc_elim__initialize_procs_6_0);
	init_label(mercury__dead_proc_elim__initialize_procs_6_0_i4);
	init_label(mercury__dead_proc_elim__initialize_procs_6_0_i5);
	init_label(mercury__dead_proc_elim__initialize_procs_6_0_i1002);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__initialize_procs'/6 in mode 0 */
Define_static(mercury__dead_proc_elim__initialize_procs_6_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__initialize_procs_6_0_i1002);
	incr_sp_push_msg(5, "dead_proc_elim__initialize_procs");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	detstackvar(2) = (Integer) r4;
	{
	Declare_entry(mercury__queue__put_3_0);
	call_localret(ENTRY(mercury__queue__put_3_0),
		mercury__dead_proc_elim__initialize_procs_6_0_i4,
		STATIC(mercury__dead_proc_elim__initialize_procs_6_0));
	}
	}
Define_label(mercury__dead_proc_elim__initialize_procs_6_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_procs_6_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__dead_proc_elim__initialize_procs_6_0_i5,
		STATIC(mercury__dead_proc_elim__initialize_procs_6_0));
	}
Define_label(mercury__dead_proc_elim__initialize_procs_6_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_procs_6_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__dead_proc_elim__initialize_procs_6_0,
		STATIC(mercury__dead_proc_elim__initialize_procs_6_0));
Define_label(mercury__dead_proc_elim__initialize_procs_6_0_i1002);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module7)
	init_entry(mercury__dead_proc_elim__initialize_pragma_exports_5_0);
	init_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i4);
	init_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i5);
	init_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i1002);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__initialize_pragma_exports'/5 in mode 0 */
Define_static(mercury__dead_proc_elim__initialize_pragma_exports_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i1002);
	incr_sp_push_msg(4, "dead_proc_elim__initialize_pragma_exports");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	{
	Declare_entry(mercury__queue__put_3_0);
	call_localret(ENTRY(mercury__queue__put_3_0),
		mercury__dead_proc_elim__initialize_pragma_exports_5_0_i4,
		STATIC(mercury__dead_proc_elim__initialize_pragma_exports_5_0));
	}
	}
Define_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_pragma_exports_5_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__dead_proc_elim__initialize_pragma_exports_5_0_i5,
		STATIC(mercury__dead_proc_elim__initialize_pragma_exports_5_0));
	}
Define_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_pragma_exports_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__dead_proc_elim__initialize_pragma_exports_5_0,
		STATIC(mercury__dead_proc_elim__initialize_pragma_exports_5_0));
Define_label(mercury__dead_proc_elim__initialize_pragma_exports_5_0_i1002);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module8)
	init_entry(mercury__dead_proc_elim__initialize_base_gen_infos_5_0);
	init_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1007);
	init_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i6);
	init_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i9);
	init_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i10);
	init_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i4);
	init_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1004);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__initialize_base_gen_infos'/5 in mode 0 */
Define_static(mercury__dead_proc_elim__initialize_base_gen_infos_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1004);
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 4)) != ((Integer) 5)))
		GOTO_LABEL(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1007);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r3;
	r3 = (Integer) r4;
	r6 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 3));
	r5 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	incr_sp_push_msg(4, "dead_proc_elim__initialize_base_gen_infos");
	detstackvar(4) = (Integer) succip;
	GOTO_LABEL(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i6);
	}
Define_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1007);
	incr_sp_push_msg(4, "dead_proc_elim__initialize_base_gen_infos");
	detstackvar(4) = (Integer) succip;
	if (((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 4)) != ((Integer) 6)))
		GOTO_LABEL(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i4);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r3;
	r3 = (Integer) r4;
	r6 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 3));
	r5 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	}
Define_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i6);
	detstackvar(2) = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 3));
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	field(mktag(1), (Integer) r3, ((Integer) 2)) = (Integer) r6;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r5;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	{
	Declare_entry(mercury__queue__put_3_0);
	call_localret(ENTRY(mercury__queue__put_3_0),
		mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i9,
		STATIC(mercury__dead_proc_elim__initialize_base_gen_infos_5_0));
	}
Define_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i9);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_base_gen_infos_5_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i10,
		STATIC(mercury__dead_proc_elim__initialize_base_gen_infos_5_0));
	}
Define_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i10);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__initialize_base_gen_infos_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__dead_proc_elim__initialize_base_gen_infos_5_0,
		STATIC(mercury__dead_proc_elim__initialize_base_gen_infos_5_0));
Define_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i4);
	r1 = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__dead_proc_elim__initialize_base_gen_infos_5_0,
		STATIC(mercury__dead_proc_elim__initialize_base_gen_infos_5_0));
Define_label(mercury__dead_proc_elim__initialize_base_gen_infos_5_0_i1004);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module9)
	init_entry(mercury__dead_proc_elim__examine_5_0);
	init_label(mercury__dead_proc_elim__examine_5_0_i4);
	init_label(mercury__dead_proc_elim__examine_5_0_i8);
	init_label(mercury__dead_proc_elim__examine_5_0_i7);
	init_label(mercury__dead_proc_elim__examine_5_0_i11);
	init_label(mercury__dead_proc_elim__examine_5_0_i14);
	init_label(mercury__dead_proc_elim__examine_5_0_i17);
	init_label(mercury__dead_proc_elim__examine_5_0_i19);
	init_label(mercury__dead_proc_elim__examine_5_0_i16);
	init_label(mercury__dead_proc_elim__examine_5_0_i13);
	init_label(mercury__dead_proc_elim__examine_5_0_i23);
	init_label(mercury__dead_proc_elim__examine_5_0_i24);
	init_label(mercury__dead_proc_elim__examine_5_0_i25);
	init_label(mercury__dead_proc_elim__examine_5_0_i26);
	init_label(mercury__dead_proc_elim__examine_5_0_i28);
	init_label(mercury__dead_proc_elim__examine_5_0_i29);
	init_label(mercury__dead_proc_elim__examine_5_0_i30);
	init_label(mercury__dead_proc_elim__examine_5_0_i22);
	init_label(mercury__dead_proc_elim__examine_5_0_i3);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__examine'/5 in mode 0 */
Define_static(mercury__dead_proc_elim__examine_5_0);
	incr_sp_push_msg(8, "dead_proc_elim__examine");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__queue__get_3_0);
	call_localret(ENTRY(mercury__queue__get_3_0),
		mercury__dead_proc_elim__examine_5_0_i4,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__examine_5_0_i3);
	detstackvar(4) = (Integer) r2;
	detstackvar(5) = (Integer) r3;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__dead_proc_elim__examine_5_0_i8,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i8);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__examine_5_0_i7);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__dead_proc_elim__examine_5_0,
		STATIC(mercury__dead_proc_elim__examine_5_0));
Define_label(mercury__dead_proc_elim__examine_5_0_i7);
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__dead_proc_elim__examine_5_0_i11,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i11);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	if ((tag((Integer) detstackvar(4)) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_5_0_i13);
	detstackvar(1) = (Integer) r1;
	r2 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_module__module_info_base_gen_infos_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_base_gen_infos_2_0),
		mercury__dead_proc_elim__examine_5_0_i14,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i14);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__dead_proc_elim__find_base_gen_info_5_0),
		mercury__dead_proc_elim__examine_5_0_i17,
		STATIC(mercury__dead_proc_elim__examine_5_0));
Define_label(mercury__dead_proc_elim__examine_5_0_i17);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__examine_5_0_i16);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__dead_proc_elim__examine_refs_5_0),
		mercury__dead_proc_elim__examine_5_0_i19,
		STATIC(mercury__dead_proc_elim__examine_5_0));
Define_label(mercury__dead_proc_elim__examine_5_0_i19);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__dead_proc_elim__examine_5_0,
		STATIC(mercury__dead_proc_elim__examine_5_0));
Define_label(mercury__dead_proc_elim__examine_5_0_i16);
	r3 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__dead_proc_elim__examine_5_0,
		STATIC(mercury__dead_proc_elim__examine_5_0));
Define_label(mercury__dead_proc_elim__examine_5_0_i13);
	r3 = (Integer) detstackvar(4);
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(6) = (Integer) tempr1;
	detstackvar(7) = (Integer) r2;
	tag_incr_hp(tempr2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr2, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) tempr2, ((Integer) 0)) = (Integer) tempr1;
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) tempr2;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__dead_proc_elim__examine_5_0_i23,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i23);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__dead_proc_elim__examine_5_0_i24,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i24);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	detstackvar(6) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_non_imported_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_non_imported_procids_2_0),
		mercury__dead_proc_elim__examine_5_0_i25,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i25);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__dead_proc_elim__examine_5_0_i26,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i26);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__examine_5_0_i22);
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__dead_proc_elim__examine_5_0_i28,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i28);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	r4 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__dead_proc_elim__examine_5_0_i29,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i29);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	{
	Declare_entry(mercury__hlds_pred__proc_info_goal_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_goal_2_0),
		mercury__dead_proc_elim__examine_5_0_i30,
		STATIC(mercury__dead_proc_elim__examine_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_5_0_i30);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_5_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		mercury__dead_proc_elim__examine_5_0_i19,
		STATIC(mercury__dead_proc_elim__examine_5_0));
Define_label(mercury__dead_proc_elim__examine_5_0_i22);
	r3 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__dead_proc_elim__examine_5_0,
		STATIC(mercury__dead_proc_elim__examine_5_0));
Define_label(mercury__dead_proc_elim__examine_5_0_i3);
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module10)
	init_entry(mercury__dead_proc_elim__find_base_gen_info_5_0);
	init_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1009);
	init_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i5);
	init_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1006);
	init_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1008);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__find_base_gen_info'/5 in mode 0 */
Define_static(mercury__dead_proc_elim__find_base_gen_info_5_0);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__find_base_gen_info_5_0_i1006);
	r5 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	if ((strcmp((char *)(Integer) r1, (char *)(Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 1))) !=0))
		GOTO_LABEL(mercury__dead_proc_elim__find_base_gen_info_5_0_i1009);
	if ((strcmp((char *)(Integer) r2, (char *)(Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 2))) !=0))
		GOTO_LABEL(mercury__dead_proc_elim__find_base_gen_info_5_0_i1009);
	if (((Integer) r3 != (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 3))))
		GOTO_LABEL(mercury__dead_proc_elim__find_base_gen_info_5_0_i1009);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 6));
	r1 = TRUE;
	proceed();
Define_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1009);
	incr_sp_push_msg(1, "dead_proc_elim__find_base_gen_info");
	detstackvar(1) = (Integer) succip;
	r4 = (Integer) r5;
	localcall(mercury__dead_proc_elim__find_base_gen_info_5_0,
		LABEL(mercury__dead_proc_elim__find_base_gen_info_5_0_i5),
		STATIC(mercury__dead_proc_elim__find_base_gen_info_5_0));
Define_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__find_base_gen_info_5_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__find_base_gen_info_5_0_i1008);
	r1 = TRUE;
	proceed();
Define_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1006);
	r1 = FALSE;
	proceed();
Define_label(mercury__dead_proc_elim__find_base_gen_info_5_0_i1008);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module11)
	init_entry(mercury__dead_proc_elim__examine_refs_5_0);
	init_label(mercury__dead_proc_elim__examine_refs_5_0_i4);
	init_label(mercury__dead_proc_elim__examine_refs_5_0_i5);
	init_label(mercury__dead_proc_elim__examine_refs_5_0_i1002);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__examine_refs'/5 in mode 0 */
Define_static(mercury__dead_proc_elim__examine_refs_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_refs_5_0_i1002);
	incr_sp_push_msg(4, "dead_proc_elim__examine_refs");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	{
	Declare_entry(mercury__queue__put_3_0);
	call_localret(ENTRY(mercury__queue__put_3_0),
		mercury__dead_proc_elim__examine_refs_5_0_i4,
		STATIC(mercury__dead_proc_elim__examine_refs_5_0));
	}
	}
Define_label(mercury__dead_proc_elim__examine_refs_5_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_refs_5_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__dead_proc_elim__examine_refs_5_0_i5,
		STATIC(mercury__dead_proc_elim__examine_refs_5_0));
	}
Define_label(mercury__dead_proc_elim__examine_refs_5_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_refs_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__dead_proc_elim__examine_refs_5_0,
		STATIC(mercury__dead_proc_elim__examine_refs_5_0));
Define_label(mercury__dead_proc_elim__examine_refs_5_0_i1002);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module12)
	init_entry(mercury__dead_proc_elim__examine_goals_6_0);
	init_label(mercury__dead_proc_elim__examine_goals_6_0_i4);
	init_label(mercury__dead_proc_elim__examine_goals_6_0_i1002);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__examine_goals'/6 in mode 0 */
Define_static(mercury__dead_proc_elim__examine_goals_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_goals_6_0_i1002);
	incr_sp_push_msg(3, "dead_proc_elim__examine_goals");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		mercury__dead_proc_elim__examine_goals_6_0_i4,
		STATIC(mercury__dead_proc_elim__examine_goals_6_0));
Define_label(mercury__dead_proc_elim__examine_goals_6_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_goals_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__dead_proc_elim__examine_goals_6_0,
		STATIC(mercury__dead_proc_elim__examine_goals_6_0));
Define_label(mercury__dead_proc_elim__examine_goals_6_0_i1002);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module13)
	init_entry(mercury__dead_proc_elim__examine_cases_6_0);
	init_label(mercury__dead_proc_elim__examine_cases_6_0_i4);
	init_label(mercury__dead_proc_elim__examine_cases_6_0_i1002);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__examine_cases'/6 in mode 0 */
Define_static(mercury__dead_proc_elim__examine_cases_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_cases_6_0_i1002);
	incr_sp_push_msg(3, "dead_proc_elim__examine_cases");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	call_localret(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		mercury__dead_proc_elim__examine_cases_6_0_i4,
		STATIC(mercury__dead_proc_elim__examine_cases_6_0));
Define_label(mercury__dead_proc_elim__examine_cases_6_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_cases_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__dead_proc_elim__examine_cases_6_0,
		STATIC(mercury__dead_proc_elim__examine_cases_6_0));
Define_label(mercury__dead_proc_elim__examine_cases_6_0_i1002);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module14)
	init_entry(mercury__dead_proc_elim__examine_goal_6_0);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__examine_goal'/6 in mode 0 */
Define_static(mercury__dead_proc_elim__examine_goal_6_0);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	tailcall(STATIC(mercury__dead_proc_elim__examine_expr_6_0),
		STATIC(mercury__dead_proc_elim__examine_goal_6_0));
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module15)
	init_entry(mercury__dead_proc_elim__examine_expr_6_0);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1009);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1008);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1007);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i7);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i14);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i15);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i11);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i17);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i18);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i8);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i26);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i27);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i28);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i30);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i31);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1006);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i33);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i36);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i37);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i41);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i44);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i40);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i46);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i35);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1000);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1002);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1003);
	init_label(mercury__dead_proc_elim__examine_expr_6_0_i1004);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__examine_expr'/6 in mode 0 */
Define_static(mercury__dead_proc_elim__examine_expr_6_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1006);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1000) AND
		LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1009) AND
		LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1002) AND
		LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1003) AND
		LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1004) AND
		LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1008) AND
		LABEL(mercury__dead_proc_elim__examine_expr_6_0_i1007));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1009);
	incr_sp_push_msg(6, "dead_proc_elim__examine_expr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i7);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1008);
	incr_sp_push_msg(6, "dead_proc_elim__examine_expr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i26);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1007);
	incr_sp_push_msg(6, "dead_proc_elim__examine_expr");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i30);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i7);
	if ((tag((Integer) field(mktag(3), (Integer) r1, ((Integer) 4))) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i8);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(3), (Integer) r1, ((Integer) 4)), ((Integer) 1));
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i8);
	r5 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 0));
	if (((Integer) r5 != ((Integer) 1)))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i14);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 2));
	r1 = (Integer) r4;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i11);
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i14);
	if (((Integer) r5 != ((Integer) 2)))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i15);
	r5 = (Integer) r2;
	r1 = (Integer) r4;
	r2 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r5, ((Integer) 1));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r5, ((Integer) 2));
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i11);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i15);
	if (((Integer) r5 != ((Integer) 3)))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i8);
	r5 = (Integer) r2;
	r1 = (Integer) r4;
	r2 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 3));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r5, ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r5, ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 2)) = (Integer) field(mktag(3), (Integer) r5, ((Integer) 3));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i11);
	detstackvar(2) = (Integer) r1;
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	{
	Declare_entry(mercury__queue__put_3_0);
	call_localret(ENTRY(mercury__queue__put_3_0),
		mercury__dead_proc_elim__examine_expr_6_0_i17,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i17);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_expr_6_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__dead_proc_elim__examine_expr_6_0_i18,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i18);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_expr_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i8);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i26);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	call_localret(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		mercury__dead_proc_elim__examine_expr_6_0_i27,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i27);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_expr_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		mercury__dead_proc_elim__examine_expr_6_0_i28,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i28);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_expr_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i30);
	detstackvar(2) = (Integer) r4;
	r2 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	{
	Declare_entry(mercury__queue__put_3_0);
	call_localret(ENTRY(mercury__queue__put_3_0),
		mercury__dead_proc_elim__examine_expr_6_0_i31,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i31);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_expr_6_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__dead_proc_elim__examine_expr_6_0_i18,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1006);
	incr_sp_push_msg(6, "dead_proc_elim__examine_expr");
	detstackvar(6) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i33);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__dead_proc_elim__examine_goals_6_0),
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i33);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i35);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	detstackvar(5) = (Integer) r3;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__queue__put_3_0);
	call_localret(ENTRY(mercury__queue__put_3_0),
		mercury__dead_proc_elim__examine_expr_6_0_i36,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i36);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_expr_6_0));
	r2 = (Integer) detstackvar(5);
	if (((Integer) detstackvar(3) != (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 0))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i37);
	if (((Integer) detstackvar(4) != (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 1))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i37);
	r6 = (Integer) r1;
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) r2;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i46);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i37);
	r4 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__dead_proc_elim__examine_expr_6_0_i41,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i41);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__examine_expr_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i40);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i44);
	r6 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	tag_incr_hp(r5, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = ((Integer) field(mktag(1), (Integer) r2, ((Integer) 0)) + ((Integer) 1));
	r4 = (Integer) detstackvar(5);
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i46);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i44);
	r6 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(5);
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	GOTO_LABEL(mercury__dead_proc_elim__examine_expr_6_0_i46);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i40);
	r6 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) mkword(mktag(1), (Integer) mercury_data_dead_proc_elim__common_2);
	r4 = (Integer) detstackvar(5);
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i46);
	detstackvar(1) = (Integer) r6;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__dead_proc_elim__examine_expr_6_0_i18,
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
	}
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i35);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	tailcall(STATIC(mercury__dead_proc_elim__examine_cases_6_0),
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1002);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	tailcall(STATIC(mercury__dead_proc_elim__examine_goals_6_0),
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1003);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	tailcall(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
Define_label(mercury__dead_proc_elim__examine_expr_6_0_i1004);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	tailcall(STATIC(mercury__dead_proc_elim__examine_goal_6_0),
		STATIC(mercury__dead_proc_elim__examine_expr_6_0));
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module16)
	init_entry(mercury__dead_proc_elim__eliminate_preds_7_0);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i4);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i5);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i9);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i8);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i11);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i12);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i13);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i14);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i15);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i16);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i17);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i6);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i21);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i22);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i23);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i24);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i25);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i26);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i27);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i31);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i28);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i18);
	init_label(mercury__dead_proc_elim__eliminate_preds_7_0_i1006);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__eliminate_preds'/7 in mode 0 */
Define_static(mercury__dead_proc_elim__eliminate_preds_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_preds_7_0_i1006);
	incr_sp_push_msg(11, "dead_proc_elim__eliminate_preds");
	detstackvar(11) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) r4;
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__dead_proc_elim__eliminate_preds_7_0_i4,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_import_status_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_import_status_2_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i5,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i5);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	if (((Integer) r1 != ((Integer) 7)))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_preds_7_0_i9);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r9 = (Integer) mkword(mktag(1), (Integer) mercury_data_dead_proc_elim__common_3);
	GOTO_LABEL(mercury__dead_proc_elim__eliminate_preds_7_0_i8);
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i9);
	if (((Integer) r1 != ((Integer) 8)))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_preds_7_0_i6);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r9 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i8);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r1;
	detstackvar(8) = (Integer) r9;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procids_2_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i11,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i11);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i12,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i12);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_name_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_name_2_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i13,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i13);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_arity_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_arity_2_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i14,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i14);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(8);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(10);
	r7 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__dead_proc_elim__eliminate_procs__ua10000_11_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i15,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i15);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	detstackvar(4) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_procedures_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_procedures_3_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i16,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i16);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__det_update_4_0);
	call_localret(ENTRY(mercury__map__det_update_4_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i17,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i17);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	localtailcall(mercury__dead_proc_elim__eliminate_preds_7_0,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i6);
	if (((Integer) r1 != ((Integer) 2)))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_preds_7_0_i18);
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procids_2_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i21,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i21);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i22,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i22);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 3));
	r5 = (Integer) r1;
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_4);
	r4 = (Integer) detstackvar(8);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) STATIC(mercury__dead_proc_elim__LambdaGoal__1_4_0);
	{
	Declare_entry(mercury__list__foldl_4_1);
	call_localret(ENTRY(mercury__list__foldl_4_1),
		mercury__dead_proc_elim__eliminate_preds_7_0_i23,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i23);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_procedures_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_procedures_3_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i24,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i24);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r2 = ((Integer) 0);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_import_status_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_import_status_3_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i25,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i25);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__det_update_4_0);
	call_localret(ENTRY(mercury__map__det_update_4_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i26,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i26);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	detstackvar(3) = (Integer) r1;
	r1 = ((Integer) 12);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i27,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i27);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_preds_7_0_i28);
	r4 = (Integer) r2;
	r1 = string_const("% Eliminated opt_imported predicate ", 36);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__passes_aux__write_pred_progress_message_5_0);
	call_localret(ENTRY(mercury__passes_aux__write_pred_progress_message_5_0),
		mercury__dead_proc_elim__eliminate_preds_7_0_i31,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i31);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_preds_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	localtailcall(mercury__dead_proc_elim__eliminate_preds_7_0,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i28);
	r5 = (Integer) r2;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	localtailcall(mercury__dead_proc_elim__eliminate_preds_7_0,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i18);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	localtailcall(mercury__dead_proc_elim__eliminate_preds_7_0,
		STATIC(mercury__dead_proc_elim__eliminate_preds_7_0));
Define_label(mercury__dead_proc_elim__eliminate_preds_7_0_i1006);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module17)
	init_entry(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0);
	init_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i4);
	init_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i7);
	init_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i6);
	init_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i9);
	init_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i1007);
BEGIN_CODE

/* code for predicate 'dead_proc_elim__eliminate_base_gen_infos'/3 in mode 0 */
Define_static(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i1007);
	incr_sp_push_msg(9, "dead_proc_elim__eliminate_base_gen_infos");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0,
		LABEL(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i4),
		STATIC(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0));
Define_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i4);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0));
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r4, mktag(1), ((Integer) 3));
	detstackvar(1) = (Integer) r1;
	{
	Word tempr1, tempr2, tempr3, tempr4;
	tempr2 = (Integer) detstackvar(2);
	tempr1 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 1));
	tempr3 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 2));
	tempr4 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 3));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 0));
	detstackvar(7) = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 4));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 6));
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	detstackvar(4) = (Integer) tempr1;
	detstackvar(5) = (Integer) tempr3;
	detstackvar(6) = (Integer) tempr4;
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) tempr3;
	field(mktag(1), (Integer) r4, ((Integer) 2)) = (Integer) tempr4;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i7,
		STATIC(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0));
	}
	}
Define_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i7);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i6);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i6);
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_proc_id_0[];
	r1 = (Integer) mercury_data_hlds_pred__base_type_info_pred_proc_id_0;
	}
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i9,
		STATIC(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0));
	}
Define_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i9);
	update_prof_current_proc(LABEL(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 7));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r3, ((Integer) 4)) = (Integer) detstackvar(7);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(0), (Integer) r3, ((Integer) 5)) = (Integer) tempr1;
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	field(mktag(0), (Integer) r3, ((Integer) 6)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
	}
Define_label(mercury__dead_proc_elim__eliminate_base_gen_infos_3_0_i1007);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module18)
	init_entry(mercury____Unify___dead_proc_elim__entity_0_0);
	init_label(mercury____Unify___dead_proc_elim__entity_0_0_i3);
	init_label(mercury____Unify___dead_proc_elim__entity_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___dead_proc_elim__entity_0_0);
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i3);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	if (((Integer) field(mktag(0), (Integer) r1, ((Integer) 0)) != (Integer) field(mktag(0), (Integer) r2, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	if (((Integer) field(mktag(0), (Integer) r1, ((Integer) 1)) != (Integer) field(mktag(0), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___dead_proc_elim__entity_0_0_i3);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	if ((strcmp((char *)(Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), (char *)(Integer) field(mktag(1), (Integer) r2, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	if ((strcmp((char *)(Integer) field(mktag(1), (Integer) r1, ((Integer) 1)), (char *)(Integer) field(mktag(1), (Integer) r2, ((Integer) 1))) !=0))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 2)) != (Integer) field(mktag(1), (Integer) r2, ((Integer) 2))))
		GOTO_LABEL(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___dead_proc_elim__entity_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module19)
	init_entry(mercury____Index___dead_proc_elim__entity_0_0);
	init_label(mercury____Index___dead_proc_elim__entity_0_0_i3);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___dead_proc_elim__entity_0_0);
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___dead_proc_elim__entity_0_0_i3);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___dead_proc_elim__entity_0_0_i3);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module20)
	init_entry(mercury____Compare___dead_proc_elim__entity_0_0);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i2);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i3);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i4);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i6);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i15);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i16);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i14);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i11);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i24);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i30);
	init_label(mercury____Compare___dead_proc_elim__entity_0_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___dead_proc_elim__entity_0_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury____Index___dead_proc_elim__entity_0_0),
		mercury____Compare___dead_proc_elim__entity_0_0_i2,
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___dead_proc_elim__entity_0_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury____Index___dead_proc_elim__entity_0_0),
		mercury____Compare___dead_proc_elim__entity_0_0_i3,
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___dead_proc_elim__entity_0_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i6);
	if ((tag((Integer) detstackvar(1)) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i11);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i9);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(1);
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___dead_proc_elim__entity_0_0_i15,
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
	}
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i15);
	update_prof_current_proc(LABEL(mercury____Compare___dead_proc_elim__entity_0_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i14);
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i16);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i14);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i11);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i9);
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 2));
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(1);
	r1 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 2));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	call_localret(ENTRY(mercury__builtin_compare_string_3_0),
		mercury____Compare___dead_proc_elim__entity_0_0_i24,
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
	}
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i24);
	update_prof_current_proc(LABEL(mercury____Compare___dead_proc_elim__entity_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i16);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	call_localret(ENTRY(mercury__builtin_compare_string_3_0),
		mercury____Compare___dead_proc_elim__entity_0_0_i30,
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i30);
	update_prof_current_proc(LABEL(mercury____Compare___dead_proc_elim__entity_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___dead_proc_elim__entity_0_0_i16);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
Define_label(mercury____Compare___dead_proc_elim__entity_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___dead_proc_elim__entity_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module21)
	init_entry(mercury____Unify___dead_proc_elim__needed_map_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___dead_proc_elim__needed_map_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___dead_proc_elim__needed_map_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module22)
	init_entry(mercury____Index___dead_proc_elim__needed_map_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___dead_proc_elim__needed_map_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	{
	Declare_entry(mercury____Index___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Index___tree234__tree234_2_0),
		ENTRY(mercury____Index___dead_proc_elim__needed_map_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dead_proc_elim_module23)
	init_entry(mercury____Compare___dead_proc_elim__needed_map_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___dead_proc_elim__needed_map_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_dead_proc_elim__base_type_info_entity_0;
	r4 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_dead_proc_elim__common_0);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___dead_proc_elim__needed_map_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__dead_proc_elim_bunch_0(void)
{
	mercury__dead_proc_elim_module0();
	mercury__dead_proc_elim_module1();
	mercury__dead_proc_elim_module2();
	mercury__dead_proc_elim_module3();
	mercury__dead_proc_elim_module4();
	mercury__dead_proc_elim_module5();
	mercury__dead_proc_elim_module6();
	mercury__dead_proc_elim_module7();
	mercury__dead_proc_elim_module8();
	mercury__dead_proc_elim_module9();
	mercury__dead_proc_elim_module10();
	mercury__dead_proc_elim_module11();
	mercury__dead_proc_elim_module12();
	mercury__dead_proc_elim_module13();
	mercury__dead_proc_elim_module14();
	mercury__dead_proc_elim_module15();
	mercury__dead_proc_elim_module16();
	mercury__dead_proc_elim_module17();
	mercury__dead_proc_elim_module18();
	mercury__dead_proc_elim_module19();
	mercury__dead_proc_elim_module20();
	mercury__dead_proc_elim_module21();
	mercury__dead_proc_elim_module22();
	mercury__dead_proc_elim_module23();
}

#endif

void mercury__dead_proc_elim__init(void); /* suppress gcc warning */
void mercury__dead_proc_elim__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__dead_proc_elim_bunch_0();
#endif
}
