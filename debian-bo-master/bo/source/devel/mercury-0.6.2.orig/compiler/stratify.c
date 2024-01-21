/*
** Automatically generated from `stratify.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__stratify__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__stratify__check_stratification_4_0);
Declare_label(mercury__stratify__check_stratification_4_0_i2);
Declare_label(mercury__stratify__check_stratification_4_0_i3);
Declare_label(mercury__stratify__check_stratification_4_0_i4);
Declare_label(mercury__stratify__check_stratification_4_0_i5);
Declare_label(mercury__stratify__check_stratification_4_0_i6);
Declare_label(mercury__stratify__check_stratification_4_0_i7);
Declare_label(mercury__stratify__check_stratification_4_0_i8);
Declare_static(mercury__stratify__dep_sets_to_lists_and_sets_3_0);
Declare_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i4);
Declare_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i5);
Declare_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i6);
Declare_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i1003);
Declare_static(mercury__stratify__get_proc_id_2_0);
Declare_static(mercury__stratify__first_order_check_sccs_7_0);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i6);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i7);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i5);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i9);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i12);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i17);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i10);
Declare_label(mercury__stratify__first_order_check_sccs_7_0_i1004);
Declare_static(mercury__stratify__first_order_check_scc_2_7_0);
Declare_label(mercury__stratify__first_order_check_scc_2_7_0_i4);
Declare_label(mercury__stratify__first_order_check_scc_2_7_0_i5);
Declare_label(mercury__stratify__first_order_check_scc_2_7_0_i6);
Declare_label(mercury__stratify__first_order_check_scc_2_7_0_i7);
Declare_label(mercury__stratify__first_order_check_scc_2_7_0_i8);
Declare_label(mercury__stratify__first_order_check_scc_2_7_0_i1002);
Declare_static(mercury__stratify__first_order_check_goal_10_0);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1025);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1024);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1013);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i15);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i16);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i18);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i22);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i24);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i20);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i19);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1023);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i27);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i33);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i35);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i31);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i30);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i41);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i45);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i47);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i38);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i29);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i51);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1017);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1019);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1020);
Declare_label(mercury__stratify__first_order_check_goal_10_0_i1021);
Declare_static(mercury__stratify__first_order_check_goal_list_9_0);
Declare_label(mercury__stratify__first_order_check_goal_list_9_0_i4);
Declare_label(mercury__stratify__first_order_check_goal_list_9_0_i1002);
Declare_static(mercury__stratify__first_order_check_case_list_9_0);
Declare_label(mercury__stratify__first_order_check_case_list_9_0_i4);
Declare_label(mercury__stratify__first_order_check_case_list_9_0_i1002);
Declare_static(mercury__stratify__local_proc_2_0);
Declare_label(mercury__stratify__local_proc_2_0_i2);
Declare_label(mercury__stratify__local_proc_2_0_i3);
Declare_static(mercury__stratify__emit_message_8_0);
Declare_label(mercury__stratify__emit_message_8_0_i2);
Declare_label(mercury__stratify__emit_message_8_0_i3);
Declare_label(mercury__stratify__emit_message_8_0_i7);
Declare_label(mercury__stratify__emit_message_8_0_i4);
Declare_label(mercury__stratify__emit_message_8_0_i8);
Declare_label(mercury__stratify__emit_message_8_0_i9);
Declare_label(mercury__stratify__emit_message_8_0_i10);
Declare_label(mercury__stratify__emit_message_8_0_i11);
Declare_label(mercury__stratify__emit_message_8_0_i12);
Declare_label(mercury__stratify__emit_message_8_0_i13);
Declare_label(mercury__stratify__emit_message_8_0_i14);
Declare_label(mercury__stratify__emit_message_8_0_i18);
Declare_label(mercury__stratify__emit_message_8_0_i19);
Declare_label(mercury__stratify__emit_message_8_0_i20);
Declare_label(mercury__stratify__emit_message_8_0_i15);

Word * mercury_data_stratify__common_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) STATIC(mercury__stratify__get_proc_id_2_0)
};

BEGIN_MODULE(mercury__stratify_module0)
	init_entry(mercury__stratify__check_stratification_4_0);
	init_label(mercury__stratify__check_stratification_4_0_i2);
	init_label(mercury__stratify__check_stratification_4_0_i3);
	init_label(mercury__stratify__check_stratification_4_0_i4);
	init_label(mercury__stratify__check_stratification_4_0_i5);
	init_label(mercury__stratify__check_stratification_4_0_i6);
	init_label(mercury__stratify__check_stratification_4_0_i7);
	init_label(mercury__stratify__check_stratification_4_0_i8);
BEGIN_CODE

/* code for predicate 'stratify__check_stratification'/4 in mode 0 */
Define_entry(mercury__stratify__check_stratification_4_0);
	incr_sp_push_msg(5, "stratify__check_stratification");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__dependency_graph__module_info_ensure_dependency_info_2_0);
	call_localret(ENTRY(mercury__dependency_graph__module_info_ensure_dependency_info_2_0),
		mercury__stratify__check_stratification_4_0_i2,
		ENTRY(mercury__stratify__check_stratification_4_0));
	}
Define_label(mercury__stratify__check_stratification_4_0_i2);
	update_prof_current_proc(LABEL(mercury__stratify__check_stratification_4_0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_module__module_info_dependency_info_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_dependency_info_2_0),
		mercury__stratify__check_stratification_4_0_i3,
		ENTRY(mercury__stratify__check_stratification_4_0));
	}
Define_label(mercury__stratify__check_stratification_4_0_i3);
	update_prof_current_proc(LABEL(mercury__stratify__check_stratification_4_0));
	{
	Declare_entry(mercury__hlds_module__hlds__dependency_info_get_dependency_graph_2_0);
	call_localret(ENTRY(mercury__hlds_module__hlds__dependency_info_get_dependency_graph_2_0),
		mercury__stratify__check_stratification_4_0_i4,
		ENTRY(mercury__stratify__check_stratification_4_0));
	}
Define_label(mercury__stratify__check_stratification_4_0_i4);
	update_prof_current_proc(LABEL(mercury__stratify__check_stratification_4_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_proc_id_0[];
	r1 = (Integer) mercury_data_hlds_pred__base_type_info_pred_proc_id_0;
	}
	{
	Declare_entry(mercury__relation__atsort_2_0);
	call_localret(ENTRY(mercury__relation__atsort_2_0),
		mercury__stratify__check_stratification_4_0_i5,
		ENTRY(mercury__stratify__check_stratification_4_0));
	}
Define_label(mercury__stratify__check_stratification_4_0_i5);
	update_prof_current_proc(LABEL(mercury__stratify__check_stratification_4_0));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__stratify__dep_sets_to_lists_and_sets_3_0),
		mercury__stratify__check_stratification_4_0_i6,
		ENTRY(mercury__stratify__check_stratification_4_0));
Define_label(mercury__stratify__check_stratification_4_0_i6);
	update_prof_current_proc(LABEL(mercury__stratify__check_stratification_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = ((Integer) 10);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__stratify__check_stratification_4_0_i7,
		ENTRY(mercury__stratify__check_stratification_4_0));
	}
Define_label(mercury__stratify__check_stratification_4_0_i7);
	update_prof_current_proc(LABEL(mercury__stratify__check_stratification_4_0));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_module__module_info_stratified_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_stratified_preds_2_0),
		mercury__stratify__check_stratification_4_0_i8,
		ENTRY(mercury__stratify__check_stratification_4_0));
	}
Define_label(mercury__stratify__check_stratification_4_0_i8);
	update_prof_current_proc(LABEL(mercury__stratify__check_stratification_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__stratify__first_order_check_sccs_7_0),
		ENTRY(mercury__stratify__check_stratification_4_0));
END_MODULE

BEGIN_MODULE(mercury__stratify_module1)
	init_entry(mercury__stratify__dep_sets_to_lists_and_sets_3_0);
	init_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i4);
	init_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i5);
	init_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i6);
	init_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i1003);
BEGIN_CODE

/* code for predicate 'dep_sets_to_lists_and_sets'/3 in mode 0 */
Define_static(mercury__stratify__dep_sets_to_lists_and_sets_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i1003);
	incr_sp_push_msg(4, "dep_sets_to_lists_and_sets");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_proc_id_0[];
	r1 = (Integer) mercury_data_hlds_pred__base_type_info_pred_proc_id_0;
	}
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__stratify__dep_sets_to_lists_and_sets_3_0_i4,
		STATIC(mercury__stratify__dep_sets_to_lists_and_sets_3_0));
	}
Define_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i4);
	update_prof_current_proc(LABEL(mercury__stratify__dep_sets_to_lists_and_sets_3_0));
	r4 = (Integer) r1;
	detstackvar(3) = (Integer) r1;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_proc_id_0[];
	r1 = (Integer) mercury_data_hlds_pred__base_type_info_pred_proc_id_0;
	}
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r2 = (Integer) mercury_data___base_type_info_int_0;
	}
	r3 = (Integer) mkword(mktag(0), (Integer) mercury_data_stratify__common_0);
	{
	Declare_entry(mercury__list__map_3_0);
	call_localret(ENTRY(mercury__list__map_3_0),
		mercury__stratify__dep_sets_to_lists_and_sets_3_0_i5,
		STATIC(mercury__stratify__dep_sets_to_lists_and_sets_3_0));
	}
Define_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i5);
	update_prof_current_proc(LABEL(mercury__stratify__dep_sets_to_lists_and_sets_3_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	call_localret(ENTRY(mercury__set__list_to_set_2_0),
		mercury__stratify__dep_sets_to_lists_and_sets_3_0_i6,
		STATIC(mercury__stratify__dep_sets_to_lists_and_sets_3_0));
	}
Define_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i6);
	update_prof_current_proc(LABEL(mercury__stratify__dep_sets_to_lists_and_sets_3_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__stratify__dep_sets_to_lists_and_sets_3_0,
		STATIC(mercury__stratify__dep_sets_to_lists_and_sets_3_0));
	}
Define_label(mercury__stratify__dep_sets_to_lists_and_sets_3_0_i1003);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stratify_module2)
	init_entry(mercury__stratify__get_proc_id_2_0);
BEGIN_CODE

/* code for predicate 'get_proc_id'/2 in mode 0 */
Define_static(mercury__stratify__get_proc_id_2_0);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stratify_module3)
	init_entry(mercury__stratify__first_order_check_sccs_7_0);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i6);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i7);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i5);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i9);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i12);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i17);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i10);
	init_label(mercury__stratify__first_order_check_sccs_7_0_i1004);
BEGIN_CODE

/* code for predicate 'first_order_check_sccs'/7 in mode 0 */
Define_static(mercury__stratify__first_order_check_sccs_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stratify__first_order_check_sccs_7_0_i1004);
	incr_sp_push_msg(7, "first_order_check_sccs");
	detstackvar(7) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__set__intersect_3_0);
	call_localret(ENTRY(mercury__set__intersect_3_0),
		mercury__stratify__first_order_check_sccs_7_0_i6,
		STATIC(mercury__stratify__first_order_check_sccs_7_0));
	}
	}
Define_label(mercury__stratify__first_order_check_sccs_7_0_i6);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_sccs_7_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	Declare_entry(mercury__set__empty_1_0);
	call_localret(ENTRY(mercury__set__empty_1_0),
		mercury__stratify__first_order_check_sccs_7_0_i7,
		STATIC(mercury__stratify__first_order_check_sccs_7_0));
	}
Define_label(mercury__stratify__first_order_check_sccs_7_0_i7);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_sccs_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__stratify__first_order_check_sccs_7_0_i5);
	r1 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r3 = ((Integer) 1);
	GOTO_LABEL(mercury__stratify__first_order_check_sccs_7_0_i9);
Define_label(mercury__stratify__first_order_check_sccs_7_0_i5);
	r1 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r3 = ((Integer) 0);
Define_label(mercury__stratify__first_order_check_sccs_7_0_i9);
	if (((Integer) r3 == ((Integer) 0)))
		GOTO_LABEL(mercury__stratify__first_order_check_sccs_7_0_i12);
	if (((Integer) r6 != ((Integer) 0)))
		GOTO_LABEL(mercury__stratify__first_order_check_sccs_7_0_i10);
Define_label(mercury__stratify__first_order_check_sccs_7_0_i12);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__stratify__first_order_check_scc_2_7_0),
		mercury__stratify__first_order_check_sccs_7_0_i17,
		STATIC(mercury__stratify__first_order_check_sccs_7_0));
Define_label(mercury__stratify__first_order_check_sccs_7_0_i17);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_sccs_7_0));
	r4 = (Integer) r1;
	r5 = (Integer) r2;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__stratify__first_order_check_sccs_7_0,
		STATIC(mercury__stratify__first_order_check_sccs_7_0));
Define_label(mercury__stratify__first_order_check_sccs_7_0_i10);
	r2 = (Integer) r1;
	r3 = (Integer) r6;
	r1 = (Integer) r7;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	localtailcall(mercury__stratify__first_order_check_sccs_7_0,
		STATIC(mercury__stratify__first_order_check_sccs_7_0));
Define_label(mercury__stratify__first_order_check_sccs_7_0_i1004);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stratify_module4)
	init_entry(mercury__stratify__first_order_check_scc_2_7_0);
	init_label(mercury__stratify__first_order_check_scc_2_7_0_i4);
	init_label(mercury__stratify__first_order_check_scc_2_7_0_i5);
	init_label(mercury__stratify__first_order_check_scc_2_7_0_i6);
	init_label(mercury__stratify__first_order_check_scc_2_7_0_i7);
	init_label(mercury__stratify__first_order_check_scc_2_7_0_i8);
	init_label(mercury__stratify__first_order_check_scc_2_7_0_i1002);
BEGIN_CODE

/* code for predicate 'first_order_check_scc_2'/7 in mode 0 */
Define_static(mercury__stratify__first_order_check_scc_2_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stratify__first_order_check_scc_2_7_0_i1002);
	incr_sp_push_msg(8, "first_order_check_scc_2");
	detstackvar(8) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) tempr1;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(7) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) r4;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__hlds_module__module_info_pred_info_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_pred_info_3_0),
		mercury__stratify__first_order_check_scc_2_7_0_i4,
		STATIC(mercury__stratify__first_order_check_scc_2_7_0));
	}
	}
Define_label(mercury__stratify__first_order_check_scc_2_7_0_i4);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_scc_2_7_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__stratify__first_order_check_scc_2_7_0_i5,
		STATIC(mercury__stratify__first_order_check_scc_2_7_0));
	}
Define_label(mercury__stratify__first_order_check_scc_2_7_0_i5);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_scc_2_7_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r4 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__stratify__first_order_check_scc_2_7_0_i6,
		STATIC(mercury__stratify__first_order_check_scc_2_7_0));
	}
Define_label(mercury__stratify__first_order_check_scc_2_7_0_i6);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_scc_2_7_0));
	{
	Declare_entry(mercury__hlds_pred__proc_info_goal_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_goal_2_0),
		mercury__stratify__first_order_check_scc_2_7_0_i7,
		STATIC(mercury__stratify__first_order_check_scc_2_7_0));
	}
Define_label(mercury__stratify__first_order_check_scc_2_7_0_i7);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_scc_2_7_0));
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r3 = ((Integer) 1);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(3);
	r8 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__stratify__first_order_check_goal_10_0),
		mercury__stratify__first_order_check_scc_2_7_0_i8,
		STATIC(mercury__stratify__first_order_check_scc_2_7_0));
Define_label(mercury__stratify__first_order_check_scc_2_7_0_i8);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_scc_2_7_0));
	r4 = (Integer) r1;
	r5 = (Integer) r2;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__stratify__first_order_check_scc_2_7_0,
		STATIC(mercury__stratify__first_order_check_scc_2_7_0));
Define_label(mercury__stratify__first_order_check_scc_2_7_0_i1002);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stratify_module5)
	init_entry(mercury__stratify__first_order_check_goal_10_0);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1025);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1024);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1013);
	init_label(mercury__stratify__first_order_check_goal_10_0_i15);
	init_label(mercury__stratify__first_order_check_goal_10_0_i16);
	init_label(mercury__stratify__first_order_check_goal_10_0_i18);
	init_label(mercury__stratify__first_order_check_goal_10_0_i22);
	init_label(mercury__stratify__first_order_check_goal_10_0_i24);
	init_label(mercury__stratify__first_order_check_goal_10_0_i20);
	init_label(mercury__stratify__first_order_check_goal_10_0_i19);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1023);
	init_label(mercury__stratify__first_order_check_goal_10_0_i27);
	init_label(mercury__stratify__first_order_check_goal_10_0_i33);
	init_label(mercury__stratify__first_order_check_goal_10_0_i35);
	init_label(mercury__stratify__first_order_check_goal_10_0_i31);
	init_label(mercury__stratify__first_order_check_goal_10_0_i30);
	init_label(mercury__stratify__first_order_check_goal_10_0_i41);
	init_label(mercury__stratify__first_order_check_goal_10_0_i45);
	init_label(mercury__stratify__first_order_check_goal_10_0_i47);
	init_label(mercury__stratify__first_order_check_goal_10_0_i38);
	init_label(mercury__stratify__first_order_check_goal_10_0_i29);
	init_label(mercury__stratify__first_order_check_goal_10_0_i51);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1017);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1019);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1020);
	init_label(mercury__stratify__first_order_check_goal_10_0_i1021);
BEGIN_CODE

/* code for predicate 'first_order_check_goal'/10 in mode 0 */
Define_static(mercury__stratify__first_order_check_goal_10_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i1023);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__stratify__first_order_check_goal_10_0_i1017) AND
		LABEL(mercury__stratify__first_order_check_goal_10_0_i1013) AND
		LABEL(mercury__stratify__first_order_check_goal_10_0_i1019) AND
		LABEL(mercury__stratify__first_order_check_goal_10_0_i1020) AND
		LABEL(mercury__stratify__first_order_check_goal_10_0_i1021) AND
		LABEL(mercury__stratify__first_order_check_goal_10_0_i1025) AND
		LABEL(mercury__stratify__first_order_check_goal_10_0_i1024));
Define_label(mercury__stratify__first_order_check_goal_10_0_i1025);
	incr_sp_push_msg(9, "first_order_check_goal");
	detstackvar(9) = (Integer) succip;
	{
	Word tempr1, tempr2, tempr3;
	tempr1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	tempr2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	tempr3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(1) = (Integer) r3;
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) tempr3, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) tempr3, ((Integer) 0));
	r3 = ((Integer) 0);
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	localcall(mercury__stratify__first_order_check_goal_10_0,
		LABEL(mercury__stratify__first_order_check_goal_10_0_i15),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i1024);
	incr_sp_push_msg(9, "first_order_check_goal");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i18);
Define_label(mercury__stratify__first_order_check_goal_10_0_i1013);
	r1 = (Integer) r7;
	r2 = (Integer) r8;
	proceed();
Define_label(mercury__stratify__first_order_check_goal_10_0_i15);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	r7 = (Integer) r1;
	r8 = (Integer) r2;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	localcall(mercury__stratify__first_order_check_goal_10_0,
		LABEL(mercury__stratify__first_order_check_goal_10_0_i16),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i16);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	r7 = (Integer) r1;
	r8 = (Integer) r2;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__stratify__first_order_check_goal_10_0,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i18);
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i19);
	detstackvar(1) = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_proc_id_0[];
	r1 = (Integer) mercury_data_hlds_pred__base_type_info_pred_proc_id_0;
	}
	r3 = (Integer) r4;
	detstackvar(2) = (Integer) r7;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r8;
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__stratify__first_order_check_goal_10_0_i22,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i22);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i20);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_context_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_context_2_0),
		mercury__stratify__first_order_check_goal_10_0_i24,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i24);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = string_const("call introduces a non-stratified loop", 37);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__stratify__emit_message_8_0),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i20);
	r7 = (Integer) detstackvar(2);
	r8 = (Integer) detstackvar(5);
Define_label(mercury__stratify__first_order_check_goal_10_0_i19);
	r1 = (Integer) r7;
	r2 = (Integer) r8;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__stratify__first_order_check_goal_10_0_i1023);
	incr_sp_push_msg(9, "first_order_check_goal");
	detstackvar(9) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i27);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	r5 = (Integer) r6;
	r6 = (Integer) r7;
	r7 = (Integer) r8;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__stratify__first_order_check_goal_list_9_0),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i27);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i29);
	tag_incr_hp(r9, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r9, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	field(mktag(0), (Integer) r9, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r10 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i30);
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	detstackvar(2) = (Integer) r7;
	detstackvar(5) = (Integer) r8;
	detstackvar(6) = (Integer) r10;
	detstackvar(7) = (Integer) r9;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_proc_id_0[];
	r1 = (Integer) mercury_data_hlds_pred__base_type_info_pred_proc_id_0;
	}
	r2 = (Integer) r9;
	r3 = (Integer) r4;
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__stratify__first_order_check_goal_10_0_i33,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i33);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i31);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_context_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_context_2_0),
		mercury__stratify__first_order_check_goal_10_0_i35,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i35);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = string_const("call introduces a non-stratified loop", 37);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__stratify__emit_message_8_0),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i31);
	r2 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(2);
	r8 = (Integer) detstackvar(5);
	r10 = (Integer) detstackvar(6);
	r9 = (Integer) detstackvar(7);
Define_label(mercury__stratify__first_order_check_goal_10_0_i30);
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	detstackvar(2) = (Integer) r7;
	detstackvar(5) = (Integer) r8;
	detstackvar(7) = (Integer) r9;
	r1 = (Integer) r10;
	{
	Declare_entry(mercury__hlds_goal__hlds__is_builtin_is_internal_1_0);
	call_localret(ENTRY(mercury__hlds_goal__hlds__is_builtin_is_internal_1_0),
		mercury__stratify__first_order_check_goal_10_0_i41,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i41);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i38);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__stratify__local_proc_2_0),
		mercury__stratify__first_order_check_goal_10_0_i45,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i45);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__stratify__first_order_check_goal_10_0_i38);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_context_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_context_2_0),
		mercury__stratify__first_order_check_goal_10_0_i47,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i47);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = string_const("call to non-local predicate may introduce a non-stratified loop", 63);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__stratify__emit_message_8_0),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i38);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__stratify__first_order_check_goal_10_0_i29);
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	detstackvar(2) = (Integer) r7;
	detstackvar(5) = (Integer) r8;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_context_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_context_2_0),
		mercury__stratify__first_order_check_goal_10_0_i51,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i51);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = string_const("higher order call may introduce a non-stratified loop", 53);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__stratify__emit_message_8_0),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i1017);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	r5 = (Integer) r6;
	r6 = (Integer) r7;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r7 = (Integer) r8;
	tailcall(STATIC(mercury__stratify__first_order_check_case_list_9_0),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i1019);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	r5 = (Integer) r6;
	r6 = (Integer) r7;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r7 = (Integer) r8;
	tailcall(STATIC(mercury__stratify__first_order_check_goal_list_9_0),
		STATIC(mercury__stratify__first_order_check_goal_10_0));
Define_label(mercury__stratify__first_order_check_goal_10_0_i1020);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r3 = ((Integer) 0);
	localtailcall(mercury__stratify__first_order_check_goal_10_0,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
Define_label(mercury__stratify__first_order_check_goal_10_0_i1021);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	localtailcall(mercury__stratify__first_order_check_goal_10_0,
		STATIC(mercury__stratify__first_order_check_goal_10_0));
	}
END_MODULE

BEGIN_MODULE(mercury__stratify_module6)
	init_entry(mercury__stratify__first_order_check_goal_list_9_0);
	init_label(mercury__stratify__first_order_check_goal_list_9_0_i4);
	init_label(mercury__stratify__first_order_check_goal_list_9_0_i1002);
BEGIN_CODE

/* code for predicate 'first_order_check_goal_list'/9 in mode 0 */
Define_static(mercury__stratify__first_order_check_goal_list_9_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stratify__first_order_check_goal_list_9_0_i1002);
	incr_sp_push_msg(6, "first_order_check_goal_list");
	detstackvar(6) = (Integer) succip;
	r8 = (Integer) r7;
	r7 = (Integer) r6;
	r6 = (Integer) r5;
	detstackvar(4) = (Integer) r5;
	r5 = (Integer) r4;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) r3;
	detstackvar(2) = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	call_localret(STATIC(mercury__stratify__first_order_check_goal_10_0),
		mercury__stratify__first_order_check_goal_list_9_0_i4,
		STATIC(mercury__stratify__first_order_check_goal_list_9_0));
	}
Define_label(mercury__stratify__first_order_check_goal_list_9_0_i4);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_goal_list_9_0));
	r6 = (Integer) r1;
	r7 = (Integer) r2;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__stratify__first_order_check_goal_list_9_0,
		STATIC(mercury__stratify__first_order_check_goal_list_9_0));
Define_label(mercury__stratify__first_order_check_goal_list_9_0_i1002);
	r1 = (Integer) r6;
	r2 = (Integer) r7;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stratify_module7)
	init_entry(mercury__stratify__first_order_check_case_list_9_0);
	init_label(mercury__stratify__first_order_check_case_list_9_0_i4);
	init_label(mercury__stratify__first_order_check_case_list_9_0_i1002);
BEGIN_CODE

/* code for predicate 'first_order_check_case_list'/9 in mode 0 */
Define_static(mercury__stratify__first_order_check_case_list_9_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stratify__first_order_check_case_list_9_0_i1002);
	incr_sp_push_msg(6, "first_order_check_case_list");
	detstackvar(6) = (Integer) succip;
	r8 = (Integer) r7;
	r7 = (Integer) r6;
	r6 = (Integer) r5;
	detstackvar(4) = (Integer) r5;
	r5 = (Integer) r4;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) r3;
	detstackvar(2) = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	call_localret(STATIC(mercury__stratify__first_order_check_goal_10_0),
		mercury__stratify__first_order_check_case_list_9_0_i4,
		STATIC(mercury__stratify__first_order_check_case_list_9_0));
	}
Define_label(mercury__stratify__first_order_check_case_list_9_0_i4);
	update_prof_current_proc(LABEL(mercury__stratify__first_order_check_case_list_9_0));
	r6 = (Integer) r1;
	r7 = (Integer) r2;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__stratify__first_order_check_case_list_9_0,
		STATIC(mercury__stratify__first_order_check_case_list_9_0));
Define_label(mercury__stratify__first_order_check_case_list_9_0_i1002);
	r1 = (Integer) r6;
	r2 = (Integer) r7;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stratify_module8)
	init_entry(mercury__stratify__local_proc_2_0);
	init_label(mercury__stratify__local_proc_2_0_i2);
	init_label(mercury__stratify__local_proc_2_0_i3);
BEGIN_CODE

/* code for predicate 'local_proc'/2 in mode 0 */
Define_static(mercury__stratify__local_proc_2_0);
	incr_sp_push_msg(2, "local_proc");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__hlds_module__module_info_pred_info_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_pred_info_3_0),
		mercury__stratify__local_proc_2_0_i2,
		STATIC(mercury__stratify__local_proc_2_0));
	}
Define_label(mercury__stratify__local_proc_2_0_i2);
	update_prof_current_proc(LABEL(mercury__stratify__local_proc_2_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_non_imported_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_non_imported_procids_2_0),
		mercury__stratify__local_proc_2_0_i3,
		STATIC(mercury__stratify__local_proc_2_0));
	}
Define_label(mercury__stratify__local_proc_2_0_i3);
	update_prof_current_proc(LABEL(mercury__stratify__local_proc_2_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__member_2_0);
	tailcall(ENTRY(mercury__list__member_2_0),
		STATIC(mercury__stratify__local_proc_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__stratify_module9)
	init_entry(mercury__stratify__emit_message_8_0);
	init_label(mercury__stratify__emit_message_8_0_i2);
	init_label(mercury__stratify__emit_message_8_0_i3);
	init_label(mercury__stratify__emit_message_8_0_i7);
	init_label(mercury__stratify__emit_message_8_0_i4);
	init_label(mercury__stratify__emit_message_8_0_i8);
	init_label(mercury__stratify__emit_message_8_0_i9);
	init_label(mercury__stratify__emit_message_8_0_i10);
	init_label(mercury__stratify__emit_message_8_0_i11);
	init_label(mercury__stratify__emit_message_8_0_i12);
	init_label(mercury__stratify__emit_message_8_0_i13);
	init_label(mercury__stratify__emit_message_8_0_i14);
	init_label(mercury__stratify__emit_message_8_0_i18);
	init_label(mercury__stratify__emit_message_8_0_i19);
	init_label(mercury__stratify__emit_message_8_0_i20);
	init_label(mercury__stratify__emit_message_8_0_i15);
BEGIN_CODE

/* code for predicate 'emit_message'/8 in mode 0 */
Define_static(mercury__stratify__emit_message_8_0);
	incr_sp_push_msg(5, "emit_message");
	detstackvar(5) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(1) = (Integer) r2;
	tag_incr_hp(r4, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r5;
	detstackvar(4) = (Integer) r5;
	r5 = (Integer) r6;
	{
	Declare_entry(mercury__passes_aux__report_pred_proc_id_7_0);
	call_localret(ENTRY(mercury__passes_aux__report_pred_proc_id_7_0),
		mercury__stratify__emit_message_8_0_i2,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i2);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__prog_out__write_context_3_0);
	call_localret(ENTRY(mercury__prog_out__write_context_3_0),
		mercury__stratify__emit_message_8_0_i3,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i3);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	if (((Integer) detstackvar(3) != ((Integer) 1)))
		GOTO_LABEL(mercury__stratify__emit_message_8_0_i4);
	detstackvar(1) = (Integer) detstackvar(4);
	r2 = (Integer) r1;
	r1 = string_const("  warning: ", 11);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__stratify__emit_message_8_0_i7,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i7);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__stratify__emit_message_8_0_i11);
Define_label(mercury__stratify__emit_message_8_0_i4);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__hlds_module__module_info_incr_errors_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_incr_errors_2_0),
		mercury__stratify__emit_message_8_0_i8,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i8);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = ((Integer) 1);
	{
	Declare_entry(mercury__io__set_exit_status_3_0);
	call_localret(ENTRY(mercury__io__set_exit_status_3_0),
		mercury__stratify__emit_message_8_0_i9,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i9);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = string_const("  error: ", 9);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__stratify__emit_message_8_0_i10,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i10);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
Define_label(mercury__stratify__emit_message_8_0_i11);
	detstackvar(1) = (Integer) r3;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__stratify__emit_message_8_0_i12,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i12);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 10);
	{
	Declare_entry(mercury__io__write_char_3_0);
	call_localret(ENTRY(mercury__io__write_char_3_0),
		mercury__stratify__emit_message_8_0_i13,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i13);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 13);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__stratify__emit_message_8_0_i14,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i14);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__stratify__emit_message_8_0_i15);
	r1 = string_const("\tA non-stratified loop is a loop in the call graph of the given\n", 64);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__stratify__emit_message_8_0_i18,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i18);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = string_const("\tpredicate/function that allows it to call itself negatively. This\n", 67);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__stratify__emit_message_8_0_i19,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i19);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = string_const("\tcan cause problems for bottom up evaluation of the predicate/function.\n", 72);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__stratify__emit_message_8_0_i20,
		STATIC(mercury__stratify__emit_message_8_0));
	}
Define_label(mercury__stratify__emit_message_8_0_i20);
	update_prof_current_proc(LABEL(mercury__stratify__emit_message_8_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__stratify__emit_message_8_0_i15);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__stratify_bunch_0(void)
{
	mercury__stratify_module0();
	mercury__stratify_module1();
	mercury__stratify_module2();
	mercury__stratify_module3();
	mercury__stratify_module4();
	mercury__stratify_module5();
	mercury__stratify_module6();
	mercury__stratify_module7();
	mercury__stratify_module8();
	mercury__stratify_module9();
}

#endif

void mercury__stratify__init(void); /* suppress gcc warning */
void mercury__stratify__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__stratify_bunch_0();
#endif
}
