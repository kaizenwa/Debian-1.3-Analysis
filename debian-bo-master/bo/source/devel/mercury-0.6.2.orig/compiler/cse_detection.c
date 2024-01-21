/*
** Automatically generated from `cse_detection.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__cse_detection__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__cse_detection__detect_cse_4_0);
Declare_label(mercury__cse_detection__detect_cse_4_0_i2);
Define_extern_entry(mercury__cse_detection__detect_cse_in_proc_6_0);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i2);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i3);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i4);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i7);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i8);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i9);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i10);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i12);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i13);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i14);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i15);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i16);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i17);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i18);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i19);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i20);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i21);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i11);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i25);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i29);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i30);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i31);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i26);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i32);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i33);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i36);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i34);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i37);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i41);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i42);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i43);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i38);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i44);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i45);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i49);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i50);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i51);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i46);
Declare_label(mercury__cse_detection__detect_cse_in_proc_6_0_i54);
Declare_static(mercury__cse_detection__detect_cse_in_preds_5_0);
Declare_label(mercury__cse_detection__detect_cse_in_preds_5_0_i4);
Declare_label(mercury__cse_detection__detect_cse_in_preds_5_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_preds_5_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_preds_5_0_i7);
Declare_label(mercury__cse_detection__detect_cse_in_preds_5_0_i1002);
Declare_static(mercury__cse_detection__detect_cse_in_procs_6_0);
Declare_label(mercury__cse_detection__detect_cse_in_procs_6_0_i4);
Declare_label(mercury__cse_detection__detect_cse_in_procs_6_0_i1002);
Declare_static(mercury__cse_detection__detect_cse_in_goal_6_0);
Declare_static(mercury__cse_detection__detect_cse_in_goal_1_7_0);
Declare_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i2);
Declare_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i3);
Declare_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i4);
Declare_static(mercury__cse_detection__detect_cse_in_goal_2_7_0);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1028);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1027);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1026);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1025);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1024);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1023);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i7);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i9);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i13);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i10);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i15);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i16);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i19);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i20);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i23);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i24);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i25);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i26);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i27);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i28);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i29);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1019);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1022);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i33);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i32);
Declare_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1021);
Declare_static(mercury__cse_detection__detect_cse_in_conj_6_0);
Declare_label(mercury__cse_detection__detect_cse_in_conj_6_0_i4);
Declare_label(mercury__cse_detection__detect_cse_in_conj_6_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_conj_6_0_i9);
Declare_label(mercury__cse_detection__detect_cse_in_conj_6_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_conj_6_0_i10);
Declare_label(mercury__cse_detection__detect_cse_in_conj_6_0_i11);
Declare_label(mercury__cse_detection__detect_cse_in_conj_6_0_i1003);
Declare_static(mercury__cse_detection__detect_cse_in_disj_9_0);
Declare_label(mercury__cse_detection__detect_cse_in_disj_9_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_disj_9_0_i7);
Declare_label(mercury__cse_detection__detect_cse_in_disj_9_0_i9);
Declare_label(mercury__cse_detection__detect_cse_in_disj_9_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_disj_9_0_i3);
Declare_label(mercury__cse_detection__detect_cse_in_disj_9_0_i14);
Declare_static(mercury__cse_detection__detect_cse_in_disj_2_6_0);
Declare_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i4);
Declare_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i1002);
Declare_static(mercury__cse_detection__detect_cse_in_cases_11_0);
Declare_label(mercury__cse_detection__detect_cse_in_cases_11_0_i8);
Declare_label(mercury__cse_detection__detect_cse_in_cases_11_0_i10);
Declare_label(mercury__cse_detection__detect_cse_in_cases_11_0_i11);
Declare_label(mercury__cse_detection__detect_cse_in_cases_11_0_i13);
Declare_label(mercury__cse_detection__detect_cse_in_cases_11_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_cases_11_0_i3);
Declare_label(mercury__cse_detection__detect_cse_in_cases_11_0_i18);
Declare_static(mercury__cse_detection__detect_cse_in_cases_2_6_0);
Declare_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i4);
Declare_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i1003);
Declare_static(mercury__cse_detection__detect_cse_in_ite_12_0);
Declare_label(mercury__cse_detection__detect_cse_in_ite_12_0_i6);
Declare_label(mercury__cse_detection__detect_cse_in_ite_12_0_i7);
Declare_label(mercury__cse_detection__detect_cse_in_ite_12_0_i9);
Declare_label(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_ite_12_0_i3);
Declare_label(mercury__cse_detection__detect_cse_in_ite_12_0_i17);
Declare_static(mercury__cse_detection__detect_cse_in_ite_2_10_0);
Declare_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i2);
Declare_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i3);
Declare_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i4);
Declare_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i5);
Declare_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i6);
Declare_static(mercury__cse_detection__common_deconstruct_2_7_0);
Declare_label(mercury__cse_detection__common_deconstruct_2_7_0_i1001);
Declare_label(mercury__cse_detection__common_deconstruct_2_7_0_i4);
Declare_label(mercury__cse_detection__common_deconstruct_2_7_0_i5);
Declare_label(mercury__cse_detection__common_deconstruct_2_7_0_i6);
Declare_label(mercury__cse_detection__common_deconstruct_2_7_0_i8);
Declare_label(mercury__cse_detection__common_deconstruct_2_7_0_i9);
Declare_label(mercury__cse_detection__common_deconstruct_2_7_0_i1);
Declare_static(mercury__cse_detection__common_deconstruct_cases_2_7_0);
Declare_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1001);
Declare_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i4);
Declare_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i5);
Declare_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i6);
Declare_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i8);
Declare_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i9);
Declare_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1);
Declare_static(mercury__cse_detection__find_bind_var_for_cse_9_0);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i7);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i8);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i11);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i4);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i16);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i21);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i23);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i26);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i27);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i18);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i17);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i32);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i34);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i37);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i38);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i40);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i29);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i28);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i41);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i46);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i45);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i48);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i13);
Declare_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i1001);
Declare_static(mercury__cse_detection__construct_common_unify_8_0);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i1001);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i9);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i10);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i11);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i12);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i13);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i1000);
Declare_label(mercury__cse_detection__construct_common_unify_8_0_i14);
Declare_static(mercury__cse_detection__create_parallel_subterms_9_0);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i4);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i5);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i6);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i7);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i8);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i9);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i3);
Declare_label(mercury__cse_detection__create_parallel_subterms_9_0_i10);
Declare_static(mercury__cse_detection__find_similar_deconstruct_4_0);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i7);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i9);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i10);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i11);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i1003);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i2);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i12);
Declare_label(mercury__cse_detection__find_similar_deconstruct_4_0_i1);
Declare_static(mercury__cse_detection__pair_subterms_5_0);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i6);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i9);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i8);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i11);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i1009);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i1007);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i13);
Declare_label(mercury__cse_detection__pair_subterms_5_0_i1008);

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_cse_detection__base_type_layout_cse_info_0[];
Word * mercury_data_cse_detection__base_type_info_cse_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_cse_detection__base_type_layout_cse_info_0
};

extern Word * mercury_data_cse_detection__common_4[];
Word * mercury_data_cse_detection__base_type_layout_cse_info_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_cse_detection__common_4),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_info_0[];
Word * mercury_data_cse_detection__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_info_0
};

extern Word * mercury_data_varset__base_type_info_varset_0[];
Word * mercury_data_cse_detection__common_1[] = {
	(Word *) (Integer) mercury_data_varset__base_type_info_varset_0
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
extern Word * mercury_data_mercury_builtin__base_type_info_term_0[];
Word * mercury_data_cse_detection__common_2[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_term_0
};

extern Word * mercury_data_hlds_module__base_type_info_module_info_0[];
Word * mercury_data_cse_detection__common_3[] = {
	(Word *) (Integer) mercury_data_hlds_module__base_type_info_module_info_0
};

Word * mercury_data_cse_detection__common_4[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_cse_detection__common_1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_cse_detection__common_2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_cse_detection__common_3),
	(Word *) string_const("cse_info", 8)
};

BEGIN_MODULE(mercury__cse_detection_module0)
	init_entry(mercury__cse_detection__detect_cse_4_0);
	init_label(mercury__cse_detection__detect_cse_4_0_i2);
BEGIN_CODE

/* code for predicate 'detect_cse'/4 in mode 0 */
Define_entry(mercury__cse_detection__detect_cse_4_0);
	incr_sp_push_msg(3, "detect_cse");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_predids_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_predids_2_0),
		mercury__cse_detection__detect_cse_4_0_i2,
		ENTRY(mercury__cse_detection__detect_cse_4_0));
	}
Define_label(mercury__cse_detection__detect_cse_4_0_i2);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_4_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__cse_detection__detect_cse_in_preds_5_0),
		ENTRY(mercury__cse_detection__detect_cse_4_0));
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module1)
	init_entry(mercury__cse_detection__detect_cse_in_proc_6_0);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i2);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i3);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i4);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i7);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i8);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i9);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i10);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i12);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i13);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i14);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i15);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i16);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i17);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i18);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i19);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i20);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i21);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i11);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i25);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i29);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i30);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i31);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i26);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i32);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i33);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i36);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i34);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i37);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i41);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i42);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i43);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i38);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i44);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i45);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i49);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i50);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i51);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i46);
	init_label(mercury__cse_detection__detect_cse_in_proc_6_0_i54);
BEGIN_CODE

/* code for predicate 'detect_cse_in_proc'/6 in mode 0 */
Define_entry(mercury__cse_detection__detect_cse_in_proc_6_0);
	incr_sp_push_msg(13, "detect_cse_in_proc");
	detstackvar(13) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i2,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i2);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r3 = (Integer) r1;
	detstackvar(6) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__cse_detection__detect_cse_in_proc_6_0_i3,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i3);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i4,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r3 = (Integer) r1;
	detstackvar(8) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__cse_detection__detect_cse_in_proc_6_0_i5,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	detstackvar(9) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__proc_info_goal_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_goal_2_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i6,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_pred__proc_info_get_initial_instmap_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_get_initial_instmap_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i7,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_pred__proc_info_variables_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_variables_2_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i8,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i8);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_pred__proc_info_vartypes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_vartypes_2_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i9,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(10);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(11);
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i10,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i10);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	if (((Integer) r2 == ((Integer) 0)))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i12);
	r6 = (Integer) r2;
	r5 = (Integer) detstackvar(3);
	r1 = (Integer) r5;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i11);
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i12);
	detstackvar(5) = (Integer) r2;
	detstackvar(10) = (Integer) r3;
	detstackvar(11) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(12) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_pred__proc_info_headvars_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_headvars_2_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i13,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i13);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) detstackvar(10);
	r3 = (Integer) detstackvar(11);
	r4 = (Integer) detstackvar(12);
	{
	Declare_entry(mercury__quantification__implicitly_quantify_clause_body_8_0);
	call_localret(ENTRY(mercury__quantification__implicitly_quantify_clause_body_8_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i14,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i14);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	detstackvar(9) = (Integer) r2;
	r2 = (Integer) tempr1;
	detstackvar(10) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_goal_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_goal_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i15,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i15);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_variables_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_variables_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i16,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i16);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_vartypes_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_vartypes_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i17,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i17);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r3 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__cse_detection__detect_cse_in_proc_6_0_i18,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i18);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_procedures_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_procedures_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i19,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i19);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__cse_detection__detect_cse_in_proc_6_0_i20,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i20);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_module__module_info_set_preds_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_set_preds_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i21,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i21);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i11);
	if (((Integer) r6 == ((Integer) 1)))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i54);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r1;
	r1 = ((Integer) 12);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i25,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i25);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i26);
	detstackvar(5) = (Integer) r1;
	r1 = string_const("% Repeating mode check for ", 27);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i29,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i29);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_out__write_pred_id_4_0);
	call_localret(ENTRY(mercury__hlds_out__write_pred_id_4_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i30,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i30);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i31,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i31);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i32);
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i26);
	r4 = (Integer) r2;
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(3);
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i32);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	{
	Declare_entry(mercury__modes__modecheck_proc_7_0);
	call_localret(ENTRY(mercury__modes__modecheck_proc_7_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i33,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i33);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	if (((Integer) r2 <= ((Integer) 0)))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i34);
	detstackvar(4) = (Integer) r1;
	detstackvar(6) = (Integer) r3;
	r1 = string_const("mode check fails when repeated", 30);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i36,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i36);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i37);
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i34);
	r2 = (Integer) r3;
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(5);
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i37);
	if (((Integer) r5 != ((Integer) 0)))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i38);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	r1 = string_const("% Repeating switch detection for ", 33);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i41,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i41);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_out__write_pred_id_4_0);
	call_localret(ENTRY(mercury__hlds_out__write_pred_id_4_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i42,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i42);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i43,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i43);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i44);
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i38);
	{
	Word tempr1;
	tempr1 = (Integer) r6;
	r6 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i44);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	{
	Declare_entry(mercury__switch_detection__detect_switches_in_proc_4_0);
	call_localret(ENTRY(mercury__switch_detection__detect_switches_in_proc_4_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i45,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i45);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	if (((Integer) detstackvar(5) != ((Integer) 0)))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_proc_6_0_i46);
	r2 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r1;
	r1 = string_const("% Repeating common deconstruction detection for ", 48);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i49,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i49);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_out__write_pred_id_4_0);
	call_localret(ENTRY(mercury__hlds_out__write_pred_id_4_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i50,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i50);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__cse_detection__detect_cse_in_proc_6_0_i51,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i51);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_proc_6_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	localtailcall(mercury__cse_detection__detect_cse_in_proc_6_0,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i46);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	localtailcall(mercury__cse_detection__detect_cse_in_proc_6_0,
		ENTRY(mercury__cse_detection__detect_cse_in_proc_6_0));
Define_label(mercury__cse_detection__detect_cse_in_proc_6_0_i54);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module2)
	init_entry(mercury__cse_detection__detect_cse_in_preds_5_0);
	init_label(mercury__cse_detection__detect_cse_in_preds_5_0_i4);
	init_label(mercury__cse_detection__detect_cse_in_preds_5_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_preds_5_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_preds_5_0_i7);
	init_label(mercury__cse_detection__detect_cse_in_preds_5_0_i1002);
BEGIN_CODE

/* code for predicate 'detect_cse_in_preds'/5 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_preds_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_preds_5_0_i1002);
	incr_sp_push_msg(5, "detect_cse_in_preds");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__cse_detection__detect_cse_in_preds_5_0_i4,
		STATIC(mercury__cse_detection__detect_cse_in_preds_5_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_preds_5_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_preds_5_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__cse_detection__detect_cse_in_preds_5_0_i5,
		STATIC(mercury__cse_detection__detect_cse_in_preds_5_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_preds_5_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_preds_5_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_non_imported_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_non_imported_procids_2_0),
		mercury__cse_detection__detect_cse_in_preds_5_0_i6,
		STATIC(mercury__cse_detection__detect_cse_in_preds_5_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_preds_5_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_preds_5_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_procs_6_0),
		mercury__cse_detection__detect_cse_in_preds_5_0_i7,
		STATIC(mercury__cse_detection__detect_cse_in_preds_5_0));
Define_label(mercury__cse_detection__detect_cse_in_preds_5_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_preds_5_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__cse_detection__detect_cse_in_preds_5_0,
		STATIC(mercury__cse_detection__detect_cse_in_preds_5_0));
Define_label(mercury__cse_detection__detect_cse_in_preds_5_0_i1002);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module3)
	init_entry(mercury__cse_detection__detect_cse_in_procs_6_0);
	init_label(mercury__cse_detection__detect_cse_in_procs_6_0_i4);
	init_label(mercury__cse_detection__detect_cse_in_procs_6_0_i1002);
BEGIN_CODE

/* code for predicate 'detect_cse_in_procs'/6 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_procs_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_procs_6_0_i1002);
	incr_sp_push_msg(3, "detect_cse_in_procs");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
		call_localret(STATIC(mercury__cse_detection__detect_cse_in_proc_6_0),
		mercury__cse_detection__detect_cse_in_procs_6_0_i4,
		STATIC(mercury__cse_detection__detect_cse_in_procs_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_procs_6_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_procs_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__cse_detection__detect_cse_in_procs_6_0,
		STATIC(mercury__cse_detection__detect_cse_in_procs_6_0));
Define_label(mercury__cse_detection__detect_cse_in_procs_6_0_i1002);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module4)
	init_entry(mercury__cse_detection__detect_cse_in_goal_6_0);
BEGIN_CODE

/* code for predicate 'detect_cse_in_goal'/6 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_goal_6_0);
	tailcall(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		STATIC(mercury__cse_detection__detect_cse_in_goal_6_0));
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module5)
	init_entry(mercury__cse_detection__detect_cse_in_goal_1_7_0);
	init_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i2);
	init_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i3);
	init_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i4);
BEGIN_CODE

/* code for predicate 'detect_cse_in_goal_1'/7 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_goal_1_7_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	incr_sp_push_msg(5, "detect_cse_in_goal_1");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0),
		mercury__cse_detection__detect_cse_in_goal_1_7_0_i2,
		STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i2);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_1_7_0));
	{
	Word tempr1, tempr2;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	tempr2 = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) tempr2;
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) tempr2;
	detstackvar(4) = (Integer) tempr1;
	detstackvar(3) = (Integer) r2;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_instmap_delta_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_instmap_delta_2_0),
		mercury__cse_detection__detect_cse_in_goal_1_7_0_i3,
		STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0));
	}
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i3);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_1_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__instmap__apply_instmap_delta_3_0);
	call_localret(ENTRY(mercury__instmap__apply_instmap_delta_3_0),
		mercury__cse_detection__detect_cse_in_goal_1_7_0_i4,
		STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_1_7_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_1_7_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module6)
	init_entry(mercury__cse_detection__detect_cse_in_goal_2_7_0);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1028);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1027);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1026);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1025);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1024);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1023);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i7);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i9);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i13);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i10);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i15);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i16);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i19);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i20);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i23);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i24);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i25);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i26);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i27);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i28);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i29);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1019);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1022);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i33);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i32);
	init_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1021);
BEGIN_CODE

/* code for predicate 'detect_cse_in_goal_2'/7 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_goal_2_7_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1022);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1028) AND
		LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1027) AND
		LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1026) AND
		LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1025) AND
		LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1024) AND
		LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1023) AND
		LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1019));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1028);
	incr_sp_push_msg(9, "detect_cse_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i5);
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1027);
	incr_sp_push_msg(9, "detect_cse_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i9);
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1026);
	incr_sp_push_msg(9, "detect_cse_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i15);
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1025);
	incr_sp_push_msg(9, "detect_cse_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i23);
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1024);
	incr_sp_push_msg(9, "detect_cse_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i25);
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1023);
	incr_sp_push_msg(9, "detect_cse_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i27);
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i5);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i6,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i7,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(7);
	r7 = (Integer) detstackvar(2);
	r8 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__cse_detection__detect_cse_in_cases_11_0),
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i9);
	r2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r5 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r6 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r7 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r8 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) r7) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i10);
	detstackvar(1) = (Integer) r8;
	detstackvar(2) = (Integer) r6;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 2));
	detstackvar(8) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 3));
	r1 = (Integer) field(mktag(2), (Integer) r7, ((Integer) 4));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_6_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i13,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i13);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(2), ((Integer) 5));
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) tempr1;
	field(mktag(3), (Integer) r3, ((Integer) 4)) = (Integer) detstackvar(3);
	field(mktag(3), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r3, ((Integer) 5)) = (Integer) detstackvar(4);
	field(mktag(2), (Integer) tempr1, ((Integer) 4)) = (Integer) r4;
	field(mktag(2), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(8);
	field(mktag(2), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(7);
	field(mktag(2), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(6);
	field(mktag(2), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i10);
	r9 = (Integer) r2;
	r1 = (Integer) r4;
	r2 = ((Integer) 1);
	tag_incr_hp(r3, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) r8;
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) r7;
	field(mktag(3), (Integer) r3, ((Integer) 3)) = (Integer) r6;
	field(mktag(3), (Integer) r3, ((Integer) 4)) = (Integer) r5;
	field(mktag(3), (Integer) r3, ((Integer) 5)) = (Integer) r9;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i15);
	r5 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r6 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if (((Integer) r6 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i16);
	r1 = (Integer) r4;
	r2 = ((Integer) 1);
	tag_incr_hp(r3, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i16);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r5;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i19,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i19);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i20,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i20);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__cse_detection__detect_cse_in_disj_9_0),
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i23);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_6_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i24,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i24);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i25);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_6_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i26,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i26);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i27);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	detstackvar(8) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i28,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i28);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i29,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i29);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(7);
	r6 = (Integer) detstackvar(1);
	r7 = (Integer) detstackvar(8);
	r8 = (Integer) detstackvar(2);
	r9 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(STATIC(mercury__cse_detection__detect_cse_in_ite_12_0),
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1019);
	r3 = (Integer) r1;
	r1 = (Integer) r4;
	r2 = ((Integer) 1);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1022);
	incr_sp_push_msg(9, "detect_cse_in_goal_2");
	detstackvar(9) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i32);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_conj_6_0),
		mercury__cse_detection__detect_cse_in_goal_2_7_0_i33,
		STATIC(mercury__cse_detection__detect_cse_in_goal_2_7_0));
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i33);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0));
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i32);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1021);
	r3 = (Integer) r1;
	r1 = (Integer) r4;
	r2 = ((Integer) 1);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_goal_2_7_0_i1021);
	r3 = (Integer) r1;
	r1 = (Integer) r4;
	r2 = ((Integer) 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module7)
	init_entry(mercury__cse_detection__detect_cse_in_conj_6_0);
	init_label(mercury__cse_detection__detect_cse_in_conj_6_0_i4);
	init_label(mercury__cse_detection__detect_cse_in_conj_6_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_conj_6_0_i9);
	init_label(mercury__cse_detection__detect_cse_in_conj_6_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_conj_6_0_i10);
	init_label(mercury__cse_detection__detect_cse_in_conj_6_0_i11);
	init_label(mercury__cse_detection__detect_cse_in_conj_6_0_i1003);
BEGIN_CODE

/* code for predicate 'detect_cse_in_conj'/6 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_conj_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_conj_6_0_i1003);
	incr_sp_push_msg(4, "detect_cse_in_conj");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		mercury__cse_detection__detect_cse_in_conj_6_0_i4,
		STATIC(mercury__cse_detection__detect_cse_in_conj_6_0));
Define_label(mercury__cse_detection__detect_cse_in_conj_6_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_conj_6_0));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) tempr1;
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r4;
	localcall(mercury__cse_detection__detect_cse_in_conj_6_0,
		LABEL(mercury__cse_detection__detect_cse_in_conj_6_0_i5),
		STATIC(mercury__cse_detection__detect_cse_in_conj_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_conj_6_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_conj_6_0));
	r4 = (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 0));
	if ((tag((Integer) r4) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_conj_6_0_i6);
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_cse_detection__common_0);
	r2 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__cse_detection__detect_cse_in_conj_6_0_i9,
		STATIC(mercury__cse_detection__detect_cse_in_conj_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_conj_6_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_conj_6_0));
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__cse_detection__detect_cse_in_conj_6_0_i10);
Define_label(mercury__cse_detection__detect_cse_in_conj_6_0_i6);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
Define_label(mercury__cse_detection__detect_cse_in_conj_6_0_i10);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	{
	Declare_entry(mercury__bool__or_3_0);
	call_localret(ENTRY(mercury__bool__or_3_0),
		mercury__cse_detection__detect_cse_in_conj_6_0_i11,
		STATIC(mercury__cse_detection__detect_cse_in_conj_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_conj_6_0_i11);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_conj_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_conj_6_0_i1003);
	r1 = (Integer) r3;
	r2 = ((Integer) 1);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module8)
	init_entry(mercury__cse_detection__detect_cse_in_disj_9_0);
	init_label(mercury__cse_detection__detect_cse_in_disj_9_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_disj_9_0_i7);
	init_label(mercury__cse_detection__detect_cse_in_disj_9_0_i9);
	init_label(mercury__cse_detection__detect_cse_in_disj_9_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_disj_9_0_i3);
	init_label(mercury__cse_detection__detect_cse_in_disj_9_0_i14);
BEGIN_CODE

/* code for predicate 'detect_cse_in_disj'/9 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_disj_9_0);
	incr_sp_push_msg(8, "detect_cse_in_disj");
	detstackvar(8) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_disj_9_0_i3);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) tempr1;
	detstackvar(6) = (Integer) tempr1;
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r5;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	{
	Declare_entry(mercury__instmap__lookup_var_3_0);
	call_localret(ENTRY(mercury__instmap__lookup_var_3_0),
		mercury__cse_detection__detect_cse_in_disj_9_0_i6,
		STATIC(mercury__cse_detection__detect_cse_in_disj_9_0));
	}
	}
Define_label(mercury__cse_detection__detect_cse_in_disj_9_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_disj_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(5), ((Integer) 2));
	{
	Declare_entry(mercury__mode_util__inst_is_ground_or_any_2_0);
	call_localret(ENTRY(mercury__mode_util__inst_is_ground_or_any_2_0),
		mercury__cse_detection__detect_cse_in_disj_9_0_i7,
		STATIC(mercury__cse_detection__detect_cse_in_disj_9_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_disj_9_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_disj_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_disj_9_0_i5);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__cse_detection__common_deconstruct_2_7_0),
		mercury__cse_detection__detect_cse_in_disj_9_0_i9,
		STATIC(mercury__cse_detection__detect_cse_in_disj_9_0));
Define_label(mercury__cse_detection__detect_cse_in_disj_9_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_disj_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_disj_9_0_i5);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_disj_9_0_i5);
	r1 = (Integer) r2;
	r2 = ((Integer) 0);
	r6 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	tag_incr_hp(r7, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	tag_incr_hp(r8, mktag(1), ((Integer) 2));
	tag_incr_hp(r9, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(0), (Integer) r9, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r6;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r7, ((Integer) 1)) = (Integer) r8;
	field(mktag(1), (Integer) r8, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r8, ((Integer) 0)) = (Integer) r9;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r7;
	field(mktag(0), (Integer) r9, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
	}
Define_label(mercury__cse_detection__detect_cse_in_disj_9_0_i5);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__cse_detection__detect_cse_in_disj_9_0,
		STATIC(mercury__cse_detection__detect_cse_in_disj_9_0));
Define_label(mercury__cse_detection__detect_cse_in_disj_9_0_i3);
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r2;
	r2 = (Integer) r5;
	r3 = (Integer) r6;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_disj_2_6_0),
		mercury__cse_detection__detect_cse_in_disj_9_0_i14,
		STATIC(mercury__cse_detection__detect_cse_in_disj_9_0));
Define_label(mercury__cse_detection__detect_cse_in_disj_9_0_i14);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_disj_9_0));
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module9)
	init_entry(mercury__cse_detection__detect_cse_in_disj_2_6_0);
	init_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i4);
	init_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i1002);
BEGIN_CODE

/* code for predicate 'detect_cse_in_disj_2'/6 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_disj_2_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_disj_2_6_0_i1002);
	incr_sp_push_msg(3, "detect_cse_in_disj_2");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		mercury__cse_detection__detect_cse_in_disj_2_6_0_i4,
		STATIC(mercury__cse_detection__detect_cse_in_disj_2_6_0));
Define_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_disj_2_6_0));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) tempr1;
	localcall(mercury__cse_detection__detect_cse_in_disj_2_6_0,
		LABEL(mercury__cse_detection__detect_cse_in_disj_2_6_0_i5),
		STATIC(mercury__cse_detection__detect_cse_in_disj_2_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_disj_2_6_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	{
	Declare_entry(mercury__bool__or_3_0);
	call_localret(ENTRY(mercury__bool__or_3_0),
		mercury__cse_detection__detect_cse_in_disj_2_6_0_i6,
		STATIC(mercury__cse_detection__detect_cse_in_disj_2_6_0));
	}
	}
Define_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_disj_2_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_disj_2_6_0_i1002);
	r1 = (Integer) r3;
	r2 = ((Integer) 1);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module10)
	init_entry(mercury__cse_detection__detect_cse_in_cases_11_0);
	init_label(mercury__cse_detection__detect_cse_in_cases_11_0_i8);
	init_label(mercury__cse_detection__detect_cse_in_cases_11_0_i10);
	init_label(mercury__cse_detection__detect_cse_in_cases_11_0_i11);
	init_label(mercury__cse_detection__detect_cse_in_cases_11_0_i13);
	init_label(mercury__cse_detection__detect_cse_in_cases_11_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_cases_11_0_i3);
	init_label(mercury__cse_detection__detect_cse_in_cases_11_0_i18);
BEGIN_CODE

/* code for predicate 'detect_cse_in_cases'/11 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_cases_11_0);
	incr_sp_push_msg(10, "detect_cse_in_cases");
	detstackvar(10) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_cases_11_0_i3);
	detstackvar(9) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(8) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r8;
	{
	Declare_entry(mercury____Unify___mercury_builtin__var_0_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__var_0_0),
		mercury__cse_detection__detect_cse_in_cases_11_0_i8,
		STATIC(mercury__cse_detection__detect_cse_in_cases_11_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_cases_11_0_i8);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_11_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_cases_11_0_i5);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__instmap__lookup_var_3_0);
	call_localret(ENTRY(mercury__instmap__lookup_var_3_0),
		mercury__cse_detection__detect_cse_in_cases_11_0_i10,
		STATIC(mercury__cse_detection__detect_cse_in_cases_11_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_cases_11_0_i10);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_11_0));
	r2 = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(7), ((Integer) 2));
	{
	Declare_entry(mercury__mode_util__inst_is_ground_or_any_2_0);
	call_localret(ENTRY(mercury__mode_util__inst_is_ground_or_any_2_0),
		mercury__cse_detection__detect_cse_in_cases_11_0_i11,
		STATIC(mercury__cse_detection__detect_cse_in_cases_11_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_cases_11_0_i11);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_11_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_cases_11_0_i5);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__cse_detection__common_deconstruct_cases_2_7_0),
		mercury__cse_detection__detect_cse_in_cases_11_0_i13,
		STATIC(mercury__cse_detection__detect_cse_in_cases_11_0));
Define_label(mercury__cse_detection__detect_cse_in_cases_11_0_i13);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_11_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_cases_11_0_i5);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_cases_11_0_i5);
	r1 = (Integer) r2;
	r2 = ((Integer) 0);
	r6 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	tag_incr_hp(r7, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	tag_incr_hp(r8, mktag(1), ((Integer) 2));
	tag_incr_hp(r9, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 5));
	field(mktag(0), (Integer) r9, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) r6;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(1), (Integer) r7, ((Integer) 1)) = (Integer) r8;
	field(mktag(1), (Integer) r8, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r8, ((Integer) 0)) = (Integer) r9;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r7;
	field(mktag(0), (Integer) r9, ((Integer) 1)) = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__cse_detection__detect_cse_in_cases_11_0_i5);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	localtailcall(mercury__cse_detection__detect_cse_in_cases_11_0,
		STATIC(mercury__cse_detection__detect_cse_in_cases_11_0));
Define_label(mercury__cse_detection__detect_cse_in_cases_11_0_i3);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(5) = (Integer) r6;
	r1 = (Integer) r4;
	r2 = (Integer) r7;
	r3 = (Integer) r8;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_cases_2_6_0),
		mercury__cse_detection__detect_cse_in_cases_11_0_i18,
		STATIC(mercury__cse_detection__detect_cse_in_cases_11_0));
Define_label(mercury__cse_detection__detect_cse_in_cases_11_0_i18);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_11_0));
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(3), ((Integer) 5));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r3, ((Integer) 3)) = (Integer) r4;
	field(mktag(3), (Integer) r3, ((Integer) 4)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module11)
	init_entry(mercury__cse_detection__detect_cse_in_cases_2_6_0);
	init_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i4);
	init_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i1003);
BEGIN_CODE

/* code for predicate 'detect_cse_in_cases_2'/6 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_cases_2_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_cases_2_6_0_i1003);
	incr_sp_push_msg(4, "detect_cse_in_cases_2");
	detstackvar(4) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		mercury__cse_detection__detect_cse_in_cases_2_6_0_i4,
		STATIC(mercury__cse_detection__detect_cse_in_cases_2_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_2_6_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	localcall(mercury__cse_detection__detect_cse_in_cases_2_6_0,
		LABEL(mercury__cse_detection__detect_cse_in_cases_2_6_0_i5),
		STATIC(mercury__cse_detection__detect_cse_in_cases_2_6_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_2_6_0));
	r4 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r5 = (Integer) detstackvar(2);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	detstackvar(2) = (Integer) tempr1;
	r1 = (Integer) r5;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r4;
	{
	Declare_entry(mercury__bool__or_3_0);
	call_localret(ENTRY(mercury__bool__or_3_0),
		mercury__cse_detection__detect_cse_in_cases_2_6_0_i6,
		STATIC(mercury__cse_detection__detect_cse_in_cases_2_6_0));
	}
	}
Define_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_cases_2_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__cse_detection__detect_cse_in_cases_2_6_0_i1003);
	r1 = (Integer) r3;
	r2 = ((Integer) 1);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module12)
	init_entry(mercury__cse_detection__detect_cse_in_ite_12_0);
	init_label(mercury__cse_detection__detect_cse_in_ite_12_0_i6);
	init_label(mercury__cse_detection__detect_cse_in_ite_12_0_i7);
	init_label(mercury__cse_detection__detect_cse_in_ite_12_0_i9);
	init_label(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_ite_12_0_i3);
	init_label(mercury__cse_detection__detect_cse_in_ite_12_0_i17);
BEGIN_CODE

/* code for predicate 'detect_cse_in_ite'/12 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_ite_12_0);
	incr_sp_push_msg(12, "detect_cse_in_ite");
	detstackvar(12) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_ite_12_0_i3);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) tempr1;
	detstackvar(9) = (Integer) tempr1;
	detstackvar(10) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(11) = (Integer) field(mktag(0), (Integer) r9, ((Integer) 2));
	r1 = (Integer) r8;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r8;
	detstackvar(8) = (Integer) r9;
	{
	Declare_entry(mercury__instmap__lookup_var_3_0);
	call_localret(ENTRY(mercury__instmap__lookup_var_3_0),
		mercury__cse_detection__detect_cse_in_ite_12_0_i6,
		STATIC(mercury__cse_detection__detect_cse_in_ite_12_0));
	}
	}
Define_label(mercury__cse_detection__detect_cse_in_ite_12_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_12_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(11);
	{
	Declare_entry(mercury__mode_util__inst_is_ground_or_any_2_0);
	call_localret(ENTRY(mercury__mode_util__inst_is_ground_or_any_2_0),
		mercury__cse_detection__detect_cse_in_ite_12_0_i7,
		STATIC(mercury__cse_detection__detect_cse_in_ite_12_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_ite_12_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_12_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(9);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__cse_detection__common_deconstruct_2_7_0),
		mercury__cse_detection__detect_cse_in_ite_12_0_i9,
		STATIC(mercury__cse_detection__detect_cse_in_ite_12_0));
Define_label(mercury__cse_detection__detect_cse_in_ite_12_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_12_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	if (((Integer) field(mktag(1), (Integer) r3, ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r3, ((Integer) 1)), ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	r1 = (Integer) r2;
	r2 = ((Integer) 0);
	r6 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	tag_incr_hp(r7, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	tag_incr_hp(r8, mktag(1), ((Integer) 2));
	tag_incr_hp(r9, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 6));
	field(mktag(0), (Integer) r9, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r6, ((Integer) 1)), ((Integer) 0));
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) field(mktag(1), (Integer) r6, ((Integer) 0));
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) tempr1, ((Integer) 5)) = (Integer) detstackvar(6);
	field(mktag(1), (Integer) r7, ((Integer) 1)) = (Integer) r8;
	field(mktag(1), (Integer) r8, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r8, ((Integer) 0)) = (Integer) r9;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r7;
	field(mktag(0), (Integer) r9, ((Integer) 1)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
	}
Define_label(mercury__cse_detection__detect_cse_in_ite_12_0_i5);
	r1 = (Integer) detstackvar(10);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	localtailcall(mercury__cse_detection__detect_cse_in_ite_12_0,
		STATIC(mercury__cse_detection__detect_cse_in_ite_12_0));
Define_label(mercury__cse_detection__detect_cse_in_ite_12_0_i3);
	detstackvar(1) = (Integer) r2;
	detstackvar(6) = (Integer) r7;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	r4 = (Integer) r8;
	r5 = (Integer) r9;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_ite_2_10_0),
		mercury__cse_detection__detect_cse_in_ite_12_0_i17,
		STATIC(mercury__cse_detection__detect_cse_in_ite_12_0));
Define_label(mercury__cse_detection__detect_cse_in_ite_12_0_i17);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_12_0));
	r6 = (Integer) r3;
	tag_incr_hp(r3, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) r6;
	field(mktag(3), (Integer) r3, ((Integer) 3)) = (Integer) r4;
	field(mktag(3), (Integer) r3, ((Integer) 4)) = (Integer) r5;
	field(mktag(3), (Integer) r3, ((Integer) 5)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module13)
	init_entry(mercury__cse_detection__detect_cse_in_ite_2_10_0);
	init_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i2);
	init_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i3);
	init_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i4);
	init_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i5);
	init_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i6);
BEGIN_CODE

/* code for predicate 'detect_cse_in_ite_2'/10 in mode 0 */
Define_static(mercury__cse_detection__detect_cse_in_ite_2_10_0);
	incr_sp_push_msg(6, "detect_cse_in_ite_2");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		mercury__cse_detection__detect_cse_in_ite_2_10_0_i2,
		STATIC(mercury__cse_detection__detect_cse_in_ite_2_10_0));
Define_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i2);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	detstackvar(4) = (Integer) r3;
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		mercury__cse_detection__detect_cse_in_ite_2_10_0_i3,
		STATIC(mercury__cse_detection__detect_cse_in_ite_2_10_0));
Define_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i3);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r3;
	tempr2 = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) tempr2;
	r3 = (Integer) tempr1;
	call_localret(STATIC(mercury__cse_detection__detect_cse_in_goal_1_7_0),
		mercury__cse_detection__detect_cse_in_ite_2_10_0_i4,
		STATIC(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	detstackvar(5) = (Integer) r2;
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__bool__or_3_0);
	call_localret(ENTRY(mercury__bool__or_3_0),
		mercury__cse_detection__detect_cse_in_ite_2_10_0_i5,
		STATIC(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	}
	}
Define_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__bool__or_3_0);
	call_localret(ENTRY(mercury__bool__or_3_0),
		mercury__cse_detection__detect_cse_in_ite_2_10_0_i6,
		STATIC(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	}
Define_label(mercury__cse_detection__detect_cse_in_ite_2_10_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__detect_cse_in_ite_2_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module14)
	init_entry(mercury__cse_detection__common_deconstruct_2_7_0);
	init_label(mercury__cse_detection__common_deconstruct_2_7_0_i1001);
	init_label(mercury__cse_detection__common_deconstruct_2_7_0_i4);
	init_label(mercury__cse_detection__common_deconstruct_2_7_0_i5);
	init_label(mercury__cse_detection__common_deconstruct_2_7_0_i6);
	init_label(mercury__cse_detection__common_deconstruct_2_7_0_i8);
	init_label(mercury__cse_detection__common_deconstruct_2_7_0_i9);
	init_label(mercury__cse_detection__common_deconstruct_2_7_0_i1);
BEGIN_CODE

/* code for predicate 'common_deconstruct_2'/7 in mode 0 */
Define_static(mercury__cse_detection__common_deconstruct_2_7_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__common_deconstruct_2_7_0_i1001);
	r2 = (Integer) r4;
	r4 = (Integer) r3;
	r1 = TRUE;
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__cse_detection__common_deconstruct_2_7_0_i1001);
	incr_sp_push_msg(7, "common_deconstruct_2");
	detstackvar(7) = (Integer) succip;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__hlds_goal__goal_to_conj_list_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_to_conj_list_2_0),
		mercury__cse_detection__common_deconstruct_2_7_0_i4,
		STATIC(mercury__cse_detection__common_deconstruct_2_7_0));
	}
Define_label(mercury__cse_detection__common_deconstruct_2_7_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_2_7_0));
	r2 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r1;
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__cse_detection__common_deconstruct_2_7_0_i5,
		STATIC(mercury__cse_detection__common_deconstruct_2_7_0));
	}
Define_label(mercury__cse_detection__common_deconstruct_2_7_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_2_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0),
		mercury__cse_detection__common_deconstruct_2_7_0_i6,
		STATIC(mercury__cse_detection__common_deconstruct_2_7_0));
Define_label(mercury__cse_detection__common_deconstruct_2_7_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_2_7_0));
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__common_deconstruct_2_7_0_i1);
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_goal__conj_list_to_goal_3_0);
	call_localret(ENTRY(mercury__hlds_goal__conj_list_to_goal_3_0),
		mercury__cse_detection__common_deconstruct_2_7_0_i8,
		STATIC(mercury__cse_detection__common_deconstruct_2_7_0));
	}
Define_label(mercury__cse_detection__common_deconstruct_2_7_0_i8);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_2_7_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__cse_detection__common_deconstruct_2_7_0,
		LABEL(mercury__cse_detection__common_deconstruct_2_7_0_i9),
		STATIC(mercury__cse_detection__common_deconstruct_2_7_0));
Define_label(mercury__cse_detection__common_deconstruct_2_7_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_2_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__common_deconstruct_2_7_0_i1);
	r1 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__cse_detection__common_deconstruct_2_7_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module15)
	init_entry(mercury__cse_detection__common_deconstruct_cases_2_7_0);
	init_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1001);
	init_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i4);
	init_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i5);
	init_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i6);
	init_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i8);
	init_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i9);
	init_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1);
BEGIN_CODE

/* code for predicate 'common_deconstruct_cases_2'/7 in mode 0 */
Define_static(mercury__cse_detection__common_deconstruct_cases_2_7_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1001);
	r2 = (Integer) r4;
	r4 = (Integer) r3;
	r1 = TRUE;
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1001);
	incr_sp_push_msg(8, "common_deconstruct_cases_2");
	detstackvar(8) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(5) = (Integer) r1;
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__hlds_goal__goal_to_conj_list_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_to_conj_list_2_0),
		mercury__cse_detection__common_deconstruct_cases_2_7_0_i4,
		STATIC(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	}
	}
Define_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	r2 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) r1;
	detstackvar(7) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__cse_detection__common_deconstruct_cases_2_7_0_i5,
		STATIC(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	}
Define_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0),
		mercury__cse_detection__common_deconstruct_cases_2_7_0_i6,
		STATIC(mercury__cse_detection__common_deconstruct_cases_2_7_0));
Define_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1);
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_goal__conj_list_to_goal_3_0);
	call_localret(ENTRY(mercury__hlds_goal__conj_list_to_goal_3_0),
		mercury__cse_detection__common_deconstruct_cases_2_7_0_i8,
		STATIC(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	}
Define_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i8);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	r2 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(4);
	localcall(mercury__cse_detection__common_deconstruct_cases_2_7_0,
		LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0_i9),
		STATIC(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	}
Define_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1);
	r1 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__cse_detection__common_deconstruct_cases_2_7_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module16)
	init_entry(mercury__cse_detection__find_bind_var_for_cse_9_0);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i7);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i8);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i11);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i4);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i16);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i21);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i23);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i26);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i27);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i18);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i17);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i32);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i34);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i37);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i38);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i40);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i29);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i28);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i41);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i46);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i45);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i48);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i13);
	init_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i1001);
BEGIN_CODE

/* code for predicate 'find_bind_var_for_cse'/9 in mode 0 */
Define_static(mercury__cse_detection__find_bind_var_for_cse_9_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i1001);
	r7 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r6 = (Integer) field(mktag(0), (Integer) r7, ((Integer) 1));
	r8 = (Integer) field(mktag(0), (Integer) r7, ((Integer) 0));
	r9 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	incr_sp_push_msg(15, "find_bind_var_for_cse");
	detstackvar(15) = (Integer) succip;
	if ((tag((Integer) r8) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i4);
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(8) = (Integer) r7;
	detstackvar(9) = (Integer) r9;
	detstackvar(10) = (Integer) r6;
	r1 = (Integer) field(mktag(0), (Integer) r8, ((Integer) 0));
	localcall(mercury__cse_detection__find_bind_var_for_cse_9_0,
		LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i7),
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i8);
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(9);
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) detstackvar(10);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(15);
	decr_sp_pop_msg(15);
	proceed();
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i8);
	r2 = (Integer) r3;
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	localcall(mercury__cse_detection__find_bind_var_for_cse_9_0,
		LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i11),
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i11);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(8);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(15);
	decr_sp_pop_msg(15);
	proceed();
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i4);
	if ((tag((Integer) r8) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i13);
	if (((Integer) field(mktag(3), (Integer) r8, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i13);
	detstackvar(13) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 4));
	detstackvar(12) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 2));
	detstackvar(11) = (Integer) field(mktag(3), (Integer) r8, ((Integer) 1));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(8) = (Integer) r7;
	detstackvar(9) = (Integer) r9;
	detstackvar(10) = (Integer) r6;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	{
	Declare_entry(mercury__term__apply_rec_substitution_3_0);
	call_localret(ENTRY(mercury__term__apply_rec_substitution_3_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i16,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i16);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i17);
	if ((tag((Integer) detstackvar(13)) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i17);
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(14) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) detstackvar(13), ((Integer) 0));
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__term__apply_rec_substitution_3_0);
	call_localret(ENTRY(mercury__term__apply_rec_substitution_3_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i21,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i21);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i18);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury____Unify___mercury_builtin__var_0_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__var_0_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i23,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i23);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i18);
	if (((Integer) detstackvar(4) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i18);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(5);
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__cse_detection__construct_common_unify_8_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i26,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i26);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	r5 = (Integer) detstackvar(6);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r5;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	detstackvar(6) = (Integer) tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	detstackvar(7) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_cse_detection__common_0);
	r2 = (Integer) r4;
	r3 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i27,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i27);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(15);
	decr_sp_pop_msg(15);
	proceed();
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i18);
	r1 = (Integer) detstackvar(14);
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i17);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i28);
	if ((tag((Integer) detstackvar(13)) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i28);
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(14) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) detstackvar(13), ((Integer) 0));
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__term__apply_rec_substitution_3_0);
	call_localret(ENTRY(mercury__term__apply_rec_substitution_3_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i32,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i32);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i29);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury____Unify___mercury_builtin__var_0_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__var_0_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i34,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i34);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i29);
	r2 = (Integer) detstackvar(4);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i29);
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_context_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_context_2_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i37,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i37);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(13);
	call_localret(STATIC(mercury__cse_detection__find_similar_deconstruct_4_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i38,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i38);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i29);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_cse_detection__common_0);
	r3 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i40,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i40);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(15);
	decr_sp_pop_msg(15);
	proceed();
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i29);
	r1 = (Integer) detstackvar(14);
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i28);
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i41);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(15);
	decr_sp_pop_msg(15);
	proceed();
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i41);
	r1 = (Integer) detstackvar(11);
	r2 = (Integer) detstackvar(12);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__det_util__interpret_unify_4_0);
	call_localret(ENTRY(mercury__det_util__interpret_unify_4_0),
		mercury__cse_detection__find_bind_var_for_cse_9_0_i46,
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
	}
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i46);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i45);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(8);
	r1 = (Integer) detstackvar(9);
	GOTO_LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i48);
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i45);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(8);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(2);
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i48);
	detstackvar(8) = (Integer) r6;
	localcall(mercury__cse_detection__find_bind_var_for_cse_9_0,
		LABEL(mercury__cse_detection__find_bind_var_for_cse_9_0_i11),
		STATIC(mercury__cse_detection__find_bind_var_for_cse_9_0));
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i13);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) r5;
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(15);
	decr_sp_pop_msg(15);
	proceed();
Define_label(mercury__cse_detection__find_bind_var_for_cse_9_0_i1001);
	r1 = (Integer) r5;
	r3 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module17)
	init_entry(mercury__cse_detection__construct_common_unify_8_0);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i1001);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i9);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i10);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i11);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i12);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i13);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i1000);
	init_label(mercury__cse_detection__construct_common_unify_8_0_i14);
BEGIN_CODE

/* code for predicate 'construct_common_unify'/8 in mode 0 */
Define_static(mercury__cse_detection__construct_common_unify_8_0);
	if ((tag((Integer) field(mktag(0), (Integer) r2, ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__cse_detection__construct_common_unify_8_0_i1000);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__cse_detection__construct_common_unify_8_0_i1000);
	if ((tag((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 4))) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__construct_common_unify_8_0_i1000);
	r5 = (Integer) field(mktag(1), (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 4)), ((Integer) 2));
	r6 = (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 5));
	r7 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	if ((tag((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 2))) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__construct_common_unify_8_0_i1001);
	{
	Word tempr1, tempr2;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 6));
	tempr2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) field(mktag(3), (Integer) tempr2, ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) field(mktag(3), (Integer) tempr2, ((Integer) 2));
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 1);
	r8 = (Integer) r2;
	r10 = (Integer) r1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r6;
	r1 = (Integer) r7;
	r6 = (Integer) tempr1;
	tempr1 = (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) r8, ((Integer) 0)), ((Integer) 4));
	tag_incr_hp(tempr2, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) tempr2, ((Integer) 3)) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 3));
	field(mktag(1), (Integer) tempr2, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	field(mktag(1), (Integer) tempr2, ((Integer) 4)) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 4));
	field(mktag(3), (Integer) r6, ((Integer) 5)) = (Integer) r4;
	field(mktag(1), (Integer) tempr2, ((Integer) 2)) = (Integer) r5;
	field(mktag(1), (Integer) tempr2, ((Integer) 0)) = (Integer) r10;
	field(mktag(3), (Integer) r6, ((Integer) 4)) = (Integer) tempr2;
	incr_sp_push_msg(7, "construct_common_unify");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__cse_detection__construct_common_unify_8_0_i10);
	}
Define_label(mercury__cse_detection__construct_common_unify_8_0_i1001);
	incr_sp_push_msg(7, "construct_common_unify");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r7;
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r5;
	r1 = string_const("unexpected unify structure in construct_common_unify", 52);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__cse_detection__construct_common_unify_8_0_i9,
		STATIC(mercury__cse_detection__construct_common_unify_8_0));
	}
Define_label(mercury__cse_detection__construct_common_unify_8_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__construct_common_unify_8_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
Define_label(mercury__cse_detection__construct_common_unify_8_0_i10);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_context_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_context_2_0),
		mercury__cse_detection__construct_common_unify_8_0_i11,
		STATIC(mercury__cse_detection__construct_common_unify_8_0));
	}
Define_label(mercury__cse_detection__construct_common_unify_8_0_i11);
	update_prof_current_proc(LABEL(mercury__cse_detection__construct_common_unify_8_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__cse_detection__create_parallel_subterms_9_0),
		mercury__cse_detection__construct_common_unify_8_0_i12,
		STATIC(mercury__cse_detection__construct_common_unify_8_0));
Define_label(mercury__cse_detection__construct_common_unify_8_0_i12);
	update_prof_current_proc(LABEL(mercury__cse_detection__construct_common_unify_8_0));
	r5 = (Integer) detstackvar(3);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r5;
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__goal_util__rename_vars_in_goal_3_0);
	call_localret(ENTRY(mercury__goal_util__rename_vars_in_goal_3_0),
		mercury__cse_detection__construct_common_unify_8_0_i13,
		STATIC(mercury__cse_detection__construct_common_unify_8_0));
	}
Define_label(mercury__cse_detection__construct_common_unify_8_0_i13);
	update_prof_current_proc(LABEL(mercury__cse_detection__construct_common_unify_8_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__cse_detection__construct_common_unify_8_0_i1000);
	incr_sp_push_msg(7, "construct_common_unify");
	detstackvar(7) = (Integer) succip;
	r1 = string_const("unexpected goal in construct_common_unify", 41);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__cse_detection__construct_common_unify_8_0_i14,
		STATIC(mercury__cse_detection__construct_common_unify_8_0));
	}
Define_label(mercury__cse_detection__construct_common_unify_8_0_i14);
	update_prof_current_proc(LABEL(mercury__cse_detection__construct_common_unify_8_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module18)
	init_entry(mercury__cse_detection__create_parallel_subterms_9_0);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i4);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i5);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i6);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i7);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i8);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i9);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i3);
	init_label(mercury__cse_detection__create_parallel_subterms_9_0_i10);
BEGIN_CODE

/* code for predicate 'create_parallel_subterms'/9 in mode 0 */
Define_static(mercury__cse_detection__create_parallel_subterms_9_0);
	incr_sp_push_msg(9, "create_parallel_subterms");
	detstackvar(9) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__create_parallel_subterms_9_0_i3);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__cse_detection__create_parallel_subterms_9_0,
		LABEL(mercury__cse_detection__create_parallel_subterms_9_0_i4),
		STATIC(mercury__cse_detection__create_parallel_subterms_9_0));
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i4);
	update_prof_current_proc(LABEL(mercury__cse_detection__create_parallel_subterms_9_0));
	detstackvar(4) = (Integer) r2;
	detstackvar(6) = (Integer) r3;
	detstackvar(7) = (Integer) r4;
	{
	Declare_entry(mercury__varset__new_var_3_0);
	call_localret(ENTRY(mercury__varset__new_var_3_0),
		mercury__cse_detection__create_parallel_subterms_9_0_i5,
		STATIC(mercury__cse_detection__create_parallel_subterms_9_0));
	}
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i5);
	update_prof_current_proc(LABEL(mercury__cse_detection__create_parallel_subterms_9_0));
	detstackvar(3) = (Integer) r2;
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__cse_detection__create_parallel_subterms_9_0_i6,
		STATIC(mercury__cse_detection__create_parallel_subterms_9_0));
	}
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__create_parallel_subterms_9_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__cse_detection__create_parallel_subterms_9_0_i7,
		STATIC(mercury__cse_detection__create_parallel_subterms_9_0));
	}
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__create_parallel_subterms_9_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__cse_detection__create_parallel_subterms_9_0_i8,
		STATIC(mercury__cse_detection__create_parallel_subterms_9_0));
	}
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i8);
	update_prof_current_proc(LABEL(mercury__cse_detection__create_parallel_subterms_9_0));
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	detstackvar(1) = (Integer) r1;
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r5 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__make_hlds__create_atomic_unification_6_0);
	call_localret(ENTRY(mercury__make_hlds__create_atomic_unification_6_0),
		mercury__cse_detection__create_parallel_subterms_9_0_i9,
		STATIC(mercury__cse_detection__create_parallel_subterms_9_0));
	}
	}
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__create_parallel_subterms_9_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) r5;
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i3);
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__cse_detection__create_parallel_subterms_9_0_i10,
		STATIC(mercury__cse_detection__create_parallel_subterms_9_0));
	}
Define_label(mercury__cse_detection__create_parallel_subterms_9_0_i10);
	update_prof_current_proc(LABEL(mercury__cse_detection__create_parallel_subterms_9_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module19)
	init_entry(mercury__cse_detection__find_similar_deconstruct_4_0);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i7);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i9);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i10);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i11);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i1003);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i2);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i12);
	init_label(mercury__cse_detection__find_similar_deconstruct_4_0_i1);
BEGIN_CODE

/* code for predicate 'find_similar_deconstruct'/4 in mode 0 */
Define_static(mercury__cse_detection__find_similar_deconstruct_4_0);
	{
	Word tempr1, tempr2, tempr3;
	tempr1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) tempr1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__cse_detection__find_similar_deconstruct_4_0_i1003);
	if (((Integer) field(mktag(3), (Integer) tempr1, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__cse_detection__find_similar_deconstruct_4_0_i1003);
	tempr3 = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 4));
	if ((tag((Integer) tempr3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_similar_deconstruct_4_0_i1003);
	incr_sp_push_msg(6, "find_similar_deconstruct");
	detstackvar(6) = (Integer) succip;
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__cse_detection__find_similar_deconstruct_4_0_i2);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 5));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) tempr3, ((Integer) 2));
	r1 = (Integer) field(mktag(1), (Integer) tempr3, ((Integer) 1));
	detstackvar(1) = (Integer) r3;
	{
	Declare_entry(mercury____Unify___hlds_data__cons_id_0_0);
	call_localret(ENTRY(mercury____Unify___hlds_data__cons_id_0_0),
		mercury__cse_detection__find_similar_deconstruct_4_0_i7,
		STATIC(mercury__cse_detection__find_similar_deconstruct_4_0));
	}
	}
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i7);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_similar_deconstruct_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__find_similar_deconstruct_4_0_i1);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__cse_detection__find_similar_deconstruct_4_0_i9,
		STATIC(mercury__cse_detection__find_similar_deconstruct_4_0));
	}
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_similar_deconstruct_4_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__cse_detection__find_similar_deconstruct_4_0_i10,
		STATIC(mercury__cse_detection__find_similar_deconstruct_4_0));
	}
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i10);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_similar_deconstruct_4_0));
	if (((Integer) detstackvar(5) != (Integer) r1))
		GOTO_LABEL(mercury__cse_detection__find_similar_deconstruct_4_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__cse_detection__pair_subterms_5_0),
		mercury__cse_detection__find_similar_deconstruct_4_0_i11,
		STATIC(mercury__cse_detection__find_similar_deconstruct_4_0));
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i11);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_similar_deconstruct_4_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i1003);
	incr_sp_push_msg(6, "find_similar_deconstruct");
	detstackvar(6) = (Integer) succip;
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i2);
	r1 = string_const("find_similar_deconstruct: non-deconstruct unify", 47);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__cse_detection__find_similar_deconstruct_4_0_i12,
		STATIC(mercury__cse_detection__find_similar_deconstruct_4_0));
	}
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i12);
	update_prof_current_proc(LABEL(mercury__cse_detection__find_similar_deconstruct_4_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__cse_detection__find_similar_deconstruct_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__cse_detection_module20)
	init_entry(mercury__cse_detection__pair_subterms_5_0);
	init_label(mercury__cse_detection__pair_subterms_5_0_i6);
	init_label(mercury__cse_detection__pair_subterms_5_0_i9);
	init_label(mercury__cse_detection__pair_subterms_5_0_i8);
	init_label(mercury__cse_detection__pair_subterms_5_0_i11);
	init_label(mercury__cse_detection__pair_subterms_5_0_i1009);
	init_label(mercury__cse_detection__pair_subterms_5_0_i1007);
	init_label(mercury__cse_detection__pair_subterms_5_0_i13);
	init_label(mercury__cse_detection__pair_subterms_5_0_i1008);
BEGIN_CODE

/* code for predicate 'pair_subterms'/5 in mode 0 */
Define_static(mercury__cse_detection__pair_subterms_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__pair_subterms_5_0_i1009);
	incr_sp_push_msg(6, "pair_subterms");
	detstackvar(6) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__pair_subterms_5_0_i1007);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__cse_detection__pair_subterms_5_0,
		LABEL(mercury__cse_detection__pair_subterms_5_0_i6),
		STATIC(mercury__cse_detection__pair_subterms_5_0));
Define_label(mercury__cse_detection__pair_subterms_5_0_i6);
	update_prof_current_proc(LABEL(mercury__cse_detection__pair_subterms_5_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury____Unify___mercury_builtin__var_0_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__var_0_0),
		mercury__cse_detection__pair_subterms_5_0_i9,
		STATIC(mercury__cse_detection__pair_subterms_5_0));
	}
Define_label(mercury__cse_detection__pair_subterms_5_0_i9);
	update_prof_current_proc(LABEL(mercury__cse_detection__pair_subterms_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__cse_detection__pair_subterms_5_0_i8);
	r1 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__cse_detection__pair_subterms_5_0_i8);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r5 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__make_hlds__create_atomic_unification_6_0);
	call_localret(ENTRY(mercury__make_hlds__create_atomic_unification_6_0),
		mercury__cse_detection__pair_subterms_5_0_i11,
		STATIC(mercury__cse_detection__pair_subterms_5_0));
	}
	}
Define_label(mercury__cse_detection__pair_subterms_5_0_i11);
	update_prof_current_proc(LABEL(mercury__cse_detection__pair_subterms_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__cse_detection__pair_subterms_5_0_i1009);
	incr_sp_push_msg(6, "pair_subterms");
	detstackvar(6) = (Integer) succip;
Define_label(mercury__cse_detection__pair_subterms_5_0_i1007);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__pair_subterms_5_0_i13);
	decr_sp_pop_msg(6);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__cse_detection__pair_subterms_5_0_i1008);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__cse_detection__pair_subterms_5_0_i13);
	r1 = string_const("mismatched length lists in pair_subterms", 40);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__cse_detection__pair_subterms_5_0));
	}
Define_label(mercury__cse_detection__pair_subterms_5_0_i1008);
	r1 = string_const("mismatched length lists in pair_subterms", 40);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__cse_detection__pair_subterms_5_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__cse_detection_bunch_0(void)
{
	mercury__cse_detection_module0();
	mercury__cse_detection_module1();
	mercury__cse_detection_module2();
	mercury__cse_detection_module3();
	mercury__cse_detection_module4();
	mercury__cse_detection_module5();
	mercury__cse_detection_module6();
	mercury__cse_detection_module7();
	mercury__cse_detection_module8();
	mercury__cse_detection_module9();
	mercury__cse_detection_module10();
	mercury__cse_detection_module11();
	mercury__cse_detection_module12();
	mercury__cse_detection_module13();
	mercury__cse_detection_module14();
	mercury__cse_detection_module15();
	mercury__cse_detection_module16();
	mercury__cse_detection_module17();
	mercury__cse_detection_module18();
	mercury__cse_detection_module19();
	mercury__cse_detection_module20();
}

#endif

void mercury__cse_detection__init(void); /* suppress gcc warning */
void mercury__cse_detection__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__cse_detection_bunch_0();
#endif
}
