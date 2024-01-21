/*
** Automatically generated from `clause_to_proc.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__clause_to_proc__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0);
Declare_label(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i2);
Declare_label(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i3);
Define_extern_entry(mercury__clause_to_proc__copy_clauses_to_procs_2_0);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i2);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i3);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i4);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i5);
Define_extern_entry(mercury__clause_to_proc__copy_clauses_to_proc_4_0);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i2);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i3);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i4);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i8);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i12);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i9);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i13);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i14);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i15);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i16);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i17);
Declare_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i18);
Define_extern_entry(mercury__clause_to_proc__maybe_add_default_modes_3_0);
Declare_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i4);
Declare_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i5);
Declare_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i6);
Declare_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i1002);
Define_extern_entry(mercury__clause_to_proc__maybe_add_default_mode_2_0);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i2);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i3);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i7);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i9);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i10);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i11);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i12);
Declare_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i5);
Declare_static(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0);
Declare_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i4);
Declare_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i5);
Declare_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i6);
Declare_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i1002);
Declare_static(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i4);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i5);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i6);
Declare_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i1002);
Declare_static(mercury__clause_to_proc__select_matching_clauses_3_0);
Declare_label(mercury__clause_to_proc__select_matching_clauses_3_0_i4);
Declare_label(mercury__clause_to_proc__select_matching_clauses_3_0_i5);
Declare_label(mercury__clause_to_proc__select_matching_clauses_3_0_i10);
Declare_label(mercury__clause_to_proc__select_matching_clauses_3_0_i9);
Declare_label(mercury__clause_to_proc__select_matching_clauses_3_0_i1005);
Declare_static(mercury__clause_to_proc__get_clause_goals_2_0);
Declare_label(mercury__clause_to_proc__get_clause_goals_2_0_i4);
Declare_label(mercury__clause_to_proc__get_clause_goals_2_0_i5);
Declare_label(mercury__clause_to_proc__get_clause_goals_2_0_i1002);

Word * mercury_data_clause_to_proc__common_0[] = {
	(Word *) string_const("mercury_builtin", 15),
	(Word *) string_const("out", 3)
};

Word * mercury_data_clause_to_proc__common_1[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_clause_to_proc__common_0),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_clause_to_proc__common_2[] = {
	(Word *) string_const("mercury_builtin", 15),
	(Word *) string_const("in", 2)
};

Word * mercury_data_clause_to_proc__common_3[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_clause_to_proc__common_2),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_clause_to_proc__common_4[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_clause_to_proc__common_1),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word mercury_data_clause_to_proc__common_5[] = {
	((Integer) 0)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_info_0[];
Word * mercury_data_clause_to_proc__common_6[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_info_0
};

BEGIN_MODULE(mercury__clause_to_proc_module0)
	init_entry(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0);
	init_label(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i2);
	init_label(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i3);
BEGIN_CODE

/* code for predicate 'copy_module_clauses_to_procs'/3 in mode 0 */
Define_entry(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0);
	incr_sp_push_msg(3, "copy_module_clauses_to_procs");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i2,
		ENTRY(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0));
	}
Define_label(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i2);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0),
		mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i3,
		ENTRY(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0));
Define_label(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0_i3);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__hlds_module__module_info_set_preds_3_0);
	tailcall(ENTRY(mercury__hlds_module__module_info_set_preds_3_0),
		ENTRY(mercury__clause_to_proc__copy_module_clauses_to_procs_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module1)
	init_entry(mercury__clause_to_proc__copy_clauses_to_procs_2_0);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i2);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i3);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i4);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i5);
BEGIN_CODE

/* code for predicate 'copy_clauses_to_procs'/2 in mode 0 */
Define_entry(mercury__clause_to_proc__copy_clauses_to_procs_2_0);
	incr_sp_push_msg(4, "copy_clauses_to_procs");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__clause_to_proc__copy_clauses_to_procs_2_0_i2,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i2);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__pred_info_clauses_info_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_clauses_info_2_0),
		mercury__clause_to_proc__copy_clauses_to_procs_2_0_i3,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i3);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__pred_info_non_imported_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_non_imported_procids_2_0),
		mercury__clause_to_proc__copy_clauses_to_procs_2_0_i4,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i4);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0),
		mercury__clause_to_proc__copy_clauses_to_procs_2_0_i5,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_0_i5);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_procedures_3_0);
	tailcall(ENTRY(mercury__hlds_pred__pred_info_set_procedures_3_0),
		ENTRY(mercury__clause_to_proc__copy_clauses_to_procs_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module2)
	init_entry(mercury__clause_to_proc__copy_clauses_to_proc_4_0);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i2);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i3);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i4);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i8);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i12);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i9);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i13);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i14);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i15);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i16);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i17);
	init_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i18);
BEGIN_CODE

/* code for predicate 'copy_clauses_to_proc'/4 in mode 0 */
Define_entry(mercury__clause_to_proc__copy_clauses_to_proc_4_0);
	incr_sp_push_msg(7, "copy_clauses_to_proc");
	detstackvar(7) = (Integer) succip;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	r2 = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	call_localret(STATIC(mercury__clause_to_proc__select_matching_clauses_3_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i2,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i2);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	call_localret(STATIC(mercury__clause_to_proc__get_clause_goals_2_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i3,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i3);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i4);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i4);
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_body_6_0);
	tailcall(ENTRY(mercury__hlds_pred__proc_info_set_body_6_0),
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i4);
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_goal__goal_info_init_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_init_1_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i8,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i8);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	if (((Integer) detstackvar(5) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i9);
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) detstackvar(5), ((Integer) 0)), ((Integer) 1));
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_context_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_context_2_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i12,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i12);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i14);
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i9);
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__proc_info_context_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_context_2_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i13,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i13);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i14);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_context_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_context_3_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i15,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i15);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	detstackvar(6) = (Integer) r1;
	{
	extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	}
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	call_localret(ENTRY(mercury__set__list_to_set_2_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i16,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i16);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_nonlocals_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_nonlocals_3_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i17,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i17);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	detstackvar(6) = (Integer) r1;
	{
	extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_lval_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_lval_0;
	}
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__clause_to_proc__copy_clauses_to_proc_4_0_i18,
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_proc_4_0_i18);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) r6;
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_body_6_0);
	tailcall(ENTRY(mercury__hlds_pred__proc_info_set_body_6_0),
		ENTRY(mercury__clause_to_proc__copy_clauses_to_proc_4_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module3)
	init_entry(mercury__clause_to_proc__maybe_add_default_modes_3_0);
	init_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i4);
	init_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i5);
	init_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i6);
	init_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i1002);
BEGIN_CODE

/* code for predicate 'maybe_add_default_modes'/3 in mode 0 */
Define_entry(mercury__clause_to_proc__maybe_add_default_modes_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__maybe_add_default_modes_3_0_i1002);
	incr_sp_push_msg(4, "maybe_add_default_modes");
	detstackvar(4) = (Integer) succip;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__clause_to_proc__maybe_add_default_modes_3_0_i4,
		ENTRY(mercury__clause_to_proc__maybe_add_default_modes_3_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i4);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_modes_3_0));
	{
		call_localret(STATIC(mercury__clause_to_proc__maybe_add_default_mode_2_0),
		mercury__clause_to_proc__maybe_add_default_modes_3_0_i5,
		ENTRY(mercury__clause_to_proc__maybe_add_default_modes_3_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i5);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_modes_3_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__clause_to_proc__maybe_add_default_modes_3_0_i6,
		ENTRY(mercury__clause_to_proc__maybe_add_default_modes_3_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i6);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_modes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__clause_to_proc__maybe_add_default_modes_3_0,
		ENTRY(mercury__clause_to_proc__maybe_add_default_modes_3_0));
Define_label(mercury__clause_to_proc__maybe_add_default_modes_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module4)
	init_entry(mercury__clause_to_proc__maybe_add_default_mode_2_0);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i2);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i3);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i7);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i9);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i10);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i11);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i12);
	init_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i5);
BEGIN_CODE

/* code for predicate 'maybe_add_default_mode'/2 in mode 0 */
Define_entry(mercury__clause_to_proc__maybe_add_default_mode_2_0);
	incr_sp_push_msg(4, "maybe_add_default_mode");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__clause_to_proc__maybe_add_default_mode_2_0_i2,
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i2);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__pred_info_get_is_pred_or_func_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_get_is_pred_or_func_2_0),
		mercury__clause_to_proc__maybe_add_default_mode_2_0_i3,
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i3);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0_i5);
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__is_empty_1_0);
	call_localret(ENTRY(mercury__map__is_empty_1_0),
		mercury__clause_to_proc__maybe_add_default_mode_2_0_i7,
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i7);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0_i5);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__pred_info_arity_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_arity_2_0),
		mercury__clause_to_proc__maybe_add_default_mode_2_0_i9,
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i9);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) mkword(mktag(1), (Integer) mercury_data_clause_to_proc__common_1);
	{
	extern Word * mercury_data_prog_data__base_type_info_mode_0[];
	r1 = (Integer) mercury_data_prog_data__base_type_info_mode_0;
	}
	r2 = ((Integer) detstackvar(2) - ((Integer) 1));
	r3 = (Integer) mkword(mktag(1), (Integer) mercury_data_clause_to_proc__common_3);
	{
	Declare_entry(mercury__list__duplicate_3_0);
	call_localret(ENTRY(mercury__list__duplicate_3_0),
		mercury__clause_to_proc__maybe_add_default_mode_2_0_i10,
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i10);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data_prog_data__base_type_info_mode_0[];
	r1 = (Integer) mercury_data_prog_data__base_type_info_mode_0;
	}
	r3 = (Integer) mkword(mktag(1), (Integer) mercury_data_clause_to_proc__common_4);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__clause_to_proc__maybe_add_default_mode_2_0_i11,
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i11);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__pred_info_context_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_context_2_0),
		mercury__clause_to_proc__maybe_add_default_mode_2_0_i12,
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i12);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r5 = (Integer) mkword(mktag(1), (Integer) mercury_data_clause_to_proc__common_5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__make_hlds__add_new_proc_8_0);
	tailcall(ENTRY(mercury__make_hlds__add_new_proc_8_0),
		ENTRY(mercury__clause_to_proc__maybe_add_default_mode_2_0));
	}
Define_label(mercury__clause_to_proc__maybe_add_default_mode_2_0_i5);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module5)
	init_entry(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0);
	init_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i4);
	init_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i5);
	init_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i6);
	init_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i1002);
BEGIN_CODE

/* code for predicate 'copy_module_clauses_to_procs_2'/3 in mode 0 */
Define_static(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i1002);
	incr_sp_push_msg(4, "copy_module_clauses_to_procs_2");
	detstackvar(4) = (Integer) succip;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i4,
		STATIC(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0));
	}
Define_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i4);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0));
	{
		call_localret(STATIC(mercury__clause_to_proc__copy_clauses_to_procs_2_0),
		mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i5,
		STATIC(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0));
	}
Define_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i5);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__det_update_4_0);
	call_localret(ENTRY(mercury__map__det_update_4_0),
		mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i6,
		STATIC(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0));
	}
Define_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i6);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0,
		STATIC(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0));
Define_label(mercury__clause_to_proc__copy_module_clauses_to_procs_2_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module6)
	init_entry(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i4);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i5);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i6);
	init_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i1002);
BEGIN_CODE

/* code for predicate 'copy_clauses_to_procs_2'/4 in mode 0 */
Define_static(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i1002);
	incr_sp_push_msg(5, "copy_clauses_to_procs_2");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i4,
		STATIC(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__clause_to_proc__copy_clauses_to_proc_4_0),
		mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i5,
		STATIC(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i5);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i6,
		STATIC(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0));
	}
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0,
		STATIC(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0));
Define_label(mercury__clause_to_proc__copy_clauses_to_procs_2_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module7)
	init_entry(mercury__clause_to_proc__select_matching_clauses_3_0);
	init_label(mercury__clause_to_proc__select_matching_clauses_3_0_i4);
	init_label(mercury__clause_to_proc__select_matching_clauses_3_0_i5);
	init_label(mercury__clause_to_proc__select_matching_clauses_3_0_i10);
	init_label(mercury__clause_to_proc__select_matching_clauses_3_0_i9);
	init_label(mercury__clause_to_proc__select_matching_clauses_3_0_i1005);
BEGIN_CODE

/* code for predicate 'select_matching_clauses'/3 in mode 0 */
Define_static(mercury__clause_to_proc__select_matching_clauses_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__select_matching_clauses_3_0_i1005);
	incr_sp_push_msg(4, "select_matching_clauses");
	detstackvar(4) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	localcall(mercury__clause_to_proc__select_matching_clauses_3_0,
		LABEL(mercury__clause_to_proc__select_matching_clauses_3_0_i4),
		STATIC(mercury__clause_to_proc__select_matching_clauses_3_0));
Define_label(mercury__clause_to_proc__select_matching_clauses_3_0_i4);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__select_matching_clauses_3_0));
	if (((Integer) detstackvar(3) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__select_matching_clauses_3_0_i5);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__clause_to_proc__select_matching_clauses_3_0_i5);
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__clause_to_proc__select_matching_clauses_3_0_i10,
		STATIC(mercury__clause_to_proc__select_matching_clauses_3_0));
	}
Define_label(mercury__clause_to_proc__select_matching_clauses_3_0_i10);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__select_matching_clauses_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__clause_to_proc__select_matching_clauses_3_0_i9);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__clause_to_proc__select_matching_clauses_3_0_i9);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__clause_to_proc__select_matching_clauses_3_0_i1005);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__clause_to_proc_module8)
	init_entry(mercury__clause_to_proc__get_clause_goals_2_0);
	init_label(mercury__clause_to_proc__get_clause_goals_2_0_i4);
	init_label(mercury__clause_to_proc__get_clause_goals_2_0_i5);
	init_label(mercury__clause_to_proc__get_clause_goals_2_0_i1002);
BEGIN_CODE

/* code for predicate 'get_clause_goals'/2 in mode 0 */
Define_static(mercury__clause_to_proc__get_clause_goals_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__clause_to_proc__get_clause_goals_2_0_i1002);
	incr_sp_push_msg(2, "get_clause_goals");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	{
	Declare_entry(mercury__hlds_goal__goal_to_disj_list_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_to_disj_list_2_0),
		mercury__clause_to_proc__get_clause_goals_2_0_i4,
		STATIC(mercury__clause_to_proc__get_clause_goals_2_0));
	}
Define_label(mercury__clause_to_proc__get_clause_goals_2_0_i4);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__get_clause_goals_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__clause_to_proc__get_clause_goals_2_0,
		LABEL(mercury__clause_to_proc__get_clause_goals_2_0_i5),
		STATIC(mercury__clause_to_proc__get_clause_goals_2_0));
Define_label(mercury__clause_to_proc__get_clause_goals_2_0_i5);
	update_prof_current_proc(LABEL(mercury__clause_to_proc__get_clause_goals_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_clause_to_proc__common_6);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__clause_to_proc__get_clause_goals_2_0));
	}
Define_label(mercury__clause_to_proc__get_clause_goals_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__clause_to_proc_bunch_0(void)
{
	mercury__clause_to_proc_module0();
	mercury__clause_to_proc_module1();
	mercury__clause_to_proc_module2();
	mercury__clause_to_proc_module3();
	mercury__clause_to_proc_module4();
	mercury__clause_to_proc_module5();
	mercury__clause_to_proc_module6();
	mercury__clause_to_proc_module7();
	mercury__clause_to_proc_module8();
}

#endif

void mercury__clause_to_proc__init(void); /* suppress gcc warning */
void mercury__clause_to_proc__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__clause_to_proc_bunch_0();
#endif
}
