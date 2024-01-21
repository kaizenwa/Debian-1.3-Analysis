/*
** Automatically generated from `constraint.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__constraint__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__constraint__constraint_propagation_4_0);
Declare_label(mercury__constraint__constraint_propagation_4_0_i2);
Declare_label(mercury__constraint__constraint_propagation_4_0_i3);
Declare_label(mercury__constraint__constraint_propagation_4_0_i4);
Declare_static(mercury__constraint__constraint_propagation2_5_0);
Declare_label(mercury__constraint__constraint_propagation2_5_0_i4);
Declare_label(mercury__constraint__constraint_propagation2_5_0_i1002);
Declare_static(mercury__constraint__constraint_propagation3_5_0);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i4);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i5);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i6);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i7);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i8);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i9);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i10);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i11);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i12);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i13);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i14);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i15);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i16);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i17);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i18);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i19);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i20);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i21);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i22);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i23);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i24);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i25);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i26);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i27);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i33);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i28);
Declare_label(mercury__constraint__constraint_propagation3_5_0_i1003);
Declare_static(mercury__constraint__propagate_goal_4_0);
Declare_label(mercury__constraint__propagate_goal_4_0_i2);
Declare_label(mercury__constraint__propagate_goal_4_0_i3);
Declare_label(mercury__constraint__propagate_goal_4_0_i4);
Declare_label(mercury__constraint__propagate_goal_4_0_i5);
Declare_label(mercury__constraint__propagate_goal_4_0_i6);
Declare_label(mercury__constraint__propagate_goal_4_0_i7);
Declare_static(mercury__constraint__propagate_goal_2_4_0);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1021);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1020);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1019);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1018);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1017);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1016);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1015);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i5);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i6);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i7);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i8);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i9);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i10);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i12);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i13);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1008);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i16);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i17);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1009);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i20);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i21);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i22);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i24);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i25);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i26);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i27);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i28);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i29);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i30);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i32);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i33);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1014);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i36);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i1013);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i35);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i40);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i39);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i42);
Declare_label(mercury__constraint__propagate_goal_2_4_0_i43);
Declare_static(mercury__constraint__propagate_disj_4_0);
Declare_label(mercury__constraint__propagate_disj_4_0_i4);
Declare_label(mercury__constraint__propagate_disj_4_0_i5);
Declare_label(mercury__constraint__propagate_disj_4_0_i6);
Declare_label(mercury__constraint__propagate_disj_4_0_i7);
Declare_label(mercury__constraint__propagate_disj_4_0_i1002);
Declare_static(mercury__constraint__propagate_cases_4_0);
Declare_label(mercury__constraint__propagate_cases_4_0_i4);
Declare_label(mercury__constraint__propagate_cases_4_0_i5);
Declare_label(mercury__constraint__propagate_cases_4_0_i6);
Declare_label(mercury__constraint__propagate_cases_4_0_i7);
Declare_label(mercury__constraint__propagate_cases_4_0_i1003);
Declare_static(mercury__constraint__propagate_conj_4_0);
Declare_label(mercury__constraint__propagate_conj_4_0_i2);
Declare_label(mercury__constraint__propagate_conj_4_0_i3);
Declare_label(mercury__constraint__propagate_conj_4_0_i4);
Declare_label(mercury__constraint__propagate_conj_4_0_i5);
Declare_label(mercury__constraint__propagate_conj_4_0_i6);
Declare_label(mercury__constraint__propagate_conj_4_0_i7);
Declare_label(mercury__constraint__propagate_conj_4_0_i8);
Declare_label(mercury__constraint__propagate_conj_4_0_i9);
Declare_label(mercury__constraint__propagate_conj_4_0_i10);
Declare_label(mercury__constraint__propagate_conj_4_0_i11);
Declare_label(mercury__constraint__propagate_conj_4_0_i12);
Declare_label(mercury__constraint__propagate_conj_4_0_i13);
Declare_label(mercury__constraint__propagate_conj_4_0_i16);
Declare_static(mercury__constraint__find_constraints_5_0);
Declare_label(mercury__constraint__find_constraints_5_0_i4);
Declare_label(mercury__constraint__find_constraints_5_0_i5);
Declare_label(mercury__constraint__find_constraints_5_0_i9);
Declare_label(mercury__constraint__find_constraints_5_0_i10);
Declare_label(mercury__constraint__find_constraints_5_0_i6);
Declare_label(mercury__constraint__find_constraints_5_0_i12);
Declare_label(mercury__constraint__find_constraints_5_0_i15);
Declare_label(mercury__constraint__find_constraints_5_0_i17);
Declare_label(mercury__constraint__find_constraints_5_0_i16);
Declare_label(mercury__constraint__find_constraints_5_0_i19);
Declare_label(mercury__constraint__find_constraints_5_0_i20);
Declare_label(mercury__constraint__find_constraints_5_0_i21);
Declare_label(mercury__constraint__find_constraints_5_0_i22);
Declare_label(mercury__constraint__find_constraints_5_0_i23);
Declare_label(mercury__constraint__find_constraints_5_0_i1013);
Declare_label(mercury__constraint__find_constraints_5_0_i1007);

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_constraint__base_type_layout_constraint_0[];
Word * mercury_data_constraint__base_type_info_constraint_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_constraint__base_type_layout_constraint_0
};

extern Word * mercury_data_constraint__common_1[];
Word * mercury_data_constraint__base_type_layout_constraint_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_constraint__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_constraint__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_constraint__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_constraint__common_1)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_info_0[];
Word * mercury_data_constraint__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_info_0
};

Word * mercury_data_constraint__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_constraint__common_0)
};

BEGIN_MODULE(mercury__constraint_module0)
	init_entry(mercury__constraint__constraint_propagation_4_0);
	init_label(mercury__constraint__constraint_propagation_4_0_i2);
	init_label(mercury__constraint__constraint_propagation_4_0_i3);
	init_label(mercury__constraint__constraint_propagation_4_0_i4);
BEGIN_CODE

/* code for predicate 'constraint_propagation'/4 in mode 0 */
Define_entry(mercury__constraint__constraint_propagation_4_0);
	incr_sp_push_msg(3, "constraint_propagation");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__dependency_graph__module_info_ensure_dependency_info_2_0);
	call_localret(ENTRY(mercury__dependency_graph__module_info_ensure_dependency_info_2_0),
		mercury__constraint__constraint_propagation_4_0_i2,
		ENTRY(mercury__constraint__constraint_propagation_4_0));
	}
Define_label(mercury__constraint__constraint_propagation_4_0_i2);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation_4_0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_module__module_info_dependency_info_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_dependency_info_2_0),
		mercury__constraint__constraint_propagation_4_0_i3,
		ENTRY(mercury__constraint__constraint_propagation_4_0));
	}
Define_label(mercury__constraint__constraint_propagation_4_0_i3);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation_4_0));
	{
	Declare_entry(mercury__hlds_module__hlds__dependency_info_get_dependency_ordering_2_0);
	call_localret(ENTRY(mercury__hlds_module__hlds__dependency_info_get_dependency_ordering_2_0),
		mercury__constraint__constraint_propagation_4_0_i4,
		ENTRY(mercury__constraint__constraint_propagation_4_0));
	}
Define_label(mercury__constraint__constraint_propagation_4_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation_4_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__constraint__constraint_propagation2_5_0),
		ENTRY(mercury__constraint__constraint_propagation_4_0));
END_MODULE

BEGIN_MODULE(mercury__constraint_module1)
	init_entry(mercury__constraint__constraint_propagation2_5_0);
	init_label(mercury__constraint__constraint_propagation2_5_0_i4);
	init_label(mercury__constraint__constraint_propagation2_5_0_i1002);
BEGIN_CODE

/* code for predicate 'constraint_propagation2'/5 in mode 0 */
Define_static(mercury__constraint__constraint_propagation2_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__constraint__constraint_propagation2_5_0_i1002);
	incr_sp_push_msg(2, "constraint_propagation2");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__constraint__constraint_propagation3_5_0),
		mercury__constraint__constraint_propagation2_5_0_i4,
		STATIC(mercury__constraint__constraint_propagation2_5_0));
Define_label(mercury__constraint__constraint_propagation2_5_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation2_5_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__constraint__constraint_propagation2_5_0,
		STATIC(mercury__constraint__constraint_propagation2_5_0));
Define_label(mercury__constraint__constraint_propagation2_5_0_i1002);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__constraint_module2)
	init_entry(mercury__constraint__constraint_propagation3_5_0);
	init_label(mercury__constraint__constraint_propagation3_5_0_i4);
	init_label(mercury__constraint__constraint_propagation3_5_0_i5);
	init_label(mercury__constraint__constraint_propagation3_5_0_i6);
	init_label(mercury__constraint__constraint_propagation3_5_0_i7);
	init_label(mercury__constraint__constraint_propagation3_5_0_i8);
	init_label(mercury__constraint__constraint_propagation3_5_0_i9);
	init_label(mercury__constraint__constraint_propagation3_5_0_i10);
	init_label(mercury__constraint__constraint_propagation3_5_0_i11);
	init_label(mercury__constraint__constraint_propagation3_5_0_i12);
	init_label(mercury__constraint__constraint_propagation3_5_0_i13);
	init_label(mercury__constraint__constraint_propagation3_5_0_i14);
	init_label(mercury__constraint__constraint_propagation3_5_0_i15);
	init_label(mercury__constraint__constraint_propagation3_5_0_i16);
	init_label(mercury__constraint__constraint_propagation3_5_0_i17);
	init_label(mercury__constraint__constraint_propagation3_5_0_i18);
	init_label(mercury__constraint__constraint_propagation3_5_0_i19);
	init_label(mercury__constraint__constraint_propagation3_5_0_i20);
	init_label(mercury__constraint__constraint_propagation3_5_0_i21);
	init_label(mercury__constraint__constraint_propagation3_5_0_i22);
	init_label(mercury__constraint__constraint_propagation3_5_0_i23);
	init_label(mercury__constraint__constraint_propagation3_5_0_i24);
	init_label(mercury__constraint__constraint_propagation3_5_0_i25);
	init_label(mercury__constraint__constraint_propagation3_5_0_i26);
	init_label(mercury__constraint__constraint_propagation3_5_0_i27);
	init_label(mercury__constraint__constraint_propagation3_5_0_i33);
	init_label(mercury__constraint__constraint_propagation3_5_0_i28);
	init_label(mercury__constraint__constraint_propagation3_5_0_i1003);
BEGIN_CODE

/* code for predicate 'constraint_propagation3'/5 in mode 0 */
Define_static(mercury__constraint__constraint_propagation3_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__constraint__constraint_propagation3_5_0_i1003);
	incr_sp_push_msg(13, "constraint_propagation3");
	detstackvar(13) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__constraint__constraint_propagation3_5_0_i4,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
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
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__constraint__constraint_propagation3_5_0_i5,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i5);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__constraint__constraint_propagation3_5_0_i6,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i6);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
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
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__constraint__constraint_propagation3_5_0_i7,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i7);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(9) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__proc_info_goal_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_goal_2_0),
		mercury__constraint__constraint_propagation3_5_0_i8,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i8);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_pred__proc_info_variables_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_variables_2_0),
		mercury__constraint__constraint_propagation3_5_0_i9,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i9);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	{
	Declare_entry(mercury__varset__vars_2_0);
	call_localret(ENTRY(mercury__varset__vars_2_0),
		mercury__constraint__constraint_propagation3_5_0_i10,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i10);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	}
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	call_localret(ENTRY(mercury__set__list_to_set_2_0),
		mercury__constraint__constraint_propagation3_5_0_i11,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i11);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__proc_info_get_initial_instmap_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_get_initial_instmap_3_0),
		mercury__constraint__constraint_propagation3_5_0_i12,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i12);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(12) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_pred__proc_info_context_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_context_2_0),
		mercury__constraint__constraint_propagation3_5_0_i13,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i13);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(11);
	r7 = (Integer) detstackvar(12);
	{
	Declare_entry(mercury__mode_info__mode_info_init_8_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_init_8_0),
		mercury__constraint__constraint_propagation3_5_0_i14,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i14);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__constraint_propagation3_5_0_i15,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
Define_label(mercury__constraint__constraint_propagation3_5_0_i15);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(10) = (Integer) r2;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_get_io_state_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_io_state_2_0),
		mercury__constraint__constraint_propagation3_5_0_i16,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i16);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__mode_info__mode_info_get_varset_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_varset_2_0),
		mercury__constraint__constraint_propagation3_5_0_i17,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i17);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__mode_info__mode_info_get_var_types_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_var_types_2_0),
		mercury__constraint__constraint_propagation3_5_0_i18,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i18);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) detstackvar(10);
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_get_module_info_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_module_info_2_0),
		mercury__constraint__constraint_propagation3_5_0_i19,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i19);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) detstackvar(9);
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(11);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_variables_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_variables_3_0),
		mercury__constraint__constraint_propagation3_5_0_i20,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i20);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_vartypes_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_vartypes_3_0),
		mercury__constraint__constraint_propagation3_5_0_i21,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i21);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_goal_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_goal_3_0),
		mercury__constraint__constraint_propagation3_5_0_i22,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i22);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
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
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__constraint__constraint_propagation3_5_0_i23,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i23);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_procedures_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_procedures_3_0),
		mercury__constraint__constraint_propagation3_5_0_i24,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i24);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
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
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__constraint__constraint_propagation3_5_0_i25,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i25);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_module__module_info_set_preds_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_set_preds_3_0),
		mercury__constraint__constraint_propagation3_5_0_i26,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i26);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__modes__modecheck_proc_7_0);
	call_localret(ENTRY(mercury__modes__modecheck_proc_7_0),
		mercury__constraint__constraint_propagation3_5_0_i27,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i27);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	if (((Integer) r2 == ((Integer) 0)))
		GOTO_LABEL(mercury__constraint__constraint_propagation3_5_0_i28);
	detstackvar(1) = (Integer) r1;
	r1 = string_const("constraint_propagation3", 23);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__constraint__constraint_propagation3_5_0_i33,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
	}
Define_label(mercury__constraint__constraint_propagation3_5_0_i33);
	update_prof_current_proc(LABEL(mercury__constraint__constraint_propagation3_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	localtailcall(mercury__constraint__constraint_propagation3_5_0,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
Define_label(mercury__constraint__constraint_propagation3_5_0_i28);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	localtailcall(mercury__constraint__constraint_propagation3_5_0,
		STATIC(mercury__constraint__constraint_propagation3_5_0));
Define_label(mercury__constraint__constraint_propagation3_5_0_i1003);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__constraint_module3)
	init_entry(mercury__constraint__propagate_goal_4_0);
	init_label(mercury__constraint__propagate_goal_4_0_i2);
	init_label(mercury__constraint__propagate_goal_4_0_i3);
	init_label(mercury__constraint__propagate_goal_4_0_i4);
	init_label(mercury__constraint__propagate_goal_4_0_i5);
	init_label(mercury__constraint__propagate_goal_4_0_i6);
	init_label(mercury__constraint__propagate_goal_4_0_i7);
BEGIN_CODE

/* code for predicate 'constraint__propagate_goal'/4 in mode 0 */
Define_static(mercury__constraint__propagate_goal_4_0);
	incr_sp_push_msg(5, "constraint__propagate_goal");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_dcg_get_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_dcg_get_instmap_3_0),
		mercury__constraint__propagate_goal_4_0_i2,
		STATIC(mercury__constraint__propagate_goal_4_0));
	}
Define_label(mercury__constraint__propagate_goal_4_0_i2);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_4_0));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_instmap_delta_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_instmap_delta_2_0),
		mercury__constraint__propagate_goal_4_0_i3,
		STATIC(mercury__constraint__propagate_goal_4_0));
	}
Define_label(mercury__constraint__propagate_goal_4_0_i3);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__instmap__apply_instmap_delta_3_0);
	call_localret(ENTRY(mercury__instmap__apply_instmap_delta_3_0),
		mercury__constraint__propagate_goal_4_0_i4,
		STATIC(mercury__constraint__propagate_goal_4_0));
	}
Define_label(mercury__constraint__propagate_goal_4_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_4_0));
	detstackvar(3) = (Integer) r1;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__constraint__propagate_goal_4_0_i5,
		STATIC(mercury__constraint__propagate_goal_4_0));
	}
Define_label(mercury__constraint__propagate_goal_4_0_i5);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__constraint__propagate_goal_2_4_0),
		mercury__constraint__propagate_goal_4_0_i6,
		STATIC(mercury__constraint__propagate_goal_4_0));
Define_label(mercury__constraint__propagate_goal_4_0_i6);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_4_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__constraint__propagate_goal_4_0_i7,
		STATIC(mercury__constraint__propagate_goal_4_0));
	}
Define_label(mercury__constraint__propagate_goal_4_0_i7);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__constraint_module4)
	init_entry(mercury__constraint__propagate_goal_2_4_0);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1021);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1020);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1019);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1018);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1017);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1016);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1015);
	init_label(mercury__constraint__propagate_goal_2_4_0_i5);
	init_label(mercury__constraint__propagate_goal_2_4_0_i6);
	init_label(mercury__constraint__propagate_goal_2_4_0_i7);
	init_label(mercury__constraint__propagate_goal_2_4_0_i8);
	init_label(mercury__constraint__propagate_goal_2_4_0_i9);
	init_label(mercury__constraint__propagate_goal_2_4_0_i10);
	init_label(mercury__constraint__propagate_goal_2_4_0_i12);
	init_label(mercury__constraint__propagate_goal_2_4_0_i13);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1008);
	init_label(mercury__constraint__propagate_goal_2_4_0_i16);
	init_label(mercury__constraint__propagate_goal_2_4_0_i17);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1009);
	init_label(mercury__constraint__propagate_goal_2_4_0_i20);
	init_label(mercury__constraint__propagate_goal_2_4_0_i21);
	init_label(mercury__constraint__propagate_goal_2_4_0_i22);
	init_label(mercury__constraint__propagate_goal_2_4_0_i24);
	init_label(mercury__constraint__propagate_goal_2_4_0_i25);
	init_label(mercury__constraint__propagate_goal_2_4_0_i26);
	init_label(mercury__constraint__propagate_goal_2_4_0_i27);
	init_label(mercury__constraint__propagate_goal_2_4_0_i28);
	init_label(mercury__constraint__propagate_goal_2_4_0_i29);
	init_label(mercury__constraint__propagate_goal_2_4_0_i30);
	init_label(mercury__constraint__propagate_goal_2_4_0_i32);
	init_label(mercury__constraint__propagate_goal_2_4_0_i33);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1014);
	init_label(mercury__constraint__propagate_goal_2_4_0_i36);
	init_label(mercury__constraint__propagate_goal_2_4_0_i1013);
	init_label(mercury__constraint__propagate_goal_2_4_0_i35);
	init_label(mercury__constraint__propagate_goal_2_4_0_i40);
	init_label(mercury__constraint__propagate_goal_2_4_0_i39);
	init_label(mercury__constraint__propagate_goal_2_4_0_i42);
	init_label(mercury__constraint__propagate_goal_2_4_0_i43);
BEGIN_CODE

/* code for predicate 'constraint__propagate_goal_2'/4 in mode 0 */
Define_static(mercury__constraint__propagate_goal_2_4_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i1014);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__constraint__propagate_goal_2_4_0_i1021) AND
		LABEL(mercury__constraint__propagate_goal_2_4_0_i1020) AND
		LABEL(mercury__constraint__propagate_goal_2_4_0_i1019) AND
		LABEL(mercury__constraint__propagate_goal_2_4_0_i1018) AND
		LABEL(mercury__constraint__propagate_goal_2_4_0_i1017) AND
		LABEL(mercury__constraint__propagate_goal_2_4_0_i1016) AND
		LABEL(mercury__constraint__propagate_goal_2_4_0_i1015));
Define_label(mercury__constraint__propagate_goal_2_4_0_i1021);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i5);
Define_label(mercury__constraint__propagate_goal_2_4_0_i1020);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i9);
Define_label(mercury__constraint__propagate_goal_2_4_0_i1019);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i12);
Define_label(mercury__constraint__propagate_goal_2_4_0_i1018);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i16);
Define_label(mercury__constraint__propagate_goal_2_4_0_i1017);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i20);
Define_label(mercury__constraint__propagate_goal_2_4_0_i1016);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i24);
Define_label(mercury__constraint__propagate_goal_2_4_0_i1015);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i32);
Define_label(mercury__constraint__propagate_goal_2_4_0_i5);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = ((Integer) 0);
	r2 = string_const("switch", 6);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i6,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__constraint__propagate_cases_4_0),
		mercury__constraint__propagate_goal_2_4_0_i7,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i7);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 5));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(1);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("switch", 6);
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 0);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i8);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__constraint__propagate_goal_2_4_0_i9);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	r1 = ((Integer) 0);
	r2 = string_const("unify", 5);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i10,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i10);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r3 = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("unify", 5);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i12);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = ((Integer) 0);
	r2 = string_const("disj", 4);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i13,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i13);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__constraint__propagate_disj_4_0),
		mercury__constraint__propagate_goal_2_4_0_i1008,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i1008);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("disj", 4);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i16);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = ((Integer) 0);
	r2 = string_const("not", 3);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i17,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i17);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__propagate_goal_2_4_0_i1009,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i1009);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("not", 3);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 3);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i20);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = ((Integer) 0);
	r2 = string_const("some", 4);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i21,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i21);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__propagate_goal_2_4_0_i22,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i22);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r3 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("some", 4);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 4);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i24);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r1 = ((Integer) 0);
	r2 = string_const("if_then_else", 12);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i25,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i25);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	{
	Declare_entry(mercury__mode_info__mode_info_dcg_get_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_dcg_get_instmap_3_0),
		mercury__constraint__propagate_goal_2_4_0_i26,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i26);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__propagate_goal_2_4_0_i27,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i27);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__propagate_goal_2_4_0_i28,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i28);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__constraint__propagate_goal_2_4_0_i29,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i29);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__propagate_goal_2_4_0_i30,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i30);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r3 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("if_then_else", 12);
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(3);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) tempr1, ((Integer) 5)) = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i32);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	r1 = ((Integer) 0);
	r2 = string_const("pragma_c_code", 13);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i33,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i33);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r3 = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("pragma_c_code", 13);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i1014);
	incr_sp_push_msg(7, "constraint__propagate_goal_2");
	detstackvar(7) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i35);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = ((Integer) 0);
	r2 = string_const("conj", 4);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i36,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i36);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__constraint__propagate_conj_4_0),
		mercury__constraint__propagate_goal_2_4_0_i1013,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
Define_label(mercury__constraint__propagate_goal_2_4_0_i1013);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("conj", 4);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i35);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__constraint__propagate_goal_2_4_0_i39);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	r1 = ((Integer) 0);
	r2 = string_const("call", 4);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i40,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i40);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r3 = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("call", 4);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i8,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i39);
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	r1 = ((Integer) 0);
	r2 = string_const("higher-order call", 17);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i42,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i42);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r3 = (Integer) r1;
	r1 = ((Integer) 1);
	r2 = string_const("higher-order call", 17);
	{
	Declare_entry(mercury__mode_debug__mode_checkpoint_4_0);
	call_localret(ENTRY(mercury__mode_debug__mode_checkpoint_4_0),
		mercury__constraint__propagate_goal_2_4_0_i43,
		STATIC(mercury__constraint__propagate_goal_2_4_0));
	}
Define_label(mercury__constraint__propagate_goal_2_4_0_i43);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_goal_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__constraint_module5)
	init_entry(mercury__constraint__propagate_disj_4_0);
	init_label(mercury__constraint__propagate_disj_4_0_i4);
	init_label(mercury__constraint__propagate_disj_4_0_i5);
	init_label(mercury__constraint__propagate_disj_4_0_i6);
	init_label(mercury__constraint__propagate_disj_4_0_i7);
	init_label(mercury__constraint__propagate_disj_4_0_i1002);
BEGIN_CODE

/* code for predicate 'constraint__propagate_disj'/4 in mode 0 */
Define_static(mercury__constraint__propagate_disj_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__constraint__propagate_disj_4_0_i1002);
	incr_sp_push_msg(3, "constraint__propagate_disj");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_dcg_get_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_dcg_get_instmap_3_0),
		mercury__constraint__propagate_disj_4_0_i4,
		STATIC(mercury__constraint__propagate_disj_4_0));
	}
Define_label(mercury__constraint__propagate_disj_4_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_disj_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__propagate_disj_4_0_i5,
		STATIC(mercury__constraint__propagate_disj_4_0));
Define_label(mercury__constraint__propagate_disj_4_0_i5);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_disj_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__constraint__propagate_disj_4_0_i6,
		STATIC(mercury__constraint__propagate_disj_4_0));
	}
Define_label(mercury__constraint__propagate_disj_4_0_i6);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_disj_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__constraint__propagate_disj_4_0,
		LABEL(mercury__constraint__propagate_disj_4_0_i7),
		STATIC(mercury__constraint__propagate_disj_4_0));
Define_label(mercury__constraint__propagate_disj_4_0_i7);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_disj_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__constraint__propagate_disj_4_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__constraint_module6)
	init_entry(mercury__constraint__propagate_cases_4_0);
	init_label(mercury__constraint__propagate_cases_4_0_i4);
	init_label(mercury__constraint__propagate_cases_4_0_i5);
	init_label(mercury__constraint__propagate_cases_4_0_i6);
	init_label(mercury__constraint__propagate_cases_4_0_i7);
	init_label(mercury__constraint__propagate_cases_4_0_i1003);
BEGIN_CODE

/* code for predicate 'constraint__propagate_cases'/4 in mode 0 */
Define_static(mercury__constraint__propagate_cases_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__constraint__propagate_cases_4_0_i1003);
	incr_sp_push_msg(4, "constraint__propagate_cases");
	detstackvar(4) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_dcg_get_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_dcg_get_instmap_3_0),
		mercury__constraint__propagate_cases_4_0_i4,
		STATIC(mercury__constraint__propagate_cases_4_0));
	}
Define_label(mercury__constraint__propagate_cases_4_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_cases_4_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__propagate_cases_4_0_i5,
		STATIC(mercury__constraint__propagate_cases_4_0));
Define_label(mercury__constraint__propagate_cases_4_0_i5);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_cases_4_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__constraint__propagate_cases_4_0_i6,
		STATIC(mercury__constraint__propagate_cases_4_0));
	}
Define_label(mercury__constraint__propagate_cases_4_0_i6);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_cases_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	localcall(mercury__constraint__propagate_cases_4_0,
		LABEL(mercury__constraint__propagate_cases_4_0_i7),
		STATIC(mercury__constraint__propagate_cases_4_0));
Define_label(mercury__constraint__propagate_cases_4_0_i7);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_cases_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__constraint__propagate_cases_4_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__constraint_module7)
	init_entry(mercury__constraint__propagate_conj_4_0);
	init_label(mercury__constraint__propagate_conj_4_0_i2);
	init_label(mercury__constraint__propagate_conj_4_0_i3);
	init_label(mercury__constraint__propagate_conj_4_0_i4);
	init_label(mercury__constraint__propagate_conj_4_0_i5);
	init_label(mercury__constraint__propagate_conj_4_0_i6);
	init_label(mercury__constraint__propagate_conj_4_0_i7);
	init_label(mercury__constraint__propagate_conj_4_0_i8);
	init_label(mercury__constraint__propagate_conj_4_0_i9);
	init_label(mercury__constraint__propagate_conj_4_0_i10);
	init_label(mercury__constraint__propagate_conj_4_0_i11);
	init_label(mercury__constraint__propagate_conj_4_0_i12);
	init_label(mercury__constraint__propagate_conj_4_0_i13);
	init_label(mercury__constraint__propagate_conj_4_0_i16);
BEGIN_CODE

/* code for predicate 'constraint__propagate_conj'/4 in mode 0 */
Define_static(mercury__constraint__propagate_conj_4_0);
	incr_sp_push_msg(3, "constraint__propagate_conj");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_get_delay_info_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_delay_info_2_0),
		mercury__constraint__propagate_conj_4_0_i2,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i2);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	{
	Declare_entry(mercury__delay_info__enter_conj_2_0);
	call_localret(ENTRY(mercury__delay_info__enter_conj_2_0),
		mercury__constraint__propagate_conj_4_0_i3,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i3);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__mode_info__mode_info_set_delay_info_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_delay_info_3_0),
		mercury__constraint__propagate_conj_4_0_i4,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	{
	Declare_entry(mercury__mode_info__mode_info_dcg_get_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_dcg_get_instmap_3_0),
		mercury__constraint__propagate_conj_4_0_i5,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i5);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__constraint__find_constraints_5_0),
		mercury__constraint__propagate_conj_4_0_i6,
		STATIC(mercury__constraint__propagate_conj_4_0));
Define_label(mercury__constraint__propagate_conj_4_0_i6);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__constraint__propagate_conj_4_0_i7,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
	}
Define_label(mercury__constraint__propagate_conj_4_0_i7);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_constraint__common_0);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__constraint__propagate_conj_4_0_i8,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i8);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__transform__reschedule_conj_4_0);
	call_localret(ENTRY(mercury__transform__reschedule_conj_4_0),
		mercury__constraint__propagate_conj_4_0_i9,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i9);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_get_delay_info_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_delay_info_2_0),
		mercury__constraint__propagate_conj_4_0_i10,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i10);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	{
	Declare_entry(mercury__delay_info__leave_conj_3_0);
	call_localret(ENTRY(mercury__delay_info__leave_conj_3_0),
		mercury__constraint__propagate_conj_4_0_i11,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i11);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__mode_info__mode_info_set_delay_info_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_delay_info_3_0),
		mercury__constraint__propagate_conj_4_0_i12,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i12);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__constraint__propagate_conj_4_0_i13);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__constraint__propagate_conj_4_0_i13);
	r1 = string_const("constraint__propagate_conj", 26);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__constraint__propagate_conj_4_0_i16,
		STATIC(mercury__constraint__propagate_conj_4_0));
	}
Define_label(mercury__constraint__propagate_conj_4_0_i16);
	update_prof_current_proc(LABEL(mercury__constraint__propagate_conj_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__constraint_module8)
	init_entry(mercury__constraint__find_constraints_5_0);
	init_label(mercury__constraint__find_constraints_5_0_i4);
	init_label(mercury__constraint__find_constraints_5_0_i5);
	init_label(mercury__constraint__find_constraints_5_0_i9);
	init_label(mercury__constraint__find_constraints_5_0_i10);
	init_label(mercury__constraint__find_constraints_5_0_i6);
	init_label(mercury__constraint__find_constraints_5_0_i12);
	init_label(mercury__constraint__find_constraints_5_0_i15);
	init_label(mercury__constraint__find_constraints_5_0_i17);
	init_label(mercury__constraint__find_constraints_5_0_i16);
	init_label(mercury__constraint__find_constraints_5_0_i19);
	init_label(mercury__constraint__find_constraints_5_0_i20);
	init_label(mercury__constraint__find_constraints_5_0_i21);
	init_label(mercury__constraint__find_constraints_5_0_i22);
	init_label(mercury__constraint__find_constraints_5_0_i23);
	init_label(mercury__constraint__find_constraints_5_0_i1013);
	init_label(mercury__constraint__find_constraints_5_0_i1007);
BEGIN_CODE

/* code for predicate 'constraint__find_constraints'/5 in mode 0 */
Define_static(mercury__constraint__find_constraints_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__constraint__find_constraints_5_0_i1007);
	incr_sp_push_msg(8, "constraint__find_constraints");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_dcg_get_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_dcg_get_instmap_3_0),
		mercury__constraint__find_constraints_5_0_i4,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i4);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__constraint__propagate_goal_4_0),
		mercury__constraint__find_constraints_5_0_i5,
		STATIC(mercury__constraint__find_constraints_5_0));
Define_label(mercury__constraint__find_constraints_5_0_i5);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r4 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r4) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__constraint__find_constraints_5_0_i6);
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_constraint__common_0);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__constraint__find_constraints_5_0_i9,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i9);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__constraint__find_constraints_5_0_i10,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i10);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__constraint__find_constraints_5_0,
		STATIC(mercury__constraint__find_constraints_5_0));
Define_label(mercury__constraint__find_constraints_5_0_i6);
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) tempr1;
	detstackvar(3) = (Integer) r3;
	localcall(mercury__constraint__find_constraints_5_0,
		LABEL(mercury__constraint__find_constraints_5_0_i12),
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i12);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r2;
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_determinism_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_determinism_2_0),
		mercury__constraint__find_constraints_5_0_i15,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i15);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__constraint__find_constraints_5_0_i17);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__constraint__find_constraints_5_0_i16);
Define_label(mercury__constraint__find_constraints_5_0_i17);
	if (((Integer) r1 != ((Integer) 7)))
		GOTO_LABEL(mercury__constraint__find_constraints_5_0_i1013);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
Define_label(mercury__constraint__find_constraints_5_0_i16);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_instmap_delta_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_instmap_delta_2_0),
		mercury__constraint__find_constraints_5_0_i19,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i19);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__constraint__find_constraints_5_0_i20,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i20);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__mode_info__mode_info_get_module_info_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_module_info_2_0),
		mercury__constraint__find_constraints_5_0_i21,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i21);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__mode_info__mode_info_get_instmap_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_instmap_2_0),
		mercury__constraint__find_constraints_5_0_i22,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i22);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__instmap__no_output_vars_4_0);
	call_localret(ENTRY(mercury__instmap__no_output_vars_4_0),
		mercury__constraint__find_constraints_5_0_i23,
		STATIC(mercury__constraint__find_constraints_5_0));
	}
Define_label(mercury__constraint__find_constraints_5_0_i23);
	update_prof_current_proc(LABEL(mercury__constraint__find_constraints_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__constraint__find_constraints_5_0_i1013);
	r1 = (Integer) detstackvar(5);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__constraint__find_constraints_5_0_i1013);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__constraint__find_constraints_5_0_i1007);
	r3 = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__constraint_bunch_0(void)
{
	mercury__constraint_module0();
	mercury__constraint_module1();
	mercury__constraint_module2();
	mercury__constraint_module3();
	mercury__constraint_module4();
	mercury__constraint_module5();
	mercury__constraint_module6();
	mercury__constraint_module7();
	mercury__constraint_module8();
}

#endif

void mercury__constraint__init(void); /* suppress gcc warning */
void mercury__constraint__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__constraint_bunch_0();
#endif
}
