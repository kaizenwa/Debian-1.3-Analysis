/*
** Automatically generated from `delay_info.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__delay_info__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___delay_info_delay_info_0__ua10000_2_0);
Declare_static(mercury__delay_info__check_invariant__ua10000_1_0);
Define_extern_entry(mercury__delay_info__check_invariant_1_0);
Define_extern_entry(mercury__delay_info__init_1_0);
Declare_label(mercury__delay_info__init_1_0_i2);
Declare_label(mercury__delay_info__init_1_0_i3);
Declare_label(mercury__delay_info__init_1_0_i4);
Declare_label(mercury__delay_info__init_1_0_i5);
Declare_label(mercury__delay_info__init_1_0_i6);
Define_extern_entry(mercury__delay_info__enter_conj_2_0);
Declare_label(mercury__delay_info__enter_conj_2_0_i2);
Declare_label(mercury__delay_info__enter_conj_2_0_i3);
Declare_label(mercury__delay_info__enter_conj_2_0_i4);
Declare_label(mercury__delay_info__enter_conj_2_0_i5);
Declare_label(mercury__delay_info__enter_conj_2_0_i6);
Define_extern_entry(mercury__delay_info__leave_conj_3_0);
Declare_label(mercury__delay_info__leave_conj_3_0_i2);
Declare_label(mercury__delay_info__leave_conj_3_0_i3);
Declare_label(mercury__delay_info__leave_conj_3_0_i4);
Declare_label(mercury__delay_info__leave_conj_3_0_i5);
Declare_label(mercury__delay_info__leave_conj_3_0_i6);
Declare_label(mercury__delay_info__leave_conj_3_0_i7);
Declare_label(mercury__delay_info__leave_conj_3_0_i8);
Define_extern_entry(mercury__delay_info__delay_goal_4_0);
Declare_label(mercury__delay_info__delay_goal_4_0_i2);
Declare_label(mercury__delay_info__delay_goal_4_0_i3);
Declare_label(mercury__delay_info__delay_goal_4_0_i4);
Declare_label(mercury__delay_info__delay_goal_4_0_i5);
Declare_label(mercury__delay_info__delay_goal_4_0_i6);
Declare_label(mercury__delay_info__delay_goal_4_0_i7);
Declare_label(mercury__delay_info__delay_goal_4_0_i8);
Declare_label(mercury__delay_info__delay_goal_4_0_i9);
Declare_label(mercury__delay_info__delay_goal_4_0_i10);
Define_extern_entry(mercury__delay_info__bind_var_list_3_0);
Declare_label(mercury__delay_info__bind_var_list_3_0_i4);
Declare_label(mercury__delay_info__bind_var_list_3_0_i1002);
Define_extern_entry(mercury__delay_info__bind_var_3_0);
Declare_label(mercury__delay_info__bind_var_3_0_i2);
Declare_label(mercury__delay_info__bind_var_3_0_i5);
Declare_label(mercury__delay_info__bind_var_3_0_i7);
Declare_label(mercury__delay_info__bind_var_3_0_i8);
Declare_label(mercury__delay_info__bind_var_3_0_i9);
Declare_label(mercury__delay_info__bind_var_3_0_i4);
Define_extern_entry(mercury__delay_info__bind_all_vars_2_0);
Declare_label(mercury__delay_info__bind_all_vars_2_0_i2);
Define_extern_entry(mercury__delay_info__wakeup_goals_3_0);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i4);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i5);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i8);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i9);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i10);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i11);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i12);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i13);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i14);
Declare_label(mercury__delay_info__wakeup_goals_3_0_i3);
Declare_static(mercury__delay_info__remove_delayed_goals_5_0);
Declare_label(mercury__delay_info__remove_delayed_goals_5_0_i4);
Declare_label(mercury__delay_info__remove_delayed_goals_5_0_i5);
Declare_label(mercury__delay_info__remove_delayed_goals_5_0_i6);
Declare_label(mercury__delay_info__remove_delayed_goals_5_0_i1002);
Declare_static(mercury__delay_info__add_waiting_vars_5_0);
Declare_label(mercury__delay_info__add_waiting_vars_5_0_i6);
Declare_label(mercury__delay_info__add_waiting_vars_5_0_i5);
Declare_label(mercury__delay_info__add_waiting_vars_5_0_i8);
Declare_label(mercury__delay_info__add_waiting_vars_5_0_i9);
Declare_label(mercury__delay_info__add_waiting_vars_5_0_i10);
Declare_label(mercury__delay_info__add_waiting_vars_5_0_i11);
Declare_label(mercury__delay_info__add_waiting_vars_5_0_i1003);
Declare_static(mercury__delay_info__add_pending_goals_6_0);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i4);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i5);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i8);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i10);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i7);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i11);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i12);
Declare_label(mercury__delay_info__add_pending_goals_6_0_i1004);
Declare_static(mercury__delay_info__delete_waiting_vars_4_0);
Declare_label(mercury__delay_info__delete_waiting_vars_4_0_i4);
Declare_label(mercury__delay_info__delete_waiting_vars_4_0_i5);
Declare_label(mercury__delay_info__delete_waiting_vars_4_0_i8);
Declare_label(mercury__delay_info__delete_waiting_vars_4_0_i10);
Declare_label(mercury__delay_info__delete_waiting_vars_4_0_i7);
Declare_label(mercury__delay_info__delete_waiting_vars_4_0_i11);
Declare_label(mercury__delay_info__delete_waiting_vars_4_0_i1003);
Define_extern_entry(mercury____Unify___delay_info__delay_info_0_0);
Declare_label(mercury____Unify___delay_info__delay_info_0_0_i2);
Declare_label(mercury____Unify___delay_info__delay_info_0_0_i4);
Declare_label(mercury____Unify___delay_info__delay_info_0_0_i6);
Declare_label(mercury____Unify___delay_info__delay_info_0_0_i1005);
Declare_label(mercury____Unify___delay_info__delay_info_0_0_i1);
Define_extern_entry(mercury____Index___delay_info__delay_info_0_0);
Define_extern_entry(mercury____Compare___delay_info__delay_info_0_0);
Declare_label(mercury____Compare___delay_info__delay_info_0_0_i4);
Declare_label(mercury____Compare___delay_info__delay_info_0_0_i5);
Declare_label(mercury____Compare___delay_info__delay_info_0_0_i3);
Declare_label(mercury____Compare___delay_info__delay_info_0_0_i10);
Declare_label(mercury____Compare___delay_info__delay_info_0_0_i16);
Declare_label(mercury____Compare___delay_info__delay_info_0_0_i22);

extern Word * mercury_data_delay_info__base_type_layout_delay_info_0[];
Word * mercury_data_delay_info__base_type_info_delay_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___delay_info__delay_info_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___delay_info__delay_info_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___delay_info__delay_info_0_0),
	(Word *) (Integer) mercury_data_delay_info__base_type_layout_delay_info_0
};

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_delay_info__base_type_layout_depth_num_0[];
Word * mercury_data_delay_info__base_type_info_depth_num_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_delay_info__base_type_layout_depth_num_0
};

extern Word * mercury_data_delay_info__base_type_layout_goal_num_0[];
Word * mercury_data_delay_info__base_type_info_goal_num_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_delay_info__base_type_layout_goal_num_0
};

extern Word * mercury_data_delay_info__base_type_layout_pending_goals_table_0[];
Word * mercury_data_delay_info__base_type_info_pending_goals_table_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_delay_info__base_type_layout_pending_goals_table_0
};

extern Word * mercury_data_delay_info__base_type_layout_seq_num_0[];
Word * mercury_data_delay_info__base_type_info_seq_num_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_delay_info__base_type_layout_seq_num_0
};

extern Word * mercury_data_delay_info__base_type_layout_waiting_goals_0[];
Word * mercury_data_delay_info__base_type_info_waiting_goals_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_delay_info__base_type_layout_waiting_goals_0
};

extern Word * mercury_data_delay_info__base_type_layout_waiting_goals_table_0[];
Word * mercury_data_delay_info__base_type_info_waiting_goals_table_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_delay_info__base_type_layout_waiting_goals_table_0
};

extern Word * mercury_data_delay_info__common_6[];
Word * mercury_data_delay_info__base_type_layout_waiting_goals_table_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_6),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_6),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_6),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_6)
};

extern Word * mercury_data_delay_info__common_7[];
Word * mercury_data_delay_info__base_type_layout_waiting_goals_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_7),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_7),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_7),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_7)
};

extern Word * mercury_data_delay_info__common_9[];
Word * mercury_data_delay_info__base_type_layout_seq_num_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9)
};

extern Word * mercury_data_delay_info__common_11[];
Word * mercury_data_delay_info__base_type_layout_pending_goals_table_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_11)
};

extern Word * mercury_data_delay_info__common_12[];
Word * mercury_data_delay_info__base_type_layout_goal_num_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_12),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_12),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_12),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_12)
};

Word * mercury_data_delay_info__base_type_layout_depth_num_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_delay_info__common_9)
};

extern Word * mercury_data_delay_info__common_15[];
Word * mercury_data_delay_info__base_type_layout_delay_info_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_delay_info__common_15),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data___base_type_info_int_0[];
extern Word * mercury_data_mode_errors__base_type_info_delayed_goal_0[];
Word * mercury_data_delay_info__common_0[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
Word * mercury_data_delay_info__common_1[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
Word * mercury_data_delay_info__common_2[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0
};

Word * mercury_data_delay_info__common_3[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2)
};

Word * mercury_data_delay_info__common_4[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_delay_info__common_5[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3)
};

Word * mercury_data_delay_info__common_6[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_5)
};

Word * mercury_data_delay_info__common_7[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3)
};

Word * mercury_data_delay_info__common_8[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_delay_info__common_9[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_8)
};

Word * mercury_data_delay_info__common_10[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4)
};

Word * mercury_data_delay_info__common_11[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_10)
};

Word * mercury_data_delay_info__common_12[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1)
};

extern Word * mercury_data_stack__base_type_info_stack_1[];
Word * mercury_data_delay_info__common_13[] = {
	(Word *) (Integer) mercury_data_stack__base_type_info_stack_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0)
};

Word * mercury_data_delay_info__common_14[] = {
	(Word *) (Integer) mercury_data_stack__base_type_info_stack_1,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_delay_info__common_15[] = {
	(Word *) ((Integer) 5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_8),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_13),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_10),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_14),
	(Word *) string_const("delay_info", 10)
};

BEGIN_MODULE(mercury__delay_info_module0)
	init_entry(mercury____Index___delay_info_delay_info_0__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___delay_info_delay_info_0__ua10000'/2 in mode 0 */
Define_static(mercury____Index___delay_info_delay_info_0__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module1)
	init_entry(mercury__delay_info__check_invariant__ua10000_1_0);
BEGIN_CODE

/* code for predicate 'delay_info__check_invariant__ua10000'/1 in mode 0 */
Define_static(mercury__delay_info__check_invariant__ua10000_1_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module2)
	init_entry(mercury__delay_info__check_invariant_1_0);
BEGIN_CODE

/* code for predicate 'delay_info__check_invariant'/1 in mode 0 */
Define_entry(mercury__delay_info__check_invariant_1_0);
	tailcall(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		ENTRY(mercury__delay_info__check_invariant_1_0));
END_MODULE

BEGIN_MODULE(mercury__delay_info_module3)
	init_entry(mercury__delay_info__init_1_0);
	init_label(mercury__delay_info__init_1_0_i2);
	init_label(mercury__delay_info__init_1_0_i3);
	init_label(mercury__delay_info__init_1_0_i4);
	init_label(mercury__delay_info__init_1_0_i5);
	init_label(mercury__delay_info__init_1_0_i6);
BEGIN_CODE

/* code for predicate 'delay_info__init'/1 in mode 0 */
Define_entry(mercury__delay_info__init_1_0);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	incr_sp_push_msg(4, "delay_info__init");
	detstackvar(4) = (Integer) succip;
	{
	Declare_entry(mercury__stack__init_1_0);
	call_localret(ENTRY(mercury__stack__init_1_0),
		mercury__delay_info__init_1_0_i2,
		ENTRY(mercury__delay_info__init_1_0));
	}
Define_label(mercury__delay_info__init_1_0_i2);
	update_prof_current_proc(LABEL(mercury__delay_info__init_1_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__delay_info__init_1_0_i3,
		ENTRY(mercury__delay_info__init_1_0));
	}
Define_label(mercury__delay_info__init_1_0_i3);
	update_prof_current_proc(LABEL(mercury__delay_info__init_1_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__delay_info__init_1_0_i4,
		ENTRY(mercury__delay_info__init_1_0));
	}
Define_label(mercury__delay_info__init_1_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__init_1_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__stack__init_1_0);
	call_localret(ENTRY(mercury__stack__init_1_0),
		mercury__delay_info__init_1_0_i5,
		ENTRY(mercury__delay_info__init_1_0));
	}
Define_label(mercury__delay_info__init_1_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__init_1_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = ((Integer) 0);
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__init_1_0_i6,
		ENTRY(mercury__delay_info__init_1_0));
Define_label(mercury__delay_info__init_1_0_i6);
	update_prof_current_proc(LABEL(mercury__delay_info__init_1_0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module4)
	init_entry(mercury__delay_info__enter_conj_2_0);
	init_label(mercury__delay_info__enter_conj_2_0_i2);
	init_label(mercury__delay_info__enter_conj_2_0_i3);
	init_label(mercury__delay_info__enter_conj_2_0_i4);
	init_label(mercury__delay_info__enter_conj_2_0_i5);
	init_label(mercury__delay_info__enter_conj_2_0_i6);
BEGIN_CODE

/* code for predicate 'delay_info__enter_conj'/2 in mode 0 */
Define_entry(mercury__delay_info__enter_conj_2_0);
	incr_sp_push_msg(6, "delay_info__enter_conj");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__enter_conj_2_0_i2,
		ENTRY(mercury__delay_info__enter_conj_2_0));
Define_label(mercury__delay_info__enter_conj_2_0_i2);
	update_prof_current_proc(LABEL(mercury__delay_info__enter_conj_2_0));
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__delay_info__enter_conj_2_0_i3,
		ENTRY(mercury__delay_info__enter_conj_2_0));
	}
Define_label(mercury__delay_info__enter_conj_2_0_i3);
	update_prof_current_proc(LABEL(mercury__delay_info__enter_conj_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__stack__push_3_0);
	call_localret(ENTRY(mercury__stack__push_3_0),
		mercury__delay_info__enter_conj_2_0_i4,
		ENTRY(mercury__delay_info__enter_conj_2_0));
	}
Define_label(mercury__delay_info__enter_conj_2_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__enter_conj_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(5);
	r3 = ((Integer) 0);
	{
	Declare_entry(mercury__stack__push_3_0);
	call_localret(ENTRY(mercury__stack__push_3_0),
		mercury__delay_info__enter_conj_2_0_i5,
		ENTRY(mercury__delay_info__enter_conj_2_0));
	}
Define_label(mercury__delay_info__enter_conj_2_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__enter_conj_2_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = ((Integer) detstackvar(1) + ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__enter_conj_2_0_i6,
		ENTRY(mercury__delay_info__enter_conj_2_0));
Define_label(mercury__delay_info__enter_conj_2_0_i6);
	update_prof_current_proc(LABEL(mercury__delay_info__enter_conj_2_0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module5)
	init_entry(mercury__delay_info__leave_conj_3_0);
	init_label(mercury__delay_info__leave_conj_3_0_i2);
	init_label(mercury__delay_info__leave_conj_3_0_i3);
	init_label(mercury__delay_info__leave_conj_3_0_i4);
	init_label(mercury__delay_info__leave_conj_3_0_i5);
	init_label(mercury__delay_info__leave_conj_3_0_i6);
	init_label(mercury__delay_info__leave_conj_3_0_i7);
	init_label(mercury__delay_info__leave_conj_3_0_i8);
BEGIN_CODE

/* code for predicate 'delay_info__leave_conj'/3 in mode 0 */
Define_entry(mercury__delay_info__leave_conj_3_0);
	incr_sp_push_msg(7, "delay_info__leave_conj");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__leave_conj_3_0_i2,
		ENTRY(mercury__delay_info__leave_conj_3_0));
Define_label(mercury__delay_info__leave_conj_3_0_i2);
	update_prof_current_proc(LABEL(mercury__delay_info__leave_conj_3_0));
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 4));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	{
	Declare_entry(mercury__stack__pop_det_3_0);
	call_localret(ENTRY(mercury__stack__pop_det_3_0),
		mercury__delay_info__leave_conj_3_0_i3,
		ENTRY(mercury__delay_info__leave_conj_3_0));
	}
Define_label(mercury__delay_info__leave_conj_3_0_i3);
	update_prof_current_proc(LABEL(mercury__delay_info__leave_conj_3_0));
	r3 = (Integer) r1;
	detstackvar(5) = (Integer) r1;
	detstackvar(6) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0;
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__delay_info__leave_conj_3_0_i4,
		ENTRY(mercury__delay_info__leave_conj_3_0));
	}
Define_label(mercury__delay_info__leave_conj_3_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__leave_conj_3_0));
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__delay_info__remove_delayed_goals_5_0),
		mercury__delay_info__leave_conj_3_0_i5,
		ENTRY(mercury__delay_info__leave_conj_3_0));
Define_label(mercury__delay_info__leave_conj_3_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__leave_conj_3_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__stack__pop_det_3_0);
	call_localret(ENTRY(mercury__stack__pop_det_3_0),
		mercury__delay_info__leave_conj_3_0_i6,
		ENTRY(mercury__delay_info__leave_conj_3_0));
	}
Define_label(mercury__delay_info__leave_conj_3_0_i6);
	update_prof_current_proc(LABEL(mercury__delay_info__leave_conj_3_0));
	detstackvar(4) = ((Integer) detstackvar(1) - ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0;
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__values_2_0);
	call_localret(ENTRY(mercury__map__values_2_0),
		mercury__delay_info__leave_conj_3_0_i7,
		ENTRY(mercury__delay_info__leave_conj_3_0));
	}
Define_label(mercury__delay_info__leave_conj_3_0_i7);
	update_prof_current_proc(LABEL(mercury__delay_info__leave_conj_3_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__leave_conj_3_0_i8,
		ENTRY(mercury__delay_info__leave_conj_3_0));
Define_label(mercury__delay_info__leave_conj_3_0_i8);
	update_prof_current_proc(LABEL(mercury__delay_info__leave_conj_3_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module6)
	init_entry(mercury__delay_info__delay_goal_4_0);
	init_label(mercury__delay_info__delay_goal_4_0_i2);
	init_label(mercury__delay_info__delay_goal_4_0_i3);
	init_label(mercury__delay_info__delay_goal_4_0_i4);
	init_label(mercury__delay_info__delay_goal_4_0_i5);
	init_label(mercury__delay_info__delay_goal_4_0_i6);
	init_label(mercury__delay_info__delay_goal_4_0_i7);
	init_label(mercury__delay_info__delay_goal_4_0_i8);
	init_label(mercury__delay_info__delay_goal_4_0_i9);
	init_label(mercury__delay_info__delay_goal_4_0_i10);
BEGIN_CODE

/* code for predicate 'delay_info__delay_goal'/4 in mode 0 */
Define_entry(mercury__delay_info__delay_goal_4_0);
	incr_sp_push_msg(9, "delay_info__delay_goal");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__delay_goal_4_0_i2,
		ENTRY(mercury__delay_info__delay_goal_4_0));
Define_label(mercury__delay_info__delay_goal_4_0_i2);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 4));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	detstackvar(7) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 3));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) detstackvar(2), ((Integer) 0));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__stack__pop_det_3_0);
	call_localret(ENTRY(mercury__stack__pop_det_3_0),
		mercury__delay_info__delay_goal_4_0_i3,
		ENTRY(mercury__delay_info__delay_goal_4_0));
	}
Define_label(mercury__delay_info__delay_goal_4_0_i3);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r3 = ((Integer) detstackvar(8) + ((Integer) 1));
	{
	Declare_entry(mercury__stack__push_3_0);
	call_localret(ENTRY(mercury__stack__push_3_0),
		mercury__delay_info__delay_goal_4_0_i4,
		ENTRY(mercury__delay_info__delay_goal_4_0));
	}
Define_label(mercury__delay_info__delay_goal_4_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	r2 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	{
	Declare_entry(mercury__stack__pop_det_3_0);
	call_localret(ENTRY(mercury__stack__pop_det_3_0),
		mercury__delay_info__delay_goal_4_0_i5,
		ENTRY(mercury__delay_info__delay_goal_4_0));
	}
Define_label(mercury__delay_info__delay_goal_4_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	tag_incr_hp(r5, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(2);
	r3 = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0;
	r4 = (Integer) detstackvar(8);
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__delay_info__delay_goal_4_0_i6,
		ENTRY(mercury__delay_info__delay_goal_4_0));
	}
Define_label(mercury__delay_info__delay_goal_4_0_i6);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__stack__push_3_0);
	call_localret(ENTRY(mercury__stack__push_3_0),
		mercury__delay_info__delay_goal_4_0_i7,
		ENTRY(mercury__delay_info__delay_goal_4_0));
	}
Define_label(mercury__delay_info__delay_goal_4_0_i7);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(8);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__delay_info__delay_goal_4_0_i8,
		ENTRY(mercury__delay_info__delay_goal_4_0));
	}
Define_label(mercury__delay_info__delay_goal_4_0_i8);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) r1;
	r4 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__delay_info__add_waiting_vars_5_0),
		mercury__delay_info__delay_goal_4_0_i9,
		ENTRY(mercury__delay_info__delay_goal_4_0));
Define_label(mercury__delay_info__delay_goal_4_0_i9);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	r2 = (Integer) detstackvar(1);
	tag_incr_hp(r3, mktag(0), ((Integer) 5));
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 4)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(7);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) r1;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__delay_goal_4_0_i10,
		ENTRY(mercury__delay_info__delay_goal_4_0));
Define_label(mercury__delay_info__delay_goal_4_0_i10);
	update_prof_current_proc(LABEL(mercury__delay_info__delay_goal_4_0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module7)
	init_entry(mercury__delay_info__bind_var_list_3_0);
	init_label(mercury__delay_info__bind_var_list_3_0_i4);
	init_label(mercury__delay_info__bind_var_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'delay_info__bind_var_list'/3 in mode 0 */
Define_entry(mercury__delay_info__bind_var_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__delay_info__bind_var_list_3_0_i1002);
	incr_sp_push_msg(2, "delay_info__bind_var_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__delay_info__bind_var_3_0),
		mercury__delay_info__bind_var_list_3_0_i4,
		ENTRY(mercury__delay_info__bind_var_list_3_0));
	}
Define_label(mercury__delay_info__bind_var_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__bind_var_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__delay_info__bind_var_list_3_0,
		ENTRY(mercury__delay_info__bind_var_list_3_0));
Define_label(mercury__delay_info__bind_var_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module8)
	init_entry(mercury__delay_info__bind_var_3_0);
	init_label(mercury__delay_info__bind_var_3_0_i2);
	init_label(mercury__delay_info__bind_var_3_0_i5);
	init_label(mercury__delay_info__bind_var_3_0_i7);
	init_label(mercury__delay_info__bind_var_3_0_i8);
	init_label(mercury__delay_info__bind_var_3_0_i9);
	init_label(mercury__delay_info__bind_var_3_0_i4);
BEGIN_CODE

/* code for predicate 'delay_info__bind_var'/3 in mode 0 */
Define_entry(mercury__delay_info__bind_var_3_0);
	incr_sp_push_msg(8, "delay_info__bind_var");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__bind_var_3_0_i2,
		ENTRY(mercury__delay_info__bind_var_3_0));
Define_label(mercury__delay_info__bind_var_3_0_i2);
	update_prof_current_proc(LABEL(mercury__delay_info__bind_var_3_0));
	r4 = (Integer) detstackvar(2);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(1);
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 3));
	detstackvar(4) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__delay_info__bind_var_3_0_i5,
		ENTRY(mercury__delay_info__bind_var_3_0));
	}
	}
Define_label(mercury__delay_info__bind_var_3_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__bind_var_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__delay_info__bind_var_3_0_i4);
	r3 = (Integer) r2;
	detstackvar(7) = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2);
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__delay_info__bind_var_3_0_i7,
		ENTRY(mercury__delay_info__bind_var_3_0));
	}
Define_label(mercury__delay_info__bind_var_3_0_i7);
	update_prof_current_proc(LABEL(mercury__delay_info__bind_var_3_0));
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__delay_info__add_pending_goals_6_0),
		mercury__delay_info__bind_var_3_0_i8,
		ENTRY(mercury__delay_info__bind_var_3_0));
Define_label(mercury__delay_info__bind_var_3_0_i8);
	update_prof_current_proc(LABEL(mercury__delay_info__bind_var_3_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 4)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) r1;
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) r2;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__bind_var_3_0_i9,
		ENTRY(mercury__delay_info__bind_var_3_0));
Define_label(mercury__delay_info__bind_var_3_0_i9);
	update_prof_current_proc(LABEL(mercury__delay_info__bind_var_3_0));
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__delay_info__bind_var_3_0_i4);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module9)
	init_entry(mercury__delay_info__bind_all_vars_2_0);
	init_label(mercury__delay_info__bind_all_vars_2_0_i2);
BEGIN_CODE

/* code for predicate 'delay_info__bind_all_vars'/2 in mode 0 */
Define_entry(mercury__delay_info__bind_all_vars_2_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	incr_sp_push_msg(2, "delay_info__bind_all_vars");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__delay_info__bind_all_vars_2_0_i2,
		ENTRY(mercury__delay_info__bind_all_vars_2_0));
	}
Define_label(mercury__delay_info__bind_all_vars_2_0_i2);
	update_prof_current_proc(LABEL(mercury__delay_info__bind_all_vars_2_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__delay_info__bind_var_list_3_0),
		ENTRY(mercury__delay_info__bind_all_vars_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__delay_info_module10)
	init_entry(mercury__delay_info__wakeup_goals_3_0);
	init_label(mercury__delay_info__wakeup_goals_3_0_i4);
	init_label(mercury__delay_info__wakeup_goals_3_0_i5);
	init_label(mercury__delay_info__wakeup_goals_3_0_i8);
	init_label(mercury__delay_info__wakeup_goals_3_0_i9);
	init_label(mercury__delay_info__wakeup_goals_3_0_i10);
	init_label(mercury__delay_info__wakeup_goals_3_0_i11);
	init_label(mercury__delay_info__wakeup_goals_3_0_i12);
	init_label(mercury__delay_info__wakeup_goals_3_0_i13);
	init_label(mercury__delay_info__wakeup_goals_3_0_i14);
	init_label(mercury__delay_info__wakeup_goals_3_0_i3);
BEGIN_CODE

/* code for predicate 'delay_info__wakeup_goals'/3 in mode 0 */
Define_entry(mercury__delay_info__wakeup_goals_3_0);
	incr_sp_push_msg(9, "delay_info__wakeup_goals");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__wakeup_goals_3_0_i4,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
Define_label(mercury__delay_info__wakeup_goals_3_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(1);
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 3));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 4));
	detstackvar(5) = (Integer) r3;
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__delay_info__wakeup_goals_3_0_i5,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
	}
	}
Define_label(mercury__delay_info__wakeup_goals_3_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__delay_info__wakeup_goals_3_0_i3);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__delay_info__wakeup_goals_3_0_i3);
	r5 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 3));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__delay_info__wakeup_goals_3_0_i8,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
	}
Define_label(mercury__delay_info__wakeup_goals_3_0_i8);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__stack__pop_det_3_0);
	call_localret(ENTRY(mercury__stack__pop_det_3_0),
		mercury__delay_info__wakeup_goals_3_0_i9,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
	}
Define_label(mercury__delay_info__wakeup_goals_3_0_i9);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	r3 = (Integer) r1;
	detstackvar(2) = (Integer) r1;
	detstackvar(8) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0;
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__delay_info__wakeup_goals_3_0_i10,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
	}
Define_label(mercury__delay_info__wakeup_goals_3_0_i10);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0;
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__delete_3_1);
	call_localret(ENTRY(mercury__map__delete_3_1),
		mercury__delay_info__wakeup_goals_3_0_i11,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
	}
Define_label(mercury__delay_info__wakeup_goals_3_0_i11);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__stack__push_3_0);
	call_localret(ENTRY(mercury__stack__push_3_0),
		mercury__delay_info__wakeup_goals_3_0_i12,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
	}
Define_label(mercury__delay_info__wakeup_goals_3_0_i12);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r2;
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(7);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	call_localret(STATIC(mercury__delay_info__check_invariant__ua10000_1_0),
		mercury__delay_info__wakeup_goals_3_0_i13,
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
Define_label(mercury__delay_info__wakeup_goals_3_0_i13);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	r1 = (Integer) detstackvar(3);
	localcall(mercury__delay_info__wakeup_goals_3_0,
		LABEL(mercury__delay_info__wakeup_goals_3_0_i14),
		ENTRY(mercury__delay_info__wakeup_goals_3_0));
Define_label(mercury__delay_info__wakeup_goals_3_0_i14);
	update_prof_current_proc(LABEL(mercury__delay_info__wakeup_goals_3_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__delay_info__wakeup_goals_3_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module11)
	init_entry(mercury__delay_info__remove_delayed_goals_5_0);
	init_label(mercury__delay_info__remove_delayed_goals_5_0_i4);
	init_label(mercury__delay_info__remove_delayed_goals_5_0_i5);
	init_label(mercury__delay_info__remove_delayed_goals_5_0_i6);
	init_label(mercury__delay_info__remove_delayed_goals_5_0_i1002);
BEGIN_CODE

/* code for predicate 'remove_delayed_goals'/5 in mode 0 */
Define_static(mercury__delay_info__remove_delayed_goals_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__delay_info__remove_delayed_goals_5_0_i1002);
	incr_sp_push_msg(6, "remove_delayed_goals");
	detstackvar(6) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r4;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mercury_data_mode_errors__base_type_info_delayed_goal_0;
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__delay_info__remove_delayed_goals_5_0_i4,
		STATIC(mercury__delay_info__remove_delayed_goals_5_0));
	}
Define_label(mercury__delay_info__remove_delayed_goals_5_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__remove_delayed_goals_5_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(4);
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r3;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__delay_info__remove_delayed_goals_5_0_i5,
		STATIC(mercury__delay_info__remove_delayed_goals_5_0));
	}
Define_label(mercury__delay_info__remove_delayed_goals_5_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__remove_delayed_goals_5_0));
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__delay_info__delete_waiting_vars_4_0),
		mercury__delay_info__remove_delayed_goals_5_0_i6,
		STATIC(mercury__delay_info__remove_delayed_goals_5_0));
Define_label(mercury__delay_info__remove_delayed_goals_5_0_i6);
	update_prof_current_proc(LABEL(mercury__delay_info__remove_delayed_goals_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__delay_info__remove_delayed_goals_5_0,
		STATIC(mercury__delay_info__remove_delayed_goals_5_0));
Define_label(mercury__delay_info__remove_delayed_goals_5_0_i1002);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module12)
	init_entry(mercury__delay_info__add_waiting_vars_5_0);
	init_label(mercury__delay_info__add_waiting_vars_5_0_i6);
	init_label(mercury__delay_info__add_waiting_vars_5_0_i5);
	init_label(mercury__delay_info__add_waiting_vars_5_0_i8);
	init_label(mercury__delay_info__add_waiting_vars_5_0_i9);
	init_label(mercury__delay_info__add_waiting_vars_5_0_i10);
	init_label(mercury__delay_info__add_waiting_vars_5_0_i11);
	init_label(mercury__delay_info__add_waiting_vars_5_0_i1003);
BEGIN_CODE

/* code for predicate 'add_waiting_vars'/5 in mode 0 */
Define_static(mercury__delay_info__add_waiting_vars_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__delay_info__add_waiting_vars_5_0_i1003);
	incr_sp_push_msg(6, "add_waiting_vars");
	detstackvar(6) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r4;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r4;
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__delay_info__add_waiting_vars_5_0_i6,
		STATIC(mercury__delay_info__add_waiting_vars_5_0));
	}
Define_label(mercury__delay_info__add_waiting_vars_5_0_i6);
	update_prof_current_proc(LABEL(mercury__delay_info__add_waiting_vars_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__delay_info__add_waiting_vars_5_0_i5);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	r7 = (Integer) detstackvar(4);
	r8 = (Integer) detstackvar(5);
	r3 = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2);
	GOTO_LABEL(mercury__delay_info__add_waiting_vars_5_0_i9);
Define_label(mercury__delay_info__add_waiting_vars_5_0_i5);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__delay_info__add_waiting_vars_5_0_i8,
		STATIC(mercury__delay_info__add_waiting_vars_5_0));
	}
Define_label(mercury__delay_info__add_waiting_vars_5_0_i8);
	update_prof_current_proc(LABEL(mercury__delay_info__add_waiting_vars_5_0));
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	r7 = (Integer) detstackvar(4);
	r8 = (Integer) detstackvar(5);
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2);
Define_label(mercury__delay_info__add_waiting_vars_5_0_i9);
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(4) = (Integer) r7;
	detstackvar(5) = (Integer) r8;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__delay_info__add_waiting_vars_5_0_i10,
		STATIC(mercury__delay_info__add_waiting_vars_5_0));
	}
Define_label(mercury__delay_info__add_waiting_vars_5_0_i10);
	update_prof_current_proc(LABEL(mercury__delay_info__add_waiting_vars_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__delay_info__add_waiting_vars_5_0_i11,
		STATIC(mercury__delay_info__add_waiting_vars_5_0));
	}
Define_label(mercury__delay_info__add_waiting_vars_5_0_i11);
	update_prof_current_proc(LABEL(mercury__delay_info__add_waiting_vars_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__delay_info__add_waiting_vars_5_0,
		STATIC(mercury__delay_info__add_waiting_vars_5_0));
Define_label(mercury__delay_info__add_waiting_vars_5_0_i1003);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module13)
	init_entry(mercury__delay_info__add_pending_goals_6_0);
	init_label(mercury__delay_info__add_pending_goals_6_0_i4);
	init_label(mercury__delay_info__add_pending_goals_6_0_i5);
	init_label(mercury__delay_info__add_pending_goals_6_0_i8);
	init_label(mercury__delay_info__add_pending_goals_6_0_i10);
	init_label(mercury__delay_info__add_pending_goals_6_0_i7);
	init_label(mercury__delay_info__add_pending_goals_6_0_i11);
	init_label(mercury__delay_info__add_pending_goals_6_0_i12);
	init_label(mercury__delay_info__add_pending_goals_6_0_i1004);
BEGIN_CODE

/* code for predicate 'add_pending_goals'/6 in mode 0 */
Define_static(mercury__delay_info__add_pending_goals_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__delay_info__add_pending_goals_6_0_i1004);
	incr_sp_push_msg(8, "add_pending_goals");
	detstackvar(8) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(7) = (Integer) r4;
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__delay_info__add_pending_goals_6_0_i4,
		STATIC(mercury__delay_info__add_pending_goals_6_0));
	}
Define_label(mercury__delay_info__add_pending_goals_6_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__add_pending_goals_6_0));
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__delay_info__delete_waiting_vars_4_0),
		mercury__delay_info__add_pending_goals_6_0_i5,
		STATIC(mercury__delay_info__add_pending_goals_6_0));
Define_label(mercury__delay_info__add_pending_goals_6_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__add_pending_goals_6_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__delay_info__add_pending_goals_6_0_i8,
		STATIC(mercury__delay_info__add_pending_goals_6_0));
	}
Define_label(mercury__delay_info__add_pending_goals_6_0_i8);
	update_prof_current_proc(LABEL(mercury__delay_info__add_pending_goals_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__delay_info__add_pending_goals_6_0_i7);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__delay_info__add_pending_goals_6_0_i10,
		STATIC(mercury__delay_info__add_pending_goals_6_0));
	}
Define_label(mercury__delay_info__add_pending_goals_6_0_i10);
	update_prof_current_proc(LABEL(mercury__delay_info__add_pending_goals_6_0));
	r6 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(3);
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
	GOTO_LABEL(mercury__delay_info__add_pending_goals_6_0_i11);
Define_label(mercury__delay_info__add_pending_goals_6_0_i7);
	r6 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(3);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
Define_label(mercury__delay_info__add_pending_goals_6_0_i11);
	detstackvar(1) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(3) = (Integer) r8;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__delay_info__add_pending_goals_6_0_i12,
		STATIC(mercury__delay_info__add_pending_goals_6_0));
	}
Define_label(mercury__delay_info__add_pending_goals_6_0_i12);
	update_prof_current_proc(LABEL(mercury__delay_info__add_pending_goals_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__delay_info__add_pending_goals_6_0,
		STATIC(mercury__delay_info__add_pending_goals_6_0));
Define_label(mercury__delay_info__add_pending_goals_6_0_i1004);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module14)
	init_entry(mercury__delay_info__delete_waiting_vars_4_0);
	init_label(mercury__delay_info__delete_waiting_vars_4_0_i4);
	init_label(mercury__delay_info__delete_waiting_vars_4_0_i5);
	init_label(mercury__delay_info__delete_waiting_vars_4_0_i8);
	init_label(mercury__delay_info__delete_waiting_vars_4_0_i10);
	init_label(mercury__delay_info__delete_waiting_vars_4_0_i7);
	init_label(mercury__delay_info__delete_waiting_vars_4_0_i11);
	init_label(mercury__delay_info__delete_waiting_vars_4_0_i1003);
BEGIN_CODE

/* code for predicate 'delete_waiting_vars'/4 in mode 0 */
Define_static(mercury__delay_info__delete_waiting_vars_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__delay_info__delete_waiting_vars_4_0_i1003);
	incr_sp_push_msg(6, "delete_waiting_vars");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__delay_info__delete_waiting_vars_4_0_i4,
		STATIC(mercury__delay_info__delete_waiting_vars_4_0));
	}
Define_label(mercury__delay_info__delete_waiting_vars_4_0_i4);
	update_prof_current_proc(LABEL(mercury__delay_info__delete_waiting_vars_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2);
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__delete_3_1);
	call_localret(ENTRY(mercury__map__delete_3_1),
		mercury__delay_info__delete_waiting_vars_4_0_i5,
		STATIC(mercury__delay_info__delete_waiting_vars_4_0));
	}
Define_label(mercury__delay_info__delete_waiting_vars_4_0_i5);
	update_prof_current_proc(LABEL(mercury__delay_info__delete_waiting_vars_4_0));
	r3 = (Integer) r1;
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_2);
	{
	Declare_entry(mercury__map__is_empty_1_0);
	call_localret(ENTRY(mercury__map__is_empty_1_0),
		mercury__delay_info__delete_waiting_vars_4_0_i8,
		STATIC(mercury__delay_info__delete_waiting_vars_4_0));
	}
Define_label(mercury__delay_info__delete_waiting_vars_4_0_i8);
	update_prof_current_proc(LABEL(mercury__delay_info__delete_waiting_vars_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__delay_info__delete_waiting_vars_4_0_i7);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__delete_3_1);
	call_localret(ENTRY(mercury__map__delete_3_1),
		mercury__delay_info__delete_waiting_vars_4_0_i10,
		STATIC(mercury__delay_info__delete_waiting_vars_4_0));
	}
Define_label(mercury__delay_info__delete_waiting_vars_4_0_i10);
	update_prof_current_proc(LABEL(mercury__delay_info__delete_waiting_vars_4_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__delay_info__delete_waiting_vars_4_0,
		STATIC(mercury__delay_info__delete_waiting_vars_4_0));
Define_label(mercury__delay_info__delete_waiting_vars_4_0_i7);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__delay_info__delete_waiting_vars_4_0_i11,
		STATIC(mercury__delay_info__delete_waiting_vars_4_0));
	}
Define_label(mercury__delay_info__delete_waiting_vars_4_0_i11);
	update_prof_current_proc(LABEL(mercury__delay_info__delete_waiting_vars_4_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__delay_info__delete_waiting_vars_4_0,
		STATIC(mercury__delay_info__delete_waiting_vars_4_0));
Define_label(mercury__delay_info__delete_waiting_vars_4_0_i1003);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module15)
	init_entry(mercury____Unify___delay_info__delay_info_0_0);
	init_label(mercury____Unify___delay_info__delay_info_0_0_i2);
	init_label(mercury____Unify___delay_info__delay_info_0_0_i4);
	init_label(mercury____Unify___delay_info__delay_info_0_0_i6);
	init_label(mercury____Unify___delay_info__delay_info_0_0_i1005);
	init_label(mercury____Unify___delay_info__delay_info_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___delay_info__delay_info_0_0);
	if (((Integer) field(mktag(0), (Integer) r1, ((Integer) 0)) != (Integer) field(mktag(0), (Integer) r2, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___delay_info__delay_info_0_0_i1005);
	incr_sp_push_msg(7, "__Unify__");
	detstackvar(7) = (Integer) succip;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	{
	Declare_entry(mercury____Unify___stack__stack_1_0);
	call_localret(ENTRY(mercury____Unify___stack__stack_1_0),
		mercury____Unify___delay_info__delay_info_0_0_i2,
		ENTRY(mercury____Unify___delay_info__delay_info_0_0));
	}
Define_label(mercury____Unify___delay_info__delay_info_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___delay_info__delay_info_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___delay_info__delay_info_0_0_i1);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Unify___tree234__tree234_2_0),
		mercury____Unify___delay_info__delay_info_0_0_i4,
		ENTRY(mercury____Unify___delay_info__delay_info_0_0));
	}
Define_label(mercury____Unify___delay_info__delay_info_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Unify___delay_info__delay_info_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___delay_info__delay_info_0_0_i1);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Unify___tree234__tree234_2_0),
		mercury____Unify___delay_info__delay_info_0_0_i6,
		ENTRY(mercury____Unify___delay_info__delay_info_0_0));
	}
Define_label(mercury____Unify___delay_info__delay_info_0_0_i6);
	update_prof_current_proc(LABEL(mercury____Unify___delay_info__delay_info_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___delay_info__delay_info_0_0_i1);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury____Unify___stack__stack_1_0);
	tailcall(ENTRY(mercury____Unify___stack__stack_1_0),
		ENTRY(mercury____Unify___delay_info__delay_info_0_0));
	}
Define_label(mercury____Unify___delay_info__delay_info_0_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___delay_info__delay_info_0_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__delay_info_module16)
	init_entry(mercury____Index___delay_info__delay_info_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___delay_info__delay_info_0_0);
	tailcall(STATIC(mercury____Index___delay_info_delay_info_0__ua10000_2_0),
		ENTRY(mercury____Index___delay_info__delay_info_0_0));
END_MODULE

BEGIN_MODULE(mercury__delay_info_module17)
	init_entry(mercury____Compare___delay_info__delay_info_0_0);
	init_label(mercury____Compare___delay_info__delay_info_0_0_i4);
	init_label(mercury____Compare___delay_info__delay_info_0_0_i5);
	init_label(mercury____Compare___delay_info__delay_info_0_0_i3);
	init_label(mercury____Compare___delay_info__delay_info_0_0_i10);
	init_label(mercury____Compare___delay_info__delay_info_0_0_i16);
	init_label(mercury____Compare___delay_info__delay_info_0_0_i22);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___delay_info__delay_info_0_0);
	incr_sp_push_msg(9, "__Compare__");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(7) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___delay_info__delay_info_0_0_i4,
		ENTRY(mercury____Compare___delay_info__delay_info_0_0));
	}
Define_label(mercury____Compare___delay_info__delay_info_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___delay_info__delay_info_0_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___delay_info__delay_info_0_0_i3);
Define_label(mercury____Compare___delay_info__delay_info_0_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury____Compare___delay_info__delay_info_0_0_i3);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_0);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury____Compare___stack__stack_1_0);
	call_localret(ENTRY(mercury____Compare___stack__stack_1_0),
		mercury____Compare___delay_info__delay_info_0_0_i10,
		ENTRY(mercury____Compare___delay_info__delay_info_0_0));
	}
Define_label(mercury____Compare___delay_info__delay_info_0_0_i10);
	update_prof_current_proc(LABEL(mercury____Compare___delay_info__delay_info_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___delay_info__delay_info_0_0_i5);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_3);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Compare___tree234__tree234_2_0),
		mercury____Compare___delay_info__delay_info_0_0_i16,
		ENTRY(mercury____Compare___delay_info__delay_info_0_0));
	}
Define_label(mercury____Compare___delay_info__delay_info_0_0_i16);
	update_prof_current_proc(LABEL(mercury____Compare___delay_info__delay_info_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___delay_info__delay_info_0_0_i5);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_delay_info__common_4);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Compare___tree234__tree234_2_0),
		mercury____Compare___delay_info__delay_info_0_0_i22,
		ENTRY(mercury____Compare___delay_info__delay_info_0_0));
	}
Define_label(mercury____Compare___delay_info__delay_info_0_0_i22);
	update_prof_current_proc(LABEL(mercury____Compare___delay_info__delay_info_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___delay_info__delay_info_0_0_i5);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	{
	Declare_entry(mercury____Compare___stack__stack_1_0);
	tailcall(ENTRY(mercury____Compare___stack__stack_1_0),
		ENTRY(mercury____Compare___delay_info__delay_info_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__delay_info_bunch_0(void)
{
	mercury__delay_info_module0();
	mercury__delay_info_module1();
	mercury__delay_info_module2();
	mercury__delay_info_module3();
	mercury__delay_info_module4();
	mercury__delay_info_module5();
	mercury__delay_info_module6();
	mercury__delay_info_module7();
	mercury__delay_info_module8();
	mercury__delay_info_module9();
	mercury__delay_info_module10();
	mercury__delay_info_module11();
	mercury__delay_info_module12();
	mercury__delay_info_module13();
	mercury__delay_info_module14();
	mercury__delay_info_module15();
	mercury__delay_info_module16();
	mercury__delay_info_module17();
}

#endif

void mercury__delay_info__init(void); /* suppress gcc warning */
void mercury__delay_info__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__delay_info_bunch_0();
#endif
}
