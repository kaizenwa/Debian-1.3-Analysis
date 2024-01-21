/*
** Automatically generated from `transform.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__transform__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__transform__reschedule_conj_4_0);
Declare_label(mercury__transform__reschedule_conj_4_0_i4);
Declare_label(mercury__transform__reschedule_conj_4_0_i5);
Declare_label(mercury__transform__reschedule_conj_4_0_i6);
Declare_label(mercury__transform__reschedule_conj_4_0_i7);
Declare_label(mercury__transform__reschedule_conj_4_0_i13);
Declare_label(mercury__transform__reschedule_conj_4_0_i8);
Declare_label(mercury__transform__reschedule_conj_4_0_i15);
Declare_label(mercury__transform__reschedule_conj_4_0_i16);
Declare_label(mercury__transform__reschedule_conj_4_0_i17);
Declare_label(mercury__transform__reschedule_conj_4_0_i18);
Declare_label(mercury__transform__reschedule_conj_4_0_i1003);

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_info_0[];
Word * mercury_data_transform__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_info_0
};

BEGIN_MODULE(mercury__transform_module0)
	init_entry(mercury__transform__reschedule_conj_4_0);
	init_label(mercury__transform__reschedule_conj_4_0_i4);
	init_label(mercury__transform__reschedule_conj_4_0_i5);
	init_label(mercury__transform__reschedule_conj_4_0_i6);
	init_label(mercury__transform__reschedule_conj_4_0_i7);
	init_label(mercury__transform__reschedule_conj_4_0_i13);
	init_label(mercury__transform__reschedule_conj_4_0_i8);
	init_label(mercury__transform__reschedule_conj_4_0_i15);
	init_label(mercury__transform__reschedule_conj_4_0_i16);
	init_label(mercury__transform__reschedule_conj_4_0_i17);
	init_label(mercury__transform__reschedule_conj_4_0_i18);
	init_label(mercury__transform__reschedule_conj_4_0_i1003);
BEGIN_CODE

/* code for predicate 'transform__reschedule_conj'/4 in mode 0 */
Define_entry(mercury__transform__reschedule_conj_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__transform__reschedule_conj_4_0_i1003);
	incr_sp_push_msg(6, "transform__reschedule_conj");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__mode_info__mode_info_get_instmap_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_instmap_2_0),
		mercury__transform__reschedule_conj_4_0_i4,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i4);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__mode_info__mode_info_get_delay_info_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_delay_info_2_0),
		mercury__transform__reschedule_conj_4_0_i5,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i5);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	{
	Declare_entry(mercury__delay_info__wakeup_goals_3_0);
	call_localret(ENTRY(mercury__delay_info__wakeup_goals_3_0),
		mercury__transform__reschedule_conj_4_0_i6,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i6);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__mode_info__mode_info_set_delay_info_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_delay_info_3_0),
		mercury__transform__reschedule_conj_4_0_i7,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i7);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	if (((Integer) detstackvar(2) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__transform__reschedule_conj_4_0_i8);
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_transform__common_0);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__transform__reschedule_conj_4_0_i13,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i13);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__transform__reschedule_conj_4_0,
		ENTRY(mercury__transform__reschedule_conj_4_0));
Define_label(mercury__transform__reschedule_conj_4_0_i8);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(3), ((Integer) 1));
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_instmap_delta_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_instmap_delta_2_0),
		mercury__transform__reschedule_conj_4_0_i15,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i15);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__instmap__apply_instmap_delta_3_0);
	call_localret(ENTRY(mercury__instmap__apply_instmap_delta_3_0),
		mercury__transform__reschedule_conj_4_0_i16,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i16);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__mode_info__mode_info_set_instmap_3_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_set_instmap_3_0),
		mercury__transform__reschedule_conj_4_0_i17,
		ENTRY(mercury__transform__reschedule_conj_4_0));
	}
Define_label(mercury__transform__reschedule_conj_4_0_i17);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	localcall(mercury__transform__reschedule_conj_4_0,
		LABEL(mercury__transform__reschedule_conj_4_0_i18),
		ENTRY(mercury__transform__reschedule_conj_4_0));
Define_label(mercury__transform__reschedule_conj_4_0_i18);
	update_prof_current_proc(LABEL(mercury__transform__reschedule_conj_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__transform__reschedule_conj_4_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__transform_bunch_0(void)
{
	mercury__transform_module0();
}

#endif

void mercury__transform__init(void); /* suppress gcc warning */
void mercury__transform__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__transform_bunch_0();
#endif
}
