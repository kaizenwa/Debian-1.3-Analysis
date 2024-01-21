/*
** Automatically generated from `stack.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__stack__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__stack__pop_det__ua10000_3_0);
Declare_label(mercury__stack__pop_det__ua10000_3_0_i1003);
Declare_static(mercury__stack__pop__ua0_3_0);
Declare_label(mercury__stack__pop__ua0_3_0_i1);
Declare_static(mercury__stack__top__ua0_2_0);
Declare_label(mercury__stack__top__ua0_2_0_i1);
Declare_static(mercury__stack__push_list__ua10000_3_0);
Declare_label(mercury__stack__push_list__ua10000_3_0_i4);
Declare_label(mercury__stack__push_list__ua10000_3_0_i1002);
Declare_static(mercury__stack__push__ua10000_3_0);
Declare_static(mercury__stack__is_full__ua0_1_0);
Declare_static(mercury__stack__is_empty__ua0_1_0);
Declare_label(mercury__stack__is_empty__ua0_1_0_i1);
Declare_static(mercury__stack__init__ua10000_1_0);
Define_extern_entry(mercury__stack__init_1_0);
Define_extern_entry(mercury__stack__is_empty_1_0);
Define_extern_entry(mercury__stack__is_full_1_0);
Define_extern_entry(mercury__stack__push_3_0);
Define_extern_entry(mercury__stack__push_list_3_0);
Define_extern_entry(mercury__stack__top_2_0);
Declare_label(mercury__stack__top_2_0_i2);
Declare_label(mercury__stack__top_2_0_i1000);
Define_extern_entry(mercury__stack__pop_3_0);
Declare_label(mercury__stack__pop_3_0_i2);
Declare_label(mercury__stack__pop_3_0_i1000);
Define_extern_entry(mercury__stack__pop_det_3_0);
Define_extern_entry(mercury__stack__depth_2_1);
Declare_label(mercury__stack__depth_2_1_i2);
Declare_label(mercury__stack__depth_2_1_i1);
Define_extern_entry(mercury__stack__depth_2_0);
Define_extern_entry(mercury____Unify___stack__stack_1_0);
Define_extern_entry(mercury____Index___stack__stack_1_0);
Define_extern_entry(mercury____Compare___stack__stack_1_0);

extern Word * mercury_data_stack__base_type_layout_stack_1[];
Word * mercury_data_stack__base_type_info_stack_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___stack__stack_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___stack__stack_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___stack__stack_1_0),
	(Word *) (Integer) mercury_data_stack__base_type_layout_stack_1
};

extern Word * mercury_data_stack__common_1[];
Word * mercury_data_stack__base_type_layout_stack_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_stack__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_stack__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_stack__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_stack__common_1)
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_stack__common_0[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) ((Integer) 1)
};

Word * mercury_data_stack__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_stack__common_0)
};

BEGIN_MODULE(mercury__stack_module0)
	init_entry(mercury__stack__pop_det__ua10000_3_0);
	init_label(mercury__stack__pop_det__ua10000_3_0_i1003);
BEGIN_CODE

/* code for predicate 'stack__pop_det__ua10000'/3 in mode 0 */
Define_static(mercury__stack__pop_det__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stack__pop_det__ua10000_3_0_i1003);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	proceed();
Define_label(mercury__stack__pop_det__ua10000_3_0_i1003);
	r1 = string_const("stack__pop_det: pop from empty stack", 36);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__stack__pop_det__ua10000_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__stack_module1)
	init_entry(mercury__stack__pop__ua0_3_0);
	init_label(mercury__stack__pop__ua0_3_0_i1);
BEGIN_CODE

/* code for predicate 'stack__pop__ua0'/3 in mode 0 */
Define_static(mercury__stack__pop__ua0_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stack__pop__ua0_3_0_i1);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__stack__pop__ua0_3_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module2)
	init_entry(mercury__stack__top__ua0_2_0);
	init_label(mercury__stack__top__ua0_2_0_i1);
BEGIN_CODE

/* code for predicate 'stack__top__ua0'/2 in mode 0 */
Define_static(mercury__stack__top__ua0_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stack__top__ua0_2_0_i1);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	proceed();
Define_label(mercury__stack__top__ua0_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module3)
	init_entry(mercury__stack__push_list__ua10000_3_0);
	init_label(mercury__stack__push_list__ua10000_3_0_i4);
	init_label(mercury__stack__push_list__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'stack__push_list__ua10000'/3 in mode 0 */
Define_static(mercury__stack__push_list__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stack__push_list__ua10000_3_0_i1002);
	incr_sp_push_msg(2, "stack__push_list__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	call_localret(STATIC(mercury__stack__push__ua10000_3_0),
		mercury__stack__push_list__ua10000_3_0_i4,
		STATIC(mercury__stack__push_list__ua10000_3_0));
Define_label(mercury__stack__push_list__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__stack__push_list__ua10000_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__stack__push_list__ua10000_3_0,
		STATIC(mercury__stack__push_list__ua10000_3_0));
Define_label(mercury__stack__push_list__ua10000_3_0_i1002);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module4)
	init_entry(mercury__stack__push__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'stack__push__ua10000'/3 in mode 0 */
Define_static(mercury__stack__push__ua10000_3_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module5)
	init_entry(mercury__stack__is_full__ua0_1_0);
BEGIN_CODE

/* code for predicate 'stack__is_full__ua0'/1 in mode 0 */
Define_static(mercury__stack__is_full__ua0_1_0);
	{
	Declare_entry(mercury__std_util__semidet_fail_0_0);
	tailcall(ENTRY(mercury__std_util__semidet_fail_0_0),
		STATIC(mercury__stack__is_full__ua0_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__stack_module6)
	init_entry(mercury__stack__is_empty__ua0_1_0);
	init_label(mercury__stack__is_empty__ua0_1_0_i1);
BEGIN_CODE

/* code for predicate 'stack__is_empty__ua0'/1 in mode 0 */
Define_static(mercury__stack__is_empty__ua0_1_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__stack__is_empty__ua0_1_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__stack__is_empty__ua0_1_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module7)
	init_entry(mercury__stack__init__ua10000_1_0);
BEGIN_CODE

/* code for predicate 'stack__init__ua10000'/1 in mode 0 */
Define_static(mercury__stack__init__ua10000_1_0);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module8)
	init_entry(mercury__stack__init_1_0);
BEGIN_CODE

/* code for predicate 'stack__init'/1 in mode 0 */
Define_entry(mercury__stack__init_1_0);
	tailcall(STATIC(mercury__stack__init__ua10000_1_0),
		ENTRY(mercury__stack__init_1_0));
END_MODULE

BEGIN_MODULE(mercury__stack_module9)
	init_entry(mercury__stack__is_empty_1_0);
BEGIN_CODE

/* code for predicate 'stack__is_empty'/1 in mode 0 */
Define_entry(mercury__stack__is_empty_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__stack__is_empty__ua0_1_0),
		ENTRY(mercury__stack__is_empty_1_0));
END_MODULE

BEGIN_MODULE(mercury__stack_module10)
	init_entry(mercury__stack__is_full_1_0);
BEGIN_CODE

/* code for predicate 'stack__is_full'/1 in mode 0 */
Define_entry(mercury__stack__is_full_1_0);
	tailcall(STATIC(mercury__stack__is_full__ua0_1_0),
		ENTRY(mercury__stack__is_full_1_0));
END_MODULE

BEGIN_MODULE(mercury__stack_module11)
	init_entry(mercury__stack__push_3_0);
BEGIN_CODE

/* code for predicate 'stack__push'/3 in mode 0 */
Define_entry(mercury__stack__push_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__stack__push__ua10000_3_0),
		ENTRY(mercury__stack__push_3_0));
END_MODULE

BEGIN_MODULE(mercury__stack_module12)
	init_entry(mercury__stack__push_list_3_0);
BEGIN_CODE

/* code for predicate 'stack__push_list'/3 in mode 0 */
Define_entry(mercury__stack__push_list_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__stack__push_list__ua10000_3_0),
		ENTRY(mercury__stack__push_list_3_0));
END_MODULE

BEGIN_MODULE(mercury__stack_module13)
	init_entry(mercury__stack__top_2_0);
	init_label(mercury__stack__top_2_0_i2);
	init_label(mercury__stack__top_2_0_i1000);
BEGIN_CODE

/* code for predicate 'stack__top'/2 in mode 0 */
Define_entry(mercury__stack__top_2_0);
	incr_sp_push_msg(2, "stack__top");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__stack__top__ua0_2_0),
		mercury__stack__top_2_0_i2,
		ENTRY(mercury__stack__top_2_0));
Define_label(mercury__stack__top_2_0_i2);
	update_prof_current_proc(LABEL(mercury__stack__top_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__stack__top_2_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__stack__top_2_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module14)
	init_entry(mercury__stack__pop_3_0);
	init_label(mercury__stack__pop_3_0_i2);
	init_label(mercury__stack__pop_3_0_i1000);
BEGIN_CODE

/* code for predicate 'stack__pop'/3 in mode 0 */
Define_entry(mercury__stack__pop_3_0);
	incr_sp_push_msg(2, "stack__pop");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__stack__pop__ua0_3_0),
		mercury__stack__pop_3_0_i2,
		ENTRY(mercury__stack__pop_3_0));
Define_label(mercury__stack__pop_3_0_i2);
	update_prof_current_proc(LABEL(mercury__stack__pop_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__stack__pop_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__stack__pop_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module15)
	init_entry(mercury__stack__pop_det_3_0);
BEGIN_CODE

/* code for predicate 'stack__pop_det'/3 in mode 0 */
Define_entry(mercury__stack__pop_det_3_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__stack__pop_det__ua10000_3_0),
		ENTRY(mercury__stack__pop_det_3_0));
END_MODULE

BEGIN_MODULE(mercury__stack_module16)
	init_entry(mercury__stack__depth_2_1);
	init_label(mercury__stack__depth_2_1_i2);
	init_label(mercury__stack__depth_2_1_i1);
BEGIN_CODE

/* code for predicate 'stack__depth'/2 in mode 1 */
Define_entry(mercury__stack__depth_2_1);
	incr_sp_push_msg(2, "stack__depth");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__stack__depth_2_1_i2,
		ENTRY(mercury__stack__depth_2_1));
	}
Define_label(mercury__stack__depth_2_1_i2);
	update_prof_current_proc(LABEL(mercury__stack__depth_2_1));
	if (((Integer) detstackvar(1) != (Integer) r1))
		GOTO_LABEL(mercury__stack__depth_2_1_i1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__stack__depth_2_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__stack_module17)
	init_entry(mercury__stack__depth_2_0);
BEGIN_CODE

/* code for predicate 'stack__depth'/2 in mode 0 */
Define_entry(mercury__stack__depth_2_0);
	{
	Declare_entry(mercury__list__length_2_0);
	tailcall(ENTRY(mercury__list__length_2_0),
		ENTRY(mercury__stack__depth_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__stack_module18)
	init_entry(mercury____Unify___stack__stack_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___stack__stack_1_0);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury____Unify___stack__stack_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__stack_module19)
	init_entry(mercury____Index___stack__stack_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___stack__stack_1_0);
	{
	Declare_entry(mercury____Index___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Index___mercury_builtin__list_1_0),
		ENTRY(mercury____Index___stack__stack_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__stack_module20)
	init_entry(mercury____Compare___stack__stack_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___stack__stack_1_0);
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		ENTRY(mercury____Compare___stack__stack_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__stack_bunch_0(void)
{
	mercury__stack_module0();
	mercury__stack_module1();
	mercury__stack_module2();
	mercury__stack_module3();
	mercury__stack_module4();
	mercury__stack_module5();
	mercury__stack_module6();
	mercury__stack_module7();
	mercury__stack_module8();
	mercury__stack_module9();
	mercury__stack_module10();
	mercury__stack_module11();
	mercury__stack_module12();
	mercury__stack_module13();
	mercury__stack_module14();
	mercury__stack_module15();
	mercury__stack_module16();
	mercury__stack_module17();
	mercury__stack_module18();
	mercury__stack_module19();
	mercury__stack_module20();
}

#endif

void mercury__stack__init(void); /* suppress gcc warning */
void mercury__stack__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__stack_bunch_0();
#endif
}
