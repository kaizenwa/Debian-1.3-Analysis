/*
** Automatically generated from `queue.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__queue__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__queue__list_to_queue__ua10000_2_0);
Declare_static(mercury__queue__put_list__ua10000_3_0);
Declare_label(mercury__queue__put_list__ua10000_3_0_i4);
Declare_label(mercury__queue__put_list__ua10000_3_0_i1002);
Declare_static(mercury__queue__put__ua10000_3_0);
Declare_static(mercury__queue__is_full__ua0_1_0);
Declare_static(mercury__queue__is_empty__ua0_1_0);
Declare_label(mercury__queue__is_empty__ua0_1_0_i1);
Declare_static(mercury__queue__init__ua10000_1_0);
Define_extern_entry(mercury__queue__init_1_0);
Define_extern_entry(mercury__queue__equal_2_0);
Declare_label(mercury__queue__equal_2_0_i2);
Declare_label(mercury__queue__equal_2_0_i3);
Declare_label(mercury__queue__equal_2_0_i4);
Declare_label(mercury__queue__equal_2_0_i5);
Define_extern_entry(mercury__queue__is_empty_1_0);
Define_extern_entry(mercury__queue__is_full_1_0);
Define_extern_entry(mercury__queue__put_3_0);
Define_extern_entry(mercury__queue__put_list_3_0);
Define_extern_entry(mercury__queue__first_2_0);
Declare_label(mercury__queue__first_2_0_i4);
Declare_label(mercury__queue__first_2_0_i1003);
Declare_label(mercury__queue__first_2_0_i1005);
Define_extern_entry(mercury__queue__get_3_0);
Declare_label(mercury__queue__get_3_0_i4);
Declare_label(mercury__queue__get_3_0_i1001);
Declare_label(mercury__queue__get_3_0_i1);
Define_extern_entry(mercury__queue__length_2_0);
Declare_label(mercury__queue__length_2_0_i2);
Declare_label(mercury__queue__length_2_0_i3);
Define_extern_entry(mercury__queue__list_to_queue_2_0);
Define_extern_entry(mercury____Unify___queue__queue_1_0);
Define_extern_entry(mercury____Index___queue__queue_1_0);
Define_extern_entry(mercury____Compare___queue__queue_1_0);

extern Word * mercury_data_queue__base_type_layout_queue_1[];
Word * mercury_data_queue__base_type_info_queue_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___queue__queue_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___queue__queue_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___queue__queue_1_0),
	(Word *) (Integer) mercury_data_queue__base_type_layout_queue_1
};

extern Word * mercury_data_queue__common_3[];
Word * mercury_data_queue__base_type_layout_queue_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_queue__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_queue__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_queue__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_queue__common_3)
};

Word mercury_data_queue__common_0[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 0))),
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_queue__common_1[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) ((Integer) 1)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
Word * mercury_data_queue__common_2[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_queue__common_1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_queue__common_1)
};

Word * mercury_data_queue__common_3[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_queue__common_2)
};

BEGIN_MODULE(mercury__queue_module0)
	init_entry(mercury__queue__list_to_queue__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'queue__list_to_queue__ua10000'/2 in mode 0 */
Define_static(mercury__queue__list_to_queue__ua10000_2_0);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__queue_module1)
	init_entry(mercury__queue__put_list__ua10000_3_0);
	init_label(mercury__queue__put_list__ua10000_3_0_i4);
	init_label(mercury__queue__put_list__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'queue__put_list__ua10000'/3 in mode 0 */
Define_static(mercury__queue__put_list__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__queue__put_list__ua10000_3_0_i1002);
	incr_sp_push_msg(2, "queue__put_list__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	call_localret(STATIC(mercury__queue__put__ua10000_3_0),
		mercury__queue__put_list__ua10000_3_0_i4,
		STATIC(mercury__queue__put_list__ua10000_3_0));
Define_label(mercury__queue__put_list__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__queue__put_list__ua10000_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__queue__put_list__ua10000_3_0,
		STATIC(mercury__queue__put_list__ua10000_3_0));
Define_label(mercury__queue__put_list__ua10000_3_0_i1002);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__queue_module2)
	init_entry(mercury__queue__put__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'queue__put__ua10000'/3 in mode 0 */
Define_static(mercury__queue__put__ua10000_3_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	proceed();
	}
END_MODULE

BEGIN_MODULE(mercury__queue_module3)
	init_entry(mercury__queue__is_full__ua0_1_0);
BEGIN_CODE

/* code for predicate 'queue__is_full__ua0'/1 in mode 0 */
Define_static(mercury__queue__is_full__ua0_1_0);
	{
	Declare_entry(mercury__std_util__semidet_fail_0_0);
	tailcall(ENTRY(mercury__std_util__semidet_fail_0_0),
		STATIC(mercury__queue__is_full__ua0_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__queue_module4)
	init_entry(mercury__queue__is_empty__ua0_1_0);
	init_label(mercury__queue__is_empty__ua0_1_0_i1);
BEGIN_CODE

/* code for predicate 'queue__is_empty__ua0'/1 in mode 0 */
Define_static(mercury__queue__is_empty__ua0_1_0);
	if (((Integer) field(mktag(0), (Integer) r1, ((Integer) 0)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__queue__is_empty__ua0_1_0_i1);
	if (((Integer) field(mktag(0), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__queue__is_empty__ua0_1_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__queue__is_empty__ua0_1_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__queue_module5)
	init_entry(mercury__queue__init__ua10000_1_0);
BEGIN_CODE

/* code for predicate 'queue__init__ua10000'/1 in mode 0 */
Define_static(mercury__queue__init__ua10000_1_0);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_queue__common_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__queue_module6)
	init_entry(mercury__queue__init_1_0);
BEGIN_CODE

/* code for predicate 'queue__init'/1 in mode 0 */
Define_entry(mercury__queue__init_1_0);
	tailcall(STATIC(mercury__queue__init__ua10000_1_0),
		ENTRY(mercury__queue__init_1_0));
END_MODULE

BEGIN_MODULE(mercury__queue_module7)
	init_entry(mercury__queue__equal_2_0);
	init_label(mercury__queue__equal_2_0_i2);
	init_label(mercury__queue__equal_2_0_i3);
	init_label(mercury__queue__equal_2_0_i4);
	init_label(mercury__queue__equal_2_0_i5);
BEGIN_CODE

/* code for predicate 'queue__equal'/2 in mode 0 */
Define_entry(mercury__queue__equal_2_0);
	incr_sp_push_msg(5, "queue__equal");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) r1;
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__queue__equal_2_0_i2,
		ENTRY(mercury__queue__equal_2_0));
	}
Define_label(mercury__queue__equal_2_0_i2);
	update_prof_current_proc(LABEL(mercury__queue__equal_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__queue__equal_2_0_i3,
		ENTRY(mercury__queue__equal_2_0));
	}
Define_label(mercury__queue__equal_2_0_i3);
	update_prof_current_proc(LABEL(mercury__queue__equal_2_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__queue__equal_2_0_i4,
		ENTRY(mercury__queue__equal_2_0));
	}
Define_label(mercury__queue__equal_2_0_i4);
	update_prof_current_proc(LABEL(mercury__queue__equal_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__queue__equal_2_0_i5,
		ENTRY(mercury__queue__equal_2_0));
	}
Define_label(mercury__queue__equal_2_0_i5);
	update_prof_current_proc(LABEL(mercury__queue__equal_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury__queue__equal_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__queue_module8)
	init_entry(mercury__queue__is_empty_1_0);
BEGIN_CODE

/* code for predicate 'queue__is_empty'/1 in mode 0 */
Define_entry(mercury__queue__is_empty_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__queue__is_empty__ua0_1_0),
		ENTRY(mercury__queue__is_empty_1_0));
END_MODULE

BEGIN_MODULE(mercury__queue_module9)
	init_entry(mercury__queue__is_full_1_0);
BEGIN_CODE

/* code for predicate 'queue__is_full'/1 in mode 0 */
Define_entry(mercury__queue__is_full_1_0);
	tailcall(STATIC(mercury__queue__is_full__ua0_1_0),
		ENTRY(mercury__queue__is_full_1_0));
END_MODULE

BEGIN_MODULE(mercury__queue_module10)
	init_entry(mercury__queue__put_3_0);
BEGIN_CODE

/* code for predicate 'queue__put'/3 in mode 0 */
Define_entry(mercury__queue__put_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__queue__put__ua10000_3_0),
		ENTRY(mercury__queue__put_3_0));
END_MODULE

BEGIN_MODULE(mercury__queue_module11)
	init_entry(mercury__queue__put_list_3_0);
BEGIN_CODE

/* code for predicate 'queue__put_list'/3 in mode 0 */
Define_entry(mercury__queue__put_list_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__queue__put_list__ua10000_3_0),
		ENTRY(mercury__queue__put_list_3_0));
END_MODULE

BEGIN_MODULE(mercury__queue_module12)
	init_entry(mercury__queue__first_2_0);
	init_label(mercury__queue__first_2_0_i4);
	init_label(mercury__queue__first_2_0_i1003);
	init_label(mercury__queue__first_2_0_i1005);
BEGIN_CODE

/* code for predicate 'queue__first'/2 in mode 0 */
Define_entry(mercury__queue__first_2_0);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__queue__first_2_0_i1003);
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(1, "queue__first");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__queue__first_2_0_i4,
		ENTRY(mercury__queue__first_2_0));
	}
Define_label(mercury__queue__first_2_0_i4);
	update_prof_current_proc(LABEL(mercury__queue__first_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__queue__first_2_0_i1005);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	proceed();
Define_label(mercury__queue__first_2_0_i1003);
	r2 = (Integer) field(mktag(1), (Integer) field(mktag(0), (Integer) r2, ((Integer) 1)), ((Integer) 0));
	r1 = TRUE;
	proceed();
Define_label(mercury__queue__first_2_0_i1005);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__queue_module13)
	init_entry(mercury__queue__get_3_0);
	init_label(mercury__queue__get_3_0_i4);
	init_label(mercury__queue__get_3_0_i1001);
	init_label(mercury__queue__get_3_0_i1);
BEGIN_CODE

/* code for predicate 'queue__get'/3 in mode 0 */
Define_entry(mercury__queue__get_3_0);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__queue__get_3_0_i1001);
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(1, "queue__get");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__queue__get_3_0_i4,
		ENTRY(mercury__queue__get_3_0));
	}
Define_label(mercury__queue__get_3_0_i4);
	update_prof_current_proc(LABEL(mercury__queue__get_3_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__queue__get_3_0_i1);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__queue__get_3_0_i1001);
	r1 = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) field(mktag(0), (Integer) r1, ((Integer) 1)), ((Integer) 0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) field(mktag(0), (Integer) r1, ((Integer) 1)), ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__queue__get_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__queue_module14)
	init_entry(mercury__queue__length_2_0);
	init_label(mercury__queue__length_2_0_i2);
	init_label(mercury__queue__length_2_0_i3);
BEGIN_CODE

/* code for predicate 'queue__length'/2 in mode 0 */
Define_entry(mercury__queue__length_2_0);
	incr_sp_push_msg(3, "queue__length");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__queue__length_2_0_i2,
		ENTRY(mercury__queue__length_2_0));
	}
Define_label(mercury__queue__length_2_0_i2);
	update_prof_current_proc(LABEL(mercury__queue__length_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__queue__length_2_0_i3,
		ENTRY(mercury__queue__length_2_0));
	}
Define_label(mercury__queue__length_2_0_i3);
	update_prof_current_proc(LABEL(mercury__queue__length_2_0));
	r1 = ((Integer) detstackvar(1) + (Integer) r1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__queue_module15)
	init_entry(mercury__queue__list_to_queue_2_0);
BEGIN_CODE

/* code for predicate 'queue__list_to_queue'/2 in mode 0 */
Define_entry(mercury__queue__list_to_queue_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__queue__list_to_queue__ua10000_2_0),
		ENTRY(mercury__queue__list_to_queue_2_0));
END_MODULE

BEGIN_MODULE(mercury__queue_module16)
	init_entry(mercury____Unify___queue__queue_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___queue__queue_1_0);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) r5;
	{
	Declare_entry(mercury____Unify___std_util__pair_2_0);
	tailcall(ENTRY(mercury____Unify___std_util__pair_2_0),
		ENTRY(mercury____Unify___queue__queue_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__queue_module17)
	init_entry(mercury____Index___queue__queue_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___queue__queue_1_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) r4;
	{
	Declare_entry(mercury____Index___std_util__pair_2_0);
	tailcall(ENTRY(mercury____Index___std_util__pair_2_0),
		ENTRY(mercury____Index___queue__queue_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__queue_module18)
	init_entry(mercury____Compare___queue__queue_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___queue__queue_1_0);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) r5;
	{
	Declare_entry(mercury____Compare___std_util__pair_2_0);
	tailcall(ENTRY(mercury____Compare___std_util__pair_2_0),
		ENTRY(mercury____Compare___queue__queue_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__queue_bunch_0(void)
{
	mercury__queue_module0();
	mercury__queue_module1();
	mercury__queue_module2();
	mercury__queue_module3();
	mercury__queue_module4();
	mercury__queue_module5();
	mercury__queue_module6();
	mercury__queue_module7();
	mercury__queue_module8();
	mercury__queue_module9();
	mercury__queue_module10();
	mercury__queue_module11();
	mercury__queue_module12();
	mercury__queue_module13();
	mercury__queue_module14();
	mercury__queue_module15();
	mercury__queue_module16();
	mercury__queue_module17();
	mercury__queue_module18();
}

#endif

void mercury__queue__init(void); /* suppress gcc warning */
void mercury__queue__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__queue_bunch_0();
#endif
}
