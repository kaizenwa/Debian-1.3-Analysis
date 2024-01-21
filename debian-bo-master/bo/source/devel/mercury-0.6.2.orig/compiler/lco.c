/*
** Automatically generated from `lco.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__lco__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__lco__lco_in_conj__ua10000_4_0);
Declare_label(mercury__lco__lco_in_conj__ua10000_4_0_i1009);
Declare_label(mercury__lco__lco_in_conj__ua10000_4_0_i4);
Declare_label(mercury__lco__lco_in_conj__ua10000_4_0_i12);
Declare_label(mercury__lco__lco_in_conj__ua10000_4_0_i13);
Declare_label(mercury__lco__lco_in_conj__ua10000_4_0_i9);
Declare_label(mercury__lco__lco_in_conj__ua10000_4_0_i15);
Declare_label(mercury__lco__lco_in_conj__ua10000_4_0_i1007);
Declare_static(mercury__lco__lco_in_cases__ua10000_3_0);
Declare_label(mercury__lco__lco_in_cases__ua10000_3_0_i4);
Declare_label(mercury__lco__lco_in_cases__ua10000_3_0_i5);
Declare_label(mercury__lco__lco_in_cases__ua10000_3_0_i1003);
Declare_static(mercury__lco__lco_in_disj__ua10000_3_0);
Declare_label(mercury__lco__lco_in_disj__ua10000_3_0_i4);
Declare_label(mercury__lco__lco_in_disj__ua10000_3_0_i5);
Declare_label(mercury__lco__lco_in_disj__ua10000_3_0_i1002);
Declare_static(mercury__lco__lco_in_goal_2__ua10000_3_0);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1016);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1015);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1014);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1013);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i5);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i6);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i8);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i9);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i11);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i12);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i13);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i14);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i15);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1012);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i18);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i19);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1009);
Declare_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i2);
Declare_static(mercury__lco__lco_in_goal__ua10000_3_0);
Declare_label(mercury__lco__lco_in_goal__ua10000_3_0_i2);
Define_extern_entry(mercury__lco__lco_modulo_constructors_7_0);
Declare_label(mercury__lco__lco_modulo_constructors_7_0_i2);
Declare_label(mercury__lco__lco_modulo_constructors_7_0_i3);
Declare_label(mercury__lco__lco_modulo_constructors_7_0_i6);
Declare_label(mercury__lco__lco_modulo_constructors_7_0_i5);
Declare_label(mercury__lco__lco_modulo_constructors_7_0_i8);
Declare_label(mercury__lco__lco_modulo_constructors_7_0_i9);
Declare_label(mercury__lco__lco_modulo_constructors_7_0_i10);

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_info_0[];
Word * mercury_data_lco__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_info_0
};

BEGIN_MODULE(mercury__lco_module0)
	init_entry(mercury__lco__lco_in_conj__ua10000_4_0);
	init_label(mercury__lco__lco_in_conj__ua10000_4_0_i1009);
	init_label(mercury__lco__lco_in_conj__ua10000_4_0_i4);
	init_label(mercury__lco__lco_in_conj__ua10000_4_0_i12);
	init_label(mercury__lco__lco_in_conj__ua10000_4_0_i13);
	init_label(mercury__lco__lco_in_conj__ua10000_4_0_i9);
	init_label(mercury__lco__lco_in_conj__ua10000_4_0_i15);
	init_label(mercury__lco__lco_in_conj__ua10000_4_0_i1007);
BEGIN_CODE

/* code for predicate 'lco_in_conj__ua10000'/4 in mode 0 */
Define_static(mercury__lco__lco_in_conj__ua10000_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__lco__lco_in_conj__ua10000_4_0_i1007);
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__lco__lco_in_conj__ua10000_4_0_i1009);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__lco__lco_in_conj__ua10000_4_0_i1009);
	r6 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 4));
	incr_sp_push_msg(2, "lco_in_conj__ua10000");
	detstackvar(2) = (Integer) succip;
	if ((tag((Integer) r6) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lco__lco_in_conj__ua10000_4_0_i4);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) r3;
	r1 = (Integer) r5;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__lco__lco_in_conj__ua10000_4_0,
		STATIC(mercury__lco__lco_in_conj__ua10000_4_0));
Define_label(mercury__lco__lco_in_conj__ua10000_4_0_i1009);
	incr_sp_push_msg(2, "lco_in_conj__ua10000");
	detstackvar(2) = (Integer) succip;
Define_label(mercury__lco__lco_in_conj__ua10000_4_0_i4);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lco__lco_in_conj__ua10000_4_0_i9);
	detstackvar(1) = (Integer) r5;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lco__common_0);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__lco__lco_in_conj__ua10000_4_0_i12,
		STATIC(mercury__lco__lco_in_conj__ua10000_4_0));
	}
Define_label(mercury__lco__lco_in_conj__ua10000_4_0_i12);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_conj__ua10000_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lco__common_0);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__lco__lco_in_conj__ua10000_4_0_i13,
		STATIC(mercury__lco__lco_in_conj__ua10000_4_0));
	}
Define_label(mercury__lco__lco_in_conj__ua10000_4_0_i13);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_conj__ua10000_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lco__common_0);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__lco__lco_in_conj__ua10000_4_0));
	}
Define_label(mercury__lco__lco_in_conj__ua10000_4_0_i9);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lco__common_0);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__lco__lco_in_conj__ua10000_4_0_i15,
		STATIC(mercury__lco__lco_in_conj__ua10000_4_0));
	}
Define_label(mercury__lco__lco_in_conj__ua10000_4_0_i15);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_conj__ua10000_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lco__common_0);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__lco__lco_in_conj__ua10000_4_0));
	}
Define_label(mercury__lco__lco_in_conj__ua10000_4_0_i1007);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lco_module1)
	init_entry(mercury__lco__lco_in_cases__ua10000_3_0);
	init_label(mercury__lco__lco_in_cases__ua10000_3_0_i4);
	init_label(mercury__lco__lco_in_cases__ua10000_3_0_i5);
	init_label(mercury__lco__lco_in_cases__ua10000_3_0_i1003);
BEGIN_CODE

/* code for predicate 'lco_in_cases__ua10000'/3 in mode 0 */
Define_static(mercury__lco__lco_in_cases__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__lco__lco_in_cases__ua10000_3_0_i1003);
	incr_sp_push_msg(3, "lco_in_cases__ua10000");
	detstackvar(3) = (Integer) succip;
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	call_localret(STATIC(mercury__lco__lco_in_goal__ua10000_3_0),
		mercury__lco__lco_in_cases__ua10000_3_0_i4,
		STATIC(mercury__lco__lco_in_cases__ua10000_3_0));
Define_label(mercury__lco__lco_in_cases__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_cases__ua10000_3_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__lco__lco_in_cases__ua10000_3_0,
		LABEL(mercury__lco__lco_in_cases__ua10000_3_0_i5),
		STATIC(mercury__lco__lco_in_cases__ua10000_3_0));
Define_label(mercury__lco__lco_in_cases__ua10000_3_0_i5);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_cases__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lco__lco_in_cases__ua10000_3_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lco_module2)
	init_entry(mercury__lco__lco_in_disj__ua10000_3_0);
	init_label(mercury__lco__lco_in_disj__ua10000_3_0_i4);
	init_label(mercury__lco__lco_in_disj__ua10000_3_0_i5);
	init_label(mercury__lco__lco_in_disj__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'lco_in_disj__ua10000'/3 in mode 0 */
Define_static(mercury__lco__lco_in_disj__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__lco__lco_in_disj__ua10000_3_0_i1002);
	incr_sp_push_msg(2, "lco_in_disj__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__lco__lco_in_goal__ua10000_3_0),
		mercury__lco__lco_in_disj__ua10000_3_0_i4,
		STATIC(mercury__lco__lco_in_disj__ua10000_3_0));
Define_label(mercury__lco__lco_in_disj__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_disj__ua10000_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__lco__lco_in_disj__ua10000_3_0,
		LABEL(mercury__lco__lco_in_disj__ua10000_3_0_i5),
		STATIC(mercury__lco__lco_in_disj__ua10000_3_0));
Define_label(mercury__lco__lco_in_disj__ua10000_3_0_i5);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_disj__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lco__lco_in_disj__ua10000_3_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lco_module3)
	init_entry(mercury__lco__lco_in_goal_2__ua10000_3_0);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1016);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1015);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1014);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1013);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i5);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i6);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i8);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i9);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i11);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i12);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i13);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i14);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i15);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1012);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i18);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i19);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1009);
	init_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i2);
BEGIN_CODE

/* code for predicate 'lco_in_goal_2__ua10000'/3 in mode 0 */
Define_static(mercury__lco__lco_in_goal_2__ua10000_3_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1012);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1016) AND
		LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1009) AND
		LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1015) AND
		LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1009) AND
		LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1014) AND
		LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1013) AND
		LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i1009));
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1016);
	incr_sp_push_msg(5, "lco_in_goal_2__ua10000");
	detstackvar(5) = (Integer) succip;
	GOTO_LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i5);
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1015);
	incr_sp_push_msg(5, "lco_in_goal_2__ua10000");
	detstackvar(5) = (Integer) succip;
	GOTO_LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i8);
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1014);
	incr_sp_push_msg(5, "lco_in_goal_2__ua10000");
	detstackvar(5) = (Integer) succip;
	GOTO_LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i11);
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1013);
	incr_sp_push_msg(5, "lco_in_goal_2__ua10000");
	detstackvar(5) = (Integer) succip;
	GOTO_LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i13);
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	call_localret(STATIC(mercury__lco__lco_in_cases__ua10000_3_0),
		mercury__lco__lco_in_goal_2__ua10000_3_0_i6,
		STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0));
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 5));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 4)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i8);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__lco__lco_in_disj__ua10000_3_0),
		mercury__lco__lco_in_goal_2__ua10000_3_0_i9,
		STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0));
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i9);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i11);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	call_localret(STATIC(mercury__lco__lco_in_goal__ua10000_3_0),
		mercury__lco__lco_in_goal_2__ua10000_3_0_i12,
		STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0));
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i12);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i13);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	call_localret(STATIC(mercury__lco__lco_in_goal__ua10000_3_0),
		mercury__lco__lco_in_goal_2__ua10000_3_0_i14,
		STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0));
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i14);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lco__lco_in_goal__ua10000_3_0),
		mercury__lco__lco_in_goal_2__ua10000_3_0_i15,
		STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0));
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i15);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(3);
	field(mktag(3), (Integer) r1, ((Integer) 4)) = (Integer) r2;
	field(mktag(3), (Integer) r1, ((Integer) 5)) = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1012);
	incr_sp_push_msg(5, "lco_in_goal_2__ua10000");
	detstackvar(5) = (Integer) succip;
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0_i2);
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lco__common_0);
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__lco__lco_in_goal_2__ua10000_3_0_i18,
		STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0));
	}
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i18);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__lco__lco_in_conj__ua10000_4_0),
		mercury__lco__lco_in_goal_2__ua10000_3_0_i19,
		STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0));
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i19);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal_2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i1009);
	proceed();
Define_label(mercury__lco__lco_in_goal_2__ua10000_3_0_i2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lco_module4)
	init_entry(mercury__lco__lco_in_goal__ua10000_3_0);
	init_label(mercury__lco__lco_in_goal__ua10000_3_0_i2);
BEGIN_CODE

/* code for predicate 'lco_in_goal__ua10000'/3 in mode 0 */
Define_static(mercury__lco__lco_in_goal__ua10000_3_0);
	incr_sp_push_msg(2, "lco_in_goal__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__lco__lco_in_goal_2__ua10000_3_0),
		mercury__lco__lco_in_goal__ua10000_3_0_i2,
		STATIC(mercury__lco__lco_in_goal__ua10000_3_0));
Define_label(mercury__lco__lco_in_goal__ua10000_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lco__lco_in_goal__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lco_module5)
	init_entry(mercury__lco__lco_modulo_constructors_7_0);
	init_label(mercury__lco__lco_modulo_constructors_7_0_i2);
	init_label(mercury__lco__lco_modulo_constructors_7_0_i3);
	init_label(mercury__lco__lco_modulo_constructors_7_0_i6);
	init_label(mercury__lco__lco_modulo_constructors_7_0_i5);
	init_label(mercury__lco__lco_modulo_constructors_7_0_i8);
	init_label(mercury__lco__lco_modulo_constructors_7_0_i9);
	init_label(mercury__lco__lco_modulo_constructors_7_0_i10);
BEGIN_CODE

/* code for predicate 'lco_modulo_constructors'/7 in mode 0 */
Define_entry(mercury__lco__lco_modulo_constructors_7_0);
	incr_sp_push_msg(7, "lco_modulo_constructors");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	r1 = (Integer) r4;
	{
	Declare_entry(mercury__hlds_pred__proc_info_goal_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_goal_2_0),
		mercury__lco__lco_modulo_constructors_7_0_i2,
		ENTRY(mercury__lco__lco_modulo_constructors_7_0));
	}
Define_label(mercury__lco__lco_modulo_constructors_7_0_i2);
	update_prof_current_proc(LABEL(mercury__lco__lco_modulo_constructors_7_0));
	detstackvar(6) = (Integer) r1;
	call_localret(STATIC(mercury__lco__lco_in_goal__ua10000_3_0),
		mercury__lco__lco_modulo_constructors_7_0_i3,
		ENTRY(mercury__lco__lco_modulo_constructors_7_0));
Define_label(mercury__lco__lco_modulo_constructors_7_0_i3);
	update_prof_current_proc(LABEL(mercury__lco__lco_modulo_constructors_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0;
	r2 = (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_info_0;
	r4 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury____Unify___std_util__pair_2_0);
	call_localret(ENTRY(mercury____Unify___std_util__pair_2_0),
		mercury__lco__lco_modulo_constructors_7_0_i6,
		ENTRY(mercury__lco__lco_modulo_constructors_7_0));
	}
Define_label(mercury__lco__lco_modulo_constructors_7_0_i6);
	update_prof_current_proc(LABEL(mercury__lco__lco_modulo_constructors_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lco__lco_modulo_constructors_7_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__lco__lco_modulo_constructors_7_0_i5);
	r1 = string_const("% Can introduce LCO in ", 23);
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__lco__lco_modulo_constructors_7_0_i8,
		ENTRY(mercury__lco__lco_modulo_constructors_7_0));
	}
Define_label(mercury__lco__lco_modulo_constructors_7_0_i8);
	update_prof_current_proc(LABEL(mercury__lco__lco_modulo_constructors_7_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_out__write_pred_proc_id_5_0);
	call_localret(ENTRY(mercury__hlds_out__write_pred_proc_id_5_0),
		mercury__lco__lco_modulo_constructors_7_0_i9,
		ENTRY(mercury__lco__lco_modulo_constructors_7_0));
	}
Define_label(mercury__lco__lco_modulo_constructors_7_0_i9);
	update_prof_current_proc(LABEL(mercury__lco__lco_modulo_constructors_7_0));
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__lco__lco_modulo_constructors_7_0_i10,
		ENTRY(mercury__lco__lco_modulo_constructors_7_0));
	}
Define_label(mercury__lco__lco_modulo_constructors_7_0_i10);
	update_prof_current_proc(LABEL(mercury__lco__lco_modulo_constructors_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__lco_bunch_0(void)
{
	mercury__lco_module0();
	mercury__lco_module1();
	mercury__lco_module2();
	mercury__lco_module3();
	mercury__lco_module4();
	mercury__lco_module5();
}

#endif

void mercury__lco__init(void); /* suppress gcc warning */
void mercury__lco__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__lco_bunch_0();
#endif
}
