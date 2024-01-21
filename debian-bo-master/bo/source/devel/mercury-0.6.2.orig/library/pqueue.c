/*
** Automatically generated from `pqueue.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__pqueue__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___pqueue_pqueue_2__ua10000_2_0);
Declare_label(mercury____Index___pqueue_pqueue_2__ua10000_2_0_i3);
Declare_static(mercury__pqueue__remove_2__ua10000_3_0);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i6);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i10);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i11);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i7);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i12);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i13);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i1007);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i1010);
Declare_label(mercury__pqueue__remove_2__ua10000_3_0_i1009);
Declare_static(mercury__pqueue__insert_2__ua10000_6_0);
Declare_label(mercury__pqueue__insert_2__ua10000_6_0_i8);
Declare_label(mercury__pqueue__insert_2__ua10000_6_0_i6);
Declare_label(mercury__pqueue__insert_2__ua10000_6_0_i9);
Declare_label(mercury__pqueue__insert_2__ua10000_6_0_i1008);
Declare_label(mercury__pqueue__insert_2__ua10000_6_0_i1015);
Declare_label(mercury__pqueue__insert_2__ua10000_6_0_i1014);
Declare_static(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0);
Declare_label(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i4);
Declare_label(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i1003);
Declare_static(mercury__pqueue__to_assoc_list__ua10000_2_0);
Declare_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i4);
Declare_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i6);
Declare_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i3);
Declare_static(mercury__pqueue__remove__ua0_4_0);
Declare_label(mercury__pqueue__remove__ua0_4_0_i3);
Declare_label(mercury__pqueue__remove__ua0_4_0_i1002);
Declare_static(mercury__pqueue__insert__ua10000_4_0);
Declare_label(mercury__pqueue__insert__ua10000_4_0_i4);
Declare_label(mercury__pqueue__insert__ua10000_4_0_i8);
Declare_label(mercury__pqueue__insert__ua10000_4_0_i5);
Declare_label(mercury__pqueue__insert__ua10000_4_0_i9);
Declare_label(mercury__pqueue__insert__ua10000_4_0_i1007);
Declare_static(mercury__pqueue__init__ua10000_1_0);
Define_extern_entry(mercury__pqueue__init_1_0);
Define_extern_entry(mercury__pqueue__insert_4_0);
Define_extern_entry(mercury__pqueue__remove_4_0);
Declare_label(mercury__pqueue__remove_4_0_i2);
Declare_label(mercury__pqueue__remove_4_0_i1000);
Define_extern_entry(mercury__pqueue__to_assoc_list_2_0);
Define_extern_entry(mercury__pqueue__assoc_list_to_pqueue_2_0);
Define_extern_entry(mercury____Unify___pqueue__pqueue_2_0);
Declare_label(mercury____Unify___pqueue__pqueue_2_0_i1012);
Declare_label(mercury____Unify___pqueue__pqueue_2_0_i6);
Declare_label(mercury____Unify___pqueue__pqueue_2_0_i8);
Declare_label(mercury____Unify___pqueue__pqueue_2_0_i10);
Declare_label(mercury____Unify___pqueue__pqueue_2_0_i1009);
Declare_label(mercury____Unify___pqueue__pqueue_2_0_i1);
Define_extern_entry(mercury____Index___pqueue__pqueue_2_0);
Define_extern_entry(mercury____Compare___pqueue__pqueue_2_0);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i2);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i3);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i4);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i6);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i11);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i16);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i17);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i15);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i22);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i28);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i34);
Declare_label(mercury____Compare___pqueue__pqueue_2_0_i9);

extern Word * mercury_data_pqueue__base_type_layout_pqueue_2[];
Word * mercury_data_pqueue__base_type_info_pqueue_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) ENTRY(mercury____Unify___pqueue__pqueue_2_0),
	(Word *) (Integer) ENTRY(mercury____Index___pqueue__pqueue_2_0),
	(Word *) (Integer) ENTRY(mercury____Compare___pqueue__pqueue_2_0),
	(Word *) (Integer) mercury_data_pqueue__base_type_layout_pqueue_2
};

extern Word * mercury_data_pqueue__common_0[];
extern Word * mercury_data_pqueue__common_3[];
Word * mercury_data_pqueue__base_type_layout_pqueue_2[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_pqueue__common_0),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_pqueue__common_3),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_pqueue__common_0[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 1),
	(Word *) string_const("empty", 5)
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_pqueue__common_1[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_pqueue__common_2[] = {
	(Word *) (Integer) mercury_data_pqueue__base_type_info_pqueue_2,
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2)
};

Word * mercury_data_pqueue__common_3[] = {
	(Word *) ((Integer) 5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_pqueue__common_1),
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_pqueue__common_2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_pqueue__common_2),
	(Word *) string_const("pqueue", 6)
};

BEGIN_MODULE(mercury__pqueue_module0)
	init_entry(mercury____Index___pqueue_pqueue_2__ua10000_2_0);
	init_label(mercury____Index___pqueue_pqueue_2__ua10000_2_0_i3);
BEGIN_CODE

/* code for predicate '__Index___pqueue_pqueue_2__ua10000'/2 in mode 0 */
Define_static(mercury____Index___pqueue_pqueue_2__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Index___pqueue_pqueue_2__ua10000_2_0_i3);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___pqueue_pqueue_2__ua10000_2_0_i3);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module1)
	init_entry(mercury__pqueue__remove_2__ua10000_3_0);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i6);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i10);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i11);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i7);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i12);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i13);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i1007);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i1010);
	init_label(mercury__pqueue__remove_2__ua10000_3_0_i1009);
BEGIN_CODE

/* code for predicate 'pqueue__remove_2__ua10000'/3 in mode 0 */
Define_static(mercury__pqueue__remove_2__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__remove_2__ua10000_3_0_i1010);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__remove_2__ua10000_3_0_i1007);
	incr_sp_push_msg(14, "pqueue__remove_2__ua10000");
	detstackvar(14) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 3));
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 4));
	detstackvar(8) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 4));
	detstackvar(9) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	detstackvar(10) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(12) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) r3;
	detstackvar(11) = (Integer) r2;
	detstackvar(13) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__pqueue__remove_2__ua10000_3_0_i6,
		STATIC(mercury__pqueue__remove_2__ua10000_3_0));
	}
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i6);
	update_prof_current_proc(LABEL(mercury__pqueue__remove_2__ua10000_3_0));
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__pqueue__remove_2__ua10000_3_0_i7);
	r1 = ((Integer) detstackvar(12) - ((Integer) 1));
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__int__max_3_0);
	call_localret(ENTRY(mercury__int__max_3_0),
		mercury__pqueue__remove_2__ua10000_3_0_i10,
		STATIC(mercury__pqueue__remove_2__ua10000_3_0));
	}
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i10);
	update_prof_current_proc(LABEL(mercury__pqueue__remove_2__ua10000_3_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(13);
	r2 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(8);
	localcall(mercury__pqueue__remove_2__ua10000_3_0,
		LABEL(mercury__pqueue__remove_2__ua10000_3_0_i11),
		STATIC(mercury__pqueue__remove_2__ua10000_3_0));
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i11);
	update_prof_current_proc(LABEL(mercury__pqueue__remove_2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(11);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(10);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(14);
	decr_sp_pop_msg(14);
	proceed();
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i7);
	r1 = ((Integer) detstackvar(12) - ((Integer) 1));
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__int__max_3_0);
	call_localret(ENTRY(mercury__int__max_3_0),
		mercury__pqueue__remove_2__ua10000_3_0_i12,
		STATIC(mercury__pqueue__remove_2__ua10000_3_0));
	}
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i12);
	update_prof_current_proc(LABEL(mercury__pqueue__remove_2__ua10000_3_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(13);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	localcall(mercury__pqueue__remove_2__ua10000_3_0,
		LABEL(mercury__pqueue__remove_2__ua10000_3_0_i13),
		STATIC(mercury__pqueue__remove_2__ua10000_3_0));
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i13);
	update_prof_current_proc(LABEL(mercury__pqueue__remove_2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(14);
	decr_sp_pop_msg(14);
	proceed();
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i1007);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i1010);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__remove_2__ua10000_3_0_i1009);
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__pqueue__remove_2__ua10000_3_0_i1009);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module2)
	init_entry(mercury__pqueue__insert_2__ua10000_6_0);
	init_label(mercury__pqueue__insert_2__ua10000_6_0_i8);
	init_label(mercury__pqueue__insert_2__ua10000_6_0_i6);
	init_label(mercury__pqueue__insert_2__ua10000_6_0_i9);
	init_label(mercury__pqueue__insert_2__ua10000_6_0_i1008);
	init_label(mercury__pqueue__insert_2__ua10000_6_0_i1015);
	init_label(mercury__pqueue__insert_2__ua10000_6_0_i1014);
BEGIN_CODE

/* code for predicate 'pqueue__insert_2__ua10000'/6 in mode 0 */
Define_static(mercury__pqueue__insert_2__ua10000_6_0);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__insert_2__ua10000_6_0_i1015);
	if (((Integer) r5 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__insert_2__ua10000_6_0_i1008);
	r6 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	r7 = (Integer) field(mktag(1), (Integer) r5, ((Integer) 0));
	incr_sp_push_msg(2, "pqueue__insert_2__ua10000");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r6 <= (Integer) r7))
		GOTO_LABEL(mercury__pqueue__insert_2__ua10000_6_0_i6);
	detstackvar(1) = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r5;
	call_localret(STATIC(mercury__pqueue__insert__ua10000_4_0),
		mercury__pqueue__insert_2__ua10000_6_0_i8,
		STATIC(mercury__pqueue__insert_2__ua10000_6_0));
Define_label(mercury__pqueue__insert_2__ua10000_6_0_i8);
	update_prof_current_proc(LABEL(mercury__pqueue__insert_2__ua10000_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__pqueue__insert_2__ua10000_6_0_i6);
	{
	Word tempr1;
	tempr1 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r4;
	r4 = (Integer) tempr1;
	detstackvar(1) = (Integer) r5;
	call_localret(STATIC(mercury__pqueue__insert__ua10000_4_0),
		mercury__pqueue__insert_2__ua10000_6_0_i9,
		STATIC(mercury__pqueue__insert_2__ua10000_6_0));
	}
Define_label(mercury__pqueue__insert_2__ua10000_6_0_i9);
	update_prof_current_proc(LABEL(mercury__pqueue__insert_2__ua10000_6_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__pqueue__insert_2__ua10000_6_0_i1008);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	r1 = (Integer) r4;
	field(mktag(1), (Integer) tempr1, ((Integer) 2)) = (Integer) r3;
	field(mktag(1), (Integer) tempr1, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 4)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = ((Integer) 0);
	proceed();
	}
Define_label(mercury__pqueue__insert_2__ua10000_6_0_i1015);
	incr_sp_push_msg(2, "pqueue__insert_2__ua10000");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r5 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__insert_2__ua10000_6_0_i1014);
	tag_incr_hp(r1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__pqueue__insert_2__ua10000_6_0_i1014);
	tag_incr_hp(r1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module3)
	init_entry(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0);
	init_label(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i4);
	init_label(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i1003);
BEGIN_CODE

/* code for predicate 'pqueue__assoc_list_to_pqueue__ua10000'/2 in mode 0 */
Define_static(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i1003);
	incr_sp_push_msg(4, "pqueue__assoc_list_to_pqueue__ua10000");
	detstackvar(4) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	localcall(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0,
		LABEL(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i4),
		STATIC(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0));
Define_label(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i4);
	update_prof_current_proc(LABEL(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__pqueue__insert__ua10000_4_0),
		STATIC(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0));
Define_label(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0_i1003);
	tailcall(STATIC(mercury__pqueue__init__ua10000_1_0),
		STATIC(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__pqueue_module4)
	init_entry(mercury__pqueue__to_assoc_list__ua10000_2_0);
	init_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i4);
	init_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i6);
	init_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i3);
BEGIN_CODE

/* code for predicate 'pqueue__to_assoc_list__ua10000'/2 in mode 0 */
Define_static(mercury__pqueue__to_assoc_list__ua10000_2_0);
	incr_sp_push_msg(3, "pqueue__to_assoc_list__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__pqueue__remove__ua0_4_0),
		mercury__pqueue__to_assoc_list__ua10000_2_0_i4,
		STATIC(mercury__pqueue__to_assoc_list__ua10000_2_0));
Define_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i4);
	update_prof_current_proc(LABEL(mercury__pqueue__to_assoc_list__ua10000_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__pqueue__to_assoc_list__ua10000_2_0_i3);
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	r2 = (Integer) r4;
	localcall(mercury__pqueue__to_assoc_list__ua10000_2_0,
		LABEL(mercury__pqueue__to_assoc_list__ua10000_2_0_i6),
		STATIC(mercury__pqueue__to_assoc_list__ua10000_2_0));
Define_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i6);
	update_prof_current_proc(LABEL(mercury__pqueue__to_assoc_list__ua10000_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__pqueue__to_assoc_list__ua10000_2_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module5)
	init_entry(mercury__pqueue__remove__ua0_4_0);
	init_label(mercury__pqueue__remove__ua0_4_0_i3);
	init_label(mercury__pqueue__remove__ua0_4_0_i1002);
BEGIN_CODE

/* code for predicate 'pqueue__remove__ua0'/4 in mode 0 */
Define_static(mercury__pqueue__remove__ua0_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__remove__ua0_4_0_i1002);
	incr_sp_push_msg(3, "pqueue__remove__ua0");
	detstackvar(3) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 4));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	call_localret(STATIC(mercury__pqueue__remove_2__ua10000_3_0),
		mercury__pqueue__remove__ua0_4_0_i3,
		STATIC(mercury__pqueue__remove__ua0_4_0));
Define_label(mercury__pqueue__remove__ua0_4_0_i3);
	update_prof_current_proc(LABEL(mercury__pqueue__remove__ua0_4_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__pqueue__remove__ua0_4_0_i1002);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module6)
	init_entry(mercury__pqueue__insert__ua10000_4_0);
	init_label(mercury__pqueue__insert__ua10000_4_0_i4);
	init_label(mercury__pqueue__insert__ua10000_4_0_i8);
	init_label(mercury__pqueue__insert__ua10000_4_0_i5);
	init_label(mercury__pqueue__insert__ua10000_4_0_i9);
	init_label(mercury__pqueue__insert__ua10000_4_0_i1007);
BEGIN_CODE

/* code for predicate 'pqueue__insert__ua10000'/4 in mode 0 */
Define_static(mercury__pqueue__insert__ua10000_4_0);
	incr_sp_push_msg(9, "pqueue__insert__ua10000");
	detstackvar(9) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__pqueue__insert__ua10000_4_0_i1007);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 3));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 4));
	detstackvar(7) = ((Integer) field(mktag(1), (Integer) r2, ((Integer) 0)) + ((Integer) 1));
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) tempr1;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	detstackvar(2) = (Integer) r4;
	detstackvar(8) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__pqueue__insert__ua10000_4_0_i4,
		STATIC(mercury__pqueue__insert__ua10000_4_0));
	}
	}
Define_label(mercury__pqueue__insert__ua10000_4_0_i4);
	update_prof_current_proc(LABEL(mercury__pqueue__insert__ua10000_4_0));
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__pqueue__insert__ua10000_4_0_i5);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__pqueue__insert_2__ua10000_6_0),
		mercury__pqueue__insert__ua10000_4_0_i8,
		STATIC(mercury__pqueue__insert__ua10000_4_0));
Define_label(mercury__pqueue__insert__ua10000_4_0_i8);
	update_prof_current_proc(LABEL(mercury__pqueue__insert__ua10000_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__pqueue__insert__ua10000_4_0_i5);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__pqueue__insert_2__ua10000_6_0),
		mercury__pqueue__insert__ua10000_4_0_i9,
		STATIC(mercury__pqueue__insert__ua10000_4_0));
Define_label(mercury__pqueue__insert__ua10000_4_0_i9);
	update_prof_current_proc(LABEL(mercury__pqueue__insert__ua10000_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__pqueue__insert__ua10000_4_0_i1007);
	tag_incr_hp(r1, mktag(1), ((Integer) 5));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) r4;
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module7)
	init_entry(mercury__pqueue__init__ua10000_1_0);
BEGIN_CODE

/* code for predicate 'pqueue__init__ua10000'/1 in mode 0 */
Define_static(mercury__pqueue__init__ua10000_1_0);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module8)
	init_entry(mercury__pqueue__init_1_0);
BEGIN_CODE

/* code for predicate 'pqueue__init'/1 in mode 0 */
Define_entry(mercury__pqueue__init_1_0);
	tailcall(STATIC(mercury__pqueue__init__ua10000_1_0),
		ENTRY(mercury__pqueue__init_1_0));
END_MODULE

BEGIN_MODULE(mercury__pqueue_module9)
	init_entry(mercury__pqueue__insert_4_0);
BEGIN_CODE

/* code for predicate 'pqueue__insert'/4 in mode 0 */
Define_entry(mercury__pqueue__insert_4_0);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	tailcall(STATIC(mercury__pqueue__insert__ua10000_4_0),
		ENTRY(mercury__pqueue__insert_4_0));
END_MODULE

BEGIN_MODULE(mercury__pqueue_module10)
	init_entry(mercury__pqueue__remove_4_0);
	init_label(mercury__pqueue__remove_4_0_i2);
	init_label(mercury__pqueue__remove_4_0_i1000);
BEGIN_CODE

/* code for predicate 'pqueue__remove'/4 in mode 0 */
Define_entry(mercury__pqueue__remove_4_0);
	incr_sp_push_msg(2, "pqueue__remove");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__pqueue__remove__ua0_4_0),
		mercury__pqueue__remove_4_0_i2,
		ENTRY(mercury__pqueue__remove_4_0));
Define_label(mercury__pqueue__remove_4_0_i2);
	update_prof_current_proc(LABEL(mercury__pqueue__remove_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__pqueue__remove_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__pqueue__remove_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module11)
	init_entry(mercury__pqueue__to_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'pqueue__to_assoc_list'/2 in mode 0 */
Define_entry(mercury__pqueue__to_assoc_list_2_0);
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__pqueue__to_assoc_list__ua10000_2_0),
		ENTRY(mercury__pqueue__to_assoc_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__pqueue_module12)
	init_entry(mercury__pqueue__assoc_list_to_pqueue_2_0);
BEGIN_CODE

/* code for predicate 'pqueue__assoc_list_to_pqueue'/2 in mode 0 */
Define_entry(mercury__pqueue__assoc_list_to_pqueue_2_0);
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__pqueue__assoc_list_to_pqueue__ua10000_2_0),
		ENTRY(mercury__pqueue__assoc_list_to_pqueue_2_0));
END_MODULE

BEGIN_MODULE(mercury__pqueue_module13)
	init_entry(mercury____Unify___pqueue__pqueue_2_0);
	init_label(mercury____Unify___pqueue__pqueue_2_0_i1012);
	init_label(mercury____Unify___pqueue__pqueue_2_0_i6);
	init_label(mercury____Unify___pqueue__pqueue_2_0_i8);
	init_label(mercury____Unify___pqueue__pqueue_2_0_i10);
	init_label(mercury____Unify___pqueue__pqueue_2_0_i1009);
	init_label(mercury____Unify___pqueue__pqueue_2_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___pqueue__pqueue_2_0);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___pqueue__pqueue_2_0_i1012);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___pqueue__pqueue_2_0_i1009);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___pqueue__pqueue_2_0_i1012);
	incr_sp_push_msg(9, "__Unify__");
	detstackvar(9) = (Integer) succip;
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___pqueue__pqueue_2_0_i1);
	if (((Integer) field(mktag(1), (Integer) r3, ((Integer) 0)) != (Integer) field(mktag(1), (Integer) r4, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___pqueue__pqueue_2_0_i1);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 2));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 3));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 4));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 3));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 4));
	detstackvar(7) = (Integer) r1;
	detstackvar(8) = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury____Unify___pqueue__pqueue_2_0_i6,
		ENTRY(mercury____Unify___pqueue__pqueue_2_0));
	}
Define_label(mercury____Unify___pqueue__pqueue_2_0_i6);
	update_prof_current_proc(LABEL(mercury____Unify___pqueue__pqueue_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___pqueue__pqueue_2_0_i1);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury____Unify___pqueue__pqueue_2_0_i8,
		ENTRY(mercury____Unify___pqueue__pqueue_2_0));
	}
Define_label(mercury____Unify___pqueue__pqueue_2_0_i8);
	update_prof_current_proc(LABEL(mercury____Unify___pqueue__pqueue_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___pqueue__pqueue_2_0_i1);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(5);
	localcall(mercury____Unify___pqueue__pqueue_2_0,
		LABEL(mercury____Unify___pqueue__pqueue_2_0_i10),
		ENTRY(mercury____Unify___pqueue__pqueue_2_0));
Define_label(mercury____Unify___pqueue__pqueue_2_0_i10);
	update_prof_current_proc(LABEL(mercury____Unify___pqueue__pqueue_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___pqueue__pqueue_2_0_i1);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury____Unify___pqueue__pqueue_2_0,
		ENTRY(mercury____Unify___pqueue__pqueue_2_0));
Define_label(mercury____Unify___pqueue__pqueue_2_0_i1009);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___pqueue__pqueue_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__pqueue_module14)
	init_entry(mercury____Index___pqueue__pqueue_2_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___pqueue__pqueue_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury____Index___pqueue_pqueue_2__ua10000_2_0),
		ENTRY(mercury____Index___pqueue__pqueue_2_0));
END_MODULE

BEGIN_MODULE(mercury__pqueue_module15)
	init_entry(mercury____Compare___pqueue__pqueue_2_0);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i2);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i3);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i4);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i6);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i11);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i16);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i17);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i15);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i22);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i28);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i34);
	init_label(mercury____Compare___pqueue__pqueue_2_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___pqueue__pqueue_2_0);
	incr_sp_push_msg(11, "__Compare__");
	detstackvar(11) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(9) = (Integer) r1;
	detstackvar(10) = (Integer) r2;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury____Index___pqueue_pqueue_2__ua10000_2_0),
		mercury____Compare___pqueue__pqueue_2_0_i2,
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
Define_label(mercury____Compare___pqueue__pqueue_2_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___pqueue__pqueue_2_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury____Index___pqueue_pqueue_2__ua10000_2_0),
		mercury____Compare___pqueue__pqueue_2_0_i3,
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
Define_label(mercury____Compare___pqueue__pqueue_2_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___pqueue__pqueue_2_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury____Compare___pqueue__pqueue_2_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury____Compare___pqueue__pqueue_2_0_i6);
	if (((Integer) detstackvar(1) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i11);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury____Compare___pqueue__pqueue_2_0_i11);
	r3 = (Integer) detstackvar(2);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i9);
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 2));
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 3));
	detstackvar(8) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 4));
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(1);
	r1 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 4));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___pqueue__pqueue_2_0_i16,
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
	}
	}
Define_label(mercury____Compare___pqueue__pqueue_2_0_i16);
	update_prof_current_proc(LABEL(mercury____Compare___pqueue__pqueue_2_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i15);
Define_label(mercury____Compare___pqueue__pqueue_2_0_i17);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury____Compare___pqueue__pqueue_2_0_i15);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury____Compare___pqueue__pqueue_2_0_i22,
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
	}
Define_label(mercury____Compare___pqueue__pqueue_2_0_i22);
	update_prof_current_proc(LABEL(mercury____Compare___pqueue__pqueue_2_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i17);
	r1 = (Integer) detstackvar(10);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury____Compare___pqueue__pqueue_2_0_i28,
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
	}
Define_label(mercury____Compare___pqueue__pqueue_2_0_i28);
	update_prof_current_proc(LABEL(mercury____Compare___pqueue__pqueue_2_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i17);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(10);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(7);
	localcall(mercury____Compare___pqueue__pqueue_2_0,
		LABEL(mercury____Compare___pqueue__pqueue_2_0_i34),
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
Define_label(mercury____Compare___pqueue__pqueue_2_0_i34);
	update_prof_current_proc(LABEL(mercury____Compare___pqueue__pqueue_2_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___pqueue__pqueue_2_0_i17);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(10);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	localtailcall(mercury____Compare___pqueue__pqueue_2_0,
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
Define_label(mercury____Compare___pqueue__pqueue_2_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___pqueue__pqueue_2_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__pqueue_bunch_0(void)
{
	mercury__pqueue_module0();
	mercury__pqueue_module1();
	mercury__pqueue_module2();
	mercury__pqueue_module3();
	mercury__pqueue_module4();
	mercury__pqueue_module5();
	mercury__pqueue_module6();
	mercury__pqueue_module7();
	mercury__pqueue_module8();
	mercury__pqueue_module9();
	mercury__pqueue_module10();
	mercury__pqueue_module11();
	mercury__pqueue_module12();
	mercury__pqueue_module13();
	mercury__pqueue_module14();
	mercury__pqueue_module15();
}

#endif

void mercury__pqueue__init(void); /* suppress gcc warning */
void mercury__pqueue__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__pqueue_bunch_0();
#endif
}
