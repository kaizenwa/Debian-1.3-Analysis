/*
** Automatically generated from `assoc_list.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__assoc_list__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__assoc_list__from_corresponding_2__ua0_3_0);
Declare_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1010);
Declare_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i6);
Declare_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1007);
Declare_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1);
Declare_static(mercury__assoc_list__remove__ua0_4_0);
Declare_label(mercury__assoc_list__remove__ua0_4_0_i5);
Declare_label(mercury__assoc_list__remove__ua0_4_0_i4);
Declare_label(mercury__assoc_list__remove__ua0_4_0_i7);
Declare_label(mercury__assoc_list__remove__ua0_4_0_i1004);
Declare_label(mercury__assoc_list__remove__ua0_4_0_i1);
Declare_static(mercury__assoc_list__search__ua0_3_0);
Declare_label(mercury__assoc_list__search__ua0_3_0_i5);
Declare_label(mercury__assoc_list__search__ua0_3_0_i4);
Declare_label(mercury__assoc_list__search__ua0_3_0_i7);
Declare_label(mercury__assoc_list__search__ua0_3_0_i1004);
Declare_label(mercury__assoc_list__search__ua0_3_0_i1006);
Declare_static(mercury__assoc_list__values__ua10000_2_0);
Declare_label(mercury__assoc_list__values__ua10000_2_0_i3);
Declare_label(mercury__assoc_list__values__ua10000_2_0_i4);
Declare_label(mercury__assoc_list__values__ua10000_2_0_i1);
Declare_static(mercury__assoc_list__keys__ua10000_2_0);
Declare_label(mercury__assoc_list__keys__ua10000_2_0_i3);
Declare_label(mercury__assoc_list__keys__ua10000_2_0_i4);
Declare_label(mercury__assoc_list__keys__ua10000_2_0_i1);
Declare_static(mercury__assoc_list__from_corresponding_lists__ua10000_3_0);
Declare_label(mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i4);
Declare_label(mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i1000);
Declare_static(mercury__assoc_list__reverse_members__ua10000_2_0);
Declare_label(mercury__assoc_list__reverse_members__ua10000_2_0_i3);
Declare_label(mercury__assoc_list__reverse_members__ua10000_2_0_i4);
Declare_label(mercury__assoc_list__reverse_members__ua10000_2_0_i1);
Define_extern_entry(mercury__assoc_list__reverse_members_2_0);
Define_extern_entry(mercury__assoc_list__from_corresponding_lists_3_0);
Define_extern_entry(mercury__assoc_list__keys_2_0);
Define_extern_entry(mercury__assoc_list__values_2_0);
Define_extern_entry(mercury__assoc_list__search_3_0);
Declare_label(mercury__assoc_list__search_3_0_i2);
Declare_label(mercury__assoc_list__search_3_0_i1000);
Define_extern_entry(mercury__assoc_list__remove_4_0);
Declare_label(mercury__assoc_list__remove_4_0_i2);
Declare_label(mercury__assoc_list__remove_4_0_i1000);
Define_extern_entry(mercury____Unify___assoc_list__assoc_list_2_0);
Define_extern_entry(mercury____Index___assoc_list__assoc_list_2_0);
Define_extern_entry(mercury____Compare___assoc_list__assoc_list_2_0);
Define_extern_entry(mercury____Unify___assoc_list__assoc_list_1_0);
Define_extern_entry(mercury____Index___assoc_list__assoc_list_1_0);
Define_extern_entry(mercury____Compare___assoc_list__assoc_list_1_0);

extern Word * mercury_data_assoc_list__base_type_layout_assoc_list_1[];
Word * mercury_data_assoc_list__base_type_info_assoc_list_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___assoc_list__assoc_list_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___assoc_list__assoc_list_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___assoc_list__assoc_list_1_0),
	(Word *) (Integer) mercury_data_assoc_list__base_type_layout_assoc_list_1
};

extern Word * mercury_data_assoc_list__base_type_layout_assoc_list_2[];
Word * mercury_data_assoc_list__base_type_info_assoc_list_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) ENTRY(mercury____Unify___assoc_list__assoc_list_2_0),
	(Word *) (Integer) ENTRY(mercury____Index___assoc_list__assoc_list_2_0),
	(Word *) (Integer) ENTRY(mercury____Compare___assoc_list__assoc_list_2_0),
	(Word *) (Integer) mercury_data_assoc_list__base_type_layout_assoc_list_2
};

extern Word * mercury_data_assoc_list__common_2[];
Word * mercury_data_assoc_list__base_type_layout_assoc_list_2[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_2),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_2),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_2),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_2)
};

extern Word * mercury_data_assoc_list__common_5[];
Word * mercury_data_assoc_list__base_type_layout_assoc_list_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_5),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_5),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_5),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_assoc_list__common_5)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
Word * mercury_data_assoc_list__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2)
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_assoc_list__common_1[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_assoc_list__common_0)
};

Word * mercury_data_assoc_list__common_2[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_assoc_list__common_1)
};

Word * mercury_data_assoc_list__common_3[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 1)
};

Word * mercury_data_assoc_list__common_4[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_assoc_list__common_3)
};

Word * mercury_data_assoc_list__common_5[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_assoc_list__common_4)
};

BEGIN_MODULE(mercury__assoc_list_module0)
	init_entry(mercury__assoc_list__from_corresponding_2__ua0_3_0);
	init_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1010);
	init_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i6);
	init_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1007);
	init_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1);
BEGIN_CODE

/* code for predicate 'assoc_list__from_corresponding_2__ua0'/3 in mode 0 */
Define_static(mercury__assoc_list__from_corresponding_2__ua0_3_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1010);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1007);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = TRUE;
	proceed();
Define_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1010);
	incr_sp_push_msg(2, "assoc_list__from_corresponding_2__ua0");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1);
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__assoc_list__from_corresponding_2__ua0_3_0,
		LABEL(mercury__assoc_list__from_corresponding_2__ua0_3_0_i6),
		STATIC(mercury__assoc_list__from_corresponding_2__ua0_3_0));
Define_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i6);
	update_prof_current_proc(LABEL(mercury__assoc_list__from_corresponding_2__ua0_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1007);
	r1 = FALSE;
	proceed();
Define_label(mercury__assoc_list__from_corresponding_2__ua0_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module1)
	init_entry(mercury__assoc_list__remove__ua0_4_0);
	init_label(mercury__assoc_list__remove__ua0_4_0_i5);
	init_label(mercury__assoc_list__remove__ua0_4_0_i4);
	init_label(mercury__assoc_list__remove__ua0_4_0_i7);
	init_label(mercury__assoc_list__remove__ua0_4_0_i1004);
	init_label(mercury__assoc_list__remove__ua0_4_0_i1);
BEGIN_CODE

/* code for predicate 'assoc_list__remove__ua0'/4 in mode 0 */
Define_static(mercury__assoc_list__remove__ua0_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__remove__ua0_4_0_i1004);
	incr_sp_push_msg(6, "assoc_list__remove__ua0");
	detstackvar(6) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) tempr1;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r3;
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__assoc_list__remove__ua0_4_0_i5,
		STATIC(mercury__assoc_list__remove__ua0_4_0));
	}
	}
Define_label(mercury__assoc_list__remove__ua0_4_0_i5);
	update_prof_current_proc(LABEL(mercury__assoc_list__remove__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__remove__ua0_4_0_i4);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__assoc_list__remove__ua0_4_0_i4);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__assoc_list__remove__ua0_4_0,
		LABEL(mercury__assoc_list__remove__ua0_4_0_i7),
		STATIC(mercury__assoc_list__remove__ua0_4_0));
Define_label(mercury__assoc_list__remove__ua0_4_0_i7);
	update_prof_current_proc(LABEL(mercury__assoc_list__remove__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__remove__ua0_4_0_i1);
	r1 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__assoc_list__remove__ua0_4_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__assoc_list__remove__ua0_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module2)
	init_entry(mercury__assoc_list__search__ua0_3_0);
	init_label(mercury__assoc_list__search__ua0_3_0_i5);
	init_label(mercury__assoc_list__search__ua0_3_0_i4);
	init_label(mercury__assoc_list__search__ua0_3_0_i7);
	init_label(mercury__assoc_list__search__ua0_3_0_i1004);
	init_label(mercury__assoc_list__search__ua0_3_0_i1006);
BEGIN_CODE

/* code for predicate 'assoc_list__search__ua0'/3 in mode 0 */
Define_static(mercury__assoc_list__search__ua0_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__search__ua0_3_0_i1004);
	incr_sp_push_msg(5, "assoc_list__search__ua0");
	detstackvar(5) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__assoc_list__search__ua0_3_0_i5,
		STATIC(mercury__assoc_list__search__ua0_3_0));
	}
	}
Define_label(mercury__assoc_list__search__ua0_3_0_i5);
	update_prof_current_proc(LABEL(mercury__assoc_list__search__ua0_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__search__ua0_3_0_i4);
	r2 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__assoc_list__search__ua0_3_0_i4);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__assoc_list__search__ua0_3_0,
		LABEL(mercury__assoc_list__search__ua0_3_0_i7),
		STATIC(mercury__assoc_list__search__ua0_3_0));
Define_label(mercury__assoc_list__search__ua0_3_0_i7);
	update_prof_current_proc(LABEL(mercury__assoc_list__search__ua0_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__search__ua0_3_0_i1006);
	r1 = TRUE;
	proceed();
Define_label(mercury__assoc_list__search__ua0_3_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__assoc_list__search__ua0_3_0_i1006);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module3)
	init_entry(mercury__assoc_list__values__ua10000_2_0);
	init_label(mercury__assoc_list__values__ua10000_2_0_i3);
	init_label(mercury__assoc_list__values__ua10000_2_0_i4);
	init_label(mercury__assoc_list__values__ua10000_2_0_i1);
BEGIN_CODE

/* code for predicate 'assoc_list__values__ua10000'/2 in mode 0 */
Define_static(mercury__assoc_list__values__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__values__ua10000_2_0_i1);
	r3 = (Integer) sp;
Define_label(mercury__assoc_list__values__ua10000_2_0_i3);
	while (1) {
	incr_sp_push_msg(1, "assoc_list__values__ua10000");
	detstackvar(1) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__assoc_list__values__ua10000_2_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r3))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__assoc_list__values__ua10000_2_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module4)
	init_entry(mercury__assoc_list__keys__ua10000_2_0);
	init_label(mercury__assoc_list__keys__ua10000_2_0_i3);
	init_label(mercury__assoc_list__keys__ua10000_2_0_i4);
	init_label(mercury__assoc_list__keys__ua10000_2_0_i1);
BEGIN_CODE

/* code for predicate 'assoc_list__keys__ua10000'/2 in mode 0 */
Define_static(mercury__assoc_list__keys__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__keys__ua10000_2_0_i1);
	r3 = (Integer) sp;
Define_label(mercury__assoc_list__keys__ua10000_2_0_i3);
	while (1) {
	incr_sp_push_msg(1, "assoc_list__keys__ua10000");
	detstackvar(1) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__assoc_list__keys__ua10000_2_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r3))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__assoc_list__keys__ua10000_2_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module5)
	init_entry(mercury__assoc_list__from_corresponding_lists__ua10000_3_0);
	init_label(mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i4);
	init_label(mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i1000);
BEGIN_CODE

/* code for predicate 'assoc_list__from_corresponding_lists__ua10000'/3 in mode 0 */
Define_static(mercury__assoc_list__from_corresponding_lists__ua10000_3_0);
	incr_sp_push_msg(1, "assoc_list__from_corresponding_lists__ua10000");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__assoc_list__from_corresponding_2__ua0_3_0),
		mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i4,
		STATIC(mercury__assoc_list__from_corresponding_lists__ua10000_3_0));
Define_label(mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__assoc_list__from_corresponding_lists__ua10000_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__assoc_list__from_corresponding_lists__ua10000_3_0_i1000);
	r1 = string_const("assoc_list__from_corresponding_lists: lists have different lengths.", 67);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__assoc_list__from_corresponding_lists__ua10000_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module6)
	init_entry(mercury__assoc_list__reverse_members__ua10000_2_0);
	init_label(mercury__assoc_list__reverse_members__ua10000_2_0_i3);
	init_label(mercury__assoc_list__reverse_members__ua10000_2_0_i4);
	init_label(mercury__assoc_list__reverse_members__ua10000_2_0_i1);
BEGIN_CODE

/* code for predicate 'assoc_list__reverse_members__ua10000'/2 in mode 0 */
Define_static(mercury__assoc_list__reverse_members__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__assoc_list__reverse_members__ua10000_2_0_i1);
	r3 = (Integer) sp;
Define_label(mercury__assoc_list__reverse_members__ua10000_2_0_i3);
	while (1) {
	incr_sp_push_msg(1, "assoc_list__reverse_members__ua10000");
	tag_incr_hp(detstackvar(1), mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) detstackvar(1), ((Integer) 0)) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	field(mktag(0), (Integer) detstackvar(1), ((Integer) 1)) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__assoc_list__reverse_members__ua10000_2_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r3))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__assoc_list__reverse_members__ua10000_2_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module7)
	init_entry(mercury__assoc_list__reverse_members_2_0);
BEGIN_CODE

/* code for predicate 'assoc_list__reverse_members'/2 in mode 0 */
Define_entry(mercury__assoc_list__reverse_members_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__assoc_list__reverse_members__ua10000_2_0),
		ENTRY(mercury__assoc_list__reverse_members_2_0));
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module8)
	init_entry(mercury__assoc_list__from_corresponding_lists_3_0);
BEGIN_CODE

/* code for predicate 'assoc_list__from_corresponding_lists'/3 in mode 0 */
Define_entry(mercury__assoc_list__from_corresponding_lists_3_0);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	tailcall(STATIC(mercury__assoc_list__from_corresponding_lists__ua10000_3_0),
		ENTRY(mercury__assoc_list__from_corresponding_lists_3_0));
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module9)
	init_entry(mercury__assoc_list__keys_2_0);
BEGIN_CODE

/* code for predicate 'assoc_list__keys'/2 in mode 0 */
Define_entry(mercury__assoc_list__keys_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__assoc_list__keys__ua10000_2_0),
		ENTRY(mercury__assoc_list__keys_2_0));
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module10)
	init_entry(mercury__assoc_list__values_2_0);
BEGIN_CODE

/* code for predicate 'assoc_list__values'/2 in mode 0 */
Define_entry(mercury__assoc_list__values_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__assoc_list__values__ua10000_2_0),
		ENTRY(mercury__assoc_list__values_2_0));
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module11)
	init_entry(mercury__assoc_list__search_3_0);
	init_label(mercury__assoc_list__search_3_0_i2);
	init_label(mercury__assoc_list__search_3_0_i1000);
BEGIN_CODE

/* code for predicate 'assoc_list__search'/3 in mode 0 */
Define_entry(mercury__assoc_list__search_3_0);
	incr_sp_push_msg(2, "assoc_list__search");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__assoc_list__search__ua0_3_0),
		mercury__assoc_list__search_3_0_i2,
		ENTRY(mercury__assoc_list__search_3_0));
Define_label(mercury__assoc_list__search_3_0_i2);
	update_prof_current_proc(LABEL(mercury__assoc_list__search_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__search_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__assoc_list__search_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module12)
	init_entry(mercury__assoc_list__remove_4_0);
	init_label(mercury__assoc_list__remove_4_0_i2);
	init_label(mercury__assoc_list__remove_4_0_i1000);
BEGIN_CODE

/* code for predicate 'assoc_list__remove'/4 in mode 0 */
Define_entry(mercury__assoc_list__remove_4_0);
	incr_sp_push_msg(2, "assoc_list__remove");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__assoc_list__remove__ua0_4_0),
		mercury__assoc_list__remove_4_0_i2,
		ENTRY(mercury__assoc_list__remove_4_0));
Define_label(mercury__assoc_list__remove_4_0_i2);
	update_prof_current_proc(LABEL(mercury__assoc_list__remove_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__assoc_list__remove_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__assoc_list__remove_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module13)
	init_entry(mercury____Unify___assoc_list__assoc_list_2_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___assoc_list__assoc_list_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r2;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) tempr1;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury____Unify___assoc_list__assoc_list_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module14)
	init_entry(mercury____Index___assoc_list__assoc_list_2_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___assoc_list__assoc_list_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r2;
	r2 = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	{
	Declare_entry(mercury____Index___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Index___mercury_builtin__list_1_0),
		ENTRY(mercury____Index___assoc_list__assoc_list_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module15)
	init_entry(mercury____Compare___assoc_list__assoc_list_2_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___assoc_list__assoc_list_2_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r2;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) tempr1;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		ENTRY(mercury____Compare___assoc_list__assoc_list_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module16)
	init_entry(mercury____Unify___assoc_list__assoc_list_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___assoc_list__assoc_list_1_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r1;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury____Unify___assoc_list__assoc_list_1_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module17)
	init_entry(mercury____Index___assoc_list__assoc_list_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___assoc_list__assoc_list_1_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r3;
	{
	Declare_entry(mercury____Index___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Index___mercury_builtin__list_1_0),
		ENTRY(mercury____Index___assoc_list__assoc_list_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__assoc_list_module18)
	init_entry(mercury____Compare___assoc_list__assoc_list_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___assoc_list__assoc_list_1_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r1;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mercury_data_std_util__base_type_info_pair_2;
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		ENTRY(mercury____Compare___assoc_list__assoc_list_1_0));
	}
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__assoc_list_bunch_0(void)
{
	mercury__assoc_list_module0();
	mercury__assoc_list_module1();
	mercury__assoc_list_module2();
	mercury__assoc_list_module3();
	mercury__assoc_list_module4();
	mercury__assoc_list_module5();
	mercury__assoc_list_module6();
	mercury__assoc_list_module7();
	mercury__assoc_list_module8();
	mercury__assoc_list_module9();
	mercury__assoc_list_module10();
	mercury__assoc_list_module11();
	mercury__assoc_list_module12();
	mercury__assoc_list_module13();
	mercury__assoc_list_module14();
	mercury__assoc_list_module15();
	mercury__assoc_list_module16();
	mercury__assoc_list_module17();
	mercury__assoc_list_module18();
}

#endif

void mercury__assoc_list__init(void); /* suppress gcc warning */
void mercury__assoc_list__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__assoc_list_bunch_0();
#endif
}
