/*
** Automatically generated from `llds_common.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__llds_common__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___llds_common_cell_info_0__ua10000_2_0);
Declare_static(mercury__llds_common__LambdaGoal__1_3_0);
Define_extern_entry(mercury__llds_common__llds_common_6_0);
Declare_label(mercury__llds_common__llds_common_6_0_i2);
Declare_label(mercury__llds_common__llds_common_6_0_i3);
Declare_label(mercury__llds_common__llds_common_6_0_i4);
Declare_label(mercury__llds_common__llds_common_6_0_i5);
Declare_label(mercury__llds_common__llds_common_6_0_i6);
Declare_label(mercury__llds_common__llds_common_6_0_i7);
Declare_static(mercury__llds_common__cell_pairs_to_modules_3_0);
Declare_label(mercury__llds_common__cell_pairs_to_modules_3_0_i3);
Declare_label(mercury__llds_common__cell_pairs_to_modules_3_0_i4);
Declare_label(mercury__llds_common__cell_pairs_to_modules_3_0_i1);
Declare_static(mercury__llds_common__process_modules_4_0);
Declare_label(mercury__llds_common__process_modules_4_0_i1008);
Declare_label(mercury__llds_common__process_modules_4_0_i8);
Declare_label(mercury__llds_common__process_modules_4_0_i7);
Declare_label(mercury__llds_common__process_modules_4_0_i9);
Declare_label(mercury__llds_common__process_modules_4_0_i4);
Declare_label(mercury__llds_common__process_modules_4_0_i10);
Declare_label(mercury__llds_common__process_modules_4_0_i1006);
Declare_static(mercury__llds_common__process_procs_4_0);
Declare_label(mercury__llds_common__process_procs_4_0_i4);
Declare_label(mercury__llds_common__process_procs_4_0_i5);
Declare_label(mercury__llds_common__process_procs_4_0_i1003);
Declare_static(mercury__llds_common__process_instrs_4_0);
Declare_label(mercury__llds_common__process_instrs_4_0_i4);
Declare_label(mercury__llds_common__process_instrs_4_0_i5);
Declare_label(mercury__llds_common__process_instrs_4_0_i1003);
Declare_static(mercury__llds_common__process_instr_4_0);
Declare_label(mercury__llds_common__process_instr_4_0_i1023);
Declare_label(mercury__llds_common__process_instr_4_0_i1022);
Declare_label(mercury__llds_common__process_instr_4_0_i1021);
Declare_label(mercury__llds_common__process_instr_4_0_i1020);
Declare_label(mercury__llds_common__process_instr_4_0_i1019);
Declare_label(mercury__llds_common__process_instr_4_0_i1018);
Declare_label(mercury__llds_common__process_instr_4_0_i1017);
Declare_label(mercury__llds_common__process_instr_4_0_i5);
Declare_label(mercury__llds_common__process_instr_4_0_i6);
Declare_label(mercury__llds_common__process_instr_4_0_i7);
Declare_label(mercury__llds_common__process_instr_4_0_i8);
Declare_label(mercury__llds_common__process_instr_4_0_i1013);
Declare_label(mercury__llds_common__process_instr_4_0_i14);
Declare_label(mercury__llds_common__process_instr_4_0_i15);
Declare_label(mercury__llds_common__process_instr_4_0_i17);
Declare_label(mercury__llds_common__process_instr_4_0_i18);
Declare_label(mercury__llds_common__process_instr_4_0_i19);
Declare_label(mercury__llds_common__process_instr_4_0_i20);
Declare_label(mercury__llds_common__process_instr_4_0_i22);
Declare_label(mercury__llds_common__process_instr_4_0_i23);
Declare_label(mercury__llds_common__process_instr_4_0_i25);
Declare_label(mercury__llds_common__process_instr_4_0_i26);
Declare_label(mercury__llds_common__process_instr_4_0_i1016);
Declare_label(mercury__llds_common__process_instr_4_0_i30);
Declare_label(mercury__llds_common__process_instr_4_0_i1015);
Declare_static(mercury__llds_common__process_rval_4_0);
Declare_label(mercury__llds_common__process_rval_4_0_i6);
Declare_label(mercury__llds_common__process_rval_4_0_i5);
Declare_label(mercury__llds_common__process_rval_4_0_i7);
Declare_label(mercury__llds_common__process_rval_4_0_i9);
Declare_label(mercury__llds_common__process_rval_4_0_i8);
Declare_label(mercury__llds_common__process_rval_4_0_i10);
Declare_label(mercury__llds_common__process_rval_4_0_i11);
Declare_label(mercury__llds_common__process_rval_4_0_i1047);
Declare_label(mercury__llds_common__process_rval_4_0_i12);
Declare_label(mercury__llds_common__process_rval_4_0_i14);
Declare_label(mercury__llds_common__process_rval_4_0_i13);
Declare_label(mercury__llds_common__process_rval_4_0_i18);
Declare_label(mercury__llds_common__process_rval_4_0_i21);
Declare_label(mercury__llds_common__process_rval_4_0_i20);
Declare_label(mercury__llds_common__process_rval_4_0_i25);
Declare_label(mercury__llds_common__process_rval_4_0_i1045);
Declare_label(mercury__llds_common__process_rval_4_0_i27);
Declare_label(mercury__llds_common__process_rval_4_0_i28);
Declare_label(mercury__llds_common__process_rval_4_0_i15);
Declare_static(mercury__llds_common__process_maybe_rvals_4_0);
Declare_label(mercury__llds_common__process_maybe_rvals_4_0_i6);
Declare_label(mercury__llds_common__process_maybe_rvals_4_0_i5);
Declare_label(mercury__llds_common__process_maybe_rvals_4_0_i4);
Declare_label(mercury__llds_common__process_maybe_rvals_4_0_i7);
Declare_label(mercury__llds_common__process_maybe_rvals_4_0_i1004);
Declare_static(mercury____Unify___llds_common__cell_info_0_0);
Declare_label(mercury____Unify___llds_common__cell_info_0_0_i1);
Declare_static(mercury____Index___llds_common__cell_info_0_0);
Declare_static(mercury____Compare___llds_common__cell_info_0_0);
Declare_label(mercury____Compare___llds_common__cell_info_0_0_i4);
Declare_label(mercury____Compare___llds_common__cell_info_0_0_i3);

extern Word * mercury_data_llds_common__base_type_layout_cell_info_0[];
Word * mercury_data_llds_common__base_type_info_cell_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) STATIC(mercury____Unify___llds_common__cell_info_0_0),
	(Word *) (Integer) STATIC(mercury____Index___llds_common__cell_info_0_0),
	(Word *) (Integer) STATIC(mercury____Compare___llds_common__cell_info_0_0),
	(Word *) (Integer) mercury_data_llds_common__base_type_layout_cell_info_0
};

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_llds_common__base_type_layout_common_info_0[];
Word * mercury_data_llds_common__base_type_info_common_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_llds_common__base_type_layout_common_info_0
};

extern Word * mercury_data_llds_common__common_7[];
Word * mercury_data_llds_common__base_type_layout_common_info_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_llds_common__common_7),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_llds_common__common_9[];
Word * mercury_data_llds_common__base_type_layout_cell_info_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_llds_common__common_9),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_std_util__base_type_info_maybe_1[];
extern Word * mercury_data_llds__base_type_info_rval_0[];
Word * mercury_data_llds_common__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_maybe_1,
	(Word *) (Integer) mercury_data_llds__base_type_info_rval_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_llds_common__common_1[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_0)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
Word * mercury_data_llds_common__common_2[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_1),
	(Word *) (Integer) mercury_data_llds_common__base_type_info_cell_info_0
};

Word * mercury_data_llds_common__common_3[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) STATIC(mercury__llds_common__LambdaGoal__1_3_0)
};

extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_llds_common__common_4[] = {
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_llds_common__common_5[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_llds_common__common_6[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_1),
	(Word *) (Integer) mercury_data_llds_common__base_type_info_cell_info_0
};

Word * mercury_data_llds_common__common_7[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_4),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_6),
	(Word *) string_const("common_info", 11)
};

extern Word * mercury_data_bool__base_type_info_bool_0[];
Word * mercury_data_llds_common__common_8[] = {
	(Word *) (Integer) mercury_data_bool__base_type_info_bool_0
};

Word * mercury_data_llds_common__common_9[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_8),
	(Word *) string_const("cell_info", 9)
};

BEGIN_MODULE(mercury__llds_common_module0)
	init_entry(mercury____Index___llds_common_cell_info_0__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___llds_common_cell_info_0__ua10000'/2 in mode 0 */
Define_static(mercury____Index___llds_common_cell_info_0__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module1)
	init_entry(mercury__llds_common__LambdaGoal__1_3_0);
BEGIN_CODE

/* code for predicate 'llds_common__LambdaGoal__1'/3 in mode 0 */
Define_static(mercury__llds_common__LambdaGoal__1_3_0);
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(0), (Integer) r1, ((Integer) 1)), ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(0), (Integer) r2, ((Integer) 1)), ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		STATIC(mercury__llds_common__LambdaGoal__1_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__llds_common_module2)
	init_entry(mercury__llds_common__llds_common_6_0);
	init_label(mercury__llds_common__llds_common_6_0_i2);
	init_label(mercury__llds_common__llds_common_6_0_i3);
	init_label(mercury__llds_common__llds_common_6_0_i4);
	init_label(mercury__llds_common__llds_common_6_0_i5);
	init_label(mercury__llds_common__llds_common_6_0_i6);
	init_label(mercury__llds_common__llds_common_6_0_i7);
BEGIN_CODE

/* code for predicate 'llds_common'/6 in mode 0 */
Define_entry(mercury__llds_common__llds_common_6_0);
	incr_sp_push_msg(4, "llds_common");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_1);
	r2 = (Integer) mercury_data_llds_common__base_type_info_cell_info_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__llds_common__llds_common_6_0_i2,
		ENTRY(mercury__llds_common__llds_common_6_0));
	}
Define_label(mercury__llds_common__llds_common_6_0_i2);
	update_prof_current_proc(LABEL(mercury__llds_common__llds_common_6_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = ((Integer) 0);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__llds_common__process_procs_4_0),
		mercury__llds_common__llds_common_6_0_i3,
		ENTRY(mercury__llds_common__llds_common_6_0));
Define_label(mercury__llds_common__llds_common_6_0_i3);
	update_prof_current_proc(LABEL(mercury__llds_common__llds_common_6_0));
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__llds_common__process_modules_4_0),
		mercury__llds_common__llds_common_6_0_i4,
		ENTRY(mercury__llds_common__llds_common_6_0));
Define_label(mercury__llds_common__llds_common_6_0_i4);
	update_prof_current_proc(LABEL(mercury__llds_common__llds_common_6_0));
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_1);
	r2 = (Integer) mercury_data_llds_common__base_type_info_cell_info_0;
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__to_assoc_list_2_0),
		mercury__llds_common__llds_common_6_0_i5,
		ENTRY(mercury__llds_common__llds_common_6_0));
	}
Define_label(mercury__llds_common__llds_common_6_0_i5);
	update_prof_current_proc(LABEL(mercury__llds_common__llds_common_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_2);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_3);
	{
	Declare_entry(mercury__list__sort_3_0);
	call_localret(ENTRY(mercury__list__sort_3_0),
		mercury__llds_common__llds_common_6_0_i6,
		ENTRY(mercury__llds_common__llds_common_6_0));
	}
Define_label(mercury__llds_common__llds_common_6_0_i6);
	update_prof_current_proc(LABEL(mercury__llds_common__llds_common_6_0));
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__llds_common__cell_pairs_to_modules_3_0),
		mercury__llds_common__llds_common_6_0_i7,
		ENTRY(mercury__llds_common__llds_common_6_0));
Define_label(mercury__llds_common__llds_common_6_0_i7);
	update_prof_current_proc(LABEL(mercury__llds_common__llds_common_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module3)
	init_entry(mercury__llds_common__cell_pairs_to_modules_3_0);
	init_label(mercury__llds_common__cell_pairs_to_modules_3_0_i3);
	init_label(mercury__llds_common__cell_pairs_to_modules_3_0_i4);
	init_label(mercury__llds_common__cell_pairs_to_modules_3_0_i1);
BEGIN_CODE

/* code for predicate 'llds_common__cell_pairs_to_modules'/3 in mode 0 */
Define_static(mercury__llds_common__cell_pairs_to_modules_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__llds_common__cell_pairs_to_modules_3_0_i1);
	r4 = (Integer) sp;
Define_label(mercury__llds_common__cell_pairs_to_modules_3_0_i3);
	while (1) {
	incr_sp_push_msg(1, "llds_common__cell_pairs_to_modules");
	tag_incr_hp(r3, mktag(1), ((Integer) 6));
	detstackvar(1) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1)), ((Integer) 0));
	field(mktag(1), (Integer) detstackvar(1), ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) detstackvar(1), ((Integer) 2)) = (Integer) field(mktag(0), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1)), ((Integer) 1));
	field(mktag(1), (Integer) detstackvar(1), ((Integer) 3)) = ((Integer) 1);
	field(mktag(1), (Integer) detstackvar(1), ((Integer) 4)) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	field(mktag(1), (Integer) detstackvar(1), ((Integer) 5)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__llds_common__cell_pairs_to_modules_3_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r4))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__llds_common__cell_pairs_to_modules_3_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module4)
	init_entry(mercury__llds_common__process_modules_4_0);
	init_label(mercury__llds_common__process_modules_4_0_i1008);
	init_label(mercury__llds_common__process_modules_4_0_i8);
	init_label(mercury__llds_common__process_modules_4_0_i7);
	init_label(mercury__llds_common__process_modules_4_0_i9);
	init_label(mercury__llds_common__process_modules_4_0_i4);
	init_label(mercury__llds_common__process_modules_4_0_i10);
	init_label(mercury__llds_common__process_modules_4_0_i1006);
BEGIN_CODE

/* code for predicate 'llds_common__process_modules'/4 in mode 0 */
Define_static(mercury__llds_common__process_modules_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__llds_common__process_modules_4_0_i1006);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__llds_common__process_modules_4_0_i1008);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	incr_sp_push_msg(7, "llds_common__process_modules");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_modules_4_0_i4);
Define_label(mercury__llds_common__process_modules_4_0_i1008);
	incr_sp_push_msg(7, "llds_common__process_modules");
	detstackvar(7) = (Integer) succip;
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__llds_common__process_modules_4_0_i7);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 3));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 5));
	r1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 4));
	call_localret(STATIC(mercury__llds_common__process_maybe_rvals_4_0),
		mercury__llds_common__process_modules_4_0_i8,
		STATIC(mercury__llds_common__process_modules_4_0));
Define_label(mercury__llds_common__process_modules_4_0_i8);
	update_prof_current_proc(LABEL(mercury__llds_common__process_modules_4_0));
	tag_incr_hp(r3, mktag(1), ((Integer) 6));
	field(mktag(1), (Integer) r3, ((Integer) 5)) = (Integer) detstackvar(6);
	field(mktag(1), (Integer) r3, ((Integer) 4)) = (Integer) r2;
	field(mktag(1), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__llds_common__process_modules_4_0_i4);
Define_label(mercury__llds_common__process_modules_4_0_i7);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__llds_common__process_modules_4_0_i9);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	GOTO_LABEL(mercury__llds_common__process_modules_4_0_i4);
Define_label(mercury__llds_common__process_modules_4_0_i9);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
Define_label(mercury__llds_common__process_modules_4_0_i4);
	detstackvar(1) = (Integer) r3;
	localcall(mercury__llds_common__process_modules_4_0,
		LABEL(mercury__llds_common__process_modules_4_0_i10),
		STATIC(mercury__llds_common__process_modules_4_0));
Define_label(mercury__llds_common__process_modules_4_0_i10);
	update_prof_current_proc(LABEL(mercury__llds_common__process_modules_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__llds_common__process_modules_4_0_i1006);
	r1 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module5)
	init_entry(mercury__llds_common__process_procs_4_0);
	init_label(mercury__llds_common__process_procs_4_0_i4);
	init_label(mercury__llds_common__process_procs_4_0_i5);
	init_label(mercury__llds_common__process_procs_4_0_i1003);
BEGIN_CODE

/* code for predicate 'llds_common__process_procs'/4 in mode 0 */
Define_static(mercury__llds_common__process_procs_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__llds_common__process_procs_4_0_i1003);
	incr_sp_push_msg(5, "llds_common__process_procs");
	detstackvar(5) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	call_localret(STATIC(mercury__llds_common__process_instrs_4_0),
		mercury__llds_common__process_procs_4_0_i4,
		STATIC(mercury__llds_common__process_procs_4_0));
Define_label(mercury__llds_common__process_procs_4_0_i4);
	update_prof_current_proc(LABEL(mercury__llds_common__process_procs_4_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 4));
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	localcall(mercury__llds_common__process_procs_4_0,
		LABEL(mercury__llds_common__process_procs_4_0_i5),
		STATIC(mercury__llds_common__process_procs_4_0));
Define_label(mercury__llds_common__process_procs_4_0_i5);
	update_prof_current_proc(LABEL(mercury__llds_common__process_procs_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__llds_common__process_procs_4_0_i1003);
	r1 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module6)
	init_entry(mercury__llds_common__process_instrs_4_0);
	init_label(mercury__llds_common__process_instrs_4_0_i4);
	init_label(mercury__llds_common__process_instrs_4_0_i5);
	init_label(mercury__llds_common__process_instrs_4_0_i1003);
BEGIN_CODE

/* code for predicate 'llds_common__process_instrs'/4 in mode 0 */
Define_static(mercury__llds_common__process_instrs_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__llds_common__process_instrs_4_0_i1003);
	incr_sp_push_msg(3, "llds_common__process_instrs");
	detstackvar(3) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	call_localret(STATIC(mercury__llds_common__process_instr_4_0),
		mercury__llds_common__process_instrs_4_0_i4,
		STATIC(mercury__llds_common__process_instrs_4_0));
Define_label(mercury__llds_common__process_instrs_4_0_i4);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instrs_4_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__llds_common__process_instrs_4_0,
		LABEL(mercury__llds_common__process_instrs_4_0_i5),
		STATIC(mercury__llds_common__process_instrs_4_0));
Define_label(mercury__llds_common__process_instrs_4_0_i5);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instrs_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instrs_4_0_i1003);
	r1 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module7)
	init_entry(mercury__llds_common__process_instr_4_0);
	init_label(mercury__llds_common__process_instr_4_0_i1023);
	init_label(mercury__llds_common__process_instr_4_0_i1022);
	init_label(mercury__llds_common__process_instr_4_0_i1021);
	init_label(mercury__llds_common__process_instr_4_0_i1020);
	init_label(mercury__llds_common__process_instr_4_0_i1019);
	init_label(mercury__llds_common__process_instr_4_0_i1018);
	init_label(mercury__llds_common__process_instr_4_0_i1017);
	init_label(mercury__llds_common__process_instr_4_0_i5);
	init_label(mercury__llds_common__process_instr_4_0_i6);
	init_label(mercury__llds_common__process_instr_4_0_i7);
	init_label(mercury__llds_common__process_instr_4_0_i8);
	init_label(mercury__llds_common__process_instr_4_0_i1013);
	init_label(mercury__llds_common__process_instr_4_0_i14);
	init_label(mercury__llds_common__process_instr_4_0_i15);
	init_label(mercury__llds_common__process_instr_4_0_i17);
	init_label(mercury__llds_common__process_instr_4_0_i18);
	init_label(mercury__llds_common__process_instr_4_0_i19);
	init_label(mercury__llds_common__process_instr_4_0_i20);
	init_label(mercury__llds_common__process_instr_4_0_i22);
	init_label(mercury__llds_common__process_instr_4_0_i23);
	init_label(mercury__llds_common__process_instr_4_0_i25);
	init_label(mercury__llds_common__process_instr_4_0_i26);
	init_label(mercury__llds_common__process_instr_4_0_i1016);
	init_label(mercury__llds_common__process_instr_4_0_i30);
	init_label(mercury__llds_common__process_instr_4_0_i1015);
BEGIN_CODE

/* code for predicate 'llds_common__process_instr'/4 in mode 0 */
Define_static(mercury__llds_common__process_instr_4_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__llds_common__process_instr_4_0_i1016);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__llds_common__process_instr_4_0_i1023) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1022) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1021) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1020) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1019) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1018) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1017) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013) AND
		LABEL(mercury__llds_common__process_instr_4_0_i1013));
Define_label(mercury__llds_common__process_instr_4_0_i1023);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_instr_4_0_i5);
Define_label(mercury__llds_common__process_instr_4_0_i1022);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_instr_4_0_i7);
Define_label(mercury__llds_common__process_instr_4_0_i1021);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_instr_4_0_i14);
Define_label(mercury__llds_common__process_instr_4_0_i1020);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_instr_4_0_i17);
Define_label(mercury__llds_common__process_instr_4_0_i1019);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_instr_4_0_i19);
Define_label(mercury__llds_common__process_instr_4_0_i1018);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_instr_4_0_i22);
Define_label(mercury__llds_common__process_instr_4_0_i1017);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__llds_common__process_instr_4_0_i25);
Define_label(mercury__llds_common__process_instr_4_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	call_localret(STATIC(mercury__llds_common__process_instrs_4_0),
		mercury__llds_common__process_instr_4_0_i6,
		STATIC(mercury__llds_common__process_instr_4_0));
Define_label(mercury__llds_common__process_instr_4_0_i6);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instr_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i7);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	call_localret(STATIC(mercury__llds_common__process_rval_4_0),
		mercury__llds_common__process_instr_4_0_i8,
		STATIC(mercury__llds_common__process_instr_4_0));
Define_label(mercury__llds_common__process_instr_4_0_i8);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instr_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i1013);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i14);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__llds_common__process_rval_4_0),
		mercury__llds_common__process_instr_4_0_i15,
		STATIC(mercury__llds_common__process_instr_4_0));
Define_label(mercury__llds_common__process_instr_4_0_i15);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instr_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 7);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i17);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__llds_common__process_rval_4_0),
		mercury__llds_common__process_instr_4_0_i18,
		STATIC(mercury__llds_common__process_instr_4_0));
Define_label(mercury__llds_common__process_instr_4_0_i18);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instr_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 9);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i19);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	call_localret(STATIC(mercury__llds_common__process_rval_4_0),
		mercury__llds_common__process_instr_4_0_i20,
		STATIC(mercury__llds_common__process_instr_4_0));
Define_label(mercury__llds_common__process_instr_4_0_i20);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instr_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 10);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i22);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__llds_common__process_rval_4_0),
		mercury__llds_common__process_instr_4_0_i23,
		STATIC(mercury__llds_common__process_instr_4_0));
Define_label(mercury__llds_common__process_instr_4_0_i23);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instr_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 12);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i25);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__llds_common__process_rval_4_0),
		mercury__llds_common__process_instr_4_0_i26,
		STATIC(mercury__llds_common__process_instr_4_0));
Define_label(mercury__llds_common__process_instr_4_0_i26);
	update_prof_current_proc(LABEL(mercury__llds_common__process_instr_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 14);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i1016);
	incr_sp_push_msg(3, "llds_common__process_instr");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__llds_common__process_instr_4_0_i30);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i30);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__llds_common__process_instr_4_0_i1015);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__llds_common__process_instr_4_0_i1015);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module8)
	init_entry(mercury__llds_common__process_rval_4_0);
	init_label(mercury__llds_common__process_rval_4_0_i6);
	init_label(mercury__llds_common__process_rval_4_0_i5);
	init_label(mercury__llds_common__process_rval_4_0_i7);
	init_label(mercury__llds_common__process_rval_4_0_i9);
	init_label(mercury__llds_common__process_rval_4_0_i8);
	init_label(mercury__llds_common__process_rval_4_0_i10);
	init_label(mercury__llds_common__process_rval_4_0_i11);
	init_label(mercury__llds_common__process_rval_4_0_i1047);
	init_label(mercury__llds_common__process_rval_4_0_i12);
	init_label(mercury__llds_common__process_rval_4_0_i14);
	init_label(mercury__llds_common__process_rval_4_0_i13);
	init_label(mercury__llds_common__process_rval_4_0_i18);
	init_label(mercury__llds_common__process_rval_4_0_i21);
	init_label(mercury__llds_common__process_rval_4_0_i20);
	init_label(mercury__llds_common__process_rval_4_0_i25);
	init_label(mercury__llds_common__process_rval_4_0_i1045);
	init_label(mercury__llds_common__process_rval_4_0_i27);
	init_label(mercury__llds_common__process_rval_4_0_i28);
	init_label(mercury__llds_common__process_rval_4_0_i15);
BEGIN_CODE

/* code for predicate 'llds_common__process_rval'/4 in mode 0 */
Define_static(mercury__llds_common__process_rval_4_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i1047);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 0));
	incr_sp_push_msg(8, "llds_common__process_rval");
	detstackvar(8) = (Integer) succip;
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__llds_common__process_rval_4_0,
		LABEL(mercury__llds_common__process_rval_4_0_i6),
		STATIC(mercury__llds_common__process_rval_4_0));
Define_label(mercury__llds_common__process_rval_4_0_i6);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__llds_common__process_rval_4_0_i5);
	if (((Integer) r3 != ((Integer) 1)))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i7);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__llds_common__process_rval_4_0_i7);
	if (((Integer) r3 != ((Integer) 2)))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__llds_common__process_rval_4_0,
		LABEL(mercury__llds_common__process_rval_4_0_i9),
		STATIC(mercury__llds_common__process_rval_4_0));
Define_label(mercury__llds_common__process_rval_4_0_i9);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__llds_common__process_rval_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__llds_common__process_rval_4_0,
		LABEL(mercury__llds_common__process_rval_4_0_i10),
		STATIC(mercury__llds_common__process_rval_4_0));
Define_label(mercury__llds_common__process_rval_4_0_i10);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r3;
	localcall(mercury__llds_common__process_rval_4_0,
		LABEL(mercury__llds_common__process_rval_4_0_i11),
		STATIC(mercury__llds_common__process_rval_4_0));
Define_label(mercury__llds_common__process_rval_4_0_i11);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__llds_common__process_rval_4_0_i1047);
	incr_sp_push_msg(8, "llds_common__process_rval");
	detstackvar(8) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i12);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__llds_common__process_rval_4_0_i12);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i13);
	r1 = string_const("var rval found in llds_common__process_rval", 43);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__llds_common__process_rval_4_0_i14,
		STATIC(mercury__llds_common__process_rval_4_0));
	}
Define_label(mercury__llds_common__process_rval_4_0_i14);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__llds_common__process_rval_4_0_i13);
	r3 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 2));
	if (((Integer) r3 != ((Integer) 1)))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i15);
	detstackvar(2) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__llds_common__process_maybe_rvals_4_0),
		mercury__llds_common__process_rval_4_0_i18,
		STATIC(mercury__llds_common__process_rval_4_0));
Define_label(mercury__llds_common__process_rval_4_0_i18);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(7) = (Integer) r3;
	r4 = (Integer) r2;
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_1);
	r2 = (Integer) mercury_data_llds_common__base_type_info_cell_info_0;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__llds_common__process_rval_4_0_i21,
		STATIC(mercury__llds_common__process_rval_4_0));
	}
Define_label(mercury__llds_common__process_rval_4_0_i21);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i20);
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	tag_incr_hp(r4, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r4, ((Integer) 0)) = ((Integer) 1);
	tag_incr_hp(r5, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r5, ((Integer) 0)) = ((Integer) 2);
	tag_incr_hp(r6, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) detstackvar(5);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	field(mktag(0), (Integer) r6, ((Integer) 2)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r4;
	field(mktag(3), (Integer) r4, ((Integer) 1)) = (Integer) r5;
	field(mktag(3), (Integer) r5, ((Integer) 1)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
	}
Define_label(mercury__llds_common__process_rval_4_0_i20);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__llds_out__args_contain_pointers_2_0);
	call_localret(ENTRY(mercury__llds_out__args_contain_pointers_2_0),
		mercury__llds_common__process_rval_4_0_i25,
		STATIC(mercury__llds_common__process_rval_4_0));
	}
Define_label(mercury__llds_common__process_rval_4_0_i25);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__llds_common__process_rval_4_0_i1045);
	tag_incr_hp(r6, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r6, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r6, ((Integer) 1)) = (Integer) detstackvar(2);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	tag_incr_hp(r2, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 2);
	tag_incr_hp(r3, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = ((Integer) 0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) r6, ((Integer) 2)) = (Integer) r1;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) tempr1;
	r3 = (Integer) detstackvar(7);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_1);
	r2 = (Integer) mercury_data_llds_common__base_type_info_cell_info_0;
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r5, ((Integer) 1)) = ((Integer) 0);
	tempr1 = (Integer) detstackvar(6);
	r8 = ((Integer) tempr1 + ((Integer) 1));
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	GOTO_LABEL(mercury__llds_common__process_rval_4_0_i27);
	}
Define_label(mercury__llds_common__process_rval_4_0_i1045);
	tag_incr_hp(r6, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r6, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r6, ((Integer) 1)) = (Integer) detstackvar(2);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	tag_incr_hp(r2, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 2);
	tag_incr_hp(r3, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = ((Integer) 1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) r6, ((Integer) 2)) = (Integer) r1;
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) tempr1;
	r3 = (Integer) detstackvar(7);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_llds_common__common_1);
	r2 = (Integer) mercury_data_llds_common__base_type_info_cell_info_0;
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r5, ((Integer) 1)) = ((Integer) 1);
	tempr1 = (Integer) detstackvar(6);
	r8 = ((Integer) tempr1 + ((Integer) 1));
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	}
Define_label(mercury__llds_common__process_rval_4_0_i27);
	detstackvar(1) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	detstackvar(3) = (Integer) r8;
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__llds_common__process_rval_4_0_i28,
		STATIC(mercury__llds_common__process_rval_4_0));
	}
Define_label(mercury__llds_common__process_rval_4_0_i28);
	update_prof_current_proc(LABEL(mercury__llds_common__process_rval_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__llds_common__process_rval_4_0_i15);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module9)
	init_entry(mercury__llds_common__process_maybe_rvals_4_0);
	init_label(mercury__llds_common__process_maybe_rvals_4_0_i6);
	init_label(mercury__llds_common__process_maybe_rvals_4_0_i5);
	init_label(mercury__llds_common__process_maybe_rvals_4_0_i4);
	init_label(mercury__llds_common__process_maybe_rvals_4_0_i7);
	init_label(mercury__llds_common__process_maybe_rvals_4_0_i1004);
BEGIN_CODE

/* code for predicate 'llds_common__process_maybe_rvals'/4 in mode 0 */
Define_static(mercury__llds_common__process_maybe_rvals_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__llds_common__process_maybe_rvals_4_0_i1004);
	incr_sp_push_msg(2, "llds_common__process_maybe_rvals");
	detstackvar(2) = (Integer) succip;
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__llds_common__process_maybe_rvals_4_0_i5);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	call_localret(STATIC(mercury__llds_common__process_rval_4_0),
		mercury__llds_common__process_maybe_rvals_4_0_i6,
		STATIC(mercury__llds_common__process_maybe_rvals_4_0));
Define_label(mercury__llds_common__process_maybe_rvals_4_0_i6);
	update_prof_current_proc(LABEL(mercury__llds_common__process_maybe_rvals_4_0));
	tag_incr_hp(r3, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__llds_common__process_maybe_rvals_4_0_i4);
Define_label(mercury__llds_common__process_maybe_rvals_4_0_i5);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
Define_label(mercury__llds_common__process_maybe_rvals_4_0_i4);
	detstackvar(1) = (Integer) r3;
	localcall(mercury__llds_common__process_maybe_rvals_4_0,
		LABEL(mercury__llds_common__process_maybe_rvals_4_0_i7),
		STATIC(mercury__llds_common__process_maybe_rvals_4_0));
Define_label(mercury__llds_common__process_maybe_rvals_4_0_i7);
	update_prof_current_proc(LABEL(mercury__llds_common__process_maybe_rvals_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__llds_common__process_maybe_rvals_4_0_i1004);
	r1 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module10)
	init_entry(mercury____Unify___llds_common__cell_info_0_0);
	init_label(mercury____Unify___llds_common__cell_info_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_static(mercury____Unify___llds_common__cell_info_0_0);
	if (((Integer) field(mktag(0), (Integer) r1, ((Integer) 0)) != (Integer) field(mktag(0), (Integer) r2, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___llds_common__cell_info_0_0_i1);
	if (((Integer) field(mktag(0), (Integer) r1, ((Integer) 1)) != (Integer) field(mktag(0), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury____Unify___llds_common__cell_info_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___llds_common__cell_info_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__llds_common_module11)
	init_entry(mercury____Index___llds_common__cell_info_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_static(mercury____Index___llds_common__cell_info_0_0);
	tailcall(STATIC(mercury____Index___llds_common_cell_info_0__ua10000_2_0),
		STATIC(mercury____Index___llds_common__cell_info_0_0));
END_MODULE

BEGIN_MODULE(mercury__llds_common_module12)
	init_entry(mercury____Compare___llds_common__cell_info_0_0);
	init_label(mercury____Compare___llds_common__cell_info_0_0_i4);
	init_label(mercury____Compare___llds_common__cell_info_0_0_i3);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_static(mercury____Compare___llds_common__cell_info_0_0);
	incr_sp_push_msg(3, "__Compare__");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___llds_common__cell_info_0_0_i4,
		STATIC(mercury____Compare___llds_common__cell_info_0_0));
	}
Define_label(mercury____Compare___llds_common__cell_info_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___llds_common__cell_info_0_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___llds_common__cell_info_0_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury____Compare___llds_common__cell_info_0_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		STATIC(mercury____Compare___llds_common__cell_info_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__llds_common_bunch_0(void)
{
	mercury__llds_common_module0();
	mercury__llds_common_module1();
	mercury__llds_common_module2();
	mercury__llds_common_module3();
	mercury__llds_common_module4();
	mercury__llds_common_module5();
	mercury__llds_common_module6();
	mercury__llds_common_module7();
	mercury__llds_common_module8();
	mercury__llds_common_module9();
	mercury__llds_common_module10();
	mercury__llds_common_module11();
	mercury__llds_common_module12();
}

#endif

void mercury__llds_common__init(void); /* suppress gcc warning */
void mercury__llds_common__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__llds_common_bunch_0();
#endif
}
