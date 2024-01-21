/*
** Automatically generated from `dense_switch.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__dense_switch__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__dense_switch__is_dense_switch_9_0);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i2);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i5);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i7);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i11);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i12);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i13);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i16);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i18);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i15);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i8);
Declare_label(mercury__dense_switch__is_dense_switch_9_0_i1);
Define_extern_entry(mercury__dense_switch__generate_11_0);
Declare_label(mercury__dense_switch__generate_11_0_i2);
Declare_label(mercury__dense_switch__generate_11_0_i3);
Declare_label(mercury__dense_switch__generate_11_0_i6);
Declare_label(mercury__dense_switch__generate_11_0_i8);
Declare_label(mercury__dense_switch__generate_11_0_i9);
Declare_label(mercury__dense_switch__generate_11_0_i7);
Declare_label(mercury__dense_switch__generate_11_0_i10);
Declare_label(mercury__dense_switch__generate_11_0_i14);
Declare_label(mercury__dense_switch__generate_11_0_i11);
Define_extern_entry(mercury__dense_switch__calc_density_3_0);
Define_extern_entry(mercury__dense_switch__type_range_5_0);
Declare_label(mercury__dense_switch__type_range_5_0_i1001);
Declare_label(mercury__dense_switch__type_range_5_0_i7);
Declare_label(mercury__dense_switch__type_range_5_0_i6);
Declare_label(mercury__dense_switch__type_range_5_0_i9);
Declare_label(mercury__dense_switch__type_range_5_0_i10);
Declare_label(mercury__dense_switch__type_range_5_0_i11);
Declare_label(mercury__dense_switch__type_range_5_0_i12);
Declare_label(mercury__dense_switch__type_range_5_0_i13);
Declare_label(mercury__dense_switch__type_range_5_0_i14);
Declare_label(mercury__dense_switch__type_range_5_0_i18);
Declare_label(mercury__dense_switch__type_range_5_0_i15);
Declare_label(mercury__dense_switch__type_range_5_0_i1);
Declare_static(mercury__dense_switch__generate_cases_12_0);
Declare_label(mercury__dense_switch__generate_cases_12_0_i2);
Declare_label(mercury__dense_switch__generate_cases_12_0_i4);
Declare_label(mercury__dense_switch__generate_cases_12_0_i9);
Declare_label(mercury__dense_switch__generate_cases_12_0_i10);
Declare_label(mercury__dense_switch__generate_cases_12_0_i11);
Declare_label(mercury__dense_switch__generate_cases_12_0_i12);
Declare_label(mercury__dense_switch__generate_cases_12_0_i13);
Declare_label(mercury__dense_switch__generate_cases_12_0_i5);
Declare_label(mercury__dense_switch__generate_cases_12_0_i14);
Declare_label(mercury__dense_switch__generate_cases_12_0_i15);
Declare_label(mercury__dense_switch__generate_cases_12_0_i16);
Declare_label(mercury__dense_switch__generate_cases_12_0_i19);
Declare_label(mercury__dense_switch__generate_cases_12_0_i20);

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_prog_data__base_type_info_sym_name_0[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_dense_switch__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_prog_data__base_type_info_sym_name_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

BEGIN_MODULE(mercury__dense_switch_module0)
	init_entry(mercury__dense_switch__is_dense_switch_9_0);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i2);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i5);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i7);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i11);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i12);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i13);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i16);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i18);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i15);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i8);
	init_label(mercury__dense_switch__is_dense_switch_9_0_i1);
BEGIN_CODE

/* code for predicate 'dense_switch__is_dense_switch'/9 in mode 0 */
Define_entry(mercury__dense_switch__is_dense_switch_9_0);
	incr_sp_push_msg(8, "dense_switch__is_dense_switch");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	{
	extern Word * mercury_data_switch_gen__base_type_info_extended_case_0[];
	r1 = (Integer) mercury_data_switch_gen__base_type_info_extended_case_0;
	}
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__dense_switch__is_dense_switch_9_0_i2,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i2);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	if (((Integer) r1 <= ((Integer) 2)))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i1);
	{
	Word tempr1, tempr2, tempr3;
	tempr1 = (Integer) detstackvar(2);
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i1);
	tempr3 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0)), ((Integer) 1));
	if ((tag((Integer) tempr3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i1);
	if (((Integer) field(mktag(3), (Integer) tempr3, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i1);
	r2 = (Integer) tempr1;
	r3 = (Integer) r1;
	detstackvar(2) = (Integer) r1;
	detstackvar(6) = (Integer) field(mktag(3), (Integer) tempr3, ((Integer) 1));
	{
	extern Word * mercury_data_switch_gen__base_type_info_extended_case_0[];
	r1 = (Integer) mercury_data_switch_gen__base_type_info_extended_case_0;
	}
	{
	Declare_entry(mercury__list__index1_det_3_0);
	call_localret(ENTRY(mercury__list__index1_det_3_0),
		mercury__dense_switch__is_dense_switch_9_0_i5,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i5);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i1);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i1);
	{
	Word tempr1, tempr2;
	tempr2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	detstackvar(7) = (Integer) tempr2;
	r1 = (Integer) detstackvar(2);
	r2 = (((Integer) tempr2 - (Integer) detstackvar(6)) + ((Integer) 1));
	{
		call_localret(STATIC(mercury__dense_switch__calc_density_3_0),
		mercury__dense_switch__is_dense_switch_9_0_i7,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i7);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	if (((Integer) r1 <= (Integer) detstackvar(4)))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i1);
	if (((Integer) detstackvar(3) != ((Integer) 0)))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__code_info__variable_type_4_0);
	call_localret(ENTRY(mercury__code_info__variable_type_4_0),
		mercury__dense_switch__is_dense_switch_9_0_i11,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i11);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__code_info__get_module_info_3_0);
	call_localret(ENTRY(mercury__code_info__get_module_info_3_0),
		mercury__dense_switch__is_dense_switch_9_0_i12,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i12);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	detstackvar(5) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__type_util__classify_type_3_0);
	call_localret(ENTRY(mercury__type_util__classify_type_3_0),
		mercury__dense_switch__is_dense_switch_9_0_i13,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i13);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(5);
	{
		call_localret(STATIC(mercury__dense_switch__type_range_5_0),
		mercury__dense_switch__is_dense_switch_9_0_i16,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i16);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i15);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r3;
	{
		call_localret(STATIC(mercury__dense_switch__calc_density_3_0),
		mercury__dense_switch__is_dense_switch_9_0_i18,
		ENTRY(mercury__dense_switch__is_dense_switch_9_0));
	}
Define_label(mercury__dense_switch__is_dense_switch_9_0_i18);
	update_prof_current_proc(LABEL(mercury__dense_switch__is_dense_switch_9_0));
	if (((Integer) r1 <= (Integer) detstackvar(4)))
		GOTO_LABEL(mercury__dense_switch__is_dense_switch_9_0_i15);
	r2 = ((Integer) 0);
	r3 = ((Integer) detstackvar(1) - ((Integer) 1));
	r4 = ((Integer) 1);
	r5 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__dense_switch__is_dense_switch_9_0_i15);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(5);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__dense_switch__is_dense_switch_9_0_i8);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(5);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__dense_switch__is_dense_switch_9_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dense_switch_module1)
	init_entry(mercury__dense_switch__generate_11_0);
	init_label(mercury__dense_switch__generate_11_0_i2);
	init_label(mercury__dense_switch__generate_11_0_i3);
	init_label(mercury__dense_switch__generate_11_0_i6);
	init_label(mercury__dense_switch__generate_11_0_i8);
	init_label(mercury__dense_switch__generate_11_0_i9);
	init_label(mercury__dense_switch__generate_11_0_i7);
	init_label(mercury__dense_switch__generate_11_0_i10);
	init_label(mercury__dense_switch__generate_11_0_i14);
	init_label(mercury__dense_switch__generate_11_0_i11);
BEGIN_CODE

/* code for predicate 'dense_switch__generate'/11 in mode 0 */
Define_entry(mercury__dense_switch__generate_11_0);
	incr_sp_push_msg(9, "dense_switch__generate");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r8;
	r1 = (Integer) r4;
	r2 = (Integer) r9;
	{
	Declare_entry(mercury__code_info__produce_variable_5_0);
	call_localret(ENTRY(mercury__code_info__produce_variable_5_0),
		mercury__dense_switch__generate_11_0_i2,
		ENTRY(mercury__dense_switch__generate_11_0));
	}
Define_label(mercury__dense_switch__generate_11_0_i2);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_11_0));
	if (((Integer) detstackvar(2) != ((Integer) 0)))
		GOTO_LABEL(mercury__dense_switch__generate_11_0_i3);
	r9 = (Integer) r1;
	r10 = (Integer) r2;
	r2 = (Integer) r3;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	GOTO_LABEL(mercury__dense_switch__generate_11_0_i6);
Define_label(mercury__dense_switch__generate_11_0_i3);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	r9 = (Integer) r1;
	r11 = (Integer) r3;
	tag_incr_hp(r10, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r10, ((Integer) 2)) = (Integer) r2;
	field(mktag(3), (Integer) r10, ((Integer) 1)) = ((Integer) 1);
	field(mktag(3), (Integer) r10, ((Integer) 0)) = ((Integer) 3);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	tag_incr_hp(r13, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r13, ((Integer) 0)) = ((Integer) 1);
	r2 = (Integer) r11;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	field(mktag(3), (Integer) r10, ((Integer) 3)) = (Integer) r13;
	field(mktag(3), (Integer) r13, ((Integer) 1)) = (Integer) tempr1;
	}
Define_label(mercury__dense_switch__generate_11_0_i6);
	if (((Integer) r6 == ((Integer) 0)))
		GOTO_LABEL(mercury__dense_switch__generate_11_0_i8);
	r6 = (Integer) r8;
	r8 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	r5 = (Integer) r7;
	r7 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r11 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	GOTO_LABEL(mercury__dense_switch__generate_11_0_i7);
Define_label(mercury__dense_switch__generate_11_0_i8);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r9;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r8;
	detstackvar(8) = (Integer) r10;
	tag_incr_hp(r1, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = ((Integer) 23);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = ((Integer) 6);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) r10;
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) tempr1;
	tag_incr_hp(r11, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r11, ((Integer) 0)) = ((Integer) 1);
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(3), (Integer) r11, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r11;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = ((Integer) r4 - (Integer) r3);
	{
	Declare_entry(mercury__code_info__generate_test_and_fail_4_0);
	call_localret(ENTRY(mercury__code_info__generate_test_and_fail_4_0),
		mercury__dense_switch__generate_11_0_i9,
		ENTRY(mercury__dense_switch__generate_11_0));
	}
	}
Define_label(mercury__dense_switch__generate_11_0_i9);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_11_0));
	r8 = (Integer) r2;
	r11 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(6);
	r6 = (Integer) detstackvar(7);
	r7 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r9 = (Integer) detstackvar(5);
	r10 = (Integer) detstackvar(8);
Define_label(mercury__dense_switch__generate_11_0_i7);
	detstackvar(5) = (Integer) r9;
	detstackvar(8) = (Integer) r10;
	detstackvar(1) = (Integer) r11;
	call_localret(STATIC(mercury__dense_switch__generate_cases_12_0),
		mercury__dense_switch__generate_11_0_i10,
		ENTRY(mercury__dense_switch__generate_11_0));
Define_label(mercury__dense_switch__generate_11_0_i10);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_11_0));
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dense_switch__generate_11_0_i11);
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r2 = (Integer) r4;
	{
	Declare_entry(mercury__code_info__set_liveness_info_3_0);
	call_localret(ENTRY(mercury__code_info__set_liveness_info_3_0),
		mercury__dense_switch__generate_11_0_i14,
		ENTRY(mercury__dense_switch__generate_11_0));
	}
Define_label(mercury__dense_switch__generate_11_0_i14);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_11_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(2), ((Integer) 2));
	field(mktag(2), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(5);
	tag_incr_hp(r3, mktag(2), ((Integer) 2));
	field(mktag(2), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	tag_incr_hp(r4, mktag(2), ((Integer) 2));
	tag_incr_hp(r5, mktag(1), ((Integer) 1));
	tag_incr_hp(r6, mktag(1), ((Integer) 2));
	tag_incr_hp(r7, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(0), (Integer) r7, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(8);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 7);
	field(mktag(2), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	field(mktag(2), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	field(mktag(2), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(2), (Integer) r4, ((Integer) 0)) = (Integer) r5;
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r6, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r6, ((Integer) 0)) = (Integer) r7;
	field(mktag(0), (Integer) r7, ((Integer) 1)) = string_const("switch (using dense jump table)", 31);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
	}
Define_label(mercury__dense_switch__generate_11_0_i11);
	r1 = string_const("dense_switch__generate: no liveness!", 36);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__dense_switch__generate_11_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dense_switch_module2)
	init_entry(mercury__dense_switch__calc_density_3_0);
BEGIN_CODE

/* code for predicate 'dense_switch__calc_density'/3 in mode 0 */
Define_entry(mercury__dense_switch__calc_density_3_0);
	r1 = (((Integer) r1 * ((Integer) 100)) / (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dense_switch_module3)
	init_entry(mercury__dense_switch__type_range_5_0);
	init_label(mercury__dense_switch__type_range_5_0_i1001);
	init_label(mercury__dense_switch__type_range_5_0_i7);
	init_label(mercury__dense_switch__type_range_5_0_i6);
	init_label(mercury__dense_switch__type_range_5_0_i9);
	init_label(mercury__dense_switch__type_range_5_0_i10);
	init_label(mercury__dense_switch__type_range_5_0_i11);
	init_label(mercury__dense_switch__type_range_5_0_i12);
	init_label(mercury__dense_switch__type_range_5_0_i13);
	init_label(mercury__dense_switch__type_range_5_0_i14);
	init_label(mercury__dense_switch__type_range_5_0_i18);
	init_label(mercury__dense_switch__type_range_5_0_i15);
	init_label(mercury__dense_switch__type_range_5_0_i1);
BEGIN_CODE

/* code for predicate 'dense_switch__type_range'/5 in mode 0 */
Define_entry(mercury__dense_switch__type_range_5_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury__dense_switch__type_range_5_0_i1001);
	r2 = ((Integer) 128);
	r1 = TRUE;
	proceed();
Define_label(mercury__dense_switch__type_range_5_0_i1001);
	incr_sp_push_msg(4, "dense_switch__type_range");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 5)))))
		GOTO_LABEL(mercury__dense_switch__type_range_5_0_i1);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__type_util__type_to_type_id_3_0);
	call_localret(ENTRY(mercury__type_util__type_to_type_id_3_0),
		mercury__dense_switch__type_range_5_0_i7,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i7);
	update_prof_current_proc(LABEL(mercury__dense_switch__type_range_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dense_switch__type_range_5_0_i6);
	r1 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__dense_switch__type_range_5_0_i10);
Define_label(mercury__dense_switch__type_range_5_0_i6);
	r1 = string_const("dense_switch__type_range: invalid enum type?", 44);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__dense_switch__type_range_5_0_i9,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i9);
	update_prof_current_proc(LABEL(mercury__dense_switch__type_range_5_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
Define_label(mercury__dense_switch__type_range_5_0_i10);
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__code_info__get_module_info_3_0);
	call_localret(ENTRY(mercury__code_info__get_module_info_3_0),
		mercury__dense_switch__type_range_5_0_i11,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i11);
	update_prof_current_proc(LABEL(mercury__dense_switch__type_range_5_0));
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_types_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_types_2_0),
		mercury__dense_switch__type_range_5_0_i12,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i12);
	update_prof_current_proc(LABEL(mercury__dense_switch__type_range_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_dense_switch__common_0);
	{
	extern Word * mercury_data_hlds_data__base_type_info_hlds__type_defn_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_hlds__type_defn_0;
	}
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__dense_switch__type_range_5_0_i13,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i13);
	update_prof_current_proc(LABEL(mercury__dense_switch__type_range_5_0));
	{
	Declare_entry(mercury__hlds_data__get_type_defn_body_2_0);
	call_localret(ENTRY(mercury__hlds_data__get_type_defn_body_2_0),
		mercury__dense_switch__type_range_5_0_i14,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i14);
	update_prof_current_proc(LABEL(mercury__dense_switch__type_range_5_0));
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__dense_switch__type_range_5_0_i15);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_id_0[];
	r1 = (Integer) mercury_data_hlds_data__base_type_info_cons_id_0;
	}
	{
	extern Word * mercury_data_hlds_data__base_type_info_cons_tag_0[];
	r2 = (Integer) mercury_data_hlds_data__base_type_info_cons_tag_0;
	}
	{
	Declare_entry(mercury__map__count_2_0);
	call_localret(ENTRY(mercury__map__count_2_0),
		mercury__dense_switch__type_range_5_0_i18,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i18);
	update_prof_current_proc(LABEL(mercury__dense_switch__type_range_5_0));
	r2 = (Integer) r1;
	r3 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dense_switch__type_range_5_0_i15);
	r1 = string_const("dense_switch__type_range: enum type is not d.u. type?", 53);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__dense_switch__type_range_5_0_i18,
		ENTRY(mercury__dense_switch__type_range_5_0));
	}
Define_label(mercury__dense_switch__type_range_5_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dense_switch_module4)
	init_entry(mercury__dense_switch__generate_cases_12_0);
	init_label(mercury__dense_switch__generate_cases_12_0_i2);
	init_label(mercury__dense_switch__generate_cases_12_0_i4);
	init_label(mercury__dense_switch__generate_cases_12_0_i9);
	init_label(mercury__dense_switch__generate_cases_12_0_i10);
	init_label(mercury__dense_switch__generate_cases_12_0_i11);
	init_label(mercury__dense_switch__generate_cases_12_0_i12);
	init_label(mercury__dense_switch__generate_cases_12_0_i13);
	init_label(mercury__dense_switch__generate_cases_12_0_i5);
	init_label(mercury__dense_switch__generate_cases_12_0_i14);
	init_label(mercury__dense_switch__generate_cases_12_0_i15);
	init_label(mercury__dense_switch__generate_cases_12_0_i16);
	init_label(mercury__dense_switch__generate_cases_12_0_i19);
	init_label(mercury__dense_switch__generate_cases_12_0_i20);
BEGIN_CODE

/* code for predicate 'dense_switch__generate_cases'/12 in mode 0 */
Define_static(mercury__dense_switch__generate_cases_12_0);
	incr_sp_push_msg(11, "dense_switch__generate_cases");
	detstackvar(11) = (Integer) succip;
	if (((Integer) r2 <= (Integer) r3))
		GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i2);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tag_incr_hp(r2, mktag(1), ((Integer) 1));
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r6;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = string_const("End of dense switch", 19);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	r3 = (Integer) r7;
	r4 = (Integer) r8;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i2);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	r1 = (Integer) r8;
	{
	Declare_entry(mercury__code_info__get_next_label_3_0);
	call_localret(ENTRY(mercury__code_info__get_next_label_3_0),
		mercury__dense_switch__generate_cases_12_0_i4,
		STATIC(mercury__dense_switch__generate_cases_12_0));
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i4);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	if (((Integer) detstackvar(1) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i5);
	r3 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) detstackvar(1), ((Integer) 0)), ((Integer) 1));
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i5);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i5);
	if (((Integer) detstackvar(2) != (Integer) field(mktag(3), (Integer) r3, ((Integer) 1))))
		GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i5);
	r3 = (Integer) detstackvar(1);
	detstackvar(8) = (Integer) r1;
	detstackvar(9) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r3, ((Integer) 0)), ((Integer) 3));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__code_info__grab_code_info_3_0);
	call_localret(ENTRY(mercury__code_info__grab_code_info_3_0),
		mercury__dense_switch__generate_cases_12_0_i9,
		STATIC(mercury__dense_switch__generate_cases_12_0));
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i9);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	r3 = (Integer) r2;
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__code_gen__generate_goal_5_0);
	call_localret(ENTRY(mercury__code_gen__generate_goal_5_0),
		mercury__dense_switch__generate_cases_12_0_i10,
		STATIC(mercury__dense_switch__generate_cases_12_0));
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i10);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__code_info__generate_branch_end_5_0);
	call_localret(ENTRY(mercury__code_info__generate_branch_end_5_0),
		mercury__dense_switch__generate_cases_12_0_i11,
		STATIC(mercury__dense_switch__generate_cases_12_0));
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i11);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	r3 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(2), ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(2), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) r2;
	field(mktag(2), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	{
	Declare_entry(mercury__code_info__get_liveness_info_3_0);
	call_localret(ENTRY(mercury__code_info__get_liveness_info_3_0),
		mercury__dense_switch__generate_cases_12_0_i12,
		STATIC(mercury__dense_switch__generate_cases_12_0));
	}
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i12);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	r3 = (Integer) detstackvar(10);
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__code_info__slap_code_info_3_0);
	call_localret(ENTRY(mercury__code_info__slap_code_info_3_0),
		mercury__dense_switch__generate_cases_12_0_i13,
		STATIC(mercury__dense_switch__generate_cases_12_0));
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i13);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	r8 = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r9 = (Integer) detstackvar(7);
	r10 = (Integer) detstackvar(8);
	tag_incr_hp(r7, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) detstackvar(10);
	tag_incr_hp(r11, mktag(2), ((Integer) 2));
	tag_incr_hp(r12, mktag(1), ((Integer) 1));
	tag_incr_hp(r13, mktag(1), ((Integer) 2));
	tag_incr_hp(r14, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r10;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(1), (Integer) r13, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r14, ((Integer) 1)) = string_const("case of dense switch", 20);
	field(mktag(2), (Integer) r11, ((Integer) 0)) = (Integer) r12;
	field(mktag(1), (Integer) r12, ((Integer) 0)) = (Integer) r13;
	field(mktag(1), (Integer) r13, ((Integer) 0)) = (Integer) r14;
	field(mktag(0), (Integer) r14, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r12, mktag(2), ((Integer) 2));
	field(mktag(2), (Integer) r12, ((Integer) 0)) = (Integer) detstackvar(1);
	tag_incr_hp(r13, mktag(1), ((Integer) 1));
	tag_incr_hp(r14, mktag(1), ((Integer) 2));
	tag_incr_hp(r15, mktag(0), ((Integer) 2));
	tag_incr_hp(r16, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r16, ((Integer) 0)) = ((Integer) 6);
	field(mktag(1), (Integer) r14, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r6;
	field(mktag(0), (Integer) r15, ((Integer) 1)) = string_const("branch to end of dense switch", 29);
	field(mktag(3), (Integer) r16, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(2), (Integer) r11, ((Integer) 1)) = (Integer) r12;
	field(mktag(2), (Integer) r12, ((Integer) 1)) = (Integer) r13;
	field(mktag(1), (Integer) r13, ((Integer) 0)) = (Integer) r14;
	field(mktag(1), (Integer) r14, ((Integer) 0)) = (Integer) r15;
	field(mktag(0), (Integer) r15, ((Integer) 0)) = (Integer) r16;
	GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i15);
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i5);
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__code_info__generate_failure_3_0);
	call_localret(ENTRY(mercury__code_info__generate_failure_3_0),
		mercury__dense_switch__generate_cases_12_0_i14,
		STATIC(mercury__dense_switch__generate_cases_12_0));
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i14);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	r8 = (Integer) r1;
	r12 = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r9 = (Integer) detstackvar(7);
	r10 = (Integer) detstackvar(8);
	tag_incr_hp(r11, mktag(2), ((Integer) 2));
	tag_incr_hp(r13, mktag(1), ((Integer) 1));
	tag_incr_hp(r14, mktag(1), ((Integer) 2));
	tag_incr_hp(r15, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r10;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(1), (Integer) r14, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r15, ((Integer) 1)) = string_const("compiler-introduced `fail' case of dense switch", 47);
	field(mktag(2), (Integer) r11, ((Integer) 0)) = (Integer) r13;
	field(mktag(1), (Integer) r13, ((Integer) 0)) = (Integer) r14;
	field(mktag(1), (Integer) r14, ((Integer) 0)) = (Integer) r15;
	field(mktag(0), (Integer) r15, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r13, mktag(2), ((Integer) 2));
	field(mktag(2), (Integer) r13, ((Integer) 0)) = (Integer) r8;
	tag_incr_hp(r14, mktag(1), ((Integer) 1));
	tag_incr_hp(r15, mktag(1), ((Integer) 2));
	tag_incr_hp(r16, mktag(0), ((Integer) 2));
	tag_incr_hp(r17, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r17, ((Integer) 0)) = ((Integer) 6);
	r8 = (Integer) r12;
	field(mktag(1), (Integer) r15, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r6;
	field(mktag(0), (Integer) r16, ((Integer) 1)) = string_const("branch to end of dense switch", 29);
	field(mktag(3), (Integer) r17, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(2), (Integer) r11, ((Integer) 1)) = (Integer) r13;
	field(mktag(2), (Integer) r13, ((Integer) 1)) = (Integer) r14;
	field(mktag(1), (Integer) r14, ((Integer) 0)) = (Integer) r15;
	field(mktag(1), (Integer) r15, ((Integer) 0)) = (Integer) r16;
	field(mktag(0), (Integer) r16, ((Integer) 0)) = (Integer) r17;
	}
Define_label(mercury__dense_switch__generate_cases_12_0_i15);
	if (((Integer) r9 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i16);
	r9 = (Integer) r10;
	r10 = (Integer) r11;
	r2 = ((Integer) r2 + ((Integer) 1));
	GOTO_LABEL(mercury__dense_switch__generate_cases_12_0_i19);
Define_label(mercury__dense_switch__generate_cases_12_0_i16);
	r7 = (Integer) r9;
	r9 = (Integer) r10;
	r10 = (Integer) r11;
	r2 = ((Integer) r2 + ((Integer) 1));
Define_label(mercury__dense_switch__generate_cases_12_0_i19);
	detstackvar(8) = (Integer) r9;
	detstackvar(1) = (Integer) r10;
	localcall(mercury__dense_switch__generate_cases_12_0,
		LABEL(mercury__dense_switch__generate_cases_12_0_i20),
		STATIC(mercury__dense_switch__generate_cases_12_0));
Define_label(mercury__dense_switch__generate_cases_12_0_i20);
	update_prof_current_proc(LABEL(mercury__dense_switch__generate_cases_12_0));
	r5 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(8);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r5;
	r6 = (Integer) r2;
	tag_incr_hp(r2, mktag(2), ((Integer) 2));
	field(mktag(2), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(2), (Integer) r2, ((Integer) 1)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__dense_switch_bunch_0(void)
{
	mercury__dense_switch_module0();
	mercury__dense_switch_module1();
	mercury__dense_switch_module2();
	mercury__dense_switch_module3();
	mercury__dense_switch_module4();
}

#endif

void mercury__dense_switch__init(void); /* suppress gcc warning */
void mercury__dense_switch__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__dense_switch_bunch_0();
#endif
}
