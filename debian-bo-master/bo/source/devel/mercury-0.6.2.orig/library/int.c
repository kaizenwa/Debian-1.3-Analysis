/*
** Automatically generated from `int.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__int__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__int__f_less_than_2_0);
Declare_label(mercury__int__f_less_than_2_0_i1);
Define_extern_entry(mercury__int__f_greater_than_2_0);
Declare_label(mercury__int__f_greater_than_2_0_i1);
Define_extern_entry(mercury__int__f_less_or_equal_2_0);
Declare_label(mercury__int__f_less_or_equal_2_0_i1);
Define_extern_entry(mercury__int__f_greater_or_equal_2_0);
Declare_label(mercury__int__f_greater_or_equal_2_0_i1);
Define_extern_entry(mercury__int__abs_2_0);
Declare_label(mercury__int__abs_2_0_i4);
Define_extern_entry(mercury__int__max_3_0);
Declare_label(mercury__int__max_3_0_i4);
Define_extern_entry(mercury__int__min_3_0);
Declare_label(mercury__int__min_3_0_i4);
Define_extern_entry(mercury__int__to_float_2_0);
Define_extern_entry(mercury__int__pow_3_0);
Declare_label(mercury__int__pow_3_0_i1003);
Define_extern_entry(mercury__int__log2_2_0);
Declare_label(mercury__int__log2_2_0_i1003);
Define_extern_entry(mercury__fn__int__f_plus_2_0);
Define_extern_entry(mercury__fn__int__f_plus_2_1);
Define_extern_entry(mercury__fn__int__f_plus_2_2);
Define_extern_entry(mercury__fn__int__f_times_2_0);
Define_extern_entry(mercury__fn__int__f_minus_2_0);
Define_extern_entry(mercury__fn__int__f_minus_2_1);
Define_extern_entry(mercury__fn__int__f_minus_2_2);
Define_extern_entry(mercury__fn__int__mod_2_0);
Define_extern_entry(mercury__fn__int__f_47_47_2_0);
Define_extern_entry(mercury__fn__int__f_60_60_2_0);
Define_extern_entry(mercury__fn__int__f_62_62_2_0);
Define_extern_entry(mercury__fn__int__f_47_92_2_0);
Define_extern_entry(mercury__fn__int__f_92_47_2_0);
Define_extern_entry(mercury__fn__int__f_94_2_0);
Define_extern_entry(mercury__fn__int__f_92_1_0);
Define_extern_entry(mercury__fn__int__f_plus_1_0);
Define_extern_entry(mercury__fn__int__f_minus_1_0);
Define_extern_entry(mercury__int__is_2_0);
Define_extern_entry(mercury__int__is_2_1);
Define_extern_entry(mercury__int__builtin_plus_3_0);
Define_extern_entry(mercury__int__builtin_plus_3_1);
Define_extern_entry(mercury__int__builtin_unary_plus_2_0);
Define_extern_entry(mercury__int__builtin_minus_3_0);
Define_extern_entry(mercury__int__builtin_unary_minus_2_0);
Define_extern_entry(mercury__int__builtin_times_3_0);
Define_extern_entry(mercury__int__builtin_div_3_0);
Define_extern_entry(mercury__int__builtin_mod_3_0);
Define_extern_entry(mercury__int__builtin_left_shift_3_0);
Define_extern_entry(mercury__int__builtin_right_shift_3_0);
Define_extern_entry(mercury__int__builtin_bit_or_3_0);
Define_extern_entry(mercury__int__builtin_bit_and_3_0);
Define_extern_entry(mercury__int__builtin_bit_xor_3_0);
Define_extern_entry(mercury__int__builtin_bit_neg_2_0);
Declare_static(mercury__int__pow_2_4_0);
Declare_label(mercury__int__pow_2_4_0_i1003);
Declare_static(mercury__int__log2_2_3_0);
Declare_label(mercury__int__log2_2_3_0_i1003);

BEGIN_MODULE(mercury__int_module0)
	init_entry(mercury__int__f_less_than_2_0);
	init_label(mercury__int__f_less_than_2_0_i1);
BEGIN_CODE

/* code for predicate '<'/2 in mode 0 */
Define_entry(mercury__int__f_less_than_2_0);
	if (((Integer) r1 >= (Integer) r2))
		GOTO_LABEL(mercury__int__f_less_than_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__int__f_less_than_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module1)
	init_entry(mercury__int__f_greater_than_2_0);
	init_label(mercury__int__f_greater_than_2_0_i1);
BEGIN_CODE

/* code for predicate '>'/2 in mode 0 */
Define_entry(mercury__int__f_greater_than_2_0);
	if (((Integer) r1 <= (Integer) r2))
		GOTO_LABEL(mercury__int__f_greater_than_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__int__f_greater_than_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module2)
	init_entry(mercury__int__f_less_or_equal_2_0);
	init_label(mercury__int__f_less_or_equal_2_0_i1);
BEGIN_CODE

/* code for predicate '=<'/2 in mode 0 */
Define_entry(mercury__int__f_less_or_equal_2_0);
	if (((Integer) r1 > (Integer) r2))
		GOTO_LABEL(mercury__int__f_less_or_equal_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__int__f_less_or_equal_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module3)
	init_entry(mercury__int__f_greater_or_equal_2_0);
	init_label(mercury__int__f_greater_or_equal_2_0_i1);
BEGIN_CODE

/* code for predicate '>='/2 in mode 0 */
Define_entry(mercury__int__f_greater_or_equal_2_0);
	if (((Integer) r1 < (Integer) r2))
		GOTO_LABEL(mercury__int__f_greater_or_equal_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__int__f_greater_or_equal_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module4)
	init_entry(mercury__int__abs_2_0);
	init_label(mercury__int__abs_2_0_i4);
BEGIN_CODE

/* code for predicate 'int__abs'/2 in mode 0 */
Define_entry(mercury__int__abs_2_0);
	if (((Integer) r1 >= ((Integer) 0)))
		GOTO_LABEL(mercury__int__abs_2_0_i4);
	r1 = (((Integer) 0) - (Integer) r1);
Define_label(mercury__int__abs_2_0_i4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module5)
	init_entry(mercury__int__max_3_0);
	init_label(mercury__int__max_3_0_i4);
BEGIN_CODE

/* code for predicate 'int__max'/3 in mode 0 */
Define_entry(mercury__int__max_3_0);
	if (((Integer) r1 > (Integer) r2))
		GOTO_LABEL(mercury__int__max_3_0_i4);
	r1 = (Integer) r2;
Define_label(mercury__int__max_3_0_i4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module6)
	init_entry(mercury__int__min_3_0);
	init_label(mercury__int__min_3_0_i4);
BEGIN_CODE

/* code for predicate 'int__min'/3 in mode 0 */
Define_entry(mercury__int__min_3_0);
	if (((Integer) r1 < (Integer) r2))
		GOTO_LABEL(mercury__int__min_3_0_i4);
	r1 = (Integer) r2;
Define_label(mercury__int__min_3_0_i4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module7)
	init_entry(mercury__int__to_float_2_0);
BEGIN_CODE

/* code for predicate 'int__to_float'/2 in mode 0 */
Define_entry(mercury__int__to_float_2_0);
	{
		Integer	IntVal;
		Float	FloatVal;
		IntVal = (Integer) r1;
		
	FloatVal = IntVal;

		r2 = float_to_word(FloatVal);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module8)
	init_entry(mercury__int__pow_3_0);
	init_label(mercury__int__pow_3_0_i1003);
BEGIN_CODE

/* code for predicate 'int__pow'/3 in mode 0 */
Define_entry(mercury__int__pow_3_0);
	if (((Integer) r2 >= ((Integer) 0)))
		GOTO_LABEL(mercury__int__pow_3_0_i1003);
	r1 = string_const("int__pow: negative exponent", 27);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__int__pow_3_0));
	}
Define_label(mercury__int__pow_3_0_i1003);
	r3 = ((Integer) 1);
	tailcall(STATIC(mercury__int__pow_2_4_0),
		ENTRY(mercury__int__pow_3_0));
END_MODULE

BEGIN_MODULE(mercury__int_module9)
	init_entry(mercury__int__log2_2_0);
	init_label(mercury__int__log2_2_0_i1003);
BEGIN_CODE

/* code for predicate 'int__log2'/2 in mode 0 */
Define_entry(mercury__int__log2_2_0);
	if (((Integer) r1 <= ((Integer) 0)))
		GOTO_LABEL(mercury__int__log2_2_0_i1003);
	r2 = ((Integer) 0);
	tailcall(STATIC(mercury__int__log2_2_3_0),
		ENTRY(mercury__int__log2_2_0));
Define_label(mercury__int__log2_2_0_i1003);
	r1 = string_const("int__log2: cannot take log of a non-positive number", 51);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__int__log2_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__int_module10)
	init_entry(mercury__fn__int__f_plus_2_0);
BEGIN_CODE

/* code for predicate '+'/3 in mode 0 */
Define_entry(mercury__fn__int__f_plus_2_0);
	r1 = ((Integer) r1 + (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module11)
	init_entry(mercury__fn__int__f_plus_2_1);
BEGIN_CODE

/* code for predicate '+'/3 in mode 1 */
Define_entry(mercury__fn__int__f_plus_2_1);
	r1 = ((Integer) r2 - (Integer) r1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module12)
	init_entry(mercury__fn__int__f_plus_2_2);
BEGIN_CODE

/* code for predicate '+'/3 in mode 2 */
Define_entry(mercury__fn__int__f_plus_2_2);
	r1 = ((Integer) r2 - (Integer) r1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module13)
	init_entry(mercury__fn__int__f_times_2_0);
BEGIN_CODE

/* code for predicate '*'/3 in mode 0 */
Define_entry(mercury__fn__int__f_times_2_0);
	r1 = ((Integer) r1 * (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module14)
	init_entry(mercury__fn__int__f_minus_2_0);
BEGIN_CODE

/* code for predicate '-'/3 in mode 0 */
Define_entry(mercury__fn__int__f_minus_2_0);
	r1 = ((Integer) r1 - (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module15)
	init_entry(mercury__fn__int__f_minus_2_1);
BEGIN_CODE

/* code for predicate '-'/3 in mode 1 */
Define_entry(mercury__fn__int__f_minus_2_1);
	r1 = ((Integer) r1 + (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module16)
	init_entry(mercury__fn__int__f_minus_2_2);
BEGIN_CODE

/* code for predicate '-'/3 in mode 2 */
Define_entry(mercury__fn__int__f_minus_2_2);
	r1 = ((Integer) r1 - (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module17)
	init_entry(mercury__fn__int__mod_2_0);
BEGIN_CODE

/* code for predicate 'mod'/3 in mode 0 */
Define_entry(mercury__fn__int__mod_2_0);
	r1 = ((Integer) r1 % (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module18)
	init_entry(mercury__fn__int__f_47_47_2_0);
BEGIN_CODE

/* code for predicate '//'/3 in mode 0 */
Define_entry(mercury__fn__int__f_47_47_2_0);
	r1 = ((Integer) r1 / (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module19)
	init_entry(mercury__fn__int__f_60_60_2_0);
BEGIN_CODE

/* code for predicate '<<'/3 in mode 0 */
Define_entry(mercury__fn__int__f_60_60_2_0);
	r1 = ((Integer) r1 << (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module20)
	init_entry(mercury__fn__int__f_62_62_2_0);
BEGIN_CODE

/* code for predicate '>>'/3 in mode 0 */
Define_entry(mercury__fn__int__f_62_62_2_0);
	r1 = ((Integer) r1 >> (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module21)
	init_entry(mercury__fn__int__f_47_92_2_0);
BEGIN_CODE

/* code for predicate '/\'/3 in mode 0 */
Define_entry(mercury__fn__int__f_47_92_2_0);
	r1 = ((Integer) r1 & (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module22)
	init_entry(mercury__fn__int__f_92_47_2_0);
BEGIN_CODE

/* code for predicate '\/'/3 in mode 0 */
Define_entry(mercury__fn__int__f_92_47_2_0);
	r1 = ((Integer) r1 | (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module23)
	init_entry(mercury__fn__int__f_94_2_0);
BEGIN_CODE

/* code for predicate '^'/3 in mode 0 */
Define_entry(mercury__fn__int__f_94_2_0);
	r1 = ((Integer) r1 ^ (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module24)
	init_entry(mercury__fn__int__f_92_1_0);
BEGIN_CODE

/* code for predicate '\'/2 in mode 0 */
Define_entry(mercury__fn__int__f_92_1_0);
	r1 = ~((Integer) r1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module25)
	init_entry(mercury__fn__int__f_plus_1_0);
BEGIN_CODE

/* code for predicate '+'/2 in mode 0 */
Define_entry(mercury__fn__int__f_plus_1_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module26)
	init_entry(mercury__fn__int__f_minus_1_0);
BEGIN_CODE

/* code for predicate '-'/2 in mode 0 */
Define_entry(mercury__fn__int__f_minus_1_0);
	r1 = (((Integer) 0) - (Integer) r1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module27)
	init_entry(mercury__int__is_2_0);
BEGIN_CODE

/* code for predicate 'is'/2 in mode 0 */
Define_entry(mercury__int__is_2_0);
	{
		Word	TypeInfo_for_T;
		Word	X;
		Word	Y;
		TypeInfo_for_T = (Integer) r1;
		Y = (Integer) r2;
		X = Y;
		r3 = X;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module28)
	init_entry(mercury__int__is_2_1);
BEGIN_CODE

/* code for predicate 'is'/2 in mode 1 */
Define_entry(mercury__int__is_2_1);
	{
		Word	TypeInfo_for_T;
		Word	X;
		Word	Y;
		TypeInfo_for_T = (Integer) r1;
		Y = (Integer) r2;
		X = Y;
		r3 = X;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module29)
	init_entry(mercury__int__builtin_plus_3_0);
BEGIN_CODE

/* code for predicate 'builtin_plus'/3 in mode 0 */
Define_entry(mercury__int__builtin_plus_3_0);
	r1 = ((Integer) r1 + (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module30)
	init_entry(mercury__int__builtin_plus_3_1);
BEGIN_CODE

/* code for predicate 'builtin_plus'/3 in mode 1 */
Define_entry(mercury__int__builtin_plus_3_1);
	r1 = ((Integer) r1 + (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module31)
	init_entry(mercury__int__builtin_unary_plus_2_0);
BEGIN_CODE

/* code for predicate 'builtin_unary_plus'/2 in mode 0 */
Define_entry(mercury__int__builtin_unary_plus_2_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module32)
	init_entry(mercury__int__builtin_minus_3_0);
BEGIN_CODE

/* code for predicate 'builtin_minus'/3 in mode 0 */
Define_entry(mercury__int__builtin_minus_3_0);
	r1 = ((Integer) r1 - (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module33)
	init_entry(mercury__int__builtin_unary_minus_2_0);
BEGIN_CODE

/* code for predicate 'builtin_unary_minus'/2 in mode 0 */
Define_entry(mercury__int__builtin_unary_minus_2_0);
	r1 = (((Integer) 0) - (Integer) r1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module34)
	init_entry(mercury__int__builtin_times_3_0);
BEGIN_CODE

/* code for predicate 'builtin_times'/3 in mode 0 */
Define_entry(mercury__int__builtin_times_3_0);
	r1 = ((Integer) r1 * (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module35)
	init_entry(mercury__int__builtin_div_3_0);
BEGIN_CODE

/* code for predicate 'builtin_div'/3 in mode 0 */
Define_entry(mercury__int__builtin_div_3_0);
	r1 = ((Integer) r1 / (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module36)
	init_entry(mercury__int__builtin_mod_3_0);
BEGIN_CODE

/* code for predicate 'builtin_mod'/3 in mode 0 */
Define_entry(mercury__int__builtin_mod_3_0);
	r1 = ((Integer) r1 % (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module37)
	init_entry(mercury__int__builtin_left_shift_3_0);
BEGIN_CODE

/* code for predicate 'builtin_left_shift'/3 in mode 0 */
Define_entry(mercury__int__builtin_left_shift_3_0);
	r1 = ((Integer) r1 << (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module38)
	init_entry(mercury__int__builtin_right_shift_3_0);
BEGIN_CODE

/* code for predicate 'builtin_right_shift'/3 in mode 0 */
Define_entry(mercury__int__builtin_right_shift_3_0);
	r1 = ((Integer) r1 >> (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module39)
	init_entry(mercury__int__builtin_bit_or_3_0);
BEGIN_CODE

/* code for predicate 'builtin_bit_or'/3 in mode 0 */
Define_entry(mercury__int__builtin_bit_or_3_0);
	r1 = ((Integer) r1 | (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module40)
	init_entry(mercury__int__builtin_bit_and_3_0);
BEGIN_CODE

/* code for predicate 'builtin_bit_and'/3 in mode 0 */
Define_entry(mercury__int__builtin_bit_and_3_0);
	r1 = ((Integer) r1 & (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module41)
	init_entry(mercury__int__builtin_bit_xor_3_0);
BEGIN_CODE

/* code for predicate 'builtin_bit_xor'/3 in mode 0 */
Define_entry(mercury__int__builtin_bit_xor_3_0);
	r1 = ((Integer) r1 ^ (Integer) r2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module42)
	init_entry(mercury__int__builtin_bit_neg_2_0);
BEGIN_CODE

/* code for predicate 'builtin_bit_neg'/2 in mode 0 */
Define_entry(mercury__int__builtin_bit_neg_2_0);
	r1 = ~((Integer) r1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__int_module43)
	init_entry(mercury__int__pow_2_4_0);
	init_label(mercury__int__pow_2_4_0_i1003);
BEGIN_CODE

/* code for predicate 'int__pow_2'/4 in mode 0 */
Define_static(mercury__int__pow_2_4_0);
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__int__pow_2_4_0_i1003);
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__int__pow_2_4_0_i1003);
	r2 = ((Integer) r2 - ((Integer) 1));
	r3 = ((Integer) r3 * (Integer) r1);
	localtailcall(mercury__int__pow_2_4_0,
		STATIC(mercury__int__pow_2_4_0));
END_MODULE

BEGIN_MODULE(mercury__int_module44)
	init_entry(mercury__int__log2_2_3_0);
	init_label(mercury__int__log2_2_3_0_i1003);
BEGIN_CODE

/* code for predicate 'int__log2_2'/3 in mode 0 */
Define_static(mercury__int__log2_2_3_0);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__int__log2_2_3_0_i1003);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__int__log2_2_3_0_i1003);
	r1 = (((Integer) r1 + ((Integer) 1)) / ((Integer) 2));
	r2 = ((Integer) r2 + ((Integer) 1));
	localtailcall(mercury__int__log2_2_3_0,
		STATIC(mercury__int__log2_2_3_0));
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__int_bunch_0(void)
{
	mercury__int_module0();
	mercury__int_module1();
	mercury__int_module2();
	mercury__int_module3();
	mercury__int_module4();
	mercury__int_module5();
	mercury__int_module6();
	mercury__int_module7();
	mercury__int_module8();
	mercury__int_module9();
	mercury__int_module10();
	mercury__int_module11();
	mercury__int_module12();
	mercury__int_module13();
	mercury__int_module14();
	mercury__int_module15();
	mercury__int_module16();
	mercury__int_module17();
	mercury__int_module18();
	mercury__int_module19();
	mercury__int_module20();
	mercury__int_module21();
	mercury__int_module22();
	mercury__int_module23();
	mercury__int_module24();
	mercury__int_module25();
	mercury__int_module26();
	mercury__int_module27();
	mercury__int_module28();
	mercury__int_module29();
	mercury__int_module30();
	mercury__int_module31();
	mercury__int_module32();
	mercury__int_module33();
	mercury__int_module34();
	mercury__int_module35();
	mercury__int_module36();
	mercury__int_module37();
	mercury__int_module38();
	mercury__int_module39();
	mercury__int_module40();
}

static void mercury__int_bunch_1(void)
{
	mercury__int_module41();
	mercury__int_module42();
	mercury__int_module43();
	mercury__int_module44();
}

#endif

void mercury__int__init(void); /* suppress gcc warning */
void mercury__int__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__int_bunch_0();
	mercury__int_bunch_1();
#endif
}
