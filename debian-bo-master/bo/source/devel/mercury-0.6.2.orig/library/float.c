/*
** Automatically generated from `float.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__float__init
ENDINIT
*/

#include "imp.h"


	#include <float.h>
	#include <math.h>




	#if defined USE_SINGLE_PREC_FLOAT
		#define	MERCURY_FLOAT_MAX	FLT_MAX
		#define	MERCURY_FLOAT_MIN	FLT_MIN
		#define	MERCURY_FLOAT_EPSILON	FLT_EPSILON
	#else
		#define	MERCURY_FLOAT_MAX	DBL_MAX
		#define	MERCURY_FLOAT_MIN	DBL_MIN
		#define	MERCURY_FLOAT_EPSILON	DBL_EPSILON
	#endif



Define_extern_entry(mercury__float__f_less_than_2_0);
Declare_label(mercury__float__f_less_than_2_0_i1);
Define_extern_entry(mercury__float__f_greater_than_2_0);
Declare_label(mercury__float__f_greater_than_2_0_i1);
Define_extern_entry(mercury__float__f_less_or_equal_2_0);
Declare_label(mercury__float__f_less_or_equal_2_0_i1);
Define_extern_entry(mercury__float__f_greater_or_equal_2_0);
Declare_label(mercury__float__f_greater_or_equal_2_0_i1);
Define_extern_entry(mercury__float__abs_2_0);
Declare_label(mercury__float__abs_2_0_i4);
Define_extern_entry(mercury__float__max_3_0);
Declare_label(mercury__float__max_3_0_i4);
Define_extern_entry(mercury__float__min_3_0);
Declare_label(mercury__float__min_3_0_i4);
Define_extern_entry(mercury__fn__float__f_plus_2_0);
Define_extern_entry(mercury__fn__float__f_plus_2_1);
Define_extern_entry(mercury__fn__float__f_plus_2_2);
Define_extern_entry(mercury__fn__float__f_minus_2_0);
Define_extern_entry(mercury__fn__float__f_minus_2_1);
Define_extern_entry(mercury__fn__float__f_minus_2_2);
Define_extern_entry(mercury__fn__float__f_times_2_0);
Define_extern_entry(mercury__fn__float__f_times_2_1);
Define_extern_entry(mercury__fn__float__f_times_2_2);
Define_extern_entry(mercury__fn__float__f_slash_2_0);
Define_extern_entry(mercury__fn__float__f_slash_2_1);
Define_extern_entry(mercury__fn__float__f_slash_2_2);
Define_extern_entry(mercury__fn__float__f_plus_1_0);
Define_extern_entry(mercury__fn__float__f_minus_1_0);
Define_extern_entry(mercury__float__builtin_float_plus_3_0);
Define_extern_entry(mercury__float__builtin_float_minus_3_0);
Define_extern_entry(mercury__float__builtin_float_times_3_0);
Define_extern_entry(mercury__float__builtin_float_divide_3_0);
Define_extern_entry(mercury__float__builtin_float_gt_2_0);
Declare_label(mercury__float__builtin_float_gt_2_0_i1);
Define_extern_entry(mercury__float__builtin_float_lt_2_0);
Declare_label(mercury__float__builtin_float_lt_2_0_i1);
Define_extern_entry(mercury__float__builtin_float_ge_2_0);
Declare_label(mercury__float__builtin_float_ge_2_0_i1);
Define_extern_entry(mercury__float__builtin_float_le_2_0);
Declare_label(mercury__float__builtin_float_le_2_0_i1);
Define_extern_entry(mercury__float__ceiling_to_int_2_0);
Define_extern_entry(mercury__float__floor_to_int_2_0);
Define_extern_entry(mercury__float__round_to_int_2_0);
Define_extern_entry(mercury__float__truncate_to_int_2_0);
Define_extern_entry(mercury__float__pow_3_0);
Declare_label(mercury__float__pow_3_0_i1001);
Declare_label(mercury__float__pow_3_0_i8);
Declare_label(mercury__float__pow_3_0_i11);
Declare_label(mercury__float__pow_3_0_i14);
Define_extern_entry(mercury__float__max_1_0);
Define_extern_entry(mercury__float__min_1_0);
Define_extern_entry(mercury__float__epsilon_1_0);

BEGIN_MODULE(mercury__float_module0)
	init_entry(mercury__float__f_less_than_2_0);
	init_label(mercury__float__f_less_than_2_0_i1);
BEGIN_CODE

/* code for predicate '<'/2 in mode 0 */
Define_entry(mercury__float__f_less_than_2_0);
	if ((word_to_float((Integer) r1) >= word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__f_less_than_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__f_less_than_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module1)
	init_entry(mercury__float__f_greater_than_2_0);
	init_label(mercury__float__f_greater_than_2_0_i1);
BEGIN_CODE

/* code for predicate '>'/2 in mode 0 */
Define_entry(mercury__float__f_greater_than_2_0);
	if ((word_to_float((Integer) r1) <= word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__f_greater_than_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__f_greater_than_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module2)
	init_entry(mercury__float__f_less_or_equal_2_0);
	init_label(mercury__float__f_less_or_equal_2_0_i1);
BEGIN_CODE

/* code for predicate '=<'/2 in mode 0 */
Define_entry(mercury__float__f_less_or_equal_2_0);
	if ((word_to_float((Integer) r1) > word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__f_less_or_equal_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__f_less_or_equal_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module3)
	init_entry(mercury__float__f_greater_or_equal_2_0);
	init_label(mercury__float__f_greater_or_equal_2_0_i1);
BEGIN_CODE

/* code for predicate '>='/2 in mode 0 */
Define_entry(mercury__float__f_greater_or_equal_2_0);
	if ((word_to_float((Integer) r1) < word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__f_greater_or_equal_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__f_greater_or_equal_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module4)
	init_entry(mercury__float__abs_2_0);
	init_label(mercury__float__abs_2_0_i4);
BEGIN_CODE

/* code for predicate 'float__abs'/2 in mode 0 */
Define_entry(mercury__float__abs_2_0);
	{
	static const Float mercury_float_const_0 = 0;
	if ((word_to_float((Integer) r1) > ((Float) 0.00000000000000)))
		GOTO_LABEL(mercury__float__abs_2_0_i4);
	}
	{
	static const Float mercury_float_const_0 = 0;
	r1 = float_to_word(((Float) 0.00000000000000) - word_to_float((Integer) r1));
	}
Define_label(mercury__float__abs_2_0_i4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module5)
	init_entry(mercury__float__max_3_0);
	init_label(mercury__float__max_3_0_i4);
BEGIN_CODE

/* code for predicate 'float__max'/3 in mode 0 */
Define_entry(mercury__float__max_3_0);
	if ((word_to_float((Integer) r1) >= word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__max_3_0_i4);
	r1 = (Integer) r2;
Define_label(mercury__float__max_3_0_i4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module6)
	init_entry(mercury__float__min_3_0);
	init_label(mercury__float__min_3_0_i4);
BEGIN_CODE

/* code for predicate 'float__min'/3 in mode 0 */
Define_entry(mercury__float__min_3_0);
	if ((word_to_float((Integer) r1) <= word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__min_3_0_i4);
	r1 = (Integer) r2;
Define_label(mercury__float__min_3_0_i4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module7)
	init_entry(mercury__fn__float__f_plus_2_0);
BEGIN_CODE

/* code for predicate '+'/3 in mode 0 */
Define_entry(mercury__fn__float__f_plus_2_0);
	r1 = float_to_word(word_to_float((Integer) r1) + word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module8)
	init_entry(mercury__fn__float__f_plus_2_1);
BEGIN_CODE

/* code for predicate '+'/3 in mode 1 */
Define_entry(mercury__fn__float__f_plus_2_1);
	r1 = float_to_word(word_to_float((Integer) r2) - word_to_float((Integer) r1));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module9)
	init_entry(mercury__fn__float__f_plus_2_2);
BEGIN_CODE

/* code for predicate '+'/3 in mode 2 */
Define_entry(mercury__fn__float__f_plus_2_2);
	r1 = float_to_word(word_to_float((Integer) r2) - word_to_float((Integer) r1));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module10)
	init_entry(mercury__fn__float__f_minus_2_0);
BEGIN_CODE

/* code for predicate '-'/3 in mode 0 */
Define_entry(mercury__fn__float__f_minus_2_0);
	r1 = float_to_word(word_to_float((Integer) r1) - word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module11)
	init_entry(mercury__fn__float__f_minus_2_1);
BEGIN_CODE

/* code for predicate '-'/3 in mode 1 */
Define_entry(mercury__fn__float__f_minus_2_1);
	r1 = float_to_word(word_to_float((Integer) r1) + word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module12)
	init_entry(mercury__fn__float__f_minus_2_2);
BEGIN_CODE

/* code for predicate '-'/3 in mode 2 */
Define_entry(mercury__fn__float__f_minus_2_2);
	r1 = float_to_word(word_to_float((Integer) r1) - word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module13)
	init_entry(mercury__fn__float__f_times_2_0);
BEGIN_CODE

/* code for predicate '*'/3 in mode 0 */
Define_entry(mercury__fn__float__f_times_2_0);
	r1 = float_to_word(word_to_float((Integer) r1) * word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module14)
	init_entry(mercury__fn__float__f_times_2_1);
BEGIN_CODE

/* code for predicate '*'/3 in mode 1 */
Define_entry(mercury__fn__float__f_times_2_1);
	r1 = float_to_word(word_to_float((Integer) r2) / word_to_float((Integer) r1));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module15)
	init_entry(mercury__fn__float__f_times_2_2);
BEGIN_CODE

/* code for predicate '*'/3 in mode 2 */
Define_entry(mercury__fn__float__f_times_2_2);
	r1 = float_to_word(word_to_float((Integer) r2) / word_to_float((Integer) r1));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module16)
	init_entry(mercury__fn__float__f_slash_2_0);
BEGIN_CODE

/* code for predicate '/'/3 in mode 0 */
Define_entry(mercury__fn__float__f_slash_2_0);
	r1 = float_to_word(word_to_float((Integer) r1) / word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module17)
	init_entry(mercury__fn__float__f_slash_2_1);
BEGIN_CODE

/* code for predicate '/'/3 in mode 1 */
Define_entry(mercury__fn__float__f_slash_2_1);
	r1 = float_to_word(word_to_float((Integer) r1) * word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module18)
	init_entry(mercury__fn__float__f_slash_2_2);
BEGIN_CODE

/* code for predicate '/'/3 in mode 2 */
Define_entry(mercury__fn__float__f_slash_2_2);
	r1 = float_to_word(word_to_float((Integer) r1) / word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module19)
	init_entry(mercury__fn__float__f_plus_1_0);
BEGIN_CODE

/* code for predicate '+'/2 in mode 0 */
Define_entry(mercury__fn__float__f_plus_1_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module20)
	init_entry(mercury__fn__float__f_minus_1_0);
BEGIN_CODE

/* code for predicate '-'/2 in mode 0 */
Define_entry(mercury__fn__float__f_minus_1_0);
	{
	static const Float mercury_float_const_0 = 0;
	r1 = float_to_word(((Float) 0.00000000000000) - word_to_float((Integer) r1));
	}
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module21)
	init_entry(mercury__float__builtin_float_plus_3_0);
BEGIN_CODE

/* code for predicate 'builtin_float_plus'/3 in mode 0 */
Define_entry(mercury__float__builtin_float_plus_3_0);
	r1 = float_to_word(word_to_float((Integer) r1) + word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module22)
	init_entry(mercury__float__builtin_float_minus_3_0);
BEGIN_CODE

/* code for predicate 'builtin_float_minus'/3 in mode 0 */
Define_entry(mercury__float__builtin_float_minus_3_0);
	r1 = float_to_word(word_to_float((Integer) r1) - word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module23)
	init_entry(mercury__float__builtin_float_times_3_0);
BEGIN_CODE

/* code for predicate 'builtin_float_times'/3 in mode 0 */
Define_entry(mercury__float__builtin_float_times_3_0);
	r1 = float_to_word(word_to_float((Integer) r1) * word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module24)
	init_entry(mercury__float__builtin_float_divide_3_0);
BEGIN_CODE

/* code for predicate 'builtin_float_divide'/3 in mode 0 */
Define_entry(mercury__float__builtin_float_divide_3_0);
	r1 = float_to_word(word_to_float((Integer) r1) / word_to_float((Integer) r2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module25)
	init_entry(mercury__float__builtin_float_gt_2_0);
	init_label(mercury__float__builtin_float_gt_2_0_i1);
BEGIN_CODE

/* code for predicate 'builtin_float_gt'/2 in mode 0 */
Define_entry(mercury__float__builtin_float_gt_2_0);
	if ((word_to_float((Integer) r1) <= word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__builtin_float_gt_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__builtin_float_gt_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module26)
	init_entry(mercury__float__builtin_float_lt_2_0);
	init_label(mercury__float__builtin_float_lt_2_0_i1);
BEGIN_CODE

/* code for predicate 'builtin_float_lt'/2 in mode 0 */
Define_entry(mercury__float__builtin_float_lt_2_0);
	if ((word_to_float((Integer) r1) >= word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__builtin_float_lt_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__builtin_float_lt_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module27)
	init_entry(mercury__float__builtin_float_ge_2_0);
	init_label(mercury__float__builtin_float_ge_2_0_i1);
BEGIN_CODE

/* code for predicate 'builtin_float_ge'/2 in mode 0 */
Define_entry(mercury__float__builtin_float_ge_2_0);
	if ((word_to_float((Integer) r1) < word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__builtin_float_ge_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__builtin_float_ge_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module28)
	init_entry(mercury__float__builtin_float_le_2_0);
	init_label(mercury__float__builtin_float_le_2_0_i1);
BEGIN_CODE

/* code for predicate 'builtin_float_le'/2 in mode 0 */
Define_entry(mercury__float__builtin_float_le_2_0);
	if ((word_to_float((Integer) r1) > word_to_float((Integer) r2)))
		GOTO_LABEL(mercury__float__builtin_float_le_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__float__builtin_float_le_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module29)
	init_entry(mercury__float__ceiling_to_int_2_0);
BEGIN_CODE

/* code for predicate 'float__ceiling_to_int'/2 in mode 0 */
Define_entry(mercury__float__ceiling_to_int_2_0);
	{
		Float	X;
		Integer	Ceil;
		X = word_to_float((Integer) r1);
		
	Ceil = (Integer)ceil(X);

		r2 = Ceil;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module30)
	init_entry(mercury__float__floor_to_int_2_0);
BEGIN_CODE

/* code for predicate 'float__floor_to_int'/2 in mode 0 */
Define_entry(mercury__float__floor_to_int_2_0);
	{
		Float	X;
		Integer	Floor;
		X = word_to_float((Integer) r1);
		
	Floor = (Integer)floor(X);

		r2 = Floor;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module31)
	init_entry(mercury__float__round_to_int_2_0);
BEGIN_CODE

/* code for predicate 'float__round_to_int'/2 in mode 0 */
Define_entry(mercury__float__round_to_int_2_0);
	{
		Float	X;
		Integer	Round;
		X = word_to_float((Integer) r1);
		
	Round = (Integer)floor(X+0.5);

		r2 = Round;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module32)
	init_entry(mercury__float__truncate_to_int_2_0);
BEGIN_CODE

/* code for predicate 'float__truncate_to_int'/2 in mode 0 */
Define_entry(mercury__float__truncate_to_int_2_0);
	{
		Float	X;
		Integer	Trunc;
		X = word_to_float((Integer) r1);
		
	Trunc = (Integer)X;

		r2 = Trunc;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module33)
	init_entry(mercury__float__pow_3_0);
	init_label(mercury__float__pow_3_0_i1001);
	init_label(mercury__float__pow_3_0_i8);
	init_label(mercury__float__pow_3_0_i11);
	init_label(mercury__float__pow_3_0_i14);
BEGIN_CODE

/* code for predicate 'float__pow'/3 in mode 0 */
Define_entry(mercury__float__pow_3_0);
	if (((Integer) r2 >= ((Integer) 0)))
		GOTO_LABEL(mercury__float__pow_3_0_i1001);
	r1 = string_const("float__pow taken with exponent < 0\n", 35);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__float__pow_3_0));
	}
Define_label(mercury__float__pow_3_0_i1001);
	incr_sp_push_msg(2, "float__pow");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r2 == ((Integer) 1)))
		GOTO_LABEL(mercury__float__pow_3_0_i14);
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__float__pow_3_0_i8);
	{
	static const Float mercury_float_const_1 = 1;
	r1 = (Word)(&mercury_float_const_1);
	}
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__float__pow_3_0_i8);
	detstackvar(1) = (Integer) r1;
	r2 = ((Integer) r2 - ((Integer) 1));
	localcall(mercury__float__pow_3_0,
		LABEL(mercury__float__pow_3_0_i11),
		ENTRY(mercury__float__pow_3_0));
Define_label(mercury__float__pow_3_0_i11);
	update_prof_current_proc(LABEL(mercury__float__pow_3_0));
	r1 = float_to_word(word_to_float((Integer) detstackvar(1)) * word_to_float((Integer) r1));
Define_label(mercury__float__pow_3_0_i14);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module34)
	init_entry(mercury__float__max_1_0);
BEGIN_CODE

/* code for predicate 'float__max'/1 in mode 0 */
Define_entry(mercury__float__max_1_0);
	{
		Float	Max;
		Max = MERCURY_FLOAT_MAX;
		r1 = float_to_word(Max);

	}
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module35)
	init_entry(mercury__float__min_1_0);
BEGIN_CODE

/* code for predicate 'float__min'/1 in mode 0 */
Define_entry(mercury__float__min_1_0);
	{
		Float	Min;
		Min = MERCURY_FLOAT_MIN;
		r1 = float_to_word(Min);

	}
	proceed();
END_MODULE

BEGIN_MODULE(mercury__float_module36)
	init_entry(mercury__float__epsilon_1_0);
BEGIN_CODE

/* code for predicate 'float__epsilon'/1 in mode 0 */
Define_entry(mercury__float__epsilon_1_0);
	{
		Float	Eps;
		Eps = MERCURY_FLOAT_EPSILON;
		r1 = float_to_word(Eps);

	}
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__float_bunch_0(void)
{
	mercury__float_module0();
	mercury__float_module1();
	mercury__float_module2();
	mercury__float_module3();
	mercury__float_module4();
	mercury__float_module5();
	mercury__float_module6();
	mercury__float_module7();
	mercury__float_module8();
	mercury__float_module9();
	mercury__float_module10();
	mercury__float_module11();
	mercury__float_module12();
	mercury__float_module13();
	mercury__float_module14();
	mercury__float_module15();
	mercury__float_module16();
	mercury__float_module17();
	mercury__float_module18();
	mercury__float_module19();
	mercury__float_module20();
	mercury__float_module21();
	mercury__float_module22();
	mercury__float_module23();
	mercury__float_module24();
	mercury__float_module25();
	mercury__float_module26();
	mercury__float_module27();
	mercury__float_module28();
	mercury__float_module29();
	mercury__float_module30();
	mercury__float_module31();
	mercury__float_module32();
	mercury__float_module33();
	mercury__float_module34();
	mercury__float_module35();
	mercury__float_module36();
}

#endif

void mercury__float__init(void); /* suppress gcc warning */
void mercury__float__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__float_bunch_0();
#endif
}
