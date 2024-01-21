/*
** Automatically generated from `math.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__math__init
ENDINIT
*/

#include "imp.h"


	#include <math.h>

	/*
	** Mathematical constants.
	*/

	#define	MERCURY_FLOAT__E		2.7182818284590452354
	#define	MERCURY_FLOAT__PI		3.1415926535897932384
	#define	MERCURY_FLOAT__LN2		0.69314718055994530941

	void mercury_domain_error(const char *where);



Define_extern_entry(mercury__math__pi_1_0);
Define_extern_entry(mercury__math__e_1_0);
Define_extern_entry(mercury__math__ceiling_2_0);
Define_extern_entry(mercury__math__floor_2_0);
Define_extern_entry(mercury__math__round_2_0);
Define_extern_entry(mercury__math__truncate_2_0);
Define_extern_entry(mercury__math__sqrt_2_0);
Define_extern_entry(mercury__math__pow_3_0);
Define_extern_entry(mercury__math__exp_2_0);
Define_extern_entry(mercury__math__ln_2_0);
Define_extern_entry(mercury__math__log10_2_0);
Define_extern_entry(mercury__math__log2_2_0);
Define_extern_entry(mercury__math__log_3_0);
Define_extern_entry(mercury__math__sin_2_0);
Define_extern_entry(mercury__math__cos_2_0);
Define_extern_entry(mercury__math__tan_2_0);
Define_extern_entry(mercury__math__asin_2_0);
Define_extern_entry(mercury__math__acos_2_0);
Define_extern_entry(mercury__math__atan_2_0);
Define_extern_entry(mercury__math__atan2_3_0);
Define_extern_entry(mercury__math__sinh_2_0);
Define_extern_entry(mercury__math__cosh_2_0);
Define_extern_entry(mercury__math__tanh_2_0);


	/*
	** Handle domain errors.
	*/
	void
	mercury_domain_error(const char *where)
	{
		fflush(stdout);
		fprintf(stderr,
			"Software error: Domain error in call to `%s'
",
			where);
		exit(1);
	}



BEGIN_MODULE(mercury__math_module0)
	init_entry(mercury__math__pi_1_0);
BEGIN_CODE

/* code for predicate 'math__pi'/1 in mode 0 */
Define_entry(mercury__math__pi_1_0);
	{
		Float	Pi;
		Pi = MERCURY_FLOAT__PI;
		r1 = float_to_word(Pi);

	}
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module1)
	init_entry(mercury__math__e_1_0);
BEGIN_CODE

/* code for predicate 'math__e'/1 in mode 0 */
Define_entry(mercury__math__e_1_0);
	{
		Float	E;
		E = MERCURY_FLOAT__E;
		r1 = float_to_word(E);

	}
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module2)
	init_entry(mercury__math__ceiling_2_0);
BEGIN_CODE

/* code for predicate 'math__ceiling'/2 in mode 0 */
Define_entry(mercury__math__ceiling_2_0);
	{
		Float	Num;
		Float	Ceil;
		Num = word_to_float((Integer) r1);
		Ceil = ceil(Num);
		r2 = float_to_word(Ceil);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module3)
	init_entry(mercury__math__floor_2_0);
BEGIN_CODE

/* code for predicate 'math__floor'/2 in mode 0 */
Define_entry(mercury__math__floor_2_0);
	{
		Float	Num;
		Float	Floor;
		Num = word_to_float((Integer) r1);
		Floor = floor(Num);
		r2 = float_to_word(Floor);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module4)
	init_entry(mercury__math__round_2_0);
BEGIN_CODE

/* code for predicate 'math__round'/2 in mode 0 */
Define_entry(mercury__math__round_2_0);
	{
		Float	Num;
		Float	Rounded;
		Num = word_to_float((Integer) r1);
		
	Rounded = floor(Num+0.5);

		r2 = float_to_word(Rounded);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module5)
	init_entry(mercury__math__truncate_2_0);
BEGIN_CODE

/* code for predicate 'math__truncate'/2 in mode 0 */
Define_entry(mercury__math__truncate_2_0);
	{
		Float	X;
		Float	Trunc;
		X = word_to_float((Integer) r1);
		
	if (X < 0.0) {
	    Trunc = ceil(X);
	} else {
	    Trunc = floor(X);
	}

		r2 = float_to_word(Trunc);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module6)
	init_entry(mercury__math__sqrt_2_0);
BEGIN_CODE

/* code for predicate 'math__sqrt'/2 in mode 0 */
Define_entry(mercury__math__sqrt_2_0);
	{
		Float	X;
		Float	SquareRoot;
		X = word_to_float((Integer) r1);
		
	if (X < 0.0) {
	    mercury_domain_error("math__sqrt");
	}
	SquareRoot = sqrt(X);

		r2 = float_to_word(SquareRoot);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module7)
	init_entry(mercury__math__pow_3_0);
BEGIN_CODE

/* code for predicate 'math__pow'/3 in mode 0 */
Define_entry(mercury__math__pow_3_0);
	{
		Float	X;
		Float	Y;
		Float	Res;
		X = word_to_float((Integer) r1);
		Y = word_to_float((Integer) r2);
		
	if (X < 0.0) {
	    mercury_domain_error("math__pow");
	}
	if (X == 0.0) {
	    if (Y <= 0.0) {
		mercury_domain_error("math__pow");
	    }
	    Res = 0.0;
	} else {
	    Res = pow(X, Y);
	}

		r3 = float_to_word(Res);

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module8)
	init_entry(mercury__math__exp_2_0);
BEGIN_CODE

/* code for predicate 'math__exp'/2 in mode 0 */
Define_entry(mercury__math__exp_2_0);
	{
		Float	X;
		Float	Exp;
		X = word_to_float((Integer) r1);
		Exp = exp(X);
		r2 = float_to_word(Exp);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module9)
	init_entry(mercury__math__ln_2_0);
BEGIN_CODE

/* code for predicate 'math__ln'/2 in mode 0 */
Define_entry(mercury__math__ln_2_0);
	{
		Float	X;
		Float	Log;
		X = word_to_float((Integer) r1);
		
	if (X <= 0.0) {
	    mercury_domain_error("math__ln");
	}
	Log = log(X);

		r2 = float_to_word(Log);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module10)
	init_entry(mercury__math__log10_2_0);
BEGIN_CODE

/* code for predicate 'math__log10'/2 in mode 0 */
Define_entry(mercury__math__log10_2_0);
	{
		Float	X;
		Float	Log10;
		X = word_to_float((Integer) r1);
		
	if (X <= 0.0)
	    mercury_domain_error("math__log10");
	Log10 = log10(X);

		r2 = float_to_word(Log10);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module11)
	init_entry(mercury__math__log2_2_0);
BEGIN_CODE

/* code for predicate 'math__log2'/2 in mode 0 */
Define_entry(mercury__math__log2_2_0);
	{
		Float	X;
		Float	Log2;
		X = word_to_float((Integer) r1);
		
	if (X <= 0.0) {
	    mercury_domain_error("math__log2");
	}
	Log2 = log(X) / MERCURY_FLOAT__LN2;

		r2 = float_to_word(Log2);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module12)
	init_entry(mercury__math__log_3_0);
BEGIN_CODE

/* code for predicate 'math__log'/3 in mode 0 */
Define_entry(mercury__math__log_3_0);
	{
		Float	B;
		Float	X;
		Float	Log;
		B = word_to_float((Integer) r1);
		X = word_to_float((Integer) r2);
		
	if (X <= 0.0 || B <= 0.0) {
	    mercury_domain_error("math__log");
	}
	if (B == 1.0) {
	    mercury_domain_error("math__log");
	}
	Log = log(X)/log(B);

		r3 = float_to_word(Log);

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module13)
	init_entry(mercury__math__sin_2_0);
BEGIN_CODE

/* code for predicate 'math__sin'/2 in mode 0 */
Define_entry(mercury__math__sin_2_0);
	{
		Float	X;
		Float	Sin;
		X = word_to_float((Integer) r1);
		Sin = sin(X);
		r2 = float_to_word(Sin);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module14)
	init_entry(mercury__math__cos_2_0);
BEGIN_CODE

/* code for predicate 'math__cos'/2 in mode 0 */
Define_entry(mercury__math__cos_2_0);
	{
		Float	X;
		Float	Cos;
		X = word_to_float((Integer) r1);
		Cos = cos(X);
		r2 = float_to_word(Cos);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module15)
	init_entry(mercury__math__tan_2_0);
BEGIN_CODE

/* code for predicate 'math__tan'/2 in mode 0 */
Define_entry(mercury__math__tan_2_0);
	{
		Float	X;
		Float	Tan;
		X = word_to_float((Integer) r1);
		Tan = tan(X);
		r2 = float_to_word(Tan);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module16)
	init_entry(mercury__math__asin_2_0);
BEGIN_CODE

/* code for predicate 'math__asin'/2 in mode 0 */
Define_entry(mercury__math__asin_2_0);
	{
		Float	X;
		Float	ASin;
		X = word_to_float((Integer) r1);
		
	if (X < -1.0 || X > 1.0) {
	    mercury_domain_error("math__asin");
	}
	ASin = asin(X);

		r2 = float_to_word(ASin);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module17)
	init_entry(mercury__math__acos_2_0);
BEGIN_CODE

/* code for predicate 'math__acos'/2 in mode 0 */
Define_entry(mercury__math__acos_2_0);
	{
		Float	X;
		Float	ACos;
		X = word_to_float((Integer) r1);
		
	if (X < -1.0 || X > 1.0) {
	    mercury_domain_error("math__acos");
	}
	ACos = asin(X);

		r2 = float_to_word(ACos);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module18)
	init_entry(mercury__math__atan_2_0);
BEGIN_CODE

/* code for predicate 'math__atan'/2 in mode 0 */
Define_entry(mercury__math__atan_2_0);
	{
		Float	X;
		Float	ATan;
		X = word_to_float((Integer) r1);
		ATan = atan(X);
		r2 = float_to_word(ATan);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module19)
	init_entry(mercury__math__atan2_3_0);
BEGIN_CODE

/* code for predicate 'math__atan2'/3 in mode 0 */
Define_entry(mercury__math__atan2_3_0);
	{
		Float	Y;
		Float	X;
		Float	ATan2;
		Y = word_to_float((Integer) r1);
		X = word_to_float((Integer) r2);
		
	ATan2 = atan2(Y, X);

		r3 = float_to_word(ATan2);

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module20)
	init_entry(mercury__math__sinh_2_0);
BEGIN_CODE

/* code for predicate 'math__sinh'/2 in mode 0 */
Define_entry(mercury__math__sinh_2_0);
	{
		Float	X;
		Float	Sinh;
		X = word_to_float((Integer) r1);
		Sinh = sinh(X);
		r2 = float_to_word(Sinh);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module21)
	init_entry(mercury__math__cosh_2_0);
BEGIN_CODE

/* code for predicate 'math__cosh'/2 in mode 0 */
Define_entry(mercury__math__cosh_2_0);
	{
		Float	X;
		Float	Cosh;
		X = word_to_float((Integer) r1);
		Cosh = cosh(X);
		r2 = float_to_word(Cosh);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__math_module22)
	init_entry(mercury__math__tanh_2_0);
BEGIN_CODE

/* code for predicate 'math__tanh'/2 in mode 0 */
Define_entry(mercury__math__tanh_2_0);
	{
		Float	X;
		Float	Tanh;
		X = word_to_float((Integer) r1);
		Tanh = tanh(X);
		r2 = float_to_word(Tanh);

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__math_bunch_0(void)
{
	mercury__math_module0();
	mercury__math_module1();
	mercury__math_module2();
	mercury__math_module3();
	mercury__math_module4();
	mercury__math_module5();
	mercury__math_module6();
	mercury__math_module7();
	mercury__math_module8();
	mercury__math_module9();
	mercury__math_module10();
	mercury__math_module11();
	mercury__math_module12();
	mercury__math_module13();
	mercury__math_module14();
	mercury__math_module15();
	mercury__math_module16();
	mercury__math_module17();
	mercury__math_module18();
	mercury__math_module19();
	mercury__math_module20();
	mercury__math_module21();
	mercury__math_module22();
}

#endif

void mercury__math__init(void); /* suppress gcc warning */
void mercury__math__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__math_bunch_0();
#endif
}
