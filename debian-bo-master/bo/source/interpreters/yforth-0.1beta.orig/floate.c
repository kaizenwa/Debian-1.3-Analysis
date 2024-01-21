/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     floate.c
 * Abstract:        floating-extension word set
 */

#include <stdio.h>
#include <math.h>
#include "yforth.h"
#include "floate.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

static Cell precision = 15;

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _d_f_store() {
    register double *addr = (double *) *sp++;
    *addr = (double) *fp++;
}

void _d_f_fetch() {
    register double *addr = (double *) *sp++;
    *--fp = (Real) *addr;
}

void _d_float_plus() {
    sp[0] += sizeof(double);
}

void _d_floats() {
    sp[0] *= sizeof(double);
}

void _f_star_star() {
    fp[1] = pow(fp[1], fp[0]);
    fp++;
}

void _f_dot() {
    printf("%.*f ", precision, (double) *fp++);
}

void _f_abs() {
    *fp = fabs(*fp);
}

void _f_a_cos() {
    *fp = acos(*fp);
}

void _f_a_cosh() {
#ifdef HAVE_ACOSH
	*fp = acosh(*fp);
#else
	*fp = log(*fp + sqrt(*fp * *fp - 1));
#endif
}

void _f_a_log() {
    *fp = pow(10, *fp);
}

void _f_a_sin() {
    *fp = asin(*fp);
}

void _f_a_sinh() {
#ifdef HAVE_ASINH
	*fp = asinh(*fp);
#else
	*fp = log(*fp + sqrt(*fp * *fp + 1));
#endif
}

void _f_a_tan() {
    *fp = atan(*fp);
}

void _f_a_tan2() {
	fp[1] = atan2(fp[1], fp[0]);
    fp++;
}

void _f_a_tanh() {
#ifdef HAVE_ATANH
	*fp = atanh(*fp);
#else
	*fp = 0.5 * log((1 + *fp) / (1 - *fp));
#endif
}

void _f_cos() {
    *fp = cos(*fp);
}

void _f_cosh() {
    *fp = cosh(*fp);
}

void _f_e_dot() {
    register Real r = *fp++;
    register int esp = 0;
    if (r != 0.0)
        while (r < 1.0 || r > 1000.0) {
            if (r < 1.0) {
                r *= 1000.0;
                esp -= 3;
            } else {
                r /= 1000.0;
                esp += 3;
            }
        }
    printf("%.*fE%d ", precision, (double) r, esp);
}

void _f_exp() {
    *fp = exp(*fp);
}

void _f_exp_m_one() {
    *fp = exp(*fp) - 1.0;
}

void _f_ln() {
    *fp = log(*fp);
}

void _f_ln_p_one() {
    *fp = log(*fp) + 1.0;
}

void _f_log() {
    *fp = log10(*fp);
}

void _f_s_dot() {
    printf("%.*e ", precision, (double) *fp++);
}

void _f_sin() {
    *fp = sin(*fp);
}

void _f_sin_cos() {
    fp--;
    fp[0] = cos(fp[1]);
    fp[1] = sin(fp[1]);
}

void _f_sinh() {
    *fp = sinh(*fp);
}

void _f_sqrt() {
    *fp = sqrt(*fp);
}

void _f_tan() {
    *fp = tan(*fp);
}

void _f_tanh() {
    *fp = tanh(*fp);
}

void _f_proximate() {
    register Real r3 = *fp++;
    register Real r2 = *fp++;
    register Real r1 = *fp++;
    if (r3 > 0.0) *--sp = FFLAG(fabs(r1 - r2) < r3);
    else if (r3 < 0.0) *--sp = FFLAG(fabs(r1 - r2) < (-r3) * (fabs(r1) + fabs(r2)));
    else *--sp = FFLAG(r1 == r2);
}

void _precision() {
    *--sp = precision;
}

void _set_precision() {
    precision = *sp++;
}

void _s_f_store() {
    register float *addr = (float *) *sp++;
    *addr = (float) *fp++;
}

void _s_f_fetch() {
    register float *addr = (float *) *sp++;
	*--fp = (Real) *addr;
}

void _s_float_plus() {
    sp[0] += sizeof(float);
}

void _s_floats() {
    sp[0] *= sizeof(float);
}

