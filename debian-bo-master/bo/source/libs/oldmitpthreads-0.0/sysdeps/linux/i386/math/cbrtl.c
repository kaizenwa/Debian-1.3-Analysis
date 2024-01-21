/* @(#)s_cbrt.c 5.1 93/09/24 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 * Prototype changed for linux libc by flebbe@tat.physik.uni-tuebingen.de
 * 15.3.94
 * Pointer aliasing bug fixed by Stephen Moshier (moshier@world.std.com)
 * 29.9.94
 * Additional change by Olaf Flebbe.
*/

#include <ansidecl.h>
#include <math.h>

/* cbrtl(x)
 * Return cube root of x
 */
#ifdef __STDC__
static const unsigned 
#else
static unsigned 
#endif
	B1 = 715094163, /* B1 = (682-0.03306235651)*2**20 */
	B2 = 696219795; /* B2 = (664-0.03306235651)*2**20 */

#ifdef __STDC__
static const double 
#else
static double
#endif
C =  5.42857142857142815906e-01, /* 19/35     = 0x3FE15F15, 0xF15F15F1 */
D = -7.05306122448979611050e-01, /* -864/1225 = 0xBFE691DE, 0x2532C834 */
E =  1.41428571428571436819e+00, /* 99/70     = 0x3FF6A0EA, 0x0EA0EA0F */
F =  1.60714285714285720630e+00, /* 45/28     = 0x3FF9B6DB, 0x6DB6DB6E */
G =  3.57142857142857150787e-01; /* 5/14      = 0x3FD6DB6D, 0xB6DB6DB7 */

/* Define n0 1 for little endian */
/* Define n0 0 for big endian machines */
#define n0 1

long double DEFUN( cbrtl, (x), long double x) 
{
	int	hx;
	double r,s,w;
	long double lt;
	unsigned sign;
	union {
           double t;
           unsigned pt[2];
        } ut, ux;

	ut.t = 0.0;
        ux.t = x;

	hx = ux.pt[n0];                 /* high word of x */
	sign=hx&0x80000000; 		/* sign= sign(x) */
	hx  ^=sign;
	if(hx>=0x7ff00000) return(x+x); /* cbrt(NaN,INF) is itself */
	if((hx| ux.pt[1-n0])==0) 
	    return(ux.t);	                /* cbrt(0) is itself */

        ux.pt[n0] = hx;
    /* rough cbrt to 5 bits */
	if(hx<0x00100000) 		/* subnormal number */
	  {ut.pt[n0]=0x43500000; 		/* set t= 2**54 */
	   ut.t*=x; ut.pt[n0]=ut.pt[n0]/3+B2;
	  }
	else
	  ut.pt[n0]=hx/3+B1;


    /* new cbrt to 23 bits, may be implemented in single precision */
	r=ut.t*ut.t/ux.t;
	s=C+r*ut.t;
	ut.t*=G+F/(s+E+D/s);

    /* chopped to 20 bits and make it larger than cbrt(x) */ 
	ut.pt[1-n0]=0; ut.pt[n0]+=0x00000001;


    /* one step newton iteration to 53 bits with error less than 0.667 ulps */
	s=ut.t*ut.t;		/* t*t is exact */
	r=ux.t/s;
	w=ut.t+ut.t;
	r=(r-ut.t)/(w+r);	/* r-s is exact */
	ut.t=ut.t+ut.t*r;

	
    /* restore the sign bit */
	ut.pt[n0] |= sign;

	lt = ut.t;
	lt -= (lt - (x/(lt*lt))) * 0.333333333333333333333L;
	return lt;
}
