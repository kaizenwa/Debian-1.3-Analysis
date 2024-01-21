/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* Coefficients for long double precision by Stephen L. Moshier
   (moshier@world.std.com).  This routine was spot chaecked at
   10,000 random arguments between -40 and +40.  The largest error
   observed was 2.2e-19.  Error criterion was absolute for function
   value < 1, relative otherwise.  */

#if 0
#ifndef lint
static char sccsid[] = "@(#)lgamma.c	5.3 (Berkeley) 9/22/88";
#endif /* not lint */
#endif

/*
	C program for floating point log Gamma function

	lgamma(x) computes the log of the absolute
	value of the Gamma function.
	The sign of the Gamma function is returned in the
	external quantity signgaml.

	The coefficients for expansion around zero
	are #5243 from Hart & Cheney; for expansion
	around infinity they are #5404.

	Calls log, floor and sin.
*/
long double floorl(long double);
long double logl(long double);
long double sinl(long double);

#if 0
#include "mathimpl.h"
#else
#include <math.h>
#endif
#if defined(vax)||defined(tahoe)
#include <errno.h>
#endif	/* defined(vax)||defined(tahoe) */

int signgaml;

  /* log(2*pi)/2 */
static const long double goobie = 0.9189385332046727417803297L;
static const long double pi   = 3.1415926535897932384626434L;

/*
log gamma(x) = ( x - 0.5 ) * log(x) - x + LS2PI + 1/x P(1/x^2)
Relative error
n=6, d=0
Peak error =  1.51e-21
Relative error spread =  5.7e-21
*/
#define M 7
static const long double p1[] = {
 8.33333333333333144750e-2L,
-2.77777777775034960344e-3L,
 7.93650779585507075567e-4L,
-5.95234585176568851461e-4L,
 8.41272329732249808063e-4L,
-1.88080193811937690718e-3L,
 4.88502614243227078116e-3L
};
/*
gamma(x+2) = P(x)/Q(x)
Relative error
n=7, d=8
Peak error =  1.84e-20
Relative error spread =  8.4e-23
*/
#define N 9
static const long double p2[] = {
-7.15743521530849602425e4L,
-5.99650230220855581642e4L,
-2.59780216007146401957e4L,
-7.96667499622741999770e3L,
-1.70730828800510297666e3L,
-2.92929976820724030353e2L,
-3.25157411956062339893e1L,
-3.01525602666895735709e0L,
 0.0L
};
static const long double q2[] = {
-7.15743521530849602412e4L,
-2.97045081369399940529e4L,
 1.60577839621734713377e4L,
 3.31667508019495079814e3L,
-1.98526250512761318471e3L,
 5.69440799097468430177e1L,
 8.85946791747759881659e1L,
-1.67955233807178858919e1L,
 1.00000000000000000000e0L,
};

static long double pos(long double);
static long double neg(long double);
static long double asym(long double);

long double
lgammal(long double arg)
{

	signgaml = 1;
	if(arg <= 0.L) return(neg(arg));
	if(arg > 8.L) return(asym(arg));
	return(logl(pos(arg)));
}

static long double
asym(long double arg)
{
	long double n, argsq;
	int i;

	n = 0.0L;

	/* Avoid overflow of arg*arg. */
	if(arg > 1.0e10L)
	  goto noexpan;

	argsq = 1./(arg*arg);
	for(i=M-1; i>=0; i--){
		n = n*argsq + p1[i];
	}
noexpan:
	return((arg-.5L)*logl(arg) - arg + goobie + n/arg);
}

static long double
neg(long double arg)
{
	long double t;

	arg = -arg;
     /*
      * to see if arg were a true integer, the old code used the
      * mathematically correct observation:
      * sin(n*pi) = 0 <=> n is an integer.
      * but in finite precision arithmetic, sin(n*PI) will NEVER
      * be zero simply because n*PI is a rational number.  hence
      *	it failed to work with our newer, more accurate sin()
      * which uses true pi to do the argument reduction...
      *	temp = sin(pi*arg);
      */
	t = floorl(arg);
	if (arg - t  > 0.5e0L)
	    t += 1.e0L;				/* t := integer nearest arg */
#if defined(vax)||defined(tahoe)
	if (arg == t) {
	    return(__infnan(ERANGE));		/* +INF */
	}
#endif	/* defined(vax)||defined(tahoe) */
	signgaml = (int) (t - 2*floorl(t/2));	/* signgam =  1 if t was odd, */
						/*            0 if t was even */
	signgaml = signgaml - 1 + signgaml;	/* signgam =  1 if t was odd, */
						/*           -1 if t was even */
	t = arg - t;				/*  -0.5 <= t <= 0.5 */
	if (t < 0.e0L) {
	    t = -t;
	    signgaml = -signgaml;
	}
	return(-logl(arg*pos(arg)*sinl(pi*t)/pi));
}

static long double
pos(long double arg)
{
	long double n, d, s;
	register i;

	if(arg < 2.L) return(pos(arg+1.L)/arg);
	if(arg > 3.L) return((arg-1.0L)*pos(arg-1.0L));

	s = arg - 2.0L;
/*	for(n=0,d=0,i=N-1; i>=0; i--){*/
/*
	n = 0.0L;
	d = 0.0L;
*/
	for(n=0,d=0,i=N-1; i>=0; i--){
		n = n*s + p2[i];
		d = d*s + q2[i];
	}
	return(n/d);
}
