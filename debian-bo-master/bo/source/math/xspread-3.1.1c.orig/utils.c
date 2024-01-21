#include <config.h>
#include <curses.h>
#include <math.h>
#include "sc.h"

#ifdef HAVE_X11_X_H
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "scXstuff.h"
#endif /* HAVE_X11_X_H */

static double	cash_flow PROTO((double, double *, int, int, int, int));
static double	dorlirr PROTO((int, int, int, int));
static void	populate_power PROTO((double, double *, int));
static void	set_step PROTO((int, int, int, int, int *, int *));
static int	valid_nrange PROTO((int, int, int, int, int));

extern int	cellerror;	/* interp.c: is there an error in this cell */

#ifndef HAVE_RINT
/**     round-to-even, also known as ``banker's rounding''.
	With round-to-even, a number exactly halfway between two values is
	rounded to whichever is even; e.g. rnd(0.5)=0, rnd(1.5)=2,
	rnd(2.5)=2, rnd(3.5)=4.  This is the default rounding mode for
	IEEE floating point, for good reason: it has better numeric
	properties.  For example, if X+Y is an integer,
	then X+Y = rnd(X)+rnd(Y) with round-to-even,
	but not always with sc's rounding (which is
	round-to-positive-infinity).  I ran into this problem when trying to
	split interest in an account to two people fairly.
**/
double 
rint(d)
double d;
{
	/* as sent */
	double fl = floor(d),  fr = d - fl;

	return ((fr < 0.5) || ((fr == 0.5) && (fl == floor(fl/2) * 2)))
	        ? fl : ceil(d);
}
#endif


/*
 * pow10 calculates the power function of base 10
 */
#ifndef HAVE_POW10
double 
pow10(p)
   double p;
{
   double q;

   p = floor(p);
   if (p >= 0) {
      for (q = 1; p > 0; --p)
         q = q * 10;
   }
   else {
      p = -p;
      for (q = 1; p > 0; --p)
         q = q / 10;
   }
   return q;
}
#endif /* linux */

/***
 function scerror() used to be a macro, but the X call is more complicated than 
 the curses version was 
***/
void
scerror(errstring)
char *errstring;
{

#ifdef PSC
   fprintf(stderr, errstring);
#else
# ifdef HAVE_X11_X_H
  if (using_X)
  {
	clearlines(1, 1);
	XBell(dpy, 100);
	/*fprintf(stderr,"\007");*/
	XDrawImageString(dpy, mainwin, maingc,
		   textcol(0), textrow(1),
		   errstring, strlen(errstring));
	XFlush(dpy);
	seenerr = 1;
  } else
# endif /* HAVE_X11_X_H */
  {	move(1,0); clrtoeol();  printw("%s", errstring);
  } /* HAVE_X11_X_H, end curses */
#endif
}


/* =============================================== */
/* This function displays the input data or message*/
/* at the first line of the spread sheet screen    */
/* for approximately one second.                   */
/* =============================================== */

void
message(x)  
char *x;        /* string for display */
{
#ifdef PSC
   fprintf(stderr, x);
#else
# ifdef HAVE_X11_X_H
   if (using_X)
   {
	clearlines(0,0);
	XDrawImageString(dpy, mainwin, maingc,
		    textcol(0), textrow(0),
		    x, strlen(x));
	XFlush(dpy);      /* force it onto the screen */
	sleep(1);         /* delay 1 second */
	clearlines(0,0);
   } else
# endif /* HAVE_X11_X_H */
   {	move(1,0); clrtoeol();  printw("%s", x);
   } /* HAVE_X11_X_H, end curses */
#endif
} /* end of message */


#ifndef PSC
/*
 * DOIRR -- calculates the "internal-rate-of-return" for a cash flow
 * over equal length intervals, designated in the range.  I.e.  
 * the rate-of-return which would yield a cash flow with these properties,
 * common in bond-returns.  E.g. -103.4 9.13 9.13 109.13  might be
 * the cell contents for a bond which costs 103.4, returns 9 1/8% on
 * par value 100, and then returns the principal and last payment 
 * all at once.
 *
 * Formula:
 *	based on solving for "r" in:
 *		  t
 *		Sum	CF(j) / (1+r)^j
 * 		  j = 0
 */
double
doirr(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
	static double 	ret = 0;
/*	int	cinc = 0, rinc = 0;*/
	double	dorlirr();

	/* first check validity of all entries,
	 * as well as validity of the range
	 */
	if (minr != maxr && minc != maxc) {
		/* range is wrong shape, the formula needs changing */
		scerror(" range specified to @doirr");
		cellerror = CELLERROR;
		return ret;
	}
	/* must be at least 2 numbers in the cash flow. */
	if (!valid_nrange(minr, minc, maxr, maxc, 2))
	{
		/* formula may be fine, range values may be changed */
		cellerror = CELLINVALID;
		return ret;
	}

	/* now run the irr approximation */
	return dorlirr(minr, minc, maxr, maxc);
}


/* calculate @irr() over range, approximating the rate, using exponential
 * doubling to find an upper bound for a guess, then narrowing in using
 * binary search.
 */
static double
dorlirr(minr, minc, maxr, maxc)
int	minr, minc, maxr, maxc;
{
	double 	guess = 1.0;
	double	err = 0.0001;
	double	rate = guess;
	double	*rate_pow;
	int	growing = 1;		/* finding upper bound for rate */
	int	npow;
	double	fabs();
	double	oval = -123456.0;	/* arbitrary number */
	double	val, cash_flow();

	/* rate power table */
	npow = (maxr-minr+1) *(maxc-minc+1) + 1;
	rate_pow = (double *)scxmalloc(sizeof(double)*npow);
	/* repeatedly guess */
	for (;;)
	{
		populate_power(rate, rate_pow, npow);
		val = cash_flow(rate, rate_pow, minr, minc, maxr, maxc);
		if (fabs(val) <= err)
			break;
		if (fabs(val - oval) <= err || fabs(rate) <= err) {
			/* not converging, no rate */
			cellerror = CELLERROR;
			break;
		}
		oval = val;
		/* adjust guess */
		if (val > 0.0 && growing) {
			/* still trying to find an upper bound */
			rate += guess;
			guess *= 2;
		}
		else  {
			/* bin search */
			guess *= 0.5;
			if (val > 0.0) {
				/* grow rate */
				rate += guess;
			}
			else {
				/* diminish rate */
				growing = 0;
				rate -= guess;
			}
		}
	}

	scxfree((char *)rate_pow);
	return rate;
}

/*
 * valid numeric range?
 * emptyok:  -1 -- all range must be numeric
 *            # -- skip non-numeric or missing, must be at least #
 *		  numerics present
 * any cellerror values will cause invalid range.
 */
static int
valid_nrange(minr, minc, maxr, maxc, emptyok)
int	minr, minc, maxr, maxc;
int	emptyok;			/* accept empty or non-n cells */
{
	register struct ent	*p;
	int	cinc, rinc;
	int	numerics = 0;

	set_step(minr, minc, maxr, maxc, &rinc, &cinc);
	for (; minr <= maxr && minc <= maxc; minr += rinc, minc += cinc) {
		p = *ATBL(tbl, minr, minc);
		if (p) {
			/* error */
			if (p->cellerror)
				return 0;
			/* valid number? */
			if (p->flags & is_valid)
				numerics++;
			/* !ok to be empty */
			else if (emptyok == -1)
				return 0;
		}
		else if (emptyok == -1)
			return 0;
	}
	return numerics >= emptyok;
}

/* for one-dim matrices, sets steps to traverse. */
static void
set_step(minr, minc, maxr, maxc, rincp, cincp)
int	minr, minc, maxr, maxc;
int	*cincp, *rincp;
{
	*cincp = *rincp = 0;
	/* establish steps */
	if (minr == maxr)
		*cincp = 1;
	else
		*rincp = 1;
}


static void
populate_power(rate, rate_pow, npow)
double	rate, *rate_pow;
int	npow;
{
	double	c = 1.0;
	while (--npow >= 0) {
		*rate_pow++ = c;
		c *= rate;
	}
}


static double 
cash_flow(rate, rate_pow, minr, minc, maxr, maxc)
double	rate;
double	*rate_pow;
int	minr, minc, maxr, maxc;
{
	int	rinc, cinc;
	struct ent	*p;
	double ret;

	ret = 0.0;
	set_step(minr, minc, maxr, maxc, &rinc, &cinc);
	for (; minr <= maxr && minc <= maxc; minr += rinc, minc += cinc)
		/* skip empties and labels. */
		if ((p = *ATBL(tbl, minr, minc)) && (p->flags&is_valid))
			ret += (*ATBL(tbl, minr, minc))->v / *rate_pow++;
	return (ret);
}
#endif /* PSC */
