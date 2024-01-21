/* 95/01/13 - Ulrich Drepper <drepper@ira.uka.de>
 *            modified for reentrent library
 */
#include <stdlib.h>
#include <math.h>
#include <ieee754.h>

#define	IEEE	1

/*
 *	ecvt converts to decimal
 *	the number of digits is specified by ndigit
 *	decpt is set to the position of the decimal point
 *	sign is set to 0 for positive, 1 for negative
 */

#ifdef IEEE

static inline int
__isspecial(double f, char *bp, int len)
{
	union ieee754_double *ip = (union ieee754_double *) &f;

	if ((ip->ieee.exponent & 0x7ff) != 0x7ff)
		return(0);
	if (ip->ieee.mantissa0 || ip->ieee.mantissa1)
		strncpy(bp, "NaN", len);
	else if (ip->ieee.negative)
		strncpy(bp, "-Infinity", len);
	else
		strncpy(bp, "Infinity", len);
	return(1);
}

#endif

static int
cvt_r(double arg, size_t ndigits, int *decpt, int *sign, int eflag,
      char *buf, int len)
{
	register int r2;
	double fi, fj;
	register char *p, *p1;

	if (buf == NULL || len <= 0 || len <= ndigits)
		return(-1);

#ifdef IEEE
	/* XXX */
	if (__isspecial(arg, buf, len))
		return(0);
#endif
	if (ndigits>=len-1)
		ndigits = len-2;
	r2 = 0;
	*sign = 0;
	p = &buf[0];
	if (arg<0) {
		*sign = 1;
		arg = -arg;
	}
	arg = modf(arg, &fi);
	/*
	 * Do integer part
	 */
	if (fi != 0) {
		p1 = &buf[len];
		while (fi != 0) {
			if (p1 < p) {
				p[0] = '\0';
				return -1;
			}
			fj = modf(fi/10, &fi);
			*--p1 = (int)((fj+.03)*10) + '0';
			r2++;
		}
		while (p1 < &buf[len])
			*p++ = *p1++;
	} else if (arg > 0) {
		while ((fj = arg*10) < 1) {
			arg = fj;
			r2--;
		}
	}
	p1 = &buf[ndigits];
	if (eflag==0)
		p1 += r2;
	*decpt = r2;
	if (p1 >= &buf[len])
		p1 = &buf[len-1];
	while (p<=p1+5 || p<&buf[len]) {
		arg *= 10;
		arg = modf(arg, &fj);
		*p++ = (int)fj + '0';
	}
	if (p1 == &buf[len-1]) {
		buf[len-1] = '\0';
		return 0;
	}
	p = p1;
	*p1 += 5;
	while (*p1 > '9') {
		*p1 = '0';
		if (p1>buf)
			++*--p1;
		else {
			*p1 = '1';
			(*decpt)++;
			if (eflag==0) {
				if (p>buf)
					*p = '0';
				p++;
			}
		}
	}
	*p = '\0';
	return 0;
}

int
ecvt_r(double arg, size_t ndigits, int *decpt, int *sign, char *buf, int len)
{
	return(cvt_r(arg, ndigits, decpt, sign, 1, buf, len));
}

int
fcvt_r(double arg, size_t ndigits, int *decpt, int *sign, char *buf, int len)
{
	return(cvt_r(arg, ndigits, decpt, sign, 0, buf, len));
}

