/*	@(#)drand48.c	2.2	*/
/*LINTLIBRARY*/
/*
 *	drand48, etc. pseudo-random number generator
 *	This implementation assumes unsigned short integers of at least
 *	16 bits, long integers of at least 32 bits, and ignores
 *	overflows on adding or multiplying two unsigned integers.
 *	Two's-complement representation is assumed in a few places.
 *	Some extra masking is done if unsigneds are exactly 16 bits
 *	or longs are exactly 32 bits, but so what?
 *	An assembly-language implementation would run significantly faster.
 */

#include <errno.h>
#include <stdlib.h>
#include <random.h>

#define N	16
#define MASK	((unsigned)(1 << (N - 1)) + (1 << (N - 1)) - 1)
#define LOW(x)	((unsigned)(x) & MASK)
#define HIGH(x)	LOW((x) >> N)
#define MUL(x, y, z)	{ long l = (long)(x) * (long)(y); \
		(z)[0] = LOW(l); (z)[1] = HIGH(l); }
#define CARRY(x, y)	((long)(x) + (long)(y) > MASK)
#define ADDEQU(x, y, z)	(z = CARRY(x, (y)), x = LOW(x + (y)))
#define X0	0x330E
#define X1	0xABCD
#define X2	0x1234
#define A0	0xE66D
#define A1	0xDEEC
#define A2	0x5
#define C	0xB
#define SET3(x, x0, x1, x2)	((x)[0] = (x0), (x)[1] = (x1), (x)[2] = (x2))
#define SETLOW(x, y, n) SET3(x, LOW((y)[n]), LOW((y)[(n)+1]), LOW((y)[(n)+2]))
#define SEED(x0, x1, x2) (SET3(rand_data->x, x0, x1, x2), \
                          SET3(rand_data->a, A0, A1, A2), \
                          rand_data->c = C)
#define REST()	for (i = 0; i < 3; i++) { xsubi[i] = rand_data->x[i]; \
                                          rand_data->x[i] = temp[i]; } \
		return 0;
#define NEST(TYPE, f, F)	int f(xsubi, retval, rand_data) \
    register unsigned short int *xsubi; TYPE *retval; \
    struct rand48_data *rand_data; { \
	register int i; unsigned temp[3]; \
	if (retval == NULL || rand_data == NULL) return -1; \
	for (i = 0; i < 3; i++) { temp[i] = rand_data->x[i]; \
				  rand_data->x[i] = LOW(xsubi[i]); }  \
	if (F(retval, rand_data) < 0) return -1; REST(); }
#define HI_BIT	(1L << (2 * N - 1))

static void next();

int
drand48_r(double *retval, struct rand48_data *rand_data)
{
	static const double two16m = 1.0 / (1L << N);

	if (retval == NULL || rand_data == NULL)
	  {
	    errno = EINVAL;
	    return -1;
	  }

	next(rand_data);
	*retval = (two16m * (two16m * (two16m * rand_data->x[0]
				       + rand_data->x[1])
			     + rand_data->x[2]));
	return 0;
}

NEST(double, erand48_r, drand48_r);

int
lrand48_r(long int *retval, struct rand48_data *rand_data)
{
        if (retval == NULL || rand_data == NULL) {
	    errno = EINVAL;
	    return -1;
	}

	next(rand_data);
	*retval = (((long)rand_data->x[2] << (N - 1))
		   + (rand_data->x[1] >> 1));
	return 0;
}

int
mrand48_r(long int *retval, struct rand48_data *rand_data)
{
	register long l;

	if (retval == NULL || rand_data == NULL) {
	    errno = EINVAL;
	    return -1;
	}

	next(rand_data);
	/* sign-extend in case length of a long > 32 bits
						(as on Honeywell) */
	*retval = ((l = ((long)rand_data->x[2] << N) 
		    + rand_data->x[1]) & HI_BIT ? l | -HI_BIT : l);
	return 0;
}

static void
next(struct rand48_data *rand_data)
{
	unsigned p[2], q[2], r[2], carry0, carry1;

	MUL(rand_data->a[0], rand_data->x[0], p);
	ADDEQU(p[0], rand_data->c, carry0);
	ADDEQU(p[1], carry0, carry1);
	MUL(rand_data->a[0], rand_data->x[1], q);
	ADDEQU(p[1], q[0], carry0);
	MUL(rand_data->a[1], rand_data->x[0], r);
	rand_data->x[2] = LOW(carry0 + carry1 + CARRY(p[1], r[0])
			      + q[1] + r[1] 
			      + rand_data->a[0] * rand_data->x[2] 
			      + rand_data->a[1] * rand_data->x[1]
			      + rand_data->a[2] * rand_data->x[0]);
	rand_data->x[1] = LOW(p[1] + r[0]);
	rand_data->x[0] = LOW(p[0]);
}

int
srand48_r(long int seedval, struct rand48_data *rand_data)
{
        if (rand_data == NULL) {
                errno = EINVAL;
		return -1;
	}

	SEED(X0, LOW(seedval), HIGH(seedval));

	return 0;
}

int
seed48_r(unsigned short seed16v[3], unsigned short *oldseed,
	 struct rand48_data *rand_data)
{
        if (oldseed == NULL || rand_data == NULL) {
	        errno = EINVAL;
		return -1;
	}

	SETLOW(oldseed, rand_data->x, 0);
	SEED(LOW(seed16v[0]), LOW(seed16v[1]), LOW(seed16v[2]));
	return 0;
}

int
lcong48_r(unsigned short int param[7], struct rand48_data *rand_data)
{
        if (rand_data == NULL) {
	        errno = EINVAL;
		return -1;
	}

	SETLOW(rand_data->x, param, 0);
	SETLOW(rand_data->a, param, 3);
	rand_data->c = LOW(param[6]);
	return 0;
}

NEST(long, nrand48_r, lrand48_r);

NEST(long, jrand48_r, mrand48_r);

#ifdef DRIVER
/*
	This should print the sequences of integers in Tables 2
		and 1 of the TM:
	1623, 3442, 1447, 1829, 1305, ...
	657EB7255101, D72A0C966378, 5A743C062A23, ...
 */
#include <stdio.h>

main()
{
	int i;

	for (i = 0; i < 80; i++) {
		printf("%4d ", (int)(4096 * drand48()));
		printf("%.4X%.4X%.4X\n", x[2], x[1], x[0]);
	}
}
#endif
