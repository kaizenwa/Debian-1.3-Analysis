/*   mtst.c
 Consistency tests for math functions.

 With NTRIALS=10000, the following are typical results for
 an alleged IEEE long double precision arithmetic:

Consistency test of math functions.
Max and rms errors for 10000 random arguments.
A = absolute error criterion (but relative if >1):
Otherwise, estimate is of relative error
x =   cbrt(   cube(x) ):  max = 7.66E-20   rms = 3.29E-21
x =   atan(    tan(x) ):  max = 2.02E-19   rms = 3.92E-20
x =    sin(   asin(x) ):  max = 2.15E-19   rms = 2.84E-20
x =   sqrt( square(x) ):  max = 0.00E+00   rms = 0.00E+00
x =    log(    exp(x) ):  max = 1.08E-18   rms = 1.42E-20
x =   log2(   exp2(x) ):  max = 9.91E-20 A rms = 3.81E-21 A
x =  log10(  exp10(x) ):  max = 1.40E-18   rms = 1.84E-20
x =  acosh(   cosh(x) ):  max = 9.49E-20   rms = 9.49E-22
x = pow( pow(x,a),1/a ):  max = 8.41E-18   rms = 1.53E-19
x =   tanh(  atanh(x) ):  max = 1.08E-19   rms = 1.31E-20
x =  asinh(   sinh(x) ):  max = 0.00E+00   rms = 0.00E+00
x =    cos(   acos(x) ):  max = 1.63E-19 A rms = 4.78E-20 A

*/

/*
Cephes Math Library Release 2.1:  December, 1988
Copyright 1984, 1987, 1988 by Stephen L. Moshier
Direct inquiries to 30 Frost Street, Cambridge, MA 02140
*/


/* #include "mconf.h" */

#define NTRIALS 10000
#define WTRIALS (NTRIALS/5)
#define STRTST 0

/* Note, fabsl may be an intrinsic function. */

long double fabsl(), sqrtl();
long double cbrtl(), expl(), logl(), tanl(), atanl();
long double sinl(), asinl(), cosl(), acosl(), powl();
long double tanhl(), atanhl(), sinhl(), asinhl(), coshl(), acoshl();
long double pow2l(), log2l(), pow10l(), log10l();
long double jnl(), ynl(), expm1l(), log1pl(), lgammal();
extern int merror;

/*
NYI:
double iv(), kn();
double ndtr(), ndtri(), ellpe(), ellpk(), gamma(), lgam();
*/

/* Provide inverses for square root and cube root: */
long double squarel(x)
long double x;
{
return( x * x );
}

long double cubel(x)
long double x;
{
return( x * x * x );
}

/* lookup table for each function */
struct fundef
	{
	char *nam1;		/* the function */
	long double (*name )();
	char *nam2;		/* its inverse  */
	long double (*inv )();
	int nargs;		/* number of function arguments */
	int tstyp;		/* type code of the function */
	long ctrl;		/* relative error flag */
	long double arg1w;		/* width of domain for 1st arg */
	long double arg1l;		/* lower bound domain 1st arg */
	long arg1f;		/* flags, e.g. integer arg */
	long double arg2w;		/* same info for args 2, 3, 4 */
	long double arg2l;
	long arg2f;
/*
	double arg3w;
	double arg3l;
	long arg3f;
	double arg4w;
	double arg4l;
	long arg4f;
*/
	};


/* fundef.ctrl bits: */
#define RELERR 1
#define EXPSCAL 4

/* fundef.tstyp  test types: */
#define POWER 1 
#define ELLIP 2 
#define GAMMA 3
#define WRONK1 4
#define WRONK2 5
#define WRONK3 6
#define EXPM1 7
#define LOG1P 8

/* fundef.argNf  argument flag bits: */
#define INT 2

#define MAXLOGL   1.1356523406294143949492E4L
#define MINLOGL  -1.1355137111933024058873E4L
#define PI 3.14159265358979323846L
#define PIO2 1.570796326794896619L

#define NTESTS 16
struct fundef defs[NTESTS] = {
{"  cube",   cubel,   "  cbrt",   cbrtl, 1, 0, 1, 2000.0L, -1000.0L,   0,
0.0, 0.0, 0},
{"   tan",    tanl,   "  atan",   atanl, 1, 0, 1,    PI,     -PIO2,  0,
0.0, 0.0, 0},
{"  asin",   asinl,   "   sin",    sinl, 1, 0, 1,   2.0L,     -1.0L,  0,
0.0, 0.0, 0},
{"square", squarel,   "  sqrt",   sqrtl, 1, 0, 1,  170.0L,    -85.0L, EXPSCAL,
0.0, 0.0, 0},
{"   exp",    expl,   "   log",    logl, 1, 0, 0,  340.0L,    -170.0L,  0,
0.0, 0.0, 0},
{"  pow2",   pow2l,   "  log2",   log2l, 1, 0, 0,  340.0L,    -170.0L,  0,
0.0, 0.0, 0},
{" pow10",  pow10l,   " log10",  log10l, 1, 0, 0,  340.0L,    -170.0L,  0,
0.0, 0.0, 0},
{"  cosh",   coshl,   " acosh",  acoshl, 1, 0, 0,  340.0L,     0.0L,  0,
0.0, 0.0, 0},
{"pow",       powl,      "pow",    powl, 2, POWER, 1, 25.0L, 0.0L,   0,
50.0, -25.0, 0},
{" atanh",  atanhl,   "  tanh",   tanhl, 1, 0, 1,    2.0L,    -1.0L,  0,
0.0, 0.0, 0},
{"  sinh",   sinhl,   " asinh",  asinhl, 1, 0, 1,  340.0L,   0.0L,  0,
0.0, 0.0, 0},
{"  acos",   acosl,   "   cos",    cosl, 1, 0, 0,   2.0L,      -1.0L,  0,
0.0, 0.0, 0},
{" gamma", lgammal,   " gamma", lgammal, 1, GAMMA, 0, 20.0L,  0.0L,  0,
0.0, 0.0, 0},
{" expm1",  expm1l,   "   exp",    expl, 1, EXPM1, 0,  6.0L,    -3.0L,  0,
0.0, 0.0, 0},
{" log1p",  log1pl,   "   log",    logl, 1, LOG1P, 0,  4.0L,    -1.0L,  0,
0.0, 0.0, 0},
{"  Jn",     jnl,   "  Yn",     ynl, 2, WRONK1, 0, 30.0,  0.1,  0,
40.0, -20.0, INT},
};

static char *headrs[] = {
"x = %s( %s(x) ): ",
"x = %s( %s(x,a),1/a ): ",	/* power */
"Legendre %s, %s: ",		/* ellip */
"x gamma(x) = gamma(x+1): ",		/* gamma */
"Wronksian of %s, %s: ",  /* wronk1 */
"Wronksian of %s, %s: ",  /* wronk2 */
"Wronksian of %s, %s: ",  /* wronk3 */
"expm1(x)  =  exp(x) - 1: ",
"log1p(x)  =  log(1 + x): ",
};
 
static long double y1 = 0.0;
static long double y2 = 0.0;
static long double y3 = 0.0;
static long double y4 = 0.0;
static long double a = 0.0;
static long double x = 0.0;
static long double y = 0.0;
static long double z = 0.0;
static long double e = 0.0;
static long double max = 0.0;
static long double rmsa = 0.0;
static long double rms = 0.0;
static long double ave = 0.0;
static double da, db, dc, dd;

void ldrand();
int printf();
void exit();

void main()
{
long double (*fun )();
long double (*ifun )();
struct fundef *d;
int i, k, itst;
int m, ntr;

ntr = NTRIALS;
printf( "Consistency test of math functions.\n" );
printf( "Max and rms errors for %d random arguments.\n",
	ntr );
printf( "A = absolute error criterion (but relative if >1):\n" );
printf( "Otherwise, estimate is of relative error\n" );

/* Initialize machine dependent parameters to test near the
 * largest an smallest possible arguments.  To compare different
 * machines, use the same test intervals for all systems.
 */
/*
defs[1].arg1w = PIL;
defs[1].arg1l = -PIL/2.0;
*/
/*
defs[3].arg1w = MAXLOGL;
defs[3].arg1l = -MAXLOGL/2.0;
defs[4].arg1w = 2.0*MAXLOGL;
defs[4].arg1l = -MAXLOGL;
defs[6].arg1w = 2.0*MAXLOGL;
defs[6].arg1l = -MAXLOGL;
defs[7].arg1w = MAXLOGL;
defs[7].arg1l = 0.0;
*/

/* Outer loop, on the test number: */

for( itst=STRTST; itst<NTESTS; itst++ )
{
d = &defs[itst];
m = 0;
max = 0.0L;
rmsa = 0.0L;
ave = 0.0L;
fun = d->name;
ifun = d->inv;

/* Smaller number of trials for Wronksians
 * (put them at end of list)
 */
if( d->tstyp == WRONK1 )
	{
	ntr = WTRIALS;
	printf( "Absolute error and only %d trials:\n", ntr );
	}
/*
y1 = d->arg1l;
y2 = d->arg1w;
da = y1;
db = y2;
printf( "arg1l = %.4e, arg1w = %.4e\n", da, db );
*/
printf( headrs[d->tstyp], d->nam2, d->nam1 );

for( i=0; i<ntr; i++ )
{
m++;
k = 0;

/* make random number(s) in desired range(s) */
switch( d->nargs )
{

default:
goto illegn;
	
case 2:
ldrand( &a );
a = d->arg2w *  ( a - 1.0L )  +  d->arg2l;
if( d->arg2f & EXPSCAL )
	{
	a = expl(a);
	ldrand( &y2 );
	a -= 1.0e-13L * a * (y2 - 1.0L);
	}
if( d->arg2f & INT )
	{
	k = a + 0.25L;
	a = k;
	}

case 1:
ldrand( &x );
y1 = d->arg1l;
y2 = d->arg1w;
x = y2 *  ( x - 1.0L )  +  y1;
if( x < y1 )
	x = y1;
y1 += y2;
if( x > y1 )
	x = y1;
if( d->arg1f & EXPSCAL )
	{
	x = expl(x);
	ldrand( &y2 );
	x += 1.0e-13L * x * (y2 - 1.0L);
	}
}

/* compute function under test */
switch( d->nargs )
	{
	case 1:
	switch( d->tstyp )
		{
		case ELLIP:
		y1 = ( *(fun) )(x);
		y2 = ( *(fun) )(1.0L-x);
		y3 = ( *(ifun) )(x);
		y4 = ( *(ifun) )(1.0L-x);
		break;

		case GAMMA:
		y = lgammal(x+1.0L);
		x = lgammal(x) + logl(x);
		break;

		case EXPM1:
		y = expm1l(x);
		x = expl(x) - 1;
		break;

		case LOG1P:
		y = log1pl(x);
		x = logl(1.0+x);
		break;

		default:
		z = ( *(fun) )(x);
		y = ( *(ifun) )(z);
		}
/*
if( merror )
	{
	printf( "error: x = %.15e, z = %.15e, y = %.15e\n",
	 (double )x, (double )z, (double )y );
	}
*/
	break;
	
	case 2:
	if( d->arg2f & INT )
		{
		switch( d->tstyp )
			{
			case WRONK1:
			y1 = (*fun)( k, x ); /* jn */
			y2 = (*fun)( k+1, x );
			y3 = (*ifun)( k, x ); /* yn */
			y4 = (*ifun)( k+1, x );	
			break;

			case WRONK2:
			y1 = (*fun)( a, x ); /* iv */
			y2 = (*fun)( a+1.0L, x );
			y3 = (*ifun)( k, x ); /* kn */	
			y4 = (*ifun)( k+1, x );	
			break;

			default:
			z = (*fun)( k, x );
			y = (*ifun)( k, z );
			}
		}
	else
		{
		if( d->tstyp == POWER )
			{
			z = (*fun)( x, a );
			y = (*ifun)( z, 1.0L/a );
			}
		else
			{
			z = (*fun)( a, x );
			y = (*ifun)( a, z );
			}
		}
	break;


	default:
illegn:
	printf( "Illegal nargs= %d", d->nargs );
	exit(1);
	}	

switch( d->tstyp )
	{
	case WRONK1:
	e = (y2*y3 - y1*y4) - 2.0L/(PI*x); /* Jn, Yn */
	break;

	case WRONK2:
	e = (y2*y3 + y1*y4) - 1.0L/x; /* In, Kn */
	break;
#if 0
	case ELLIP:
	e = (y1-y3)*y4 + y3*y2 - PIO2L;
	break;
#endif
	default:
	e = y - x;
	break;
	}

if( d->ctrl & RELERR )
	{
	if( x != 0.0L )
		e /= x;
	else
		printf( "warning, x == 0\n" );
	}
else
	{
	if( fabsl(x) > 1.0L )
		e /= x;
	}

ave += e;
/* absolute value of error */
if( e < 0 )
	e = -e;

/* peak detect the error */
if( e > max )
	{
	max = e;

	if( e > 1.0e-10L )
		{
da = x;
db = z;
dc = y;
dd = max;
		printf("x %.6E z %.6E y %.6E max %.4E\n",
		da, db, dc, dd );
/*
		if( d->tstyp >= WRONK1 )
			{
		printf( "y1 %.4E y2 %.4E y3 %.4E y4 %.4E k %d x %.4E\n",
		 (double )y1, (double )y2, (double )y3,
		 (double )y4, k, (double )x );
			}
*/
		}

/*
	printf("%.8E %.8E %.4E %6ld \n", x, y, max, n);
	printf("%d %.8E %.8E %.4E %6ld \n", k, x, y, max, n);
	printf("%.6E %.6E %.6E %.4E %6ld \n", a, x, y, max, n);
	printf("%.6E %.6E %.6E %.6E %.4E %6ld \n", a, b, x, y, max, n);
	printf("%.4E %.4E %.4E %.4E %.4E %.4E %6ld \n",
		a, b, c, x, y, max, n);
*/
	}

/* accumulate rms error	*/
e *= 1.0e16L;	/* adjust range */
rmsa += e * e;	/* accumulate the square of the error */
}

/* report after NTRIALS trials */
rms = 1.0e-16L * sqrtl( rmsa/m );
da = max;
db = rms;
if(d->ctrl & RELERR)
	printf(" max = %.2E   rms = %.2E\n", da, db );
else
	printf(" max = %.2E A rms = %.2E A\n", da, db );
} /* loop on itst */
exit(0);
}

