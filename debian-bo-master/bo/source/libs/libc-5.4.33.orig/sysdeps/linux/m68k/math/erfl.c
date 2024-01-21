/*							erf.c
 *
 *	Error function
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, erf();
 *
 * y = erf( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * The integral is
 *
 *                           x 
 *                            -
 *                 2         | |          2
 *   erf(x)  =  --------     |    exp( - t  ) dt.
 *              sqrt(pi)   | |
 *                          -
 *                           0
 *
 * The magnitude of x is limited to 9.231948545 for DEC
 * arithmetic; 1 or -1 is returned outside this range.
 *
 * For 0 <= |x| < 1, erf(x) = x * P6(x^2)/Q6(x^2); otherwise
 * erf(x) = 1 - erfc(x).
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      0,1          3000       1.8e-19     6.5e-20
 *
 */
/*							erfc.c
 *
 *	Complementary error function
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, erfc();
 *
 * y = erfc( x );
 *
 *
 *
 * DESCRIPTION:
 *
 *
 *  1 - erf(x) =
 *
 *                           inf. 
 *                             -
 *                  2         | |          2
 *   erfc(x)  =  --------     |    exp( - t  ) dt
 *               sqrt(pi)   | |
 *                           -
 *                            x
 *
 *
 * For small x, erfc(x) = 1 - erf(x); otherwise rational
 * approximations are computed.
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      0,8         1000       1.8e-18     6.1e-19
 * For x > 1, error is dominated by the calculation of exp(-x^2)
 * and is of order 3e-19 x.
 *
 *
 * ERROR MESSAGES:
 *
 *   message         condition              value returned
 * erfc underflow    x > sqrt(MAXLOGL)          0.0
 *
 *
 */

/*							ndtr.c
 *
 *	Normal distribution function
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, ndtr();
 *
 * y = ndtr( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns the area under the Gaussian probability density
 * function, integrated from minus infinity to x:
 *
 *                            x
 *                             -
 *                   1        | |          2
 *    ndtr(x)  = ---------    |    exp( - t /2 ) dt
 *               sqrt(2pi)  | |
 *                           -
 *                          -inf.
 *
 *             =  ( 1 + erf(z) ) / 2
 *             =  erfc(z) / 2
 *
 * where z = x/sqrt(2). Computation is via the functions
 * erf and erfc.
 *
 *
 * ACCURACY:
 *
 * See erfl, erfc.
 *
 * ERROR MESSAGES:
 *
 *   message         condition         value returned
 * erfc underflow    x > sqrt(MAXLOGL)      0.0
 *
 */

/*
Cephes Math Library Release 2.2:  June, 1992
Copyright 1984, 1987, 1988, 1992 by Stephen L. Moshier
Direct inquiries to 30 Frost Street, Cambridge, MA 02140
*/

#include <math.h>
extern long double __polevll ( long double, long double *, int );
extern long double __p1evll ( long double, long double *, int );

#define SQRTH 7.07106781186547524401E-1L
#define MAXLOGL 1.1356523406294143949492E4L

/*
erfc(x) = exp(-x^2) P(z)/Q(z)
z(x) = 1/x
Relative error
n=9, d=10
Peak error =  7.761600916141239627567679213879910992123E-21
Relative error spread =  1.023351469044268241908780645010281568783E0
*/
static long double P[] = {
 1.257754157740948157833312785512722262237E9L,
 2.544170037359209505076053758183547658874E9L,
 2.546959074195068787444430942851029462365E9L,
 1.605230257662887752768505542850382838437E9L,
 6.899137924684927504585242368865475647796E8L,
 2.066152777382002343549858319788089708173E8L,
 4.227432144169649646138430076248102333111E7L,
 5.463192951525719736944609969792412137906E6L,
 3.509752864577976833796176700600960407890E5L,
-1.071020848327048365754654097516174838747E-5L,
};
static long double Q[] = {
/* 1.000000000000000000000000000000000000000E0 */
 1.257754146471137811029618866219716318753E9L,
 3.963393686787501126271033323119385984564E9L,
 5.761415509088700168863599482318704228527E9L,
 5.089047658881201390929483867868625212361E9L,
 3.023468726254039362316680583964735625632E9L,
 1.259993446190802606495833425577455019099E9L,
 3.710577088683661371497272954319350172686E8L,
 7.524032571027728454099705963391636990057E7L,
 9.683257455518460133999913804480458079007E6L,
 6.220874963793103874137444426727337364012E5L
};
/*
erfc(x) = exp(-x^2) z P(z**2)/Q(z**2)
z(x) = 1/x
x >= 8
Relative error
n=4, d=5
Peak error =  1.82e-21
Relative error spread =  3.9e-3
*/
static long double R[] = {
 3.621771504143418323727691824727645990561E0L,
 7.176028075353789451273277986715563076155E0L,
 3.446811092743012309113505238337669421091E0L,
 5.541324340261065929875426053951866015250E-1L,
 2.699905677541423267238580904507582955709E-2L
};
static long double S[] = {
/* 1.000000000000000000000000000000000000000E0 */
 1.073074561751358600517229966140297947840E1L,
 1.534256653809200505130664147433856235472E1L,
 6.576473386900615376978244623190582091578E0L,
 1.006101457677419240008726711451145446254E0L,
 4.785458215239962067727484634993830260306E-2L
};
/*
erf(x)  z P(z**2)/Q(z**2)
0 <= x <= 1
z(x) = x
Relative error
n=6, d=6
Peak error =  7.640688643888757419028899590163681314010E-23
Relative error spread =  9.108314657350825251106158661154911170107E-3
*/
static long double T[] = {
 1.097461701666048418572764102524978668857E-1L,
 5.403114568277600506272199159607654730741E0L,
 2.871755797503643801098000314688440010017E2L,
 2.677455035557940040284806618777105723130E3L,
 4.825836895686958085365801683371047641820E4L,
 1.549871261553367266536226453640515782388E5L,
 1.104347959782514112319886330985904796384E6L
};
static long double U[] = {
/* 1.000000000000000000000000000000000000000E0 */
 4.525736349514749653000420739995094197946E1L,
 9.715176200212976593594958594375455394272E2L,
 1.245878656969449881132817817129763145725E4L,
 9.942693068079099139656602031366763195331E4L,
 4.635880633067639826809494193933792409347E5L,
 9.787028970280835391789815008496258590638E5L
};

#define UTHRESH 37.519379347

/*
double ndtr(a)
double a;
{
double x, y, z;
double fabs(), erf(), erfc();

x = a * SQRTH;
z = fabs(x);

if( z < SQRTH )
	y = 0.5 + 0.5 * erf(x);

else
	{
	y = 0.5 * erfc(z);

	if( x > 0 )
		y = 1.0 - y;
	}

return(y);
}
*/

long double erfcl(long double a)
{
long double p,q,x,y,z;


if( a < 0.0L )
	x = -a;
else
	x = a;

if( x < 1.0L )
	return( 1.0L - erfl(a) );

z = -a * a;

if( z < -MAXLOGL )
	{
under:
/* Some kind of error flagging needed. */
/*	mtherr( "erfcl", UNDERFLOW ); */
	if( a < 0 )
		return( 2.0L );
	else
		return( 0.0L );
	}

z = expl(z);

if( x < 8.0L )
	{
	  y = 1.0/a;
	p = __polevll( y, P, 9 );
	q = __p1evll( y, Q, 10 );
	}
else
	{
	y = 1.0/(a*a);
	p = __polevll( y, R, 4 ) / a;
	q = __p1evll( y, S, 5 );
	}
y = (z * p)/q;

if( a < 0 )
	y = 2.0L - y;

if( y == 0.0L )
	goto under;

return(y);
}



long double erfl(long double x)
{
long double y, z;

if( fabsl(x) > 1.0L )
	return( 1.0L - erfcl(x) );

z = x * x;
y = x * __polevll( z, T, 6 ) / __p1evll( z, U, 6 );
return( y );
}
