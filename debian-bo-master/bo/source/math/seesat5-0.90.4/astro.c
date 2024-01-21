/*
ASTRO.C
by Paul S. Hirose, 1991 Oct 19
Coordinate transformation and astronomical functions for SEESAT satellite
tracking program.

This file is in the public domain.

200 - 299
*/

#include "SEESAT.H"	/* global header */

/* library functions:
	asin atan atan2 atof cos log10 printf sin sqrt tan
are used in this file. */


#if ECOC
extern void printf();
extern double asin(), atan(), atan2(), atof(), cos(),
	log10(), sin(), sqrt(), tan();
#endif

#if ECOC | LASERC
extern double dint();
#endif

#if STDC
static double dint(x)
double x;
{
	if (x >= 0.)
		return floor(x);
	else
		return ceil(x);
}
#endif

#define FLAT 3.35278e-3		/* flattening of earth.
				WGS-72 value = 3.35278e-3 */

/* REFEP is the default epoch to which R.A./dec. will be precessed.
E.g. (per Meeus p. 101):
3477629251. = epoch 1900.0, 3503926689. = 1950.0, 3530224800. = 2000.0.
*/

#if ENPRE		/* precession enabled */
#define REFEP 3530224800.
#endif


/* Bias to equalize SEESAT's internal magnitude scale with the absolute
satellite magnitudes being supplied from the element file or manually.  If
the absolute mags are for a satellite at R kilometers and P percent
illuminated, set MAGOFF to 2.5 * log10((6378/R)^2 * (P/50)).  E.g., for
Molczan's standard conditions of 1000 km and 50%, MAGOFF = 4.02. */

#define MAGOFF 4.02


/* 0 for normal operation.  Non-zero will make static functions and data
extern */
#define TEST 0


/*######################## LOCAL FUNCTIONS ########################*/

#if TEST
#define STATIC		/* precedes static func definitions */
#define SC extern	/* precedes static func declarations */

#else
#define STATIC static
#define SC static
#endif

SC void eqhor();		/* convert rectangular to az/el, print */
SC void eceq();			/* ecliptical to equatorial coordinates */
SC void point();		/* align +z axis with a vector */
SC void recsph();		/* rectangular to spherical coordinates */
SC void sun();			/* xyz of sun (interpol.) */
SC double sunlon();     	/* mean longitude of sun */
SC void sunref();		/* xyz of sun (non-interpol.) */
SC void topos();		/* rectangular coordinates of observer */
SC void yrot();			/* y-rotate vector */
SC void zrot();			/* z-rotate vector */

/*########################### LOCAL DATA ###########################*/

#if ENPRE

/* dircos[] is a rotation matrix.  Initialized by inpre() & used to precess
satellite Right Ascension & declination.  terep is the epoch to which the
coordinates will be precessed. */

STATIC struct {
	double ix, iy, iz, jx, jy, jz, kx, ky, kz;
} dircos;
STATIC double terep = REFEP;

#endif

/* data for observer's location */
STATIC struct {
	double
	h,		/* altitude above sea level */
	lambda,		/* longitude */
	sinlat, coslat,	/* latitude sin & cos */
	zg,		/* north distance, observer to equatorial plane */
	xc;		/* distance, observer to polar axis */
} obs;

/* Sin & cos of epsilon, the obliquity of ecliptic.  Since epsilon changes
but .013 deg/century, it's sufficient to use a fixed value, that of 1975
(23.442 deg).  That will strike a balance between 1950 & 2000 epochs. */
STATIC double sineps = .39783;
STATIC double coseps = .91746;

/*############################## CODE ##############################*/

STATIC void
eqhor(equa, t)
struct vector *equa;	/* vector to object */
double t;		/* epoch */
/* Converts vector *equa from a rectangular (Aries, equator, north) system to
a polar (azimuth, elevation, range) system at epoch t.  Prints the azimuth &
elevation rounded to nearest deg.  No correction for refraction or geocentric
parallax. */
{
	double	lhaa;	/* local hour angle of Aries */

	/* rotate to a south, east, zenith system */
	lhaa = thetag(t) + obs.lambda;
	zrot(equa, sin(lhaa), cos(lhaa));
	yrot(equa, obs.coslat, obs.sinlat);

	equa->x = -equa->x;	/* makes az run correct direction */
	recsph(equa);		/* convert to spherical coordinates */

	printf("az=%d ", (int) (equa->x * ra2de + .5));
	printf("el=%d\n", (int) (equa->y * ra2de +
	  (equa->y >= 0. ? .5 : -.5)));
}

STATIC int
eqhor2(equa, t)
struct vector *equa;	/* vector to object */
double t;		/* epoch */
/* Converts vector *equa from a rectangular (Aries, equator, north) system to
a polar (azimuth, elevation, range) system at epoch t.  Prints the azimuth &
elevation rounded to nearest deg.  No correction for refraction or geocentric
parallax. */
{
	double	lhaa;	/* local hour angle of Aries */

	/* rotate to a south, east, zenith system */
	lhaa = thetag(t) + obs.lambda;
	zrot(equa, sin(lhaa), cos(lhaa));
	yrot(equa, obs.coslat, obs.sinlat);

	equa->x = -equa->x;	/* makes az run correct direction */
	recsph(equa);		/* convert to spherical coordinates */

	return  (int) (equa->y * ra2de +
	  (equa->y >= 0. ? .5 : -.5));

}

STATIC void
eqhor_az_el(equa, t, az, el)
struct vector *equa;	/* vector to object */
double t;		/* epoch */
double *az;
double *el;
/* Converts vector *equa from a rectangular (Aries, equator, north) system to
a polar (azimuth, elevation, range) system at epoch t.  Prints the azimuth &
elevation rounded to nearest deg.  No correction for refraction or geocentric
parallax. */
{
	double	lhaa;	/* local hour angle of Aries */

	/* rotate to a south, east, zenith system */
	lhaa = thetag(t) + obs.lambda;
	zrot(equa, sin(lhaa), cos(lhaa));
	yrot(equa, obs.coslat, obs.sinlat);

	equa->x = -equa->x;	/* makes az run correct direction */
	recsph(equa);		/* convert to spherical coordinates */

	*az = equa->x;
	*el = equa->y;
}

void
dusk()
/* Print azimuth and elevation of the sun.  The next argument(s) on
the command line are taken as the epoch. */
{
	double t;
	struct vector csun;	/* equatorial coordinates of sun */

	t = tokmin();		/* get epoch from cmd line */
	sun(&csun, t);		/* get sun's position */
	eqhor(&csun, t);	/* convert to az/el, print */
}

int
sun_elev(t)
double t;
/* Print azimuth and elevation of the sun.  */
{
	struct vector csun;	/* equatorial coordinates of sun */

	sun(&csun, t);		/* get sun's position */
	return eqhor2(&csun, t);	/* convert to az/el */
}

STATIC void
eceq(eclip)
struct vector *eclip;	/* ecliptical coordinates */
/* Coverts *eclip to equatorial coordinates, i.e., x-rotates by the negative
obliquity of the ecliptic. */
{
	double tempy;

	tempy    = eclip->y * coseps - eclip->z * sineps;
	eclip->z = eclip->y * sineps + eclip->z * coseps;
	eclip->y = tempy;
}


double
fmod2p(x)
double x;
/* Reduces x to range 0 - 2pi */
{
	x /= twopi;
	x = (x - dint(x)) * twopi;
	if (x < 0.)
		return x + twopi;
	else
		return x;
}


#if ENPRE	/* precession code enabled */

void
inpre()
/* Initialize rotation matrix dircos to precess from "epoch" (a global
variable representing epoch of the satellite elements) to terep.  terep is an
external variable in this file.  Assumes that the effect of precession is a
uniform 50".4 increase of longitude per year. */
{
	double a, sina, cosa;
	struct vector *vecp;
	int i;

	a = (terep - epoch) * -4.65e-10;	/* luni-solar precession */
	sina = sin(a);
	cosa = cos(a);

	/* Ecliptical components of (I, J, K) unit vectors */
	dircos.ix = 1.;		dircos.iy = 0.;		dircos.iz = 0.;
	dircos.jx = 0.;		dircos.jy = coseps;	dircos.jz = -sineps;
	dircos.kx = 0.;		dircos.ky = sineps;	dircos.kz = coseps;

	/* Rotate each vector in longitude by the amount of luni-solar
	precession.  Then convert from ecliptical to equatorial coordinates.
	We end up with the old unit vectors expressed in terms of the new
	(i.e., precessed) equatorial coordinate system. */

	for (vecp = (struct vector *) &dircos, i = 3; i--; ++vecp) {
		zrot(vecp, sina, cosa);
		eceq(vecp);
}	}

#endif


void
moon()
/* Prints the azimuth, elevation, and % of illumination of the moon.  Epoch
is taken from the next arguments on the command line.  Position is good
within a couple degrees or so. */
{
	struct vector luna, sol, terra;
	double t, t1900, lp, mp, f, lamb, beta, cosb;

	t = tokmin();			/* obtain epoch from cmd line */

	/* formulas from Meeus */
	t1900 = t - 3477628800.;		/* 1900 Jan 0 12h UT */
	lp = fmod2p(4.72 + 1.5970243e-4 * t1900);	/* longitude */
	mp = fmod2p(5.168 + 1.5835217e-4 * t1900);	/* mean anomaly */
	f = fmod2p(.1964 + 1.6034425e-4 * t1900);	/* dist fm asc node */
	lamb = lp + .1097 * sin(mp);			/* longitude */
	beta = .0895 * sin(f);				/* latitude */

	/* get ecliptical components of unit vector to moon */
	luna.z = sin(beta);
	cosb = cos(beta);
	luna.y = sin(lamb) * cosb;
	luna.x = cos(lamb) * cosb;

	eceq(&luna);		/* ecliptical to equatorial */

	/* terra = unit vector moon to earth */
	terra.x = -luna.x;
	terra.y = -luna.y;
	terra.z = -luna.z;

	eqhor(&luna, t);	/* print az el */

	/* Get moon->sun unit vector.  sun(), which yields the earth->sun
	vector, comes close enough.   Rotate axes of terra to point +z axis
	at sun.  This will make terra.z equal to the cos of the moon's phase
	angle.  Plug that into Meeus' illumination formula, print. */

	sun(&sol, t);
	point(&terra, &sol);

	printf("%d%% illuminated\n", (int) (50. * (1. + terra.z)));
}

void moon_data(t)
double t;
/* Prints the azimuth, elevation, and % of illumination of the moon.  Epoch
is taken from the next arguments on the command line.  Position is good
within a couple degrees or so. */
{
	struct vector luna, sol, terra;
	double t1900, lp, mp, f, lamb, beta, cosb;

	/* formulas from Meeus */
	t1900 = t - 3477628800.;		/* 1900 Jan 0 12h UT */
	lp = fmod2p(4.72 + 1.5970243e-4 * t1900);	/* longitude */
	mp = fmod2p(5.168 + 1.5835217e-4 * t1900);	/* mean anomaly */
	f = fmod2p(.1964 + 1.6034425e-4 * t1900);	/* dist fm asc node */
	lamb = lp + .1097 * sin(mp);			/* longitude */
	beta = .0895 * sin(f);				/* latitude */

	/* get ecliptical components of unit vector to moon */
	luna.z = sin(beta);
	cosb = cos(beta);
	luna.y = sin(lamb) * cosb;
	luna.x = cos(lamb) * cosb;

	eceq(&luna);		/* ecliptical to equatorial */

	/* terra = unit vector moon to earth */
	terra.x = -luna.x;
	terra.y = -luna.y;
	terra.z = -luna.z;

	eqhor_az_el(&luna, t, &moon_az, &moon_el);

	/* Get moon->sun unit vector.  sun(), which yields the earth->sun
	vector, comes close enough.   Rotate axes of terra to point +z axis
	at sun.  This will make terra.z equal to the cos of the moon's phase
	angle.  Plug that into Meeus' illumination formula, print. */

	sun(&sol, t);
	point(&terra, &sol);

	moon_ill = 50. * (1. + terra.z);
}

void
parall()
/* Prints the parallactic angle:  the angle (at the satellite) between
celestial north and observer's zenith.  The next argument(s) on the command
line are taken as the desired epoch. */
{
	struct vector zenith, topsat;
	double lhaa, t, coslha, sinlha;

	t = tokmin();
	lhaa = thetag(t) + obs.lambda;	/* local hr angle of Aries */
	sinlha = sin(lhaa);
	coslha = cos(lhaa);

	/* Get equatorial coordinates of satellite at time t. */
	MODEL(t - epoch - toffs);

	/* get topocentric sat coordinates */
	topsat.x = sat.x - obs.xc * coslha;
	topsat.y = sat.y - obs.xc * sinlha;
	topsat.z = sat.z + obs.zg;

	/* Load struct "zenith" with a unit vector directed from the observer
	to the zenith, expressed in equatorial coordinates. */

	zenith.z = obs.sinlat;
	zenith.x = coslha * obs.coslat;
	zenith.y = sinlha * obs.coslat;

	/* z-rotate the axes of "zenith" so satellite is in the x-z plane of
	"zenith" (+x half plane).  Then y-rotate axes to point +z axis at
	satellite. */

	point(&zenith, &topsat);

	/* The +z axis passes through the satellite.  The north pole
	has a y-coordinate of 0 and a negative x-coordinate.  The
	origin of the coordinate system is still at the observer. */

	printf("parallactic angle = %d\n",
	  (int) (fmod2p(atan2(zenith.y, -zenith.x)) * ra2de));
}


STATIC void
point(v, d)
struct vector *v, *d;
/* rotates the axes of vector v to align the +z axis with vector d. */
{
	double fz, fz2, r;

	fz2 = d->x * d->x + d->y * d->y;
	fz = sqrt(fz2);
	zrot(v, d->y / fz, d->x / fz);
	r = sqrt(fz2 + d->z * d->z);
	yrot(v, fz / r, d->z / r);
}


STATIC void
recsph(v)
struct vector *v;
/* Converts vector v from rectangular to spherical coordinates.  On exit,
v->x = angle in the x-y plane, v->y = angle with respect to the x-y plane,
and v->z = radius.  Angle x is 0 to 2pi, y is -pi/2 to pi/2. */
{
	double temp;

	temp = sqrt(v->x * v->x + v->y * v->y + v->z * v->z);
	v->x = atan2(v->y, v->x);
	if (v->x < 0.)
		v->x += twopi;
	v->y = asin(v->z / temp);
	v->z = temp;
}


#if ENPRE	/* precession code enabled */

void
setep()
/* Sets rotation matrix "dircos" to precess from "epoch" (a global variable
representing epoch of the satellite orbital elements) to the date/time given
on the command line. */
{
	terep = tokmin();	/* get epoch from cmd line */
	inpre();	/* reinitialize rotation matrix */
}

#endif


void
seth()
/* input observer's height in km, recompute rectangular coordinates */
{
	obs.h = atof(*tokp) / xkmper;
	topos();
}


void
setlat()
/* input observer's latitude, recompute rectangular coordinates */
{
	obs.coslat = atof(*tokp) * de2ra;
	obs.sinlat = sin(obs.coslat);
	obs.coslat = cos(obs.coslat);
	topos();
}


void
setlon()
/* input observer's longitude, recompute rectangular coordinates */
{
	obs.lambda = atof(*tokp) * de2ra;
	topos();
}


STATIC void
sun(pos, ep)
struct vector *pos;
double ep;
{
/* Equatorial xyz components of a unit vector to sun at epoch "ep" are
returned in struct "pos".  Accuracy about .1 deg. */

	static double t0,	/* start epoch of a 6-day interval */
		dx, dy, dz;	/* change in x, y, z during 6 days */
	static struct vector sun0;	/* sun position at t0 */

	double t;	/* e.g. .5 if ep = t0 + 3 days */

	if ((t = (ep - t0) / 8640.) > 1.0  ||  t < 0.) {
	/* Position was requested for a time outside the current 6-day
	interval.  So set up a new 6-day interval centered at "ep". */
		struct vector sun1;

		t0 = ep - 4320.;	/* 3 days before "ep" */
		sunref(&sun0, t0);
		sunref(&sun1, ep + 4320.);	/* 3 days after "ep" */
		dx = sun1.x - sun0.x;
		dy = sun1.y - sun0.y;
		dz = sun1.z - sun0.z;
		t = .5;
	}
	/* get position by linear interpolation in the 6-day period */
	pos->x = sun0.x + t * dx;
	pos->y = sun0.y + t * dy;
	pos->z = sun0.z + t * dz;
}


STATIC double
sunlon(epoch)
double epoch;
/* Return geometric mean longitude of sun at "epoch", accurate to about .1
deg.  Formulas adapted from Meeus.  The returned value may be off by a small
multiple of 2pi.  That should cause no problem since in this program the
longitude is only used by sin() & cos(). */
{
	static double e = .016709;	/* eccentricity year 2000 */

	double t, l, m, enew, eold, v;

	t = epoch - 3477628800.;	/* since 1900 Jan 0 12h */

	/* geometric mean long. */
	l = fmod2p(4.881628 + t * 1.1946383e-5);

	/* mean anomaly */
	m = fmod2p(6.256584 + t * 1.1945812e-5);

	/* solve Kepler's equation to .1 deg. */
	enew = m;
	do {
		eold = enew;
		enew = m + e * sin(eold);
	} while (FABS(enew - eold) >= 1.7e-3);

	/* tan is asymptotic at pi/2, so if eccentric anomaly is within .1
	deg of pi radians, we'll just make true anomaly = pi. */
	if (FABS(enew - pi) <= 1.7e-3)
		v = pi;
	else
		v = 2. * atan(sqrt((1. + e) / (1. - e)) * tan(enew / 2.));

	return l + v - m;
}


STATIC void
sunref(pos, ep)
struct vector *pos;
double ep;
/* Put the mean equatorial xyz components of a unit vector to sun at
epoch "ep" into struct "pos".  Good to about .1 deg. */
{
	double lon, sinlon;

	lon = sunlon(ep);
	sinlon = sin(lon);

	pos->x = cos(lon);
	pos->y = sinlon * coseps;
	pos->z = sinlon * sineps;
}


double
thetag(ep)
double ep;	/* epoch */
/* Returns Greenwich hour angle of the mean equinox at ep.  If ep is UT1,
returned value is within .001 sec of time compared to the formula in the '85
Astronomical Almanac, for 1990 through 2010.  As a side effect, this function
sets ds50 to days since 1950 Jan 0 0h UT. */
{
	double theta;

	ds50 = (ep - 3503925360.) / xmnpda;
	theta = .27524987 + 1.00273790935 * ds50;	/* revolutions */

	return (theta - dint(theta)) * twopi;
}


STATIC void
topos()
/* Compute rectangular coordinates of observer on the ellipsoid.
Formulas from Baker & Makemson. */
{
	double c, s;
	static double f = FLAT;		/* flattening of earth */

	c = 1. / sqrt(1. - (f + f - f * f) * obs.sinlat * obs.sinlat);
	s = c * (1. - f) * (1. - f);
	obs.xc = (c + obs.h) * obs.coslat;
	obs.zg = -(s + obs.h) * obs.sinlat;
}


int
xyztop(t)
double t;	/* epoch */
/* If satellite is below horizon and aflag = 0, xyztop() returns 0.
If satellite is above horizon or aflag is true, loads globals
elsusa, azel, radec and latlon with appropriate values and returns 1. */
{
	struct vector suneq;	/* equatorial sun position */

	double temp, tempx, tempy, lhaa, sinlha, coslha, testneg;

	int retval;

	lhaa = thetag(t) + obs.lambda;		/* local hr angle Aries */
	sinlha = sin(lhaa);
	coslha = cos(lhaa);

	/* load azel & radec with sat coordinates translated to observer */
	radec.x = azel.x = sat.x - obs.xc * coslha;
	radec.y = azel.y = sat.y - obs.xc * sinlha;
	radec.z = azel.z = sat.z + obs.zg;

	/* Rotate azel into a south/east/zenith system */
	zrot(&azel, sinlha, coslha);
	yrot(&azel, obs.coslat, obs.sinlat);

	if (azel.z < 0. && aflag == 0)
		return 0;	/* below horizon and "all" flag not set */

	azel.x = -azel.x;	/* makes az go correct direction */
	FTEST((200, 3, 4, &azel.x, &azel.y, &azel.z));
	recsph(&azel);		/* convert to spherical */

#if ENPRE	/* precess RA & dec */
	tempx   = radec.x * dircos.ix + radec.y * dircos.jx +
	  radec.z * dircos.kx;
	tempy   = radec.x * dircos.iy + radec.y * dircos.jy +
	  radec.z * dircos.ky;
	radec.z = radec.x * dircos.iz + radec.y * dircos.jz +
	  radec.z * dircos.kz;

	radec.x = tempx;
	radec.y = tempy;
#endif
	FTEST((201, 3, 4, &radec.x, &radec.y, &radec.z));

	sun(&suneq, t);         /* suneq = xyz of sun */

	/* get dot product of observer->sat and observer->sun vectors */
	temp = radec.x * suneq.x + radec.y * suneq.y + radec.z * suneq.z;

	recsph(&radec);		/* convert sat xyz to RA, dec, range */

	/* Illuminated fraction of sat = (1 - cos(i)) / 2, where i is phase
	(i.e., illum. angle) of sat.  Apparent magnitude is a function of
	illuminated fraction, range squared, absolute magnitude, and a scale
	offset factor.  We can simplify considerably by observing that:  1)
	cos(i) = temp / (|sat| * |sun|) (this is a restatement of the
	definition of a dot product), 2) |sat| (range of satellite) is now in
	radec.z, 3) |sun| = 1 since sun() always generates a unit vector.  In
	addition, we can drop the "/ 2", adjusting MAGOFF accordingly. */

	apmag = 2.5 * log10(radec.z * radec.z / (1. - temp / radec.z))
	  + abmag + MAGOFF;

	phase_angle = ra2de * (acos( temp / radec.z));

	 
	/* latitude & longitude */
	latlon.x = sat.x;
	latlon.y = sat.y;
	latlon.z = sat.z;
	recsph(&latlon);	/* convert to spherical coordinates */

	/* figure longitude relative to local meridian or Greenwich,
	depending on greenwich flag. */
	latlon.x = fmod2p(latlon.x - lhaa + (gflag ? obs.lambda : 0.));

	if (latlon.x > pi)
		latlon.x -= twopi;

	latlon.z -= mean_r;	/* mean_r = mean radius of earth */

	/* Get sun elevation at satellite by rotating sun coordinate axes
	so the satellite is on the positive z-axis.  Sun elevation = arc
	sin z.  Add dip of horizon due to height of satellite. */

	point(&suneq, &sat);

	/* Calculate the arg for SQRT so it can be tested for a negative */
	/* value. Return 0 if it is. This stops the program failing with */
	/* a SQRT DOMAIN error and also stops the printing of a negative */
	/* altitude. */

	testneg = (2. + latlon.z) * latlon.z;
	retval = 1;
	if  (testneg < 0.0) {
	    testneg = 0;
	    retval = 0;
	}
	temp = (asin(suneq.z) + atan(sqrt(testneg)))
	  * ra2de;
	FTEST((202, 1, 1, &temp));
	elsusa = temp + ((temp >= 0.) ? .5 : -.5);

	return retval;	/* signifies sat elev >= 0 */
}


STATIC void
yrot(vec, sint, cost)
struct vector *vec;
double sint, cost;
/* y-rotates coordinate axes for "vec" by angle t, where "sint"
and "cost" are the sine and cosine of t. */
{
	double tempz;
	tempz  = vec->x * sint + vec->z * cost;
	vec->x = vec->x * cost - vec->z * sint;
	vec->z = tempz;
}


STATIC void
zrot(vec, sint, cost)
struct vector *vec;
double sint, cost;
/* similar to yrot(), except rotate about z axis */
{
	double tempx;

	tempx  = vec->y * sint + vec->x * cost;
	vec->y = vec->y * cost - vec->x * sint;
	vec->x = tempx;
}
