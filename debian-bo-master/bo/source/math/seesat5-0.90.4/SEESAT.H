/*
SEESAT.H
by Paul S. Hirose, 1992 Jan 4
Declarations & definitions for globals in the SEESAT satellite tracking
program.

This file is in the public domain.
*/

/* If MAIN is defined, this header will generate definitions.  Otherwise,
only declarations will be generated. */

#ifdef MAIN		/* generate DEFINITIONS */
#define CLASS
#define EQUALS(N) = N

#else			/* generate DECLARATIONS */
#define CLASS extern
#define EQUALS(N)
#endif

/*
Miscellaneous Dependencies

If your compiler is on the list below, you should be able to compile by
simply setting the appropriate #define.

STDC = Standard C
ECOC = Ecosoft C 3.5 for CP/M
LASERC = Laser C for Atari ST
TURBOC = Turbo C for MS-DOS
LINUX = Gcc for Linux
*/

#define STDC 1
#define ECOC 0
#define LASERC 0
#define TURBOC 0
#define LINUX 1

#if TURBOC
#define STDC 1
#endif

#include <stdio.h>

#if STDC
#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#endif

#if LASERC
#include <ctype.h>
#include <math.h>
#include <strings.h>
extern double atof(), log10();
#endif

/* CP/M has to use SGP to keep size down.  Everyone else gets to use SGP4.
If you want to use SGP, you'll need to get my code for sgp().  I don't
distribute it as part of the normal SEESAT package, but it is available
on request. */

#if ECOC
#define MODEL(t) sgp(t)
extern void sgp();
#else
#define MODEL(t) sgp4(t)
extern void sgp4();
#endif


/* POW(x, y) raises x to the y power.  Both args are type double. */

#if STDC | ECOC
#define POW(x, y) pow(x, y)
extern double pow();
#endif

#if LASERC
#define POW(x, y) powerd(x, y)
#endif

/* ITOA(s, i) turns int i to a string at the location pointed
to by char pointer s.  The return value from ITOA() is not used. */

#if STDC | LASERC
#define ITOA(s, i) sprintf(s, "%d", i)
#endif

#if ECOC
#define ITOA(s, i) itoa(s, i)
extern char *itoa();
#endif


/* JMPBUF defines (i.e., allocates space for) an object to be used by
setjmp() and longjmp().  SETJMP() and LONGJMP() must expand to calls to the
actual setjmp() and longjmp().  Here's how they're used in SEESAT:

JMPBUF s;	define s as a "jump buffer"
SETJMP(s);	store return location in s
LONGJMP(s, 1);	execute jump to location stored in s, return 1
*/

#if STDC | LASERC
#define JMPBUF jmp_buf
#define SETJMP(s) setjmp(s)
#define LONGJMP(s, i) longjmp(s, i)
#endif

#if ECOC
#define JMPBUF jmp_env
#define SETJMP(s) setjmp(&s)
#define LONGJMP(s, i) longjmp(&s, i)
extern void longjmp();
#endif

/* Nonzero enables precession correction to R.A./dec.  Zero will eliminate
all precession code. */
#define ENPRE 1

/* MALLOC(n) returns a pointer to n bytes of storage, or NULL if insufficient
storage available.  It need not clear the storage it allocates. */

#if STDC | LASERC
#define MALLOC(n) malloc(n)
#endif

#if ECOC
#define MALLOC(n) alloc(n)
#endif


/* VOIDP is the type of arg required by free(). */

#if STDC | ECOC
#define VOIDP void *
#endif

#if LASERC
#define VOIDP char *
#endif


/* FABS(x) returns the absolute value of double x */

#if STDC | ECOC
#define FABS(x) fabs(x)
extern double fabs();
#endif

#if LASERC
#define FABS(x) dabs(x)
#endif


#if LASERC
#ifdef MAIN

double atan2(y, x)
double x, y;
/* convert rectangular coordinates to polar, return angle (-pi to pi) */
{
	double a;
	extern double pi;

	a = atan(y / x);
		if (x < 0.)
		if (y < 0.)		/* 3rd quadrant */
			a -= pi;
		else			/* 2nd quadrant */
			a += pi;
	return a;
}
#else
extern double atan2();
#endif
#endif



/*
Mathematical Constants
*/

CLASS double
	pi EQUALS(3.141592653589793),
	e6a EQUALS(1e-6),
	pio2 EQUALS(1.570796326794897),		/* pi/2 */
	tothrd EQUALS(.6666666666666667),	/* 2/3 */
	twopi EQUALS(6.283185307179586),	/* 2pi */
	x3pio2 EQUALS(4.712388980384690),	/* 3pi/2 */
	de2ra EQUALS(.0174532925199433),	/* radians per degree */
	ra2de EQUALS(57.29577951308232);	/* deg per radian */

/*
Physical Constants
*/

/* dimensions & gravity of earth, World Geodetic System 1972 values */

CLASS double
xj2 EQUALS(1.082616e-3),	/* 2nd gravitational zonal harmonic */
xj3,				/* xj3, xj4 initialized at run time */
xj4,
ck2 EQUALS(5.41308E-4),		/* .5 * xj2 */
ck4 EQUALS(6.2098875E-7),	/* -.375 * xj4 */
xke EQUALS(.743669161e-1),	/* = (G*M)^(1/2)*(er/min)^(3/2) where G =
				Newton's grav const, M = earth mass */

xkmper EQUALS(6378.135),	/* equatorial earth radius, km */
mean_r EQUALS(.998882406),	/* mean radius, in units of xkmper */

/* SGP4/SGP8 density constants.  qoms2t = ((qo - so) / xkmper) ** 4,
s = 1 + so / xkmper, where qo = 120 and so = 78 */

qoms2t EQUALS(1.88027916E-9),
s EQUALS(1.01222928),

xmnpda EQUALS(1440.0);		/* time units/day */

/*
Units & Conventions

Unless the otherwise indicated, throughout this program
quantities are measured in the following units:

time interval		minutes
epoch			minutes since 4713 B.C. Jan 1 12h UT Julian
			proleptic calendar
angle			radians
length			equatorial earth radii (1 unit = xkmper km)

South latitudes are negative.
East longitude is positive, west negative.
Azimuth is measured starting at north, increasing east.
*/

/*
Structures
*/

/* when converted to spherical coordinates the members become e.g. azimuth,
elevation, range, respectively */

struct vector {
	double x, y, z;
};


/* Julian date, time struct.  CAUTION:  jd is the jd at 12h, while time is
measured from 0h, 12h before.  I.e., if s is a jdtim struct, you must
convert to epoch in minutes by doing:  s.jd * 1440. + s.time - 720. */

struct jdtim {
	long int jd;	/* Julian date @ 12h */
	double time;	/* minutes since 0h */
};

/*
Global Data
*/

CLASS double
	/* satellite's orbital elements. */
	xmo,	/* mean anomaly */
	xnodeo,	/* right ascension of ascending node */
	omegao,	/* argument of the perigee */
	eo,	/* eccentricity */
	xincl,	/* inclination */
	xno,	/* mean motion, radians/min */
	xndt2o,	/* 1st time derivative of mean motion, or ballistic
		coefficient (depending on ephemeris type) */
	xndd6o, /* 2nd time derivative of mean motion */
	bstar,	/* BSTAR drag term if GP4 theory was used;
		otherwise, radiation pressure coefficient */
	epoch,	/* epoch of elements */
	abmag,	/* magnitude at standard distance & illumination */

	ds50,	/* days since 1950 (only used for deep space models) */
	tzone,	/* local time - UTC */
	toffs;	/* value of OFFSET command */

/* Satellite geocentric position.  Generated by the prediction model. */
CLASS struct vector sat;

/* The following data come from xyztop() */
CLASS struct vector
	azel,		/* azimuth, elevation, slant range */
	radec,		/* Right Ascension, declination, slant range */
	latlon;		/* longitude, latitude, altitude */
CLASS double apmag;	/* apparent magnitude */
CLASS int elsusa;	/* elev of sun above sat's horizon, degrees */
CLASS int elsuob;       /* elev of sun at observer, degrees */
CLASS int last_date[3]; /* last date entered */
CLASS int reportlevel;  /* Reporting level */
CLASS char max_yyddd[15]; /* Latest satellite epoch when file opened */
CLASS char max_satname[23]; /* and satellite name */
CLASS char min_yyddd[15]; /* Earliest epoch */
CLASS char min_satname[23]; /* and satellite name */
CLASS int def_orbit_mins;   /* Default orbit time */
CLASS double moon_az;       /* Moon azimuth and elevation */
CLASS double moon_el;
CLASS int moon_ill;         /* Moon illumination percentage */
CLASS int phase_angle;      /* Angle between observer-satellite-sun */

/* miscellaneous data */

CLASS JMPBUF reset; /* LONGJMP(reset) returns you to the > prompt */

CLASS int iflag;	/* = 1 with new orbital elements,
			reset to 0 by prediction model on first call */

CLASS char **tokp;	/* pointer to command line tokens */

CLASS char name[23];	/* of satellite */
CLASS unsigned int norad_sat_nbr;

CLASS char	/* flags */
	aflag,		/* show data for predictions below horizon too */
	gflag EQUALS('\001'),	/* if true, display long. with respect to
				Greenwich if false, long. is with respect
				to local meridian */
	mflag;		/* absolute mag. was obtained from element file
			or by manual input */

CLASS int showtle; /* 1 = print keplerian data or not */

/*
Functions
*/


/* ASTRO.C */

extern void dusk();
	/* prints azimuth & elevation of sun at observer. */

extern double fmod2p();
	/* Returns arg modulo 2pi */

extern void inpre();
	/* initializes the precession rotation matrix */

extern void moon();
	/* prints az, el, % of illum of moon */

extern void parall();
	/* prints the parallactic angle at the sat */

extern void setep();
	/* sets new terminal epoch for precession */

extern void seth();
	/* set observer's height */

extern void setlat();
	/* set latitude */

extern void setlon();
	/* set longitude */

extern double thetag();
	/* Returns Greenwich hour angle of Aries */

extern int xyztop();
	/* computes topocentric coordinates of sat, return 1 if data should be
	printed. */

/* DRIVER.C */

extern void tok();
	/* get next command line, tokenize it */


/* READEL.C */

extern void aop();
	/* manually input argument of perigee */
extern void b();
	/* manually input BSTAR */
extern void ein();
	/* manually input eccentricity */
extern void epoc();
	/* manually input epoch of elements */
extern int getlin();
	/* get a line from a file */
extern void hfree();
	/* frees all heap storage */
extern void inc();
	/* manually input inclination */
extern void indx();
	/* lists satellites in the open element file */
extern void load();
extern void loadn();
	/* Loads orbital elements from the open element file */
extern void dbs();
extern void dbsn();
	/* Sets db flag in satellite table */
extern void ma();
	/* manually input mean anomaly */
extern void mm();
	/* manually input mean motion */
extern void next();
	/* load the next sat */
extern void opn();
	/* opens element file */
extern void raan();
	/* manually input Right Ascension of ascending node */
extern void setact();
	/* set actual time of launch */
extern void setlen();
	/* set max length of sat name */
extern void setnam();
	/* manually input sat name */
extern void setnd();
	/* manually input 1st derivative of mean motion */
extern void setndd();
	/* manually input 2nd derivative of mean motion */
extern void setnom();
	/* set nominal time of launch */
extern void summary();
	/* Prints latest epoch in TLE file */

/* UTIL.C */

extern double atomin();
	/* convert string to minutes */

extern char **degdms();
	/* convert double to deg, minutes, seconds strings */

extern char *jdstr();
	/* converts jd to year, month, day in string form */

extern long int julday();
	/* returns JD (unit = days) of given year, month, day @ 12h */

extern char *stoup();
	/* converts sting to all upper case */

extern char *timstr();
	/* converts minutes to string of "hhmm:ss" format */

extern void tokjum();
	/* converts date/time group on command line to JD & minutes */

extern double tokmin();
	/* converts date/time group on cmd line to epoch in minutes */




/*
Debugging
*/

/* DEBUG is controlled in the file that #includes this file. */

#ifdef DEBUG
#define DTEST(a) ddebug a
#define ETEST(a) edebug a
#define FTEST(a) fdebug a
#define STEST(a) sdebug a
extern void ddebug(), edebug(), fdebug(), sdebug();

#ifdef MAIN

/* Debugging functions.  This stuff is only activated when
SEESAT.C is compiled with DEBUG on.  Each function printf()s a
variable number of arguments of a particular type.  If the bp
argument passed to the function matches variable stopat, the
arguments are printf() all on the same line.  If bp != stopat, no
action occurs.  The arguments to be printed are passed by
reference, and the number of arguments is given by argc. */

static int stopat;		/* currently active break point */

void
ddebug(bp, argc, arg1)
int bp, argc, *arg1;
{
	if (bp == stopat) {
		int **argptr;
		printf("%d: ", bp);
		for (argptr = &arg1; argc ; ++argptr, --argc)
			printf("%d ", **argptr);
		getbp();
}	}

void
edebug(bp, argc, prec, arg1)
int bp, argc, prec;
double *arg1;
{
	if (bp == stopat) {
		double **argptr;
		printf("%d: ", bp);
		for (argptr = &arg1; argc; ++argptr, --argc)
			printf("%.*e ", prec, **argptr);
		getbp();
}	}

void
fdebug(bp, argc, prec, arg1)
int bp, argc, prec;
double *arg1;
{
	if (bp == stopat) {
		double **argptr;
		printf("%d: ", bp);
		for (argptr = &arg1; argc; ++argptr, --argc)
			printf("%.*f ", prec, **argptr);
		getbp();
}	}

void
sdebug(bp, arg)
int bp;
char *arg;
{
	if (bp == stopat) {
		printf("%d: \"%s\"\n", arg);
		getbp();
}	}

static void
getbp()
/* ask for next breakpoint, put value in stopat */
{
	char buffer[7];
	printf("\nnext breakpoint? ");
	stopat = atoi(gets(buffer));
}

#endif

#else
/* Debugging is turned off, so expand the macros to nothing */
#define DTEST(a)
#define ETEST(a)
#define FTEST(a)
#define STEST(a)

#endif
