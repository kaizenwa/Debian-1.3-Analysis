/*
DRIVER.C
by Paul S. Hirose, 1992 Jan 4
main module for the SEESAT satellite tracking program.

This file is in the public domain.

0 - 99

*/

#include "SEESAT.H"	/* global header */

/* At startup, SEESAT will execute the commands in AUTOEX */
#define AUTOEX "SEESAT5.INI"

/* library functions and #defines used in this file:
	atof() exit() fclose() FILE
	fopen() getchar() isspace() NULL printf() strcmp()
*/

#if ECOC
extern void exit(), printf();
extern FILE *fopen();
extern double atof();
#endif


/* The HLTRUN macro is placed in the command interpreter loop in main(), and
in the prediction loop in run().  HLTRUN allows you to easily install an
abort feature for your system.  I recommend doing the abort itself with
LONGJMP(reset, 1).  This will return you to the command line and turn off the
batch flag, from anywhere in the program.  The aborts I've provided for Laser
C and Turbo C are activated by pressing any key. */

#if ECOC
#define HLTRUN
#endif

#if LASERC
#include <osbind.h>
#define HLTRUN\
	if (Bconstat(2)) {\
		Bconin(2);\
		LONGJMP(reset, 1);\
	}
#endif

#if TURBOC
#include <dos.h>
#include <conio.h>
#define HLTRUN\
	if (bioskey(1)) {\
		bioskey(0);\
		LONGJMP(reset, 1);\
	}
#endif

#if LINUX
#include <time.h>
#define HLTRUN
#endif

/*##################### STATIC FUNCTIONS #####################*/

static void
	bye(), center(), exbat(),
	help(), ifprn(),
	offset(), prnval(), rep(), ret(),
	run(), sethor(), setmag(), setmb(), setmer(),
	settz(), setvismag(), setelsusa_limit(),
	setminelev(), setmaxelev(), setelsuob_limit(), setminphase(),
	skip(), span(),
	start(), step(), stop(), xrun(),
	runall(), startday(), define_label(), goto_label(),
	cmdline(), save_block(),
	runtime(), set_orbit_mins(), set_print_limit(), set_linefeed(),
	set_options(), reset_options(), stopday(), rundbs(),
	set_min_range(), set_max_range(),
	do_null_func();

static char *s_in();

/*############################## DATA ##############################*/

static double
	tf,	/* end of prediction run */
	delt,	/* time interval between predictions */
	tspan; 	/* length of run */

static float magbias;	/* to be applied to apparent magnitude */
static float vismag = 9999.9; /* default value for visual magnitude */
static int elsusa_limit = -9999;    /* default value for SUN */
static int elsuob_limit = 99; /* default value for sun elevation */
static int minelev = -999;    /* default value for minimun elevation */
static int maxelev = 99;      /* default maximum elevation */
static int minphase = -9999;  /* default minimum phase angle */
static int print_limit = 60;  /* default limit of predictions to print for */
			      /* each satellite when in runtime mode */
static unsigned int max_range = 65535; /* Maximum range default */
static unsigned int min_range = 0;     /* Minimum range default */
static int linefeed_value = 1; /* Linefeeds after sat data print */
static int shownoradnbr = 0;   /* Show Norad satellite number option */
static int show_tle_age = 0;   /* Show age of elements option */

struct block_strtr { unsigned int baz:9, eaz:9, el:7; };

static struct block_strtr block_data[30];
static int block_max = -1;

struct jdtim t1;	/* CENTER & START values share this */

struct comd {
	char *name;		/* name of command */
	void (*funp)();		/* ptr to function that executes it */
};

static struct comd comds[] = {
	{"STEP", step},
	{"step", step},
	{"START", start},
	{"start", start},
	{"STOP", stop},
	{"stop", stop},
	{"SPAN", span},
	{"span", span},
	{"CENTER", center},
	{"center", center},
	{"RUN", run},
	{"run", run},
	{"RUNTIME", runtime},
	{"runtime", runtime},
	{"RUNDBS", rundbs},
	{"rundbs", rundbs},

	{"LOAD", load},		/* READEL.C */
	{"load", load},		/* READEL.C */
	{"LOAD#", loadn},       /* READEL.C */
	{"load#", loadn},       /* READEL.C */
	{"DBS", dbs},           /* READEL.C */
	{"dbs", dbs},           /* READEL.C */
	{"DBS#", dbsn},         /* READEL.C */
	{"dbs#", dbsn},         /* READEL.C */
	{"OPEN", opn},		/* READEL.C */
	{"open", opn},		/* READEL.C */

	{"INDEX", indx},	/* READEL.C */
	{"index", indx},	/* READEL.C */
	{"NEXT", next},		/* READEL.C */
	{"next", next},		/* READEL.C */

	{"SUN", dusk},		/* ASTRO.C */
	{"sun", dusk},		/* ASTRO.C */
	{"MOON", moon},		/* ASTRO.C */
	{"moon", moon},		/* ASTRO.C */
	{"PARA", parall},	/* ASTRO.C */
	{"para", parall},	/* ASTRO.C */

	{"PRINT?", ifprn},
	{"print?", ifprn},
	{"REPEAT", rep},
	{"repeat", rep},
	{"SKIP", skip},
	{"skip", skip},
	{"EX", exbat},
	{"ex", exbat},
	{"RET", ret},
	{"ret", ret},

	{"SUMMARY", summary},   /* READEL.C */
	{"summary", summary},   /* READEL.C */
	{"MAG", setmag},
	{"mag", setmag},
	{"MAGBIAS", setmb},
	{"magbias", setmb},
	{"MINELEV", setminelev},
	{"minelev", setminelev},
	{"MAXELEV", setmaxelev},
	{"maxelev", setmaxelev},
	{"MINPHASE", setminphase},
	{"minphase", setminphase},
	{"OFFSET", offset},
	{"offset", offset},

	{"LAT", setlat},	/* ASTRO.C */
	{"lat", setlat},	/* ASTRO.C */
	{"LON", setlon},	/* ASTRO.C */
	{"lon", setlon},	/* ASTRO.C */
	{"HEIGHT", seth},	/* ASTRO.C */
	{"height", seth},	/* ASTRO.C */
	{"VISMAG", setvismag}, /* visual magnitude value */
	{"vismag", setvismag}, /* visual magnitude value */
	{"SUNELEVSAT", setelsusa_limit}, /* suppress data if sun <= value */
	{"sunelevsat", setelsusa_limit}, /* suppress data if sun <= value */
	{"SUNELEVOBS", setelsuob_limit}, /* suppress data for sun's elevation */
	{"sunelevobs", setelsuob_limit}, /* suppress data for sun's elevation */
	{"ZONE", settz},
	{"zone", settz},
#if ENPRE
	{"PRECESS", setep},	/* ASTRO.C */
	{"precess", setep},	/* ASTRO.C */
#endif
	{"LENGTH", setlen},	/* READEL.C */
	{"length", setlen},	/* READEL.C */
	{"MERIDIAN", setmer},
	{"meridian", setmer},
	{"ALL", sethor},
	{"all", sethor},
#if ECOC == 0
	{"HELP", help},
	{"help", help},
#endif
	{"EXIT", bye},
	{"exit", bye},
	{"BYE", bye},
	{"bye", bye},
	{"RUNALL", runall},
	{"runall", runall},
	{"NULL", do_null_func},
	{"null", do_null_func},
	{"TODAY", startday},
	{"today", startday},
	{"STARTDAY", startday},
	{"startday", startday},
	{"STOPDAY", stopday},
	{"stopday", stopday},
	{"LABEL", define_label},
	{"label", define_label},
	{"GOTO", goto_label},
	{"goto", goto_label},
	{"GO", goto_label},
	{"go", goto_label},
	{"CMDLINE", cmdline},
	{"cmdline", cmdline},
	{"BLOCK", save_block},
	{"block", save_block},
	{"ORBITMINS", set_orbit_mins},
	{"orbitmins", set_orbit_mins},
	{"PRINTLIMIT", set_print_limit},
	{"printlimit", set_print_limit},
	{"LINEFEED", set_linefeed},
	{"linefeed", set_linefeed},
	{"SET", set_options},
	{"set", set_options},
	{"RESET", reset_options},
	{"reset", reset_options},
	{"MAXRANGE", set_max_range},
	{"maxrange", set_max_range},
	{"MINRANGE", set_min_range},
	{"minrange", set_min_range},
	{"AOP", aop},		/* READEL.C */
	{"aop", aop},		/* READEL.C */
	{"B", b},		/* READEL.C */
	{"b", b},		/* READEL.C */
	{"E", ein},		/* READEL.C */
	{"e", ein},		/* READEL.C */
	{"EPOCH", epoc},	/* READEL.C */
	{"epoch", epoc},	/* READEL.C */
	{"I", inc},		/* READEL.C */
	{"i", inc},		/* READEL.C */
	{"MA", ma},		/* READEL.C */
	{"ma", ma},		/* READEL.C */
	{"MM", mm},		/* READEL.C */
	{"mm", mm},		/* READEL.C */
	{"MMDOT", setnd},	/* READEL.C */
	{"mmdot", setnd},	/* READEL.C */
	{"MMDOTDOT", setndd},	/* READEL.C */
	{"mmdotdot", setndd},	/* READEL.C */
	{"NAME", setnam},	/* READEL.C */
	{"name", setnam},	/* READEL.C */
	{"RAAN", raan},		/* READEL.C */
	{"raan", raan},		/* READEL.C */
	{"ACTUAL", setact},	/* READEL.C */
	{"actual", setact},	/* READEL.C */
	{"NOMINAL", setnom},	/* READEL.C */
	{"nominal", setnom},	/* READEL.C */
	{NULL}
};

static char *nullp = NULL;	/* constant */
static char **ptok0;		/* points to start of cmd line */

static FILE *batfp;		      /* batch file */

/* flags; set/reset by the CENTER, SPAN, START, STOP commands */
static char fcenter, fspan, fstart, fstop;

/* this flag used by autom(), set by run() when a prediction is printed */
static char fvis;

static char fbatch;	/* batch mode flag */

char vers[] = {"5.0g"};

char cmdline_buff[121];
int cmdline_ct;
int get_dclp;           /* Make tok() get DOS command line parameters */
int showbatcmds;        /* Suppresses .INI file lines from printing during */
			/* a GOTO commands label searching  routine        */

double distance_units = 1.0;

/*############################## CODE ##############################*/

void main (int argc, char **argv)
{
	struct comd *comdp;
	char *cp1, *cp2;


	ETEST((0, 1, 3, &pi));	/* dummy call to get control of brkpt */

	printf("SEESAT5 by K.S.Kalirai (version %s)\n", vers);

	/* Ecosoft C doesn't allow negative initializers! */
	xj3 = -.253881e-5;
	xj4 = -1.65597e-6;

	delt = 1.0;
	showtle = 1;
	showbatcmds = 1;
	reportlevel = 0;
	get_dclp = 0;
	def_orbit_mins = 60;

	if (argc > 1) {        /* found command line parameters */
	   cmdline_ct = 1;
	   cmdline_buff[0] = '\000';
	   do {
	      if (!strcmp(argv[cmdline_ct],"report") || !strcmp(argv[cmdline_ct],"REPORT")) {
		 reportlevel = 1;
	      }
	      strcat(cmdline_buff,argv[cmdline_ct]);
	      strcat(cmdline_buff," ");
	      cmdline_ct++;
	   } while (cmdline_ct < argc);
	}

	/* Attempt to open the auto-execute batch file.  Setting fbatch will
	redirect command input to the file. */

	if (batfp = fopen(AUTOEX, "r"))
		fbatch = '\001';	/* set batch flag */

	/* Execution is vectored back here for all errors */
	if (SETJMP(reset))
	/* returned from a longjmp(); turn off batch file flag */
		fbatch = '\000';

	tokp = &nullp;	/* clear command line */

	for (;;) {	/* main loop */

		readnext:

		HLTRUN				/* optional abort macro */

		while (*tokp == NULL) {		/* cmd line exhausted */
			tok();
			ptok0 = tokp;
		}
		cp1 = *tokp;		/* point to first command */

		if (cp1[0] == '/') {
		   *tokp = NULL;
		   goto readnext;
		}

		/* Find the command in the comds[] table, execute the
		corresponding function */

		for (comdp = comds; (cp2 = comdp->name) && strcmp(cp1, cp2);
		  ++comdp)
			printf("");

		if (cp2) {	/* found command; execute */
			++tokp;		/* point to first arg */
			(*comdp->funp)();
			++tokp;		/* point to next command */
		} else {
			printf("%s: BAD COMMAND\n", cp1);
			LONGJMP(reset, 1);
		}
	}
}

static void bye()
{
	hfree();	/* release index space */
	printf("\n========================================");
	printf("========================================");
	exit(0);
}

static void center()
{
	tokjum(&t1);		/* load t1 with CENTER value */
	fcenter = '\001';
	fstop = fstart = '\000';	/* start & stop flags */
	xrun();
}

static void startday()
{
#if TURBO
	struct date dosdate;
#endif
#if LINUX
	time_t jtime;
#endif
	int plus_days, today;
	plus_days = atoi(*tokp);
	tokp++;
	
	t1.time = atomin(*tokp);

/* printf("\nPlus_days = %i   t1.time = %f",plus_days,t1.time); */
#if TURBO
	/* Get system date */
	getdate(&dosdate);

	/* Load todays julian date */
	today = julday(dosdate.da_year, dosdate.da_mon - 1, dosdate.da_day);
#endif
#if LINUX
	/* Get seconds since 1970 */
	jtime = time(NULL);
/* printf("jtime = %i ", (int)jtime); */
	/* Load todays julian date */
	today = 2440588 + (int)(((long)jtime)/((long)86400));
/* printf("  today = %i ", today); */
/* printf(" %i-%i-%i\n",month_of_jd(today),day_of_jd(today),year_of_jd(today)); */
#endif
	t1.jd = today + plus_days;

	/* Also set the default last-date values to todays date plus */
	if  (plus_days != 0) {
	    last_date[0] = year_of_jd(t1.jd);
	    last_date[1] = month_of_jd(t1.jd) - 1;
	    last_date[2] = day_of_jd(t1.jd);
	} else {
	    last_date[0] = year_of_jd(today);
	    last_date[1] = month_of_jd(today) - 1;
	    last_date[2] = day_of_jd(today);
	}

	fstart = '\001';
	fcenter = '\000';
}

static void stopday()
{
#if TURBO
	struct date dosdate;
#endif
#if LINUX
	time_t *jt;
	time_t jtime;
#endif
	int stop_days, today;
	long int stop_jd;
	double stop_time;

	stop_days = atoi(*tokp);
	if  (stop_days > 180) {
	    printf("DAYS ARE GREATER THAN 180 IN STOPDAY COMMAND");
	    LONGJMP(reset, 1);
	}

#if TURBO
	/* Get system date */
	getdate(&dosdate);

	/* Load todays julian date */
	today = julday(dosdate.da_year, dosdate.da_mon - 1, dosdate.da_day);
#endif
#if LINUX
	/* Get seconds since 1970 */
	jtime = time(jt);
	
	/* Load todays julian date */
	today = 2440587 + (((int)jtime)/86400);
#endif
	stop_jd = today + stop_days;

	/* Also set the default last-date values to stop date plus */
	if  (stop_days > 0) {
	    last_date[0] = year_of_jd(t1.jd);
	    last_date[1] = month_of_jd(t1.jd) - 1;
	    last_date[2] = day_of_jd(t1.jd);
	} else {
	    last_date[0] = year_of_jd(today);
	    last_date[1] = month_of_jd(today) - 1;
	    last_date[2] = day_of_jd(today);
	}

	tokp++;
	stop_time = atomin(*tokp);

	tf = stop_jd * xmnpda - 720. + stop_time - tzone;

	fstop = '\001';
	fcenter = '\000';
}

static void
exbat()
/* Execute as a batch file the next token on the command line */
{
	if (batfp)	/* a batch file is already open */
		fclose(batfp);

	if ((batfp = fopen(*tokp, "r")) == NULL) {
		printf("CAN'T OPEN %s\n", *tokp);
		LONGJMP(reset, 1);
	}
	fbatch = '\001';
}


#if ECOC == 0

static void
help()
{
printf("SEESAT5 - The Satellite Tracking Program (v. %s)\n",vers);
printf("Location:\n");
printf(" LAT       LON       HEIGHT    ZONE      MERIDIAN  PRECESS\n");
printf(" BLOCK     \n");
printf("Options: SET or RESET\n");
printf(" SHOWTLE   REPORT    DBS\n");
printf("Date and Time\n");
printf(" TODAY     STOPDAY   START     STOP      CENTER    CENTRE\n");
printf("Predictions:\n");
printf(" RUNTIME   RUNDBS    RUNALL    RUN\n");
printf(" NEXT      PRINT?    REPEAT    SKIP      RET\n");
printf(" STEP      SPAN      OFFSET\n");
printf("File:\n");
printf(" OPEN      EX        INDEX     SUMMARY\n");
printf(" LENGTH    EPOCH\n");
printf("Satellite:\n");
printf(" LOAD      LOAD#     DBS       DBS#      \n");
printf(" AOP       B         E         I         MA        MM        MMDOT\n");
printf(" MMDOTDOT  RAAN      MAG       MAGBIAS\n");
printf("Conditions:\n");
printf(" SUNELEVSAT SUNELEVOBS MINELEV MINPHASE  VISMAG    ALL\n");
printf(" Use RESET to reset above conditions\n");
printf("Astronomical\n");
printf(" PARA      MOON      SUN       ACTUAL    NOMINAL\n");
printf("Control:\n");
printf(" EXIT      LABEL     GO        GOTO      CMDLINE    \n");
--tokp;		/* token pointer */
}

#endif

static void ifprn()
/* Causes next command to be executed if the print flag is true.  Else,
skip next command. */
{
	if (fvis)
		--tokp;
}

static void offset()
/* Apply a time offset to epoch of elements.  Do not set iflag since
initialization is independent of time. */
{
	toffs = atomin(*tokp);
}

static void moon_str()
{
	printf("  Moon  Az %3d El %2d %3d%%",
	       (int) (moon_az * ra2de + .5),
	       (int) (moon_el * ra2de + .5),
	       moon_ill);
}

static void prntitle()
{
	printf("   Time   Az El");
	if  (block_max > -1) {
	    printf(" B");
	}
	printf("  R.A.    Dec   Range SES SEO Phs ");
	if  (moon_el > 0.0) {
	    printf("MAD ");
	}
	printf("R Mag   Alt  Lat    Lon\n");
}

static int check_blocks(in_az, in_el)
int in_az, in_el;
{
	int block_curr;

	for (block_curr = 0 ; block_curr <= block_max ; block_curr++) {
	    if  (   (in_az >= block_data[block_curr].baz)
		 && (in_az <= block_data[block_curr].eaz)
		 && (in_el <= block_data[block_curr].el )
		) {
		return 1;
	    }
	}
	return 0;
}

static void prnval(time)
double time;	/* local time */
/* All tabular data from a prediction run is printed here.  Except for local
time of day, all data comes from global variables in SEESAT.H. */
{
	char **cpp;
	int moon_angle;


	/* print a + sign if the line about to be printed */
	/* satifies the selection conditions */
	if  (   ((apmag + magbias) <= vismag)
	     && (elsusa >= elsusa_limit)
	     && ((int) (azel.y * ra2de + .5) >= minelev)
	     && ((int) (azel.y * ra2de + .5) <= maxelev)
	     && (elsuob <= elsuob_limit)
	     && (phase_angle >= minphase)
	     && ( (long int) ((radec.z * xkmper) * distance_units)
		  <= max_range)
	     && ( (long int) ((radec.z * xkmper) * distance_units)
		  >= min_range)
	    ) printf("+");
	else
	      printf(" ");

	/* time, azimuth, elevation. */
	printf("%s %3d%3d ", timstr(time),
	  (int) (azel.x * ra2de + .5),
	  (int) (azel.y * ra2de + .5));

	if  (block_max > -1) {
	    if  (check_blocks( (int) (azel.x * ra2de + .5),
			       (int) (azel.y * ra2de + .5)
			     )
			       == 1)
		printf("b ");
	    else
		printf("  ");
	}

	/* Right Ascension, declination. */
	cpp = degdms(2, radec.x / twopi * 24.);
	printf("%2sh%2sm ", cpp[0], cpp[1]);
	cpp = degdms(1, radec.y * ra2de);
	printf("%3s %2s' ", cpp[0], cpp[1]);

	/* slant range, sun elevation at satellite */
	printf("%5ld %3d ", (long int) ((radec.z * xkmper) * distance_units),
		     elsusa);

	/* sun's elevation at observer */
	printf("%3d%4d", elsuob, phase_angle);

	/* Angle between Moon and Satellite */
	if  (moon_el > 0.0) {
	    moon_angle = (int) (ra2de *
				acos(  (sin(azel.y) * sin(moon_el))
				     + (  cos(azel.y) * cos(moon_el)
					* cos(azel.x - moon_az)
				       )
				    )
				+ .5
			       );
	    printf("%4d",moon_angle);
	}

	/* apparent magnitude.  Flag relative mag with '*'. */
	printf(" %c%4.1f ", mflag ? ' ' : '*', apmag + magbias);

	/* altitude */
	printf("%5ld ", (long int) ((latlon.z * xkmper) * distance_units));

	/* latitude */
	printf("%4.1f%c ", FABS(latlon.y) * ra2de,
	  (latlon.y >= 0.) ? 'N' : 'S');

	/* longitude */
	printf("%5.1f%c\n", FABS(latlon.x) * ra2de,
	  (latlon.x >= 0.) ? 'E' : 'W');
}

static void
rep()
/* causes re-execution of the command line */
{
	tokp = ptok0 - 1;
}

static void
ret()
/* toggle the batch mode flag, disregard rest of command line, get next
command line */
{
	if (fbatch)
		fbatch = '\000';
	else
	/* set fbatch only if a batch file is open */
		fbatch = batfp != NULL;

	tok();
	--tokp;
}

static void run()
/* Execute prediction run */
{
	double tsince, mins, temp;
	double ah_tsince, ah_mins;
	long int jd;
	long int ah_jd;
	unsigned int count;	/* no. of predictions */
	unsigned int ah_count;
	static char mode;
	int printed_run_heading = 0;
	int new_date_flag = 0;
	int ah_values;

	fvis = '\000';		/* clear "visible flag" */

	if (xno == 0.) {
		printf("NO ELEMENTS\n");
		LONGJMP(reset, 1);
	}

	/* jd & mins are local time */
	jd = t1.jd;
	mins = t1.time;
	/* set mode */
	if (fstart && fstop)
		mode = 1;
	else if (fspan)
		if (fstart)
			mode = 3;
		else if (fcenter)
			mode = 2;

	fstart = fstop = fspan = fcenter = '\000';

	/* tsince = CENTER or START time (whichever was given last) */
	tsince = t1.jd * xmnpda - 720. + t1.time - tzone;
	/* establish the parameters of the run, according to current mode */
	switch (mode) {
	case 1:		/* start stop */
		temp = tf - tsince;		/* length of run */
		if (temp < 0.) {
			printf("STOP < START\n");
			LONGJMP(reset, 1);
		}
		break;
	case 2:
		temp = .5 * tspan;
		mins -= temp;
		while (mins < 0.) {	/* start on previous day */
			mins += xmnpda;
			--jd;
		} tsince -= temp;	/* was initially CENTER value */
		/* fall thru to next case */
	case 3:		/* start span */
		temp = tspan;
		break;
	default:
		printf("NOT ENOUGH RUN PARAMETERS\n");
		LONGJMP(reset, 1);
	}

	if (delt == 0.) {		/* zero span */
		printf("STEP = 0\n");
		LONGJMP(reset, 1);
	}
	count = temp / delt + 1.95;	/* no. of predictions */

	tsince -= epoch;
	ah_values = 0;

redo:

do  {
		HLTRUN
		if (mins >= xmnpda) {		/* crossed midnight */
			mins -= xmnpda;
			++jd;
		}
		MODEL(tsince - toffs);	/* call prediction model */
		if  (xyztop(tsince + epoch)) {
			if  (ah_values == 0) {
				ah_tsince = tsince;
				ah_count = count;
				ah_mins = mins;
				ah_jd = jd;
				ah_values = 1;
			}
			else {
				ah_values = 1;
			}
		}
		else {
			ah_values = 0;
		}

		elsuob = sun_elev(tsince + epoch);
		if  (   (xyztop(tsince + epoch))
				&& ((apmag + magbias) <= vismag)
				&& (elsusa >= elsusa_limit)
				&& ((int) (azel.y * ra2de + .5) >= minelev)
				&& ((int) (azel.y * ra2de + .5) <= maxelev)
				&& (phase_angle >= minphase)
				&& (elsuob <= elsuob_limit)
				&& (check_blocks( (int) (azel.x * ra2de + .5),
						  (int) (azel.y * ra2de + .5)
						) == 0)
				&& ( (long int) ((radec.z * xkmper) * distance_units)
				     <= max_range)
				&& ( (long int) ((radec.z * xkmper) * distance_units)
				     >= min_range)
				) {
				/* above horizon and satisfies conditions */
				tsince = ah_tsince;
				count = ah_count;
				mins = ah_mins;
				jd = ah_jd;
				break;
		} mins += delt;
		tsince += delt;
	} while (--count);

if  (count) {

	printed_run_heading = 0;

	do {
		HLTRUN		/* optional run abort function */

		if (mins >= xmnpda) {		/* crossed midnight */
			mins -= xmnpda;
			++jd;
			new_date_flag = 1;
		}
		FTEST((1, 1, 2, &tsince));
		MODEL(tsince - toffs);	/* call prediction model */
		DTEST((2, 1, &elsusa));
		if (xyztop(tsince + epoch)) {
			/* above horizon ; print line of data */
			FTEST((3, 1, 2, &mins));
			if (printed_run_heading == 0) {
				printed_run_heading = 1;
				new_date_flag = 0;
				printf("\n%s", name); /* satellite name */
				if (shownoradnbr == 1) {
				   printf("   (#%5d)", norad_sat_nbr);
				}
				printf("   %s", jdstr(jd));	/* start date */
				if (show_tle_age == 1) {
				   printf("   TleAge %3.1f days",
					 (((t1.jd * xmnpda - 720. + t1.time
					       - epoch) / 60.0) / 24.0));
				}
				if (toffs)
					printf("   offset = %s", timstr(toffs));
				moon_data(jd * xmnpda - 720. + mins - tzone);
				if  (moon_el > 0.0) {
				    moon_str();
				}

				printf("\n");
				prntitle();
			}
			if  ( (printed_run_heading == 1)
			&&    (new_date_flag == 1)
				) {                         /* next days date */
				new_date_flag = 0;
				printf("%s", name);      /* satellite name */
				if (shownoradnbr == 1) {
				   printf("   (#%5d)", norad_sat_nbr);
				}
				printf("   %s", jdstr(jd));  /* print new date */
				if (show_tle_age == 1) {
				   printf("   TleAge %3.1f days",
					 (((t1.jd * xmnpda - 720. + t1.time
					       - epoch) / 60.0) / 24.0));
				}
				if  (toffs)
				    printf("   offset = %s", timstr(toffs));
				moon_data(jd * xmnpda - 720. + mins - tzone);
				if  (moon_el > 0.0) {
				    moon_str();
				printf("\n");
				}
			}

			moon_data(jd * xmnpda - 720. + mins - tzone);
			elsuob = sun_elev(tsince + epoch);
			prnval(mins);
			fvis = '\001';	/* set visible flag */
		}
		else
		{
		  break;
		 }
		mins += delt;
		tsince += delt;
	} while (--count);
}

if  (count)
	goto redo;

--tokp;		/* because this cmd takes no args */

}

static char *s_in(prompt, buffer)
char *prompt, *buffer;
/* Prints prompt[] on console, puts typed string in buffer[].  Backspacing
will correct typing mistakes.  Will not allow backspacing past the start of
buffer.  Returns pointer to buffer. */
{
	static char c, *ptr, last;

	for (ptr = prompt; *ptr; ++ptr)
		;	/* point to terminating null of prompt */
	last = *(ptr - 1);		/* fetch char before the null */

	printf("%s", prompt);
	ptr = buffer;
	while ((c = getchar()) != '\n')
		if (c == '\b')
			if (ptr == buffer)
				printf("%c", last);
			else
				--ptr;
		else
			*ptr++ = c;

	*ptr = '\0';
	return buffer;
}

static void
sethor()
/* toggles the flag that suppress printout of predictions below horizon */
{
	aflag = !aflag;
	--tokp;		/* because this cmd takes no args */
}

static void
setmag()
/* set absolute magnitude */
{
	abmag = atof(*tokp);
	mflag = '\001';		/* set magnitude flag */
}

static void
setmb()
/* set magnitude bias */
{
	magbias = atof(*tokp);
}

static void
setmer()
/* toggles flag that selects Greenwich or local meridian for longitude
readout. */
{
	printf("%s meridian\n", (gflag = !gflag) ? "Greenwich" : "local");
	--tokp;		/* because this cmd takes no args */
}


static void
settz()
/* sets time zone (local time - UTC) */
{
	tzone = atomin(*tokp);
}

static void setvismag()
/* set visual magnitude limit */
{
	vismag = atof(*tokp);
}

static void setelsusa_limit()
{
	elsusa_limit = atoi(*tokp);
}

static void setelsuob_limit()
{
	elsuob_limit = atoi(*tokp);
}

static void setminelev()
/* set minimum elevation value */
{
	minelev = atoi(*tokp);
}

static void setmaxelev()
/* set maximum elevation value */
{
	maxelev = atoi(*tokp);
}

static void setminphase()
/* set minimum phase angle value */
{
	int work_phase;
	work_phase = atoi(*tokp);
	if  (work_phase < 0) {
	    printf("MINIMUM PHASE SELECTION CAN'T BE < ZERO");
	    LONGJMP(reset, 1);
	}

	if  (work_phase > 180) {
	    printf("MINIMUM PHASE SELECTION CAN'T BE > 180");
	    LONGJMP(reset, 1);
	}

	minphase = work_phase;
}

static void
skip()
/* causes next command to be skipped */
{
}

static void
span()
{
	tspan = atomin(*tokp);
	fspan = '\001';		/* set flag */
	xrun();
}


static void
start()
{
	tokjum(&t1);		/* load struct with START time */
	fstart = '\001';	/* set flag */
	fcenter = '\000';	/* center flag */
	xrun();
}


static void
step()
{
	delt = atomin(*tokp);
	xrun();
}


static void
stop()
{
	tf = tokmin();
	fstop = '\001';		/* set flag */
	fcenter = '\000';	/* center flag */
	xrun();
}

void tok()
/* Inputs a command line from console (or batch file, if fbatch true).
String is converted to upper case.  Each ' ' in the string is replaced with
'\0' & successive members of tokens[] point to the sub-strings ("tokens")
thus created.  A character sequence containing spaces is considered a single
token if it's enclosed in quotes (the quotes will not become part of the
token).  End of tokens[] is marked by NULL.  On exit, tokp points to
tokens[0], i.e., it works like the traditional C variable argv. */
{
	char *cptr;
	char c;
	int notok;			/* flag; 1 = not in a token */
	static char
		*tokens[15],		/* pointers to cmd line tokens */
		buffer[85];		/* command line buffer */

	if ( (fbatch) && (get_dclp == 0)) {
		readloop:
		if (getlin(buffer, 85, batfp) == 0) {
			printf("end of batch file\n");
			fclose(batfp);
			batfp = NULL;	/* to indicate no batch file open */
			showbatcmds = 1;
			LONGJMP(reset, 1);
		}
		/* show the batch file line */
		if  (showbatcmds == 1) {
		    if  (reportlevel == 1) {
			printf("}%s\n", buffer);
		    }
		}
		if  (buffer[0] == '/') {
		    goto readloop;
		}
	}

	if ( (!fbatch) && (get_dclp == 0)) {
	    /* display '>', get command line from keyboard */
	    s_in(">", buffer);
	}

	if  (get_dclp == 1) {
	    if  (strcmp(cmdline_buff,"") != 0) {
		/* There are parameters on DOS command line */
		strcpy(buffer, cmdline_buff);
		if  (reportlevel == 1) {
		    printf("]%s\n", buffer);
		}
	    }
	}

	/* Upper case the commands */
	/*stoup(buffer);*/
	tokp = tokens;		/* point to 1st element of tokens[] */
	notok = 1;		/* set "not in token" flag true */
	for (cptr = buffer; c = *cptr; ++cptr)
		if (isspace(c)) {
			notok = 1;
			*cptr = '\0';	/*  replace ' ' with '\0' */
		} else if (notok) {	/* first char of a token */
			notok = 0;
			if (c != '"')
				*tokp++ = cptr;
			else {		/* quoted token */
				*tokp++ = ++cptr;
				while ((c = *++cptr) && c != '"')
				/* find closing '"' or end of string */
					;
				if (c)		/* found '"' */
					*cptr = '\0';
				else		/* found '\0' */
					--cptr;
		}	}
	*tokp = NULL;		/* terminate tokens[] */
	tokp = tokens;
}

static void cmdline()
/* Inputs commands from the DOS command line */
{
	if (!fbatch) {
	   printf("CMDLINE COMMAND ONLY VALID IN .INI FILE\n");
	   LONGJMP(reset, 1);
	}

	if  (strcmp(cmdline_buff,"") != 0) {
	    get_dclp = 1;
	    tok();
	    get_dclp = 0;
	}
	--tokp;
}

static void xrun()
/* do a run if command line is exhausted */
{
	if (tokp[1] == NULL) {
	   run();
	   ++tokp;		/* run() decremented it */
	}
}

static void runall()
{
	reset_tle_ptr();
	while (nextsat() == 0) {
	    run();
	    ++tokp;
	}
	reset_tle_ptr();
	--tokp;
}

static void do_null_func()
{
	--tokp;
}

static void define_label()
{

}

static void goto_label()
{
	char label_name[31];

	strcpy(label_name,*tokp);

	/* is a file open */
	if  (!batfp) {
	     printf("CANNOT 'GOTO' WHEN A FILE IS NOT OPEN\n");
	     LONGJMP(reset, 1);
	}

	/* reset file pointer to beginning*/
	fseek(batfp, 0L, 0);

	fbatch = '\001';
	showbatcmds = 0;

	for (;;) {
		tok();
		if (!strcmp(*tokp,"LABEL") || !strcmp(*tokp,"label")) {
		   ++tokp;
		   if  (!strcmp(*tokp,label_name)) {
		       goto foundit;
		   }
		}
	}
	foundit:
	showbatcmds = 1;
}

static void save_block()
{
	int work_baz, work_eaz, work_el;

	work_baz = atoi(*tokp);
	if (work_baz < 0 || work_baz > 359) {
	     printf("BEGIN AZIMUTH INVALID : MUST BE BETWEEN 0 AND 359\n");
	     LONGJMP(reset, 1);
	}

	++tokp;
	work_eaz = atoi(*tokp);
	if (work_eaz < 0 || work_eaz > 359) {
	     printf("END AZIMUTH INVALID : MUST BE BETWEEN 0 AND 359\n");
	     LONGJMP(reset, 1);
	}

	++tokp;
	work_el = atoi(*tokp);
	if (work_el < 0 || work_el > 90) {
	     printf("ELEVATION INVALID : MUST BE BETWEEN 0 AND 90\n");
	     LONGJMP(reset, 1);
	}

/*      block_data_ptr = realloc (block_data_ptr,
	       ((block_max + 1) * sizeof(struct block_strtr)));
	if (block_data_ptr == NULL) {
	    printf("NO ROOM FOR MORE BLOCK COMMANDS\n");
	    LONGJMP(reset, 1);
	}
*/

	block_max++;
	if  (block_max > 30) {
	    printf("NO ROOM FOR MORE BLOCK COMMANDS\n");
	    LONGJMP(reset, 1);
	}

	/* If the end azimuth is less than begin azimuth then it must be */
	/* a range across zero azimuth, split the block into two blocks  */
	/* of ranges before and after zero azimuth                       */


	if  (work_eaz < work_baz) {
	    block_data[block_max].baz = work_baz;
	    block_data[block_max].eaz = 359;
	    block_data[block_max].el  = work_el;

	    block_max++;
	    if  (block_max > 30) {
		printf("NO ROOM FOR EXTENDED BLOCK COMMANDS\n");
		LONGJMP(reset, 1);
	    }

	    block_data[block_max].baz = 0;
	    block_data[block_max].eaz = work_eaz;
	    block_data[block_max].el  = work_el;
	}
    else {
	    block_data[block_max].baz = work_baz;
	    block_data[block_max].eaz = work_eaz;
	    block_data[block_max].el  = work_el;
       }
}

static void set_orbit_mins()
/* set orbit time minutes */
{
	def_orbit_mins = atoi(*tokp);
	if (def_orbit_mins <= 0) {
	   printf("INVALID ORBIT MINUTES VALUE, SET TO DEFAULT OF 60");
	   def_orbit_mins = 60;
	}
}

static void set_print_limit()
/* set print limit line count */
{
	print_limit = atoi(*tokp);
	if (print_limit <= 0) {
	   printf("INVALID PRINTLIMIT VALUE, SET TO DEFAULT OF 60");
	   print_limit = 60;
	}
}

static void set_linefeed()
/* set linefeed value */
{
	linefeed_value = atoi(*tokp);
	if (linefeed_value < 1) {
	   printf("INVALID LINEFEED VALUE, SET TO DEFAULT OF 1");
	   linefeed_value = 1;
	}
}

static void runtime()
/* Executes a prediction run in time order */
{
	/* Work time values */
	double tsince,

	/* Saved master time control values */
	       mast_time,
	       temp1, temp2;

	long int mast_jd;

	int print_count;

	if  (delt == 0.0) {
	    delt = 1.0;
	}

	temp1 = t1.jd * xmnpda - 720. + t1.time - tzone;
	temp1 -= epoch;
	temp2 = tf - temp1;
	if (temp2 < 0.) {
	   printf("STOP < START\n");
	   LONGJMP(reset, 1);
	}

	reset_tle_ptr();
	if  (nextsat() == 1) {
	    printf("ERROR IN GETTING FIRST SATELLITE\n");
	    LONGJMP(reset, 1);
	}

	if  (xno == 0.) {
	    printf("NO ELEMENTS FOR FIRST SATELLITE\n");
	    LONGJMP(reset, 1);
	}

	/* This keep incrementing the time until the sun is in a     */
	/* favourable position i.e. it is dark enough to see satellites */
	for (;;) {

	    /* Is it time to stop */
	    temp1 = t1.jd * xmnpda - 720. + t1.time - tzone;
	    temp2 = tf - temp1;
	    if (temp2 < 0.) {
		goto exitloop;
	    }

	    if  (reportlevel == 1) {
		if ( ((int) t1.time) % 60 == 0) {
		   printf("\nPrediction Time %s\n",timstr(t1.time));
		}
	    }

	    /* Calc minutes */
	    tsince = t1.jd * xmnpda - 720. + t1.time - tzone;
	    if (sun_elev(tsince) <= elsuob_limit) {
	       break;
	    }

	    /* Update the time */
	    t1.time += delt;
	    if (t1.time >= xmnpda) { /* crossed midnight */
		t1.time -= xmnpda;
		++t1.jd;
	    }
	}

	reset_tle_ptr();

	reset_ahpr_flags();

runloop:

	HLTRUN

	if  (nextsat() == 1) {
	    reset_tle_ptr();
	    if (nextsat() == 1) {
	       printf("Error in getting first satellite\n");
	       LONGJMP(reset, 1);
	    }

	   up_time:

	    /* Update the time */
	    t1.time += delt;
	    if (t1.time >= xmnpda) { /* crossed midnight */
	       t1.time -= xmnpda;
	       ++t1.jd;
	    }

	    /* Is it time to stop */
	    temp1 = t1.jd * xmnpda - 720. + t1.time - tzone;
	    temp2 = tf - temp1;
	    if (temp2 < 0.) {
	       goto exitloop;
	    }

	    if  (reportlevel == 1) {
		if ( ((int) t1.time) % 60 == 0) {
		   printf("\nPrediction Time %s\n",timstr(t1.time));
		}
	    }

	    /* If the sun is too high, i.e too bright , update time */
	    if (sun_elev(temp1) > elsuob_limit) {
	       goto up_time;
	    }

	}

	/* Calc mins */
	tsince = t1.jd * xmnpda - 720. + t1.time - tzone;
	tsince -= epoch;

	/* call prediction model */
	MODEL(tsince - toffs);

	/* Is it above the hrizon */
	if  (xyztop(tsince + epoch)) {

	    search_set_index(norad_sat_nbr);

	    if  (save_ah_values(norad_sat_nbr,t1.jd,t1.time) == 1) {
		/* if 1 is returned, that means that the last printed time */
		/* for that satellite is not exceeded yet, skip it         */
		goto runloop;
	    }

	    if  (   ((apmag + magbias) <= vismag)
		 && (elsusa >= elsusa_limit)
		 && ((int) (azel.y * ra2de + .5) >= minelev)
		 && ((int) (azel.y * ra2de + .5) <= maxelev)
		 && (phase_angle >= minphase)
		 && (sun_elev(tsince + epoch) <= elsuob_limit)
		 && (check_blocks( (int) (azel.x * ra2de + .5),
				   (int) (azel.y * ra2de + .5)
				 ) == 0)
		 && (  (long int) ((radec.z * xkmper) * distance_units)
		       <= max_range)
		 && ( (long int) ((radec.z * xkmper) * distance_units)
		       >= min_range)
		) {
		   /* above horizon and satisfies conditions */

		   /* Save master time values */
		   mast_time = t1.time;
		   mast_jd = t1.jd;

		   /* restore saved above horizon values */
		   if  (restore_ah_values(norad_sat_nbr,&t1.jd,&t1.time) == 0) {
		       printf("FAILED TO RESTORE ABOVE HORIZON VALUES FOR %s (#%5d)\n",
			      name,norad_sat_nbr);
		   }

		   printf("\n%s", name);	/* satellite name */
		   if (shownoradnbr == 1) {
		      printf("   (#%5d)", norad_sat_nbr);
		   }
		   printf("   %s", jdstr(t1.jd)); /* start date */
		   if (show_tle_age == 1) {
		      printf("   TleAge %3.1f days",
			 (((t1.jd * xmnpda - 720. + t1.time
			       - epoch) / 60.0) / 24.0));
		   }
		   if (toffs)
			printf("   offset = %s", timstr(toffs));
		   moon_data(t1.jd * xmnpda - 720. + t1.time - tzone);
		   if  (moon_el > 0.0) {
		       moon_str();
		   }

		   printf("\n");
		   prntitle();

		   print_count = 0;

		   do {

			tsince = t1.jd * xmnpda - 720. + t1.time - tzone;
			tsince -= epoch;
			MODEL(tsince - toffs);
			if (!xyztop(tsince + epoch)) {
			   save_pr_values(norad_sat_nbr,t1.jd,t1.time);
			   for (print_count = 1;
				print_count < linefeed_value;
				print_count++)
				printf("\n");
			   break;
			}

			print_count++;
			if  (print_count > print_limit) {
			    save_pr_values(norad_sat_nbr,t1.jd,t1.time);
			    printf(" Warning : Stopping after %d lines printed\n",print_limit);
			    for (print_count = 1;
				 print_count < linefeed_value;
				 print_count++)
				 printf("\n");
			    break;
			}

			moon_data(t1.jd * xmnpda - 720. + t1.time - tzone);
			elsuob = sun_elev(tsince + epoch);
			prnval(t1.time);
			/* Increment time */
			t1.time += delt;
			if (t1.time >= xmnpda) { /* crossed midnight */
			   t1.time -= xmnpda;
			   ++t1.jd;
			}

			HLTRUN

		      } while (1);

		/* Restore time and date values */
		t1.jd = mast_jd;
		t1.time = mast_time;
	    }
	}

	goto runloop;

exitloop:

--tokp;

}

static void set_options()
/* sets various options */
{
	char *cp;

	cp = *tokp;

	if  (!strcmp(cp,"miles") || !strcmp(cp,"MILES")) {
	    distance_units = 0.62137;
	    goto exit;
	}
	if  (!strcmp(cp,"kilometers") || !strcmp(cp,"KILOMETERS")) {
	    distance_units = 1.0;
	    goto exit;
	}
	if  (!strcmp(cp,"showtle") || !strcmp(cp,"SHOWTLE")) {
	    showtle = 1;
	    goto exit;
	}
	if  (!strcmp(cp,"report") || !strcmp(cp,"REPORT")) {
	    reportlevel = 1;
	    goto exit;
	}
	if  (!strcmp(cp,"shownorad") || !strcmp(cp,"SHOWNORAD")) {
	    shownoradnbr = 1;
	    goto exit;
	}
	if  (!strcmp(cp,"showage") || !strcmp(cp,"SHOWAGE")) {
	    show_tle_age = 1;
	    goto exit;
	}
	printf("INVALID SET COMMAND OPTION: %s\n",cp);
	LONGJMP(reset, 1);

exit:

}

static void reset_options()
{
	char *cp;

	cp = *tokp;

	if  (!strcmp(cp,"vismag") || !strcmp(cp,"VISMAG")) {
	    vismag = 9999.9;     /* default value for visual magnitude */
	    goto exit;
	}
	if  (!strcmp(cp,"sunelevsat") || !strcmp(cp,"SUNELEVSAT")) {
	    elsusa_limit = -9999;    /* default value for SUN */
	    goto exit;
	}
	if  (!strcmp(cp,"sunelevobs") || !strcmp(cp,"SUNELEVOBS")) {
	    elsuob_limit = 99;       /* default value for sun elevation */
	    goto exit;
	}
	if  (!strcmp(cp,"minelev") || !strcmp(cp,"MINELEV")) {
	    minelev = -999;          /* default value for minimun elevation */
	    goto exit;
	}
	if  (!strcmp(cp,"maxelev") || !strcmp(cp,"MAXELEV")) {
	    maxelev = 99;            /* default value for maximum elevation */
	    goto exit;
	}
	if  ( !strcmp(cp,"minphase") || !strcmp(cp,"MINPHASE")
	    || !strcmp(cp,"minphs") || !strcmp(cp,"MINPHS")) {
	    minphase = -9999;
	    goto exit;
	}
	if  (!strcmp(cp,"showtle") || !strcmp(cp,"SHOWTLE")) {
	    showtle = 0;             /* Show tle's */
	    goto exit;
	}
	if  (!strcmp(cp,"report") || !strcmp(cp,"REPORT")) {
	    reportlevel = 0;         /* Suppress printing command lines etc */
	    goto exit;
	}
	if  ( !strcmp(cp,"block") || !strcmp(cp,"BLOCK")
	    || !strcmp(cp,"blocks") || !strcmp(cp,"BLOCKS")) {
	    block_max = -1;          /* Clear all blocks */
	    goto exit;
	}
	if  (!strcmp(cp,"printlimit") || !strcmp(cp,"PRINTLIMIT")) {
	    print_limit = 60;        /* Max lines per sat in runtime mode */
	    goto exit;
	}
	if  (!strcmp(cp,"orbitmins") || !strcmp(cp,"ORBITMINS")) {
	    def_orbit_mins = 60;     /* Orbit length in runtime mode */
	    goto exit;
	}
	if  (!strcmp(cp,"shownorad") || !strcmp(cp,"SHOWNORAD")) {
	    shownoradnbr = 0;
	    goto exit;
	}
	if  (!strcmp(cp,"showage") || !strcmp(cp,"SHOWAGE")) {
	    show_tle_age = 0;
	    goto exit;
	}
	if  (!strcmp(cp,"dbs") || !strcmp(cp,"DBS")) {
	    reset_db_flags();
	    goto exit;
	}
	if  (!strcmp(cp,"maxrange") || !strcmp(cp,"MAXRANGE")) {
	    max_range = 65535;
	    goto exit;
	}
	if  (!strcmp(cp,"minrange") || !strcmp(cp,"MINRANGE")) {
	    min_range = 0;
	    goto exit;
	}

	printf("INVALID RESET COMMAND OPTION: %s\n",cp);
	LONGJMP(reset, 1);

exit:

}

static void rundbs()
/* Executes a prediction run in time order */
{
	/* Work time values */
	double tsince,

	/* Saved master time control values */
	       mast_time,
	       temp1, temp2;

	long int mast_jd;

	int print_count;

	if  (delt == 0.0) {
	    delt = 1.0;
	}

	temp1 = t1.jd * xmnpda - 720. + t1.time - tzone;
	temp1 -= epoch;
	temp2 = tf - temp1;
	if (temp2 < 0.) {
	   printf("STOP < START\n");
	   LONGJMP(reset, 1);
	}

	reset_db_ptr();
	if  (next_db_sat() == 1) {
	    printf("ERROR:NO SATELLITES WERE SELECTED\n");
	    LONGJMP(reset, 1);
	}

	if  (xno == 0.) {
	    printf("NO ELEMENTS FOR FIRST SATELLITE\n");
	    LONGJMP(reset, 1);
	}

	reset_ahpr_flags();
	reset_db_ptr();

runloop:

	HLTRUN

	if  (next_db_sat() == 1) {
	    reset_db_ptr();
	    if (next_db_sat() == 1) {
	       printf("ERROR:NO SATELLITES WERE SELECTED\n");
	       LONGJMP(reset, 1);
	    }
	    /* Update the time */
	    t1.time += delt;
	    if (t1.time >= xmnpda) { /* crossed midnight */
		t1.time -= xmnpda;
		++t1.jd;
	    }

	    /* Is it time to stop */
	    temp1 = t1.jd * xmnpda - 720. + t1.time - tzone;
	    temp2 = tf - temp1;
	    if (temp2 < 0.) {
		goto exitloop;
	    }
	}

	/* Calc mins */
	tsince = t1.jd * xmnpda - 720. + t1.time - tzone;
	tsince -= epoch;

	/* call prediction model */
	MODEL(tsince - toffs);

	/* Is it above the hrizon */
	if  (xyztop(tsince + epoch)) {

	    search_set_index(norad_sat_nbr);

	    if  (save_ah_values(norad_sat_nbr,t1.jd,t1.time) == 1) {
		/* if 1 is returned, that means that the last printed time */
		/* for that satellite is not exceeded yet, skip it         */
		goto runloop;
	    }

	    if  (   ((apmag + magbias) <= vismag)
		 && (elsusa >= elsusa_limit)
		 && ((int) (azel.y * ra2de + .5) >= minelev)
		 && ((int) (azel.y * ra2de + .5) <= maxelev)
		 && (phase_angle >= minphase)
		 && (sun_elev(tsince + epoch) <= elsuob_limit)
		 && (check_blocks( (int) (azel.x * ra2de + .5),
				   (int) (azel.y * ra2de + .5)
				 ) == 0)
		 && ( (long int) ((radec.z * xkmper) * distance_units)
		      <= max_range)
		 && ( (long int) ((radec.z * xkmper) * distance_units)
		      >= min_range)
		) {
		   /* above horizon and satisfies conditions */

		   /* Save master time values */
		   mast_time = t1.time;
		   mast_jd = t1.jd;

		   /* restore saved above horizon values */
		   if  (restore_ah_values(norad_sat_nbr,&t1.jd,&t1.time) == 0) {
		       printf("FAILED TO RESTORE ABOVE HORIZON VALUES FOR %s (#%5d)\n",
			      name,norad_sat_nbr);
		   }

		   printf("\n%s", name);	/* satellite name */
		   if (shownoradnbr == 1) {
		      printf("   (#%5d)", norad_sat_nbr);
		   }
		   printf("   %s", jdstr(t1.jd)); /* start date */
		   if (show_tle_age == 1) {
		      printf("   TleAge %3.1f days",
			 (((t1.jd * xmnpda - 720. + t1.time
			       - epoch) / 60.0) / 24.0));
		   }
		   if (toffs)
			printf("   offset = %s", timstr(toffs));
		   moon_data(t1.jd * xmnpda - 720. + t1.time - tzone);
		   if  (moon_el > 0.0) {
		       moon_str();
		   }

		   printf("\n");
		   prntitle();

		   print_count = 0;

		   do {

			tsince = t1.jd * xmnpda - 720. + t1.time - tzone;
			tsince -= epoch;
			MODEL(tsince - toffs);
			if (!xyztop(tsince + epoch)) {
			   save_pr_values(norad_sat_nbr,t1.jd,t1.time);
			   for (print_count = 1;
				print_count < linefeed_value;
				print_count++)
				printf("\n");
			   break;
			}

			print_count++;
			if  (print_count > print_limit) {
			    save_pr_values(norad_sat_nbr,t1.jd,t1.time);
			    printf(" Warning : Stopping after %d lines printed\n",print_limit);
			    for (print_count = 1;
				 print_count < linefeed_value;
				 print_count++)
				 printf("\n");
			    break;
			}

			moon_data(t1.jd * xmnpda - 720. + t1.time - tzone);
			elsuob = sun_elev(tsince + epoch);
			prnval(t1.time);
			/* Increment time */
			t1.time += delt;
			if (t1.time >= xmnpda) { /* crossed midnight */
			   t1.time -= xmnpda;
			   ++t1.jd;
			}

			HLTRUN

		      } while (1);

		/* Restore time and date values */
		t1.jd = mast_jd;
		t1.time = mast_time;
	    }
	}

	goto runloop;

exitloop:

--tokp;

}

static void set_max_range()
/* satellite must come below or at this range to be printed */
{
	max_range = atoi(*tokp);
	if (max_range < min_range) {
	   max_range = 65535;
	   printf("MAXRANGE < MINRANGE, setting to 65535");
	}
}

static void set_min_range()
/* satellite must be at or above this range to be printed */
{
	min_range = atoi(*tokp);
	if (min_range > max_range) {
	   min_range = 0;
	   printf("MINRANGE > MAXRANGE, setting to ZERO");
	}
}

/* End Driver */
