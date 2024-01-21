#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	cmdparse.c (Command Parse)
 * Purpose:	Set options from command line
 * Subroutine:	parse_cmdline()			returns: int
 * Subroutine:	string_cmdline()		returns: void
 * Subroutine:	usage()				returns: int
 * Xlib calls:	none
 * Copyright:	1989-1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	       5 January 1989
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{2} Rick Schumeyer (Naval Research Lab)		   8 May 1991
 *		Added -button option.
 *		  The -button option get parsed in parse_etc().
 *		  I added the extern variable bad_buttons, which is defined
 *		  in btnlib/draw.c.
 *		{3} MVH added + type switches, new error message  21 Jun 1991
 *		{4} Doug Mink  added -lfrac linear autoscale      10 Nov 1993
 *		{5} Doug Mink  added -wcscom search template      26 Oct 1994
 *		{6} Doug Mink  added -b1950 and -j2000		   8 Jun 1995
 *		{7} Doug Mink  change wcs argument    		   7 Jul 1995
 *		{8} Doug Mink  added -gal			   8 Jun 1995
 *		{9} Doug Mink  allow output WCS update		  17 Aug 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <ctype.h>

#ifndef VMS
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main SAOimage parameter structures */
#include "hfiles/cmdparse.h"	/* define parse status bits */

extern int init_done;	/* flag set at end of initialization indicates redo */
#ifdef ALLIANT
extern int bad_buttons;
#endif

/*
 * Subroutine:	parse_cmdline
 * Purpose:	Process options to set filename and change defaults
 * Returns:	-1 on error, 0 OK but no filename, 1 OK and new filename given
 */
int parse_cmdline ( argc, argv, displayname )
     int argc;
     char *argv[];
     char **displayname;	/* i/o: gets server name only at init time */
{
  int i, status;
  int got, init;
  int parse_display(), parse_filename(), parse_filetype(), parse_connection();
  int parse_rotate(), parse_scale(), parse_color(), parse_fileread(), usage();
  void init_cmdline();
  static int parse_etc();

  if( displayname != NULL ) {
    /* if initial program command line, strip off program name, store line */
    argc--;
    argv = &(argv[1]);
    /* store the initial command string */
    init_cmdline(argc, argv);
    init = 1;
  } else
    init = 0;
  /* initialize parse history */
  got = 0;
  status = 1;
  buffer.lfrac = 0.0;

  /* check command line arguments for option requests */
  for( i = 0; (status == 1) && (i < argc); i++ ) {
    if( (status =
	 parse_display(argc, argv, &i, &got, displayname)) ) {
      /* check for server or window dressing specifications */
      ;
    } else if( ((got & CMD_FTYPE) == 0) &&
	       ((status = parse_filetype(argc, argv, &i, &got)) != 0) ) {
      /* check for the image file type */
      ;
    } else if( (status = parse_connection(argc, argv, &i, &got, init)) != 0 ) {
      /* check for change in remote connection status */
      ;
    } else if( (status = parse_fileread(argc, argv, &i, &got)) != 0 ) {
      /* check for image file reading parameters */
      ;
    } else if( (status = parse_rotate(argc, argv, &i, &got)) != 0 ) {
      /* check for image rotation */
      ;
    } else if( (status = parse_scale(argc, argv, &i, &got)) != 0 ) {
      /* check for scaling specifications */
      ;
    } else if( (status = parse_color(argc, argv, &i, &got)) != 0 ) {
      /* check for color map selection */
      ;
    } else if( (status = parse_etc(argc, argv, &i)) != 0 ) {
      /* check for miscelaneous environment switches */
      ;
    } else if( ((got & CMD_FNAME) == 0) &&
	       ((status = parse_filename(argc, argv, &i, &got)) != 0) ) {
      /* check for a possible image file name */
      ;
    } else {
      if( argv[i][0] == '-' )
	status = usage("unexpected switch", argc, argv, i, i);
      else
	status = usage("unrecognized token", argc, argv, i, i);
    }
  }
  if( status == 1 ) {
    return( got );
  } else
    return( -1 );
}

/*
 * Subroutine:	parse_etc
 * Purpose:	Parse command line for things settings from the etc menu
 */
static int parse_etc ( argc, argv, argi )
     int argc;		/* total number of arg tokens */
     char *argv[];	/* array of arg tokens */
     int *argi;		/* arg to check */
{
  void set_submenu_toggle();
  int i;

  i = *argi;

  if( (strcmp(argv[i], "-ct") == 0) ||
      (strcmp(argv[i], "-coord") == 0) ) {
    if( control.coord_track == 1 ) {
      control.coord_track = 0;
      if( init_done )
	set_submenu_toggle(EOP, EOP_TextTrack, 0);
    }
  } else if( (strcmp(argv[i], "+ct") == 0) ||
	     (strcmp(argv[i], "+coord") == 0) ) {
    if( control.coord_track != 1 ) {
      control.coord_track = 1;
      if( init_done )
	set_submenu_toggle(EOP, EOP_TextTrack, 1);
    }
  } else if( (strcmp(argv[i], "-mt") == 0) ||
	     (strcmp(argv[i], "-magnifier") == 0) ) {
    if( control.magni_track != 1 ) {
      /* track updates windows as mouse moves */
      control.magni_track = 1;
      if( init_done )
	set_submenu_toggle(EOP, EOP_Track, 1);
    }
  } else if( (strcmp(argv[i], "+mt") == 0) ||
	     (strcmp(argv[i], "+magnifier") == 0) ) {
    if( control.magni_track == 1 ) {
      /* track updates windows as mouse moves */
      control.magni_track = 0;
      if( init_done )
	set_submenu_toggle(EOP, EOP_Track, 0);
    }
  } else if( (strcmp(argv[i], "-wc") == 0) ||
	     (strcmp(argv[i], "-wcscom") == 0) ) {
    if ((++i) < argc) {
      strcpy (&wcscommand,argv[i]);
      if (wcs != NULL)
        wcscominit (wcs, &wcscommand);
      }
    else
      return( usage("wcscom", argc, argv, i-1, i) );

  } else if (strcmp(argv[i], "-wcsout") == 0) {
    if ((++i) < argc) {
      strcpy (&wcscoor,argv[i]);
      if (iswcs (wcs))
	wcsoutinit (wcs,argv[i]);
      }
    else
      return( usage("wcsout", argc, argv, i-1, i) );

  } else if( (strcmp(argv[i], "-b1") == 0) ||
	     (strcmp(argv[i], "-b1950") == 0) ||
	     (strcmp(argv[i], "-fk4") == 0) ) {
    strcpy (&wcscoor,"FK4");
    if (iswcs (wcs))
	wcsoutinit (wcs,"FK4");

  } else if( (strcmp(argv[i], "-j2") == 0) ||
	     (strcmp(argv[i], "-j2000") == 0) ||
	     (strcmp(argv[i], "-fk5") == 0) ) {
    strcpy (&wcscoor,"FK5");
    if (iswcs (wcs))
	wcsoutinit (wcs,"FK5");

  } else if( (strcmp(argv[i], "-gal") == 0) ||
	     (strcmp(argv[i], "-galactic") == 0) ) {
    strcpy (&wcscoor,"GALACTIC");
    if (iswcs (wcs))
	wcsoutinit (wcs,"GALACTIC");

  } else if( (strcmp(argv[i], "-q") == 0) ||
	     (strcmp(argv[i], "-quiet") == 0) ||
	     (strcmp(argv[i], "-v") == 0) ||
	     (strcmp(argv[i], "-verbose") == 0) ) {
    /* verbosity to control reporting of positions and measurements */
    control.verbose = 0;
    if( init_done )
      set_submenu_toggle(EOP, EOP_Verbose, 0);

  } else if( (strcmp(argv[i], "+v") == 0) ||
	     (strcmp(argv[i], "+verbose") == 0) ) {
    control.verbose = 1;
    if( init_done )
      set_submenu_toggle(EOP, EOP_Verbose, 1);
#ifdef ALLIANT
  } else if (strcmp(argv[i], "-alliant")==0) {
      bad_buttons = 1;
#endif
  } else {
    return( 0 );
  }
  *argi = i;
  return( 1 );
}

/*
 * Subroutine:	string_cmdline
 * Purpose:	Print tokens on one line to be like original command line
 */
void string_cmdline ( argc, argv, cmdline, linemax )
     int argc;
     char *argv[];
     char *cmdline;
     int linemax;
{
  int i;

  if( argc <= 0 ) {
    cmdline[0] = '\0';
  } else {
    (void)strcpy(cmdline, argv[0]);
    for( i=1; i<argc; i++ ) {
      (void)strncat(cmdline, " ", linemax);
      (void)strncat(cmdline, argv[i], linemax);
    }
  }
}

#define CMDMAX 200
/*
 * Subroutine:	usage
 * Purpose:	Print error mesage and list of command line switches
 * returns:	-1
 * Use:		Use to print msg and return -1 to indicate an error:
 * Example:	if(error) return( usage(errmess,argc,argv,i-?,i) );
 */
int usage ( what, argc, argv, first, last )
     char *what;
     int argc;
     char *argv[];
     int first, last;
{
  int i;
  char cmdline[CMDMAX];
  void string_cmdline();

  /* print the entire commandline */
  string_cmdline (argc, argv, cmdline, CMDMAX);
  (void)fprintf(stderr, "%s\n", cmdline);
  /* if wanted args beyond end of line, say so */
  if( last >= argc ) {
    (void)fprintf(stderr, "Missing arg(s) to switch\n");
    last = argc - 1;
  }
  /* print message and offending tokens */
  (void)fprintf(stderr,"Command line %s error:", what);
  for( i=first; i<=last; i++ ) {
    (void)fprintf(stderr, " %s", argv[i]);
  }
  (void)fprintf(stderr, "\n");
  /* list switch options */
  (void)fprintf(stderr,"usage: saoimage [ options ] [ - filename ]\n");
  (void)fprintf(stderr,"     where options are one or more of:\n");
  (void)fprintf(stderr,"  [(-display) (-d) <display>]");
  (void)fprintf(stderr," [[(-geometry) (-g) <geometry>] [-gd <geometry>]]\n");
  (void)fprintf(stderr,"  [[(-fits) (-oif)] [(-u1) (-u2)");
  (void)fprintf(stderr," (-i2) (-i4) (-r4) (-r8) <w> <h>]]\n");
  (void)fprintf(stderr,"  [(-skip) (-sk) <bytes>] [(-byteswap) (-bswap)]\n");
  (void)fprintf(stderr,"  [[-zero] [-one]]");
  (void)fprintf(stderr," [[(-lowerleft) (-ll)] [(-upperleft) (-ul)]]\n");
  (void)fprintf(stderr,"  [(-rotate) (-rot) <1,2,3>]\n");
  (void)fprintf(stderr,"  [-rmin <input clip val>]");
  (void)fprintf(stderr," [-rmax <input clip val>]\n");
  (void)fprintf(stderr,"  [-linear] [-wrap <count>] [-histeq]");
  (void)fprintf(stderr," [-sqrt <power>] [-log <exponent>]\n");
  (void)fprintf(stderr,"  [-min <scale clip val>] [-max <scale clip val>]\n");
  (void)fprintf(stderr,"  [(-palette) (-p) <number of color cells>]");
  (void)fprintf(stderr," [(-neg) (-n)]\n");
  (void)fprintf(stderr,"  [(-vertgraph) (-vg)] [(-horizgraph) (-hg)]\n");
  (void)fprintf(stderr,"  [(+/-verbose) (+/-v) (-quiet) (-q)]");
  (void)fprintf(stderr," [(+/-coord) (+/-ct)]\n");
  (void)fprintf(stderr,"  [(+/-magnifier) (+/-mt)]");
  (void)fprintf(stderr," [-mag <magnifier magnification>]\n");
  (void)fprintf(stderr,"  [(+/-imtool) (-pros)]\n");
  (void)fprintf(stderr,"  [-idev <pipe name>] [-odev <pipe name>]");
  (void)fprintf(stderr," [-fbconfig <file name>]\n");
  (void)fprintf(stderr,"  [(-red) (-green) (-blue)]\n");
  (void)fprintf(stderr,"  [(-panboxav) (-panboxsamp) (-panboxsum)");
  (void)fprintf(stderr," (-panboxmax)]\n");
  (void)fprintf(stderr,"  [-lprbuttons] [-mtf]");
  (void)fprintf(stderr," [(-bordercolor) (-bc) <color>]\n");
  (void)fprintf(stderr,"  [(-lfrac <fraction>)] [(-wcscom <command>]\n");
  (void)fprintf(stderr,"  [(-wcsout <output coordinate system>]\n");
#ifdef ALLIANT
  (void)fprintf(stderr,"  [-alliant]");
#endif
  (void)fprintf(stderr,"In most cases, the image filename needs no switch");
  (void)fprintf(stderr," and can appear anywhere\nin the options list\n");
  return( -1 );
}
