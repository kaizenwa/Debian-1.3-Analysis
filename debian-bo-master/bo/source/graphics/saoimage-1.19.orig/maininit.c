#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	maininit.c (Main Initialize)
 * Purpose:	Start of the saoimage package
 * Program:	main()
 * Subroutine:	crash_on_error()
 * Xlib calls:	XSetErrorHandler(), XCloseDisplay(), XGetErrorText()
 * UNIX calls:	setrlimit(), ieee_handler(), abrupt_underflow_()
 * Copyright:	1989,1990,1993-1996 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *              {2} Doug Mink	version 1.08: autoscale           10 Nov 1993
 *              {3} Doug Mink	version 1.09: Add WCS tracking    19 Oct 1994
 *              {4} Doug Mink	version 1.10: Add CD to WCS       22 Dec 1994
 *              {5} Doug Mink	version 1.11: Handle narrow files 20 Jan 1995
 *              {6} Doug Mink	version 1.12: Add DSS plate sol.   1 Mar 1995
 *              {7} Doug Mink	version 1.13: Fix bugs from STScI  5 May 1995
 *              {7} Doug Mink	version 1.13: Fix DSS plate sol.   5 May 1995
 *              {8} Doug Mink	version 1.14: Add coordinate conv.16 Aug 1995
 *              {9} Doug Mink	version 1.15: Bug fixes           18 Oct 1995
 *             {10} Doug Mink	version 1.16: DSS WCS Bug fix      7 Nov 1995
 *             {11} Doug Mink	version 1.17: IRAF .imh WCS       14 Dec 1995
 *             {11} Doug Mink	version 1.17: SECPIX WCS          22 Dec 1995
 *             {11} Doug Mink	version 1.17: Initial zoom factor  2 Jan 1996
 *             {11} Doug Mink	version 1.17: Fix pixel tracking   2 Feb 1996
 *             {12} Doug Mink	version 1.18: Fix IRAF header      9 Feb 1996
 *             {13} Doug Mink	version 1.18: Fix various bugs    26 Feb 1996
 *             {14} Doug Mink	version 1.19: Improved WCS         4 Sep 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, FILE, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <sys/time.h>		/* struct timeval used in <sys/resource.h> */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/define.h"	/* YES, NO, MIN, MAX and more */
#include "hfiles/struct.h"	/* declare structure types */
#ifdef DEBUG
#ifdef SUN			/* DEBUG and SUN(OS4.xx) */
#include <floatingpoint.h>	/* SIGFPE_ABORT ieee exception switch */
#endif
#else
#ifndef VMS			/* NOT DEBUG AND NOT VMS */
#ifndef SYSV
#include <sys/resource.h>	/* rlimit, RLIMIT_CORE for setrlimit */
#endif
#endif
#endif

char *RevMsg = "release 1.19 4 September 1996";
int init_done = 0;

/* declare and initialize parameter structs */
#include "defs/control.def"
#include "defs/color.def"
#include "defs/image.def"
#include "defs/dispbox.def"
#include "defs/magnibox.def"
#include "defs/colorbox.def"
#include "defs/graphbox.def"
#include "defs/panbox.def"
#include "defs/btnbox.def"
#include "defs/desktop.def"
#include "defs/cursor.def"

/* declare uninitialized structs */
struct WorldCoor *wcs;		/* WCS data structure */
struct bufferRec buffer;
struct coordRec coord;
Display *display;		/* display connection */
char wcscoor[16];		/* Output coordinate system */
char wcscommand[64];		/* WCS command */

/*
 * Program:	main
 * Purpose:	Get things started, then call event_loop
 * Xlib calls:	XSetErrorHandler()
 * UNIX calls:	setrlimit()
 */
main(argc, argv)
  int argc;
  char **argv;
{
  static void init_params(), init_packages();
  void crash_on_error(), control_event_loop();
  void say_goodbye();
  
#ifdef DEBUG
  /* install special error handler to force crash so we can trace problem */
  XSetErrorHandler(crash_on_error);
#ifdef SUN
  /* Under SunOS4 we can force floating point exceptions to report errors */
  ieee_handler("set", "common", SIGFPE_ABORT);
  abrupt_underflow_();
#endif
#else
#ifndef VMS
#ifndef SYSV
  {
    struct rlimit rlp;	/* l: UNIX resourse limit structure */
    /* disable core dumping */
    rlp.rlim_cur = 0;
    rlp.rlim_max = 0;
    setrlimit(RLIMIT_CORE, &rlp);
  }
#endif
#endif
#endif
  /* set up, read, and check parameters */
  init_params(argc, argv);
  if( control.verbose ) {
    (void)printf("\n Smithsonian Astrophysical Observatory image utility\n");
    (void)printf(" SAOimage %s\n\n", RevMsg);
  }
  init_packages(argc, argv);
  /* CALL MAIN EVENT LOOP (uses extern.h) */
  init_done = 1;
  control_event_loop();
  say_goodbye(0);
}

/*
 * Subroutine:	say_goodbye
 * Purpose:	Free server resources before exiting
 * Xlib calls:	XCloseDisplay()
 */
void say_goodbye ( code )
     int code;		/* i: code to identify exit */
{
  void free_gc();

  if( desktop.display != NULL ) {
    free_gc();
    XCloseDisplay(desktop.display);
  }
  exit(code);
}

/*
 * Subroutine:	init_params
 * Purpose:	Initialize parameters in the records
 * Note:	Resource or default file not yet used
 */
static void init_params ( argc, argv )
     int argc;
     char **argv;
{
  char *name;		/* l: both flag for init and return display name */
  int parse_stat;
  static void init_server();
  int parse_cmdline(), check_image();
  void say_goodbye(), init_connections();

  name = NULL;
  wcs = NULL;
  /* get parameters from the command line (uses extern.h) */
  strcpy (wcscoor,"same");        /* Output coordinate system */
  strcpy (wcscommand,"rgsc_%s");      /* WCS command */

  if( (parse_stat = parse_cmdline(argc, argv, &name)) < 0 )
    say_goodbye(1);
  /* check image parameters for consistency (uses extern.h) */
  if( check_image(&img, parse_stat) )
    img.file_type = SOP_Logo;
  /* connect to the display server */
  init_server(name, &desktop, &color);
  init_connections();
}

/*
 * Subroutine:	init_packages
 * Purpose:	Initialize all of the subroutine packages and windows
 */
static void init_packages ( argc, argv )
     int argc;
     char **argv;
{
  GC gc;
  int init_image();
  GC set_gc_with_background();
  void init_windows1(), init_windows2(), init_panbox_coords(), disp_dispbox();
  void disp_panbox(), init_imaging_buffers(), init_mousepointers(), init_gc();
  void init_display_buffers(), init_region_draw(), init_color(), new_display();
  void init_software_cursors(), init_buttonmenu(), init_buttonbox_settings();
  void init_magnifier(), set_magnifier(), redraw_magnifier(), init_colorbox();
  void say_goodbye(), mount_buttonmenu(), new_panimage(), init_cgraph_text();

  /* get the image dimensions (need file name, type), exit if failure */
  if( init_image() == 0 ) {
    (void)fprintf(stderr, "No image given and no input device connected.\n");
    say_goodbye(1);
  }
  /* get the colors ready (need display) */
  init_color(&color, 1);
  /* map the desktop(need image dimensions, color) */
  init_windows1(argc, argv);
  /* set up the mouse pointer icons (need display, hardcolors) */
  init_mousepointers(desktop.display, desktop.display);
  /* initialize color bar and graph labeling text font and parameters */
  init_gc(desktop.display, desktop.ID);
  init_cgraph_text();
  /* set up the image buffer (need image) */
  init_imaging_buffers();
  /* get desktop dimensions and create windows (need colors, mouse cursors) */
  init_windows2();
  /* get the soft cursors ready(need colors, img coords) */
  init_software_cursors();
  /* set up color bar */
  init_colorbox();
  /* set up display buffers (need window dimensions) */
  init_display_buffers();
  /* set params and get image data (need colors, buffers, scope, imageinit) */
  init_panbox_coords();
  init_magnifier(control.magni_track, control.coord_track);
  new_display(0, 1, 1, 1);
  /* move image data into the panbox's buffer */
  new_panimage();
  /* set up the button menu */
  gc = set_gc_with_background(&color.gcset.menu, color.gcset.menu.background);
  init_buttonmenu(&btnbox, gc, color.visual,
		  (unsigned long)color.hard.std_black,
		  (unsigned long)color.hard.std_white);
  init_buttonbox_settings();
  /* set up region action flags and drawing stuff (draw and label on) */
  init_region_draw(1, 1);
  mount_buttonmenu();
}

#ifdef DEBUG
/*
 * Subroutine:	crash_on_error
 * Purpose:	Special error handler to force crash on error
 * Xlib calls:	XGetErrorText()
 */
void crash_on_error ( dt_display, error )
     Display *dt_display;
     XErrorEvent *error;
{
  int *foo;
  char msg[SZ_LINE];

  (void)fprintf(stderr,"XError Reported: type code: %d\n",error->error_code);
  XGetErrorText(dt_display, (int)error->error_code, msg, SZ_LINE);
  (void)fprintf(stderr, "%s\n", msg);
  /* cause fatal core dump */
  foo = 0;
  *foo = 0;
  exit( 0 );
}
#endif

/*
 * Subroutine:	init_server
 * Purpose:	Open connection with chosen or default display server
 */
static void init_server ( displayname, desktop, color )
     char *displayname;
     struct windowRec *desktop;
     struct colorRec *color;
{
  Display *dt_display;
  int dt_screen;
  char errmsg[SZ_FNAME];
  void exit_errmsg();

  /* make contact with the display server */
  if( !(dt_display = XOpenDisplay(displayname)) ) {
    (void)strcpy(errmsg, "Could not connect to display: ");
    if( displayname != NULL )
      (void)strcat(errmsg, displayname);
    exit_errmsg(errmsg);
  }
  desktop->display = dt_display;
  color->display = dt_display;
  dt_screen = DefaultScreen(dt_display);
  desktop->screen = dt_screen;
  color->screen = dt_screen;
  display = dt_display;
}
