#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	cmddisp.c (Command Display)
 * Purpose:	Set options from command line pertaining to display
 * Subroutine:	parse_rotate()			returns: int
 * Subroutine:	parse_scale()			returns: int
 * Subroutine:	parse_color()			returns: int
 * Subroutine:	parse_display()			returns: int
 * Xlib calls:	none
 * Copyright:	1994 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	    20 September 1988
 *		{1} MVH	modified and renamed parse_display        18 May 1989
 *		{2} MVH	BSDonly strings.h compatability		  19 Feb 1990
 *		{3} Doug Mink           add -lfrac fraction       13 May 1994
 *		{4} Doug Mink           add -hi option            13 May 1994
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
#include "hfiles/extern.h"	/* extern main ximage parameter structures */
#include "hfiles/cmdparse.h"	/* define parse status bits */
#include "hfiles/magnify.h"	/* magnifier quick access structure */

extern struct magRec magset;
extern char *border_color_name;
/* Flag in GrphCInit.c to set color graph background black, else white */
extern int black_graph_background;
extern int init_done;
/* strings to set cursor colors to standard colors */
static char *RedString = "red";
static char *GreenString = "green";
static char *BlueString = "blue";
/*
 * Subroutine:	parse_rotate
 * Purpose:	Parse command line for coordinate adjustment or rotation
 *		parameters
 * Returns:	1 if one of its parameters was found, else 0
 */
int parse_rotate ( argc, argv, argi, new_rotate )
     int argc;		/* total number of arg tokens */
     char *argv[];	/* array of arg tokens */
     int *argi;		/* first arg to check, (returned as last arg used) */
     int *new_rotate;	/* status word knows coordinate parameter was found */
{
  int i;
  int usage();

  i = *argi;
  if( (strcmp(argv[i], "-rot") == 0) ||
      (strcmp(argv[i], "-rotate") == 0) ) {
    if( (++i >= argc) ||
        ((img.rotate_code = (3 & atoi(argv[i]))) < 0) ||
        (img.rotate_code > 3) )
      return( usage("rotate code", argc, argv, i-1, i) );
    *argi = i;
  } else if( (strcmp(argv[i], "-ul") == 0) || 
	     (strcmp(argv[i], "-upperleft") == 0) ) {
    /* --- image orientation (first row at top) --- */
    img.row_order = 0;
  } else if( (strcmp(argv[i], "-ll") == 0) ||
	     (strcmp(argv[i], "-lowerleft") == 0) ) {
    /* --- image orientation (first row at bottom) --- */
    img.row_order = 1;
  } else if( strcmp(argv[i], "-zero") == 0 ) {
    /* --- start file indexing with 0  --- */
    img.index_base = 0;
  } else if( strcmp(argv[i], "-one") == 0 ) {
    /* --- start file indexing with 1  --- */
    img.index_base = 1;
  } else {
    return( 0 );
  }
  *argi = i;
  *new_rotate |= CMD_ROTATE;
  return( 1 );
}

/*
 * Subroutine:	parse_color
 * Purpose:	Parse command line for display color parameters
 * Returns:	1 if one of its parameters was found, else 0
 */
int parse_color ( argc, argv, argi, got_color )
     int argc;		/* total number of arg tokens */
     char *argv[];	/* array of arg tokens */
     int *argi;		/* first arg to check, (returned as last arg used) */
     int *got_color;	/* status word knows coloring parameter was found */
{
  int i;
  int usage();
  void set_submenu_toggle();

  i = *argi;
  /* number of planes */
  if( (strcmp(argv[i], "-p") == 0) ||
      (strcmp(argv[i], "-palette") == 0) ) {
    if (++i >= argc)
      return( usage("planes", argc, argv, i-1, i) );
    color.cells.wanted = atoi(argv[i]);
    /* if planes request is negative, overlay for cursor */
    if( color.cells.wanted < 0 ) {
      color.cells.wanted = -color.cells.wanted;
      color.cells.overlay = 1;
    } else
      color.cells.overlay = 0;
    if( color.cells.wanted <= 1 ) {
      color.cells.wanted = 1;
      color.cells.overlay = 0;
      color.colormap_mode = VOP_Halftone;
    }
    *got_color |= CMD_COLOR;
  } else if( (strcmp(argv[i], "-n") == 0) ||
	     (strcmp(argv[i], "-neg") == 0) ) {
    /* negative map */
    color.inverse = 1;
    *got_color |= CMD_COLOR;
  /* on machines with only one functional color, use that color */
  } else if( strcmp(argv[i], "-red") == 0 ) {
    color.cur.desired_cur = RedString;
    color.cur.desired_one = RedString;
    color.cur.desired_two = RedString;
    black_graph_background = 1;
  } else if( strcmp(argv[i], "-green") == 0 ) {
    color.cur.desired_cur = GreenString;
    color.cur.desired_one = GreenString;
    color.cur.desired_two = GreenString;
    black_graph_background = 1;
  } else if( strcmp(argv[i], "-blue") == 0 ) {
    color.cur.desired_cur = BlueString;
    color.cur.desired_one = BlueString;
    color.cur.desired_two = BlueString;
    black_graph_background = 1;
  } else if( (strcmp(argv[i], "-vg") == 0) ||
	     (strcmp(argv[i], "-vertgragh") == 0) ) {
    graphbox.hints.width = 143;
    graphbox.hints.height = 550;
    graphbox.hints.min_width = 123;
    graphbox.hints.min_height = 300;
  } else if( (strcmp(argv[i], "-hg") == 0) ||
	     (strcmp(argv[i], "-horizgragh") == 0) ) {
    graphbox.hints.width = 516;
    graphbox.hints.height = 84;
    graphbox.hints.min_width = 404;
    graphbox.hints.min_height = 69;
  } else {
    return( 0 );
  }
  *argi = i;
  return( 1 );
}

/*
 * Subroutine:	parse_scale
 * Purpose:	Parse command line for image scaling parameters
 * Returns:	1 if one of its parameters was found, else 0
 */
int parse_scale ( argc, argv, argi, got_scale )
     int argc;		/* total number of arg tokens */
     char *argv[];	/* array of arg tokens */
     int *argi;		/* first arg to check, (returned as last arg used) */
     int *got_scale;	/* flag that scaling parameter was found */
{
  float val;
  int i;
  int usage();

  i = *argi;
  if( strcmp(argv[i], "-wrap") == 0 ) {
    /* image to display rescaling types */
    color.scale.mode = SOP_Wrap;
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) )
      color.scale.wrap_cnt = val;
    else
      --i;
  } else if( strcmp(argv[i], "-linear") == 0 ) {
    color.scale.mode = SOP_Linear;
  } else if( strcmp(argv[i], "-sqrt") == 0 ) {
    color.scale.mode = SOP_Sqrt;
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) )
      color.scale.root_power = val;
    else
      --i;
  } else if( strcmp(argv[i], "-log") == 0 ) {
    color.scale.mode = SOP_Log;
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) )
      color.scale.log_expo = val;
    else
      --i;
  } else if( strcmp(argv[i], "-hi") == 0 ||
             strcmp(argv[i], "-histeq") == 0 ) {
    color.scale.mode = SOP_HistEq;
  } else if( strcmp(argv[i], "-min") == 0 ) {
    /* scaling threshold level */
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) ) {
      buffer.cmdMin = val;
      buffer.min_given = 1;
    } else {
      --i;
      buffer.min_given = 0;
    }
  } else if( strcmp(argv[i], "-max") == 0 ) {
    /* scaling saturation level */
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) ) {
      buffer.cmdMax = val;
      buffer.max_given = 1;
    } else {
      --i;
      buffer.max_given = 0;
    }

  } else if( strcmp(argv[i], "-rmax") == 0 ) {
    /* file reading saturation level */
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) ) {
      img.fimax = val;
      *got_scale |= CMD_FREAD;
    } else
      return( usage("saturate", argc, argv, i-1, i) );
  } else if( strcmp(argv[i], "-rmin") == 0 ) {
    /* file reading threshold level */
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) ) {
      img.fimin = val;
      *got_scale |= CMD_FREAD;
    } else
      return( usage("threshold", argc, argv, i-1, i) );

  } else if( strcmp(argv[i], "-lfrac") == 0 ) {
    /* linear scaling limits as fraction of histogram peak */
    if( ((++i) < argc) && (sscanf(argv[i], "%f", &val) == 1) ) {
      buffer.lfrac = val;
    } else
      return( usage("linfrac", argc, argv, i-1, i) );
  } else {
    return( 0 );
  }
  *argi = i;
  *got_scale |= CMD_SCALE;
  return( 1 );
}

/*
 * Subroutine:	parse_display
 * Purpose:	Parse the command line for window and server parameters
 */
int parse_display ( argc, argv, argi, got_geo, displayname )
     int argc;			/* i: total number of arg tokens */
     char *argv[];		/* i: array of arg tokens */
     int *argi;			/* i/o: current arg in list */
     int *got_geo;		/* o: got server or window parameters */
     char **displayname;	/* o: name of display server */
{
  int i, temp;
  int usage(), parse_geometry();
  void set_magnifier(), redraw_magnifier(), SetTAEButtonLook();

  i = *argi;
  if( (strcmp(argv[i], "-d") == 0) || (strcmp(argv[i], "-display") == 0) ) {
    /* X window server socket name (overrides DISPLAY env variable) */
    *displayname = argv[++i];
  } else if( (strcmp(argv[i], "-g") == 0) ||
	     (strcmp(argv[i], "-geometry") == 0) ) {
    /* initial program desktop size and/or position */
    /* X window geometry (size and position) -g <width>x<height>+<x>+<y> */
    if( parse_geometry(argv[++i], 0) == 0 ) {
      (void)fprintf(stderr, "Error: Cannot parse geometry: %s\n", argv[i]);
      return( 0 );
    }
  } else if( strcmp(argv[i], "-gd") == 0 ) {
    /* initial display window size and/or desktop position */
    /* X window geometry (size and position) -gd <width>x<height>+<x>+<y> */
    if( parse_geometry(argv[++i], 1) == 0 ) {
      (void)fprintf(stderr, "Error: Cannot parse geometry: %s\n", argv[i]);
      return( 0 );
    }
  } else if( (strcmp(argv[i], "-bc") == 0) ||
	     (strcmp(argv[i], "-bordercolor") == 0) ) {
    /* border color */
    if( ++i >= argc )
      return( usage("border color", argc, argv, i-1, i) );
    border_color_name = argv[i];
  } else if( strcmp(argv[i], "-mtf") == 0) {
    /* make buttons less of a sore thumb when surrounded by motif */
    SetTAEButtonLook(2);
  } else if( strcmp(argv[i], "-mtfa") == 0) {
    /* as above but with even less highlighting */
    SetTAEButtonLook(1);
  } else if( strcmp(argv[i], "-mag") == 0 ) {
    /* magnifier magnification */
    if( ((++i) < argc) && ((temp = atoi(argv[i])) > 0) ) {
      magset.magnify = temp;
      /* if init has already been run, then set must be run to update */
      if( init_done && (magset.image == &magnibox.image) ) {
	set_magnifier();
	redraw_magnifier();
      }
    } else
      return( usage("magnifier", argc, argv, i-1, i) );
  } else if( strcmp(argv[i], "-panboxav") == 0 ) {
    img.panbox_zoomtype = SOP_ZoomAv;
  } else if( strcmp(argv[i], "-panboxsamp") == 0 ) {
    img.panbox_zoomtype = SOP_ZoomSamp;
  } else if( strcmp(argv[i], "-panboxsum") == 0 ) {
    img.panbox_zoomtype = SOP_ZoomSum;
  } else if( strcmp(argv[i], "-panboxmax") == 0 ) {
    img.panbox_zoomtype = SOP_ZoomMax;
  } else if( strcmp(argv[i], "-lprbuttons") == 0 ) {
    control.print_buttons = 1;
  } else {
    return( 0 );
  }
  *argi = i;
  *got_geo |= CMD_GEOMETRY;
  return( 1 );
}
