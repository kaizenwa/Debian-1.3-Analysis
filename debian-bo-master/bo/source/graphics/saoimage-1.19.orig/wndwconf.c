#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wndwconf.c (Window Configure)
 * Purpose:	Determine size of desktop and mode of presentation to
 *		display screen window manager (to later establish location
 *		and dimension of individual windows - see WndwConfig.c
 * Subroutine:	init_desktop()			returns: void
 * Subroutine:	configure_windowgroup()		returns: void
 * Subroutine:	configure_graphbox()		returns: void
 * Subroutine:	parse_geometry()		returns: int
 * Xlib calls:	XParseGeometry(), XTranslateCoordinates()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		30 April 1989
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/define.h"		/* MIN, MAX, and more */
#include "hfiles/struct.h"		/* all struct record types */
#include "hfiles/extern.h"		/* major declared structs */

extern struct windowRec desktop;
extern int screen_width, screen_height;	/* screen dimensions in WndwInit.c */

static int geo_flag = 0;		/* flag & vals from XParseGeometry */
static int geo_x, geo_y;
static unsigned int geo_width, geo_height;

/*
 * Subroutine:	init_desktop
 * Purpose:	Select the SAOimage window area.
 * PostState:	Windows size hints altered or set
 * Called by:	init_windows() in WndwInit.c
 * Method:	The size can be given or set to norm.  If given but less
 *		than min (i.e. 0,0) it becomes the minimum configuration.
 */
void init_desktop ( )
{
  static void set_parsed_geometry();

  desktop.hints.min_width =
    (2 * desktop.xzero) + (2 * dispbox.bdrwidth) + dispbox.hints.min_width;
  desktop.hints.max_width =
    (2 * desktop.xzero) + (2 * dispbox.bdrwidth) + dispbox.hints.width;
  desktop.hints.min_height = desktop.yzero +
    panbox.bdrtotal + panbox.hints.min_height +
      btnbox.bdrtotal + btnbox.hints.min_height + 2 +
	dispbox.bdrtotal + dispbox.hints.min_height +
	  colorbox.bdrtotal + colorbox.hints.min_height;
  desktop.hints.max_height = desktop.yzero +
    panbox.bdrtotal + panbox.hints.height +
      btnbox.bdrtotal + btnbox.hints.height + 2 +
	dispbox.bdrtotal + dispbox.hints.height +
	  colorbox.bdrtotal + colorbox.hints.height;
  /* if the user specified geometry */
  if( geo_flag )
    set_parsed_geometry();
  if( (geo_flag & WidthValue) == 0 )
    desktop.hints.width = desktop.hints.max_width;
  if( (geo_flag & HeightValue) == 0 )
    desktop.hints.height = desktop.hints.max_height;
}

/*
 * Subroutine:	configure_windowgroup
 * Purpose:	Set the x, y, width, and height, of all windows
 * Called by:	init_windows in WndwInit.c
 * Called by:	regroup_windows in WndwAdjust.c
 */
void configure_windowgroup ( adjust_defaults )
     int adjust_defaults;
{
  void init_panbox_dimensions();

  if( adjust_defaults ) {
    /* choose relative sizes based on overall desktop area */
    if( desktop.width < desktop.hints.max_width ) {
      magnibox.hints.width = magnibox.hints.min_width;
      panbox.hints.max_width = panbox.hints.min_width;
    }
    if( desktop.height < desktop.hints.max_height ) {
      magnibox.hints.height = magnibox.hints.min_height;
      panbox.hints.max_height = panbox.hints.min_height;
      btnbox.hints.height = btnbox.hints.min_height;
      if( desktop.height < 500 ) {
	colorbox.hints.height = colorbox.hints.min_height;
	graphbox.hints.height = graphbox.hints.min_height;
	graphbox.hints.width = graphbox.hints.min_width;
      }
    }
    /* choose a panbox size within the guidelines */
    init_panbox_dimensions();
  }
  /* sizes now known, fill in the rest based on the configuration */
  magnibox.hints.y = desktop.yzero;
  magnibox.hints.x = desktop.width -
    (desktop.xzero + magnibox.bdrtotal + magnibox.hints.width);
  panbox.hints.y = magnibox.hints.y;
  panbox.hints.x = magnibox.hints.x -
    ((2 * desktop.xzero) + panbox.bdrtotal + panbox.hints.width);
  btnbox.hints.y = MAX(magnibox.hints.height, panbox.hints.height) +
    btnbox.bdrwidth + magnibox.hints.y + magnibox.bdrtotal + 1;
  if( btnbox.bdrwidth > 0 ) {
    btnbox.hints.x = desktop.xzero;
    btnbox.hints.width = desktop.xwidth -  btnbox.bdrtotal;
  } else {
    btnbox.hints.x = 0;
    btnbox.hints.width = desktop.width;
  }
  colorbox.hints.y = desktop.height -
    (colorbox.bdrwidth + colorbox.hints.height + colorbox.bdrtotal);
  if( colorbox.bdrwidth > 0 ) {
    colorbox.hints.x = desktop.xzero;
    colorbox.hints.width = desktop.xwidth - colorbox.bdrtotal;
  } else {
    colorbox.hints.x = 0;
    colorbox.hints.width = desktop.width;
  }
  dispbox.hints.x = desktop.xzero;
  dispbox.hints.y = 1 +
    btnbox.hints.y + btnbox.bdrtotal + btnbox.hints.height + btnbox.bdrwidth;
  dispbox.hints.width = desktop.width -
    ((2 * desktop.xzero) + dispbox.bdrtotal);
  dispbox.hints.height = (colorbox.hints.y - dispbox.hints.y) -
    (dispbox.bdrtotal + btnbox.bdrwidth + colorbox.bdrwidth);
}

/*
 * Subroutine:	configure_graphbox
 * Purpose:	Set the graphbox to appear undereath colorbar
 * Xlib calls:	XTranslateCoordinates()
 */
void configure_graphbox ()
{
  Window child, *ch;
  Window parent, root;
  int left_x, right_x, lo_y, hi_y;
  int head_y, head_height;
  unsigned int nch;

  /* get desktop's parent and root */
  if( XQueryTree(desktop.display, desktop.ID, &root, &parent, &ch, &nch) == 0 )
    return;
  XFree((char *)ch);
  /* get current root window coords of desktop */
  (void)XTranslateCoordinates(desktop.display, desktop.ID, root,
			      desktop.width, desktop.height,
			      &right_x, &lo_y, &child);
  (void)XTranslateCoordinates(desktop.display, desktop.ID, root,
			      0, 0, &left_x, &hi_y, &child);
  /* if desktop's parent is not root, window manager gave it a header */
  if( parent != root ) {
    (void)XTranslateCoordinates(desktop.display, parent, root,
				0, 0, &left_x, &head_y, &child);
    head_height = hi_y - head_y;
  } else {
    head_height = 0;
    head_y = hi_y;
  }
  if( graphbox.hints.height > graphbox.hints.width ) {
    graphbox.hints.y = hi_y - 2;
    if( (right_x + graphbox.hints.width) < screen_width )
      graphbox.hints.x = right_x + 2;
    else
      graphbox.hints.x = left_x - (graphbox.hints.width + 4);
  } else {
    graphbox.hints.x = left_x - 2;
    graphbox.hints.width = desktop.width;
    /* put graphbox above colorbox if desktop is too close to bottom */
    if( (screen_height - lo_y) <
       (graphbox.hints.height + head_height + graphbox.bdrtotal) )
      graphbox.hints.y = MAX(head_y - (head_height + 4), 0);
    else
      graphbox.hints.y = lo_y + head_height + 3;
  }
}

/*
 * Subroutine:	parse_geometry
 * Purpose:	Parse a user given geometry string
 * Returns:	0 if no values were successfully parsed, else 1
 * Xlib calls:	XParseGeometry()
 */
int parse_geometry ( geometry, disp_size )
     char *geometry;
     int disp_size;	/* i: apply size to: 1=dispbox, 0=desktop */
{
  geo_flag = XParseGeometry(geometry, &geo_x, &geo_y, &geo_width, &geo_height);
  if( geo_flag != 0 ) {
    if( disp_size ) {
      if( geo_flag & WidthValue ) {
	dispbox.hints.width = geo_width;
	geo_flag ^= WidthValue;
      }
      if( geo_flag & HeightValue ) {
	dispbox.hints.height = geo_height;
	geo_flag ^= HeightValue;
      }
    }
    return( 1 );
  } else
    return( 0 );
}

/*
 * Subroutine:	set_parsed_geometry
 * Purpose:	Set up the desktop window's size_hints as per any parsed
 *		user specified geometry
 */
static void set_parsed_geometry ( )
{
  if( geo_flag == 0 )
    return;
  if( geo_flag & WidthValue )
    desktop.hints.width = (unsigned int)
      MIN(MAX(geo_width, desktop.hints.min_width), screen_width);
  if( geo_flag & HeightValue )
    desktop.hints.height = (unsigned int)
      MIN(MAX(geo_height, desktop.hints.min_height), screen_height);
  if( geo_flag & XValue ) {
    if( geo_flag & XNegative ) {
      /* I'm not sure what the standard is supposed to be here */
      if( geo_x < 0 )
	desktop.hints.x = screen_width - (desktop.hints.width - geo_x);
      else
	desktop.hints.x = screen_width - (desktop.hints.width + geo_x);
      desktop.hints.x -= desktop.bdrtotal;
    } else
      desktop.hints.x = geo_x;
  }
  if( geo_flag & YValue ) {
    if( geo_flag & YNegative ) {
      if( geo_y < 0 )
	desktop.hints.y = screen_height - (desktop.hints.height - geo_y);
      else
	desktop.hints.y = screen_height - (desktop.hints.height + geo_y);
      desktop.hints.y -= desktop.bdrtotal;
    } else
      desktop.hints.y = geo_y;
  }
  if( (geo_flag & XValue) || (geo_flag & YValue) ) {
    desktop.hints.flags =
      (USPosition | USSize) | (desktop.hints.flags & (~(PPosition | PSize)));
  } else if( (geo_flag & WidthValue) || (geo_flag & HeightValue) ) {
    desktop.hints.flags = USSize | (desktop.hints.flags & (~PSize));
  }
}
