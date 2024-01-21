#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wndwadj.c (Window Adjust)
 * Subroutine:	adjust_desktop()	returns: void
 * Subroutine:	adjust_graphbox()	returns: void
 * Xlib calls:	XResizeWindow()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		6 July 1989
 *		{1} MVH added adjust_graphbox()			21 Oct 1989
 *		{2} RPS added code to set the init_button flag when window is
 *		resized.   13 May 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/struct.h"		/* all struct record types */
#include "hfiles/extern.h"		/* major declared structs */
#include "hfiles/cgraph.h"

extern struct cgraphRec cgraph;
extern struct windowRec desktop;
#ifdef ALLIANT
extern int init_button;
#endif


/*
 * Subroutine:	adjust_desktop
 * Purpose:	Adjust displays when the main desktop window is adjusted.
 */
void adjust_desktop ( configure )
     XConfigureEvent *configure;
{
  int diff;
  int do_dispbox = 0;
  int do_graphbox = 0;
  void map_graphbox(), adjust_main_colorbar(), free_blink();
  void init_dispbuf(), new_pancursor(), new_display(), adjust_buttonmenu();

  if( configure->window != desktop.ID )
    return;
  if( configure->height != desktop.height ) {
    diff = configure->height - desktop.height;
    desktop.height = configure->height;
    desktop.yheight += diff;
    dispbox.height += diff;
    dispbox.yheight += diff;
    do_dispbox = 1;
    colorbox.y += diff;
  }
  if( configure->width != desktop.width ) {
    diff = configure->width - desktop.width;
    desktop.width = configure->width;
    desktop.xwidth += diff;
    colorbox.width += diff;
    colorbox.xwidth += diff;
    XResizeWindow(colorbox.display, colorbox.ID,
		  colorbox.width, colorbox.height);
    adjust_main_colorbar();
    btnbox.width += diff;
    btnbox.xwidth += diff;
    XResizeWindow(btnbox.display, btnbox.ID, btnbox.width, btnbox.height);
    adjust_buttonmenu (&btnbox);
#ifdef ALLIANT
    init_button=1;
#endif
    dispbox.width += diff;
    dispbox.xwidth += diff;
    do_dispbox = 1;
    panbox.x += diff;
    magnibox.x += diff;
  }
  if( do_dispbox ) {
    /* dump the blink buffers and pixmaps, they are no linger usable */
    free_blink();
    XResizeWindow(dispbox.display, dispbox.ID, dispbox.width, dispbox.height);
    init_dispbuf();
    new_pancursor(0);
    new_display(0, 0, 0, 0);
  }
  if( do_graphbox )
    /* remap the graphbox (it checks for its location) */
    map_graphbox();
}

/*
 * Subroutine:	adjust_graphbox()
 * Purpose:	respond to a resizing of the color graph window
 */
void adjust_graphbox ( configure )
     XConfigureEvent *configure;
{
  void adjust_color_graph(), adjust_graph_colorbar(), draw_cgraph();

  graphbox.x = configure->x;
  graphbox.y = configure->y;
  graphbox.width = configure->width;
  graphbox.height = configure->height;
  graphbox.xwidth = graphbox.width - (graphbox.xzero + graphbox.xzero);
  graphbox.yheight = graphbox.height - (graphbox.yzero + graphbox.yzero);
  adjust_color_graph();
  adjust_graph_colorbar();
  if( graphbox.active )
    draw_cgraph(1, 0);
}

#ifdef FOO
/*
 * Subroutine:	set_dispborder
 */
void set_dispborder ( dispbdr )
     int dispbdr;
{
  desktop.bdrwidth = dispbdr;
  desktop.bdrtotal = dispbdr + dispbdr;
  graphbox.bdrwidth = dispbdr;
  graphbox.bdrtotal = dispbdr + dispbdr;
}

/*
 * Subroutine:	set_auxborder
 */
void set_auxborder ( auxbdr )
     int auxbdr;
{
  panbox.bdrwidth = auxbdr;
  panbox.bdrtotal = auxbdr + auxbdr;
  magnibox.bdrwidth = auxbdr;
  magnibox.bdrtotal = auxbdr + auxbdr;
  dispbox.bdrwidth = auxbdr;
  dispbox.bdrtotal = auxbdr + auxbdr;
}
#endif
