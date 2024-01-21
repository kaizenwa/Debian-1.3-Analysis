#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	menuinit.c (Menu Initialize)
 * Purpose:	Create and resize menu button panels
 * Subroutine:  control_buttonbox()		returns: int *
 * Subroutine:	init_buttonmenus()		returns: void
 * Subroutine:	mount_buttonmenus()		returns: void
 * Subroutine:	resize_buttonmenu()		returns: void
 * Subroutine:	init_buttonbox_settings()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 4 July 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define NULL */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "btnlib/buttons.h"
#include "hfiles/window.h"

BoxParent parent[4];
ButtonBox box;

/*
 * Subroutine:	control_buttonbox
 * Purpose:	boxcontrol from a remote file
 * Returns:	Pointer to data of just activated button, else NULL pointer
 * Pre-state:	Most recent event from XNextEvent() in event
 * Post-state:	Mouse button event of selection, or a client event
 * Called by:	main event loop
 * Uses:	ButtonControl() in buttonlib/libbtn.a (EventCtrl.c)
 * Note:	ButtonControl returns -1 on non-X event from UNIX select
 */
int *control_buttonbox ( event )
     XEvent *event;	/* i/o: event yet to be fielded or just fielded */
{
  int *response;
  if( ButtonControl(box, event, &response) == 1 )
    return( response );
  else
    return( NULL );
}

/*
 * Subroutine:	init_buttonmenu
 * Purpose:	Create the main button menu and fit it to its windows
 * Note:	Uses preprocessed code in panel subdirectory
 */
void init_buttonmenu ( btnbox, gc, visual, foreground, background )
     struct windowRec *btnbox;
     GC gc;			/* i: optional, else 0 */
     Visual *visual;		/* i: optional, else 0 */
     unsigned long foreground;
     unsigned long background;	/* i: optional, else 0 */
{
  ButtonBox CreateMenu();	/* i: precompiled in panel/MakeMenu.c */

  parent[0].display = btnbox->display;
  parent[0].wndwID = btnbox->ID;
  parent[0].x = btnbox->xzero;
  parent[0].y = btnbox->yzero;
  parent[0].width = btnbox->width;
  parent[0].height = btnbox->height;
  parent[0].xwdth = btnbox->xwidth;
  parent[0].yhght = (parent[0].height / 2) + 2;

  parent[1].display = btnbox->display;
  parent[1].wndwID = btnbox->ID;
  parent[1].x = parent[0].x;
  parent[1].y = parent[0].y + parent[0].yhght;
  parent[1].width = btnbox->width;
  parent[1].height = btnbox->height;
  parent[1].xwdth = btnbox->xwidth;
  parent[1].yhght = btnbox->yheight - parent[0].yhght;
 
  parent[2].display = btnbox->display;
  parent[2].wndwID = btnbox->ID;
  parent[2].x = parent[1].x;
  parent[2].y = parent[1].y;
  parent[2].width = btnbox->width;
  parent[2].height = btnbox->height;
  parent[2].xwdth = (7 * btnbox->xwidth) / 8;
  parent[2].yhght = parent[1].yhght;

  parent[3].display = btnbox->display;
  parent[3].wndwID = btnbox->ID;
  parent[3].x = parent[1].x + (parent[1].xwdth + 6) / 7;
  parent[3].y = parent[1].y;
  parent[3].width = btnbox->width;
  parent[3].height = btnbox->height;
  parent[3].xwdth = parent[1].xwdth - (parent[3].x - parent[1].x);
  parent[3].yhght = parent[1].yhght;

  box = CreateMenu (parent, gc, visual, background);
}

/*
 * Subroutine:	mount_buttonmenu
 * Purpose:	Put the menu up on the screen
 */
void mount_buttonmenu ( )
{
  MountButtonMenu (box);
}

/*
 * Subroutine:	adjust_buttonmenu
 * Purpose:	Adjust all buttons in the buttonmenu
 * Note:	Uses precompiled call in panel/MakeMenu.c
 */
void adjust_buttonmenu ( btnbox )
     struct windowRec *btnbox;
{
  int flags[4];
  void ResizeMenu();

  parent[0].width = btnbox->width;
  parent[0].height = btnbox->height;
  parent[0].xwdth = btnbox->xwidth;
  flags[0] = 1;
  parent[1].width = btnbox->width;
  parent[1].height = btnbox->height;
  parent[1].xwdth = btnbox->xwidth;
  flags[1] = 1;
  parent[2].width = btnbox->width;
  parent[2].height = btnbox->height;
  parent[2].xwdth = (7 * btnbox->xwidth) / 8;
  flags[2] = 1;
  parent[3].x = parent[1].x + (parent[1].xwdth + 6) / 7;
  parent[3].width = btnbox->width;
  parent[3].height = btnbox->height;
  parent[3].xwdth = parent[1].xwdth - (parent[3].x - parent[1].x);
  flags[3] = 1;

  ResizeMenu (parent, flags);
}
