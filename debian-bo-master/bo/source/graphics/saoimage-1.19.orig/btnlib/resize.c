#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	resize.c (Resize Box)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Resize boxes and buttons for parent resize
 * Subroutines:	ResizeButtons()			returns: void
 * Subroutines:	static btn_ChkResize()		returns: void
 * Subroutines:	ResizeBox()			returns: void
 * Xlib calls:	XResizeWindow(), XDestroyImage(), XMoveResizeWindow()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		31 March 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>
#include "buttons.h"

/*
 * Subroutine:	ResizeButtons
 * Purpose:	Resize all buttons affected by resize of parent window
 * Returns:	void
 * Called by:	Application program
 * Uses:	btn_ChkResize()
 * Xlib calls:	none
 * Post-state:	All buttonboxes having given parent are resized to fill
 * Method:	Call btn_ChkResize with this menu and then its co-menus.
 */
void ResizeButtons ( buttonbox, parent )
     ButtonBox buttonbox;	/* i: top handle for button menu group */
     BoxParent *parent;		/* i: record describing parent window */
{
  int i;		/* l: loop counter */
  static void btn_ChkResize();

  /* check this buttonbox */
  btn_ChkResize(buttonbox, parent);
  /* check co-buttonboxes */
  for( i = 0; i < buttonbox->co_menu_count; i++ )
    btn_ChkResize(buttonbox->co_menu[i], parent);
}

/*
 * Subroutine:	btn_ChkResize
 * Purpose:	Resize all buttons affected by resize of parent window in
 *		this menu or any of the submenus of its buttons.
 * Returns:	void
 * Called by:	ButtonResize()
 * Uses:	ResizeBox() below, btn_ChkResize()-recursive
 * Xlib calls:	none
 * Post-state:	All buttonboxes having given parent are resized to fill
 * Method:	Check parentage of this box, then recurse on each submenu of
 *		each button.
 */
static void btn_ChkResize ( buttonbox, parent )
     ButtonBox buttonbox;	/* i: top handle for button menu tree */
     BoxParent *parent;		/* i: record describing parent window */
{
  int btn;		/* l: index of active button */
  int op;		/* l: index of button pressing modifier mask */
  int i;		/* l: index of button submenu */

  /* go through our window list to find a match */
  if( buttonbox->parentID == parent->wndwID )
    ResizeBox(buttonbox, parent);
  /* continue search through buttons' submenus (for each button) */
  for( btn = 0; btn < buttonbox->btn_cnt; btn++ ) {
    /* submenus can be specific to a selection option (for each op) */
    for( op = 0; op < buttonbox->buttons[btn].feel->nfunctions; op++ ) {
      /* if a button has submenus, check them out */
      if( buttonbox->buttons[btn].submenu_count[op] > 0 ) {
	/* for each submenu */
	for( i = 0; i < buttonbox->buttons[btn].submenu_count[op]; i++ )
	  btn_ChkResize((ButtonBox)buttonbox->buttons[btn].submenu[op][i],
			parent);
      }
    }
  }
}

/*
 * Subroutine:	ResizeBox
 * Purpose:	Resize all buttons in this box to given dimensions.
 * Returns:	void
 * Called by:	btn_ChkResize() above, application program
 * Uses:	btn_LabelButtons() in MakeBox.c
 * Xlib calls:	XResizeWindow(), XDestroyImage(), XMoveResizeWindow()
 * Post-state:	Buttonbox subwindow and buttons are resized to fill given size
 * Method:	Adjust subwindow exactly covering parent window.  Adjust
 *		button subwindows, destroy and recreate drawing stuff for
 *		each button.
 */
void ResizeBox ( buttonbox, parent )
     ButtonBox buttonbox;	/* i: handle for buttonbox record */
     BoxParent *parent;		/* i: record describing parent window */
{
  int btn_wdth, btn_hght;	/* l: dimensions for each button in box */
  int x, y;			/* l: pixel coord of button */
  unsigned int width, height;	/* l: pixel size of overall box */
  int i, j;			/* l: column and row loop counters */
  int btn;			/* l: index of accessed button */
  BoxGeometry *geo;		/* l: buttonbox's geometry parameters */
  void btn_Dimensions(), btn_LabelButtons();

  if( (buttonbox->parent_width == parent->width) &&
      (buttonbox->parent_height == parent->height) )
    return;
  /* resize the buttonbox subwindow */
  /* get the basic box and button dimensions */
  geo = buttonbox->geometry;
  btn_Dimensions(parent, geo, &btn_wdth, &btn_hght, &x, &y, &width, &height);
  XMoveResizeWindow(parent->display, buttonbox->wndwID, x, y, width, height);
  btn = 0;
  for( j = 0; j < geo->box_rows; j++ ) {
    for( i = 0; i < geo->box_cols; i++ ) {
      if( btn < geo->btn_cnt ) {
	XDestroyImage(buttonbox->buttons[btn].image[OFF_OUT]);
	XDestroyImage(buttonbox->buttons[btn].image[ON_OUT]);
	XDestroyImage(buttonbox->buttons[btn].image[OFF_IN]);
	XDestroyImage(buttonbox->buttons[btn].image[ON_IN]);
      }
      x = btn_wdth * i;
      y = btn_hght * j;
      XMoveResizeWindow(parent->display, buttonbox->buttons[btn].wndwID,
			x, y, btn_wdth, btn_hght);
      btn++;
    }
  }
  /* create XImage bitmaps for button visuals */
  btn_LabelButtons(buttonbox, btn_wdth, btn_hght,
		   geo->off_inverse, geo->on_inverse);
}
