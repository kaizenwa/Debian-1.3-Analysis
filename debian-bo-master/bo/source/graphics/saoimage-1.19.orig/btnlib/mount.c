#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	mount.c (Mount Box)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Map and unmap buttonbox windows and replace active submenus
 * Subroutine:	MountButtonMenu()			returns: void
 * Subroutine:	static btn_MapButtonbox()		returns: void
 * Subroutine:	static btn_UnmapButtonbox()		returns: void
 * Subroutine:	btn_ReplaceSubmenus()			returns: void
 * Subroutine:	btn_ReplaceCosubmenus()			returns: void
 * Xlib calls:	XMapWindow(), XUnmapWindow()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		31 March 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>	/* define stderr */
#include <X11/Xlib.h>	/* needed for Buttons.h */
#include "buttons.h"

/*
 * Subroutine:	MountButtonMenu
 * Purpose:	User call to initially display the button-panels
 * Returns:	void
 * Called by:	Application program
 * Uses:	btn_MapButtonbox()
 * Xlib calls:	none
 * Note:	This routine moves latterally to cover co-menus
 */
void MountButtonMenu ( buttonbox )
     ButtonBox buttonbox;
{
  int i;
  static void btn_MapButtonbox();

  btn_MapButtonbox(buttonbox);
  for( i = 0; i < buttonbox->co_menu_count; i++ )
    btn_MapButtonbox(buttonbox->co_menu[i]);
}

/*
 * Subroutine:	btn_MapButtonbox
 * Purpose:	Make the buttonbox and its current submenus visible
 * Returns:	void
 * Called by:	MountButtonbox() above
 * Called by:	btn_ReplaceSubmenus(), btn_ReplaceCosubmenus() below
 * Uses:	recursion
 * Xlib calls:	XMapWindow
 * Method:	Map the buttonbox and recurse on its submenus.
 */
static void btn_MapButtonbox ( buttonbox )
     ButtonBox buttonbox;
{
  int i;

  if( buttonbox != NULL ) {
    /* map this button box */
    XMapWindow(buttonbox->display, buttonbox->wndwID);
    /* mount the submenu(s) */
    for( i = 0; i < buttonbox->cosubmenu_count; i++ )
      btn_MapButtonbox(buttonbox->cosubmenu[i]);
    for( i = 0; i < buttonbox->submenu_count; i++ )
      btn_MapButtonbox(buttonbox->submenu[i]);
  }
}

/*
 * Subroutine:	btn_UnmapButtonbox
 * Purpose:	Make the buttonbox window vanish
 * Returns:	void
 * Called by:	btn_ReplaceSubmenus(), btn_ReplaceCosubmenus() below
 * Uses:	recursion
 * Xlib calls:	XUnmapWindow
 * Method:	Unmap the buttonbox and recurse on its submenus.
 */
static void btn_UnmapButtonbox ( buttonbox )
     ButtonBox buttonbox;
{
  int i;

  if( buttonbox != NULL ) {
    /* unmap this set of buttons */
    XUnmapWindow(buttonbox->display, buttonbox->wndwID);
    /* clear the submenu(s) */
    for( i = 0; i < buttonbox->cosubmenu_count; i++ )
      btn_UnmapButtonbox(buttonbox->cosubmenu[i]);
    for( i = 0; i < buttonbox->submenu_count; i++ )
      btn_UnmapButtonbox(buttonbox->submenu[i]);
  }
}

/*
 * Subroutine:	btn_ReplaceSubmenus
 * Purpose:	Change the currently active submenus from those from before to
 *		those of the identified button.
 * Returns:	void
 * Called by:	btn_PushButton() in PushButton.c
 * Uses:	btn_MapButtonbox(), btn_UnmapButtonbox() above.
 * Xlib calls:	none
 * Method:	Unmap the current submenus, link to buttonbox active list and
 *		map submenus of new button.
 * Note:	Unmapping submenus is harmless if they were not mapped.
 */
void btn_ReplaceSubmenus ( buttonbox, btn, op_num, mapping )
     ButtonBox buttonbox;
     int btn;
     int op_num;
     int mapping;		/* i: switch allows suppression of mapping */
{
  int i;
  static void btn_UnmapButtonbox(), btn_MapButtonbox();

  /* clear out the old submenus */
  for( i=0; i < buttonbox->submenu_count; i++ )
    btn_UnmapButtonbox(buttonbox->submenu[i]);
  /* install the new submenus */
  for( i=0; i < buttonbox->buttons[btn].submenu_count[op_num]; i++ ) {
    /* put submenus in list of active submenus */
    buttonbox->submenu[i] =
      (ButtonBox)buttonbox->buttons[btn].submenu[op_num][i];
    /* identify master to submenu */
    buttonbox->submenu[i]->parentmenu = buttonbox;
    /* display the new submenus */
    if( mapping )
      btn_MapButtonbox(buttonbox->submenu[i]);
  }
  /* update the count of active submenus */
  buttonbox->submenu_count = buttonbox->buttons[btn].submenu_count[op_num];
}

/*
 * Subroutine:	btn_ReplaceCosubmenus
 * Purpose:	Change the currently active cosubmenus from those from before
 *		to those of the identified button.
 * Returns:	void
 * Called by:	btn_PushButton() in PushButton.c
 * Uses:	btn_MapButtonbox(), btn_UnmapButtonbox() above.
 * Xlib calls:	none
 * Method:	Unmap the current cosubmenus, link to buttonbox active list and
 *		map cosubmenus of new button.
 * Note:	Requires search through co-menus
 * Note:	Unmapping cosubmenus is harmless if they were not mapped.
 */
void btn_ReplaceCosubmenus ( buttonbox, btn, op_num, mapping )
     ButtonBox buttonbox;
     int btn;
     int op_num;
     int mapping;		/* i: switch allows suppression of mapping */
{
  int i, j;
  static void btn_UnmapButtonbox(), btn_MapButtonbox();

  /* check to see if we have the co-mode submenus */
  if( buttonbox->cosubmenu_count > 0 ) {
    /* clear out the old cosubmenus */
    for( i=0; i < buttonbox->cosubmenu_count; i++ )
      btn_UnmapButtonbox(buttonbox->cosubmenu[i]);
  } else {
    /* clear any co-mode submenus in our co_menus */
    for( j = 0; j < buttonbox->co_menu_count; j++ ) {
      if( buttonbox->co_menu[j]->cosubmenu_count > 0 ) {
	/* clear out the old cosubmenus */
	for( i=0; i < buttonbox->co_menu[j]->cosubmenu_count; i++ )
	  btn_UnmapButtonbox(buttonbox->co_menu[j]->cosubmenu[i]);
	buttonbox->co_menu[j]->cosubmenu_count = 0;
      }
    }
  }
  /* install the new cosubmenus */
  for( i=0; i < buttonbox->buttons[btn].submenu_count[op_num]; i++ ) {
    /* put submenus in list of active submenus */
    buttonbox->cosubmenu[i] =
      (ButtonBox)buttonbox->buttons[btn].submenu[op_num][i];
    /* identify master to submenu */
    buttonbox->cosubmenu[i]->parentmenu = buttonbox;
    /* display the new submenus */
    if( mapping )
      btn_MapButtonbox(buttonbox->cosubmenu[i]);
  }
  /* update the count of active submenus */
  buttonbox->cosubmenu_count = buttonbox->buttons[btn].submenu_count[op_num];
}
