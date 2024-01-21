#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	attach.c (Attach Box)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Attach boxes to buttons as submenus and boxes to boxes comenus
 * Subroutines:	AttachSubmenu()			returns: void
 * Subroutines:	JoinMenu()			returns: void
 * Xlib calls:	none
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
#include <X11/Xlib.h>	/* define Xlib types and calls (needed by Buttons.h) */
#include "buttons.h"

/*
 * Subroutine:	AttachSubmenu
 * Purpose:	Attach a buttonbox as a submenu to a button
 * Returns:	void
 * Called by:	Application program
 * Xlib calls:	none
 * Method:	Identify btn's function which matches mask and reference, and
 *		set pointer in that functions submenu array.
 * Note:	Subbuttonbox(es) are drawn and activated when the button is
 *		selected and dumped when the button is unselected
 * Note:	Bitmask codes indicate which button option is intended
 */
void AttachSubmenu ( subbox, masterbox, btn, mask, reference )
     ButtonBox subbox;		/* i: box being attached */
     ButtonBox masterbox;	/* i: box with button recieving submenu */
     int btn;			/* i: index of button to which to attach */
     int mask;			/* i: mask of button's submenu function */
     int reference;		/* i: reference of button's submenu function */
{
  ButtonRecord *button;		/* l: pointer to record of object button */
  ButtonFeel *feel;		/* l: pointer to button's feel parameters */
  int fnum;			/* l: index of response function */
  int i;			/* l: loop counter */

  /* is the button index in error? */
  if( (btn < 0) || (btn >= masterbox->btn_cnt) ) {
    (void)fprintf(stderr, "WARNING: attaching to invalid-button: %d\n", btn);
    return;
  }
  /* isolate the object button's ButtonRecord and ButtonFeel */
  button = &masterbox->buttons[btn];
  feel = button->feel;
  /* get the correct detail option index */
  for( fnum = 0; fnum < feel->nfunctions; fnum++ )
    if( (feel->mask[fnum] == mask) && (feel->reference[fnum] == reference) )
      break;
  if( fnum >= feel->nfunctions ) {
    (void)fprintf
      (stderr, "WARNING: attaching submenu to non-existent option: %d %d\n",
       mask, reference);
    return;
  }
  /* can we accomodate any more submenus? */
  if( button->submenu_count[fnum] >= SUBMENU_LIMIT ) {
    (void)fprintf
      (stderr, "WARNING: attempt to attach too many submenus. %d\n", btn);
    return;
  }
  /* is it the kind of button that takes submenus? */
  if( (feel->function[fnum] != BTNMode) &&
      (feel->function[fnum] != BTNToggle) &&
      (feel->function[fnum] != BTNCoMode) ) {
    (void)fprintf(stderr, "WARNING: attaching to non-mode button: %d\n", btn);
  }
  /* is it already attached? */
  for( i=0; i < button->submenu_count[fnum]; i++ ) {
    if( (ButtonBox)button->submenu[fnum][i] == subbox ) {
      (void)fprintf(stderr,
		    "NOTE: submenu already attached to button: %d\n", btn);
      return;
    }
  }
  /* set pointer to buttonbox (which was declared as int *) */
  button->submenu[fnum][button->submenu_count[fnum]] = (int *)subbox;
  ++button->submenu_count[fnum];
}

/*
 * Subroutine:	JoinMenu
 * Purpose:	Attach buttonboxes as co_menus to act as one buttonbox in the
 *		cases of co_mode and while button actions.
 * Returns:	void
 * Called by:	Application program
 * Xlib calls:	none
 * Method:	Place box pointers in each other's co_menu lists.
 * Notea:	Order of A and B is not important.
 */
void JoinMenus ( box_A, box_B )
     ButtonBox	box_A;		/* i: first of pair being joined */
     ButtonBox	box_B;		/* i: second of pair being joined */
{
  int i;		 	/* l: loop counter */

  /* can either box accomodate any more co_menus? */
  if( (box_A->co_menu_count >= SUBMENU_LIMIT) ||
      (box_B->co_menu_count >= SUBMENU_LIMIT) ) {
    (void)fprintf(stderr, "WARNING: attempt to join too many buttonboxes\n");
    return;
  }
  /* are they already joined */
  for( i=0; i < box_A->co_menu_count; i++ ) {
    if( box_A->co_menu[i] == box_B ) {
      (void)fprintf(stderr, "NOTE: submenus already joined\n");
      return;
    }
  }
  /* set pointer to buttonbox (which was declared as int *) */
  box_A->co_menu[box_A->co_menu_count] = box_B;
  box_B->co_menu[box_B->co_menu_count] = box_A;
  ++box_A->co_menu_count;
  ++box_B->co_menu_count;
}
