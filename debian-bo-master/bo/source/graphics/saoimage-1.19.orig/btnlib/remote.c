#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	remote.c (Remote Control)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Simulate buttonbox event
 * Subroutine:	PushButton()			returns: int
 * Subroutine:	TouchButton()			returns: int
 * Subroutine:	ReleaseButton()			returns: int
 * Subroutine:	SetToggleButton()		returns: int
 * Subroutine:	DisableButton()			returns: int
 * Subroutine:	EnableButton()			returns: int
 * Subroutine:	ButtonNumber()			returns: int
 * Subroutine:	ButtonStatus()			returns: int
 * Xlib calls:	XSelectInput()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		31 March 1989
 *		{1} Jay Travisano (STScI) VMS,IMTOOL changes	10 Nov   1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>	/* get stderr */
#include <X11/Xlib.h>	/* needed by Buttons.h */
#include "buttons.h"

#if VMS && IMTOOL
extern void XZ_ast();
extern int  XZ_efn;
#endif

/*
 * Subroutine:	PushButton
 * Purpose:	Handle buttonbox behavior to simulate a mouse button being
 *		pressed while the mouse cursor is in a buttonbox button
 * Returns:	1 if able to find button and button function, else 0
 * Called by:	Application program
 * Uses:	btn_PushButton() in PushButton.c
 * Xlib calls:	none
 * Post-state:	Records updated, submenus replaced, buttons highlighted
 * Note:	detail=(event.xbutton.button<<16)|(event.xbutton.state&0xffff)
 * Note:	event filtering to select a response is detail&mask==reference
 */
int PushButton ( buttonbox, btn, mouse_btn, state, mapping )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
     int mouse_btn;		/* i: mouse button: event.xbutton.button */
     int state;			/* i: modifier|button: event.xbutton.state */
     int mapping;		/* i: indicate whether to map submenus */
{
  int btn_PushButton();

  /* is the button index in error? */
  if( (btn < 0) || (btn >= buttonbox->btn_cnt) ) {
    (void)fprintf(stderr,"WARNING: pressed button not a button: %d\n", btn);
    return( 0 );
  }
  return( btn_PushButton(buttonbox, btn, mouse_btn, state, mapping) );
}

/*
 * Subroutine:	TouchButton
 * Purpose:	Do as if a mouse button is clicked and released in a button
 * Returns:	1 if able to find button and button function, else 0
 * Called by:	Application program
 * Uses:	btn_PushButton() in PushButton.c, ReleaseButton()
 * Xlib calls:	none
 * Post-state:	Records updated, submenus replaced, buttons highlighted if
 *		appropriate
 */
int TouchButton ( buttonbox, btn, mouse_btn, state, mapping )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
     int mouse_btn;		/* i: mouse button: event.xbutton.button */
     int state;			/* i: modifier|button: event.xbutton.state */
     int mapping;		/* i: indicate whether to map submenus */
{
  int btn_PushButton();
  int ReleaseButton();

  /* is the button index in error? */
  if( (btn < 0) || (btn >= buttonbox->btn_cnt) ) {
    (void)fprintf(stderr,"WARNING: touched button not a button: %d\n", btn);
    return( 0 );
  }
  if( btn_PushButton(buttonbox, btn, mouse_btn, state, mapping) == 0 )
    return( 0 );
  (void)ReleaseButton (buttonbox, btn);
  return( 1 );
}

/*
 * Subroutine:	ReleaseButton
 * Purpose:	Do as if the mouse button which was down is released in a
 *		button
 * Returns:	1 if able to find button and it was being pressed, else 0
 * Called by:	Application program
 * Uses:	btn_ReleaseButton() in PushButton.c
 * Uses:	btn_DrawButton() in DrawButton.c
 * Xlib calls:	none
 * Post-state:	Records updated and buttons highlighted if appropriate
 */
int ReleaseButton ( buttonbox, btn )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
{
  int type;			/* l: response function type */
  void btn_ReleaseButton(), btn_DrawButton();

  /* is the button index in error? */
  if( (btn < 0) || (btn >= buttonbox->btn_cnt) ) {
    (void)fprintf(stderr,"WARNING: released button not a button: %d\n", btn);
    return( 0 );
  }
  if( btn != buttonbox->down_btn ) {
    (void)fprintf(stderr, "WARNING: releasing button which is not down\n");
    return( 0 );
  }
  type = buttonbox->buttons[btn].feel->function[buttonbox->down_btn_func];
  /* do all the book-keeping chores (and handle BTNCoWhile) */
  btn_ReleaseButton(buttonbox, type, btn);
  /* draw the button in its released state (if different) */
  if( (type == BTNWhile) || (type == BTNOneShot) )
    btn_DrawButton(&buttonbox->buttons[btn]);
  return( 1 );
}

/*
 * Subroutine:	SetToggleButton
 * Purpose:	Set a toggle button to indicate the indicated state
 * Returns:	0 if unable to find button and its toggle function, 2 if it
 *		changed state from what it was, else 1
 * Called by:	Application program
 * Uses:	btn_PushButton() in PushButton.c, ReleaseButton()
 * Xlib calls:	none
 * Post-state:	Records updated and buttons (un)highlighted as appropriate
 */
int SetToggleButton ( buttonbox, btn, status )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
     int status;		/* i: 1 for on, 0 for off */
{
  int fnum;
  int TouchButton(), ReleaseButton(), btn_PushButton();

  /* is the button index in error? */
  if( (btn < 0) || (btn >= buttonbox->btn_cnt) ) {
    (void)fprintf(stderr,"WARNING: toggled button not a button: %d\n", btn);
    return( 0 );
  }
  /* note which button option it is */
  for( fnum = 0; fnum < buttonbox->buttons[btn].feel->nfunctions; ++fnum )
    if( buttonbox->buttons[btn].feel->function[fnum] == BTNToggle )
      break;
  if( fnum >= buttonbox->buttons[btn].feel->nfunctions ) {
    (void)fprintf(stderr,"WARNING: set-toggle on non-toggle button: %d\n",btn);
    return( 0 );
  }
  if( status != buttonbox->buttons[btn].selected ) {
    int ref;

    /* touch button, mapping arg only applies to mode buttons */
    ref = buttonbox->buttons[btn].feel->reference[fnum];
    if( btn_PushButton(buttonbox, btn, ref >> 16, ref & 0xffff, 0) != 0 )
      (void)ReleaseButton(buttonbox, btn);
    return( 2 );
  }
  return( 1 );
}

/*
 * Subroutine:	DisableButton
 * Purpose:	Render a button unchangeable (make its basic function NoOp)
 * Returns:	1 if able to find button, else 0
 * Xlib calls:	XSelectInput()
 * Called by:	Application program
 * Note:	If button is to change state, application should call
 *		a different routine to do so before calling this one
 * Note:	Highlighting state is unchanged, but occupation is undone
 * Note:	Only affects function 0 (the primary function)
 */
int DisableButton ( buttonbox, btn )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
{
  void btn_DrawButton();

  /* is the button index in error? */
  if( (btn < 0) || (btn >= buttonbox->btn_cnt) ) {
    (void)fprintf(stderr,"WARNING: disable button not a button: %d\n", btn);
    return( 0 );
  }
  if( buttonbox->buttons[btn].occupied ) {
    buttonbox->buttons[btn].occupied = 0;
    btn_DrawButton(&buttonbox->buttons[btn]);
  }
#if VMS && IMTOOL
  XSelectAsyncInput(buttonbox->display, buttonbox->buttons[btn].wndwID,
		    ExposureMask | ButtonPressMask | ButtonReleaseMask,
		    XZ_ast, XZ_efn);
#endif
  XSelectInput(buttonbox->display, buttonbox->buttons[btn].wndwID,
	       ExposureMask | ButtonPressMask | ButtonReleaseMask);
  buttonbox->buttons[btn].feel->function[0] = BTNNoOp;
  return( 1 );
}

/*
 * Subroutine:	EnableButton
 * Purpose:	Enable a button which was previously a NoOp
 * Returns:	1 if able to find button, else 0
 * Xlib calls:	XSelectInput()
 * Called by:	Application program
 * Note:	Application must know its new function
 * Note:	Only affects function 0 (the primary function)
 */
int EnableButton ( buttonbox, btn, function )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
     int function;		/* i: code for function response type */
{
  /* is the button index in error? */
  if( (btn < 0) || (btn >= buttonbox->btn_cnt) ) {
    (void)fprintf(stderr,"WARNING: enable button not a button: %d\n", btn);
    return( 0 );
  }
  buttonbox->buttons[btn].feel->function[0] = function;
  if( buttonbox->buttons[btn].feel->nfunctions == 0 )
    buttonbox->buttons[btn].feel->nfunctions = 1;
#if VMS && IMTOOL
  XSelectAsyncInput(buttonbox->display, buttonbox->buttons[btn].wndwID,
		    ButtonReleaseMask | ButtonPressMask |
		    EnterWindowMask | LeaveWindowMask | ExposureMask,
		    XZ_ast, XZ_efn);
#endif
  XSelectInput(buttonbox->display, buttonbox->buttons[btn].wndwID,
	       ButtonReleaseMask | ButtonPressMask |
	       EnterWindowMask | LeaveWindowMask | ExposureMask);
  return( 1 );
}

/*
 * Subroutine:	ButtonStatus
 * Purpose:	Return the on/off status of a button
 * Returns:	-1 if button not found, 1 if button is currently selected (on),
 *		0 if button is currently not selected (off)
 * Called by:	Application program
 * Xlib calls:	none
 */
int ButtonStatus ( buttonbox, btn )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
{
  /* is the button index in error? */
  if( (btn < 0) || (btn >= buttonbox->btn_cnt) ) {
    (void)fprintf(stderr,"WARNING: status button not a button: %d\n", btn);
    return( -1 );
  }
  return( buttonbox->buttons[btn].selected );
}

/*
 * Subroutine:	ButtonNumber
 * Purpose:	Given panel, a datum and its data array index, identify the
 *		button index
 * Returns:	Identifying index of button in this box, else -1.
 * Called by:	Application program
 * Xlib calls:	none
 * Method:	Check given datum for each button, return index of first one
 *		that matches;
 */
int ButtonNumber ( buttonbox, data_index, datum )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int data_index;		/* i: index position in data field of datum */
     int datum;			/* i: datum to be located */
{
  int btn;			/* i: button identifying index */

  for( btn = 0; btn < buttonbox->btn_cnt; btn++ ) {
    if( buttonbox->buttons[btn].feel->data[data_index] == datum )
      return( btn );
  }
  (void)fprintf(stderr, "WARNING: button not found - index: %d, datum %d\n",
		data_index, datum);
  return (-1);
}
                                   
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                   
