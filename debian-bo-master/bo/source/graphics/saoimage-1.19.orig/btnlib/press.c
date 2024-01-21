#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	press.c (Push Button)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Handle bookkeeping and visuals for pressing a button
 * Subroutines:	btn_PushButton()			returns: int
 * Subroutines:	btn_ReleaseButton()			returns: void
 * Xlib calls:	XSync()
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
 * Subroutine:	btn_PushButton
 * Purpose:	Handle buttonbox behavior in response to a mouse button being
 *		pressed while the mouse cursor is in a buttonbox button
 *		(or as if that was the case)
 * Returns:	1 if able to find button and button function, else 0
 * Called by:	btn_Control() in EventCtrl.c
 * Called by:	TouchButton() in RemoteCtrl.c
 * Uses:	btn_PutImage(), btn_DrawButton() in DrawButton.c
 * Uses:	btn_DelightButtons() in DrawButton.c
 * Uses:	btn_ReplaceSubmenus(), btn_ReplaceCosubmenus() in MountBox.c
 * Xlib calls:	XSync()
 * Post-state:	Records updated, submenus replaced, buttons highlighted
 * Method:	
 * Note:	detail=(event.xbutton.button<<16)|(event.xbutton.state&0xffff)
 * Note:	event filtering to select a response is detail&mask==reference
 */
int btn_PushButton ( buttonbox, btn, mouse_btn, state, mapping )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int btn;			/* i: button identifying index */
     int mouse_btn;		/* i: event.xbutton.button */
     int state;			/* i: event.xbutton.state */
     int mapping;		/* i: indicate whether to map submenus */
{
  ButtonRecord *button;		/* l: pointer to record of pressed button */
  ButtonFeel *feel;		/* l: pointer to feel of pressed button */
  int detail;			/* l: composite of mouse_btn and state */
  int fnum;			/* l: index of accepting function mask/ref */
  int type;			/* l: response type of button/function */
  void btn_PutImage(), btn_DrawButton(), btn_DelightButtons();
  void btn_ReplaceSubmenus(), btn_ReplaceCosubmenus();

  /* detail = (event.xbutton.button << 16) | (event.xbutton.state & 0xffff); */
  detail = (mouse_btn << 16) | (state & 0xffff);
  button = &buttonbox->buttons[btn];
  feel = button->feel;
  /* identify which button option it is */
  for( fnum = 0; fnum < feel->nfunctions; ++fnum ) {
    if( (detail & feel->mask[fnum]) == feel->reference[fnum] )
      break;
  }
  /* if no option matches, ignore and return */
  if( fnum >= feel->nfunctions )
    return( 0 );
  /* flag this as our mouse button */
  buttonbox->down_btn = btn;
  buttonbox->down_btn_func = fnum;
  buttonbox->down_mouse_btn = mouse_btn;
  /* what kind of button was it */
  type = feel->function[fnum];
  /* if selection is a BTNNoOp don't respond */
  if( type == BTNNoOp )
    return( 0 );
  /* highlight this button (may be redone by toggle) */
  button->highlight = 1;
  switch( type ) {
  case BTNMode:
  /* if selection affects the prior mode status */
    /* unhighlight previous selection if there is one */
    if( buttonbox->mode_btn >= 0 ) {
      buttonbox->buttons[buttonbox->mode_btn].selected = 0;
      buttonbox->buttons[buttonbox->mode_btn].highlight = 0;
      btn_PutImage (&buttonbox->buttons[buttonbox->mode_btn], OFF_OUT);
      /* if this is the mode's button, blink but be highlighted */
      if( buttonbox->mode_btn == btn ) {
	button->highlight = 1;
	if( buttonbox->mode_btn_func == fnum ) {
	  /* if this is the mode, don't change anything */
	  button->selected = 1;
	  if( button->occupied == 1 )
	    btn_PutImage(button, ON_IN);
	  else
	    btn_PutImage(button, ON_OUT);
	  XSync(buttonbox->display, 0);
	  /* no change in mode occurs, so return */
	  return( 1 );
	}
      }
    }
    /* indicate this selection */
    button->selected = 1;
    buttonbox->mode_btn = btn;
    buttonbox->mode_btn_func = fnum;
    break;
  case BTNCoMode:
  /* if selection affects the prior CoMode status */
    /* unhighlight previous selection if there is one */
    if( buttonbox->co_mode_btn >= 0 ) {
      buttonbox->buttons[buttonbox->co_mode_btn].selected = 0;
      buttonbox->buttons[buttonbox->co_mode_btn].highlight = 0;
      btn_PutImage(&buttonbox->buttons[buttonbox->co_mode_btn], OFF_OUT);
      /* if this is the CoMode's button, blink but be highlighted */
      if( buttonbox->co_mode_btn == btn ) {
	button->highlight = 1;
	if( buttonbox->co_mode_func == fnum ) {
	  /* if this is the CoMode, don't change anything */
	  button->selected = 1;
	  if( button->occupied == 1 )
	    btn_PutImage(button, ON_IN);
	  else
	    btn_PutImage(button, ON_OUT);
	  XSync(buttonbox->display, 0);
	  return( 1 );
	}
      }
    } else {
      int co_btn;		/* l: current co-mode button */
      int i;			/* l: loop counter */

      for( i = 0; i < buttonbox->co_menu_count; i++ ) {
	/* look for the current CoMode button in other co_menus */
	if( (co_btn = buttonbox->co_menu[i]->co_mode_btn) >= 0 ) {
	  /* if this was the previous choice, unset it */
	  buttonbox->co_menu[i]->buttons[co_btn].selected = 0;
	  buttonbox->co_menu[i]->buttons[co_btn].highlight = 0;
	  btn_PutImage(&buttonbox->co_menu[i]->buttons[co_btn], OFF_OUT);
	  buttonbox->co_menu[i]->co_mode_btn = -1;
	  buttonbox->co_menu[i]->co_mode_func = 0;
	  /* look no further */
	  break;
	}
      }
    }
    /* indicate this selection */
    button->selected = 1;
    buttonbox->co_mode_btn = btn;
    buttonbox->co_mode_func = fnum;
    break;
  case BTNCoWhile:
  /* if selection is a temporary and exclusive CoWhile */
    /* dim all other highlighted buttons (until mouse button released) */
    btn_DelightButtons(buttonbox);
    break;
  case BTNToggle:
  /* toggle a Toggle button */
    if( button->selected == 1 ) {
      /* if highlighted, unhighlight */
      button->selected = 0;
      button->highlight = 0;
    } else {
      /* else highlight this button */
      button->selected = 1;
      button->highlight = 1;
    }
    break;
  /* for BTNFlash, make button appear briefly in opposite highlighting */
  case BTNFlash:
    /* unhighlight (or higlight) this button, mindful of status */
    if (button->selected == 1)
      button->highlight = 0;
    break;
  case BTNOneShot:
  case BTNWhile:
  /* nothing more than highlighting for OneShot or While */
  default:
    break;
  }
  /* draw highlighted (or unhighlighted) button */
  btn_DrawButton(button);
  /* redraw buttons before any other processing */
  XSync(buttonbox->display, 0);
  if( type == BTNFlash ) {
    /* unhighlight (or higlight) this button, mindful of status */
    if( button->selected == 0 )
      button->highlight = 0;
    else
      button->highlight = 1;
    /* draw highlighted (or unhighlighted) button */
    btn_DrawButton(button);
    XSync(buttonbox->display, 0);
  } else if( type == BTNMode ) {
    /* install new submenu environment */
    btn_ReplaceSubmenus(buttonbox, btn, fnum, mapping);
  } else if( type == BTNCoMode ) {
    /* install new cosubmenu environment */
    btn_ReplaceCosubmenus(buttonbox, btn, fnum, mapping);
  } else if( (type == BTNToggle) && (button->submenu_count[fnum] > 0) ) {
    if( button->selected ) {
      btn_ReplaceSubmenus(buttonbox, btn, fnum, mapping);
    } else {
      detail = button->submenu_count[fnum];
      button->submenu_count[fnum] = 0;
      btn_ReplaceSubmenus(buttonbox, btn, fnum, mapping);
      button->submenu_count[fnum] = detail;
    }
  }
  return( 1 );
}

/*
 * Subroutine:	btn_ReleaseButton
 * Purpose:	Handle most of the buttonbox behavior in response to a mouse
 *		button being released after being pressed
 * Returns:	void
 * Called by:	btn_Control() in EventCtrl.c
 * Called by:	TouchButton() in RemoteCtrl.c
 * Uses:	btn_PutImage(), btn_RelightButtons() in DrawButton.c
 * Xlib calls:	XSync()
 * Post-state:	Records updated, buttons rehighlighted
 * Method:	
 * Note:	Caller must redraw the released button
 */
void btn_ReleaseButton ( buttonbox, type, btn )
     ButtonBox buttonbox;	/* i: box which contains this button */
     int type;
     int btn;			/* i: button identifying index */
{
  ButtonRecord *button;		/* l: pointer to record of pressed button */
  void btn_PutImage(), btn_RelightButtons();

  /* indicate that our mouse button is no longer down */
  buttonbox->down_btn = -1;
  buttonbox->down_btn_func = -1;
  buttonbox->down_mouse_btn = -1;
  switch( type ) {
  /* don't take any special action for mode, toggle, or NoOp */
  case BTNNoOp:
  case BTNFlash:
  case BTNToggle:
  case BTNMode:
  case BTNCoMode:
    return;
  /* for a BTNCoWhile, rehighlight previous mode and report */
  case BTNCoWhile:
    /* unhighlight this button if it was not previously selected */
    button = &buttonbox->buttons[btn];
    if( button->selected == 0 ) {
      button->highlight = 0;
      if( button->occupied == 1 )
	btn_PutImage(button, OFF_IN);
      else
	btn_PutImage(button, OFF_OUT);
    }
    /* highlight buttons that were earlier dimmed */
    btn_RelightButtons (buttonbox);
    /* draw everything correctly before returning */
    XSync(buttonbox->display, 0);
    break;
  case BTNOneShot:
  case BTNWhile:
  default:
    /* unhighlight this button, mindful of status */
    if( buttonbox->buttons[btn].selected == 0 )
      buttonbox->buttons[btn].highlight = 0;
  }
}
