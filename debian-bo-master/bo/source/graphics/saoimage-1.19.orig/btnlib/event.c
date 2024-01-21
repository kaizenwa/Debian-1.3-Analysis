#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	event.c (Event Control)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Field buttonbox events
 * Subroutines:	ButtonEvent()			returns: int
 * Subroutines:	static btn_Event()		returns: int
 * Subroutines:	ButtonControl()			returns: int
 * Subroutines:	static btn_Control()		returns: int
 * Subroutine:	ButtonSelectMask		returns: int
 * Xlib calls:	XNextEvent(), XPutBackEvent(), XPending()
 * Xlib calls:	ConnectionNumber (macro)
 * UNIX calls:	Select()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		21 March 1989
 *		{1} Jay Travisano (STScI) VMS,IMTOOL changes    10 Nov   1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>	/* define stderr, NULL */
#include <sys/types.h>	/* define stderr, NULL */
#include <X11/Xlib.h>
#include "buttons.h"

#define MOUSESTATE (Button1Mask | Button2Mask | Button3Mask)

/* STATIC VARIABLES FOR USING UNIX SELECT TO DETECT NON-WINDOW EVENTS */
static int select_flag,  x_mask[2], select_mask[2];
static int select_size=0;


/*
 * Subroutine:	ButtonEvent
 * Purpose:	Determine if the given event belongs to the button
 *		box hierarchy
 * Returns:	1 if yes this is a button event, 0 if no
 * Called by:	Application program
 * Uses:	btn_Event() below
 * Xlib calls:	none
 * Post-state:	takes no action
 * Method:	Check the event's window to see if it is one of ours.  First
 *		check this box and the branches of active submenus.
 *		Then check on each of this boxes co-horts and do the same
 *		with their active submenus.
 * Note:	Must be called with a buttonbox at the base of the menu tree
 */
int ButtonEvent ( buttonbox, event )
     ButtonBox buttonbox;	/* i: top handle for button menu group */
     XEvent *event;		/* i: pointer to filled event record */
{
  int i;			/* l: loop counter */
  static int btn_Event();

  /* is the event in the primary buttonbox (or its sub-menus) */
  if( btn_Event(buttonbox, event) != 0 ) {
    return( 1 );
  } else {
    /* is the event in a co-menu of the primary buttonbox  .. */
    /*   .. (or one of their sub-menus) */
    for( i = 0; i < buttonbox->co_menu_count; i++ ) {
      if( btn_Event(buttonbox->co_menu[i], event) != 0 )
	return( 1 );
    }
  }
  /* none or the above */
  return( 0 );
}

/*
 * Subroutine:	btn_Event
 * Purpose:	Determine if the given event belongs to this button box
 *		or a buttonbox in its branches of active submenus.
 * Returns:	1 if yes this is a button event, 0 if no
 * Called by:	ButtonEvent() above, ButtonControl(), btn_Control() below
 * Xlib calls:	none
 * Post-state:	takes no action
 * Method:	Check this buttonbox, then recurse on each active submenu
 *		and co-submenus.
 */
static int btn_Event ( buttonbox, event )
     ButtonBox buttonbox;	/* i: top handle for button menu tree */
     XEvent *event;		/* i: pointer to filled event record */
{
  int i;			/* l: loop counter */

  /* go through our window list to find a match */
  for( i = 0; i < buttonbox->window_count; i++ )
    if( event->xany.window == buttonbox->window_list[i] )
      return( 1 );
  /* no?, do we have any submenus?, if yes, try them by recursion */
  if( buttonbox->submenu_count != 0 )
    for( i = 0; i < buttonbox->submenu_count; i++ )
      if( btn_Event(buttonbox->submenu[i], event) != 0 )
	return( 1 );
  /* no?, do we have any cosubmenus?, if yes, try them by recursion */
  if( buttonbox->cosubmenu_count != 0 )
    for( i = 0; i < buttonbox->cosubmenu_count; i++ )
      if( btn_Event(buttonbox->cosubmenu[i], event) != 0 )
	return( 1 );
  return( 0 );
}

/*
 * Subroutine:	ButtonControl
 * Purpose:	Check the event for belonging to the button menu or one of its
 *		submenus, and handle button respons appropriately.  Return
 *		code with a button selection event, or the first event which
 *		belongs to the application.
 * Returns:	1 if returns with button selection (response points at data),
 *		0 if X event in "event" is not for this button group
 *		-1 if a non-X event is indicated by UNIX select
 * Called by:	Application program
 * Uses:	btn_Event() above, btn_Control() below
 * Xlib calls:	XPutBackEvent()
 * Pre-state:	An event already in the passed event structure
 * Post-state:	Pointer pointed at by response is set to point at the selected
 *		button's data field on a button response event
 * Post-state:	If status == 1, event will contain either a ButtonPressed or
 *		ButtonReleased for one of the buttons.
 *		If status == 0 event will be an event for the application
 *		If status == -1, event will be of no interest to application
 * Method:	Loop until we have an event which does not belong to any
 *		buttonbox (must be for application) or a button selection is
 *		made.  In each loop, use btn_Event for a quick check to
 *		seek ownership of the event.  When claimed, call btn_Control
 *		to actually field the event.  If btn_Control does not call for
 *		returning, btn_Control will have returned with a new event
 *		and the loop can continue.  If nobody claims the current
 *		event, we must return.
 *		Then check on each of this boxes co-horts and do the same
 *		with their active submenus.
 * Note:	Must be called with a buttonbox at the base of the menu tree
 */
int ButtonControl ( buttonbox, event, response )
     ButtonBox buttonbox;	/* i: top handle for button menu group */
     XEvent *event;		/* i: pointer to filled event record */
     int **response;		/* o: ptr to button data ptr set on response */
{
  int status;		/* o: reason for return code (-1,0,1) */
  int not_found;	/* l: event search status */
  int i;		/* l: loop counter */
  static int btn_Event();
  static int btn_Control();

  status = 1;
  *response = NULL;
  while( 1 ) {
    not_found = 1;
    /* is the event in the primary buttonbox (or its sub-menus) */
    if( btn_Event(buttonbox, event) != 0 ) {
      status = btn_Control(buttonbox, event, response);
      not_found = 0;
    } else {
      /* is the event in a co-menu (or one of their sub-menus) */
      for( i = 0; i < buttonbox->co_menu_count; i++ ) {
	if( btn_Event(buttonbox->co_menu[i], event) != 0 ) {
	  status = btn_Control(buttonbox->co_menu[i], event, response);
	  not_found = 0;
	  break;
	}
      }
    }
    /* if current event is nobody's or there is a response, return */
    if( not_found || (status != 0) ) {
      /* if it was fielded but the status is 0, it's a new unclaimed event */
/* if new event is not to be fetched by ButtonControl, do this:
      if( status == 0 )
	XPutBackEvent (event->xany.display, event);
*/
      return( status );
    }
  }
}

/*
 * Subroutine:	btn_Control
 * Purpose:	Field events until one is not of this button box tree, or
 *		a reportable button event occurs.
 * Returns:	1 on button response (response points at data),
 *		0 if X event not claimed,
 *		-1 on non-X event indicated by UNIX select
 * Called by:	ButtonControl() above
 * Uses:	btn_Event() above, btn_Control() recurses
 * Uses:	btn_PutImage(), btn_DrawButton() in DrawButton.c
 * Uses:	btn_PushButton() in PushButton.c
 * Xlib calls:	XNextEvent(), XPending()
 * UNIX calls:	Select()
 * Pre-state:	An event already in the passed event structure
 * Post-state:	Pointer pointed at by response is set to point at the selected
 *		button's data field on a button response event
 * Post-state:	Possibly a different event in the passed event structure:
 *		either a ButtonPressed or ButtonReleased for one of the
 *		buttons, or an event which must be fielded by the application.
 * Method:	Loop until we have an event which does not belong to any
 *		buttonbox (must be for application) or a button selection is
 *		made.  In each loop, check btn_Event to see if it belongs to
 *		this button box, if not, recurse on submenus and cosubmenus
 *		until one fields it.  If still not, return NULL, else (it
 *		was fielded) get the next event and loop again.
 */
static int btn_Control ( buttonbox, event, response )
     ButtonBox buttonbox;	/* i: top handle for button menu tree */
     XEvent *event;		/* i: pointer to filled event record */
     int **response;		/* o: ptr to button data ptr set on response */
{
  int status;		/* o: reason for return code (-1,0,1) */
  int i;		/* l: loop counter */
  int virgin;		/* l: flag that 1st event was never fielded */
  int not_claimed;	/* l: flag that current event was not fielded */
  int type;		/* l: temp for button response type */
  int id;		/* l: temp for button id */
  int mask[2];		/* l: temp store for select mask (select changes it) */
  static int btn_Event();
  int btn_PushButton();
  void btn_PutImage(), btn_DrawButton(), btn_ReleaseButton();

  /* indicate that this is the passed event */
  virgin = 1;
  while( 1 ) {
    not_claimed = 1;
    status = 1;
    /* see if this event belongs to this buttonbox */
    for( i = 0; i <= buttonbox->btn_cnt; i++ ) {
      if( event->xany.window == buttonbox->window_list[i] ) {
	not_claimed = 0;
	break;
      }
    }
    /* if this was an event for an active button, field it */
    if( i < buttonbox->btn_cnt ) {
      switch( event->type ) {
      case EnterNotify:
	/* indicate occupation of this button */
	buttonbox->buttons[i].occupied = 1;
	/* display this button as occupied */
	if( buttonbox->buttons[i].highlight == 0 )
	  btn_PutImage(&buttonbox->buttons[i], OFF_IN);
	else
	  btn_PutImage(&buttonbox->buttons[i], ON_IN);
	break;
      case LeaveNotify:
	/* if button is down, ignore, we will get another after release */
	if( event->xcrossing.state & MOUSESTATE )
	  break;
	/* unindicate occupation of this button */
	buttonbox->buttons[i].occupied = 0;
	/* display this button as unoccupied */
	if( buttonbox->buttons[i].highlight == 0 )
	  btn_PutImage(&buttonbox->buttons[i], OFF_OUT);
	else
	  btn_PutImage(&buttonbox->buttons[i], ON_OUT);
	break;
      case ButtonPress:
	/* if mouse buttons are down, don't respond */
	if( event->xbutton.state & MOUSESTATE )
	  break;
	/* if missed a release (for unknown reason) too late now */
	if( buttonbox->down_mouse_btn >=0 )
	  buttonbox->down_mouse_btn = -1;
	/* call the routine to handle the buttonbox response */
	if( btn_PushButton(buttonbox, i, event->xbutton.button,
			   event->xbutton.state, 1) ) {
	  /* report selection's data to calling routine (if not NO_OP) */
	  *response = buttonbox->buttons[i].feel->data;
	  return( 1 );
	}
	break;
      case ButtonRelease:
	/* we only care about the release of a known button */
	if( event->xbutton.button != buttonbox->down_mouse_btn )
	  break;
	id = buttonbox->down_btn;
	type = buttonbox->buttons[id].feel->function[buttonbox->down_btn_func];
	/* call the routine to handle most of the buttonbox response */
	btn_ReleaseButton(buttonbox, type, id);
	if( type == BTNCoWhile ) {
	  /* for a BTNCoWhile report release (already drawn correctly) */
	  *response = buttonbox->buttons[id].feel->data;
	  return( 1 );
	} else if( (type == BTNWhile) || (type == BTNOneShot) ) {
	  /* for a BTNWhile or BTNOneShot draw button */
	  btn_DrawButton(&buttonbox->buttons[id]);
	  /* for a BTNWhile report release */
	  if( type == BTNWhile ) {
	    *response = buttonbox->buttons[id].feel->data;
	    return( 1 );
	  }
	}
	break;
      case Expose:
	/* on an expose window event, redraw the button */
	btn_DrawButton(&buttonbox->buttons[i]);
	break;
      default:
	/* server might send other events, but we are not interested */
	break;
      }
    }
    /* if event is not accounted for and there are submenus, try them */
    if( not_claimed && (buttonbox->submenu_count != 0) ) {
      /* is it one of their events */
      for( i = 0; i < buttonbox->submenu_count; i++ ) {
	/* chance of success small, so do a rapid search first */
	if( btn_Event(buttonbox->submenu[i], event) != 0 ) {
	  /* if yes, pass the torch (recurse through submenu) */
	  if( (status = btn_Control(buttonbox->submenu[i], event, response))
	     != 0 )
	    /* got a selection, pass it on */
	    return( status );
	  not_claimed = 0;
	  break;
	}
      }
    }
    /* if event is not accounted for and there are submenus, try them */
    if( not_claimed && (buttonbox->cosubmenu_count != 0) ) {
      /* is it one of their events */
      for( i = 0; i < buttonbox->cosubmenu_count; i++ ) {
	/* chance of success small, so do a rapid search first */
	if( btn_Event(buttonbox->cosubmenu[i], event) != 0 ) {
	  /* if yes, pass the torch (recurse through cosubmenu) */
	  if( (status = btn_Control(buttonbox->cosubmenu[i], event, response))
	      != 0 )
	    /* got a selection, pass it on */
	    return( status );
	  not_claimed = 0;
	  break;
	}
      }
    }
    /* if this event was not claimed, return status 0 */
    if( not_claimed ) {
      /* (Debug check) if we were called with someone else's event, shame! */
      if( virgin != 0 ) {
	(void)fprintf(stderr,
		      "WARNING: buttonbox called with unexpected event.\n");
	(void)fprintf(stderr, "  event window= %x, event type = %x\n", 
		      event->xany.window, event->type);
      }
      return( 0 );
    }
    /* if no btn_Control call has fetched next event (event fielded here) */
    if( status != 0 ) {
      /* get next event */
      if( select_flag ) {
	/* if events may be non-X UNIX events, don't block on XNextEvent */
	if( XPending(buttonbox->display) ) {
	  /* if an event is already pending, get it as X event */
	  XNextEvent(buttonbox->display, event);
	} else {
#ifndef VMS
	  /* block on UNIX select (returns on X or other events) */
	  /* set mask to screen events */
	  mask[0] = select_mask[0];
	  mask[1] = select_mask[1];

	  if( select(select_size, (fd_set *)mask,
		     (fd_set *)0, (fd_set *)0, (struct timeval *)0) <0 )
	    perror("select error");
	  /* both masks have only one bit, look for match */
	  if( (mask[0] & x_mask[0]) || (mask[1] & x_mask[1]) ) {
	    /* if event was X event, get it and loop */
	    XNextEvent(buttonbox->display, event);
	  } else {
	    /* if event was not X event, return special status code */
	    return( -1 );
	  }
#else
	  return( -1 );
#endif
	}
      } else {
	/* if only X events are expected, block waiting for next then loop */
#if VMS && IMTOOL
	if( ! XPending (buttonbox->display) )
	  return( -1 );
#endif
	XNextEvent(buttonbox->display, event);
      }
    }
    /* note that this is no longer the passed event */
    virgin = 0;
  } /* while( 1 ) loop */
}

#ifndef VMS

/*
 * Subroutine:	ButtonSelectMask
 * Purpose:	Set a UNIX select mask for event detecting by ButtonControl.
 * Returns:	1 if select flag was set, 0 if select flag could not be set
 * Called by:	Application
 * Xlib calls:	ConnectionNumber (macro)
 * Pre-state:	Application has set mask that is passed
 * Post-state:	Static variables accesible to ButtonControl are set
 * Exception:	Display file descriptors are legal under newer UNIX's but not
 *		accomodated here.
 * Notes:	UNIX select blocks until an event (usually a pending message)
 *		matching a select mask bit occurs.  XNextEvent() calls select
 *		with only its display connection message bit.  It does not
 *		return on other events.  By doing our own select and only
 *		calling XNextEvent when we know that it has an event for
 *		select, we avoid this problem.  When a non-X event is detected,
 *		ButtonControl returns control to the application with a -1
 *		status flag.  Select bits are cleared by reading from the
 *		flagging file descriptor or closing it.
 */
int ButtonSelectMask ( display, setmask, size, set )
     Display *display;	/* i: display connection for X events */
     int *setmask;	/* i: array of mask ints set for other events */
     int size;		/* i: number of bits used for select mask */
     int set;		/* i: 1=set mask, else clear mask */
{
  int xchan;		/* l: file descriptor for X display (the bit offset) */

  /* if setting select mask for first time, install X server's mask */
  if( select_size == 0 ) {
    xchan = ConnectionNumber(display);
    if( xchan < 32 ) {
      x_mask[0] = (1 << xchan);
      x_mask[1] = 0;
      select_size = 32;
    } else {
      x_mask[0] = 0;
      x_mask[1] = (1 << (xchan - 32));
      /* set number of bits for file descriptors to that of system */
      /* select_size = getdtablesize();
       * hardwire it since SYSV systems lack a call to know */
      select_size = 64;
    }
    select_mask[0] = x_mask[0];
    select_mask[1] = x_mask[1];
  }
  if( set ) {
    select_mask[0] |= setmask[0];
    if( size > 32 ) {
      select_mask[1] |= setmask[1];
      if( select_size < size )
	select_size = size;
    }
  } else {
    if( (setmask[0] != 0) && (setmask[0] != x_mask[0]) )
      select_mask[0] &= (~(setmask[0]));
    if( (size > 32) && (setmask[1] != 0) && ((setmask[1] != x_mask[1])) )
      select_mask[1] &= (~(setmask[1]));
  }
  if( (select_mask[0] != x_mask[0]) || (select_mask[1] != x_mask[1]) )
    select_flag = 1;
  else
    select_flag = 0;
  return( select_flag );
}

#endif
      
                                                               
                                                               
                                                              

                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                       
