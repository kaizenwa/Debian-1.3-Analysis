/* ########################################################################

			     main_events.c

   File: main_events.c
   Path: /home/fournigault/c/X11/xcoral-2.31/main_events.c
   Description: 
   Created: Fri Jan 27 11:17:16 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:17:17 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/time.h>
#ifdef _AIX
#include <sys/select.h>
#endif

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "xcoral.h"
#include "browser_init.h"
#include "man_box.h"
#include "list_box.h"
#include "warn_box.h"
#include "fs_box.h"
#include "dial_box.h"
#include "ctr_panel.h"
#include "main_events.h"
#include "process.h"
#include "new_window.h"
#include "text_cursor.h"
#include "browser_eve.h"
#include "page.h"
#include "chars_cmds.h"
#include "cb_names.h"
#include "get_file.h"

EdWin	*CreateWindow ();
Window	HandleMenu ();
Window  window_kill = 0;
ST	*automate ();
extern Browser br;
extern WBox w_box;
extern LBox l_box;
extern DBox dial_box;
extern FBox fs_box;
extern MBox man_box;

XEvent event;
static XEvent *ev = &event;

extern void exit ();
extern void ScrollNLine ();

FCT (static void, ButtonPressInControl, (XEvent *ev, Text *text, int i) );
FCT (static void, HandleKeyPress, () );
FCT (static void, HandleKeyRelease, () );
FCT (static void, HandleMap, () );
FCT (static void, HandleMessage, () );
FCT (static void, HandleSelection, () );
FCT (static void, IgnoreEvent, () );
FCT (static void, ButtonUserFunc, (Text *text, int x, int y, char *func_name) );

#define MAX_EVENT 64
void (* events_Handler[MAX_EVENT]) ();
static void IgnoreEvent () {}
static int lock_menus = False;
static Text *lock_buttons = 0;

jmp_buf br_env;

fd_set readfds, save_readfds; 
int width_fd;

/*
**	Function name : InitEvent
**
**	Description : Initialise la table des evenements.
**	Input : 
**	Ouput :
*/
void InitEvent ()
{
    int i;
    for ( i= 0; i < MAX_EVENT; i++ )
      events_Handler[i] = IgnoreEvent;
    
    events_Handler [ButtonPress] = HandleButtonPress;
    events_Handler [ConfigureNotify] = HandleConfigure;
    events_Handler [EnterNotify] = HandleEnter;
    events_Handler [LeaveNotify] = HandleLeave;
    events_Handler [KeyRelease] = HandleKeyRelease;
    events_Handler [KeyPress] = HandleKeyPress;
    events_Handler [Expose] = HandleExpose;
    events_Handler [MappingNotify] = HandleMap;
    events_Handler [ClientMessage] = HandleMessage;
    events_Handler [SelectionRequest] = HandleSelection;
}

/*
**	Function name : WaitForEvent
**
**	Description : Boucle principale de traitements des
**          evenements X et de l'ecoute sur des descripteurs Shell.
**	Input : 
**	Ouput :
*/
void WaitForEvent ()
{
#if defined(SVR4) || defined(UNIXWARE)
	sigset_t block, oblock;
#else
    int old_mask;
#endif
    EdWin **t;
    Text *text = 0;
    
    FD_ZERO (&readfds);
    bcopy ( (char *) &readfds, (char *) &save_readfds, sizeof (fd_set));
    SetFd ( (int) ConnectionNumber(dpy) );
    
    for (;;) {
	while ( XPending ( dpy ) == 0 ) {
	    /* Pour un ^C eventuel */
#if defined(SVR4) || defined(UNIXWARE)
		(void)sigemptyset(&block);
		(void)sigaddset(&block, SIGINT);
		if (sigprocmask(SIG_BLOCK, &block, &oblock) < 0)
		perror("sigprocmask");
#else
	    old_mask = sigblock ( sigmask (SIGINT));
#endif
	    FD_ZERO (&readfds); /* Mise a zero du mask de select */
	    /* Sauvegarde du mask */
	    bcopy ( (char *) &save_readfds, (char *) &readfds, sizeof (fd_set));
	    switch ( select ( width_fd, &readfds, (fd_set *)0,
			     (fd_set *)0,(struct timeval *) 0 )) {
	    case 0: /* Le timeout */
	      break;
	    case -1: /* Brrr... */
	      (void) perror ((char *) 0);
	      exit(1);
	      break;
	    default: /* Ya quelque chose sur un des descripteurs */
	      for ( t = TWin; t < TWin + MAXWIN; t++ ) {
		  if ( *t == 0 ) continue;
		  if ( FD_ISSET ( (*t) -> text -> from_shell, &readfds )) {
		      text = (*t) -> text;
		      break;
		  }
	      }
	      if ( text != 0 ) {
		  ReadFromShell ( text );
		  text = 0;
	      }
	      break;
	    }
#if defined(SVR4) || defined(UNIXWARE)
		(void)sigprocmask(SIG_SETMASK, &oblock, (sigset_t *)NULL);
#else
		(void) sigsetmask(old_mask);
#endif
	}
	/* Sinon les evenements X */
	XNextEvent ( dpy, &event );
	if ( event.type < MAX_EVENT ) {
	    (* events_Handler [event.type] ) ();
	}
    }
}


/*
**	Function name : SetFd
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetFd ( fd )
    int fd;
{
    FD_SET ( fd, &save_readfds );
    if ( (fd +1) > width_fd )
      width_fd = fd +1;
}

/*
**	Function name : ResetFd
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ResetFd ( fd )
    int fd;
{
    FD_CLR (fd, &save_readfds);
}

/*
**	Function name : WaitForMapped
**
**	Description : Attent qu'une fenetre soit reellement mappee.
**	Input : Le window id.
**	Ouput :
*/
void WaitForMapped ( win, istext )
    Window win;
    int istext;
{
    XWindowAttributes att;
    extern int initial_stat_return;
    
    XSync ( dpy, False );
    for (;;) {
	XNextEvent ( dpy, &event );
	if ( event.type < MAX_EVENT ) {
	    if ( (event.type == EnterNotify) || (event.type == LeaveNotify) ) {
		/* ATTENTION changement de contexte, il faut prevenir
		   * l'automate sinon il va rendre un etat faux.
		*/
		if ( istext == True )
		  initial_stat_return = True;
	    }
	    (* events_Handler [event.type] ) ();
	}
	XGetWindowAttributes ( dpy, win, &att );
	if ( att.map_state == IsViewable ) {
	    if ( QLength ( dpy ) == 0 )
	      break;
	}
    }
}

/*
**	Function name : WaitForXserver
**
**	Description :
**	Input :
**	Output :
*/
WaitForXserver()
{
    XSync ( dpy, False );

    while ( XEventsQueued(dpy, QueuedAfterFlush) ) {
	XNextEvent ( dpy, &event );
	if ( event.type < MAX_EVENT ) {
	    (* events_Handler [event.type] ) ();
	}
    }
}


/*
**	Function name : WaitForUnMapped
**
**	Description : Attend qu'une fenetre soit reellement unmappee.
**	Input : Le window id.
**	Ouput :
*/
void WaitForUnMapped ( win )
    Window win;
{
    XWindowAttributes att;
    
    XSync ( dpy, False );
    for (;;) {
	XNextEvent ( dpy, &event );
	if ( event.type < MAX_EVENT ) {
	    (* events_Handler [event.type] ) ();
	}
	XGetWindowAttributes ( dpy, win, &att );
	if ( att.map_state == IsUnmapped ) {
	    if ( QLength ( dpy ) == 0 )
	      break;
	}
    }
}

/*
**	Function name : ButtonPressInControl
**
**	Description : Gere le look des bouttons sur un 'ButtonPress'
**	Input : L'evenement, le text courant, le no du boutton.
**	Ouput :
*/
static void ButtonPressInControl ( ev, text, i )
    XEvent *ev;
    Text *text;
    int i;
{
    extern ButtonWindow bw[];
    
    DownButton ( ev -> xbutton.window );

    XGrabPointer ( dpy, ev -> xbutton.window, True,
		  ButtonPressMask, GrabModeAsync,GrabModeAsync,
		  ev -> xbutton.window, None, CurrentTime );

    ExecButtonFunction ( text, i );
    
    if ( (i==2) || (i==3) ) {
	UpButton ( ev -> xbutton.window );
	XUngrabPointer ( dpy, CurrentTime );
	return;
    }
    XUngrabPointer ( dpy, CurrentTime );
    UpButton ( ev -> xbutton.window );
}

/*
**	Function name : HandleMessage
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void HandleMessage ()
{
    EdWin **t;
    
    if ( strcmp (XGetAtomName(dpy,ev -> xclient.message_type), "WM_PROTOCOLS") != 0 )
      return;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( ev -> xclient.window == (*t) -> w_frame ) {
	    if ( DeleteWindow ( (*t) -> text ) == 0 ) {
		if ( IsLastWindow (0) == True ) {
		    XCloseDisplay ( dpy );
		    (void) exit (0);
		}
	    }
	    break;
	}
    }
    if ( (ev -> xclient.window == GetBrowserFrame ()) 
	|| (ev -> xclient.window == GetBrowserVisit())) {
	UnmapBrowser ();
    }
}

/*
**	Function name : HandleMap
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void HandleMap ()
{
#ifdef DEBUG			
    (void) fprintf ( stderr, "Keyboard Mapping\n" );
#endif
    XRefreshKeyboardMapping ( (XMappingEvent *) ev );
}

/*
**	Function name : HandleSelection
**
**	Description :
**	Input :
**	Output :
*/
static void HandleSelection()
{
#ifdef DEBUG
  (void) fprintf ( stderr, "SelectionRequest\n" );
#endif
}


/*
**	Function name : HandleConfigure
**
**	Description : 
**	Input : 
**	Ouput :
*/
void HandleConfigure ()
{
    EdWin	**t;
    
#ifdef DEBUG
    ( void ) fprintf ( stderr, "Configure %d\n", ev -> xconfigure.window );
#endif
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( ev -> xconfigure.window == ( * t) -> w_frame ) {
	    
	    if ( ((*t) -> width != ev -> xconfigure.width) ||
		((*t) -> height != ev -> xconfigure.height) )
	      
	      ConfigWindow ( *t, ev -> xconfigure.width,
			    ev -> xconfigure.height );
#ifdef DEBUG
	    ( void ) fprintf ( stderr,"width = %d height = %d\n",
			      ev -> xconfigure.width, ev -> xconfigure.height  );
#endif
	    while ( XCheckWindowEvent ( dpy,
				       ev -> xconfigure.window,
				       StructureNotifyMask, ev ));
	    
	    return;
	}
    }
    if ( ev -> xconfigure.window == GetBrowserFrame () ) {
	ConfigBrowser ( ev -> xconfigure.width,
		       ev -> xconfigure.height );
	return;
    }
    if ( ev -> xconfigure.window == GetBrowserVisit () ) {
	ConfigVisitWindow ( ev -> xconfigure.width,
			   ev -> xconfigure.height );
	return;
    }
    if ( ev -> xconfigure.window == GetWarningBoxFrame () ) {
	(void) ConfigWarningBox ( ev -> xconfigure.width,
				 ev -> xconfigure.height );
	return;
    }
    if ( ev -> xconfigure.window == GetListBoxFrame () ) {
	(void)ConfigListBox ( ev -> xconfigure.width,
			     ev -> xconfigure.height );
	return;
    }
    if ( ev -> xconfigure.window == GetDialBoxFrame () ) {
	ConfigDialogBox ( ev -> xconfigure.width,
			 ev -> xconfigure.height );
	return;
    }
    if ( ev -> xconfigure.window == GetFsBoxFrame () ) {
	(void) ConfigFsBox ( ev -> xconfigure.width,
			    ev -> xconfigure.height );
	return;
    }
    if ( ev -> xconfigure.window == GetManBoxFrame () ) {
	(void) ConfigManBox ( ev -> xconfigure.width,
			     ev -> xconfigure.height );
	return;
    }
}


/*
**	Function name : HandleEnter
**
**	Description : 
**	Input : 
**	Ouput :
*/
void HandleEnter ()
{
    EdWin **t;
    int trouve = False;
    
#ifdef DEBUG
    ( void ) fprintf ( stderr, "Enter Notify %d mode = %d\n", 
		      ev -> xcrossing.window,
		      ev -> xcrossing.mode );
#endif
    
    if ( ev -> xcrossing.mode != NotifyNormal
	&&  ev -> xcrossing.mode != NotifyUngrab )
      return;
    
    if ( MouseInMenuBar ( dpy, ev -> xcrossing.window ) == True )
      return;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue; 
	if ( (*t) -> w_frame != ev -> xcrossing.window ) continue;
#ifdef DEBUG
	( void ) fprintf ( stderr, "Changement de contexte \n" );
#endif
	if ( XFindContext ( dpy, ev -> xcrossing.window,
			   EdContext, (caddr_t *) &edwin ) == XCNOENT ) {
	    (void) fprintf ( stderr,"Context Error\n" );
	    (void) exit(1);
	}
	MouseIn ( edwin -> text );
	UnFreezeTextCursor ( edwin -> text );
	TextCursorOn ( edwin -> text );
	ChangeDir ( edwin -> text );
	trouve = True;
    }
    if ( trouve == True )
      SetButton ( edwin -> mwin );
}


/*
**	Function name : HandleLeave
**
**	Description : 
**	Input : 
**	Ouput :
*/
void HandleLeave ()
{
    EdWin **t;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "Leave Notify w = %d mode = %d detail = %d\n",
		    ev -> xcrossing.window, ev -> xcrossing.mode,
		    ev -> xcrossing.detail  );
#endif
    
    if ( ev -> xcrossing.mode != NotifyNormal )
      return;
    
    if ( ev -> xcrossing.detail == NotifyInferior )
      return;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( ev -> xcrossing.window == ( * t) -> w_frame ) {
	    MouseOut( ( * t ) -> text );
	    TextCursorOff (( * t ) -> text );
	    FreezeTextCursor (( * t ) -> text );
	    DisplayMessage ( (*t)  -> mwin, " "  );
	    return;
	}
    }
}

/*
**	Function name : HandleKeyRelease
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void HandleKeyRelease ()
{
    EdWin **t;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
#ifdef DEBUG
	( void ) fprintf ( stderr, "Key Release\n" );
#endif
    }			
}


/*
**	Function name : HandleKeyPress
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void HandleKeyPress ()
{
    extern ST *st_initial;
    ST *stat;
    ST **old_stat = &edwin -> stat;
    Window current_win = edwin -> w_frame;
    EdWin **t, *tmp = 0;
    
#ifdef DEBUG
    fprintf ( stderr, "Key Press Event\n");
#endif
    if ( KeyPressInBrowser ( (XKeyEvent *) ev ) == True )
      return;

    if ( KeyPressInManual ( (XKeyEvent *) ev ) == True )
       return;

    /* Pour etre sur que le contexte courant est valide.
       Dans le cas ou on manipule des icones */
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( edwin -> text == (*t) -> text ) {
	  tmp = *t;
	  break;
	}
    }
    if ( tmp == 0 )
      return;
    
    stat = (ST *) automate ( edwin -> text, ( XKeyEvent *) ev, edwin -> stat );
    if ( (int) stat == -2 ) {
	/* Ya eu un changement de contexte */
	*old_stat = (ST *) &st_initial;
    }
    else {
      if ( (int) stat != -1 ) {
	  edwin -> stat = stat;
      }
      else {
             /* Destruction d'une fenetre.
	     Normalement l'etat de l'automate n'a plus
	     d'importance, sauf si l'operation a ete
	     pilotee depuis Smac (kill_window d'une autre
	     fenetre). Dans ce cas, il faut rendre l'etat
	     initial.
	     */
	if ( window_kill != current_win ) {
	    edwin -> stat = (ST *) &st_initial;
	}
      }
    }
    
    if ( IsLastWindow ( 0 ) == True ) {
	XCloseDisplay ( dpy );
	(void) exit (0);
    }
}


/*
**	Function name : HandleButtonPress
**
**	Description : 
**	Input : 
**	Ouput :
*/
void HandleButtonPress ()
{
    int i;
    int vm, item;
    Window w_stat;
    int result;
    
#ifdef DEBUG
    ( void ) fprintf ( stderr, "ButtonPress %d\n", ev -> xbutton.window );
#endif
    
    if ( ButtonBrowser ( (XButtonEvent *) ev ) == True )
      return;
    
    if ( ButtonWarningBox ( (XButtonEvent *) ev ) == True )
      return;
    
    if ( ButtonDialogBox ( (XButtonEvent *) ev ) == True )
      return;
    
    if ( ButtonFsBox ( (XButtonEvent *) ev ) == True )
      return;
    
    if ( ButtonManBox ( (XButtonEvent *) ev ) == True )
      return;
    
    if ( lock_buttons == edwin -> text ) {
	klaxon ();
	return;
    }
    /*
       * Button press in menus
    */
    if ( (i = ButtonPressInMenu ( ev -> xbutton.window, edwin -> menu )) != -1 ) {
	if ( lock_menus == True ) {
	    klaxon ();
	    return;
	}
	w_stat = HandleMenu ( dpy, (XButtonPressedEvent *) ev, edwin->w_frame, edwin->menu, i, &vm, &item );
	ExecMenuFunc ( vm, item );
	if ( w_stat != 0 ) {
	    XFindContext ( dpy, w_stat, EdContext,(caddr_t *) &edwin );
	}
	return;
    }
    /*
       * Button press in button window,
    */
    if ( (i = IsButtonInControl ( ev -> xbutton.window )) >= 0 ) {
	if ( GetCursorStat ( edwin -> text ) != OFF )
	  TextCursorOff ( edwin -> text );
	ButtonPressInControl ( ev, edwin -> text, i );
	if ( GetCursorStat ( edwin -> text ) != ON )
	  TextCursorOn ( edwin -> text );
	return;
    }
    
    /* 
       * Button press in scroll cursor,
    */
    if ( ButtonPressInScroll ( edwin -> scroll, ev -> xbutton.window, 
			      ev -> xbutton.y, &result ) == True) {
	switch ( result ) {
	case CURSOR:
	  if ( GetCursorStat ( edwin -> text ) != OFF )
	    TextCursorOff ( edwin -> text );
	  if ( GetScrollStat( edwin -> text ))
	    GotoLeft ( edwin -> text );
	  if ( (TextInBuf ( edwin -> text ) == True) 
	      && ( GetNbLinesInBuf ( edwin -> text ) > 1))			
	    HandleScrollBar ( dpy, edwin -> scroll, ScrollNLine );
	  if ( GetCursorStat ( edwin -> text ) != ON )
	    TextCursorOn ( edwin -> text );
	  RefreshScrollBar ( dpy, edwin -> scroll ); 
	  break;
	case NEXT:
	  TextCursorOff ( edwin -> text );
	  NextPage ( edwin -> text  );
	  TextCursorOn ( edwin -> text );
	  break;
	case PREVIOUS:
	  TextCursorOff ( edwin -> text );
	  PreviousPage ( edwin -> text );
	  TextCursorOn ( edwin -> text );
	  break;
	}
	return;
    }
    
    /*
     * Button press in current text window
     */
    if ( ev -> xbutton.window == edwin -> text -> window ) {
	switch ( ev -> xbutton.button ) {
	case Button1:
	  switch ( ev -> xbutton.state ) {
	  case ControlMask:
	    (void) ButtonUserFunc ( edwin -> text,
		ev -> xbutton.x, ev -> xbutton.y, "left_button_control" );
	    break;
	  case ShiftMask:
	    (void) ButtonUserFunc ( edwin -> text,
		ev -> xbutton.x, ev -> xbutton.y, "left_button_shift" );
	    break;
	  default:
	    TextCursorOff ( edwin -> text );
/*	    
	    (void) MoveToXYinTextWindow ( edwin -> text,
		       ev -> xbutton.x, ev -> xbutton.y );
*/
	    StorePosition ( edwin -> text );
	    if ( MoveToXYinTextWindow ( edwin -> text,
				       ev -> xbutton.x, ev -> xbutton.y ) == -1 ) {
	      HoleToRight ( edwin -> text -> buf );
	      GotoLineNumber ( edwin -> text, edwin -> text -> lines_in_buf );
	      SetPosition ( edwin -> text );
	      UpdatePage ( edwin -> text );
	    }
	    TextCursorOn ( edwin -> text );
	    break;
	  }
	  break;
	case Button2:
	  switch ( ev -> xbutton.state ) {
	  case ControlMask:
	    DeleteBytesFromCutBuffer ( edwin -> text );
	    break;
	  case ShiftMask:
	    (void) ButtonUserFunc ( edwin -> text,
		ev -> xbutton.x, ev -> xbutton.y, "middle_button_shift" );
	    break;
	  default:
	      if ( strcmp( edwin ->text -> current_mode -> name, "Shell") == 0)
		SetPosInShell (edwin ->text);
	      GetBytesFromCutBuffer ( edwin -> text );
	      break;
	  }
	  break;
	case Button3:
	  switch ( ev -> xbutton.state ) {
	  case ControlMask:
	    (void) ButtonUserFunc ( edwin -> text,
		ev -> xbutton.x, ev -> xbutton.y, "right_button_control" );
	    break;
	  case ShiftMask:
	    (void) ButtonUserFunc ( edwin -> text,
		ev -> xbutton.x, ev -> xbutton.y, "right_button_shift" );
	    break;
	  default:
	    TextCursorOff ( edwin -> text );
	    XSync ( dpy, False );
	    StoreBytesInCutBuffer ( edwin -> text,
				 ev -> xbutton.x, ev -> xbutton.y );
	    TextCursorOn ( edwin -> text );
	    break;
	  }
	  break;
	}
    }
}

/*
**	Function name : ButtonUserFunc
**
**	Description :
**	Input :
**	Output :
*/
static void ButtonUserFunc ( text, x, y, func_name )
    Text *text;
    int x, y;
    char *func_name;
{
    int old_x = text -> x_pos;
    int old_y = text -> y_pos;
    int position;
    char *msg;
    int nw = text->win_id;

    (void) MoveToXYinTextWindow ( text, x, y );
    TextCursorOn ( text );
    position = ie_current_position ( text );
    TextCursorOff ( text );
    (void) MoveToXYinTextWindow ( text, old_x, old_y );
    TextCursorOn ( text );
    StorePosition ( text );
    
    msg = (char *) ie_call_function ( text, func_name, 1, &position ) ;
    text = update_cwd(nw);
    if ( msg != 0 )
      DisplayMessage ( text -> mwin, msg );
/*    SetPosition ( text ); */
    ie_redisplay ( text );
}

/*
**	Function name : HandleExpose
**
**	Description : 
**	Input : 
**	Ouput :
*/
void HandleExpose ()
{
    EdWin **t;
    
#ifdef DEBUG
    ( void ) fprintf ( stderr, "Expose %d\n", ev -> xexpose.window );
#endif
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( ev -> xexpose.window == ( *t ) -> w_frame 
	    || ev -> xexpose.window == GetMenuWindowBar ( (*t) -> menu )  ) {
#ifdef DEBUG
	    ( void ) fprintf ( stderr, "Expose Refreshbar\n" );
#endif
	    RefreshMenuBar ( dpy, ( *t ) -> menu );
	    
	    while ( XCheckWindowEvent ( dpy,
				       ev -> xexpose.window,
				       ExposureMask, ev ));
	    return;
	}
	
	if ( ev -> xexpose.window == GetTextWindow ( ( *t ) -> text )) {
	    
#ifdef DEBUG
	    ( void ) fprintf ( stderr, "Expose text window\n" );
#endif
	    ExposeTextWindow ( dpy, ( *t ) -> text, ev );
	    return;
	}
	
	if ( ExposeInControlePanel ( ev -> xexpose.window, ( *t ) -> mwin ) == True ) {
#ifdef DEBUG
	    ( void ) fprintf ( stderr, "Expose control panel\n" );
#endif
	    return;
	}
	
	if ( ExposeInScroll ( dpy, ev -> xexpose.window, ( *t ) -> scroll ) == True ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Expose sroll\n");
#endif
	    return;
	}
    }
    if ( ExposeBrowser ( ev ) == True )
      return;
    if ( ExposeWarningBox ( ev ) == True )
      return;
    if ( ExposeListBox ( ev ) == True )
      return;
    if ( ExposeDialogBox ( ev ) == True )
      return;
    if ( ExposeFsBox ( ev ) == True )
      return;
    if ( ExposeManBox ( ev ) == True )
      return;
}

/*
**	Function name : FreezeMenus
**
**	Description : 
**	Input : 
**	Ouput :
*/
void FreezeMenus ()
{
    lock_menus = True;
}

/*
**	Function name : IsFreezeMenus
**
**	Description :
**	Input :
**	Output :
*/
int IsFreezeMenus()
{
  return (lock_menus);
}


/*
**	Function name : FreezeMenus
**
**	Description : 
**	Input : 
**	Ouput :
*/
void UnFreezeMenus ()
{
    lock_menus = False;;
}

/*
**	Function name : FreezeButtons
**
**	Description : 
**	Input : 
**	Ouput :
*/
void FreezeButtons ( text )
    Text *text;
{
    lock_buttons = text;
}


/*
**	Function name : UnfreezeButtons
**
**	Description : 
**	Input : 
**	Ouput :
*/
void UnFreezeButtons ()
{
    lock_buttons = 0;
}
