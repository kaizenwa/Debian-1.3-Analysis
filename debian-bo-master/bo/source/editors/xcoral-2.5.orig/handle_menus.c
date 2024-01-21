/* ########################################################################

			     handle_menus.c

   File: handle_menus.c
   Path: /home/fournigault/c/X11/xcoral-2.31/handle_menus.c
   Description: 
   Created: Fri Jan 27 11:06:08 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:06:09 MET 1995
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
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "init_menus.h"
#include "handle_menus.h"

extern XContext	SwContext;
static int button_press;
extern int menus_color;

FCT (static Window, HandleButtonReleaseMenu, (Display *display, XButtonReleasedEvent *ev, XYMenu	*menu, Window frame, int *vm, int *it) );
FCT (static void, HandleEnterMenu, (Display *display, XEnterWindowEvent *ev, XYMenu *menu, SWindow *sw, int *p_in) );
FCT (static void, HandleExposeMenu, (Display *display, XExposeEvent *ev, XYMenu *menu, int *last) );
FCT (static void, HandleLeaveMenu, (Display *display, XLeaveWindowEvent *ev, XYMenu *menu, int *p_in) );
FCT (static void, clear_item, (Display *display, XYMenu *menu, int n) );
FCT (static void, show_item, (Display *display, XYMenu *menu, int i, int n) );
FCT (static void, clear_menu, (Display *display, XYMenu *menu, int n) );
FCT (static void, show_menu, (Display *display, XYMenu *menu, int n) );

extern void Display3D ();

/*
**	Function name : HandleMenu
**
**	Description : Point d'entree dans la gestion des menus apres un ButtonPress
**
**	Input : Le display, structure event, la fenetre parent, le menu courant,
**		le numero du menu, 	les infos pour  la callback.
**	Ouput : La fenetre du ButtonRelease.
*/
Window HandleMenu ( display, ev, frame, menu, n, vm, item )
    Display	*display;
    XButtonPressedEvent *ev;
    Window frame;
    XYMenu	*menu;
    int n;
    int *vm, *item;	/* RETURN */
{
  XEvent 	event;
  SWindow	*sw;
  int pointer_in, last;
  
  button_press = ev -> button;
  
  XGrabPointer ( display, frame, True, ButtonPressMask
		| ButtonReleaseMask, GrabModeAsync,
		GrabModeAsync, frame, None , CurrentTime);
  
  show_menu ( display, menu, n );
  menu -> w_last_item = 0;
  menu -> n_last_item = -1;
  pointer_in = True;
  
  for (;;) {
    XNextEvent ( display, &event );
    switch ( event.type ) {
    case EnterNotify:
      HandleEnterMenu ( display,
		       (XEnterWindowEvent *) &event,
		       menu, (SWindow *) &sw, &pointer_in );
      break;
    case LeaveNotify:
      HandleLeaveMenu ( display,
		       (XLeaveWindowEvent *) &event,
		       menu, &pointer_in );
      break;
      
    case Expose:
      HandleExposeMenu ( display,
			(XExposeEvent *) &event,
			menu, &last );
      break;
      
    case ButtonRelease:
      if ( button_press != event.xbutton.button )
	break;
      XUngrabPointer ( display, CurrentTime );
      if ( HandleButtonReleaseMenu ( display,
				    (XButtonReleasedEvent *) &event,
				    menu, frame, vm, item ) ) {
	return ( event.xany.window );
      }
      else {
	return 0;
      }
    }
  }
}


/*
**	Function name : HandleButtonReleaseMenu
**
**	Description : Traitement de l'evennement ButtonRelease
**
**	Input :  Le display, button event, le menu, la fenetre parent
**		les infos sur la callback.
**	Ouput : La fenetre de l'evennement,
*/
static Window HandleButtonReleaseMenu ( display, ev, menu, frame, vm, it )
    Display	*display;
    XButtonReleasedEvent *ev;
    XYMenu	*menu;
    Window 	frame;
    int *vm, *it; 	/* RETURN */
{
  int last = menu -> n_last_mapped;
  int item =  menu -> n_last_item;
#ifdef DEBUG
  ( void ) fprintf ( stderr, "HandleButtonRelease w = %d\n",
		    ev -> window );
#endif
  if ( DoesSaveUnders ( DefaultScreenOfDisplay ( display )) != True )
    menu -> save_ok = 0; /* Pour reinitialiser */
  if ( menu -> mapped != 0 ) {
    clear_menu ( display, menu, last );
    XSync ( display, True );
    if ( DoesSaveUnders ( DefaultScreenOfDisplay ( display )) != True )
      XCopyArea ( display, menu -> save, menu -> w_under,
		 menu -> Ugc, 0, 0, menu -> pix_width,
#ifdef SHADOW
		 menu -> hmax_menu + SHAD + 10,
#else
		 menu -> hmax_menu + 10,
#endif
		 0, 0 );
    if ( item != -1 ) {
      *vm = last;
      *it = item;
    }
    else {
      *vm = *it = -1;
    }
  }
  if ( ev -> window != frame
      && ev -> window != menu -> mapped 
      && ev -> window != menu -> w_title [last] ) {
#ifdef DEBUG
    ( void ) fprintf ( stderr, "\tNot Parent\n" );
#endif
    return ( ev -> window );
  }
  else
    return 0;
}

/*
**	Function name : HandleExposeMenu
**
**	Description : Traite les evenements d'exposition pendant
**		le ButtonPress et gere l'affichage des menus.	
**
**	Input : Le display, l'expose event, le menu, dernier menu expose
**	Ouput :
*/
static void HandleExposeMenu ( display, ev, menu, last )
    Display	*display;
    XExposeEvent	*ev;
    XYMenu	*menu;
    int	*last;
{
  char **str;
  int i;
  XRectangle rec [2];
#ifdef DEBUG
  ( void ) fprintf ( stderr, "HandleExpose w = %d\n",
		    ev -> window );
#endif
  *last = menu -> n_last_mapped;
  str = menu -> vmenu [*last] -> iname;
  
  for ( i=0; i < menu->vmenu [*last]->nb_items; i++ ) {
    if ( ev -> window == menu -> vmenu [*last] -> w_item [i] ) {
      XDrawString ( display,
		   menu -> vmenu [*last] -> w_item [i],
		   menu->Ngc, 10,
		   menu -> y_title + menu -> width_relief,
		   *str,strlen ( *str ));
      
      Display3D ( display, 
		 menu -> vmenu [*last] -> w_item [i],
		 menu -> top_sh, menu -> bot_sh, 
		 menu -> width_relief,
		 DEPTH_UP );
      return;
    }
    str++;
  }
  if ( DoesSaveUnders ( DefaultScreenOfDisplay ( display )) == True )
    return;
  if ( ev -> window == menu -> w_under ) {
    rec [0].x = ev -> x;
    rec [0].y = ev -> y;
    rec [0].width = ev -> width;
    rec [0].height = ev -> height;
    XSetClipRectangles ( display, menu -> Ugc, 0,0,
			rec, 1, Unsorted );
    XCopyArea ( display, menu -> save, menu -> w_under,
	       menu -> Ugc, 0, 0, menu -> pix_width,
#ifdef SHADOW
	       menu -> hmax_menu + SHAD + 10,
#else
	       menu -> hmax_menu,
#endif
	       0, 0 );
    XSetClipMask ( display, menu -> Ugc, None );
  }
}


/*
**	Function name : HandleLeaveMenu
**
**	Description : Traite les evenements LeaveNotify pendant
**		le ButtonPress et gere l'affichage des menus.
**
**	Input : Le Display, l'evenement, le menu et nn flag qui indique
**		si on est ou non dans la fenetre courante.
**	Ouput :
*/
static void HandleLeaveMenu ( display, ev, menu, p_in )
    Display	*display;
    XLeaveWindowEvent *ev;
    XYMenu	*menu;
    int *p_in;
{
  XWindowAttributes att;
#ifdef DEBUG
  (void) fprintf ( stderr, "HandleLeave w = %d\n", ev -> window );
#endif
  if ( ev -> mode == NotifyGrab ) return;
  if ( (ev -> window == menu -> mapped) || 
      (ev -> window == menu -> vmenu [ menu -> n_last_mapped ] -> v_frame) ) {
    clear_item ( display, menu,
		menu -> n_last_mapped );
  }
  if ( ev -> window == menu -> parent ) {
#ifdef DEBUG
    ( void ) fprintf ( stderr, "\t Leave environnement... pas sur.\n" );
    ( void ) fprintf ( stderr, "x = %d y = %d", ev -> x, ev -> y );
#endif
    XGetWindowAttributes ( display, menu -> parent, &att );
#ifdef DEBUG
    ( void ) fprintf ( stderr, "width = %d height = %d",  att.width, att.height );
#endif
    if ( ev -> x < 0 || ev -> y < 0
	|| ev -> x > att.width || ev -> y > att.height )
      *p_in = False;
  }
}


/*
**	Function name : HandleEnterMenu
**
**	Description : Traite les evenements EnterNotify pendant
**		le ButtonPress et gere l'affichage des menus.
**
**	Input : Le display, l'evenement, le menu courant, 
**		le contexte courant ( quel menu et quel item sont mappes ),
**		un flag qui indique si on est ou non dans la fenetre courante.
**	Ouput :
*/
static void HandleEnterMenu ( display, ev, menu, sw, p_in )
    Display *display;
    XEnterWindowEvent  *ev;
    XYMenu	*menu;
    SWindow	*sw;
    int *p_in;
{
  int i;
#ifdef DEBUG
  (void) fprintf ( stderr, "HandleEnter w = %d\n", ev -> window );
#endif
  /*
     * On verifie que le pointer est sur les menus courants.
  */
  *p_in = False;
  for ( i=0;i<MAX_MENUS;i++ ) {
    if ( ev -> window == menu -> w_title [i] ) {
      *p_in = True;
      break;
    }
  }
  for ( i=0;i<MAX_ITEMS;i++ ) {
    if ( *p_in == True ) break;
    if ( ev -> window ==
	menu -> vmenu [ menu -> n_last_mapped ] -> w_item [i] ) {
      *p_in = True;
      break;
    }
  }	       
  if ( ev -> mode == NotifyGrab || *p_in == False )
    return;
  if ( XFindContext ( display, ev -> window, SwContext, (caddr_t *) &sw ) != 0 ) {
#ifdef DEBUG
    ( void ) fprintf ( stderr, "HandleEnter XFind default\n" );
#endif
  }
  switch ( sw -> type ) {
  case MENU :
    if ( menu -> n_last_mapped != sw -> no_m ) 
      show_menu ( display, menu, sw -> no_m );
    break;
  case ITEM :
    show_item ( display, menu, sw -> no_i, sw -> no_m );
    break;
  default:
#ifdef DEBUG
    (void ) fprintf ( stderr,"HandleEnter CaseDefault\n" );
#endif
    break;
  }
  return;
}


/*
**	Function name : clear_item
**
**	Description : Efface l'item allume du menu n.
**
**	Input : Le display, le menu courant et son numero.
**	Ouput :
*/
static void clear_item ( display, menu, n )
    Display	*display;
    XYMenu	*menu;
    int n;
{
  if  ( menu -> w_last_item == 0 ) return;
  if ( menus_color == False ) 
    XFillRectangle ( display,
		    menu -> w_last_item,
		    menu -> Igc,
		    0 + menu -> width_relief,
		    0 + menu -> width_relief,
		    menu -> vmenu [n] -> width - ( 2 * menu -> width_relief ) - 6,
		    menu -> h_item - ( 2 * menu -> width_relief ));
  else
    Display3D ( display, menu -> w_last_item,
	       menu -> top_sh, menu -> bot_sh,
	       menu -> width_relief,
	       DEPTH_UP );
  
  menu -> w_last_item = 0;
  menu -> n_last_item = -1;
}


/*
**	Function name : show_item
**
**	Description : Affiche l' item i du menu n.
**
**	Input : Le display, le menu courant, l'item et le no du menu.
**	Ouput :
*/
static void show_item ( display, menu, i, n )
    Display	*display;
    XYMenu	*menu;
    int i, n;
{
  if ( menus_color == False ) 
    XFillRectangle ( display,
		    menu -> vmenu [n] -> w_item [i],
		    menu -> Igc,
		    0 + menu -> width_relief,
		    0 + menu -> width_relief,
		    menu -> vmenu [n] -> width - ( 2 * menu -> width_relief ) - 6,
		    menu -> h_item - ( 2 * menu -> width_relief )) ;
  else
    Display3D ( display, menu -> vmenu [n] -> w_item [i],
	       menu -> top_sh, menu -> bot_sh,
	       menu -> width_relief,
	       DEPTH_DOWN );
  
  if ( menu -> w_last_item != 0 )
  if ( menus_color == False ) 
      XFillRectangle ( display,
		      menu -> w_last_item,
		      menu -> Igc,
		      0 + menu -> width_relief,
		      0 + menu -> width_relief,
		      menu -> vmenu [n] -> width - ( 2 * menu -> width_relief ) - 6,
		      menu -> h_item - ( 2 * menu -> width_relief ));
  else
    Display3D ( display, menu -> w_last_item,
	       menu -> top_sh, menu -> bot_sh,
	       menu -> width_relief,
	       DEPTH_UP );
  
  menu -> w_last_item = menu -> vmenu [n] -> w_item [i];
  menu -> n_last_item = i;
}


/*
**	Function name : clear_menu
**
**	Description : Efface le menu n.
**
**	Input : Le display, le menu courant, son numero.
**	Ouput :
*/
static void clear_menu ( display, menu, n )
    Display	*display;
    XYMenu	*menu;
    int n;
{
  if ( menus_color == False ) 
    XFillRectangle ( display, menu->w_title [n], menu->Igc,
		    0 + menu -> width_relief,
		    0 + menu -> width_relief,
		    menu->title_width [n] - ( 2 * menu -> width_relief ),
		    menu -> h_item - ( 2 * menu -> width_relief ));
#ifdef SHADOW
  XUnmapWindow  ( display, menu->vmenu [n] -> trans );
#else
  XUnmapWindow  ( display, menu->vmenu [n]->v_frame );
#endif
  menu -> mapped = 0;
  menu -> w_last_item = 0;
  menu -> n_last_item = -1;
  menu -> n_last_unmapped = n;
  XFlush ( display );
}

/*
**	Function name : show_menu
**
**	Description : Affiche le menu n.
**
**	Input : Le Dispaly, le contexte de menu et le numero du menu.
**	Ouput :
*/
static void show_menu ( display, menu, n )
    Display	*display;
    XYMenu	*menu;
    int n;
{
  int last = menu -> n_last_mapped;
  
  if ( menu -> mapped != 0 ) {
    clear_menu ( display, menu, last );
    menu -> w_last_item = 0;
    menu -> n_last_item = -1;
  }
#ifdef DEBUG
  (void) fprintf ( stderr, "Show_menu : Menu : %d, width = %d\n",
		  n, menu -> title_width [n] );
#endif
  if ( menus_color == False )
    XFillRectangle ( display, menu -> w_title [n],
		    menu -> Igc,
		    0 + menu -> width_relief,
		    0 + menu -> width_relief,
		    menu -> title_width [n] - ( 2 * menu -> width_relief ),
		    menu -> h_item - ( 2 * menu -> width_relief ));
  
  if ( DoesSaveUnders ( DefaultScreenOfDisplay ( display )) != True ) {
    if ( menu -> save_ok == 0 ) {
      XCopyArea ( display, menu -> w_under,
		 menu -> save, menu -> Ugc,
		 0, 0,menu -> pix_width,
#ifdef SHADOW
		 menu -> hmax_menu + SHAD + 10,
#else
		 menu -> hmax_menu + 10,
#endif
		 0, 0 );
      menu -> save_ok = 1; /* La pixmap est pleine */
    }
  }
#ifdef SHADOW
  XMapRaised ( display, menu->vmenu [n] -> trans );
#else
  XMapRaised ( display, menu->vmenu [n]->v_frame );
#endif
#ifdef SHADOW
  menu->mapped = menu->vmenu [n] -> trans;
#else
  menu->mapped = menu->vmenu [n] -> v_frame;
#endif
  menu -> n_last_mapped = n;
}


