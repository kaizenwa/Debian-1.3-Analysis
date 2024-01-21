/* ########################################################################

			       list_box.c

   File: list_box.c
   Path: /home/fournigault/c/X11/xcoral-2.31/list_box.c
   Description: 
   Created: Fri Jan 27 11:14:23 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:14:24 MET 1995
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
#include <string.h>

#include "xcoral.h"
#include "options.h"
#include "list_box.h"
#include "parse.h"
#include "page.h"
#include "main_events.h"
#include "text_cursor.h"
#include "shadow.h"
#include "kill_buf.h"

extern Display *dpy;
LBox l_box;
static  Atom list_del;

FCT (static void, DisplayList, (char *msg) );
FCT (static void, MapListBox, () );
FCT (static void, UnmapListBox, () );
FCT (static void, UpdateSelect, () );
FCT (static void, UpdateListTitle, () );

/*
**	Function name : InitListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InitListBox ()
{
    XWindowAttributes att;
    Window root;
    int screen;
    unsigned long black, white;
    XGCValues	 gcv;
    XSizeHints sizehints;
    
    black = BlackPixel ( dpy, DefaultScreen ( dpy ));
    white = WhitePixel ( dpy, DefaultScreen ( dpy ));

    l_box.mwin = 0;
    l_box.fg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ? 
      black : GetOpColor ( OP_MENU_FG );
    l_box.bg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ?
      white : GetOpColor ( OP_MENU_BG );
    l_box.ts = GetOpColor ( OP_MENU_TS );
    l_box.bs = GetOpColor ( OP_MENU_BS );
    l_box.font = LoadFont (dpy, LBOX_FONT ); 
    l_box.gc = XCreateGC ( dpy, DefaultRootWindow ( dpy ), 0,  &gcv );
    /*XCopyGC ( dpy, DefaultGC (dpy, DefaultScreen ( dpy )), (~0), l_box.gc );*/
    
    XSetFont ( dpy, l_box.gc, l_box.font -> fid );
    XSetForeground ( dpy, l_box.gc, l_box.fg );
    XSetBackground ( dpy, l_box.gc, l_box.bg );

    screen = DefaultScreen ( dpy );
    root = RootWindow ( dpy, screen );
    XGetWindowAttributes ( dpy, root, &att );
    l_box.width = att.width / 3;
    l_box.height = att.height / 3 ;
    l_box.t_height = l_box.font -> ascent + l_box.font -> descent + LB_SPACE;
    l_box.b_width = XTextWidth ( l_box.font, LB_CANCEL, strlen (LB_CANCEL) ) + (2 * LB_SPACE);
    l_box.b_height = l_box.t_height;
    
    l_box.str_x = (l_box.b_width - (l_box.b_width - (2 * LB_SPACE))) /2 ;
    l_box.str_y = l_box.font -> ascent + (LB_SPACE / 2);
    (void) strcpy ( l_box.select_text, "Select : " );
    (void) strcpy ( l_box.title_text, " " );

    l_box.frame = XCreateSimpleWindow ( dpy, root, 0, 0,  
				       l_box.width, l_box.height, 0, black, black );
    l_box.title = XCreateSimpleWindow ( dpy, l_box.frame, 0, 0, 10, 10,0, l_box.fg, l_box.bg);
    l_box.w_select = XCreateSimpleWindow ( dpy, l_box.frame, 0, l_box.t_height, 10, 10,0, l_box.fg, l_box.bg);
    l_box.main = XCreateSimpleWindow ( dpy, l_box.frame, 0, (2 * l_box.t_height), 10, 10,0, l_box.fg, l_box.bg);
    l_box.ctr = XCreateSimpleWindow ( dpy, l_box.frame, 0, 0, 10,l_box.t_height + (2 * LB_SPACE),0, l_box.fg, l_box.bg);
    l_box.ok = XCreateSimpleWindow ( dpy, l_box.ctr, 0, 0, l_box.b_width, l_box.b_height,0, l_box.fg, l_box.bg);
    l_box.cancel = XCreateSimpleWindow ( dpy, l_box.ctr, 0, 0, l_box.b_width, l_box.b_height,0, l_box.fg, l_box.bg);
    
    sizehints.flags = PSize | PMinSize;
    sizehints.height = l_box.height;
    sizehints.width = l_box.width;
    sizehints.min_height = l_box.height - 30;
    sizehints.min_width = l_box.width - 30;
    
    XSetWMProperties ( dpy, l_box.frame, 0, 0, 0, 0, &sizehints, 0, 0 );
    list_del = XInternAtom( dpy, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols ( dpy, l_box.frame, &list_del, 1);
    
    XSelectInput ( dpy, l_box.frame, StructureNotifyMask );
    XSelectInput ( dpy, l_box.title, ExposureMask );
    XSelectInput ( dpy, l_box.w_select, ExposureMask );
    XSelectInput ( dpy, l_box.main, ExposureMask );
    XSelectInput ( dpy, l_box.ctr, ExposureMask );
    XSelectInput ( dpy, l_box.ok, ExposureMask | ButtonPressMask );
    XSelectInput ( dpy, l_box.cancel, ExposureMask | ButtonPressMask );
    
    l_box.text = ( Text * ) MakeTextWindow ( dpy, l_box.main, LB_SPACE, LB_SPACE );
    l_box.scroll = ( SWin  * ) MakeScroll ( dpy, l_box.main,  LB_SPACE, LB_SPACE );
    l_box.text -> swin = l_box.scroll;
    l_box.scroll -> text = (char *) l_box.text;
    l_box.text -> mwin = 0;
    l_box.buf = (Buf *) GetBuffer ( (unsigned) SIZEOF_BUFFER );
    l_box.text -> buf = l_box.buf;
    
    l_box.stat = LB_UNMAP;
    l_box.select = 0;
    l_box.click_time = 0;
    l_box.old_click = 0;
    
    (void) strcpy ( l_box.text -> filename, "NoName" );
    XStoreName ( dpy, l_box.frame, "Xcoral List Box" );
    
}

/*
**	Function name : MapListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void MapListBox ()
{
    XMapSubwindows ( dpy, l_box.frame );
    XMapSubwindows ( dpy, l_box.ctr );
    XMapRaised ( dpy, l_box.frame );
}

/*
**	Function name : UnmapListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UnmapListBox ()
{
    if ( l_box.stat == LB_UNMAP )
      return;
    XUnmapWindow ( dpy, l_box.frame );
    l_box.stat = LB_UNMAP;
    l_box.select = 0;
    (void) strcpy ( l_box.select_text, "Select : " );
}

/*
**	Function name : ConfigListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ConfigListBox ( width, height )
    int width, height;
{
    int m_height, bx, by;
    static int wb_first_conf = True;

    if ( (width == l_box.width) 
	&& (height == l_box.height) 
	&& (wb_first_conf == False )) {
	return;
    }
    else
	wb_first_conf = False;
    
    l_box.width = width;
    l_box.height = height;
    m_height = l_box.height - ((3 * l_box.t_height) + ( 2 * LB_SPACE));
    bx = ((width / 2) - l_box.b_width) / 2;
    by = ((l_box.t_height + (2 * LB_SPACE) - l_box.b_height)) / 2;
    
    XResizeWindow ( dpy, l_box.frame, l_box.width, l_box.height );
    XResizeWindow ( dpy, l_box.title, l_box.width, l_box.t_height );
    XResizeWindow ( dpy, l_box.w_select, l_box.width, l_box.t_height );
    XResizeWindow ( dpy, l_box.ctr, l_box.width, l_box.t_height + (2 * LB_SPACE));
    XResizeWindow ( dpy, l_box.main, l_box.width, m_height );
    XMoveWindow ( dpy, l_box.ctr, 0, (2 * l_box.t_height) + m_height );
    XMoveWindow ( dpy, l_box.ok, bx, by );
    XMoveWindow ( dpy, l_box.cancel, ( 3 * bx ) + l_box.b_width, by );
    
    ConfigTextAndScroll ( l_box.text, width, m_height, LB_SPACE );
}

/*
**	Function name : ExposeListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ExposeListBox ( ev )
    XEvent *ev;
{
    int x;
    
    if ( (ev -> xexpose.window == l_box.main) 
	|| (ev -> xexpose.window == l_box.ctr)) {
	Display3D ( dpy, ev -> xexpose.window, l_box.ts, l_box.bs, 1, 0 );
	return True;
    }
    if ( ev -> xexpose.window == l_box.title ) {
      (void) UpdateListTitle ();
	return True;
    }
    if ( ev -> xexpose.window == l_box.w_select ) {
      (void) UpdateSelect ();
	return True;
    }
	
    if ( ev -> xexpose.window == l_box.ok ) {
	x = (l_box.b_width - XTextWidth ( l_box.font, LB_OK, strlen(LB_OK))) / 2;
	XDrawString ( dpy, l_box.ok, l_box.gc, x, l_box.str_y, LB_OK, strlen(LB_OK));
	Display3D ( dpy, ev -> xexpose.window, l_box.ts, l_box.bs, 1, 0 );
	return True;
    }
    
    if ( ev -> xexpose.window == l_box.cancel ) {
	x = (l_box.b_width - XTextWidth ( l_box.font, LB_CANCEL, strlen(LB_CANCEL))) / 2;
	XDrawString ( dpy, l_box.cancel, l_box.gc, x, l_box.str_y, LB_CANCEL, strlen(LB_CANCEL));
	Display3D ( dpy, ev -> xexpose.window, l_box.ts, l_box.bs, 1, 0 );
	return True;
    }

    if ( ev -> xexpose.window == l_box.text -> window ) {
      RefreshPageAndUpdateScroll ( l_box.text );
      (void) UpdateSelect ();
      UpdateTextItem ( l_box.text, l_box.select );
      XFlush ( dpy );
      return True;
    }

    if ( ev -> xexpose.window == l_box.scroll -> frame ) {
	RefreshScrollFrame ( dpy, l_box.scroll );
	return True;
    }

    if ( ev -> xexpose.window == l_box.scroll -> scroll ) {
	RefreshScrollBar ( dpy, l_box.scroll );
	return True;
    }
    return False;
}

/*
**	Function name : UpdateTitle
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UpdateListTitle ()
{
    int width;
    
    XClearWindow ( dpy, l_box.title );
    width = XTextWidth ( l_box.font, l_box.title_text, strlen( l_box.title_text));
    if ( width > ( l_box.width - ( 2 * LB_SPACE )))
	(void) strcpy ( l_box.title_text, "<< >>" );

    XDrawString ( dpy, l_box.title, l_box.gc,
		 (( l_box.width - ( 2 * LB_SPACE )) - width) / 2 ,
		 l_box.str_y,
		 l_box.title_text, strlen( l_box.title_text));
    
    Display3D ( dpy, l_box.title, l_box.ts, l_box.bs, 1, 0 );
}

/*
**	Function name : UpdateSelect
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UpdateSelect ()
{

    XClearWindow ( dpy, l_box.w_select );
    
    XDrawString ( dpy, l_box.w_select, l_box.gc, l_box.str_x, l_box.str_y,
		 l_box.select_text, strlen( l_box.select_text));
    Display3D ( dpy, l_box.w_select, l_box.ts, l_box.bs, 1, 0 );
}

/*
**	Function name : SelectFromListBox
**
**	Description : 
**
**	Input : 
**	Ouput : L'item selectionne.
*/
char *SelectFromListBox ( msg )
    char *msg;
{
    XEvent ev;
    extern XEvent event;
    int result, new_select;
    char *str;
   
    (void) DisplayList ( msg );
    for (;;) {
	XNextEvent ( dpy, &ev );
	switch ( ev.type ) {
	case Expose:
	  XPutBackEvent ( dpy, &ev );
	  XNextEvent ( dpy, &event );
	  HandleExpose ();
	  break;
	case ConfigureNotify:
	  XPutBackEvent ( dpy, &ev );
	  XNextEvent ( dpy, &event );
	  HandleConfigure ();
	  break;
	case EnterNotify:
	  XPutBackEvent ( dpy, &ev );
	  XNextEvent ( dpy, &event );
	  HandleEnter ();
	  break;
	case LeaveNotify:
	  XPutBackEvent ( dpy, &ev );
	  XNextEvent ( dpy, &event );
	  HandleEnter ();
	  break;
	case ButtonPress:
	  if ( ev.xbutton.window == l_box.text -> window ) {
	      l_box.click_time = ev.xbutton.time;
	      if ( DoubleClick (l_box.click_time, &l_box.old_click) == True ) {
		if ( l_box.select != 0 ) {
		  l_box.select = 0;
		  (void) UnmapListBox ();
		  return ( (char *) CurrentTextItem ( l_box.text ) );
		}
	      }
	      else {
		new_select = SelectTextItem ( l_box.text, 
				  ev.xbutton.x, ev.xbutton.y, l_box.select );
		if ( new_select ) {
		  l_box.select = new_select;
		  str = (char *) CurrentTextItem ( l_box.text );
		  if ( str ) {
		    (void) sprintf ( l_box.select_text, "Select : %s", str );
		    (void) UpdateSelect ();
		    (void) free ( str );
		  }
		  l_box.old_click = l_box.click_time;
		}
		else {
		  l_box.old_click = 0;
		}
	      }
	  }
	  else if ( ButtonPressInScroll ( l_box.scroll,
		 ev.xbutton.window, ev.xbutton.y, &result )) {
	    RunScrollAndUpdateItem ( l_box.text, l_box.select, result );
	  }
	  else if ( ev.xbutton.window == l_box.ok ) {
	    if ( l_box.select == 0 ) {
	      (void) UnmapListBox ();
	      return 0;
	    }
	    else {
	      l_box.select = 0;
	      (void) UnmapListBox ();
	      return ( (char *) CurrentTextItem ( l_box.text ) );
	    }
	  }
	  else if ( ev.xbutton.window == l_box.cancel ) {
	      l_box.select = 0;
	      (void) UnmapListBox ();
	      return 0;
	  }
	  else {
	      klaxon ();
	  }
	  break;
	case KeyPress:
	  klaxon ();
	  break;
	}
    }
}

/*
**	Function name : DoubleClick
**
**	Description : 
**	Input : 
**	Ouput :
*/
int DoubleClick ( current_t, p_old_t )
    Time current_t, * p_old_t;
{
    if (! *p_old_t ) 
      return False;
	
    if ( (current_t - (*p_old_t)) < 200) {
      *p_old_t = 0;
      return True;
    }

    return False;
}

/*
**	Function name : DisplayList
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void DisplayList ( msg )
    char *msg;
{
    
    if ( l_box.stat != LB_MAP ) {
	MapListBox ();
	WaitForMapped ( l_box.frame, False );
	l_box.stat = LB_MAP;	
    }
    else
          XMapRaised ( dpy, l_box.frame );

    if ( msg != 0 ) {
	if ( strlen ( msg ) > (TITLE_SIZE - 2) ) {
	  (void) strncpy ( l_box.title_text, msg, TITLE_SIZE -1 );
	  l_box.title_text [TITLE_SIZE] = '\0';
	}
	else
	  (void) strcpy ( l_box.title_text, msg );
    }
    (void) UpdateListTitle ();
    (void) UpdateSelect ();
    TextCursorOff ( l_box.text );
    FirstPageAndUpdateScroll ( l_box.text );
}

/*
**	Function name : FillList
**
**	Description : 
**	Input : 
**	Ouput :
*/
void FillList ( str )
    char *str;
{
    if ( str == 0 )
      return;

/*    GotoEndOfBuf ( l_box.text ); */
    HoleToRight ( l_box.text -> buf );
    GotoLineNumber ( l_box.text, l_box.text -> lines_in_buf );

    InsertNchar ( l_box.buf, str, strlen ( str ));
    InsertNchar ( l_box.buf, "\n", 1 );
    SetTextModif ( l_box.text );
}

/*
**	Function name : ClearListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ClearListBox ()
{
    ClearBuffer ( l_box.text -> buf );
}

/*
**	Function name : FileNamesInListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void OpenFilesInListBox ()
{
    EdWin	**t;
    int i = 1;
    int j = 1;
    char tmp [256];
    char *name;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t != 0 ) {
	    bzero ( tmp, 256 );
	    if ( (name = strrchr ( (*t) -> text -> filename, '/' )) != 0 )
	      if ( (*t) -> text -> modif == True ) 
		(void) sprintf ( tmp,
				" %d  : buffer #%d  **\t%s\n", i, j,(char *) (name+1) );
	    else
	      (void) sprintf ( tmp,
			      " %d  : buffer #%d  \t%s\n", i, j,(char *) (name+1) );
	    else 
	      (void) sprintf ( tmp,
			      " %d  : buffer #%d ...\n", i, j );
	    InsertNchar ( l_box.buf, tmp, strlen ( tmp ));
	    i++;
	}
	j++;
    }
    SetTextModif ( l_box.text );
}


/*
**	Function name : KillBufferInList
**
**	Description : 
**	Input : 
**	Ouput :
*/
void KillBufferInList ()
{
    LoadKillBuffer ( l_box.buf );
    SetTextModif ( l_box.text );
}
