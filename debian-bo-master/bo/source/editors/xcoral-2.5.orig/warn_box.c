/* ########################################################################

			       warn_box.c

   File: warn_box.c
   Path: /home/fournigault/c/X11/xcoral-2.31/warn_box.c
   Description: 
   Created: Fri Jan 27 11:38:53 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:38:55 MET 1995
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

#include "main_text.h"
#include "options.h"
#include "warn_box.h"
#include "parse.h"
#include "shadow.h"
#include "page.h"
#include "get_file.h"
#include "main_events.h"
#include "chars_cmds.h"
#include "text_cursor.h"

extern Display *dpy;
WBox w_box;
static  Atom warn_del;

FCT (static void, MapWarningBox, () );
FCT (static void, UpdateWarningTitle, () );

/*
**	Function name : InitWarningBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InitWarningBox ()
{
    XWindowAttributes att;
    Window root;
    int screen;
    unsigned long black, white;
    XGCValues	 gcv;
    XSizeHints sizehints;
    
    black = BlackPixel ( dpy, DefaultScreen ( dpy ));
    white = WhitePixel ( dpy, DefaultScreen ( dpy ));

    w_box.mwin = 0;
    w_box.fg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ? 
      black : GetOpColor ( OP_MENU_FG );
    w_box.bg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ?
      white : GetOpColor ( OP_MENU_BG );
    w_box.ts = GetOpColor ( OP_MENU_TS );
    w_box.bs = GetOpColor ( OP_MENU_BS );
    w_box.font = LoadFont ( dpy, WBOX_FONT );
    w_box.gc = XCreateGC ( dpy, DefaultRootWindow ( dpy ), 0,  &gcv );
    /*XCopyGC ( dpy, DefaultGC (dpy, DefaultScreen ( dpy )), (~0), w_box.gc );*/
    
    XSetFont ( dpy, w_box.gc, w_box.font -> fid );
    XSetForeground ( dpy, w_box.gc, w_box.fg );
    XSetBackground ( dpy, w_box.gc, w_box.bg );

    screen = DefaultScreen ( dpy );
    root = RootWindow ( dpy, screen );
    XGetWindowAttributes ( dpy, root, &att );
    w_box.width = ( 2 * att.width ) / 5;
    w_box.height = att.height / 4 ;
    w_box.t_height = w_box.font -> ascent + w_box.font -> descent + WB_SPACE;
    w_box.b_width = XTextWidth ( w_box.font, WB_CLOSE, strlen (WB_CLOSE) ) + (2 * WB_SPACE);
    w_box.b_height = w_box.t_height;
    
    w_box.str_x = (w_box.b_width - (w_box.b_width - (2 * WB_SPACE))) /2 ;
    w_box.str_y = w_box.font -> ascent + (WB_SPACE / 2);
    (void) strcpy ( w_box.title_text, "From : " );

    w_box.frame = XCreateSimpleWindow ( dpy, root, 0, 0,  
				       w_box.width, w_box.height, 0, black, black );
    w_box.title = XCreateSimpleWindow ( dpy, w_box.frame, 0, 0, 10, 10,0, w_box.fg, w_box.bg);
    w_box.main = XCreateSimpleWindow ( dpy, w_box.frame, 0, w_box.t_height, 10, 10,0, w_box.fg, w_box.bg);
    w_box.ctr = XCreateSimpleWindow ( dpy, w_box.frame, 0, 0, 10,w_box.t_height + (2 * WB_SPACE),0, w_box.fg, w_box.bg);
    w_box.save = XCreateSimpleWindow ( dpy, w_box.ctr, 0, 0, w_box.b_width, w_box.b_height,0, w_box.fg, w_box.bg);
    w_box.close = XCreateSimpleWindow ( dpy, w_box.ctr, 0, 0, w_box.b_width, w_box.b_height,0, w_box.fg, w_box.bg);
    
    sizehints.flags = PSize | PMinSize;
    sizehints.height = w_box.height;
    sizehints.width = w_box.width;
    sizehints.min_height = w_box.height - 30;
    sizehints.min_width = w_box.width - 30;
    
    XSetWMProperties ( dpy, w_box.frame, 0, 0, 0, 0, &sizehints, 0, 0 );
    warn_del = XInternAtom( dpy, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols ( dpy, w_box.frame, &warn_del, 1);
    
    XSelectInput ( dpy, w_box.frame, StructureNotifyMask );
    XSelectInput ( dpy, w_box.title, ExposureMask );
    XSelectInput ( dpy, w_box.main, ExposureMask );
    XSelectInput ( dpy, w_box.ctr, ExposureMask );
    XSelectInput ( dpy, w_box.save, ExposureMask | ButtonPressMask );
    XSelectInput ( dpy, w_box.close, ExposureMask | ButtonPressMask );
    
    w_box.text = ( Text * ) MakeTextWindow ( dpy, w_box.main, WB_SPACE, WB_SPACE );
    w_box.scroll = ( SWin  * ) MakeScroll ( dpy, w_box.main,  WB_SPACE, WB_SPACE );
    w_box.text -> swin = w_box.scroll;
    w_box.scroll -> text = (char *) w_box.text;
    w_box.text -> mwin = 0;
    w_box.buf = (Buf *) GetBuffer ( (unsigned) SIZEOF_BUFFER );
    w_box.text -> buf = w_box.buf;
    
    w_box.stat = WB_UNMAP;
    
    (void) strcpy ( w_box.text -> filename, "_xcoralWarnings" );
    XStoreName ( dpy, w_box.frame, "Xcoral Messages Box" );
    
}

/*
**	Function name : MapWarningBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void MapWarningBox ()
{
    XMapSubwindows ( dpy, w_box.frame );
    XMapSubwindows ( dpy, w_box.ctr );
    XMapRaised ( dpy, w_box.frame );
}

/*
**	Function name : UnmapWarningBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void UnmapWarningBox ()
{
    if ( w_box.stat == WB_UNMAP )
      return;
    XUnmapWindow ( dpy, w_box.frame );
    w_box.stat = WB_UNMAP;    
}

/*
**	Function name : ConfigWarningBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ConfigWarningBox ( width, height )
    int width, height;
{
    int m_height, bx, by;
    static int wb_first_conf = True;

    if ( (width == w_box.width) 
	&& (height == w_box.height) 
	&& (wb_first_conf == False )) {
	return;
    }
    else
	wb_first_conf = False;
    
    w_box.width = width;
    w_box.height = height;
    m_height = w_box.height - ((2 * w_box.t_height) + ( 2 * WB_SPACE));
    bx = ((width / 2) - w_box.b_width) / 2;
    by = ((w_box.t_height + (2 * WB_SPACE) - w_box.b_height)) / 2;
    
    XResizeWindow ( dpy, w_box.frame, w_box.width, w_box.height );
    XResizeWindow ( dpy, w_box.title, w_box.width, w_box.t_height );
    XResizeWindow ( dpy, w_box.ctr, w_box.width, w_box.t_height + (2 * WB_SPACE));
    XResizeWindow ( dpy, w_box.main, w_box.width, m_height );
    XMoveWindow ( dpy, w_box.ctr, 0, w_box.t_height + m_height );
    XMoveWindow ( dpy, w_box.save, bx, by );
    XMoveWindow ( dpy, w_box.close, ( 3 * bx ) + w_box.b_width, by );
    
    ConfigTextAndScroll ( w_box.text, width, m_height, WB_SPACE );
		   
}

/*
**	Function name : ExposeWarningBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ExposeWarningBox ( ev )
    XEvent *ev;
{
    int x;
    
    if ( (ev -> xexpose.window == w_box.title)
	|| (ev -> xexpose.window == w_box.main)
	|| (ev -> xexpose.window == w_box.ctr)) {
	Display3D ( dpy, ev -> xexpose.window, w_box.ts, w_box.bs, 1, 0 );
	(void) UpdateWarningTitle ();
	return True;
    }
	
    if ( ev -> xexpose.window == w_box.save) {
	x = (w_box.b_width - XTextWidth ( w_box.font, WB_SAVE, strlen(WB_SAVE))) / 2;
	XDrawString ( dpy, w_box.save, w_box.gc, x, w_box.str_y, WB_SAVE, strlen(WB_SAVE));
	Display3D ( dpy, ev -> xexpose.window, w_box.ts, w_box.bs, 1, 0 );
	return True;
    }
    
    if (ev -> xexpose.window == w_box.close ) {	
	x = (w_box.b_width - XTextWidth ( w_box.font, WB_CLOSE, strlen(WB_CLOSE))) / 2;	
	XDrawString ( dpy, w_box.close, w_box.gc, x, w_box.str_y, WB_CLOSE, strlen(WB_CLOSE));
	Display3D ( dpy, ev -> xexpose.window, w_box.ts, w_box.bs, 1, 0 );
	return True;
    }

    if ( ev -> xexpose.window == w_box.text -> window ) {
	XClearWindow ( dpy, w_box.text -> window );
	Display3D ( dpy, w_box.text -> window, w_box.ts, w_box.bs, 2, 1 ); 
	SetAndDisplayPage ( w_box.text );
	TextCursorOff ( w_box.text );
	XFlush ( dpy );
	return True;
    }

    if ( ev -> xexpose.window == w_box.scroll -> frame ) {
	RefreshScrollFrame ( dpy, w_box.scroll );
	return True;
    }

    if ( ev -> xexpose.window == w_box.scroll -> scroll ) {
	RefreshScrollBar ( dpy, w_box.scroll );
	return True;
    }
    return False;
}


/*
**	Function name : UpdateWarningTitle
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UpdateWarningTitle ()
{
    XDrawString ( dpy, w_box.title, w_box.gc, w_box.str_x, w_box.str_y,
		 w_box.title_text, strlen( w_box.title_text));
    
}

/*
**	Function name : DisplayWMessage
**
**	Description : 
**	Input : 
**	Ouput :
*/
void DisplayWMessage ( mess, from, separator )
    char *mess, *from;
    int separator;
{
    int lines;
    
    if ( mess == 0 ) {
	return;
    }

    if ( from != 0 ) {
      (void) strcpy ( w_box.title_text, from );
      (void) UpdateWarningTitle ();
    }
    if ( w_box.stat != WB_MAP ) {
	MapWarningBox ();
	WaitForMapped ( w_box.frame, False );
	w_box.stat = WB_MAP;	
    }
    else
          XMapRaised ( dpy, w_box.frame );

    TextCursorOff ( w_box.text );
    GotoEndOfBuf ( w_box.text );    
    ClipOn ( w_box.text, 0 );
    lines = GetNewLine ( mess, strlen(mess) );
    SetTextModif ( w_box.text );

    if ( separator ) 
      InsertLines ( w_box.text, "\n>>>\n", 5, 2 );
    InsertLines ( w_box.text, mess, strlen(mess), lines );
    LastPage ( w_box.text );
    RefreshScrollBar ( dpy, w_box.text -> swin );
    TextCursorOff ( w_box.text );
    ClipOff ( w_box.text );
}


/*
**	Function name : ButtonWBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ButtonWarningBox ( ev )
    XButtonEvent *ev;
{
    int result;

    if ( ButtonPressInScroll ( w_box.scroll, ev -> window, ev -> y, &result ) == True ) {
	RunScroll ( w_box.text, result );
	TextCursorOff ( w_box.text );
	return True;
    }
    
    if ( ev -> window == w_box.close ) {
	Display3D ( dpy, ev -> window, w_box.ts, w_box.bs, 1, 1 );
	XUnmapWindow ( dpy, w_box.frame );
	return True;
    }
    if ( ev -> window == w_box.save ) {
	Display3D ( dpy, ev -> window, w_box.ts, w_box.bs, 1, 1 );
	(void) WriteFile ( w_box.text );
	Display3D ( dpy, ev -> window, w_box.ts, w_box.bs, 1, 0 );
	return True;
    }
    return False;
}

