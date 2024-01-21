/* ########################################################################

			       dial_box.c

   File: dial_box.c
   Path: /home/fournigault/c/X11/xcoral-2.31/dial_box.c
   Description: 
   Created: Fri Jan 27 11:00:30 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:00:32 MET 1995
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
#include "main_text.h"
#include "options.h"
#include "dial_box.h"
#include "parse.h"
#include "shadow.h"
#include "input_str.h"
#include "main_events.h"

extern Display *dpy;
DBox dial_box;
static  Atom dial_del;
static  int display_ok = True;

FCT (static void, DisplayDialogBox, () );
FCT (static void, UpdateDialogTitle, () );
FCT (static void, MapDialogBox, () );
FCT (static void, UnmapDialogBox, () );

/*
**	Function name : InitDialBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InitDialogBox ()
{
    XWindowAttributes att;
    Window root;
    int screen;
    unsigned long black, white;
    XGCValues	 gcv;
    XSizeHints sizehints;
    
    black = BlackPixel ( dpy, DefaultScreen ( dpy ));
    white = WhitePixel ( dpy, DefaultScreen ( dpy ));

    dial_box.fg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ? 
      black : GetOpColor ( OP_MENU_FG );
    dial_box.bg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ?
      white : GetOpColor ( OP_MENU_BG );
    dial_box.ts = GetOpColor ( OP_MENU_TS );
    dial_box.bs = GetOpColor ( OP_MENU_BS );
    dial_box.font = LoadFont ( dpy, DBOX_FONT );
    dial_box.gc = XCreateGC ( dpy, DefaultRootWindow ( dpy ), 0,  &gcv );
    /*XCopyGC ( dpy, DefaultGC (dpy, DefaultScreen ( dpy )), (~0), dial_box.gc );*/
    
    XSetFont ( dpy, dial_box.gc, dial_box.font -> fid );
    XSetForeground ( dpy, dial_box.gc, dial_box.fg );
    XSetBackground ( dpy, dial_box.gc, dial_box.bg );

    screen = DefaultScreen ( dpy );
    root = RootWindow ( dpy, screen );
    XGetWindowAttributes ( dpy, root, &att );
    dial_box.width = ( 2 * att.width ) / 4;
    dial_box.height = ( 2 * att.height ) / 11  ;
    dial_box.t_height = dial_box.font -> ascent + dial_box.font -> descent + DB_SPACE;
    dial_box.b_width = XTextWidth ( dial_box.font, DB_CANCEL, strlen (DB_CANCEL) ) + (2 * DB_SPACE);
    dial_box.b_height = dial_box.t_height;
    
    dial_box.str_x = (dial_box.b_width - (dial_box.b_width - (2 * DB_SPACE))) /2 ;
    dial_box.str_y = dial_box.font -> ascent + (DB_SPACE / 2);
    (void) strcpy ( dial_box.title_text, " " );

    dial_box.frame = XCreateSimpleWindow ( dpy, root, ((att.width/2) - (dial_box.width/2)),
					  ((att.height/2) - (dial_box.height/2)),  
				       dial_box.width, dial_box.height, 0, black, black );
    dial_box.title = XCreateSimpleWindow ( dpy, dial_box.frame, 0, 0,
					  10, 10,0, dial_box.fg, dial_box.bg);
    dial_box.main = XCreateSimpleWindow ( dpy, dial_box.frame, 0, dial_box.t_height,
					 10, 10 ,0, dial_box.fg, dial_box.bg);
    dial_box.w_mb = XCreateSimpleWindow ( dpy, dial_box.main, DB_SPACE, DB_SPACE, 10,
					 10,0, dial_box.fg, dial_box.bg);
    dial_box.ctr = XCreateSimpleWindow ( dpy, dial_box.frame, 0, 0,
					10,dial_box.t_height + (2 * DB_SPACE),0, dial_box.fg, dial_box.bg);
    dial_box.ok = XCreateSimpleWindow ( dpy, dial_box.ctr, 0, 0,
				       dial_box.b_width, dial_box.b_height,0, dial_box.fg, dial_box.bg);
    dial_box.cancel = XCreateSimpleWindow ( dpy, dial_box.ctr, 0, 0,
					   dial_box.b_width, dial_box.b_height,0, dial_box.fg, dial_box.bg);
    
    sizehints.flags = PPosition | USPosition | PSize | PMinSize | PMaxSize;
    sizehints.height = dial_box.height;
    sizehints.width = dial_box.width;
    sizehints.min_height = dial_box.height;
    sizehints.min_width = dial_box.width;
    sizehints.max_height = dial_box.height;
    sizehints.max_width = dial_box.width;
    sizehints.x = ((att.width/2) - (dial_box.width/2));
    sizehints.y = ((att.height/2) - (dial_box.height/2));
   
    XSetWMProperties ( dpy, dial_box.frame, 0, 0, 0, 0, &sizehints, 0, 0 );
    dial_del = XInternAtom( dpy, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols ( dpy, dial_box.frame, &dial_del, 1);
    
    XSelectInput ( dpy, dial_box.frame, StructureNotifyMask | KeyPressMask );
    XSelectInput ( dpy, dial_box.title, ExposureMask );
    XSelectInput ( dpy, dial_box.main, ExposureMask );
    XSelectInput ( dpy, dial_box.w_mb, ExposureMask );
    XSelectInput ( dpy, dial_box.ctr, ExposureMask );
    XSelectInput ( dpy, dial_box.ok, ExposureMask | ButtonPressMask );
    XSelectInput ( dpy, dial_box.cancel, ExposureMask | ButtonPressMask );
    
    dial_box.stat = DB_UNMAP;
    
    XStoreName ( dpy, dial_box.frame, "Xcoral Dialog Box" );
}

/*
**	Function name : ConfigDialBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ConfigDialogBox ( width, height )
    int width, height;
{
    int m_height, bx, by;
    static int db_first_conf = True;

    if ( (width == dial_box.width) 
	&& (height == dial_box.height) 
	&& (db_first_conf == False )) {
	return;
    }
    else
	db_first_conf = False;
    
    dial_box.width = width;
    dial_box.height = height;
    m_height = dial_box.height - ((2 * dial_box.t_height) + ( 2 * DB_SPACE));
    bx = ((width / 2) - dial_box.b_width) / 2;
    by = ((dial_box.t_height + (2 * DB_SPACE) - dial_box.b_height)) / 2;
    
    XResizeWindow ( dpy, dial_box.frame, dial_box.width, dial_box.height );
    XResizeWindow ( dpy, dial_box.title, dial_box.width, dial_box.t_height );
    XResizeWindow ( dpy, dial_box.ctr, dial_box.width, dial_box.t_height + (2 * DB_SPACE));
    XResizeWindow ( dpy, dial_box.main, dial_box.width, m_height  );
    XResizeWindow ( dpy, dial_box.w_mb, dial_box.width - ( 2 * DB_SPACE), dial_box.t_height );
    XMoveWindow ( dpy, dial_box.ctr, 0, dial_box.t_height + m_height );
    XMoveWindow ( dpy, dial_box.ok, bx, by );
    XMoveWindow ( dpy, dial_box.cancel, ( 3 * bx ) + dial_box.b_width, by );
}

/*
**	Function name : ExposeDialBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ExposeDialogBox ( ev )
    XEvent *ev;
{
    int x;
    
    if ( (ev -> xexpose.window == dial_box.main) 
	|| (ev -> xexpose.window == dial_box.ctr)) {
	Display3D ( dpy, ev -> xexpose.window, dial_box.ts, dial_box.bs, 1, 0 );
	return True;
    }
    if ( ev -> xexpose.window == dial_box.title ) {
      (void) UpdateDialogTitle ();
	return True;
    }
    if ( ev -> xexpose.window == dial_box.w_mb ) {
	Display3D ( dpy, ev -> xexpose.window, dial_box.ts, dial_box.bs, 1, 0 );
	return True;
    }
	
    if ( ev -> xexpose.window == dial_box.cancel) {
	x = (dial_box.b_width - XTextWidth ( dial_box.font, DB_CANCEL, strlen(DB_CANCEL))) / 2;	
	XDrawString ( dpy, dial_box.cancel, dial_box.gc, x, dial_box.str_y,
		     DB_CANCEL, strlen(DB_CANCEL));

	Display3D ( dpy, ev -> xexpose.window, dial_box.ts, dial_box.bs, 1, 0 );
	return True;
    }
    if ( ev -> xexpose.window == dial_box.ok ) {
	x = (dial_box.b_width - XTextWidth ( dial_box.font, DB_OK, strlen(DB_OK))) / 2;
	XDrawString ( dpy, dial_box.ok, dial_box.gc, x, dial_box.str_y,
		     DB_OK, strlen(DB_OK));
	Display3D ( dpy, ev -> xexpose.window, dial_box.ts, dial_box.bs, 1, 0 );
	return True;
    }
    return False;
}


/*
**	Function name : GetStringFromDB
**
**	Description : 
**	Input : 
**	Ouput :
*/
char *GetStringFromDB ( prompt, one_char  )
    char *prompt;
    int one_char;
{
    char *s;
    
    if ( one_char == True )
      display_ok = False;

    if ( strlen (prompt) > 44 )
      prompt [44] = '\0';
    (void) DisplayDialogBox ();
    s = (char *) InputString ( dial_box.w_mb,
	      dial_box.gc, dial_box.font, prompt , one_char );
    (void) UnmapDialogBox ();
    display_ok = True;
    return (s);
}

/*
**	Function name : DisplayDB
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void DisplayDialogBox ()
{
    int by, bx;
      
    if ( dial_box.stat != DB_MAP ) {
      (void) MapDialogBox ();
	WaitForMapped ( dial_box.frame, False );
	dial_box.stat = DB_MAP;	
    }
    else
          XMapRaised ( dpy, dial_box.frame );

    by = ((dial_box.t_height + (2 * DB_SPACE) - dial_box.b_height)) / 2;
    bx = ((dial_box.width / 2) - dial_box.b_width) / 2;

    if ( display_ok == False ) {
	XUnmapWindow ( dpy, dial_box.ok );
	XMoveWindow ( dpy, dial_box.cancel,
		     ((dial_box.width/2) - (dial_box.b_width)/2), by );
    }
    else {
	XMoveWindow ( dpy, dial_box.cancel,
		     ( 3 * bx ) + dial_box.b_width, by );
    }	

    (void) UpdateDialogTitle ();
}

/*
**	Function name : UpdateDTitle
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UpdateDialogTitle ()
{
    XClearWindow ( dpy, dial_box.title );

    XDrawString ( dpy, dial_box.title, dial_box.gc,
		 2 * DB_SPACE,
		 dial_box.str_y,
		 "Enter string", strlen("Enter string"));
    
    Display3D ( dpy, dial_box.title, dial_box.ts, dial_box.bs, 1, 0 );
}


/*
**	Function name : MapDialBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void MapDialogBox ()
{
    XMapSubwindows ( dpy, dial_box.frame );
    XMapSubwindows ( dpy, dial_box.main );    
    XMapSubwindows ( dpy, dial_box.ctr );
    XMapRaised ( dpy, dial_box.frame );
}

/*
**	Function name : UnmapListBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UnmapDialogBox ()
{
    if ( dial_box.stat == DB_UNMAP )
      return;
    XUnmapWindow ( dpy, dial_box.frame );
    dial_box.stat = DB_UNMAP;
}


/*
**	Function name : ButtonInDialBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ButtonDialogBox ( ev )
    XButtonEvent *ev;
{
    if ( ev -> window == dial_box.cancel) {
        Display3D ( dpy, dial_box.cancel, dial_box.ts, dial_box.bs, 1, 1 );
	SetCancelButton ();
	return True;
    }
    if ( ev -> window == dial_box.ok) {
        Display3D ( dpy, dial_box.ok, dial_box.ts, dial_box.bs, 1, 1 );
	SetOkButton ();
	return True;
    }
    return False;
}
