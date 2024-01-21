/* ########################################################################

			       man_box.c

   File: man_box.c
   Path: /home/fournigault/c/X11/xcoral-2.31/man_box.c
   Description: 
   Created: Fri Jan 27 11:19:03 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:19:03 MET 1995
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
#include <X11/cursorfont.h>
#include <string.h>

#include "xcoral.h"
#include "main_text.h"
#include "options.h"
#include "man_box.h"
#include "justify.h"
#include "parse.h"
#include "main_events.h"
#include "page.h"
#include "text_cursor.h"
#include "shadow.h"
#include "bm_search.h"

extern Display *dpy;
MBox man_box;
static  Atom man_del;
static  fill_man = True;

FCT (static void, LoadManText, () );
FCT (static void, RefreshManTitleBox, () );
FCT (static void, UnmapManBox, () );


/*
**	Function name : InitManBox
**
**	Description : 
**	Input : 
**	Ouput : 
*/
void InitManBox ()
{
    XWindowAttributes att;
    Window root;
    int screen;
    unsigned long black, white;
    XGCValues	 gcv;
    XSizeHints sizehints;
    
    black = BlackPixel ( dpy, DefaultScreen ( dpy ));
    white = WhitePixel ( dpy, DefaultScreen ( dpy ));

    man_box.mwin = 0;
    man_box.fg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ? 
      black : GetOpColor ( OP_MENU_FG );
    man_box.bg = (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ?
      white : GetOpColor ( OP_MENU_BG );
    man_box.ts = GetOpColor ( OP_MENU_TS );
    man_box.bs = GetOpColor ( OP_MENU_BS );
    man_box.font = LoadFont ( dpy, MBOX_FONT );
    man_box.gc = XCreateGC ( dpy, DefaultRootWindow ( dpy ), 0,  &gcv );
    /*XCopyGC ( dpy, DefaultGC (dpy, DefaultScreen ( dpy )), (~0), man_box.gc );*/
    
    XSetFont ( dpy, man_box.gc, man_box.font -> fid );
    XSetForeground ( dpy, man_box.gc, man_box.fg );
    XSetBackground ( dpy, man_box.gc, man_box.bg );

    screen = DefaultScreen ( dpy );
    root = RootWindow ( dpy, screen );
    XGetWindowAttributes ( dpy, root, &att );
    man_box.width = att.width / 2 ;
    man_box.height = ( 4 * att.height ) / 5 ;
    man_box.t_height = man_box.font -> ascent + man_box.font -> descent + MB_SPACE;
    man_box.b_width = XTextWidth ( man_box.font, MB_SEARCH, strlen (MB_SEARCH) ) + (2 * MB_SPACE);
    man_box.b_height = man_box.t_height;

    man_box.str_x = (man_box.b_width - (man_box.b_width - (2 * MB_SPACE))) /2 ;
    man_box.str_y = man_box.font -> ascent + (MB_SPACE / 2);
    
    man_box.frame = XCreateSimpleWindow ( dpy, root, ((att.width/2) - (man_box.width/2)),
					  ((att.height/2) - (man_box.height/2)),  
				       man_box.width, man_box.height, 0, black, black );
    man_box.title = XCreateSimpleWindow ( dpy, man_box.frame, 0, 0,
					  10, 10,0, man_box.fg, man_box.bg);

    man_box.manframe = XCreateSimpleWindow ( dpy, man_box.frame, 0, man_box.t_height,
					 10, 10 ,0, man_box.fg, man_box.bg);
    man_box.mantitle = XCreateSimpleWindow ( dpy, man_box.manframe, MB_SPACE, MB_SPACE,
					 10, 10 ,0, man_box.fg, man_box.bg);
    man_box.mantext = XCreateSimpleWindow ( dpy, man_box.manframe, 
	MB_SPACE, ( 2* MB_SPACE ) + man_box.t_height, 10, 10 ,0, man_box.fg, man_box.bg);
    
    man_box.indexframe = XCreateSimpleWindow ( dpy, man_box.frame, 0, man_box.t_height,
					 10, 10 ,0, man_box.fg, man_box.bg);
    man_box.indextitle = XCreateSimpleWindow ( dpy, man_box.indexframe, MB_SPACE, MB_SPACE,
					 10, 10 ,0, man_box.fg, man_box.bg);
    man_box.indextext = XCreateSimpleWindow ( dpy, man_box.indexframe, 
	MB_SPACE, ( 2* MB_SPACE ) + man_box.t_height, 10, 10 ,0, man_box.fg, man_box.bg);
    
    man_box.tocframe = XCreateSimpleWindow ( dpy, man_box.frame, 0, man_box.t_height,
					 10, 10 ,0, man_box.fg, man_box.bg);
    man_box.toctitle = XCreateSimpleWindow ( dpy, man_box.tocframe, MB_SPACE, MB_SPACE,
					 10, 10 ,0, man_box.fg, man_box.bg);
    man_box.toctext = XCreateSimpleWindow ( dpy, man_box.tocframe, 
	MB_SPACE, ( 2* MB_SPACE ) + man_box.t_height, 10, 10 ,0, man_box.fg, man_box.bg);
    
    man_box.ctr = XCreateSimpleWindow ( dpy, man_box.frame, 0, 0,
	10,(2 * man_box.t_height) + (3 * MB_SPACE),0, man_box.fg, man_box.bg);

    man_box.close = XCreateSimpleWindow ( dpy, man_box.ctr, MB_SPACE, MB_SPACE,
       man_box.b_width, man_box.b_height,0, man_box.fg, man_box.bg);
    man_box.search = XCreateSimpleWindow ( dpy, man_box.ctr,
       man_box.b_width + MB_SPACE + 2, MB_SPACE,
       man_box.b_width, man_box.b_height,0, man_box.fg, man_box.bg);
    man_box.up = XCreateSimpleWindow ( dpy, man_box.ctr,
      ( 2 *man_box.b_width ) + MB_SPACE + 4, MB_SPACE,
       man_box.b_width, man_box.b_height, 0, man_box.fg, man_box.bg);
    man_box.down = XCreateSimpleWindow ( dpy, man_box.ctr, 
	( 3 *man_box.b_width ) + MB_SPACE + 6, MB_SPACE,
       man_box.b_width, man_box.b_height,0, man_box.fg, man_box.bg);

    sizehints.flags = USPosition | PPosition | PSize | PMinSize | PMaxSize;

    sizehints.width = man_box.width;
    sizehints.height = man_box.height;
    sizehints.min_width = (3 * man_box.width) / 4;
    sizehints.min_height = (3 * man_box.height) / 4;
    sizehints.max_width = ( 5 * att.width ) / 6;
    sizehints.max_height = ( 5 * att.height ) / 6;
    sizehints.x = ((att.width/2) - (man_box.width/2));
    sizehints.y = ((att.height/2) - (man_box.height/2));
   
    XSetWMProperties ( dpy, man_box.frame, 0, 0, 0, 0, &sizehints, 0, 0 );
    man_del = XInternAtom( dpy, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols ( dpy, man_box.frame, &man_del, 1);
    
    XSelectInput ( dpy, man_box.frame, StructureNotifyMask );
    XSelectInput ( dpy, man_box.title, ExposureMask );
    XSelectInput ( dpy, man_box.manframe, ExposureMask );
    XSelectInput ( dpy, man_box.mantitle, ExposureMask );
    XSelectInput ( dpy, man_box.mantext, ExposureMask );
    XSelectInput ( dpy, man_box.indexframe, ExposureMask );
    XSelectInput ( dpy, man_box.indextitle, ExposureMask );
    XSelectInput ( dpy, man_box.indextext, ExposureMask );
    XSelectInput ( dpy, man_box.tocframe, ExposureMask );
    XSelectInput ( dpy, man_box.toctitle, ExposureMask );
    XSelectInput ( dpy, man_box.toctext, ExposureMask );
    XSelectInput ( dpy, man_box.ctr, ExposureMask );
    XSelectInput ( dpy, man_box.close, ExposureMask | ButtonPressMask | ButtonReleaseMask );
    XSelectInput ( dpy, man_box.search, ExposureMask | ButtonPressMask | ButtonReleaseMask );
    XSelectInput ( dpy, man_box.up, ExposureMask | ButtonPressMask | ButtonReleaseMask );
    XSelectInput ( dpy, man_box.down, ExposureMask | ButtonPressMask | ButtonReleaseMask );

    man_box.man_text = ( Text * ) MakeTextWindow ( dpy, man_box.mantext, MB_SPACE, MB_SPACE );
    man_box.man_scroll = ( SWin  * ) MakeScroll ( dpy, man_box.mantext, MB_SPACE, MB_SPACE );
    man_box.man_text -> swin = man_box.man_scroll;
    man_box.man_scroll -> text = (char *) man_box.man_text;
    man_box.man_text -> mwin = 0;
    man_box.man_buf = (Buf *) GetBuffer ( (unsigned) SIZEOF_BUFFER );
    man_box.man_text -> buf = man_box.man_buf;

    man_box.index_text = ( Text * ) MakeTextWindow ( dpy, man_box.indextext, MB_SPACE, MB_SPACE );
    man_box.index_scroll = ( SWin  * ) MakeScroll ( dpy, man_box.indextext, MB_SPACE, MB_SPACE );
    man_box.index_text -> swin = man_box.index_scroll;
    man_box.index_scroll -> text = (char *) man_box.index_text;
    man_box.index_text -> mwin = 0;
    man_box.index_buf = (Buf *) GetBuffer ( (unsigned) SIZEOF_BUFFER );
    man_box.index_text -> buf = man_box.index_buf;

    man_box.toc_text = ( Text * ) MakeTextWindow ( dpy, man_box.toctext, MB_SPACE, MB_SPACE );
    man_box.toc_scroll = ( SWin  * ) MakeScroll ( dpy, man_box.toctext, MB_SPACE, MB_SPACE );
    man_box.toc_text -> swin = man_box.toc_scroll;
    man_box.toc_scroll -> text = (char *) man_box.toc_text;
    man_box.toc_text -> mwin = 0;
    man_box.toc_buf = (Buf *) GetBuffer ( (unsigned) SIZEOF_BUFFER );
    man_box.toc_text -> buf = man_box.toc_buf;

    man_box.stat = MB_UNMAP;
    man_box.select_index = 0;
    man_box.select_toc = 0;
    man_box.click_time = 0;
    man_box.old_click = 0;
    man_box.empty = True;
    man_box.width_text = man_box.width - ( 8 * MB_SPACE ) - GetScrollWidth () - 1;
    man_box.man_text -> x_or = 2 * MB_SPACE;
    XStoreName ( dpy, man_box.frame, "Xcoral Manual" );
    (void) strcpy ( man_box.title_text, "Ladies and gentlemen, now the nice Xcoral online Manual" );

/*    (void) LoadManText (); */
}

/*
**	Function name : DisplayManBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void DisplayManBox ()
{
  static first_display = True;
  
  XMapSubwindows ( dpy, man_box.ctr );
  XMapWindow ( dpy, man_box.ctr );
    
  XMapSubwindows ( dpy, man_box.manframe );
  XMapWindow ( dpy, man_box.manframe );

  XMapSubwindows ( dpy, man_box.indexframe );
  XMapWindow ( dpy, man_box.indexframe );

  XMapSubwindows ( dpy, man_box.tocframe );
  XMapWindow ( dpy, man_box.tocframe );
	
  XMapSubwindows ( dpy, man_box.frame );
  XMapRaised ( dpy, man_box.frame );

  if ( man_box.stat != MB_MAP ) {
    WaitForMapped ( man_box.frame, False );
    man_box.stat = MB_MAP;	
  }
  else
    XMapRaised ( dpy, man_box.frame );
  
  if ( first_display ) {
      WatchOn ( man_box.frame );
      LoadManText ();
      RefreshPageAndUpdateScroll ( man_box.man_text );
      RefreshPageAndUpdateScroll ( man_box.index_text );
      RefreshPageAndUpdateScroll ( man_box.toc_text );
      WatchOff ( man_box.frame );
      first_display = False;
      if ( fill_man ) {
	  AtLineDisplayPage ( man_box.man_text,
			     GetLineManFromToc ( 2 )); /* Chapter 1 */
	  TextCursorOff ( man_box.man_text );
      }
  }
}

/*
**	Function name : LoadManText
**
**	Description : Charge les buffers man.index et toc
**         et preparation pour l'affichage des premiers pages.  
**	Input : 
**	Ouput :
*/
static void LoadManText ()
{
  int ml, il, tl;
  XFontStruct *man_font_text = GetOpFont ( OP_TEXT_FONT );
  
  if ( FillManText ( man_font_text, man_box.width_text,
	       man_box.man_buf, man_box.index_buf, man_box.toc_buf,
		    &ml, &il, &tl ) == 0 )
    fill_man = False;

  SetTextModif ( man_box.man_text );
  SetTextModif ( man_box.index_text );
  SetTextModif ( man_box.toc_text );

  HoleToLeft ( man_box.man_text -> buf ); 
  man_box.man_text -> lines_in_buf = GetNumberOfLineInBuf ( man_box.man_text -> buf );
  SetScrollLine ( man_box.man_text -> swin, man_box.man_text -> lines_in_buf );

  HoleToLeft ( man_box.index_text -> buf ); 
  man_box.index_text -> lines_in_buf = GetNumberOfLineInBuf ( man_box.index_text -> buf );
  SetScrollLine ( man_box.index_text -> swin, man_box.index_text -> lines_in_buf );

  HoleToLeft ( man_box.toc_text -> buf ); 
  man_box.toc_text -> lines_in_buf = GetNumberOfLineInBuf ( man_box.toc_text -> buf );
  SetScrollLine ( man_box.toc_text -> swin, man_box.toc_text -> lines_in_buf );
}

/*
**	Function name : UnmapManBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UnmapManBox ()
{
    XUnmapWindow ( dpy, man_box.frame );
}

/*
**	Function name : RefreshManTitleBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void RefreshManTitleBox ()
{
    int width;
    
    XClearWindow ( dpy, man_box.title );
    width = XTextWidth ( man_box.font, man_box.title_text, strlen( man_box.title_text));
    if ( width > ( man_box.width - ( 2 * MB_SPACE )))
      XDrawString ( dpy, man_box.title, man_box.gc,
		   man_box.index_text -> x_or,
		   man_box.str_y,
		   "Xcoral online Manual", strlen("Xcoral online Manual" ));
    
    else
      XDrawString ( dpy, man_box.title, man_box.gc,
		   man_box.index_text -> x_or,
		   man_box.str_y,
		   man_box.title_text, strlen( man_box.title_text));
    
    Display3D ( dpy, man_box.title, man_box.ts, man_box.bs, 1, 0 );
}

/*
**	Function name : ConfigManBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ConfigManBox ( width, height )
    int width, height;
{
    static int man_first_conf = True;
    int m_height, u_height, t_height;
    int mf_width,if_width,tf_width,mt_width,it_width,tt_width;

    if ( (width == man_box.width) 
	&& (height == man_box.height) 
	&& (man_first_conf == False )) {
	return;
    }
    else
	man_first_conf = False;
    
    man_box.width = width;
    man_box.height = height;
	
    XResizeWindow ( dpy, man_box.frame, man_box.width, man_box.height );
    XResizeWindow ( dpy, man_box.title, man_box.width, man_box.t_height );

    mf_width = man_box.width; /* man frame width */
    if_width = man_box.width / 2; /* index frame width */
    tf_width = man_box.width - if_width; /* toc frame width */
    mt_width = mf_width - ( 2 * MB_SPACE ); /* man title width */
    it_width = if_width - ( 2 * MB_SPACE ); /* index title width */
    tt_width = tf_width - ( 2 * MB_SPACE ); /* toc title width */

    t_height = (man_box.height - ( 2 * man_box.t_height ) - ( 2 * MB_SPACE ));
    u_height = t_height / 3;
    m_height = 2 * u_height; /* Hauteur la page de man */
    
    XResizeWindow ( dpy, man_box.manframe, mf_width, m_height );
    XResizeWindow ( dpy, man_box.indexframe, if_width, t_height - m_height );
    XResizeWindow ( dpy, man_box.tocframe, tf_width, t_height - m_height );
				     
    XResizeWindow ( dpy, man_box.mantitle, mt_width, man_box.t_height );
    XResizeWindow ( dpy, man_box.indextitle, it_width, man_box.t_height );
    XResizeWindow ( dpy, man_box.toctitle, tt_width, man_box.t_height );
    
    m_height = 2 * u_height - (( 3 * MB_SPACE ) + man_box.t_height ); /* height frame text */
    
    XResizeWindow ( dpy, man_box.mantext, mt_width, m_height );
    
    m_height = t_height - 2 * u_height - (( 3 * MB_SPACE ) + man_box.t_height ); /* height frame text */
    XResizeWindow ( dpy, man_box.indextext, it_width, m_height );
    XResizeWindow ( dpy, man_box.toctext, tt_width, m_height );

    XMoveWindow ( dpy, man_box.indexframe, 0, man_box.t_height + 2 * u_height );
    XMoveWindow ( dpy, man_box.tocframe, if_width , man_box.t_height + 2 * u_height );

    XMoveResizeWindow ( dpy, man_box.ctr,
		       0,man_box.height  - man_box.t_height - ( 2 * MB_SPACE ),
		       man_box.width, man_box.t_height + ( 2 * MB_SPACE ) );
    
    m_height = (2 * u_height) - man_box.t_height - ( 3 * MB_SPACE );
    ConfigTextAndScroll ( man_box.man_text, mt_width, m_height, MB_SPACE );
    m_height = t_height - (2 * u_height) - man_box.t_height - ( 3 * MB_SPACE );
    ConfigTextAndScroll ( man_box.index_text, it_width, m_height, MB_SPACE );
    ConfigTextAndScroll ( man_box.toc_text, tt_width, m_height, MB_SPACE );

    if ( man_box.width_text != 
	(man_box.width - (8 * MB_SPACE) - GetScrollWidth () - 1) ) {
      man_box.width_text = man_box.width - (8 * MB_SPACE) - GetScrollWidth () - 1;
      WatchOn ( man_box.frame );
      man_box.select_index = 0;
      man_box.select_toc = 0;
      KillText ( dpy, man_box.man_text );
      KillText ( dpy, man_box.index_text );
      KillText ( dpy, man_box.toc_text );
      LoadManText ();
      RefreshPageAndUpdateScroll ( man_box.man_text );
      RefreshPageAndUpdateScroll ( man_box.index_text );
      RefreshPageAndUpdateScroll ( man_box.toc_text );
      WatchOff ( man_box.frame );
      if ( fill_man ) {
	  AtLineDisplayPage ( man_box.man_text,
			     GetLineManFromToc ( 2 )); /* Chapter 1 */
	  TextCursorOff ( man_box.man_text );
      }
    }
}


/*
**	Function name : ButtonManBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ButtonManBox ( ev )
    XButtonEvent *ev;
{
  int result, line, new_select;
  
  if ( ev -> window == man_box.close) {
    Display3D ( dpy, man_box.close, man_box.ts, man_box.bs, 1, 1 );
    (void) UnmapManBox ();
    return True;
  }
  if ( ev -> window == man_box.search) {
    Display3D ( dpy, man_box.search, man_box.ts, man_box.bs, 1, 1 );
    SearchInMan ( man_box.man_text );
    Display3D ( dpy, man_box.search, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  if ( ButtonPressInScroll ( man_box.man_scroll, 
	    ev -> window, ev -> y, &result )) {
    RunScroll ( man_box.man_text, result );
    TextCursorOn ( man_box.man_text );
    TextCursorOff ( man_box.man_text );
    return True;
  }
  if ( ButtonPressInScroll ( man_box.index_scroll, 
	    ev -> window, ev -> y, &result )) {
    RunScrollAndUpdateItem ( man_box.index_text, man_box.select_index, result );
    return True;
  }
  if ( ButtonPressInScroll ( man_box.toc_scroll, 
	    ev -> window, ev -> y, &result )) {
    RunScrollAndUpdateItem ( man_box.toc_text, man_box.select_toc, result );
    return True;
  }
  if ( ev -> window == man_box.index_text -> window ) {
      new_select = SelectTextItem ( man_box.index_text,
		  ev -> x, ev -> y, man_box.select_index );
      if ( new_select != 0 ) {
	man_box.select_index = new_select;
	line = GetLineManFromIndex ( man_box.select_index );
	AtLineDisplayPage ( man_box.man_text, line );
	TextCursorOff ( man_box.man_text );
      }
      return True;
  }
  if ( ev -> window == man_box.toc_text -> window ) {
      new_select = SelectTextItem ( man_box.toc_text,
		  ev -> x, ev -> y, man_box.select_toc );
      if ( new_select != 0 ) {
	man_box.select_toc = new_select;
	line = GetLineManFromToc ( man_box.select_toc );
	AtLineDisplayPage ( man_box.man_text, line );
	TextCursorOff ( man_box.man_text );
      }
      return True;
  }
  if ( ev -> window == man_box.up ) {
    Display3D ( dpy, ev -> window, man_box.ts, man_box.bs, 1, 1 );
    ScrollUpCont ( man_box.man_text );
    Display3D ( dpy, ev -> window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  if ( ev -> window == man_box.down ) {
    Display3D ( dpy, ev -> window, man_box.ts, man_box.bs, 1, 1 );
    ScrollDownCont ( man_box.man_text );
    Display3D ( dpy, ev -> window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  return False;
}
    
/*
**	Function name : ExposeManBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ExposeManBox ( ev )
    XEvent *ev;
{
  int x;
  
  if ( ev -> xexpose.window == man_box.title ) {
    (void) RefreshManTitleBox ();
    return True;
  }
  if ( (ev -> xexpose.window == man_box.manframe)
      || (ev -> xexpose.window == man_box.indexframe)
      || (ev -> xexpose.window == man_box.tocframe)
      || (ev -> xexpose.window == man_box.mantext)
      || (ev -> xexpose.window == man_box.indextext)
      || (ev -> xexpose.window == man_box.toctext)
      || (ev -> xexpose.window == man_box.ctr) ) {
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  if ( ev -> xexpose.window == man_box.mantitle ) {
    x = ( man_box.width - ( 2 * MB_SPACE) - 
	 XTextWidth ( man_box.font, MB_MANTITLE, strlen(MB_MANTITLE))) / 2;
    XDrawString ( dpy, man_box.mantitle, man_box.gc, x, man_box.str_y, MB_MANTITLE, strlen(MB_MANTITLE));
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  
  if ( ev -> xexpose.window == man_box.indextitle ) {
    x = ((( man_box.width) / 2) - ( 2 * MB_SPACE) - 
	 XTextWidth ( man_box.font, MB_INDEXTITLE, strlen(MB_INDEXTITLE))) / 2;
    XDrawString ( dpy, man_box.indextitle, man_box.gc, x, man_box.str_y, MB_INDEXTITLE, strlen(MB_INDEXTITLE));
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  
  if ( ev -> xexpose.window == man_box.toctitle ) {
    x = ((( man_box.width) / 2) - ( 2 * MB_SPACE) - 
	 XTextWidth ( man_box.font, MB_TOCTITLE, strlen(MB_TOCTITLE))) / 2;
    if ( x > 0 )
      XDrawString ( dpy, man_box.toctitle, man_box.gc,
		   x, man_box.str_y, MB_TOCTITLE, strlen(MB_TOCTITLE));
    else {
      x = ((( man_box.width) / 2) - ( 2 * MB_SPACE) - 
	 XTextWidth ( man_box.font, "Toc", 3)) / 2;
      XDrawString ( dpy, man_box.toctitle, man_box.gc,
		   x, man_box.str_y, "Toc", 3 );
    }
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == man_box.up ) {
    x = (man_box.b_width - XTextWidth ( man_box.font, MB_UP, strlen(MB_UP))) / 2;
    XDrawString ( dpy, man_box.up, man_box.gc, x, man_box.str_y, MB_UP, strlen(MB_UP));
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == man_box.down ) {
    x = (man_box.b_width - XTextWidth ( man_box.font, MB_DOWN, strlen(MB_DOWN))) / 2;
    XDrawString ( dpy, man_box.down, man_box.gc, x, man_box.str_y, MB_DOWN, strlen(MB_DOWN));
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  
  if ( ev -> xexpose.window == man_box.close ) {
    x = (man_box.b_width - XTextWidth ( man_box.font, MB_CLOSE, strlen(MB_CLOSE))) / 2;
    XDrawString ( dpy, man_box.close, man_box.gc, x, man_box.str_y, MB_CLOSE, strlen(MB_CLOSE));
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == man_box.search ) {
    x = (man_box.b_width - XTextWidth ( man_box.font, MB_SEARCH, strlen(MB_SEARCH))) / 2;
    XDrawString ( dpy, man_box.search, man_box.gc, x, man_box.str_y, MB_SEARCH, strlen(MB_SEARCH));
    Display3D ( dpy, ev -> xexpose.window, man_box.ts, man_box.bs, 1, 0 );
    return True;
  }
  
  if ( ev -> xexpose.window == man_box.man_text -> window ) {
    RefreshPageAndUpdateScroll ( man_box.man_text );
    return True;
  }

  if ( ev -> xexpose.window == man_box.index_text -> window ) {
    RefreshPageAndUpdateScroll ( man_box.index_text );
    UpdateTextItem ( man_box.index_text, man_box.select_index  );
    return True;
  }

  if ( ev -> xexpose.window == man_box.toc_text -> window ) {
    RefreshPageAndUpdateScroll ( man_box.toc_text );
    UpdateTextItem ( man_box.toc_text, man_box.select_toc );
    return True;
  }

  if ( ev -> xexpose.window == man_box.man_scroll -> frame ) {
    RefreshScrollFrame ( dpy, man_box.man_scroll );
    return True;
  }
  
  if ( ev -> xexpose.window == man_box.man_scroll -> scroll ) {
    RefreshScrollBar ( dpy, man_box.man_scroll );
    return True;
  }
  if ( ev -> xexpose.window == man_box.index_scroll -> frame ) {
    RefreshScrollFrame ( dpy, man_box.index_scroll );
    return True;
  }
  
  if ( ev -> xexpose.window == man_box.index_scroll -> scroll ) {
    RefreshScrollBar ( dpy, man_box.index_scroll );
    return True;
  }
  if ( ev -> xexpose.window == man_box.toc_scroll -> frame ) {
    RefreshScrollFrame ( dpy, man_box.toc_scroll );
    return True;
  }
  
  if ( ev -> xexpose.window == man_box.toc_scroll -> scroll ) {
    RefreshScrollBar ( dpy, man_box.toc_scroll );
    return True;
  }
  return False;
}

/*
**    Function name : KeyPressInMan
**
**    Description :
**    Input :
**    Output :
*/
int KeyPressInManual(ev)
    XKeyEvent * ev;
{
  Window w = ev->window;
  
  if ((w == man_box.man_text->window) ||
      (w == man_box.index_text->window) ||
      (w == man_box.toc_text->window)) {
    int numlig;
    char buf[32];
    static KeySym ksym;
    static XComposeStatus compose;
    InfosKey *infos;
    
    bzero( buf, 32 );
    
    XLookupString ( ev, buf, 32, &ksym, &compose );
    if ((numlig = FirstIndexLineBeginningWith(buf[0])) != 0) {
      if ( man_box.select_index ) {
	UpdateTextItem ( man_box.index_text, man_box.select_index );
	man_box.select_index = 0;
      }
      AtLineDisplayPage(man_box.index_text, numlig);
      TextCursorOff ( man_box.index_text );
    }
    return True;
  }
  return False;
}
