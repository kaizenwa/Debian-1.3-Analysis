/* ########################################################################

			       man_box.h

   File: man_box.h
   Path: /home/fournigault/c/X11/xcoral-2.31/man_box.h
   Description: 
   Created: Fri Jan 27 11:19:32 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:19:33 MET 1995
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


#ifndef  _MAN_BOX_H_
#define  _MAN_BOX_H_

#include "proto_decl.h"

#define MBOX_FONT "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1"
#define TITLE_SIZE 128

#define MB_BUFFER_SIZE	10000
#define MB_SPACE  10
#define MB_MAP    1
#define MB_UNMAP  2
#define MB_TITLE "This is the man"
#define MB_MANTITLE "Manual page"
#define MB_INDEXTITLE "Index"
#define MB_TOCTITLE "Table of contents"
#define MB_SEARCH "Search"
#define MB_CLOSE "Close"
#define MB_UP "Up"
#define MB_DOWN "Down"

typedef struct _MBox {

  Window frame, title, ctr, close, search, up, down;
  Window manframe, indexframe, tocframe;
  Window mantitle, indextitle, toctitle;
  Window mantext, indextext, toctext;
  unsigned long fg, bg, ts, bs, text_fg, text_bg;
  XFontStruct	*font;
  GC gc;
  int	width, height, t_height, b_width, b_height;
  int str_x, str_y;
  Text *man_text, *index_text, *toc_text;
  char title_text [TITLE_SIZE];
  Buf *man_buf, *index_buf, *toc_buf;
  MWin *mwin;
  SWin *man_scroll, *index_scroll, *toc_scroll;
  int select_index, select_toc;
  int	m_width, m_height;
  Time click_time;
  Time old_click;
  int stat;
  int empty;
  int width_text;
  
} MBox;

FCT (int, ButtonManBox, ( XButtonEvent * ev ));
FCT (void, ConfigManBox, (int width, int height) );
FCT (void, DisplayManBox, () );
FCT (int, ExposeManBox, (XEvent *ev) );
FCT (void, InitManBox, () );
FCT (int, KeyPressInManual, (XKeyEvent * ev ));
    
#define GetManBoxFrame() (man_box.frame)

#endif /* _MAN_BOX_H_ */
