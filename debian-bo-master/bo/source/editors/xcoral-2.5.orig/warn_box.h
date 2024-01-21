/* ########################################################################

			       warn_box.h

   File: warn_box.h
   Path: /home/fournigault/c/X11/xcoral-2.31/warn_box.h
   Description: 
   Created: Fri Jan 27 11:39:29 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:39:30 MET 1995
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


#ifndef  _WARN_BOX_H_
#define  _WARN_BOX_H_

#include "proto_decl.h"

#define WBOX_FONT "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1"
#define TITLE_SIZE 128

#define WB_SPACE 10
#define WB_MAP    1
#define WB_UNMAP  2

#define WB_SAVE "Save"
#define WB_CLOSE "Close"
#define WB_FROM "From : "

#define GetWarningBoxFrame() (w_box.frame)

typedef struct {
	Window		frame, title, main, ctr, save, close;
	unsigned long 	fg, bg, ts, bs;
	GC		gc;
	XFontStruct 	*font;
	int		height, width, b_width, b_height;
	int             t_height, t_width;
	int             str_x, str_y;
	char            title_text [TITLE_SIZE];
	Text            *text;
	SWin            *scroll;
	Buf             *buf;
	MWin            *mwin;
	int             stat;
} WBox;

FCT (int, ButtonWarningBox, (XButtonEvent *ev) );
FCT (void, ConfigWarningBox, (int width, int height) );
FCT (void, DisplayWMessage, (char *mess, char *from, int separator) );
FCT (int, ExposeWarningBox , (XEvent *ev) );
FCT (void, InitWarningBox, () );
FCT (void, UnmapWarningBox, () );

#endif /* _WARN_BOX_H_ */
