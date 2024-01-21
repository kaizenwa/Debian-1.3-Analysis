/* ########################################################################

			       dial_box.h

   File: dial_box.h
   Path: /home/fournigault/c/X11/xcoral-2.31/dial_box.h
   Description: 
   Created: Fri Jan 27 11:00:57 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:00:59 MET 1995
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


#ifndef  _DIAL_BOX_H_
#define  _DIAL_BOX_H_

#include "proto_decl.h"

#define DBOX_FONT "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1"
#define TITLE_SIZE 128

#define DB_SPACE 10
#define DB_MAP    1
#define DB_UNMAP  2

#define DB_OK "Ok"
#define DB_CANCEL "Cancel"

typedef struct _DBox {
	Window		frame, title, main, w_mb, ctr, ok, cancel;
	unsigned long 	fg, bg, ts, bs;
	GC		gc;
	XFontStruct 	*font;
	int		height, width, b_width, b_height;
	int             t_height, t_width;
	int             str_x, str_y;
	char            title_text [TITLE_SIZE];
	int             stat;
} DBox;

#define GetDialBoxFrame() (dial_box.frame)

FCT (void, InitDialogBox, () );
FCT (void, ConfigDialogBox, (int width, int height) );
FCT (int, ExposeDialogBox, (XEvent *ev) );
FCT (char *, GetStringFromDB, (char *prompt, int one_char) );
FCT (int, ButtonDialogBox, (XButtonEvent *ev) );

#endif /* _DIAL_BOX_H_ */
