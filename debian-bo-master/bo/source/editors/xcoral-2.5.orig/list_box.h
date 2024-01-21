/* ########################################################################

			       list_box.h

   File: list_box.h
   Path: /home/fournigault/c/X11/xcoral-2.31/list_box.h
   Description: 
   Created: Fri Jan 27 11:14:47 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:14:49 MET 1995
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


#ifndef  _LIST_BOX_H_
#define  _LIST_BOX_H_

#include "proto_decl.h"

#define LBOX_FONT "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1"
#define TITLE_SIZE 128

#define LB_SPACE 10
#define LB_MAP    1
#define LB_UNMAP  2

#define LB_OK "Ok"
#define LB_CANCEL "Cancel"

#define GetListBoxFrame() (l_box.frame)

typedef struct _LBox {
	Window		frame, title, w_select, main, ctr, ok, cancel;
	unsigned long 	fg, bg, ts, bs;
	GC		gc;
	XFontStruct 	*font;
	int		height, width, b_width, b_height;
	int             t_height, t_width;
	int             str_x, str_y;
	char            title_text [TITLE_SIZE];
	char            select_text [TITLE_SIZE];
	Text            *text;
	SWin            *scroll;
	Buf             *buf;
	MWin            *mwin;
	int             stat;
	int		select;
	Time		click_time;
	Time		old_click;
} LBox;

FCT (void, ClearListBox, () );
FCT (void, ConfigListBox, (int width, int height) );
FCT (int, DoubleClick, (Time current_t, Time * old_t) );
FCT (int, ExposeListBox, (XEvent *ev) );
FCT (void, FillList, (char *str) );
FCT (void, InitListBox, () );
FCT (void, KillBufferInList, () );
FCT (void, OpenFilesInListBox, () );
FCT (char *, SelectFromListBox, (char *msg) );

#endif /* _LIST_BOX_H_ */
