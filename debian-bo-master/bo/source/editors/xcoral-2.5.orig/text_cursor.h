/* ########################################################################

			     text_cursor.h

   File: text_cursor.h
   Path: /home/fournigault/c/X11/xcoral-2.31/text_cursor.h
   Description: 
   Created: Fri Jan 27 11:37:47 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:37:48 MET 1995
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


#ifndef _TEXT_CURSOR_H_
#define _TEXT_CURSOR_H_

#include "proto_decl.h"

FCT (void, BackwardChar, (Text *text) );
FCT (void, DownCursor, (Text *text) );
FCT (int, ForwardChar, (Text *text) );
FCT (void, FreezeTextCursor, (Text *text) );
FCT (void, GotoLeft, (Text *text) );
FCT (char *, HscrollString, (char *s, Text *text) );
FCT (void, MoveToBline, (Text *text) );
FCT (void, MoveToEline, (Text *text) );
FCT (int, MoveToXYinTextWindow, (Text *text, int x, int y) );
FCT (void, TextCursorOn, (Text *text) );
FCT (void, TextCursorOff, (Text *text) );
FCT (void, UnFreezeTextCursor, (Text *text) );
FCT (void, UpCursor, (Text *text) );

#endif /* _TEXT_CURSOR_H_ */
