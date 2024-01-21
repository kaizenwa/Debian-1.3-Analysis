/* ########################################################################

				 page.h

   File: page.h
   Path: /home/fournigault/c/X11/xcoral-2.31/page.h
   Description: 
   Created: Fri Jan 27 11:23:18 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:23:19 MET 1995
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


#ifndef _PAGE_H_
#define _PAGE_H_

#include "proto_decl.h"

FCT (void, AtLineDisplayPage, (Text *text, int line) );
FCT (void, ClearLine, (Text *text, int i) );
FCT (void, ClearPage, (Text *text) );
FCT (void, ClipOff, (Text *text) );
FCT (void, ClipOn, (Text *text, int line) );
FCT (void, CurrentLineToMiddle, (Text *text) );
FCT (void, CurrentLineToTop, (Text *text) );
FCT (void, CurrentLineToTopFromMenu, (Text *text) );
FCT (char *, CurrentTextItem, (Text *text) );
FCT (void, ExposePage, (Region r, Text *text) );
FCT (void, FirstPage, (Text *text) );
FCT (void, FirstPageAndUpdateScroll, (Text *text) );
FCT (void, GotoEndOfBuf, (Text *text) );
FCT (void, GotoLineNumber, (Text *text, int n) );
FCT (void, LastPage, (Text *text) );
FCT (void, NextPage, (Text *text) );
FCT (void, PreviousPage, (Text *text) );
FCT (void, RefreshPage, (Text *text) );
FCT (void, RefreshPageAndUpdateScroll, (Text *text) );
FCT (void, RunScroll, (Text *text, int result) );
FCT (void, RunScrollAndUpdateItem, (Text *text, int select, int s_type) );
FCT (void, ScrollDownCont, (Text *text) );
FCT (void, ScrollUpCont, (Text *text) );
FCT (void, ScrollNLine, (int n, Text *text) );
FCT (int, SelectTextItem, (Text *text, int x, int y, int select) );
FCT (void, SetAndDisplayPage, (Text *text) );
FCT (void, SetCurrentLine, (Text *text) );
FCT (void, SetLinesTable, (Text *text) );
FCT (void, SetPosition, (Text *text) );
FCT (void, StorePosition, (Text *text) );
FCT (void, UpdatePage, (Text *text) );
FCT (void, UpdateTextItem, (Text *text, int n) );
FCT (void, WatchOff, (Window win) );
FCT (void, WatchOn, (Window win) );
FCT (void, klaxon, () );

#endif /* _PAGE_H_ */
