/* ########################################################################

			      bm_search.h

   File: bm_search.h
   Path: /home/fournigault/c/X11/xcoral-2.31/bm_search.h
   Description: 
   Created: Fri Jan 27 10:43:47 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:43:48 MET 1995
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


#ifndef _BM_SEARCH_H_
#define _BM_SEARCH_H_

#include "proto_decl.h"

#define FORWARD	1
#define BACKWARD	-1
#define GLOBAL	1
#define QUERY	2
#define S_MENU	0
#define S_KBD	1

FCT (void, BackwardSearch, (Text *text) );
FCT (int, BmSearch, (char *buf, char *str, int buf_len, int dir ) );
FCT (void, DeleteStrings, () );
FCT (void, ForwardSearch, (Text *text) );
FCT (void, GlobalReplace, (Text *text) );
FCT (int, HandleBackward, (char *str, Text *text) );
FCT (int, HandleForward, (char *str, Text *text) );
FCT (int, HandleGlobal, (Text *text) );
FCT (void, MenuBackwardSearch, (Text *text) );
FCT (void, MenuForwardSearch, (Text *text) );
FCT (void, MenuGlobalReplace, (Text *text) );
FCT (void, MenuQueryReplace, (Text *text) );
FCT (void, QueryReplace, (Text *text) );
FCT (void, ResetPatterns, () );
FCT (void, ResetSearchString, () );
FCT (void, SearchInMan, (Text *text) );
FCT (void, SetPatterns, (char *old, char *new) );
FCT (void, RE_MenuForwardSearch, (Text *text) );
FCT (void, RE_MenuBackwardSearch, (Text *text) );
FCT (void, RE_MenuReplace, (Text *text) );
FCT (int, RE_Search, (char* regex, char *buffer, int size, int direction) );
FCT (int, RE_Match, (int n, int beginning ) );
FCT (void, UpdateRegs, (int n) );

#endif /* _BM_SEARCH_H_ */
