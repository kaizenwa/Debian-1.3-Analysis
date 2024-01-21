/* ########################################################################

			      chars_cmds.h

   File: chars_cmds.h
   Path: /home/fournigault/c/X11/xcoral-2.31/chars_cmds.h
   Description: 
   Created: Fri Jan 27 10:52:13 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:52:14 MET 1995
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


#ifndef _CHARS_CMDS_H_
#define _CHARS_CMDS_H_

#include "proto_decl.h"

FCT (void, CheckModifAndFilename, (Text *text) );
FCT (void, Control_D, (Text *text) );
FCT (void, Control_K, (Text *text, int n) );
FCT (void, Control_Y, (Text *text, int i) );
FCT (void, DeleteBytesFromCutBuffer, (Text *text) );
FCT (void, DisplayKillBuffer, (Text *text) );
FCT (void, GetBytesFromCutBuffer, (Text *text) );
FCT (void, InsertLines, (Text *text, char *s, int nbytes, int lines) );

FCT (void, ClassHeader, (Text *text) );
FCT (void, FunctionHeader, (Text *text) );
FCT (void, MethodHeader, (Text *text) );
FCT (void, IncludeHeader, (Text *text) );
FCT (void, LatexMacros, (Text *text) );
FCT (void, HtmlMacros, (Text *text) );
FCT (void, MiscCommands, (Text *text) );
FCT (void, UserCommands, (Text *text) );

FCT (void, OpenSpace, (Text *text) );
FCT (void, SmallTime, (long t) );
FCT (void, StoreBytesInCutBuffer, (Text *text, int x, int y) );
FCT (void, f_delete, (Text *text) );
FCT (void, f_impc, (Text *text, int c) );
FCT (void, f_return, (Text *text) );
FCT (void, f_tab, (Text *text) );
FCT (void, QuotedChar, (Text *text) );


#endif /* _CHARS_CMDS_H_ */
