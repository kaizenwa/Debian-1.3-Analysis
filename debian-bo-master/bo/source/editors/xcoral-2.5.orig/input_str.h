/* ########################################################################

			      input_str.h

   File: input_str.h
   Path: /home/fournigault/c/X11/xcoral-2.31/input_str.h
   Description: 
   Created: Fri Jan 27 11:10:14 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:10:15 MET 1995
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


#ifndef _INPUT_STR_H_
#define _INPUT_STR_H_

#include "proto_decl.h"

FCT (char *, GetString, (Text *text, char *prompt, int reply) );
FCT (void, FillMiniBuffer, (char *str) );
FCT (void, EmptyMiniBuffer, () );
FCT (void, InitInputString, () );
FCT (void, InputStrAbort, () );
FCT (void, InputStrPrevious, (Text *text) );
FCT (void, InputStrReturn, (Text *text) );
FCT (void, InputStrTab, (Text *text) );
FCT (char *, ExpandTildeName, (char *name) );
FCT (char *, InputString, (Window parent, GC gc, XFontStruct *font, char *prompt, int reply) );
FCT (void, SetCancelButton, () );
FCT (void, SetOkButton, () );
FCT (void, SetMBcontext, (int cont) );

#endif /* _INPUT_STR_H_ */
