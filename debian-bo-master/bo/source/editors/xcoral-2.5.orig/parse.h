/* ########################################################################

				parse.h

   File: parse.h
   Path: /home/fournigault/c/X11/xcoral-2.31/parse.h
   Description: 
   Created: Fri Jan 27 11:24:29 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:24:31 MET 1995
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


#ifndef _PARSE_H_
#define _PARSE_H_

#include "proto_decl.h"

FCT (unsigned long, GetOpBD, () );
FCT (unsigned int, GetOpBW, () );
FCT (unsigned long, GetOpColor, (int type) );
FCT (char *, GetOpFilename, () );
FCT (XFontStruct *, GetOpFont, (int type) );
FCT (int, GetOpGeo, (int type) );
FCT (void, GetUserDatabase, () );
FCT (void, MergeOptions, () );
FCT (int, OpVerbose, () );
FCT (void, ParseOpenDisp, (int *argc, char *argv[]) );
FCT (unsigned long, PixelValue, (Display *dpy, char *str, int *result) );
FCT (int, UseColor, () );


#endif /* _PARSE_H_ */
