/* ########################################################################

			       justify.h

   File: justify.h
   Path: /home/fournigault/c/X11/xcoral-2.31/justify.h
   Description: 
   Created: Tue Jan 31 11:33:45 MET 1995
   Author: Lionel Fournigault
   Modified: Tue Jan 31 11:33:46 MET 1995
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


#ifndef _justify_h
#define _justify_h

#include <X11/Xlib.h>
#include "main_buffer.h"

FCT(int, FillManText,(XFontStruct * font, int width, Buf * textbuf,
		      Buf * indexbuf, Buf * tblbuf,
		      int * textnlig, int * indexnlig, int * tblnlig)	);
FCT(int, GetLineManFromIndex,(int index_line)				);
FCT(int, GetLineManFromToc,(int index_line)				);

FCT(int, FirstIndexLineBeginningWith,(int code)				);

#endif
