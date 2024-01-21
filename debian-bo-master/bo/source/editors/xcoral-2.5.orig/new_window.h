/* ########################################################################

			      new_window.h

   File: new_window.h
   Path: /home/fournigault/c/X11/xcoral-2.31/new_window.h
   Description: 
   Created: Fri Jan 27 11:21:59 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:22:00 MET 1995
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


#ifndef _NEW_WINDOW_H_
#define _NEW_WINDOW_H_

#include "proto_decl.h"

FCT (void, ConfigWindow, (EdWin *e, int width, int height) );
FCT (EdWin *, CreateWindow, () );
FCT (int, DeleteWindow, (Text *text) );
FCT (void , DisplayOpenFiles, (Text *text) );
FCT (void , InitIconPixmap, () );
FCT (void , Help, (Text *text) );
FCT (int, IsAlreadyLoad, (char *s, Text *from_text, Text **text) );
FCT (int, IsLastWindow, (int nb) );
FCT (void , NewWindow, (Text *text) );
FCT (void , Version, (Text *text) );

#endif /* _NEW_WINDOW_H_ */
