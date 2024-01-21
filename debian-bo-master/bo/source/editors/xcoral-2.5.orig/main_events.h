/* ########################################################################

			     main_events.h

   File: main_events.h
   Path: /home/fournigault/c/X11/xcoral-2.31/main_events.h
   Description: 
   Created: Fri Jan 27 11:17:46 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:17:47 MET 1995
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


#ifndef _MAIN_EVENTS_H_
#define _MAIN_EVENTS_H_

#include "proto_decl.h"

FCT (void, FreezeButtons, (Text *text) );
FCT (void, FreezeMenus, () );
FCT (int, IsFreezeMenus, () );
FCT (void, UnFreezeButtons, () );
FCT (void, UnFreezeMenus, () );
FCT (void, HandleButtonPress, () );
FCT (void, HandleConfigure, () );
FCT (void, HandleEnter, () );
FCT (void, HandleExpose, () );
FCT (void, HandleLeave, () );
FCT (void, InitEvent, () );
FCT (void, ResetFd, (int fd) );
FCT (void, SetFd, (int fd) );
FCT (void, WaitButtonRelease, (unsigned int button) );
FCT (void, WaitForEvent, () );
FCT (void, WaitForEvent, () );
FCT (void, WaitForMapped, (Window win, int istext) );
FCT (void, WaitForUnMapped, (Window win) );

#endif /* _MAIN_EVENTS_H_ */
