/* ########################################################################

			     browser_eve.h

   File: browser_eve.h
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_eve.h
   Description: 
   Created: Fri Jan 27 10:46:17 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:46:18 MET 1995
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


#ifndef _BROWSER_EVE_H_
#define _BROWSER_EVE_H_

#include "main_text.h"

#include "proto_decl.h"

FCT (int , ExposeBrowser, (XEvent *ev) );
FCT (int , ButtonBrowser, (XButtonEvent *ev) );
FCT (int , ButtonBrowser, (XButtonEvent *ev) );
FCT (int , GoodSuffix, (char *s) );
FCT (int , KeyPressInBrowser, (XKeyEvent *ev) );
FCT (void , RefreshBrowserInfos, () );

#endif /* _BROWSER_EVE_H_ */
