/* ########################################################################

			       process.h

   File: process.h
   Path: /home/fournigault/c/X11/xcoral-2.31/process.h
   Description: 
   Created: Fri Jan 27 11:26:17 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:26:18 MET 1995
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


#ifndef _PROCESS_H_
#define _PROCESS_H_

#include "proto_decl.h"

FCT (void, InitShellMode, () );
FCT (void, KillShell, (Text *text) );
FCT (void, ReadFromShell, (Text *text) );
FCT (void, SetPosInShell, (Text *text) );
FCT (void, SetShellMode, (Text *text) );
FCT (void, shell_CtrC, (Text *text) );
FCT (void, shell_CtrD, (Text *text) );
FCT (void, shell_key, (Text *text) );
FCT (void, shell_return, (Text *text) );

#endif /* _PROCESS_H_ */
