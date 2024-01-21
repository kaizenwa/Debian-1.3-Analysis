/* ########################################################################

			       get_file.h

   File: get_file.h
   Path: /home/fournigault/c/X11/xcoral-2.31/get_file.h
   Description: 
   Created: Fri Jan 27 11:04:10 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:04:12 MET 1995
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


#ifndef _GET_FILE_H_
#define _GET_FILE_H_

#include "proto_decl.h"

FCT (void, KbdInsertFile, (Text *text) );
FCT (void, KbdReadFile, (Text *text) );
FCT (void, KbdSaveFile, (Text *text) );
FCT (void, KbdWriteFile, (Text *text) );
FCT (void, KillBuffer, (Text *text) );
FCT (void, LoadAndEvalFile, (Text *text) );
FCT (int, LoadFile, (Text *text, char *s, int type) );
FCT (void, MenuGotoLine, (Text *text) );
FCT (void, MenuInsertFile, (Text *text) );
FCT (void, MenuNewFile, (Text *text) );
FCT (void, MenuReadFile, (Text *text) );
FCT (void, MenuSaveFile, (Text *text) );
FCT (void, MenuWriteFile, (Text *text) );
FCT (int, SaveCurrentBuffer, (Text *text, int from) );
FCT (void, SetDirAndFilename, (Text *text, char *name) );
FCT (void, WriteFile, (Text *text) );
FCT (void, SetWindowName, (Text *text) );
FCT (Text *, update_cwd,(int)	);
#endif /* _GET_FILE_H_ */
