/* ########################################################################

			       mark_reg.h

   File: mark_reg.h
   Path: /home/fournigault/c/X11/xcoral-2.31/mark_reg.h
   Description: 
   Created: Fri Jan 27 11:20:20 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:20:22 MET 1995
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


#ifndef _MARK_REG_H_
#define _MARK_REG_H_

#include "proto_decl.h"

#define KILLREGION	0
#define COPYREGION	1

FCT (void, CopyRegion, (Text *text) );
FCT (void, EvalExpressionFromKey, (Text *text) );
FCT (void, EvalExpressionFromMenu, (Text *text) );
FCT (void, EvalRegion, (Text *text) );
FCT (void, ExchangePointMark, (Text *text) );
FCT (void, GotoTheMark, (Text *text) );
FCT (void, IndentRegion, (Text *text) );
FCT (void, KillRegion, (Text *text) );
FCT (void, PasteRegion, (Text *text) );
FCT (void, ResetMark, (Text *text) );
FCT (void, RestoreMark, (Text *text) );
FCT (void, SaveMark, (Text *text) );
FCT (void, SetMark, (Text *text) );
FCT (int, UpdateMark, (Text *text, int len, int lines) );
FCT (void, UpdateMarkPos, (Text *text) );
FCT (void, ColorRegion, (Text *text) );
FCT (void, ColorBuffer, (Text *text) );

#endif /* _MARK_REG_H_ */
