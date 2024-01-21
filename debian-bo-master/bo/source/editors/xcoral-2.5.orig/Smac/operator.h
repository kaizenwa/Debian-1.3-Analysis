/* ########################################################################

			       operator.h

   File: operator.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/operator.h
   Description: 
   Created: Tue Feb 21 12:59:05 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:59:06 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

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



#ifndef _operator_h
#define _operator_h

#include "Type.h"
#include "Function.h"
#include "fctdecl.h"

FCT (Function *, find_binary_oper,(char *, Type *, Type *, Type **)   );
FCT ( Object,(FCT2(*,find_modifbinary_oper,(char *, Type *, Type *, Type **))),
     (Object, Object, int));

#endif
