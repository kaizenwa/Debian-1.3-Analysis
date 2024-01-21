/* ########################################################################

				 memo.h

   File: memo.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/memo.h
   Description: 
   Created: Tue Feb 21 12:58:17 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:58:18 MET 1995
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



#ifndef _memo_h
#define _memo_h

#include "list.h"
#include "fctdecl.h"

extern Type * the_abstract_type();

extern FCT(Type *, the_typed_var,(char ** var)	);
extern FCT(void, preserve_list,()			);
extern FCT(void, array_dims,()			);
extern FCT(void, array_dim,()				);
     
#endif
