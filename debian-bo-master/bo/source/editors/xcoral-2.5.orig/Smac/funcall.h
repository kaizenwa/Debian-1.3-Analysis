/* ########################################################################

			       funcall.h

   File: funcall.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/funcall.h
   Description: 
   Created: Tue Feb 21 12:54:05 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:54:06 MET 1995
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



#ifndef _funcall_h
#define _funcall_h

#include "Function.h"
#include "list.h"

extern FCT( Instruction *, make_funcall,(List *) );    
FCT( int, bad_number_of_args,(Function * fct, int nbrearg)	);

#endif
