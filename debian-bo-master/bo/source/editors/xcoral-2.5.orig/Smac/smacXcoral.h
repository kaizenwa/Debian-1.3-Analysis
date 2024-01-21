/* ########################################################################

			      smacXcoral.h

   File: smacXcoral.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/smacXcoral.h
   Description: 
   Created: Tue Feb 21 13:01:17 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:01:17 MET 1995
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



#ifndef _smacXcoral_h
#define _smacXcoral_h

#include "fctdecl.h"

extern char * i_buffer_courant;
extern char * i_minibuffer;
extern int i_curseur, i_curseur_sup;

extern FCT(	char *, ie_load_file,(char * buff, char * filename)	);
extern FCT(	char *, ie_eval_region,(char * buff)			);
extern FCT(	char *, ie_eval_expression,(char * buff, char * expr)	);
extern FCT(	char *, ie_call_function,(char *, char *, int, int *)	);

extern FCT(	char *, ie_data_list,(int)				);
extern FCT(	char *, ie_function_list,(int)				);

extern FCT(	void, Init_Smac_Xcoral,()				);

#endif
