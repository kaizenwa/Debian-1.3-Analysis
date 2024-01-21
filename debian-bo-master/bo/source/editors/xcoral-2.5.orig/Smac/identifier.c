/* ########################################################################

			      identifier.c

   File: identifier.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/identifier.c
   Description: 
   Created: Tue Feb 21 12:55:01 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:55:01 MET 1995
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



#include <stdio.h>

#include "identifier.h"
#include "Identifier.h"
#include "function.h"
#include "list.h"
#include "error.h"

/* Retourne ce qui correspond au nom donne : une variable locale,
   globale ou une fonction */

Object make_identifier(l)
     List * l;
{
  char * name = (char *) l->info;

  free(l);
    
  {
    Object result;
    
    if ((result = HashTable__Search(DynamicVarsHashTable, (Object) name)) != 0) {
      free(name);
      return result;
    }
  }

  {
    Identifier * id;
    
    if ((id = find_identifier(name)) != 0) {
      free(name);
      if (Identifier__FunctionDef(id))
	return (Object) Identifier__FunctionDef(id);
      return (Object) Identifier__GlobalVar(id);
    }

    sprintf(err_msg, "%s unknown", name);
    free(name);
    Error(err_msg);

    return 0;			/* pour le compilo */
  }
}
