/* ########################################################################

			      Identifier.c

   File: Identifier.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Identifier.c
   Description: 
   Created: Tue Feb 21 10:56:22 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:56:23 MET 1995
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



#include "Identifier.h"

HashTable * IdentifierHashTable;
  
Identifier * Identifier__Identifier(name, functiondef, globalvar)
     char * name;
     Function * functiondef;
     GlobalVar * globalvar;
{
  Identifier * this = (Identifier *) Malloc(sizeof(Identifier));

  this->_name = name;
  this->_functiondef = functiondef;
  this->_globalvar = globalvar;

  HashTable__Add(IdentifierHashTable, (Object) name, (Object) this);
  
  return this;
}

/* Retourne l'identifier correspondant au nom, le cree si besoin */

Identifier * Get_Identifier(name)
     char * name;
{
  Identifier * result;

  if ((result = find_identifier(name)) != 0)
    return result;

  return Identifier__Identifier(name, 0, 0);
}

/**/

void Init_Identifier()
{
  IdentifierHashTable = HashTable__HashTable(30, 1);
}
