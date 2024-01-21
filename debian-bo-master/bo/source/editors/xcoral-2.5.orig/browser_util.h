/* ########################################################################

			     browser_util.h

   File: browser_util.h
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_util.h
   Description: 
   Created: Fri Jan 27 10:50:21 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 10:50:22 MET 1995
   Last maintained by: Dominique Leveque

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Dominique Leveque

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


#ifndef BROWSERUTIL_H
#define BROWSERUTIL_H


/*------------------------------------------------------------------------------
//       La macro utilisee pour l'acces au table de "hash code"
//------------------------------------------------------------------------------
*/
#define get_head_Rec(name, dict, dict_size, head)  {                   \
  char*     current_char;                                              \
  unsigned  g, h = 0;                                                  \
                                                                       \
  for (current_char = name; *current_char != '\0'; current_char++) {   \
    h = (h << 4) + (*current_char);                                    \
    if (g = h & 0xf0000000) {                                          \
      h = h ^ (g >> 24);                                               \
      h = h ^ g;                                                       \
    }                                                                  \
  }                                                                    \
  head = & (dict[h % dict_size]);                                      \
}


/*------------------------------------------------------------------------------
//       Les macros utilisees pour manipuler les list de "TypeRec"
//------------------------------------------------------------------------------
*/
#define create_Rec(name, TypeRec, head, current, PREFIX, PLENGTH)  {   \
   char* cpy_name;                                                     \
                                                                       \
   cpy_name       = ((char*) current) + sizeof(TypeRec) ;              \
   strcpy(cpy_name, PREFIX);                                           \
   cpy_name      += PLENGTH;                                           \
   current->_name = cpy_name;                                          \
   strcpy(cpy_name, name);                                             \
   current->_next = *head;                                             \
   *head          = current;                                           \
}


#define search_Rec(name, TypeRec, head, current)  {                    \
  current = *head;                                                     \
  while (current != Null) {                                            \
    if (strcmp(current->_name, name) == 0)                             \
      break;                                                           \
    else                                                               \
      current = current->_next;                                        \
  }                                                                    \
}


#define extract_Rec(name, TypeRec, head, current)  {                   \
  TypeRec* previous;                                                   \
                                                                       \
  previous = Null;                                                     \
  current  = *head;                                                    \
  while (current != Null) {                                            \
    if (strcmp(current->_name, name) == 0) {                           \
      if (previous == Null)                                            \
	    *head = current->_next;                                        \
      else                                                             \
	    previous->_next = current->_next;                              \
      break;                                                           \
    }                                                                  \
    previous = current;                                                \
    current  = current->_next;                                         \
  }                                                                    \
}


/*------------------------------------------------------------------------------
//                    Quelques types d'usage divers
//------------------------------------------------------------------------------
*/
#define False   ((0))
#define True    ((!False))


/*------------------------------------------------------------------------------
//                    Quelques procedures d'usage divers
//------------------------------------------------------------------------------
*/
extern char* xmalloc     (/* int size  */);

extern int sort_util     (/* char** i,
                             char** j  */);

#endif    /*  BROWSERUTIL_H  */

