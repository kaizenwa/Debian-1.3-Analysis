/* ########################################################################

			      class_dict.c

   File: class_dict.c
   Path: /home/fournigault/c/X11/xcoral-2.31/class_dict.c
   Description: 
   Created: Fri Jan 27 10:53:29 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 10:53:30 MET 1995
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


#include "result_types.h"
#include "file_dict.h"
#include "class_dict.h"
#include "browser_util.h"
#include <string.h>
#include <stdio.h>


/*------------------------------------------------------------------------------
//                         Le dictionnaire des classes
//------------------------------------------------------------------------------
*/

ClassRec*  class_dict[CLASS_DICT_SIZE];

int        class_count = 0;


/*------------------------------------------------------------------------------
*/
ClassRec* create_class (class_name)
  char* class_name;
{
	ClassRec**  head;
  ClassRec*   current_class;
  int         x_size;

	get_head_Rec(class_name, class_dict, CLASS_DICT_SIZE, head);
  search_Rec(class_name, ClassRec, head, current_class);
  if (current_class == Null) {
    x_size = sizeof(ClassRec) + CLASS_PLENGTH + strlen(class_name) + 1;
    current_class = (ClassRec*) xmalloc(x_size);
    if (current_class != Null) {
      create_Rec(class_name, ClassRec, head, current_class, CLASS_PREFIX, CLASS_PLENGTH);
      current_class->_decl_file     = Null;
	    current_class->_decl_line     = 0;
	    current_class->_parents_list  = Null;
	    current_class->_parents_count = 0;
	    current_class->_methods_list  = Null;
	    current_class->_next_marked   = Null;
		 	class_count++;
		}
	}
  return(current_class);
}


/*------------------------------------------------------------------------------
*/
ClassRec* find_class(class_name)
  char* class_name;
{
	ClassRec**  head;
  ClassRec*   current_class;

	get_head_Rec(class_name, class_dict, CLASS_DICT_SIZE, head);
  search_Rec(class_name, ClassRec, head, current_class);
  return(current_class);
}


/*------------------------------------------------------------------------------
*/
typedef long Bits;

#define LAST_BIT  ((sizeof(Bits) * 8) - 1)

static Bits erazed_bits[(CLASS_DICT_SIZE + LAST_BIT) / (LAST_BIT + 1)];


void class_eraze_file(file_name)
    char* file_name;
{
  Bits*      erazed_slice;
  FileRec*   current_file;
  ClassRec*  current_class;
  MethodRec* current_method;
  ParentRec* current_parent;
  int        index;

  current_file = find_file(file_name);
  if (current_file != Null) {
    erazed_slice = erazed_bits;
    for (index = 0; index < CLASS_DICT_SIZE; index++) {
      current_class = class_dict[index];
      while (current_class != Null) {
        if (current_class->_decl_file == current_file) {
          current_class->_decl_file     = Null;
          current_class->_decl_line     = 0;
          current_class->_parents_count = 0;
          current_parent = current_class->_parents_list;
          while (current_parent != Null) {
            current_parent->_scope = UNKNOWN_SCOPE;
            current_parent         = current_parent->_next;
          }
          (*erazed_slice) |= ((0x00000001L) << (index & LAST_BIT));
        }
        current_method = current_class->_methods_list;
        while (current_method != Null) {
          if (current_method->_decl_file == current_file) {
            current_method->_decl_file = Null;
            current_method->_decl_line = 0;
            (*erazed_slice) |= ((0x00000001L) << (index & LAST_BIT));
          }
          if (current_method->_impl_file == current_file) {
            current_method->_impl_file = Null;
            current_method->_impl_line = 0;
            (*erazed_slice) |= ((0x00000001L) << (index & LAST_BIT));
          }
          current_method = current_method->_next;
        }
	      current_class = current_class->_next;
      }
      if ((index & LAST_BIT) == LAST_BIT)
        erazed_slice++;
    }
  }  
}


/*------------------------------------------------------------------------------
*/
void garbage_class()
{
  Bits*      erazed_slice;
  ClassRec*  previous_class;
  ClassRec*  current_class;
  ClassRec*  next_class;
  MethodRec* previous_method;
  MethodRec* current_method;
  MethodRec* next_method;
  ParentRec* previous_parent;
  ParentRec* current_parent;
  ParentRec* next_parent;
  int        index;

  erazed_slice = erazed_bits;
  for (index = 0; index < CLASS_DICT_SIZE; index++) {
    if ((*erazed_slice) == 0) {
      index += LAST_BIT;
      erazed_slice++;
    }
    else {
      if (((*erazed_slice) & ((0x00000001L) << (index & LAST_BIT))) != 0) {
        previous_class = Null;
        current_class  = class_dict[index];
        while (current_class != Null) {
          previous_method = Null;
          current_method  = current_class->_methods_list;
          while (current_method != Null) {
            if (   (current_method->_decl_file == Null)
                && (current_method->_impl_file == Null)) {
              next_method = current_method->_next;
              if (previous_method == Null)
                current_class->_methods_list = next_method;
              else
                previous_method->_next       = next_method;
              free(current_method);
              current_method = next_method;
            }
            else {
              previous_method = current_method;
              current_method  = current_method->_next;
            }
          }
          previous_parent = Null;
          current_parent  = current_class->_parents_list;
          while (current_parent != Null) {
            if (current_parent->_scope == UNKNOWN_SCOPE) {
              next_parent = current_parent->_next;
              if (previous_parent == Null)
                current_class->_parents_list = next_parent;
              else
                previous_parent->_next       = next_parent;
              free(current_parent);
              current_parent = next_parent;
            }
            else {
              previous_parent = current_parent;
              current_parent  = current_parent->_next;
            }
          }
          if (   (current_class->_methods_list == Null)
              && (current_class->_decl_file    == Null)) {
            next_class = current_class->_next;
            if (previous_class == Null)
              class_dict[index]     = next_class;
            else
              previous_class->_next = next_class;
            --class_count;
            free(current_class);
            current_class = next_class;
          }
          else {
            previous_class = current_class;
   	        current_class  = current_class->_next;
  	      }
        }
      }
      if ((index & LAST_BIT) == LAST_BIT) {
        *erazed_slice = 0;
        erazed_slice++;
      }
    }
  }
}


/*------------------------------------------------------------------------------
*/
void init_class() {
  Bits* erazed_slice;
  int   index;

  erazed_slice = erazed_bits;
  for (index = 0; index < CLASS_DICT_SIZE; index++) {
    if ((index & LAST_BIT) == LAST_BIT) {
      *erazed_slice = 0;
      erazed_slice++;
    }
    class_dict[index] = Null;
  }
}



