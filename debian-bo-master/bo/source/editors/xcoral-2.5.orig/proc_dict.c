/* ########################################################################

			      proc_dict.c

   File: proc_dict.c
   Path: /home/fournigault/c/X11/xcoral-2.31/proc_dict.c
   Description: 
   Created: Fri Jan 27 11:25:07 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 11:25:08 MET 1995
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
#include "proc_dict.h"
#include "browser_util.h"
#include <string.h>
#include <stdio.h>

/*------------------------------------------------------------------------------
//                         Le dictionnaire des procedures
//------------------------------------------------------------------------------
*/

ProcRec* proc_dict[PROC_DICT_SIZE];

int proc_count = 0;


/*------------------------------------------------------------------------------
*/
ProcRec* create_proc(proc_name)
  char* proc_name;
{
  ProcRec**  head;
  ProcRec*   current_proc;
  int        x_size;
  
  get_head_Rec(proc_name, proc_dict, PROC_DICT_SIZE, head);
  search_Rec(proc_name, ProcRec, head, current_proc);
  if (current_proc == Null) {
    x_size = sizeof(ProcRec) + PROC_PLENGTH + strlen(proc_name)  + 1;
    current_proc = (ProcRec*) xmalloc(x_size);
    if (current_proc != Null) {
      create_Rec(proc_name, ProcRec, head, current_proc, PROC_PREFIX, PROC_PLENGTH);
      current_proc->_impl_file = Null;
      current_proc->_impl_line = 0;
      current_proc->_proc_type = UNKNOWN_PROC;
      proc_count++;
    }
  }
  return(current_proc);
}


/*------------------------------------------------------------------------------
*/
ProcRec* find_proc(proc_name)
  char* proc_name;
{
  ProcRec**  head;
  ProcRec*   current_proc;

  get_head_Rec(proc_name, proc_dict, PROC_DICT_SIZE, head);
  search_Rec(proc_name, ProcRec, head, current_proc);
  return(current_proc);
}


/*------------------------------------------------------------------------------
*/
typedef long Bits;

#define LAST_BIT  ((sizeof(Bits) * 8) - 1)

static Bits erazed_bits[(PROC_DICT_SIZE + LAST_BIT) / (LAST_BIT + 1)];


void proc_eraze_file(file_name)
    char* file_name;
{
  Bits*      erazed_slice;
  FileRec*   current_file;
  ProcRec*   current_proc;
  int        index;
  
  current_file = find_file(file_name);
  if (current_file != Null) {
    erazed_slice = erazed_bits;
    for (index = 0; index < PROC_DICT_SIZE; index++) {
      current_proc = proc_dict[index];
      while (current_proc != Null) {
        if (current_proc->_impl_file == current_file) {
          current_proc->_impl_file = Null;
          current_proc->_impl_line = 0;
          (*erazed_slice) |= ((0x00000001L) << (index & LAST_BIT));
        }
	    current_proc = current_proc->_next;
      }
      if ((index & LAST_BIT) == LAST_BIT)
        erazed_slice++;
    }
  }  
}


/*------------------------------------------------------------------------------
*/
void garbage_proc()
{
  Bits*     erazed_slice;
  ProcRec*  previous_proc;
  ProcRec*  current_proc;
  ProcRec*  next_proc;
  int       index;

  erazed_slice = erazed_bits;
  for (index = 0; index < PROC_DICT_SIZE; index++) {
    if ((*erazed_slice) == 0) {
      index += LAST_BIT;
      erazed_slice++;
    }
    else {
      if (((*erazed_slice) & ((0x00000001L) << (index & LAST_BIT))) != 0) {
        previous_proc = Null;
        current_proc = proc_dict[index];
        while (current_proc != Null) {
          if (current_proc->_impl_file == Null) {
            next_proc = current_proc->_next;
            if (previous_proc == Null)
              proc_dict[index] = next_proc;
            else
              previous_proc->_next = next_proc;
            --proc_count;
            free(current_proc);
            current_proc = next_proc;
          }
          else {
            previous_proc = current_proc;
	        current_proc  = current_proc->_next;
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
void init_proc() {
  Bits* erazed_slice;
  int   index;

  erazed_slice = erazed_bits;
  for (index = 0; index < PROC_DICT_SIZE; index++) {
    if ((index & LAST_BIT) == LAST_BIT) {
      *erazed_slice = 0;
      erazed_slice++;
    }
    proc_dict[index] = Null;
  }
}




