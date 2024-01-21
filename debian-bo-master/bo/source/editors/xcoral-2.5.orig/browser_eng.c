/* ########################################################################

			     browser_eng.c

   File: browser_eng.c
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_eng.c
   Description: 
   Created: Fri Jan 27 10:44:37 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 10:44:38 MET 1995
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
#include "class_dict.h"
#include "browser_eng.h"
#include "browser_pars.h"
#include "browser_util.h"
#include <string.h>
#include <stdio.h>


/*------------------------------------------------------------------------------
*/
StringTable* get_files_list () {
  FileRec*     current_file;
  StringTable* result;
  char**       current_entry;
  int          index;
  int          x_size;

  x_size = sizeof(char*) * (file_count + 1);
  result = (StringTable*) xmalloc(x_size);
  if (result != Null) {
    current_entry = (char**) result;
    for (index = 0; index < FILE_DICT_SIZE; index++) {
      current_file = file_dict[index];
      while(current_file != Null) {
        (*current_entry) = current_file->_name;
	    current_entry++;
	    current_file = current_file->_next;
      }
    }
    (*current_entry) = Null;
    qsort(result, file_count, sizeof(char*), sort_util);
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
StringTable* get_classes_list () {
  ClassRec*    current_class;
  StringTable* result;
  char**       current_entry;
  int          index;
  int          x_size;
  char*        infos;

  x_size = sizeof(char*) * (class_count + 1);
  result = (StringTable*) xmalloc(x_size);
  if (result != Null) {
    current_entry = (char**) result;
    for (index = 0; index < CLASS_DICT_SIZE; index++) {
      current_class = class_dict[index];
      while(current_class != Null) {
        (*current_entry) = current_class->_name;
	    infos = (*current_entry) - CLASS_PLENGTH + 1;
	    if (current_class->_decl_file != Null)
	      *infos = 'd';
	    else
	      *infos = '?';
	    current_entry++;
	    current_class = current_class->_next;
      }
    }
    (*current_entry) = Null;
    qsort(result, class_count, sizeof(char*), sort_util);
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
StringTable* get_parents_list (class_name)
     char* class_name;
{
  StringTable* result;
  char**       current_entry;
  ClassRec*    current_class;
  ClassRec*    parent_class;
  ParentRec*   current_parent;
  int          x_size;
  char*        infos;

  result = Null; 
  current_class = find_class(class_name);
  if (current_class != Null) {
    x_size = sizeof(char*) * (current_class->_parents_count + 1);
    result = (StringTable*) xmalloc(x_size);
    if (result != Null) {
      current_entry  = (char**) result;
      current_parent = current_class->_parents_list;
      while (current_parent != Null) {
	    (*current_entry) = current_parent->_name;
	    infos = (*current_entry) - PARENT_PLENGTH + 1;
	    parent_class = find_class(*current_entry);
	    if ((parent_class != Null) && (parent_class->_decl_file != Null))
	      *infos = 'd';
	    else
	      *infos = '?';
	    current_entry++;
	    current_parent = current_parent->_next;
      }
      (*current_entry) = Null;
      qsort(result, current_class->_parents_count, sizeof(char*), sort_util);
    }
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
StringTable* get_sons_list(class_name)
    char* class_name;
{
  int          index;
  int          marked_count;
  ClassRec*    marked_list;
  ClassRec*    current_class;
  ParentRec*   current_parent;
  StringTable* result;
  char**       current_entry;
  int          x_size;
  char*        infos;

  marked_list  = Null;
  marked_count = 0;
  for (index = 0; index < CLASS_DICT_SIZE; index++) {
    current_class = class_dict[index];
    while (current_class != Null) {
      current_parent = current_class->_parents_list;
      while (current_parent != Null) {
        if (strcmp(current_parent->_name, class_name) == 0) {
          current_class->_next_marked = marked_list;
          marked_list                 = current_class;
          marked_count++;
        }
        current_parent = current_parent->_next;
      }
      current_class = current_class->_next;
    }
  }
  x_size = sizeof(char*) * (marked_count + 1);
  result = (StringTable*) xmalloc(x_size);
  if (result != Null) {
    current_entry = (char**) result;
    while (marked_list != Null) {
      (*current_entry) = marked_list->_name;
	  infos = (*current_entry) - CLASS_PLENGTH + 1;
	  if (marked_list->_decl_file != Null)
	    *infos = 'd';
	  else
	    *infos = '?';
	  current_entry++;
	  marked_list = marked_list->_next_marked;
    }
    (*current_entry) = Null;
    qsort(result, marked_count, sizeof(char*), sort_util);
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
static MethodRec* marked_list;
static int        marked_count;

#define END_MARKED_LIST ((MethodRec*) 0x00000001)


int sort_method(i,j)
  char** i;
  char** j;
{
  char* ti = *i;
  char* tj = *j;
  char* par_posit;
  int   result;

  if ((**i) == '~')
    ti = (*i) + 1;
  if ((**j) == '~')
    tj = (*j) + 1;
  par_posit = strchr(ti, '(');
  result    = strncmp(ti, tj, (par_posit - ti));
  if (result == 0) {
    if ((**i) == '~')
      result = 1;
    else if ((**j) == '~')
      result = -1;
    else {
      result = strcmp(*i, *j);
      if (result == 0) {
        if (*((*i) - METHOD_PLENGTH + 2) > *((*j) - METHOD_PLENGTH + 2))
          result = -1;
        else
          result = 1;
      }
    }
  }
  return(result);
}


void post_compute(result)
    StringTable* result;
{
  char**  current_entry;
  char*   current_depth;
  char*   infos;
  char*   virtual_method_name = "";
  int     virtual_flag;

  virtual_flag  = False;
  current_entry = (char**) result;
  while ((*current_entry) != Null) {
    infos = (*current_entry) - METHOD_PLENGTH + 1;
    if ((*infos) == 'V') {
      if (strcmp(virtual_method_name, (*current_entry)) == 0)
        (*infos) = 'v';
      else {
        virtual_flag        = True;
        virtual_method_name = (*current_entry);
      }
    }
    else if (virtual_flag == True) {
      if (strcmp(virtual_method_name, (*current_entry)) == 0)
        (*infos) = 'v';
      else
        virtual_flag = False;
    }
    current_depth = infos + 1;
	if ((*current_depth) == 0)
	  (*current_depth) = 'L';
	else {
	  (*current_depth) = '0' + (*current_depth);
	  (*current_depth) = (((*current_depth) <= '9') ? (*current_depth) : '9');
	}
    current_entry++;
  }
}


void get_inherit_list(current_class, max_scope, depth)
     ClassRec* current_class;
     ScopeType max_scope;
     int       depth;
{
  MethodRec*  current_method;
  ParentRec*  current_parent;

  current_method = current_class->_methods_list;
  while(current_method != Null) {
    if (   (current_method->_scope <= max_scope)
        && (current_method->_next_marked == Null)) {
      *(current_method->_name - METHOD_PLENGTH + 2) = depth;
	  current_method->_next_marked = marked_list;
	  marked_list                  = current_method;
	  marked_count++;
    }
    current_method = current_method->_next;
  }
  current_parent = current_class->_parents_list;
  while (current_parent != Null) {
    if (depth < 255)
      depth++;
    if (current_parent->_scope < PRIVATE_SCOPE) {
	    current_class = find_class(current_parent->_name);
	    if (current_class != Null)
	      get_inherit_list(current_class, PROTECTED_SCOPE, depth);
    }
    current_parent = current_parent->_next;
  }
}


StringTable* get_methods_list(class_name)
	char* class_name;
{
  ClassRec*    current_class;
  MethodRec*   current_method;
  StringTable* result;
  char**       current_entry;
  int          x_size;
  char*        infos;

  result        = Null;
  marked_list   = END_MARKED_LIST;
  marked_count  = 0;
  current_class = find_class(class_name);
  if (current_class != Null) {
    get_inherit_list(current_class, PRIVATE_SCOPE, 0);
    if (marked_list != END_MARKED_LIST)  {
      x_size = sizeof(char*) * (marked_count + 1);
      result = (StringTable*) xmalloc(x_size);
	  if (result != Null) {
        current_entry  = (char**) result;
        current_method = marked_list;
        while(current_method != END_MARKED_LIST) {
          (*current_entry) = current_method->_name;
          infos = (*current_entry) - METHOD_PLENGTH + 1;
          if (current_method->_decl_file != Null)
            *(infos + 2) = 'd';
          else
            *(infos + 2) = '?';
          if (current_method->_impl_file != Null)
            *(infos + 3) = 'i';
          else
            *(infos + 3) = '?';
          if (strncmp(current_method->_class_name, 
                      current_method->_name,
                      strlen(current_method->_class_name)) == 0)
            *infos = 'c';
          else if ((*(current_method->_name)) == '~')
            *infos = 'd';
          else if (current_method->_virtual_decl == True)
            *infos = 'V';
          else
            *infos = ' ';
	      current_entry++;
	      marked_list = marked_list->_next_marked;
	      current_method->_next_marked = Null;
	      current_method = marked_list;
        }
        (*current_entry) = Null;
        qsort(result, marked_count, sizeof(char*), sort_method);
        post_compute(result);
      }
    }
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
Position* get_class_decl (class_name)
    char* class_name;
{
  Position*     result;
  ClassRec*     current_class;
  int           x_size;

  result = Null;
  current_class = find_class(class_name);
  x_size = sizeof(Position);
  if (   (current_class != Null)
      && (current_class->_decl_file != Null)
      && ((result = (Position*) xmalloc(x_size)) != Null)) {
    result->file_name   = current_class->_decl_file->_name;
    result->line_number = current_class->_decl_line;
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
char* get_method_class(method_name)
    char* method_name;
{
  MethodRec* current_method;
  int        x_size;

  x_size = sizeof(MethodRec);
  current_method = (MethodRec*) (method_name - x_size - METHOD_PLENGTH);
  return(current_method->_class_name);
}

  
/*------------------------------------------------------------------------------
*/
Position* get_method_decl (class_name, method_name)
    char* class_name;
    char* method_name;
{
  Position*     result;
  ClassRec*     current_class;
  MethodRec*    current_method;
  int           x_size;
  
  result = Null;
  current_class = find_class(class_name);
  if (current_class != Null) {
    current_method = current_class->_methods_list;
    while (current_method != Null) {
      if (strcmp(current_method->_name, method_name) == 0)
        break;
      current_method = current_method->_next;
    }
    x_size = sizeof(Position);
    if (   (current_method != Null)
        && (current_method->_decl_file != Null)
        && ((result = (Position*) xmalloc(x_size)) != Null)) {
      result->file_name   = current_method->_decl_file->_name;
      result->line_number = current_method->_decl_line;
    }
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
Position* get_method_impl (class_name, method_name)
     char* class_name;
     char* method_name;
{
  Position*     result;
  ClassRec*     current_class;
  MethodRec*    current_method;
  int           x_size;

  result = Null;
  current_class = find_class(class_name);
  if (current_class != Null) {
    current_method = current_class->_methods_list;
    while (current_method != Null) {
      if (strcmp(current_method->_name, method_name) == 0)
        break;
      current_method = current_method->_next;
    }
    x_size = sizeof(Position);
    if (   (current_method != Null)
        && (current_method->_impl_file != Null)
        && ((result = (Position*) xmalloc(x_size)) != Null)) {
      result->file_name   = current_method->_impl_file->_name;
      result->line_number = current_method->_impl_line;
    }
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
StringTable* get_procs_list () {
  ProcRec*     current_proc;
  StringTable* result;
  char**       current_entry;
  int          index;
  int          x_size;
  char*        infos;

  x_size = sizeof(char*) * (proc_count + 1);
  result = (StringTable*) xmalloc(x_size);
  if (result != Null) {
    current_entry = (char**) result;
    for (index = 0; index < PROC_DICT_SIZE; index++) {
      current_proc = proc_dict[index];
      while(current_proc != Null) {
        (*current_entry) = current_proc->_name;
	    infos = (*current_entry) - PROC_PLENGTH + 1;
	    switch (current_proc->_proc_type) {
	      case CPLUS_PROC :
	        *(infos + 1) = '+';
	        *(infos + 2) = '+';
	      case C_PROC :
	        *infos = 'c';
	        break;
	      default :
	        *infos = '?';
	    }
		current_entry++;
		current_proc = current_proc->_next;
      }
    }
    (*current_entry) = Null;
    qsort(result, proc_count, sizeof(char*), sort_util);
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
Position* get_proc_impl (proc_name)
    char* proc_name;
{
  Position*     result;
  ProcRec*      current_proc;
  int           x_size;
 
  result = Null;
  current_proc = find_proc(proc_name);
  x_size = sizeof(Position);
  if (   (current_proc != Null)
      && (current_proc->_impl_file != Null)
      && ((result = (Position*) xmalloc(x_size)) != Null)) {
    result->file_name   = current_proc->_impl_file->_name;
    result->line_number = current_proc->_impl_line;
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
void parse_file(file_name)
     char* file_name;
{
  parsed_file = find_file(file_name);
  if (parsed_file != Null) {
    proc_eraze_file(file_name);
    class_eraze_file(file_name);
  }
  else
    parsed_file = create_file(file_name);
  if (parsed_file != Null) {
    if (browser_yyparse(file_name) == 1) {
      if ( OpVerbose () == True )
        fprintf(stderr, "warning >>  file \"%s\" will be ignored\n\n", file_name);
      proc_eraze_file(file_name);
      class_eraze_file(file_name);
      remove_file(file_name);
    }
    garbage_proc();
    garbage_class();
  }
  parsed_file = Null;
  class_cache = Null;
}


/*------------------------------------------------------------------------------
*/
void delete_file(file_name)
     char* file_name;
{
  FileRec* current_file;

  current_file = find_file(file_name);
  if (current_file != Null) {
    proc_eraze_file(file_name);
    class_eraze_file(file_name);
    garbage_proc();
    garbage_class();
  }
  remove_file(file_name);
}


/*------------------------------------------------------------------------------
*/
void init_browser() {
  init_file();
  init_proc();
  init_class();
}
