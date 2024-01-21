/* ########################################################################

			     browser_pars.c

   File: browser_pars.c
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_pars.c
   Description: 
   Created: Fri Jan 27 10:48:44 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 10:48:45 MET 1995
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
#include "browser_pars.h"
#include "browser_util.h"
#include <string.h>
#include <stdio.h>


FileRec*  parsed_file = Null;

ClassRec* class_cache = Null;


/*------------------------------------------------------------------------------
*/
BrowserError add_proc_impl(proc_name, proc_type, impl_line)
    char*      proc_name;
    ProcType   proc_type;
    LineNumber impl_line;
{
  ProcRec* current_proc;
  
  current_proc = create_proc(proc_name);
  if (current_proc != Null) {
    if (current_proc->_impl_file == Null) {
      current_proc->_impl_file = parsed_file;
	  current_proc->_impl_line = impl_line;
	  current_proc->_proc_type = proc_type;
      return(NO_BERROR);
    }
    else {
	if ( OpVerbose () == True ) {
	      fprintf(stderr, "warning >>>>>>  Browser Error at line %d\n", impl_line);
	      fprintf(stderr, "warning    >>  Procedure \"%s\" already implemented at line %d in file %s\n",
	              proc_name,
	              current_proc->_impl_line,
	              current_proc->_impl_file->_name);
	}
    }
  }
  return(BERROR);
}


/*------------------------------------------------------------------------------
*/
BrowserError add_class_decl(class_name, decl_line)
    char*      class_name;
	LineNumber decl_line;
{
  ClassRec* current_class;

  if ((class_cache != Null) && (strcmp(class_cache->_name, class_name) == 0))
    current_class = class_cache;
  else {
    current_class = create_class(class_name);
    class_cache   = current_class;
  }
  if (current_class != Null) {
    if (current_class->_decl_file == Null) {
      current_class->_decl_file = parsed_file;
	  current_class->_decl_line = decl_line;
      return(NO_BERROR);
    }
    else {
	if ( OpVerbose () == True ) {
	      fprintf(stderr, "warning >>>>>>  Browser Error at line %d\n", decl_line);
	      fprintf(stderr, "warning    >>  Class \"%s\" already declared at line %d in file %s\n",
	                      class_name,
	                      current_class->_decl_line,
	                      current_class->_decl_file->_name);
	}
    }
  }
  return(BERROR);
}
	

/*------------------------------------------------------------------------------
*/
BrowserError add_parent(class_name, parent_name, scope)
    char*      class_name;
	char*      parent_name;
	ScopeType  scope; 
{
  ClassRec*    current_class;
  ParentRec*   current_parent;
  ParentRec**  head;
  int          x_size;

  if ((class_cache != Null) && (strcmp(class_cache->_name, class_name) == 0))
    current_class = class_cache;
  else {
    current_class = find_class(class_name);
    class_cache   = current_class;
  }
  if (current_class != Null) {
	head = & (current_class->_parents_list);
    search_Rec(parent_name, ParentRec, head, current_parent);
	if (current_parent == Null) {
	  x_size = sizeof(ParentRec) + PARENT_PLENGTH + strlen(parent_name) + 1;
	  current_parent = (ParentRec*) xmalloc(x_size);
      if (current_parent != Null) {
        create_Rec(parent_name, ParentRec, head, current_parent, PARENT_PREFIX, PARENT_PLENGTH);
		current_parent->_scope = scope;
	    current_class->_parents_count++;
	    return(NO_BERROR);
	  }
    }
    else {
      if (current_parent->_scope == UNKNOWN_SCOPE) {
		current_parent->_scope = scope;
	    current_class->_parents_count++;
	    return(NO_BERROR);
	  }
      else {
	if ( OpVerbose () == True ) {
	        fprintf(stderr, "warning >>>>>>  Parser FATAL ERROR\n");
	        fprintf(stderr, "warning    >>  For class \"%s\" parent \"%s\" already declared\n",
	                        class_name, parent_name);
	}
      }
    }
  }
  else {
	if ( OpVerbose () == True ) {
	    fprintf(stderr, "warning >>>>>>  Parser FATAL ERROR\n");
	    fprintf(stderr, "warning    >>  Class \"%s\" not declared\n",
                  	  class_name);
	}
  }
  return(BERROR);
}


/*------------------------------------------------------------------------------
*/
BrowserError add_method_decl(class_name, method_name, scope, virtual_decl, decl_line)
    char*      class_name; 
    char*      method_name;
    ScopeType  scope;
    int        virtual_decl;
    LineNumber decl_line;
{
  ClassRec*   current_class;
  MethodRec** head;
  MethodRec*  current_method;
  int         x_size;

  if ((class_cache != Null) && (strcmp(class_cache->_name, class_name) == 0))
    current_class = class_cache;
  else {
    current_class = find_class(class_name);
    class_cache   = current_class;
  }
  if (current_class != Null) {
    head = & (current_class->_methods_list);
    search_Rec(method_name, MethodRec, head, current_method);
	if (current_method == Null) {
	  x_size = sizeof(MethodRec) + METHOD_PLENGTH + strlen(method_name) + 1;
	  current_method = (MethodRec*) xmalloc(x_size);
	  if (current_method != Null) {
	    create_Rec(method_name, MethodRec, head, current_method, METHOD_PREFIX, METHOD_PLENGTH);
	    current_method->_class_name   = current_class->_name;
		current_method->_scope        = scope;
		current_method->_virtual_decl = virtual_decl;
		current_method->_decl_file    = parsed_file;
		current_method->_decl_line    = decl_line;
		current_method->_impl_file    = Null;
		current_method->_impl_line    = 0;
		current_method->_next_marked  = Null;
        return(NO_BERROR);
      }
	}
	else if (current_method->_decl_file == Null) {
      current_method->_scope        = scope;
      current_method->_virtual_decl = virtual_decl;
      current_method->_decl_file    = parsed_file;
	  current_method->_decl_line    = decl_line;
      return(NO_BERROR);
	}
	else {
		if ( OpVerbose () == True ) {
		      fprintf(stderr, "warning >>>>>>  Browser Error at line %d\n", decl_line);
		      fprintf(stderr, "warning    >>  Method \"%s\" already declared at line %d in file %s\n",
		                      method_name,
		                      current_method->_decl_line,
		                      current_method->_decl_file->_name);
		}
	}
  }
  else {
	if ( OpVerbose () == True ) {
	    fprintf(stderr, "warning >>>>>>  Parser FATAL ERROR at line %d\n", decl_line);
	    fprintf(stderr, "warning    >>  Class \"%s\" not declared\n", class_name);
	}
  }
  return(BERROR);
}


/*------------------------------------------------------------------------------
*/
BrowserError add_method_impl(class_name, method_name, impl_line)
    char*      class_name;
    char*      method_name;
    LineNumber impl_line;
{
  ClassRec*   current_class;
  MethodRec** head;
  MethodRec*  current_method;
  int         x_size;

  if ((class_cache != Null) && (strcmp(class_cache->_name, class_name) == 0))
    current_class = class_cache;
  else {
    current_class = create_class(class_name);
    class_cache   = current_class;
  }
  if (current_class != Null) {
    head = & (current_class->_methods_list);
    search_Rec(method_name, MethodRec, head, current_method);
	if (current_method == Null) {
	  x_size = sizeof(MethodRec) + METHOD_PLENGTH + strlen(method_name) + 1;
	  current_method = (MethodRec*) xmalloc(x_size);
	  if (current_method != Null) {
	    create_Rec(method_name, MethodRec, head, current_method, METHOD_PREFIX, METHOD_PLENGTH);
	    current_method->_class_name   = current_class->_name;
		current_method->_scope        = UNKNOWN_SCOPE;
		current_method->_virtual_decl = False;
		current_method->_decl_file    = Null;
		current_method->_decl_line    = 0;
		current_method->_impl_file   = parsed_file;
		current_method->_impl_line   = impl_line;
		current_method->_next_marked = Null;
        return(NO_BERROR);
      }
	}
	else if (current_method->_impl_file == Null) {
      current_method->_impl_file = parsed_file;
	  current_method->_impl_line = impl_line;
      return(NO_BERROR);
	}
	else {
		if ( OpVerbose () == True ) {
		      fprintf(stderr, "warning >>>>>>  Browser Error at line %d\n", impl_line);
		      fprintf(stderr, "warning    >>  Method \"%s\" already implemented at line %d in file %s\n",
		                      method_name,
		                      current_method->_impl_line,
		                      current_method->_impl_file->_name);
		}
	}
  }
  return(BERROR);
}
