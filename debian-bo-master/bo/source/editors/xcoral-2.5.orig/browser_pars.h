/* ########################################################################

			     browser_pars.h

   File: browser_pars.h
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_pars.h
   Description: 
   Created: Fri Jan 27 10:49:13 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 10:49:14 MET 1995
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


#ifndef BROWSERPARSE_H
#define BROWSERPARSE_H


/*------------------------------------------------------------------------------
*/
enum BrowserError {
  NO_BERROR,
  BERROR
};

typedef enum BrowserError BrowserError;


/*------------------------------------------------------------------------------
*/
extern FileRec*  parsed_file;

extern ClassRec* class_cache;


/*------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
*/

extern BrowserError add_file        (/* char*      file_name */);


/*------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
*/

extern BrowserError add_proc        (/* char*      proc_name,
                                        ProcType   proc_type,
                                        LineNumber impl_line */);


/*------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
*/

extern BrowserError add_class       (/* char*      class_name,
                                        LineNumber decl_line */);

extern BrowserError add_parent      (/* char*      class_name,
                                        char*      parent_name,
                                        ScopeType  scope */);

extern BrowserError add_method_decl (/* char*      class_name, 
                                        char*      method_name,
                                        ScopeType  scope,
                                        int        virtual_decl,
                                        LineNumber decl_line */);

extern BrowserError add_method_impl (/* char*      class_name,
                                        char*      method_name,
                                        LineNumber impl_line */);


#endif    /*  BROWSERPARSE_H  */




