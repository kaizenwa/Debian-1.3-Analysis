/* ########################################################################

			     browser_eng.h

   File: browser_eng.h
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_eng.h
   Description: 
   Created: Fri Jan 27 10:45:06 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 10:45:08 MET 1995
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


#ifndef BROWSERENGINE_H
#define BROWSERENGINE_H


/*------------------------------------------------------------------------------
//       Les procedures pour consulter le dictionnaire des classes
//
//    toutes les informations sont retournees via un pointeur sur une 
//     structure de donnees alloue'e dynamiquement. Ce pointeur (et 
//     uniquement lui) doit e^tre de'salloue' par un appel explicite
//     a la fonction free().
//------------------------------------------------------------------------------
*/

extern StringTable* get_files_list    ();


extern StringTable* get_classes_list  ();

extern StringTable* get_parents_list  (/* char* class_name */);

extern StringTable* get_sons_list     (/* char* class_name */);

extern StringTable* get_methods_list  (/* char* class_name */);

extern Position*    get_class_decl    (/* char* class_name */);

extern Position*    get_method_decl   (/* char* class_name,
				                          char* method_name */);

extern Position*    get_method_impl   (/* char* class_name,
				                          char* method_name */);


extern StringTable* get_procs_list    ();

extern Position*    get_proc_impl     (/* char* proc_name */);


/*------------------------------------------------------------------------------
//                     Les procedures de gestion du "Browser"
//------------------------------------------------------------------------------
*/

extern void init_browser ();

extern void parse_file   (/* char* file_name */);

extern void delete_file  (/* char* file_name */);


#endif  /*  BROWSERENGINE_H  */

