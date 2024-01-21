/* ########################################################################

			      proc_dict.h

   File: proc_dict.h
   Path: /home/fournigault/c/X11/xcoral-2.31/proc_dict.h
   Description: 
   Created: Fri Jan 27 11:25:30 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 11:25:31 MET 1995
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


#ifndef PROCDICT_H
#define PROCDICT_H

/*------------------------------------------------------------------------------
*/
enum ProcType {
    UNKNOWN_PROC,
	C_PROC,
	CPLUS_PROC
};
typedef enum ProcType ProcType;


struct ProcRec {
  char*           _name;
  FileRec*        _impl_file;
  LineNumber      _impl_line;
  ProcType        _proc_type;
  struct ProcRec* _next;
};
typedef struct ProcRec ProcRec;


/*------------------------------------------------------------------------------
//                         Le dictionnaire des procedures
//------------------------------------------------------------------------------
*/

#define PROC_DICT_SIZE     503

extern  ProcRec* proc_dict[];

extern  int proc_count;


/*------------------------------------------------------------------------------
//       Les procedures utilisees manipuler le dictionnaire des procedures
//------------------------------------------------------------------------------
*/

extern ProcRec*     create_proc   (/* char* proc_name */);

extern ProcRec*     find_proc     (/* char* proc_name */);


/*------------------------------------------------------------------------------
//       La procedure d'initialisation du dictionnaire des procedures
//------------------------------------------------------------------------------
*/

extern void         init_proc     ();


#endif  /*  PROCDICT_H  */




