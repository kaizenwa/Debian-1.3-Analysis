/* ########################################################################

			      file_dict.h

   File: file_dict.h
   Path: /home/fournigault/c/X11/xcoral-2.31/file_dict.h
   Description: 
   Created: Fri Jan 27 11:01:45 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 11:01:46 MET 1995
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


#ifndef FILEDICT_H
#define FILEDICT_H

/*------------------------------------------------------------------------------
*/
struct FileRec {
  char*           _name;
  struct FileRec* _next;
};
typedef struct FileRec FileRec;



/*------------------------------------------------------------------------------
//                         Le dictionnaire des fichiers
//------------------------------------------------------------------------------
*/

#define FILE_DICT_SIZE    101

extern  FileRec* file_dict[];

extern  int      file_count;


/*------------------------------------------------------------------------------
//       Les procedures utilisees manipuler le dictionnaire des fichiers
//------------------------------------------------------------------------------
*/

extern FileRec*     create_file   (/* char* file_name */);

extern FileRec*     find_file     (/* char* file_name */);

extern void         remove_file   (/* char* file_name */);


/*------------------------------------------------------------------------------
//       La procedure d'initialisation du dictionnaire des fichiers
//------------------------------------------------------------------------------
*/

extern void         init_file     ();


#endif  /*  FILEDICT_H  */



