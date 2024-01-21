/* ########################################################################

			      file_dict.c

   File: file_dict.c
   Path: /home/fournigault/c/X11/xcoral-2.31/file_dict.c
   Description: 
   Created: Fri Jan 27 11:01:24 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 11:01:25 MET 1995
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
#include "browser_util.h"
#include <string.h>
#include <stdio.h>

/*------------------------------------------------------------------------------
//                         Le dictionnaire des fichiers
//------------------------------------------------------------------------------
*/

FileRec* file_dict[FILE_DICT_SIZE];

int      file_count = 0;


/*------------------------------------------------------------------------------
*/
FileRec* create_file (file_name)
  char* file_name;
{
	FileRec**  head;
  FileRec*   current_file;
	int        x_size;

	get_head_Rec(file_name, file_dict, FILE_DICT_SIZE, head);
  search_Rec(file_name, FileRec, head, current_file);
  if (current_file == Null) {
    x_size = sizeof(FileRec) + FILE_PLENGTH + strlen(file_name)  + 1;
    current_file = (FileRec*) xmalloc(x_size);
    if (current_file != Null) {
      create_Rec(file_name, FileRec, head, current_file, FILE_PREFIX, FILE_PLENGTH);
			file_count++;
		}
	}
  return(current_file);
}


/*------------------------------------------------------------------------------
*/
FileRec* find_file(file_name)
  char* file_name;
{
	FileRec**  head;
  FileRec*   current_file;

	get_head_Rec(file_name, file_dict, FILE_DICT_SIZE, head);
  search_Rec(file_name, FileRec, head, current_file);
  return(current_file);
}


/*------------------------------------------------------------------------------
*/
void remove_file(file_name)
  char* file_name;
{
  FileRec**  head;
  FileRec*   current_file;

	get_head_Rec(file_name, file_dict, FILE_DICT_SIZE, head);
  extract_Rec(file_name, FileRec, head, current_file);
	if (current_file != Null) {
	  free(current_file);
		--file_count;
	}
}


/*------------------------------------------------------------------------------
*/
void init_file() {
  int index;

  for (index = 0; index < FILE_DICT_SIZE; index++)
    file_dict[index] = Null;
}


