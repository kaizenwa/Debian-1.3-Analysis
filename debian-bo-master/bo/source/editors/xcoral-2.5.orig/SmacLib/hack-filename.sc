/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: hack-filename.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/hack-filename.sc
   Description: 
   Created: Sun Aug  7 16:06:40 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 16:06:42 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: filename hack, called in comments.sc and make-ext.sc

   Defines: hack_file_name

   ########################################################################

   Copyright (c) : Thierry Emery

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


/* -------------------------------------------------------------------------
   default hack_file_name function
   
   hack_file_name() can be redefined to modify an inserted filename, for
   instance to restore the logical path from the automount path

   if you redefine this function after loading this file, execute
   `remove_function_definition(hack_file_name);' before redefinition
   
   examples of hacks:
   
   - /local/... -> /usr/...
     if (backward_search("/local/")) {
       delete_chars(7);
       insert_string("/usr/")
     }

   - /tmpmnt/usr/... -> /usr/...
     if (backward_search("/tmpmnt/usr/"))
       delete_chars(7);

   - /tmpmnt/automnt/mnt/... -> /usr/...
     if (backward_search("/tmpmnt/automnt/mnt/")) {
       delete_chars(20);
       insert_string("/users/");
     }
   ------------------------------------------------------------------------- */
{
  if (! function("delete_chars"))
    load_file("utilities.sc");
}

void hack_file_name() {
  int bline;
  int pos = current_position();
  
  goto_beginning_of_line();
  bline = current_position();
  goto_char(pos);

  while(backward_search("/tmp_mnt")) {
    if (current_position() < bline ) /* on est trop loin */
      break;
    delete_chars(8);
    pos-=8;
  }

  goto_char(pos);
}

/* -------------------------------------------------------------------------
   default hack_file_string :
   /tmp_mnt/users/         -> /users/
   ------------------------------------------------------------------------- */

char* hack_file_string (char* str) {

  char *command, *result,
       *format="echo \"%s\" | sed -e \"s|/tmp_mnt/users/|/users/|g\"";

  command = (char*) malloc(strlen(format)+strlen(str)+1);
  sprintf(command,format,str);

  result = cmd_shell_to_string(command);
  free(command);
  return result;
}

