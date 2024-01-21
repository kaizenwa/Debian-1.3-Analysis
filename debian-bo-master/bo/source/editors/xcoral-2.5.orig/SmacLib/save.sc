/* #########################################################################

                 SMAC FILE USED BY XCORAL EDITOR

   File: save.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/save.sc
   Created: Tue Nov 30 19:02:35 1993
   Author: Thierry Emery
   Modified: Sun Jul 31 11:48:59 DST
   Last maintained by: Lionel Fournigault

   RCS $Revision: 1.2 $ $State: Exp $

   #########################################################################

   Note: save file utilities
   
   Requires: title.sc

   Defines: update_title_and_save_file, update_title_backup_and_save_file

   Suggested bindings: "^xz" "update_title_and_save_file"
		       "^xs" "update_title_backup_and_save_file"

   Procedure: - to update the `File', `Path', `Modified' and
		`Last maintained by' fields, and save the file, execute
		`update_title_and_save_file'
	      - to do the same, but move old version to <filename>~ first,
		execute `update_title_backup_and_save_file'

   #########################################################################

   Copyright (c) : Lionel Fournigault

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

   ######################################################################### */

{
  if (! function("update_title"))
    load_file("title.sc");
}

/* -------------------------------------------------------------------------
   update title and save file
   ------------------------------------------------------------------------- */

void update_title_and_save_file() {

  char* file_name=filename();

  if (file_name) {
    update_title();
    save_file();
    free(file_name);
  }
  else {
    file_name = gets("Write file :");
    if (file_name) {
      write_file(file_name);
      update_title();
      save_file();
      free(file_name);
    }
  }
}

/* -------------------------------------------------------------------------
   update title, remove old backup ('<filename>~') with "rm -f",
   make new backup with "cp -p" and finally save file
   ------------------------------------------------------------------------- */

void update_title_backup_and_save_file() {

  char *file_name=filename(), *rm_command, *cp_command;

  if (file_name) {
    rm_command = (char*) malloc(strlen(file_name)+8);
    sprintf(rm_command,"rm -f %s~",file_name);

    cp_command = (char*) malloc(2*strlen(file_name)+9);
    sprintf(cp_command,"cp -p %s %s~",file_name,file_name);

    cmd_shell(rm_command);
    cmd_shell(cp_command);
    update_title();
    save_file();

    free(rm_command);
    free(cp_command);
    free(file_name);
  }
  else {
    file_name = gets("Write file :");
    if (file_name) {
      write_file(file_name);
      update_title();
      save_file();
      free(file_name);
    }
  }
}

