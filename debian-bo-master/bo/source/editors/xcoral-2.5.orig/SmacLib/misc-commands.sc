/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: misc_commands.sc
   Path: /home/fournigault/c/X11/xcoral-2.404/SmacLib/misc_commands.sc
   Description: 
   Created: Fri Aug  4 11:00:05 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Aug  4 11:00:06 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   Requires: 

   Defines: 

   Suggested bindings: 

   Procedure: 

   ########################################################################

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

   ######################################################################## */

misc_commands()
{
    char *str;
    int win = current_window();

    clear_list();
    add_list_item("title");
    add_list_item("shell_title");
    add_list_item("imake_title");
    add_list_item("smac_title");
    add_list_item("custom_title");
    add_list_item("custom_shell_title");
    add_list_item("custom_imake_title");
    add_list_item(" "); 
    add_list_item("update_title");
    add_list_item("update_title_and_save_file");
    add_list_item("update_title_backup_and_save_file");
    add_list_item(" "); 
    add_list_item("sharp_comment");
    add_list_item("plus_comment");
    add_list_item("equal_comment");
    add_list_item("minus_comment");
    add_list_item("percent_comment");
    add_list_item(" "); 
    add_list_item("change_return_to_newline");
    add_list_item("print_ascii");
    add_list_item("purge");

    str = select_from_list("Misc commands");
    redisplay();
    select_window(win);
    
    if(str==0 || strlen(str) < 2) {
	return;
    }
    
    if (strcmp(str,"title")==0) {
      str = gets("Enter title ");
      if (gets_string_cancelled(str))
	return;
      else
	title(str);
      return;
    }

    if (strcmp(str,"shell_title")==0) {
      str = gets("Enter shell_title ");
      if (gets_string_cancelled(str))
	return;
      else
	shell_title(str);
      return;
    }

    if (strcmp(str,"imake_title")==0) {
      str = gets("Enter imake_title ");
      if (gets_string_cancelled(str))
	return;
      else
	imake_title(str);
      return;
    }

    if (strcmp(str,"smac_title")==0) {
      smac_title();
      return;
    }

    if (strcmp(str,"custom_title")==0) {
      str = gets("Enter custom__title ");
      if (gets_string_cancelled(str))
	return;
      else
	custom_title(str);
      return;
    }

    if (strcmp(str,"custom_shell_title")==0) {
      str = gets("Enter custom_shell_title ");
      if (gets_string_cancelled(str))
	return;
      else
	custom_shell_title(str);
      return;
    }

    if (strcmp(str,"custom_imake_title")==0) {
      str = gets("Enter custom_imake_title ");
      if (gets_string_cancelled(str))
	return;
      else
	custom_imake_title(str);
      return;
    }

    if (strcmp(str,"update_title")==0) {
      update_title();
      return;
    }

    if (strcmp(str,"update_title_and_save_file")==0) {
      update_title_and_save_file();
      return;
    }

    if (strcmp(str,"update_title_backup_and_save_file")==0) {
      update_title_backup_and_save_file();
      return;
    }

    if (strcmp(str,"sharp_comment")==0) {
      sharp_comment();
      return;
    }

    if (strcmp(str,"equal_comment")==0) {
      equal_comment();
      return;
    }

    if (strcmp(str,"minus_comment")==0) {
      minus_comment();
      return;
    }

    if (strcmp(str,"plus_comment")==0) {
      plus_comment();
      return;
    }

    if (strcmp(str,"percent_comment")==0) {
      percent_comment();
      return;
    }

    if (strcmp(str,"change_return_to_newline")==0) {
      change_return_to_newline();
      return;
    }

    if (strcmp(str,"print_ascii")==0) {
      print_ascii();
      return;
    }

    if (strcmp(str,"commit")==0) {
      purge();
      return;
    }
}
