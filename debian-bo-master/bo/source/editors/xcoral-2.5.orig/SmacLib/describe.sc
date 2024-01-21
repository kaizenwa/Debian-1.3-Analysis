/* #########################################################################

                 SMAC FILE USED BY XCORAL EDITOR

   File: describe_function.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/describe_function.sc
   Created: Tue Nov 30 19:10:24 1993
   Author: Thierry Emery
   Modified: Sun Jul 31 12:03:22 DST
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$

   #########################################################################

   Note: SMAC functions writer and/or user help

   Defines: describe, describe_all, describe_matching,
	    display_int, display_char, display_string

   Procedure: - to get a SMAC function description, execute
		`describe(<function>)' (eg `describe(describe)')
	      - to get a listing of all current SMAC functions descriptions
		in a new window, execute `describe_all()'
	      - to get a listing of descriptions of all current SMAC
	        functions matching a regexp in a new window, execute
		`describe_matching(<regexp>)'
	      - to display a global variable value or a function return
		value, execute `display_int(<exp>)', `display_char(<exp>)',
		or `display_string(<exp>)', depending on the result type

   #########################################################################

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

   ######################################################################### */

/* -------------------------------------------------------------------------
   describe a function > Messages Box (inspired from describe_functions)
   ------------------------------------------------------------------------- */

void describe(void* f) {

  char* name = function_name(f);
  char* type = function_type(f);
  int	n_args = function_arg_count(f),
	builtin_p = (function_is_builtin(f)) ? 10 : 0;
  char* description;
  
  description = (char*) malloc(strlen(name)+builtin_p+strlen(type)+30);
    
  sprintf(description, "\n%s %s: %d param%s, %s\n",
	  name, (function_is_builtin(f)) ? "(builtin) " : "",
	  n_args, (n_args != 1) ? "s" : "", type);

  display_message(description,"Function description :",1);
  
  free(name);
  free(type);
  free(description);
}

/* -------------------------------------------------------------------------
   describe all functions > new window (modified from describe_functions)
   ------------------------------------------------------------------------- */

void describe_all() {
  
  int descr_win=new_window();
  
  if (descr_win != -1) {
    
    void* f;
    char *tmp_file_name=(char*)malloc(32), *hour_str=(char*)malloc(7),
         *command=(char*)malloc(64), *huge_str, *huge_command;
    
    hour_str=cmd_shell_to_string("date '+%H%M%S'");
    strcpy(tmp_file_name,"/tmp/smac_functions_");
    strcat(tmp_file_name,hour_str);
    free(hour_str);
    
    select_window(descr_win);
    watch_on();
    init_function_list();
    
    while (f = function_list()) {
      char* name = function_name(f);
      char* type = function_type(f);
      
      int   n_args = function_arg_count(f);
      
      wprintf("echo '%s : %s%d param%s, %s'; \\\n",
	      name, (function_is_builtin(f)) ? "(builtin) " : "",
	      n_args, (n_args != 1) ? "s" : "", type);
      free(name);
      free(type);
    }
    
    huge_str = window_substring(0,end_of_file());
    huge_command = (char*)malloc(end_of_file()+64);
    sprintf(huge_command,"(%s) > %s",huge_str,tmp_file_name);
    free(huge_str);
    cmd_shell(huge_command);
    free(huge_command);
    kill_current_buffer();
    
    sprintf(command,"sort %s",tmp_file_name);
    cmd_shell(command);
    sprintf(command,"/bin/rm -f %s >/dev/null 2>&1",tmp_file_name);
    cmd_shell(command);
    
    free(tmp_file_name);
    free(command);
    
    goto_char(0);
    raise_window();
    watch_off();
  }
}

/* -------------------------------------------------------------------------
   describe all matching functions  > new window (modified from describe_all)
   ------------------------------------------------------------------------- */

void describe_matching(char* regexp) {
  
  int descr_win=new_window();
  
  if (descr_win != -1) {
    
    void* f;
    char *tmp_file_name=(char*)malloc(32), *hour_str=(char*)malloc(7),
	 *command=(char*)malloc(64), *huge_str, *huge_command;
    
    hour_str=cmd_shell_to_string("date '+%H%M%S'");
    strcpy(tmp_file_name,"/tmp/smac_functions_");
    strcat(tmp_file_name,hour_str);
    free(hour_str);
    
    init_function_list();
    select_window(descr_win);
    watch_on();
    
    while (f = function_list()) {
      char* name = function_name(f);
      char* type = function_type(f);
      int   n_args = function_arg_count(f);
      
      wprintf("echo '%s : %s%d param%s, %s'; \\\n",
	      name, (function_is_builtin(f)) ? "(builtin) " : "",
	      n_args, (n_args != 1) ? "s" : "", type);
      
      goto_previous_line();
      goto_beginning_of_line();
      if (re_forward_search(regexp) >= 0)
	goto_next_line();
      else {
	delete_to_end_of_line();
	delete_char();
      }
      free(name);
      free(type);
    }

    huge_str = window_substring(0,end_of_file());
    huge_command = (char*)malloc(end_of_file()+64);
    sprintf(huge_command,"(%s) > %s",huge_str,tmp_file_name);
    free(huge_str);
    cmd_shell(huge_command);
    free(huge_command);
    kill_current_buffer();
    
    sprintf(command,"sort %s",tmp_file_name);
    cmd_shell(command);
    sprintf(command,"/bin/rm -f %s >/dev/null 2>&1",tmp_file_name);
    cmd_shell(command);
    
    free(tmp_file_name);
    free(command);
    
    goto_char(0);
    raise_window();
    watch_off();
  }
}

/* -------------------------------------------------------------------------
   display_int, display_char, display_string : can be used to check a
   global variable value or a function return value
   ------------------------------------------------------------------------- */

void display_int(int value) {

    char val_message [64];

    sprintf(val_message,"= %d",value);
    display_message(val_message,"Value of int",1);
}

void display_char(char value) {

    char val_message [16];

    sprintf(val_message,"= '%c'",value);
    display_message(val_message,"Value of char",1);
}

void display_string(char* value) {

    char* val_message = (char*) malloc(strlen(value)+5);

    sprintf(val_message,"= \"%s\"",value);
    display_message(val_message,"Value of string",1);
    free(val_message);
}


/*
void describe_functions()
{
  void * f;
  
  init_function_list();
  while (f = function_list()) {
    char * name = function_name(f);
    char * decl = function_type(f);
    
    printf("%s : %s%d params, %s\n",
           name, (function_is_builtin(f)) ? "(builtin) " : "",
           function_arg_count(f), decl);
    
    free(name);
    free(decl);
  }
}
*/
