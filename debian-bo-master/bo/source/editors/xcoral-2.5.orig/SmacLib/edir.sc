/* #########################################################################

			       XCORAL SMAC

   File: edir.sc
   Created: Tue Nov 8 18:14:18 1994
   Author: Thierry Emery
   Modified: Mon Dec  5 14:07:37 1994
   Last maintained by: Thierry Emery

   RCS $Revision: 1.9 $ $State: Exp $
   

   #########################################################################

   Note: edit directory utility

   Requires: cmd.sc, utilities.sc, shell-commands.sc, hack_file_string()

   Defines: edir, edir_switches, edir_dir, edir mode, edir_find, edir_new,
	    edir_reread, edir_quit, edir_shell_command

   Suggested bindings: "^xd" "edir"

   Procedure: ^xd <directory>
   	      ? for help
	      f to find a file or a directory in the same window
	      n to find a file or a directory in another window
	      < to find .. in the same window
	      > to find .. in another window
	      q to quit the current edir
	      r to reread the current directory
	      s to issue a shell command
	      
	      C to copy a file
	      G to change a group
	      H to create a hard link
	      L to load a SMAC file
	      M to change a mode
	      P to print a file
	      R to rename
	      S to create a symbolic link
	      Z to compress or uncompress a file
	      = to call diff

   #########################################################################

   Permission to use, copy, and distribute for non-commercial purposes, is
   hereby granted without fee, providing that this permission notice appear
   in all copies and in supporting documentation. The software may be
   modified for your own purposes, but modified versions may not be
   distributed. This software is provided "as is" without any expressed or
   implied warranty.

   ######################################################################### */

{
  if (! function("hack_file_string"))
    load_file("hack-filename.sc");
  if (! function("find_titled_window"))
    load_file("cmd.sc");
  if (! function("color_buffer"))
    load_file("color.sc");
}

/* ################################################################################
				   Variables
   ################################################################################ */

/* -------------------------------------------------------------------------
   edir switches : must contain -l
   ------------------------------------------------------------------------- */

char* edir_switches="-alg";

/* -------------------------------------------------------------------------
   edir compressed extension
   ------------------------------------------------------------------------- */

char* edir_compressed_extension=".gz";

/* -------------------------------------------------------------------------
   edir compress command
   ------------------------------------------------------------------------- */

char* edir_compress_command="gzip";

/* -------------------------------------------------------------------------
   edir uncompress command
   ------------------------------------------------------------------------- */

char* edir_uncompress_command="gunzip";

/* -------------------------------------------------------------------------
   edir head start
   ------------------------------------------------------------------------- */

char* edir_head_start="*** Edir Window - ";

/* -------------------------------------------------------------------------
   edir head end
   ------------------------------------------------------------------------- */

char* edir_head_end=" ***";


/* ################################################################################
				   Utilities
   ################################################################################ */

/* -------------------------------------------------------------------------
   edir hack dir utility : calls cd & pwd, then hack_file_string(), removes
   possible trailing new line, and possible trailing slash
   ------------------------------------------------------------------------- */

char* edir_hack_dir_from (char* from_dir, char* dir) {

  char *command="(cd %s;cd %s 2>&1;pwd)", *full_command,
       *real_dir, *hacked_dir;

  full_command = (char*) malloc(strlen(command)+strlen(from_dir)+
				strlen(dir)+1);
  sprintf(full_command,command,from_dir,dir);
  real_dir = cmd_shell_to_string(full_command);
  free(full_command);
  
  if (real_dir && !strstr(real_dir,"bad directory")) {
    hacked_dir = hack_file_string(real_dir);
    if (hacked_dir[strlen(hacked_dir)-1] == '\n')
      hacked_dir[strlen(hacked_dir)-1] = 0;
    if (hacked_dir[strlen(hacked_dir)-1] == '/' && strlen(hacked_dir) > 1)
      hacked_dir[strlen(hacked_dir)-1] = 0;
  }
  else
    hacked_dir = 0;

  if (real_dir)
    free(real_dir);
  return hacked_dir;
}

/* -------------------------------------------------------------------------
   edir hack dir utility : calls edir_hack_dir_from "."
   ------------------------------------------------------------------------- */

char* edir_hack_dir (char* dir) {

  return edir_hack_dir_from(".",dir);
}

/* -------------------------------------------------------------------------
   edir utility : optionally in a new window, optionally raising the window
   ------------------------------------------------------------------------- */

void edir_dir (char* dir, int new_window_p, int raise_p) {

  char *head, *hacked_dir, *ls_command;
  int origin_win=current_window(), edir_win, pos;

  hacked_dir = edir_hack_dir(dir);

  if (!hacked_dir)
    return;

  head = (char*) malloc(strlen(edir_head_start)+strlen(hacked_dir)
			+strlen(edir_head_end)+1);
  sprintf(head,"%s%s%s",edir_head_start,hacked_dir,edir_head_end);
  edir_win=find_titled_window(head, -1);

  if (new_window_p) {
    if (edir_win == -1)
      edir_win = new_window();
  }
  else {
    if (edir_win != -1 && edir_win != origin_win) {
      select_window(origin_win);
      kill_window(edir_win);
    }
    edir_win = origin_win;
  }

  if (edir_win == -1) {
    display_message("Cannot create Edir Window...",
		    "IMPORTANT NOTE",1);
    free(hacked_dir);
    free(head);
    return;
  }
  
  select_window(edir_win);
  
  kill_current_buffer();
  insert_string(" ");
  insert_string(head);
  insert_string(" \n");
  select_window(edir_win);

  ls_command = (char*) malloc(strlen(edir_switches)+strlen(dir)+5);
  sprintf(ls_command,"ls %s %s",edir_switches,dir);
  cmd_shell(ls_command);
  goto_char(0);
  set_mode("Edir");

  if (raise_p)
    raise_window();

  color_buffer();
  
  free(ls_command);
  free(hacked_dir);
  free(head);
}


/* ################################################################################
				  Entry Point
   ################################################################################ */

/* -------------------------------------------------------------------------
   edit dir
   ------------------------------------------------------------------------- */

void edir () {

  char *default_dir, *msg, *relative_dir=0, *dir, *base;

  default_dir = edir_hack_dir(".");

  msg = (char*) malloc(strlen(default_dir)+10);
  base = basename(default_dir);
  sprintf(msg,"Edir (%s) :", base);
  relative_dir = gets(msg);
  free(msg);

  if (!relative_dir)
    edir_dir(default_dir,1,1);
  else {
    if (!gets_string_cancelled(relative_dir)) {
      dir = edir_hack_dir_from(default_dir,relative_dir);
      if (dir) {
	edir_dir(dir,1,1);
	free(dir);
      }
      else {
	display_message(relative_dir,"ERROR",1);
	display_message(" : not a directory");
      }
    }
    free(relative_dir);
  }
  free(base);
  free(default_dir);
}


/* ################################################################################
				   Edir Mode
   ################################################################################ */

/* -------------------------------------------------------------------------
   remove key defs
   ------------------------------------------------------------------------- */

void edir_noop_keys() {

  char *key=" ";
  int c;

  for (c=32;c<=125;++c) {
    sprintf(key,"%c",c);
    key_def("Edir",key,"edir_noop");
  }
}

/* -------------------------------------------------------------------------
   Edir mode
   ------------------------------------------------------------------------- */

{
  create_mode("Edir");

  key_def("Edir","^c","edir_noop");
  key_def("Edir","^d","edir_noop");
  key_def("Edir","^h","edir_noop");
  key_def("Edir","^i","edir_noop");
  key_def("Edir","^j","edir_noop");
  key_def("Edir","^k","edir_noop");
  key_def("Edir","^m","edir_noop");
  key_def("Edir","^o","edir_noop");
  key_def("Edir","^q","edir_noop");
  key_def("Edir","^t","edir_noop");
  key_def("Edir","^u","edir_noop");
  key_def("Edir","^w","edir_noop");
  key_def("Edir","^y","edir_noop");
  edir_noop_keys();

  key_def("Edir", "?", "edir_help");
  key_def("Edir", "f", "edir_find");
  key_def("Edir", "n", "edir_new");
  key_def("Edir", "q", "edir_quit");
  key_def("Edir", "r", "edir_reread");
  key_def("Edir", "s", "edir_shell_command");
  key_def("Edir", "<", "edir_up");
  key_def("Edir", ">", "edir_up_new");
	      
  key_def("Edir", "^x^c", "edir_quit");
  
  /* a la Emacs : */
  key_def("Edir", " ", "goto_next_line");
  key_def("Edir", "^h", "goto_previous_line");
  key_def("Edir", "\177", "goto_previous_line");
  key_def("Edir", "o", "edir_new");
  key_def("Edir", "g", "edir_reread");
  key_def("Edir", "!", "edir_shell_command");

  key_def("Edir", "C", "edir_copy");
  key_def("Edir", "G", "edir_change_group");
  key_def("Edir", "H", "edir_hardlink");
  key_def("Edir", "L", "edir_load");
  key_def("Edir", "M", "edir_change_mode");
  key_def("Edir", "P", "edir_print");
  key_def("Edir", "R", "edir_rename");
  key_def("Edir", "S", "edir_symlink");
  key_def("Edir", "Z", "edir_compress_uncompress");
  key_def("Edir", "=", "edir_diff");

  set_mode_font ("Edir", "-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso8859-1");
}

/* -------------------------------------------------------------------------
   edir no operation
   ------------------------------------------------------------------------- */

void edir_noop () {

}

/* --------------------------------------------------------------------------------
   edir help
   -------------------------------------------------------------------------------- */

void edir_help () {

  char* help_msg = "? for help\nf to find a file or a directory in the same window\nn to find a file or a directory in another window\n< to find .. in the same window\n> to find .. in another window\nq to quit the current edir\nr to reread the current directory\ns to issue a shell command\nC to copy a file\nG to change a group\nH to create a hard link\nL to load a SMAC file\nM to change a mode\nP to print a file\nR to rename\nS to create a symbolic link\nZ to compress or uncompress a file\n= to call diff";

  display_message(help_msg,"Edir Help",1);

}

/* -------------------------------------------------------------------------
   current directory shown in edir
   ------------------------------------------------------------------------- */

char* edir_current_dir () {

  char *dir;
  int origin=current_position(), dir_start, dir_end;

  goto_char(0);
  forward_search(edir_head_start);
  dir_start = current_position()+strlen(edir_head_start);
  forward_search(edir_head_end);
  dir_end = current_position();
  dir = window_substring(dir_start,dir_end);

  goto_char(origin);
  return dir;
}

/* -------------------------------------------------------------------------
   edir in types predicate
   ------------------------------------------------------------------------- */

int edir_in_types (char* types) {

  int origin=current_position(), in_types;

  goto_beginning_of_line();
  in_types = (strchr(types,current_char()) != 0);
  goto_char(origin);

  return in_types;
}

/* -------------------------------------------------------------------------
   edir on file predicate
   ------------------------------------------------------------------------- */

int edir_on_file () {

  return edir_in_types("-");
}

/* -------------------------------------------------------------------------
   edir on dir predicate
   ------------------------------------------------------------------------- */

int edir_on_dir () {

  return edir_in_types("d");
}

/* -------------------------------------------------------------------------
   edir on link predicate
   ------------------------------------------------------------------------- */

int edir_on_link () {

  return edir_in_types("l");
}

/* -------------------------------------------------------------------------
   edir current file name : on which line the cursor is (0 if not on a file,
   directory or link)
   ------------------------------------------------------------------------- */

char* edir_current_filename () {

  int origin=current_position(), filename_start, filename_end;
  char *filename;

  if (edir_in_types("-dl")) {
    goto_end_of_line();
    filename_end = current_position();
    backward_search(" ");
    filename_start = current_position()+1;
    filename = window_substring(filename_start,filename_end);
    goto_char(origin);
  }
  else
    filename = 0;

  return filename;
}

/* --------------------------------------------------------------------------------
   edir expand filename
   -------------------------------------------------------------------------------- */

char* edir_expand_filename (char* dir, char* filename) {

  char *file;

  if (!dir || !filename)
    return 0;

  if (filename[0] == '/') {
    file = (char*) malloc(strlen(filename)+1);
    strcpy(file,filename);
  }
  else {
    if (dir == "/") {
      file = (char*) malloc(strlen(filename)+2);
      sprintf(file,"/%s",dir,filename);
    }
    else {
      file = (char*) malloc(strlen(dir)+strlen(filename)+2);
      sprintf(file,"%s/%s",dir,filename);
    }
  }

  return file;
}

/* --------------------------------------------------------------------------------
   edir current file
   -------------------------------------------------------------------------------- */

char* edir_current_file () {

  char *dir, *filename, *file;

  dir = edir_current_dir();
  if (!dir)
    return;
  
  filename = edir_current_filename();
  if (!filename) {
    free(dir);
    return;
  }

  file = edir_expand_filename(dir,filename);

  free(filename);
  free(dir);
  return file;
}

/* -------------------------------------------------------------------------
   find internal
   ------------------------------------------------------------------------- */

void edir_find_internal (int new_window_p) {

  char *file=edir_current_file();

  if (!file)
    return;

  if (edir_on_dir() || (edir_on_link() && is_dir(file)))
    edir_dir(file,new_window_p,1);
  else
    if (edir_on_file() || (edir_on_link() && is_file(file))) {
      if (new_window_p)
	select_window(new_window());
      read_file(file);
      color_buffer();
  }

  free(file);
}

/* -------------------------------------------------------------------------
   find
   ------------------------------------------------------------------------- */

void edir_find () {

    edir_find_internal(0);
}

/* -------------------------------------------------------------------------
   new
   ------------------------------------------------------------------------- */

void edir_new () {

    edir_find_internal(1);
}

/* -------------------------------------------------------------------------
   edir up utility
   ------------------------------------------------------------------------- */

void edir_up_internal (int new_window_p) {

  char *dir, *file;

  dir = edir_current_dir();
  if (!dir)
    return;
  
  file = (char*) malloc(strlen(dir)+4);
  sprintf(file,"%s/..",dir);
  edir_dir(file,new_window_p,1);

  free(file);
  free(dir);
}

/* -------------------------------------------------------------------------
   edir up
   ------------------------------------------------------------------------- */

void edir_up () {

  edir_up_internal(0);
}

/* -------------------------------------------------------------------------
   edir up new
   ------------------------------------------------------------------------- */

void edir_up_new () {

  edir_up_internal(1);
}

/* -------------------------------------------------------------------------
   quit edir
   ------------------------------------------------------------------------- */

void edir_quit () {

  kill_current_buffer();
  kill_window(current_window());
}

/* -------------------------------------------------------------------------
   reread directory
   ------------------------------------------------------------------------- */

void edir_reread () {

  int origin=current_position();
  char* dir=edir_current_dir();

  edir_dir(dir,0,1);
  goto_char(origin);
  free(dir);
}

/* -------------------------------------------------------------------------
   edir shell command
   ------------------------------------------------------------------------- */

void edir_shell_command () {

  char *dir=edir_current_dir(), *command;

  command = gets("Shell command : ");
  if (!gets_string_cancelled(command))
    cmd_in_dir(command,dir);

  free(command);
  free(dir);
}

/* -------------------------------------------------------------------------
   edir generic apply command to file
   ------------------------------------------------------------------------- */

void edir_command_on_file (char* command, char* query_format,
			   int args_first) {

  int origin=current_position();
  char *dir, *filename, *msg, *args, *full_command;

  dir = edir_current_dir();
  if (!dir)
    return;

  filename = edir_current_filename();
  if (!filename) {
    free(dir);
    return;
  }

  msg = (char*) malloc(strlen(query_format)+strlen(filename)+1);
  sprintf(msg,query_format,filename);
  args = gets(msg);

  if (!gets_string_cancelled(args)) {

    full_command = (char*) malloc(strlen(command)+strlen(filename)
				  +strlen(args)+3);
    if (args_first)
      sprintf(full_command,"%s %s %s",command,args,filename);
    else
      sprintf(full_command,"%s %s %s",command,filename,args);
    cmd_in_dir(full_command,dir);
    free(full_command);
  }

  if (args)
    free(args);
  free(msg);
  free(filename);

  edir_dir(dir,0,0);
  goto_char(origin);
  free(dir);
}

/* -------------------------------------------------------------------------
   edir copy
   ------------------------------------------------------------------------- */

void edir_copy () {

  edir_command_on_file("cp -p","Copy %s to : ",0);
}

/* -------------------------------------------------------------------------
   edir change group
   ------------------------------------------------------------------------- */

void edir_change_group () {

  edir_command_on_file("chgrp","Change Group of %s : ",1);
}

/* -------------------------------------------------------------------------
   edir hard link
   ------------------------------------------------------------------------- */

void edir_hardlink () {

  edir_command_on_file("ln","Create hard link to %s as : ",0);
}

/* -------------------------------------------------------------------------
   edir load
   ------------------------------------------------------------------------- */

void edir_load () {

  char* file=edir_current_file();

  if (file) {
    load_file(file);
    free(file);
  }
}

/* -------------------------------------------------------------------------
   edir change mode
   ------------------------------------------------------------------------- */

void edir_change_mode () {

  edir_command_on_file("chmod","Change Mode of %s : ",1);
}

/* -------------------------------------------------------------------------
   edir print
   ------------------------------------------------------------------------- */

void edir_print () {

  edir_command_on_file("lpr"," Print %s with args : ",1);
}

/* -------------------------------------------------------------------------
   edir rename
   ------------------------------------------------------------------------- */

void edir_rename () {

  edir_command_on_file("mv"," Rename %s to : ",0);
}

/* -------------------------------------------------------------------------
   edir symbolic link
   ------------------------------------------------------------------------- */

void edir_symlink () {

  edir_command_on_file("ln -s","Create symbolic link to %s as : ",0);
}

/* -------------------------------------------------------------------------
   edir diff
   ------------------------------------------------------------------------- */

void edir_diff () {

  edir_command_on_file("diff"," Diff %s with : ",0);
}

/* -------------------------------------------------------------------------
   edir compress/uncompress
   ------------------------------------------------------------------------- */

void edir_compress_uncompress () {

  int compress_p;
  char *dir, *filename, *file, *full_command;

  filename = edir_current_filename();
  if (!filename)
    return;

  dir = edir_current_dir();
  if (!dir) {
    free(filename);
    return;
  }

  file = edir_expand_filename(dir,filename);
  if (!file || !is_file(file)) {
    if (file)
      free(file);
    free(dir);
    free(filename);
    return;
  }
  free(file);

  compress_p = (! strstr(filename,edir_compressed_extension));

  full_command = (char*) malloc(strlen(filename)+3);
  if (compress_p)
    sprintf(full_command,"%s %s",edir_compress_command,filename);
  else
    sprintf(full_command,"%s %s",edir_uncompress_command,filename);
  cmd_in_dir(full_command,dir);

  free(full_command);
  free(dir);
  free(filename);
}
