/* ########################################################################
   
			SMAC FILE USED BY XCORAL EDITOR
   
   File: cmd.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/cmd.sc
   Description: 
   Created: Sun Jul 10 14:05:21 MET 1994
   Author: Bruno Pages
   Modified: Mon Jan 23 16:46:00 DST
   Last maintained by: Bruno Pages
   
   RCS $Revision$ $State$
   
   ########################################################################
   
   Note: 
   This file contains the following users Smac functions :
   
   -cmd() : Execute a Unix command in a shell window
   -go_next() : Go to the indicated lines and filename.
   -edit_file_at_line () : create new window if necessary and load a file
   at the indicated line.
   
   The recognized line indications are :
   
   "file_name" <any>{0-9}<any>
   file_name:{0-9}<any>
   <any>"file_name", line {0-9}<any>
   <any> file_name, line {0-9}<any>
   
   Usage : 
   
   To execute the command :  
   
   ^x^e cmd("command with its arguments");
   
   To go to the indicated lines :
   
   In c mode, c++ mode and default mode : ^xg 
   else : ^x^e go_next(); 
   
   ########################################################################
   
   Copyright (c) : Bruno Pages
   
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

{
  if (! function("basename"))
    load_file("utilities.sc");
  if (! function("hack_file_name"))
    load_file("hack-filename.sc");
}

int cmd_window = -1;			/* command window number */
char * cmd_dir;				/* command directory */
int cmd_from_go_next = 0;		/* for edit_file_at_line */
int last_cmd_pos = 0;			/* Pour memoriser la derniere commande */
char * head_cmd_window = 
  "***Shell Cmd Window***";		/* Pour reperer la fenetre */
int cmd_first_in_shell = 0;

/* Search the file name and the line number for the pattern
   "file_name" <any>line_number<any> */

int find_cmd_trace1(int * bline, int * bfile, int * efile)
{
  if (current_char() == '"') {
    *bfile = current_position() + 1;
    do
      goto_next_char();
    while (! strchr("\"\n", current_char()));
    if ((current_char() == '"') && 
	(((*efile = current_position()) - *bfile) > 1) &&
	(msearch("0123456789", end_of_line(), 1))) {
      *bline = current_position();
      return 1;
    }
  }
  
  /* nothing recognized */
  goto_beginning_of_line();
  return 0;
}


/* Search the file name and the line number for the pattern
   file_name:line_number<any> */

int find_cmd_trace2(int * bline, int * bfile, int * efile)
{
  *bfile = current_position();
  while (! strchr("\n:", current_char()))
    goto_next_char();
  if ((current_char() == ':') &&
      (((*efile = current_position()) - *bfile) > 1) &&
      (strchr("0123456789", next_char()))) {
    *bline = current_position() + 1;
    return 1;
  }
  
  /* nothing recognized */
  goto_beginning_of_line();
  return 0;
}


int cmd_following_is(char * s)
{
  int pos = current_position();
  
  while (*s) {
    if (current_char() != *s++) {
      goto_char(pos);
      return 0;
    }
    else
      goto_next_char();
  }
  
  goto_char(pos);
  return 1;
}

/* Search the file name and the line number for the pattern
   <any>"file_name", line {0-9}<any> */

int find_cmd_trace3(int * bline, int * bfile, int * efile)
{
  while (! strchr("\n", current_char())) {
    if (current_char() == '\"') {
      *bfile = current_position() + 1;
      do goto_next_char();
      while (! strchr("\n\"", current_char()));
      if (cmd_following_is("\", line ") &&
	  strchr("0123456789", the_char(current_position() + 8))) {
	*efile = current_position();
	*bline = current_position() + 8;
	return 1;
      }
    }
    else
      goto_next_char();
  }
  
  /* nothing recognized */
  goto_beginning_of_line();
  return 0;
}

/* Search the file name and the line number for the pattern
   <any> file_name, line {0-9}<any> */

int find_cmd_trace4(int * bline, int * bfile, int * efile)
{
  while (! strchr("\n", current_char())) {
    if (cmd_following_is(", line ") &&
	strchr("0123456789", the_char(current_position() + 7))) {
      *efile = current_position();
      *bline = current_position() + 7;
      while (! strchr(" \t\n", previous_char()))
	goto_previous_char();
      *bfile = current_position();
      return 1;
    }
    else
      goto_next_char();
  }
  
  /* nothing recognized */
  goto_beginning_of_line();
  return 0;
}


/* Returns 1 if the current buffer is `file'
   filename() returns an absolute path, but file is absolute or relative */

int cmd_desired_buffer_p(char * file, int win)
{
  char * fname;
  int result = 0;
  
  if ((select_window(win) != -1) && (fname = filename())) {
    if (*file == '.') {
      /* remove the ../ or ./ at the beginning of file */
      do file += 1; while ((*file == '.') || (*file == '/'));
    }
    if (strlen(fname) >= strlen(file))
      result = ! strcmp(file,
			(strchr(file, '/'))
			? (*file == '/') ? fname
					 : fname + strlen(fname) - strlen(file)
			: strrchr(fname, '/') + 1);
    
    free(fname);
  }
  
  return result;
}

int cmd_read_file_in_a_new_buffer(char * file)
{
  int win;
  
  if ((win = new_window()) == -1) {
    cmd_from_go_next = 0;
    error("no more buffer");
  }
  
  select_window(win);
  
  if (! read_file(file)) {
    kill_window(win);
    return 0;
  }
  
  return 1;
}

int cmd_read_file_in_a_new_buffer_in_cmd_dir(char * file)
{
  if (*file == '/')
    return cmd_read_file_in_a_new_buffer(file);
  
  {
    char * dirfile = (char *) malloc(strlen(file) + strlen(cmd_dir) + 1);
    int result;
    
    if (! dirfile)
      error("not enought memory");
    
    sprintf(dirfile, "%s/%s", cmd_dir, file);
    result = cmd_read_file_in_a_new_buffer(dirfile);
    free(dirfile);
    return result;
  }
}

int edit_file_at_line(char * file, int line)
{
  int cwin = current_window();
  int win;
  
  /* Find a buffer associated with file */
  
  /* to avoid access to window which will imply redisplay */
  if (cmd_desired_buffer_p(file, cwin)) {
    goto_line(line - 1);
    raise_window();
    return;
  }
  
  /* perhaps another window */
  for (win = 31; win != -1; win -= 1)
    if (cmd_desired_buffer_p(file, win)) {
    goto_line(line - 1);
    raise_window();
    return;
  }
  
  /* not find, use a new buffer */
  
  if ((! cmd_from_go_next) ||
      (select_window(cmd_window) < 0) ||	/* to have the right dir */
      (! cmd_read_file_in_a_new_buffer_in_cmd_dir(file))) {
    select_window(cwin);
    if (! cmd_read_file_in_a_new_buffer(file))
      return 0;
  }
  
  goto_line(line - 1);
  raise_window();
}

/* --------------------------------------------------------------------------------
   Function name : find_titled_window
   
   Description : Look for a shell window with the specified title
   Input : the title and the probable window
   Output : Shell window id or -1. 
   -------------------------------------------------------------------------------- */

int is_the_titled_window(char* head, int cur_win)
{
  if (select_window(cur_win) != -1) {
    int cur_origin=current_position();
    
    goto_char(0);
    if (forward_search(head) && previous_char() != '"') {
      goto_char(cur_origin);
      return 1;
    }
    goto_char(cur_origin);
  }
  return 0;
}

int find_titled_window(char* head, int prefered)
{
  int orig_win = current_window();
  int result = -1;

  if (is_the_titled_window(head, prefered))
    result = prefered;
  else {
    int cur_win;
    
    for (cur_win=0; cur_win < 32; cur_win += 1) {
      if (is_the_titled_window(head, cur_win)) {
	result = cur_win;
	break;
      }
    }
  }
  select_window(orig_win);
  return result;
}

/* --------------------------------------------------------------------------------
   Function name : create_shell_window
   
   Description :
   Input :
   Output :
   -------------------------------------------------------------------------------- */

int create_shell_window()
{
  int shell_win = new_window();
  
  if (shell_win != -1) {
    select_window(shell_win);
    insert_string(" ");
    insert_string(head_cmd_window);
    cmd_window = shell_win;
    insert_string(" ");
    set_mode("Shell");
  }
  else {
    display_message("Cannot create Shell Cmd Window...",
		    "IMPORTANT NOTE",1);
    return -1;
  }
  return (shell_win);
}

/*
   **	Function name : cmd_in_dir
   **
   **	Description : Execute a Unix command in a shell window
   **         in the specified directory.
   **         If this one doesn't exist, it's created.  
   **	Input : the Unix command and the directory.
   **	Output :
*/
void cmd_in_dir (char *s, char* dir)
{
  int shell_win;
  char *the_cmd;
  int win_orig = current_window();
  
  shell_win =find_titled_window(head_cmd_window, cmd_window);
  cmd_first_in_shell = 0;
  cmd_dir = dir;
  if (shell_win == -1) {
    if ((shell_win = create_shell_window()) == -1)
      return;
    cmd_first_in_shell = 1;
  }
  
  select_window(shell_win);
  raise_window();
  goto_end_of_file();
  
  /* On sauvegarde la position */
  last_cmd_pos = current_position();
  
  the_cmd = (char *) malloc(strlen(s)+strlen(dir)+27);
  if (! the_cmd)
    error("not enought memory");
  if (strlen(s) && (s[strlen(s) - 1] == '\n'))
    s[strlen(s) - 1] = '\0';
  sprintf(the_cmd,"(echo \"\";cd %s;%s;echo Done)", dir, s);
  insert_string(the_cmd);
  hack_file_name();
  goto_end_of_line();
  insert_char('\n');
  
  free(the_cmd);
  select_window(win_orig);
}

/*
   **	Function name : cmd
   **
   **	Description : Execute a Unix command in a shell window.
   **         If this one doesn't exist, it's created.  
   **	Input : the Unix command.
   **	Output :
*/
void cmd (char* s)
{
  cmd_in_dir(s, cmd_shell_to_string("pwd"));
}

/* The classic and useful grep, Make(args) et make. */

void grep (char * args)
{
  /* add /dev/null to avoid deadlock when user forget filenames */
  char * str = (char *) malloc(strlen(args) + 30);
  
  if (! str)
    error("not enougth memory");
  
  sprintf(str, "grep -n %s /dev/null", args);
  cmd(str);
  free(str);
}

void Make(char *s)
{
  char *tmp = (char *) malloc(strlen(s) + 6);
  char *file = filename();
  
  if (! tmp)
    error("not enougth memory");
  
  sprintf(tmp,"make %s", s );
  cmd(tmp);
  free(tmp);
}

void make(){Make(" -k");}

/* --------------------------------------------------------------------------------
   latex commande
   -------------------------------------------------------------------------------- */
void latex ()
{
  char *file = filename();
  char *tmp;
  
  if(!file) {
    display_message("No file in current buffer",
		    "Latex cmd failed", 1);
    return;
  }
  tmp = (char *) malloc(strlen(file) + strlen("echo Xx | latex %s") + 2);
  if (! tmp)
    error("not enought memory");
  sprintf (tmp,"echo Xx | latex %s", file);
  cmd(tmp);
  free(tmp);
}

/* --------------------------------------------------------------------------------
   xdvi command
   -------------------------------------------------------------------------------- */

void xdvi ()
   {
   char *file = filename();
   char *tmp;
   
   if(!file) {
       display_message("No file in current buffer",
		   "xdvi cmd failed", 1);
       return;
   }
   tmp = (char *) malloc(strlen(file)+ 8);
   if (! tmp)
    error("not enought memory");
   sprintf (tmp,"%s .tex", file);
   file = basename(tmp);
   free(tmp);
   tmp = (char *) malloc(strlen(file) + strlen("xdvi %s &") + 8);
   if (! tmp)
    error("not enought memory");
   sprintf (tmp,"xdvi %s.dvi &", file);
   cmd(tmp);
   free(tmp);
   free(file);
   }

/*
   **	Function name :  go_first
   **  
   **	Description : On est dans le cas du premier go_next
   **         apres la premiere commande executee dans la fenetre shell.
   **         Le probleme est le suivant : le nom de la commande est
   **         inseree avant le prompt retourne par le shell plus
   **         un warning. On essaye de se deplacer apres le premier
   **         prompt.
   **
   **	Input :
   **	Output :
*/
go_first ()
{
  int pos,p_start,p_end,p_len,tmp ;
  
  pos = current_position();
  goto_beginning_of_line();
  
  if (forward_search(head_cmd_window)) {
    if (forward_search("Warning")){
      goto_next_line();
      goto_beginning_of_line();
      
      /* On sauvegarde la position */
      tmp = current_position(); 
      
      /* On va checher la longueur du curseur le curseur */
      pos = current_position();
      goto_end_of_file ();
      p_end = current_position();
      goto_beginning_of_line();
      p_start = current_position();
      
      /* on retourne ou on etait */
      goto_char (tmp);
      for (p_len = p_end - p_start; p_len; p_len -= 1)
	delete_char();
    }
  }
  else
    goto_char(pos);
  
  cmd_first_in_shell = 0;
}

/*
   **	Function name : go_next
   **
   **	Description : Look for the next line number and filename in the
   **         the signed shell window 'Shell Cmd Window', then show the filename
   **         at the specified line. If necessary a new window is opened to laod 
   **         the file.
   **         In the shell window the cursor move to next line and will be ready
   **         for the next go_next() command.
   **	Input :
   **	Output :
*/
void go_next()
{
  int cwin = current_window();
  int shell_win = find_titled_window(head_cmd_window, cmd_window);
  
  if (shell_win == -1) {
    display_message ("No Shell window");
    return;
  }
  
  select_window (shell_win);
  
  /*
     Si c'est le premier go_next depuis la derniere commande
     alors on va au debut du resultat de cell-ci.
  */
  if ((last_cmd_pos != 0) &&
      /* If the user change the cursor position */
      (current_position() == end_of_file()))
    goto_char(last_cmd_pos);
  
  last_cmd_pos = 0;
  
  /*
     Si c'est le premier go_next depuis la creation du shell
     il faut se placer au bon endroit (probleme de prompt)
  */
  if (cmd_first_in_shell &&
      /* If the user change the cursor position */
      (current_position() == end_of_file()))
    go_first();
  
  /* 
     Dans une shell window le dernier caractere n'est pas
     un '\n'. Il faut verifier des maintenant sinon les
     divers 'find_trace' peuvent cherher tres lomptemps
     la fin de la ligne....
  */
  goto_end_of_line();
  if (at_end_of_file()) {
    error("no more case");
    return;
  }
  else {
    int bline, bfile, efile;
    
    /* If the user change the cursor position */
    goto_beginning_of_line();
    
    while (! at_end_of_file()) {
      if (find_cmd_trace1(&bline, &bfile, &efile) ||
	  find_cmd_trace2(&bline, &bfile, &efile) ||
	  find_cmd_trace3(&bline, &bfile, &efile) ||
	  find_cmd_trace4(&bline, &bfile, &efile)) {
	int line = 0;
	char filename[128];
	char * pfilename = filename;
	
	/* compute the line number */
	goto_char(bline);
	while (strchr("0123456789", current_char())) {
	  line = line * 10 + current_char() - '0';
	  goto_next_char();
	}
	
	/* take the filename */
	goto_char(bfile);
	do {
	  *pfilename++ = current_char();
	  goto_next_char();
	} while (current_position() != efile);
	*pfilename = 0;
	
	/* go to the next case for the next call */
	
	current_line_to_top();
	goto_end_of_line();
	goto_next_char();
	
	select_window(cwin);
	cmd_from_go_next = 1;
	edit_file_at_line(filename, line);
	cmd_from_go_next = 0;
	return;
      }
      else {
	/* no line indication, go to the next */
	goto_end_of_line();
	goto_next_char();
      }
    }
    error("no more case");
  }
}

{
  key_def("C-mode", "^xg", "go_next");
  key_def("C++mode", "^xg", "go_next");
  key_def("default", "^xg", "go_next");
  key_def("Shell", "^xg", "go_next");
}
