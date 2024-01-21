/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: utilities.sc
   Path: /users/system/lib/xcoral/utilities.sc
   Description: 
   Created: Sun Aug  7 16:14:19 MET 1994
   Author: Thierry Emery
   Modified: Fri Dec  2 11:36:06 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: general SMAC programmer and XCORAL user utilities

   Defines: window_substring, insert_chars, transpose_chars,
	    delete_chars, delete_region, delete_to_end_of_line,
	    delete_to_beginning_of_line, delete_previous_char,
	    delete_line_blanks, just_one_blank,
	    center_line_within, center_line,
	    recenter, read_int_from_string, gets_string_cancelled,
	    quit_shell
   
   Suggested bindings: "^xt"  "transpose_chars"
		       "^[\\" "delete_line_blanks"
		       "^[ "  "just_one_blank"
		       "^[k"  "delete_to_beginning_of_line"
		       "^xc"  "center_line"
		       "^[l"  "recenter"
		       "^x^c" "quit_shell"

   Procedure: call or execute the defined functions ...
	      (see comments below)

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
   extract a string in current window from <beg> to <end> (malloc)
   ------------------------------------------------------------------------- */

char* window_substring(int beg, int end) {

  int start=current_position(), len=end-beg, index;
  char* str;
  
  str = (char*) malloc(len+1);
  
  goto_char(beg);
  
  for (index=0;index<len;++index)
    {
      str[index]=current_char();
      goto_next_char();
    }
  str[len] = 0;
  
  goto_char(start);

  return str;
}

/* -------------------------------------------------------------------------
   insert <n> times <repeated_char>
   ------------------------------------------------------------------------- */

void insert_chars(int n, char repeated_char) {

  int n_ins;
  
  for (n_ins=0;n_ins<n;++n_ins)
    insert_char(repeated_char);
}

/* -------------------------------------------------------------------------
   transpose current and preceding char
   ------------------------------------------------------------------------- */

void transpose_chars() {

  char current=current_char();
  
  delete_char();
  goto_previous_char();
  insert_char(current);
}

/* -------------------------------------------------------------------------
   delete <ndel> chars
   ------------------------------------------------------------------------- */

void delete_chars(int ndel) {

  while (ndel--) delete_char();
}

/* -------------------------------------------------------------------------
   delete chars from <beg> to <end>
   ------------------------------------------------------------------------- */

void delete_region(int beg, int end) {

  int origin=current_position();
  
  goto_char(beg);
  delete_chars(end-beg);

  if (origin<beg)
    goto_char(origin);
  else
    if (origin>end)
      goto_char(origin-end+beg);
}

/* -------------------------------------------------------------------------
   delete to end of line
   ------------------------------------------------------------------------- */

void delete_to_end_of_line() {

  delete_chars(end_of_line()-current_position());
}

/* -------------------------------------------------------------------------
   delete back to beginning of line
   ------------------------------------------------------------------------- */

void delete_to_beginning_of_line() {

  delete_region(beginning_of_line(), current_position());
}

/* -------------------------------------------------------------------------
   delete previous char
   ------------------------------------------------------------------------- */

void delete_previous_char() {

  goto_previous_char();
  delete_char();
}

/* -------------------------------------------------------------------------
   delete blank space to left and right
   ------------------------------------------------------------------------- */

void delete_line_blanks() {

  while (previous_char() == ' ' || previous_char() == '\t')
    delete_previous_char();

  while (current_char() == ' ' || current_char() == '\t')
    delete_char();
}

/* -------------------------------------------------------------------------
   delete blank space to left and right, leaving just one blank space
   ------------------------------------------------------------------------- */

void just_one_blank() {

  delete_line_blanks();
  insert_char(' ');
}

/* -------------------------------------------------------------------------
   center line within <width>
   ------------------------------------------------------------------------- */

void center_line_within(int width) {

  int tab_width = 8, line_width, indent, n_tabs, n_spaces, n_ins;
  
  goto_end_of_line();
  delete_line_blanks();
  goto_beginning_of_line();
  delete_line_blanks();
  
  line_width = end_of_line() - current_position();
  
  if (line_width < width)
    {
      goto_beginning_of_line();
      indent = (width - line_width) / 2;
      n_tabs = indent / tab_width;
      for (n_ins=0;n_ins<n_tabs;++n_ins)
	insert_char('\t');
      n_spaces = indent % tab_width;
      for (n_ins=0;n_ins<n_spaces;++n_ins)
	insert_char(' ');
    }
}

/* -------------------------------------------------------------------------
   center line within width of 80
   ------------------------------------------------------------------------- */

void center_line() {

  center_line_within(80);
}

/* -------------------------------------------------------------------------
   recenter current position in window
   ------------------------------------------------------------------------- */

void recenter() {

  int origin=current_position(), up_lines=window_height()/2;
  
  while (up_lines--) goto_previous_line();
  current_line_to_top();
  
  goto_char(origin);
}

/* -------------------------------------------------------------------------
   reads an int from a string
   ------------------------------------------------------------------------- */

int read_int_from_string(char* str) {

  int index=0, result=0, negative_p=0;
  char c;

  for (; index<strlen(str); ++index) {
    c = str[index];
    if (c != ' ' && c != '\t')
      break;
  }

  if (str[index] == '-') {
    negative_p = 1;
    ++index;
  }

  for (; index<strlen(str); ++index) {
    c = str[index];
    if (c >= '0' && c <= '9') {
      result *= 10;
      result += (c - '0');
    }
    else
      break;
  }

  if (negative_p)
    return -result;
  else
    return result;
}

/* -------------------------------------------------------------------------
   checks that a string returned by gets is valid (not empty and not cancelled)
   ------------------------------------------------------------------------- */

int gets_string_cancelled(char* str) {

  return (!str || strlen(str) == 0
	  || (strlen(str) == 1 && str[0] == ((char) 7)));
}

/* -------------------------------------------------------------------------
   sleep n seconds
   ------------------------------------------------------------------------- */

void sleep(s)
    int s;
{
    if (s <= 0)
      return;
    while(s--) {
	usleep(500000);
	usleep(500000);
    }
}

/* -------------------------------------------------------------------------
   checks that a string is numeric (1.2 is valid)
   ------------------------------------------------------------------------- */
int is_num(char *str)
{
    int len;
    
    if (!str || strlen(str)==0)
      return 0;
    len = strlen(str);
    while(len) {
	if (strchr(".0123456789",str[len-1])) {
	    len --;
	    if (len)
	      continue;
	    else
	      break;
	}
	else {
	  return 0;
	}
    }
    return 1;
}

char* basename (char* file_name) {

    char *command, *base_name;

    if (file_name == 0)
      return 0;
    
    command = (char*) malloc(strlen(file_name)+10);
    sprintf(command,"basename %s",file_name);

    base_name = cmd_shell_to_string(command);

    free(command);
    return base_name;
}

char* dirname (char* file_name) {

    char *command, *dir_name;

    command = (char*) malloc(strlen(file_name)+10);
    sprintf(command,"dirname %s",file_name);

    dir_name = cmd_shell_to_string(command);

    free(command);
    return dir_name;
}


void debug_display(char *mess, int x)
{
  char *tmp = (char *) malloc(256);

  if(mess)
    sprintf(tmp,"____%s____\n%d\n", mess, x);
  else
    sprintf(tmp,"____(null)____\n%d\n", x);

  display_message(tmp,"SHOW", 1);
  free(tmp);
}

int does_unix_command_exist(char *unix_cmd)
{
    char *result;
    char *tmp;
    int val = 0;
    
    if (unix_cmd == 0)
      return 0;
    
    tmp = (char *) malloc(64);
    sprintf(tmp,"which %s", unix_cmd);
    result = cmd_shell_to_string(tmp);
    
    if(result) {
	if(result[0] == '.' || result[0] == '/')
	    val = 1;
	else
	  val = 0;
	free(result);
    }
    free(tmp);
    return val;
}

/* -------------------------------------------------------------------------
   check if a file exist.
   ------------------------------------------------------------------------- */

int does_file_exist (char *filename)
{
    char *tmp = (char *) malloc (256);
    char *result;
    int val;

    sprintf ( tmp,"ls %s 2>&1\n", filename ); 
    result = cmd_shell_to_string (tmp);
    if ( result != 0 ) {
	if (strlen(result) != strlen(filename))
	    val = 0;
	else
	  val = 1;
    }
    else
      val = 0;

    free(tmp);
    free(result);
    return(val);
}

void print_ascii ()
{
    int i;
    for(i=0;i<256;i++) {
	wprintf("%d = %c\n", i, i);
    }
}

void change_return_to_newline()
{
  int c;
  
  while(! at_end_of_file()){
    c = current_char();
    if(c == '\r') {
      delete_char();
      insert_char('\n');
    }
    goto_next_char();
  }
}

/* --------------------------------------------------------------------------------
   Concat
   -------------------------------------------------------------------------------- */

char *concat2 (char *s1, char *s2)
{
    char *s = (char*) malloc ((strlen(s1)+strlen(s2) +1));
    sprintf ( s, "%s%s", s1, s2 );
    return s;
}
char *concat3 (char *s1, char *s2, char *s3)
{
    char *s = (char*) malloc ((strlen(s1)+strlen(s2)+strlen(s3) +1));
    sprintf ( s, "%s%s%s", s1, s2, s3 );
    return s;
}
char *concat4 (char *s1, char *s2, char *s3, char *s4)
{
    char *s = (char*) malloc ((strlen(s1)+strlen(s2)+strlen(s3)+strlen(s4) +1));
    sprintf ( s, "%s%s%s%s", s1, s2, s3, s4 );
    return s;
}

char *concat (char *s1, char *s2)
{ 
    concat2(s1, s2);
}

char *substring (char *s, int n, int m)
{
    /* n = position dans s et m = nb caracteres 
       exemple : substring ("chapeau", 2, 3) = "ape"
    */
    char * tmp;
    
    if (s==0 || (m==0))
      return 0;
    tmp = (char *) malloc (m + 1);
    strncpy (tmp, s+n, m);
    tmp [m] = 0;
    return tmp;
}

/* --------------------------------------------------------------------------------
   file type
   -------------------------------------------------------------------------------- */

char* file_type (char* filename) {

  char *command="file %s | sed -e \"s|.*:[^a-zA-Z]*||g\"", *full_command, *filetype;

  full_command = (char*) malloc(strlen(command)+strlen(filename)+1);
  sprintf(full_command,command,filename);
  filetype = cmd_shell_to_string(full_command);

  free(full_command);
  return filetype;
}

/* --------------------------------------------------------------------------------
   is dir
   -------------------------------------------------------------------------------- */

int is_dir (char* filename) {

  char* filetype=file_type(filename);
  int is_dir;

  is_dir = (!strcmp(filetype,"directory"));

  free(filetype);
  return is_dir;
}

/* --------------------------------------------------------------------------------
   is file
   -------------------------------------------------------------------------------- */
int is_file (char* filename) {

  char* filetype=file_type(filename);
  int is_file;

  is_file = (strcmp(filetype,"directory") && !strstr(filetype,"link") && 
	     !strstr(filetype,"socket") && !strstr(filetype,"special"));

  free(filetype);
  return is_file;
}

/* -----------------------------------------------------------------------------------
   Wait until a file is present or not.
   -----------------------------------------------------------------------------------*/
void wait_for_file_internal (char *filename, int exist)
{
    char *tmp = (char *) malloc (256);
    char *result;

    sprintf ( tmp,"ls %s 2>&1\n", filename );
    while (1) {
	usleep(500000); /* 1/2 seconde */
	result = cmd_shell_to_string (tmp);
	if ( result != 0 ) {
	    if (! exist) { /* no file */
	      if (strlen(filename) < strlen(result)) /* Ya 'not found' */
	      break;
	    }
	    else {
		if (strlen(result) == strlen(filename))
		  break;
	    }
	}
	else
	  break; /* popen error */
    }
    free(result);
    free(tmp);
}


/* -----------------------------------------------------------------------------------
   wait until a file is not present
   -----------------------------------------------------------------------------------*/
void wait_for_nofile(char *filename)
{
    wait_for_file_internal(filename, 0);
}

/* -----------------------------------------------------------------------------------
   Wait until a file is present
   -----------------------------------------------------------------------------------*/
void wait_for_file (char *filename)
{
    wait_for_file_internal(filename,1);
}

/* -----------------------------------------------------------------------------------
   Quit shell window (bind to ^x^c in Shell mode)
   -----------------------------------------------------------------------------------*/
void quit_shell () {

  set_mode("Default");
  kill_current_buffer();
  kill_window(current_window());
}

/* -----------------------------------------------------------------------------------
   Tue les fenetres des buffers non modifies
   -----------------------------------------------------------------------------------*/
void purge()
{
  int i;
  
  for (i = 0; i != 32 ; i += 1) {
    if ((select_window(i) != -1) &&
	(! current_buffer_is_modified()))
      kill_window(i);
  }
}

/* --------------------------------------------------------------------------------
   indent_region 
   -------------------------------------------------------------------------------- */
void c_indent_region();
void latex_indent_region();

indent_region()
{
    char *mode_name = current_mode();
    
    if ((strcmp(mode_name,"C-mode") == 0)
	||(strcmp(mode_name,"C++mode") == 0)) {
	c_indent_region();
    }
    else if ((strcmp(mode_name,"Latex") == 0)) {
	latex_indent_region();
    }
    else
      /* Other modes */
      ;

    if(mode_name)
      free(mode_name);
}

/* -------------------------------------------------------------------------
   number of lines of a file
   ------------------------------------------------------------------------- */

int file_lines (char* file_name) {

    char *command, *result;
    int lines;

    command = (char*) malloc(strlen(file_name)+64);
    sprintf(command,"wc -l %s 2>/dev/null | sed -e 's/[         a-zA-Z]//g'",file_name);

    result = cmd_shell_to_string(command);

    if (result) {
        if (is_num(result))
          lines = read_int_from_string(result);
        else
          lines = 0;
        free(result);
    }
    
    free(command);
 
    return lines;
}

/* ------------------------------------------------------------------------
   looking at a regexp ?
   ------------------------------------------------------------------------ */

int looking_at(char* regexp) {

  int origin=current_position(), result;
  
  result = (re_forward_search(regexp) >= 0 && re_match_beginning(0) == origin);
  goto_char(origin);
  
  return result;
}
