/* ########################################################################

                 SMAC FILE USED BY XCORAL EDITOR

   File: title.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/title.sc
   Description: 
   Created: Sat Jul 30 11:49:59 MET 1994
   Author: Thierry Emery
   Modified: Sat Aug  6 14:22:19 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   
   ########################################################################

   Note: file title and custom organization title

   Requires: utilities.sc
   
   User interface: title, shell_title, imake_title, update_title,
		   custom_title, custom_shell_title,
		   custom_imake_title, smac_title.

   SMAC programmer interface: 
  
       insert_title_xCS_info
       insert_general_title, insert_title,
       insert_shell_title, insert_imake_title,
       smac_title, custom_title, custom_shell_title.
  
   Procedure: 
  
       - change if desired the value of insert_title_xCS_info
       - to insert a file title, execute `insert_general_title',
       `insert_title', `insert_shell_title', `insert_imake_title',
       `smac_title', `title', or a function of yours which calls
       one of these.

       - to update the `File', `Path', `Modified' and
       `Last maintained by' fields, execute `update_title'

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

{
  if (! function("delete_region"))
    load_file("utilities.sc");
  if (! function("hack_file_name"))
    load_file("hack-filename.sc");
}

/* ------------------------------------------------------------------------
   Default Copyleft
   ------------------------------------------------------------------------ */
char *gnu_string = "   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n";

/* -------------------------------------------------------------------------
   Copyright file inserted in place of gnu_string if not 0
   ------------------------------------------------------------------------- */
char* CopyrightFile = 0;

/* -------------------------------------------------------------------------
   Default organization and copyright
   ------------------------------------------------------------------------- */
char* custom_organization = "XCORAL GALACTIC COMPANY\n 99999 Stars City\nOrion nebula";
char* custom_copyright = "(c) Copyright 1994\nXcoral Galactic Company";


/* -------------------------------------------------------------------------
   insert_title_xCS_info : redefinable variable for RCS (or even SCCS) info
   (possibly on several lines) - the default values are split up because this
   file is under RCS
   ------------------------------------------------------------------------- */

char *insert_title_short_xCS_info = (char*) malloc(25);
char *insert_title_long_xCS_info = (char*) malloc(64);
char *insert_title_xCS_info;

{
    sprintf(insert_title_short_xCS_info,"RCS $%s$ $%s$\n", "Revision","State");
    sprintf(insert_title_long_xCS_info,
	    "%sChecked-in by $%s$ on $%s$\n$%s$\n",
	    insert_title_short_xCS_info,"Author","Date","Log");
    insert_title_xCS_info = insert_title_short_xCS_info;
    /* insert_title_xCS_info = insert_title_long_xCS_info */
}

/* -------------------------------------------------------------------------
   redefinable variable for Imakefile comment (supposes #define XCOMM #)
   ------------------------------------------------------------------------- */

char *imake_comment_macro = "XCOMM ";

/* -------------------------------------------------------------------------
   insert file base name (with extension)
   ------------------------------------------------------------------------- */

void insert_basename(char* file_name) {
    
    int origin = current_position(), beg, end;
    insert_string(file_name);
    end = current_position();
    backward_search("/");
    beg = current_position()+1;
    delete_region(origin, beg);
    goto_char(origin+end-beg);
}

/* -------------------------------------------------------------------------
   insert file name (with call to hack_file_name for logical path)
   ------------------------------------------------------------------------- */

void insert_filename(char* file_name) {
    
    insert_string(file_name);
    hack_file_name(); /* redefinable file name hack */
}

/* -------------------------------------------------------------------------
   insert user's full name
   ------------------------------------------------------------------------- */

void insert_user_full_name() {
  
  char *yp_server=cmd_shell_to_string("ypwhich"),
  *yp_running=cmd_shell_to_string("ypwhich | grep 'not running'");
  
  if ( yp_server == 0 || (yp_running != 0 && strcmp(yp_running,"")))
    cmd_shell("cat /etc/passwd | awk -F: '($1==\"'\"$USER\"'\") {print $5;}'");
  else
    cmd_shell("ypcat passwd | awk -F: '($1==\"'\"$USER\"'\") {print $5;}'");
  
  if (the_char(current_position()-1) == 10)
    delete_previous_char(); /* On enleve le return */
  
  if (yp_server)
    free(yp_server);
  if (yp_running)
    free(yp_running);
}

/* -------------------------------------------------------------------------
   insert date
   ------------------------------------------------------------------------- */

void insert_date() {
    
    cmd_shell("date | sed -e \"s/ DST//g\"");
    delete_previous_char(); /* On enleve le return */
   
}

/* -------------------------------------------------------------------------
   insert a title separator line
   ------------------------------------------------------------------------- */

void insert_title_separator(char* comment_start, int multi_line_p,
			    char line_char, int width) {
    
    if (!multi_line_p)
      insert_string(comment_start);
    else
      insert_chars(strlen(comment_start),' ');
    insert_chars(width, line_char);
    insert_char('\n');
}

/* -------------------------------------------------------------------------
   insert a title `blank' line
   ------------------------------------------------------------------------- */

void insert_title_blank(char* comment_start, int multi_line_p, int width) {
    
    if (!multi_line_p)
      insert_string(comment_start);
    insert_char('\n');
}

/* -------------------------------------------------------------------------
   insert a title line beginning with <str>
   ------------------------------------------------------------------------- */

void insert_title_line_start(char* str, char* comment_start,
			     int multi_line_p) {
    
    if (multi_line_p)
      insert_chars(strlen(comment_start),' ');
    else
      insert_string(comment_start);
    insert_string(str);
}

/* -------------------------------------------------------------------------
   insert title lines, optionally centered
   ------------------------------------------------------------------------- */

void insert_title_lines(char* str, char* comment_start, char* comment_end,
			int multi_line_p, int centered_p, int width) {
    
    int origin=current_position(), end;
    
    insert_string(str);
    insert_char('\n');
    end = current_line();
    
    goto_char(origin);
    goto_beginning_of_line();
    while(current_line() < end) {
	if (centered_p)
	  center_line_within(width);
	else
	  if (comment_end && strlen(comment_end) > 0)
	    insert_chars(strlen(comment_start),' ');
	if (!multi_line_p) {
	    goto_beginning_of_line();
	    insert_string(comment_start);
	}
	goto_next_line();
	goto_beginning_of_line();
    }
    goto_line(end);
    goto_beginning_of_line();
}

/* -------------------------------------------------------------------------
   insert title at beginning of file (similar to the one above), centering
   <name>, including author and creation date, using <comment_start> and
   <comment_end>, and using char <line_char> to build separator lines of
   width <width>
   ------------------------------------------------------------------------- */

void insert_general_title(char* name, char* organization, char* copyright,
			  char* comment_start, char* comment_end,
			  int multi_line_p, char line_char, int width) {
    
    char* file_name=filename();
    
    goto_char(0);
    insert_title_separator(comment_start,0,line_char,width);
    insert_title_blank(comment_start,multi_line_p,width);
    
    insert_title_lines(name,comment_start,comment_end,multi_line_p,1,width);
    insert_title_blank(comment_start,multi_line_p,width);
    
    if (organization && strlen(organization) > 0) {
	insert_title_lines(organization,comment_start,comment_end,multi_line_p, 1,width);
	insert_title_blank(comment_start,multi_line_p,width);
    }
    
    insert_title_line_start("File: ",comment_start,multi_line_p);
    if (file_name)
      insert_basename(file_name);
    insert_char('\n');
    
    insert_title_line_start("Path: ",comment_start,multi_line_p);
    if (file_name) {
	insert_filename(file_name);
    }
    insert_char('\n');
    
    insert_title_line_start("Description: \n",comment_start,multi_line_p);
    
    insert_title_line_start("Created: ",comment_start,multi_line_p);
    insert_date();
    insert_char('\n');
    
    insert_title_line_start("Author: ",comment_start,multi_line_p);
    insert_user_full_name();
    insert_char('\n');
    
    insert_title_line_start("Modified: ",comment_start,multi_line_p);
    insert_date();
    insert_char('\n');
    
    insert_title_line_start("Last maintained by: ",comment_start,multi_line_p);
    insert_user_full_name();
    insert_char('\n');
    insert_title_blank(comment_start,multi_line_p,width);
    
    insert_title_lines(insert_title_xCS_info,comment_start,comment_end,multi_line_p,0,width);
    insert_title_blank(comment_start,multi_line_p,width);
    
    if (copyright) {
	insert_title_lines(copyright,comment_start,comment_end,multi_line_p,1,width);
	insert_title_blank(comment_start,multi_line_p,width);
    }
    
    insert_title_separator(comment_start,multi_line_p,line_char,width);
    insert_title_blank(comment_start,multi_line_p,width);
    insert_title_line_start("Note: \n",comment_start,multi_line_p);
    insert_title_blank(comment_start,multi_line_p,width);
    
    insert_title_separator(comment_start,multi_line_p,line_char,width);
    insert_char('\n');

    if(organization == 0) {
	insert_string ("   Copyright (c) : ");
	insert_user_full_name();
	insert_char('\n');
	insert_char('\n');
	insert_title_separator(comment_start,multi_line_p,line_char,width);
    }
    else {
	delete_previous_char();
	delete_previous_char();
    }
         
    if (multi_line_p) {
	delete_previous_char();
	insert_string(comment_end);
    }
    
    insert_chars(2,'\n');
    
    if (file_name)
      free(file_name);
}

/* -------------------------------------------------------------------------
   insert a multi-line title at beginning of file (similar to the one above),
   centering <name>, including author and creation date, with C/C++ type
   comment separators, using char <line_char> to build separator lines of
   width <width>
   ------------------------------------------------------------------------- */

void insert_title(char* name, char* organization, char* copyright,
		  char line_char, int width) {
    
    insert_general_title(name,organization,copyright,
			 "/* "," */",1,line_char,width);
}

/* -------------------------------------------------------------------------
   adds gnu_string or contents of CopyrightFile and goes to first line after
   title
   ------------------------------------------------------------------------- */
void add_copyright_file()
{
  goto_char(0);
  if(forward_search("Copyright (c) :")) {
    goto_next_line();
    goto_beginning_of_line();
    insert_char('\n');
    if(CopyrightFile == 0)
      insert_string(gnu_string);
    else {
      insert_file(CopyrightFile);
      goto_line(current_line()+file_lines(CopyrightFile));
    }
    goto_line(current_line()+3);
    goto_beginning_of_line();
  }
}

/* -------------------------------------------------------------------------
   insert a multi-line title at beginning of file (similar to the one above),
   centering <name>, including author and creation date, with shell type
   comment separators, using char <line_char> to build separator lines of
   width <width>
   ------------------------------------------------------------------------- */

void insert_shell_title(char* name, char* organization, char* copyright,
		       char line_char, int width) {
    
    insert_general_title(name,organization,copyright,
			 "# ",0,0,line_char,width);
}

/* -------------------------------------------------------------------------
   insert a multi-line title at beginning of file (similar to the one above),
   centering <name>, including author and creation date, with Imakefile type
   comment separators, using char <line_char> to build separator lines of
   width <width>
   ------------------------------------------------------------------------- */

void insert_imake_title(char* name, char* organization, char* copyright,
		        char line_char, int width) {
    
    insert_general_title(name,organization,copyright,
			 imake_comment_macro,0,0,line_char,width);
}

/* -------------------------------------------------------------------------
   insert SMAC title at beginning of file
   ------------------------------------------------------------------------- */

void smac_title() {
    
    insert_title("SMAC FILE USED BY XCORAL EDITOR",0,0,'#',72);
    goto_char(0);
    forward_search("Note:");
    goto_next_line();
    insert_string("\n   Requires: \n\n   Defines: \n");
    insert_string("\n   Suggested bindings: \n\n   Procedure: \n");
    
    add_copyright_file();
}

/* -------------------------------------------------------------------------
   insert named title at beginning of file
   ------------------------------------------------------------------------- */

void title(char* name)
{
    insert_title(name,0,0,'#',72);
    add_copyright_file();
}


/* -------------------------------------------------------------------------
   custom title
   ------------------------------------------------------------------------- */

void custom_title(char* str) {
    
    insert_title(str,custom_organization,custom_copyright,'#',72);
}

/* -------------------------------------------------------------------------
   Makefile/Shell/Unix title
   ------------------------------------------------------------------------- */
void shell_title(char* str) {

    int title_end_line;
    
    insert_shell_title(str, 0, 0,'#', 72);
    goto_char(0);
    add_copyright_file();
    title_end_line = current_line()-1;
    goto_char(0);
    while(current_line() != title_end_line) {
	if(current_char() != '#')
	  insert_char('#');
	goto_next_line();
	goto_beginning_of_line();
    }
    goto_line(title_end_line+2);
    goto_beginning_of_line();
}

/* -------------------------------------------------------------------------
   custom Makefile/Shell/Unix title
   ------------------------------------------------------------------------- */

void custom_shell_title(char* str) {
    insert_shell_title(str,custom_organization,custom_copyright,'#', 72);
}

/* -------------------------------------------------------------------------
   Imakefile title
   ------------------------------------------------------------------------- */

void imake_title(char* str) {

    int title_end_line;
    
    insert_imake_title(str, 0, 0,'#', 72);
    goto_char(0);
    add_copyright_file();
    title_end_line = current_line()-1;
    goto_char(0);
    while(current_line() != title_end_line) {
	if (!looking_at(imake_comment_macro))
	  insert_string(imake_comment_macro);
	goto_next_line();
	goto_beginning_of_line();
    }
    goto_line(title_end_line+2);
    goto_beginning_of_line();
}

/* -------------------------------------------------------------------------
   custom Imakefile title
   ------------------------------------------------------------------------- */

void custom_imake_title(char* str) {
    insert_imake_title(str,custom_organization,custom_copyright,'#', 72);
}

/* -------------------------------------------------------------------------
   update File, Path, Modified and Last maintained by
   
   hack_file_name() can be redefined to modify the inserted filename, for
   instance to restore the logical path from the automount path (see
   default-hack-filename.sc)
   ------------------------------------------------------------------------- */

void update_title() {
    
    int origin=current_position(), beg;
    char* file_name=filename();
    
    goto_char(0);
    if (forward_search("Note:")) {
	
	if (backward_search("Last maintained by:")) {
	    forward_search(": ");
	    goto_char(current_position()+2);
	    delete_to_end_of_line();
	    insert_user_full_name();
	}
	
	if (backward_search("Modified:")) {
	    forward_search(": ");
	    goto_char(current_position()+2);
	    delete_to_end_of_line();
	    insert_date();
	}
	
	if (file_name && backward_search("Path:")) {
	    forward_search(": ");
	    goto_char(current_position()+2);
	    delete_to_end_of_line();
	    insert_filename(file_name);
	}
	
	if (file_name && backward_search("File:")) {
	    forward_search(": ");
	    beg = current_position()+2;
	    goto_char(beg);
	    delete_to_end_of_line();
	    insert_basename(file_name);
	}
    }
    
    goto_char(origin);
}
