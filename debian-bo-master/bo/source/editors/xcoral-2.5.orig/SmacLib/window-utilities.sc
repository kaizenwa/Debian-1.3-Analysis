/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: window-utilities.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/window-utilities.sc
   Description: 
   Created: Sun Aug  7 16:16:11 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 16:16:13 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: window utilities

   Defines: choose_window, current_position_in, end_of_file_in, the_char_in,
	    goto_char_in

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
  if (! function("read_int_from_string"))
    load_file("utilities.sc");
}

/* -------------------------------------------------------------------------
   Window choice
   ------------------------------------------------------------------------- */

int choose_window(char* msg) {

  int win;
  char *item, *file;
  
  clear_list();

  for (win=0; win<32; ++win) {
    if (select_window(win) >= 0) {
      file = filename();
      if (file) {
	item = (char*) malloc(strlen(file)+4);
	sprintf(item,"%d %s",win,file);
	free(file);
	add_list_item(item);
      }
    }
  }

  item = select_from_list(msg);

  if (item)
    win = read_int_from_string(item);
  else
    win = -1;

  clear_list();

  return win;
}

/* -------------------------------------------------------------------------
   returns current position in window <win>
   ------------------------------------------------------------------------- */

int current_position_in(int win) {

  int origin_win=current_window(), pos;

  select_window(win);
  pos = current_position();
  select_window(origin_win);

  return pos;
}

/* -------------------------------------------------------------------------
   returns end of file position in window <win>
   ------------------------------------------------------------------------- */

int end_of_file_in(int win) {

  int origin_win=current_window(), pos;

  select_window(win);
  pos = end_of_file();
  select_window(origin_win);

  return pos;
}

/* -------------------------------------------------------------------------
   returns the char <pos> in window <win>
   ------------------------------------------------------------------------- */

char the_char_in(int win, int pos) {

  int origin_win=current_window();
  char c;

  select_window(win);
  c = the_char(pos);
  select_window(origin_win);

  return c;
}

/* -------------------------------------------------------------------------
   goes to char <pos> in window <win>
   ------------------------------------------------------------------------- */

void goto_char_in(int win, int pos) {

  int origin_win=current_window();

  select_window(win);
  goto_char(pos);
  select_window(origin_win);

  return;
}

