/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: mouse.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/mouse.sc
   Description: 
   Created: Sun Aug  7 16:11:18 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 16:11:20 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################


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
  if (! function("transpose_forms"))
    load_file("mode-ext.sc");
}

/* -------------------------------------------------------------------------
   C-left : delete clicked-on char
   ------------------------------------------------------------------------- */

void left_button_control(int pos) {

  goto_char(pos);
  delete_char();
}

/* -------------------------------------------------------------------------
   C-right : delete from clicked-on char to end of line
   ------------------------------------------------------------------------- */

void right_button_control(int pos) {

  goto_char(pos);

  if (current_position() == end_of_line())
    delete_char();
  else 
    delete_region(pos,end_of_line());
}

/* -------------------------------------------------------------------------
   transpose clicked-on form and preceding form
   ------------------------------------------------------------------------- */

void left_button_shift(int pos) {

  goto_char(pos);
  transpose_forms();
}

/* -------------------------------------------------------------------------
   insert clicked-on form at current position
   ------------------------------------------------------------------------- */

 middle_button_shift(int pos) {

  int origin = current_position();
  char* str;
  
  goto_char(pos);
  str = current_form();
  
  goto_char(origin);
  insert_string(str);
  
  free(str);
}

/* -------------------------------------------------------------------------
   delete clicked-on form
   ------------------------------------------------------------------------- */

void right_button_shift(int pos) {

  goto_char(pos);
  delete_current_form();
}

