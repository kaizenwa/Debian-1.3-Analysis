/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: mode-ext.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/mode-ext.sc
   Description: 
   Created: Sun Aug  7 16:09:33 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 16:09:35 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: extensions of C and C++ modes
   
   Requires: mode.sc
	     utilities.sc

   Defines: next_form, next_form_and_delete, current_form,
	    delete_current_form, transpose_forms, blink_paren

   Suggested bindings: "^xy" transpose_forms
                       "^]z" blink_paren

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
  if (! function("goto_beginning_of_c_definition"))
    load_file("mode.sc");
  if (! function("window_substring"))
    load_file("utilities.sc");
}

/* -------------------------------------------------------------------------
   extract and optionnally (depending on <delete_p>) delete next form (malloc)
   ------------------------------------------------------------------------- */

char* next_form_delete_opt(int delete_p) {

  int origin=current_position(),beg,end;
  char* form;

  forward_c_form();
  end = current_position();
  backward_c_form();
  beg = current_position();
  form = window_substring(beg,end);
  if (delete_p)
    delete_region(beg,end);
  
  goto_char(origin);
  
  return form;
}

/* -------------------------------------------------------------------------
   extract next form (malloc)
   ------------------------------------------------------------------------- */

char* next_form() {

  return next_form_delete_opt(0);
}

/* -------------------------------------------------------------------------
   extract and delete next form (malloc)
   ------------------------------------------------------------------------- */

char* next_form_and_delete() {

  return next_form_delete_opt(1);
}

/* -------------------------------------------------------------------------
   current form (malloc)
   ------------------------------------------------------------------------- */

char* current_form() {

  forward_c_form();
  backward_c_form();

  return next_form();
}

/* -------------------------------------------------------------------------
   delete current form
   ------------------------------------------------------------------------- */

void delete_current_form() {

  forward_c_form();
  backward_c_form();
  delete_next_c_form();
}

/* -------------------------------------------------------------------------
   transpose current and preceding form
   ------------------------------------------------------------------------- */

void transpose_forms() {

  char* current;
  
  forward_c_form();
  backward_c_form();
  current=next_form_and_delete();
  backward_c_form();
  insert_string(current);
  free(current);
}


/* -------------------------------------------------------------------------
   blink at matching parenthesis
   ------------------------------------------------------------------------- */

void blink_paren() {

  int origin=current_position(), blpos;
  
  if (strchr(")}]", previous_char())) {
    backward_c_form();
    blpos = current_position();
    goto_char(origin);
    blink(blpos);
  }
}
