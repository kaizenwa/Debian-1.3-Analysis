/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: compare-win.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/compare-win.sc
   Description: 
   Created: Sun Aug  7 16:02:16 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 16:02:17 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: compare two windows

   Requires: basics.sc, window-utilities.sc

   Defines: Compare, CompareAgain

   Suggested bindings: ^xC for Compare_again()

   Procedure: position cursor successively in the two windows to be compared,
	      then execute `Compare' from one. Alter the windows or move
	      cursor in either or both in order to execute `CompareAgain'

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
  if (! function("current_position_in"))
    load_file("window-utilities.sc");
}

/* -------------------------------------------------------------------------
   Compared windows
   ------------------------------------------------------------------------- */

int Compare_compared_window=-1, Compare_reference_window=-1;

/* -------------------------------------------------------------------------
   Internal mechanics
   ------------------------------------------------------------------------- */

int Compare_compare_internal() {
  
  int compared_pos, reference_pos, compared_end, reference_end;
  
  watch_on();

  compared_pos = current_position_in(Compare_compared_window);
  reference_pos = current_position_in(Compare_reference_window);
  compared_end = end_of_file_in(Compare_compared_window);
  reference_end = end_of_file_in(Compare_reference_window);
  
  while (compared_pos <= compared_end
	 && reference_pos <= reference_end
	 && (the_char_in(Compare_compared_window,compared_pos)
	     == the_char_in(Compare_reference_window,reference_pos))) {
    goto_char_in(Compare_compared_window,++compared_pos);
    goto_char_in(Compare_reference_window,++reference_pos);
  }
  
  watch_off();

  if (compared_pos < compared_end && reference_pos < reference_end)
    return -1;
  else
    return 0;
}

/* -------------------------------------------------------------------------
   Start comparing current window to another one
   ------------------------------------------------------------------------- */

void Compare() {
  
  Compare_compared_window = current_window();
  Compare_reference_window = choose_window("Compare to");
  
  if (Compare_reference_window > 0
      && Compare_reference_window != Compare_compared_window)
    if (Compare_compare_internal() < 0)
      error("Difference detected");
}

/* -------------------------------------------------------------------------
   Resume comparing current window to other one
   ------------------------------------------------------------------------- */

void CompareAgain() {

  if (select_window(Compare_reference_window) >= 0
      && select_window(Compare_compared_window) >= 0)
    if (Compare_compare_internal() < 0)
      error("Difference detected");
}


