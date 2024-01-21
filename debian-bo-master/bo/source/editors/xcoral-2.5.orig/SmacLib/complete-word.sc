/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: complete-word.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/complete-word.sc
   Description: 
   Created: Sun Aug  7 16:04:37 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 16:04:39 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: word completion

   Requires: utilities.sc
	     mode.sc

   Defines: complete_word

   Suggested bindings: "^[/" "complete_word"

   Procedure: hit Esc / as many times as necessary to make adequate
	      completion appear

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
  if (! function("delete_region"))
    load_file("utilities.sc");
}

/* -------------------------------------------------------------------------
   global variable for window in which complete_word was last used
   ------------------------------------------------------------------------- */

int complete_word_window=-1;

/* -------------------------------------------------------------------------
   global variable for last origin
   ------------------------------------------------------------------------- */

int complete_word_origin=-1;

/* -------------------------------------------------------------------------
   global variable for last occurrence position
   ------------------------------------------------------------------------- */

int complete_word_occurrence_pos=-1;

/* -------------------------------------------------------------------------
   global variable for last completion tried
   ------------------------------------------------------------------------- */

char* complete_word_completion=0;

/* -------------------------------------------------------------------------
   word completion function : search backward for incomplete word in current
   window, memorizing last try and avoiding immediate repeat of same candidate
   ------------------------------------------------------------------------- */

void complete_word() {

    char *incomplete_word, *completion_candidate;
    int  completion_length=(complete_word_completion ?
			    strlen(complete_word_completion) : 0),
	 completion_found=0, completion_start;

    if (! strchr(chars_of_word, previous_char())) return;

    if (current_window() != complete_word_window) {
	complete_word_window=current_window();
	complete_word_origin=-1;
    }

    if (current_position() < completion_length
	|| complete_word_origin != (current_position()-completion_length)) {
	complete_word_origin=current_position();
	complete_word_occurrence_pos=-1;
	if (complete_word_completion != 0) {
	    free(complete_word_completion);
	    complete_word_completion = 0;
	    completion_length = 0;
	}
    }
    else {
	delete_region(complete_word_origin,current_position());
	goto_char(complete_word_origin);
    }
    
    backward_word();
    incomplete_word = window_substring(current_position(),complete_word_origin);

    if (complete_word_occurrence_pos != -1)
      goto_char(complete_word_occurrence_pos);

    while (!completion_found && backward_search(incomplete_word)) {
	if (! strchr(chars_of_word, previous_char())) {
	    complete_word_occurrence_pos = current_position();
	    completion_start = complete_word_occurrence_pos
			       + strlen(incomplete_word);
	    goto_char(completion_start);
	    if (strchr(chars_of_word, current_char()))
	      forward_word();
	    completion_candidate =
	      window_substring(completion_start, current_position());
	    if (strlen(completion_candidate) != 0)
	      completion_found =
		(completion_length == 0 ||
		 strcmp(completion_candidate, complete_word_completion) != 0);
	    if (! completion_found) {
		free(completion_candidate);
		goto_char(complete_word_occurrence_pos);
	    }
	}
    }

    if (completion_found) {
	if (complete_word_completion != 0) { free(complete_word_completion);
					     complete_word_completion = 0; }
	complete_word_completion = completion_candidate;
	goto_char(complete_word_origin);
	insert_string(complete_word_completion);
    }
    else {
	complete_word_occurrence_pos=-1;
	if (complete_word_completion != 0) { free(complete_word_completion);
					     complete_word_completion = 0; }
	goto_char(complete_word_origin);
	display_message("No other completion... Back to start","Message",1);
    }

    free(incomplete_word);
    return;
}

