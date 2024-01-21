/* ########################################################################

			       ie_func.h

   File: ie_func.h
   Path: /home/fournigault/c/X11/xcoral-2.31/ie_func.h
   Description: 
   Created: Fri Jan 27 11:07:52 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:07:53 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

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


#ifndef _IE_FUNC_H_
#define _IE_FUNC_H_

#include "proto_decl.h"

FCT (int, ie_at_end_of_file, (Text *text) );
FCT (int, ie_backward_search, (Text *text, char *str) );
FCT (int, ie_beginning_of_line, (Text *text) );
FCT (void, ie_blink, (Text *text, int pos) );
FCT (int, ie_cmd_shell, (Text *text, char *str) );
FCT (Mode *, ie_create_mode, (Text *text, char *mode_name) );
FCT (char, ie_current_char, (Text *text) );
FCT (int, ie_current_line, (Text *text) );
FCT (void, ie_current_line_to_top, (Text *text) );
FCT (int, ie_current_position, (Text *text) );
FCT (int, ie_current_window, (Text *text) );
FCT (void, ie_delete_char, (Text *text) );
FCT (int, ie_end_of_file, (Text *text) );
FCT (int, ie_end_of_line, (Text *text) );
FCT (int, ie_end_of_region, (Text *text) );
FCT (char *, ie_file_select, (Text *text) );
FCT (char *, ie_filename, (Text *text) );
FCT (int, ie_forward_search, (Text *text, char *str) );
FCT (int, ie_global_replace, (Text *text, char *old, char *new) );
FCT (void, ie_goto_beginning_of_line, (Text *text) );
FCT (void, ie_goto_char, (Text *text, int p) );
FCT (void, ie_goto_end_of_file, (Text *text) );
FCT (void, ie_goto_end_of_line, (Text *text) );
FCT (void, ie_goto_next_char, (Text *text) );
FCT (void, ie_goto_previous_char, (Text *text) );
FCT (void, ie_insert_char, (Text *text, int c) );
FCT (int, ie_insert_file, (Text *text, char *filename) );
FCT (void, ie_key_def, (Text *text, char *mode_name, char *keys, char *func_name) );
FCT (void, ie_kill_current_buffer, (Text *text) );
FCT (int, ie_kill_window, (Text *text, int num) );
FCT (int, ie_last_key, (Text *text) );
FCT (int, ie_line_count, (Text *text) );
FCT (void, ie_lower_window, (Text *text) );
FCT (int, ie_mark_position, (Text *text) );
FCT (int, ie_msearch, (Text *text, char *chars, int end, int direction) );
FCT (int, ie_new_window, (Text *text) );
FCT (char, ie_next_char, (Text *text) );
FCT (char, ie_previous_char, (Text *text) );
FCT (int, ie_read_file, (Text *text, char *filename) );
FCT (void, ie_redisplay, (Text *text) );
FCT (void, ie_replace_char, (Text *text, int c) );
FCT (void, ie_save_file, (Text *text) );
FCT (Text *, ie_select_window, (Text *text, int win_id) );
FCT (void, ie_set_mode, (Text *text, char *name) );
FCT (void, ie_set_mode_font, (Text *text, char *mode_name, char *font_name) );
FCT (void, ie_set_mode_suffixes, (Text *text, char *mode_name, char *suf) );
FCT (char, ie_the_char, (Text *text, int p) );
FCT (void, ie_upper_window, (Text *text) );
FCT (int, ie_window_height, (Text *text) );
FCT (int, ie_window_width, (Text *text, int c) );
FCT (void, ie_set_font, (Text *text, char *font) );
FCT (int, ie_re_search_forward, (Text *text, char *regex ) );
FCT (int, ie_re_search_backward, (Text *text, char *regex ) );
FCT (int, ie_re_replace, (Text *text, char *regex, char* newstring ) );
FCT (int, ie_re_replace_match, (Text *text, char *newstring ) );
FCT (int, ie_re_match_beginning, (Text *text, int n) );
FCT (int, ie_re_match_end, (Text *text, int n) );
FCT (char *, ie_shell_to_string, (Text *text, char *str ) );
FCT (void, ie_set_mark, (Text *text, int pos ) );
FCT (void, ie_goto_mark, (Text *text ) );
FCT (void, ie_reset_mark, (Text *text ) );
FCT (void, ie_color_area, (Text *text, int start, int end, char *colorname ) );
FCT (void, ie_paste_region, (Text *text ) );
FCT (void, ie_copy_region, (Text *text ) );
FCT (void, ie_cut_region, (Text *text ) );
FCT (void, ie_watch_on, (Text *text ) );
FCT (void, ie_watch_off, (Text *text ) );
FCT (void, ie_usleep, (Text *text, int t ) );
FCT (char *, ie_current_mode, (Text *text ) );

#endif /* _IE_FUNC_H_ */
