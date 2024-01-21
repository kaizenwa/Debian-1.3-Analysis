/*
 * File:	cmd.h
 * Purpose:	Declarations for user-callable editing commands.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd.h,v 1.44 1996/12/07 16:28:24 liw Exp $"
 * Description:	All command functions have the same type.  If they succeed,
 *		they return 0.  If they fail, they leave the state of the
 *		system as unchanged as possible, record an error message
 *		with error (see error.h), and return -1.
 *
 *		The command functions get a pointer to the editing window
 *		descriptor (struct editwin; see editwin.h) as the only 
 *		argument.
 *
 *		Each command function has a corresponding action function
 *		(see actions.h), which is registered via Xt so that the user
 *		can use resources to bind the action function (and, therefore,
 *		the command function) to an X event.
 */
 
#ifndef cmd_h
#define cmd_h

#include <stddef.h>

struct win;
typedef int cmd_function(struct win *);

extern int cmd_prev_was_cut;

int cmd_insert(struct win *, const char *, size_t);

cmd_function cmd_indent_selection_space;
cmd_function cmd_undent_selection_space;
cmd_function cmd_indent_selection;
cmd_function cmd_undent_selection;
cmd_function cmd_indent_line;
cmd_function cmd_unindent_right_curly;

cmd_function cmd_forward;
cmd_function cmd_backward;
cmd_function cmd_next_line;
cmd_function cmd_prev_line;
cmd_function cmd_goto_boln;
cmd_function cmd_goto_eoln;
cmd_function cmd_next_word;
cmd_function cmd_prev_word;
cmd_function cmd_next_para;
cmd_function cmd_prev_para;
cmd_function cmd_goto_bof;
cmd_function cmd_goto_eof;
cmd_function cmd_goto_top;
cmd_function cmd_goto_bottom;
cmd_function cmd_goto_selection;

cmd_function cmd_cut;
cmd_function cmd_copy;
cmd_function cmd_cut_ctrl_k;
cmd_function cmd_cut_previous;
cmd_function cmd_cut_next;
cmd_function cmd_cut_prev_word;
cmd_function cmd_cut_next_word;

cmd_function cmd_yank;
cmd_function cmd_yank_previous;
cmd_function cmd_yank_column;
cmd_function cmd_yank_column_previous;

cmd_function cmd_about;
cmd_function cmd_help;
cmd_function cmd_help_getting;
cmd_function cmd_help_bugs;
cmd_function cmd_help_list;

cmd_function cmd_quit;
cmd_function cmd_quit_and_save_all;
cmd_function cmd_quit_without_saving;

cmd_function cmd_search_popup;
cmd_function cmd_search_next;
cmd_function cmd_search_selection;
cmd_function cmd_search_and_replace_all;
cmd_function cmd_replace_and_search;
cmd_function cmd_replace_once;
cmd_function cmd_cancel_search;

cmd_function cmd_query_load_file;
cmd_function cmd_set_name_and_load_file;
cmd_function cmd_load_file;
cmd_function cmd_reload_file;
cmd_function cmd_load_selection;
cmd_function cmd_save_as;
cmd_function cmd_set_name_and_save_file;
cmd_function cmd_save_file;
cmd_function cmd_save_all;
cmd_function cmd_fake_save;
cmd_function cmd_kill_buffer;

cmd_function cmd_insert_file;
cmd_function cmd_query_insert_file;
cmd_function cmd_write_to;
cmd_function cmd_query_write_to;

cmd_function cmd_pipe;
cmd_function cmd_query_pipe;
cmd_function cmd_format_selection;

cmd_function cmd_goto_line;
cmd_function cmd_query_goto_line;

cmd_function cmd_new;
cmd_function cmd_next_file;
cmd_function cmd_prev_file;

cmd_function cmd_clone;
cmd_function cmd_close;
cmd_function cmd_close_others;
cmd_function cmd_open_all;

cmd_function cmd_next_page;
cmd_function cmd_prev_page;
cmd_function cmd_scroll_next_page;
cmd_function cmd_scroll_prev_page;
cmd_function cmd_scroll_left_one;
cmd_function cmd_scroll_right_one;
cmd_function cmd_center;

cmd_function cmd_select_para;

cmd_function cmd_toggle_columnar;
cmd_function cmd_make_columnar;
cmd_function cmd_make_linear;

cmd_function cmd_extend_forward;
cmd_function cmd_extend_backward;
cmd_function cmd_extend_boln;
cmd_function cmd_extend_eoln;
cmd_function cmd_extend_prev_line;
cmd_function cmd_extend_next_line;
cmd_function cmd_extend_bof;
cmd_function cmd_extend_eof;
cmd_function cmd_extend_prev_page;
cmd_function cmd_extend_next_page;
cmd_function cmd_extend_prev_word;
cmd_function cmd_extend_next_word;
cmd_function cmd_extend_prev_para;
cmd_function cmd_extend_next_para;
cmd_function cmd_extend_top;
cmd_function cmd_extend_bottom;
cmd_function cmd_extend_mark_0;
cmd_function cmd_extend_mark_1;
cmd_function cmd_extend_mark_2;
cmd_function cmd_extend_mark_3;
cmd_function cmd_extend_mark_4;
cmd_function cmd_extend_mark_5;
cmd_function cmd_extend_mark_6;
cmd_function cmd_extend_mark_7;
cmd_function cmd_extend_mark_8;
cmd_function cmd_extend_mark_9;
cmd_function cmd_extend_pair;
cmd_function cmd_extend_line;

cmd_function cmd_set_mark_0;
cmd_function cmd_set_mark_1;
cmd_function cmd_set_mark_2;
cmd_function cmd_set_mark_3;
cmd_function cmd_set_mark_4;
cmd_function cmd_set_mark_5;
cmd_function cmd_set_mark_6;
cmd_function cmd_set_mark_7;
cmd_function cmd_set_mark_8;
cmd_function cmd_set_mark_9;
cmd_function cmd_goto_mark_0;
cmd_function cmd_goto_mark_1;
cmd_function cmd_goto_mark_2;
cmd_function cmd_goto_mark_3;
cmd_function cmd_goto_mark_4;
cmd_function cmd_goto_mark_5;
cmd_function cmd_goto_mark_6;
cmd_function cmd_goto_mark_7;
cmd_function cmd_goto_mark_8;
cmd_function cmd_goto_mark_9;

#endif
