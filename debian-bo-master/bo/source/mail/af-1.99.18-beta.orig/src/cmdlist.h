/* Cmdlist.h - List of internal commands for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/****************************************************************************/
/* RCS info */

#ifndef lint
static char *CmdlistId = "$Id: cmdlist.h,v 1.17 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The modes for commands for different modes */

#define CM_MAIL		(M_MAIL)
#define CM_TYPE		(M_TYPEOUT)
#define CM_MBUF		(M_MBUF)
#define CM_GLOB		(M_MAIL | M_TYPEOUT | M_MBUF)
#define CM_NMAIL	(M_TYPEOUT | M_MBUF)
#define CM_NTYPE	(M_MAIL | M_MBUF)
#define CM_NMBUF	(M_MAIL | M_TYPEOUT)

/****************************************************************************/
/* The argument specifications for commands with arguments */

static ARGSPEC a_sym[] =       { FT_SYMBOL, FT_NULL };
static ARGSPEC a_symany[] =    { FT_SYMBOL, FT_ANY, FT_NULL };
static ARGSPEC a_symostr[] =   { FT_SYMBOL, FT_STRING | FT_OPTIONAL, FT_NULL };

static ARGSPEC a_num[] =       { FT_NUMBER, FT_NULL };
static ARGSPEC a_onum[] =      { FT_NUMBER | FT_OPTIONAL, FT_NULL };
static ARGSPEC a_chronum[] =   { FT_NUMBER | FT_STRING,
				 FT_NUMBER | FT_OPTIONAL, FT_NULL };

static ARGSPEC a_str[] =       { FT_STRING, FT_NULL };
static ARGSPEC a_ostr[] =      { FT_STRING | FT_OPTIONAL, FT_NULL };
static ARGSPEC a_stronum[] =   { FT_STRING, FT_NUMBER | FT_OPTIONAL, FT_NULL };
static ARGSPEC a_strstr[] =    { FT_STRING, FT_STRING, FT_NULL };
static ARGSPEC a_strostr[] =   { FT_STRING, FT_STRING | FT_OPTIONAL, FT_NULL };
static ARGSPEC a_strstrstr[] = { FT_STRING, FT_STRING, FT_STRING, FT_NULL };
static ARGSPEC a_strstronum[] = { FT_STRING, FT_STRING, FT_NUMBER |
				  FT_OPTIONAL, FT_NULL };

static ARGSPEC a_set[] =       { FT_STRING, FT_SYMBOL | FT_LIST, FT_NULL };

/****************************************************************************/
/* The list of af commands by name */

static COMMAND commands[] = {
	{ "af-version", version, NULL, CM_GLOB, TRUE },
	{ "apropos", apropos, a_strostr, CM_GLOB, TRUE },
	{ "backward-char", back_char, a_onum, CM_MBUF, TRUE },
	{ "backward-kill-word", back_kill_word, a_onum, CM_MBUF, TRUE },
	{ "backward-word", back_word, a_onum, CM_MBUF, TRUE },
	{ "beginning-of-buffer", buf_start, a_onum, CM_GLOB, TRUE },
	{ "beginning-of-line", start_of_line, NULL, CM_MBUF, TRUE },
	{ "bounce-message", bounce, a_str, CM_MAIL, TRUE },
	{ "call-last-kbd-macro", call_macro, a_onum, CM_GLOB, TRUE },
	{ "capitalize-word", cap_word, a_onum, CM_MBUF, TRUE },
	{ "cd", change_dir, a_str, CM_GLOB, FALSE },
	{ "clear-echo-area", clear_mb, NULL, CM_NMBUF, TRUE },
	{ "copy-region-as-kill", copy_region, NULL, CM_NTYPE, TRUE },
	{ "copy-tagset-as-kill", copy_tagset, a_str, CM_MAIL, TRUE },
	{ "delete-backward-char", del_back, a_onum, CM_MBUF, TRUE },
	{ "delete-char", del_fwd, a_onum, CM_MBUF, TRUE },
	{ "delete-message", del_msg, NULL, CM_MAIL, TRUE },
	{ "delete-other-windows", del_owin, NULL, CM_MAIL, TRUE },
	{ "delete-window", del_cwin, NULL, CM_MAIL, TRUE },
	{ "describe-copying", copying, a_ostr, CM_GLOB, TRUE },
	{ "describe-function", func_describe, a_symostr, CM_GLOB, TRUE },
	{ "describe-key", key_describe, a_strostr, CM_GLOB, TRUE },
	{ "describe-key-briefly", brf_describe, a_str, CM_GLOB, TRUE },
	{ "describe-major-mode", major_describe, a_ostr, CM_GLOB, TRUE },
	{ "describe-mode", mode_describe, a_symostr, CM_GLOB, TRUE },
	{ "describe-no-warranty", warranty, a_ostr, CM_GLOB, TRUE },
	{ "describe-variable", var_describe, a_symostr, CM_GLOB, TRUE },
	{ "digit-argument", digit_arg, a_num, CM_GLOB, TRUE },
	{ "downcase-word", dcase_word, a_onum, CM_MBUF, TRUE },
	{ "edit-message", edit_msg, a_onum, CM_MAIL, TRUE },
	{ "end-kbd-macro", end_macro, a_onum, CM_GLOB, TRUE },
	{ "end-of-buffer", buf_end, a_onum, CM_GLOB, TRUE },
	{ "end-of-line", end_of_line, NULL, CM_MBUF, TRUE },
	{ "enlarge-window", enlarge_win, a_onum, CM_MAIL, TRUE },
	{ "exchange-point-and-mark", exchange, NULL, CM_GLOB, TRUE },
	{ "execute-extended-command", exec_command, NULL, CM_GLOB, FALSE },
	{ "explode-digest", explode, NULL, CM_MAIL, TRUE },
	{ "find-alternate-file", find_alternate, a_str, CM_MAIL, TRUE },
	{ "find-file", find_file, a_str, CM_MAIL, TRUE },
	{ "find-file-other-window", owin_find, a_str, CM_MAIL, TRUE },
	{ "find-file-read-only", find_readonly, a_str, CM_MAIL, TRUE },
	{ "forward-char", fwd_char, a_onum, CM_MBUF, TRUE },
	{ "forward-message", forward, a_str, CM_MAIL, TRUE },
	{ "forward-word", fwd_word, a_onum, CM_MBUF, TRUE },
	{ "global-set-key", gl_set_key, a_set, CM_GLOB, FALSE },
	{ "global-unset-key", gl_unset_key, a_str, CM_GLOB, FALSE },
	{ "goto-line", goto_line, a_num, CM_NMBUF, TRUE },
	{ "group-reply-to-message", group_reply, NULL, CM_MAIL, TRUE },
	{ "help-on-help", help_help, NULL, CM_GLOB, TRUE },
	{ "info", info, NULL, CM_GLOB, TRUE },
	{ "insert-file", insert_file, a_str, CM_MAIL, TRUE },
	{ "kbd-macro-query", macro_query, NULL, CM_GLOB, TRUE },
	{ "keyboard-quit", kbd_quit, NULL, CM_GLOB, TRUE },
	{ "kill-buffer", kill_buf, a_sym, CM_MAIL, TRUE },
	{ "kill-line", kill_line, a_onum, CM_NTYPE, TRUE },
	{ "kill-region", kill_region, NULL, CM_NTYPE, TRUE },
	{ "kill-some-buffers", kill_some, NULL, CM_MAIL, TRUE },
	{ "kill-tagset", kill_tagset, a_str, CM_MAIL, TRUE },
	{ "kill-word", fwd_kill_word, a_onum, CM_MBUF, TRUE },
	{ "list-aliases", alias_list, a_ostr, CM_GLOB, TRUE },
	{ "list-bindings", bind_list, a_ostr, CM_GLOB, TRUE },
	{ "list-buffers", buf_list, a_ostr, CM_GLOB, TRUE },
	{ "list-commands", cmd_list, a_ostr, CM_GLOB, TRUE },
	{ "list-functions", func_list, a_ostr, CM_GLOB, TRUE },
	{ "list-kbd-macros", macro_list, a_ostr, CM_GLOB, TRUE },
	{ "list-keymaps", map_list, a_ostr, CM_GLOB, TRUE },
	{ "list-minibuffer-history", hist_list, a_ostr, CM_GLOB, TRUE },
	{ "list-variables", var_list, a_ostr, CM_GLOB, TRUE },
	{ "load-file", load_file, a_str, CM_GLOB, FALSE },
	{ "load-library", load_lib, a_str, CM_GLOB, FALSE },
	{ "local-set-key", lo_set_key, a_set, CM_GLOB, FALSE },
	{ "local-unset-key", lo_unset_key, a_str, CM_GLOB, FALSE },
	{ "make-keymap", make_keymap, a_sym, CM_GLOB, FALSE },
	{ "message-info", msg_info, NULL, CM_MAIL, TRUE },
	{ "message-tags", msg_tags, NULL, CM_MAIL, TRUE },
	{ "minibuffer-complete", mb_complete, NULL, CM_MBUF, TRUE },
	{ "minibuffer-complete-and-exit", mb_exit_complete,
		  NULL, CM_MBUF, TRUE },
	{ "minibuffer-complete-word", mb_word_complete,
		  NULL, CM_MBUF, TRUE },
	{ "minibuffer-list-completions", mb_list_completions,
		  NULL, CM_MBUF, TRUE },
	{ "minibuffer-set-key", mb_set_key, a_set, CM_GLOB, FALSE },
	{ "minibuffer-unset-key", mb_unset_key, a_str, CM_GLOB, FALSE },
	{ "move-to-window-line", move_win, a_num, CM_MAIL, TRUE },
	{ "name-last-kbd-macro", name_macro, a_sym, CM_GLOB, TRUE },
	{ "narrow-to-region", nrw_region, NULL, CM_MAIL, TRUE },
	{ "narrow-to-tagset", nrw_tagset, a_str, CM_MAIL, TRUE },
	{ "negative-argument", neg_arg, a_num, CM_GLOB, TRUE },
	{ "newline", newline, NULL, CM_MBUF, TRUE },
	{ "next-line", next_line, a_onum, CM_GLOB, TRUE },
	{ "not-modified", not_modified, a_onum, CM_MAIL, TRUE },
	{ "open-message", open_msg, a_onum, CM_MAIL, TRUE },
	{ "other-window", other_win, a_onum, CM_MAIL, TRUE },
	{ "page-message", page_msg, a_onum, CM_MAIL, TRUE },
	{ "pipe-message", pipe_msg, a_stronum, CM_MAIL, TRUE },
	{ "pipe-region", pipe_region, a_stronum, CM_MAIL, TRUE },
	{ "pipe-tagset", pipe_tagset, a_strstronum, CM_MAIL, TRUE },
	{ "previous-line", prev_line, a_onum, CM_GLOB, TRUE },
	{ "previous-window", prev_win, a_onum, CM_MAIL, TRUE },
	{ "print-message", print_msg, a_onum, CM_MAIL, TRUE },
	{ "print-region", print_region, a_onum, CM_MAIL, TRUE },
	{ "print-tagset", print_tagset, a_stronum, CM_MAIL, TRUE },
	{ "pwd", current_dir, NULL, CM_GLOB, TRUE },
	{ "quoted-insert", quote_char, a_chronum, CM_MBUF, TRUE },
	{ "read-pending-file", read_pending, a_str, CM_MAIL, TRUE },
	{ "recenter", recenter, a_onum, CM_GLOB, TRUE },
	{ "remove-tags", rm_tags, a_str, CM_MAIL, TRUE },
	{ "reply-to-message", reply, NULL, CM_MAIL, TRUE },
	{ "resync-buffer", resync, NULL, CM_MAIL, TRUE },
	{ "revert-buffer", revert_buffer, NULL, CM_MAIL, TRUE },
	{ "save-all-buffers", save_all, NULL, CM_MAIL, TRUE },
	{ "save-all-kill-af", save_and_exit, NULL, CM_MAIL, TRUE },
	{ "save-buffer", save, NULL, CM_MAIL, TRUE },
	{ "save-buffers-kill-af", exit_af, NULL, CM_MAIL, TRUE },
	{ "save-message", save_msg, a_stronum, CM_MAIL, TRUE },
	{ "save-region", save_region, a_stronum, CM_MAIL, TRUE },
	{ "save-some-buffers", save_some, NULL, CM_MAIL, TRUE },
	{ "save-tagset", save_tagset, a_strstronum, CM_MAIL, TRUE },
	{ "scroll-down", down_scroll, a_onum, CM_NMBUF, TRUE },
	{ "scroll-other-window", owin_scroll, a_onum, CM_MAIL, TRUE },
	{ "scroll-up", up_scroll, a_onum, CM_NMBUF, TRUE },
	{ "search-and-tag", se_tag, a_strstronum, CM_MAIL, TRUE },
	{ "search-backward", se_back, a_stronum, CM_GLOB, TRUE },
	{ "search-forward", se_fwd, a_stronum, CM_GLOB, TRUE },
	{ "self-insert-command", self_insert, a_chronum, CM_MBUF, TRUE },
	{ "send-mail", mail, a_str, CM_MAIL, TRUE },
	{ "set-alias", set_alias, a_strstrstr, CM_MAIL, FALSE },
	{ "set-mark-command", set_mark, NULL, CM_GLOB, TRUE },
	{ "set-variable", set, a_symany, CM_GLOB, FALSE },
	{ "shell", shell, NULL, CM_GLOB, TRUE },
	{ "shell-command", shellcmd, a_str, CM_GLOB, TRUE },
	{ "shell-command-to-typeout", typecmd, a_str, CM_GLOB, TRUE },
	{ "shrink-window", shrink_win, a_onum, CM_MAIL, TRUE },
	{ "sort-buffer", sort_buffer, a_str, CM_MAIL, TRUE },
	{ "sort-region", sort_region, a_str, CM_MAIL, TRUE },
	{ "sort-tagset", sort_tagset, a_strstr, CM_MAIL, TRUE },
	{ "split-window-vertically", split_win, a_onum, CM_MAIL, TRUE },
	{ "start-kbd-macro", start_macro, a_onum, CM_GLOB, TRUE },
#ifdef HAVE_JOBCONTROL
	{ "suspend-af", suspend, NULL, CM_GLOB, TRUE },
#endif /* HAVE_JOBCONTROL */
	{ "switch-to-buffer", switch_buf, a_sym, CM_MAIL, TRUE },
	{ "switch-to-buffer-other-window", owin_switch,
		  a_sym, CM_MAIL, TRUE },
	{ "tag-message", tag_msg, a_str, CM_MAIL, TRUE },
	{ "tag-search-backward", st_back, a_str, CM_MAIL, TRUE },
	{ "tag-search-forward", st_fwd, a_str, CM_MAIL, TRUE },
	{ "tag-thread", tag_thread, a_str, CM_MAIL, TRUE },
	{ "toggle-read-only", readonly, a_onum, CM_MAIL, TRUE },
	{ "transpose-chars", transpose, a_onum, CM_MBUF, TRUE },
	{ "typeout-scroll-or-exit", to_scroll, a_onum, CM_TYPE, TRUE },
	{ "typeout-set-key", to_set_key, a_set, CM_GLOB, FALSE },
	{ "typeout-unset-key", to_unset_key, a_str, CM_GLOB, FALSE },
	{ "undelete-message", undel_msg, NULL, CM_MAIL, TRUE },
	{ "universal-argument", univ_arg, a_num, CM_GLOB, TRUE },
	{ "untag-message", untag_msg, a_str, CM_MAIL, TRUE },
	{ "upcase-word", ucase_word, a_onum, CM_MBUF, TRUE },
	{ "view-af-news", news, a_ostr, CM_GLOB, TRUE },
	{ "view-lossage", view_lossage, a_ostr, CM_GLOB, TRUE },
	{ "what-cursor-position", cursor_pos, NULL, CM_NMBUF, TRUE },
	{ "widen", widen, NULL, CM_MAIL, TRUE },
	{ "write-configuration", write_config, a_str, CM_GLOB, TRUE },
	{ "write-file", save_name, a_str, CM_MAIL, TRUE },
	{ "yank", yank, a_onum, CM_NTYPE, TRUE },
	{ "yank-pop", yank_pop, a_num, CM_NTYPE, TRUE },
	{ NULL, NULL, NULL, M_NULL, FALSE }
};

/****************************************************************************/
