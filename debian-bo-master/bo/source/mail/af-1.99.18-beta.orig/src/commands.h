/* Commands.h - Key binding structure and value definitions for af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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

#define COMMANDID	"$Id: commands.h,v 1.24 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The internal representation of a command  */

typedef struct command {
	char *name;			/* The name of the command */
	FORM *(*func)();		/* The function to call */
	ARGSPEC *args;			/* The argument specifier */
	unsigned modes : 7;		/* Mode to which command applies */
	unsigned defer : 1;		/* Defer this command on startup? */
} COMMAND;

/****************************************************************************/
/* And the internal representation of a command argument */

typedef struct {
	unsigned universal : 1;		/* Is the argument universal? */
	unsigned negative : 1;		/* Is the argument negative? */
	unsigned value;			/* Value of the argument */
} ARGUMENT;

/****************************************************************************/
/* The definition of the region, for commands which use it */

typedef struct region {
	MESSAGE *start, *end;		/* Start and end of the region */
	int above_point;		/* Does the region lie above point? */
} REGION;

/****************************************************************************/
/* The set of messages on which a command can operate */

#define MS_MESSAGE	0
#define MS_REGION	1
#define MS_TAGSET	2

/****************************************************************************/
/* The possible types of keypress required by a command */

#define K_COMMAND	0
#define K_ARGUMENT	1
#define K_DIGIT		2

/****************************************************************************/
/* The multiplier for the universal-argument */

#define ARG_MULTIPLIER	4

/****************************************************************************/
/* The possible things we could be looking for with describe-xxx */

#define D_AF		"af"
#define D_COMMAND	"command"
#define D_FUNCTION	"function"
#define D_VARIABLE	"variable"
#define D_MODE		"mode"

/****************************************************************************/
/* The entries displayed by short-cut help commands */

#define D_HELP		"help"
#define D_NEWS		"news"
#define D_COPYING	"copying"
#define D_WARRANTY	"warranty"
#define D_STARTUP	"startup"

/****************************************************************************/
/* Command handlers defined in commands.c */

FORM *exec_command(), *univ_arg(), *digit_arg();
FORM *neg_arg(), *kbd_quit();

/* Command handlers defined in buf_cmd.c */

FORM *switch_buf(), *owin_switch(), *kill_buf(), *kill_some();
FORM *buf_list(), *widen(), *sort_buffer();

/* Command handlers defined in conf_cmd.c */

FORM *readonly(), *set(), *set_alias(), *gl_set_key();
FORM *gl_unset_key(), *lo_set_key(), *lo_unset_key();
FORM *mb_set_key(), *mb_unset_key(), *to_set_key();
FORM *to_unset_key(), *make_keymap(), *start_macro();
FORM *end_macro(), *call_macro(), *macro_query();
FORM *name_macro(), *write_config();

/* Command handlers defined in file_cmd.c */

FORM *find_file(), *owin_find(), *find_readonly();
FORM *find_alternate(), *read_pending(), *revert_buffer();
FORM *resync(), *insert_file(), *not_modified(), *save();
FORM *save_name(), *save_some(), *save_all(), *load_file();
FORM *load_lib();

/* Command handlers defined in help_cmd.c */

FORM *alias_list(), *bind_list(), *cmd_list(), *func_list();
FORM *macro_list(), *map_list(), *hist_list(), *var_list();
FORM *func_describe(), *key_describe(), *brf_describe();
FORM *major_describe(), *mode_describe(), *var_describe();
FORM *apropos(), *info(), *help_help(), *news(), *copying();
FORM *warranty(), *view_lossage();

/* Command handlers defined in mail_cmd.c */

FORM *mail(), *reply(), *group_reply(), *forward();
FORM *bounce(), *open_msg(), *page_msg(), *save_msg();
FORM *save_region(), *save_tagset(), *print_msg();
FORM *print_region(), *print_tagset(), *pipe_msg();
FORM *pipe_region(), *pipe_tagset(), *explode();
FORM *edit_msg(), *msg_info();

/* Command handlers defined in mark_cmd.c */

FORM *set_mark(), *exchange(), *kill_line(), *kill_region();
FORM *copy_region(), *nrw_region(), *sort_region();
FORM *yank(), *yank_pop();

/* Command handlers defined in misc_cmd.c */

FORM *shell(), *shellcmd(), *typecmd(), *change_dir();
FORM *current_dir(), *version();
FORM *exit_af(), *save_and_exit();

#ifdef HAVE_JOBCONTROL
FORM *suspend();
#endif /* HAVE_JOBCONTROL */

/* Command handlers defined in move_cmd.c */

FORM *prev_line(), *next_line(), *move_win(), *goto_line();
FORM *buf_start(), *buf_end(), *down_scroll(), *up_scroll();
FORM *owin_scroll(), *recenter(), *cursor_pos();

/* Command handlers defined in readline.c */

FORM *self_insert(), *quote_char(), *mb_mark(), *mb_exchange();
FORM *del_fwd(), *del_back(), *fwd_kill_word(), *back_kill_word();
FORM *mb_lkill(), *mb_rkill(), *mb_rcopy(), *mb_yank();
FORM *ucase_word(), *dcase_word(), *cap_word(), *transpose();
FORM *fwd_char(), *back_char(), *fwd_word(), *back_word();
FORM *start_of_line(), *end_of_line(), *prev_hist(), *next_hist();
FORM *hist_start(), *hist_end(), *hsch_fwd(), *hsch_back();
FORM *newline(), *redraw(), *clear_mb(), *mb_complete();
FORM *mb_word_complete(), *mb_exit_complete();
FORM *mb_list_completions();

/* Command handlers defined in srch_cmd.c */

FORM *se_fwd(), *se_back(), *st_fwd(), *st_back(), *se_tag();

/* Command handlers defined in tag_cmd.c */

FORM *del_msg(), *undel_msg(), *tag_msg(), *tag_thread();
FORM *untag_msg(), *rm_tags(), *kill_tagset(), *copy_tagset();
FORM *nrw_tagset(), *sort_tagset(), *msg_tags();

/* Command handlers defined in typeout.c */

FORM *to_scroll(), *to_up(), *to_down(), *to_prev();
FORM *to_next(), *to_start(), *to_end(), *to_goto();
FORM *to_redraw(), *to_mark(), *to_exchange();
FORM *to_sfwd(), *to_sback(), *to_cursor();

/* Command handlers defined in win_cmd.c */

FORM *split_win(), *del_cwin(), *del_owin(), *enlarge_win();
FORM *shrink_win(), *other_win(), *prev_win();

/****************************************************************************/
