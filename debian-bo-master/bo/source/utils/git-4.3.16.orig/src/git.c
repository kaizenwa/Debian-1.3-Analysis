/* git.c -- The main git file.  */

/* Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

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

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#else /* !HAVE_STDLIB_H */
#include "ansi_stdlib.h"
#endif /* !HAVE_STDLIB_H */

#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#include "file.h"
#include <ctype.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <limits.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "stdc.h"
#include "xstring.h"
#include "xmalloc.h"
#include "xio.h"
#include "tty.h"
#include "window.h"
#include "inputline.h"
#include "status.h"
#include "panel.h"
#include "configure.h"
#include "signals.h"
#include "system.h"
#include "history.h"
#include "tilde.h"
#include "misc.h"


#ifndef HAVE_PUTENV
#ifndef HAVE_SETENV
#ifdef NeXT
#define HAVE_PUTENV 1
#undef HAVE_SETENV
extern int putenv PROTO ((char *));
#endif /* NeXT */
#endif /* HAVE_SETENV */
#endif /* HAVE_PUTENV */


extern int suspend_allowed;
extern int signals_status;


#define MAX_STATIC_SIZE 50


#ifdef HAVE_LINUX
extern int LinuxConsole;
#endif /* HAVE_LINUX */


#ifdef HAVE_LINUX
int AnsiColors = ON;
#else   /* !HAVE_LINUX */
int AnsiColors = OFF;
#endif  /* !HAVE_LINUX */

int TypeSensitivity = ON;

/* These are the only possible values for `current_mode'. Used while
   resuming from suspended mode in order to correctly refresh the
   display. */
#define GIT_SCREEN_MODE         0
#define GIT_TERMINAL_MODE       1


pid_t pid;
char *home;
char *program;
int two_panel_mode = 1;
int SCREEN_X;
int SCREEN_Y;
int current_mode = GIT_SCREEN_MODE;

int wait_msg = 0;

int UseLastScreenChar;

char cSection[]  = "[GIT-Color]";
char bwSection[] = "[GIT-Monochrome]";

#ifdef HAVE_GCC
char title[] = " "PRODUCT" "VERSION;
#else
char title[] = " GNU Interactive Tools 4.3.16";
#endif /* !HAVE_GCC */

char login[] = "User:";
char tty[] = "tty:";

char lock_think[] = "Wait, I am thinking...";
char lock_ok[]    = "Wait, I am thinking... well, it looks good.";
char lock_bad[]   = "Wait, I am thinking... well, lets try again.";

#ifdef HAVE_GCC
char exit_msg[] = "Exit "PRODUCT" ? ";
#else
char exit_msg[] = "Exit GNU Interactive Tools ? ";
#endif /* !HAVE_GCC */

char *screen;
char PS1[4] = " $ ";
panel_t *left_panel, *right_panel, *src_panel, *dest_panel, *temp_panel;

char *TempDirectory = "";

static char *NormalModeHelp      = "";
static char *CommandLineModeHelp = "";
static int  ConfirmOnExit;

/* Directory history stuff.  */
char **dir_history;
int dir_history_count;
int dir_history_point;

#define BUILTIN_OPERATIONS                       84

#define BUILTIN_copy				-1
#define BUILTIN_move				-2
#define BUILTIN_make_directory			-3
#define BUILTIN_delete				-4
#define BUILTIN_exit				-5
#define BUILTIN_previous_history_element	-6
#define BUILTIN_tty_mode			-7
#define BUILTIN_refresh				-8
#define BUILTIN_switch_panels			-9
#define BUILTIN_next_history_element		-10
#define BUILTIN_panel_enable_next_mode		-11
#define BUILTIN_panel_enable_owner_group	-12
#define BUILTIN_panel_enable_date_time		-13
#define BUILTIN_panel_enable_size		-14
#define BUILTIN_panel_enable_mode		-15
#define BUILTIN_panel_enable_full_name		-16
#define BUILTIN_panel_sort_next_method		-17
#define BUILTIN_panel_sort_by_name		-18
#define BUILTIN_panel_sort_by_extension		-19
#define BUILTIN_panel_sort_by_size		-20
#define BUILTIN_panel_sort_by_date		-21
#define BUILTIN_panel_sort_by_mode		-22
#define BUILTIN_panel_sort_by_owner_id		-23
#define BUILTIN_panel_sort_by_group_id		-24
#define BUILTIN_panel_sort_by_owner_name	-25
#define BUILTIN_panel_sort_by_group_name	-26
#define BUILTIN_select_entry			-27
#define BUILTIN_entry_to_input_line		-28
#define BUILTIN_beginning_of_panel		-29
#define BUILTIN_end_of_panel			-30
#define BUILTIN_scroll_down			-31
#define BUILTIN_scroll_up			-32
#define BUILTIN_previous_line			-33
#define BUILTIN_next_line			-34
#define BUILTIN_other_panel			-35
#define BUILTIN_change_directory		-36
#define BUILTIN_hard_refresh			-37
#define BUILTIN_select_files_matching_pattern	-38
#define BUILTIN_unselect_files_matching_pattern	-39
#define BUILTIN_adapt_current_directory		-40
#define BUILTIN_adapt_other_directory		-41
#define BUILTIN_other_path_to_input_line	-42
#define BUILTIN_selected_entries_to_input_line	-43
#define BUILTIN_backward_char			-44
#define BUILTIN_forward_char			-45
#define BUILTIN_backward_word			-46
#define BUILTIN_forward_word			-47
#define BUILTIN_beginning_of_line		-48
#define BUILTIN_end_of_line			-49
#define BUILTIN_delete_char			-50
#define BUILTIN_backward_delete_char		-51
#define BUILTIN_kill_word			-52
#define BUILTIN_backward_kill_word		-53
#define BUILTIN_kill_line			-54
#define BUILTIN_kill_to_beginning_of_line	-55
#define BUILTIN_kill_to_end_of_line		-56
#define BUILTIN_just_one_space			-57
#define BUILTIN_delete_horizontal_space		-58
#define BUILTIN_downcase_word			-59
#define BUILTIN_upcase_word			-60
#define BUILTIN_capitalize_word			-61
#define BUILTIN_action				-62
#define BUILTIN_set_mark			-63
#define BUILTIN_kill_region			-64
#define BUILTIN_kill_ring_save			-65
#define BUILTIN_yank				-66
#define BUILTIN_exchange_point_and_mark		-67
#define BUILTIN_set_scroll_step			-68
#define BUILTIN_isearch_backward		-69
#define BUILTIN_isearch_forward			-70
#define BUILTIN_previous_directory		-71
#define BUILTIN_next_directory			-72
#define BUILTIN_reset_directory_history		-73
#define BUILTIN_enlarge_panel			-74
#define BUILTIN_enlarge_other_panel		-75
#define BUILTIN_two_panels			-76
#define BUILTIN_lock				-77
#define BUILTIN_quick_compare_panels		-78
#define BUILTIN_thorough_compare_panels		-79
#define BUILTIN_name_downcase			-80
#define BUILTIN_name_upcase			-81
#define BUILTIN_up_one_dir			-82
#define BUILTIN_compare				-83
#define BUILTIN_bin_packing			-84


#define MAX_BUILTIN_NAME			 35


char built_in[BUILTIN_OPERATIONS][MAX_BUILTIN_NAME] =
{
    "copy",
    "move",
    "make-directory",
    "delete",
    "exit",
    "previous-history-element",
    "tty-mode",
    "refresh",
    "switch-panels",
    "next-history-element",
    "panel-enable-next-mode",
    "panel-enable-owner-group",
    "panel-enable-date-time",
    "panel-enable-size",
    "panel-enable-mode",
    "panel-enable-full-name",
    "panel-sort-next-method",
    "panel-sort-by-name",
    "panel-sort-by-extension",
    "panel-sort-by-size",
    "panel-sort-by-date",
    "panel-sort-by-mode",
    "panel-sort-by-owner-id",
    "panel-sort-by-group-id",
    "panel-sort-by-owner-name",
    "panel-sort-by-group-name",
    "select-entry",
    "entry-to-input-line",
    "beginning-of-panel",
    "end-of-panel",
    "scroll-down",
    "scroll-up",
    "previous-line",
    "next-line",
    "other-panel",
    "change-directory",
    "hard-refresh",
    "select-files-matching-pattern",
    "unselect-files-matching-pattern",
    "adapt-current-directory",
    "adapt-other-directory",
    "other-path-to-input-line",
    "selected-entries-to-input-line",
    "backward-char",
    "forward-char",
    "backward-word",
    "forward-word",
    "beginning-of-line",
    "end-of-line",
    "delete-char",
    "backward-delete-char",
    "kill-word",
    "backward-kill-word",
    "kill-line",
    "kill-to-beginning-of-line",
    "kill-to-end-of-line",
    "just-one-space",
    "delete-horizontal-space",
    "downcase-word",
    "upcase-word",
    "capitalize-word",
    "action",
    "set-mark",
    "kill-region",
    "kill-ring-save",
    "yank",
    "exchange-point-and-mark",
    "set-scroll-step",
    "isearch-backward",
    "isearch-forward",
    "previous-directory",
    "next-directory",
    "reset-directory-history",
    "enlarge-panel",
    "enlarge-other-panel",
    "two-panels",
    "lock",
    "quick-compare-panels",
    "thorough-compare-panels",
    "name-downcase",
    "name-upcase",
    "up-one-dir",
    "compare",
    "bin-packing",
};


typedef struct
{
    char *name;         /* The command name.  */
    char *body;         /* The unexpanded command body.  */
    char *new_dir;      /* If exit code == 0, goto this directory.  */
    char  save_screen;  /* Save the screen contents (if possible).  */
    char  pause;        /* Wait for a key before restoring the panels.  */
    char  hide;         /* Hide the output, emulating a builtin command.  */
    char  builtin;      /* This is a builtin command.  */
    char *sequence;     /* The ascii representation of the key sequence on
			   which the command is binded; used only for error
			   reporting purposes.  */
    xstack_t *history;  /* The history of the strings used to expand the
			   command body.  */
} command_t;


#define MAX_KEYS        2048      /* enough ?   :-) */
#define KEYSDATA_FIELDS    8


#define TITLE_FIELDS    5

static char *TitleFields[TITLE_FIELDS] =
{
    "TitleForeground",
    "TitleBackground",
    "TitleBrightness",
    "UserName",
    "TtyName",
};

#ifdef HAVE_LINUX
static int TitleColors[TITLE_FIELDS] =
{
    CYAN, BLUE, ON, YELLOW, YELLOW
};
#else   /* !HAVE_LINUX */
static int TitleColors[TITLE_FIELDS] =
{
    WHITE, BLACK, ON, WHITE, WHITE
};
#endif  /* !HAVE_LINUX */

#define TitleForeground TitleColors[0]
#define TitleBackground TitleColors[1]
#define TitleBrightness TitleColors[2]
#define UserName        TitleColors[3]
#define TtyName         TitleColors[4]



/*****************************************/
/* The GIT interface to the input line.  */
/*****************************************/

#define IL_ISEARCH_BEGIN        0
#define IL_ISEARCH_BACKWARD     1
#define IL_ISEARCH_FORWARD      2
#define IL_ISEARCH_END          3


extern int il_dispatch_commands PROTO ((int, int));
extern char *il_fix_text PROTO ((char *));
extern char *il_build_help_from_string PROTO ((char *));
extern char *il_isearch PROTO ((char *, char **, int, int *));
extern char il_read_char PROTO ((char *, char *, int));
extern char *il_read_line PROTO ((char *, char **, char *, xstack_t *));


/*
 * Add a string to the history.
 */

void
il_history_add_entry(history, text)
    xstack_t *history;
    char *text;
{
    char *history_text;

    /* Avoid duplicates.  */
    if (xstack_preview(history, &history_text, 1) &&
	strcmp(history_text, text) == 0)
	return;

    history_text = xstrdup(text);
    xstack_push(history, &history_text);
}


/*
 * Preview a history string.
 */

char *
il_history_view_entry(history, offset)
    xstack_t *history;
    int offset;
{
    char *history_text;

    return xstack_preview(history, &history_text, offset) ?
	   history_text : NULL;
}


/*
 * Dispatch input line commands. key is the actual command while flags is
 * a set of IL_*s or-ed together, allowing us to customize the  behaviour
 * of the input line.  If IL_MOVE is not specified, the  IL_EDIT  flag is
 * ignored.  Returns 1 if key has been processed and 0 otherwise.
 */

int
il_dispatch_commands(key, flags)
    int key;
    int flags;
{
    if ((flags & IL_MOVE) == 0)
	return 0;

    switch (key)
    {
	case BUILTIN_backward_char:
	    il_backward_char();
	    break;

	case BUILTIN_forward_char:
	    il_forward_char();
	    break;

	case BUILTIN_backward_word:
	    il_backward_word();
	    break;

	case BUILTIN_forward_word:
	    il_forward_word();
	    break;

	case BUILTIN_beginning_of_line:
	    il_beginning_of_line();
	    break;

	case BUILTIN_end_of_line:
	    il_end_of_line();
	    break;

	case BUILTIN_delete_char:
	    if (flags & IL_EDIT)
		il_delete_char();
	    break;

	case BUILTIN_backward_delete_char:
	    if (flags & IL_EDIT)
		il_backward_delete_char();
	    break;

	case BUILTIN_kill_word:
	    if (flags & IL_EDIT)
		il_kill_word();
	    break;

	case BUILTIN_backward_kill_word:
	    if (flags & IL_EDIT)
		il_backward_kill_word();
	    break;

	case BUILTIN_kill_line:
	    if (flags & IL_EDIT)
		il_kill_line(IL_STORE);
	    break;

	case BUILTIN_kill_to_beginning_of_line:
	    if (flags & IL_EDIT)
		il_kill_to_beginning_of_line();
	    break;

	case BUILTIN_kill_to_end_of_line:
	    if (flags & IL_EDIT)
		il_kill_to_end_of_line();
	    break;

	case BUILTIN_just_one_space:
	    if (flags & IL_EDIT)
		il_just_one_space();
	    break;

	case BUILTIN_delete_horizontal_space:
	    if (flags & IL_EDIT)
		il_delete_horizontal_space();
	    break;

	case BUILTIN_downcase_word:
	    if (flags & IL_EDIT)
		il_downcase_word();
	    break;

	case BUILTIN_upcase_word:
	    if (flags & IL_EDIT)
		il_upcase_word();
	    break;

	case BUILTIN_capitalize_word:
	    if (flags & IL_EDIT)
		il_capitalize_word();
	    break;

	case BUILTIN_set_mark:
	    il_set_mark();
	    break;

	case BUILTIN_kill_region:
	    if (flags & IL_EDIT)
		il_kill_region();
	    break;

	case BUILTIN_kill_ring_save:
	    il_kill_ring_save();
	    break;

	case BUILTIN_yank:
	    if (flags & IL_EDIT)
		il_yank();
	    break;

	case BUILTIN_exchange_point_and_mark:
	    il_exchange_point_and_mark();
	    break;

	default:
	    if ((flags & IL_EDIT) && is_print(key))
		il_insert_char(key);
	    else
		return 0;
	    break;
    }

    return 1;
}


/*
 * Fix the text.  Replace non-printable characters with spaces and expand
 * tabs.  Return a malloc-ed pointer to the fixed text.  The caller should
 * free the new text.
 */

char *
il_fix_text(text)
   char *text;
{
    int i, j;
    char *fixed_text;
    size_t fixed_text_length;


    if (text == NULL)
	return NULL;

    fixed_text = xmalloc(fixed_text_length = (strlen(text) + 1));

    for (i = 0, j = 0; text[i]; i++)
	if (text[i] == '\t')
	{
	    fixed_text = xrealloc(fixed_text, fixed_text_length += 8);
	    memcpy(&fixed_text[j], "        ", 8);
	    j += 8;
	}
	else
	    if (is_print(text[i]))
		fixed_text[j++] = text[i];
	    else
		fixed_text[j++] = ' ';

    fixed_text[j] = 0;

    return fixed_text;
}


char *
il_build_help_from_string(options)
    char *options;
{
    size_t len = 0;
    char *options_ptr = options;
    char *help = xmalloc(1 + strlen(options) * 3 + 8);

    help[len++] = '(';

    for (; *(options_ptr + 1); options_ptr++)
    {
	help[len++] = *options_ptr;
	help[len++] = ',';
	help[len++] = ' ';
    }

    help[len++] = *options_ptr;
    help[len++] = ')';
    help[len++] = ' ';
    help[len++] = '\0';

    return help;
}


/*
 * Read only one char from the input line.  message is a string explaining
 * what is this all about.  options is a string containing only those
 * characters that are valid answers, NULL if any character is a valid
 * answer.  The default char is the first char in the options string.
 * Returns 0 if it was interrupted, a valid character otherwise.
 */

char
il_read_char(message, options, flags)
    char *message;
    char *options;
    int flags;
{
    char *help;
    tty_key_t *ks;
    int key, repeat_count;
    command_t *command;
    input_line_t *saved_il = NULL;

    if (flags & IL_SAVE)
	saved_il = il_save();

    il_reset_line();

    if (message)
    {
	char *text = il_fix_text(message);

	if (flags & IL_ERROR)
	{
	    il_insert_text("*** ");
	    il_set_error_flag(1);
	}

	il_insert_text(text);

	if (flags & IL_HOME)
	    il_beginning_of_line();

	xfree(text);

	if (options)
	{
	    help = il_build_help_from_string(options);
	    il_insert_text(help);
	    xfree(help);
	}
    }

    il_full_update();

    if (flags & IL_BEEP)
	tty_beep();

    while (1)
    {
	ks  = tty_get_key(&repeat_count);
	key = ks->key_seq[0];

	command = (command_t *)ks->aux_data;

	if (command && command->builtin)
	    key = - 1 - (command->name - built_in[0]) / MAX_BUILTIN_NAME;

	switch (key)
	{
	    case BUILTIN_action:
		if (options != NULL)
		    key = *options;

	    case key_INTERRUPT:
		goto done;

	    default:
		while (repeat_count--)
		    if (il_dispatch_commands(key, flags) == 0)
			goto il_error;

		il_update();
		break;

	      il_error:

		if (options == NULL)
		    goto done;

		if (options && strchr(options, key))
			goto done;

		tty_beep();
		break;
	}

	il_update_point();
    }

  done:

    il_set_error_flag(0);

    if ((flags & IL_SAVE) && saved_il)
    {
	il_restore(saved_il);
	il_full_update();
    }

    return (key == key_INTERRUPT) ? 0 : key;
}


/*
 * WARNING: dest *must* be a pointer to a NULL pointer or a pointer
 * to a pointer allocated with xmalloc().  In the first case,
 * il_read_line() will return a string allocated with xmalloc().  In
 * the second case, it will reallocate the pointer as needed using
 * xrealloc().  The caller should free the memory allocated this way.
 */

char *
il_read_line(static_text, dest, default_string, history)
    char *static_text;
    char **dest;
    char *default_string;
    xstack_t *history;
{
    tty_key_t *ks;
    char *history_text;
    command_t *command;
    int key, repeat_count, offset = 0;


    il_reset_line();

    if (static_text)
	il_set_static_text(static_text);

    if (default_string)
	il_insert_text(default_string);

    if (history && default_string)
    {
	il_history_add_entry(history, default_string);
	offset = 1;
    }

    il_full_update();

    while (1)
    {
	ks  = tty_get_key(&repeat_count);
	key = ks->key_seq[0];

	command = (command_t *)ks->aux_data;

	if (command && command->builtin)
	    key = - 1 - (command->name - built_in[0]) / MAX_BUILTIN_NAME;

	switch (key)
	{
	    case BUILTIN_previous_line:
	    case BUILTIN_previous_history_element:
		if (history == NULL)
		    break;

		history_text = il_history_view_entry(history, ++offset);

		if (history_text == NULL)
		{
		    offset--;
		    tty_beep();
		}
		else
		{
		    il_kill_line(IL_DONT_STORE);
		    il_insert_text(history_text);
		    il_full_update();
		}
		break;

	    case BUILTIN_next_line:
	    case BUILTIN_next_history_element:
		if (history == NULL)
		    break;

		if (offset == 0)
		{
		    il_kill_line(IL_DONT_STORE);
		    il_full_update();
		    break;
		}

		il_kill_line(IL_DONT_STORE);

		offset--;

		if (offset > 0)
		{
		    history_text = il_history_view_entry(history, offset);
		    il_insert_text(history_text);
		}

		il_full_update();
		break;

	    case BUILTIN_action:
		il_get_contents(dest);

	    case key_INTERRUPT:
		goto done;

	    default:
		while (repeat_count--)
		    if (il_dispatch_commands(key, IL_MOVE | IL_EDIT) == 0)
		    {
			tty_beep();
			/* Well, there use to be a `goto done;' here,
			   but some people prefer to be able to
			   continue editing the current line after
			   accidentally hitting an unrelated key.  I
			   guess we will restore the original
			   behaviour.  */
		    }

		il_update();
		break;
	}

	il_update_point();
    }

  done:

    if (key == BUILTIN_action)
    {
	if (history)
	    il_history_add_entry(history, *dest);
	return *dest;
    }
    else
	return NULL;
}


/*
 * status = IL_ISEARCH_BEGIN    -> we are beginning to isearch; initialize
 * status = IL_ISEARCH_BACKWARD -> we are in the middle of an isearch-backward
 * status = IL_ISEARCH_FORWARD  -> we are in the middle of an isearch-forward
 * status = IL_ISEARCH_END      -> isearch complete; clean up
 *
 * *action = IL_ISEARCH_ACTION_DECREASE -> the user pressed the backspace key
 *					   so if there is no matching element
 *					   in the panel stack, we should delete
 *					   the last character in the input line
 * *action = IL_ISEARCH_ACTION_RETRY    -> the user pressed the isearch-forward
 *					   character again, so we should try to
 *					   find a new match for the current
 *					   string
 * *action = IL_ISEARCH_ACTION_INCREASE -> a new character has been inserted
 *					   into the input line so we should try
 *					   to find a match for the new string
 */

char *
il_isearch(static_text, dest, status, action)
    char *static_text;
    char **dest;
    int status;
    int *action;
{
    int key;
    tty_key_t *ks;
    command_t *command;
    static input_line_t *saved_il;


    if (status == IL_ISEARCH_BEGIN)
    {
	saved_il = il_save();
	il_reset_line();

	if (static_text)
	    il_set_static_text(static_text);

	return NULL;
    }

    if (status == IL_ISEARCH_END)
    {
	il_restore(saved_il);
	il_full_update();
	return NULL;
    }

    if (action == NULL)
	return NULL;

    *action = IL_ISEARCH_ACTION_NONE;

    il_full_update();

    ks  = tty_get_key(NULL);
    key = ks->key_seq[0];

    command = (command_t *)ks->aux_data;

    if (command && command->builtin)
	key = - 1 - (command->name - built_in[0]) / MAX_BUILTIN_NAME;

    switch (key)
    {
	case key_INTERRUPT:
	case BUILTIN_action:
	    break;

	case BUILTIN_backward_delete_char:
	    /* If the input line is empty, just beep.  */
	    if (il_is_empty())
		tty_beep();
	    else
	    {
		*action = IL_ISEARCH_ACTION_DECREASE;
		/* Don't call il_backward_delete_char().  There might be
		   several history elements in the panel stack that match
		   the current string so we have to delay the call to
		   il_backward_delete_char() until all the matching elements
		   have been pop-ed.  */
	    }

	    break;

	default:
	    if ((key == BUILTIN_isearch_backward &&
		 status == IL_ISEARCH_BACKWARD)  ||
		(key == BUILTIN_isearch_forward  &&
		 status == IL_ISEARCH_FORWARD))
	    {
		if (!il_is_empty())
		{
		    *action = IL_ISEARCH_ACTION_RETRY;
		    break;
		}
	    }

	    if (is_print(key))
	    {
		il_insert_char(key);
		*action = IL_ISEARCH_ACTION_INCREASE;
	    }
	    else
		key = key_INTERRUPT;    /* Force a NULL return value.  */

	    break;
    }

    il_full_update();
    il_get_contents(dest);

    return (key == BUILTIN_action || key == key_INTERRUPT) ? NULL : *dest;
}


/****************************************/
/* The directory history function set.  */
/****************************************/


void
dir_history_reset()
{
    if (dir_history)
    {
	int i;

	for (i = 0; i < dir_history_count; i++)
	    xfree(dir_history[i]);

	xfree(dir_history);
	dir_history = NULL;
    }

    dir_history_count = 0;
    dir_history_point = 0;
}


void
dir_history_add(directory)
    char *directory;
{
    dir_history_point = dir_history_count;

    dir_history = (char **)xrealloc(dir_history, ++dir_history_count *
						 sizeof(char *));

    dir_history[dir_history_point] = xstrdup(directory);
}


void
dir_history_next(this, link)
    panel_t *this;
    panel_t *link;
{
    if (dir_history_point < dir_history_count - 1)
	panel_action(this, act_CHDIR, link,
		     dir_history[++dir_history_point], 1);
    else
	tty_beep();
}


void
dir_history_prev(this, link)
    panel_t *this;
    panel_t *link;
{
    if (dir_history_point)
	panel_action(this, act_CHDIR, link,
		     dir_history[--dir_history_point], 1);
    else
	tty_beep();
}


void
clean_up()
{
    tty_exit(NULL);

    /* It is better not to do this here.  It can lead to an endless loop
       if xmalloc fails in write_history because xmalloc will call fatal
       and fatal will call clean_up again...  */
#if 0
    if (il)
	il_end();
#endif

    status_end();
    removelog();
}


void
fatal(postmsg)
    char *postmsg;
{
    clean_up();
    fprintf(stderr, "%s: fatal error: %s.\n", program, postmsg);
    exit(1);
}


void
settitle()
{
    char *buf;
    size_t len;
    tty_status_t status;
    window_t *title_win = window_init(0, 0, 1, SCREEN_X);

    tty_save(&status);

    tty_colors(TitleBrightness, TitleForeground, TitleBackground);

    window_goto(title_win, 0, 0);
    window_puts(title, strlen(title));

    buf = xmalloc(SCREEN_X + 1);

    len = (sizeof(login) - 1) + 1 + login_name_len + 2 +
	  (sizeof(tty)   - 1) + 1 + tty_name_len;

    memset(buf, ' ', len = SCREEN_X - strlen(title) - len - 1);
    window_goto(title_win, 0, strlen(title));
    window_puts(buf, len);

    xfree(buf);

    window_goto(title_win, 0, strlen(title) + len);
    window_puts(login, sizeof(login) - 1);
    window_putc(' ');
    tty_foreground(UserName);
    window_puts(login_name, login_name_len);
    window_putc(' ');
    window_putc(' ');
    tty_foreground(TitleForeground);
    window_puts(tty, sizeof(tty) - 1);
    window_putc(' ');
    tty_foreground(TtyName);
    window_puts(tty_name, tty_name_len);

    tty_foreground(TitleForeground);
    window_putc(' ');

    tty_restore(&status);
}


/*
 * This function is a mess. Don't try to understand what it does
 * ... :-( It basically expands a configuration line macros.  The
 * return value is 0 on error, -1 if some condition failed (the
 * command contains a %d but the current entry is not a directory), 1
 * if everything is ok, 2 if the command was correctly expanded and it
 * contains a '%i' and 3 if it contains a '%I'.
 */

int
command_expand(command, dest, p, l)
    command_t *command;
    char **dest;
    panel_t *p, *l;
{
    char c;
    uid_t uid;
    gid_t gid;
    int retval;
    panel_t *t;
    size_t len;
    struct group *grp;
    struct passwd *pwd;
    static int busy = 0;
    char *answer = NULL;
    char *question = NULL;
    int i_flag = 0, entry;
    size_t oldtmplen, tmplen;
    char *ptr, *tmp = NULL, *d, *flag;
    char *src = command->body, *save_body;


    d = *dest = xmalloc(len = (strlen(src) + 1));

    while (*src)
    {
	if (*src != '%')
	    *d++ = *src++;
	else
	{
	    t = islower(*++src) ? p : l;

	    switch (*src)
	    {
		case '?':

		    if (busy)
		    {
			busy = 0;
			goto bad_command;
		    }

		    if (*++src != '{')
			goto bad_command;

		    if ((ptr = strchr(++src, '}')) == NULL)
			goto bad_command;

		    *ptr = 0;
		     c = il_read_char(src, "yn", IL_MOVE);
		    *ptr = '}';

		    if (c != 'y')
			goto strings_dont_match;

		    src = ptr;

		    break;

		case 's':

		    if (busy)
		    {
			busy = 0;
			goto bad_command;
		    }

		    if (*++src != '{')
			goto bad_command;

		    if ((ptr = strchr(++src, ',')) == NULL)
			goto bad_command;

		    *ptr = 0;
		    busy = 1;

		    save_body = command->body;
		    command->body = src;
		    retval = command_expand(command, &answer, p, l);
		    command->body = save_body;

		    busy = 0;

		    if (retval < 1)
		    {
			*ptr = ',';
			if (retval == 0)
			    goto bad_command;
			else
			    goto strings_dont_match;
		    }

		    question = xmalloc(16 + strlen(command->name) +
				       strlen(answer) + 1);
		    sprintf(question, "%s: %s", command->name, answer);
		    xfree(answer);
		    answer =  NULL;
		    *ptr++ = ',';

		    if ((src = strchr(ptr, '}')) == NULL)
			goto bad_command;

		    *src = 0;

		    if (strlen(question) > MAX_STATIC_SIZE)
			question[MAX_STATIC_SIZE] = 0;

		    busy = 1;

		    save_body = command->body;
		    command->body = ptr;
		    retval = command_expand(command, &answer, p, l);
		    command->body = save_body;

		    busy = 0;

		    if (retval < 1)
		    {
			*src = '}';
			xfree(question);
			question = NULL;
			if (retval == 0)
			    goto bad_command;
			goto strings_dont_match;
		    }

		    flag = il_read_line(question, &tmp, answer,
					command->history);

		    xfree(question);
		    xfree(answer);
		    question = answer = NULL;

		    if (flag == NULL)
		    {
			*src = '}';
			goto strings_dont_match;
		    }

		    *src = '}';
		    break;

		case 'f':
		case 'F':

		    if (panel_get_current_file_type(t) != FILE_ENTRY)
			goto strings_dont_match;

		  get_file_name:

		    ptr = panel_get_current_file_name(t);
		    tmp = xmalloc(1 + strlen(ptr) + 1 + 1);
		    sprintf(tmp, "\"%s\"", ptr);
		    break;

		case 'd':
		case 'D':

		    if (panel_get_current_file_type(t) != DIR_ENTRY)
			goto strings_dont_match;
		    goto get_file_name;

		case 'l':
		case 'L':

		    if (panel_get_current_file_type(t) != SYMLINK_ENTRY)
			goto strings_dont_match;
		    goto get_file_name;

		case 't':
		case 'T':

		    if (panel_get_current_file_type(t) != FIFO_ENTRY)
			goto strings_dont_match;
		    goto get_file_name;

		case 'z':
		case 'Z':

		    if (panel_get_current_file_type(t) != SOCKET_ENTRY)
			goto strings_dont_match;
		    goto get_file_name;

		case 'a':
		case 'A':

		    goto get_file_name;

		case 'm':
		case 'M':

		    tmp = xmalloc(16);
		    sprintf(tmp, "%o",
			    (int)panel_get_current_file_mode(t) & 07777);
		    break;

		case 'o':
		case 'O':

		    uid = panel_get_current_file_uid(t);
		    pwd = getpwuid(uid);

		    if (pwd)
			tmp = xstrdup(pwd->pw_name);
		    else
		    {
			tmp = xmalloc(16);
			sprintf(tmp, "%o", (int)uid);
		    }

		    break;

		case 'g':
		case 'G':

		    gid = panel_get_current_file_gid(t);
		    grp = getgrgid(gid);

		    if (grp)
			tmp = xstrdup(grp->gr_name);
		    else
		    {
			tmp = xmalloc(16);
			sprintf(tmp, "%o", (int)gid);
		    }

		    break;

		case 'p':
		case 'P':

		    tmp = xmalloc(1 + strlen(t->path) + 1 + 1);
		    sprintf(tmp, "\"%s\"", t->path);
		    break;

		case 'b':
		case 'B':

		    ptr = strrchr(t->path, '/');
		    ptr = (*++ptr) ? ptr : "/root";
		    tmp = xmalloc(1 + strlen(ptr) + 1 + 1);
		    sprintf(tmp, "\"%s\"", ptr);
		    break;

		case 'i':
		case 'I':

		    i_flag = (*src == 'i') ? 1 : 2;

		    if (busy && t->selected_entries)
		    {
			tmplen = 20;
			tmp = xmalloc(tmplen + 1);
			strcpy(tmp, "selected entries(s)");
			break;
		    }

		    tmp = NULL;
		    tmplen = 0;

		    panel_init_iterator(t);

		    while ((entry = panel_get_next(t)) != -1)
		    {
			oldtmplen = tmplen;
			tmplen += 1 + strlen(t->dir_entry[entry].name) + 1 + 1;
			tmp = xrealloc(tmp, tmplen + 1);
			tmp[oldtmplen] = '"';
			strcpy(tmp + oldtmplen + 1, t->dir_entry[entry].name);
			tmp[tmplen - 2] = '"';
			tmp[tmplen - 1] = ' ';
			tmp[tmplen    ] = 0;
		    }

		    /* This can happen when there is no selected entry
		       and the current file is "..".  */
		    if (tmplen == 0)
			goto strings_dont_match;

		    break;

		default:

		    goto bad_command;
	    }

	    src++;
	    *d = 0;

	    if (tmp)
	    {
		*dest = xrealloc(*dest, len += strlen(tmp));
		strcat(*dest, tmp);
		d = *dest + strlen(*dest);
		xfree(tmp);
		tmp = NULL;
	    }
	}
    }

    *d = 0;
    return 1 + i_flag;

  bad_command:

    xfree(*dest);
    *dest = NULL;
    return 0;

  strings_dont_match:

    if (tmp)
	xfree(tmp);

    *dest = NULL;
    return -1;
}


void
refresh_after_suspend(mode)
    int mode;
{
    char *cmdln = NULL;
    char PWD[MAX_STATIC_SIZE + 1];

    if (mode == GIT_SCREEN_MODE)
    {
	/* Switch back to noncanonical mode.  */
	tty_set_mode(TTY_NONCANONIC);
	tty_defaults();

	settitle();

	status(NULL, 0, 0, 1, MSG_OK, MSG_CENTERED);

	panel_no_optimizations(src_panel);
	panel_no_optimizations(dest_panel);
	panel_action(src_panel, act_REFRESH, dest_panel, (void *)-1, 1);
	panel_action(dest_panel, act_REFRESH, src_panel, (void *)-1, 1);

	panel_set_focus(src_panel, ON);

	il_get_contents(&cmdln);
	il_reset_line();
	il_set_static_text(
	    strcat(truncate_string(panel_get_path(src_panel),
				   PWD, MAX_STATIC_SIZE - strlen(PS1) + 1),
		   PS1));
	il_insert_text(cmdln);
	il_full_update();

	signals(SIG_ON);
    }
    else
    {
	/* Switch back to noncanonical mode.  */
	tty_set_mode(TTY_NONCANONIC);
	tty_defaults();

	panel_no_optimizations(src_panel);
	panel_no_optimizations(dest_panel);
	tty_put_screen(screen);
	status(CommandLineModeHelp, 0, 0, 0, MSG_OK, MSG_CENTERED);
	il_full_update();
    }
}


void
add_to_environment(variable, alternate_variable, value)
    char *variable;
    char *alternate_variable;
    char *value;
{
    int result;
    char *alternate_value, *environment_string;

    if (getenv(variable) == NULL)
    {
	if (alternate_variable && (alternate_value=getenv(alternate_variable)))
	{
#ifdef HAVE_PUTENV
	    environment_string = xmalloc(strlen(variable) + 1 +
					 strlen(alternate_value) + 1);
	    sprintf(environment_string, "%s=%s", variable, alternate_value);
	    result = putenv(environment_string);
#else
	    result = setenv(variable, alternate_value, 1);
#endif /* !HAVE_PUTENV */
	}
	else
	{
#ifdef HAVE_PUTENV
	    environment_string = xmalloc(strlen(variable) + 1 +
					 strlen(value) + 1);
	    sprintf(environment_string, "%s=%s", variable, value);
	    result = putenv(environment_string);
#else
	    result = setenv(variable, value, 1);
#endif /* !HAVE_PUTENV */
	}

	if (result == -1)
	    fprintf(stderr, "%s: warning: cannot add '%s' to environment\n",
		    program, variable);
    }
}


/*
 * Read keys from the current section ([GIT-Keys] is supposed to be in
 * use when read_keys() is called).  Return the number of keys read.
 */

int
read_keys(keys, errors)
    int keys;
    int *errors;
{
    int i, j;
    command_t *command;
    int need_conversion;
    char key_seq[80];
    char *contents[KEYSDATA_FIELDS - 2];

    *errors = 0;

    for (i = keys; i < MAX_KEYS; i++)
    {
	configuration_getvarinfo(key_seq, contents,
				 KEYSDATA_FIELDS - 2, NO_SEEK);

	if (*key_seq == '\0')
	    break;

	if (*key_seq != '^')
	{
	    char *key_seq_ptr = tty_get_symbol_key_seq(key_seq);

	    if (key_seq_ptr)
	    {
		/* Ignore empty/invalid key sequences.  */
		if (*key_seq_ptr == '\0')
		    continue;

		/* We got the key sequence in the correct form, as
		   returned by tgetstr, so there is no need for
		   further conversion.  */
		strcpy(key_seq, key_seq_ptr);
		need_conversion = 0;
	    }
	    else
	    {
		/* This is not a TERMCAP symbol, it is a key sequence
		   that we will have to convert it with
		   tty_key_convert() into a machine usable form before
		   using it.  */
		need_conversion = 1;
	    }
	}
	else
	    need_conversion = 1;

	command = (command_t *)xcalloc(1, sizeof(command_t));

	if (contents[0])
	    command->name = xstrdup(contents[0]);
	else
	{
	    xfree(command);
	    continue;
	}

	command->history = xstack_init(sizeof(char *));

	if (contents[2])
	    command->new_dir = xstrdup(contents[2]);

	if (contents[1])
	    command->body = xstrdup(contents[1]);
	else
	    goto insert;

	if (contents[3])
	    command->save_screen = ((tolower(contents[3][0]) == 'y') ? 1 : 0);
	else
	    command->save_screen = 1;

	if (contents[4])
	    command->pause = ((tolower(contents[4][0]) == 'y') ? 1:0);

	if (contents[5])
	    command->hide = ((tolower(contents[5][0]) == 'y') ? 1:0);

      insert:

	/* This speeds up the process.  It is not a limitation because,
	   by convention, all the build-in command names contain only
	   lowercase letters.  Avoid searching through the list of
	   built-in command names.  */
	if (islower(command->name[0]))
	    for (j = 0; j < BUILTIN_OPERATIONS; j++)
	    {
		if (strcmp(command->name, built_in[j]) == 0)
		{
		    xfree(command->name);
		    command->name = built_in[j];
		    command->builtin = 1;
		    break;
		}
	    }

	command->sequence = xstrdup(key_seq);

	if (command->builtin || command->body || command->new_dir)
	    if (need_conversion)
	    {
		if (tty_key_convert((unsigned char *)key_seq))
		    tty_key_list_insert((unsigned char *)key_seq,
					(void *)command);
		else
		{
		    fprintf(stderr, "%s: warning: invalid key sequence '%s'\n",
			    program, key_seq);
		    (*errors)++;
		}
	    }
	    else
		tty_key_list_insert((unsigned char *)key_seq, (void *)command);
    }

    return i;
}


int
main(argc, argv)
    int argc;
    char *argv[];
{
    tty_key_t *ks;
    command_t *command;
    size_t len = 0, ptrlen;
    int previous_isearch_failed;
    char PWD[MAX_STATIC_SIZE + 1];
    input_line_t *saved_il = NULL;
    char *lock_password, *unlock_password;
    int child_exit_code, repeat_count, keys;
    int entry, key, app_end = 0, first_time = 1, errors = 0;
    int panel_no = 0, action_status, i, retval, to_case, cmp_mode;
    char *data = NULL, *cmdln = NULL, *input = NULL, *ptr, *srcptr;


#if defined(SIGTSTP) && defined(SIGCONT)
    /* Job control stuff. */
    signal(SIGTSTP, suspend);
    signal(SIGCONT, resume);
#endif

    signal(SIGSEGV, fatal_signal);
    signal(SIGHUP,  fatal_signal);

    signals(SIG_OFF);
    ignore_signals();

    printf("\n");

#ifdef HAVE_GCC
    printf(PRODUCT" "VERSION" (%s), %s %s\n",
	   HOST, __TIME__, __DATE__);
#else
    printf("GNU Interactive Tools (%s)\n",
	   HOST);
#endif /* !HAVE_GCC */

    printf("GIT is free software; you can redistribute it and/or modify it under the\n");
    printf("terms of the GNU General Public License as published by the Free Software\n");
    printf("Foundation; either version 2, or (at your option) any later version.\n");
    printf("Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.\n");
    printf("Written by Tudor Hulubei and Andrei Pitis, Bucharest, Romania\n\n");

    program = argv[0];
    pid     = getpid();

    home = getenv("HOME");
    if (home == NULL)
	home = ".";

    get_tty_name();
    get_login_name();

/*
    Well, this seems to work differently on BSDs, so just forget about it.
    My guess is that you do have a command processor :-)

    if (system(NULL) == 0)
	fprintf(stderr, "%s: warning: cannot find a command processor\n",
		program);
*/

    add_to_environment("GIT_EDITOR",   "EDITOR",     "vi");
    add_to_environment("GIT_SHELL",    "SHELL",	     "/bin/sh");
    add_to_environment("GIT_RMAIL",    (char *)NULL, "emacs -f rmail");
    add_to_environment("GIT_PAGER",    (char *)NULL, "more");
    add_to_environment("GIT_VMSTAT",   (char *)NULL, "free");

    tty_get_capabilities();
    tty_kbdinit(TTY_RESTRICTED_INPUT);

    common_configuration_init();

    use_section("[GIT-FTI]");
    get_file_type_info();

    use_section("[GIT-Keys]");
    keys = read_keys(0, &errors);
    wait_msg += errors;
    configuration_end();

    wait_msg += (specific_configuration_init() == 0);


    use_section("[Setup]");

    configuration_getvarinfo("TempDirectory", &data, 1, DO_SEEK);
    TempDirectory = data ? tilde_expand(data) : "/tmp";

    stdout_log_name = xmalloc(32 + strlen(TempDirectory) + 1);
    stderr_log_name = xmalloc(32 + strlen(TempDirectory) + 1);
    sprintf(stdout_log_name, "%s/git.1.%d", TempDirectory, (int)pid);
    sprintf(stderr_log_name, "%s/git.2.%d", TempDirectory, (int)pid);

    AnsiColors        = get_flag_var("AnsiColors", OFF);
    UseLastScreenChar = get_flag_var("UseLastScreenChar",  OFF);


    use_section("[GIT-Setup]");

    if (AnsiColors == ON)
	TypeSensitivity = get_flag_var("TypeSensitivity", ON);
    else
	TypeSensitivity = OFF;

    ConfirmOnExit       = get_flag_var("ConfirmOnExit", OFF);
    NormalModeHelp      = get_string_var("NormalModeHelp",      "");
    CommandLineModeHelp = get_string_var("CommandLineModeHelp", "");


    use_section(AnsiColors ? cSection : bwSection);

    get_colorset_var(TitleColors, TitleFields, TITLE_FIELDS);

    use_section("[GIT-FTI]");
    get_file_type_info();

    use_section("[GIT-Keys]");
    keys = read_keys(keys, &errors);
    wait_msg += errors;

    if (keys == MAX_KEYS)
	fprintf(stderr, "%s: too many key sequences; only %d are allowed.\n",
		program, MAX_KEYS);

    tty_get_size(&SCREEN_X, &SCREEN_Y);
    tty_startup(UseLastScreenChar);

#ifndef HAVE_LONG_FILE_NAMES
    fprintf(stderr,
	    "%s: warning: your system doesn't support long file names.",
	    program);
#endif /* !HAVE_LONG_FILE_NAMES */

#ifdef HAVE_LINUX
    if (LinuxConsole)
	screen = xmalloc(4 + SCREEN_X * SCREEN_Y * 2);
#endif  /* HAVE_LINUX */

    status_init(SCREEN_X, SCREEN_Y - 1, NormalModeHelp);

    if (getuid() == 0)
	PS1[1] = '#';

    il_init(SCREEN_X, SCREEN_Y - 2);

    {
	int right_panel_x = (SCREEN_X >> 1);
	int left_panel_x  = right_panel_x + (SCREEN_X & 1);

	left_panel =panel_init(0,            1, SCREEN_Y-3, left_panel_x, ".");
	right_panel=panel_init(left_panel_x, 1, SCREEN_Y-3, right_panel_x,".");
    }

    configuration_end();

    tty_get_screen(screen);
    tty_set_mode(TTY_NONCANONIC);
    tty_defaults();

    dir_history       = NULL;
    dir_history_count = 0;
    dir_history_point = 0;

  restart:

    signals(SIG_OFF);

    il_restore(saved_il);

    if (wait_msg)
    {
	fprintf(stdout, "Press almost any key to continue\n");
	tty_goto(SCREEN_Y - 1, 0);
	tty_get_key(NULL);
	wait_msg = 0;
    }

    settitle();

    status(NULL, 0, 0, 1, MSG_OK, MSG_CENTERED);

    src_panel  = panel_no ? right_panel : left_panel;
    dest_panel = panel_no ?  left_panel : right_panel;

    /* Save the input line contents.  */
    saved_il = il_save();

    panel_action(src_panel, act_REFRESH, dest_panel, (void *)-1, 1);
    panel_action(dest_panel, act_REFRESH, src_panel, (void *)-1, 1);

    /* Restore the input line contents.  */
    il_restore(saved_il);

    panel_set_focus(src_panel, ON);

    if (first_time)
    {
	dir_history_add(panel_get_path(src_panel));
	first_time = 0;
    }

    signals(SIG_ON);

    il_set_static_text(
	strcat(truncate_string(panel_get_path(src_panel), PWD,
			       MAX_STATIC_SIZE - strlen(PS1) + 1),
	       PS1));
    saved_il = il_save();

    while(!app_end)
    {
	il_restore(saved_il);
	saved_il = il_save();
	il_full_update();
	il_get_contents(&cmdln);

	UserHeartAttack = 0;
	suspend_allowed = ON;
	ks  = tty_get_key(&repeat_count);
	key = ks->key_seq[0];
	suspend_allowed = OFF;

	signals(SIG_OFF);

	command = (command_t *)ks->aux_data;

	if (command)
	    if (command->builtin)
		key = - 1 - (command->name-built_in[0]) / MAX_BUILTIN_NAME;
	 else
	 {
	    if (command->name)
	    {
		panel_no_optimizations(src_panel);
		panel_no_optimizations(dest_panel);

		if (command->body)
		{
		    char *cmd = NULL;

		    retval = command_expand(command, &cmd,
					    src_panel, dest_panel);

		    if (retval)
		    {
			if (retval > 0)
			{
			    size_t msglen = 32 + strlen(command->name) +
					    strlen(cmd) + 1;
			    char *msg = xmalloc(msglen);

			    sprintf(msg, "%s: %s", command->name, cmd);
			    status(msg, 0, 0, 0, MSG_STATUS, 0);
			    xfree(msg);

			    if (command->hide)
			    {
				msg = xmalloc(64 + strlen(command->name) + 1);
				sprintf(msg, "Wait, running %s command %s...",
					"user-defined", command->name);
				il_message(msg);
				xfree(msg);
			    }

			    child_exit_code = start(cmd, command->hide);
			    xfree(cmd);

			    if (command->hide)
			    {
				if (child_exit_code != 0)
				{
				    tty_beep();
				    display_errors(command->name);
				}
			    }
			    else
			    {
				if (command->save_screen)
				    tty_get_screen(screen);

				tty_touch();

				if (command->pause)
				    wait_msg = 1;
			    }

			    if (child_exit_code == 0 && command->new_dir)
			    {
				char *expanded_dir =
				    tilde_expand(command->new_dir);

				panel_action(src_panel, act_CHDIR,
					     dest_panel, expanded_dir, 1);

				dir_history_add(panel_get_path(src_panel));
				xfree(expanded_dir);
			    }

			    if (child_exit_code == 0)
				if (retval == 2)
				    panel_unselect_all(src_panel);
				else
				    if (retval == 3)
					panel_unselect_all(dest_panel);

			    goto restart;
			}
			else
			    continue;
		    }
		    else
		    {
			char *msg = xmalloc(80+strlen((char *)ks->key_seq)+1);

			sprintf(msg,"%s: invalid command on key sequence %s !",
				command->name, command->sequence);
			il_read_char(msg, (char *)NULL,
				     IL_FREEZED|IL_BEEP|IL_SAVE|IL_ERROR);
			xfree(msg);
			continue;
		    }
		}
		else
		{
		    if (command->new_dir)
		    {
			char *expanded_dir = tilde_expand(command->new_dir);

			panel_action(src_panel, act_CHDIR, dest_panel,
				     expanded_dir, 1);

			dir_history_add(panel_get_path(src_panel));
			xfree(expanded_dir);
		    }

		    goto restart;
		}
	     }
	}

	signals(SIG_ON);

	switch (key)
	{
	    case key_INTERRUPT:
		il_free(saved_il);
		il_kill_line(IL_DONT_STORE);
		saved_il = il_save();
		break;

	    case BUILTIN_other_panel:
		signals(SIG_OFF);

		if (!two_panel_mode)
		    goto one_panel_mode;

		if ((repeat_count & 1) == 0)
		    break;

		panel_set_focus(src_panel, OFF);

		temp_panel = src_panel;
		src_panel  = dest_panel;
		dest_panel = temp_panel;

		panel_no = !panel_no;
		panel_set_focus(src_panel, ON);

		il_free(saved_il);
		il_set_static_text(
		    strcat(truncate_string(panel_get_path(src_panel), PWD,
					   MAX_STATIC_SIZE - strlen(PS1) + 1),
			   PS1));
		saved_il = il_save();
		break;

	    case BUILTIN_previous_line:
		signals(SIG_OFF);
		panel_action(src_panel,act_UP,dest_panel,NULL,repeat_count);
		break;

	    case BUILTIN_next_line:
		signals(SIG_OFF);
		panel_action(src_panel,act_DOWN,dest_panel,NULL,repeat_count);
		break;

	    case BUILTIN_action:
		action_status = 0;
		il_free(saved_il);
		il_get_contents(&cmdln);

		switch (*cmdln)
		{
		    case '+':
			action_status = panel_action(src_panel, act_SELECT_ALL,
						     dest_panel, NULL, 1);
			break;

		    case '-':
			action_status = panel_action(src_panel,
						     act_UNSELECT_ALL,
						     dest_panel, NULL, 1);
			break;

		    case '*':
			action_status = panel_action(src_panel, act_TOGGLE,
						     dest_panel, NULL, 1);
			break;

		    case 0:
			signals(SIG_OFF);
			action_status = panel_action(src_panel, act_ENTER,
						     dest_panel, screen, 1);
			break;

		    default:
			{
			    char *output_string;

			    if (history_expand(cmdln, &output_string) >= 0)
			    {
				int bg_cmd;

				if (is_an_empty_command(output_string))
				{
				    saved_il = il_save();
				    il_read_char("Void command.", (char *)NULL,
						 IL_FREEZED | IL_BEEP |
						 IL_SAVE    | IL_ERROR);
				    break;
				}

				bg_cmd = is_a_bg_command(output_string);

				il_kill_line(IL_DONT_STORE);
				il_insert_text(output_string);
				start(output_string, bg_cmd);

				if (bg_cmd)
				{
				    il_history(IL_RECORD);
				    il_kill_line(IL_DONT_STORE);
				}
				else
				{
				    panel_no_optimizations(src_panel);
				    panel_no_optimizations(dest_panel);
				    tty_touch();
				    action_status = -1;
				    wait_msg = 1;
				}
			    }
			    else
			    {
				il_read_char(output_string, (char *)NULL,
					     IL_FREEZED | IL_BEEP |
					     IL_SAVE    | IL_ERROR);
			    }
			}
			break;
		}

		if (action_status == 1)
		{
		    il_set_static_text(
			strcat(truncate_string(panel_get_path(src_panel), PWD,
					       MAX_STATIC_SIZE-strlen(PS1)+1),
			       PS1));
		    il_kill_line(IL_DONT_STORE);
		}
		else
		    if (action_status == -1)
		    {
			tty_get_screen(screen);
			il_history(IL_RECORD);
			il_kill_line(IL_DONT_STORE);

			saved_il = il_save();
			goto restart;
		    }

		saved_il = il_save();
		break;

	    case BUILTIN_select_entry:
		signals(SIG_OFF);

		for (i = 0; i < repeat_count; i++)
		    panel_action(src_panel, act_SELECT, dest_panel, NULL, 1);
		break;

	    case BUILTIN_scroll_down:
		signals(SIG_OFF);

		for (i = 0; i < repeat_count; i++)
		    panel_action(src_panel, act_PGUP, dest_panel, NULL, 1);

		break;

	    case BUILTIN_scroll_up:
		signals(SIG_OFF);

		for (i = 0; i < repeat_count; i++)
		    panel_action(src_panel, act_PGDOWN, dest_panel, NULL, 1);
		break;

	    case BUILTIN_beginning_of_panel:
		signals(SIG_OFF);
		panel_action(src_panel, act_HOME, dest_panel, NULL, 1);
		break;

	    case BUILTIN_end_of_panel:
		signals(SIG_OFF);
		panel_action(src_panel, act_END, dest_panel, NULL, 1);
		break;

	    case BUILTIN_hard_refresh:
		tty_touch();

	    case BUILTIN_refresh:
		panel_no_optimizations(src_panel);
		panel_no_optimizations(dest_panel);

		goto restart;

	    case BUILTIN_tty_mode:
		if ((repeat_count & 1) == 0)
		    break;

		tty_put_screen(screen);
		status(CommandLineModeHelp, 0, 0, 0, MSG_OK, MSG_CENTERED);

		while (1)
		{
		    il_restore(saved_il);
		    saved_il = il_save();
		    il_full_update();
		    il_get_contents(&cmdln);

		    current_mode = GIT_TERMINAL_MODE;
		    suspend_allowed = ON;
		    ks  = tty_get_key(&repeat_count);
		    key = ks->key_seq[0];
		    suspend_allowed = OFF;

		    command = (command_t *)ks->aux_data;
		    if (command && command->builtin)
			key = - 1 - (command->name - built_in[0]) /
				     MAX_BUILTIN_NAME;

		    if (key == BUILTIN_tty_mode && (repeat_count & 1))
		    {
			il_free(saved_il);
			saved_il = il_save();
			break;
		    }

		    switch (key)
		    {
			case key_INTERRUPT:
			    il_free(saved_il);
			    il_kill_line(IL_DONT_STORE);
			    saved_il = il_save();
			    break;

			case BUILTIN_action:
			    if (*cmdln)
			    {
				char *output_string;

				il_free(saved_il);

				if (history_expand(cmdln, &output_string) < 0)
				{
				    il_read_char(output_string, (char *)NULL,
						 IL_FREEZED | IL_BEEP |
						 IL_SAVE    | IL_ERROR);
				    saved_il = il_save();
				    break;
				}

				tty_put_screen(screen);
				il_kill_line(IL_DONT_STORE);
				il_insert_text(output_string);
				start(output_string, 0);
				tty_get_screen(screen);
				il_history(IL_RECORD);
				status(CommandLineModeHelp, 0, 0, 0,
				       MSG_OK, MSG_CENTERED);
				il_kill_line(IL_DONT_STORE);
				saved_il = il_save();
			    }

			    break;

			case BUILTIN_previous_history_element:
			case BUILTIN_previous_line:
			    il_free(saved_il);

			    for (i = 0; i < repeat_count; i++)
				il_history(IL_PREVIOUS);

			    saved_il = il_save();
			    break;

			case BUILTIN_next_history_element:
			case BUILTIN_next_line:
			    il_free(saved_il);

			    for (i = 0; i < repeat_count; i++)
				il_history(IL_NEXT);

			    saved_il = il_save();
			    break;

			case BUILTIN_refresh:
			    panel_no_optimizations(src_panel);
			    panel_no_optimizations(dest_panel);
			    tty_put_screen(screen);
			    status(CommandLineModeHelp, 0, 0, 0,
				   MSG_OK, MSG_CENTERED);
			    break;

			case BUILTIN_exit:
			    if (ConfirmOnExit == OFF ||
				il_read_char(exit_msg,"yn",IL_FREEZED) == 'y')
			    {
				app_end = 1;
				goto end_tty_mode;
			    }

			    status(CommandLineModeHelp, 0, 0, 0,
				   MSG_OK, MSG_CENTERED);
			    break;

			default:
			    if (key)
			    {
				il_free(saved_il);

				while (repeat_count--)
				    il_dispatch_commands(key, IL_MOVE|IL_EDIT);

				saved_il = il_save();
			    }
			    break;
		    }
		}

	      end_tty_mode:

		panel_no_optimizations(src_panel);
		panel_no_optimizations(dest_panel);
		tty_touch();
		status(NULL, 0, 0, 1, MSG_OK, MSG_CENTERED);

		current_mode = GIT_SCREEN_MODE;

		if (app_end)
		    continue;

		goto restart;

	    case BUILTIN_copy:
		panel_action(src_panel, act_COPY, dest_panel, NULL, 1);
		break;

	    case BUILTIN_move:
		panel_action(src_panel, act_MOVE, dest_panel, NULL, 1);
		break;

	    case BUILTIN_make_directory:
		signals(SIG_OFF);
		panel_action(src_panel, act_MKDIR, dest_panel, NULL, 1);
		break;

	    case BUILTIN_delete:
		panel_action(src_panel, act_DELETE, dest_panel, NULL, 1);
		break;

	    case BUILTIN_panel_enable_next_mode:

	    case BUILTIN_panel_enable_owner_group:
	    case BUILTIN_panel_enable_date_time:
	    case BUILTIN_panel_enable_size:
	    case BUILTIN_panel_enable_mode:
	    case BUILTIN_panel_enable_full_name:
		signals(SIG_OFF);
		panel_action(src_panel,
			     act_ENABLE_NEXT_MODE -
			     (key - BUILTIN_panel_enable_next_mode),
			     NULL, NULL, 1);
		break;

	    case BUILTIN_panel_sort_next_method:

	    case BUILTIN_panel_sort_by_name:
	    case BUILTIN_panel_sort_by_extension:
	    case BUILTIN_panel_sort_by_size:
	    case BUILTIN_panel_sort_by_date:
	    case BUILTIN_panel_sort_by_mode:
	    case BUILTIN_panel_sort_by_owner_id:
	    case BUILTIN_panel_sort_by_group_id:
	    case BUILTIN_panel_sort_by_owner_name:
	    case BUILTIN_panel_sort_by_group_name:
		signals(SIG_OFF);
		panel_action(src_panel,
			     act_SORT_NEXT_METHOD -
			     (key - BUILTIN_panel_sort_next_method),
			     NULL, NULL, 1);
		break;

	    case BUILTIN_exit:
		signals(SIG_OFF);

		if (ConfirmOnExit == OFF ||
		    il_read_char(exit_msg, "yn", IL_FREEZED) == 'y')
		    app_end = 1;

		break;

	    case BUILTIN_entry_to_input_line:
		signals(SIG_OFF);
		len = strlen(cmdln);

		srcptr = panel_get_current_file_name(src_panel);
		ptr = xmalloc(ptrlen = 1 + 1 + strlen(srcptr) + 1 + 1 + 1);

	      copy_to_cmdln:

		il_free(saved_il);
		sprintf(ptr, " \"%s\" ", srcptr);
		cmdln = xrealloc(cmdln, len + ptrlen);

		for (i = 0; ptr[i]; i++)
		    if (!is_print(ptr[i]))
		    {
			tty_beep();
			ptr[i] = 0;
			break;
		    }

		strcpy(&cmdln[len], ptr);
		il_insert_text(ptr);
		xfree(ptr);
		saved_il = il_save();
		break;

	    case BUILTIN_other_path_to_input_line:
		signals(SIG_OFF);

		ptrlen = 1 + 1 + dest_panel->pathlen + 1 + 1 + 1;
		ptr = xmalloc(ptrlen);
		srcptr = dest_panel->path;

		goto copy_to_cmdln;

	    case BUILTIN_selected_entries_to_input_line:
		signals(SIG_OFF);
		len = strlen(cmdln);
		il_free(saved_il);

		panel_init_iterator(src_panel);

		while ((entry = panel_get_next(src_panel)) != -1)
		{
		    srcptr = src_panel->dir_entry[entry].name;
		    ptr = xmalloc(ptrlen = 1 + 1 + strlen(srcptr) + 1 + 1 + 1);
		    sprintf(ptr, " \"%s\" ", srcptr);
		    cmdln = xrealloc(cmdln, len + ptrlen);

		    for (i = 0; ptr[i]; i++)
			if (!is_print(ptr[i]))
			{
			    tty_beep();
			    ptr[i] = 0;
			    saved_il = il_save();
			    break;
			}

		    strcpy(&cmdln[len], ptr);
		    il_insert_text(ptr);
		    xfree(ptr);
		}

		saved_il = il_save();
		break;

	    case BUILTIN_previous_history_element:
		signals(SIG_OFF);
		il_free(saved_il);

		for (i = 0; i < repeat_count; i++)
		    il_history(IL_PREVIOUS);

		saved_il = il_save();
		break;

	    case BUILTIN_next_history_element:
		signals(SIG_OFF);
		il_free(saved_il);

		for (i = 0; i < repeat_count; i++)
		    il_history(IL_NEXT);

		saved_il = il_save();
		break;

	    case BUILTIN_switch_panels:
		if ((repeat_count & 1) == 0)
		    break;

		signals(SIG_OFF);
		panel_no_optimizations(src_panel);
		panel_no_optimizations(dest_panel);
		panel_action(src_panel, act_SWITCH, dest_panel, NULL, 1);
		panel_action(dest_panel, act_REFRESH, src_panel, (void*)-1, 1);
		panel_action(src_panel, act_REFRESH, dest_panel, (void*)-1, 1);
		break;

	    case BUILTIN_change_directory:
		signals(SIG_OFF);

		if (il_read_line("Directory: ", &input, (char *)NULL,
				 command->history))
		{
		    char *expanded_input;

		    if (input[0] == 0)
			break;

		    panel_action(src_panel, act_CHDIR, dest_panel,
				 expanded_input = tilde_expand(input), 1);

		    dir_history_add(panel_get_path(src_panel));

		    xfree(expanded_input);
		    xfree(input);
		    input = NULL;

		    il_restore(saved_il);
		    il_set_static_text(
			strcat(truncate_string(panel_get_path(src_panel), PWD,
					       MAX_STATIC_SIZE-strlen(PS1)+1),
			       PS1));
		    saved_il = il_save();
		}

		break;

	    case BUILTIN_select_files_matching_pattern:
		signals(SIG_OFF);

		if (il_read_line(
		    "Select files matching one of the patterns: ",
		    &input, (char *)NULL, command->history))
		{
		    if (input[0] == 0)
			break;

		    panel_action(src_panel, act_PATTERN_SELECT,
				 dest_panel, input, 1);

		    xfree(input);
		    input = NULL;
		}
		break;

	    case BUILTIN_unselect_files_matching_pattern:
		signals(SIG_OFF);

		if (il_read_line(
		    "Unselect files matching one of the patterns: ",
		    &input, (char *)NULL, command->history))
		{
		    if (input[0] == 0)
			break;

		    panel_action(src_panel, act_PATTERN_UNSELECT,
				 dest_panel, input, 1);

		    xfree(input);
		    input = NULL;
		}
		break;

	    case BUILTIN_adapt_current_directory:
		signals(SIG_OFF);

		panel_action(src_panel, act_CHDIR, dest_panel,
			     dest_panel->path, 1);

		dir_history_add(panel_get_path(src_panel));

		il_free(saved_il);
		il_set_static_text(
		    strcat(truncate_string(panel_get_path(src_panel), PWD,
					   MAX_STATIC_SIZE - strlen(PS1) + 1),
			   PS1));
		saved_il = il_save();
		break;

	    case BUILTIN_adapt_other_directory:
		signals(SIG_OFF);

		panel_action(dest_panel, act_CHDIR, src_panel,
			     src_panel->path, 1);

		dir_history_add(panel_get_path(dest_panel));
		break;

	    case BUILTIN_set_scroll_step:
		signals(SIG_OFF);

		if (il_read_line("Scroll step: ", &input, (char *)NULL,
				 command->history))
		{
		    if (input[0] == 0)
			break;

		    panel_action(src_panel, act_SET_SCROLL_STEP,
				 dest_panel, input, 1);

		    xfree(input);
		    input = NULL;
		}

		break;

	    case BUILTIN_isearch_backward:
		signals(SIG_OFF);

		previous_isearch_failed = 0;
		il_isearch("I-search backward: ", (char **)NULL,
			   IL_ISEARCH_BEGIN, (int *)NULL);
		panel_action(src_panel, act_ISEARCH_BEGIN,
			     dest_panel, NULL, 1);

		for(;;)
		{
		    isearch_aux_t iai;

		    if (il_isearch((char *)NULL, &input, IL_ISEARCH_BACKWARD,
				   &iai.action) == NULL)
			break;

		    /* Wrap around.  */
		    if (iai.action == IL_ISEARCH_ACTION_RETRY &&
			previous_isearch_failed)
		    {
			panel_set_wrapped_isearch_flag(src_panel);
			previous_isearch_failed = 0;
		    }

		    iai.string = input;
		    panel_action(src_panel, act_ISEARCH_BACKWARD,
				 dest_panel, &iai, 1);

		    if (iai.action == IL_ISEARCH_ACTION_FAILED)
		    {
			previous_isearch_failed = 1;
			tty_beep();
		    }
		    else
			if (iai.length < strlen(input))
			{
			    il_backward_delete_char();
			    il_full_update();
			}
		}

		panel_action(src_panel, act_ISEARCH_END, dest_panel, NULL, 1);
		il_isearch((char *)NULL, (char **)NULL,
			   IL_ISEARCH_END, (int *)NULL);
		break;

	    case BUILTIN_isearch_forward:
		signals(SIG_OFF);

		previous_isearch_failed = 0;
		il_isearch("I-search: ", (char **)NULL,
			   IL_ISEARCH_BEGIN, (int *)NULL);
		panel_action(src_panel, act_ISEARCH_BEGIN,
			     dest_panel, NULL, 1);

		for(;;)
		{
		    isearch_aux_t iai;

		    if (il_isearch((char *)NULL, &input, IL_ISEARCH_FORWARD,
				   &iai.action) == NULL)
			break;

		    /* Wrap around.  */
		    if (iai.action == IL_ISEARCH_ACTION_RETRY &&
			previous_isearch_failed)
		    {
			panel_set_wrapped_isearch_flag(src_panel);
			previous_isearch_failed = 0;
		    }

		    iai.string = input;
		    panel_action(src_panel, act_ISEARCH_FORWARD,
				 dest_panel, &iai, 1);

		    if (iai.action == IL_ISEARCH_ACTION_FAILED)
		    {
			previous_isearch_failed = 1;
			tty_beep();
		    }
		    else
			if (iai.length < strlen(input))
			{
			    il_backward_delete_char();
			    il_full_update();
			}
		}

		panel_action(src_panel, act_ISEARCH_END, dest_panel, NULL, 1);
		il_isearch((char *)NULL, (char **)NULL,
			   IL_ISEARCH_END, (int *)NULL);
		break;

	    case BUILTIN_reset_directory_history:
		signals(SIG_OFF);

		dir_history_reset();
		dir_history_add(panel_get_path(src_panel));
		break;

	    case BUILTIN_previous_directory:
		signals(SIG_OFF);

		dir_history_prev(src_panel, dest_panel);

		il_restore(saved_il);
		il_set_static_text(
		    strcat(truncate_string(panel_get_path(src_panel), PWD,
					   MAX_STATIC_SIZE - strlen(PS1) + 1),
			   PS1));
		saved_il = il_save();
		break;

	    case BUILTIN_next_directory:
		signals(SIG_OFF);

		dir_history_next(src_panel, dest_panel);

		il_restore(saved_il);
		il_set_static_text(
		    strcat(truncate_string(panel_get_path(src_panel), PWD,
					   MAX_STATIC_SIZE - strlen(PS1) + 1),
			   PS1));
		saved_il = il_save();
		break;

	    case BUILTIN_enlarge_other_panel:
		signals(SIG_OFF);

	      one_panel_mode:

		panel_set_focus(src_panel, OFF);

		temp_panel = src_panel;
		src_panel  = dest_panel;
		dest_panel = temp_panel;

		panel_no = !panel_no;
		panel_set_focus(src_panel, ON);
		panel_activate(src_panel);

		il_free(saved_il);
		il_set_static_text(
		    strcat(truncate_string(panel_get_path(src_panel), PWD,
					   MAX_STATIC_SIZE - strlen(PS1) + 1),
			   PS1));
		saved_il = il_save();

	    case BUILTIN_enlarge_panel:
		signals(SIG_OFF);

		panel_no_optimizations(src_panel);
		panel_no_optimizations(dest_panel);
		tty_touch();
		panel_deactivate(dest_panel);
		panel_resize(src_panel, 0, 1, SCREEN_Y - 3, SCREEN_X);

		two_panel_mode = 0;
		panel_action(src_panel,  act_ENABLE_ALL, NULL, NULL, 1);
		panel_action(dest_panel, act_ENABLE_ALL, NULL, NULL, 1);

		panel_update(src_panel);
		break;

	    case BUILTIN_two_panels:
		signals(SIG_OFF);

		panel_no_optimizations(src_panel);
		panel_no_optimizations(dest_panel);
		tty_touch();
		panel_activate(dest_panel);

		{
		    int right_panel_x = (SCREEN_X >> 1);
		    int left_panel_x  = right_panel_x + (SCREEN_X & 1);

		    panel_resize(src_panel, 0, 1, SCREEN_Y - 3, left_panel_x);
		    panel_resize(dest_panel, left_panel_x, 1, SCREEN_Y - 3,
				 right_panel_x);
		}

		two_panel_mode = 1;
		panel_action(src_panel,  act_ENABLE_SIZE, NULL, NULL, 1);
		panel_action(dest_panel, act_ENABLE_SIZE, NULL, NULL, 1);

		panel_update(src_panel);
		panel_update(dest_panel);
		break;

	    case BUILTIN_lock:
		/* Turn echo off.  */
		il_echo(0);

		lock_password = NULL;
		il_read_line("Enter a password: ", &lock_password,
			     (char *)NULL, (xstack_t *)NULL);

		if (lock_password == NULL || *lock_password == '\0')
		{
		    il_echo(1);
		    break;
		}

		for (unlock_password = NULL;;)
		{
		    il_read_line("Enter password to unlock: ",
				 &unlock_password, (char *)NULL,
				 (xstack_t *)NULL);
		    il_message(lock_think);
		    sleep(1);

		    if (unlock_password &&
			strcmp(lock_password, unlock_password) == 0)
			break;

		    il_message(lock_bad);
		    tty_beep();
		    sleep(2);
		}

		il_message(lock_ok);
		sleep(1);
		xfree(lock_password);
		xfree(unlock_password);

		/* Turn echo back on.  */
		il_echo(1);
		break;

	    case BUILTIN_quick_compare_panels:
		cmp_mode = CMPDIR_QUICK;
		panel_action(src_panel, act_CMPDIR, dest_panel, &cmp_mode, 1);
		break;

	    case BUILTIN_thorough_compare_panels:
		cmp_mode = CMPDIR_THOROUGH;
		panel_action(src_panel, act_CMPDIR, dest_panel, &cmp_mode, 1);
		break;

	    case BUILTIN_name_downcase:
		to_case = CASE_DOWN;
		panel_action(src_panel, act_CASE, dest_panel, &to_case, 1);
		break;

	    case BUILTIN_name_upcase:
		to_case = CASE_UP;
		panel_action(src_panel, act_CASE, dest_panel, &to_case, 1);
		break;

	    case BUILTIN_up_one_dir:
		panel_action(src_panel, act_UP_ONE_DIR, dest_panel, NULL, 1);
		break;

	    case BUILTIN_compare:
		panel_action(src_panel, act_COMPARE, dest_panel, NULL, 1);
		break;

	    case BUILTIN_bin_packing:
		signals(SIG_OFF);

		if (il_read_line("Bin size (in Kb): ", &input, "0",
				 command->history))
		{
		    if (input[0] == 0)
			break;

		    panel_action(src_panel, act_BIN_PACKING,
				 dest_panel, input, 1);

		    xfree(input);
		    input = NULL;
		}
		break;

	    default:
		signals(SIG_OFF);

		if (key)
		{
		    il_free(saved_il);

		    while (repeat_count--)
			il_dispatch_commands(key, IL_MOVE | IL_EDIT);

		    saved_il = il_save();
		}
		break;
	}

	signals(SIG_ON);
    }

    signals(SIG_OFF);
    panel_end(left_panel);
    panel_end(right_panel);
    tty_set_mode(TTY_CANONIC);
    tty_defaults();

    if (il)
	il_end();

    status_end();
    removelog();

    tty_exit(screen);

    return 0;
}
