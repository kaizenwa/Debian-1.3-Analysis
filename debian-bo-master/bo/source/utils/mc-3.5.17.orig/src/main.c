
/* Main program for the Midnight Commander
   Copyright (C) 1994, 1995, 1996, 1997 The Free Software Foundation
   
   Written by: 1994, 1995, 1996, 1997 Miguel de Icaza
               1994, 1995 Janne Kukonlehto
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <config.h>
#ifdef _OS_NT
#    include <windows.h>
#endif

#include "tty.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/param.h>

#include <sys/stat.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif

/* unistd.h defines _POSIX_VERSION on POSIX.1 systems. */
#if defined(HAVE_DIRENT_H) || defined(_POSIX_VERSION)
#   include <dirent.h>
#   define NLENGTH(dirent) (strlen ((dirent)->d_name))
#else
#   define dirent direct
#   define NLENGTH(dirent) ((dirent)->d_namlen)

#   ifdef HAVE_SYS_NDIR_H
#       include <sys/ndir.h>
#   endif /* HAVE_SYS_NDIR_H */

#   ifdef HAVE_SYS_DIR_H
#       include <sys/dir.h>
#   endif /* HAVE_SYS_DIR_H */

#   ifdef HAVE_NDIR_H
#       include <ndir.h>
#   endif /* HAVE_NDIR_H */
#endif /* not (HAVE_DIRENT_H or _POSIX_VERSION) */

#if HAVE_SYS_WAIT_H
#   include <sys/wait.h>	/* For waitpid() */
#endif

#include <errno.h>
#include <pwd.h>
#include <ctype.h>
#include <fcntl.h>	/* For O_RDWR */
#include <signal.h>

/* Program include files */
#include "mad.h"
#include "dir.h"
#include "color.h"
#include "global.h"
#include "util.h"
#include "dialog.h"
#include "menu.h"
#include "file.h"
#include "panel.h"
#include "main.h"
#include "win.h"
#include "user.h"
#include "mem.h"
#include "mouse.h"
#include "option.h"
#include "tree.h"
#include "cons.saver.h"
#include "subshell.h"
#include "key.h"	/* For init_key() and mi_getch() */
#include "setup.h"	/* save_setup() */
#include "profile.h"	/* free_profiles() */
#include "boxes.h"
#include "layout.h"
#include "cmd.h"		/* Normal commands */
#include "hotlist.h"
#include "panelize.h"
#include "learn.h"
#include "listmode.h"
#include "background.h"

/* Listbox for the command history feature */
#include "widget.h"
#include "command.h"
#include "wtools.h"
#include "complete.h"		/* For the free_completion */

#include "chmod.h"
#include "chown.h"

#include "../vfs/vfs.h"
#include "../vfs/extfs.h"

#ifdef HAVE_XVIEW
#   include "../xv/xvmain.h"
#endif

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

#ifndef USE_VFS
#ifdef USE_NETCODE
#undef USE_NETCODE
#endif
#endif

/* "$Id: main.c,v 1.23 1995/02/21 19:06:29 miguel Exp $" */

/* When the modes are active, left_panel, right_panel and tree_panel */
/* Point to a proper data structure.  You should check with the functions */
/* get_current_type and get_other_type the types of the panels before using */
/* This pointer variables */

/* The structures for the panels */
WPanel *left_panel;
WPanel *right_panel;

/* The pointer to the tree */
WTree *the_tree;

/* The Menubar */
WMenu *the_menubar;

/* Pointers to the selected and unselected panel */
WPanel *current_panel = NULL;

/* Set when we want use advanced chmod command instead of chmod and/or chown */
int advanced_chfns = 0;

/* Set when main loop should be terminated */
volatile int quit = 0;

/* Set if you want the possible completions dialog for the first time */
int show_all_if_ambiguous = 0;

/* Set when cd symlink following is desirable (bash mode) */
int cd_symlinks = 1;

/* If set then dialogs just clean the screen when refreshing, else */
/* they do a complete refresh, refreshing all the parts of the program */
int fast_refresh = 0;

/* If true, marking a files moves the cursor down */
int   mark_moves_down = 1;

/* If true, at startup the user-menu is invoked */
int   auto_menu = 0;

/* If true, use + and \ keys normally and select/unselect do if M-+ / M-\ and M--
   and keypad + / - */
int   alternate_plus_minus = 0;

/* If true, then the +, - and \ keys have their special meaning only if the
 * command line is emtpy, otherwise they behave like regular letters
 */
int   only_leading_plus_minus = 1;

/* If true, after executing a command, wait for a keystroke */
enum { pause_never, pause_on_dumb_terminals, pause_always };

int   pause_after_run = pause_on_dumb_terminals;

/* It true saves the setup when quitting */
int auto_save_setup = 0;

/* If true, be eight bit clean */
int eight_bit_clean = 0;

/* If true, then display chars 0-255, else iso-8859-1,
   requires eight_bit_clean */
int full_eight_bits = 0;

/* If true use the internal viewer */
int use_internal_view = 1;

/* Have we shown the fast-reload warning in the past? */
int fast_reload_w = 0;

/* Move page/item? When clicking on the top or bottom of a panel */
int mouse_move_pages = 1;

/* If true: l&r arrows are used to chdir if the input line is empty */
int navigate_with_arrows = 0;

/* If it is set, the commander will iconify itself when executing a program */
int iconify_on_exec = 1;

/* The prompt */
char *prompt = 0;

/* The widget where we draw the prompt */
WLabel *the_prompt;

/* The hint bar */
WLabel *the_hint;

/* The button bar */
WButtonBar *the_bar;
#ifdef HAVE_X
WButtonBar *the_bar2;
#endif

/* For slow terminals */
int slow_terminal = 0;

/* use mouse? */
int use_mouse_p = GPM_MOUSE;

/* If true, assume we are running on an xterm terminal */
static int force_xterm = 0;

/* Controls screen clearing before an exec */
int clear_before_exec = 1;

/* Asks for confirmation before deleting a file */
int confirm_delete = 1;

/* Asks for confirmation before overwriting a file */
int confirm_overwrite = 1;

/* Asks for confirmation before leaving the program */
int confirm_exit = 1;

/* Asks for confirmation when using F3 to view a directory and there
   are tagged files */
int confirm_view_dir = 0;

/* This flag is set by xterm detection routine in function main() */
/* It is used by function view_other_cmd() */
int xterm_flag = 0;

/* This flag indicates if the pull down menus by default drop down */
int drop_menus = 0;

/* The dialog handle for the main program */
Dlg_head *midnight_dlg;

/* Subshell: if set, then the prompt was not saved on CONSOLE_SAVE */
/* We need to paint it after CONSOLE_RESTORE, see: load_prompt */
int update_prompt = 0;

/* The home directory */
char *home_dir;

/* The value of the other directory, only used when loading the setup */
char *other_dir = 0;
char *this_dir = 0;

/* If true, then print on stdout the last directory we were at */
static int print_last_wd = 0;
static char *last_wd_string;
static int print_last_revert = 0;

/* widget colors for the midnight commander */
int midnight_colors [4];

/* Force colors, only used by Slang */
int force_colors = 0;

/* colors specified on the command line: they override any other setting */
char *command_line_colors;

/* File name to view if argument was supplied */
char *view_one_file = 0;

/* File name to view if argument was supplied */
char *edit_one_file = 0;

/* Used so that widgets know if they are being destroyed or
   shut down */
int midnight_shutdown = 0;

/* to show nice prompts */
static int last_paused = 0;

/* Only used at program boot */
int boot_current_is_left = 1;

/* Used for keeping track of the original stdout */
int stdout_fd = 0;

/* Ugly hack in order to distinguish between left and right panel in menubar */
int is_right;
#define MENU_PANEL_IDX  (is_right ? 1 : 0)

char cmd_buf [512];

static void save_setup_cmd (void);
static void menu_cmd (void);

static const int status_mouse_support = 
#ifdef HAVE_LIBGPM
    1;
#else
    0;
#endif

const int status_using_ncurses =
#ifdef HAVE_SLANG
    0;
#else
#ifdef USE_NCURSES
    1;
#else
    0;
#endif
#endif

static const int status_using_old_tools =
#ifdef OLD_TOOLS
    1;
#else
    0;
#endif

int panel_event    (Gpm_Event *event, WPanel *panel);
int menu_bar_event (Gpm_Event *event, void *);

void try_to_select (WPanel *panel, char *name)
{
    Xtry_to_select (panel, name);
    select_item (panel);
    display_mini_info (panel);
}

/* If we moved to the parent directory move the selection pointer to
   the old directory name */
void cd_try_to_select (WPanel *panel)
{
    char *p, *q;
    int i, j = 4;

    if (strlen (panel->lwd) > strlen (panel->cwd)
	&& strncmp (panel->cwd, panel->lwd, strlen (panel->cwd)) == 0
	&& strchr (panel->lwd + strlen (panel->cwd) + 1, PATH_SEP) == 0)
	try_to_select (panel, panel->lwd);
    else
#ifdef USE_VFS
	if ((!strncmp (panel->lwd, "tar:", 4) && 
             !strncmp (panel->lwd + 4, panel->cwd, strlen (panel->cwd))) ||
             ((i = extfs_prefix_to_type (panel->lwd)) != -1 && 
             !strncmp (panel->lwd + (j = strlen (extfs_get_prefix (i)) + 1), 
             panel->cwd, strlen (panel->cwd)))) {
        p = strdup (panel->lwd + j + strlen (panel->cwd));
        q = strchr (p, PATH_SEP);
        if (q != NULL && (q != p || (q = strchr (q + 1, PATH_SEP)) != NULL))
            *q = 0;
        try_to_select (panel, p);
        free (p);
    } else
#endif
	try_to_select (panel, NULL);
}

void reload_panelized (WPanel *panel)
{
    int i, j;
    dir_list *list = &panel->dir;
    
    if (panel != current_panel)
	mc_chdir (panel->cwd);

    for (i = 0, j = 0; i < panel->count; i++){
	if (mc_lstat (list->list [i].fname, &list->list [i].buf)){
	    free (list->list [i].fname);
	    continue;
	}
	if (j != i)
	    list->list [j] = list->list [i];
	j++;
    }
    if (j == 0)
	panel->count = set_zero_dir (list);
    else
	panel->count = j;

    if (panel != current_panel)
	mc_chdir (current_panel->cwd);
}

void update_one_panel (int which, int force_update, char *current_file)
{
    int free_pointer;
    WPanel *panel;

    if (get_display_type (which) != view_listing)
	return;

    panel = (WPanel *) get_panel_widget (which);

    if (force_update & UP_RELOAD){
	panel->is_panelized = 0;

	ftpfs_flushdir ();
	bzero (&(panel->dir_stat), sizeof (panel->dir_stat));
    }
    /* If current_file == -1 (an invalid pointer) then preserve selection */
    if (current_file == UP_KEEPSEL){
	free_pointer = 1;
	current_file = strdup (panel->dir.list [panel->selected].fname);
    } else
	free_pointer = 0;
    
    if (panel->is_panelized)
	reload_panelized (panel);
    else
	panel_reload (panel);

    try_to_select (panel, current_file);
    panel->dirty = 1;

    if (free_pointer)
	free (current_file);
}

/* This routine reloads the directory in both panels. It tries to
** select current_file in current_panel and other_file in other_panel.
** If current_file == -1 then it automatically sets current_file and
** other_file to the currently selected files in the panels.
**
** if force_update has the UP_ONLY_CURRENT bit toggled on, then it
** will not reload the other panel.
*/
void update_panels (int force_update, char *current_file, char *other_file)
{
    int reload_other = !(force_update & UP_ONLY_CURRENT);
    WPanel *panel;
    
    update_one_panel (get_current_index (), force_update, current_file);
    if (reload_other)
	update_one_panel (get_other_index (), force_update, other_file);

    if (get_current_type () == view_listing)
	panel = (WPanel *) get_panel_widget (get_current_index ());
    else
	panel = (WPanel *) get_panel_widget (get_other_index ());

    mc_chdir (panel->cwd);
}

#ifdef WANT_PARSE
static void select_by_index (WPanel *panel, int i);

/* Called by parse_control_file */
static int index_by_name (file_entry *list, int count)
{
    char *name;
    int i;

    name = strtok (NULL, " \t\n");
    if (!name || !*name)
	return -1;
    for (i = 0; i < count; i++){
	if (strcmp (name, list[i].fname) == 0)
	    return i;
    }
    return -1;
}

/* Called by parse_control_file */
static void select_by_index (WPanel *panel, int i)
{
    if (i >= panel->count)
	return;
    
    unselect_item (panel);
    panel->selected = i;

#ifndef HAVE_X    
    while (panel->selected - panel->top_file >= ITEMS (panel)){
	/* Scroll window half screen */
	panel->top_file += ITEMS (panel)/2;
	paint_dir (panel);
	select_item (panel);
    } 
    while (panel->selected < panel->top_file){
	/* Scroll window half screen */
	panel->top_file -= ITEMS (panel)/2;
	if (panel->top_file < 0) panel->top_file = 0;
	paint_dir (panel);
    } 
#endif
    select_item (panel);
}

/* Called by my_system
   No error reporting, just exits on the first sign of trouble */
static void parse_control_file (void)
{
    char *data, *current;
    WPanel *panel;
    file_entry *list;
    int i;
    FILE *file;
    struct stat s;
    
    if ((file = fopen (control_file, "r")) == NULL){
	return;
    }
    /* Use of fstat prevents race conditions */
    if (fstat (fileno (file), &s) != 0){
	fclose (file);
	return;
    }
#ifndef _OS_NT
    /* Security: Check that the user owns the control file to prevent
       other users from playing tricks on him/her. */
    if (s.st_uid != getuid ()){
	fclose (file);
	return;
    }
#endif
    data = (char *) xmalloc (s.st_size+1, "main, parse_control_file");
    if (!data){
	fclose (file);
	return;
    }
    if (s.st_size != fread (data, 1, s.st_size, file)){
	free (data);
	fclose (file);
	return;
    }
    data [s.st_size] = 0;
    fclose (file);
    
    /* The Control file has now been loaded to memory -> start parsing. */
    current = strtok (data, " \t\n");
    while (current && *current){
	if (isupper (*current)){
	    if (get_other_type () != view_listing)
		break;
	    else
		panel = other_panel;
	} else
	    panel = current_panel;

	list = panel->dir.list;
	*current = tolower (*current);

	if (strcmp (current, "clear_tags") == 0){
	    panel->marked = 0;
	    panel->total = 0;
	    panel->dirs_marked = 0;
	    for (i = 0; i < panel->count; i++)
		file_mark (list, i, 0);

	} else if (strcmp (current, "tag") == 0){
	    i = index_by_name (list, panel->count);
	    if (i >= 0 && ! list[i].f.marked){
		file_mark (panel, i, 1);
		if (S_ISDIR (list[i].buf.st_mode))
		    panel->dirs_marked++;
		panel->marked++;
		panel->total += list[i].buf.st_size;
	    }
	} else if (strcmp (current, "untag") == 0){
	    i = index_by_name (list, panel->count);
	    if (i >= 0 && list[i].f.marked){
		set_mark (panel, i, 0);
		if (S_ISDIR (list[i].buf.st_mode))
		    panel->dirs_marked--;
		panel->marked--;
		panel->total -= list[i].buf.st_size;
	    }
	} else if (strcmp (current, "select") == 0){
	    i = index_by_name (list, panel->count);
	    if (i >= 0){
		select_by_index (panel, i);
	    }
	} else if (strcmp (current, "change_panel") == 0){
	    change_panel ();
	} else if (strcmp (current, "cd") == 0){
	    int change = 0;
	    current = strtok (NULL, " \t\n");
	    if (!current) break;
	    if (cpanel != panel){
		change_panel ();
		change = 1;
	    }
	    do_cd (current, cd_parse_command);
	    if (change)
		change_panel ();
	} else {
	    /* Unknown command -> let's give up */
	    break;
	}
	current = strtok (NULL, " \t\n");
    }

    free (data);
    paint_panel (cpanel);
    paint_panel (opanel);
}
#else
#define parse_control_file()
#endif /* WANT_PARSE */

void clr_scr (void)
{
    standend ();
    dlg_erase (midnight_dlg);
    mc_refresh ();
    doupdate ();
}

/* Sets up the terminal before executing a program */
static void pre_exec (void)
{
    use_dash (0);
    if (clear_before_exec)
	clr_scr ();
    else {
	if (!(console_flag || xterm_flag))
	    printf ("\n\n");
    }

#ifdef HAVE_TK
    if (iconify_on_exec)
	tk_evalf ("wm iconify .");
#endif
#ifndef HAVE_X
    channels_down ();
    if (use_mouse_p)
	shut_mouse ();

    reset_shell_mode ();
    keypad (stdscr, FALSE);
    endwin ();

    if (alternate_plus_minus && (console_flag || xterm_flag)) {
        fprintf (stderr, "\033>"); fflush (stderr);
    }
        
    /* on xterms: maybe endwin did not leave the terminal on the shell
     * screen page: do it now.
     *
     * Do not move this before endwin: in some systems rmcup includes
     * a call to clear screen, so it will end up clearing the sheel screen.
     */
    if (!status_using_ncurses){
	do_exit_ca_mode ();
    }
#endif /* HAVE_X */
}

/* Restores the terminal after an exec, see execute for extra post-exec code */
static void post_exec (void)
{
#ifndef HAVE_X
    do_enter_ca_mode ();

    /* FIXME: Missing on slang endwin? */
    reset_prog_mode ();
    flushinp ();
    
    keypad (stdscr, TRUE);
    mc_raw_mode ();
    channels_up ();
    if (use_mouse_p)
	init_mouse ();
    if (alternate_plus_minus && (console_flag || xterm_flag)) {
        fprintf (stderr, "\033="); fflush (stderr);
    }
        
#endif /* have_x */
#ifdef HAVE_TK
    if (iconify_on_exec)
	tk_evalf ("wm deiconify .");
#endif
}

/* Save current stat of directories to avoid reloading the panels */
/* when no modifications have taken place */
void save_cwds_stat (void)
{
    if (fast_reload){
	mc_stat (cpanel->cwd, &(cpanel->dir_stat));
	if (get_other_type () == view_listing)
	    mc_stat (opanel->cwd, &(opanel->dir_stat));
    }
}

#ifdef HAVE_SUBSHELL_SUPPORT
void do_possible_cd (char *new_dir)
{
    if (!do_cd (new_dir, cd_exact))
	message (1, " Warning ",
		 " The Commander can't change to the directory that \n"
		 " the subshell claims you are in.  Perhaps you have \n"
		 " deleted your working directory, or given yourself \n"
		 " extra access permissions with the \"su\" command? ");
}

void do_update_prompt ()
{
    if (update_prompt){
	printf ("%s", subshell_prompt);
	fflush (stdout);
	update_prompt = 0;
    }
}
#endif

void restore_console (void)
{
    handle_console (CONSOLE_RESTORE);
}

void exec_shell ()
{
    int old_use_subshell = use_subshell;

    do_execute (shell, 0, 1);
    use_subshell = old_use_subshell;
}

void do_execute (const char *shell, const char *command, int internal_command)
{
#ifdef HAVE_SUBSHELL_SUPPORT
    char *new_dir = NULL;
#ifdef USE_VFS
    char *old_vfs_dir = 0;

    if (!vfs_current_is_local ())
	old_vfs_dir = strdup (vfs_get_current_dir ());
#endif
#endif

    save_cwds_stat ();
    pre_exec ();
    if (console_flag)
	restore_console ();

    unlink (control_file);
    if (!use_subshell && !internal_command){
	printf ("%s%s%s\n", last_paused ? "\r\n":"", prompt, command);
	last_paused = 0;
    }

#ifdef HAVE_SUBSHELL_SUPPORT
    if (use_subshell && !internal_command){
	do_update_prompt ();

	/* We don't care if it died, higher level takes care of this */
	invoke_subshell (command, VISIBLY, &new_dir);
    } else
#endif
	my_system (!internal_command, shell, command);

    if (!internal_command){
	if (console_flag){
	    if (output_lines && keybar_visible)
		putchar('\n');
	    handle_console (CONSOLE_SAVE);
	}

	if ((pause_after_run == pause_always ||
	    (pause_after_run == pause_on_dumb_terminals &&
	     !xterm_flag && !console_flag)) && !quit){
	    printf ("Press any key to continue...");
	    last_paused = 1;
	    fflush (stdout);
	    mc_raw_mode ();
	    xgetch ();
	    /* printf ("\r\n");
	    fflush (stdout); */
	}
    }

    post_exec ();

#ifdef HAVE_SUBSHELL_SUPPORT
	if (new_dir)
	    do_possible_cd (new_dir);
#endif
	
#ifdef USE_VFS
	if (old_vfs_dir){
	    mc_chdir (old_vfs_dir);
	    free (old_vfs_dir);
	}
#endif

    update_panels (UP_OPTIMIZE, UP_KEEPSEL, UP_KEEPSEL);
    
    parse_control_file ();
    unlink (control_file);
    do_refresh ();
    use_dash (TRUE);
}

/* Executes a command */
void shell_execute (char *command, int internal)
{
#ifdef HAVE_SUBSHELL_SUPPORT
    if (use_subshell)
	if (subshell_state == INACTIVE)
	    do_execute (shell, command, internal);
	else
	    message (1, " Error ", " The shell is already running a command ");
    else
#endif

	do_execute (shell, command, internal);
}

void execute (char *command)
{
    shell_execute (command, 0);
}

void change_panel (void)
{
    free_completions (input_w (cmdline));
/*  #ifdef HAVE_X
    send_message (midnight_dlg, (Widget *) cpanel, WIDGET_UNFOCUS, 0);
    send_message (midnight_dlg, (Widget *) opanel, WIDGET_FOCUS, 0);
    #else
    #endif
*/
    dlg_one_down (midnight_dlg);
}

static int quit_cmd_internal (int quiet)
{
    int q = quit;

    if (quiet || !confirm_exit){
	q = 1;
    } else  {
	if (query_dialog (" The Midnight Commander ",
		     " Do you really want to quit the Midnight Commander? ",
			  0, 2, " Yes ", " No ") == 0)
	    q = 1;
    }
    if (q){
#ifdef HAVE_SUBSHELL_SUPPORT
	if (!use_subshell)
	    midnight_dlg->running = 0;
	else
	    if ((q = exit_subshell ()))
#endif
		midnight_dlg->running = 0;
    }
    if (q)
        quit |= 1;
    return quit;
}

int quit_cmd (void)
{
    quit_cmd_internal (0);
    return quit;
}

int quiet_quit_cmd (void)
{
    print_last_revert = 1;
    quit_cmd_internal (1);
    return quit;
}

/*
 * Touch window and refresh window functions
 */

/* This routine untouches the first line on both panels in order */
/* to avoid the refreshing the menu bar */

#ifdef HAVE_X
void untouch_bar (void)
{
}

void repaint_screen (void)
{
    do_refresh ();
}

#else /* HAVE_X */
void untouch_bar (void)
{
    do_refresh ();
}

void repaint_screen (void)
{
    do_refresh ();
    mc_refresh ();
}
#endif /* HAVE_X */

/* Wrapper for do_subshell_chdir, check for availability of subshell */
void subshell_chdir (char *directory)
{
#ifdef HAVE_SUBSHELL_SUPPORT
    if (use_subshell){
	if (vfs_current_is_local ())
	    do_subshell_chdir (directory, 0, 1);
    }
#endif
}

/* Changes the current panel directory */
int do_panel_cd (WPanel *panel, char *new_dir, enum cd_enum cd_type)
{
    char *directory, *olddir;
    char temp [MC_MAXPATHLEN];

    olddir = strdup (panel->cwd);

    /* Convert *new_path to a suitable pathname, handle ~user */
    
    if (cd_type == cd_parse_command){
	while (*new_dir == ' ')
	    new_dir++;
    
	if (!strcmp (new_dir, "-")){
	    strcpy (temp, panel->lwd);
	    new_dir = temp;
	}
    }
    directory = *new_dir ? new_dir : home_dir;

    if (mc_chdir (directory) == -1){
	strcpy (panel->cwd, olddir);
	free (olddir);
	return 0;
    }

    /* Success: save previous directory, shutdown status of previous dir */
    directory_history_add (olddir);
    strcpy (panel->lwd, olddir);
    free (olddir);
    free_completions (input_w (cmdline));
    
    mc_get_current_wd (panel->cwd, sizeof (panel->cwd) - 2);

    subshell_chdir (panel->cwd);

    /* Reload current panel */
    clean_dir (&panel->dir, panel->count);
    panel->count = do_load_dir (&panel->dir, panel->sort_type,
				 panel->reverse, panel->case_sensitive, panel->filter);
    panel->top_file = 0;
    panel->selected = 0;
    panel->marked = 0;
    panel->dirs_marked = 0;
    panel->total = 0;
    cd_try_to_select (panel);
    load_hint ();
    panel_update_contents (panel);
    return 1;
}

int do_cd (char *new_dir, enum cd_enum exact)
{
    return do_panel_cd (cpanel, new_dir, exact);
}

#ifdef HAVE_SUBSHELL_SUPPORT
int load_prompt (int fd, void *unused)
{
    if (!read_subshell_prompt (QUIETLY))
	return 0;
    
    if (command_prompt){
	int  prompt_len;

	prompt = strip_ctrl_codes (subshell_prompt);
	prompt_len = strlen (prompt);

	/* Check for prompts too big */
	if (prompt_len > COLS - 8) {
	    prompt [COLS - 8 ] = 0;
	    prompt_len = COLS - 8;
	}
	winput_set_origin ((WInput *)cmdline, prompt_len, COLS-prompt_len);
	label_set_text (the_prompt, prompt);

	/* since the prompt has changed, and we are called from one of the 
	 * get_event channels, the prompt updating does not take place
	 * automatically: force a cursor update and a screen refresh
	 */
	if (current_dlg == midnight_dlg){
	    update_cursor (midnight_dlg);
	    mc_refresh ();
	}
    }
    update_prompt = 1;
    return 0;
}
#endif

/* The user pressed the enter key */
int menu_bar_event (Gpm_Event *event, void *x)
{
    if (event->type != GPM_DOWN)
	return MOU_NORMAL;

    return MOU_ENDLOOP;
}

/* Used to emulate Lynx's entering leaving a directory with the arrow keys */
int maybe_cd (int char_code, int move_up_dir)
{
    if (navigate_with_arrows){
	if (!input_w (cmdline)->buffer [0]){
	    if (!move_up_dir){
		do_cd ("..", cd_exact);
		return 1;
	    }
	    if (S_ISDIR (selection (cpanel)->buf.st_mode)
		|| link_isdir (selection (cpanel))){
		do_cd (selection (cpanel)->fname, cd_exact);
		return 1;
	    }
	}
    }
    return 0;
}

#if 0
static void tree_leave (WPanel *p)
{
    char *dir;
    int change = 0;
    
    dir = p->cwd;
    if (cpanel != p){
	change_panel ();
	change = 1;
    }
    if (!do_cd (dir, cd_exact))
	message (1, " Error ", " Can't chdir to \"%s\" \n %s ",
		 dir, unix_error_string (errno));
    if (change)
	change_panel ();
}
#endif

static void info_cmd_no_menu (void)
{
    set_display_type (cpanel == left_panel ? 1 : 0, view_info);
}

static void quick_cmd_no_menu (void)
{
    set_display_type (cpanel == left_panel ? 1 : 0, view_quick);
}

static void switch_to_listing (int panel_index)
{
    if (get_display_type (panel_index) != view_listing)
	set_display_type (panel_index, view_listing);
}

static void listing_cmd (void)
{
    int   view_type, use_msformat;
    char  *user, *status;
    WPanel *p;
    int   display_type;

    display_type = get_display_type (MENU_PANEL_IDX);
    if (display_type == view_listing)
	p = MENU_PANEL_IDX == 0 ? left_panel : right_panel;
    else
	p = 0;

    view_type = display_box (p, &user, &status, &use_msformat, MENU_PANEL_IDX);

    if (view_type == -1)
	return;

    switch_to_listing (MENU_PANEL_IDX);

    p = MENU_PANEL_IDX == 0 ? left_panel : right_panel;    
    free (p->user_format);
    p->user_format = user;
    
    free (p->mini_status_format);
    p->mini_status_format = status;
     
    p->user_mini_status = use_msformat; 
    p->list_type = view_type;
    
    if (set_panel_format (p, panel_format (p))){
	free (p->user_format);
	p->user_format  = strdup (DEFAULT_USER_FORMAT);
	set_panel_format (p, DEFAULT_USER_FORMAT);
    }
    do_refresh ();
}

void tree_cmd (void)
{
    set_display_type (MENU_PANEL_IDX, view_tree);
}

static void info_cmd (void)
{
    set_display_type (MENU_PANEL_IDX, view_info);
}

void quick_view_cmd (void)
{
    set_display_type (MENU_PANEL_IDX, view_quick);
}

/* Handle the tree internal listing modes switching */
int set_basic_panel_listing_to (int panel_index, int listing_mode)
{
    WPanel *p = (WPanel *) get_panel_widget (panel_index);
    
    switch_to_listing (panel_index);
    p->list_type = listing_mode;
    if (set_panel_format (p, panel_format (p)))
	return 0;
    do_refresh ();
    return 1;
}

void toggle_listing_cmd ()
{
    int current = get_current_index ();
    WPanel *p = (WPanel *) get_panel_widget (current);
    int list_mode = p->list_type;
    int m;
    
    switch (list_mode){
    case list_full:
    case list_brief:
	m = list_long;
	break;
    case list_long:
	m = list_user;
	break;
    default:
	m = list_full;
    }
    if (set_basic_panel_listing_to (current, m))
	return;
    set_basic_panel_listing_to (current, list_full);
}

void set_sort_to (WPanel *p, sortfn *sort_order)
{
    p->sort_type = sort_order;
    
    /* The directory is already sorted, we have to load the unsorted stuff */
    if (sort_order == (sortfn *) unsorted){
	char *current_file;
	
	current_file = strdup (cpanel->dir.list [cpanel->selected].fname);
	panel_reload (cpanel);
	try_to_select (cpanel, current_file);
	free (current_file);
    }
    do_re_sort (p);
}

static void sort_cmd (void)
{
    WPanel  *p;
    sortfn *sort_order;

    if (!SELECTED_IS_PANEL)
	return;

    p = MENU_PANEL;
    sort_order = sort_box (p->sort_type, &p->reverse, &p->case_sensitive);

    if (sort_order == 0)
	return;

    p->sort_type = sort_order;

    /* The directory is already sorted, we have to load the unsorted stuff */
    if (sort_order == (sortfn *) unsorted){
	char *current_file;
	
	current_file = strdup (cpanel->dir.list [cpanel->selected].fname);
	panel_reload (cpanel);
	try_to_select (cpanel, current_file);
	free (current_file);
    }
    do_re_sort (p);
}

static void tree_box (void)
{
    char *sel_dir;

    sel_dir = tree (selection (cpanel)->fname);
    if (sel_dir){
	try_to_select (cpanel, sel_dir);
	free (sel_dir);
    }
}

#if VERSION_4
static void listmode_cmd (void)
{
    char *newmode;
    newmode = listmode_edit ("half <type,>name,|,size:8,|,perm:4+");
    message (0, " Listing format edit ", " New mode is \"%s\" ", newmode);
    free (newmode);
}
#endif

static menu_entry PanelMenu [] = {
    { ' ', "Listing mode...",	'L',        listing_cmd },
    { ' ', "Quick view",        'Q',        quick_view_cmd }, 
    { ' ', "Info",              'I',        info_cmd },
    { ' ', "Tree",              'T',        tree_cmd },
    { ' ', "", ' ', 0 },
    { ' ', "Sort order...",     'S',        sort_cmd },
    { ' ', "", ' ', 0 },
    { ' ', "Filter...",	        'F',        filter_cmd },
#ifdef USE_NETCODE
    { ' ', "", ' ', 0 },
    { ' ', "Network link...",   'N',        netlink_cmd },
    { ' ', "FTP link...",       'P',        ftplink_cmd },
#endif
    { ' ', "", ' ', 0 },
#ifdef _OS_NT
    { ' ', "Drive...  M-d",         ALT('d'),drive_cmd_a },
#endif
    { ' ', "Rescan    C-r",         XCTRL('R'), reread_cmd }
};

static menu_entry RightMenu [] = {
    { ' ', "Listing mode...",	'L',        listing_cmd },
    { ' ', "Quick view",        'Q',        quick_view_cmd }, 
    { ' ', "Info",              'I',        info_cmd },
    { ' ', "Tree",              'T',        tree_cmd },
    { ' ', "", ' ', 0 },
    { ' ', "Sort order...",     'S',        sort_cmd },
    { ' ', "", ' ', 0 },
    { ' ', "Filter...",	        'F',        filter_cmd },
#ifdef USE_NETCODE
    { ' ', "", ' ', 0 },
    { ' ', "Network link...",   'N',        netlink_cmd },
    { ' ', "FTP link...",       'P',        ftplink_cmd },
#endif
    { ' ', "", ' ', 0 },
#ifdef _OS_NT
    { ' ', "Drive...  M-d",         ALT('d'),drive_cmd_b },
#endif
    { ' ', "Rescan    C-r",         XCTRL('R'), reread_cmd }
};


static menu_entry FileMenu [] = {
    { ' ', "User menu          F2",     KEY_F(2), user_menu_cmd },
    { ' ', "View               F3",     KEY_F(3), view_cmd },
    { ' ', "Filtered view     M-!",     ALT('!'), filtered_view_cmd },
    { ' ', "Edit               F4",     KEY_F(4), edit_cmd },
    { ' ', "Copy               F5",     KEY_F(5), copy_cmd },
    { ' ', "Chmod           C-x c",  'C',      chmod_cmd },
#ifndef _OS_NT
    { ' ', "Link            C-x l",  'L',      link_cmd },
    { ' ', "SymLink         C-x s",  'S',      symlink_cmd },
    { ' ', "edit sYmlink  C-x C-s",  'Y',      edit_symlink_cmd },
    { ' ', "chOwn           C-x o",  'O',      chown_cmd },
    { ' ', "Advanced chown       ",  'A',      chown_advanced_cmd },
#endif
    { ' ', "Rename/Move        F6",     KEY_F(6), ren_cmd },
    { ' ', "Mkdir              F7",     KEY_F(7), mkdir_cmd },
    { ' ', "Delete             F8",     KEY_F(8), delete_cmd },
    { ' ', "Quick cd          M-c",    ALT('c'), quick_cd_cmd },
    { ' ', "", ' ', 0 },
    { ' ', "Select group      M-+",    ALT('+'),	  select_cmd },
    { ' ', "Unselect group    M-\\",   ALT('\\'),   unselect_cmd },
    { ' ', "Reverse selection M-*",  ALT('*'),	  reverse_selection_cmd },
    { ' ', "", ' ', 0 },
    { ' ', "Quit            F10",    KEY_F(10), (callfn) quit_cmd }
};

void external_panelize (void);
static menu_entry CmdMenu [] = {
    /* I know, I'm lazy, but the tree widget when it's not running
     * as a panel still has some problems, I have not yet finished
     * the WTree widget port, sorry.
     */
    { ' ', "Directory tree",	        'D',	      tree_box },
    { ' ', "Find file            M-?",  ALT('?'),     find_cmd },
#ifndef HAVE_XVIEW    
    { ' ', "Swap panels          C-u",  XCTRL('u'),   swap_cmd },
    { ' ', "Switch panels on/off C-o",  XCTRL('o'),   view_other_cmd },
#endif    
    { ' ', "Compare directories",	'C',	      compare_dirs_cmd },
    { ' ', "eXternal panelize    C-x !",'X',	      external_panelize },
#ifdef HAVE_DUSUM
    { ' ', "show directory sIzes",      'I',	      dirsizes_cmd },
#endif
    { ' ', "", ' ', 0 },
    { ' ', "command History",           'H',	      history_cmd },
    { ' ', "Directory hotlist    C-\\",  XCTRL('\\'), quick_chdir_cmd },
#ifdef USE_VFS
    { ' ', "Active VFS list      C-x a", 'A',         reselect_vfs },
#endif
    { ' ', "", ' ', 0 },
#ifdef USE_EXT2FSLIB
    { ' ', "Undelete files (ext2fs only)",'U',        undelete_cmd },
#endif
#ifdef VERSION_4
    { ' ', "Listing format edit",	'L',	      listmode_cmd},
#endif
    { ' ', "Extension file edit",	'E',	      ext_cmd },
    { ' ', "Menu file edit",		'M',	      menu_edit_cmd }
};

/* Must keep in sync with the constants in menu_cmd */
static menu_entry OptMenu [] = {
    { ' ', "Configuration...",    'C', configure_box },
    { ' ', "Layout...",           'L', layout_cmd },
    { ' ', "cOnfirmation...",     'O', confirm_box },
    { ' ', "Display bits...",     'D', display_bits_box },
#ifndef HAVE_X
    { ' ', "learn Keys...",       'K', learn_keys },
#endif
#ifdef USE_VFS    
    { ' ', "Virtual FS...",	  'V', configure_vfs },
#endif
    { ' ', "", ' ', 0 }, 
    { ' ', "Save setup",          'S', save_setup_cmd }
};

#define menu_entries(x) sizeof(x)/sizeof(menu_entry)

Menu MenuBar [5];
#ifndef HAVE_X
static Menu MenuBarEmpty [5];
#endif

static void init_menu (void)
{
    int i;
    
    MenuBar [0] = create_menu (" Left ", PanelMenu, menu_entries (PanelMenu));
    MenuBar [1] = create_menu (" File ", FileMenu, menu_entries (FileMenu));
    MenuBar [2] = create_menu (" Command ", CmdMenu, menu_entries (CmdMenu));
    MenuBar [3] = create_menu (" Options ", OptMenu, menu_entries (OptMenu));
#ifndef HAVE_XVIEW
    MenuBar [4] = create_menu (" Right ", RightMenu, menu_entries (PanelMenu));

#ifndef HAVE_X
    for (i = 0; i < 5; i++)
	MenuBarEmpty [i] = create_menu (MenuBar [i]->name, 0, 0);
#endif /* ! HAVE_X */
#endif /* ! HAVE_XVIEW */
}

static void done_menu (void)
{
    int i;

#ifndef HAVE_XVIEW
    for (i = 0; i < 5; i++){
	destroy_menu (MenuBar [i]);
#ifndef HAVE_X
	destroy_menu (MenuBarEmpty [i]);
#endif
#else
    for (i = 0; i < 4; i++){
	destroy_menu (MenuBar [i]);
#endif
    }
}

/* All the drop_menu mess is so that the user could select from the
   capital letters on the MenuBar.  Suggested by Torben */

enum {
    menu_select_current,
    menu_select_last,
    menu_select_pos
};

static void menu_last_selected_cmd (void)
{
    the_menubar->active = 1;
    the_menubar->dropped = drop_menus;
    the_menubar->previous_selection = dlg_item_number (midnight_dlg);
    dlg_select_widget (midnight_dlg, the_menubar);
}

static void menu_cmd (void)
{
    if (the_menubar->active)
	return;
    
    if (get_current_index () == 0)
	the_menubar->selected = 0;
    else
	the_menubar->selected = 4;
    menu_last_selected_cmd ();
}

/* Flag toggling functions */
void toggle_confirm_delete (void)
{
    confirm_delete = !confirm_delete;
}

void toggle_fast_reload (void)
{
    fast_reload = !fast_reload;
    if (fast_reload_w == 0 && fast_reload){
	message (0, " Information ",
		 " Using the fast reload option may not reflect the exact \n"
		 " directory contents. In this cases you'll need to do a  \n"
		 " manual reload of the directory. See the man page for   \n"
		 " the details.                                           ");
	fast_reload_w = 1;
    }
}

void toggle_mix_all_files (void)
{
    mix_all_files = !mix_all_files;
    update_panels (UP_RELOAD, UP_KEEPSEL, UP_KEEPSEL);
}

void toggle_show_backup (void)
{
    show_backups = !show_backups;
    update_panels (UP_RELOAD, UP_KEEPSEL, UP_KEEPSEL);
}

void toggle_show_hidden (void)
{
    show_dot_files = !show_dot_files;
    update_panels (UP_RELOAD, UP_KEEPSEL, UP_KEEPSEL);
}

void toggle_show_mini_status (void)
{
    show_mini_info = !show_mini_info;
    paint_panel (cpanel);
    if (get_other_type () == view_listing)
	paint_panel (opanel);
}

void toggle_align_extensions (void)
{
    align_extensions = !align_extensions;
}

static void create_panels (void)
{
    int current_index;
    int other_index;
    int current_mode;
    int other_mode;
    char original_dir [1024];

    original_dir [0] = 0;
    
    if (boot_current_is_left){
	current_index = 0;
	other_index = 1;
	current_mode = startup_left_mode;
	other_mode = startup_right_mode;
    } else {
	current_index = 1;
	other_index = 0;
	current_mode = startup_right_mode;
	other_mode = startup_left_mode;
    }
    /* Creates the left panel */
    if (this_dir){
	if (other_dir){
	    /* Ok, user has specified two dirs, save the original one,
	     * since we may not be able to chdir to the proper
	     * second directory later
	     */
	    mc_get_current_wd (original_dir, sizeof (original_dir)-2);
	}
	mc_chdir (this_dir);
    }
    set_display_type (current_index, current_mode);

    /* The other panel */
    if (other_dir){
	if (original_dir [0])
	    mc_chdir (original_dir);
	mc_chdir (other_dir);
    }
    set_display_type (other_index, other_mode);

    if (startup_left_mode == view_listing){
	cpanel = left_panel;
    } else {
	if (right_panel)
	    cpanel = right_panel;
	else
	    cpanel = left_panel;
    }

    /* Create the nice widgets */
    cmdline     = command_new (0, 0, 0);
    the_prompt  = label_new (0, 0, prompt, NULL);
    the_prompt->transparent = 1;
    the_bar     = buttonbar_new (keybar_visible);
    the_hint    = label_new (0, 0, 0, NULL);
    the_hint->transparent = 1;
    the_hint->auto_adjust_cols = 0;
    the_hint->widget.cols = COLS;

#ifdef BACKGROUND
    process_status = label_new (0, COLS-12, 0, NULL);
    process_status->transparent = 0;
    process_status->auto_adjust_cols = 0;
    process_status->widget.cols = 0;
#endif
    
#ifndef HAVE_XVIEW
    the_menubar = menubar_new (0, 0, COLS, MenuBar, 5);
#else
    the_menubar = menubar_new (0, 0, COLS, MenuBar + 1, 3);
    the_bar2    = buttonbar_new (keybar_visible);
#endif
}

static void copy_current_pathname (void)
{
    if (!command_prompt)
	return;

    stuff (input_w (cmdline), cpanel->cwd, 0);
    if (cpanel->cwd [strlen (cpanel->cwd) - 1] != PATH_SEP)
        stuff (input_w (cmdline), PATH_SEP_STR, 0);
}

static void copy_other_pathname (void)
{
    if (get_other_type () != view_listing)
	return;

    if (!command_prompt)
	return;

    stuff (input_w (cmdline), opanel->cwd, 0);
    if (cpanel->cwd [strlen (opanel->cwd) - 1] != PATH_SEP)
        stuff (input_w (cmdline), PATH_SEP_STR, 0);
}

static void copy_readlink (WPanel *panel)
{
    if (!command_prompt)
	return;
    if (S_ISLNK (selection (panel)->buf.st_mode)) {
	char buffer [MC_MAXPATHLEN], *p = (panel->cwd [strlen (panel->cwd) - 1] != PATH_SEP) ? 
	  copy_strings (panel->cwd, PATH_SEP_STR, selection (panel)->fname, NULL) :
	  copy_strings (panel->cwd, selection (panel)->fname, NULL);
	int i;
	
	i = readlink (p, buffer, MC_MAXPATHLEN);
	free (p);
	if (i > 0) {
	    buffer [i] = 0;
	    stuff (input_w (cmdline), buffer, 0);
	}
    }
}

static void copy_current_readlink (void)
{
    copy_readlink (cpanel);
}

static void copy_other_readlink (void)
{
    if (get_other_type () != view_listing)
	return;
    copy_readlink (opanel);
}

/* Inserts the selected file name into the input line */
/* Exported so that the command modules uses it */
void copy_prog_name (void)
{
    if (!command_prompt)
	return;

    if (get_current_type () == view_tree){
	WTree *tree = (WTree *) get_panel_widget (get_current_index ());

	stuff (input_w (cmdline), tree->selected_ptr->name, 1);
    } else
	stuff (input_w (cmdline), selection (cpanel)->fname, 1);
}

static void copy_tagged (WPanel *panel)
{
    int i;

    if (!command_prompt)
	return;
    input_disable_update (input_w (cmdline));
    if (panel->marked){
	for (i = 0; i < panel->count; i++)
	    if (panel->dir.list [i].f.marked)
		stuff (input_w (cmdline), panel->dir.list [i].fname, 1);
    } else 
	stuff (input_w (cmdline), panel->dir.list [panel->selected].fname, 1);
    input_enable_update (input_w (cmdline));
}
    
static void copy_current_tagged (void)
{
    copy_tagged (cpanel);
}

static void copy_other_tagged (void)
{
    if (get_other_type () != view_listing)
	return;
    copy_tagged (opanel);
}

static void do_suspend_cmd (void)
{
    pre_exec ();

    if (console_flag && !use_subshell)
	restore_console ();

#ifndef _OS_NT
    {
	struct sigaction sigtstp_action;
	
	/* Make sure that the SIGTSTP below will suspend us directly,
	   without calling ncurses' SIGTSTP handler; we *don't* want
	   ncurses to redraw the screen immediately after the SIGCONT */
	sigaction (SIGTSTP, &startup_handler, &sigtstp_action);
    
	kill (getpid (), SIGTSTP);

	/* Restore previous SIGTSTP action */
	sigaction (SIGTSTP, &sigtstp_action, NULL);
    }
#endif
    
    if (console_flag && !use_subshell)
	handle_console (CONSOLE_SAVE);
    
    post_exec ();
}

void suspend_cmd (void)
{
    save_cwds_stat ();
    do_suspend_cmd ();
    update_panels (UP_OPTIMIZE, UP_KEEPSEL, UP_KEEPSEL);
    do_refresh ();
}

void init_labels (Widget *paneletc)
{
    define_label (midnight_dlg, paneletc, 1, "Help", help_cmd);
    define_label (midnight_dlg, paneletc, 2, "Menu", user_menu_cmd);
    define_label (midnight_dlg, paneletc, 9, "PullDn", menu_cmd);
    define_label (midnight_dlg, paneletc, 10, "Quit", (voidfn) quit_cmd);
}

static void save_setup_cmd (void)
{
    save_setup ();
    sync_profiles ();
    message (0, " Setup ", " Setup saved to ~/.mc.ini ");
}

#ifndef HAVE_XVIEW
static key_map ctl_x_map [] = {
    { XCTRL('c'),   (callfn) quit_cmd },
#ifdef USE_VFS
    { 'a',          reselect_vfs },
#endif
    { 'd',          compare_dirs_cmd },
    { 'p',          copy_current_pathname },
    { XCTRL('p'),   copy_other_pathname },
    { 't',          copy_current_tagged },
    { XCTRL('t'),   copy_other_tagged },
    { 'c',          chmod_cmd },
#ifndef _OS_NT
    { 'o',          chown_cmd },
    { 'l',          link_cmd },
    { XCTRL('l'),   other_symlink_cmd },
    { 's',          symlink_cmd },
    { XCTRL('s'),   edit_symlink_cmd },
    { 'r',          copy_current_readlink },
    { XCTRL('r'),   copy_other_readlink },
#endif
    { 'i',          info_cmd_no_menu },
    { 'q',          quick_cmd_no_menu },
    { 'h',          add2hotlist_cmd },
    { '!',          external_panelize },
#ifdef HAVE_SETSOCKOPT
    { '%',          source_routing },
#endif
    { 0,  0 }
};

static void ctl_x_cmd (int ignore)
{
    int i;
    int key = mi_getch ();

    for (i = 0; i < ctl_x_map [i].key_code; i++){
	if (key == ctl_x_map [i].key_code){
	    (*ctl_x_map [i].fn)(key);
	    break;
	}
    }
}

static void nothing ()
{
}

static key_map default_map [] = {
    { KEY_F(19),  menu_last_selected_cmd },
    
    { KEY_F(13),  (key_callback) view_simple_cmd },
    { KEY_F(20),  (key_callback) quiet_quit_cmd },

    /* Copy useful information to the command line */
    { ALT('\n'),  copy_prog_name },
    { ALT('\r'),  copy_prog_name },
    { ALT('a'),   copy_current_pathname },
    { ALT('A'),   copy_other_pathname },
    
    { ALT('+'),	  select_cmd },
    { ALT('\\'),  unselect_cmd },
    { ALT('-'),	  unselect_cmd },
    { ALT('*'),	  reverse_selection_cmd },
    
    { ALT('c'),	  quick_cd_cmd },

    /* To access the directory hotlist */
    { XCTRL('\\'), quick_chdir_cmd },

    /* The filtered view command */
    { ALT('!'),   filtered_view_cmd_cpanel },
    
    /* Find file */
    { ALT('?'),	  find_cmd },
	
    /* Suspend */
    { XCTRL('z'), suspend_cmd },

    /* Panel refresh */
    { XCTRL('r'), reread_cmd },

    { ALT('t'),   toggle_listing_cmd },
    
#ifndef HAVE_XVIEW
    /* Swap panels */
    { XCTRL('u'), swap_cmd },

    /* View output */
    { XCTRL('o'), view_other_cmd },
#endif
    
    /* Control-X keybindings */
    { XCTRL('x'), ctl_x_cmd },

    /* Trap dlg's exit commands */
    { ESC_CHAR,   nothing },
    { XCTRL('c'), nothing },
    { XCTRL('g'), nothing },
    { 0, 0 },
};
#endif

static void setup_sigwinch ()
{
#ifndef _OS_NT
    struct sigaction act, oact;
    
#   ifdef HAVE_SLANG
#       ifdef SIGWINCH
            act.sa_handler = flag_winch;
            sigemptyset (&act.sa_mask);
	    act.sa_flags = 0;
#           ifdef SA_RESTART
                act.sa_flags |= SA_RESTART;
#           endif
            sigaction (SIGWINCH, &act, &oact);
#       endif
#   endif
#endif
}

/* Midnight Commander and file viewer setup */
static void setup_pre ()
{
    /* Call all the inits */
#   ifndef HAVE_SLANG
    meta (stdscr, eight_bit_clean);
#else
    SLsmg_Display_Eight_Bit = full_eight_bits ? 128 : 160;
#   endif
}

static void setup_post ()
{
    setup_sigwinch ();
    
    init_uid_gid_cache ();

#ifndef HAVE_X
    if (baudrate () < 9600 || slow_terminal){
	verbose = 0;
    }
    if (use_mouse_p)
	init_mouse ();
#endif

    midnight_colors [0] = 0;
    midnight_colors [1] = REVERSE_COLOR;     /* FOCUSC */
    midnight_colors [2] = INPUT_COLOR;       /* HOT_NORMALC */
    midnight_colors [3] = NORMAL_COLOR;	     /* HOT_FOCUSC */
}

static void setup_mc (void)
{
    setup_pre ();
    init_menu ();
    create_panels ();
    setup_panels ();
    
#ifdef HAVE_SUBSHELL_SUPPORT
    if (use_subshell)
	add_select_channel (subshell_pty, load_prompt, 0);
#endif
    
    setup_post ();
}

static void setup_dummy_mc (const char *file)
{
    setup_pre ();
    setup_post ();

    /* Create a fake current_panel, this is needed because the
     * expand_format routine will use current panel.
     */
    cpanel = xmalloc (sizeof (WPanel), "fake_panel");
    strcpy (cpanel->cwd, ".");
    cpanel->dir.list = (file_entry *) xmalloc (sizeof (file_entry), "fake_p");
    cpanel->selected = 0;
    cpanel->count = 1;
    cpanel->dir.list[0].fname = (char *) file;
}

static void done_screen ()
{
/* #ifndef HAVE_X */
    if (!(quit & SUBSHELL_EXIT))
	clr_scr ();
    reset_shell_mode ();
#ifndef HAVE_X
    mc_noraw_mode ();
    if (use_mouse_p)
	shut_mouse ();
#endif
    keypad (stdscr, FALSE);
}

static void done_mc ()
{
    done_menu ();
    
    /* Setup shutdown
     *
     * We sync the profiles since the hotlist may have changed, while
     * we only change the setup data if we have the auto save feature set
     */
    if (auto_save_setup)
	save_setup ();   /* does also call save_hotlist */
    else
	save_hotlist();
    done_screen ();
    vfs_add_current_stamps ();
    if (xterm_flag && xterm_hintbar)
        set_hintbar("Thank you for using GNU Midnight Commander");
}

/* This should be called after destroy_dlg since panel widgets
 *  save their state on the profiles
 */
static void done_mc_profile ()
{
    if (!auto_save_setup)
	profile_forget_profile (profile_name);
    sync_profiles ();
    done_setup ();
    free_profiles ();
}

/* This routine only handles cpanel, and opanel, it is easy to
 * change to use npanels, just loop over the number of panels
 * and use get_panel_widget (i) and make the test done below
 */
void make_panels_dirty ()
{
    if (cpanel->dirty)
	panel_update_contents (cpanel);
    
    if ((get_other_type () == view_listing) && opanel->dirty)
	panel_update_contents (opanel);
}

int midnight_callback (struct Dlg_head *h, int id, int msg)
{
    int i;
    
    switch (msg){
#ifndef HAVE_XVIEW

	/* Speed up routine: now, we just set the  */
    case DLG_PRE_EVENT:
	make_panels_dirty ();
	return MSG_HANDLED;
	
    case DLG_KEY:
	if (id == KEY_F(10) && !the_menubar->active){
	    quit_cmd ();
	    return MSG_HANDLED;
	}

	if (id == '\t')
	    free_completions (input_w (cmdline));

	/* On Linux, we can tell the difference */
	if (id == '\n' && ctrl_pressed ()){
	    copy_prog_name ();
	    return MSG_HANDLED;
	}

	if (id == '\n' && input_w (cmdline)->buffer [0]){
	    send_message_to (h, (Widget *) cmdline, WIDGET_KEY, id);
	    return MSG_HANDLED;
	}

	if (!alternate_plus_minus || !(console_flag || xterm_flag)) {
	    if(!only_leading_plus_minus) {
		/* Special treatement, since the input line will eat them */
		if (id == '+' && !quote && !cpanel->searching){
		    select_cmd ();
		    return MSG_HANDLED;
		}
		
		if (id == '\\' && !quote && !cpanel->searching){
		    unselect_cmd ();
		    return MSG_HANDLED;
		}
	    } else if (command_prompt && !strlen (input_w (cmdline)->buffer)
		       && !cpanel->searching) {
		/* Special treatement '+', '-', '\', '*' only when this is 
		 * first char on input line
		 */
		
		if (id == '+' && !quote && !cpanel->searching){
		    select_cmd ();
		    return MSG_HANDLED;
		}
		
		if ((id == '\\' || id == '-') && !quote && !cpanel->searching){
		    unselect_cmd ();
		    return MSG_HANDLED;
		}
		
		if (id == '*' && !quote && !cpanel->searching){
		    reverse_selection_cmd ();
		    return MSG_HANDLED;
		}
	    }   
	} 
	break;

    case DLG_HOTKEY_HANDLED:
	if (get_current_type () == view_listing)
	    cpanel->searching = 0;
	break;
	
    case DLG_UNHANDLED_KEY:
	if (command_prompt){
	    int v;
	    
	    v = send_message_to (h, (Widget *) cmdline, WIDGET_KEY, id);
	    if (v)
		return v;
	}
	for (i = 0; default_map [i].key_code; i++){
	    if (id == default_map [i].key_code){
		(*default_map [i].fn)(id);
		break;
	    }
	}
	if (default_map [i].key_code)
	    return MSG_HANDLED;
	else
	    return MSG_NOT_HANDLED;
#endif
#ifndef HAVE_X
	/* We handle the special case of the output lines */
    case DLG_DRAW:
	attrset (SELECTED_COLOR);
	if (console_flag && output_lines)
	    show_console_contents (output_start_y,
				   LINES-output_lines-keybar_visible-1,
				   LINES-keybar_visible-1);
	break;
#endif
	
    }
    return default_dlg_callback (h, id, msg);
}

#ifdef HAVE_X
/* This should be rewritten in order to support as many panel containers as
   the user wants */

widget_data containers [2];
int containers_no = 2;

void xtoolkit_panel_setup ()
{
    containers [0] = x_create_panel_container (0);
    containers [1] = x_create_panel_container (1);
    input_w (cmdline)->widget.wcontainer = containers [0];
    input_w (cmdline)->widget.area = AREA_BOTTOM;
    the_prompt->widget.wcontainer = containers [0];
    the_prompt->widget.area = AREA_BOTTOM;
    the_bar->widget.wcontainer = containers [0];
    the_bar->widget.area = AREA_TOP;
#ifdef HAVE_XVIEW
    the_bar2->widget.wcontainer = containers [1];
    the_bar2->widget.area = AREA_TOP;
#endif
    get_panel_widget (0)->wcontainer = containers [0];
    get_panel_widget (0)->area = AREA_RIGHT;
    get_panel_widget (1)->wcontainer = containers [1];
    get_panel_widget (1)->area = AREA_RIGHT;
    the_menubar->widget.wcontainer = (widget_data) NULL;
}
#else
#    define xtoolkit_panel_setup()
#endif

void load_hint ()
{
    char *hint;

    if (!the_hint->widget.parent)
	return;
	
    if (!message_visible && (!xterm_flag || !xterm_hintbar)){
        label_set_text (the_hint, 0);
	return;
    }

    if ((hint = get_random_hint ())){
	if (*hint)
	    set_hintbar (hint);
	free (hint);
    } else {
	set_hintbar ("The Midnight Commander " VERSION
			" (C) 1995-1997 the Free Software Foundation");
    }
}

static void
setup_panels_and_run_mc ()
{
    int first, second;

    xtoolkit_panel_setup ();
    tk_new_frame (midnight_dlg, "p.");
#ifndef HAVE_X
    add_widget (midnight_dlg, the_hint);
    load_hint ();
#ifdef BACKGROUND
    add_widget (midnight_dlg, process_status);
#endif /* BACKGROUND */
#endif /* HAVE_X */
    add_widgetl (midnight_dlg, cmdline, XV_WLAY_RIGHTOF);
    add_widgetl (midnight_dlg, the_prompt, XV_WLAY_DONTCARE);
    tk_end_frame ();
    add_widget (midnight_dlg, the_bar);
#ifdef HAVE_XVIEW
    add_widget (midnight_dlg, the_bar2);
#endif
    if (boot_current_is_left){
	first = 1;
	second = 0;
    } else {
	first = 0;
	second = 1;
    }
    add_widget (midnight_dlg, get_panel_widget (first));
    add_widget (midnight_dlg, get_panel_widget (second));
    add_widget (midnight_dlg, the_menubar);
    
    init_labels (get_panel_widget (0));
    init_labels (get_panel_widget (1));

    /* Run the Midnight Commander if no file was specified in the command line */
    run_dlg (midnight_dlg);
}

/* result must be free'd (I think this should go in util.c) */
char *prepend_cwd (const char *filename)
{
    char *d;
    int l;
    if (*filename == PATH_SEP)	/* an absolute pathname */
	return strdup (filename);
    d = malloc (MC_MAXPATHLEN + strlen (filename) + 2);
    mc_get_current_wd (d, MC_MAXPATHLEN);
    l = strlen(d);
    d[l++] = PATH_SEP;
    strcpy (d + l, filename);
    return canonicalize_pathname (d);
}

#ifdef USE_INTERNAL_EDIT
void edit (const char *file_name);
#endif

static void do_nc (void)
{
    midnight_dlg = create_dlg (0, 0, LINES, COLS,
	    midnight_colors, midnight_callback, "[main]", "midnight", 0);
    midnight_dlg->has_menubar = 1;

    if (view_one_file || edit_one_file) {
	/* Invoke the internal view/edit routine with:
	 * the default processing and forcing the internal viewer/editor
	 */
	char *path;
	if (view_one_file) {
	    path = prepend_cwd (view_one_file);
	    setup_dummy_mc (path);
	    view_file (path, 0, 1);
	}
#ifdef USE_INTERNAL_EDIT
	else {
	    path = prepend_cwd ("");
	    setup_dummy_mc (path);
	    edit (edit_one_file);
	}
#endif
	free (path);
	midnight_shutdown = 1;
	done_screen ();
	return;
    }
    setup_mc ();

    setup_panels_and_run_mc ();

    /* Program end */
    midnight_shutdown = 1;

    /* destroy_dlg destroys even cpanel->cwd, so we have to save a copy :) */
    if (print_last_wd) {
	if (!vfs_current_is_local ())
	    last_wd_string = strdup (".");
	else
	    last_wd_string = strdup (cpanel->cwd);
    }
    done_mc ();

    destroy_dlg (midnight_dlg);
    done_mc_profile ();
}



#if OLD
static int do_nc (void)
{
    int key;
    int i;

    if (COLS < 70 || LINES < 22){
	endwin ();
	fprintf (stderr, "Screen too small: you need at least 70x22\n");
	return 0;
    }

    init_panels ();


    /* FIXME: need to run the automenus on the new commander version */
    if (auto_menu)
	user_menu_cmd ();

    
    /* Main program loop */
    for (quit = 0; !quit;){
	remove_dash ();
	/* we need to change the layout here, since it can't be done   */
	/* on an inner loop of the program because of the setup frames */
	if (layout_do_change){
	    layout_change ();
	    refresh_screen (0);
	}
#ifdef HAVE_SLANG
	if (winch_flag)
	    change_screen_size ();
#endif
	
	if (was_searching != searching){
	    display_mini_info (cpanel);
	    mc_refresh ();
	}

	/* This is needed for ncurses 1.8.7, ugh :-( */
	/* Also SysV curses benefits from this */
	if (command_prompt){
	    leaveok (cmdline_win);
	    wrefresh (cmdline_win);
	}
	
	key = mi_getch ();

	was_searching = searching;
	if (quote){
	    default_key (key);
	    quote = 0;
	    continue;
	}
	/* This function should set the searching variable to 0 before */
	/* invoking the appropiate routine */
	if (check_fkeys (key)){
	    searching = 0;
	    continue;
	}
	else {
    /* Shutdown the program */
    destroy_input (cmdline, IN_NORMAL);
    done_mc ();
    
    pop_refresh ();
    return 1;
}
#endif /* EVENT mc */

#include "features.inc"

static void version (int verbose)
{
    fprintf (stderr, "Midnight Commander %s\n", VERSION);
    if (!verbose)
	return;
    
#ifndef HAVE_X
    fprintf (stderr,
	    "with mouse support on xterm%s%s%s.\n",
	     status_mouse_support ? " and the Linux console" : "",
	     status_using_ncurses ? "\nusing the ncurses library" : "\n",
	     status_using_old_tools ? " and compiled with old tools" : "");
#endif /* HAVE_X */

    fprintf (stderr, features);
    if (print_last_wd)
	write (stdout_fd, ".", 1);
}

#ifdef _OS_NT
#define CONTROL_FILE "\\mc.%d.control"
#else
#define CONTROL_FILE "/tmp/mc.%d.control"
#endif
char control_file [sizeof (CONTROL_FILE) + 8];
char *shell;

static void handle_args (int argc, char *argv [])
{
    extern int optind;
    extern char *optarg;
    int    c;
    char   *termvalue;
    int    finish_program = 0;

#ifdef HAVE_SLANG
#ifdef _OS_NT
    static
#else
    extern
#endif
    int SLtt_try_termcap;
#endif

#ifdef USE_TERMCAP
    SLtt_try_termcap = 1;
#endif

    while ((c = getopt (argc, argv, "VbsdPcfC:hmxtuUe:v:l:NXB")) != -1) {
	switch (c) {
	case 'V':
	    version (1);
	    finish_program = 1;
	    break;

	case 'B':
	    background_wait = 1;
	    break;

	case 'b':
	    disable_colors = 1;
	    break;

	case 'c':
	    disable_colors = 0;
#ifdef HAVE_SLANG
	    force_colors = 1;
#endif
	    break;

	case 'f':
	    fprintf (stderr, "Library directory for the Midnight Commander: " LIBDIR "\n");
	    finish_program = 1;
	    break;

	case 'm':
	    use_8th_bit_as_meta = 0;
	    break;

	case 'e':
	    edit_one_file = optarg;
	    break;

	case 'v':
	    view_one_file = optarg;
	    break;

#ifdef USE_NETCODE
	case 'l':
	    ftpfs_set_debug (optarg);
	    break;
#endif
	case 'P':
	    print_last_wd = 1;
	    break;

	case 's':
	    slow_terminal = 1;
	    break;

	case 'd':
	    use_mouse_p = NO_MOUSE;
	    break;

	case 'C':
	    command_line_colors = optarg;
	    break;

	case 'x':
	    force_xterm = 1;
	    break;

#ifdef HAVE_SLANG
	case 't':
	    SLtt_try_termcap = 1;
	    break;

#ifndef _OS_NT
	case 'N':
	    SLtt_want_unicode ();
	    break;
#endif				/* _OS_NT */
#endif				/* HAVE_SLANG */

	case 'u':
#ifdef HAVE_SUBSHELL_SUPPORT
		use_subshell = 0;
#endif
	    break;

	case 'X':
#ifdef HAVE_SUBSHELL_SUPPORT
	    debug_subshell = 1;
#endif
	    break;

	case 'U':
#ifdef HAVE_SUBSHELL_SUPPORT
	    use_subshell = 1;
#endif
	    break;

	case '?':
	case 'h':
	default:
	    version (0);
	    fprintf (stderr, "Usage is:\n"
		"mc [-bCcdfhlmNPstuUVvx?] [other_panel_dir] [this-dir]\n"
		     "-b Force black and white\n"
		     "-C <keyword>=<fore>,<back> Color usage keywords:\n"
	     "   normal, selected, marked, markselect, errors, reverse\n"
	     "   dnormal, dfocus, dhotnormal, dhotfocus, menu, menuhot\n"
		     "   menusel, menuhotsel."
		     "   colors: black, red, green, brightgreen, brown,\n"
		 "   yellow, blue, brightblue, magenta, brightmagenta,\n"
		     "   cyan, brightcyan, lightgray and white\n"
		 "-c Force color, only available if terminfo allows it\n"
		     "-d Disable mouse support\n"
		     "-f Print configured paths\n"
		  "-m Don't use the highest character bit as meta flag\n"
		     "-P At exit, print last working directory\n"
		   "-s Disables verbose operation (for slow terminals)\n"
#ifdef HAVE_SLANG
		     "-t Activate support for the TERMCAP variable\n"
		     "-N Use Unicode if available\n"
#endif
#ifdef USE_NETCODE
		     "-l file Log ftpfs commands to the file\n"
#endif
		     "-u Disable the concurrent subshell mode\n"
		     "-U Force the concurrent subshell mode\n"
		     "-v [file] start up into the viewer mode\n"
		 "-x Force xterm mouse support and screen save/restore\n"
		);
	    finish_program = 1;
	}
    }
    if (finish_program){
	if (print_last_wd)
	    printf (".");
	exit (1);
    }
#ifdef _OS_NT
    SetConsoleTitle ("GNU Midnight Commander");
    shell = getenv ("COMSPEC");
    if (!shell || !*shell)
	shell = "cmd.exe";
    /* Default opening mode for files is binary, not text (CR/LF translation) */
    _fmode = _O_BINARY;
#else
    termvalue = getenv ("TERM");
    if (!termvalue){
	fprintf (stderr, "The TERM environment variable is unset!\n");
	termvalue = "";
    }
    if (force_xterm || strncmp (termvalue, "xterm", 5) == 0){
	use_mouse_p = XTERM_MOUSE;
	xterm_flag = 1;
#    ifdef SET_TITLE
	printf ("\33]0;GNU Midnight Commander\7");
#    endif
    }
    shell = getenv ("SHELL");
    if (!shell || !*shell)
	shell = strdup (getpwuid (geteuid ())->pw_shell);
    if (!shell || !*shell)
	shell = "/bin/sh";
#endif				/* _OS_NT */

    sprintf (control_file, CONTROL_FILE, getpid ());
    my_putenv ("MC_CONTROL_FILE", control_file);

    /* sets the current dir and the other dir */
    if (strstr (argv[0], "mcedit") == argv[0] + strlen(argv[0]) - 6) {
	edit_one_file = "";
	for (; optind < argc; optind++) {
	    edit_one_file = strdup (argv[optind]);
	}
    } else {
	for (; optind < argc; optind++) {
	    if (this_dir) {
		other_dir = strdup (argv[optind]);
	    } else {
		char buffer[MC_MAXPATHLEN + 2];
		this_dir = argv[optind];
		mc_get_current_wd (buffer, sizeof (buffer) - 2);
	    }
	}
    }
}


#ifdef _OS_NT
static void sigchld_handler_no_subshell (int sig)
{
}

void init_sigchld (void)
{
}

#else
static void sigchld_handler_no_subshell (int sig)
{
    int pid, status;

    if (!console_flag)
	return;
    
    /* COMMENT: if it were true that after the call to handle_console(..INIT)
       the value of console_flag never changed, we could simply not install
       this handler at all if (!console_flag && !use_subshell). */

    /* That comment is no longer true.  We need to wait() on a sigchld
       handler (that's at least what the tarfs code expects currently). */

    pid = waitpid (cons_saver_pid, &status, WUNTRACED | WNOHANG);
    
    if (pid == cons_saver_pid){
	/* {{{ Someone has stopped or killed cons.saver; restart it */

	if (WIFSTOPPED (status))
	    kill (pid, SIGCONT);
	else
	{
	    handle_console (CONSOLE_DONE);
	    handle_console (CONSOLE_INIT);
	}
	/* }}} */
    }

    /* If we get here, some other child exited; ignore it */
}

void init_sigchld (void)
{
    struct sigaction sigchld_action;

    sigchld_action.sa_handler =
#ifdef HAVE_SUBSHELL_SUPPORT
	use_subshell ? sigchld_handler :
#endif
	sigchld_handler_no_subshell;

    sigemptyset (&sigchld_action.sa_mask);

#ifdef SA_RESTART
        sigchld_action.sa_flags = SA_RESTART;
#else
        sigchld_action.sa_flags = 0;
#endif

    sigaction (SIGCHLD, &sigchld_action, NULL);
}	

#endif /* OS_NT */

int main (int argc, char *argv [])
{
#ifndef _OS_NT
    /* Backward compatibility: Gives up privileges in case someone
       installed the mc as setuid */
    setuid (getuid ());
#endif
    vfs_init ();

#ifdef HAVE_X
    /* NOTE: This call has to be before any our argument handling :) */
    if (xtoolkit_init (&argc, argv) == -1)
	exit (1);
#endif
#ifdef HAVE_SLANG
    SLtt_Ignore_Beep = 1;
#endif

    /* NOTE: This has to be called before slang_init or whatever routine
       calls any define_sequence */
    init_key ();
	
    handle_args (argc, argv);
    
    /* Used to report the last working directory at program end */
    if (print_last_wd){
#ifndef _OS_NT	
	stdout_fd = dup (1);
	close (1);
	if (open (ttyname (0), O_RDWR) < 0)
	    if (open ("/dev/tty", O_RDWR) < 0) {
	    /* Try if stderr is not redirected as the last chance */
	        char *p = strdup (ttyname (0));
	        
	        if (!strcmp (p, ttyname (2)))
	            dup2 (2, 1);
	        else {
	            fprintf (stderr,
	            	     "Couldn't open tty line. You have to run mc without the -P flag.\n"
			     "On some systems you may want to run # `which mc`\n");
		    exit (1);
	        }
	        free (p);
	    }
#endif
    }

#   ifdef HAVE_X
    /* This is to avoid subshell trying to restard any child pid
     * that happends to have cons_saver_pid (a random startup value).
     * and PID 1 is init, unlikely we could be the parent of it.
     */
    cons_saver_pid = 1;
#   else
    /* Must be done before installing the SIGCHLD handler [[FIXME]] */
    handle_console (CONSOLE_INIT);
#   endif
    
#   ifdef HAVE_SUBSHELL_SUPPORT
    subshell_get_console_attributes ();
#   endif
    
    /* Install the SIGCHLD handler; must be done before init_subshell() */
    init_sigchld ();
    
    /* This variable is used by the subshell */
#ifdef _OS_NT
    home_dir = copy_strings (getenv ("HOMEDRIVE"), getenv ("HOMEPATH"), NULL);
    /* we need to malloc something (we free(home_dir)) */
    if (!home_dir) {
	home_dir = malloc(MAX_PATH);
	GetWindowsDirectory(home_dir, MAX_PATH);
    }	
#else
    home_dir = getenv ("HOME");
    home_dir = home_dir ? home_dir : PATH_SEP_STR;
#endif

#   ifdef HAVE_X
    /* We need this, since ncurses endwin () doesn't restore the signals */
    save_stop_handler ();
#   endif

    /* Must be done before init_subshell, to set up the terminal size: */
    /* FIXME: Should be removed and LINES and COLS computed on subshell */
    slang_init ();
    initscr ();

#   ifdef HAVE_SUBSHELL_SUPPORT
	/* Done here to ensure that the subshell doesn't  */
	/* inherit the file descriptors opened below, etc */

	if (use_subshell)
	    init_subshell ();  
#   endif

    load_setup ();

    init_curses ();

    /* The directory hot list */
    load_hotlist ();

#   ifndef HAVE_X
    /* Removing this from the X code let's us type C-c */
    load_key_defs ();

    /* Also done after init_subshell, to save any shell init file messages */
    if (console_flag)
	handle_console (CONSOLE_SAVE);
    if (alternate_plus_minus && (console_flag || xterm_flag)) {
        fprintf (stderr, "\033="); fflush (stderr);
    }
#   endif

#   ifdef HAVE_SUBSHELL_SUPPORT
	if (use_subshell){
	    prompt = strip_ctrl_codes (subshell_prompt);
	    if (!prompt)
		prompt = "";
	} else
#   endif
	    prompt = (geteuid () == 0) ? "# " : "$ ";

    /* Program main loop */
    do_nc ();
    
    /* Virtual File System shutdown */
    vfs_shut ();

    flush_extension_file (); /* does only free memory */

#   ifndef HAVE_X
    /* Miguel, maybe the fix in slang is not required and
     * it could be done by removing the slang_done_screen.
     * Do I need to call slang_reset_tty then?
     */
    endwin ();
    slang_shutdown ();

    if (console_flag && !(quit & SUBSHELL_EXIT))
	restore_console ();
    if (alternate_plus_minus && (console_flag || xterm_flag)) {
        fprintf (stderr, "\033>"); fflush (stderr);
    }
#   endif

#ifndef _OS_NT
    signal (SIGCHLD, SIG_DFL);  /* Disable the SIGCHLD handler */
#endif
    
#   ifndef HAVE_X
    if (console_flag)
	handle_console (CONSOLE_DONE);
#   endif
    putchar ('\r');  /* Hack to make shell's prompt start at left of screen */

#ifdef _OS_NT
    /* On NT, home_dir is malloced */
    free (home_dir);
#endif
    if (print_last_wd) {
        if (print_last_revert || edit_one_file || view_one_file)
            write (stdout_fd, ".", 1);
        else
	    write (stdout_fd, last_wd_string, strlen (last_wd_string));
	free (last_wd_string);
    }

#ifdef HAVE_MAD
    done_key ();
#endif
    mad_finalize (__FILE__, __LINE__);
#ifdef HAVE_X
    xtoolkit_end ();
#endif
    return 0;
}
