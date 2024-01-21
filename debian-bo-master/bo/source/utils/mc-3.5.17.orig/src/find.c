/* Find file command for the Midnight Commander
   Copyright (C) The Free Software Foundation
   Written 1995 by Miguel de Icaza

   Complete rewrote.
   
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
#include "tty.h"
#include <string.h>
#include <stdio.h>
#include <malloc.h>	/* For free() */
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

#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <ctype.h>
#include "global.h"
#include "mad.h"
#include "util.h"
#include "win.h"
#include "color.h"
#include "global.h"

extern int verbose;		/* Should be in a more sensible header file */

/* Dialog manager and widgets */
#include "dlg.h"
#include "widget.h"

#include "dialog.h"     /* For do_refresh() */
#define  DIR_H_INCLUDE_HANDLE_DIRENT
#include "dir.h"
#include "panel.h"		/* current_panel */
#include "main.h"		/* do_cd, try_to_select */
#include "wtools.h"
#include "tree.h"
#include "../vfs/vfs.h"

#ifdef HAVE_XVIEW
#include "xvmain.h"
#endif


/* Size of the find parameters window */
#define FIND_Y 12
#define FIND_X 50

/* Size of the find window */
#define FIND2_Y LINES-4
#define FIND2_X COLS-16

#ifdef HAVE_X
#   define FIND2_X_USE 35
#else
#   define FIND2_X_USE FIND2_X-20
#endif

/* A couple of extra messages we need */
enum {
    B_STOP = B_USER + 1,
    B_AGAIN,
    B_PANELIZE,
    B_TREE,
    B_VIEW
};

/* A list of directories to be ignores, separated with ':' */
char *find_ignore_dirs = 0;

static Dlg_head *find_dlg;	/* The dialog */
static WInput *in_start;	/* Start path */
static WInput *in_name;		/* Pattern to search */
static WInput *in_with;		/* text inside filename */
static WListbox *find_list;	/* Listbox with the file list */
static int running = 0;		/* nice flag */
static WButton *stop_button;	/* pointer to the stop button */
static WLabel *status_label;	/* Finished, Searching etc. */
static char *find_pattern;	/* Pattern to search */
static char *content_pattern;	/* pattern to search inside files */
static int count;		/* Number of files displayed */
static int matches;		/* Number of matches */
static int is_start;		/* Status of the start/stop toggle button */
int max_loops_in_idle = 10;
static char *old_dir;

/* For nice updating */
static char *rotating_dash = "|/-\\";

/* This keeps track of the directory stack */
typedef struct dir_stack {
    char *name;
    struct dir_stack *prev;
} dir_stack ;

dir_stack *dir_stack_base = 0;

/*
 * find_parameters: gets information from the user
 *
 * If the return value is true, then the following holds:
 *
 * START_DIR and PATTERN are pointers to char * and upon return they
 * contain the information provided by the user.
 *
 * CONTENT holds a strdup of the contents specified by the user if he
 * asked for them or 0 if not (note, this is different from the
 * behavior for the other two parameters.
 *
 */

static int
find_parameters (char **start_dir, char **pattern, char **content)
{
    int return_value;
    char *temp_dir;
    static char *in_contents = NULL;
    static char *in_start_dir = NULL;
    static char *in_start_name = NULL;

find_par_start:
    if (!in_start_dir)
	in_start_dir = strdup (".");
    if (!in_start_name)
	in_start_name = strdup (easy_patterns ? "*" : ".");
    if (!in_contents)
	in_contents = strdup ("");
    
    find_dlg = create_dlg (0, 0, FIND_Y, FIND_X, dialog_colors,
			   common_dialog_callback, "[Find File]", "findfile",
			   DLG_CENTER | DLG_GRID);
    x_set_dialog_title (find_dlg, "Find File");

    in_start = input_new (3, 14, INPUT_COLOR, 30, in_start_dir, "start");
    in_name  = input_new (5, 14, INPUT_COLOR, 30, in_start_name, "name");
    in_with  = input_new (7, 14, INPUT_COLOR, 30, in_contents, "content");
    
#ifndef HAVE_XVIEW
    add_widget (find_dlg, in_start);
#endif
    add_widgetl (find_dlg, button_new (9, 36, B_CANCEL,"[ Cancel ]",'c',2,0,0, "cancel"),
        XV_WLAY_RIGHTOF);
    
    add_widgetl (find_dlg, button_new (9, 12, B_TREE, "[ Tree ]", 't', 2,0,0, "tree"),
        XV_WLAY_RIGHTOF);
    add_widgetl (find_dlg, button_new (9, 4, B_ENTER,"[ Ok ]",'o', 2, 0, 0, "ok"),
        XV_WLAY_CENTERROW);

    add_widget (find_dlg, in_with);
    add_widgetl (find_dlg, in_name, XV_WLAY_RIGHTOF);

    add_widgetl (find_dlg, label_new (7, 3, "Content: ", "label-cont"), XV_WLAY_NEXTROW);
    add_widgetl (find_dlg, label_new (5, 3, "Filename:", "label-file"), XV_WLAY_NEXTROW);

#ifdef HAVE_XVIEW
    add_widgetl (find_dlg, in_start, XV_WLAY_RIGHTOF);
#endif    
    add_widget (find_dlg, label_new (3, 3, "Start at:", "label-start"));

    run_dlg (find_dlg);
    if (find_dlg->ret_value == B_CANCEL)
	return_value = 0;
    else if (find_dlg->ret_value == B_TREE){
	temp_dir = strdup (in_start->buffer);
	destroy_dlg (find_dlg);
	free (in_start_dir);
	if (strcmp (temp_dir, ".") == 0){
	    free (temp_dir);
	    temp_dir = strdup (cpanel->cwd);
	}
	in_start_dir = tree (temp_dir);
	if (in_start_dir)
	    free (temp_dir);
	else
	    in_start_dir = temp_dir;
	/* Warning: Dreadful goto */
	goto find_par_start;
    } else {
	return_value = 1;
	*start_dir = strdup (in_start->buffer);
	*pattern   = strdup (in_name->buffer);

	if (in_with->buffer [0])
	    *content   = strdup (in_with->buffer);
	else
	    *content   = 0;
	free (in_start_dir);
	in_start_dir = strdup (*start_dir);
	free (in_start_name);
	in_start_name = strdup (*pattern);
    }

    destroy_dlg (find_dlg);
			 
    return return_value;
}

static void
push_directory (char *dir)
{
    dir_stack *new;

    new = xmalloc (sizeof (dir_stack), "find: push_directory");
    new->name = strdup (dir);
    new->prev = dir_stack_base;
    dir_stack_base = new;
}

static char*
pop_directory (void)
{
    char *name; 
    dir_stack *next;

    if (dir_stack_base){
	name = dir_stack_base->name;
	next = dir_stack_base->prev;
	free (dir_stack_base);
	dir_stack_base = next;
	return name;
    } else
	return 0;
}

static void
insert_file (char *dir, char *file)
{
    char *tmp_name;
    static char *dirname;
    int i;

    if (dir [0] == PATH_SEP && dir [1] == PATH_SEP)
	dir++;
    i = strlen (dir);
    if (i){
	if (dir [i - 1] != PATH_SEP){
	    dir [i] = PATH_SEP;
	    dir [i + 1] = 0;
	}
    }

    if (old_dir){
	if (strcmp (old_dir, dir)){
	    free (old_dir);
	    old_dir = strdup (dir);
	    dirname = listbox_add_item (find_list, 0, 0, dir, 0);
	}
    } else {
	old_dir = strdup (dir);
	dirname = listbox_add_item (find_list, 0, 0, dir, 0);
    }
    
    tmp_name = copy_strings ("    ", file, 0);
    listbox_add_item (find_list, 0, 0, tmp_name, dirname);
    free (tmp_name);
}

static void
find_add_match (Dlg_head *h, char *dir, char *file)
{
    int p = ++matches & 7;

    insert_file (dir, file);
    
    /* Scroll nicely */
    if (!p)
	listbox_select_last (find_list, 1);
    else
	listbox_select_last (find_list, 0);
    
#ifndef HAVE_X
	/* Updates the current listing */
	send_message (h, &find_list->widget, WIDGET_DRAW, 0);
	if (p == 7)
	    mc_refresh ();
#endif	    
}

/* 
 * search_content:
 *
 * Search with egrep the global (FIXME) content_pattern string in the
 * DIRECTORY/FILE.  It will add the found entries to the find listbox.
 */
void
search_content (Dlg_head *h, char *directory, char *filename)
{
    struct stat s;
    char buffer [128];
    char *fname, *p;
    int file_fd, pipe, ignoring;
    char c, i;
    pid_t pid;

    fname = get_full_name (directory, filename);

    if (mc_stat (fname, &s) != 0 && !S_ISREG (s.st_mode)){
	free (fname);
	return;
    }
    if (!S_ISREG (s.st_mode)){
	free (fname);
	return;
    }
    
    file_fd = mc_open (fname, O_RDONLY);
    free (fname);
    
    if (file_fd == -1)
	return;

    pipe = mc_doublepopen (file_fd, -1, &pid, "/usr/bin/egrep",
			   "/usr/bin/egrep", "-n", content_pattern, "-", NULL);
	
    if (pipe == -1){
	mc_close (file_fd);
	return;
    }
    
    sprintf (buffer, "Grepping in %s", name_trunc (filename, FIND2_X_USE));

    label_set_text (status_label, buffer);
    mc_refresh ();
    p = buffer;
    ignoring = 0;
    
    enable_interrupt_key ();
    got_interrupt ();
    while (1){
	i = read (pipe, &c, 1);
	if (i != 1)
	    break;
	
	if (c == '\n'){
	    p = buffer;
	    ignoring = 0;
	}
	if (ignoring)
	    continue;
	
	if (c == ':'){
	    char *the_name;
	    
	    *p = 0;
	    ignoring = 1;
	    the_name = copy_strings (buffer, ":", filename, NULL);
	    find_add_match (h, directory, the_name);
	    free (the_name);
	} else {
	    if (p - buffer < (sizeof (buffer)-1) && ISASCII (c) && isdigit (c))
		*p++ = c;
	    else
		*p = 0;
	} 
    }
    disable_interrupt_key ();
    if (i == -1)
	message (1, " Find/read ", " Problem reading from child ");

    mc_doublepclose (pipe, pid);
    mc_close (file_fd);
}

static void
do_search (struct Dlg_head *h)
{
    static struct dirent *dp   = 0;
    static DIR  *dirp = 0;
    static char directory [MC_MAXPATHLEN+2];
    struct stat tmp_stat;
    static int pos;
    static int subdirs_left = 0;
    char *tmp_name;		/* For bulding file names */

    if (!h) { /* someone forces me to close dirp */
	if (dirp) {
	    mc_closedir (dirp);
	    dirp = 0;
	}
        dp = 0;
	return;
    }

 do_search_begin:
    while (!dp){
	
	if (dirp){
	    mc_closedir (dirp);
	    dirp = 0;
	}
	
	while (!dirp){
	    char *tmp;

#ifndef HAVE_X	    
	    attrset (REVERSE_COLOR);
#endif
	    while (1) {
		tmp = pop_directory ();
		if (!tmp){
		    running = 0;
		    label_set_text (status_label, "Finished");
		    set_idle_proc (h, 0);
		    return;
		}
		if (find_ignore_dirs){
		    char *temp_dir = copy_strings (":", tmp, ":", 0);
		    if (strstr (find_ignore_dirs, temp_dir))
			free (tmp);
		    else
			break;
		} else
		    break;
	    } 
	    
	    strcpy (directory, tmp);
	    free (tmp);

	    if (verbose){
		    char buffer [50];

		    sprintf (buffer, "Searching %s", name_trunc (directory, FIND2_X_USE));
		    label_set_text (status_label, buffer);
	    }
	    dirp = mc_opendir (directory);
	    mc_stat (directory, &tmp_stat);
	    subdirs_left = tmp_stat.st_nlink - 2;
	    /* Commented out as unnecessary
	       if (subdirs_left < 0)
	       subdirs_left = MAXINT;
	    */
	}
	dp = mc_readdir (dirp);
    }

    if (strcmp (dp->d_name, ".") == 0 ||
	strcmp (dp->d_name, "..") == 0){
	dp = mc_readdir (dirp);
#ifdef HAVE_XVIEW
	xv_post_proc (h, (void (*)(void *))do_search, (void *)h);
#endif		
	return;
    }
    
    tmp_name = get_full_name (directory, dp->d_name);

    if (subdirs_left){
	mc_lstat (tmp_name, &tmp_stat);
	if (S_ISDIR (tmp_stat.st_mode)){
	    push_directory (tmp_name);
	    subdirs_left--;
	}
    }

    if (regexp_match (find_pattern, dp->d_name, match_file)){
	if (content_pattern)
	    search_content (h, directory, dp->d_name);
	else 
	    find_add_match (h, directory, dp->d_name);
    }
    
    free (tmp_name);
    dp = mc_readdir (dirp);

    /* Displays the nice dot */
    count++;
    if (!(count & 31)){
	if (verbose){
#ifndef HAVE_X
	    pos = (++pos) % 4;
	    attrset (NORMALC);
	    dlg_move (h, FIND2_Y-6, FIND2_X - 4);
	    addch (rotating_dash [pos]);
	    mc_refresh ();
	}
    } else
	goto do_search_begin;
#else
        }
    }
#ifdef HAVE_XVIEW
    xv_post_proc (h, (void (*)(void *))do_search, (void *)h);
#endif		
#endif
}

static int
view_currently_selected_file (int unparsed_view)
{
    WLEntry *entry = find_list->current;
    char *dir, *fullname, *filename;
    int line;

    if (!entry)
        return MSG_NOT_HANDLED;

    dir = entry->data;

    if (!entry->text || !dir)
	return MSG_NOT_HANDLED;

    if (content_pattern){
	filename = strchr (entry->text + 4, ':') + 1;
	line = atoi (entry->text + 4);
    } else {
	 filename = entry->text + 4;
	 line = 0;
    }
    if (dir [0] == '.' && dir [1] == 0)
	 fullname = strdup (filename);
    else if (dir [0] == '.' && dir [1] == PATH_SEP)
	 fullname = get_full_name (dir+2, filename);
    else
	 fullname = get_full_name (dir, filename);

    view_file_at_line (fullname, unparsed_view, use_internal_view, line);
    free (fullname);
    return MSG_HANDLED;
}

static int
find_callback (struct Dlg_head *h, int id, int Msg)
{
    switch (Msg){
#ifndef HAVE_X    
    case DLG_DRAW:
        common_dialog_repaint (h);
	break;
#endif

    case DLG_KEY:
	if (id == KEY_F(3) || id == KEY_F(13)){
	    int unparsed_view = (id == KEY_F(13));
	    return view_currently_selected_file (unparsed_view);
	 }
	 return MSG_NOT_HANDLED;

     case DLG_IDLE:
	 do_search (h);
	 break;
     }
     return 0;
}

#define BUTTON_NEW(i,msg,str,name) \
   button_new (FIND2_Y-4, 3+i*12, msg, str, str [2], 2, 0, 0,name)

#define BUTTON_NEW_CBACK(i,msg,str,a,b,name) \
   button_new (FIND2_Y-4, 3+i*12, msg, str, str [2], 2, a, b, name)

#define BUTTON_NEW_CBACKY(i,j,msg,str,a,b,name) \
   button_new (FIND2_Y-4+j, 3+i*12, msg, str, str [2], 2, a, b, name)

/* Handles the Stop/Start button in the find window */
static int
start_stop (int button, void *extra)
{
    char *button_labels [2] = { "[ Stop ] ", "[ Start ]" };
    running = is_start;
    set_idle_proc (find_dlg, running);
    is_start = !is_start;

    label_set_text (status_label, is_start ? "Stopped" : "Searching");
    button_set_text (stop_button, button_labels [is_start]);

    return 0;
}

/* Handle view command, when invoked as a button */
static int
find_do_view_file (int button, void *extra)
{
    view_currently_selected_file (0);
    return 0;
}

static void
init_find_vars (void)
{
    char *dir;
    
    if (old_dir){
	free (old_dir);
	old_dir = 0;
    }
    count = 0;
    matches = 0;

    /* Remove all the items in the stack */
    while ((dir = pop_directory ()) != NULL)
	free (dir);
}


struct spaced_dirent {
    struct dirent dp;
#ifdef NEED_EXTRA_DIRENT_BUFFER
    char extra_buffer [MC_MAXPATHLEN];
#endif
};

static int
find_file (char *start_dir, char *pattern, char *content, char **dirname,  char **filename)
{
    int return_value = 0;
    char *dir;
    char *dir_tmp, *file_tmp;
    
    find_dlg = create_dlg (0, 0, FIND2_Y, FIND2_X, dialog_colors,
			   find_callback, "[Find File]", "mfind", DLG_CENTER | DLG_GRID);
    
    x_set_dialog_title (find_dlg, "Find file");

    find_list = listbox_new (2, 2, FIND2_X-4, FIND2_Y-9, listbox_finish, 0, "listbox");

#ifndef HAVE_X
    add_widget (find_dlg, find_list);
#endif

    add_widgetl (find_dlg,
		button_new (FIND2_Y-3, 3, B_PANELIZE, "[ Panelize ]", 'p', 2, 0, 0, "button-panelize"),
		XV_WLAY_CENTERROW);
    add_widgetl (find_dlg,
		 BUTTON_NEW_CBACKY(2, 1,B_VIEW, "[ View - F3 ]", find_do_view_file, find_dlg, "button-view"),
		 0);
    add_widgetl (find_dlg, BUTTON_NEW (3, B_CANCEL, "[ Quit ]", "button-quit"),
        XV_WLAY_RIGHTOF);
    stop_button = BUTTON_NEW_CBACK (2, B_STOP,   "[ Stop  ]", start_stop,
				    find_dlg, "start-stop");
    add_widgetl (find_dlg, stop_button,
        XV_WLAY_RIGHTOF);
    add_widgetl (find_dlg, BUTTON_NEW (1, B_AGAIN,  "[ Again ]", "button-again"),
        XV_WLAY_RIGHTOF);
    add_widgetl (find_dlg, BUTTON_NEW (0, B_ENTER,  "[ Chdir ]", "button-chdir"), 
        XV_WLAY_CENTERROW);

    status_label = label_new (FIND2_Y-6, 4, "Searching", "label-search");
    add_widgetl (find_dlg, status_label, XV_WLAY_BELOWOF);
#ifdef HAVE_X
    add_widgetl (find_dlg, find_list, XV_WLAY_EXTENDWIDTH);
#endif

    /* FIXME: Need to cleanup this, this ought to be passed non-globaly */
    find_pattern    = pattern;
    content_pattern = content;
    
    set_idle_proc (find_dlg, 1);
    init_find_vars ();
    push_directory (start_dir);

#ifdef HAVE_XVIEW
    xv_post_proc (find_dlg, (void (*)(void *))do_search, (void *)find_dlg);
#endif    
    run_dlg (find_dlg);

    return_value = find_dlg->ret_value;

    /* Remove all the items in the stack */
    while ((dir = pop_directory ()) != NULL)
	free (dir);
    
    listbox_get_current (find_list, &file_tmp, &dir_tmp);

    if (dir_tmp)
	*dirname  = strdup (dir_tmp);
    if (file_tmp)
	*filename = strdup (file_tmp);
    if (return_value == B_PANELIZE && *dirname && *filename){
	struct spaced_dirent dp;
	int status, link_to_dir, stalled_link;
	int next_free = 0;
	int i, k;
	struct stat buf;
	WLEntry *entry = find_list->list;
	dir_list *list = &cpanel->dir;
	char *dir, *name;

	for (i = 0; entry && i < find_list->count; entry = entry->next, i++){
	    char *filename;

	    if (content_pattern)
		filename = strchr (entry->text+4, ':')+1;
	    else
		filename = entry->text+4;

	    if (!entry->text || !entry->data)
		continue;
	    dir = entry->data;
	    if (dir [0] == '.' && dir [1] == 0)
		name = strdup (filename);
	    else if (dir [0] == '.' && dir [1] == PATH_SEP)
		name = get_full_name (dir + 2, filename);
	    else
		name = get_full_name (dir, filename);
	    strcpy (dp.dp.d_name, name);
	    free (name);
#if !defined(HAVE_DIRENT_H) && !defined(_POSIX_VERSION)
	    NLENGTH (&dp.dp) = strlen (dp.dp.d_name);
#endif
	    status = handle_dirent (list, NULL, &dp.dp, &buf, next_free, &link_to_dir,
	        &stalled_link);
	    if (status == 0)
		continue;
	    if (status == -1)
		break;

	    /* don't add files more than once to the panel */
	    if (content_pattern && next_free > 0){
		if (strcmp (list->list [next_free-1].fname, dp.dp.d_name) == 0)
		    continue;
	    }

	    if (!next_free) /* first turn i.e clean old list */
    		clean_dir (list, cpanel->count);
	    list->list [next_free].fnamelen = NLENGTH (&(dp.dp));
	    list->list [next_free].fname = strdup (dp.dp.d_name);
	    list->list [next_free].cache = NULL;
	    file_mark (cpanel, next_free, 0);
	    list->list [next_free].f.link_to_dir = link_to_dir;
	    list->list [next_free].f.stalled_link = stalled_link;
	    list->list [next_free].buf = buf;
	    next_free++;
           if (!(next_free & 15))
	       rotate_dash ();
	}
	if (next_free){
	    cpanel->count = next_free;
	    cpanel->is_panelized = 1;
	    cpanel->dirs_marked = 0;
	    cpanel->marked = 0;
	    cpanel->total = 0;

	    if (start_dir [0] == PATH_SEP){
		strcpy (cpanel->cwd, PATH_SEP_STR);
		chdir (PATH_SEP_STR);
	    }
	}
    }

    /* cpanel->top_file and cpanel->selected must be set to a valid
     * value before call of destroy_dlg which calls paint_panel  */
    try_to_select (cpanel, NULL);
    destroy_dlg (find_dlg);
    do_search (0); /* force do_search to release resources */
    if (old_dir){
	free (old_dir);
	old_dir = 0;
    }
    return return_value;
}

void
do_find (void)
{
    char *start_dir, *pattern, *content;
    char *filename, *dirname;
    int  v, dir_and_file_set;
    int done = 0;
    
    while (!done){
	if (!find_parameters (&start_dir, &pattern, &content))
	    break;

	dirname = filename = NULL;
	is_start = 0;
	v = find_file (start_dir, pattern, content, &dirname, &filename);
	free (start_dir);
	free (pattern);
	if (content)
	    free (content);

	if (v == B_ENTER){
	    if (dirname || filename){
		if (dirname){
		    do_cd (dirname, cd_exact);
		    if (filename)
			try_to_select (current_panel, filename + 4);
		} else if (filename)
		    do_cd (filename, cd_exact);
		paint_panel (current_panel);
		select_item (cpanel);
	    }
	    if (dirname)  
		free (dirname);
	    if (filename) 
		free (filename);
	    break;
	}
	dir_and_file_set = dirname && filename;
	if (dirname)  free (dirname);
	if (filename) free (filename);
	if (v == B_CANCEL)
	    break;
	
	if (v == B_PANELIZE){
	    if (dir_and_file_set){
	        try_to_select (cpanel, NULL);
	        paint_panel (cpanel);
	    }
	    break;
	}
    }
}

