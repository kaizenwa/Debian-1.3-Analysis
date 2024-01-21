/* External panelize
   Copyright (C) 1995 The Free Software Foundation
   
   Written by: 1995 Janne Kukonlehto
               1995 Jakub Jelinek

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  
 */

#include <config.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#include <string.h>
#include <stdio.h>
#include <stdlib.h>		/* For malloc() */
#include <errno.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>
#include "tty.h"
#include "mad.h"
#include "util.h"		/* Needed for the externs */
#include "win.h"
#include "color.h"
#include "dlg.h"
#include "widget.h"
#include "dialog.h"		/* For do_refresh() */
#include "setup.h"		/* For profile_bname */
#include "profile.h"		/* Load/save directories panelize */
#include "fs.h"

/* Needed for the extern declarations of integer parameters */
#define DIR_H_INCLUDE_HANDLE_DIRENT
#include "dir.h"
#include "panel.h"		/* Needed for the externs */
#include "file.h"
#include "main.h"
#include "global.h"
#include "../vfs/vfs.h"

void do_external_panelize (char *command);

#define UX		5
#define UY		2

#define BX		5
#define BY		18

#define BUTTONS		4
#define LABELS          3
#define B_ADD		B_USER
#define B_REMOVE        B_USER + 1

static WListbox *l_panelize;

static Dlg_head *panelize_dlg;

static WInput *pname;

static char *panelize_section = "Panelize";

static int last_listitem;

struct {
    int ret_cmd, y, x;
    char *text;
    int hkey, hpos;
    char *tkname;
} panelize_but[BUTTONS] = {
    { B_CANCEL, 0, 53, "[ Cancel ]",  'c', 2, "c"},
    { B_ADD,    0, 28, "[ Add new ]", 'a', 2, "a"},
    { B_REMOVE, 0, 16, "[ Remove ]",  'r', 2, "r"},
    { B_ENTER,  0,  0, "[[ Panelize ]]", 'p', 5, "p"},
};

/* Directory panelize */
static struct panelize{
    char *command;
    char *label;
    struct panelize *next;
} *panelize = NULL;

#ifndef HAVE_X
static void panelize_refresh (void)
{
    attrset (REVERSE_COLOR);
    dlg_erase (panelize_dlg);
    
    draw_box (panelize_dlg, 1, 2, 20, 70);
    draw_box (panelize_dlg, UY, UX, 12, 63);
    
    attrset (COLOR_HOT_NORMAL);
    dlg_move (panelize_dlg, 1, (72 - 19) / 2);
    addstr (" External panelize ");
}
#endif

static INLINE void update_command ()
{
    if (l_panelize->pos != last_listitem) {
    	last_listitem = l_panelize->pos;
        assign_text (pname, 
            ((struct panelize *) l_panelize->current->data)->command);
	pname->point = 0;
        update_input (pname);
    }
}

static int panelize_callback (Dlg_head * h, int Par, int Msg)
{
    switch (Msg) {
#ifndef HAVE_X    
    case DLG_DRAW:
	panelize_refresh ();
	break;
#endif	

    case DLG_POST_KEY:
	/* fall */
    case DLG_INIT:
	attrset (MENU_ENTRY_COLOR);
	update_command ();
	break;
    }
    return 0;
}

static int l_call (void *data)
{
    return 1;
}

static void init_panelize (void)
{
    int i;
    struct panelize *current = panelize;
    
    last_listitem = 0;
    
    do_refresh ();

    panelize_dlg = create_dlg (0, 0, 22, 74, dialog_colors,
			      panelize_callback, "[External panelize]", "panelize",
			      DLG_CENTER|DLG_GRID);
    x_set_dialog_title (panelize_dlg, "External panelize");

#define XTRACT(i) BY+panelize_but[i].y, BX+panelize_but[i].x, panelize_but[i].ret_cmd, panelize_but[i].text, panelize_but[i].hkey, panelize_but[i].hpos, 0, 0, panelize_but[i].tkname

    for (i = 0; i < BUTTONS; i++)
	add_widgetl (panelize_dlg, button_new (XTRACT (i)), (i == BUTTONS - 1) ?
	    XV_WLAY_CENTERROW : XV_WLAY_RIGHTOF);

    pname = input_new (UY+14, UX, INPUT_COLOR, 63, "", "in");
    add_widgetl (panelize_dlg, pname, XV_WLAY_RIGHTOF);

    add_widgetl (panelize_dlg, label_new (UY+13, UX, "Command", "label-command"), XV_WLAY_NEXTROW);

    /* get new listbox */
    l_panelize = listbox_new (UY + 1, UX + 1, 61, 10, 0, l_call, "li");

    while (current){
	listbox_add_item (l_panelize, 0, 0, current->label, current);
	current = current->next;
    }

    /* add listbox to the dialogs */
    add_widgetl (panelize_dlg, l_panelize, XV_WLAY_EXTENDWIDTH); 

    listbox_select_entry (l_panelize, 
        listbox_search_text (l_panelize, "Other command"));
}

static void panelize_done (void)
{
    destroy_dlg (panelize_dlg);
    repaint_screen ();
}

static void add2panelize (char *label, char *command)
{
    struct panelize *current, *old;

    old = NULL;
    current = panelize;
    while (current && strcmp (current->label, label) <= 0){
	old = current;
	current = current->next;
    }

    if (old == NULL){
	panelize = malloc (sizeof (struct panelize));
	panelize->label = label;
	panelize->command = command;
	panelize->next = current;
    } else {
	struct panelize *new;
	new = malloc (sizeof (struct panelize));
	new->label = label;
	new->command = command;
	old->next = new;
	new->next = current;
    }
}

void add2panelize_cmd (void)
{
    char *label;

    if (pname->buffer && (*pname->buffer)) {
	label = input_dialog (" Add to external panelize ", "Unnamed find", 
			      "");
	if (!label)
	    return;
	if (!*label) {
	    free (label);
	    return;
	}
	
	add2panelize (label, strdup(pname->buffer));
    }
}

static void remove_from_panelize (struct panelize *entry)
{
    if (strcmp (entry->label, "Other command") != 0) {
	if (entry == panelize) {
	    panelize = panelize->next;
	} else {
	    struct panelize *current = panelize;
	    while (current && current->next != entry)
		current = current->next;
	    if (current) {
		current->next = entry->next;
	    }
	}

	free (entry->label);
	free (entry->command);
	free (entry);
    }
}

void external_panelize (void)
{
    char *target = NULL;

    if (!vfs_current_is_local ()){
	message (1, " Oops... ",
	    " I can't run external panalize while logged on a non local directory ");
	return;
    }

    init_panelize ();

    /* display file info */
    attrset (SELECTED_COLOR);

    run_dlg (panelize_dlg);

    switch (panelize_dlg->ret_value) {
    case B_CANCEL:
	break;

    case B_ADD:
	add2panelize_cmd ();
	break;

    case B_REMOVE:
	remove_from_panelize (l_panelize->current->data);
	break;

    case B_ENTER:
	target = pname->buffer;
	if (target != NULL && *target) {
	    char *cmd = strdup (target);
	    destroy_dlg (panelize_dlg);
	    do_external_panelize (cmd);
	    free (cmd);
	    repaint_screen ();
	    return;
	}
	break;
    }

    panelize_done ();
}

void load_panelize (void)
{
    void *profile_keys;
    char *key, *value;
    
    profile_keys = profile_init_iterator (panelize_section, profile_name);
    
    add2panelize (strdup ("Other command"), strdup (""));

    if (!profile_keys){
	add2panelize (strdup ("Find rejects after patching"), strdup ("find . -name \\*.rej -print"));
	add2panelize (strdup ("Find *.orig after patching"), strdup ("find . -name \\*.orig -print"));
	add2panelize (strdup ("Find SUID and SGID programs"), strdup ("find . \\( \\( -perm -04000 -a -perm +011 \\) -o \\( -perm -02000 -a -perm +01 \\) \\) -print"));
	return;
    }
    
    while (profile_keys){
	profile_keys = profile_iterator_next (profile_keys, &key, &value);
	add2panelize (strdup (key), strdup (value));
    }
}

void save_panelize (void)
{
    struct panelize *current = panelize;
    
    profile_clean_section (panelize_section, profile_name);
    for (;current; current = current->next){
    	if (strcmp (current->label, "Other command"))
	    WritePrivateProfileString (panelize_section,
				       current->label,
				       current->command,
				       profile_name);
    }
    sync_profiles ();
}

void done_panelize (void)
{
    struct panelize *current = panelize;
    struct panelize *next;

    for (; current; current = next){
	next = current->next;
	free (current->label);
	free (current->command);
	free (current);
    }
}

struct spaced_dirent {
    struct dirent dp;
#ifdef NEED_EXTRA_DIRENT_BUFFER
    char extra_buffer [MC_MAXPATHLEN];
#endif
};

void do_external_panelize (char *command)
{
    struct spaced_dirent dp;
    int status, link_to_dir, stalled_link;
    int next_free = 0;
    struct stat buf;
    dir_list *list = &cpanel->dir;
    char line [MC_MAXPATHLEN];
    FILE *external;

    open_error_pipe ();
    external = popen (command, "r");
    if (!external){
	close_error_pipe (1, "Cannot invoke command.");
	return;
    }
    clean_dir (list, cpanel->count);

    /* Clear the counters */
    cpanel->total = cpanel->dirs_marked = cpanel->marked = 0;
    while (1) {
	clearerr(external);
	if (fgets (line, MC_MAXPATHLEN, external) == NULL)
	    if (ferror(external) && errno == EINTR)
		continue;
	    else
		break;
	if (line[strlen(line)-1] == '\n')
	    line[strlen(line)-1] = 0;
	if (strlen(line) < 1)
	    continue;
	if (line [0] == '.' && line[1] == PATH_SEP)
	    strcpy (dp.dp.d_name, line + 2);
	else
	    strcpy (dp.dp.d_name, line);
#if !defined(HAVE_DIRENT_H) && !defined(_POSIX_VERSION)
	NLENGTH (&dp.dp) = strlen (dp.dp.d_name);
#endif
	status = handle_dirent (list, NULL, &dp.dp, &buf, next_free, &link_to_dir,
	    &stalled_link);
	if (status == 0)
	    continue;
	if (status == -1)
	    break;
	list->list [next_free].fnamelen = NLENGTH (&(dp.dp));
	list->list [next_free].fname = strdup (dp.dp.d_name);
	list->list [next_free].cache = NULL;
	file_mark (cpanel, next_free, 0);
	list->list [next_free].f.link_to_dir = link_to_dir;
	list->list [next_free].f.stalled_link = stalled_link;
	list->list [next_free].buf = buf;
	next_free++;
	if (!(next_free & 32))
	    rotate_dash ();
    }
    if (next_free){
	cpanel->count = next_free;
	cpanel->is_panelized = 1;
	if (list->list [0].fname [0] == PATH_SEP){
	    strcpy (cpanel->cwd, PATH_SEP_STR);
	    chdir (PATH_SEP_STR);
	}
    } else {
	cpanel->count = set_zero_dir (list);
    }
    if (pclose (external) < 0)
	message (0, "External panelize", "Pipe close failed");
    close_error_pipe (0, 0);
    try_to_select (cpanel, NULL);
    paint_panel (cpanel);
}
