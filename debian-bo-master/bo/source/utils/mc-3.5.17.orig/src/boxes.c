/* Some misc dialog boxes for the program.
   
   Copyright (C) 1994, 1995 the Free Software Foundation
   
   Authors: 1994, 1995 Miguel de Icaza
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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <config.h>
#include "tty.h"
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <malloc.h>
#include "global.h"
#include "mad.h"		/* The great mad */
#include "util.h"		/* Required by panel.h */
#include "win.h"		/* Our window tools */
#include "color.h"		/* Color definitions */
#include "dlg.h"		/* The nice dialog manager */
#include "widget.h"		/* The widgets for the nice dialog manager */
#include "dialog.h"		/* For do_refresh() */
#include "wtools.h"
#include "setup.h"		/* For profile_name */
#include "profile.h"		/* Load/save user formats */
#include "key.h"		/* XCTRL and ALT macros  */
#include "command.h"		/* For cmdline */
#include "dir.h"
#include "panel.h"
#include "boxes.h"
#include "main.h"		/* For the confirm_* variables */
#include "tree.h"
#include "layout.h"		/* for get_nth_panel_name proto */

#define DISPLAY_X  48
#define DISPLAY_Y  15

static Dlg_head *dd;
static WInput *user;
static WInput *status;
static WCheck *check_status;
static int current_mode;
extern int ftpfs_always_use_proxy;

#define VIEW_TYPES 4

static char *displays [VIEW_TYPES] = {
    "Full file list", "Brief file list", "Long file list",
    "User:"
};

static char *formats_section = "Formats";
static WRadio *my_radio;

static char *select_format (WInput *i)
{
    void     *profile_keys;
    char     *key;
    char     *value;
    int      in_list;
    Chooser  *my_user_list;
    WListbox *p;

    my_user_list = new_chooser (DISPLAY_Y - 4, DISPLAY_X - 4, "Listing Mode...",
				CHOOSE_EDITABLE);
    p = my_user_list->listbox;
    p->allow_duplicates = 0;
    
    in_list = 0;
    profile_keys = profile_init_iterator (formats_section, profile_name);
    while (profile_keys){
	profile_keys = profile_iterator_next (profile_keys, &key, &value);
	listbox_add_item (p, 0, 0, value, 0);
    }
    listbox_add_item (my_user_list->listbox, 0, 0, i->buffer, 0);

    value = 0;
    if (run_chooser (my_user_list) != B_CANCEL){
	char     key [2];
	int      count;
	WLEntry  *e;

	key [1] = 0;
	profile_clean_section (formats_section, profile_name);
	e = p->top;
	
	for (count = 0 ; count < p->count; count++, e = e->next){
	    key [0] = 'A' + count - 1;
	    WritePrivateProfileString (formats_section, key, e->text,
				       profile_name);
	}
	value = strdup (p->current->text);
    }
    
    destroy_chooser (my_user_list);
    return value;
}

static int display_callback (struct Dlg_head *h, int id, int Msg)
{
#ifndef HAVE_X
    char *text;
    WInput *input;
    
    switch (Msg){
    case DLG_DRAW:
	attrset (REVERSE_COLOR);
	dlg_erase (h);
	draw_box (h, 1, 1, DISPLAY_Y-2, DISPLAY_X-2);

	attrset (COLOR_HOT_NORMAL);
	dlg_move (h, 1, 2);
	addstr (" Listing mode ");
	attrset (COLOR_NORMAL);
	dlg_move (h, 11, 3);
	addstr ("On input lines, use C-v to get a listbox");
	dlg_move (h, 12, 3);
	addstr ("with other formats");
	break;
	
    case DLG_KEY:
	if (id == '\n'){
	    if ((WInput *) h->current->widget == user){
		h->running = 0;
		h->ret_value = B_USER + 6;
		break;
	    }
	
	    if ((WInput *) h->current->widget == status){
		h->running = 0;
		h->ret_value = B_USER + 7;
		break;
	    }
	}

	/* Handle the above C-v */
	if (id == XCTRL('v') &&
	    ((WInput *) h->current->widget == user ||
	     (WInput *) h->current->widget == status)){
	    
	    input = (WInput *) h->current->widget;
	    
	    text = select_format (input);
	    if (text){
		assign_text (input, text);
		update_input (input);
	    }
	    return MSG_HANDLED;
	}
	
	if ((id|0x20) == 'u' && h->current->widget != (Widget *) user
	    && h->current->widget != (Widget *) status){
	    my_radio->sel = 3;
	    dlg_select_widget (h, my_radio); /* force redraw */
    dlg_select_widget (h, user);
	    return MSG_HANDLED;
	}
    }
#endif    
    return MSG_NOT_HANDLED;
}

static void display_init (int radio_sel, char *init_text,
			  int _check_status, char *_status)
{
    dd = create_dlg (0, 0, DISPLAY_Y, DISPLAY_X, dialog_colors,
		     display_callback, "[Left and Right Menus]", "display",
		     DLG_CENTER | DLG_GRID);

    x_set_dialog_title (dd, "Listing mode");
    add_widgetl (dd,
        button_new (3, 32, B_CANCEL, "[ Cancel ]", 'c', 2, 0, 0, "cancel-button"),
	XV_WLAY_RIGHTOF);

    add_widgetl (dd,
	button_new (5, 32, B_ENTER,  "[[ Ok ]]", 'o', 3, 0, 0, "ok-button"),
	 XV_WLAY_CENTERROW);

    status = input_new (9, 8, NORMAL_COLOR, 34, _status, "mini-input");
    add_widgetl (dd, status, XV_WLAY_RIGHTDOWN);
    input_set_point (status, 0);

    check_status = check_new (8, 4, _check_status, "user Mini status",'m', 5, "mini-status");
    add_widgetl (dd, check_status, XV_WLAY_NEXTROW);
    
    user = input_new  (6, 14, NORMAL_COLOR, 29, init_text, "user-fmt-input");
    add_widgetl (dd, user, XV_WLAY_RIGHTDOWN);
    input_set_point (user, 0);

    my_radio = radio_new (3, 4, VIEW_TYPES, displays, 1, "radio");
    my_radio->sel = my_radio->pos = current_mode;
    add_widgetl (dd, my_radio, XV_WLAY_BELOWCLOSE);
}

int display_box (WPanel *panel, char **userp, char **minip, int *use_msformat,
    int num)
{
    int result;
    char *section = NULL;
    char *p;

    if (!panel) {
        p = get_nth_panel_name (num);
        panel = (WPanel *) xmalloc (sizeof (WPanel), "temporary panel");
        panel->list_type = list_full;
        panel->user_format = strdup (DEFAULT_USER_FORMAT);
        panel->user_mini_status = 0;
        panel->mini_status_format = strdup (DEFAULT_USER_FORMAT);
        section = copy_strings ("Temporal:", p, 0);
        if (!profile_has_section (section, profile_name)) {
            free (section);
            section = strdup (p);
        }
        panel_load_setup (panel, section);
        free (section);
    }

    current_mode = panel->list_type;
    display_init (panel->list_type, panel->user_format,
		  panel->user_mini_status, panel->mini_status_format);
		  
    run_dlg (dd);

    result = -1;
    
    if (section) {
        free (panel->user_format);
        free (panel->mini_status_format);
        free (panel);
    }
    
    if (dd->ret_value != B_CANCEL){
	result = my_radio->sel;
	*userp = strdup (user->buffer);
	*minip = strdup (status->buffer);
	*use_msformat = check_status->state & C_BOOL;
    }
    destroy_dlg (dd);

    return result;
}

#define SORT_X 40
#define SORT_Y 14

char *sort_orders_names [SORT_TYPES];

sortfn *sort_box (sortfn *sort_fn, int *reverse, int *case_sensitive)
{
    int i, r;
    sortfn *result;
    WCheck *c, *case_sense;

    result = 0;
    
    for (i = 0; i < SORT_TYPES; i++)
	if ((sortfn *) (sort_orders [i].sort_fn) == sort_fn){
	    current_mode = i;
	    break;
	}
    
    dd = create_dlg (0, 0, SORT_Y, SORT_X, dialog_colors, common_dialog_callback,
		     "[Left and Right Menus]", "sort", DLG_CENTER | DLG_GRID);
		     
    x_set_dialog_title (dd, "Sort order");

    add_widgetl (dd, button_new (6, 25, B_CANCEL, "[ Cancel ]", 'c', 2, 0, 0, "cancel-button"),
        XV_WLAY_CENTERROW);

    add_widgetl (dd, button_new (8, 25, B_ENTER, "[ Ok ]", 'o', 2, 0, 0, "ok-button"),
	XV_WLAY_RIGHTDOWN);

    case_sense = check_new (4, 19, *case_sensitive, "case sensitive", 't', 10, "case-check");
    add_widgetl (dd, case_sense, XV_WLAY_RIGHTDOWN);
    c = check_new (3, 19, *reverse, "Reverse", 'r', 0, "reverse-check");
    add_widgetl (dd, c, XV_WLAY_RIGHTDOWN);

    for (i = SORT_TYPES-1; i >= 0; i--){
	sort_orders_names [i] = sort_orders [i].sort_name;
    }
    my_radio = radio_new (3, 3, SORT_TYPES, sort_orders_names, 1, "radio-1");
    my_radio->sel = my_radio->pos = current_mode;
    
    add_widget (dd, my_radio);
    run_dlg (dd);

    r = dd->ret_value;
    if (r != B_CANCEL){
	result = (sortfn *) sort_orders [my_radio->sel].sort_fn;
	*reverse = c->state & C_BOOL;
	*case_sensitive = case_sense->state & C_BOOL;
    } else
	result = sort_fn;
    destroy_dlg (dd);

    return result;
}

#define CONFY 10
#define CONFX 46

static int my_delete;
static int my_overwrite;
static int my_exit;

static QuickWidget conf_widgets [] = {
{ quick_button,   4, 6, 4, CONFY, " [ Cancel ]",
      'c', 3, B_CANCEL, 0, 0, XV_WLAY_RIGHTOF, "c" },
{ quick_button,   4, 6, 3, CONFY, " [ oK ] ",
      'k', 4, B_ENTER, 0, 0, XV_WLAY_CENTERROW, "o" },

{ quick_checkbox, 1, 13, 5, CONFY, " confirm Exit ",
      'e', 9, 0, &my_exit, 0, XV_WLAY_BELOWCLOSE, "e" },
{ quick_checkbox, 1, 13, 4, CONFY, " confirm Overwrite ",
      'o', 9, 0, &my_overwrite, 0, XV_WLAY_BELOWCLOSE, "ov" },
{ quick_checkbox, 1, 13, 3, CONFY, " confirm Delete ",
      'd', 9, 0, &my_delete, 0, XV_WLAY_BELOWCLOSE, "de" },
{ 0,              0, 0, 0, 0, 0, 0, 0, 0, 0, XV_WLAY_DONTCARE }
};

static QuickDialog confirmation =
{ CONFX, CONFY, -1, -1, " Confirmation ", "[Confirmation]", "quick_confirm",
      conf_widgets };

void confirm_box ()
{
    my_delete    = confirm_delete;
    my_overwrite = confirm_overwrite;
    my_exit      = confirm_exit;
    if (quick_dialog (&confirmation) != B_CANCEL){
	confirm_delete    = my_delete;
	confirm_overwrite = my_overwrite;
	confirm_exit      = my_exit;
    }
}

#define DISPY 9
#define DISPX 46

static int new_mode;

char *display_bits_str [] =
{ "Full 8 bits", "ISO 8859-1", "7 bits" };

static QuickWidget display_widgets [] = {
{ quick_button,   4,  6,    3, DISPY, " [ Ok ]",
      'o', 3, B_ENTER, 0, 0, XV_WLAY_CENTERROW, "o" },
{ quick_button,   4,  6,    4, DISPY, " [ Cancel ]",
      'c', 3, B_CANCEL, 0, 0, XV_WLAY_CENTERROW, "c" },
{ quick_radio,    4, DISPX, 3, DISPY, "", 3, 0, 0,
      &new_mode, display_bits_str, XV_WLAY_BELOWCLOSE, "r" },
{ 0,              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, XV_WLAY_DONTCARE }
};

static QuickDialog display_bits =
{ DISPX, DISPY, -1, -1, " Display bits ", "[Display bits]",
  "dbits", display_widgets };

void display_bits_box ()
{
    int v;
    int current_mode;

    if (full_eight_bits)
	current_mode = 0;
    else if (eight_bit_clean)
	current_mode = 1;
    else
	current_mode = 2;
    
    display_widgets [2].value = current_mode;
    v = quick_dialog (&display_bits);
    if (v != B_ENTER)
	return;

    eight_bit_clean = new_mode < 2;
    full_eight_bits = new_mode == 0;
#ifndef HAVE_SLANG
    meta (stdscr, eight_bit_clean);
#else
    SLsmg_Display_Eight_Bit = full_eight_bits ? 128 : 160;
#endif
}

#define TREE_Y 20
#define TREE_X 60

static int tree_colors [4];

static int tree_callback (struct Dlg_head *h, int id, int msg)
{
    switch (msg){

    case DLG_POST_KEY:
	/* The enter key will be processed by the tree widget */
	if (id == '\n' || ((WTree *)(h->current->widget))->done){
	    h->running = 0;
	    h->ret_value = B_ENTER;
	}
	return MSG_HANDLED;
	
    case DLG_DRAW:
	common_dialog_repaint (h);
	break;
    }
    return MSG_NOT_HANDLED;
}

char *tree (char *current_dir)
{
    WTree    *mytree;
    Dlg_head *dlg;
    char     *val;

    tree_colors [3] = dialog_colors [0];
    tree_colors [1] = dialog_colors [1];
    
    /* Create the components */
    dlg = create_dlg (0, 0, TREE_Y, TREE_X, tree_colors,
		      tree_callback, "[Directory Tree]", "tree", DLG_CENTER);
    mytree = tree_new (0, 2, 2, TREE_Y - 6, TREE_X - 5);
    add_widget (dlg, mytree);

    run_dlg (dlg);
    if (dlg->ret_value == B_ENTER)
	val = strdup (mytree->selected_ptr->name);
    else
	val = 0;
    
    destroy_dlg (dlg);
    return val;
}
#ifndef USE_VFS
#ifdef USE_NETCODE
#undef USE_NETCODE
#endif
#endif

#ifdef USE_VFS

#if defined(USE_NETCODE)
#define VFSY 15
#else
#define VFSY 11
#endif

#define VFSX 53

extern int vfs_timeout;
extern int tar_gzipped_memlimit;
extern int ftpfs_always_use_proxy;

#if defined(USE_NETCODE)
extern char *ftpfs_anonymous_passwd;
extern char *ftpfs_proxy_host;
extern ftpfs_directory_timeout;
extern int use_netrc;
#endif

int vfs_use_limit = 1;
static char *ret_timeout;
static char *ret_limit;

#if defined(USE_NETCODE)
static char *ret_passwd;
static char *ret_directory_timeout;
static char *ret_ftp_proxy;
static int ret_use_netrc;
#endif

#if 0
/* Not used currently */
{ quick_checkbox,  4, VFSX, 10, VFSY, "Use ~/.netrc",
      'U', 0, 0, &ret_use_netrc, 0, XV_WLAY_BELOWCLOSE, "" },
#endif

char *confvfs_str [] =
{ "Always to memory", "If size less than:" };

static QuickWidget confvfs_widgets [] = {
{ quick_button,   VFSX - 10 - 4,  VFSX,    VFSY - 3, VFSY, "[ Cancel ]",
      'c', 2, B_CANCEL, 0, 0, XV_WLAY_RIGHTOF, "button-cancel" },
{ quick_button,   4, VFSX,    VFSY - 3, VFSY, "[ Ok ]",
      'o', 4, B_ENTER, 0, 0, XV_WLAY_CENTERROW, "button-ok" },
#if defined(USE_NETCODE)
{ quick_input,    30, VFSX, 10, VFSY, 0, 0, 10, 0, 0, &ret_ftp_proxy,
      XV_WLAY_RIGHTDOWN, "input-ftp-proxy" },
{ quick_checkbox,    4, VFSX, 10, VFSY, "always use ftp proxy", 0, 0, 0,
      &ftpfs_always_use_proxy, 0, XV_WLAY_RIGHTDOWN, "check-ftp-proxy" },
{ quick_label,    46, VFSX, 9, VFSY, "sec",
      0, 0, 0, 0, 0, XV_WLAY_RIGHTOF, "label-sec" },
{ quick_input,    35, VFSX, 9, VFSY, 0, 0, 10, 0, 0, &ret_directory_timeout,
      XV_WLAY_RIGHTDOWN, "input-timeout" },
{ quick_label,     4, VFSX, 9, VFSY, "ftpfs directory cache timeout:",
      0, 0, 0, 0, 0, XV_WLAY_NEXTROW, "label-cache"},
{ quick_input,    28, VFSX, 8, VFSY, 0, 0, 21, 0, 0, &ret_passwd,
      XV_WLAY_RIGHTDOWN, "input-passwd" },
{ quick_label,     4, VFSX, 8, VFSY, "ftp anonymous password:",
      0, 0, 0, 0, 0, XV_WLAY_NEXTROW, "label-pass"},
#endif
{ quick_input,    26, VFSX, 6, VFSY, 0, 0, 10, 0, 0, &ret_limit, 
      XV_WLAY_RIGHTDOWN, "input-limit" },
{ quick_radio,    4, VFSX, 5, VFSY, "", 2, 0, 0,
      &vfs_use_limit, confvfs_str, XV_WLAY_BELOWCLOSE, "radio" },
{ quick_label,    4,  VFSX, 4, VFSY, "Gzipped tar archive extract:", 
      0, 0, 0, 0, 0, XV_WLAY_NEXTROW, "label-tar" },
{ quick_label,    46, VFSX, 3, VFSY, "sec",
      0, 0, 0, 0, 0, XV_WLAY_RIGHTOF, "label-sec2" },
{ quick_input,    35, VFSX, 3, VFSY, 0, 0, 10, 0, 0, &ret_timeout, 
      XV_WLAY_RIGHTOF, "input-timo-vfs" },
{ quick_label,    4,  VFSX, 3, VFSY, "Timeout for freeing VFSs:", 
      0, 0, 0, 0, 0, XV_WLAY_BELOWCLOSE, "label-vfs" },
{ 0,              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, XV_WLAY_DONTCARE, 0 }
};

static QuickDialog confvfs_dlg =
{ VFSX, VFSY, -1, -1, " Virtual File System Setting ", "[Virtual FS]", "quick_vfs", confvfs_widgets };

#if defined(USE_NETCODE)
#define VFS_WIDGETBASE 7
#else
#define VFS_WIDGETBASE 0
#endif

void configure_vfs ()
{
    char buffer1 [15], buffer2 [15];
#if defined(USE_NETCODE)
    char buffer3[15];
#endif

    if (tar_gzipped_memlimit > -1) {
        if (tar_gzipped_memlimit == 0)
            strcpy (buffer1, "0 B");
	else if ((tar_gzipped_memlimit % (1024*1024)) == 0) /* I.e. in M */
	    sprintf (buffer1, "%i MB", (int)(((unsigned)tar_gzipped_memlimit) >> 20));
	else if ((tar_gzipped_memlimit % 1024) == 0) /* I.e. in K */
	    sprintf (buffer1, "%i KB", (int)(((unsigned)tar_gzipped_memlimit) >> 10));
	else if ((tar_gzipped_memlimit % 1000) == 0)
	    sprintf (buffer1, "%i kB", (int)(tar_gzipped_memlimit / 1000));
	else
	    sprintf (buffer1, "%i B", (int)tar_gzipped_memlimit);
        confvfs_widgets [2 + VFS_WIDGETBASE].text = buffer1;
    } else
    	confvfs_widgets [2 + VFS_WIDGETBASE].text = "5 MB";
    sprintf (buffer2, "%i", vfs_timeout);
    confvfs_widgets [6 + VFS_WIDGETBASE].text = buffer2;
    confvfs_widgets [3 + VFS_WIDGETBASE].value = vfs_use_limit;
#if defined(USE_NETCODE)
    ret_use_netrc = use_netrc;
    sprintf(buffer3, "%i", ftpfs_directory_timeout);
    confvfs_widgets[5].text = buffer3;
    confvfs_widgets[7].text = ftpfs_anonymous_passwd;
    confvfs_widgets[2].text = ftpfs_proxy_host ? ftpfs_proxy_host : "";
#endif

    if (quick_dialog (&confvfs_dlg) != B_CANCEL) {
        char *p;
        
        vfs_timeout = atoi (ret_timeout);
        free (ret_timeout);
        if (vfs_timeout < 0 || vfs_timeout > 10000)
            vfs_timeout = 10;
        if (!vfs_use_limit)
            tar_gzipped_memlimit = -1;
        else {
            tar_gzipped_memlimit = atoi (ret_limit);
            if (tar_gzipped_memlimit < 0)
                tar_gzipped_memlimit = -1;
            else {
                for (p = ret_limit; *p == ' ' || (*p >= '0' && *p <= '9'); p++);
                switch (*p) {
		case 'm':
		case 'M': tar_gzipped_memlimit <<= 20; break;
		case 'K': tar_gzipped_memlimit <<= 10; break;
		case 'k': tar_gzipped_memlimit *= 1000; break;
                }
            }
        }
        free (ret_limit);
#if defined(USE_NETCODE)
	free(ftpfs_anonymous_passwd);
	ftpfs_anonymous_passwd = ret_passwd;
	if (ftpfs_proxy_host)
	    free(ftpfs_proxy_host);
	ftpfs_proxy_host = ret_ftp_proxy;
	ftpfs_directory_timeout = atoi(ret_directory_timeout);
	use_netrc = ret_use_netrc;
	free(ret_directory_timeout);
#endif
    }
}

#endif

char *cd_dialog (void)
{
    QuickDialog Quick_input;
    QuickWidget quick_widgets [] = {
#ifdef HAVE_TK
#define INPUT_INDEX 2
    { quick_button, 0, 1, 0, 1, "Cancel", 'c', 0, B_CANCEL, 0, 0,
	  XV_WLAY_DONTCARE, "cancel" },
    { quick_button, 0, 1, 0, 1, "Ok", 'o', 0, B_ENTER, 0, 0,
	  XV_WLAY_DONTCARE, "ok" },
#else
#define INPUT_INDEX 0
#endif
    { quick_input,  8, 80, 5, 0, 0, 0, 50, 0, 0, 0, XV_WLAY_RIGHTOF, "input" },
    { quick_label,  3, 80, 2, 0, 0, 0, 0, 0, 0, 0, XV_WLAY_DONTCARE, "label" },
    { 0 } };
    
    char *my_str;
    
    Quick_input.xlen  = 57;
    Quick_input.title = "Quick cd";
    Quick_input.help  = "[Quick cd]";
    Quick_input.class = "quick_input";
    quick_widgets [INPUT_INDEX].text = "";
    quick_widgets [INPUT_INDEX].value = 2; /* want cd like completion */
    quick_widgets [INPUT_INDEX+1].text = "cd";
    quick_widgets [INPUT_INDEX+1].y_divisions =
	quick_widgets [INPUT_INDEX].y_divisions =
	        Quick_input.ylen  = 5;
    Quick_input.xpos = 2;
    Quick_input.ypos = ((Widget *) cmdline)->y - Quick_input.ylen;
    quick_widgets [INPUT_INDEX].relative_y = 2;
    quick_widgets [INPUT_INDEX].str_result = &my_str;
    
    Quick_input.widgets = quick_widgets;
    if (quick_dialog (&Quick_input) != B_CANCEL){
	return *(quick_widgets [INPUT_INDEX].str_result);
    } else
	return 0;
}

void symlink_dialog (char *existing, char *new, char **ret_existing, 
    char **ret_new)
{
    QuickDialog Quick_input;
    QuickWidget quick_widgets [] = {
#ifdef HAVE_TK
#define INPUT_INDEX 2
    { quick_button, 0, 1, 0, 1, "Cancel", 'c', 0, B_CANCEL, 0, 0,
	  XV_WLAY_DONTCARE, "cancel" },
    { quick_button, 0, 1, 0, 1, "Ok", 'o', 0, B_ENTER, 0, 0,
	  XV_WLAY_DONTCARE, "ok" },
#else
#define INPUT_INDEX 0
#endif
    { quick_input,  4, 80, 5, 8, 0, 0, 58, 0, 0, 0, XV_WLAY_BELOWCLOSE, "input-1" },
    { quick_label,  4, 80, 4, 8, 0, 0, 0, 0, 0, 0, XV_WLAY_BELOWOF, "label-1" },
    { quick_input,  4, 80, 3, 8, 0, 0, 58, 0, 0, 0, XV_WLAY_BELOWCLOSE, "input-2" },
    { quick_label,  4, 80, 2, 8, 0, 0, 0, 0, 0, 0, XV_WLAY_DONTCARE, "label-2" },
    { 0 } };
    
    Quick_input.xlen  = 64;
    Quick_input.ylen  = 8;
    Quick_input.title = "Symbolic link";
    Quick_input.help  = "[File Menu]";
    Quick_input.class = "quick_symlink";
    quick_widgets [INPUT_INDEX].text = new;
    quick_widgets [INPUT_INDEX+1].text = "Symbolic link filename:";
    quick_widgets [INPUT_INDEX+2].text = existing;
    quick_widgets [INPUT_INDEX+3].text = "Existing filename (filename symlink will point to):";
    Quick_input.xpos = -1;
    quick_widgets [INPUT_INDEX].str_result = ret_new;
    quick_widgets [INPUT_INDEX+2].str_result = ret_existing;
    
    Quick_input.widgets = quick_widgets;
    if (quick_dialog (&Quick_input) == B_CANCEL){
        *ret_new = NULL;
        *ret_existing = NULL;
    }
}

