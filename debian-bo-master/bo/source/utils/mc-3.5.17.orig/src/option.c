/* Configure box module for the Midnight Commander
   Copyright (C) 1994 Radek Doulik

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
#include <string.h>
#include <stdio.h>
/* Needed for the extern declarations of integer parameters */
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#include "tty.h"
#include "mad.h"
#include "util.h"
#include "win.h"
#include "color.h"
#include "dlg.h"
#include "widget.h"
#include "setup.h"		/* For save_setup() */
#include "dialog.h"		/* For do_refresh() */
#include "main.h"
#include "profile.h"		/* For sync_profiles */

#include "dir.h"
#include "panel.h"		/* Needed for the externs */
#include "file.h"
#include "layout.h"		/* For nice_rotating_dash */
#define PX	5
#define PY	2

#define RX      5
#define RY      11

#define CX	5
#define CY	2

#define BX	15
#define BY	16

#define OX	34
#define OY	2

#define TX	36
#define TY	11


static Dlg_head *conf_dlg;

static int r_but;

#define TOGGLE_VARIABLE 0

extern int use_internal_edit;

static struct {
    char   *text;
    int    *variable;
    void   (*toggle_function)(void);
    WCheck *widget;
    int    hkey;
    int    hpos;
    char   *tk;
} check_options [] = {
   {"safe deLete",       &know_what_am_i_doing, TOGGLE_VARIABLE,    0,'l', 7, "safe-del" },
   {"cd follows linKs",  &cd_symlinks,       TOGGLE_VARIABLE,       0,'k',14, "cd-follow" },
   {"advanced choWn",    &advanced_chfns,    TOGGLE_VARIABLE,       0,'w',12, "achown" },
   {"lYnx-like motion",  &navigate_with_arrows,TOGGLE_VARIABLE,     0,'y', 1, "lynx" },
   {"Rotating dash",     &nice_rotating_dash,TOGGLE_VARIABLE,       0,'r', 0, "rotating" },
   {"Complete: show all",&show_all_if_ambiguous,TOGGLE_VARIABLE,    0,'c', 0, "completion" },
   {"Use internal view", &use_internal_view, TOGGLE_VARIABLE,       0,'u', 0, "view-int" },
   {"use internal edIt", &use_internal_edit, TOGGLE_VARIABLE,       0,'i',15, "edit-int" },
   {"auto mEnus",        &auto_menu,         TOGGLE_VARIABLE,       0,'e', 6, "auto-menus" },
   {"Auto save setup",   &auto_save_setup,   TOGGLE_VARIABLE,       0,'a', 0, "auto-save" },
   {"shell Patterns",    &easy_patterns,     TOGGLE_VARIABLE,       0,'p', 6, "shell-patt" },
   {"Verbose operation", &verbose,           TOGGLE_VARIABLE,       0,'v', 0, "verbose" },
   {"Fast dir reload",   &fast_reload,       toggle_fast_reload,    0,'f', 0, "fast-reload" },
   {"miX all files",     &mix_all_files,     toggle_mix_all_files,  0,'x', 2, "mix-files" },
   {"Drop down menus",   &drop_menus,        TOGGLE_VARIABLE,       0,'d', 0, "drop-menus" },
   {"Mark moves down",   &mark_moves_down,   TOGGLE_VARIABLE,       0,'m', 0, "mark-moves" },
   {"show Hidden files", &show_dot_files,    toggle_show_hidden,    0,'h', 5, "show-hidden" },
   {"show Backup files", &show_backups,      toggle_show_backup,    0,'b', 5, "show-backup" },
   { 0, 0, 0, 0 }
};

static WRadio *pause_radio;

static char *pause_options [3] = {
    "Never",
    "on dumb Terminals",
    "alwaYs" };

static int configure_callback (struct Dlg_head *h, int Id, int Msg)
{
    switch (Msg) {
    case DLG_DRAW:
	attrset (REVERSE_COLOR);
	dlg_erase (h);
	
	draw_box (h, 1, 2, 16, 62);
	draw_box (h, PY, PX, 8, 27);
	draw_box (h, OY, OX, 14, 27);
	draw_box (h, RY, RX, 5, 27);
	break;

    case DLG_END:
	r_but = Id;
	break;
    }
    return 0;
}

static void init_configure (void)
{
    int i;

    conf_dlg = create_dlg (0, 0, 19, 66, dialog_colors,
			   configure_callback, "[Options Menu]",
			   "option", DLG_CENTER | DLG_GRID);
    x_set_dialog_title (conf_dlg, "Configure options");

    add_widgetl (conf_dlg,
	button_new (BY, BX+26, B_CANCEL, "[ Cancel ]",'c',2, 0, 0, "button-cancel"),
	XV_WLAY_RIGHTOF);

    add_widgetl (conf_dlg,
	button_new (BY, BX+12, B_EXIT,   "[ Save ]",'s',2, 0, 0, "button-save"),
	XV_WLAY_RIGHTOF);
    
    add_widgetl (conf_dlg,
        button_new (BY, BX,    B_ENTER,  "[[ Ok ]]",'o',3, 0, 0, "button-ok"),
        XV_WLAY_CENTERROW);

#define XTRACT(i) *check_options[i].variable, check_options[i].text, check_options[i].hkey, check_options[i].hpos, check_options [i].tk

    /* Add all the checkboxes */
    for (i = 0; i < 12; i++){
	check_options [i].widget = check_new (OY + (12-i), OX+2, XTRACT(i));
	add_widgetl (conf_dlg, check_options [i].widget,
	    XV_WLAY_BELOWCLOSE);
    }

    add_widgetl (conf_dlg, label_new (OY, OX + 1, "Other options", "label-other"),
        XV_WLAY_NEXTCOLUMN);

    pause_radio = radio_new (RY+1, RX+1, 3, pause_options, 1, "pause-radio");
    pause_radio->sel = pause_after_run;
    add_widgetl (conf_dlg, pause_radio, XV_WLAY_BELOWCLOSE);

    add_widgetl (conf_dlg, label_new (RY, RX + 1, "Pause after run...", "label-pause"),
        XV_WLAY_BELOWOF);

    for (i = 0; i < 6; i++){
	check_options [i+12].widget = check_new (PY + (6-i), PX+2,
						  XTRACT(i+12));
	add_widgetl (conf_dlg, check_options [i+12].widget,
	    XV_WLAY_BELOWCLOSE);
    }
    add_widgetl (conf_dlg, label_new (PY, PX + 1, "Panel options", "label-panel"),
        XV_WLAY_NEXTCOLUMN);
}


void configure_box (void)
{
    int result, i;
    
    init_configure ();
    run_dlg (conf_dlg);

    result = conf_dlg->ret_value;
    if (result == B_ENTER || result == B_EXIT){
	for (i = 0; check_options [i].text; i++)
	    if (check_options [i].widget->state & C_CHANGE){
		if (check_options [i].toggle_function)
		    (*check_options [i].toggle_function)();
		else
		    *check_options [i].variable =
			!(*check_options [i].variable);
	    }
	pause_after_run = pause_radio->sel;
    }

    /* If they pressed the save button */
    if (result == B_EXIT){
	save_configure ();
	sync_profiles ();
    }

    destroy_dlg (conf_dlg);
}
