/* editor menu definitions and initialisation

   Copyright (C) 1996 the Free Software Foundation
   
   Authors: 1996 Paul Sheer

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#include <config.h>
#include "edit.h"

#include "editcmddef.h"

#ifdef MIDNIGHT
#include "src/mad.h"

extern WEdit *wedit;
extern WButtonBar *edit_bar;
extern Dlg_head *edit_dlg;
extern WMenu *edit_menubar;

#define Cmessagedialogue(w,x,y,h,s) query_dialog (h, s, 0, 1, " Ok ")
#define CFocus(x)  

static void menu_cmd(int i)
{
    send_message (wedit->widget.parent, (Widget *) wedit, WIDGET_COMMAND, i);
}

static void menu_key(int i)
{
    send_message (wedit->widget.parent, (Widget *) wedit, WIDGET_KEY, i);
}

#else

extern CWidget *wedit;

void CSetEditMenu(const char *ident)
{
    wedit = Cwidget(ident);
}

static void menu_cmd (int i)
{
    XEvent e;
    if (wedit) {
	memset (&e, 0, sizeof (XEvent));
	e.type = EditorCommand;
	e.xkey.keycode = i;
	e.xkey.window = wedit->winid;
	CFocus (wedit);
	CSendEvent (&e);
    }
}



static void menu_key (KeySym i, int state)
{
    int cmd, ch;
    if (edit_translate_key (wedit->editor, i, state, &cmd, &ch)) {
	if (cmd > 0)
	    menu_cmd (cmd);
    }
}



#endif


void edit_wrap_cmd()
{
    Cmessagedialogue (CMain, 20, 20, " Word wrap ", " Not yet supported ");
    CFocus (wedit);
    return;  /* TODO */
}

void edit_layout_cmd()
{
    Cmessagedialogue (CMain, 20, 20, " Layout ", " Not yet supported ");
    CFocus (wedit);
    return;  /* TODO */
}

void edit_about_cmd ()
{
#ifdef MIDNIGHT
    Cmessagedialogue (CMain, 20, 20, " About ",
		  "\n"
		  "                 Cooledit  v2.1\n"
		  "\n"
		  " Copyright (C) 1996 the Free Software Foundation\n"
		  "\n"
		  "       A user friendly text editor written\n"
		  "           for the Midnight Commander.\n"
	);
#else
    Cmessagedialogue (CMain, 20, 20, " About ",
      "\n"
      "                        Cooledit  version  " VERSION "\n"
      "\n"
      "                     A user friendly text editor\n"
      "                        written for XWindows.\n"
      "\n"
      "                   Copyright (C) 1996  Paul Sheer\n"
      "\n"
      "        Cooledit comes with ABSOLUTELY NO WARRANTY; for details\n"
      "               click on 'no Warranty' in the same menu.\n"
      "   This is free software, and you are welcome to redistribute it under   \n"
      "          certain conditions; click on 'Copying' for details.\n"
    );
#endif
    CFocus (wedit);
}



#ifndef MIDNIGHT

#include "copyright.h"

void edit_gnu_license ()
{
    Ctextboxmessagedialog (CMain, 20, 20, TEXT_M_WIDTH * 77 + 43, 444, " Copying ", copyright, 0);
}

void edit_no_warrenty_cmd ()
{
    Ctextboxmessagedialog (CMain, 20, 20, TEXT_M_WIDTH * 77 + 43, 444, " No Warrenty ", copyright, 257);
}

#endif


void menu_load_cmd (void)		{  menu_cmd(CK_Load); }
void menu_new_cmd (void)		{  menu_cmd(CK_New); }
void menu_save_cmd (void)		{  menu_cmd(CK_Save); }
void menu_save_as_cmd (void)		{  menu_cmd(CK_Save_As); }
void menu_insert_file_cmd (void)	{  menu_cmd(CK_Insert_File); }
void menu_quit_cmd (void)		{  menu_cmd(CK_Exit); }
void menu_mark_cmd (void)		{  menu_cmd(CK_Mark); }
void menu_ins_cmd (void)		{  menu_cmd(CK_Toggle_Insert); }
void menu_copy_cmd (void)		{  menu_cmd(CK_Copy); }
void menu_move_cmd (void)		{  menu_cmd(CK_Move); }
void menu_delete_cmd (void)		{  menu_cmd(CK_Remove); }
void menu_cut_cmd (void)		{  menu_cmd(CK_Save_Block); }
void menu_search_cmd (void)		{  menu_cmd(CK_Find); }
void menu_search_again_cmd (void)	{  menu_cmd(CK_Find_Again); }
void menu_replace_cmd (void)		{  menu_cmd(CK_Replace); }
void menu_begin_record_cmd (void)	{  menu_cmd(CK_Begin_Record_Macro); }
void menu_end_record_cmd (void)		{  menu_cmd(CK_End_Record_Macro); }
void menu_wrap_cmd (void)		{  edit_wrap_cmd(); }
void menu_layout_cmd (void)		{  edit_layout_cmd(); }

#ifdef MIDNIGHT
void menu_exec_macro_cmd (void)		{  menu_key(XCTRL ('a')); }
#else
void menu_exec_macro_cmd (void)		{  menu_key(XK_a, ControlMask); }
#endif

#ifdef MIDNIGHT
void menu_c_form_cmd (void)		{  menu_key(KEY_F(19)); }
#else
void menu_c_form_cmd (void)		{  menu_key(XK_F19, 0); }
#endif

void menu_date_cmd (void)		{  menu_cmd(CK_Date); }
void menu_undo_cmd (void)		{  menu_cmd(CK_Undo); }
void menu_beginning_cmd (void)		{  menu_cmd(CK_Beginning_Of_Text); }
void menu_end_cmd (void)		{  menu_cmd(CK_End_Of_Text); }
void menu_refresh_cmd (void)		{  menu_cmd(CK_Refresh); }
void menu_goto_line (void)		{  menu_cmd(CK_Goto); }

#ifdef MIDNIGHT
void menu_lit_cmd (void)		{  menu_key(XCTRL ('q')); }
#else
/* void menu_lit_cmd (void)		{  menu_key(XK_q, ControlMask); } */
#endif

#ifdef MIDNIGHT

static menu_entry FileMenu[] =
{
    {' ', "Open/load...     C-o", 'O', menu_load_cmd},
    {' ', "New              C-n", 'N', menu_new_cmd},
    {' ', "", ' ', 0},
    {' ', "Save              F2", 'S', menu_save_cmd},
    {' ', "save As...       F12", 'A', menu_save_as_cmd},
    {' ', "", ' ', 0},
    {' ', "Insert file...   F15", 'I', menu_insert_file_cmd},
    {' ', "", ' ', 0},
    {' ', "aBout...            ", 'B', edit_about_cmd},
    {' ', "", ' ', 0},
    {' ', "Quit             F10", 'Q', menu_quit_cmd}
};

static menu_entry EditMenu[] =
{
    {' ', "Toggle Mark       F3", 'T', menu_mark_cmd},
    {' ', "", ' ', 0},
    {' ', "toggle Ins./overw.  ", 'I', menu_ins_cmd},
    {' ', "", ' ', 0},
    {' ', "Copy              F5", 'C', menu_copy_cmd},
    {' ', "Move              F6", 'M', menu_move_cmd},
    {' ', "Delete            F8", 'D', menu_delete_cmd},
    {' ', "", ' ', 0},
    {' ', "cut to File...   C-f", 'F', menu_cut_cmd},
    {' ', "", ' ', 0},
    {' ', "Undo             C-u", 'U', menu_undo_cmd},
    {' ', "", ' ', 0},
    {' ', "Beginning     C-PgUp", 'B', menu_beginning_cmd},
    {' ', "End           C-PgDn", 'E', menu_end_cmd}
};

static menu_entry SearReplMenu[] =
{
    {' ', "Search...         F7", 'S', menu_search_cmd},
    {' ', "search Again     F17", 'A', menu_search_again_cmd},
    {' ', "Replace...        F4", 'R', menu_replace_cmd}
};

static menu_entry CmdMenu[] =
{
    {' ', "Goto line...            M-l", 'G', menu_goto_line},
    {' ', "", ' ', 0},
    {' ', "insert Literal...       C-q", 'L', menu_lit_cmd},
    {' ', "", ' ', 0},
    {' ', "Refresh screen          C-l", 'R', menu_refresh_cmd},
    {' ', "", ' ', 0},
    {' ', "Start record macro      C-r", 'S', menu_begin_record_cmd},
    {' ', "Finish record macro...  C-r", 'F', menu_end_record_cmd},
    {' ', "Execute macro...   C-a, KEY", 'E', menu_exec_macro_cmd},
    {' ', "", ' ', 0},
    {' ', "insert Date/time        C-d", 'D', menu_date_cmd},
    {' ', "", ' ', 0},
    {' ', "'indent' C Formatter    F19", 'C', menu_c_form_cmd}
};

static menu_entry OptMenu[] =
{
    {' ', "Word wrap...", 'W', menu_wrap_cmd},
    {' ', "Layout...", 'L', menu_layout_cmd}
};


#define menu_entries(x) sizeof(x)/sizeof(menu_entry)

Menu EditMenuBar[N_menus];

void edit_init_menu (void)
{
    EditMenuBar[0] = create_menu (" File ", FileMenu, menu_entries (FileMenu));
    EditMenuBar[1] = create_menu (" Edit ", EditMenu, menu_entries (EditMenu));
    EditMenuBar[2] = create_menu (" Sear/Repl ", SearReplMenu, menu_entries (SearReplMenu));
    EditMenuBar[3] = create_menu (" Command ", CmdMenu, menu_entries (CmdMenu));
    EditMenuBar[4] = create_menu (" Options ", OptMenu, menu_entries (OptMenu));
}

void edit_done_menu (void)
{
    int i;
    for (i = 0; i < N_menus; i++)
	destroy_menu (EditMenuBar[i]);
}


void edit_drop_menu_cmd (WEdit * e, int which)
{
    if (edit_menubar->active)
	return;
    edit_menubar->active = 1;
    edit_menubar->dropped = drop_menus;
    edit_menubar->previous_selection = which >= 0 ? which : dlg_item_number (edit_dlg);
    if (which >= 0)
	edit_menubar->selected = which;
    dlg_select_widget (edit_dlg, edit_menubar);
}


void edit_menu_cmd (WEdit * e)
{
    edit_drop_menu_cmd (e, -1);
}


int edit_drop_hotkey_menu (WEdit * e, int key)
{
    int m = 0;
    switch (key) {
    case ALT ('f'):
	m = 0;
	break;
    case ALT ('e'):
	m = 1;
	break;
    case ALT ('s'):
	m = 2;
	break;
    case ALT ('c'):
	m = 3;
	break;
    case ALT ('o'):
	m = 4;
	break;
    default:
	return 0;
    }

    edit_drop_menu_cmd (e, m);
    return 1;
}

#else


void CDrawEditMenuButtons (const char *ident, Window parent, Window focus_return, int x, int y)
{
    Cdrawmenubutton (catstrs(ident, ".filemenu", 0), parent, focus_return, x, y, 80, 20, 13,
		     " File ",
		     "Open...          C-o", 'O', menu_load_cmd,
		     "New              C-n", 'N', menu_new_cmd,
		     "", ' ', 0,
		     "Save              F2", 'S', menu_save_cmd,
		     "save As...       F12", 'A', menu_save_as_cmd,
		     "", ' ', 0,
		     "Insert file...   F15", 'I', menu_insert_file_cmd,
		     "", ' ', 0,
		     "About...            ", 'A', edit_about_cmd,
		     "Copying...          ", 'C', edit_gnu_license,
		     "no Warranty...      ", 'W', edit_no_warrenty_cmd,
		     "", ' ', 0,
		     "Close            F10", 'Q', menu_quit_cmd
	);

    Cdrawmenubutton (catstrs (ident, ".editmenu", 0), parent, focus_return, 90 + x, y, 80, 20, 14,
		     " Edit ",
		     "Toggle Mark         F3", 'T', menu_mark_cmd,
		     "", ' ', 0,
		     "toggle Ins./overw.    ", 'I', menu_ins_cmd,
		     "", ' ', 0,
		     "Copy                F5", 'C', menu_copy_cmd,
		     "Move                F6", 'M', menu_move_cmd,
		     "Delete              F8", 'D', menu_delete_cmd,
		     "", ' ', 0,
		     "cut to File...     C-f", 'F', menu_cut_cmd,
		     "", ' ', 0,
		     "Undo       C-BackSpace", 'U', menu_undo_cmd,
		     "", ' ', 0,
		     "Beginning       C-PgUp", 'B', menu_beginning_cmd,
		     "End             C-PgDn", 'E', menu_end_cmd
	);

    Cdrawmenubutton (catstrs (ident, ".searchmenu", 0), parent, focus_return, 180 + x, y, 100, 20, 3,
		     " Sear/Repl ",
		     "Search...         F7", 'S', menu_search_cmd,
		     "search Again     F17", 'A', menu_search_again_cmd,
		     "Replace...        F4", 'R', menu_replace_cmd
	);

    Cdrawmenubutton (catstrs (ident, ".commandmenu", 0), parent, focus_return, 290 + x, y, 80, 20, 7,
		     " Command ",
		     "Goto line...            M-l", 'G', menu_goto_line,
		     "", ' ', 0,
/*
		     "insert Literal...       C-q", 'L', menu_lit_cmd,
		     "", ' ', 0,
		     "Refresh screen          C-l", 'R', menu_refresh_cmd,
		     "", ' ', 0,
*/
		     "Start record macro      C-r", 'S', menu_begin_record_cmd,
		     "Finish record macro...  C-r", 'F', menu_end_record_cmd,
		     "Execute macro...   C-a, KEY", 'E', menu_exec_macro_cmd,
		     "", ' ', 0,
		     "insert Date/time        C-d", 'D', menu_date_cmd,
		     "", ' ', 0,
		     "'indent' C Formatter    F19", 'C', menu_c_form_cmd
	);

    Cdrawmenubutton (catstrs (ident, ".optionsmenu", 0), parent, focus_return, 380 + x, y, 80, 20, 2,
		     " Options ",
		     "Word wrap...", 'W', menu_wrap_cmd,
		     "Layout...", 'L', menu_layout_cmd
	);
}


#endif

