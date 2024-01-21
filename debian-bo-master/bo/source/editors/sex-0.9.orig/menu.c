/*
 * File:	menu.c
 * Purpose:	Implement menus.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: menu.c,v 1.32 1996/12/07 16:28:24 liw Exp $"
 */
 
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/MenuButton.h>

#include <publib.h>

#include "menu.h"
#include "cmd.h"
#include "win.h"
#include "error.h"



/*
 * Structure:	buf_arg
 * Purpose:	Argument to buf_select.
 * Fields:	win	the window to change in
 *		buf	the buffer to change to
 */
struct buf_arg {
	struct win *win;
	Sbuf *buf;
};



/*
 * Structure:	menu_buf
 * Purpose:	Descriptor for a buffer menu.
 * Fields:	win		the window the buffer menu belongs to
 *		button		the button that triggers the menu
 *		menu		the menu widget
 *		entry		list of entry widgets
 *		args		entry specific args for callback functions
 *		num_entries	number of entries
 */

#define MAX_ENTRIES 128

struct menu_buf {
	struct win *win;
	Widget button;
	Widget menu;
	Widget entry[MAX_ENTRIES];
	struct buf_arg args[MAX_ENTRIES];
	Sbuf *bufs[MAX_ENTRIES];
	int num_entries;
	int current_entry;
};



/*
 * Structure:	menu
 * Purpose:	A menu descriptor.
 * Description:	The build_menu function does whatever is necessary to do 
 *		to create a menu based on the information in a menu 
 *		descriptor.  The menu descriptor describes the entries 
 *		and gives a function to call for each entry.
 * Fields:	menu	name of menu widget
 *		button	name of button widget
 *		label	text in menu button
 *		entries	list of menu entries
 *			label	text in entry
 *			hook	pointer to function that handles menu entry
 * Note:	The first character in the entry label gives its type:
 *		`-' means the entry is just a line, `.' means the entry
 *		has not been implemented yet, ` ' means a normal entry.
 */
#define MAX_MENU_ENTRIES	20
struct menu {
	char *menu;
	char *button;
	char *label;
	struct menu_entry {
		char *label;
		cmd_function *hook;
	} entries[MAX_MENU_ENTRIES];
};



/*
 * The file menu descriptor.
 */
static struct menu file_menu = {
	"file_menu",
	"file_button",
	"File",
	{
		{ " New",		cmd_new },
		{ " Load...",		cmd_query_load_file },
		{ " Reload",		cmd_reload_file },
		{ " Save",		cmd_save_file },
		{ " Save as...",	cmd_save_as },
		{ " Save all modified",	cmd_save_all },
		{ "-",			NULL },
		{ " Insert file...",	cmd_query_insert_file },
		{ " Write selection to...",	cmd_query_write_to },
		{ "-",			NULL },
		{ " Remove buffer",	cmd_kill_buffer },
		{ " Mark unmodified",	cmd_fake_save },
		{ "-", 			NULL },
		{ " Next file",		cmd_next_file },
		{ " Previous file",	cmd_prev_file },
		{ "-", 			NULL },
		{ " Exit", 		cmd_quit },
		{ NULL },
	},
};



/*
 * The edit menu descriptor.
 */
static struct menu edit_menu = {
	"edit_menu",
	"edit_button",
	"Edit",
	{
		{ ".Undo",		NULL },
		{ ".Redo",		NULL },
		{ "-",			NULL },
		{ " Toggle columnar",	cmd_toggle_columnar },
		{ "-",			NULL },
		{ " Cut",		cmd_cut },
		{ " Copy",		cmd_copy },
		{ " Paste",		cmd_yank },
		{ " Paste previous",	cmd_yank_previous },
		{ "-",			NULL },
		{ " Pipe...",		cmd_query_pipe },
		{ "-",			NULL },
		{ " Format selection",	cmd_format_selection },
		{ NULL },
	},
};



/*
 * The goto menu descriptor.
 */
static struct menu goto_menu = {
	"goto_menu",
	"goto_button",
	"Goto",
	{
		{ " Line...",		cmd_query_goto_line },
		{ "-",			NULL },
		{ " Goto mark 1",	cmd_goto_mark_1 },
		{ " Goto mark 2",	cmd_goto_mark_2 },
		{ " Goto mark 3",	cmd_goto_mark_3 },
		{ " Goto mark 4",	cmd_goto_mark_4 },
		{ " Goto mark 5",	cmd_goto_mark_5 },
		{ "-",			NULL },
		{ " Set mark 1",	cmd_set_mark_1 },
		{ " Set mark 2",	cmd_set_mark_2 },
		{ " Set mark 3",	cmd_set_mark_3 },
		{ " Set mark 4",	cmd_set_mark_4 },
		{ " Set mark 5",	cmd_set_mark_5 },
		{ NULL },
	},
};



/*
 * The search menu descriptor.
 */
static struct menu search_menu = {
	"search_menu",
	"search_button",
	"Search",
	{
		{ " Search...",		cmd_search_popup },
		{ " Search next",	cmd_search_next },
		{ " Replace & search",	cmd_replace_and_search },
		{ " Replace this",	cmd_replace_once },
		{ " Replace all",	cmd_search_and_replace_all },
		{ NULL },
	},
};



/*
 * The window menu descriptor.
 */
static struct menu window_menu = {
	"window_menu",
	"window_button",
	"Window",
	{
		{ " Clone",		cmd_clone },
		{ " Every file",	cmd_open_all },
		{ " Close this",	cmd_close },
		{ " Close others",	cmd_close_others },
		{ NULL },
	},
};



/*
 * The buffer menu descriptor.
 * Note that this really must be empty.
 */
static struct menu buf_menu = {
	"buf_menu",
	"buf_button",
	"Buffers",
	{
		{ NULL },
	},
};



/*
 * Variable:	menu_list
 * Purpose:	List of all menus.
 */
static struct menu *menu_list[] = {
	&file_menu,
	&edit_menu,
	&goto_menu,
	&search_menu,
	&window_menu,
	&buf_menu,
	NULL,
};



/*
 * Structure:	hook
 * Purpose:	Argument to generic_select.
 * Fields:	cmd	the command function to be called
 *		win	the window argument for cmd
 */
struct hook {
	cmd_function *cmd;
	struct win *win;
};



/*
 * Local functions.
 */
static int create_menu(Widget *, Widget, struct menu *, struct win *);
static Widget create_button(Widget, struct menu *);
static void generic_select(Widget, XtPointer, XtPointer);
static void buf_select(Widget, XtPointer, XtPointer);



/*
 * Function:	menu_add
 * Purpose:	Create menu buttons and menus, add them to a window.
 * Arguments:	bufbut	where widget of buffer menu button is stored
 *		bufmenu	where widget of buffer menu is stored
 *		win	the top level window
 *		wid	form widget to which the menus are added
 * Return:	-1 for failure, 0 for success.
 */
int menu_add(struct menu_buf **bufmenu, Widget *but, struct win *win, 
Widget wid) {
	Widget button, menu, prev;
	int i;

	*bufmenu = malloc(sizeof(struct menu_buf));
	if (*bufmenu == NULL) {
		error(win, "Can't allocate memory for menus.");
		return -1;
	}
	(*bufmenu)->win = win;
	(*bufmenu)->num_entries = 0;
	(*bufmenu)->current_entry = 0;

	prev = 0;
	for (i = 0; menu_list[i] != NULL; ++i) {
		button = create_button(wid, menu_list[i]);
		if (prev != 0)
			XtVaSetValues(button, XtNfromHoriz, prev, NULL);
		win_add_child(win, button);
		if (create_menu(&menu, button, menu_list[i], win) == -1)
			return -1;
		if (menu_list[i] == &buf_menu) {
			(*bufmenu)->button = button;
			(*bufmenu)->menu = menu;
		}
		prev = button;
	}

	*but = prev;
	return 0;
}



/*
 * Function:	menu_clear_bufs
 * Purpose:	Clear the buffer menu that belongs to a window.
 * Arguments:	wid	the widget for the menu (may change!)
 *		but	the button that triggers the menu
 *		win	the window itself
 * Return:	-1 for failure, 0 for success.
 */
int menu_clear_bufs(struct menu_buf *bufmenu) {
	bufmenu->current_entry = 0;
	return 0;
}



/*
 * Function:	menu_add_buf
 * Purpose:	Add one more buffer to the buffer menu.
 * Arguments:	menu	the menu widget
 *		win	the window
 *		buf	the buffer
 * Return:	-1 for failure, 0 for success.
 */
int menu_add_buf(struct menu_buf *bufmenu, Sbuf *buf) {
	Widget entry;
	struct buf_arg *p;
	char *name, *status, str[1024];

	if (bufmenu->num_entries == MAX_ENTRIES) {
		error(bufmenu->win, "Too many buffers, can't build menu");
		return -1;
	}

	if (!sbuf_has_flags(buf, SBUF_LOADED_FLAG))
		status = "--";
	else if (sbuf_is_dirty(buf))
		status = "**";
	else
		status = "  ";
	name = sbuf_get_name(buf);
	if (name == NULL)
		name = "(untitled)";
	sprintf(str, "%s %.512s", status, name);

	if (bufmenu->current_entry == bufmenu->num_entries) {
		entry = bufmenu->entry[bufmenu->current_entry] = 
			XtCreateManagedWidget(str, smeBSBObjectClass, 
				bufmenu->menu, NULL, 0);
		++bufmenu->num_entries;
	} else {
		entry = bufmenu->entry[bufmenu->current_entry];
		XtVaSetValues(entry, XtNlabel, str, NULL);
	}

	p = bufmenu->args + bufmenu->current_entry;
	p->buf= buf;
	p->win = bufmenu->win;
	XtAddCallback(entry, XtNcallback, buf_select, p);

	++bufmenu->current_entry;
	return 0;
}



/*
 * Function:	menu_done_adding
 * Purpose:	Signal end of adding new entries to a buffer menu.
 * Arguments:	bufmenu	the buffer menu
 * Return:	Nothing.
 */
void menu_done_adding(struct menu_buf *bufmenu) {
	int i;

	for (i = bufmenu->current_entry; i < bufmenu->num_entries; ++i)
		XtDestroyWidget(bufmenu->entry[i]);
	bufmenu->num_entries = bufmenu->current_entry;
}



/**********************************************************************
 * Local functions follow.
 */



/*
 * Function:	create_menu
 * Purpose:	Create a menu.
 */
static int create_menu(Widget *menuptr, Widget button, struct menu *p, 
struct win *win) {
	Widget entry, menu;
	struct hook *h;
	struct menu_entry *pp;

	menu = XtCreatePopupShell(p->menu, simpleMenuWidgetClass, button, 0, 0);
	*menuptr = menu;

	for (pp = p->entries; pp->label != NULL; ++pp) {
		switch (pp->label[0]) {
		case '-':
			entry = XtCreateManagedWidget("line",
				smeLineObjectClass, menu, NULL, 0);
			break;
		default:
			entry = XtCreateManagedWidget(pp->label+1,
				smeBSBObjectClass, menu, NULL, 0);
			if (pp->label[0] == '.')
				XtVaSetValues(entry, XtNsensitive, False, NULL);

			h = malloc(sizeof(struct hook));
			if (h == NULL) {
				error(win, "out of memory creating menus");
				return -1;
			}
			h->cmd = pp->hook;
			h->win = win;

			XtAddCallback(entry, XtNcallback, generic_select, h);
			break;
		}
	}

	return 0;
}



/*
 * Function:	create_button
 * Purpose:	Create a new menu button.
 * Arguments:	form	the form widget which will hold the button
 *		p	pointer to menu descriptor for which button is created
 * Return:	The widget of the button.
 */
static Widget create_button(Widget form, struct menu *p) {
	return XtVaCreateManagedWidget(
		p->button,
		menuButtonWidgetClass,
		form,
		XtNmenuName, p->menu,
		XtNlabel, p->label,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNborderWidth, 0,
		(void *) 0);
}



/*
 * This is the generic callback function for menu selections.  It gets
 * a pointer to the function that handles the menu entry as client_data.
 */
static void generic_select(Widget w, XtPointer hook, XtPointer dummy) {
	struct hook *h;

	h = hook;
	if (h == NULL)
		XtWarning("oops - hook is null in generic_select!");
        else if (h->cmd != NULL) {
		h->cmd(h->win);
		win_update_all();
	}
}



/*
 * This is the callback function for buffer menu selections.
 */
static void buf_select(Widget w, XtPointer hook, XtPointer dummy) {
	struct buf_arg *p;

	p = hook;
	win_set_buf(p->win, p->buf);
	win_update_all();
}
