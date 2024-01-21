/*
 * File:	actions.c
 * Purpose:	Actions functions.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: actions.c,v 1.55 1996/12/06 13:15:27 liw Exp $"
 */


#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/MenuButton.h>

#include <publib.h>

#include "actions.h"
#include "filewin.h"
#include "cmd.h"
#include "selections.h"
#include "win.h"
#include "searchwin.h"
#include "error.h"


struct trans {
	const char *arg;
	int (*cmd)(struct win *);
};


static struct trans yow_args[] = {
	{ "format", cmd_format_selection },
	{ NULL },
};


static struct trans quit_args[] = {
	{ "asksave", cmd_quit },
	{ "autosave", cmd_quit_and_save_all },
	{ "nosave", cmd_quit_without_saving },
	{ NULL },
};


static struct trans move_args[] = {
	{ "nextchar", cmd_forward },
	{ "prevchar", cmd_backward },
	{ "nextline", cmd_next_line },
	{ "prevline", cmd_prev_line },
	{ "nextword", cmd_next_word },
	{ "prevword", cmd_prev_word },
	{ "nextpara", cmd_next_para },
	{ "prevpara", cmd_prev_para },
	{ "nextpage", cmd_next_page },
	{ "prevpage", cmd_prev_page },
	{ "boln", cmd_goto_boln },
	{ "eoln", cmd_goto_eoln },
	{ "bof", cmd_goto_bof },
	{ "eof", cmd_goto_eof },
	{ "top", cmd_goto_top },
	{ "bottom", cmd_goto_bottom },
	{ "mark0", cmd_goto_mark_0 },
	{ "mark1", cmd_goto_mark_1 },
	{ "mark2", cmd_goto_mark_2 },
	{ "mark3", cmd_goto_mark_3 },
	{ "mark4", cmd_goto_mark_4 },
	{ "mark5", cmd_goto_mark_5 },
	{ "mark6", cmd_goto_mark_6 },
	{ "mark7", cmd_goto_mark_7 },
	{ "mark8", cmd_goto_mark_8 },
	{ "mark9", cmd_goto_mark_9 },
	{ "setmark0", cmd_set_mark_0 },
	{ "setmark1", cmd_set_mark_1 },
	{ "setmark2", cmd_set_mark_2 },
	{ "setmark3", cmd_set_mark_3 },
	{ "setmark4", cmd_set_mark_4 },
	{ "setmark5", cmd_set_mark_5 },
	{ "setmark6", cmd_set_mark_6 },
	{ "setmark7", cmd_set_mark_7 },
	{ "setmark8", cmd_set_mark_8 },
	{ "setmark9", cmd_set_mark_9 },
	{ "selection", cmd_goto_selection },
	{ "scrollnextpage", cmd_scroll_next_page },
	{ "scrollprevpage", cmd_scroll_prev_page },
	{ "scrollleftone", cmd_scroll_left_one },
	{ "scrollrightone", cmd_scroll_right_one },
	{ "scrollcenter", cmd_center },
	{ NULL },
};


static struct trans edit_args[] = {
	{ "cut", cmd_cut },
	{ "copy", cmd_copy },
	{ "cuteoln", cmd_cut_ctrl_k },
	{ "cutprev", cmd_cut_previous },
	{ "cutnext", cmd_cut_next },
	{ "cutprevword", cmd_cut_prev_word },
	{ "cutnextword", cmd_cut_next_word },
	{ "yank", cmd_yank },
	{ "yankprev", cmd_yank_previous },
	{ "yankcol", cmd_yank_column },
	{ "yankprevcol", cmd_yank_column_previous },
	{ "indent", cmd_indent_selection },
	{ "undent", cmd_undent_selection },
	{ "indentspace", cmd_indent_selection_space },
	{ "undentspace", cmd_undent_selection_space },
	{ "indentline", cmd_indent_line },
	{ "undentcurly", cmd_unindent_right_curly },
	{ "format", cmd_format_selection },
	{ NULL },
};


static struct trans file_args[] = {
	{ "save", cmd_save_file },
	{ "load", cmd_query_load_file },
	{ "loadsel", cmd_load_selection },
	{ "reload", cmd_load_file },
	{ "saveas", cmd_save_as },
	{ "saveall", cmd_save_all },
	{ "next", cmd_next_file },
	{ "prev", cmd_prev_file },
	{ "insert", cmd_query_insert_file },
	{ "unmark", cmd_fake_save },
	{ "kill", cmd_kill_buffer },
	{ NULL },
};


static struct trans win_args[] = {
	{ "clone", cmd_clone },
	{ "close", cmd_close },
	{ "closeothers", cmd_close_others },
	{ "openall", cmd_open_all },
	{ NULL },
};


static struct trans extend_args[] = {
	{ "togglecolumnar", cmd_toggle_columnar },
	{ "nextchar", cmd_extend_forward },
	{ "prevchar", cmd_extend_backward },
	{ "nextword", cmd_extend_next_word },
	{ "prevword", cmd_extend_prev_word },
	{ "nextpara", cmd_extend_next_para },
	{ "prevpara", cmd_extend_prev_para },
	{ "nextline", cmd_extend_next_line },
	{ "prevline", cmd_extend_prev_line },
	{ "nextpage", cmd_extend_next_page },
	{ "prevpage", cmd_extend_prev_page },
	{ "boln", cmd_extend_boln },
	{ "eoln", cmd_extend_eoln },
	{ "top", cmd_extend_top },
	{ "bottom", cmd_extend_bottom },
	{ "bof", cmd_extend_bof },
	{ "eof", cmd_extend_eof },
	{ "mark0", cmd_extend_mark_0 },
	{ "mark1", cmd_extend_mark_1 },
	{ "mark2", cmd_extend_mark_2 },
	{ "mark3", cmd_extend_mark_3 },
	{ "mark4", cmd_extend_mark_4 },
	{ "mark5", cmd_extend_mark_5 },
	{ "mark6", cmd_extend_mark_6 },
	{ "mark7", cmd_extend_mark_7 },
	{ "mark8", cmd_extend_mark_8 },
	{ "mark9", cmd_extend_mark_9 },
	{ NULL },
};


static struct trans find_args[] = {
	{ "selection", cmd_search_selection },
	{ "popup", cmd_search_popup },
	{ "next", cmd_search_next },
	{ "replace+find", cmd_replace_and_search },
	{ "replaceall", cmd_search_and_replace_all },
	{ "cancel", cmd_cancel_search },
	{ NULL },
};


static void do_arg(struct trans *tab, const char *s, struct win *win) {
	int i;
	
	for (i = 0; tab[i].arg != NULL; ++i) {
		if (strcmp(s, tab[i].arg) == 0) {
			(void) tab[i].cmd(win);
			break;
		}
	}
}


/*
 * Macro:	ACTION
 * Purpose:	Generate code for a typical action function.
 * Arguments:	name	name of function to be generated
 *		body	what the function is supposed to do
 */
#define ACTION(name, body) \
	static void name(Widget w, XEvent *e, String *s, Cardinal *n) {	\
		struct win *win; 					\
		Time time;						\
									\
		win = win_find(w);					\
		if (win == NULL)					\
			XtWarning("oops - event to unknown window");	\
		else {							\
			body;						\
									\
			switch (e->type) {				\
			case KeyPress:					\
				time = e->xkey.time; break;		\
			case ButtonPress:				\
				time = e->xbutton.time; break;		\
			default:					\
				time = 0; break;			\
			}						\
									\
			if (time != 0 &&				\
			    sbuf_mark_length(win_selection(win)) > 0)	\
				(void) sel_own(win, time);		\
			win_update_all();				\
		}							\
	}




/*
 * The simple action functions.
 */

ACTION(yow, do_arg(yow_args, *(char **)s, win))
ACTION(cache_stats, win_cache_stats())

ACTION(file, do_arg(file_args, *(char **)s, win))
ACTION(edit, do_arg(edit_args, *(char **)s, win))
ACTION(move, do_arg(move_args, *(char **)s, win))
ACTION(quit, do_arg(quit_args, *(char **)s, win))
ACTION(win, do_arg(win_args, *(char **)s, win))
ACTION(extend, do_arg(extend_args, *(char **)s, win))
ACTION(find, do_arg(find_args, *(char **)s, win))

ACTION(newline, cmd_insert(win, "\n", 1))

ACTION(filename_cancel, \
	{ struct filewin *fw = filewin_find(w); if (fw) filewin_popdown(fw); })
ACTION(filename_ok, \
	{ struct filewin *fw = filewin_find(w); if (fw) filewin_ok(fw); })

ACTION(focus_search, searchwin_focus(win_searchwin(win), 0))
ACTION(focus_replace, searchwin_focus(win_searchwin(win), 1))

ACTION(dismiss_message, win_popdown_msg(win))

ACTION(start_selection, 
	sel_start(win, e->xbutton.x, e->xbutton.y, e->xbutton.time))
ACTION(continue_selection, 
	sel_continue(win, e->xbutton.x, e->xbutton.y, e->xbutton.time))
ACTION(extend_selection, 
	sel_extend(win, e->xbutton.x, e->xbutton.y, e->xbutton.time))
ACTION(cut_or_paste, 
	sel_cut_or_paste(win, e->xbutton.x, e->xbutton.y, e->xbutton.time))
ACTION(select_para, cmd_select_para(win))

ACTION(build_buf_menu, win_build_buf_menu(win))


/*
 * Prototypes for local functions.
 */
 
typedef void action_function(Widget, XEvent *, String *, Cardinal *);

static action_function resize;
static action_function redraw;
static action_function insert;



/*
 * Variable:	action_table
 * Purpose:	List all action functions and their X names.
 * Note:	This is given to XtAppAddActions as an argument.
 */
static XtActionsRec action_table[] = {
	{ "yow",		yow },
	{ "cache_stats",	cache_stats },

	{ "redraw",		redraw },
	{ "resize",		resize },
	{ "insert",		insert },
	{ "start_selection",	start_selection },
	{ "continue_selection",	continue_selection },
	{ "extend_selection",	extend_selection },
	{ "select_para",	select_para },
	{ "filename_ok",	filename_ok },
	{ "filename_cancel",	filename_cancel },
	{ "dismiss_message",	dismiss_message },
	{ "focus_search",	focus_search },
	{ "focus_replace",	focus_replace },
	{ "build_buf_menu",	build_buf_menu },
	{ "cut_or_paste",	cut_or_paste },
	{ "newline",		newline },

	{ "file",		file },
	{ "edit",		edit },
	{ "move",		move },
	{ "win",		win },
	{ "quit",		quit },
	{ "extend",		extend },
	{ "find",		find },
	{ NULL },
};



/*
 * Function:	action_add
 * Purpose:	Add all action functions to the application context.
 * Arguments:	app	pointer to the application context
 * Return:	-1 for failure, 0 for success.
 */
int action_add(XtAppContext *app) {
	assert(app != NULL);
	XtAppAddActions(*app, action_table, XtNumber(action_table));
	return 0;
}



/*
 * Function:	redraw
 * Purpose:	Redraw window (after move, expose, uniconify, etc).
 */
static void redraw(Widget window, XEvent *event, String *argv, Cardinal *argc) {
	struct win *win;
	
	win = win_find(window);
	if (win == NULL)
		error(0, __FILE__ " redraw: unknown window");
	else {
		win_force_update(win);
		win_update_all();	/* sets timeout, if necessary */
	}
}



/*
 * Function:	resize
 * Purpose:	Resize and redraw window.
 */
static void resize(Widget window, XEvent *event, String *argv, Cardinal *argc) {
	struct win *win;
	
	win = win_find(window);
	if (win == NULL)
		error(0, __FILE__ " resize: unknown window");
	else {
		win_resize(win);
		win_force_update(win);
		win_update_all();	/* sets timeout, if necessary */
	}
}


/*
 * Function:	insert
 * Purpose:	React to key press by inserting the corresponding characters.
 */
static void insert(Widget window, XEvent *event, String *argv, Cardinal *argc) {
	XComposeStatus compose;
	KeySym key;
	int n;
	char buf[128] = {0};
	struct win *win;

	n = XLookupString(&event->xkey, buf, sizeof(buf)-1, &key, &compose);
	if (n > 0) {
		win = win_find(window);
		if (win == NULL)
			error(0, __FILE__ " insert: unknown window");
		else {
			cmd_insert(win, buf, n);
			win_update_all();
		}
	}
}
