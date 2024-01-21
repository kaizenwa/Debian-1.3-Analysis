/*
 * File:	selections.c
 * Purpose:	Handle selections (our's and X's)
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: selections.c,v 1.26 1997/01/07 01:05:35 liw Exp $"
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

#include "anchor.h"
#include "cmd.h"
#include "killring.h"
#include "win.h"
#include "selections.h"
#include "error.h"



/*
 * Variable:	we_own_the_X_selection
 * Purpose:	Do we own the X selection?
 */
static int we_own_the_X_selection = 0;



/*
 * Variables:	selbuf, selmark
 * Purpose:	Buffer containing converted selection, an auxiliary mark,
 *		and a pointer to the string with the data.
 */

static Sbuf *selbuf = NULL;
static Sbufmark *selmark = NULL;



/*
 * Communciation between sel_string and save.
 */
struct save_args {
	void (*fun)(char *, long, void *);
	struct win *win;
	void *arg;
};



/*
 * Prototypes for local functions.
 */

static void paste(Widget, XtPointer, Atom *, Atom *, 
	XtPointer, unsigned long *, int *);
static void save(Widget, XtPointer, Atom *, Atom *, 
	XtPointer, unsigned long *, int *);
static int init_convert(void);
static int set_convert(struct win *, char **, size_t *);
static Boolean convert(Widget, Atom *, Atom *, Atom *, XtPointer *,
	unsigned long *, int *);
static void lose(Widget ww, Atom *);



/*
 * Function:	sel_start
 */
int sel_start(struct win *win, int x, int y, Time time) {
	anchor_up(win);
	win_start_selection(win, x, y);
	if (sbuf_mark_length(win_selection(win)) > 0)
		if (sel_own(win, time) == -1)
			return -1;
	return 0;
}



/*
 * Function:	sel_continue
 * Purpose:	Handle sweeps with mouse button pressed.
 * Arguments:	win	the window clicked in
 *		x,y	the current mouse position (in pixels)
 *		time	time of the event that triggered the call
 * Return:	0 for ok, -1 for failure.
 */
int sel_continue(struct win *win, int x, int y, Time time) {
	anchor_up(win);
	win_continue_selection(win, x, y);
	if (sbuf_mark_length(win_selection(win)) > 0)
		if (sel_own(win, time) == -1)
			return -1;
	return 0;
}


/*
 * Function:	sel_extend
 * Purpose:	Handle extending the selection.
 * Arguments:	win	the window clicked in
 *		x,y	the pixel position clicked in
 *		time	when the mouse button was clicked
 * Return:	0 for ok, -1 for failure.
 */
int sel_extend(struct win *win, int x, int y, Time time) {
	anchor_up(win);
	win_extend_selection(win, x, y);
	if (sbuf_mark_length(win_selection(win)) > 0)
		if (sel_own(win, time) == -1)
			return -1;
	return 0;
}


/*
 * Function:	sel_cut_or_paste
 * Purpose:	Cut or paste the selection.
 * Arguments:	win	the window clicked in
 *		x,y	the pixel position clicked in
 *		time	when the mouse button was clicked
 * Return:	0 for ok, -1 for failure.
 */
int sel_cut_or_paste(struct win *win, int x, int y, Time time) {
	Sbufmark *sel;
	long pos;

	anchor_up(win);

	pos = win_pixel_to_text(win, x, y);
	sel = win_selection(win);

	if (sbuf_pos_inside_mark(sel, pos)) {
		if (cmd_cut(win) == -1)
			return -1;
	} else if (sbuf_mark_length(sel) > 0) {
		if (cmd_copy(win) == -1)
			return -1;
		sbuf_remark(sel, pos, 0);
		sbuf_mark_set_columnar(sel, 0);
		if (cmd_yank(win) == -1)
			return -1;
		if (sel_own(win, time) == -1)
			return -1;
	} else {
		sbuf_remark(sel, pos, 0);
		sbuf_mark_set_columnar(sel, 0);
		XtGetSelectionValue(win_toplevel(win), XA_PRIMARY, XA_STRING,
			paste, 0, time);
	}

	return 0;
}



/*
 * Function:	sel_own
 * Purpose:	Make sure we own the selection.
 * Arguments:	win	the window that is to own the selection
 *		time	timestamp of ownership
 * Return:	-1 for failure, 0 for success.
 */
int sel_own(struct win *win, Time time) {
	Boolean ret;

	if (time == (Time) -1)
		time = CurrentTime;
	ret = XtOwnSelection(win_toplevel(win), XA_PRIMARY, time,
		convert, lose, 0);
	if (!ret) {
		we_own_the_X_selection = 0;
		error(win, "Error: could not become selection owner!");
		return -1;
	}

	we_own_the_X_selection = 1;
	return 0;
}



/*
 * Function:	sel_string
 * Purpose:	Process current selection string (X or our own).
 * Arguments:	win	which window's selection to return, if no X selection
 *		fun	function to call when selection is known
 * Return:	-1 for failure, 0 for ok.
 * Note:	The function may be called quite some time after sel_string
 *		returns.
 */
int sel_string(struct win *win, void (*fun)(char *, long, void *), void *arg) {
	struct save_args *p;

	p = malloc(sizeof(*p));
	if (p == NULL)
		return -1;
	p->fun = fun;
	p->win = win;
	p->arg = arg;
	XtGetSelectionValue(win_toplevel(win), XA_PRIMARY, XA_STRING,
		save, p, CurrentTime);
	return 0;
}



/******************************************************************
 * Local functions follow.
 */



/*
 * Function:	init_convert
 * Purpose:	Initialize variables needed for selection conversion
 * Arguments:	None.
 * Return:	-1 for failure, 0 for success.
 */
static int init_convert(void) {
	selbuf = sbuf_create();
	if (selbuf == NULL) {
		error(0, "can't create buffer, can't convert");
		return -1;
	}
	selmark = sbuf_mark(selbuf, 0, 0);
	if (selmark == NULL) {
		error(0, "can't create mark, can't convert");
		return -1;
	}
	return 0;
}



/*
 * Function:	set_convert
 * Purpose:	Set variables that hold converted selection value.
 * Arguments:	win	the window that owns the selection
 * Return:	-1 for failure, 0 for success.
 */
static int set_convert(struct win *win, char **buf, size_t *len) {
	Sbufmark *sel;
	int ret;
	char *temp;
	
	sbuf_remark(selmark, 0, sbuf_length(selbuf));

	sel = win_selection(win);

	if (killring_get_first_mark() == NULL || sbuf_mark_length(sel) > 0)
		ret = sbuf_change(selmark, sel);
	else
		ret = killring_get_first(selmark);
	if (ret == -1) {
		error(win, "Error: out of memory converting selection");
		return -1;
	}

	*len = sbuf_length(selbuf);
	*buf = malloc(*len);
	if (*buf == NULL) {
		error(win, "Out of memory during selection conversion");
		return -1;
	}
	temp = sbuf_lock(selbuf);
	if (temp == NULL) {
		error(win, 
			"error locking aux buffer for selection conversion");
		free(*buf);
		return -1;
	}
	memcpy(*buf, temp, *len);
	sbuf_unlock(selbuf);

	return 0;
}



/*
 * Function:	convert
 * Purpose:	Convert the selection into desired target type.
 */
static Boolean convert(Widget wid, Atom *selection, Atom *target,
	Atom *type_return, XtPointer *value_return,
	unsigned long *length_return, int *format_return)
{
	struct win *win;
	char *buf;
	size_t len;

	if (!we_own_the_X_selection) {
		error(0, "we don't own the X selection, can't convert");
		return False;
	}

	win = win_find(wid);
	if (win == NULL) {
		error(0, "can't find window, can't convert");
		return False;
	}

	if (selbuf == NULL && init_convert() == -1)
		return False;

	if (set_convert(win, &buf, &len) == -1)
		return False;

	*value_return = buf;
	*length_return = len;
	*type_return = *target;
	*format_return = 8;

	return True;
}



/*
 * Oops, we lost the selection...
 */
static void lose(Widget wid, Atom *selection) {
	struct win *win;
	Sbufmark *sel;

	win = win_find(wid);
	if (win == NULL) {
		error(0, "oops - can't find window when losing selectiion");
		return;
	}

	if (selmark != NULL)
		(void) sbuf_strchange(selmark, "", 0);
	sel = win_selection(win);
	sbuf_remark(sel, sbuf_mark_begin(sel), 0);
	sbuf_mark_set_columnar(sel, 0);
	we_own_the_X_selection = 0;
	win_update_all();
}



/*
 * Function:	paste
 * Purpose:	Callback function for Xt paste operation.
 */
static void paste(Widget w, XtPointer ptr, Atom *sel, Atom *type, 
XtPointer value, unsigned long *length, int *format)
{
	struct win *win;

	win = win_find(w);
	if (value != NULL && *length > 0) {
		if (sbuf_strchange(win_selection(win), value, *length) == -1)
			XtWarning("strchange failed when pasting!\n");
	} else {
		cmd_yank(win);
	}

	win_update_all();

	XFree(value);

	(void) sel_own(win, CurrentTime);
}




/*
 * Function:	save
 * Purpose:	Callback function for Xt paste operation that saves string.
 * Note:	Used by sel_string.
 */
static void save(Widget w, XtPointer ptr, Atom *sel, Atom *type, 
XtPointer value, unsigned long *length, int *format)
{
	struct save_args *p;
	char *str;
	long len;

	p = ptr;

	if (value != NULL && *length > 0) {
		str = malloc(*length + 1);
		if (str == NULL)
			error(p->win, "Out of memory");
		else {
			memcpy(str, value, *length);
			str[*length] = '\0';
			p->fun(str, (long) *length, p->arg);
			free(str);
		}
	} else {
		Sbufmark *sel, *kill;

		sel = win_selection(p->win);
		len = sbuf_mark_length(sel);
		if (len == 0) {
			kill = killring_get_first_mark();
			if (kill != NULL) {
				sel = kill;
				len = sbuf_mark_length(sel);
			}
		}
		str = malloc(len+1);
		if (str != NULL) {
			sbuf_strat(str, sel);
			str[len] = '\0';
			p->fun(str, len, p->arg);
			free(str);
		} else
			error(p->win, "out of memory getting selection");
	}

	XFree(value);

	free(p);

	win_update_all();
}
