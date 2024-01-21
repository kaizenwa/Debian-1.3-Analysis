/*
 * File:	win.c
 * Purpose:	Implement top level windows.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: win.c,v 1.57 1996/12/22 20:06:12 liw Exp $"
 */

#include <assert.h>
#include <stdlib.h>
#include <publib.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>

#include "anchor.h"
#include "win.h"
#include "buflist.h"
#include "cmd.h"
#include "editwin.h"
#include "endprompt.h"
#include "menu.h"
#include "msgwin.h"
#include "searchwin.h"
#include "filewin.h"
#include "font.h"
#include "x.h"
#include "error.h"
#include "status.h"
#include "tab.h"



/*
 * Structure:	win
 * Purpose:	Descriptor for top level window.
 * Fields:	top		an Xt widget for the top level window
 *		form		an Xt form widget
 *		edit		an Xt widget for the editing window
 *		children	list of child widgets
 *		num_children	number of children widgets
 *		ew		the editing window descriptor
 *		sw		the search and replace dialog box
 *		load		the dialog box for loading a file
 *		save		the dialog box for saving a file
 *		msg		the message box
 *		click_x		x coordinate of previous selection event
 *		click_y		y coordinate of previous selection event
 *		click_pos	text position of previous selection click 
 *				(not paint)
 *		click_count	number of clicks on same location
 */
struct win {
	Widget top;
	Widget form;
	Widget edit;
	Widget info;
	Widget vscrollbar;
	Widget hscrollbar;
	struct menu_buf *bufmenu;
#define MAX_CHILDREN 256
	Widget children[MAX_CHILDREN];
	int num_children;
	struct editwin *ew;
	struct searchwin *sw;
	struct filewin *fwtab[WIN_FILEWINS];
	struct msgwin *msg;
	struct endprompt *endprompt;
	GC black_on_white;
	GC white_on_black;
	int title_has_dirty;
	struct status status;
	long row1, col1;
	long row2, col2;
	long erow, epos;
	int click_x, click_y;
	long click_pos;
	int click_count;
};



/*
 * Variable:	window_list
 * Purpose:	A list of all currently existing top level windows.
 * Description:	All windows that are created are put into this list.
 *		It is needed when we need to convert an Xt widget
 *		identifier to our internal data structures; see win_find.
 *
 *		The list is updated by win_create and win_destroy, and
 *		used by win_find.
 * Note:	The current implementation is a static array.  This should
 *		be fixed some time.
 */

static struct dynarr window_list;
static int num_windows = 0;
static int win_list_alloc = 0;



/*
 * Prototypes for local functions
 */

static int any_unsaved_buffers(void);
static struct win *alloc_win(void); 
static void free_win(struct win *);
static void delete_win(Widget);
static void jump_vert(Widget, XtPointer, XtPointer);
static void jump_horiz(Widget, XtPointer, XtPointer);
static void scroll_vert(Widget, XtPointer, XtPointer);
static void scroll_horiz(Widget, XtPointer, XtPointer);
static long widest_visible_line(struct win *);
static int create_x_windows(struct win *);
static int create_gc(struct win *);
static int create_menu_and_edit(struct win *);
static int create_editwin(struct win *, Sbuf *);
static int create_searchwin(struct win *);
static int create_filewin(struct win *);
static int create_msgwin(struct win *);
static int create_endprompt(struct win *);
static int realize_windows(struct win *);
static void set_status(struct win *);
static void show_errors(struct win *);



/*
 * Function:	win_create
 * Purpose:	Create a new top level window.
 * Arguments:	win	pointer to pointer to window descriptor
 *		buf	buffer that is to be displayed in window
 * Return:	-1 for error, 0 for success.
 */
int win_create(struct win **win, Sbuf *buf) {
	struct win *p;

	p = alloc_win();

	if (p != NULL) {
		p->num_children = 0;
		if (create_x_windows(p) == 0 &&
		    create_menu_and_edit(p) == 0 &&
		    realize_windows(p) == 0 &&
		    create_gc(p) == 0 &&
    		    create_editwin(p, buf) == 0 &&
		    create_searchwin(p) == 0 &&
		    create_filewin(p) == 0 &&
		    create_msgwin(p) == 0 &&
		    create_endprompt(p) == 0
		) {
			p->title_has_dirty = -1;
			p->row1 = -1;
			p->click_x = -1;
			p->click_y = -1;
			p->click_count = -1;
			p->click_pos = -1;
    			*win = p;
			status_init(&p->status, *win);
			win_set_title(p);
			return 0;
    		}
	}

	error(0, "out of memory, can't create new window");
	
	if (p != NULL)
		free_win(p);

	*win = NULL;
	return -1;
}



/*
 * Function:	win_destroy
 * Purpose:	Close a window.  If it was the last window, exit the program.
 * Arguments:	win	the window to be destroyed
 * Return:	Nothing.
 */
void win_destroy(struct win *win) {
	int i;

	if (num_windows == 1 && sbuf_is_dirty(win_buf(win)))
		endprompt_popup(win->endprompt);
	else {
		editwin_destroy(win->ew);
		menu_clear_bufs(win->bufmenu); /* fixme: memory leak? */
		searchwin_destroy(win->sw);
		for (i = 0; i < WIN_FILEWINS; ++i)
			filewin_destroy(win->fwtab[i]);
		msgwin_destroy(win->msg);
		endprompt_destroy(win->endprompt);

		free_win(win);
		XtDestroyWidget(win->top);
		if (num_windows == 0)
			exit(0);
	}
}



/*
 * Function:	win_destroy_all
 * Purpose:	Close all windows.
 * Arguments:	win	current window
 * Return:	-1 for failure (unsaved edits), 0 for success.
 */
int win_destroy_all(struct win *win) {
	if (any_unsaved_buffers()) {
		endprompt_popup(win->endprompt);
		return -1;
	}

	while (num_windows > 0)
		free_win(win_ith_win(0));

	return 0;
}



/*
 * Function:	win_cache_stats
 * Purpose:	Write out cache stats for all buffers.
 */
void win_cache_stats(void) {
	Sbuf *buf, *first;

	buf = first = buflist_first();
	do {	
		sbuf_cache_stats(buf, stdout);
		buf = buflist_next(buf);
	} while (buf != first);
}



/*
 * Function:	win_build_buf_menu
 * Purpose:	Re-create the buffer menu for a window.
 * Arguments:	win	the window
 * Return:	-1 for failure, 0 for success.
 */
int win_build_buf_menu(struct win *win) {
	Sbuf *this, *buf;

	if (menu_clear_bufs(win->bufmenu) == -1)
		return -1;

	buf = this = buflist_first();
	if (this == NULL) {
		error(win, "List of buffers is empty, can't build menu.");
		return -1;
	}

	do {
		if (menu_add_buf(win->bufmenu, buf) == -1)
			return -1;
		buf = buflist_next(buf);
	} while (buf != this);

	menu_done_adding(win->bufmenu);

	return 0;
}



/*
 * Function:	win_set_buf
 * Purpose:	Set the buffer shown in a window.
 * Arguments:	win	the window
 *		buf	the new buffer
 * Return:	-1 for failure, 0 for success.
 */
int win_set_buf(struct win *win, Sbuf *buf) {
	if (!sbuf_has_flags(buf, SBUF_LOADED_FLAG))
		if (sbuf_load(buf) == -1)
			return -1;
	if (editwin_set_buf(win->ew, buf) == -1)
		return -1;
	win->click_pos = -1;
	win->click_count = 0;
	win_set_title(win);
	return 0;
}



/*
 * Function:	win_change_buf_all
 * Purpose:	Change the buffer to in all windows that show a given one.
 * Arguments:	orig	original buffer
 *		repl	replacement buffer
 * Return:	-1 for failure, 0 for success.
 */
int win_change_buf_all(Sbuf *orig, Sbuf *repl) {
	int i;
	struct win *win;

	assert(repl != NULL);
	for (i = 0; i < num_windows; ++i) {
		win = win_ith_win(i);
		if (win != NULL && win_buf(win) == orig)
			if (win_set_buf(win, repl) == -1)
				return -1;
	}

	if (editwin_all_forget_buf(orig) == -1) {
		error(NULL, "Error in buffer killing logic!");
		return -1;
	}
	
	return 0;
}



/*
 * Function:	win_add_child
 * Purpose:	Add a new widget to the list of child widgets
 * Arguments:	win	the window
 *		child	the child widget
 * Return:	Nothing.
 */
void win_add_child(struct win *win, Widget child) {
	if (win->num_children < MAX_CHILDREN)
		win->children[win->num_children++] = child;
}



/*
 * Function:	win_force_update
 * Purpose:	Update all parts of a top level window, even if not changed.
 * Arguments:	win	the window to be updated
 * Return:	nothing
 */
void win_force_update(struct win *win) {
	char *p;
	int rows, cols;

	win_get_dimensions(win, &rows, &cols);
	editwin_force_update(win->ew, rows, cols);
	p = next_error(win);
	if (p != NULL)
		win_set_msg(win, p);

	show_errors(win);
	set_status(win);
}



/*
 * Function:	win_update
 * Purpose:	Update changed parts of a top level window.
 * Arguments:	win	the window to be updated
 * Return:	nothing
 */
void win_update(struct win *win) {
	int rows, cols;

	win_get_dimensions(win, &rows, &cols);
	editwin_update(win->ew, rows, cols);
	if (win->title_has_dirty != sbuf_is_dirty(win_buf(win)))
		win_set_title(win);
	show_errors(win);
	set_status(win);
	win_set_scrollbar(win);
}



/*
 * Function:	win_update_all
 * Purpose:	Update all windows.
 * Arguments:	none.
 * Return:	nothing.
 */
void win_update_all(void) {
	int i;
	struct win *win;

	for (i = 0; i < num_windows && !x_pending(); ++i) {
		win = win_ith_win(i);
		if (win != NULL)
			win_update(win);
	}
	if (i < num_windows)
		x_timeout(1000, win_update_all);
}



/*
 * Function:	win_find
 * Purpose:	Find a top level window, given an Xt widget.
 * Arguments:	widget	the X11 widget
 * Return:	Pointer to the descriptor for the window, if found, or NULL
 *		if not found.
 * Description: The action functions receive an Xt widget to identify
 *		which window they need to act on.  win_find converts the
 *		widget to a struct win.
 */
struct win *win_find(Widget widget) {
	int i, j;
	struct win *win;

	for (i = 0; i < num_windows; ++i) {
		win = win_ith_win(i);
		if (win == NULL)
			continue;
		if (win->top == widget || 
		    win->form == widget || 
		    win->edit == widget ||
		    searchwin_contains(win->sw, widget) ||
		    msgwin_contains(win->msg, widget))
			return win;
		for (j = 0; j < WIN_FILEWINS; ++j)
			if (filewin_contains(win->fwtab[j], widget))
				return win;
		for (j = 0; j < win->num_children; ++j)
			if (win->children[j] == widget)
				return win;
	}
	return NULL;
}



/*
 * Function:	win_find_by_buf
 * Purpose:	Find a top level window, given a buffer.
 * Arguments:	buf	the buffer
 * Return:	Pointer to the descriptor for the window, if found, or NULL
 *		if not found.
 */
struct win *win_find_by_buf(Sbuf *buf) {
	int i;
	struct win *win;

	for (i = 0; i < num_windows; ++i) {
		win = win_ith_win(i);
		if (win != NULL && win_buf(win) == buf)
			return win;
	}
	return NULL;
}



/*
 * Function:	win_get_dimensions
 * Purpose:	Return the width and height (in characters) of a window
 *		(the editwin part)
 * Arguments:	win	pointer to the window
 *		rows	pointer to integer where number of rows is stored
 *		cols	pointer to integer where number of columns is stored
 * Return:	nothing
 */
void win_get_dimensions(struct win *win, int *rows, int *cols) {
	assert(win != NULL);
	assert(rows != NULL);
	assert(cols != NULL);
	
	if (win->ew->rows == -1 || win->ew->cols == -1) {
		unsigned w, h;
		x_window_dimensions(win->ew->window, &w, &h);
		win->ew->rows = (int) h / font_height(win->ew->font);
		win->ew->cols = (int) w / font_width(win->ew->font);
	}

	assert(win->ew->rows >= 0);
	assert(win->ew->cols >= 0);
	*rows = win->ew->rows;
	*cols = win->ew->cols;
}


/*
 * Function:	win_resize
 * Purpose:	Resize a window.
 * Arguments:	win	pointer to window to be resized
 * Return:	nothing.
 * Note:	This function must be called by the action function when
 *		the window changes its size.  It further calls 
 *		editwin_set_dimensions.
 */
void win_resize(struct win *win) {
	win->ew->rows = -1;
	editwin_dirtify(win->ew);
}



/*
 * Function:	win_get_top
 * Purpose:	Return topmost visible position.
 * Arguments:	win	the window that shows the buffer
 * Return:	The position.
 */
long win_get_top(struct win *win) {
	return sbuf_mark_begin(win->ew->buf_current->top_mark);
}



/*
 * Function:	win_get_bottom
 * Purpose:	Return bottommost visible position.
 * Arguments:	win	the window that shows the buffer
 * Return:	The position.
 */
long win_get_bottom(struct win *win) {
	Sbuf *buf;
	long pos;
	int rows, cols;

	buf = win->ew->buf_current->buf;
	pos = win_get_top(win);
	win_get_dimensions(win, &rows, &cols);
	while (--rows >= 1)
		pos = sbuf_eoln(buf, pos);
	return pos;
}



/*
 * Function:	win_set_top
 * Purpose:	Move window to new position in text.
 * Arguments:	win	the window that shows the buffer
 *		pos	new topmost visible position
 * Return:	Nothing.
 */
void win_set_top(struct win *win, long pos) {
	Sbuf *buf;

	buf = win->ew->buf_current->buf;
	sbuf_remark(win->ew->buf_current->top_mark, sbuf_boln(buf, pos), 0);
#if 0
	dirtify(ew);
#else
	win->ew->num_marks = -1;
#endif
}



/*
 * Function:	win_get_left
 * Purpose:	Return first visible column.
 * Arguments:	win	the window
 * Return:	The number of the first visible column.  First column is 0.
 */
long win_get_left(struct win *win) {
	return win->ew->buf_current->first_col;
}



/*
 * Function:	win_set_left
 * Purpose:	Set first visible column.
 * Arguments:	win	the window
 *		col	the new first visible column
 * Return:	Nothing.
 */
void win_set_left(struct win *win, long col) {
	if (col >= 0)
		win->ew->buf_current->first_col = col;
}



/*
 * Function:	win_set_title
 * Purpose:	Update the window and icon titles (shown by window manager).
 * Arguments:	win	pointer to window
 * Return:	nothing.
 */
void win_set_title(struct win *win) {
	char *s;
	char title_buf[1024];
	char icon_buf[1024];
	Sbuf *buf;

	buf = win_buf(win);
	s = sbuf_get_name(buf);
	if (s == NULL)
		s = "(untitled)";
	sprintf(title_buf, "SeX: %.*s", 512, s);
	sprintf(icon_buf, "%.*s", 512, s);

	if (sbuf_is_dirty(buf)) {
		strcat(title_buf, " (modified)");
		strcat(icon_buf, " ***");
	}

	XtVaSetValues(win->top, 
		XtNtitle, title_buf, 
		XtNiconName, icon_buf, 
		(XtPointer) 0);

	win->title_has_dirty = sbuf_is_dirty(buf);
}



/*
 * Function:	win_selection
 * Purpose:	Return handle to current selection in a window.
 * Arguments:	win	window to return selection for
 * Return:	the handle to the selection
 */
Sbufmark *win_selection(struct win *win) {
	return win->ew->buf_current->selection;
}



/*
 * Function:	win_show
 * Purpose:	Make sure a given position in text is visible in window.
 * Arguments:	win	the window
 *		pos	the position
 */
void win_show(struct win *win, long pos) {
	int rows, cols;
	Sbuf *buf;
	long i, p, col, buflen;

	win_get_dimensions(win, &rows, &cols);
	buf = win->ew->buf_current->buf;
	p = sbuf_mark_begin(win->ew->buf_current->top_mark);
	
	/* Fix vertical location */
	if (pos < p) {
		sbuf_remark(win->ew->buf_current->top_mark, sbuf_boln(buf, pos),0);
		editwin_dirtify(win->ew);
	} else {
		buflen = sbuf_length(buf);
		for (i = 0; i < rows && p < buflen; ++i)
			p = sbuf_eoln(buf, p);
			
		/* The following if-condition tests whether the position is
		 * after the last visible line.  It takes into account the
 		 * situation where the position is at the end of the file and
 		 * the last line does not end in a newline (this is the second
 		 * line in the condition).  It's a bit messy, but you can
 		 * probably figure it out, eventually.
 		 */
		if (i == rows && pos >= p && 
		    !(pos==p && p==buflen && p>0 && sbuf_charat(buf,p-1)!='\n'))
		{
			for (p = pos; p > 0 && i > 1; --i)
				p = sbuf_boln(buf, p-1);
			sbuf_remark(win->ew->buf_current->top_mark, p, 0);
			editwin_dirtify(win->ew);
		}
	}

	/* Fix horizontal location */
	col = sbuf_colno(buf, pos, tab_width());
	if (win->ew->buf_current->first_col > col)
		win->ew->buf_current->first_col = col;
	else if (win->ew->buf_current->first_col <= col - cols)
		win->ew->buf_current->first_col = col - cols + 1;
}



/*
 * Function:	win_pixel_to_text
 * Purpose:	Convert a pixel position to text position.
 * Arguments:	win	the window
 *		x, y	the pixel position
 * Return:	Text position (the position _before_ the character).
 */
long win_pixel_to_text(struct win *win, int x, int y) {
	long pos, col;
	Sbuf *buf;
	int c;

	x /= font_width(win->ew->font);
	y /= font_height(win->ew->font);
	
	buf = win->ew->buf_current->buf;
	for (pos = sbuf_mark_begin(win->ew->buf_current->top_mark); y > 0; --y)
		pos = sbuf_eoln(buf, pos);

	col = 0;
	while (col < x + win->ew->buf_current->first_col) {
		c = sbuf_charat(buf, pos);
		if (c == '\t')
			col = tab_next(col);
		else if (c == '\n' || c == EOF)
			break;
		else
			++col;
		++pos;
	}
	return pos;
}



/*
 * Function:	win_editwin
 * Purpose:	Return handle to editing window in a window.
 * Arguments:	win	window to return editing window for
 * Return:	the handle to the editing window
 */
struct editwin *win_editwin(struct win *win) {
	return win->ew;
}



/*
 * Function:	win_searchwin
 * Purpose:	Return handle to search and replace dialog for a window.
 * Arguments:	win	the window
 * Return:	the handle to the dialog
 */
struct searchwin *win_searchwin(struct win *win) {
	return win->sw;
}



/*
 * Function:	win_filewin
 * Purpose:	Return handle to a filename dialog.
 * Arguments:	win	the window
 *		fwid	which filewin dialog
 * Return:	the handle to the dialog
 */
struct filewin *win_filewin(struct win *win, enum win_filewin fwid) {
	assert(fwid >= 0 && fwid < WIN_FILEWINS);
	return win->fwtab[fwid];
}



/*
 * Function:	win_buf
 * Purpose:	Return handle to buffer in a window.
 * Arguments:	win	window to return buffer for
 * Return:	the handle to the buffer
 */
Sbuf *win_buf(struct win *win) {
	return win->ew->buf_current->buf;
}



/*
 * Function:	win_toplevel
 * Purpose:	Return top level widget for window.
 * Arguments:	win	the window
 * Return:	The widget.
 */
Widget win_toplevel(struct win *win) {
	return win->top;
}



/*
 * Function:	win_set_filename
 * Purpose:	Set the name of the buffer displayed in a window.
 * Arguments:	win	the window
 *		name	the new name
 * Return:	-1 for failure, 0 for success.
 */
int win_set_filename(struct win *win, char *name) {
	Sbuf *buf;

	buf = win_buf(win);
	if (sbuf_set_name(buf, name) == -1)
		return -1;
	buflist_sort();
	win_set_title(win);
	return 0;
}



/*
 * Function:	win_set_msg
 * Purpose:	Set message at bottom of window.
 * Arguments:	win	the window
 *		msg	the message
 * Return:	Nothing.
 */
void win_set_msg(struct win *win, const char *msg) {
	msgwin_set(win->msg, msg);
	msgwin_popup(win->msg);
}



/*
 * Function:	win_popdown_msg
 * Purpose:	Remove (hide) message box.
 * Arguments:	win	the window
 * Return:	Nothing.
 */
void win_popdown_msg(struct win *win) {
	msgwin_popdown(win->msg);
}



/*
 * Function: win_set_scrollbar
 * Purpose: Update scrollbar visually.
 */
void win_set_scrollbar(struct win *win) {
	float top, shown;
	Arg arg[2];
	XtArgVal *dummy[2];
	Sbuf *buf;
	long topline, leftcol, total;
	int rows, cols;
	
	buf = win_buf(win);
	win_get_dimensions(win, &rows, &cols);
	dummy[0] = (XtArgVal *) &top;
	dummy[1] = (XtArgVal *) &shown;

	topline = sbuf_lineno(buf, win_get_top(win));
	total = sbuf_lineno(buf, sbuf_length(buf));
	if (rows > total - topline)
		rows = total - topline;
	if (total > 0) {
		top = (float) topline / total;
		shown = (float) rows / total;
	} else {
		top = 0.0;
		shown = 1.0;
	}

	XtSetArg(arg[0], XtNtopOfThumb, *dummy[0]);
	XtSetArg(arg[1], XtNshown, *dummy[1]);
	XtSetValues(win->vscrollbar, &arg[0], 2);

	leftcol = win_get_left(win);
	total = widest_visible_line(win);
	if (cols > total - leftcol)
		cols = total - leftcol;

	if (total > 0) {
		top = (float) leftcol / total;
		shown = (float) cols / total;
	} else {
		top = 0.0;
		shown = 1.0;
	}
	
	XtSetArg(arg[0], XtNtopOfThumb, *dummy[0]);
	XtSetArg(arg[1], XtNshown, *dummy[1]);
	XtSetValues(win->hscrollbar, &arg[0], 2);
}



/*
 * Function:	win_start_selection
 * Purpose:	Handle clicks at the same location.
 * Arguments:	win	the window clicked in
 *		x,y	the pixel position clicked in
 * Return:	Nothing.
 */
void win_start_selection(struct win *win, int x, int y) {
	long textpos, p1, p2;
	Sbuf *buf;

	anchor_up(win);
	if (x != win->click_x || y != win->click_y || win->click_count == -1)
		win->click_count = 0;
	else
		win->click_count = (win->click_count + 1) % 3;
		
	textpos = win_pixel_to_text(win, x, y);
	buf = win_buf(win);
		
	switch (win->click_count) {
	default:	/* this is an error! */
		error(0, "click_count is wrong, setting it to zero");
		win->click_count = 0;
	case 0:
		p1 = textpos;
		p2 = p1;
		break;
	case 1:
		p1 = sbuf_find_pair(buf, textpos);
		if (p1 != -1) {
			if (p1 < textpos)
				p2 = textpos + 1;
			else {
				p2 = p1 + 1;
				p1 = textpos;
			}
		} else {
			p1 = sbuf_bow(buf, textpos);
			p2 = sbuf_eow(buf, textpos);
		}
		break;
	case 2:
		p1 = sbuf_boln(buf, textpos);
		p2 = sbuf_eoln(buf, textpos);
		break;
	}
	sbuf_remark(win_selection(win), p1, p2-p1);
	sbuf_mark_set_columnar(win_selection(win), 0);
	win->click_x = x;
	win->click_y = y;
	win->click_pos = textpos;
}



/*
 * Function:	win_continue_selection
 * Purpose:	Handle "painting" with the mouse.
 * Arguments:	win	the editing window
 *		x,y	the current pixel position of the mouse
 * Return:	Nothing.
 */
void win_continue_selection(struct win *win, int x, int y) {
	long textpos, p1, p2;
	Sbuf *buf;

	anchor_up(win);
	if (win->click_pos == -1) {
		error(0, "click_pos is uninitialized, ignoring event");
		return;
	}

	textpos = win_pixel_to_text(win, x, y);
	buf = win_buf(win);
		
	switch (win->click_count) {
	default:	/* this is an error! */
		error(0, "click_count is wrong, setting it to zero");
		win->click_count = 0;
	case 0:
		if (textpos < win->click_pos) {
			p1 = textpos;
			p2 = win->click_pos;
		} else {
			p1 = win->click_pos;
			p2 = textpos;
			if (p2 < sbuf_length(buf))
				++p2;
		}
		break;
	case 1:
		if (textpos < win->click_pos) {
			p1 = sbuf_bow(buf, textpos);
			p2 = sbuf_eow(buf, win->click_pos);
		} else {
			p1 = sbuf_bow(buf, win->click_pos);
			p2 = sbuf_eow(buf, textpos);
		}
		break;
	case 2:
		if (textpos < win->click_pos) {
			p1 = sbuf_boln(buf, textpos);
			p2 = sbuf_eoln(buf, win->click_pos);
		} else {
			p1 = sbuf_boln(buf, win->click_pos);
			p2 = sbuf_eoln(buf, textpos);
		}
		break;
	}
	sbuf_remark(win_selection(win), p1, p2-p1);
	win->click_x = x;
	win->click_y = y;
}



/*
 * Function:	win_extend_selection
 * Purpose:	Handle clicks with mouse button 3.
 * Arguments:	win	the editing window
 *		x,y	the current pixel position of the mouse
 * Return:	Nothing.
 */
void win_extend_selection(struct win *win, int x, int y) {
	long begin, middle, end, len, textpos, p1, p2;
	Sbuf *buf;
	Sbufmark *sel;

	anchor_up(win);
	sel = win_selection(win);
	begin = sbuf_mark_begin(sel);
	end = sbuf_mark_end(sel);
	middle = begin + (end - begin) / 2;
	textpos = win_pixel_to_text(win, x, y);
	buf = win_buf(win);
	len = sbuf_length(buf);
		
	switch (win->click_count) {
	default:	/* this is NOT an error! */
		win->click_count = 0;
	case 0:
		if (textpos < middle) {
			p1 = textpos;
			p2 = end;
		} else {
			p1 = begin;
			p2 = textpos;
		}
		break;
	case 1:
		if (textpos < middle) {
			p1 = sbuf_bow(buf, textpos);
			p2 = end;
		} else {
			p1 = begin;
			p2 = sbuf_eow(buf, textpos);
		}
		break;
	case 2:
		if (textpos < middle) {
			p1 = sbuf_boln(buf, textpos);
			p2 = end;
		} else {
			p1 = begin;
			p2 = sbuf_eoln(buf, textpos);
		}
		break;
	}
	sbuf_remark(win_selection(win), p1, p2-p1);
	win->click_x = x;
	win->click_y = y;
}



/*
 * Function:	win_ith_win
 * Purpose:	Return pointer to ith window
 */
struct win *win_ith_win(int i) {
	struct win **list;
	int j;
	
	assert(win_list_alloc);

	list = window_list.data;
	for (j = 0; j < window_list.used; ++j) {
		if (list[j] != NULL) {
			if (i == 0)
				break;
			--i;
		}
	}
	if (j < window_list.used)
		return list[j];
	return NULL;
}



/**********************************************************************
 * Local functions follow                                             *
 **********************************************************************/
 

/*
 * Function:	alloc_win
 * Purpose:	Allocate a new entry in window_list.
 * Return:	NULL for error, else pointer to struct window.
 */
static struct win *alloc_win(void) {
	struct win **list;
	int i;
	
	if (!win_list_alloc) {
		dynarr_init(&window_list, sizeof(struct win *));
		win_list_alloc = 1;
	}
	
	list = window_list.data;
	for (i = 0; i < window_list.used && list[i] != NULL; ++i)
		continue;
	if (i == window_list.used) {
		if (dynarr_resize(&window_list, window_list.used + 1) == -1)
			return NULL;
		++window_list.used;
	}

	list = window_list.data;
	list[i] = malloc(sizeof(struct win));
	if (list[i] != NULL)
		++num_windows;
	return list[i];
}



/*
 * Function:	free_win
 * Purpose:	Free entry in window_list.
 */
static void free_win(struct win *win) {
	struct win **list;
	int i;

	assert(win_list_alloc);
	
	list = window_list.data;
	for (i = 0; i < window_list.used && list[i] != win; ++i)
		continue;
	if (i < window_list.used) {
		list[i] = NULL;
		--num_windows;
	}
#if 0 /* FIXME: this is a memory leak, but we can't free it here, because
 	this win will still be used; use a periodic garbage collector? */
	free(win);
#endif
}



/*
 * Function:	create_x_windows
 * Purpose:	Create the X windows and subwindows we need.
 * Arguments:	win	pointer to the window descriptor we're creating
 * Return:	-1 for error, 0 for success.
 * Note:	This function is called by win_create.
 */
static int create_x_windows(struct win *win) {
	win->top = x_create_top_level();
	
	win->form = XtVaCreateManagedWidget("form", formWidgetClass,
		win->top, XtNallowVert, True, XtNdefaultDistance, 0, NULL);

	return 0;
}



/*
 * Function:	create_gc
 * Purpose:	Create the graphics contexts needed by the window
 * Arguments:	win	pointer to the window
 * Return:	-1 for failure, 0 for success.
 */
static int create_gc(struct win *win) {
	Screen *scr;
	unsigned long black, white;
	struct font *font;
	
	scr = XtScreen(win->top);
	black = BlackPixelOfScreen(scr);
	white = WhitePixelOfScreen(scr);
	font = font_get_default();
	
	win->black_on_white = x_create_gc(win->top, font, black, white);
	win->white_on_black = x_create_gc(win->top, font, white, black);
	
	return 0;
}




/*
 * Function:	create_editwin
 * Purpose:	Create the editwin that belongs to a top level window.
 * Arguments:	but	widget (menu button) below which the editing window
 *			is located
 *		win	pointer to the top level window
 *		buf	the initial editing buffer
 * Return:	-1 for failure, 0 for success.
 */
static int create_editwin(struct win *win, Sbuf *buf) {
	return editwin_create(&win->ew, win->edit, win->black_on_white,
				win->white_on_black, buf);
}



/*
 * Function:	create_menu_and_edit
 * Purpose:	Create the menus for a top level window and the edit widget.
 * Arguments:	win	pointer to the top level window
 * Return:	-1 for failure, 0 for success.
 */
static int create_menu_and_edit(struct win *win) {
	int ret;
	Widget but;
	const int width = 480;
	const int height = 300;
	XtCallbackRec jump_callbacks[] = {
		{ jump_vert, NULL },
		{ NULL, NULL },
	};
	XtCallbackRec scroll_callbacks[] = {
		{ scroll_vert, NULL },
		{ NULL, NULL },
	};

	ret = menu_add(&win->bufmenu, &but, win, win->form);
	win->edit = XtVaCreateManagedWidget(
		"text_window",
		coreWidgetClass,
		win->form,
		XtNwidth, width,
		XtNheight, height,
		XtNleft, XtChainLeft,
		XtNright, XtChainRight,
		XtNtop, XtChainTop,
		XtNbottom, XtChainBottom,
		XtNhorizDistance, 0,
		XtNfromVert, but,
		NULL);

	jump_callbacks[0].closure = win;
	scroll_callbacks[0].closure = win;
	win->vscrollbar = XtVaCreateManagedWidget(
		"verticalscrollbar",
		scrollbarWidgetClass,
		win->form,
		XtNfromHoriz, win->edit,
		XtNfromVert, but,
		XtNlength, height,
		XtNthickness, 15,
		XtNjumpProc, jump_callbacks,
		XtNscrollProc, scroll_callbacks,
		XtNleft, XtChainRight,
		XtNright, XtChainRight,
		XtNtop, XtChainTop,
		XtNbottom, XtChainBottom,
		NULL);

	jump_callbacks[0].callback = jump_horiz;
	scroll_callbacks[0].callback = scroll_horiz;
	win->hscrollbar = XtVaCreateManagedWidget(
		"horizontalscrollbar",
		scrollbarWidgetClass,
		win->form,
		XtNfromVert, win->edit,
		XtNorientation, XtorientHorizontal,
		XtNlength, width,
		XtNthickness, 15,
		XtNjumpProc, jump_callbacks,
		XtNscrollProc, scroll_callbacks,
		XtNleft, XtChainLeft,
		XtNright, XtChainRight,
		XtNtop, XtChainBottom,
		XtNbottom, XtChainBottom,
		NULL);

	win->info = XtVaCreateManagedWidget(
		"msg_box",
		labelWidgetClass,
		win->form,
		XtNlabel, "123:00 - 999:99",
		XtNwidth, 480,
		XtNleft, XtChainLeft,
		XtNright, XtChainRight,
		XtNtop, XtChainBottom,
		XtNbottom, XtChainBottom,
		XtNfromVert, win->hscrollbar,
		XtNborderWidth, 0,
		XtNjustify, XtJustifyLeft,
		NULL);

	return ret;
}



/*
 * Function:	create_searchwin
 * Purpose:	Create the searchwin that belongs to a top level window.
 * Arguments:	win	pointer to the top level window
 * Return:	-1 for failure, 0 for success.
 */
static int create_searchwin(struct win *win) {
	return searchwin_create(&win->sw, win);
}



/*
 * Function:	create_filewin
 * Purpose:	Create the filename dialog that belongs to a top level window.
 * Arguments:	win	pointer to the top level window
 * Return:	-1 for failure, 0 for success.
 */
static int create_filewin(struct win *win) {
	static struct {
		int i;
		char *p1, *p2;
		cmd_function *cmd;
	} tab[] = {
		{ WIN_LOAD, "Load which file?", "Filename:", cmd_set_name_and_load_file },
		{ WIN_SAVE, "Save file as?", "Filename:", cmd_set_name_and_save_file },
		{ WIN_INSERT, "Insert file?", "Filename:", cmd_insert_file },
		{ WIN_WRITE_TO, "Write selection to?", "Filename:", cmd_write_to },
		{ WIN_PIPE, "Pipe selection through command?", "Command:", cmd_pipe },
		{ WIN_LINENO, "Go to line number?", "Line:", cmd_goto_line },
	};
	int i;
	
	for (i = 0; i < sizeof(tab)/sizeof(*tab); ++i)
		if (filewin_create(&win->fwtab[tab[i].i], win,
		                   tab[i].p1, tab[i].p2, tab[i].cmd) == -1)
			return -1;
	
	return 0;
}



/*
 * Function:	create_msgwin
 * Purpose:	Create the message box that belongs to a top level window.
 * Arguments:	win	pointer to the top level window
 * Return:	-1 for failure, 0 for success.
 */
static int create_msgwin(struct win *win) {
	return msgwin_create(&win->msg, win);
}



/*
 * Function:	create_endprompt
 * Purpose:	Create the endprompt that belongs to a top level window.
 * Arguments:	win	pointer to the top level window
 * Return:	-1 for failure, 0 for success.
 */
static int create_endprompt(struct win *win) {
	return endprompt_create(&win->endprompt, win);
}



/*
 * Function:	realize_windows
 * Purpose:	Realize the windows.
 * Arguments:	win	pointer to the top level window
 * Return:	-1 for failure, 0 for success.
 */
static int realize_windows(struct win *win) {
	XtRealizeWidget(win->top);
	x_set_wm_protocols(win->top, delete_win);
	XtSetKeyboardFocus(win->top, win->edit);
	return 0;
}



/*
 * Function:	delete_win
 * Purpose:	Delete a top level window as a reaction to WM_DELETE.
 * Arguments:	wid	the widget for the window
 * Return:	Nothing.
 */
static void delete_win(Widget wid) {
	struct win *win;

	win = win_find(wid);
	if (win != NULL)
		win_destroy(win);
	else
		error(0, "can't find window");
}



/*
 * Function:	jump_vert
 * Purpose:	Callback function for vertical scrollbar.
 */
static void jump_vert(Widget w, XtPointer client_data, XtPointer call_data) {
	float thumb;
	struct win *win;
	Sbuf *buf;
	long pos;
	
	thumb = *(float *) call_data;
	win = client_data;
	buf = win_buf(win);
	pos = sbuf_boln(buf, thumb * sbuf_length(buf));
	win_set_top(win, pos);
	win_update_all();
}



/*
 * Function:	scroll_vert
 * Purpose:	Callback function for vertical scrollbar.
 */
static void scroll_vert(Widget w, XtPointer client_data, XtPointer call_data) {
	int delta, rows, cols, lines;
	Dimension height;
	struct win *win;
	Sbuf *buf;
	long length, pos;
	
	delta = (int) call_data;
	win = client_data;
	buf = win_buf(win);

	win_get_dimensions(win, &rows, &cols);
	XtVaGetValues(win->vscrollbar, XtNlength, &height, NULL);
	lines = (int) ((double) rows * delta / height);

	pos = win_get_top(win);	
	length = sbuf_length(buf);
	if (lines > 0) {
		while (lines-- > 0 && pos < length)
			pos = sbuf_eoln(buf, pos);
	} else {
		while (lines++ < 0 && pos > 0)
			pos = sbuf_boln(buf, pos - 1);
	}
	win_set_top(win, pos);
	
	win_update_all();
}




/*
 * Function:	jump_horiz
 * Purpose:	Callback function for horizontal scrollbar.
 */
static void jump_horiz(Widget w, XtPointer client_data, XtPointer call_data) {
	float thumb;
	struct win *win;
	Sbuf *buf;
	long leftcol;
	
	thumb = *(float *) call_data;
	win = client_data;
	buf = win_buf(win);
	leftcol = thumb * widest_visible_line(win);
	win_set_left(win, leftcol);
	win_update_all();
}



/*
 * Function:	scroll_horiz
 * Purpose:	Callback function for horizontal scrollbar.
 */
static void scroll_horiz(Widget w, XtPointer client_data, XtPointer call_data) {
	int delta, rows, cols, coldelta;
	Dimension width;
	struct win *win;
	long leftcol, max_width;
	
	delta = (int) call_data;
	win = client_data;

	win_get_dimensions(win, &rows, &cols);
	XtVaGetValues(win->hscrollbar, XtNlength, &width, NULL);
	coldelta = (int) ((double) cols * delta / width);

	leftcol = win_get_left(win) + delta;
	max_width = widest_visible_line(win);
	if (max_width < cols)
		max_width = 0;
	else
		max_width -= cols;
	if (leftcol < 0)
		leftcol = 0;
	if (leftcol > max_width)
		leftcol = max_width;
	win_set_left(win, leftcol);
	
	win_update_all();
}




/*
 * Function:	show_errors
 * Purpose:	Popup the message window to show any error messages.
 * Arguments:	win	the window
 * Return:	Nothing.
 */
static void show_errors(struct win *win) {
	char *p;

	p = next_error(win);
	if (p != NULL)
		win_set_msg(win, p);
}



/*
 * Function:	set_status
 * Purpose:	Set message to the selection area.
 * Arguments:	win	the window
 * Return:	Nothing.
 */
static void set_status(struct win *win) {
	char *p;

	if (status_get(&win->status, &p) == 1)
		XtVaSetValues(win->info, XtNlabel, p, NULL);
}



static int any_unsaved_buffers(void) {
	Sbuf *buf, *first;
	
	buf = first = buflist_first();
	if (buf != NULL) {
		do {
			if (sbuf_has_flags(buf, SBUF_DIRTY_FLAG))
				return 1;
			buf = buflist_next(buf);
		} while (buf != first);
	}
	return 0;
}



static long widest_visible_line(struct win *win) {
	Sbuf *buf;
	int i, rows, cols;
	long pos, next_line, max;

	buf = win_buf(win);	
	win_get_dimensions(win, &rows, &cols);
	pos = win_get_top(win);
	max = 0;
	for (i = 0; i < rows; ++i) {
		next_line = sbuf_eoln(buf, pos);
		if (next_line - pos > max)
			max = next_line - pos;
		pos = next_line;
	}
	return max;
}
