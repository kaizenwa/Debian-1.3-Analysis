/*
 * File:	cmd-move.c
 * Purpose:	Functions that implement moving commands.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd-move.c,v 1.28 1996/12/07 16:28:23 liw Exp $"
 */

#include <ctype.h>
#include <stdlib.h>
#include <publib.h>

#include "anchor.h"
#include "cmd.h"
#include "filewin.h"
#include "selections.h"
#include "win.h"
#include "x.h"
#include "error.h"
#include "tab.h"



/*
 * Prototypes for local functions.
 */
static int goto_mark(struct win *, int);
static int set_mark(struct win *, int);
static int line_is_empty(Sbuf *, long);



/*
 * Function:	cmd_forward
 * Purpose:	Move selection to next character.
 */
int cmd_forward(struct win *win) {
	Sbufmark *sel;
	long pos;

	cmd_prev_was_cut = 0;	
	anchor_up(win);
	sel = win_selection(win);
	pos = sbuf_mark_end(sel);
	if (sbuf_mark_length(sel) == 0) {
		if (pos == sbuf_length(win_buf(win)))
			return -1;
		++pos;
	}
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_backward
 * Purpose:	Move selection to previous character.
 */
int cmd_backward(struct win *win) {
	Sbufmark *sel;
	long pos;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	sel = win_selection(win);
	pos = sbuf_mark_begin(sel);
	if (sbuf_mark_length(sel) == 0) {
		if (pos == 0)
			return -1;
		--pos;
	}
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_next_line
 * Purpose:	Move selection to previous line.
 */
int cmd_next_line(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long col, pos;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	sel = win_selection(win);
	pos = sbuf_mark_end(sel);
	if (sbuf_mark_length(sel) == 0) {
		buf = win_buf(win);
		col = sbuf_colno(buf, pos, tab_width());
		pos = sbuf_eoln(buf, pos);
		pos = sbuf_colpos(buf, pos, col, tab_width());
	}
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_prev_line
 * Purpose:	Move selection to previous line.
 */
int cmd_prev_line(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long col, pos;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	sel = win_selection(win);
	pos = sbuf_mark_begin(sel);
	if (sbuf_mark_length(sel) == 0) {
		buf = win_buf(win);
		col = sbuf_colno(buf, pos, tab_width());
		pos = sbuf_boln(buf, pos);
		if (pos > 0)
			pos = sbuf_boln(buf, pos-1);
		pos = sbuf_colpos(buf, pos, col, tab_width());
	}
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_goto_boln
 * Purpose:	Move selection to beginning of line.
 */
int cmd_goto_boln(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	buf = win_buf(win);
	sel = win_selection(win);
	pos = sbuf_mark_begin(sel);
	pos = sbuf_boln(buf, pos);
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_goto_eoln
 * Purpose:	Move selection to end of line.
 */
int cmd_goto_eoln(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos, orig;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	buf = win_buf(win);
	sel = win_selection(win);
	pos = sbuf_mark_end(sel);
	if (pos != sbuf_mark_begin(sel))
		--pos;
	if (sbuf_charat(buf, pos) != '\n') {
		orig = pos;
		pos = sbuf_eoln(buf, pos);
		if (pos > orig && (pos < sbuf_length(buf) || 
				sbuf_charat(buf, pos-1) == '\n'))
			--pos;
	}
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_next_word
 * Purpose:	Select empty string after next word after selection.
 */
int cmd_next_word(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos, newpos;
	int c;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	buf = win_buf(win);
	sel = win_selection(win);
	pos = sbuf_mark_end(sel);
	newpos = pos;
	do {
		pos = newpos++;
		c = sbuf_charat(buf, pos);
	} while (c != EOF && !(isalnum(c) || c == '_'));
	pos = sbuf_eow(buf, pos);
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_prev_word
 * Purpose:	Select empty string before word before selection.
 */
int cmd_prev_word(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos;
	int c;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	buf = win_buf(win);
	sel = win_selection(win);
	pos = sbuf_mark_begin(sel);

	if (pos == 0)
		return -1;

	--pos;
	while (pos > 0 && !isalnum(c = sbuf_charat(buf, pos)) && c != '_')
		--pos;

	pos = sbuf_bow(buf, pos);
	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_query_goto_line
 * Purpose:	Popup dialog box to let user enter line number.
 */
int cmd_query_goto_line(struct win *win) {
	struct filewin *fw;

	cmd_prev_was_cut = 0;	
	anchor_up(win);
	fw = win_filewin(win, WIN_LINENO);
	filewin_popup(fw);
	return 0;
}



/*
 * Function:	cmd_goto_line
 * Purpose:	Go to the line the user has asked for.
 */
int cmd_goto_line(struct win *win) {
	long lineno, pos;
	char *p;

	cmd_prev_was_cut = 0;	
	anchor_up(win);
	p = filewin_filename(win_filewin(win, WIN_LINENO));
	while (*p != '\0' && !isdigit(*p))
		++p;
	lineno = atol(p);
	if (lineno < 1) {
		error(win, "bad line number");
		return -1;
	}

	pos = sbuf_linepos(win_buf(win), lineno-1);

	sbuf_remark(win_selection(win), pos, 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	win_show(win, pos);

	return 0;
}



/*
 * Function:	cmd_prev_page
 * Purpose:	Select empty string at top of previous windowful.
 */
int cmd_prev_page(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos;
	int i, rows, cols;

	cmd_prev_was_cut = 0;	
	anchor_up(win);
	buf = win_buf(win);
	sel = win_selection(win);

	pos = win_get_top(win);
	if (pos > 0) {
		win_get_dimensions(win, &rows, &cols);
		for (i = 0; i < rows-2 && pos > 0; ++i)
			pos = sbuf_boln(buf, pos-1);
		win_set_top(win, pos);
	}

	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);

	return 0;
}



/*
 * Function:	cmd_next_page
 * Purpose:	Select empty string at top of next windowful.
 */
int cmd_next_page(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos;
	int i, rows, cols;

	cmd_prev_was_cut = 0;	
	anchor_up(win);
	buf = win_buf(win);
	sel = win_selection(win);

	pos = win_get_top(win);
	win_get_dimensions(win, &rows, &cols);
	for (i = 0; i < rows-2; ++i)
		pos = sbuf_eoln(buf, pos);

	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_set_top(win, pos);

	return 0;
}



/*
 * Function:	cmd_center
 * Purpose:	Scroll window so that beginning of selection is at center.
 */
int cmd_center(struct win *win) {
	Sbuf *buf;
	long pos;
	int i, rows, cols;

	cmd_prev_was_cut = 0;	
	anchor_up(win);
	buf = win_buf(win);

	pos = sbuf_mark_begin(win_selection(win));
	win_get_dimensions(win, &rows, &cols);
	for (i = rows/2; i > 0 && pos > 0; --i)
		pos = sbuf_boln(buf, pos-1);
	win_set_top(win, pos);

	return 0;
}



/*
 * Function:	cmd_scroll_prev_page
 * Purpose:	Move window one screenful backwards.
 */
int cmd_scroll_prev_page(struct win *win) {
	Sbuf *buf;
	long pos;
	int i, rows, cols;

	cmd_prev_was_cut = 0;	
	buf = win_buf(win);

	pos = win_get_top(win);
	if (pos > 0) {
		win_get_dimensions(win, &rows, &cols);
		for (i = 0; i < rows-2 && pos > 0; ++i)
			pos = sbuf_boln(buf, pos-1);
		win_set_top(win, pos);
	}

	return 0;
}



/*
 * Function:	cmd_scroll_next_page
 * Purpose:	Move window one screenful forwards.
 */
int cmd_scroll_next_page(struct win *win) {
	Sbuf *buf;
	long pos;
	int i, rows, cols;

	cmd_prev_was_cut = 0;	
	buf = win_buf(win);

	pos = win_get_top(win);
	win_get_dimensions(win, &rows, &cols);
	for (i = 0; i < rows-2; ++i)
		pos = sbuf_eoln(buf, pos);

	win_set_top(win, pos);

	return 0;
}



/*
 * Function:	cmd_scroll_left_one
 * Purpose:	Scroll window one column to the left.
 */
int cmd_scroll_left_one(struct win *win) {
	long col;
	
	col = win_get_left(win);
	if (col == 0) {
		error(win, "Can't scroll further to the left");
		return -1;
	}
	win_set_left(win, col-1);
	return 0;
}



/*
 * Function:	cmd_scroll_right_one
 * Purpose:	Scroll window one column to the right.
 */
int cmd_scroll_right_one(struct win *win) {
	win_set_left(win, win_get_left(win) + 1);
	return 0;
}



/*
 * Function:	cmd_goto_bof
 * Purpose:	Select empty string at beginning of file.
 */
int cmd_goto_bof(struct win *win) {
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	sbuf_remark(win_selection(win), 0, 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	win_show(win, 0);
	return 0;
}



/*
 * Function:	cmd_goto_eof
 * Purpose:	Select empty string at end of file.
 */
int cmd_goto_eof(struct win *win) {
	long len;

	cmd_prev_was_cut = 0;	
	anchor_up(win);
	len = sbuf_length(win_buf(win));
	sbuf_remark(win_selection(win), len, 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	win_show(win, len);
	return 0;
}



/*
 * Function:	cmd_goto_top
 * Purpose:	Select empty string at top of visible text.
 */
int cmd_goto_top(struct win *win) {
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	sbuf_remark(win_selection(win), win_get_top(win), 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	return 0;
}



/*
 * Function:	cmd_goto_bottom
 * Purpose:	Select empty string at bottom of visible text.
 */
int cmd_goto_bottom(struct win *win) {
	sbuf_remark(win_selection(win), win_get_bottom(win), 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	return 0;
}



/*
 * Function:	cmd_goto_selection
 * Purpose:	Set search string to selection, then search after selection.
 */

static void do_goto_line(char *str, long len, void *win) {
	filewin_set_filename(win_filewin(win, WIN_LINENO), str);
	(void) cmd_goto_line(win);
}

int cmd_goto_selection(struct win *win) {
	cmd_prev_was_cut = 0;	
	anchor_up(win);
	if (sel_string(win, do_goto_line, win) == -1)
		return -1;
	return 0;
}



/*
 * Function:	cmd_prev_para
 * Purpose:	Select empty string at beginning of previous paragraph.
 */
int cmd_prev_para(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos;

	cmd_prev_was_cut = 0;	
	anchor_up(win);

	buf = win_buf(win);
	sel = win_selection(win);
	pos = sbuf_boln(buf, sbuf_mark_begin(sel));

	if (pos > 0)
		pos = sbuf_boln(buf, pos-1);
	while (pos > 0 && line_is_empty(buf, pos))
		pos = sbuf_boln(buf, pos-1);
	while (pos > 0 && !line_is_empty(buf, pos))
		pos = sbuf_boln(buf, pos-1);
	if (line_is_empty(buf, pos))
		pos = sbuf_eoln(buf, pos);

	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_next_para
 * Purpose:	Select empty string at next end of previous paragraph.
 */
int cmd_next_para(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long len, pos;
	
	cmd_prev_was_cut = 0;	
	anchor_up(win);

	buf = win_buf(win);
	sel = win_selection(win);
	len = sbuf_length(buf);
	pos = sbuf_boln(buf, sbuf_mark_begin(sel));

	if (pos < len)
		pos = sbuf_eoln(buf, pos);
	while (pos < len && line_is_empty(buf, pos))
		pos = sbuf_eoln(buf, pos);
	while (pos < len && !line_is_empty(buf, pos))
		pos = sbuf_eoln(buf, pos);
		
	if (pos > 0 && line_is_empty(buf, pos))
		--pos;

	sbuf_remark(sel, pos, 0);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	cmd_goto_mark_0, ...
 * Purpose:	Select the empty string at user-settable mark 0/1/2/.../9.
 */
int cmd_goto_mark_0(struct win *win) { return goto_mark(win, 0); }
int cmd_goto_mark_1(struct win *win) { return goto_mark(win, 1); }
int cmd_goto_mark_2(struct win *win) { return goto_mark(win, 2); }
int cmd_goto_mark_3(struct win *win) { return goto_mark(win, 3); }
int cmd_goto_mark_4(struct win *win) { return goto_mark(win, 4); }
int cmd_goto_mark_5(struct win *win) { return goto_mark(win, 5); }
int cmd_goto_mark_6(struct win *win) { return goto_mark(win, 6); }
int cmd_goto_mark_7(struct win *win) { return goto_mark(win, 7); }
int cmd_goto_mark_8(struct win *win) { return goto_mark(win, 8); }
int cmd_goto_mark_9(struct win *win) { return goto_mark(win, 9); }




/*
 * Function:	cmd_set_mark_0, ...
 * Purpose:	Set the user-settable mark 0-9 at beginning of selection.
 */
int cmd_set_mark_0(struct win *win) { return set_mark(win, 0); }
int cmd_set_mark_1(struct win *win) { return set_mark(win, 1); }
int cmd_set_mark_2(struct win *win) { return set_mark(win, 2); }
int cmd_set_mark_3(struct win *win) { return set_mark(win, 3); }
int cmd_set_mark_4(struct win *win) { return set_mark(win, 4); }
int cmd_set_mark_5(struct win *win) { return set_mark(win, 5); }
int cmd_set_mark_6(struct win *win) { return set_mark(win, 6); }
int cmd_set_mark_7(struct win *win) { return set_mark(win, 7); }
int cmd_set_mark_8(struct win *win) { return set_mark(win, 8); }
int cmd_set_mark_9(struct win *win) { return set_mark(win, 9); }



/**********************************************************************
 * Local functions follow.
 */


/*
 * Function:	goto_mark
 * Purpose:	Select the empty string at a user-settable mark.
 * Arguments:	win	the window
 *		i	the number of the mark
 */
static int goto_mark(struct win *win, int i) {
	Sbufmark *m;
	long pos;

	anchor_up(win);
	m = sbuf_find_mark_by_code(win_buf(win), USER_MARK_0 + i);
	if (m == NULL)
		return -1;
	pos = sbuf_mark_begin(m);
	sbuf_remark(win_selection(win), pos, 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	win_show(win, pos);
	return 0;
}



/*
 * Function:	set_mark
 * Purpose:	Set a user-settable mark at beginning of selection.
 */
static int set_mark(struct win *win, int i) {
	Sbufmark *m;

	anchor_up(win);
	m = sbuf_find_mark_by_code(win_buf(win), USER_MARK_0 + i);
	if (m == NULL)
		return -1;
	sbuf_remark(m, sbuf_mark_begin(win_selection(win)), 0);
	return 0;
}



/*
 * Function:	line_is_empty
 * Purpose:	Is line beginning at pos empty (only whitespace)?
 */
static int line_is_empty(Sbuf *buf, long pos) {
	int c;

	do {
		c = sbuf_charat(buf, pos);
		++pos;
	} while (c != '\n' && isspace(c));
	return c == '\n' || c == EOF;
}
