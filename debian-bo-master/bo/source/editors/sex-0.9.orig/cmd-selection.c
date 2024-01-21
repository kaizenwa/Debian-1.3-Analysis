/*
 * File:	cmd-selections.c
 * Purpose:	Functions that implement selection commands.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd-selection.c,v 1.12 1996/12/17 14:31:55 liw Exp $"
 */

#include <assert.h>
#include <ctype.h>
#include <publib.h>

#include "anchor.h"
#include "cmd.h"
#include "win.h"
#include "tab.h"
#include "x.h"
#include "error.h"


static int line_is_empty(Sbuf *, long);
static int extend_mark(struct win *, int);


/*
 * Function:	cmd_extend_forward
 * Purpose:	Move end of selection one character forward.
 */
int cmd_extend_forward(struct win *win) {
	anchor_move_delta(win, 1);
	return 0;
}



/*
 * Function:	cmd_extend_backward
 * Purpose:	Move beginning of selection one character backward
 */
int cmd_extend_backward(struct win *win) {
	anchor_move_delta(win, -1);
	return 0;
}



/*
 * Function:	cmd_extend_boln
 * Purpose:	Move beginning of selection to beginning of line
 */
int cmd_extend_boln(struct win *win) {
	long boln, pos;
	Sbuf *buf;

	buf = win_buf(win);	
	pos = anchor_mobile(win);
	boln = sbuf_boln(buf, pos);
	if (pos == boln && boln > 0)
		boln = sbuf_boln(buf, boln-1);
	anchor_move_to(win, boln);
	return 0;
}



/*
 * Function:	cmd_extend_eoln
 * Purpose:	Move end of selection to end of line.
 */
int cmd_extend_eoln(struct win *win) {
	long pos;

	pos = sbuf_eoln(win_buf(win), anchor_mobile(win));
	anchor_move_to(win, pos);
	return 0;
}



/*
 * Function:	cmd_extend_bof
 * Purpose:	Move beginning of selection to beginning of file.
 */
int cmd_extend_bof(struct win *win) {
	anchor_move_to(win, 0);
	return 0;
}



/*
 * Function:	cmd_extend_eof
 * Purpose:	Move end of selection to end of file.
 */
int cmd_extend_eof(struct win *win) {
	anchor_move_to(win, sbuf_length(win_buf(win)));
	return 0;
}


/*
 * Function:	cmd_extend_top
 * Purpose:	Move end of selection to top of window.
 */
int cmd_extend_top(struct win *win) {
	anchor_move_to(win, win_get_top(win));
	return 0;
}


/*
 * Function:	cmd_extend_bottom
 * Purpose:	Move end of selection to bottom of window.
 */
int cmd_extend_bottom(struct win *win) {
	anchor_move_to(win, win_get_bottom(win));
	return 0;
}


/*
 * Function:	cmd_extend_next_word
 * Purpose:	Move end of selection to next word.
 */
int cmd_extend_next_word(struct win *win) {
	Sbuf *buf;
	long pos, newpos;
	int c;
	
	cmd_prev_was_cut = 0;	
	buf = win_buf(win);
	pos = anchor_mobile(win);
	newpos = pos;
	do {
		pos = newpos++;
		c = sbuf_charat(buf, pos);
	} while (c != EOF && !(isalnum(c) || c == '_'));
	pos = sbuf_eow(buf, pos);
	anchor_move_to(win, pos);
	return 0;
}


/*
 * Function:	cmd_extend_prev_word
 * Purpose:	Move end of selection to previous word.
 */
int cmd_extend_prev_word(struct win *win) {
	Sbuf *buf;
	long pos;
	int c;
	
	cmd_prev_was_cut = 0;	
	buf = win_buf(win);
	pos = anchor_mobile(win);

	if (pos == 0)
		return -1;

	--pos;
	while (pos > 0 && !isalnum(c = sbuf_charat(buf, pos)) && c != '_')
		--pos;

	pos = sbuf_bow(buf, pos);
	anchor_move_to(win, pos);
	return 0;
}


/*
 * Function:	cmd_extend_next_line
 * Purpose:	Move end of selection to next line.
 */
int cmd_extend_next_line(struct win *win) {
	Sbuf *buf;
	long pos, col;

	buf = win_buf(win);
	pos = anchor_mobile(win);
	col = sbuf_colno(buf, pos, tab_width());
	pos = sbuf_eoln(buf, pos);
	pos = sbuf_colpos(buf, pos, col, tab_width());
	anchor_move_to(win, pos);
	return 0;
}


/*
 * Function:	cmd_extend_prev_line
 * Purpose:	Move end of selection to previous line.
 */
int cmd_extend_prev_line(struct win *win) {
	Sbuf *buf;
	long pos, col;

	buf = win_buf(win);
	pos = anchor_mobile(win);
	col = sbuf_colno(buf, pos, tab_width());
	pos = sbuf_boln(buf, pos);
	if (pos > 0) {
		pos = sbuf_boln(buf, pos - 1);
		pos = sbuf_colpos(buf, pos, col, tab_width());
	}
	anchor_move_to(win, pos);
	return 0;
}


/*
 * Function:	cmd_extend_next_para
 * Purpose:	Move end of selection to next paragraph.
 */
int cmd_extend_next_para(struct win *win) {
	Sbuf *buf;
	long len, pos;
	
	cmd_prev_was_cut = 0;	
	buf = win_buf(win);
	len = sbuf_length(buf);
	pos = anchor_mobile(win);

	if (pos < len)
		pos = sbuf_eoln(buf, pos);
	while (pos < len && line_is_empty(buf, pos))
		pos = sbuf_eoln(buf, pos);
	while (pos < len && !line_is_empty(buf, pos))
		pos = sbuf_eoln(buf, pos);
		
	if (pos > 0 && line_is_empty(buf, pos))
		--pos;

	anchor_move_to(win, pos);
	return 0;
}


/*
 * Function:	cmd_extend_prev_para
 * Purpose:	Move end of selection to previous paragraph.
 */
int cmd_extend_prev_para(struct win *win) {
	Sbuf *buf;
	long pos;

	cmd_prev_was_cut = 0;	
	buf = win_buf(win);
	pos = anchor_mobile(win);

	if (pos > 0)
		pos = sbuf_boln(buf, pos-1);
	while (pos > 0 && line_is_empty(buf, pos))
		pos = sbuf_boln(buf, pos-1);
	while (pos > 0 && !line_is_empty(buf, pos))
		pos = sbuf_boln(buf, pos-1);
	if (line_is_empty(buf, pos))
		pos = sbuf_eoln(buf, pos);

	anchor_move_to(win, pos);
	return 0;
}


/*
 * Function:	cmd_extend_next_page
 * Purpose:	Move end of selection to next page.
 */
int cmd_extend_next_page(struct win *win) {
	Sbuf *buf;
	long pos;
	int i, rows, cols;

	win_get_dimensions(win, &rows, &cols);
	buf = win_buf(win);

	pos = anchor_mobile(win);
	for (i = 0; i < rows-2; ++i)
		pos = sbuf_eoln(buf, pos);
	anchor_move_to(win, pos);

	return 0;
}


/*
 * Function:	cmd_extend_prev_page
 * Purpose:	Move end of selection to previous page.
 */
int cmd_extend_prev_page(struct win *win) {
	Sbuf *buf;
	long pos;
	int i, rows, cols;

	win_get_dimensions(win, &rows, &cols);
	buf = win_buf(win);

	pos = anchor_mobile(win);
	for (i = 0; pos > 0 && i < rows-2; ++i)
		pos = sbuf_boln(buf, pos-1);
	anchor_move_to(win, pos);

	return 0;
}


/*
 * Function:	cmd_extend_mark_*
 * Purpose:	Extend selection to user settable mark.
 */
int cmd_extend_mark_0(struct win *win) { return extend_mark(win, 0); }
int cmd_extend_mark_1(struct win *win) { return extend_mark(win, 1); }
int cmd_extend_mark_2(struct win *win) { return extend_mark(win, 2); }
int cmd_extend_mark_3(struct win *win) { return extend_mark(win, 3); }
int cmd_extend_mark_4(struct win *win) { return extend_mark(win, 4); }
int cmd_extend_mark_5(struct win *win) { return extend_mark(win, 5); }
int cmd_extend_mark_6(struct win *win) { return extend_mark(win, 6); }
int cmd_extend_mark_7(struct win *win) { return extend_mark(win, 7); }
int cmd_extend_mark_8(struct win *win) { return extend_mark(win, 8); }
int cmd_extend_mark_9(struct win *win) { return extend_mark(win, 9); }


/*
 * Function:	cmd_select_para
 * Purpose:	Select current paragraph.
 */
int cmd_select_para(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long len, pos, pos2, begin;
	
	cmd_prev_was_cut = 0;

	buf = win_buf(win);
	sel = win_selection(win);
	len = sbuf_length(buf);
	pos = sbuf_boln(buf, sbuf_mark_begin(sel));

	while (pos < len && line_is_empty(buf, pos))
		pos = sbuf_eoln(buf, pos);
	while (pos > 0) {
		pos2 = sbuf_boln(buf, pos-1);
		if (line_is_empty(buf, pos2))
			break;
		pos = pos2;
	}
	begin = pos;

	do {
		pos = sbuf_eoln(buf, pos);
	} while (pos < len && !line_is_empty(buf, pos));

	sbuf_remark(sel, begin, pos - begin);
	sbuf_mark_set_columnar(sel, 0);
	win_show(win, begin);

	return 0;
}


/*
 * Function:	cmd_toggle_columnar
 * Purpose:	Toggle columnarity of selection.
 */
int cmd_toggle_columnar(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long bcol, ecol;
	
	sel = win_selection(win);
	if (sbuf_mark_is_columnar(sel))
		sbuf_mark_set_columnar(sel, 0);
	else {
		buf = win_buf(win);
		bcol = sbuf_colno(buf, sbuf_mark_begin(sel), tab_width());
		ecol = sbuf_colno(buf, sbuf_mark_end(sel), tab_width());
		if (bcol >= ecol) {
			error(win, "Error: selection is in wrong form for columnar mode");
			return -1;
		}
		sbuf_mark_set_columnar(sel, 1);
	}
	return 0;
}


/***********************************************************************
 * Local functions.
 */


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


/*
 * Function:	extend_mark
 * Purpose:	Extend selection to user-settable mark.
 * Arguments:	win	the window
 *		i	the number of the mark
 */
static int extend_mark(struct win *win, int i) {
	Sbufmark *m;

	m = sbuf_find_mark_by_code(win_buf(win), USER_MARK_0 + i);
	if (m == NULL)
		return -1;
	anchor_move_to(win, sbuf_mark_begin(m));
	return 0;
}
