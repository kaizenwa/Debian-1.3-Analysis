/*
 * File:	cmd-misc.c
 * Purpose:	Functions that implement commands.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd-misc.c,v 1.18 1996/12/22 20:10:54 liw Exp $"
 */

#include <assert.h>
#include <ctype.h>
#include <publib.h>

#include "anchor.h"
#include "config.h"
#include "cmd.h"
#include "win.h"
#include "error.h"
#include "x.h"
#include "tab.h"



/*
 * Local function prototypes.
 */

static long next_nonspace(Sbuf *, long, long);
static long prev_nonspace(Sbuf *, long);
static long current_indent(Sbuf *, long, long);
static long wanted_indent(Sbuf *, long, long);
static int remove_indent(Sbuf *, Sbufmark *, long, long);
static int insert_indent(Sbufmark *, long, long);
static int add_indent(struct win *, long);




/*
 * Function:	cmd_insert
 * Purpose:	Replace current selection with string, and select empty string
 *		after last inserted character.
 * Note:	This is function has a different prototype from the rest of
 *		the cmd_* functions.
 */
int cmd_insert(struct win *win, const char *str, size_t len) {
	Sbufmark *sel;

	anchor_up(win);	
	cmd_prev_was_cut = 0;
	sel = win_selection(win);
	if (sbuf_mark_is_columnar(sel)) {
		if (sbuf_strchange(sel, "", 0) == -1) {
			error(win, "error changing buffer (out of memory?)");
			return -1;
		}
		sbuf_mark_set_columnar(sel, 0);
	}
	if (sbuf_strchange(sel, str, len) == -1) {
		error(win, "error changing buffer (out of memory?)");
		return -1;
	}
	sbuf_remark(sel, sbuf_mark_end(sel), 0);
	win_show(win, sbuf_mark_begin(sel));
	return 0;
}



/*
 * Function:	cmd_indent_selection_space
 * Purpose:	Add one space to beginning of all lines that have selection.
 */
int cmd_indent_selection_space(struct win *win) {
	return add_indent(win, 1);
}



/*
 * Function:	cmd_undent_selection_space
 * Purpose:	Remove one space's worth of tabs and spaces from the beginning 
 *		of all lines that have selection.
 */
int cmd_undent_selection_space(struct win *win) {
	return add_indent(win, -1);
}



/*
 * Function:	cmd_indent_selection
 * Purpose:	Add one tab to beginning of all lines that have selection.
 */
int cmd_indent_selection(struct win *win) {
	return add_indent(win, config_get_long(CONFIG_INDENT_WIDTH));
}



/*
 * Function:	cmd_undent_selection
 * Purpose:	Remove one tabs worth of tabs and spaces from the beginning 
 *		of all lines that have selection.
 */
int cmd_undent_selection(struct win *win) {
	return add_indent(win, -config_get_long(CONFIG_INDENT_WIDTH));
}



/*
 * Function:	cmd_indent_line
 * Purpose:	Fix indentation for current line (possibly non-empty).
 * Description:	First remove all indentation on current line.  Then
 *		insert indentation again.  Indentation is the same as
 *		the previous line, or one tab more, if previous line
 *		ends in left curly brace, parenthesis, or bracket.
 *		If this line begins with right curly brace, parenthesis
 *		or bracket, it is indented as much the line that contains
 *		its pair.
 */
int cmd_indent_line(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long begin, pos, orig_pos, col;

	anchor_up(win);	
	cmd_prev_was_cut = 0;

	buf = win_buf(win);
	sel = win_selection(win);
	begin = sbuf_mark_begin(sel);
	orig_pos = pos = sbuf_boln(buf, begin);

	col = wanted_indent(buf, pos, begin);
	if (remove_indent(buf, sel, pos, begin) == -1) {
		error(win, "error removing old indentation");
		return -1;
	}
	if (insert_indent(sel, pos, col) == -1) {
		error(win, "error inserting indentation");
		return -1;
	}

	return 0;
}



/*
 * Function:	cmd_unindent_right_curly
 * Purpose:	Remove one level of indentation.
 * Description:	If selection is empty, and current line contains only
 *		whitespace and one right brace, then do cmd_indent,
 *		else do nothing.
 */
int cmd_unindent_right_curly(struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos, begin;
	int c;

	anchor_up(win);	
	sel = win_selection(win);
	if (sbuf_mark_length(sel) > 0)
		return 0;

	buf = win_buf(win);
	begin = sbuf_mark_begin(sel);
	pos = sbuf_boln(buf, begin);
	pos = next_nonspace(buf, pos, begin);
	c = sbuf_charat(buf, pos);
	if (c == '}' || c == ']' || c == ')') {
		c = sbuf_charat(buf, next_nonspace(buf, pos+1, begin));
		if (c == '\n' || c == EOF) {
			if (cmd_indent_line(win) == -1)
				return -1;
			return cmd_goto_eoln(win);
		}
	}

	return 0;
}



/*
 * Function:	cmd_about
 * Purpose:	Display About text.
 */
int cmd_about(struct win *win) {
	cmd_prev_was_cut = 0;
	win_set_msg(win, "Unreleased version of SeX, by Lars Wirzenius.");
	return 0;
}




/*
 * Function:	cmd_help
 * Purpose:	Display help text.
 */
int cmd_help(struct win *win) {
	cmd_prev_was_cut = 0;
	win_set_msg(win, "Help is in file " HELP_FILE ".");
	return 0;
}




/*
 * Function:	cmd_help_getting
 * Purpose:	Display "Getting SeX" text.
 */
int cmd_help_getting(struct win *win) {
	cmd_prev_was_cut = 0;
	win_set_msg(win, "You can't get SeX yet.");
	return 0;
}




/*
 * Function:	cmd_help_bugs
 * Purpose:	Display bug reporting text.
 */
int cmd_help_bugs(struct win *win) {
	cmd_prev_was_cut = 0;
	win_set_msg(win, "SeX has no bugs.");
	return 0;
}




/*
 * Function:	cmd_help_list
 * Purpose:	Display mailing list text.
 */
int cmd_help_list(struct win *win) {
	cmd_prev_was_cut = 0;
	win_set_msg(win, "There is no mailing list, but see news:alt.sex.");
	return 0;
}




/*
 * Function:	cmd_quit
 * Purpose:	Terminate the editor; ask if any files should be saved.
 */
int cmd_quit(struct win *win) {
	cmd_prev_was_cut = 0;
	if (win_destroy_all(win) == -1)
		return -1;
	exit(0);
}


 


/*
 * Function:	cmd_quit_and_save_all
 * Purpose:	Terminate the editor; save all unsaved files.
 */
int cmd_quit_and_save_all(struct win *win) {
	cmd_prev_was_cut = 0;
	if (cmd_save_all(win) == -1)
		return -1;
	exit(0);
}



/*
 * Function:	cmd_quit_without_saving
 * Purpose:	Terminate the editor without saving any files.
 */
int cmd_quit_without_saving(struct win *win) {
	cmd_prev_was_cut = 0;
	exit(0);
}



/**********************************************************************
 * Local functions follow.
 */



/*
 * Function:	current_indent
 * Purpose:	Compute the indent this line has now.
 * Arguments:	buf	the buffer
 *		pos	beginning of current line
 *		begin	limit for indentation computation
 */
static long current_indent(Sbuf *buf, long pos, long begin) {
	return sbuf_colno(buf, next_nonspace(buf, pos, begin), tab_width());
}



/*
 * Function:	wanted_indent
 * Purpose:	Compute the indent this line _should_ have.
 * Arguments:	buf	the buffer
 *		pos	beginning of current line
 *		begin	limit for indentation computation
 */
static long wanted_indent(Sbuf *buf, long pos, long begin) {
	long i, j, col, prev, tab;
	int c;

	if (pos == 0)
		return sbuf_colno(buf, next_nonspace(buf, pos, begin),
				tab_width());

	prev = sbuf_boln(buf, pos - 1);
	i = next_nonspace(buf, prev, begin);
	col = sbuf_colno(buf, i, tab_width());
	tab = tab_width();

	j = next_nonspace(buf, pos, begin);
	c = sbuf_charat(buf, j);
	if (c == '}' || c == ']' || c == ')') {
		j = sbuf_find_pair(buf, j);
		if (j != -1) {
			j = next_nonspace(buf, sbuf_boln(buf, j), begin);
			col = sbuf_colno(buf, j, tab);
		}
	} else {
		c = sbuf_charat(buf, prev_nonspace(buf, pos-1));
		if (c == '{' || c == '(' || c == '[')
			col = tab_next(col);
	}

	return col;
}



/*
 * Function:	remove_indent
 * Purpose:	Remove indentation from this line.
 */
static int remove_indent(Sbuf *buf, Sbufmark *sel, long pos, long begin) {
	long i;
	int c;

	for (i = pos; (c = sbuf_charat(buf, i)) != '\n' && isspace(c); ++i)
		if (i == begin)
			break;
	sbuf_remark(sel, pos, i-pos);
	sbuf_mark_set_columnar(sel, 0);
	return sbuf_strchange(sel, "", 0);
}



/*
 * Function:	insert_indent
 * Purpose:	Insert indentation to this line.
 */
static int insert_indent(Sbufmark *sel, long pos, long col) {
	long tab;
	
	tab = tab_width();
	
	while (col >= tab) {
		if (sbuf_strchange(sel, "\t", 1) == -1)
			return -1;
		sbuf_remark(sel, sbuf_mark_end(sel), 0);
		col -= tab;
	}

	while (col-- > 0) {
		if (sbuf_strchange(sel, " ", 1) == -1)
			return -1;
		sbuf_remark(sel, sbuf_mark_end(sel), 0);
	}

	sbuf_mark_set_columnar(sel, 0);
	return 0;
}



/*
 * Function:	next_nonspace
 * Purpose:	Find the next non-space character on this line.
 * Arguments:	buf	the buffer
 *		pos	the starting position
 *		begin	maximum position to search
 * Return:	The position before the character.
 * Note:	Can't fail.
 */
static long next_nonspace(Sbuf *buf, long pos, long begin) {
	int c;

	while (pos<begin && (c = sbuf_charat(buf, pos)) != '\n' && isspace(c))
		++pos;
	return pos;
}



/*
 * Function:	prev_nonspace
 * Purpose:	Find the previous non-space character on this line.
 * Arguments:	buf	the buffer
 *		pos	the starting position
 * Return:	The position before the character.
 * Note:	Can't fail.
 */
static long prev_nonspace(Sbuf *buf, long pos) {
	while (pos > 0 && sbuf_charat(buf, pos-1) != '\n' &&
	       isspace(sbuf_charat(buf, pos)))
		--pos;
	return pos;
}



/*
 * Function:	add_indent
 * Purpose:	Add delta space's worth of indentation.
 */
static int add_indent(struct win *win, long delta) {
	Sbuf *buf;
	Sbufmark *aux, *sel;
	long n, pos, begin, end;

	buf = win_buf(win);
	aux = sbuf_find_mark_by_code(buf, AUX_MARK);
	if (aux == NULL) {
		error(win, "Couldn't find auxliary mark (internal error?)");
		return -1;
	}
	sel = win_selection(win);
	begin = sbuf_mark_begin(sel);

	end = sbuf_mark_end(sel);
	pos = sbuf_boln(buf, end);
	if (pos > begin && pos == end)
		pos = sbuf_boln(buf, pos-1);

	for (;;) {
		end = sbuf_length(buf);
		n = current_indent(buf, pos, end);
		n += delta;
		if (n < 0)
			n = 0;
		if (remove_indent(buf, aux, pos, end) == -1 ||
		    insert_indent(aux, pos, n) == -1) {
			error(win, "Couldn't modify text (out of memory?)");
			return -1;
		}
		if (pos <= begin)
			break;
		assert(pos > 0);
		pos = sbuf_boln(buf, pos-1);
	}
	
	return 0;
}
