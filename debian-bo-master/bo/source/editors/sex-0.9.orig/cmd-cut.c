/*
 * File:	cmd-cut.c
 * Purpose:	Functions that implement cut commands.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd-cut.c,v 1.18 1996/12/08 20:29:15 liw Exp $"
 */

#include <ctype.h>
#include <publib.h>

#include "anchor.h"
#include "cmd.h"
#include "killring.h"
#include "win.h"
#include "error.h"
#include "selections.h"



/*
 * Variable:	cmd_prev_was_cut
 * Purpose:	Did the previous command delete text?
 *		If so, the next cut command will add to same killring entry.
 */
int cmd_prev_was_cut = 0;



/*
 * Function:	cmd_cut
 * Purpose:	Replace selection with empty string.
 * Note:	No-op if selection is empty.
 */
int cmd_cut(struct win *win) {
	Sbufmark *sel;

	anchor_up(win);	
	sel = win_selection(win);
	if (sbuf_mark_length(sel) == 0)
		return 0;
	if (killring_add(sel) == -1)
		return -1;
	if (sbuf_strchange(sel, "", 0) == -1) {
		error(win, "error changing text (out of memory?)");
		return -1;
	}
	cmd_prev_was_cut = 1;
	sbuf_mark_set_columnar(sel, 0);
	return 0;
}



/*
 * Function:	cmd_copy
 * Purpose:	Copy current selection to kill ring.
 * Note:	No-op if selection is empty.
 */
int cmd_copy(struct win *win) {
	Sbufmark *sel;
	
	cmd_prev_was_cut = 0;
	sel = win_selection(win);
	if (sbuf_mark_length(sel) == 0)
		return 0;
	if (killring_add(sel) == -1)
		return -1;
	return 0;
}



/*
 * Function:	cmd_cut_ctrl_k
 * Purpose:	Delete the rest of line, or newline if at newline.
 * Note:	If the selection is non-empty, the selection is cut.
 */
int cmd_cut_ctrl_k(struct win *win) {
	Sbufmark *sel;
	Sbuf *buf;
	long pos, end;
	
	anchor_up(win);	
	sel = win_selection(win);
	if (sbuf_mark_length(sel) == 0) {
		pos = sbuf_mark_begin(sel);
		buf = win_buf(win);
		if (sbuf_charat(buf, pos) == '\n')
			end = pos + 1;
		else {
			end = sbuf_eoln(buf, pos);
			if (end > pos && sbuf_charat(buf, end-1) == '\n')
				--end;
		}
		sbuf_remark(sel, pos, end - pos);
	}
	if (cmd_prev_was_cut) {
		if (killring_append_to_latest(sel) == -1)
			return -1;
	} else if (killring_add(sel) == -1)
		return -1;
	if (sbuf_strchange(sel, "", 0) == -1) {
		error(win, "error changing text (out of memory?)");
		return -1;
	}
	sbuf_mark_set_columnar(sel, 0);
	cmd_prev_was_cut = 1;
	return 0;
}



/*
 * Function:	cmd_cut_previous
 * Purpose:	Delete the character before the selection.
 * Note:	If the selection is non-empty, the selection is cut.
 */
int cmd_cut_previous(struct win *win) {
	Sbufmark *sel;
	long begin;
	
	anchor_up(win);	
	sel = win_selection(win);
	if (sbuf_mark_length(sel) == 0) {
		begin = sbuf_mark_begin(sel);
		if (begin > 0)
			sbuf_remark(sel, begin - 1, 1);
	}
	if (cmd_prev_was_cut) {
		if (killring_prepend_to_latest(sel) == -1)
			return -1;
	} else if (killring_add(sel) == -1)
		return -1;
	if (sbuf_strchange(sel, "", 0) == -1) {
		error(win, "error changing text (out of memory?)");
		return -1;
	}
	sbuf_mark_set_columnar(sel, 0);
	cmd_prev_was_cut = 1;
	return 0;
}



/*
 * Function:	cmd_cut_next
 * Purpose:	Delete the character after the selection.
 * Note:	If the selection is non-empty, the selection is cut.
 */
int cmd_cut_next(struct win *win) {
	Sbufmark *sel;
	long begin;
	
	anchor_up(win);	
	sel = win_selection(win);
	if (sbuf_mark_length(sel) == 0) {
		begin = sbuf_mark_begin(sel);
		if (begin == sbuf_length(win_buf(win)))
			return -1;
		sbuf_remark(sel, begin, 1);
	}
	if (cmd_prev_was_cut) {
		if (killring_append_to_latest(sel) == -1)
			return -1;
	} else if (killring_add(sel) == -1)
		return -1;
	if (sbuf_strchange(sel, "", 0) == -1) {
		error(win, "error changing text (out of memory?)");
		return -1;
	}
	cmd_prev_was_cut = 1;
	sbuf_mark_set_columnar(sel, 0);
	return 0;
}



/*
 * Function:	cmd_cut_prev_word
 * Purpose:	Delete the word before the selection.
 * Note:	If the selection is non-empty, the selection is cut.
 */
int cmd_cut_prev_word(struct win *win) {
	Sbufmark *sel;
	Sbuf *buf;
	long begin, end;
	int c;

	anchor_up(win);	
	buf = win_buf(win);
	sel = win_selection(win);

	if (sbuf_mark_length(sel) == 0) {
		begin = sbuf_mark_begin(sel);
		end = begin;
		if (begin > 0)
			--begin;
		while (begin > 0 && !isalnum(c = sbuf_charat(buf, begin)) && 
			c != '_')
				--begin;
		begin = sbuf_bow(buf, begin);
		sbuf_remark(sel, begin, end - begin);
	}

	if (cmd_prev_was_cut) {
		if (killring_prepend_to_latest(sel) == -1)
			return -1;
	} else if (killring_add(sel) == -1)
		return -1;
	if (sbuf_strchange(sel, "", 0) == -1) {
		error(win, "error changing text (out of memory?)");
		return -1;
	}
	cmd_prev_was_cut = 1;
	sbuf_mark_set_columnar(sel, 0);
	return 0;
}



/*
 * Function:	cmd_cut_next_word
 * Purpose:	Delete the word after the selection.
 * Note:	If the selection is non-empty, the selection is cut.
 */
int cmd_cut_next_word(struct win *win) {
	Sbufmark *sel;
	Sbuf *buf;
	long begin, end;
	int c;

	anchor_up(win);	
	buf = win_buf(win);
	sel = win_selection(win);

	if (sbuf_mark_length(sel) == 0) {
		begin = sbuf_mark_begin(sel);
		end = begin;
		while(!isalnum(c=sbuf_charat(buf,end)) && c != '_' && c != EOF)
			++end;
		end = sbuf_eow(buf, end);
		sbuf_remark(sel, begin, end - begin);
	}

	if (cmd_prev_was_cut) {
		if (killring_append_to_latest(sel) == -1)
			return -1;
	} else if (killring_add(sel) == -1)
		return -1;
	if (sbuf_strchange(sel, "", 0) == -1) {
		error(win, "error changing text (out of memory?)");
		return -1;
	}
	cmd_prev_was_cut = 1;
	sbuf_mark_set_columnar(sel, 0);
	return 0;
}



/*
 * Function:	cmd_yank
 * Purpose:	Replace selection with the first entry in the kill ring.
 */

static void do_yank(char *str, long len, void *arg) {
	struct win *win;
	Sbufmark *sel;
	
	win = arg;
	sel = win_selection(win);
	if (str == NULL) {
		if (killring_get_first(sel) == -1)
			error(win, "Error: Out of memory");
	} else {
		if (sbuf_strchange(sel, str, len) == -1)
			error(win, "Error: Out of memory");
	}
}

int cmd_yank(struct win *win) {
	anchor_up(win);	
	cmd_prev_was_cut = 0;
	if (sel_string(win, do_yank, win) == -1) {
		error(win, "Out of memory");
		return -1;
	}
	return 0;
}



/*
 * Function:	cmd_yank_previous
 * Purpose:	Replace selection with the first entry in the kill ring.
 */
int cmd_yank_previous(struct win *win) {
	anchor_up(win);	
	cmd_prev_was_cut = 0;
	return killring_get_next(win_selection(win));
}



/*
 * Function:	cmd_yank_column
 * Purpose:	Replace selection with the first entry in the kill ring.
 */
int cmd_yank_column(struct win *win) {
	anchor_up(win);	
	cmd_prev_was_cut = 0;
	sbuf_mark_set_columnar(win_selection(win), 1);
	if (sel_string(win, do_yank, win) == -1) {
		error(win, "Out of memory");
		return -1;
	}
	return 0;
}



/*
 * Function:	cmd_yank_column_previous
 * Purpose:	Replace selection with the first entry in the kill ring.
 */
int cmd_yank_column_previous(struct win *win) {
	Sbufmark *sel;

	anchor_up(win);	
	cmd_prev_was_cut = 0;
	sel = win_selection(win);
	sbuf_mark_set_columnar(sel, 1);
	return killring_get_next(win_selection(win));
}
