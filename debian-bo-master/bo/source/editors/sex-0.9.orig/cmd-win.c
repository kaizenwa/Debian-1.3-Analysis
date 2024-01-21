/*
 * File:	cmd-clone.c
 * Purpose:	Functions that implement window commands.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd-win.c,v 1.14 1996/12/22 20:06:11 liw Exp $"
 */

#include <ctype.h>
#include <publib.h>

#include "anchor.h"
#include "cmd.h"
#include "buflist.h"
#include "win.h"



/*
 * Function:	cmd_clone
 * Purpose:	Make an identical copy of the current window.
 */
int cmd_clone(struct win *win) {
	struct win *win2;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	if (win_create(&win2, win_buf(win)) == -1)
		return -1;
	win_set_top(win2, win_get_top(win));
	sbuf_remark(win_selection(win2), 
		sbuf_mark_begin(win_selection(win)), 0);
	sbuf_mark_set_columnar(win_selection(win2), 0);
	return 0;
}



/*
 * Function:	cmd_close
 * Purpose:	Close a window.  If last window, exit the editor.
 */
int cmd_close(struct win *win) {
	anchor_up(win);
	cmd_prev_was_cut = 0;
	win_destroy(win);
	return 0;
}



/*
 * Function:	cmd_close_others
 * Purpose:	Close all other windows.
 */
int cmd_close_others(struct win *win) {
	struct win *p;
	int i;
	
	i = 0;
	for (;;) {
		p = win_ith_win(i);
		if (p == NULL)
			break;
		else if (p == win)
			i = 1;
		else
			win_destroy(p);
	}
	return 0;
}



/*
 * Function:	cmd_open_all
 * Purpose:	Make sure every buffer has a window.
 */
int cmd_open_all(struct win *win) {
	Sbuf *buf, *this;
	struct win *dummy;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	this = win_buf(win);
	buf = this;
	while ((buf = buflist_next(buf)) != this) {
		if (win_find_by_buf(buf) == NULL) {
			if (win_create(&dummy, buf) == -1)
				return -1;
			if (!sbuf_has_flags(buf, SBUF_LOADED_FLAG))
				if (sbuf_load(buf) == -1)
					return -1;
			sbuf_remark(win_selection(dummy), 0, 0);
			sbuf_mark_set_columnar(win_selection(dummy), 0);
		}
	}
	return 0;
}
