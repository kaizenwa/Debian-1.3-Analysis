/*
 * anchor.c - implement the "anchor" for the cursor
 * Lars Wirzenius
 */

#include <assert.h>

#include "anchor.h"
#include "win.h"
#include "x.h"

static void anchor_down(struct win *win) {
	Sbufmark *m, *sel;
	Sbuf *buf;
	
	buf = win_buf(win);
	m = sbuf_find_mark_by_code(buf, ANCHOR_UP_MARK);
	if (m != NULL) {
		sbuf_set_mark_code(m, ANCHOR_DOWN_MARK);
		sel = win_selection(win);
		sbuf_remark(m, sbuf_mark_begin(sel), 0);

		m = sbuf_find_mark_by_code(buf, ANCHOR_MOBILE_MARK);
		assert(m != NULL);
		sbuf_remark(m, sbuf_mark_begin(sel), 0);
	}
}


void anchor_up(struct win *win) {
	Sbufmark *m;
	
	m = sbuf_find_mark_by_code(win_buf(win), ANCHOR_DOWN_MARK);
	if (m != NULL)
		sbuf_set_mark_code(m, ANCHOR_UP_MARK);
}


void anchor_move_delta(struct win *win, long n) {
	long pos, len;

	pos = n + anchor_mobile(win);
	if (pos < 0)
		pos = 0;
	len = sbuf_length(win_buf(win));
	if (pos > len)
		pos = len;
	anchor_move_to(win, pos);
}

static long anchor_end(struct win *win, int mark) {
	Sbufmark *m;

	anchor_down(win);
	m = sbuf_find_mark_by_code(win_buf(win), mark);
	assert(m != NULL);
	return sbuf_mark_begin(m);
}

long anchor_mobile(struct win *win) {
	return anchor_end(win, ANCHOR_MOBILE_MARK);
}

long anchor_fixed(struct win *win) {
	return anchor_end(win, ANCHOR_DOWN_MARK);
}

void anchor_move_to(struct win *win, long pos) {
	Sbufmark *mobile, *fixed;
	long p1, p2;

	anchor_down(win);
	mobile = sbuf_find_mark_by_code(win_buf(win), ANCHOR_MOBILE_MARK);
	fixed = sbuf_find_mark_by_code(win_buf(win), ANCHOR_DOWN_MARK);
	assert(mobile != NULL);
	assert(fixed != NULL);

	sbuf_remark(mobile, pos, 0);
	p1 = sbuf_mark_begin(fixed);
	p2 = sbuf_mark_begin(mobile);
	if (p1 < p2)
		sbuf_remark(win_selection(win), p1, p2-p1);
	else
		sbuf_remark(win_selection(win), p2, p1-p2);
	win_show(win, p2);
}
