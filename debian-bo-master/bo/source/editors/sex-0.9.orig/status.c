/*
 * status.c -- implement generation of status messages.
 * Lars Wirzenius.
 */

#include <stdio.h>
#include <publib.h>

#include "status.h"
#include "win.h"
#include "tab.h"

static void fill(struct status *st, struct win *win) {
	Sbuf *buf;
	Sbufmark *sel;
	long pos, epos;

	buf = win_buf(win);
	sel = win_selection(win);

	pos = sbuf_mark_begin(sel);
	st->row = sbuf_lineno(buf, pos);
	st->col = sbuf_colno(buf, pos, tab_width());

	epos = sbuf_mark_end(sel);
	st->erow = sbuf_lineno(buf, epos);
	st->ecol = sbuf_colno(buf, epos, tab_width());
	
	st->columnar = sbuf_mark_is_columnar(sel);

	st->total_chars = sbuf_length(buf);
	st->total_rows = sbuf_lineno(buf, st->total_chars);
	
	st->dirty = sbuf_is_dirty(buf);
}

static int different(struct status *a, struct status *b) {
	return a->row != b->row || a->col != b->col ||
		a->erow != b->erow || a->ecol != b->ecol ||
		a->columnar != b->columnar ||
		a->total_rows != b->total_rows ||
		a->total_chars != b->total_chars ||
		a->dirty != b->dirty;
}

static void copy(struct status *to, struct status *from) {
	to->row = from->row;
	to->col = from->col;
	to->erow = from->erow;
	to->ecol = from->ecol;
	to->columnar = from->columnar;
	to->total_rows = from->total_rows;
	to->total_chars = from->total_chars;
	to->dirty = from->dirty;
}

static void generate(struct status *st) {
	double size;
	char *unit, *modified, *columnar;
	int decimals;
	
	size = st->total_chars;
	if (size < 1024) {
		decimals = 0;
		unit = "chars";
	} else if (size < 1024*1024) {
		size /= 1024;
		decimals = 1;
		unit = "kB";
	} else {
		size /= 1024*1024;
		decimals = 2;
		unit = "MB";
	}

	columnar = st->columnar ? " [col]" : "";	
	modified = st->dirty ? "**" : "";
	
	if (st->row == st->erow && st->col == st->ecol) {
		sprintf(st->msg,
		"Pos %ld:%ld%s  Total: %ld rows (%.*f %s) %s",
			st->row+1, st->col+1, columnar, st->total_rows,
			decimals, size, unit, modified);
	} else {
		sprintf(st->msg,
		"Pos %ld:%ld - %ld:%ld%s  Total: %ld rows (%.*f %s) %s",
			st->row+1, st->col+1, st->erow+1, st->ecol+1,
			columnar,
			st->total_rows, decimals, size, unit, modified);
	}
}

void status_init(struct status *st, struct win *win) {
	st->win = win;
	st->row = -1;	/* trick! force generation of message */
	st->col = 0;
	st->total_rows = 0;
	st->total_chars = 0;
	st->dirty = 0;
}

int status_get(struct status *st, char **msg) {
	struct status aux;

	*msg = st->msg;
	fill(&aux, st->win);
	if (different(&aux, st)) {
		copy(st, &aux);
		generate(st);
		return 1;
	}
	return 0;
}
