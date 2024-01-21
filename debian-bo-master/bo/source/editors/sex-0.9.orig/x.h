/*
 * File:	x.h
 * Purpose:	Declarations for X utility functions.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: x.h,v 1.7 1996/11/04 00:23:38 liw Exp $"
 */

#ifndef x_h
#define x_h

#include <publib.h>
#include <X11/Intrinsic.h>

struct font;

enum {
	DUMMY_BEFORE_MARK = 128,
	USER_MARK_0,
	USER_MARK_1,
	USER_MARK_2,
	USER_MARK_3,
	USER_MARK_4,
	USER_MARK_5,
	USER_MARK_6,
	USER_MARK_7,
	USER_MARK_8,
	USER_MARK_9,
	AUX_MARK,
	ANCHOR_UP_MARK,
	ANCHOR_MOBILE_MARK,
	DUMMY_AFTER_MARK,

	ANCHOR_DOWN_MARK,	/* these are not created automatically */
	DUMMY_DELETE_MARK	/* delete everything up to here */
};

Sbuf *create_buffer(void);
void destroy_buffer(Sbuf *);

void x_do_it(int, char **);
Display *x_display(void);
Widget x_create_top_level(void);
Widget x_root(void);
int x_pending(void);
void x_timeout(int, void (*)(void));
void x_set_wm_protocols(Widget, void (*)(Widget));
void x_window_dimensions(Widget, unsigned *, unsigned *);
void x_draw_line(Widget, int, int, char *, int, int);
GC x_create_gc(Widget, struct font *, unsigned long, unsigned long);

#endif
