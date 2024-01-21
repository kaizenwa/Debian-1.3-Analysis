/*
 * anchor.h - declarations for "anchor" for cursor
 * Lars Wirzenius
 */


#ifndef anchor_h
#define anchor_h

struct win;

void anchor_up(struct win *);
long anchor_mobile(struct win *);
long anchor_fixed(struct win *);
void anchor_move_delta(struct win *, long);
void anchor_move_to(struct win *, long);

#endif
