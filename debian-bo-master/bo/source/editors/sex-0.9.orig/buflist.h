/*
 * File:	buflist.h
 * Purpose:	Declarations for buffer list.
 */

#ifndef buflist_h_included
#define buflist_h_included

#include <publib.h>

int buflist_add(Sbuf *);
void buflist_remove(Sbuf *);
Sbuf *buflist_first(void);
Sbuf *buflist_next(Sbuf *);
Sbuf *buflist_prev(Sbuf *);
Sbuf *buflist_by_name(const char *);

void buflist_sort(void);

#endif
