/*
 * File:	selections.h
 * Purpose:	Declarations for selections.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: selections.h,v 1.4 1996/01/03 13:05:28 liw Exp $"
 */
 
#ifndef selections_h
#define selections_h

#include <X11/Xlib.h>	/* Need Time */

struct win;

int sel_start(struct win *, int, int, Time);
int sel_continue(struct win *, int, int, Time);
int sel_extend(struct win *, int, int, Time);
int sel_cut_or_paste(struct win *, int, int, Time);

int sel_own(struct win *, Time);

int sel_string(struct win *, void (*)(char *, long, void *), void *);

#endif
