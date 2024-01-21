/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *	header file for sh_slist.c
 */

#ifndef _sh_slist_h_
#define _sh_slist_h_

/*
 *	function prototypes
 */

void init_slist_window(void);
void draw_slist(int);
void draw_newpic(int, int);
void show_slist(void);
void hide_slist(void);
void redraw_slist(Widget, XExposeEvent*, String*, Cardinal*);

#endif /* _sh_slist_h_ */
