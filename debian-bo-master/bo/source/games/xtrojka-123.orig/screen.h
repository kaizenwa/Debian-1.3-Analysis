/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *	header file for screen.c
 */

#ifndef _screen_h_
#define _screen_h_

/*
 *	function prototypes
 */

void set_color(Widget, int, int);
void paint_widget(Widget, int, int);
void draw_string(Widget, int, int, char *);

#endif /* _screen_h_ */
