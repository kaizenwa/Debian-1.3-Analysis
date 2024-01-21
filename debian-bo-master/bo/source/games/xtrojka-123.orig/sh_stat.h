/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *	header file for sh_stat.c
 */

#ifndef _sh_stat_h_
#define _sh_stat_h_

/*
 *	size of statistics window
 */
#define STAT_XSIZE	200
#define STAT_YSIZE	200

/*
 *	function prototypes
 */

void init_stat_window(void);
void show_stat(void);
void redraw_stat(Widget, XExposeEvent*, String*, Cardinal*);
void update_stat(void);

#endif /* _sh_stat_h_ */
