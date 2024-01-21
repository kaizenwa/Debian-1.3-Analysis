/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *	header file for sh_main.c
 */

#ifndef _sh_main_h_
#define _sh_main_h_

/*
 *	dimensions of a block
 */
#define BLOCK_XSIZE	36
#define BLOCK_YSIZE	19

/*
 *	location of the game-title
 */
#define TITLE_X		3
#define TITLE_Y		100

/*
 *	function prototypes
 */
void show_score(void);
void trojka_speedup_callback(int);
void show_speed(int);
void redraw_leftpillar(Widget, XExposeEvent *, String *, Cardinal *);
void redraw_rightpillar(Widget, XExposeEvent *, String *, Cardinal *);
void redraw_screen(Widget, XExposeEvent *, String *, Cardinal *);
void draw_field(void);
void draw_title(void);
void redraw_screen(Widget, XExposeEvent  *, String *, Cardinal *);


#endif /* _sh_main_h_ */
