/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	31.xii.1995
 *	modified:
 *
 *	Do drawing for the game window
 */


#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xlib.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include "sh_slist.h"
#include "sh_main.h"

#include "debug.h"
#include "xtrojka.h"
#include "screen.h"
#include "window.h"
#include "pictures.h"
#include "menu.h"
#include "tr_core.h"

extern flag is_color;
extern flag is_slick;

extern GAME_STATE game_state;

extern Widget screen;
extern Widget speed_box;	/* for "Score:" and score */
extern Widget score_box;	/* for "Speed:" and speed */
extern Widget leftpillar;
extern Widget rightpillar;

extern Pixmap blockPic[tc_blocks + 1];
#ifdef XPM
extern Pic	blockPicXpm[tc_blocks];
extern Pic 	leftpillarPicXpm;
extern Pic	rightpillarPicXpm;
#endif

extern Pixmap newPic;
extern Pixmap titlePic;
extern Pixmap rightpillarPic;
extern Pixmap leftpillarPic;

extern int speed;

/*
 *	implemenation
 */
void trojka_explode_callback(a,b)
int a,b;
{
}


void trojka_make_block_callback(x,y,i)
int x,y,i;
/*
 *	draws a block on the screen
 */
{
	DEBUG("sh_main.c", "makeblock")

	if(is_color) {
		set_color(screen, i, BLACK);
#ifdef XPM
		if(is_slick) {
			/*
			 * draw using a pixmap, except for an empty block which
			 * is drawn as a black rectangle
			 */
			if(i == 0) {
				set_color(screen, i, BLACK);
				XFillRectangle(XtDisplay(screen),
					XtWindow(screen), 
					DefaultGCOfScreen(XtScreen(screen)),
					x*BLOCK_XSIZE+2,
					(tc_pm_top-y) * BLOCK_YSIZE,
					BLOCK_XSIZE-1, BLOCK_YSIZE-1
				);
			} else {
				i--;
				XCopyArea(XtDisplay(screen),
					blockPicXpm[i].pic, XtWindow(screen),
					DefaultGCOfScreen(XtScreen(screen)),
					0,0,
					blockPicXpm[i].attr.width,
					blockPicXpm[i].attr.height, 
					x * BLOCK_XSIZE + 2,
					(tc_pm_top - y) * BLOCK_YSIZE
				);
			}
		} else {
#endif
			/*
			 * draw using a rectangle fill
			 */
			XFillRectangle(XtDisplay(screen), XtWindow(screen), 
				DefaultGCOfScreen(XtScreen(screen)),
				x*BLOCK_XSIZE + 2, (tc_pm_top-y) * BLOCK_YSIZE,
				BLOCK_XSIZE-1, BLOCK_YSIZE-1);
#ifdef XPM
		}
#endif
	} else {
		/*
		 * draw a pattern
		 */
		XCopyArea(XtDisplay(screen), blockPic[i], XtWindow(screen), 
			DefaultGCOfScreen(XtScreen(screen)),
			0,0, BLOCK_XSIZE, BLOCK_YSIZE,
			x * BLOCK_XSIZE + 2, (tc_pm_top - y) * BLOCK_YSIZE
		);
	}
}


void trojka_wipe_block_callback(x,y)
int x,y;
/*
 *	wipes a block from the screen
 */
{
	DEBUG("sh_main.c", "wipeblock")

	trojka_make_block_callback(x,y, 0);
}


void trojka_trojka_callback(val)
tt_long val;
{	/* JUST A DUMMY FUNC */
/*
	printf("Trojka! (%ld)\n", val);
*/
}


void trojka_spider_callback(val)
tt_long val;
{
/*
	printf("spider!(%ld)\n", val);
*/
}

void trojka_bottom_callback(val)
tt_long val;
{
/*
	printf("bottom!(%ld)\n", val);
*/
}


void show_score(void)
{
/*
 *	display the score 
 */
	char str_score[10];
	
	DEBUG("sh_main.c", "show_score")

	if(!XtIsRealized(score_box))
		return;


	sprintf(str_score,"%lu", tv_score);


	XtVaSetValues(score_box, 
		XtNlabel, str_score,
		NULL
	);
}


void trojka_speedup_callback(cb_speed)
int cb_speed;
{
	set_speed_item(cb_speed);
}


void show_speed(the_speed)
int the_speed;
{
/*
 *	Displays the speed
 */

	char	str_speed[10];

	DEBUG("sh_main.c", "show_speed")

	if(!XtIsRealized(speed_box))
		return;

	sprintf(str_speed,"%d", the_speed);
	XtVaSetValues(speed_box, 
		XtNlabel, str_speed,
		NULL
	);
}

/*
 *	these are redraw actions triggered by the expose event
 */
void redraw_leftpillar(w, xexp, s, c)
Widget w;
XExposeEvent *xexp;
String *s;
Cardinal *c;
{
	DEBUG("sh_main.c", "redraw_leftpillar")

	if(xexp->type != Expose)
		return;

	if(is_color) {
#ifndef XPM
		set_color(leftpillar, BLACK, WHITE);
		XCopyPlane(XtDisplay(leftpillar), leftpillarPic,
			XtWindow(leftpillar),
			DefaultGCOfScreen(XtScreen(leftpillar)),
			0,0, PILLAR_XSIZE, PILLAR_YSIZE, 0,0, 1
		);
#else
		XCopyArea(XtDisplay(leftpillar),
			leftpillarPicXpm.pic,
			XtWindow(leftpillar),
			DefaultGCOfScreen(XtScreen(leftpillar)),
			0,0,
			leftpillarPicXpm.attr.width,
			leftpillarPicXpm.attr.height, 
			0,0
		);
#endif
	} else {
		XCopyArea(XtDisplay(leftpillar), leftpillarPic,
			XtWindow(leftpillar),
			DefaultGCOfScreen(XtScreen(leftpillar)),
			0,0, PILLAR_XSIZE, PILLAR_YSIZE, 0,0
		);
	}
}


void redraw_rightpillar(w, xexp, s, c)
Widget w;
XExposeEvent *xexp;
String *s;
Cardinal *c;
{
	DEBUG("sh_main.c", "redraw_rightpillar")

	if(xexp->type != Expose)
		return;

	if(is_color) {
#ifndef XPM
		set_color(rightpillar, BLACK, WHITE);
		XCopyPlane(XtDisplay(rightpillar), rightpillarPic,
			XtWindow(rightpillar),
			DefaultGCOfScreen(XtScreen(rightpillar)),
			0,0, PILLAR_XSIZE, PILLAR_YSIZE, 0,0, 1);
#else
		XCopyArea(XtDisplay(rightpillar),
			rightpillarPicXpm.pic,
			XtWindow(rightpillar),
			DefaultGCOfScreen(XtScreen(rightpillar)),
			0,0,
			rightpillarPicXpm.attr.width,
			rightpillarPicXpm.attr.height, 
			0,0
		);
#endif
	} else {
		XCopyArea(XtDisplay(rightpillar), rightpillarPic,
			XtWindow(rightpillar),
			DefaultGCOfScreen(XtScreen(rightpillar)),
			0,0, PILLAR_XSIZE, PILLAR_YSIZE, 0,0);
	}
}


void draw_field(void)
{
	tt_command CMD_DRAWFIELD;
	
	DEBUG("sh_main.c", "draw_field")

	paint_widget(screen, BLACK, BLACK);

	CMD_DRAWFIELD.command = tc_c_drawfield;
	trojka_api(&CMD_DRAWFIELD);
}


void draw_title(void)
{
	DEBUG("sh_main.c", "draw_title")

	if(is_color) {
		paint_widget(screen, PURPLE, YELLOW);
		set_color(screen, YELLOW, PURPLE);
		XCopyPlane(XtDisplay(screen),
			titlePic, XtWindow(screen),
			DefaultGCOfScreen(XtScreen(screen)),
			0,0, title_width, title_height, TITLE_X, TITLE_Y, 1);
	} else {
		clear_window(screen);
		XCopyArea(XtDisplay(screen),
			titlePic, XtWindow(screen),
			DefaultGCOfScreen(XtScreen(screen)),
			0,0, title_width, title_height, TITLE_X, TITLE_Y);
	}
}


void redraw_screen(w, xexp, s, c)
Widget w;
XExposeEvent *xexp;
String *s;
Cardinal *c;
{
	DEBUG("sh_main.c", "redraw_screen")

	if(xexp->type != Expose)
		return;

	if(game_state == st_playing) {

		/*
		 * draw field
		 */
		draw_field();

	}
	if(game_state == st_idle) {

		/*
		 *	draw title
		 */
		draw_title();

	}
}

