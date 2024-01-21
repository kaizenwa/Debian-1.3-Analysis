/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:
 *
 *	Do some basic drawing functions
 */

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xlib.h>
#include "screen.h"

#include "debug.h"
#include "tr_core.h"
#include "screen.h"
#include "xtrojka.h"
/*
#include "window.h"
#include "pictures.h"
*/

extern flag is_color;

/*
 *	implementation
 */
void set_color(w, fg, bg)
Widget w;
int fg, bg;
{
	DEBUG("screen.c", "set_color")

	if(is_color) {

		if(fg < 0)
			return;

		XSetForeground(XtDisplay(w),
			DefaultGCOfScreen(XtScreen(w)), app_data.color[fg]);

		if(bg < 0)
			return;

		XSetBackground(XtDisplay(w),
			DefaultGCOfScreen(XtScreen(w)), app_data.color[bg]);
	}

}


void paint_widget(w, fg, bg)
Widget w;
int fg,bg;
{
	DEBUG("screen.c", "paint_widget")

	set_color(w, fg, bg);
	XFillRectangle(	XtDisplay(w),
			XtWindow(w),
			DefaultGCOfScreen(XtScreen(w)), 0,0,
			WidthOfScreen(XtScreen(w)),
			HeightOfScreen(XtScreen(w))
	);
}


void draw_string(w, x,y,s)
Widget w;
int x,y;
char *s;
{
	DEBUG("screen.c", "draw_string")

	XDrawString(XtDisplay(w),
		XtWindow(w),
		DefaultGCOfScreen(XtScreen(w)),
		x,y, s, strlen(s)
	);
}

