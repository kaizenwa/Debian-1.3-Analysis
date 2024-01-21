/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	5.ii.1996
 *	modified:
 *
 *	Main file for the statistics window
 */

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xlib.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>

#include "sh_stat.h"
#include "debug.h"
#include "tr_core.h"
#include "xtrojka.h"
#include "screen.h"
#include "window.h"

#include "pictures.h"
#include "_strdefs.h"

extern flag is_color;
extern Pixmap patPic[tc_blocks];

extern Widget screen;
extern Widget stat_screen;
extern Widget main_screen;

extern Colormap the_colormap;
extern int the_depth;

flag is_gc_created;
Widget stat_shell;	

Display *stat_dpy;
GC	stat_gc;
GC 	arc_gc;
XGCValues	arc_val;
Screen	*stat_scr;
int 	fheight;
int	scount;


void init_stat_window(void)
{
/*
 *	initialize the statistics window
 */
	XtTranslations mytrans;
	Dimension s_width, s_height;

	DEBUG("sh_stat.c", "init_stat_window")

	mytrans = XtParseTranslationTable("<Expose>: redraw_stat()");

	stat_shell = XtVaCreatePopupShell(
		STR_WIN_STAT,
		applicationShellWidgetClass,
		main_screen,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);


	stat_dpy = XtDisplay(stat_shell);
	stat_scr = XtScreen(stat_shell);
	stat_gc = DefaultGCOfScreen(stat_scr);
	is_gc_created = False;

	s_width = STAT_XSIZE;
	s_height = STAT_YSIZE;

	DEBUG("sh_stat.c", "init_stat_window")
	stat_screen =  XtVaCreateManagedWidget(
		"stat_screen",
		widgetClass,
		stat_shell,
		XtNwidth,	s_width,
		XtNheight,	s_height,
		XtNtranslations, mytrans,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);

	if(is_color)
		XtVaSetValues(stat_screen,
			XtNbackground,	app_data.color[BLACK],	
			NULL
		);
			
}


void show_stat(void)
{
/*
 *	popup stat box
 */
	DEBUG("sh_stat.c", "show_stat")

	XtPopup(stat_shell, XtGrabNone);
}


void redraw_stat(w, xexp, s, c)
Widget w;
XExposeEvent *xexp;
String *s;
Cardinal *c;
{
	DEBUG("sh_stat.c", "redraw_stat")

	if(!XtIsRealized(stat_shell))
		return;

	if(xexp->type != Expose)
		return;
/*
	fix_dimensions(stat_shell);
*/

	update_stat();
}


void update_stat(void)
{
	Widget w = stat_screen;
	Dimension xsize, ysize;
	unsigned long around = 360*64;
	unsigned long start_angle, inc_angle;
	int i;
	unsigned long vmask;

	DEBUG("sh_stat.c", "draw_stat")

	if(tv_blocks == 0)
		return;

	clear_window(stat_screen);

	if(!is_gc_created) {
		arc_val.graphics_exposures;
		arc_gc = XCreateGC(	stat_dpy,
					XtWindow(stat_shell),
					GCGraphicsExposures,
					&arc_val);
		is_gc_created = True;
	}

	start_angle = inc_angle = 0L;
	for(i = 0; i < tc_blocks; i++) {

		/*
		 *	calculate the angle and compensate for
		 *	precision loss
		 */
		inc_angle =
			 ((tv_block_count[i] * around) / tv_blocks);
		
/*
		if((start_angle >= around) || (i == tc_blocks - 1))
			new_angle = around - 1;
*/

		if(is_color) {
			arc_val.fill_style = FillSolid;
			arc_val.foreground = app_data.color[i+1];
			vmask = GCFillStyle | GCForeground;
		} else {
			arc_val.fill_style = FillTiled;
			arc_val.tile = patPic[i];
			vmask = GCFillStyle | GCTile;
		}
		
		XChangeGC(stat_dpy,
			arc_gc,	
			vmask,
			&arc_val);

			XtVaGetValues(w,
				XtNwidth, &xsize,
				XtNheight, &ysize,
				NULL
			);
			XFillArc(XtDisplay(w),
				XtWindow(w),
				arc_gc,
				0,0,
				xsize, ysize,
				(unsigned int)start_angle,
				(unsigned int)inc_angle);

		start_angle += inc_angle;
	}
	
}




