/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	27.xi.1995
 *	modified:	27.xii.1995
 *
 *	Low level window/widget manager
 */


#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>

#include "xtrojka.h"
#include "window.h"
#include "tr_core.h"
#include "sh_slist.h"
#include "sh_stat.h"
#include "debug.h"


Widget trojkamenu_but;
Widget speedmenu_but;
extern XtAppContext app_context;

extern flag is_color;

extern void redraw_leftpillar();
extern void redraw_rightpillar();
extern void redraw_screen();
extern void redraw_stat();
extern void redraw_slist();

extern AppData app_data;

extern Colormap the_colormap;
extern int the_depth;


Widget main_screen;
Widget form;
Widget slist_shell;
Widget leftpillar;
Widget rightpillar;	
Widget screen;
Widget slist_screen;
Widget stat_screen;
Widget speed_label;
Widget speed_box;	/* for "Score:" and score */
Widget score_label;
Widget score_box;	/* for "Speed:" and speed */

XFontStruct *font;

XtTranslations mytrans;

void init_windows(void)
/*
 *	initialize the whole widget stuff, INCLUDING the menu buttons.
 *	The rest of the menu-stuff is handled in 'scrx_menu.c'
 */
{
	static XtActionsRec redraw_actions[] = {
		{ "redraw_rightpillar", redraw_rightpillar },
		{ "redraw_leftpillar", redraw_leftpillar },
		{ "redraw_screen", redraw_screen },
		{ "redraw_stat", redraw_stat },
		{ "redraw_slist", redraw_slist }
	};

	DEBUG("window.c", "init_windows")

	DEBUG("\tinit_windows", "form")
	form = XtVaCreateManagedWidget(
		"form", formWidgetClass, main_screen,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);

	DEBUG("\tinit_windows", "trojkamenu_but")
	trojkamenu_but = XtVaCreateManagedWidget(
		"trojka_button", commandWidgetClass, form,
		NULL
	);


	DEBUG("\tinit_windows", "speedmenu_but")
	speedmenu_but = XtVaCreateManagedWidget(
		"speed_button", commandWidgetClass, form,
		XtNfromHoriz,	trojkamenu_but,
		NULL
	);

	DEBUG("\tinit_windows", "score_label")
	score_label = XtVaCreateManagedWidget(
		"score_label", labelWidgetClass, form,
		XtNfromVert,	trojkamenu_but,
		XtNborderWidth,	0,
		NULL
	);

	DEBUG("\tinit_windows", "score_box")
	score_box = XtVaCreateManagedWidget(
		"score_box", labelWidgetClass, form,
		XtNfromVert,	trojkamenu_but,
		XtNfromHoriz,	score_label,
		XtNlabel,	"         ",
		XtNborderWidth,	0,
		NULL
	);

	DEBUG("\tinit_windows", "speed_label")
	speed_label = XtVaCreateManagedWidget(
		"speed_label", labelWidgetClass, form,
		XtNfromVert,	trojkamenu_but,
		XtNfromHoriz,	score_box,
		XtNborderWidth,	0,
		NULL
	);

	DEBUG("\tinit_windows", "speed_box")
	speed_box = XtVaCreateManagedWidget(
		"speed_box", labelWidgetClass, form,
		XtNlabel,	"       ",
		XtNfromVert,	trojkamenu_but,
		XtNfromHoriz,	speed_label,
		XtNborderWidth,	0,
		NULL
	);
	
	DEBUG("\tinit_windows", "leftpillar")
	mytrans = XtParseTranslationTable("<Expose>: redraw_leftpillar()");
	leftpillar =  XtVaCreateManagedWidget(
		"leftpillar", widgetClass, form,
		XtNwidth,	PILLAR_XSIZE,
		XtNheight,	WIN_YSIZE,
		XtNfromVert,	score_box,
		XtNtranslations,mytrans,
		XtNborderWidth,	0,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);

	DEBUG("\tinit_windows", "screen")
	mytrans = XtParseTranslationTable("<Expose>: redraw_screen()");
	screen =  XtVaCreateManagedWidget(
		"screen", widgetClass, form,
		XtNwidth,	WIN_XSIZE,
		XtNheight,	WIN_YSIZE,
		XtNfromHoriz,	leftpillar,
		XtNfromVert,	score_box,
		XtNtranslations,mytrans,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);


	DEBUG("\tinit_windows", "rightpillar")
	mytrans = XtParseTranslationTable("<Expose>: redraw_rightpillar()");
	rightpillar =  XtVaCreateManagedWidget(
		"rightpillar", widgetClass, form,
		XtNwidth,	PILLAR_XSIZE,
		XtNheight,	WIN_YSIZE,
		XtNfromHoriz,	screen,
		XtNfromVert,	score_box,
		XtNborderWidth,	0,
		XtNtranslations,mytrans,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);

	DEBUG("\tinit_windows", "if(is_color...")
	if(is_color) {
		XtVaSetValues(screen,
			XtNforeground, app_data.color[WHITE],
			XtNbackground, app_data.color[BLACK],
			NULL
		);
	}

	init_slist_window();
	init_stat_window();

	XtAppAddActions(app_context, redraw_actions, XtNumber(redraw_actions));
}


		



void fix_dimensions(shell)
Widget shell;
{
	Dimension width, height;
	
	DEBUG("window.c", "fix_dimensions")

	XtVaGetValues(shell,
		XtNwidth, &width,
		XtNheight, &height,
		NULL
	);
	
	XtVaSetValues(shell,
		XtNmaxWidth, width,
		XtNminWidth, width,
		XtNmaxHeight, height,
		XtNminHeight, height,
		NULL
	);
}



void clear_window(w)
Widget w;
{
	DEBUG("window.c", "clear_window")

	XClearWindow(XtDisplay(w), XtWindow(w));
}





