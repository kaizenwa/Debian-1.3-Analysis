/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	31.xii.1995
 *	modified:
 *
 *	handle drawing of the highscore status window
 */

#include <stdio.h>
#include <pwd.h>
#include <time.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xlib.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>

#include "sh_slist.h"
#include "xtrojka.h"
#include "screen.h"
#include "window.h"
#include "font.h"
#include "scores.h"
#include "pictures.h"
#include "slist.h"

#include "debug.h"
#include "tr_core.h"
#include "_strdefs.h"

#define SLIST_X	30
#define SLIST_Y(y)	(((y) + 1) * 18)


extern flag is_color;
extern GAME_STATE game_state;
extern AppData app_data;

extern Widget screen;
extern Widget main_screen;
extern Widget slist_screen;
extern Widget slist_shell;
extern SCORES scores[NUMSCORES];
extern flag changed[NUMSCORES];
extern int position;

extern Colormap the_colormap;
extern int the_depth;

extern Pixmap newPic;

Display *slist_dpy;
GC	slist_gc;
Screen	*slist_scr;
Font	slist_font;
XFontStruct *slist_fontstr;
int 	slist_fh;		/* font height */

/*
 *	implementation
 */
		
void init_slist_window(void)
{
	XtTranslations mytrans;
	Dimension s_width, s_height;

	DEBUG("sh_slist.c", "init_slist_window")

	mytrans=XtParseTranslationTable("<Expose>: redraw_slist()");


	slist_shell = XtVaCreatePopupShell(
		STR_WIN_SCORES,
		applicationShellWidgetClass,
		main_screen,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);
	slist_dpy = XtDisplay(slist_shell);
	slist_scr = XtScreen(slist_shell);
	slist_gc = DefaultGCOfScreen(slist_scr);
	slist_font = XLoadFont(slist_dpy, app_data.slist_font);
	slist_fontstr = XQueryFont(slist_dpy, slist_font);
	s_width = FontWidth(slist_fontstr) * 68 + 20;
	s_height = FontHeight(slist_fontstr) * 20 + 20;

	XSetFont(slist_dpy, slist_gc,  slist_font);
	DEBUG("sh_slist.c", "init_slist_window")

	slist_screen =  XtVaCreateManagedWidget(
		"slist_screen",
		widgetClass,
		slist_shell,
		XtNtranslations,mytrans,
		XtNwidth,	s_width,
		XtNheight,	s_height,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);
	
	if(is_color)
		XtVaSetValues(slist_screen,
			XtNforeground,	app_data.color[WHITE],
			XtNbackground,	app_data.color[BLUE],
			NULL
		);
		

}

void draw_slist(mode)
int mode;
{
	char string[100];
	char date_st[30];
	char user_st[30];
	struct passwd *pw;
	flag f_changed;
	int i;

	DEBUG("sh_slist.c", "draw_slist");

	f_changed = file_changed(SCOREFILE);

	if(f_changed) {
		copy_oldscores();
		read_scores();
		compare_scores();
	} 

	if(!f_changed && (mode == kUNFORCED)) {
		return;
	}

	clear_window(slist_screen);

	if(is_color)
		set_color(slist_screen, WHITE, NO_COLOR);

	draw_string(slist_screen, SLIST_X, SLIST_Y(0), 
						app_data.str_best_players);
	draw_string(slist_screen, SLIST_X+1, SLIST_Y(0),
						app_data.str_best_players);


	for(i = 0;  i < NUMSCORES; i++) {
		if(scores[i].score == 0)
			break;

		strcpy(date_st, ctime(&(scores[i].date)));
		date_st[strlen(date_st)-1] = '\0';
		pw = getpwuid(scores[i].user);
		if(pw)
			strcpy(user_st, pw->pw_name);
		else
			sprintf(user_st,"(uid=%d)",scores[i].user);

		compose_score_string(string, i,scores[i], user_st, date_st);

		if(changed[i]) {
			draw_newpic(3, SLIST_Y(i+1) + 8);
		}
		draw_string(slist_screen, SLIST_X, SLIST_Y(i+2), string);
		if(position == i) {
			if(is_color)
				set_color(slist_screen, YELLOW, NO_COLOR);
			draw_string(slist_screen,SLIST_X+1,SLIST_Y(i+2),string);
			draw_string(slist_screen, SLIST_X, SLIST_Y(i+2), string);
			if(is_color)
				set_color(slist_screen, WHITE, NO_COLOR);
		} 
	}
}


void draw_newpic(x,y)
int x,y;
{
	DEBUG("sh_slist.c", "draw_newpic")

	if(is_color) {
		set_color(slist_screen, CYAN, BLUE);
		XCopyPlane(slist_dpy, newPic, XtWindow(slist_screen), slist_gc,
			0,0, new_width, new_height, x,y, 1);
		set_color(slist_screen, WHITE, BLUE);
	} else {
		XCopyArea(slist_dpy, newPic, XtWindow(slist_screen), slist_gc,
			0,0, new_width, new_height, x,y);
	}
}


void show_slist(void)
{
/*
 *	pops up the score list window
 */

	DEBUG("sh_slist.c", "show_slist")

	XtPopup(slist_shell, XtGrabNone);
}


void hide_slist(void)
{
/*
 *	pops down the score list window
 */

	DEBUG("sh_slist.c", "hide_slist")

	XtPopdown(slist_shell);
}



void redraw_slist(w, xexp, s, c)
Widget w;
XExposeEvent *xexp;
String *s;
Cardinal *c;
{
	DEBUG("sh_slist.c", "redraw_slist")

	if(xexp->type == Expose)
		draw_slist(kFORCED);
}


