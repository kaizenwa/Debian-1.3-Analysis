/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:	27.xii.1995
 *
 *	The main program
 */


#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include "xtrojka.h"
#include "scores.h"
#include "options.h"
#include "preferences.h"
#include "sh_slist.h"
#include "sh_stat.h"
#include "game.h"
#include "menu_tool.h"
#include "menu.h"
#include "window.h"
#include "pics.h"
#include "slist.h"
#include "actions.h"

#include "debug.h"
#include "tr_core.h"
#include "_strdefs.h"

extern Widget screen;
extern Widget slist_shell;

extern Widget main_screen;	/* the mother of all widgets */

XtAppContext	app_context;
Colormap	the_colormap;	/* the colormap of the application */
int		the_depth;	/* the colormap of the application */
Visual		*the_visual;
Colormap	my_colormap;

GAME_STATE game_state;

static char *COPYRIGHT = "@(#) xtrojka (c) 1994,1995,1996 Maarten Los";


flag	is_color;
flag	is_NOcolor;
flag	is_debug_info;
flag	is_wizard;
flag	is_slick;

int 	speed;
int	starting_speed;


static String fallback_resources[] = {

#include "_resdefs.h"

};

static XtResource resources[] = {
	{ "color0", "Color0", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[0]), XtRString, (caddr_t)"black" },
	{ "color1", "Color1", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[1]), XtRString, (caddr_t)"magenta" },
	{ "color2", "Color2", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[2]), XtRString, (caddr_t)"blue" },
	{ "color3", "Color3", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[3]), XtRString, (caddr_t)"cyan" },
	{ "color4", "Color4", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[4]), XtRString, (caddr_t)"yellow" },
	{ "color5", "Color5", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[5]), XtRString, (caddr_t)"green" },
	{ "color6", "Color6", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[6]), XtRString, (caddr_t)"red" },
	{ "color7", "Color7", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[7]), XtRString, (caddr_t)"purple" },
	{ "color8", "Color8", XtRPixel, sizeof(Pixel),
	  XtOffsetOf(AppData, color[8]), XtRString, (caddr_t)"white" },

	{ "slist_font", "Slist_font", XtRString, sizeof(String),
	  XtOffsetOf(AppData, slist_font), XtRString, (caddr_t)"7x13" },

	{ "game_font", "Game_font", XtRString, sizeof(String),
	  XtOffsetOf(AppData, game_font), XtRString, (caddr_t)"7x13bold" },

	{ "str_best_players", "Str_best_players", XtRString, sizeof(String),
	  XtOffsetOf(AppData, str_best_players), XtRString, (caddr_t)"<undef>" },

	{ "wstr_stat", "Wstr_stat", XtRString, sizeof(String),
	  XtOffsetOf(AppData, wstr_stat), XtRString, (caddr_t)"<undef>" },

	{ "wstr_open_prefs", "Wstr_open_prefs", XtRString, sizeof(String),
	  XtOffsetOf(AppData, wstr_open_prefs), XtRString, (caddr_t)"<undef>" },

	{ "wstr_create_prefs", "Wstr_create_prefs", XtRString, sizeof(String),
	  XtOffsetOf(AppData, wstr_create_prefs), XtRString, (caddr_t)"<undef>" },

	{ "wstr_write_prefs", "Wstr_write_prefs", XtRString, sizeof(String),
	  XtOffsetOf(AppData, wstr_write_prefs), XtRString, (caddr_t)"<undef>" },

	{ "wstr_read_prefs", "Wstr_read_prefs", XtRString, sizeof(String),
	  XtOffsetOf(AppData, wstr_read_prefs), XtRString, (caddr_t)"<undef>" },
};

AppData app_data;


main(argc, argv)
int argc;
char **argv;
{
	/*
	 *	set default values
	 */
	is_debug_info = False;
	is_NOcolor = False;

	/*
	 *	get the command line options
	 */
	get_options(argc, argv);
	if(is_debug_info)
		show_startup_string();

	/*
	 *	initialize the scores
	 */
	init_scores();


	/*
	 *	initialize the application
	 */
	DEBUG("xtrojka.c", "main/1")
	main_screen = XtVaAppInitialize(
		&app_context,	/* application context */
		"XTrojka",	/* application class */
		NULL,0,		/* commandlineoptionlist */
		&argc, argv,	/* command line arguments */
		fallback_resources,	/* fallback resources*/
		NULL);		/* terminate varargslist */

	/*
	 *	check if we have color
	 */
	DEBUG("xtrojka.c", "main/2")

	the_depth = DefaultDepthOfScreen(XtScreen(main_screen));
	is_color = the_depth > 1;

	the_colormap = DefaultColormap(XtDisplay(main_screen),
		DefaultScreen(XtDisplay(main_screen)));

	/*
	 *	now set the properties for the toplevel widget
	 */
	XtVaSetValues(main_screen,
		XtNcolormap, the_colormap,
		XtNdepth, the_depth,
		NULL
	);
	XSetWindowColormap(XtDisplay(main_screen), DefaultRootWindow(XtDisplay(main_screen)), the_colormap);

	/*
	 *	get the application resource from hardcoded data
	 */
	DEBUG("xtrojka.c", "main/3")
	XtGetApplicationResources(main_screen, &app_data, resources,
		XtNumber(resources), NULL, 0);

	/*
	 *	read the preferences
	 */
	init_preferences();

	/*
	 *	intialize menu's, windows, shells
	 */
	init_uif();

	/*
	 *	this is the last manual initialization before
	 *	the application is realized
	 */
	init_xtrojka();


	/*
	 *	Now install the routine that will catch the
	 *	'MapNotify' event
	 */
	init_map_catcher();

	/*
	 *	Realize the application. As soon as the application
	 *	is realized, a 'MapNotify' event will be generated
	 *	which will be caught by 'i_just_got_mapped_hlr()'.
	 */

	DEBUG("xtrojka.c", "main/4");
	XtRealizeWidget(main_screen);

	/*
	 *	Finally, start the game loop
	 */
	show_slist();
	show_stat();
	mainloop();
}



void quit_appl_action(w, unused, event, continue_to_dispatch)
Widget w;
XtPointer unused;
XEvent *event;
Boolean *continue_to_dispatch;
/*
 *	this callback is called on 'Alt-Q'
 */
{
	quit_appl();
}


void quit_appl(void)
{
	DEBUG("xtrojka.c", "quit_appl")

	write_prefs();	
	exit(0);
}


void init_xtrojka(void)
{
	DEBUG("xtrojka.c", "initialize")

	check_wizard_item(is_wizard);	
	check_slick_item(is_slick);	
	set_speed_item(speed);
}


void init_uif()
{
	/*
	 *	first initialize the basic widget system
	 */
	init_windows();

	/*
	 *	now initialize the subsystem
	 */
	init_menu_sys();

	init_bitmaps();


	set_icons();
}


void init_map_catcher(void)
/*
 *	install a map handler. Call just before 'XtAppRealize'
 */
{
	XtAddEventHandler(main_screen,
		StructureNotifyMask,
		False,
		(XtEventHandler)i_just_got_mapped_hlr,
		(Opaque)NULL
	);
}


void i_just_got_mapped_hlr(w, p, me, continue_to_dispatch)
Widget w;
XtPointer p;
XMapEvent *me;
Boolean *continue_to_dispatch;
{
	*continue_to_dispatch = TRUE;


	if(me->type != MapNotify)
		return;

	fix_dimensions(main_screen);
	fix_dimensions(slist_shell);

	init_slist_mgr();
	init_actions();
	init_event_handlers();
}


void show_no_debug(void)
{
	fprintf(stderr,"%s\n", STR_NO_DEBUG);
}


void show_help(void)
{
	fprintf(stderr,"%s: xtrojka [-scores] [-help] [-debug] ", STR_USAGE);
	fprintf(stderr,"[%s] \n\n", STR_X11_OPTIONS);
	fprintf(stderr,"-scores\t%s\n", STR_DISP_SCORE);
	fprintf(stderr,"-help\t%s\n", 	STR_THIS_HELP);
	fprintf(stderr,"-debug\t%s\n",	STR_SHOW_DEBUG);
	fprintf(stderr,"%s.\n\n", 	STR_FOR_X11);
	fflush(stderr);
}	


void show_startup_string(void)
{
	fprintf(stderr,"Xtrojka %s\n", VERSION);

	fprintf(stderr,"%s:\n",STR_COM_OPT);
#ifdef LOCKING
	fprintf(stderr,"\tLOCKING\n");
#endif
#ifdef DEBUG_INFO
	fprintf(stderr,"\tDEBUG_INFO\n");
#endif
#ifdef XPM
	fprintf(stderr,"\tXPM\n");
#endif
	fputc('\n', stderr);

	fprintf(stderr,"%s: %s\n", STR_SCOREFILE, SCOREFILE);

	fflush(stderr);
}




