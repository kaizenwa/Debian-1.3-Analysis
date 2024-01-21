/* Initialization for the X11 interface to Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "xconq.h"
extern int smallest_image PARAMS ((ImageFamily *imf, int *wp, int *hp));

extern XrmOptionDescRec xoptions[];
extern int xoptions_count;

#ifndef __STDC__
void abort ();
#endif

static void init_gcs PARAMS ((Side *side));
static void init_fonts PARAMS ((Side *side));
static void init_bitmaps PARAMS ((Side *side));
static void init_cursors PARAMS ((Side *side));

static void init_unit_images PARAMS ((Side *side));
static void init_terrain_images PARAMS ((Side *side));
static void init_emblem_images PARAMS ((Side *side));
static void init_emblem PARAMS ((Side *side, Side *side2));
static void get_default_terrain_image PARAMS ((Side *side, ImageFamily *imf,
					      int t));
static void get_default_emblem PARAMS ((Side *side, ImageFamily *imf,
				       Side *side2));
static void describe_imf PARAMS ((Side *side, char *classname, char *typename,
				 ImageFamily *imf));

static void x11_interp_fn PARAMS ((ImageFamily *imf));
static void x11_load_fn PARAMS ((ImageFamily *imf));

typedef struct s_varw {
  Widget dialog;
  char *value;
  char *mess;
} Varw;

typedef struct s_assignw {
  Widget sidew, pname, pid;
  char *vname, *vid, *messname, *messid;
  int vflag;
} Assignw;

typedef struct s_gamepop {
  Widget shell;
  Widget form;
  Widget name;
  Widget basemodule;
  Widget version;
  Widget title;
  Widget blurb;
  Widget instructions;
  Widget notes;
  Widget ok;
  Widget add;
  Widget verify;
  Widget cancel;
  Varw *variants;
  int numvariants;
  Assignw *assign;
} GamePop;

typedef struct s_gamew {
  Widget form;
  Widget name;
  Widget title;
  Widget blurb;
  GamePop *pop;
} Gamew;


static void popup_game PARAMS ((int g, int flag));
static void quit_dialog PARAMS ((Widget w, XtPointer client_data,
				 XtPointer call_data));
static void game_callback PARAMS ((Widget w, XtPointer client_data,
				   XtPointer call_data));
static void cancel_dialog PARAMS ((Widget w, XtPointer client_data,
				   XtPointer call_data));
static void ok_dialog PARAMS ((Widget w, XtPointer client_data,
			       XtPointer call_data));
static void go_dialog PARAMS ((Widget w, XtPointer client_data,
			       XtPointer call_data));
static void verify_dialog PARAMS ((Widget w, XtPointer client_data,
				   XtPointer call_data));
static void add_player_dialog PARAMS ((Widget w, XtPointer client_data,
				       XtPointer call_data));
static int valid_gamepop PARAMS ((void));
static void splat_obj_in_textw PARAMS ((Widget w, Obj *obj));
static Widget createTextWidget PARAMS ((char *name, Widget parent,
					Widget up, Obj *obj));
static char *variant_default PARAMS ((Variant *var));
static char *variant_name PARAMS ((Variant *var));
static void horizontalDialog PARAMS ((Widget w));
static void reverseVideoDialog PARAMS ((Widget w));
static void implement_variant PARAMS ((char *value, Variant *var));
static int valid_variant PARAMS ((char *value, Variant *var));
static int type_player PARAMS ((char *value));
static void init_assignw_player PARAMS ((Assignw *assignw, Player *player));
static int can_open_display PARAMS ((char *dpyname));
static int boolean_lisp PARAMS ((char *value));

static int loop, chosen_game;

static Widget gameShell;

static Gamew *games;

/* we could share this buffers with other modules */
char buf1[BUFSIZE], buf2[BUFSIZE], buf3[BUFSIZE], errormess[BUFSIZE];

Obj *variants;

char *imflib = IMFLIB;

/* Display and window globals used in callbacks from generic imf
   handling code. */

Display *tmp_display;

Window tmp_root_window;

/* How much space to leave for a unit image, if all images should get
   the same amount (such as for a list of unit types). */

int min_w_for_unit_image = 16;
int min_h_for_unit_image = 16;

/* various bitmap definitions. */

#include "bitmaps/lookglass.b"
#include "bitmaps/lookmask.b"
#include "bitmaps/movecurs.b"
#include "bitmaps/movemask.b"
#include "bitmaps/shootcurs.b"
#include "bitmaps/shootmask.b"
#include "bitmaps/buildcurs.b"
#include "bitmaps/buildmask.b"

#include "bitmaps/boxcurs.b"
#include "bitmaps/boxmask.b"

#include "bitmaps/bomb1.b"
#include "bitmaps/bomb2.b"
#include "bitmaps/bomb3.b"
#include "bitmaps/bomb4.b"

#include "bitmaps/miss.b"
#include "bitmaps/hit.b"
#include "bitmaps/kill.b"

#include "bitmaps/hex8.b"
#include "bitmaps/hex8b.b"
#include "bitmaps/hex16.b"
#include "bitmaps/hex16b.b"
#include "bitmaps/hex32.b"
#include "bitmaps/hex32b.b"

#include "bitmaps/closer.b"
#include "bitmaps/farther.b"

#include "bitmaps/gray.b"
#include "bitmaps/darkgray.b"

#ifdef DESIGNERS
#include "bitmaps/hexcurs.b"
#include "bitmaps/hexcursmask.b"
#include "bitmaps/bordcurs.b"
#include "bitmaps/conncurs.b"
#include "bitmaps/addunits.b"
#include "bitmaps/people.b"
#include "bitmaps/feature.b"
#include "bitmaps/featuremask.b"
#endif /* DESIGNERS */

/* (add arrays of info about various drawing styles and allowable options) */

enum whattouse terrstyles[] = {
    useblocks,
    useblocks,
    useblocks,
    useblocks,
    usepictures,
    usepictures,
    usepolygons,
    usepolygons
};

/* The set of X11 resources that Xconq knows about. */

static XtResource resources[] = {
    { XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
      XtOffsetOf(UI, foreground), XtRString, XtDefaultForeground },
    { XtNbackground, XtCBackground, XtRPixel, sizeof (Pixel),
      XtOffsetOf(UI, background), XtRString, XtDefaultBackground },
    { XtNgeometry, XtCGeometry, XtRString, sizeof (String),
      XtOffsetOf(UI, geospec), XtRString, NULL },
};

/* Do a precheck of the intended displays.  This happens just after
   player spec reading so that any losing displays can be found before
   game startup gets in too deep. */

void
check_player_displays()
{
    int numfailures = 0;
    char *dpyname;
    Player *player;
    Display *dpy;

    for_all_players(player) {
	/* Try to open the display. */
	dpyname = player->displayname;
	if (dpyname != NULL) {
	    if (strcmp("_", dpyname) == 0)
	      dpyname = getenv("DISPLAY");
	    dpy = XOpenDisplay(dpyname);
	    if (dpy == NULL) {
		fprintf(stderr, "Cannot open display \"%s\"!\n", dpyname);
		++numfailures;
	    } else {
		XCloseDisplay(dpy);
	    }
	}
    }
    if (numfailures > 0) {
	init_error("can't open all the required displays");
	exit(1);
    }
}

/* Set up the basic user interface for a side.  This is called from the
   kernel while doing the final assignment of players to sides, and
   does not cause any display activity. */

void
init_ui(side)
Side *side;
{
    if (side_wants_display(side)) {
        side->ui = (UI *) xmalloc(sizeof(UI));
	Dprintf("One UI is %d bytes.\n", sizeof(UI));
    } else {
	side->ui = NULL;
    }
}

/* Set up all sides' displays all at once. */

int numdisplays;

void
init_all_displays()
{
    Side *side;

    numdisplays = 0;
    for_all_sides(side) {
	if (side_has_display(side)) {
	    init_display(side);
	    ++numdisplays;
	}
    }
    if (numdisplays == 0) {
	fprintf(stderr, "Must have at least one display to start.\n");
	exit(0);
    }
}

/* Do a full redraw on each display. */

void
init_redraws()
{
    Side *side;

    for_all_sides(side) {
	if (side->ui != NULL) {
	    /* The moment of truth - up to now output has been suppressed. */
	    side->ui->active = TRUE;
	    draw_all_maps(side);
	}
    }
}

/* Open display, create all the windows we'll need, do misc setup things,
   and initialize some globals to out-of-range values for recognition later. */

int toplevel_display_used = FALSE;

void
init_display(side)
Side *side;
{
    int argc, p, u, t, s, depth;
    char *dpyname, *str;
    Display *dpy;

    dpyname = side->player->displayname;

    Dprintf("Will try to open %s display `%s'...\n",
	    side_desig(side), dpyname);

    /* Detect the placeholder for the default display. */
    if (strcmp("_", dpyname) == 0)
      dpyname = getenv("DISPLAY");

    /* See if we can use the already-opened display for this side;
       if not, then open one. */
    dpy = NULL;
    if (!toplevel_display_used) {
	str = XDisplayString(XtDisplay(thistoplevel));
	if (!empty_string(str) && strcmp(str, dpyname) == 0) {
	    dpy = XtDisplay(thistoplevel);
	    side->ui->shell = thistoplevel;
	    toplevel_display_used = TRUE;
	}
    }
    if (dpy == NULL) {
	argc = 0;
	dpy = XtOpenDisplay(thisapp, dpyname, PROGRAMNAME, PROGRAMCLASSNAME,
			    xoptions, xoptions_count, &argc, NULL);
	side->ui->shell = XtAppCreateShell(PROGRAMNAME, PROGRAMCLASSNAME,
					  topLevelShellWidgetClass, dpy,
					  tmpargs, 0);
    }
    side->ui->dpy = dpy;

    if (DebugG)
      XSynchronize(side->ui->dpy, True);

    XtGetApplicationResources(side->ui->shell, side->ui,
			      resources, XtNumber(resources), NULL, 0);

    side->ui->kill_atom = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    /* Cache some generally useful values. */
    side->ui->screen = DefaultScreen(dpy);
    side->ui->rootwin = DefaultRootWindow(dpy);

    /* Set up things shared by all the windows. */
    side->ui->dflt_color_terr_images = TRUE;
    side->ui->dflt_color_unit_images = TRUE;
    side->ui->dflt_color_embl_images = TRUE;
    side->ui->bonw = BLACKONWHITE;
    depth = XDisplayCells(dpy, side->ui->screen);
    DGprintf("%d different colors available ...\n", depth);
    side->ui->monochrome = (depth <= 2);
    for (p = 0; p < NUMPOWERS; ++p) {
	for_all_terrain_types(t) {
	    /* Decide what display technique to use at each power. */
	    side->ui->usewhat[p][t] = terrstyles[p];
	    side->ui->terrpics[p][t] = None;
	    side->ui->terrchars[p][t] = '\0';
	    side->ui->terrfonts[p][t] = NULL;
	}
	for_all_unit_types(u) {
	    side->ui->unitpics[p][u] = None;
	    side->ui->unitmasks[p][u] = None;
	    side->ui->unitchars[p][u] = '\0';
	    side->ui->unitfonts[p][u] = NULL;
	    side->ui->ulegendfonts[p][u] = NULL;
	    /* (these values really should be filled in better) */
	    side->ui->unitw[p][u] = 1;  side->ui->unith[p][u] = 1;
	}
    }
    for (s = 0; s < MAXSIDES; ++s) {
	side->ui->emblempics[s] = None;
	side->ui->emblemmasks[s] = None;
    }
    set_colors(side);
    init_fonts(side);
    init_unit_images(side);
    init_terrain_images(side);
    init_emblem_images(side);
    init_bitmaps(side);
    init_gcs(side);
    init_cursors(side);
    /* Create the generic windows. */
    side->ui->maps = NULL;

    create_map(side, 5, side->ui->geospec);

    /* At this point, the toplevel widget has been realized, so we can */
    /* start doing things to it...  So, first things first: */
    XSetWMProtocols(dpy, XtWindow(side->ui->shell),
		    &side->ui->kill_atom, 1);
    XtOverrideTranslations(side->ui->shell,
	XtParseTranslationTable("<Message>WM_PROTOCOLS: wm-quit()"));

#ifdef DESIGNERS
    /* If this side is already a designer (perhaps via command-line option)
       popup the design controls now. */
    if (side->designer)
      popup_design(side);
#endif /* DESIGNERS */

    /* Preallocate space needed to save display state. */
    ui_save_state(side);

    Dprintf("Successfully initialized `%s'!\n", dpyname);
}

/* This will set up the correct set of colors at any point in the game.
   Colors are all specified by name; if any are not available, it is up to
   the graphics interface to supply a substitute. */

void
set_colors(side)
Side *side;
{
    int	t;
    char *colorname, substname[BUFSIZE];
    char *unseencolorname = "black";
    char *gridcolorname = "black";

    side->ui->fgcolor = side->ui->foreground;
    side->ui->bgcolor = side->ui->background;
    side->ui->whitecolor = request_color(side, "white");
    side->ui->blackcolor = request_color(side, "black");
    side->ui->diffcolor = side->ui->blackcolor;
    side->ui->graycolor = side->ui->whitecolor;
    side->ui->enemycolor = side->ui->whitecolor;
    side->ui->neutcolor = side->ui->whitecolor;
    side->ui->goodcolor = side->ui->badcolor = side->ui->whitecolor;
    if (!empty_string(g_grid_color()))
      gridcolorname = g_grid_color();
    side->ui->gridcolor =
      (strcmp(gridcolorname, "white") == 0 ? side->ui->whitecolor : side->ui->blackcolor);
    if (!empty_string(g_unseen_color()))
      unseencolorname = g_unseen_color();
    side->ui->unseencolor =
      (strcmp(unseencolorname, "white") == 0 ? side->ui->whitecolor : side->ui->blackcolor);
    for_all_terrain_types(t) {
	if (side->ui->monochrome) {
	    side->ui->cellcolor[t] = side->ui->blackcolor;
	} else {
	    colorname = t_color(t);
	    /* Warn if we have to substitute a valid color name. */
	    if (empty_string(colorname)) {
		/* Substitute grays 33 to 99 only. */
		sprintf(substname, "gray%d", ((t * 66) / numttypes) + 33);
		init_warning("no color for terrain type \"%s\", using \"%s\"",
			     t_type_name(t), substname);
		colorname = substname;
	    }
	    side->ui->cellcolor[t] = request_color(side, colorname);
	}
    }
    /* Get special-purpose colors. */
    if (!side->ui->monochrome) {
	side->ui->diffcolor = request_color(side, "maroon");
	side->ui->graycolor = request_color(side, "light gray");
	side->ui->enemycolor = request_color(side, "red");
	side->ui->neutcolor = request_color(side, "gray");
	side->ui->goodcolor = request_color(side, "green");
	side->ui->badcolor = request_color(side, "red");
	side->ui->gridcolor = request_color(side, gridcolorname);
	side->ui->unseencolor = request_color(side, unseencolorname);
    }
}

/* A predicate that tests whether our display can safely be written to. */
/* (should have a macro version for internal-to-interface use?) */

int
active_display(side)
Side *side;
{
    return (side && side->ui && side->ui->active);
}

/* Set up all the GCs. */

static void
init_gcs(side)
Side *side;
{
    unsigned long mask;
    XGCValues values;
    GC gc;
    Display *dpy = side->ui->dpy;
    Window rootwin = side->ui->rootwin;

    gc = DefaultGCOfScreen(DefaultScreenOfDisplay(dpy));

    side->ui->gc = XCreateGC(dpy, rootwin, 0L, NULL);
    side->ui->textgc = XCreateGC(dpy, rootwin, 0L, NULL);
    side->ui->ltextgc = XCreateGC(dpy, rootwin, 0L, NULL);
    side->ui->terrgc = XCreateGC(dpy, rootwin, 0L, NULL);
    side->ui->unitgc = XCreateGC(dpy, rootwin, 0L, NULL);
    side->ui->emblgc = XCreateGC(dpy, rootwin, 0L, NULL);
    side->ui->bdrygc = XCreateGC(dpy, rootwin, 0L, NULL);

    /* Set the fonts associated with each GC. */
    XSetFont(dpy, side->ui->gc, side->ui->textfont->fid);
    XSetFont(dpy, side->ui->textgc, side->ui->textfont->fid);
    XSetFont(dpy, side->ui->ltextgc, side->ui->textfont->fid);
    if (side->ui->unitfont != NULL)
      XSetFont(dpy, side->ui->unitgc, side->ui->unitfont->fid);
 
    /* Set misc properties of each GC. */
    mask = GCFillStyle | GCGraphicsExposures;
    values.fill_style = FillSolid;
    values.graphics_exposures = FALSE;
    XChangeGC(dpy, side->ui->terrgc, mask, &values);
    XChangeGC(dpy, side->ui->unitgc, mask, &values);
    XChangeGC(dpy, side->ui->emblgc, mask, &values);
    XChangeGC(dpy, side->ui->bdrygc, mask, &values);

    DGprintf("GCs stored ...\n");
}

/* Set up all the fonts. */

static void
init_fonts(side)
Side *side;
{
    int p, u;
    XFontStruct *font;

    side->ui->textfont =
      open_font(side, TEXTFONT, "TextFont", NULL, "", NULL);
    side->ui->fw = font_width(side->ui->textfont);
    side->ui->fh = font_height(side->ui->textfont);
    font =
      open_font(side, "*-bold-r-*-10-*", "LegendFont",
		side->ui->textfont, "text", NULL);
    for (p = 0; p < NUMPOWERS; ++p) {
	for_all_unit_types(u) {
	    side->ui->ulegendfonts[p][u] = font;
	}
    }
    side->ui->flegendfonts[0] =
      open_font(side, "-*-helvetica-medium-r-*-*-8-*-*-*-*-*-*-*",
		"LegendFont0", side->ui->textfont, "text",
		&side->ui->flegendfids[0]);
    side->ui->flegendfonts[1] =
      open_font(side, "-*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*",
		"LegendFont1", side->ui->textfont, "text",
		&side->ui->flegendfids[1]);
    side->ui->flegendfonts[2] =
      open_font(side, "-*-helvetica-medium-r-*-*-14-*-*-*-*-*-*-*",
		"LegendFont2", side->ui->textfont, "text",
		&side->ui->flegendfids[2]);
    side->ui->flegendfonts[3] =
      open_font(side, "-*-helvetica-medium-r-*-*-18-*-*-*-*-*-*-*",
		"LegendFont3", side->ui->textfont, "text",
		&side->ui->flegendfids[3]);
    side->ui->flegendfonts[4] =
      open_font(side, "-*-helvetica-medium-r-*-*-24-*-*-*-*-*-*-*",
		"LegendFont4", side->ui->textfont, "text",
		&side->ui->flegendfids[4]);
    DGprintf("Fonts opened ...\n");
}

/* Get a color set up and warn if not getting what was asked for.  We can */
/* tolerate being off somewhat.  (Note X rgb value range is 0-65535.) */

#define CLOSE_ENOUGH(X,Y) (ABS(((int) X) - ((int) Y)) < 2000)

long
request_color(side, name)
Side *side;
char *name;
{
    XColor c, avail;

    DGprintf("Requesting color %s\n", (name ? name : "<null>"));
    /* This might be called to get user-specified colors, even on a mono
       display, so deal with it. */
    if (empty_string(name)) {
	init_warning(
          "Requesting anonymous color on display \"%s\", substituting white",
		     side->player->displayname);
	return WhitePixel(side->ui->dpy, side->ui->screen);
    } else if (side->ui->monochrome) {
	if (strcmp("white", name) == 0) {
	    return WhitePixel(side->ui->dpy, side->ui->screen);
	} else if (strcmp("black", name) == 0) {
	    return BlackPixel(side->ui->dpy, side->ui->screen);
	} else {
	    init_warning(
              "No color \"%s\" on the mono display \"%s\", substituting white",
			 name, side->player->displayname);
	    return WhitePixel(side->ui->dpy, side->ui->screen);
	}
    } else {
	XAllocNamedColor(side->ui->dpy,
			 DefaultColormap(side->ui->dpy, side->ui->screen),
			 name, &avail, &c);
	if (!(CLOSE_ENOUGH(c.red, avail.red)
	      && CLOSE_ENOUGH(c.green, avail.green)
	      && CLOSE_ENOUGH(c.blue, avail.blue))) {
	    init_warning("%s color is way off on display \"%s\"!",
			 name, side->player->displayname);
	    init_warning("(%d %d %d instead of %d %d %d), but using it anyway",
			 avail.red, avail.green, avail.blue,
			 c.red, c.green, c.blue);
	}
	return avail.pixel;
    }
}

static void
init_bitmaps(side)
Side *side;
{
    int i;
    Display *dpy = side->ui->dpy;
    Window win = side->ui->rootwin;

    /* Get the solid hex outlines. */
    side->ui->hexpics[3] =
      XCreateBitmapFromData(dpy, win, hex8_bits, hex8_width, hex8_height);
    side->ui->bhexpics[3] =
      XCreateBitmapFromData(dpy, win, hex8b_bits, hex8b_width, hex8b_height);
    side->ui->hexpics[4] =
      XCreateBitmapFromData(dpy, win, hex16_bits, hex16_width, hex16_height);
    side->ui->bhexpics[4] =
      XCreateBitmapFromData(dpy, win, hex16b_bits, hex16b_width, hex16b_height);
    side->ui->hexpics[5] =
      XCreateBitmapFromData(dpy, win, hex32_bits, hex32_width, hex32_height);
    side->ui->bhexpics[5] =
      XCreateBitmapFromData(dpy, win, hex32b_bits, hex32b_width, hex32b_height);
    /* Get pictures of mushroom clouds. */
    side->ui->bombpics[0] =
      XCreateBitmapFromData(dpy, win, bomb1_bits, bomb1_width, bomb1_height);
    side->ui->bombpics[1] =
      XCreateBitmapFromData(dpy, win, bomb2_bits, bomb2_width, bomb2_height);
    side->ui->bombpics[2] =
      XCreateBitmapFromData(dpy, win, bomb3_bits, bomb3_width, bomb3_height);
    side->ui->bombpics[3] =
      XCreateBitmapFromData(dpy, win, bomb4_bits, bomb4_width, bomb4_height);
    side->ui->hitpics[0] =
      XCreateBitmapFromData(dpy, win, miss_bits, miss_width, miss_height);
    side->ui->hitpics[1] =
      XCreateBitmapFromData(dpy, win, hit_bits, hit_width, hit_height);
    side->ui->hitpics[2] =
      XCreateBitmapFromData(dpy, win, kill_bits, kill_width, kill_height);
    side->ui->boxcurs =
      XCreateBitmapFromData(dpy, win, boxcurs_bits, boxcurs_width, boxcurs_height);
    side->ui->boxmask =
      XCreateBitmapFromData(dpy, win, boxmask_bits, boxmask_width, boxmask_height);
    side->ui->grays[gray] =
      XCreateBitmapFromData(dpy, win, gray_bits, gray_width, gray_height);
    side->ui->grays[darkgray] =
      XCreateBitmapFromData(dpy, win, darkgray_bits, darkgray_width, darkgray_height);
    /* Collect pictures that will be used with map controls. */
    for (i = 0; i < numcontrols; ++i)
      side->ui->controlpics[i] = None;
    side->ui->controlpics[LOOK] =
      XCreateBitmapFromData(dpy, win, lookglass_bits, 16, 16);
    side->ui->controlpics[MOVE] =
      XCreateBitmapFromData(dpy, win, shootcursor_bits, 16, 16);
    side->ui->controlpics[UNIT_MOVE] =
      XCreateBitmapFromData(dpy, win, movecursor_bits, 16, 16);
    side->ui->controlpics[UNIT_SHOOT] =
      XCreateBitmapFromData(dpy, win, shootcursor_bits, 16, 16);
    side->ui->controlpics[UNIT_BUILD] =
      XCreateBitmapFromData(dpy, win, buildcursor_bits, 16, 16);
    side->ui->controlpics[ZOOM_OUT] =
      XCreateBitmapFromData(dpy, win, farther_bits, 16, 16);
    side->ui->controlpics[ZOOM_IN] =
      XCreateBitmapFromData(dpy, win, closer_bits, 16, 16);
    DGprintf("Bitmaps stored ...\n");
}

/* Make all the different kinds of cursors we intend to use.  Should be
   one for each kind of mode or tool.  Cursors should always be 16x16. */

static void
init_cursors(side)
Side *side;
{
    int i;
    Display *dpy = side->ui->dpy;
    Window win = side->ui->rootwin;
    int fg = side->ui->fgcolor, bg = side->ui->bgcolor;

    for (i = 0; i < numtools; ++i)
      side->ui->toolcursors[i] = None;
    side->ui->toolcursors[looktool] =
      make_cursor(dpy, win, lookglass_bits, lookmask_bits,
		  fg, bg, lookglass_x_hot, lookglass_y_hot);
    side->ui->toolcursors[movetool] =
      make_cursor(dpy, win, shootcursor_bits, shootmask_bits,
		  fg, bg, shootcursor_x_hot, shootcursor_y_hot);
    side->ui->toolcursors[unitmovetool] =
      make_cursor(dpy, win, movecursor_bits, movemask_bits,
		  fg, bg, movecursor_x_hot, movecursor_y_hot);
    side->ui->toolcursors[unitshoottool] =
      make_cursor(dpy, win, shootcursor_bits, shootmask_bits,
		  fg, bg, shootcursor_x_hot, shootcursor_y_hot);
    side->ui->toolcursors[unitbuildtool] =
      make_cursor(dpy, win, buildcursor_bits, buildmask_bits,
		  fg, bg, buildcursor_x_hot, buildcursor_y_hot);
#ifdef DESIGNERS
    side->ui->toolcursors[cellpainttool] =
      make_cursor(dpy, win, hexcursor_bits, hexcursormask_bits,
		  fg, bg, hexcursor_x_hot, hexcursor_y_hot);
    side->ui->toolcursors[bordpainttool] =
      make_cursor(dpy, win, bordcursor_bits, bordcursor_bits, /* should have separate mask */
		  fg, bg, bordcursor_x_hot, bordcursor_y_hot);
    side->ui->toolcursors[connpainttool] =
      make_cursor(dpy, win, conncursor_bits, conncursor_bits, /* should have separate mask */
		  fg, bg, conncursor_x_hot, conncursor_y_hot);
    side->ui->toolcursors[unitaddtool] =
      make_cursor(dpy, win, addunits_bits, addunits_bits,
		  fg, bg, addunits_x_hot, addunits_y_hot);
    side->ui->toolcursors[peoplepainttool] =
      make_cursor(dpy, win, people_bits, people_bits,
		  fg, bg, people_x_hot, people_y_hot);
    side->ui->toolcursors[featurepainttool] =
      make_cursor(dpy, win, feature_bits, featuremask_bits,
		  fg, bg, feature_x_hot, feature_y_hot);
#endif /* DESIGNERS */
    DGprintf("Cursors stored ...\n");
}

/* Since XCreatePixmapCursor() takes XColors and not pixel values we
   have to look up the colors associated with the foreground and
   background pixel values in the color table and pass them to
   XCreatePixmapCursor(). */
   
Cursor
make_cursor(dpy, win, cursbits, maskbits, fg, bg, x, y)
Display *dpy;
Window win;
char *cursbits, *maskbits;
unsigned long fg, bg;
unsigned int x, y;
{
    Pixmap curs, mask;
    XColor fgcolor, bgcolor;
    Cursor rslt;

    mask = XCreateBitmapFromData(dpy, win, maskbits, 16, 16);
    curs = XCreateBitmapFromData(dpy, win, cursbits, 16, 16);
    fgcolor.pixel = fg;
    XQueryColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &fgcolor);
    bgcolor.pixel = bg;
    XQueryColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &bgcolor);
    rslt = XCreatePixmapCursor(dpy, curs, mask, &fgcolor, &bgcolor, x, y);
    XFreePixmap(dpy, mask);
    XFreePixmap(dpy, curs);
    return rslt;
}

/* Open a font, possibly substituting another font if our desired one is
   missing for some reason. */

XFontStruct *
open_font(side, name, xdefault, altfont, alttype, fid)
Side *side;
char *name, *xdefault, *alttype;
XFontStruct *altfont;
Font *fid;
{
    XFontStruct *font;

    if (fid) {
	*fid = XLoadFont(side->ui->dpy, name);
	font = XQueryFont(side->ui->dpy, *fid);
    } else {
	font = XLoadQueryFont(side->ui->dpy, name);
    }
    if (font == NULL) {
	init_warning("Can't open font \"%s\" on \"%s\"",
		     name, side->player->displayname);
	if (altfont != NULL) {
	    init_warning("Substituting the %s font, hope it works!", alttype);
	    return altfont;
	} else {
	    return NULL;
	}
    }
    DGprintf("Opened font \"%s\" ...\n", name);
    return font;
}

/* Get a total bounding box on the font. */

int
font_width(font)
XFontStruct *font;
{
    return font->max_bounds.width;
}

int
font_height(font)
XFontStruct *font;
{
    return font->max_bounds.ascent + font->max_bounds.descent;
}

static void
x11_interp_fn(imf)
ImageFamily *imf;
{
    x11_interp_imf(tmp_display, tmp_root_window, imf, FALSE);
}

static void
x11_load_fn(imf)
ImageFamily *imf;
{
    x11_load_imf(tmp_display, tmp_root_window, imf);
}

static void
init_unit_images(side)
Side *side;
{
    int u, w, h;
    ImageFamily *imf;

    side->ui->uimages =
      (ImageFamily **) xmalloc(numutypes * sizeof(ImageFamily *));
    tmp_display = side->ui->dpy;
    tmp_root_window = side->ui->rootwin;
    for_all_unit_types(u) {
	imf = get_unit_type_images(side, u, x11_interp_fn, x11_load_fn, NULL);
	/* (should fabricate something as a substitute) */
	if (DebugG)
	  describe_imf(side, "unit type", u_type_name(u), imf);
	side->ui->uimages[u] = imf;
	if (smallest_image(imf, &w, &h)) {
	    if (w > min_w_for_unit_image)
	      min_w_for_unit_image = w;
	    if (h > min_h_for_unit_image)
	      min_h_for_unit_image = h;
	}
    }
}

/* Collect images for all the terrain types. */

static void
init_terrain_images(side)
Side *side;
{
    int t, p;
    ImageFamily *imf;
    Image *timg;
    X11Image *ximg;

    side->ui->timages =
      (ImageFamily **) xmalloc(numttypes * sizeof(ImageFamily *));
    tmp_display = side->ui->dpy;
    tmp_root_window = side->ui->rootwin;
    for_all_terrain_types(t) {
	imf = get_terrain_type_images(side, t, x11_interp_fn, x11_load_fn, NULL);
	if (imf != NULL && imf->numsizes == 0)
	  get_default_terrain_image(side, imf, t);
	if (DebugG)
	  describe_imf(side, "terrain type", t_type_name(t), imf);
	side->ui->timages[t] = imf;
	/* Precalculate which images to use at which magnifications. */
	for (p = 0; p < NUMPOWERS; ++p) {
	    if (1) {
		timg = best_image(side->ui->timages[t], hws[p], hhs[p]);
		if (timg != NULL) {
		    ximg = (X11Image *) timg->hook;
		    if (ximg != NULL) {
			if (!side->ui->monochrome
/* if somebody changes their mind about using the color images, this will lose */
 			    && side->ui->dflt_color_terr_images
 			    && ximg->colr != None)
			  side->ui->terrpics[p][t] = ximg->colr;
			else
			  side->ui->terrpics[p][t] = ximg->mono;
		    }
		}
	    }
	}
    }
}

static void
get_default_terrain_image(side, imf, t)
Side *side;
ImageFamily *imf;
int t;
{
    Image *img;
    X11Image *ximg;

    img = get_img(imf, 20, 22);
    img->minw = 1;  img->minh = 1;
    img->maxw = 999;  img->maxh = 999;
    ximg = get_x11_image(img);
    ximg->mono = 
      XCreateBitmapFromData(side->ui->dpy, side->ui->rootwin,
			    hex16b_bits, hex16b_width, hex16b_height);
}

/* Set up a side's view of everybody else's colors and emblems. */

static void
init_emblem_images(side)
Side *side;
{
    Side *side2;
    
    side->ui->eimages =
      (ImageFamily **) xmalloc((numsides + 1) * sizeof(ImageFamily *));
    /* Independent units may have a distinguishing emblem, so the
       indepside is included here. */
    for_all_sides_plus_indep(side2) {
	init_emblem(side, side2);
    }
}

/* Compute the distinctive emblem by which one side will recognize units
   of another side.  This does both our view of ourselves and others
   orthogonally.  Note that sides without displays can still have emblems
   and colors that the sides with displays will see, but that sides
   without displays don't need to do any emblem init. */

static void
init_emblem(side, side2)
Side *side, *side2;
{
    char cbuf[BUFSIZ], *s, *c;
    int s2 = side_number(side2), i;
    ImageFamily *imf;

    /* Collect the color names of the other side and try to request
       them for our own display. */
    if (!side->ui->monochrome && !empty_string(side2->colorscheme)) {
	/* take spec apart and iterate for multiple colors */
	for (s = side2->colorscheme, c = cbuf, i = 0; i < 3; ++s) {
	    if (*s == ',' || *s == '\0') {
		*c = '\0';
		c = cbuf;
		side->ui->colors[s2][i++] = request_color(side, cbuf);
	    } else {
		*c++ = *s;
	    }
	    if (*s == '\0')
	      break;
	}
	side->ui->numcolors[s2] = i;
    } else {
	side->ui->numcolors[s2] = 0;
    }
    tmp_display = side->ui->dpy;
    tmp_root_window = side->ui->rootwin;
    imf = get_emblem_images(side, side2, x11_interp_fn, x11_load_fn, NULL);
    if (imf != NULL
	&& imf->numsizes == 0
	&& !(imf->name != NULL && strcmp(imf->name, "none") == 0)) {
	get_default_emblem(side, imf, side2);
    }
    if (DebugG)
      describe_imf(side, "emblem", side_desig(side2), imf);
    side->ui->eimages[s2] = imf;
}

static void
get_default_emblem(side, imf, side2)
Side *side, *side2;
ImageFamily *imf;
{
    Image *img;
    X11Image *ximg;

    img = get_img(imf, 8, 8);
    img->minw = 8;  img->minh = 8;
    img->maxw = 128;  img->maxh = 128;
    ximg = get_x11_image(img);
    /* (should make something for this image) */
}

/* Output a general description of an image family. */

static void
describe_imf(side, classname, typename, imf)
Side *side;
char *classname, *typename;
ImageFamily *imf;
{
    Image *img;
    X11Image *ximg;

    if (imf == NULL) {
	DGprintf("No image family for %s %s for %s",
		 classname, typename, side_desig(side));
	return;
    }
    DGprintf("%s %s family for %s has %d images",
	     classname, typename, side_desig(side), imf->numsizes);
    if (imf->location)
      DGprintf(" and is in %s", imf->location->name);
    DGprintf("\n");
    for (img = imf->images; img != NULL; img = img->next) {
	DGprintf("    %dx%d", img->w, img->h);
	ximg = (X11Image *) img->hook;
	if (ximg)
	  DGprintf(" (x11 mono %d color %d mask %d)", ximg->mono, ximg->colr, ximg->mask);
	DGprintf("\n");
    }
}

void 
popup_game_dialog ()
{
    int i;
    Widget outform, label, quit, viewp, inform;
    Module *module;
    
    games = (Gamew *) xmalloc(numgames*sizeof(Gamew));
    
    gameShell =
      XtVaCreatePopupShell("gameDialog", topLevelShellWidgetClass, thistoplevel,
			   NULL);
    outform =
      XtVaCreateManagedWidget("outForm", formWidgetClass, gameShell,
			      NULL);
    label =
      XtVaCreateManagedWidget("label", labelWidgetClass, outform,
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainTop, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft,
			      NULL);
    quit =
      XtVaCreateManagedWidget("quit", commandWidgetClass, outform,
			      XtNfromHoriz, label,
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainTop, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft,
			      NULL);
    XtAddCallback (quit, XtNcallback, quit_dialog, NULL);

    viewp =
      XtVaCreateManagedWidget("viewport", viewportWidgetClass,  outform,
			      XtNfromVert,  label,
			      XtNallowVert, True,
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainRight,
			      NULL);
    inform =
      XtVaCreateManagedWidget("inForm", formWidgetClass, viewp,
			      NULL);
    
    for (i = 0; i < numgames; ++i) {
	module = possible_games[i];
	games[i].form =
	  XtVaCreateManagedWidget(module->name, formWidgetClass, inform, 
				  XtNfromVert, i ? games[i-1].form : NULL,
				  XtNtop,    XawChainTop, 
				  XtNbottom, XawChainTop, 
				  XtNleft,   XawChainLeft, 
				  XtNright,  XawChainLeft,
				  NULL);
	games[i].name =
	  XtVaCreateManagedWidget("name", commandWidgetClass, games[i].form,
				  XtNlabel, module->name,
				  XtNtop,    XawChainTop, 
				  XtNbottom, XawChainTop, 
				  XtNleft,   XawChainLeft, 
				  XtNright,  XawChainLeft,
				  NULL);
	XtAddCallback (games[i].name, XtNcallback, game_callback, NULL);
	if (module->title && module->title[0]) {
	    games[i].title =
	      XtVaCreateManagedWidget("title", labelWidgetClass, games[i].form,
				      XtNlabel, module->title,
				      XtNfromHoriz, games[i].name,
				      XtNtop,    XawChainTop, 
				      XtNbottom, XawChainTop, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	} else {
	    games[i].title = NULL;
	}
	if (module->blurb && module->blurb[0]) {
	    games[i].blurb =
	      XtVaCreateManagedWidget("blurb", labelWidgetClass, games[i].form,
				      XtNlabel, module->blurb,
				      XtNfromHoriz,
				      games[i].title ? games[i].title : games[i].name,
				      XtNtop,    XawChainTop, 
				      XtNbottom, XawChainTop, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	} else {
	    games[i].blurb = NULL;
	}
	games[i].pop = NULL;
    }
    
    XtPopup(gameShell, XtGrabNone);
    
    /* replacement for XtAppMainLoop(thisapp): */
    loop = 1;
    while (loop) {
	XEvent event;

	XtAppNextEvent(thisapp, &event);
	XtDispatchEvent(&event);
    }
}

static void
popup_game(g, flag)
int g, flag;
{
    char *vartypename = NULL;
    GamePop *pop = games[g].pop;
    Module *module = possible_games[g];
    Widget up, left, save;
    Variant *var;
    Varw *varw;
    int i;
    Side *side;
    Player *player;
    static char *player_type[] = { "none", "human", "machine" };
    Widget pane;

    if (!pop || flag) {
	if (!pop) {
	    games[g].pop = pop = (GamePop *) xmalloc(sizeof(GamePop));
	    pop->variants = NULL;
	}
	sprintf(buf1, "xconq game: %s", module->name);
	pop->shell =
	  XtVaCreatePopupShell(module->name, topLevelShellWidgetClass,
			       thistoplevel, 
			       XtNtitle, buf1,
			       NULL);
	pop->form =
	  XtVaCreateManagedWidget("gameForm", formWidgetClass, pop->shell,
				  NULL);
	up = left = pop->name =
	  XtVaCreateManagedWidget("name", labelWidgetClass, pop->form,
				  XtNlabel, module->name,
				  XtNtop,    XawChainTop, 
				  XtNbottom, XawChainTop, 
				  XtNleft,   XawChainLeft, 
				  XtNright,  XawChainLeft,
				  NULL);
	if (module->basemodulename && module->basemodulename[0]) {
	    sprintf(buf1, "base module: %s", module->basemodulename);
	} else {
	    strcpy(buf1, "no base module");
	}
	left = pop->basemodule =
	  XtVaCreateManagedWidget("basemodule", labelWidgetClass, pop->form,
				  XtNlabel, buf1,
				  XtNfromHoriz, left,
				  XtNtop,    XawChainTop, 
				  XtNbottom, XawChainTop, 
				  XtNleft,   XawChainLeft, 
				  XtNright,  XawChainLeft,
				  NULL);
	if (module->version && module->version[0]) {
	    sprintf(buf1, "version: %s", module->version);
	    left = pop->version =
	      XtVaCreateManagedWidget("version", labelWidgetClass, pop->form,
				      XtNlabel, module->version,
				      XtNfromHoriz, left,
				      XtNtop,    XawChainTop, 
				      XtNbottom, XawChainTop, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	}
	if (module->title && module->title[0]) {
	    up = pop->title =
	      XtVaCreateManagedWidget("title", labelWidgetClass, pop->form,
				      XtNlabel, module->title,
				      XtNfromVert, pop->name,
				      XtNtop,    XawChainTop, 
				      XtNbottom, XawChainTop, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	}
	if (module->blurb && module->blurb[0]) {
	    up = pop->blurb =
	      XtVaCreateManagedWidget("blurb", labelWidgetClass, pop->form,
				      XtNlabel, module->blurb,
				      XtNfromHoriz, pop->title,
				      XtNfromVert, pop->name,
				      XtNtop,    XawChainTop, 
				      XtNbottom, XawChainTop, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	}

	if (module->variants) {
	    for (i = 0; module->variants[i].id != lispnil; ++i) { }
	    pop->numvariants = i;
	} else {
	    pop->numvariants = 0;
	}
	if (flag && pop->numvariants) {
	    if (flag == 1) {
		pop->variants =
		  (Varw *) xmalloc(pop->numvariants*sizeof(Varw));
	    }
	    up =
	      XtVaCreateManagedWidget("variant", labelWidgetClass, pop->form,
				      XtNlabel, "Variants:",
				      XtNfromVert, up,
				      XtNtop,    XawChainTop, 
				      XtNbottom, XawChainTop, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);

	    for (i = 0; i < pop->numvariants; ++i) {
		var = &(module->variants[i]);
		varw = &(pop->variants[i]);
		vartypename = c_string(var->id);

		if (flag == 1)
		  varw->value = variant_default(var); 
		sprintf(buf1, "variant%d", i);
		up = varw->dialog =
		  XtVaCreateManagedWidget(buf1, dialogWidgetClass, pop->form,
					  XtNlabel, variant_name(var),
					  XtNvalue, varw->value,
					  XtNfromVert, up,
					  XtNtop,    XawChainTop, 
					  XtNbottom, XawChainTop, 
					  XtNleft,   XawChainLeft, 
					  XtNright,  XawChainLeft,
					  NULL);
		horizontalDialog(varw->dialog);
		if (varw->mess && varw->mess[0]) {
		    reverseVideoDialog(varw->dialog);
		    up =
		      XtVaCreateManagedWidget("varMessage", labelWidgetClass,
					      pop->form,
					      XtNlabel, varw->mess,
					      XtNfromVert, up,
					      XtNtop,    XawChainTop, 
					      XtNbottom, XawChainTop, 
					      XtNleft,   XawChainLeft, 
					      XtNright,  XawChainLeft,
					      NULL);
		}
	    }
	}

	if (module->instructions != lispnil ||
	    module->notes != lispnil) {
	    up = pane =
	      XtVaCreateManagedWidget("pane", panedWidgetClass, pop->form,
				      XtNfromVert, up,
				      XtNorientation, XtorientVertical,
				      XtNtop,    XawChainTop, 
				      XtNbottom, XawChainBottom, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainRight,
				      NULL);
	    pop->instructions =
	      createTextWidget("instructions", pane,
			       NULL, module->instructions);
	    pop->notes =
	      createTextWidget("notes", pane,
			       pop->instructions, module->notes);
	    
	}
	if (flag) {
	    up = XtVaCreateManagedWidget("assign", labelWidgetClass, pop->form,
					 XtNlabel, "Assignments:",
					 XtNfromVert, up,
					 XtNtop,    XawChainBottom, 
					 XtNbottom, XawChainBottom, 
					 XtNleft,   XawChainLeft, 
					 XtNright,  XawChainLeft,
					 NULL);
	    if (flag == 1) {
		pop->assign = (Assignw *) xmalloc(MAXSIDES*sizeof(Assignw));
	    }
	    for (i = 0; i < numsides; ++i) {
		side   = assignments[i].side;
		player = assignments[i].player;
		if (flag == 1) { 
		    init_assignw_player(&(pop->assign[i]), player);
		}
		if (side->sideclass && side->sideclass[0]) {
		    sprintf(buf1, "%s (%s)", short_side_title(side),
			    side->sideclass);
		} else {
		    strcpy(buf1, short_side_title(side));
		}
		save = left = pop->assign[i].sidew =
		  XtVaCreateManagedWidget("side", labelWidgetClass, pop->form,
					  XtNlabel, buf1,
					  XtNfromVert, up,
					  XtNtop,    XawChainBottom, 
					  XtNbottom, XawChainBottom, 
					  XtNleft,   XawChainLeft, 
					  XtNright,  XawChainLeft,
					  NULL);
		left = pop->assign[i].pname =
		  XtVaCreateManagedWidget("pname", dialogWidgetClass,
					  pop->form,
					  XtNlabel, "name",
					  XtNvalue, pop->assign[i].vname,
					  XtNfromHoriz, left,
					  XtNfromVert, up,
					  XtNtop,    XawChainBottom, 
					  XtNbottom, XawChainBottom, 
					  XtNleft,   XawChainLeft, 
					  XtNright,  XawChainLeft,
					  NULL);
		horizontalDialog(pop->assign[i].pname);
		up = pop->assign[i].pid =
		  XtVaCreateManagedWidget("pid", dialogWidgetClass, pop->form,
					  XtNlabel, player_type[pop->assign[i].vflag],
					  XtNvalue, pop->assign[i].vid,
					  XtNfromHoriz, left,
					  XtNfromVert, up,
					  XtNtop,    XawChainBottom, 
					  XtNbottom, XawChainBottom, 
					  XtNleft,   XawChainLeft, 
					  XtNright,  XawChainLeft,
					  NULL);
		horizontalDialog(pop->assign[i].pid);
		if (pop->assign[i].messname && pop->assign[i].messname[0]) {
		    reverseVideoDialog(pop->assign[i].pname);
		    up = XtVaCreateManagedWidget("nameMessage",
						 labelWidgetClass, pop->form,
						 XtNlabel, pop->assign[i].messname,
						 XtNfromHoriz, save,
						 XtNfromVert, up,
						 XtNtop,    XawChainBottom, 
						 XtNbottom, XawChainBottom, 
						 XtNleft,   XawChainLeft, 
						 XtNright,  XawChainLeft,
						 NULL);
		}
		if (pop->assign[i].messid && pop->assign[i].messid[0]) {
		    reverseVideoDialog(pop->assign[i].pid);
		    up = XtVaCreateManagedWidget("idMessage",
						 labelWidgetClass, pop->form,
						 XtNlabel, pop->assign[i].messid,
						 XtNfromHoriz, save,
						 XtNfromVert, up,
						 XtNtop,    XawChainBottom, 
						 XtNbottom, XawChainBottom, 
						 XtNleft,   XawChainLeft, 
						 XtNright,  XawChainLeft,
						 NULL);
		}
	    }
	    left = pop->add =
	      XtVaCreateManagedWidget("add", commandWidgetClass, pop->form,
				      XtNfromVert, up,
				      XtNtop,    XawChainBottom, 
				      XtNbottom, XawChainBottom, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	    XtAddCallback (pop->add, XtNcallback, add_player_dialog, NULL);
	    left = pop->verify =
	      XtVaCreateManagedWidget("verify", commandWidgetClass, pop->form,
				      XtNfromVert, up,
				      XtNfromHoriz, left,
				      XtNtop,    XawChainBottom, 
				      XtNbottom, XawChainBottom, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	    XtAddCallback (pop->verify, XtNcallback, verify_dialog, NULL);
	    left = pop->ok =
	      XtVaCreateManagedWidget("go", commandWidgetClass, pop->form,
				      XtNfromVert, up,
				      XtNfromHoriz, left,
				      XtNtop,    XawChainBottom, 
				      XtNbottom, XawChainBottom, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	    XtAddCallback (pop->ok, XtNcallback, go_dialog, NULL);
	    pop->cancel =
	      XtVaCreateManagedWidget("quit", commandWidgetClass, pop->form,
				      XtNfromVert, up,
				      XtNfromHoriz, left,
				      XtNtop,    XawChainBottom, 
				      XtNbottom, XawChainBottom, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	    XtAddCallback (pop->cancel, XtNcallback, quit_dialog, NULL);
	} else {
	    left = pop->ok =
	      XtVaCreateManagedWidget("ok", commandWidgetClass, pop->form,
				      XtNfromVert, up,
				      XtNtop,    XawChainBottom, 
				      XtNbottom, XawChainBottom, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	    XtAddCallback (pop->ok, XtNcallback, ok_dialog, NULL);
	    pop->cancel =
	      XtVaCreateManagedWidget("cancel", commandWidgetClass, pop->form,
				      XtNfromVert, up,
				      XtNfromHoriz, left,
				      XtNtop,    XawChainBottom, 
				      XtNbottom, XawChainBottom, 
				      XtNleft,   XawChainLeft, 
				      XtNright,  XawChainLeft,
				      NULL);
	    XtAddCallback (pop->cancel, XtNcallback, cancel_dialog, NULL);
	}
    }
    XtPopup (pop->shell, XtGrabNone);
    XMapRaised(XtDisplay(pop->shell), XtWindow(pop->shell));
}

static Widget
createTextWidget(name, parent, up, obj)
char *name;
Widget parent, up;
Obj *obj;
{
    Widget w, form;

    if (obj == lispnil)
      return NULL;

    form =
      XtVaCreateManagedWidget("form", formWidgetClass, parent,
			      NULL);

    sprintf(buf1, "%s_label", name);
    w =
      XtVaCreateManagedWidget(buf1, labelWidgetClass, form,
			      XtNlabel, name,
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainTop, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft,
			      NULL);
    w =
      XtVaCreateManagedWidget(name, asciiTextWidgetClass, form,
			      XtNfromVert, w,
			      XtNvertDistance, -1,
			      XtNdisplayCaret, False,
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainRight,
			      NULL);
    splat_obj_in_textw(w, obj);
    return w;
}

static void
splat_obj_in_textw(w, obj)
Widget w;
Obj *obj;
{
    Obj *rest;

    if (!obj || obj == lispnil)
      return;

    if (stringp(obj)) {
	textw_printf(w, "%s\n", c_string(obj));
	return;
    }

    if (!consp(obj))
      return;

    for (rest = obj; rest != lispnil; rest = cdr(rest)) {
	if (stringp(car(rest))) {
	    textw_printf(w, "%s\n", c_string(car(rest)));
	}
    }
}

static void
quit_dialog (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    exit(0);
}


static void
game_callback (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int i;

    for (i = 0; i < numgames; ++i) {
	if (w == games[i].name) {
	    popup_game(i, 0);
	    return;
	}
    }
}

static void
cancel_dialog (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int i;
    Widget form = XtParent(w);

    for (i = 0; i < numgames; ++i) {
	if (games[i].pop && form == games[i].pop->form) {
	    XtPopdown(games[i].pop->shell);
	}
    }
}

static void
ok_dialog (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int i;
    Widget form = XtParent(w);

    for (i = 0; i < numgames; ++i) {
	if (games[i].pop && form == games[i].pop->form) {
	    break;
	}
    }
    chosen_game = i;
  
    /* clean up */
    for (i = 0; i < numgames; ++i) {
	if (games[i].pop) {
	    XtPopdown(games[i].pop->shell);
	    XtDestroyWidget(games[i].pop->shell);
	    if (i != chosen_game) {
		if (games[i].pop->variants)
		  free(games[i].pop->variants);
		free(games[i].pop);
		games[i].pop = NULL;
	    }
	}
    }
    XtPopdown(gameShell);
    XtDestroyWidget(gameShell);

    /* initialize game and sides */
    mainmodule = possible_games[chosen_game];
    load_game_module(mainmodule, TRUE);
    check_game_validity();
    make_trial_assignments();

    popup_game(chosen_game, 1);
}

static void
go_dialog (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    GamePop *pop = games[chosen_game].pop;
    char *value;
    int i, flag;

    if (!valid_gamepop()) {
	verify_dialog(w, NULL, NULL);
	XBell(XtDisplay(pop->shell),50);
	return;
    }

    variants = lispnil;
    /* setup variants */
    for (i = 0; i < pop->numvariants; ++i) {
	value = XawDialogGetValueString(pop->variants[i].dialog);
	implement_variant(value, &(mainmodule->variants[i]));
	if (pop->variants[i].mess)
	  free(pop->variants[i].mess);
    }
    /* get side values */
    for (i = 0; i < numsides; ++i) {
	if (pop->assign[i].pname) {
	    value = XawDialogGetValueString(pop->assign[i].pname);
	    if (value && value[0]) {
		assignments[i].player->name = copy_string(value);
	    } else {
		assignments[i].player->name = NULL;
	    }
	    if (pop->assign[i].messname)
	      free(pop->assign[i].messname);
	}
	if (pop->assign[i].pid) {
	    value = XawDialogGetValueString(pop->assign[i].pid);
	    flag = type_player(value);
	    if (ABS(flag) == 1) {
		assignments[i].player->displayname = copy_string(value);
		assignments[i].player->aitypename  = NULL;
	    } else {
		assignments[i].player->displayname = NULL;
		assignments[i].player->aitypename  = copy_string(value);
	    }
	    if (pop->assign[i].messid)
	      free(pop->assign[i].messid);
	}
    }

    XtPopdown(pop->shell);
    XtDestroyWidget(pop->shell);
    if (pop->variants)
      free(pop->variants);
    if (pop->assign)
      free(pop->assign);
    free(pop);
    free(games);
    games = NULL;

    do_module_variants(mainmodule, variants);
    check_game_validity();

    loop = 0;
}

static int
valid_gamepop()
{
    GamePop *pop = games[chosen_game].pop;
    char *value;
    int i, res = 1;

    /* check variants */
    for (i = 0; i < pop->numvariants; ++i) {
	value = XawDialogGetValueString(pop->variants[i].dialog);
	res = res && valid_variant(value, &(mainmodule->variants[i]));
    }
    /* check sides */
    for (i = 0; i < numsides; ++i) {
	if (pop->assign[i].pid) {
	    value = XawDialogGetValueString(pop->assign[i].pid);
	    res = res && type_player(value)>0;
	}
    }

    return res;
}

static void
verify_dialog(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    GamePop *pop = games[chosen_game].pop;
    char *value;
    int i, flag;

    /* save variant values */
    for (i = 0; i < pop->numvariants; ++i) {
	value = XawDialogGetValueString(pop->variants[i].dialog);
	pop->variants[i].value = copy_string(value);
	if (valid_variant(value, &(mainmodule->variants[i]))) {
	    if (pop->variants[i].mess)  free(pop->variants[i].mess);
	    pop->variants[i].mess = NULL;
	} else {
	    pop->variants[i].mess = copy_string(errormess);
	}
    }
    /* save side values */
    for (i = 0; i < numsides; ++i) {
	if (pop->assign[i].pname) {
	    value = XawDialogGetValueString(pop->assign[i].pname);
	    pop->assign[i].vname = copy_string(value);
	    pop->assign[i].messname = 0;  /* names always OK? */
	}
	if (pop->assign[i].pid) {
	    value = XawDialogGetValueString(pop->assign[i].pid);
	    pop->assign[i].vid = copy_string(value);
	    flag = type_player(value);
	    if (flag > 0) {
		if (pop->assign[i].messid)
		  free(pop->assign[i].messid);
		pop->assign[i].messid = NULL;
	    } else {
		pop->assign[i].messid = copy_string(errormess);
	    }
	}
	pop->assign[i].vflag = ABS(flag);
    }

    XtPopdown(pop->shell);
    XtDestroyWidget(pop->shell);

    popup_game(chosen_game, 2);
}

static char *
variant_default(var)
Variant *var;
{
    int i1, i2, i3, i4, i5, n;
    char *p;

    switch (keyword_code(c_string(var->id))) {
      case K_WORLD_SEEN:
      case K_SEE_ALL:
      case K_SEQUENTIAL:
	if (var->dflt == lispnil)  return "true";
	return c_number(eval(var->dflt)) ? " true" : "false";
      case K_WORLD_SIZE:
	sprintlisp(buf3, var->dflt);
	for (p = buf3; *p; ++p) {
	    if (!isdigit(*p))
	      *p = ' ';
	}
	n = sscanf(buf3, "%d%d%d%d%d", &i1, &i2, &i3, &i4, &i5);
	sprintf(buf3, "(%d %d %d)",
		(n>0) ? i1 : DEFAULTWIDTH,
		(n>1) ? i2 : DEFAULTHEIGHT,
		(n>2) ? i3 : DEFAULTCIRCUMFERENCE);
	if (n == 4) {
	    sprintf(buf3, "(%d %d %d %d)", i1, i2, i3, i4);
	}
	if (n == 5) {
	    sprintf(buf3, "(%d %d %d %d %d)", i1, i2, i3, i4, i5);
	}
	return buf3;
      default:
	sprintlisp(buf3, var->dflt);
	return buf3;
    }
}

static char *
variant_name(var)
Variant *var;
{
    switch (keyword_code(c_string(var->id))) {
      case K_WORLD_SEEN:
	sprintf(buf2, "%s: make the world be seen already", var->name);
	return buf2;
      case K_SEE_ALL:
	sprintf(buf2, "%s: make everything be always seen", var->name);
	return buf2;
      case K_SEQUENTIAL:
	sprintf(buf2, "%s: move sequentially", var->name);
	return buf2;
      case K_WORLD_SIZE:
	sprintf(buf2, 
	"%s: set world size (width height circumference [longitude [latitude]])",
	    var->name);
	return buf2;
      default:
	return var->name;
    }
}

static void
horizontalDialog(w)
Widget w;
{
    Widget label, value;

    label = XtNameToWidget(w, "label");
    value = XtNameToWidget(w, "value");
    if (label && value) {
	XtVaSetValues(value,
		      XtNfromVert,  NULL,
		      XtNfromHoriz, label,
		      NULL);
    }
}

static void
reverseVideoDialog(w)
Widget w;
{
    Pixel fg, bg;
    Widget label = XtNameToWidget(w, "label");

    if (!label)
      return;
    XtVaGetValues(label, XtNbackground, &bg, XtNforeground, &fg, NULL);
    XtVaSetValues(w, XtNbackground, fg, NULL);
    XtVaSetValues(label, XtNbackground, fg, XtNforeground, bg, NULL);
}

static int
boolean_lisp(value)
char *value;
{
    char c, *p;

    if (!value || !value[0])
      return 0;

    for (p = value; *p; ++p) {
	if (isalnum(*p)){
	    c = tolower(value[0]);
	    return (c != '0' && c != 'n' && c != 'f');
	}
    }
    return FALSE;
}

static void
implement_variant(value, var)
char *value;
Variant *var;
{
    int key = keyword_code(c_string(var->id)), i1, i2, i3, i4, i5, n;
    char *p;

    switch (key) {
      case K_WORLD_SEEN:
      case K_SEE_ALL:
      case K_SEQUENTIAL:
	push_key_int_binding(&variants, key, boolean_lisp(value));
	break;
      case K_WORLD_SIZE:
	for (p=value; *p; p++) {
	    if (!isdigit(*p))  *p = ' ';
	}
	n = sscanf(value, "%d%d%d%d%d", &i1, &i2, &i3, &i4, &i5);
	if (n < 4)
	  i4 = 0;
	if (n < 5)
	  i5 = 0;
	push_key_cdr_binding(&variants, key,
			     cons(new_number(i1),
				  cons(new_number(i2),
				       cons(new_number(i3),
					    cons(new_number(i4),
						 cons(new_number(i5),
						      lispnil))))));
	break;

      default:
	/* what to do here? */
	break;
    }
}

static int
valid_variant(value, var)
char *value;
Variant *var;
{
    int key = keyword_code(c_string(var->id)), i1, i2, i3, i4, i5, n;
    char *p;

    errormess[0] = '\0';

    switch (key) {
      case K_WORLD_SEEN:
      case K_SEE_ALL:
      case K_SEQUENTIAL:
	return (value && value[0]);

      case K_WORLD_SIZE:
	for (p = value; *p; p++) {
	    if (!isdigit(*p))
	      *p = ' ';
	}
	n = sscanf(value, "%d%d%d%d%d", &i1, &i2, &i3, &i4, &i5);
	return (n > 2 && n < 6);

      default:
	/* what to do here? */
	return (value && value[0]);
    }
}

/* return codes:
   0 = none
   1 =   valid display
   2 =   valid machine player
  -1 = invalid display 
  -2 = invalid machine player */

static int
type_player(value)
char *value;
{
    errormess[0] = '\0';
    if (!value[0]) {
	strcpy(errormess, "no player specification");
	return 0;
    } else if (strchr(value,':') || !strcmp(value,"here")) {
	if (can_open_display(value)) {
	    return 1;
	} else {
	    sprintf(errormess, "cannot open display \"%s\"", value);
	    return -1;
	}
    } else {
	if (strcmp(value,"mplayer")) {
	    sprintf(errormess,
		    "invalid or unkonwn AI \"%s\", try \"mplayer\"", value);
	    return -2;
	} else {
	    return 2;
	}
    }
}

static void 
add_player_dialog(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int i, oldnumsides = numsides;
    GamePop *pop = games[chosen_game].pop;

    if (numsides >= g_sides_max()) {
	XBell(XtDisplay(pop->shell),50);
	return;
    }

    add_side_and_player();

    for (i = oldnumsides; i < numsides; ++i) {
	init_assignw_player(&(pop->assign[i]), assignments[i].player);
	pop->assign[i].messid =
	  copy_string("enter display name or \"mplayer\"");
    }

    verify_dialog(w, NULL, NULL);
}

static void
init_assignw_player(assignw, player)
Assignw *assignw;
Player *player;
{
    assignw->vname = (player->name ? player->name : "");
    if (player->displayname) {
	assignw->vid = player->displayname;
	assignw->vflag = 1;
    } else if (player->aitypename) {
	assignw->vid = player->aitypename;
	assignw->vflag = 2;
    } else {
	assignw->vid = "";
	assignw->vflag = 0;
    }
}

static int
can_open_display(dpyname)
char *dpyname;
{
    Display *dpy;

    if (dpyname == NULL)
      return FALSE;

    if (strcmp("here", dpyname) == 0)
      dpyname = getenv("DISPLAY");

    dpy = XOpenDisplay(dpyname);
    if (dpy) {
	XCloseDisplay(dpy);
	return TRUE;
    } else {
	return FALSE;
    }
}

/* Shut the side's display down. */

void
close_display(side)
Side *side;
{
    Display *dpy = side->ui->dpy;

    flush_output(side);
    XSync(dpy, True);
    /* Mark it as inactive. */
    side->ui->active = FALSE;
    /* Clean up after ourselves. */
    XFreeGC(dpy, side->ui->gc);
    XFreeGC(dpy, side->ui->textgc);
    XFreeGC(dpy, side->ui->ltextgc);
    XFreeGC(dpy, side->ui->terrgc);
    XFreeGC(dpy, side->ui->unitgc);
    XFreeGC(dpy, side->ui->emblgc);
    XtCloseDisplay(dpy);
}
