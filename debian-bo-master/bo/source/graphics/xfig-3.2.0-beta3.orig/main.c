/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Parts Copyright (c) 1991 by Paul King
 * Parts Copyright (c) 1994 by Brian V. Smith
 *
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge, a
 * full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 * nonexclusive right and license to deal in this software and
 * documentation files (the "Software"), including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software subject to the restriction stated
 * below, and to permit persons who receive copies from any such party to
 * do so, with the only requirement being that this copyright notice remain
 * intact.
 * This license includes without limitation a license to do the foregoing
 * actions under any patents of the party supplying this software to the 
 * X Consortium.
 *
 * Restriction: The GIF encoding routine "GIFencode" in f_wrgif.c may NOT
 * be included if xfig is to be sold, due to the patent held by Unisys Corp.
 * on the LZW compression algorithm.
 */

#include "fig.h"
#include "figx.h"
#include "version.h"
#include "patchlevel.h"
#include "resources.h"
#include "object.h"
#include "mode.h"
#include "u_fonts.h"
#include "w_color.h"
#include "w_drawprim.h"
#include "w_mousefun.h"
#include "w_setup.h"
#include "w_util.h"
#include "w_zoom.h"
#ifdef USE_XPM_ICON
#include <xpm.h>
#endif /* USE_XPM_ICON */

/* input extensions for an input tablet */
#ifdef USE_TAB
#include "X11/extensions/XInput.h"
#endif

/************** EXTERNAL functions **************/

extern void	quit(), undo(), paste(), redisplay_canvas(), delete_all_cmd();
extern void	popup_print_panel(), popup_file_panel(), popup_export_panel();
extern void	do_load(), do_save(), popup_unit_panel();
extern void	inc_zoom(), dec_zoom();

extern void	setup_cmd_panel();
extern		X_error_handler();
extern void	error_handler();
extern void	my_quit();
extern void	quit();
extern int	ignore_exp_cnt;
extern int	psfontnum();
extern int	latexfontnum();

#include "fig.icon.X"
Pixmap		fig_icon;

static char	tool_name[100];
static int	screen_res;

/************** FIG options ******************/

static char    *filename = NULL;

static Boolean	true = True;
static Boolean	false = False;
static int	Idefault = DEFAULT;
static int	Izero = 0;
static int	Ione = 1;
static int	Itwo = 2;
static float	Fzero = 0.0;
static float	Fone = 1.0;
static float	F100 = 100.0;

/* to get any visual the user specifies */

typedef struct
{
	Visual	*visual;
	int	depth;
} OptionsRec;

OptionsRec	Options;

XtResource resources[] =
{
	{"visual", "Visual", XtRVisual, sizeof (Visual *),
	XtOffsetOf (OptionsRec, visual), XtRImmediate, NULL},
	{"depth", "Depth", XtRInt, sizeof (int),
	XtOffsetOf (OptionsRec, depth), XtRImmediate, NULL},
};

/* actions so that we may install accelerators at the top level */
static XtActionsRec	main_actions[] =
{
    {"Quit", (XtActionProc) quit},
    {"Delete_all", (XtActionProc) delete_all_cmd},
    {"Undo", (XtActionProc) undo},
    {"Redraw", (XtActionProc) redisplay_canvas},
    {"Paste", (XtActionProc) paste},
    {"File", (XtActionProc) popup_file_panel},
      {"LoadFile", (XtActionProc) do_load},
      {"SaveFile", (XtActionProc) do_save},
    {"Export", (XtActionProc) popup_export_panel},
    {"Print", (XtActionProc) popup_print_panel},
    {"Units", (XtActionProc) popup_unit_panel},
};

static XtResource application_resources[] = {
    {"zoom", "Zoom", XtRFloat, sizeof(float),
      XtOffset(appresPtr, zoom), XtRFloat, (caddr_t) & Fone},
    {"canvasbackground",  "canvasBackground",    XtRString,  sizeof(char *),
      XtOffset(appresPtr,canvasBackground), XtRString, (caddr_t) NULL},
    {"canvasforeground",  "canvasForeground",    XtRString,  sizeof(char *),
      XtOffset(appresPtr,canvasForeground), XtRString, (caddr_t) NULL},
    {"iconGeometry",  "IconGeometry",    XtRString,  sizeof(char *),
      XtOffset(appresPtr,iconGeometry), XtRString, (caddr_t) NULL},
    {"showallbuttons", "ShowAllButtons",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, ShowAllButtons), XtRBoolean, (caddr_t) & false},
    {XtNjustify,   XtCJustify, XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, RHS_PANEL), XtRBoolean, (caddr_t) & false},
    {"landscape",   XtCOrientation, XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, landscape), XtRBoolean, (caddr_t) & true},
    {"debug", "Debug",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, DEBUG), XtRBoolean, (caddr_t) & false},
    {"pwidth",   XtCWidth, XtRFloat, sizeof(float),
      XtOffset(appresPtr, tmp_width), XtRFloat, (caddr_t) & Fzero},
    {"pheight",   XtCHeight, XtRFloat, sizeof(float),
      XtOffset(appresPtr, tmp_height), XtRFloat, (caddr_t) & Fzero},
    {XtNreverseVideo,   XtCReverseVideo, XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, INVERSE), XtRBoolean, (caddr_t) & false},
    {"trackCursor", "Track",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, TRACKING), XtRBoolean, (caddr_t) & true},
    {"inches", "Inches",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, INCHES), XtRBoolean, (caddr_t) & true},
    {"boldFont", "Font",   XtRString, sizeof(char *),
      XtOffset(appresPtr, boldFont), XtRString, (caddr_t) NULL},
    {"normalFont", "Font",   XtRString, sizeof(char *),
      XtOffset(appresPtr, normalFont), XtRString, (caddr_t) NULL},
    {"buttonFont", "Font",   XtRString, sizeof(char *),
      XtOffset(appresPtr, buttonFont), XtRString, (caddr_t) NULL},
    {"startlatexFont", "StartlatexFont",   XtRString, sizeof(char *),
      XtOffset(appresPtr, startlatexFont), XtRString, (caddr_t) NULL},
    {"startpsFont", "StartpsFont",   XtRString, sizeof(char *),
      XtOffset(appresPtr, startpsFont), XtRString, (caddr_t) NULL},
    {"startfontsize", "StartFontSize",   XtRFloat, sizeof(float),
      XtOffset(appresPtr, startfontsize), XtRFloat, (caddr_t) & Fzero},
    {"internalborderwidth", "InternalBorderWidth",   XtRInt, sizeof(int),
      XtOffset(appresPtr, internalborderwidth), XtRInt, (caddr_t) & Izero},
    {"starttextstep", "StartTextStep",   XtRFloat, sizeof(float),
      XtOffset(appresPtr, starttextstep), XtRFloat, (caddr_t) & Fzero},
    {"startfillstyle", "StartFillStyle",   XtRInt, sizeof(int),
      XtOffset(appresPtr, startfillstyle), XtRInt, (caddr_t) & Idefault},
    {"startlinewidth", "StartLineWidth",   XtRInt, sizeof(int),
      XtOffset(appresPtr, startlinewidth), XtRInt, (caddr_t) & Ione},
    {"startgridmode", "StartGridMode",   XtRInt, sizeof(int),
      XtOffset(appresPtr, startgridmode), XtRInt, (caddr_t) & Izero},
    {"startposnmode", "StartPosnMode",   XtRInt, sizeof(int),
      XtOffset(appresPtr, startposnmode), XtRInt, (caddr_t) & Ione},
    {"latexfonts", "Latexfonts",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, latexfonts), XtRBoolean, (caddr_t) & false},
    {"specialtext", "SpecialText",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, specialtext), XtRBoolean, (caddr_t) & false},
    {"scalablefonts", "ScalableFonts",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, SCALABLEFONTS), XtRBoolean, (caddr_t) & true},
    {"monochrome", "Monochrome",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, monochrome), XtRBoolean, (caddr_t) & false},
    {"latexfonts", "Latexfonts",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, latexfonts), XtRBoolean, (caddr_t) & false},
    {"keyFile", "KeyFile",   XtRString, sizeof(char *),
      XtOffset(appresPtr, keyFile), XtRString, (caddr_t) "CompKeyDB"},
    {"exportLanguage", "ExportLanguage",   XtRString, sizeof(char *),
      XtOffset(appresPtr, exportLanguage), XtRString, (caddr_t) "eps"},
    {"flushleft", "FlushLeft",   XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, flushleft), XtRBoolean, (caddr_t) & false},
    {"userscale", "UserScale",   XtRFloat, sizeof(float),
      XtOffset(appresPtr, user_scale), XtRFloat, (caddr_t) & Fone},
    {"userunit", "UserUnit",   XtRString, sizeof(char *),
      XtOffset(appresPtr, user_unit), XtRString, (caddr_t) ""},
    {"but_per_row", "But_per_row",   XtRInt, sizeof(int),
      XtOffset(appresPtr, but_per_row), XtRInt, (caddr_t) & Itwo},
    {"max_image_colors", "Max_image_colors", XtRInt, sizeof(int),
      XtOffset(appresPtr, max_image_colors), XtRInt, (caddr_t) & Izero},
    {"dont_switch_cmap", "Dont_switch_cmap", XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, dont_switch_cmap), XtRBoolean, (caddr_t) & false},
    {"tablet", "Tablet", XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, tablet), XtRBoolean, (caddr_t) & false},
    {"rulerthick", "RulerThick", XtRInt, sizeof(int),
      XtOffset(appresPtr, rulerthick), XtRInt, (caddr_t) & Izero},
    {"image_editor", "ImageEditor", XtRString, sizeof(char *),
      XtOffset(appresPtr, image_editor), XtRString, (caddr_t) NULL},
    {"magnification", "Magnification", XtRFloat, sizeof(float),
      XtOffset(appresPtr, magnification), XtRFloat, (caddr_t) & F100},
    {"paper_size", "Papersize", XtRString, sizeof(char *),
      XtOffset(appresPtr, paper_size), XtRString, (caddr_t) NULL},
    {"multiple",   XtCOrientation, XtRBoolean, sizeof(Boolean),
      XtOffset(appresPtr, multiple), XtRBoolean, (caddr_t) & false},
};

/* BE SURE TO UPDATE THE -help COMMAND OPTION LIST IF ANY CHANGES ARE MADE HERE */

static XrmOptionDescRec options[] =
{
    {"-visual", "*visual", XrmoptionSepArg, NULL},
    {"-depth", "*depth", XrmoptionSepArg, NULL},

    {"-multiple", ".multiple", XrmoptionNoArg, "True"},
    {"-single", ".multiple", XrmoptionNoArg, "False"},
    {"-magnification", ".magnification", XrmoptionSepArg, 0},
    {"-paper_size", ".paper_size", XrmoptionSepArg, (caddr_t) NULL},
    {"-zoom", ".zoom", XrmoptionSepArg, 0},
    {"-cbg", ".canvasBackground", XrmoptionSepArg, (caddr_t) NULL},
    {"-cfg", ".canvasForeground", XrmoptionSepArg, (caddr_t) NULL},
    {"-iconGeometry", ".iconGeometry", XrmoptionSepArg, (caddr_t) NULL},
    {"-showallbuttons", ".showallbuttons", XrmoptionNoArg, "True"},
    {"-right", ".justify", XrmoptionNoArg, "True"},
    {"-left", ".justify", XrmoptionNoArg, "False"},
    {"-debug", ".debug", XrmoptionNoArg, "True"},
    {"-landscape", ".landscape", XrmoptionNoArg, "True"},
    {"-Landscape", ".landscape", XrmoptionNoArg, "True"},
    {"-portrait", ".landscape", XrmoptionNoArg, "False"},
    {"-Portrait", ".landscape", XrmoptionNoArg, "False"},
    {"-pwidth", ".pwidth", XrmoptionSepArg, 0},
    {"-pheight", ".pheight", XrmoptionSepArg, 0},
    {"-inverse", ".reverseVideo", XrmoptionNoArg, "True"},
    {"-notrack", ".trackCursor", XrmoptionNoArg, "False"},
    {"-track", ".trackCursor", XrmoptionNoArg, "True"},
    {"-inches", ".inches", XrmoptionNoArg, "True"},
    {"-imperial", ".inches", XrmoptionNoArg, "True"},
    {"-centimeters", ".inches", XrmoptionNoArg, "False"},
    {"-metric", ".inches", XrmoptionNoArg, "False"},
    {"-boldFont", ".boldFont", XrmoptionSepArg, 0},
    {"-normalFont", ".normalFont", XrmoptionSepArg, 0},
    {"-buttonFont", ".buttonFont", XrmoptionSepArg, 0},
    {"-startpsFont", ".startpsFont", XrmoptionSepArg, 0},
    {"-startlatexFont", ".startlatexFont", XrmoptionSepArg, 0},
    {"-startFontSize", ".startfontsize", XrmoptionSepArg, 0},
    {"-startfontsize", ".startfontsize", XrmoptionSepArg, 0},
    {"-latexfonts", ".latexfonts", XrmoptionNoArg, "True"},
    {"-specialtext", ".specialtext", XrmoptionNoArg, "True"},
    {"-scalablefonts", ".scalablefonts", XrmoptionNoArg, "True"},
    {"-noscalablefonts", ".scalablefonts", XrmoptionNoArg, "False"},
    {"-monochrome", ".monochrome", XrmoptionNoArg, "True"},
    {"-internalBW", ".internalborderwidth", XrmoptionSepArg, 0},
    {"-internalBorderWidth", ".internalborderwidth", XrmoptionSepArg, 0},
    {"-keyFile", ".keyFile", XrmoptionSepArg, 0},
    {"-exportLanguage", ".exportLanguage", XrmoptionSepArg, 0},
    {"-flushleft", ".flushleft", XrmoptionNoArg, "True"},
    {"-center", ".flushleft", XrmoptionNoArg, "False"},
    {"-userscale", ".userscale", XrmoptionSepArg, 0},
    {"-userunit", ".userunit", XrmoptionSepArg, 0},
    {"-but_per_row", ".but_per_row", XrmoptionSepArg, 0},
    {"-starttextstep", ".starttextstep",  XrmoptionSepArg, 0},
    {"-startfillstyle", ".startfillstyle", XrmoptionSepArg, 0},
    {"-startlinewidth", ".startlinewidth", XrmoptionSepArg, 0},
    {"-startgridmode", ".startgridmode",  XrmoptionSepArg, 0},
    {"-startposnmode", ".startposnmode",  XrmoptionSepArg, 0},
    {"-max_image_colors", ".max_image_colors", XrmoptionSepArg, 0},
    {"-dontswitchcmap", ".dont_switch_cmap", XrmoptionNoArg, "True"},
    {"-tablet", ".tablet", XrmoptionNoArg, "True"},
    {"-rulerthick", ".rulerthick", XrmoptionSepArg, 0},
    {"-image_editor", ".image_editor", XrmoptionSepArg, 0},
};

char *help_list =
    "Usage:\n\
xfig [-help] \
[-boldFont <font>] \
[-but_per_row <number>] \
[-buttonFont <font>] \
\n     \
[-center] \
[-centimeters] \
[-flushleft] \
[-debug] \
[-dontswitchcmap] \
\n     \
[-exportLanguage <language>] \
[-iconGeometry <geom>] \
[-image_editor <editor>] \
\n     \
[-imperial] \
[-inches] \
[-internalBW <width>] \
[-internalBorderWidth <width>] \
\n     \
[-inverse] \
[-keyFile <file>] \
[-landscape] \
[-latexfonts] \
[-left] \
\n     \
[-magnification <print/export_mag>] \
[-max_image_colors <number>] \
\n     \
[-metric] \
[-monochrome] \
[-multiple] \
[-normalFont <font>] \
\n     \
[-noscalablefonts] \
[-notrack] \
[-paper_size <size>] \
[-pheight <height>] \
[-portrait] \
\n     \
[-pwidth <width>] \
[-right] \
[-rulerwidth <width>] \
[-scalablefonts] \
\n     \
[-showallbuttons] \
[-single] \
[-specialtext]\
[-startfillstyle <style>] \
\n     \
[-startfontsize <size>] \
[-startgridmode <number>] \
[-startlatexFont <font>] \
\n     \
[-startlinewidth <width>] \
[-startposnmode <number>] \
[-startpsFont <font>] \
\n     \
[-starttextstep <number>] \
[-tablet (if installed)] \
[-track] \
\n     \
[-userscale <scale>] \
[-userunit <units>] \
[-visual <visual>] \
[file]\n";

Atom wm_protocols[2];

static void	check_for_resize();
static void	check_colors();
XtActionsRec	form_actions[] =
{
    {"ResizeForm", (XtActionProc) check_for_resize},
    {"Quit", (XtActionProc) my_quit},
};

extern void clear_text_key();
extern void paste_panel_key();
static XtActionsRec text_panel_actions[] =
{
    {"PastePanelKey", (XtActionProc) paste_panel_key} ,
    {"EmptyTextKey", (XtActionProc) clear_text_key} ,
};

static String	form_translations =
			"<ConfigureNotify>:ResizeForm()\n";
static String	tool_translations =
			"<Message>WM_PROTOCOLS:Quit()\n";

#define NCHILDREN	9
static Widget	form;

main(argc, argv)
    int		    argc;
    char	   *argv[];

{
    Widget	    children[NCHILDREN];
    int		    ichild;
    int		    init_canv_wd, init_canv_ht;
    XWMHints	   *wmhints;
    int		    i;
    char	   *userhome;
    Dimension	    w, h;
    XGCValues	    gcv;
    Colormap	    colormap;		/* created colormap */
    XVisualInfo	    vinfo;		/* template for find visual */
    XVisualInfo	   *vinfo_list;		/* returned list of visuals */
    int		    count;		/* number of matchs (only 1?) */
    int		    cnt;		/* for the Xt args */
    Arg		    args[10];
    int		    xargc;		/* keeps copies of the command-line arguments */
    char	  **xargv;
    XColor	    dumcolor;

    DeclareArgs(5);

    /* version number only */
    if (argc > 1 && strcasecmp(argv[1],"-v")==0) {
	(void) fprintf(stderr, " XFIG %s patchlevel %s (Protocol %s)\n",
		   FIG_VERSION, PATCHLEVEL, PROTOCOL_VERSION);
	exit(0);

    /* help message only */
    } else if (argc > 1 && strcmp(argv[1],"-help")==0) {
	fprintf(stderr,"%s",help_list);
	exit(0);
    }

/* start of visual check/set */

	/*
	 * save the command line arguments
	 */

	xargc = argc;
	xargv = (char **) XtMalloc (argc * sizeof (char *));
	bcopy ((char *) argv, (char *) xargv, argc * sizeof (char *));

	/*
	 * The following creates a _dummy_ toplevel widget so we can
	 * retrieve the appropriate visual resource.
	 */
	tool = XtAppInitialize (&tool_app, "Fig", options, XtNumber (options), &argc, argv,
			       (String *) NULL, args, 0);
	/* save important info */
	tool_d = XtDisplay(tool);
	tool_s = XtScreen(tool);
	tool_sn = DefaultScreen(tool_d);

	XtGetApplicationResources (tool, &Options, resources,
				   XtNumber (resources),
				   args, 0);
	cnt = 0;
	if (Options.visual && Options.visual != DefaultVisualOfScreen (tool_s)) {
		XtSetArg (args[cnt], XtNvisual, Options.visual); ++cnt;
		/*
		 * Now we create an appropriate colormap.  We could
		 * use a default colormap based on the class of the
		 * visual; we could examine some property on the
		 * rootwindow to find the right colormap; we could
		 * do all sorts of things...
		 */
		tool_cm = XCreateColormap (tool_d,
					    RootWindowOfScreen (tool_s),
					    Options.visual,
					    AllocNone);
		XtSetArg (args[cnt], XtNcolormap, tool_cm); ++cnt;

		/*
		 * Now find some information about the visual.
		 */
		vinfo.visualid = XVisualIDFromVisual (Options.visual);
		vinfo_list = XGetVisualInfo (tool_d, VisualIDMask, &vinfo, &count);
		if (vinfo_list && count > 0) {
			XtSetArg (args[cnt], XtNdepth, vinfo_list[0].depth);
			++cnt;
			XFree ((XPointer) vinfo_list);
			/* save the depth of the visual */
			tool_dpth = vinfo_list[0].depth;
		}
		/* save the visual */
		tool_v = Options.visual;
	} else {
		/* no visual specified by the user, use default */
		tool_v = DefaultVisual(tool_d,tool_sn);
		/* same for colormap */
		tool_cm = DefaultColormapOfScreen(tool_s);
		tool_dpth = DefaultDepthOfScreen(tool_s);
	}
	/* and save the class */
	tool_vclass = tool_v->class;

	XtDestroyWidget (tool);

	/*
	 * Now create the real toplevel widget.
	 */
	XtSetArg (args[cnt], XtNargv, xargv); ++cnt;
	XtSetArg (args[cnt], XtNargc, xargc); ++cnt;
	tool = XtAppCreateShell ((String) "xfig", (String) "Fig",
				applicationShellWidgetClass,
				tool_d, args, cnt);
/* end of visual check/set */

    /* we are not writing the figure to the bitmap */
    writing_bitmap = False;

    /* get the current directory so we can go back here on abort */
    get_directory(orig_dir);

    /* get the TMPDIR environment variable for temporary files */
    if ((TMPDIR = getenv("XFIGTMPDIR"))==NULL)
	TMPDIR = "/tmp";

    (void) sprintf(tool_name, " XFIG %s patchlevel %s (Protocol %s)",
		   FIG_VERSION, PATCHLEVEL, PROTOCOL_VERSION);
    (void) strcat(file_header, PROTOCOL_VERSION);

    /* install actions to get to the functions with accelerators */
    XtAppAddActions(tool_app, main_actions, XtNumber(main_actions));

    fix_converters();

    /* get the application resources */
    XtGetApplicationResources(tool, &appres, application_resources,
			      XtNumber(application_resources), NULL, 0);

    /* All option args have now been deleted, leaving other args. (from Gordon Ross) */
    if (argc > 1) {
	    filename = argv[1];
    }

    tool_cells = CellsOfScreen(tool_s);
    screen_res = (int) ((float) WidthOfScreen(tool_s) /
			((appres.INCHES) ?
			    ((float) WidthMMOfScreen(tool_s)/2.54) :
				     WidthMMOfScreen(tool_s) ));

    /* set zoom factor for resolution chosen */
    if (appres.zoom <= 0.0)
	appres.zoom = 1.0;		/* user didn't specify starting zoom, use 1.0 */

    /* set print/export magnification to user selection (if any) */
    if (appres.magnification <= 0.0)
	appres.magnification = 100.0;	/* user didn't specify or chose bad value */

    ZOOM_FACTOR = PIX_PER_INCH/DISPLAY_PIX_PER_INCH;

    display_zoomscale = appres.zoom;
    zoomscale=display_zoomscale/ZOOM_FACTOR;

    /* parse any paper size the user wants */
    if (appres.paper_size) {
	appres.papersize = parse_papersize(appres.paper_size);
    } else {
	/* default paper size; letter for imperial and A4 for Metric */
	appres.papersize = (appres.INCHES? PAPER_LETTER: PAPER_A4);
    }
   
    /* filled in later */
    tool_w = (Window) NULL;

    if (appres.iconGeometry != (char *) 0) {
        int scr, x, y, junk;

        for(scr = 0;
            tool_s != ScreenOfDisplay(tool_d, scr);
            scr++);

        XGeometry(tool_d, scr, appres.iconGeometry,
                  "", 0, 0, 0, 0, 0, &x, &y, &junk, &junk);
        FirstArg(XtNiconX, x);
        NextArg(XtNiconY, y);
        SetValues(tool);
    }

    /* setup the defaults or the user preferences */

    /* if any of these classes, allow total number of cmap entries for image colors */
    switch( tool_vclass ){
	case GrayScale:
	case PseudoColor:
	    /* if the user hasn't specified a limit to the number of colors for images */
	    if (appres.max_image_colors <= 0)
		appres.max_image_colors = DEF_MAX_IMAGE_COLS;
	    break;
	case StaticGray:
	case StaticColor:	/* number of colors = number of map_entries */
	    appres.max_image_colors = tool_v->map_entries;
	    break;
	case DirectColor:
	case TrueColor:		/* set number of colors at max */
	    appres.max_image_colors = MAX_COLORMAP_SIZE;
	    break;
	default:
	    break;
    }

    if (appres.startfontsize >= 1.0)
	cur_fontsize = round(appres.startfontsize);

    /* allow "Modern" for "Sans Serif" and allow "SansSerif" (no space) */
    if (appres.startlatexFont) {
        if (strcmp(appres.startlatexFont,"Modern")==0 ||
	    strcmp(appres.startlatexFont,"SansSerif")==0)
	      cur_latex_font = latexfontnum ("Sans Serif");
    } else {
	    cur_latex_font = latexfontnum (appres.startlatexFont);
    }

    /* allocate black and white in case we aren't using the default colormap */
    /* (in which case we could have just used BlackPixelOfScreen...) */

    XAllocNamedColor(tool_d, tool_cm, (String) "white", &dumcolor, &dumcolor);
    white_color = dumcolor;
    XAllocNamedColor(tool_d, tool_cm, (String) "black", &dumcolor, &dumcolor);
    black_color = dumcolor;

    cur_ps_font = psfontnum (appres.startpsFont);

    if (appres.starttextstep > 0.0)
	cur_textstep = appres.starttextstep;

    if (appres.startfillstyle >= 0)
	cur_fillstyle = min2(appres.startfillstyle,NUMFILLPATS-1);

    if (appres.startlinewidth >= 0)
	cur_linewidth = min2(appres.startlinewidth,MAXLINEWIDTH);

    if (appres.startgridmode >= 0)
	cur_gridmode = min2(appres.startgridmode,GRID_3);

    if (appres.startposnmode >= 0)
	cur_pointposn = min2(appres.startposnmode,P_GRID3);

    /* turn off PSFONT_TEXT flag if user specified -latexfonts */
    if (appres.latexfonts)
	cur_textflags = cur_textflags & (~PSFONT_TEXT);
    if (appres.specialtext)
	cur_textflags = cur_textflags | SPECIAL_TEXT;

    /* turn off PSFONT_TEXT flag if user specified -latexfonts */
    if (appres.latexfonts)
	cur_textflags = cur_textflags & (~PSFONT_TEXT);

    if (appres.user_unit)
	strncpy(cur_fig_units, appres.user_unit, sizeof(cur_fig_units)-1);
    else
	cur_fig_units[0] = '\0';

    /* assume color to start */
    all_colors_available = True;

    /* check if monochrome screen */
    if (tool_cells == 2 || appres.monochrome) {
	all_colors_available = False;
	if (appres.INVERSE) {
	    XrmValue	value;
	    XrmDatabase	newdb = (XrmDatabase) 0, old;

	    value.size = sizeof("White");
	    value.addr = "White";
	    XrmPutResource(&newdb, "xfig*borderColor", "String", &value);
	    value.size = sizeof("White");
	    value.addr = "White";
	    XrmPutResource(&newdb, "xfig*foreground", "String", &value);
	    value.size = sizeof("Black");
	    value.addr = "Black";
	    XrmPutResource(&newdb, "xfig*background", "String", &value);
	    old = XtDatabase(tool_d);
	    XrmMergeDatabases(newdb, &old);

	    /* now set the tool part, since it's already created */
	    FirstArg(XtNborderColor, white_color.pixel);
	    NextArg(XtNforeground, white_color.pixel);
	    NextArg(XtNbackground, black_color.pixel);
	    SetValues(tool);
	}
    }

    init_font();

    gc = DefaultGC(tool_d, tool_sn);
    /* set the roman font for the message window */
    XSetFont(tool_d, gc, roman_font->fid);

    /* make a gc for the command buttons */
    gcv.font = button_font->fid;
    button_gc = XCreateGC(tool_d, DefaultRootWindow(tool_d), GCFont, &gcv);
    /* copy the other components from the default gc to the button_gc */
    XCopyGC(tool_d, gc, ~GCFont, button_gc);

    /*
     * check if the NUM_STD_COLS drawing colors could be allocated and have
     * different palette entries
     */
    check_colors();

    /* make the top-level widget */
    FirstArg(XtNinput, (XtArgVal) True);
    NextArg(XtNdefaultDistance, (XtArgVal) 0);
    NextArg(XtNresizable, (XtArgVal) False);
    /* if we have already switched colormaps in check_colors() */
    if (tool_cm)
	NextArg(XtNcolormap, tool_cm);
    form = XtCreateManagedWidget("form", formWidgetClass, tool,
				 Args, ArgCount);

    if (INTERNAL_BW == 0)
	INTERNAL_BW = appres.internalborderwidth;
    if (INTERNAL_BW <= 0)
	INTERNAL_BW = DEF_INTERNAL_BW;

    /* get the desired number of buttons per row for the mode panel */
    SW_PER_ROW = appres.but_per_row;
    if (SW_PER_ROW <= 0)
	SW_PER_ROW = DEF_SW_PER_ROW;
    else if (SW_PER_ROW > 6)
	SW_PER_ROW = 6;

    init_canv_wd = appres.tmp_width *
	(appres.INCHES ? PIX_PER_INCH : PIX_PER_CM)/ZOOM_FACTOR;
    init_canv_ht = appres.tmp_height *
	(appres.INCHES ? PIX_PER_INCH : PIX_PER_CM)/ZOOM_FACTOR;

    RULER_WD = appres.rulerthick;
    if (RULER_WD < DEF_RULER_WD)
	RULER_WD = DEF_RULER_WD;

    CANVAS_WD_LAND = DEF_CANVAS_WD_LAND;
    CANVAS_HT_LAND = DEF_CANVAS_HT_LAND;
    CANVAS_WD_PORT = DEF_CANVAS_WD_PORT;
    CANVAS_HT_PORT = DEF_CANVAS_HT_PORT;

    if (appres.landscape) {
	CANVAS_WD_LAND = init_canv_wd;
	CANVAS_HT_LAND = init_canv_ht;
    } else {
	CANVAS_WD_PORT = init_canv_wd;
	CANVAS_HT_PORT = init_canv_ht;
    }
    if (init_canv_wd == 0) {
	if (appres.landscape) {
	    init_canv_wd = CANVAS_WD_LAND = DEF_CANVAS_WD_LAND;
	} else {
	    init_canv_wd = CANVAS_WD_PORT = DEF_CANVAS_WD_PORT;
	}
    }
    if (init_canv_ht == 0) {
	if (appres.landscape) {
	    init_canv_ht = CANVAS_HT_LAND = DEF_CANVAS_HT_LAND;
	} else {
	    init_canv_ht = CANVAS_HT_PORT = DEF_CANVAS_HT_PORT;
	}
    }
	

    setup_sizes(init_canv_wd, init_canv_ht);
    (void) init_cmd_panel(form);
    (void) init_msg(form,filename);
    (void) init_mousefun(form);
    (void) init_mode_panel(form);
    (void) init_topruler(form);
    (void) init_canvas(form);

    /* parse any canvas background or foreground color the user wants */
    /* we had to wait until the canvas was created to get any color the
       user set through resources */
    if (appres.canvasBackground) {
	XParseColor(tool_d, tool_cm, appres.canvasBackground, &x_bg_color);
	if (XAllocColor(tool_d, tool_cm, &x_bg_color)==0) {
	    fprintf(stderr,"Can't allocate background color for canvas\n");
	    appres.canvasBackground = (char*) NULL;
	}
    } else {
	Pixel bg;
	FirstArg(XtNbackground, &bg);
	GetValues(canvas_sw);
	x_bg_color.pixel = bg;
	/* get the rgb values for it */
	XQueryColor(tool_d, tool_cm, &x_bg_color);
    }
    if (appres.canvasForeground) {
	XParseColor(tool_d, tool_cm, appres.canvasForeground, &x_fg_color);
	if (XAllocColor(tool_d, tool_cm, &x_fg_color)==0) {
	    fprintf(stderr,"Can't allocate background color for canvas\n");
	    appres.canvasForeground = (char*) NULL;
	}
    } else {
	Pixel fg;
	FirstArg(XtNforeground, &fg);
	GetValues(canvas_sw);
	x_fg_color.pixel = fg;
	/* get the rgb values for it */
	XQueryColor(tool_d, tool_cm, &x_fg_color);
    }
    /* now set the canvas to the user's choice, if any */
    FirstArg(XtNbackground, x_bg_color.pixel);
    NextArg(XtNforeground, x_fg_color.pixel);
    SetValues(canvas_sw);

    /* now fix the global GC */
    XSetState(tool_d, gc, x_fg_color.pixel, x_bg_color.pixel, GXcopy,
	      AllPlanes);

    /* setup the cursors */
    init_cursor();

    /* and recolor the cursors */
    recolor_cursors();

    (void) init_fontmenu(form); /* printer font menu */
    (void) init_unitbox(form);
    (void) init_sideruler(form);
    (void) init_ind_panel(form);

    ichild = 0;
    children[ichild++] = cmd_panel;	/* command buttons */
    children[ichild++] = mousefun;	/* labels for mouse fns */
    children[ichild++] = msg_form;	/* message window form */
    children[ichild++] = mode_panel;	/* current mode */
    children[ichild++] = topruler_sw;	/* top ruler */
    children[ichild++] = unitbox_sw;	/* box containing units */
    children[ichild++] = sideruler_sw;	/* side ruler */
    children[ichild++] = canvas_sw;	/* main drawing canvas */
    children[ichild++] = ind_panel;	/* current settings indicators */

    /*
     * until the following XtRealizeWidget() is called, there are NO windows
     * in existence
     */

    XtManageChildren(children, NCHILDREN);
    XtRealizeWidget(tool);
    tool_w = XtWindow(tool);

    /* make sure we have the most current colormap */
    set_cmap(tool_w);

    /* get this one for other sub windows too */
    wm_delete_window = XInternAtom(XtDisplay(tool), "WM_DELETE_WINDOW", False);

    /* for the main window trap delete window and save_yourself (my_quit) is called */
    wm_protocols[0] = wm_delete_window;
    /* remove WM_SAVE_YOURSELF until I do the "right" thing with it */
#ifdef WHEN_SAVE_YOURSELF_IS_FIXED
    wm_protocols[1] = XInternAtom(XtDisplay(tool), "WM_SAVE_YOURSELF", False);
    (void) XSetWMProtocols(XtDisplay(tool), tool_w, wm_protocols, 2);
#else
    (void) XSetWMProtocols(XtDisplay(tool), tool_w, wm_protocols, 1);
#endif

    /* use the XPM color icon for color display */
#ifdef USE_XPM_ICON
    if (all_colors_available) {
	Pixmap		dum;
	Window		iconWindow;
	int		status;

	/*  make a window for the icon */
	iconWindow = XCreateSimpleWindow(tool_d, DefaultRootWindow(tool_d),
					 0, 0, 1, 1, 0,
					black_color.pixel, black_color.pixel);
	xfig_icon_attr.valuemask = XpmReturnPixels | XpmCloseness;
	xfig_icon_attr.colormap = tool_cm;
	/* flag whether or not to free colors when quitting xfig */
	xfig_icon_attr.npixels = 0;
	status = XpmCreatePixmapFromData(tool_d, iconWindow,
				     fig_c_icon_X, &fig_icon, &dum, &xfig_icon_attr);
	/* if all else fails, use standard monochrome bitmap for icon */
	if (status == XpmSuccess) {
	    XResizeWindow(tool_d, iconWindow,
			  xfig_icon_attr.width,
			  xfig_icon_attr.height);
	    XSetWindowBackgroundPixmap(tool_d, iconWindow, fig_icon);
	    XtVaSetValues(tool, XtNiconWindow, iconWindow, NULL);
	} else {
	    fig_icon = XCreateBitmapFromData(tool_d, tool_w,
				     (char *) fig_bits, fig_width, fig_height);
	}
    } else {
#endif /* USE_XPM_ICON */
	fig_icon = XCreateBitmapFromData(tool_d, tool_w,
				     (char *) fig_bits, fig_width, fig_height);
#ifdef USE_XPM_ICON
    }
#endif /* USE_XPM_ICON */

    FirstArg(XtNtitle, tool_name);
    NextArg(XtNiconPixmap, fig_icon);
    SetValues(tool);
    /* Set the input field to true to allow keyboard input */
    wmhints = XGetWMHints(tool_d, tool_w);
    wmhints->flags |= InputHint;/* add in input hint */
    wmhints->input = True;
    XSetWMHints(tool_d, tool_w, wmhints);
    XFree((char *) wmhints);

    if (appres.RHS_PANEL) {	/* side button panel is on right size */
	FirstArg(XtNfromHoriz, 0);
	NextArg(XtNhorizDistance, SIDERULER_WD + INTERNAL_BW);
	SetValues(topruler_sw);

	FirstArg(XtNfromHoriz, 0);
	NextArg(XtNhorizDistance, 0);
	NextArg(XtNfromVert, topruler_sw);
	NextArg(XtNleft, XtChainLeft);	/* chain to left of form */
	NextArg(XtNright, XtChainLeft);
	SetValues(sideruler_sw);

	FirstArg(XtNfromHoriz, 0);
	NextArg(XtNhorizDistance, 0);
	NextArg(XtNfromVert, msg_form);
	NextArg(XtNleft, XtChainLeft);	/* chain to left of form */
	NextArg(XtNright, XtChainLeft);
	SetValues(unitbox_sw);

	/* relocate the side button panel */
	XtUnmanageChild(mode_panel);
	XtUnmanageChild(canvas_sw);
	FirstArg(XtNfromHoriz, sideruler_sw);	/* canvas goes right of side ruler */
	SetValues(canvas_sw);
	FirstArg(XtNfromHoriz, canvas_sw);	/* panel goes right of canvas */
	NextArg(XtNhorizDistance, -INTERNAL_BW);
	NextArg(XtNfromVert, mousefun);
	NextArg(XtNleft, XtChainRight);
	NextArg(XtNright, XtChainRight);
	SetValues(mode_panel);
	XtManageChild(canvas_sw);
	XtManageChild(mode_panel);
    }

    init_gc();

    /* get the size of the whole shebang */
    FirstArg(XtNwidth, &w);
    NextArg(XtNheight, &h);
    GetValues(tool);
    TOOL_WD = (int) w;
    TOOL_HT = (int) h;

    setup_cmd_panel();
    setup_msg();
    setup_canvas();
    setup_rulers();
    setup_mode_panel();
    setup_mousefun();
    setup_fontmenu();		/* setup bitmaps in printer font menu */
    setup_ind_panel();
    get_directory(cur_dir);

    /* parse the export language resource */
    for (i=0; i<NUM_EXP_LANG; i++)
	if (strcasecmp(appres.exportLanguage, lang_items[i])==0)
	    break;
    /* found it set the language number */
    if (i < NUM_EXP_LANG)
	cur_exp_lang = i;
    else
	file_msg("Unknown export language: %s, using default: %s",
		appres.exportLanguage, lang_items[cur_exp_lang]);

    /* install the accelerators - cmd_panel, ind_panel and mode_panel
	accelerators are installed in their respective setup_xxx procedures */
    XtInstallAllAccelerators(canvas_sw, tool);
    XtInstallAllAccelerators(mousefun, tool);
    XtInstallAllAccelerators(msg_form, tool);
    XtInstallAllAccelerators(topruler_sw, tool);
    XtInstallAllAccelerators(sideruler_sw, tool);
    XtInstallAllAccelerators(unitbox_sw, tool);
    XtInstallAllAccelerators(ind_panel, tool);
    XtInstallAllAccelerators(mode_panel, tool);

    XtAppAddActions(tool_app, form_actions, XtNumber(form_actions));
    XtAppAddActions(tool_app, text_panel_actions, XtNumber(text_panel_actions));
    XtOverrideTranslations(tool, XtParseTranslationTable(tool_translations));
    XtOverrideTranslations(form, XtParseTranslationTable(form_translations));

    if (!appres.DEBUG) {
	XSetErrorHandler(X_error_handler);
	XSetIOErrorHandler((XIOErrorHandler) X_error_handler);
    }

    (void) signal(SIGHUP, error_handler);
    (void) signal(SIGFPE, error_handler);
#ifdef SIGBUS
    (void) signal(SIGBUS, error_handler);
#endif
    (void) signal(SIGSEGV, error_handler);
    (void) signal(SIGINT, SIG_IGN);	/* in case user accidentally types ctrl-c */

    put_msg("READY. Select a mode or load a file");

    /*
     * decide on filename for cut buffer: first try users HOME directory to
     * allow cutting and pasting between sessions, if this fails create
     * unique filename in /tmp dir
     */

    userhome = getenv("HOME");
    if (userhome != NULL && *strcpy(cut_buf_name, userhome) != '\0') {
	strcat(cut_buf_name, "/.xfig");
    } else {
	sprintf(cut_buf_name, "%s%06d", "/tmp/xfig", getpid());
    }

    if (filename == NULL)
	strcpy(cur_filename, DEF_NAME);
    else
	load_file(filename,0,0);
    update_cur_filename(cur_filename);

    app_flush();

    /* If the user requests a tablet then do the set up for it */
    /*   and handle the tablet XInput extension events */
    /*   in a custom XtAppMainLoop gjl */

    if (appres.tablet) {

#ifndef USE_TAB
	file_msg("Input tablet not compiled in xfig - option ignored");
	appres.tablet = False;
#else

#define TABLETINCHES 11.7
#define SETBUTEVT(d, e) ((d).serial = (e)->serial, \
	(d).window 	= (e)->window, (d).root = (e)->root, \
	(d).subwindow 	= (e)->subwindow, (d).time = (e)->time, \
	(d).x 		= (e)->axis_data[0] / max2(tablet_res, 0.1), \
	(d).state 	= (e)->state, \
	(d).y 		= ((int) ht - (e)->axis_data[1] / max2(tablet_res, 0.1)), \
	(d).button 	= (e)->button)
/* Switch buttons because of the layout of the buttons on the mouse */
#define SWITCHBUTTONS(d) ((d).button = ((d).button == Button2) ? Button1 : \
        ((d).button == Button1) ? Button2 : \
        ((d).button == Button4) ? Button3 : Button4)

	XEventClass eventList[3];
	XEvent event;
	XDeviceMotionEvent *devmotevt;
	XDeviceButtonEvent *devbutevt;
	XDevice		*tablet;
	XDeviceState	*tabletState;
	XValuatorState	*valState;
	int i, numDevs, motiontype, butprstype, butreltype, dum;
	long  minval, maxval;

	/* Get the device list */
	XDeviceInfo *devPtr, *devInfo;
	/* tablet_res is ratio between the tablet res and the screen res */
	float tablet_res = 10.0;
	/* ht is the height of the tablet at 100dpi */
	Dimension ht, wd;

	XButtonEvent xprs, xrel;

	xprs.type = ButtonPress, xrel.type = ButtonRelease;
	xprs.send_event = xprs.same_screen =
	  xrel.send_event = xrel.same_screen = True;
	xprs.button = xrel.button = Button1;

	/* check if the XInputExtension exists */
	if (!XQueryExtension(tool_d, INAME, &dum, &dum, &dum))
		goto notablet;

	/* now search the device list for the tablet */
        devInfo = XListInputDevices(tool_d, &numDevs);
	if (numDevs == 0)
		goto notablet;
    
	/* Open the tablet device and select the event types */
	for (i = 0, devPtr = devInfo; i < numDevs; i++, devPtr++)
	  if (! strcmp(devPtr->name, XI_TABLET))
	    if ((tablet = XOpenDevice(tool_d, devPtr->id)) == NULL)
	      printf("Unable to open tablet\n");

	    else {
	      DeviceMotionNotify(tablet,  motiontype, eventList[0]);
	      DeviceButtonPress(tablet,   butprstype, eventList[1]);
	      DeviceButtonRelease(tablet, butreltype, eventList[2]);

	      if (XSelectExtensionEvent(tool_d,
	             XtWindow(canvas_sw), eventList, 3))
	        printf("Bad status on XSelectExtensionEvent\n");
	    }

	XFreeDeviceList(devInfo);

	/* Get the valuator data which should give the resolution */
	/*   of the tablet in absolute mode (the default / what we want) */
	/*   Problem with sgi array index (possibly word size related ) */
	tabletState = XQueryDeviceState(tool_d, tablet);
	valState = (XValuatorState *) tabletState->data;
	for (i = 0; i < tabletState->num_classes; i++)
	  if ((int) valState->class == ValuatorClass)
	  {
	    if (valState->num_valuators)
	    {
#if sgi
	      minval = valState->valuators[4];
	      maxval = valState->valuators[5];
#else
	      minval = valState->valuators[0];
	      maxval = valState->valuators[1];
#endif
	      tablet_res = ((float) maxval / TABLETINCHES / screen_res);
	      if (tablet_res <= 0.0 || tablet_res > 100.0)
	        tablet_res = 12.0;

              if (appres.DEBUG)
	        printf("TABLET: Res: %f %d %d %d %d\n", tablet_res,
	          valState->valuators[8], valState->valuators[10],
	          minval, maxval);
	    }
	  }
	  else
	    valState = (XValuatorState *)
	      ((long) valState + (int) valState->length);

	XFreeDeviceState(tabletState);

	xprs.display = xrel.display = tool_d;
        FirstArg(XtNheight, &ht);
        NextArg(XtNwidth, &wd);
        GetValues(canvas_sw);
	
	/* "XtAppMainLoop" customized for extension events */
        /* For tablet puck motion events use the location */
        /* info to warp the cursor to the corresponding screen */
        /* position.  For puck button events switch the buttons */
        /* to correspond to the mouse buttons and send a mouse */
        /* button event to the server so the program will just */
        /* think it is getting a mouse button event and act */
        /* appropriately */
	for (;;) {
	  XtAppNextEvent(tool_app, &event);
	  if (event.type == motiontype) {
	    devmotevt = (XDeviceMotionEvent *) &event;
            devmotevt->axis_data[0] /= tablet_res;
            devmotevt->axis_data[1] /= tablet_res;

            /* Keep the pointer within the canvas window */
            FirstArg(XtNheight, &ht);
            NextArg(XtNwidth, &wd);
            GetValues(canvas_sw);
	    XWarpPointer(tool_d, None, XtWindow(canvas_sw), None, None, 0, 0,
		 min2(devmotevt->axis_data[0], (int) wd),
		 max2((int) ht - devmotevt->axis_data[1], 0));
	  }
	  else if (event.type == butprstype) {
	    devbutevt = (XDeviceButtonEvent *) &event;
	    SETBUTEVT(xprs, devbutevt);
            SWITCHBUTTONS(xprs);
	    XSendEvent(tool_d, PointerWindow, True,
	       ButtonPressMask, (XEvent *) &xprs);
	  }
	  else if (event.type == butreltype) {
	    devbutevt = (XDeviceButtonEvent *) &event;
	    SETBUTEVT(xrel, devbutevt);
            SWITCHBUTTONS(xrel);
	    XSendEvent(tool_d, PointerWindow, True, 
	       ButtonReleaseMask, (XEvent *) &xrel);
	  }
	  else
	    XtDispatchEvent(&event);
	}
notablet:
	file_msg("No input tablet present");

#endif /* USE_TAB */

	XtAppMainLoop(tool_app);
    }
    else
	XtAppMainLoop(tool_app);
    return 0;
}

static void
check_for_resize(tool, event, params, nparams)
    Widget	    tool;
    XButtonEvent   *event;
    String	   *params;
    Cardinal	   *nparams;
{
    int		    dx, dy;
    XConfigureEvent *xc = (XConfigureEvent *) event;

    if (xc->width == TOOL_WD && xc->height == TOOL_HT)
	return;		/* no size change */
    dx = xc->width - TOOL_WD;
    dy = xc->height - TOOL_HT;
    TOOL_WD = xc->width;
    TOOL_HT = xc->height;
    resize_all(CANVAS_WD + dx, CANVAS_HT + dy);
}

/* resize whole shebang given new canvas size (width,height) */

resize_all(width, height)
    int width, height;
{
    DeclareArgs(3);
    Dimension	    b;

    setup_sizes(width, height);

    XawFormDoLayout(form, False);
    ignore_exp_cnt++;		/* canvas is resized twice - redraw only once */

    FirstArg(XtNborderWidth, &b);
    /* first redo the top panels */
    GetValues(cmd_panel);
    XtResizeWidget(cmd_panel, CMDPANEL_WD, CMDPANEL_HT, b);
    GetValues(mousefun);
    XtResizeWidget(mousefun, MOUSEFUN_WD, MOUSEFUN_HT, b);
    XtUnmanageChild(mousefun);
    resize_mousefun();
    XtManageChild(mousefun);	/* so that it shifts with msg_panel */
    /* resize the message form by setting the current filename */
    update_cur_filename(cur_filename);

    /* now redo the center area */
    XtUnmanageChild(mode_panel);
    FirstArg(XtNheight, (MODEPANEL_SPACE + 1) / 2);
    SetValues(d_label);
    FirstArg(XtNheight, (MODEPANEL_SPACE) / 2);
    SetValues(e_label);
    XtManageChild(mode_panel);	/* so that it adjusts properly */

    FirstArg(XtNborderWidth, &b);
    GetValues(canvas_sw);
    XtResizeWidget(canvas_sw, CANVAS_WD, CANVAS_HT, b);
    GetValues(topruler_sw);
    XtResizeWidget(topruler_sw, TOPRULER_WD, TOPRULER_HT, b);
    resize_topruler();
    GetValues(sideruler_sw);
    XtResizeWidget(sideruler_sw, SIDERULER_WD, SIDERULER_HT, b);
    resize_sideruler();
    XtUnmanageChild(sideruler_sw);
    XtManageChild(sideruler_sw);/* so that it shifts with canvas */
    XtUnmanageChild(unitbox_sw);
    XtManageChild(unitbox_sw);	/* so that it shifts with canvas */

    XawFormDoLayout(form, True);
}

static void
check_colors()
{
    int		    i;
    XColor	    dum,color;

    /* no need to allocate black and white specially */
    colors[BLACK] = black_color.pixel;
    colors[WHITE] = white_color.pixel;
    /* fill the colors array with black (except for white) */
    for (i=0; i<NUM_STD_COLS; i++)
	if (i != BLACK && i != WHITE)
		colors[i] = colors[BLACK];

    /* initialize user color cells */
    for (i=0; i<MAX_USR_COLS; i++) {
	    colorFree[i] = True;
	    n_colorFree[i] = True;
	    num_usr_cols = 0;
    }

    /* if monochrome resource is set, do not even check for colors */
    if (!all_colors_available || appres.monochrome) {
	return;
    }

    for (i=0; i<NUM_STD_COLS; i++) {
	/* try to allocate another named color */
	/* first try by #xxxxx form if exists, then by name from rgb.txt file */
	if (!xallncol(colorNames[i+1].rgb,&color,&dum)) {
	     /* can't allocate it, switch colormaps try again */
	     if (!switch_colormap() || 
	        (!xallncol(colorNames[i+1].rgb,&color,&dum))) {
		    fprintf(stderr, "Not enough colormap entries available for basic colors\n");
		    fprintf(stderr, "using monochrome mode.\n");
		    all_colors_available = False;
		    return;
	    }
	}
	/* put the colorcell number in the color array */
	colors[i] = color.pixel;
    }
}

/* useful when using ups */
XSyncOn()
{
	XSynchronize(tool_d, True);
	XFlush(tool_d);
}

XSyncOff()
{
	XSynchronize(tool_d, False);
	XFlush(tool_d);
}

/* 
 * This will parse the hexadecimal form of the named colors in the standard color
 * names.  Some servers can't parse the hex form for XAllocNamedColor()
 */

int
xallncol(name,color,exact)
    char	*name;
    XColor	*color,*exact;
{
    unsigned	short r,g,b;
    char	nam[30];

    if (*name != '#')
	return XAllocNamedColor(tool_d,tool_cm,name,color,exact);

    /* gcc doesn't allow writing on constant strings without the -fwritable_strings
       option, and apparently some versions of sscanf need to write a char back */
    strcpy(nam,name);
    if (sscanf(nam,"#%2hx%2hx%2hx",&r,&g,&b) != 3 || nam[7] != '\0') {
	fprintf(stderr,
	  "Malformed color specification %s in resources.c must be 6 hex digits",nam);
	exit(1);
    }

    color->red   = r<<8;
    color->green = g<<8;
    color->blue  = b<<8;
    color->flags = DoRed|DoGreen|DoBlue;
    *exact = *color;
    return XAllocColor(tool_d,tool_cm,color);
}
