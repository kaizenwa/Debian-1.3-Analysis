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
#include "resources.h"
#include "mode.h"
#include "paintop.h"
#include "w_drawprim.h"
#include "w_icons.h"
#include "w_mousefun.h"
#include "w_setup.h"
#include "w_util.h"
#include "w_zoom.h"
#include "object.h"

/*
 * The following will create rulers the same size as the initial screen size.
 * if the user resizes the xfig window, the rulers won't have numbers there.
 * Should really reset the sizes if the screen resizes.
 */

/* height of ticks for fraction of inch/cm */
#define			INCH_MARK		8
#define			HALF_MARK		8
#define			QUARTER_MARK		6
#define			SIXTEENTH_MARK		4

#define			TRM_WID			16
#define			TRM_HT			8
#define			SRM_WID			8
#define			SRM_HT			16

extern		pan_origin();
extern	GC	tr_gc, tr_xor_gc, tr_erase_gc;
extern	GC	sr_gc, sr_xor_gc, sr_erase_gc;

static int	lasty = -100, lastx = -100;
static int	troffx = -8, troffy = -10;
static int	orig_zoomoff;
static int	last_drag_x, last_drag_y;
static unsigned char	tr_marker_bits[] = {
    0xFE, 0xFF,		/* ***************  */
    0x04, 0x40,		/*  *           *  */
    0x08, 0x20,		/*   *         *  */
    0x10, 0x10,		/*    *       *  */
    0x20, 0x08,		/*     *     *  */
    0x40, 0x04,		/*      *   *  */
    0x80, 0x02,		/*       * *  */
    0x00, 0x01,		/*        *  */
};
icon_struct trm_pr = {TRM_WID, TRM_HT, (char*)tr_marker_bits};

static int	srroffx = 2, srroffy = -7;
static unsigned char	srr_marker_bits[] = {
    0x80,		/*        *  */
    0xC0,		/*       **  */
    0xA0,		/*      * *  */
    0x90,		/*     *  *  */
    0x88,		/*    *   *  */
    0x84,		/*   *    *  */
    0x82,		/*  *     *  */
    0x81,		/* *      *  */
    0x82,		/*  *     *  */
    0x84,		/*   *    *  */
    0x88,		/*    *   *  */
    0x90,		/*     *  *  */
    0xA0,		/*      * *  */
    0xC0,		/*       **  */
    0x80,		/*        *  */
    0x00
};
icon_struct srrm_pr = {SRM_WID, SRM_HT, (char*)srr_marker_bits};

static int	srloffx = -10, srloffy = -7;
static unsigned char	srl_marker_bits[] = {
    0x01,		/* *	      */
    0x03,		/* **	      */
    0x05,		/* * *	      */
    0x09,		/* *  *	      */
    0x11,		/* *   *      */
    0x21,		/* *    *     */
    0x41,		/* *     *    */
    0x81,		/* *      *   */
    0x41,		/* *     *    */
    0x21,		/* *    *     */
    0x11,		/* *   *      */
    0x09,		/* *  *	      */
    0x05,		/* * *	      */
    0x03,		/* **	      */
    0x01,		/* *	      */
    0x00
};
icon_struct srlm_pr = {SRM_WID, SRM_HT, (char*)srl_marker_bits};

static Pixmap	toparrow_pm = 0, sidearrow_pm = 0;
static Pixmap	topruler_pm = 0, sideruler_pm = 0;

DeclareStaticArgs(14);

static		topruler_selected();
static		topruler_exposed();
static		sideruler_selected();
static		sideruler_exposed();

redisplay_rulers()
{
    redisplay_topruler();
    redisplay_sideruler();
}

setup_rulers()
{
    setup_topruler();
    setup_sideruler();
}

reset_rulers()
{
    reset_topruler();
    reset_sideruler();
}

set_rulermark(x, y)
    int		    x, y;
{
    if (appres.TRACKING) {
	set_siderulermark(y);
	set_toprulermark(x);
    }
}

erase_rulermark()
{
    if (appres.TRACKING) {
	erase_siderulermark();
	erase_toprulermark();
    }
}

static int	HINCH = (PIX_PER_INCH / 2);
static int	QINCH = (PIX_PER_INCH / 4);
static int	SINCH = (PIX_PER_INCH / 16);
static int	TWOMM = (PIX_PER_CM / 5);
static int	ONEMM = (PIX_PER_CM / 10);

/************************* UNITBOX ************************/

extern Atom	wm_delete_window;
extern char	*panel_get_value();
void popup_unit_panel();
static Boolean	rul_unit_setting;
static int	fig_unit_setting=0, fig_scale_setting=0;
static Widget	rul_unit_panel, fig_unit_panel, fig_scale_panel;
static Widget	rul_unit_menu, fig_unit_menu, fig_scale_menu;
static Widget	scale_factor_lab, scale_factor_panel;
static Widget	user_unit_lab, user_unit_panel;


XtActionsRec	unitbox_actions[] =
{
    {"EnterUnitBox", (XtActionProc) draw_mousefun_unitbox},
    {"LeaveUnitBox", (XtActionProc) clear_mousefun},
    {"HomeRulers", (XtActionProc) pan_origin},
    {"PopupUnits", (XtActionProc) popup_unit_panel},
};

static String	unitbox_translations =
"<EnterWindow>:EnterUnitBox()\n\
    <LeaveWindow>:LeaveUnitBox()\n\
    <Btn1Down>:HomeRulers()\n\
    <Btn3Down>:PopupUnits()\n";

int
init_unitbox(tool)
    Widget	    tool;
{
    FirstArg(XtNwidth, SIDERULER_WD);
    NextArg(XtNheight, RULER_WD);
    NextArg(XtNlabel, appres.INCHES ? "in" : "cm");
    NextArg(XtNfromHoriz, canvas_sw);
    NextArg(XtNhorizDistance, -INTERNAL_BW);
    NextArg(XtNfromVert, msg_form);
    NextArg(XtNvertDistance, -INTERNAL_BW);
    NextArg(XtNresizable, False);
    NextArg(XtNtop, XtChainTop);
    NextArg(XtNbottom, XtChainTop);
    NextArg(XtNleft, XtChainLeft);
    NextArg(XtNright, XtChainLeft);
    NextArg(XtNborderWidth, INTERNAL_BW);

    if (strlen(cur_fig_units))
	fig_unit_setting = 1;
    else {
	strcpy(cur_fig_units, appres.INCHES ? "in" : "cm");
    }

    if (appres.user_scale != 1.0)
	fig_scale_setting = 1;

    unitbox_sw = XtCreateWidget("unitbox", labelWidgetClass, tool,
				Args, ArgCount);
    XtAppAddActions(tool_app, unitbox_actions, XtNumber(unitbox_actions));
    XtOverrideTranslations(unitbox_sw,
			   XtParseTranslationTable(unitbox_translations));
    return (1);
}

static String   unit_translations =
        "<Message>WM_PROTOCOLS: QuitUnits()\n";
static void     unit_panel_cancel(), unit_panel_set();
static XtActionsRec     unit_actions[] =
{
    {"QuitUnits", (XtActionProc) unit_panel_cancel},
    {"SetUnit", (XtActionProc) unit_panel_set},
};

static Widget	unit_popup, form, cancel, set, beside, below, label;

/* handle unit/scale settings */

static void
unit_panel_dismiss()
{
    XtDestroyWidget(unit_popup);
    XtSetSensitive(unitbox_sw, True);
}

static void
unit_panel_cancel(w, ev)
    Widget	    w;
    XButtonEvent   *ev;
{
    unit_panel_dismiss();
}

static void
unit_panel_set(w, ev)
    Widget	    w;
    XButtonEvent   *ev;
{
    int old_rul_unit;

    old_rul_unit = appres.INCHES;
    appres.INCHES = rul_unit_setting ? True : False;
    init_grid();	/* change point positioning messages if necessary */
    if (fig_scale_setting)
	appres.user_scale = (float) atof(panel_get_value(scale_factor_panel));
    else
	appres.user_scale = 1.0;

    if (fig_unit_setting) {
        strncpy(cur_fig_units,
	    panel_get_value(user_unit_panel),
	    sizeof(cur_fig_units)-1);
	put_msg("Ruler scale: %s,  Figure scale: 1 %s = %.4f %s",
		appres.INCHES ? "in" : "cm",
		appres.INCHES ? "in" : "cm",
		appres.user_scale, cur_fig_units);
    } else {
	strcpy(cur_fig_units, appres.INCHES ? "in" : "cm");
	put_msg("Ruler scale: %s,  Figure scale = 1:%.4f",
		appres.INCHES ? "in" : "cm", appres.user_scale);
    }

    if (old_rul_unit != appres.INCHES) {
	/* change the label in the widget */
	FirstArg(XtNlabel, appres.INCHES ? "in" : "cm");
	SetValues(unitbox_sw);
	reset_rulers();
	setup_grid(cur_gridmode);
	if(!emptyfigure()) { /* rescale, HWS */
	  if(old_rul_unit)
	    read_scale_compound(&objects,(2.54*PIX_PER_CM)/((float)PIX_PER_INCH),0);
	  else
	    read_scale_compound(&objects,((float)PIX_PER_INCH)/(2.54*PIX_PER_CM),0);
	  redisplay_canvas();
	}
    }
    unit_panel_dismiss();
}

static void
fig_unit_select(w, new_unit, garbage)
    Widget          w;
    XtPointer       new_unit, garbage;
{
    FirstArg(XtNlabel, XtName(w));
    SetValues(fig_unit_panel);
    fig_unit_setting = (int) new_unit;
    XtSetSensitive(user_unit_lab, fig_unit_setting ? True : False);
    XtSetSensitive(user_unit_panel, fig_unit_setting ? True : False);
    put_msg(fig_unit_setting ? "user defined figure units"
			     : "figure units = ruler units");
}

static void
fig_scale_select(w, new_scale, garbage)
    Widget          w;
    XtPointer       new_scale, garbage;
{
    FirstArg(XtNlabel, XtName(w));
    SetValues(fig_scale_panel);
    fig_scale_setting = (int) new_scale;
    XtSetSensitive(scale_factor_lab, fig_scale_setting ? True : False);
    XtSetSensitive(scale_factor_panel, fig_scale_setting ? True : False);
    put_msg(fig_scale_setting ? "user defined scale factor"
			     : "figure scale = 1:1");
}

static void
rul_unit_select(w, new_unit, garbage)
    Widget          w;
    XtPointer       new_unit, garbage;
{
    FirstArg(XtNlabel, XtName(w));
    SetValues(rul_unit_panel);
    rul_unit_setting = ((int)new_unit)==1? True: False;
    if (rul_unit_setting)
	put_msg("ruler scale : inches");
    else
	put_msg("ruler scale : centimetres");
}

void
popup_unit_panel()
{
    Position	    x_val, y_val;
    Dimension	    width, height;
    char	    buf[32];
    static int      actions_added=0;
    static char    *rul_unit_items[] = {
    "Metric (cm)  ", "Imperial (in)"};
    static char    *fig_unit_items[] = {
    "Ruler units ", "User defined"};
    static char    *fig_scale_items[] = {
    "Unity       ", "User defined"};

    FirstArg(XtNwidth, &width);
    NextArg(XtNheight, &height);
    GetValues(tool);
    /* position the popup 2/3 in from left and 1/3 down from top */
    XtTranslateCoords(tool, (Position) (2 * width / 3), (Position) (height / 3),
		      &x_val, &y_val);

    FirstArg(XtNx, x_val);
    NextArg(XtNy, y_val);
    NextArg(XtNwidth, 240);
    NextArg(XtNcolormap, tool_cm);

    unit_popup = XtCreatePopupShell("set_unit_panel",
				    transientShellWidgetClass, tool,
				    Args, ArgCount);
    XtOverrideTranslations(unit_popup,
                       XtParseTranslationTable(unit_translations));
    if (!actions_added) {
        XtAppAddActions(tool_app, unit_actions, XtNumber(unit_actions));
	actions_added = 1;
    }

    form = XtCreateManagedWidget("form", formWidgetClass, unit_popup, NULL, 0);

    FirstArg(XtNborderWidth, 0);
    sprintf(buf, "      Unit/Scale settings");
    label = XtCreateManagedWidget(buf, labelWidgetClass, form, Args, ArgCount);

    /* make ruler units menu */

    rul_unit_setting = appres.INCHES ? True : False;
    FirstArg(XtNfromVert, label);
    NextArg(XtNborderWidth, 0);
    beside = XtCreateManagedWidget("Ruler Units  =", labelWidgetClass,
                                   form, Args, ArgCount);

    FirstArg(XtNfromVert, label);
    NextArg(XtNfromHoriz, beside);
    rul_unit_panel = XtCreateManagedWidget(rul_unit_items[rul_unit_setting? 1:0],
				menuButtonWidgetClass, form, Args, ArgCount);
    below = rul_unit_panel;
    rul_unit_menu = make_popup_menu(rul_unit_items, XtNumber(rul_unit_items),
                                     rul_unit_panel, rul_unit_select);

    /* make figure units menu */

    FirstArg(XtNfromVert, below);
    NextArg(XtNborderWidth, 0);
    beside = XtCreateManagedWidget("Figure units =", labelWidgetClass,
                                   form, Args, ArgCount);

    FirstArg(XtNfromVert, below);
    NextArg(XtNfromHoriz, beside);
    fig_unit_panel = XtCreateManagedWidget(fig_unit_items[fig_unit_setting],
				menuButtonWidgetClass, form, Args, ArgCount);
    below = fig_unit_panel;
    fig_unit_menu = make_popup_menu(fig_unit_items, XtNumber(fig_unit_items),
                                     fig_unit_panel, fig_unit_select);

    /* user defined units */

    FirstArg(XtNfromVert, below);
    NextArg(XtNborderWidth, 0);
    NextArg(XtNlabel, "Unit Name =");
    user_unit_lab = XtCreateManagedWidget("user_units",
                                labelWidgetClass, form, Args, ArgCount);

    FirstArg(XtNfromVert, below);
    NextArg(XtNborderWidth, INTERNAL_BW);
    NextArg(XtNfromHoriz, label);
    NextArg(XtNstring, cur_fig_units);
    NextArg(XtNinsertPosition, strlen(buf));
    NextArg(XtNeditType, XawtextEdit);
    NextArg(XtNwidth, 40);
    user_unit_panel = XtCreateManagedWidget(buf, asciiTextWidgetClass,
					form, Args, ArgCount);
    XtOverrideTranslations(user_unit_panel,
		XtParseTranslationTable(text_translations));
    below = user_unit_panel;

    /* make figure scale menu */

    FirstArg(XtNfromVert, below);
    NextArg(XtNborderWidth, 0);
    beside = XtCreateManagedWidget("Figure scale =", labelWidgetClass,
                                   form, Args, ArgCount);

    FirstArg(XtNfromVert, below);
    NextArg(XtNfromHoriz, beside);
    fig_scale_panel = XtCreateManagedWidget(fig_scale_items[fig_scale_setting],
				menuButtonWidgetClass, form, Args, ArgCount);
    below = fig_scale_panel;
    fig_scale_menu = make_popup_menu(fig_scale_items, XtNumber(fig_scale_items),
                                     fig_scale_panel, fig_scale_select);

    /* scale factor widget */

    FirstArg(XtNfromVert, below);
    NextArg(XtNborderWidth, 0);
    NextArg(XtNlabel, "Scale factor =");
    scale_factor_lab = XtCreateManagedWidget("scale_factor",
                                labelWidgetClass, form, Args, ArgCount);

    sprintf(buf, "%1.4f", appres.user_scale);
    FirstArg(XtNfromVert, below);
    NextArg(XtNborderWidth, INTERNAL_BW);
    NextArg(XtNfromHoriz, label);
    NextArg(XtNstring, buf);
    NextArg(XtNinsertPosition, strlen(buf));
    NextArg(XtNeditType, XawtextEdit);
    NextArg(XtNwidth, 40);
    scale_factor_panel = XtCreateManagedWidget(buf, asciiTextWidgetClass,
                                        form, Args, ArgCount);
    XtOverrideTranslations(scale_factor_panel,
		XtParseTranslationTable(text_translations));
    below = scale_factor_panel;

    /* standard cancel/set buttons */

    FirstArg(XtNlabel, "cancel");
    NextArg(XtNfromVert, below);
    NextArg(XtNborderWidth, INTERNAL_BW);
    cancel = XtCreateManagedWidget("cancel", commandWidgetClass,
				   form, Args, ArgCount);
    XtAddEventHandler(cancel, ButtonReleaseMask, (Boolean) 0,
		      (XtEventHandler)unit_panel_cancel, (XtPointer) NULL);

    FirstArg(XtNlabel, "set");
    NextArg(XtNfromVert, below);
    NextArg(XtNfromHoriz, cancel);
    NextArg(XtNborderWidth, INTERNAL_BW);
    set = XtCreateManagedWidget("set", commandWidgetClass,
				form, Args, ArgCount);
    XtAddEventHandler(set, ButtonReleaseMask, (Boolean) 0,
		      (XtEventHandler)unit_panel_set, (XtPointer) NULL);

    XtPopup(unit_popup, XtGrabExclusive);
    /* insure that the most recent colormap is installed */
    set_cmap(XtWindow(unit_popup));

    XtSetSensitive(user_unit_lab, fig_unit_setting ? True : False);
    XtSetSensitive(user_unit_panel, fig_unit_setting ? True : False);
    XtSetSensitive(scale_factor_lab, fig_scale_setting ? True : False);
    XtSetSensitive(scale_factor_panel, fig_scale_setting ? True : False);

    (void) XSetWMProtocols(XtDisplay(unit_popup), XtWindow(unit_popup),
                           &wm_delete_window, 1);
}
/************************* TOPRULER ************************/

XtActionsRec	topruler_actions[] =
{
    {"EventTopRuler", (XtActionProc) topruler_selected},
    {"ExposeTopRuler", (XtActionProc) topruler_exposed},
    {"EnterTopRuler", (XtActionProc) draw_mousefun_topruler},
    {"LeaveTopRuler", (XtActionProc) clear_mousefun},
};

static String	topruler_translations =
"Any<BtnDown>:EventTopRuler()\n\
    Any<BtnUp>:EventTopRuler()\n\
    <Btn2Motion>:EventTopRuler()\n\
    Meta <Btn3Motion>:EventTopRuler()\n\
    <EnterWindow>:EnterTopRuler()\n\
    <LeaveWindow>:LeaveTopRuler()\n\
    <KeyPress>:EnterTopRuler()\n\
    <KeyRelease>:EnterTopRuler()\n\
    <Expose>:ExposeTopRuler()\n";

static
topruler_selected(tool, event, params, nparams)
    Widget	    tool;
    XButtonEvent   *event;
    String	   *params;
    Cardinal	   *nparams;
{
    XButtonEvent   *be = (XButtonEvent *) event;

    switch (event->type) {
    case ButtonPress:
	if (be->button == Button3 && be->state & Mod1Mask) {
	    be->button = Button2;
	}
	switch (be->button) {
	case Button1:
	    XDefineCursor(tool_d, topruler_win, l_arrow_cursor);
	    break;
	case Button2:
	    XDefineCursor(tool_d, topruler_win, bull_cursor);
	    orig_zoomoff = zoomxoff;
	    last_drag_x = event->x;
	    break;
	case Button3:
	    XDefineCursor(tool_d, topruler_win, r_arrow_cursor);
	    break;
	}
	break;
    case ButtonRelease:
	if (be->button == Button3 && be->state & Mod1Mask) {
	    be->button = Button2;
	}
	switch (be->button) {
	case Button1:
	    pan_left(event->state&ShiftMask);
	    break;
	case Button2:
	    if (orig_zoomoff != zoomxoff)
		setup_grid(cur_gridmode);
	    break;
	case Button3:
	    pan_right(event->state&ShiftMask);
	    break;
	}
	XDefineCursor(tool_d, topruler_win, lr_arrow_cursor);
	break;
    case MotionNotify:
	if (event->x != last_drag_x)
	    if ((zoomxoff != 0) || (event->x < last_drag_x)) {
		zoomxoff -= ((event->x - last_drag_x)/zoomscale*
					(event->state&ShiftMask?5.0:1.0));
		if (zoomxoff < 0)
		    zoomxoff = 0;
		reset_topruler();
		redisplay_topruler();
	    }
	last_drag_x = event->x;
	break;
    }
}

erase_toprulermark()
{
    XClearArea(tool_d, topruler_win, ZOOMX(lastx) + troffx,
	       TOPRULER_HT + troffy, trm_pr.width,
	       trm_pr.height, False);
}

set_toprulermark(x)
    int		    x;
{
    XClearArea(tool_d, topruler_win, ZOOMX(lastx) + troffx,
	       TOPRULER_HT + troffy, trm_pr.width,
	       trm_pr.height, False);
    XCopyArea(tool_d, toparrow_pm, topruler_win, tr_xor_gc,
	      0, 0, trm_pr.width, trm_pr.height,
	      ZOOMX(x) + troffx, TOPRULER_HT + troffy);
    lastx = x;
}

static
topruler_exposed(tool, event, params, nparams)
    Widget	    tool;
    XButtonEvent   *event;
    String	   *params;
    Cardinal	   *nparams;
{
    if (((XExposeEvent *) event)->count > 0)
	return;
    redisplay_topruler();
}

redisplay_topruler()
{
    XClearWindow(tool_d, topruler_win);
}

int
init_topruler(tool)
    Widget	    tool;
{
    FirstArg(XtNwidth, TOPRULER_WD);
    NextArg(XtNheight, TOPRULER_HT);
    NextArg(XtNlabel, "");
    NextArg(XtNfromHoriz, mode_panel);
    NextArg(XtNhorizDistance, -INTERNAL_BW);
    NextArg(XtNfromVert, msg_form);
    NextArg(XtNvertDistance, -INTERNAL_BW);
    NextArg(XtNresizable, False);
    NextArg(XtNtop, XtChainTop);
    NextArg(XtNbottom, XtChainTop);
    NextArg(XtNleft, XtChainLeft);
    NextArg(XtNright, XtChainLeft);
    NextArg(XtNborderWidth, INTERNAL_BW);

    topruler_sw = XtCreateWidget("topruler", labelWidgetClass, tool,
				 Args, ArgCount);

    XtAppAddActions(tool_app, topruler_actions, XtNumber(topruler_actions));
    XtOverrideTranslations(topruler_sw,
			   XtParseTranslationTable(topruler_translations));
    return (1);
}

setup_topruler()
{
    unsigned long   bg, fg;
    XGCValues	    gcv;
    unsigned long   gcmask;
    PIX_FONT	    font;

    topruler_win = XtWindow(topruler_sw);
    gcmask = GCFunction | GCForeground | GCBackground | GCFont;

    /* set up the GCs */
    FirstArg(XtNbackground, &bg);
    NextArg(XtNforeground, &fg);
    NextArg(XtNfont, &font);
    GetValues(topruler_sw);

    gcv.font = font->fid;
    gcv.foreground = bg;
    gcv.background = bg;
    gcv.function = GXcopy;
    tr_erase_gc = XCreateGC(tool_d, topruler_win, gcmask, &gcv);

    gcv.foreground = fg;
    tr_gc = XCreateGC(tool_d, topruler_win, gcmask, &gcv);
    /*
     * The arrows will be XORed into the rulers. We want the foreground color
     * in the arrow to result in the foreground or background color in the
     * display. so if the source pixel is fg^bg, it produces fg when XOR'ed
     * with bg, and bg when XOR'ed with bg. If the source pixel is zero, it
     * produces fg when XOR'ed with fg, and bg when XOR'ed with bg.
     */

    /* make pixmaps for top ruler arrow */
    toparrow_pm = XCreatePixmapFromBitmapData(tool_d, topruler_win, 
				trm_pr.bits, trm_pr.width, trm_pr.height, 
				fg^bg, (unsigned long) 0, tool_dpth);

    /* now make the real xor gc */
    gcv.background = bg;
    gcv.function = GXxor;
    tr_xor_gc = XCreateGC(tool_d, topruler_win, gcmask, &gcv);

    XDefineCursor(tool_d, topruler_win, lr_arrow_cursor);

    topruler_pm = XCreatePixmap(tool_d, topruler_win,
				TOPRULER_WD, TOPRULER_HT, tool_dpth);

    reset_topruler();
}

resize_topruler()
{
    XFreePixmap(tool_d, topruler_pm);
    topruler_pm = XCreatePixmap(tool_d, topruler_win,
				TOPRULER_WD, TOPRULER_HT, tool_dpth);

    reset_topruler();
}

reset_topruler()
{
    register int    i;
    register Pixmap p = topruler_pm;
    char	    number[6],clen;
    int		    X0;
    float	    skip;

    /* top ruler, adjustments for digits are kludges based on 6x13 char */
    XFillRectangle(tool_d, p, tr_erase_gc, 0, 0, TOPRULER_WD, TOPRULER_HT);

    clen = char_width(roman_font);
    skip = 1;
    if (display_zoomscale < 0.2 && !appres.INCHES)
	skip = 10;
    else if ((display_zoomscale < 0.2 && appres.INCHES) ||
	     (display_zoomscale < 0.3 && !appres.INCHES))
	        skip = 4;
    else if ((display_zoomscale < 0.3 && appres.INCHES) ||
	     (display_zoomscale < 0.7 && !appres.INCHES))
		skip = 2;
    else if (display_zoomscale >= 2.0 && appres.INCHES)
		skip = 0.5;

    X0 = BACKX(0);
    if (appres.INCHES) {
	X0 -= (X0 % SINCH);
	for (i = X0; i <= X0+round(TOPRULER_WD/zoomscale); i += SINCH) {
	    if ((int)(i/skip) % PIX_PER_INCH == 0) {
		if (1.0*i/PIX_PER_INCH == (int)(i/PIX_PER_INCH))
		    sprintf(number, "%-d", (int)(i / PIX_PER_INCH));
		else
		    sprintf(number, "%-.1f", (float)(1.0 * i / PIX_PER_INCH));
		XDrawString(tool_d, p, tr_gc, ZOOMX(i) - strlen(number)*clen/2,
			TOPRULER_HT - INCH_MARK - 5, number, strlen(number));
	    }
	    if (i % PIX_PER_INCH == 0) {
		XDrawLine(tool_d, p, tr_gc, ZOOMX(i), TOPRULER_HT - 1, ZOOMX(i),
			  TOPRULER_HT - INCH_MARK - 1);
	    } else if (i % HINCH == 0)
		XDrawLine(tool_d, p, tr_gc, ZOOMX(i), TOPRULER_HT - 1, ZOOMX(i),
			  TOPRULER_HT - HALF_MARK - 1);
	    else if (i % QINCH == 0 && display_zoomscale >= 0.2)
		XDrawLine(tool_d, p, tr_gc, ZOOMX(i), TOPRULER_HT - 1, ZOOMX(i),
			  TOPRULER_HT - QUARTER_MARK - 1);
	    else if (i % SINCH == 0 && display_zoomscale >= 0.4)
		XDrawLine(tool_d, p, tr_gc, ZOOMX(i), TOPRULER_HT - 1, ZOOMX(i),
			  TOPRULER_HT - SIXTEENTH_MARK - 1);
	}
    } else {
	X0 -= (X0 % TWOMM);
	for (i = X0; i <= X0+round(TOPRULER_WD/zoomscale); i += ONEMM) {
	    if ((int)(i/skip) % PIX_PER_CM == 0) {
		if (1.0*i/PIX_PER_CM == (int)(i/PIX_PER_CM))
		    sprintf(number, "%-d", (int)(i / PIX_PER_CM));
		else
		    sprintf(number, "%-.1f", (float)(1.0 * i / PIX_PER_CM));
		XDrawString(tool_d, p, tr_gc, ZOOMX(i) - strlen(number)*clen/2,
			TOPRULER_HT - INCH_MARK - 5, number, strlen(number));
	    }
	    if (i % PIX_PER_CM == 0)
		XDrawLine(tool_d, p, tr_gc, ZOOMX(i), TOPRULER_HT - 1, ZOOMX(i),
			  TOPRULER_HT - INCH_MARK - 1);
	    else if (i % TWOMM == 0 && display_zoomscale >= 0.3)
		XDrawLine(tool_d, p, tr_gc, ZOOMX(i), TOPRULER_HT - 1, ZOOMX(i),
			  TOPRULER_HT - QUARTER_MARK - 1);
	}
    }
    /* change the pixmap ID to fool the intrinsics to actually set the pixmap */
    FirstArg(XtNbackgroundPixmap, 0);
    SetValues(topruler_sw);
    FirstArg(XtNbackgroundPixmap, p);
    SetValues(topruler_sw);
}

/************************* SIDERULER ************************/

XtActionsRec	sideruler_actions[] =
{
    {"EventSideRuler", (XtActionProc) sideruler_selected},
    {"ExposeSideRuler", (XtActionProc) sideruler_exposed},
    {"EnterSideRuler", (XtActionProc) draw_mousefun_sideruler},
    {"LeaveSideRuler", (XtActionProc) clear_mousefun},
};

static String	sideruler_translations =
"Any<BtnDown>:EventSideRuler()\n\
    Any<BtnUp>:EventSideRuler()\n\
    <Btn2Motion>:EventSideRuler()\n\
    Meta <Btn3Motion>:EventSideRuler()\n\
    <EnterWindow>:EnterSideRuler()\n\
    <LeaveWindow>:LeaveSideRuler()\n\
    <KeyPress>:EnterSideRuler()\n\
    <KeyRelease>:EnterSideRuler()\n\
    <Expose>:ExposeSideRuler()\n";

static
sideruler_selected(tool, event, params, nparams)
    Widget	    tool;
    XButtonEvent   *event;
    String	   *params;
    Cardinal	   *nparams;
{
    XButtonEvent   *be = (XButtonEvent *) event;

    switch (event->type) {
    case ButtonPress:
	if (be->button == Button3 && be->state & Mod1Mask) {
	    be->button = Button2;
	}
	switch (be->button) {
	case Button1:
	    XDefineCursor(tool_d, sideruler_win, u_arrow_cursor);
	    break;
	case Button2:
	    XDefineCursor(tool_d, sideruler_win, bull_cursor);
	    orig_zoomoff = zoomyoff;
	    last_drag_y = event->y;
	    break;
	case Button3:
	    XDefineCursor(tool_d, sideruler_win, d_arrow_cursor);
	    break;
	}
	break;
    case ButtonRelease:
	if (be->button == Button3 && be->state & Mod1Mask) {
	    be->button = Button2;
	}
	switch (be->button) {
	case Button1:
	    pan_up(event->state&ShiftMask);
	    break;
	case Button2:
	    if (orig_zoomoff != zoomyoff)
		setup_grid(cur_gridmode);
	    break;
	case Button3:
	    pan_down(event->state&ShiftMask);
	    break;
	}
	XDefineCursor(tool_d, sideruler_win, ud_arrow_cursor);
	break;
    case MotionNotify:
	if (event->y != last_drag_y)
	    if ((zoomyoff != 0) || (event->y < last_drag_y)) {
		zoomyoff -= ((event->y - last_drag_y)/zoomscale*
					(event->state&ShiftMask?5.0:1.0));
		if (zoomyoff < 0)
		    zoomyoff = 0;
		reset_sideruler();
		redisplay_sideruler();
	    }
	last_drag_y = event->y;
	break;
    }
}

static
sideruler_exposed(tool, event, params, nparams)
    Widget	    tool;
    XButtonEvent   *event;
    String	   *params;
    Cardinal	   *nparams;
{
    if (((XExposeEvent *) event)->count > 0)
	return;
    redisplay_sideruler();
}

int
init_sideruler(tool)
    Widget	    tool;
{
    FirstArg(XtNwidth, SIDERULER_WD);
    NextArg(XtNheight, SIDERULER_HT);
    NextArg(XtNlabel, "");
    NextArg(XtNfromHoriz, canvas_sw);
    NextArg(XtNhorizDistance, -INTERNAL_BW);
    NextArg(XtNfromVert, topruler_sw);
    NextArg(XtNvertDistance, -INTERNAL_BW);
    NextArg(XtNresizable, False);
    NextArg(XtNtop, XtChainTop);
    NextArg(XtNbottom, XtChainTop);
    NextArg(XtNleft, XtChainLeft);
    NextArg(XtNright, XtChainLeft);
    NextArg(XtNborderWidth, INTERNAL_BW);

    sideruler_sw = XtCreateWidget("sideruler", labelWidgetClass, tool,
				  Args, ArgCount);

    XtAppAddActions(tool_app, sideruler_actions, XtNumber(sideruler_actions));
    XtOverrideTranslations(sideruler_sw,
			   XtParseTranslationTable(sideruler_translations));
    return (1);
}

redisplay_sideruler()
{
    XClearWindow(tool_d, sideruler_win);
}

setup_sideruler()
{
    unsigned long   bg, fg;
    XGCValues	    gcv;
    unsigned long   gcmask;
    PIX_FONT	    font;

    sideruler_win = XtWindow(sideruler_sw);
    gcmask = GCFunction | GCForeground | GCBackground | GCFont;

    /* set up the GCs */
    FirstArg(XtNbackground, &bg);
    NextArg(XtNforeground, &fg);
    NextArg(XtNfont, &font);
    GetValues(sideruler_sw);

    gcv.font = font->fid;
    gcv.foreground = bg;
    gcv.background = bg;
    gcv.function = GXcopy;
    sr_erase_gc = XCreateGC(tool_d, sideruler_win, gcmask, &gcv);

    gcv.foreground = fg;
    sr_gc = XCreateGC(tool_d, sideruler_win, gcmask, &gcv);
    /*
     * The arrows will be XORed into the rulers. We want the foreground color
     * in the arrow to result in the foreground or background color in the
     * display. so if the source pixel is fg^bg, it produces fg when XOR'ed
     * with bg, and bg when XOR'ed with bg. If the source pixel is zero, it
     * produces fg when XOR'ed with fg, and bg when XOR'ed with bg.
     */

    /* make pixmaps for side ruler arrow */
    if (appres.RHS_PANEL) {
	sidearrow_pm = XCreatePixmapFromBitmapData(tool_d, sideruler_win, 
				srlm_pr.bits, srlm_pr.width, srlm_pr.height, 
				fg^bg, (unsigned long) 0, tool_dpth);
    } else {
	sidearrow_pm = XCreatePixmapFromBitmapData(tool_d, sideruler_win, 
				srrm_pr.bits, srrm_pr.width, srrm_pr.height, 
				fg^bg, (unsigned long) 0, tool_dpth);
    }

    /* now make the real xor gc */
    gcv.background = bg;
    gcv.function = GXxor;
    sr_xor_gc = XCreateGC(tool_d, sideruler_win, gcmask, &gcv);

    XDefineCursor(tool_d, sideruler_win, ud_arrow_cursor);

    sideruler_pm = XCreatePixmap(tool_d, sideruler_win,
				 SIDERULER_WD, SIDERULER_HT, tool_dpth);

    reset_sideruler();
}

resize_sideruler()
{
    XFreePixmap(tool_d, sideruler_pm);
    sideruler_pm = XCreatePixmap(tool_d, sideruler_win,
				 SIDERULER_WD, SIDERULER_HT, tool_dpth);
    reset_sideruler();
}

reset_sideruler()
{
    register int    i;
    register Pixmap p = sideruler_pm;
    char	    number[6];
    int		    Y0;
    float	    skip;

    /* side ruler, adjustments for digits are kludges based on 6x13 char */
    XFillRectangle(tool_d, p, sr_erase_gc, 0, 0, SIDERULER_WD,
		   (int) (SIDERULER_HT));

    skip = 1;
    if (display_zoomscale < 0.2 && !appres.INCHES)
	skip = 10;
    else if ((display_zoomscale < 0.2 && appres.INCHES) ||
	     (display_zoomscale < 0.3 && !appres.INCHES))
	        skip = 4;
    else if ((display_zoomscale < 0.3 && appres.INCHES) ||
	     (display_zoomscale < 0.7 && !appres.INCHES))
		skip = 2;
    else if (display_zoomscale >= 2.0 && appres.INCHES)
		skip = 0.5;

    Y0 = BACKY(0);
    if (appres.INCHES) {
	Y0 -= (Y0 % SINCH);
	if (appres.RHS_PANEL) {
	    for (i = Y0; i <= Y0+round(SIDERULER_HT/zoomscale); i += SINCH) {
		if (i % PIX_PER_INCH == 0) {
		    XDrawLine(tool_d, p, sr_gc, SIDERULER_WD - INCH_MARK,
			      ZOOMY(i), SIDERULER_WD, ZOOMY(i));
		    if ((int)(i/skip) % PIX_PER_INCH == 0) {
			if (1.0*i/PIX_PER_INCH == (int)(i / PIX_PER_INCH))
			    sprintf(number, "%d", (int)(i / PIX_PER_INCH));
			else
			    sprintf(number, "%.1f", (float)(1.0 * i / PIX_PER_INCH));
			XDrawString(tool_d, p, sr_gc,
				SIDERULER_WD - INCH_MARK - 22, ZOOMY(i) + 3,
				number, strlen(number));
		    }
		} else if (i % HINCH == 0)
		    XDrawLine(tool_d, p, sr_gc,
			      SIDERULER_WD - HALF_MARK, ZOOMY(i),
			      SIDERULER_WD, ZOOMY(i));
		else if (i % QINCH == 0 && display_zoomscale >= 0.2)
		    XDrawLine(tool_d, p, sr_gc,
			      SIDERULER_WD - QUARTER_MARK, ZOOMY(i),
			      SIDERULER_WD, ZOOMY(i));
		else if (i % SINCH == 0 && display_zoomscale >= 0.4)
		    XDrawLine(tool_d, p, sr_gc,
			      SIDERULER_WD - SIXTEENTH_MARK, ZOOMY(i),
			      SIDERULER_WD, ZOOMY(i));
	    }
	} else {
	    for (i = Y0; i <= Y0+round(SIDERULER_HT/zoomscale); i += SINCH) {
		if (i % PIX_PER_INCH == 0) {
		    XDrawLine(tool_d, p, sr_gc, 0, ZOOMY(i),
			      INCH_MARK - 1, ZOOMY(i));
		    if ((int)(i/skip) % PIX_PER_INCH == 0) {
			if (1.0*i/PIX_PER_INCH == (int)(i / PIX_PER_INCH))
			    sprintf(number, "%d", (int)(i / PIX_PER_INCH));
			else
			    sprintf(number, "%.1f", (float)(1.0 * i / PIX_PER_INCH));
		        XDrawString(tool_d, p, sr_gc, INCH_MARK + 3,
				ZOOMY(i) + 3, number, strlen(number));
		    }
		} else if (i % HINCH == 0)
		    XDrawLine(tool_d, p, sr_gc, 0, ZOOMY(i),
			      HALF_MARK - 1, ZOOMY(i));
		else if (i % QINCH == 0 && display_zoomscale >= 0.2)
		    XDrawLine(tool_d, p, sr_gc, 0, ZOOMY(i),
			      QUARTER_MARK - 1, ZOOMY(i));
		else if (i % SINCH == 0 && display_zoomscale >= 0.4)
		    XDrawLine(tool_d, p, sr_gc, 0, ZOOMY(i),
			      SIXTEENTH_MARK - 1, ZOOMY(i));
	    }
	}
    } else {
	Y0 -= (Y0 % TWOMM);
	if (appres.RHS_PANEL) {
	    for (i = Y0; i <= Y0+round(SIDERULER_HT/zoomscale); i++) {
		if (i % PIX_PER_CM == 0) {
		    XDrawLine(tool_d, p, sr_gc, SIDERULER_WD - INCH_MARK,
			      ZOOMY(i), SIDERULER_WD, ZOOMY(i));
		    if ((int)(i/skip) % PIX_PER_CM == 0) {
			if (1.0*i/PIX_PER_CM == (int)(i / PIX_PER_CM))
			    sprintf(number, "%d", (int)(i / PIX_PER_CM));
			else
			    sprintf(number, "%.1f", (float)(1.0 * i / PIX_PER_CM));
			XDrawString(tool_d, p, sr_gc,
				SIDERULER_WD - INCH_MARK - 14, ZOOMY(i) + 3,
				number, strlen(number));
		    }
		} else if (i % TWOMM == 0 && display_zoomscale >= 0.3)
		    XDrawLine(tool_d, p, sr_gc,
			      SIDERULER_WD - QUARTER_MARK, ZOOMY(i),
			      SIDERULER_WD, ZOOMY(i));
	    }
	} else {
	    for (i = Y0; i <= Y0+round(SIDERULER_HT/zoomscale); i++) {
		if (i % PIX_PER_CM == 0) {
		    XDrawLine(tool_d, p, sr_gc, 0, ZOOMY(i),
			      INCH_MARK - 1, ZOOMY(i));
		    if ((int)(i/skip) % PIX_PER_CM == 0) {
			if (1.0*i/PIX_PER_CM == (int)(i / PIX_PER_CM))
			    sprintf(number, "%d", (int)(i / PIX_PER_CM));
			else
			    sprintf(number, "%.1f", (float)(1.0 * i / PIX_PER_CM));
			XDrawString(tool_d, p, sr_gc, INCH_MARK + 3,
				ZOOMY(i) + 3, number, strlen(number));
		    }
		} else if (i % TWOMM == 0 && display_zoomscale >= 0.3)
		    XDrawLine(tool_d, p, sr_gc, 0, ZOOMY(i),
			      QUARTER_MARK - 1, ZOOMY(i));
	    }
	}
    }
    /* change the pixmap ID to fool the intrinsics to actually set the pixmap */
    FirstArg(XtNbackgroundPixmap, 0);
    SetValues(sideruler_sw);
    FirstArg(XtNbackgroundPixmap, p);
    SetValues(sideruler_sw);
}

erase_siderulermark()
{
    if (appres.RHS_PANEL)
	XClearArea(tool_d, sideruler_win,
		   SIDERULER_WD + srloffx, ZOOMY(lasty) + srloffy,
		   srlm_pr.width, srlm_pr.height, False);
    else
	XClearArea(tool_d, sideruler_win,
		   srroffx, ZOOMY(lasty) + srroffy,
		   srlm_pr.width, srlm_pr.height, False);
}

set_siderulermark(y)
    int		    y;
{
    if (appres.RHS_PANEL) {
	/*
	 * Because the ruler uses a background pixmap, we can win here by
	 * using XClearArea to erase the old thing.
	 */
	XClearArea(tool_d, sideruler_win,
		   SIDERULER_WD + srloffx, ZOOMY(lasty) + srloffy,
		   srlm_pr.width, srlm_pr.height, False);
	XCopyArea(tool_d, sidearrow_pm, sideruler_win,
		  sr_xor_gc, 0, 0, srlm_pr.width,
		  srlm_pr.height, SIDERULER_WD + srloffx, ZOOMY(y) + srloffy);
    } else {
	/*
	 * Because the ruler uses a background pixmap, we can win here by
	 * using XClearArea to erase the old thing.
	 */
	XClearArea(tool_d, sideruler_win,
		   srroffx, ZOOMY(lasty) + srroffy,
		   srlm_pr.width, srlm_pr.height, False);
	XCopyArea(tool_d, sidearrow_pm, sideruler_win,
		  sr_xor_gc, 0, 0, srrm_pr.width,
		  srrm_pr.height, srroffx, ZOOMY(y) + srroffy);
    }
    lasty = y;
}
