/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1991 by Brian V. Smith
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
#include "u_fonts.h"		/* printer font names */
#include "w_setup.h"
#include "w_util.h"

/********************  global variables	 ***************************/

extern char    *psfont_menu_bits[];
extern char    *latexfont_menu_bits[];
extern Pixmap	psfont_menu_bitmaps[];
extern Pixmap	latexfont_menu_bitmaps[];
extern Atom	wm_delete_window;
extern struct _fstruct ps_fontinfo[];	  /* PostScript/OpenWindows font names */
extern struct _fstruct latex_fontinfo[];  /* LaTeX font names */

/* LOCAL VARIABLES */

static int     *font_ps_sel;	/* ptr to store selected ps font in */
static int     *font_latex_sel; /* ptr to store selected latex font */
static int     *flag_sel;	/* pointer to store ps/latex flag */
static Widget	font_widget;	/* widget adr to store font image in */
static int	(*font_setimage) ();

/********************  local variables	***************************/

static MenuItemRec ps_fontmenu_items[NUM_FONTS + 1];
static MenuItemRec latex_fontmenu_items[NUM_LATEX_FONTS];

static void	fontpane_select();
static void	fontpane_cancel();
static void	fontpane_swap();

static XtCallbackRec pane_callbacks[] =
{
    {fontpane_select, NULL},
    {NULL, NULL},
};

static String	fontpane_translations =
	"<Message>WM_PROTOCOLS: FontPaneCancel()\n";
static XtActionsRec	fontpane_actions[] =
{
    {"FontPaneCancel", (XtActionProc) fontpane_cancel},
};

static Widget	ps_fontpanes, ps_buttons;
static Widget	latex_fontpanes, latex_buttons;
static Widget	ps_fontpane[NUM_FONTS+1];
static Widget	latex_fontpane[NUM_LATEX_FONTS];
static Boolean	first_fontmenu;

init_fontmenu(tool)
    Widget	    tool;
{
    Widget	    tmp_but;
    register int    i;
    register MenuItemRec *mi;
    XtTranslations  pane_actions;

    DeclareArgs(8);

    first_fontmenu = True;

    FirstArg(XtNborderWidth, POPUP_BW);
    NextArg(XtNmappedWhenManaged, False);
    NextArg(XtNtitle, "Xfig: Font menu");

    ps_fontmenu = XtCreatePopupShell("ps_font_menu",
				     transientShellWidgetClass, tool,
				     Args, ArgCount);
    XtOverrideTranslations(ps_fontmenu,
			XtParseTranslationTable(fontpane_translations));
    latex_fontmenu = XtCreatePopupShell("latex_font_menu",
					transientShellWidgetClass, tool,
					Args, ArgCount);
    XtOverrideTranslations(latex_fontmenu,
			XtParseTranslationTable(fontpane_translations));
    XtAppAddActions(tool_app, fontpane_actions, XtNumber(fontpane_actions));

    FirstArg(XtNvSpace, -INTERNAL_BW);
    NextArg (XtNhSpace, -INTERNAL_BW);
    NextArg (XtNwidth, PS_FONTPANE_WD*2 + INTERNAL_BW*4);	/* two across */
    NextArg (XtNhSpace, 0);

    ps_fontpanes = XtCreateManagedWidget("menu", boxWidgetClass,
					 ps_fontmenu, Args, ArgCount);

    FirstArg(XtNvSpace, -INTERNAL_BW);
    NextArg (XtNhSpace, -INTERNAL_BW);
    NextArg (XtNwidth, LATEX_FONTPANE_WD*2 + INTERNAL_BW*4);	/* two across */
    NextArg (XtNhSpace, 0);
    latex_fontpanes = XtCreateManagedWidget("menu", boxWidgetClass,
					    latex_fontmenu, Args, ArgCount);

    for (i = 0; i < NUM_FONTS + 1; i++) {
	ps_fontmenu_items[i].type = MENU_IMAGESTRING;		/* put the fontnames in
								 * menu */
	ps_fontmenu_items[i].label = ps_fontinfo[i].name;
	ps_fontmenu_items[i].info = (caddr_t) (i - 1);		/* index for font # */
    }

    for (i = 0; i < NUM_LATEX_FONTS; i++) {
	latex_fontmenu_items[i].type = MENU_IMAGESTRING;	/* put the fontnames in
								 * menu */
	latex_fontmenu_items[i].label = latex_fontinfo[i].name;
	latex_fontmenu_items[i].info = (caddr_t) i;		/* index for font # */
    }

    FirstArg(XtNwidth, PS_FONTPANE_WD*2);
    NextArg(XtNdefaultDistance, INTERNAL_BW);
    NextArg(XtNborderWidth, 0);
    ps_buttons = XtCreateManagedWidget("ps_buttons", formWidgetClass,
				       ps_fontpanes, Args, ArgCount);

    FirstArg(XtNwidth, LATEX_FONTPANE_WD*2);
    NextArg(XtNdefaultDistance, INTERNAL_BW);
    NextArg(XtNborderWidth, 0);
    latex_buttons = XtCreateManagedWidget("latex_buttons", formWidgetClass,
					  latex_fontpanes, Args, ArgCount);

    FirstArg(XtNlabel, "Cancel");
    NextArg(XtNwidth, PS_FONTPANE_WD);
    NextArg(XtNheight, PS_FONTPANE_HT+4);
    NextArg(XtNborderWidth, 0);
    tmp_but = XtCreateManagedWidget("cancel", commandWidgetClass,
				    ps_buttons, Args, ArgCount);
    XtAddEventHandler(tmp_but, ButtonReleaseMask, (Boolean) 0,
		      fontpane_cancel, (XtPointer) NULL);

    FirstArg(XtNlabel, "Use LaTex Fonts");
    NextArg(XtNfromHoriz, tmp_but);
    NextArg(XtNwidth, PS_FONTPANE_WD);
    NextArg(XtNheight, PS_FONTPANE_HT+4);
    NextArg(XtNborderWidth, 0);
    tmp_but = XtCreateManagedWidget("use_latex_fonts", commandWidgetClass,
				    ps_buttons, Args, ArgCount);
    XtAddEventHandler(tmp_but, ButtonReleaseMask, (Boolean) 0,
		      fontpane_swap, (XtPointer) NULL);

    FirstArg(XtNlabel, "Cancel");
    NextArg(XtNwidth, LATEX_FONTPANE_WD);
    NextArg(XtNheight, LATEX_FONTPANE_HT+4);
    NextArg(XtNborderWidth, 0);
    tmp_but = XtCreateManagedWidget("cancel", commandWidgetClass,
				    latex_buttons, Args, ArgCount);
    XtAddEventHandler(tmp_but, ButtonReleaseMask, (Boolean) 0,
		      fontpane_cancel, (XtPointer) NULL);

    FirstArg(XtNlabel, "Use PostScript Fonts");
    NextArg(XtNfromHoriz, tmp_but);
    NextArg(XtNwidth, LATEX_FONTPANE_WD);
    NextArg(XtNheight, LATEX_FONTPANE_HT+4);
    NextArg(XtNborderWidth, 0);
    tmp_but = XtCreateManagedWidget("use_postscript_fonts", commandWidgetClass,
				    latex_buttons, Args, ArgCount);
    XtAddEventHandler(tmp_but, ButtonReleaseMask, (Boolean) 0,
		      fontpane_swap, (XtPointer) NULL);

    pane_actions = XtParseTranslationTable("<EnterWindow>:set()\n\
		<Btn1Up>:notify()unset()\n");

    FirstArg(XtNwidth, PS_FONTPANE_WD);
    NextArg(XtNheight, PS_FONTPANE_HT+4);
    NextArg(XtNcallback, pane_callbacks);
    NextArg(XtNbitmap, NULL);
    NextArg(XtNinternalWidth, 0);	/* space between pixmap and edge */
    NextArg(XtNinternalHeight, 0);
    NextArg(XtNborderWidth, INTERNAL_BW);
    NextArg(XtNresize, False);	/* don't allow resize */

    for (i = 0; i < NUM_FONTS + 1; ++i) {
	mi = &ps_fontmenu_items[i];
	pane_callbacks[0].closure = (caddr_t) mi;
	ps_fontpane[i] = XtCreateManagedWidget("pane", commandWidgetClass,
					       ps_fontpanes, Args, ArgCount);
	XtOverrideTranslations(ps_fontpane[i], pane_actions);
    }

    FirstArg(XtNwidth, LATEX_FONTPANE_WD);
    NextArg(XtNheight, LATEX_FONTPANE_HT+4);
    NextArg(XtNcallback, pane_callbacks);
    NextArg(XtNbitmap, NULL);
    NextArg(XtNinternalWidth, 0);	/* space between pixmap and edge */
    NextArg(XtNinternalHeight, 0);
    NextArg(XtNborderWidth, INTERNAL_BW);
    NextArg(XtNresize, False);	/* don't allow resize */

    for (i = 0; i < NUM_LATEX_FONTS; ++i) {
	mi = &latex_fontmenu_items[i];
	pane_callbacks[0].closure = (caddr_t) mi;
	latex_fontpane[i] = XtCreateManagedWidget("pane", commandWidgetClass,
					   latex_fontpanes, Args, ArgCount);
	XtOverrideTranslations(latex_fontpane[i], pane_actions);
    }

    return (1);
}

/* create the bitmaps for the font menu */

setup_fontmenu()
{
    register int    i;

    DeclareArgs(2);

    Pixel	    bg, fg;

    /* get the foreground/background of the widget */
    FirstArg(XtNforeground, &fg);
    NextArg(XtNbackground, &bg);
    GetValues(ps_fontpane[0]);

    /* Create the bitmaps */

    for (i = 0; i < NUM_FONTS + 1; i++)
	psfont_menu_bitmaps[i] = XCreatePixmapFromBitmapData(tool_d,
				   XtWindow(ind_panel), (char *) psfont_menu_bits[i],
				     PS_FONTPANE_WD, PS_FONTPANE_HT, fg, bg,
				      tool_dpth);

    for (i = 0; i < NUM_LATEX_FONTS; i++)
	latexfont_menu_bitmaps[i] = XCreatePixmapFromBitmapData(tool_d,
				     XtWindow(ind_panel), (char *) latexfont_menu_bits[i],
				      LATEX_FONTPANE_WD, LATEX_FONTPANE_HT, fg, bg,
				       tool_dpth);

    /* Store the bitmaps in the menu panes */
    for (i = 0; i < NUM_FONTS + 1; i++) {
	FirstArg(XtNbitmap, psfont_menu_bitmaps[i]);
	SetValues(ps_fontpane[i]);
    }
    for (i = 0; i < NUM_LATEX_FONTS; i++) {
	FirstArg(XtNbitmap, latexfont_menu_bitmaps[i]);
	SetValues(latex_fontpane[i]);
    }

    FirstArg(XtNbackground, black_color.pixel);
    SetValues(ps_buttons);
    SetValues(latex_buttons);

    XtRealizeWidget(ps_fontmenu);
    XtRealizeWidget(latex_fontmenu);
    /* at this point the windows are realized but not drawn */
    XDefineCursor(tool_d, XtWindow(ps_fontpanes), arrow_cursor);
    XDefineCursor(tool_d, XtWindow(latex_fontpanes), arrow_cursor);
}

void
fontpane_popup(psfont_adr, latexfont_adr, psflag_adr, showfont_fn, show_widget)
    int		   *psfont_adr, *latexfont_adr, *psflag_adr;
    int		    (*showfont_fn) ();
    Widget	    show_widget;

{
    DeclareArgs(2);
    Position	    xposn, yposn;
    Widget	    widg;

    font_ps_sel = psfont_adr;
    font_latex_sel = latexfont_adr;
    flag_sel = psflag_adr;
    font_setimage = showfont_fn;
    font_widget = show_widget;
    if (first_fontmenu) {
	first_fontmenu = False;	/* don't reposition it if user has already popped it */
	XtTranslateCoords(tool, CANVAS_WD/4, CANVAS_HT/4, &xposn, &yposn);
	FirstArg(XtNx, xposn);	/* position about 1/4 from upper-left corner of canvas */
	NextArg(XtNy, yposn);
	SetValues(ps_fontmenu);
	SetValues(latex_fontmenu);
    }
    widg = *flag_sel ? ps_fontmenu : latex_fontmenu;
    XtPopup(widg, XtGrabExclusive);
    /* insure that the most recent colormap is installed */
    set_cmap(XtWindow(widg));
    XSetWMProtocols(XtDisplay(widg), XtWindow(widg), &wm_delete_window, 1);
}

static void
fontpane_select(w, closure, call_data)
    Widget    w;
    XtPointer closure;
    XtPointer call_data;
{
    MenuItemRec	   *mi = (MenuItemRec *) closure;
    char	   *font_name = mi->label;

    if (*flag_sel)
	*font_ps_sel = (int) mi->info;	/* set ps font to one selected */
    else
	*font_latex_sel = (int) mi->info;	/* set latex font to one
						 * selected */
    put_msg("Font: %s", font_name);
    /* put image of font in indicator window */
    (*font_setimage) (font_widget);
    XtPopdown(*flag_sel ? ps_fontmenu : latex_fontmenu);
}

static void
fontpane_cancel()
{
    XtPopdown(*flag_sel ? ps_fontmenu : latex_fontmenu);
}

static void
fontpane_swap()
{
    Widget widg;

    XtPopdown(*flag_sel ? ps_fontmenu : latex_fontmenu);
    *flag_sel = 1 - *flag_sel;
    /* put image of font in indicator window */
    (*font_setimage) (font_widget);
    widg = *flag_sel ? ps_fontmenu : latex_fontmenu;
    XtPopup(widg, XtGrabExclusive);
    /* insure that the most recent colormap is installed */
    set_cmap(XtWindow(widg));
    XSetWMProtocols(XtDisplay(widg), XtWindow(widg), &wm_delete_window, 1);
}
