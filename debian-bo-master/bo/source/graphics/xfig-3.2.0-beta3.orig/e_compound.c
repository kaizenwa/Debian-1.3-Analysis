/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Enter Compound written by Bill Taylor (bill@mainstream.com) 1994
 * Parts Copyright (c) 1994 by Bill Taylor
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

/*
 * open_compound lets the user select a compound with the left button,
 * then replaces the current drawing with that compound alone so that the
 * user can edit the insides of that compound without taking it apart.
 * 
 * close_compound pops out one compound; close_all_compounds pops all the way out.
 *
 */

#include "fig.h"
#include "figx.h"
#include "resources.h"
#include "mode.h"
#include "object.h"
#include "u_search.h"
#include "w_canvas.h"
#include "w_setup.h"
#include "w_util.h"

Widget	close_compound_popup;
Boolean	close_popup_isup = False;

static void
init_open_compound(p, type, x, y, px, py, loc_tag)
    char	   *p;
    int		    type;
    int		    x, y;
    int		    px, py;
    int 	    loc_tag;
{
  F_compound *c;
  F_compound *d;

  if (type != O_COMPOUND)
    return;

  c = (F_compound *) p;
  mask_toggle_compoundmarker(c);

  c->parent = d = malloc(sizeof(F_compound));
  *d = objects;			/* Preserve the parent, it points to c */
  objects = *c;
  objects.GABPtr = c;		/* Where original compound came from */
  if (!close_popup_isup)
	popup_close_compound();
  redisplay_canvas();
}

void
open_compound()
{
  /* prepatory functions done for mode operations by sel_mode_but */
  update_markers((int)M_COMPOUND);

  set_mousefun("open compound", "", "", "", "", "");
  canvas_kbd_proc = null_proc;
  canvas_locmove_proc = null_proc;
  init_searchproc_left(init_open_compound);
  init_searchproc_middle(null_proc);
  canvas_leftbut_proc = object_search_left;
  canvas_middlebut_proc = null_proc;
  canvas_rightbut_proc = null_proc;
  set_cursor(pick15_cursor);
}

void
close_compound()
{
  F_compound *c;
  F_compound *d;		/* Destination */

  if (c = (F_compound *)objects.parent) {
    objects.parent = NULL;
    d = (F_compound *)objects.GABPtr;	/* Where this compound was */
    objects.GABPtr   = NULL;
    /* compute new bounding box if changed */
    compound_bound(&objects, &objects.nwcorner.x, &objects.nwcorner.y,
			&objects.secorner.x, &objects.secorner.y);
    *d = objects;		/* Put in any changes */
    objects = *c;		/* Restore compound above */
    /* user may have deleted all objects inside the compound */
    if (object_count(d)==0) {
	list_delete_compound(&objects.compounds, d);
    }
    free(c);
    /* popdown close panel if this is the last one */
    if ((F_compound *)objects.parent == NULL) {
	XtPopdown(close_compound_popup);
	XtDestroyWidget(close_compound_popup);
	close_popup_isup = False;
    }
    redisplay_canvas();
  }
}

void
close_all_compounds()
{
  F_compound *c;
  F_compound *d;		/* Destination */

  if (objects.parent) {
    while (c = (F_compound *)objects.parent) {
      objects.parent = NULL;
      d = (F_compound *)objects.GABPtr;	/* Where this compound was */
      objects.GABPtr   = NULL;
      /* compute new bounding box if changed */
      compound_bound(&objects, &objects.nwcorner.x, &objects.nwcorner.y,
			&objects.secorner.x, &objects.secorner.y);
      *d = objects;		/* Put in any changes */
      objects = *c;
      /* user may have deleted all objects inside the compound */
      if (object_count(d)==0) {
	list_delete_compound(&objects.compounds, d);
      }
      free(c);
    }
    /* popdown close panel */
    XtPopdown(close_compound_popup);
    XtDestroyWidget(close_compound_popup);
    close_popup_isup = False;
    redisplay_canvas();
  }
}

popup_close_compound()
{
    Widget	    close_compound_form;
    Widget	    close_compoundw, close_compound_allw;
    int		    xposn, yposn;
    Window	    win;
    XEvent	    event;
    extern Atom	    wm_delete_window;

    DeclareArgs(10);

    /* put the window in the upper-left corner of the canvas */
    XTranslateCoordinates(tool_d, canvas_win, XDefaultRootWindow(tool_d),
			  20, 20, &xposn, &yposn, &win);
    FirstArg(XtNallowShellResize, True);
    NextArg(XtNx, xposn);
    NextArg(XtNy, yposn);
    NextArg(XtNtitle, "Xfig: Close Compound");
    NextArg(XtNcolormap, tool_cm);
    close_compound_popup = XtCreatePopupShell("close_compound_popup", transientShellWidgetClass,
				     tool, Args, ArgCount);
    close_compound_form = XtCreateManagedWidget("close_compound_form", formWidgetClass,
				       close_compound_popup, (XtPointer) NULL, 0);

    FirstArg(XtNlabel, "Close This Compound")
    close_compoundw = XtCreateManagedWidget("close_compound", commandWidgetClass,
				      close_compound_form, Args, ArgCount);
    XtAddEventHandler(close_compoundw, ButtonReleaseMask, (Boolean) 0,
		      (XtEventHandler)close_compound, (XtPointer) NULL);

    FirstArg(XtNlabel, "Close All Compounds");
    NextArg(XtNfromHoriz, close_compoundw);
    close_compound_allw = XtCreateManagedWidget("close_all_compounds", commandWidgetClass,
    				 close_compound_form, Args, ArgCount);
    XtAddEventHandler(close_compound_allw, ButtonReleaseMask, (Boolean) 0,
			  (XtEventHandler)close_all_compounds, (XtPointer) NULL);

    XtPopup(close_compound_popup, XtGrabNone);
    /* insure that the most recent colormap is installed */
    set_cmap(XtWindow(close_compound_popup));
    (void) XSetWMProtocols(XtDisplay(close_compound_popup), XtWindow(close_compound_popup),
                           &wm_delete_window, 1);
    XDefineCursor(tool_d, XtWindow(close_compound_popup), arrow_cursor);
    close_popup_isup = True;
}
