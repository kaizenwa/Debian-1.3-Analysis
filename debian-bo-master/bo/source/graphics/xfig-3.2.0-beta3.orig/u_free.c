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
#include "resources.h"
#include "object.h"
#include "u_fonts.h"

extern struct _xfstruct x_fontinfo[];	/* X11 fontnames */
extern PIX_FONT	bold_font;
extern PIX_FONT	roman_font;
extern PIX_FONT	button_font;
extern PIX_FONT	canvas_font;

free_arc(list)
    F_arc	  **list;
{
    F_arc	   *a, *arc;

    for (a = *list; a != NULL;) {
	arc = a;
	a = a->next;
	if (arc->for_arrow)
	    free((char *) arc->for_arrow);
	if (arc->back_arrow)
	    free((char *) arc->back_arrow);
	free((char *) arc);
    }
    *list = NULL;
}

free_compound(list)
    F_compound	  **list;
{
    F_compound	   *c, *compound;

    for (c = *list; c != NULL;) {
	compound = c;
	c = c->next;
	free_arc(&compound->arcs);
	free_compound(&compound->compounds);
	free_ellipse(&compound->ellipses);
	free_line(&compound->lines);
	free_spline(&compound->splines);
	free_text(&compound->texts);
	free((char *) compound);
    }
    *list = NULL;
}

free_ellipse(list)
    F_ellipse	  **list;
{
    F_ellipse	   *e, *ellipse;

    for (e = *list; e != NULL;) {
	ellipse = e;
	e = e->next;
	free((char *) ellipse);
    }
    *list = NULL;
}

free_line(list)
    F_line	  **list;
{
    F_line	   *l, *line;

    for (l = *list; l != NULL;) {
	line = l;
	l = l->next;
	free_linestorage(line);
    }
    *list = NULL;
}

free_text(list)
    F_text	  **list;
{
    F_text	   *t, *text;

    for (t = *list; t != NULL;) {
	text = t;
	t = t->next;
	free(text->cstring);
	free((char *) text);
    }
    *list = NULL;
}

free_spline(list)
    F_spline	  **list;
{
    F_spline	   *s, *spline;

    for (s = *list; s != NULL;) {
	spline = s;
	s = s->next;
	free_splinestorage(spline);
    }
    *list = NULL;
}

free_splinestorage(s)
    F_spline	   *s;
{
    F_sfactor	   *a, *b;

    free_points(s->points);
    for (a = s->sfactors; a != NULL; a = b) {
	b = a->next;
	free((char *) a);
    }
    if (s->for_arrow)
	free((char *) s->for_arrow);
    if (s->back_arrow)
	free((char *) s->back_arrow);
    free((char *) s);
}

free_linestorage(l)
    F_line	   *l;
{
    free_points(l->points);
    if (l->for_arrow)
	free((char *) l->for_arrow);
    if (l->back_arrow)
	free((char *) l->back_arrow);
    if (l->pic) {
	if (l->pic->bitmap)
	    free((char *) l->pic->bitmap);
	if (l->pic->pixmap != 0)
	    XFreePixmap(tool_d, l->pic->pixmap);
	free((char *) l->pic);
    }
    free((char *) l);
}

free_points(first_point)
    F_point	   *first_point;
{
    F_point	   *p, *q;

    for (p = first_point; p != NULL; p = q) {
	q = p->next;
	free((char *) p);
    }
}

free_linkinfo(list)
    F_linkinfo	  **list;
{
    F_linkinfo	   *l, *link;

    for (l = *list; l != NULL;) {
	link = l;
	l = l->next;
	free((char *) link);
    }
    *list = NULL;
}

/* free up all the GC's before leaving xfig */

free_GCs()
	{
#ifdef USE_XPM
	/* free any colors from the xfig xpm icon (if used) */
	if (xfig_icon_attr.npixels > 0) {
	    XFreeColors(tool_d, tool_cm, 
			xfig_icon_attr.pixels, xfig_icon_attr.npixels,
			(unsigned long) 0);
	}
#endif /* USE_XPM */
	XFreeGC(tool_d, gc);
	XFreeGC(tool_d, button_gc);
	XFreeGC(tool_d, fill_color_gc);
	XFreeGC(tool_d, pen_color_gc);
	XFreeGC(tool_d, ind_button_gc);
	XFreeGC(tool_d, ind_blank_gc);
	XFreeGC(tool_d, blank_gc);
	XFreeGC(tool_d, mouse_blank_gc);
	XFreeGC(tool_d, mouse_button_gc);
	XFreeGC(tool_d, tr_gc);
	XFreeGC(tool_d, tr_erase_gc);
	XFreeGC(tool_d, tr_xor_gc);
	XFreeGC(tool_d, sr_gc);
	XFreeGC(tool_d, sr_erase_gc);
}
/* free up all the Fonts before leaving xfig */

free_Fonts()
{
  int i;
  struct xfont   *nf;


  for (i=0; i<NUM_FONTS; i++) {
    for (nf = x_fontinfo[i].xfontlist; nf != NULL;) {
      XUnloadFont(tool_d, nf->fid);
      if (nf->fstruct != NULL)
	  XFreeFont(tool_d, nf->fstruct); 
      nf = nf->next;
    } 
  }
  if (bold_font!=NULL) {
    XFreeFont(tool_d, bold_font); 
  }
  if (roman_font!=NULL) {
    XFreeFont(tool_d, roman_font); 
  };
  if (button_font!=NULL) {
    XFreeFont(tool_d, button_font); 
  };
	XFreeGC(tool_d, sr_xor_gc);

	for (i=0; i<NUMOPS; i++) {
		XFreeGC(tool_d, gccache[i]);
	}
	for (i=0; i<NUMFILLPATS; i++) {
		XFreeGC(tool_d, fill_gc[i]);
	}
}
