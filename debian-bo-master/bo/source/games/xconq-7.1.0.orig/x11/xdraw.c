/* Lower-level drawing routines for the X11 interface to Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* The routines in this file are for drawing in maps, but only work
   at the Xlib level, and would be the same no matter what higher-level
   toolkit was in use. */

/* At the moment, there are three subwindows of a map that use raw Xlib;
   the unit info, the map view itself, and the list of sides. */

#include "conq.h"
extern void location_desc PARAMS ((char *buf, Side *side, Unit *unit, int u, int x, int y));
#include "xconq.h"

int tmpdrawlighting;

#define bords_to_draw(m) (numbordtypes > 0 && bwid[(m)->vp->power] > 0)

#define conns_to_draw(m) (numconntypes > 0 && cwid[(m)->vp->power] > 0)

Unit *x_find_unit_or_occ PARAMS ((Side *side, Map *map, Unit *unit,
				 int usx, int usy, int usw, int ush,
				 int sx, int sy));
Unit *x_find_unit_at PARAMS ((Side *side, Map *map, int x, int y,
			     int sx, int sy));

static void draw_feature_name PARAMS ((Side *side, Map *map, int fid));

static void draw_border_line_multiple PARAMS ((Side *side, Map *map,
					Window win, int sx, int sy,
					int bitmask, int power, int t));
static void draw_connection_line_multiple PARAMS ((Side *side, Window win,
					 int sx, int sy, int bitmask,
					 int power, int c));
static void draw_hex_polygon PARAMS ((Side *side, Map *map, Window win,
					GC gc, int sx, int sy,
					int power, int dogrid));
static void draw_area_background PARAMS ((Side *side, Map *map));
static int cell_drawing_info PARAMS ((Side *side, int x, int y, int power, int seall,
				     Pixmap *patp, long *colorp));
static void draw_terrain_row PARAMS ((Side *side, Map *map,
				     int x0, int y0, int len, int force));
static void draw_elevations PARAMS ((Side *side, Map *map,
				    int x0, int y0, int len));
static void draw_units PARAMS ((Side *side, Map *map, int x, int y));
static void draw_unit_and_occs PARAMS ((Side *side, Map *map, Unit *unit,
				       int sx, int sy, int sw, int sh, int drawoccs));
#if 0
static void draw_one_unit PARAMS ((Side *side, Map *map, Unit *unit));
#endif
static void draw_unit_name PARAMS ((Side *side, Map *map, Unit *unit,
				   int sx, int sy, int sw, int sh));
static void draw_people PARAMS ((Side *side, Map *map, int x, int y));
static void draw_borders PARAMS ((Side *side, Map *map, int x, int y, int b));
static void draw_connections PARAMS ((Side *side, Map *map, int x, int y,
				     int c));
static void draw_legend PARAMS ((Side *side, Map *map, int x, int y));
static void draw_country_border_line PARAMS ((Side *side, Window win,
					     int sx, int sy, int dir,
					     int power));
static void draw_legend_text PARAMS ((Side *side, Window win,
				     int sx, int sy, int power, char *str,
				     int color, int maskit));
static void draw_feature_boundary PARAMS ((Side *side, Map *map,
					  int x, int y, int fid));
static void draw_cursor_icon PARAMS ((Side *side, Window win,
				     int sx, int sy, int sw, int sh));
static void draw_info_text PARAMS ((Side *side, Map *map, int x, int y,
				   int len, char *buf));


/* Put the point x, y in the center of the map, or at least as close
   as possible. */

void
recenter(side, map, x, y)
Side *side;
Map *map;
int x, y;
{
    int oldsx = map->vp->sx, oldsy = map->vp->sy;

    set_view_focus(map->vp, x, y);
    x_center_on_focus(side, map);
    if (map->vp->sx != oldsx || map->vp->sy != oldsy) {
	draw_map(side, map);
    }
}

/* Ensure that given location is visible on the front map.  We
   (should) also flush the input because any input relating to a
   different screen is probably worthless. */

void
put_on_screen(side, map, x, y)
Side *side;
Map *map;
int x, y;
{

    /* Ugly hack to prevent extra boxes being drawn during init - don't ask!*/
    if (x == 0 && y == 0)
      return;
    if (!in_middle(side, map, x, y))
      recenter(side, map, x, y);
}

/* Decide whether given location is not too close to edge of screen.
   We do this because it's a pain to move units when half the adjacent
   places aren't even visible.  This routine effectively places a lower
   limit of 5x5 for the map window. (I think) */

int
in_middle(side, map, x, y)
Side *side;
Map *map;
int x, y;
{
    int sx, sy, insetx1, insety1, insetx2, insety2;

    xform(side, map, x, y, &sx, &sy);
    /* Adjust to be the center of the cell, more reasonable if large. */
    sx += map->vp->hw / 2;  sy += map->vp->hh / 2;
    insetx1 = min(map->vp->pxw / 4, 1 * map->vp->hw);
    insety1 = min(map->vp->pxh / 4, 1 * map->vp->hch);
    insetx2 = min(map->vp->pxw / 4, 2 * map->vp->hw);
    insety2 = min(map->vp->pxh / 4, 2 * map->vp->hch);
    if (sx < insetx2)
      return FALSE;
    if (sx > map->vp->pxw - insetx2)
      return FALSE;
    if (sy < (between(2, y, area.height-3) ? insety2 : insety1))
      return FALSE;
    if (sy > map->vp->pxh - (between(2, y, area.height-3) ? insety2 : insety1))
      return FALSE;
    return TRUE;
}

/* Transform map coordinates into screen coordinates, relative to the given
   side.  Allow for cylindricalness and number of pixels in a cell. */

int
xform(side, map, x, y, sxp, syp)
Side *side;
Map *map;
int x, y, *sxp, *syp;
{
    xform_cell(map->vp, x, y, sxp, syp);
    return TRUE;
}

int
x_xform_unit(side, map, unit, sxp, syp, swp, shp)
Side *side;
Map *map;
Unit *unit;
int *sxp, *syp, *swp, *shp;
{
    xform_unit(map->vp, unit, sxp, syp, swp, shp);
    return TRUE;
}

int
x_xform_unit_self(side, map, unit, sxp, syp, swp, shp)
Side *side;
Map *map;
Unit *unit;
int *sxp, *syp, *swp, *shp;
{
    xform_unit_self(map->vp, unit, sxp, syp, swp, shp);
    return TRUE;
}

int
x_xform_occupant(side, map, transport, unit, sx, sy, sw, sh, sxp, syp, swp, shp)
Side *side;
Map *map;
Unit *transport, *unit;
int sx, sy, sw, sh, *sxp, *syp, *swp, *shp;
{
    xform_occupant(map->vp, transport, unit, sx, sy, sw, sh, sxp, syp, swp, shp);
    return TRUE;
}

int
x_nearest_cell(side, map, sx, sy, xp, yp)
Side *side;
Map *map;
int sx, sy, *xp, *yp;
{
    return nearest_cell(map->vp, sx, sy, xp, yp);
}

/* Find the closest direction of the closest boundary. */

int
x_nearest_boundary(side, map, sx, sy, xp, yp, dirp)
Side *side;
Map *map;
int sx, sy, *xp, *yp, *dirp;
{
    return nearest_boundary(map->vp, sx, sy, xp, yp, dirp);
}

Unit *
x_find_unit_or_occ(side, map, unit, usx, usy, usw, ush, sx, sy)
Side *side;
Map *map;
Unit *unit;
int usx, usy, usw, ush, sx, sy;
{
    int usx1, usy1, usw1, ush1;
    Unit *occ, *rslt;

    /* See if the point might be over an occupant. */
    if (unit->occupant != NULL) {
	for_all_occupants(unit, occ) {
	    x_xform_unit(side, map, occ, &usx1, &usy1, &usw1, &ush1);
	    rslt =
	      x_find_unit_or_occ(side, map, occ, usx1, usy1, usw1, ush1, sx, sy);
	    if (rslt) {
		return rslt;
	    }
	}
    }
    /* Otherwise see if it could be the unit itself.  This has the effect of
       "giving" the transport everything in its box that is not in an occ. */
    x_xform_unit(side, map, unit, &usx1, &usy1, &usw1, &ush1);
    if (between(usx1, sx, usx1 + usw1) && between(usy1, sy, usy1 + ush1)) {
	return unit;
    }
    return NULL;
}

Unit *
x_find_unit_at(side, map, x, y, sx, sy)
Side *side;
Map *map;
int x, y, sx, sy;
{
    int usx, usy, usw, ush;
    Unit *unit, *rslt;
	
    for_all_stack(x, y, unit) {
	x_xform_unit(side, map, unit, &usx, &usy, &usw, &ush);
	rslt = x_find_unit_or_occ(side, map, unit, usx, usy, usw, ush, sx, sy);
	if (rslt)
	  return rslt;
    }
    return NULL;
}

int
x_nearest_unit(side, map, sx, sy, unitp)
Side *side;
Map *map;
int sx, sy;
Unit **unitp;
{
    int x, y;

    if (!x_nearest_cell(side, map, sx, sy, &x, &y)) {
	*unitp = NULL;
    } else if (map->vp->power > 4) {
	*unitp = x_find_unit_at(side, map, x, y, sx, sy);
    } else {
	*unitp = unit_at(x, y);
    }
    DGprintf("Pixel %d,%d -> unit %s\n", sx, sy, unit_desig(*unitp));
    return TRUE;
}

/* Draw all the maps that are currently up. */

void
draw_all_maps(side)
Side *side;
{
    Map *map;

    for_all_maps(side, map) {
	draw_map(side, map);
    }
}

/* Display a map and all of its paraphernalia. */

void
draw_map(side, map)
Side *side;
Map *map;
{
    /* Redraw only the panes that are managed manually. */
    draw_map_sides(side, map);
    draw_map_info(side, map);
    draw_map_view(side, map);
    flush_output(side);
}

/* Draw the background area for the map. */

static void
draw_area_background(side, map)
Side *side;
Map *map;
{
    int sx, sy, aw, ah, i;
    int llx, lly, lrx, lry, rx, ry, urx, ury, ulx, uly, lx, ly;
    XPoint points[7];
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->terrgc;

    XSetClipMask(dpy, gc, None);
    XSetFillStyle(dpy, gc, FillSolid);
    XSetForeground(dpy, gc, side->ui->fgcolor);
    XFillRectangle(dpy, map->viewwin, gc,
		   0, 0, map->vp->pxw, map->vp->pxh);
    if (1 /* grid color matches unseen color */) {
	XSetForeground(dpy, gc, side->ui->gridcolor);
    }
    XSetLineAttributes(dpy, gc, 1, LineSolid, CapButt, JoinMiter);

    if (area.xwrap) {
	/* Area is cylinder; draw a rectangle. */
	xform(side, map, 0, 0, &sx, &sy);
	XFillRectangle(dpy, map->viewwin, gc,
		       0, 0, area.width * map->vp->hw, sy);
	XSetForeground(dpy, gc, side->ui->whitecolor);
	XDrawRectangle(dpy, map->viewwin, gc,
		       0, 0, area.width * map->vp->hw, sy);
    } else {
	/* Area is hexagon; draw a hexagon. */
	aw = area.width;  ah = area.height;
	xform(side, map, 0 + ah / 2, 0, &llx, &lly);
	points[0].x = llx;  points[0].y = lly;
	xform(side, map, aw - 1, 0, &lrx, &lry);
	points[1].x = lrx;  points[1].y = lry;
	xform(side, map, aw - 1, ah / 2, &rx, &ry);
	points[2].x = rx;   points[2].y = ry;
	xform(side, map, aw - 1 - ah / 2, ah - 1, &urx, &ury);
	points[3].x = urx;  points[3].y = ury;
	xform(side, map, 0, ah - 1, &ulx, &uly);
	points[4].x = ulx;  points[4].y = uly;
	xform(side, map, 0, ah / 2, &lx, &ly);
	points[5].x = lx;   points[5].y = ly;
	/* Offset so polygon edges run through middles of cells. */
	for (i = 0; i < 6; ++i) {
	    points[i].x += map->vp->hw / 2;  points[i].y += map->vp->hh / 2;
	}
	/* End up where we started. */
	points[6].x = points[0].x;  points[6].y = points[0].y;
	XFillPolygon(dpy, map->viewwin, gc,
		     points, 6, Convex, CoordModeOrigin);
	XSetForeground(dpy, gc, side->ui->whitecolor);
	XDrawLines(dpy, map->viewwin, gc, points, 7, CoordModeOrigin);
    }
    /* This probably drew a large area; make sure it's all out there. */
    XFlush(dpy);
    /* Restore the foreground color. */
    XSetForeground(dpy, gc, side->ui->fgcolor);
}

/* Draw the view proper. */

void
draw_map_view(side, map)
Side *side;
Map *map;
{
    int y1, y2, y, x1, x2, adj;
    int halfheight = area.height / 2;

    draw_area_background(side, map);

    if (map->vp->vcx < 0 || map->vp->vcy < 0) {
    	run_warning("doing a nasty hack");
	map->vp->vcx = map->vp->vcy = 2;
    }
    /* Compute the width and height. */
    map->vw = min(area.width, map->pxw / map->vp->hw + 2);
    map->vh = min(area.height, map->pxh / map->vp->hch + 2);
    /* Compute some cached values that are used a lot (still?). */
    map->vw2 = map->vw / 2;  map->vh2 = map->vh / 2;

    map->vy = ((map->vp->totsh - map->vp->sy) / map->vp->hch) - map->vh;
    /* Now adjust the bottom row so it doesn't go outside the area. */
    if (map->vy < 0)
      map->vy = 0;
    if (map->vy > area.height - map->vh)
      map->vy = area.height - map->vh;
    /* Compute the leftmost "column". */
    map->vx = map->vp->sx / map->vp->hw - map->vy / 2 - 1;
    DGprintf("Set map %x viewport to be %dx%d @ %d,%d (nom center %d,%d)\n",
	     map, map->vw, map->vh, map->vx, map->vy,
	     map->vp->vcx, map->vp->vcy);
    DGprintf("Displaying map view at %d,%d, size %d,%d, center %d,%d\n",
	     map->vx, map->vy, map->vw, map->vh, map->vp->vcx, map->vp->vcy);
    /* Compute top and bottom rows to be displayed. */
    /* Include rows that will only be partly drawn. */
    y1 = min(map->vy + map->vh, area.height - 1);
    y2 = map->vy;
    for (y = y1; y >= y2; --y) {
	/* Adjust the right and left bounds to fill the viewport as
	   much as possible, without going too far (the drawing code
	   will clip, but clipped drawing is still expensive). */
	/* could test by drawing viewport rect as lines... */
	adj = (y - map->vy) / 2;
	/* If the area doesn't wrap, then we might have to stop
	   drawing before we reach the edge of the viewport. */
	/* (is this really reliable?) */
	x1 = map->vx - (y - map->vy) / 2;
	x2 = x1 + map->vw + 2 /* bleah, shouldn't be necessary */;
	if (area.xwrap) {
	} else {
	    /* Truncate x's to stay within the area. */
	    x1 = max(0, min(x1, area.width-1));
	    x2 = max(0, min(x2, area.width));
	    /* If this row is entirely in the NE corner, don't draw anything. */
	    if (x1 + y > area.width + halfheight)
	      continue;
	    /* If this row is entirely in the SW corner, don't draw anything. */
	    if (x2 + y < halfheight)
	      continue;
	    /* If the row ends up in the NE corner, shorten it. */
	    if (x2 + y > area.width + halfheight)
	      x2 = area.width + halfheight - y;
	    /* If the row starts out in the SW corner, shorten it. */
	    if (x1 + y < halfheight)
	      x1 = halfheight - y;
	}
	/* I don't understand the wrapx below, but Massimo says it
	   fixes bugs. -sts */
	draw_row(side, map, wrapx(x1), y, x2 - x1, FALSE);
	/* (should test for input events here, would respond better) */
    }
    draw_current(side, map);
}

/* The basic map drawing routine does an entire row at a time, which yields
   order-of-magnitude speedups. */

void
draw_row(side, map, x0, y0, len, clearit)
Side *side;
Map *map;
int x0, y0, len, clearit;
{
    int x, b, c, i;

    if (map->drawterrain) {
	draw_terrain_row(side, map, x0, y0, len, (clearit == -1));
	/* The relative ordering of these is quite important.  Note that
	   each should be prepared to run independently also, since the
	   other displays might have been turned off. */
	if (any_aux_terrain_defined()) {
	    if (bords_to_draw(map)) {
		for_all_terrain_types(b) {
		    if (t_is_border(b) && aux_terrain_defined(b)) {
			for (x = x0; x < x0 + len; ++x) {
			    draw_borders(side, map, x, y0, b);
			}
		    }
		}
	    }
	    /* Draw the connections on top of the borders. */
	    if (conns_to_draw(map)) {
		for_all_terrain_types(c) {
		    if (t_is_connection(c) && aux_terrain_defined(c)) {
			for (x = x0; x < x0 + len; ++x) {
			    draw_connections(side, map, x, y0, c);
			}
		    }
		}
	    }
	}
	if (features_defined() && map->drawfeatureboundaries) {
	    for (x = x0; x < x0 + len; ++x) {
		if (!inside_area(x, y0))
		  continue;
		draw_feature_boundary(side, map, x, y0, raw_feature_at(x, y0));
	    }
	}
    }
    if (features_defined() && map->drawfeaturenames && side->ui->legends) {
	for (i = 0; i < side->ui->numfeatures; ++i) {
	    if (side->ui->legends[i].oy == y0) {
		draw_feature_name(side, map, i);
	    }
	}
    }
    if (elevations_defined() && map->drawelevations && map->vp->hw >= 20) {
	draw_elevations(side, map, x0, y0, len);
    }
    /* Draw sparse things on top of the basic row. */
    if (people_sides_defined() && map->drawpeople && map->vp->hw >= 8) {
	for (x = x0; x < x0 + len; ++x) {
	    if (!inside_area(x, y0))
	      continue;
	    draw_people(side, map, x, y0);
	}
    }
    /* Draw units. */
    if (map->drawunits) {
	for (x = x0; x < x0 + len; ++x) {
	    if (!inside_area(x, y0))
	      continue;
	    draw_units(side, map, x, y0);
	}
    }
    if (map->drawnames && map->vp->hh >= 8) {
	for (x = x0; x < x0 + len; ++x) {
	    if (!inside_area(x, y0))
	      continue;
	    draw_legend(side, map, x, y0);
	}
    }
}

char buffer[BUFSIZE];

static void
draw_feature_name(side, map, f)
Side *side;
Map *map;
int f;
{
    Legend *leg = &side->ui->legends[f];
    int y = leg->oy;
    int x = leg->ox;
    int d = ((leg->dx+1) * map->vp->hw * 9)/10;
    int i, b, l, lnew, sx0, sy0, sxc2, syc;
    char *pad, *name;
    XCharStruct *bounds;

    if (leg->dist < 0 || y < map->vy || y > map->vy + map->vh)
      return;

    name = feature_desc(find_feature(f+1), buffer);
    if (!name || !name[0])
      return;
    
    XSetFillStyle(side->ui->dpy, side->ui->gc, FillSolid);
    /* we should choose a color contrasting with main terrain */
    XSetForeground(side->ui->dpy, side->ui->gc, side->ui->fgcolor);
    xform(side, map, x, y, &sx0, &sy0);
    /* xform returns coordinates of the upper-left corner of the cell */
    sxc2 = 2 * sx0 + (leg->dx + 1) * map->vp->hw; /* twice center x */
    syc  = sy0 + map->vp->hh / 2;		  /* center y */
    if (sxc2 + 2 * d < 0)
      return;

    l = XTextWidth(side->ui->flegendfonts[0], name, strlen(name));
    for (i = 1; i < 5; i++) {
	/* check if the font is too tall for the current magnification */
	bounds = &side->ui->flegendfonts[i]->max_bounds;
	if (bounds->ascent+bounds->descent > map->vp->hch)
	  break;
	lnew = XTextWidth(side->ui->flegendfonts[i], name, strlen(name));
	if (lnew > d) {
	    XSetFont(side->ui->dpy, side->ui->gc, side->ui->flegendfids[i-1]);
	    bounds = &side->ui->flegendfonts[i-1]->max_bounds;
	    XDrawString(side->ui->dpy, map->viewwin, side->ui->gc,
			(sxc2 - l) / 2, syc + (bounds->ascent - bounds->descent) / 2,
			name, strlen(name));
	    return;
	}
	l = lnew;
    }
    /* retain the biggest usable font */
    if (i)
      --i;

    XSetFont(side->ui->dpy, side->ui->gc, side->ui->flegendfids[i]);
    bounds = &side->ui->flegendfonts[i]->max_bounds;
    for (b = 1; b < 21; b++) {
	pad = pad_blanks(name, b);
	lnew = XTextWidth(side->ui->flegendfonts[i], pad, strlen(pad));
	if (lnew > d || b == 20) {
	    if (b == 20) {
		l = lnew;
		b++;
	    }
	    pad = pad_blanks(name, b - 1);
 	    /* map->pxw is the window width */
 	    if (sxc2 - l > 2 * map->pxw)
	      return; 
	    XDrawString(side->ui->dpy, map->viewwin, side->ui->gc,
			(sxc2 - l) / 2,
			syc + (bounds->ascent - bounds->descent) / 2,
			pad, strlen(pad));
	    return;
	}
	l = lnew;
    }
}

static int
cell_drawing_info(side, x, y, power, seeall, patp, colorp)
Side *side;
int x, y, power, seeall;
Pixmap *patp;
long *colorp;
{
    int t;
    enum whattouse rslt;

    *patp = None;
    *colorp = side->ui->whitecolor;
    if (seeall || terrain_visible(side, x, y)) {
	t = terrain_at(x, y);
	*patp = side->ui->terrpics[power][t];
	*colorp = side->ui->cellcolor[t];
	rslt = side->ui->usewhat[power][t];
    } else {
	rslt = dontdraw;
    }
    return rslt;
}

static void set_terrain_gc_for_image PARAMS ((Side *side, Image *timg));

static void
set_terrain_gc_for_image(side, timg)
Side *side;
Image *timg;
{
    X11Image *ximg;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->terrgc;

    if (timg != NULL) {
	ximg = (X11Image *) timg->hook;
	if (ximg != NULL) {
	    if (!side->ui->monochrome) {
		if (side->ui->dflt_color_terr_images) {
		    if (ximg->colr != None) {
			XSetFillStyle(dpy, gc, FillTiled);
			XSetTile(dpy, gc, ximg->colr);
		    } else if (ximg->mono != None) {
			XSetFillStyle(dpy, gc, FillOpaqueStippled);
			XSetStipple(dpy, gc, ximg->mono);
		    } else {
			XSetFillStyle(dpy, gc, FillSolid);
		    }
		} else {
		    XSetFillStyle(dpy, gc, FillSolid);
		}
	    } else if (ximg->mono != None) {
		XSetFillStyle(dpy, gc, FillOpaqueStippled);
		XSetStipple(dpy, gc, ximg->mono);
	    } else {
		/* No pattern, no color - what to do? */
		XSetFillStyle(dpy, gc, FillSolid);
	    }
	} else {
	    XSetFillStyle(dpy, gc, FillSolid);
	}
    } else {
	XSetFillStyle(dpy, gc, FillSolid);
    }
}

/* This interfaces higher-level drawing decisions to the rendition of
   individual pieces of display.  The rendering technique chosen depends
   on what the init code has decided is appropriate given what it found
   during init and what magnification the display is at.

   This routine is performance-critical;  any improvements will probably
   have a noticeable effect on the display.  But also note that X's
   main bottleneck is the network connection, so it's more useful to
   eliminate roundtrips to the server than anything else. */

/* (should cache best images, never have to look up in here) */

static char *cellbuf = NULL;

static void
draw_terrain_row(side, map, x0, y0, len, force)
Side *side;
Map *map;
int x0, y0, len, force;
{
    int x1, x0w, x, xw, t, sx, sy, i = 0, j;
    int w = map->vp->hw, h = map->vp->hh, p = map->vp->power;
    int seeall = map->seeall;
    int dogrid = map->drawgrid, dofill = map->drawcellpats;
    long color, segcolor;
    enum whattouse drawmethod, segdrawmethod;
    Pixmap pat, segpat;
    Image *timg;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->terrgc;

    x1 = x0;
    x0w = wrapx(x0);
    segdrawmethod = cell_drawing_info(side, x0w, y0, p, seeall, &segpat, &segcolor);
    for (x = x0; x < x0 + len + 1; ++x) {
	xw = wrapx(x);
	t = terrain_at(xw, y0);
	drawmethod = cell_drawing_info(side, xw, y0, p, seeall, &pat, &color);
	/* Decide if the run is over and we need to dump some output. */
	if (x == x0 + len
	    || drawmethod != segdrawmethod
	    || color != segcolor
	    || pat != segpat
	    || segdrawmethod == usepolygons
	    || force) {
	    /* Note: we might end up drawing something that matches
	       the background color, which wastes time, but apparently
	       the test "(segdrawmethod != dontdraw && segcolor !=
	       side->ui->bgcolor)" is not completely sufficient.
	       (should figure this one out sometime) */
	    t = terrain_at(wrapx(x1), y0);
	    timg = best_image(side->ui->timages[t], w, h);
	    xform(side, map, x1, y0, &sx, &sy);
	    /* Last-minute fixup for when we're erasing a cell. */
	    if (force && segdrawmethod == dontdraw) {
		segdrawmethod = side->ui->usewhat[p][t];
		/* this must match the bg set in draw_area_background */
		segcolor = side->ui->gridcolor /* bgcolor */;
		timg = NULL;
	    }
	    XSetForeground(dpy, gc, segcolor);
	    XSetBackground(dpy, gc, side->ui->whitecolor);
	    switch (segdrawmethod) {
	      case dontdraw:
	        /* Don't do anything. */
	        break;
	      case useblocks:
		/* maybe adjust to go along middle of cell borders? */
		XSetClipMask(dpy, gc, None);
		set_terrain_gc_for_image(side, timg);
		XFillRectangle(dpy, map->viewwin, gc, sx, sy, i * w, h);
		break;
	      case usepictures:
		if (dofill && 0 /* have individual pictures */) {
		    XSetClipMask(dpy, gc, segpat);
		} else if (dogrid) {
		    XSetClipMask(dpy, gc, side->ui->bhexpics[p]);
		} else {
		    XSetClipMask(dpy, gc, side->ui->hexpics[p]);
		}
		set_terrain_gc_for_image(side, timg);
		for (j = 0; j < i; ++j) {
		    xform(side, map, x1 + j, y0, &sx, &sy);
		    XSetClipOrigin(dpy, gc, sx, sy);
		    XFillRectangle(dpy, map->viewwin, gc, sx, sy, w, h);
		}
		break;
	      case usefontchars:
		if (cellbuf == NULL)
		  cellbuf = xmalloc(area.width + 1);
		if (dofill) {
		    cellbuf[i] = side->ui->terrchars[p][t];
		} else if (dogrid) {
		    cellbuf[i] = 'o';
		} else {
		    cellbuf[i] = 'O';
		}
		/* No clipping when we're drawing cells with text. */
		XSetClipMask(dpy, gc, None);
		XDrawString(dpy, map->viewwin, gc, sx, sy, cellbuf, i);
		break;
	      case usepolygons:
		/* No clipping when we're drawing big polygons. */
		XSetClipMask(dpy, gc, None);
		set_terrain_gc_for_image(side, timg);
		draw_hex_polygon(side, map, map->viewwin, gc, sx, sy, p, dogrid);
	    }
	    /* Reset everything for the next run. */
	    i = 0;
	    x1 = x;
	    segdrawmethod = drawmethod;
	    segpat = pat;
	    segcolor = color;
	}
	++i;
    }
}

static void
draw_elevations(side, map, x0, y0, len)
Side *side;
Map *map;
int x0, y0, len;
{
    int x, xw, sx, sy;

    if (map->vp->hw < 20)
      return;
    for (x = x0; x < x0 + len + 1; ++x) {
	xw = wrapx(x);
	if (terrain_visible(side, xw, y0)) {
	    sprintf(spbuf, "%d", elev_at(xw, y0));
	    xform(side, map, x, y0, &sx, &sy);
	    draw_text(side, map->viewwin, sx + 5, sy + map->vp->uh / 2,
		      spbuf, side->ui->fgcolor);
	}
    }
}

/* Draw a single unit icon as appropriate.  This *also* has a bunch of
   details to worry about: centering of icon in cell, clearing a rectangular
   area for the icon, picking a color for the unit, using either a bitmap
   or font char, and adding a side emblem.

   Must also be careful not to draw black-on-black for units in space. */

static void
draw_units(side, map, x, y)
Side *side;
Map *map;
int x, y;
{
    int sx, sy, sw, sh, uview, u, s;
    int uw = map->vp->uw, uh = map->vp->uh;
    Unit *unit;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->unitgc;

    if ((map->seeall || units_visible(side, x, y))
	&& unit_at(x, y) != NULL) {
	if (uw <= 16) {
	    unit = unit_at(x, y);
	    xform(side, map, x, y, &sx, &sy);
	    /* Adjust to unit part of cell. */
	    sx += (map->vp->hw - uw) / 2;  sy += (map->vp->hh - uh) / 2;
	    if (unit != NULL) {
		if (unit->occupant != NULL
		    && uw >= 8
	    	    && (side_controls_unit(side, unit)
			|| map->seeall
			|| u_see_occupants(unit->type))) {
		    /* Draw a "grouping box", in white, but with no occs
		       actually drawn. */
		    XSetClipMask(dpy, gc, None);
		    XSetForeground(dpy, gc, side->ui->whitecolor);
		    XFillRectangle(dpy, map->viewwin, gc,
				   sx + 1, sy + 1, uw - 2, uh - 2);
		    /* Put a black border around it, for better contrast. */
		    XSetForeground(dpy, gc, side->ui->blackcolor);
		    XDrawRectangle(dpy, map->viewwin, gc,
				   sx + 1, sy + 1, uw - 2, uh - 2);
	    	}
		draw_unit_image(side, map->viewwin, sx, sy, uw, uh,
				unit->type, side_number(unit->side),
				-1, -1);
		/* (should indicate presence of other units, a la Mac) */
		if (map->drawnames)
		  draw_unit_name(side, map, unit, sx, sy, uw, uh);
	    }
	} else {
	    for_all_stack(x, y, unit) {
		x_xform_unit(side, map, unit, &sx, &sy, &sw, &sh);
		draw_unit_and_occs(side, map, unit, sx, sy, sw, sh, TRUE);
	    }
	}
    } else if ((uview = unit_view(side, x, y)) != EMPTY) {
	u = vtype(uview);  s = vside(uview);
	if (s == side_number(side))
	  return; /* should make error */
	xform(side, map, x, y, &sx, &sy);
	sx += (map->vp->hw - uw) / 2;  sy += (map->vp->hh - uh) / 2;
	draw_unit_image(side, map->viewwin, sx, sy, uw, uh,
			u, s, -1, -1);
    }
}

static void
draw_unit_and_occs(side, map, unit, sx, sy, sw, sh, drawoccs)
Side *side;
Map *map;
Unit *unit;
int sx, sy, sw, sh, drawoccs;
{
    int u = unit->type, s = side_number(unit->side), sx2, sy2, sw2, sh2;
    Unit *occ;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->unitgc;

    /* If an occupant's side is the same as its transport's, then there's
       really no need to draw its side emblem, since the transport's emblem
       will also be visible. */
    if (unit->transport && unit->side == unit->transport->side)
      s = -1;
    if (unit->occupant == NULL || sw <= 8) {
	draw_unit_image(side, map->viewwin, sx, sy, sw, sh, u, s,
			-1, -1);
	if (map->drawnames)
	  draw_unit_name(side, map, unit, sx, sy, sw, sh);
    } else {
	x_xform_occupant(side, map, unit, unit, sx, sy, sw, sh,
		       &sx2, &sy2, &sw2, &sh2);
	/* Draw a white box to indicate the grouping. */
	XSetClipMask(dpy, gc, None);
	XSetForeground(dpy, gc, side->ui->whitecolor);
	XFillRectangle(dpy, map->viewwin, gc,
		       sx + 1, sy + 1, sw - 2, sh - 2);
	/* Put a black border around it, for better contrast. */
	XSetForeground(dpy, gc, side->ui->blackcolor);
	XDrawRectangle(dpy, map->viewwin, gc,
		       sx + 1, sy + 1, sw - 2, sh - 2);
	/* Draw the transport's image. */
	draw_unit_image(side, map->viewwin, sx2, sy2, sw2, sh2, u, s,
			-1, -1);
	if (map->drawnames)
	  draw_unit_name(side, map, unit, sx2, sy2, sw2, sh2); 
	/* Maybe draw all of its occupants, recursively. */
	if (drawoccs) {
	    for_all_occupants(unit, occ) {
		x_xform_occupant(side, map, unit, occ, sx, sy, sw, sh,
				 &sx2, &sy2, &sw2, &sh2);
		draw_unit_and_occs(side, map, occ, sx2, sy2, sw2, sh2, TRUE);
	    }
	}
    }
}

#if 0  /* not needed until we update unit images individually */
static void
draw_one_unit(side, map, unit)
Side *side;
Map *map;
Unit *unit;
{
    int sx, sy, sw, sh;
    int uw = map->vp->uw, uh = map->vp->uh;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->unitgc;

    if (unit == NULL || !in_area(unit->x, unit->y))
      return;
    if (map->vp->uw <= 16) {
	xform(side, map, unit->x, unit->y, &sx, &sy);
	/* Adjust to unit part of cell. */
	sx += (map->vp->hw - uw) / 2;  sy += (map->vp->hh - uh) / 2;
	if (unit == unit_at(unit->x, unit->y)) {
	    if (unit->occupant != NULL
		&& (side_controls_unit(side, unit)
		    || map->seeall
		    || u_see_occupants(unit->type))) {
		/* Draw a "grouping box", in white, but with no occs drawn. */
		XSetForeground(dpy, gc, side->ui->whitecolor);
		XFillRectangle(dpy, map->viewwin, gc,
			       sx + 1, sy + 1, uw - 2, uh - 2);
		XSetForeground(dpy, gc, side->ui->blackcolor);
		XFillRectangle(dpy, map->viewwin, gc,
			       sx + 1, sy + 1, uw - 2, uh - 2);
	    }
	    draw_unit_image(side, map->viewwin, sx, sy, uw, uh,
			    unit->type, side_number(unit->side),
			    -1, -1);
	    /* (should indicate presence of other units, a la Mac) */
	    if (map->drawnames)
	      draw_unit_name(side, map, unit, sx, sy, uw, uh);
	}
    } else {
	x_xform_unit(side, map, unit, &sx, &sy, &sw, &sh);
	draw_unit_and_occs(side, map, unit, sx, sy, sw, sh, FALSE);
    }
}
#endif

static void
draw_unit_name(side, map, unit, sx, sy, sw, sh)
Side *side;
Map *map;
Unit *unit;
int sx, sy, sw, sh;
{
    if (!empty_string(unit->name)) {
	draw_legend_text(side, map->viewwin,
			 sx + sw,
			 sy + sh / 2 - 5,
			 map->vp->power, unit->name, side->ui->fgcolor, TRUE);
    }
}


/* Indicate what kind of people are living in the given cell. */
static void
draw_people(side, map, x, y)
Side *side;
Map *map;
int x, y;
{
    int pop, sx, sy, sw, sh, ex, ey, ew, eh, dir, x1, y1, pop1;
    int bordercell = FALSE;
    Side *side2;
    
    if (!terrain_visible(side, wrapx(x), y))
      return;
    pop = people_side_at(wrapx(x), y);
    side2 = side_n(pop);
    xform(side, map, x, y, &sx, &sy);
    /* Decide which edges are borders of the country. */
    for_all_directions(dir) {
	if (point_in_dir(x, y, dir, &x1, &y1)) {
	    pop1 = people_side_at(wrapx(x1), y1);
	    if (pop != pop1) {
		if (pop1 != NOBODY) {
		    draw_country_border_line(side, map->viewwin,
					     sx, sy, dir, map->vp->power);
		} else {
		    /* should draw in gray instead? */
		    draw_country_border_line(side, map->viewwin,
					     sx, sy, dir, map->vp->power);
		}
		bordercell = TRUE;
	    }
	}
    }
    /* Draw an emblem for the people in the cell. */
    if (map->drawpeople && bordercell) {
	sw = map->vp->hw;  sh = map->vp->hh;
	ew = min(sw, max(8, sw / 4));  eh = min(sh, max(8, sh / 4));
	ex = sx + sw / 2 - ew / 2;  ey = sy + sh / 2 - eh / 2;
	draw_side_emblem(side, map->viewwin, ex, ey, ew, eh,
			 side_number(side2), 0);
    }
}

/* Draw three borders of the given cell. */

/* (do we need another routine to repair all six borders?) */

static void
draw_borders(side, map, x, y, b)
Side *side;
Map *map;
int x, y, b;
{
    int dir, bitmask = 0, sx, sy;
	
    if (!terrain_visible(side, wrapx(x), y)
	|| !any_borders_at(x, y, b))
      return;
    for_all_directions(dir) {
	if (border_at(x, y, dir, b) && seen_border(side, x, y, dir)) {
	    bitmask |= 1 << dir;
	}
    }
    if (bitmask != 0) {
	xform(side, map, x, y, &sx, &sy);
	draw_border_line_multiple(side, map, map->viewwin, sx, sy,
				  bitmask, map->vp->power, b);
    }
}

/* Draw three connections of the given cell. */

/* Actually this draws all six half-connections.  It also only draws the
   connection if the underlying terrain is different. */

static void
draw_connections(side, map, x, y, t)
Side *side;
Map *map;
int x, y, t;
{
    int dir, bitmask = 0, sx, sy;

    if (bwid[map->vp->power] == 0)
      return;
    if (!terrain_visible(side, wrapx(x), y))
      return;
    xform(side, map, x, y, &sx, &sy);
    for_all_directions(dir) {
	if (connection_at(x, y, dir, t)) {
	    bitmask |= 1 << dir;
	}
    }
    if (bitmask != 0) {
	draw_connection_line_multiple(side, map->viewwin,
				      sx, sy, bitmask,
				      map->vp->power, t);
    }
}

/* Draw any text that should be associated with this cell. */

/* (could precompute what the string will lap over and move or truncate str),
   should be deterministic for each mag, so redraw doesn't scramble */

/* do features, label at a cell with nothing else, and declared as the
   feature's "center" */

/* Black/white text should be consistent for each period, use mask only
   to fix difficulties. */

static void
draw_legend(side, map, x, y)
Side *side;
Map *map;
int x, y;
{
    int sx, sy, pixlen;
    char *featname;
    Feature *feature;

    if (!inside_area(x, y))
      return;
    if (!terrain_visible(side, wrapx(x), y))
      return;
    /* feature object should specify legend's position */
    feature = feature_at(x, y);
    if (feature != NULL) {
	if (feature->size == 1) {
	    featname = feature_name_at(x, y);
	    if (featname != NULL) {
		pixlen = strlen(featname) * 8;
		xform(side, map, x, y, &sx, &sy);
		draw_legend_text(side, map->viewwin,
				 sx + 1 + (pixlen > map->vp->hw ? 2 :
					   (map->vp->hw/2 - pixlen / 2)),
				 sy - 6 + map->vp->hh / 2,
				 map->vp->power, featname,
				 side->ui->fgcolor, TRUE);
	    }
	}
    }
}

/* Cursor drawing also draws the unit in some other color if it's not the
   "top-level" unit in a cell. */

/* "color unders" here are not correct */

void
draw_current(side, map)
Side *side;
Map *map;
{
    int sx, sy, sw, sh;
    int uw = map->vp->uw, uh = map->vp->uh;
    Unit *unit;

    if (in_play(map->curunit)) {
	unit = map->curunit;
	/* Compute the bounding box we're going to hilite. */
	if (map->vp->power >= 5) { /* not ideal test */
	    x_xform_unit_self(side, map, unit, &sx, &sy, &sw, &sh);
	} else {
	    xform(side, map, unit->x, unit->y, &sx, &sy);
	    /* Adjust to unit part of cell. */
	    sx += (map->vp->hw - uw) / 2;  sy += (map->vp->hh - uh) / 2;
	    sw = uw;  sh = uh;
	}
	/* Maybe redraw the unit that the cursor is showing. */
	if (unit->transport != NULL) {
	    if (side->ui->monochrome) {
		draw_unit_image(side, map->viewwin, sx, sy, sw, sh,
				unit->type, -1, -1, -1);
	    } else {
		/* Leave any underlying image alone, but draw over it
		   in a different color. */
		draw_unit_image(side, map->viewwin, sx, sy, sw, sh,
				unit->type, -1, side->ui->diffcolor, -1);
	    }
	}
	/* Draw the cursor icon proper. */
	draw_cursor_icon(side, map->viewwin, sx, sy, sw, sh);
    } else if (inside_area(map->curx, map->cury)) {
	xform(side, map, map->curx, map->cury, &sx, &sy);
	/* Adjust to unit part of cell. */
	sx += (map->vp->hw - uw) / 2;  sy += (map->vp->hh - uh) / 2;
	draw_cursor_icon(side, map->viewwin, sx, sy, uw, uh);
    }
}

/* Get rid of curunit indicator by redrawing the cell. */

void
erase_current(side, map, x, y, unit)
Side *side;
Map *map;
int x, y;
Unit *unit;
{
    /* (should use unit to decide whether to redraw only its part of
       the cell, instead of doing whole cell) */
    if (in_area(x, y)) {
	draw_row(side, map, x, y, 1, -1);
    } else if (unit != NULL && in_area(unit->x, unit->y)) {
#if 0 /* this would be a good optimization if it were fixed... */
	draw_one_unit(side, map, unit);
#else
	draw_row(side, map, unit->x, unit->y, 1, -1);
#endif
    }
}

void
draw_blast_image(side, map, sx, sy, sw, sh, blasttype)
Side *side;
Map *map;
int sx, sy, sw, sh, blasttype;
{
    int sx2, sy2;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->gc;

    if (sw >= 16 && sh >= 16) {
	sx2 = sx + (sw - 16) / 2;  sy2 = sy + (sh - 16) / 2;
	XSetClipMask(dpy, gc, side->ui->hitpics[blasttype]);
	XSetClipOrigin(dpy, gc, sx2 + 1, sy2 + 1);
	XSetForeground(dpy, gc, side->ui->blackcolor);
	XFillRectangle(dpy, map->viewwin, gc, sx2 + 1, sy2 + 1, 16, 16);
	flush_output(side);
	XSetClipOrigin(dpy, gc, sx2, sy2);
	XSetForeground(dpy, gc, side->ui->badcolor);
	XFillRectangle(dpy, map->viewwin, gc, sx2, sy2, 16, 16);
	flush_output(side);
    } else {
	XSetForeground(dpy, gc, side->ui->badcolor);
	XFillRectangle(dpy, map->viewwin, gc, sx, sy, sw, sh);
	flush_output(side);
	XSetForeground(dpy, gc, side->ui->blackcolor);
	XFillRectangle(dpy, map->viewwin, gc, sx, sy, sw, sh);
	flush_output(side);
	XSetFunction(dpy, gc, GXcopy);
    }
}

/* Flash an area of the screen. */

void
invert_unit_subarea(side, map, x, y)
Side *side;
Map *map;
int x, y;
{
    int sx, sy;

    xform(side, map, x, y, &sx, &sy);
    sx += (map->vp->hw - map->vp->uw) / 2;
    sy += (map->vp->hh - map->vp->uh) / 2;
    XSetFunction(side->ui->dpy, side->ui->gc, GXinvert);
    XFillRectangle(side->ui->dpy, map->viewwin, side->ui->gc,
		   sx, sy, map->vp->uw, map->vp->uh);
    flush_output(side);
    XSetFunction(side->ui->dpy, side->ui->gc, GXcopy);
}

#if 0
/* Flash an area of the screen. */

invert_whole_map(side, map)
Side *side;
Map *map;
{
    int sw = map->vw * map->vp->hw, sh = map->vh * map->vp->hh;

    XFillRectangle(side->ui->dpy, map->viewwin, side->ui->gc,
		   0, 0, sw, sh);
    flush_output(side);
}
#endif

#if 0
/* Draw just one of the mushroom cloud shapes. */

draw_mushroom(side, map, x, y, i)
Side *side;
Map *map;
int x, y, i;
{
}
#endif

static void
draw_feature_boundary(side, map, x, y, fid)
Side *side;
Map *map;
int x, y, fid;
{
    int wid, p, wid2, d, color, fid0, x1, y1, sx, sy;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->bdrygc;
    Pixmap graylev;

    if (!terrain_visible(side, wrapx(x), y))
      return;
    p = map->vp->power;
    wid = bwid[p];
    fid0 = raw_feature_at(x, y);
    if (fid0 == 0)
      return;
    xform(side, map, x, y, &sx, &sy);
    if (wid == 0)
      return;
    wid2 = wid / 2;

    /* for now: */
    if (fid0 == side->ui->curfid) {
	color = side->ui->enemycolor;
    } else {
	color = side->ui->neutcolor;
    }
    XSetForeground(dpy, gc, color);
    if (side->ui->monochrome) {
	if (fid0 == side->ui->curfid) {
	    graylev = side->ui->grays[darkgray];
	} else {
	    graylev = side->ui->grays[gray];
	}
	XSetFillStyle(dpy, gc, FillStippled);
	XSetStipple(dpy, gc, graylev);
    } else {
	XSetFillStyle(dpy, gc, FillSolid);
    }
    XSetClipMask(dpy, gc, None);
    XSetLineAttributes(dpy, gc, bwid[p], LineSolid, CapButt, JoinMiter); 
    for_all_directions(d) {
	if (point_in_dir(x, y, d, &x1, &y1)) {
	    if (raw_feature_at(x1,y1) != fid0) {
		XDrawLine(dpy, map->viewwin, gc,
			  sx + qx[p][d], sy + qy[p][d],
			  sx + qx[p][d+1], sy + qy[p][d+1]);
	    }
	}
    }
}

/* Do the grody work of drawing very large polygons accurately. */

static void
draw_hex_polygon(side, map, win, gc, sx, sy, power, dogrid)
Side *side;
Map *map;
Window win;
GC gc;
int sx, sy, power, dogrid;
{
    XPoint points[6];
    int hw = hws[power], hh = hhs[power], delt = (hhs[power] - hcs[power]);
    int ew = (dogrid ? 1 : 0);
	    
    points[0].x = sx + hw / 2;        points[0].y = sy;
    points[1].x = hw / 2 - ew;        points[1].y = delt - ew;
    points[2].x = 0;                  points[2].y = hh - 2 * delt - ew;
    points[3].x = 0 - (hw / 2 - ew);  points[3].y = delt - ew;
    points[4].x = 0 - (hw / 2 - ew);  points[4].y = 0 - (delt - ew);
    points[5].x = 0;                  points[5].y = 0 - (hh - 2 * delt - ew);
    XFillPolygon(side->ui->dpy, win, gc, points, 6, Convex, CoordModePrevious);
    XFlush(side->ui->dpy);
}

/* Draw a mask of borders for the given location. */

static void
draw_border_line_multiple(side, map, win, sx, sy, bitmask, power, t)
Side *side;
Map *map;
Window win;
int sx, sy, bitmask, power, t;
{
    int wid = bwid[power], wid2, dir, color, sx1, sy1, sx2, sy2;
    Image *timg;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->terrgc;

    if (wid == 0)
      return;
    wid2 = wid / 2;
    color = side->ui->cellcolor[t];
    XSetForeground(dpy, gc, color);
    XSetBackground(dpy, gc, side->ui->whitecolor);
    timg = best_image(side->ui->timages[t], wid, wid);
    set_terrain_gc_for_image(side, timg);
    XSetClipMask(dpy, gc, None);
    XSetLineAttributes(dpy, gc, bwid[power], LineSolid, CapButt, JoinMiter); 

    for_all_directions(dir) {
	if (bitmask & (1 << dir)) {
	    sx1 = bsx[power][dir];  sy1 = bsy[power][dir];
	    sx2 = bsx[power][dir+1];  sy2 = bsy[power][dir+1];
	    XDrawLine(dpy, win, gc,
		      sx + sx1 - wid2, sy + sy1 - wid2,
		      sx + sx2 - wid2, sy + sy2 - wid2);
	}
    }
}

/* Draw a mask of connection terrain at the given location. */

static void
draw_connection_line_multiple(side, win, sx, sy, bitmask, power, t)
Side *side;
Window win;
int sx, sy, bitmask, power, t;
{
    int wid = cwid[power], wid2, cx = hws[power] / 2, cy = hhs[power] / 2;
    int color, dir;
    Image *timg;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->terrgc;

    if (wid == 0 || lsx[power][0] == 0)
      return;
    wid2 = wid / 2;
    color = side->ui->cellcolor[t];
    XSetForeground(dpy, gc, color);
    XSetBackground(dpy, gc, side->ui->whitecolor);
    timg = best_image(side->ui->timages[t], wid, wid);
    set_terrain_gc_for_image(side, timg);
    XSetClipMask(dpy, gc, None);
    XSetLineAttributes(dpy, gc, wid, LineSolid, CapButt, JoinMiter); 

    for_all_directions(dir) {
	if (bitmask & (1 << dir)) {
	    XDrawLine(dpy, win, gc,
		      sx + cx - wid2, sy + cy - wid2,
		      sx + cx + lsx[power][dir] - wid2,
		      sy + cy + lsy[power][dir] - wid2);
	}
    }
}

/* Map legends are stencils usually. */

static void
draw_legend_text(side, win, sx, sy, power, str, color, maskit)
Side *side;
Window win;
int sx, sy, power, color, maskit;
char *str;
{
    sy += (side->ui->ulegendfonts[power][0])->max_bounds.ascent;
    XSetFont(side->ui->dpy, side->ui->ltextgc,
	     (side->ui->ulegendfonts[power][0])->fid);
    XSetForeground(side->ui->dpy, side->ui->ltextgc, color);
    if (maskit) {
	XSetBackground(side->ui->dpy, side->ui->ltextgc, 
		       (color == side->ui->bgcolor ? side->ui->fgcolor :
			side->ui->bgcolor));
	XDrawImageString(side->ui->dpy, win, side->ui->ltextgc,
			 sx, sy, str, strlen(str));
    } else {
	XDrawString(side->ui->dpy, win, side->ui->ltextgc,
		    sx, sy, str, strlen(str));
    }
}

/* Splash a unit image (either bitmap or font char) onto some window. */

void
draw_unit_image(side, win, sx, sy, sw, sh, u, s2, fg, bg)
Side *side;
Window win;
int sx, sy, sw, sh, u, s2, fg, bg;
{
    char buf[1];
    int sx2, sy2, ex, ey, ew, eh, desperate = FALSE;
    long imagecolor, maskcolor;
    Image *uimg;
    X11Image *ximg;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->unitgc;

    uimg = best_image(side->ui->uimages[u], sw, sh);
    if (uimg) {
	/* Offset the image to draw in the middle of its area,
	   whether larger or smaller than the given area. */
	sx2 = sx + (sw - uimg->w) / 2;  sy2 = sy + (sh - uimg->h) / 2;
	/* Only change the size of the rectangle being drawn if it's
	   smaller than what was passed in. */
	if (uimg->w < sw) {
	    sx = sx2;
	    sw = uimg->w;
	}
	if (uimg->h < sh) {
	    sy = sy2;
	    sh = uimg->h;
	}
	/* Figure out what colors to use. */
	imagecolor = ((fg != -1) ? fg : side->ui->blackcolor);
	if (side->ui->unitcolors) {
#if 0
	  (side->ui->numcolors[s2] > 2) ? side->ui->colors[s2][2] :
	    (side->ui->numcolors[s2] > 0) ? side->ui->colors[s2][0] :
#endif
	}
	maskcolor = ((bg != -1) ? bg : side->ui->whitecolor);
	if (side->ui->unitcolors) {
#if 0
	  (side->ui->numcolors[s2] > 1) ? side->ui->colors[s2][1] :
	    side->ui->whitecolor;
#endif
	}
	ximg = (X11Image *) uimg->hook;
	if (ximg != NULL) {
	    if (!side->ui->monochrome
		&& side->ui->dflt_color_unit_images
		&& ximg->colr != None) {
		if (ximg->mask != None) {
		    /* set the clip mask */
		    XSetClipOrigin(dpy, gc, sx2, sy2);
		    XSetClipMask(dpy, gc, ximg->mask);
		}
		/* Draw the color image. */
		XCopyArea(dpy, ximg->colr, win, gc, 0, 0, sw, sh, sx, sy);
	    } else if (ximg->mono != None || ximg->mask != None) {
		/* Set the origin for any subsequent clipping. */
		XSetClipOrigin(dpy, gc, sx2, sy2);
		/* Set the color we're going to use for the mask; use
		   the imagecolor if we'll be using the mask as the
		   only image. */
		XSetForeground(dpy, gc,
			       (ximg->mono == None ? imagecolor : maskcolor));
		/* Set the clip mask to be explicit mask or unit's image. */
		if (ximg->mask)
		  XSetClipMask(dpy, gc, ximg->mask);
		else
		  XSetClipMask(dpy, gc, ximg->mono);
		/* Draw the mask. */
		XFillRectangle(dpy, win, gc, sx, sy, sw, sh);
		/* Draw the image proper. */
		if (ximg->mono != None) {
		    XSetForeground(dpy, gc, imagecolor);
		    XSetClipMask(dpy, gc, ximg->mono);
		    XFillRectangle(dpy, win, gc, sx, sy, sw, sh);
		}
	    } else if (ximg->monochar) {
		XSetForeground(dpy, gc, imagecolor);
		buf[0] = ximg->monochar;
		/*	    sy += uimg->font->max_bounds.ascent;  */
		/* need to set font as the one in use here? */
		XDrawString(dpy, win, gc, sx, sy, buf, 1);
	    }
	} else
	  desperate = TRUE;
    } else
      desperate = TRUE;
    if (desperate) {
	/* a bad error? */
	XSetClipOrigin(dpy, gc, sx, sy);
	/* Pretty desperate, or else we're at a scale that doesn't matter. */
	/* should actually clip to cell boundaries, maybe even inside
	   border terrain if necessary? */
	XSetClipMask(dpy, gc, None);
	/* Draw an edge, at least for larger mags. */
	if (sw >= 4) {
	    XSetForeground(dpy, gc, maskcolor);
	    XFillRectangle(dpy, win, gc, sx, sy, sw, sh);
	}
	/* Draw a filled box inside the edge. */
	XSetForeground(dpy, gc, imagecolor);
	XFillRectangle(dpy, win, gc, sx + 1, sy + 1, sw - 2, sh - 2);
	/* Might be cool to draw unit type name or char */
    }
    if (between(0, s2, numsides)) {
	if (1 /* emblem is not already part of image */) {
	    if (0 /* size is specified with image */) {
	    } else {
		ew = min(sw, max(8, sw / 4));  eh = min(sh, max(8, sh / 4));
	    }
	    if (0 /* pos is specified with image */) {
	    } else {
		ex = sw - ew;  ey = 0;
	    }
	    draw_side_emblem(side, win, sx + ex, sy + ey, ew, eh, s2, 0);
	}
    }
}

/* Draw an emblem identifying the given side.  If a side does not have a
   distinguishing emblem, fall back on some defaults. */

void
draw_side_emblem(side, win, ex, ey, ew, eh, s2, style)
Side *side;
Window win;
int ex, ey, ew, eh, s2, style;
{
    int ex2, ey2;
    long imagecolor, maskcolor;
    Image *img;
    X11Image *ximg;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->emblgc;

    /* Draw the emblem's mask, or else an enclosing box. */
    img = best_image(side->ui->eimages[s2], ew, eh);
    if (img != NULL) {
	/* Offset the image to draw in the middle of its area,
	   whether larger or smaller than the given area. */
	ex2 = ex + (ew - img->w) / 2;  ey2 = ey + (eh - img->h) / 2;
	/* Only change the size of the rectangle being drawn if it's
	   smaller than what was passed in. */
	if (img->w < ew) {
	    ex = ex2;
	    ew = img->w;
	}
	if (img->h < eh) {
	    ey = ey2;
	    eh = img->h;
	}
    }
    if (style == 1) {
	XSetForeground(dpy, gc, side->ui->fgcolor);
	XDrawRectangle(dpy, win, gc, ex - 1, ey - 1, ew + 2, eh + 2);
    }
    /* If an image is available, display it, otherwise do nothing. */
    if (img != NULL) {
	ximg = (X11Image *) img->hook;
	/* Decide on the colors to use with the emblem. */
	if (side->ui->numcolors[s2] > 0) {
	    imagecolor = side->ui->colors[s2][0];
	} else {
	    imagecolor = side->ui->blackcolor;
	}
	if (side->ui->numcolors[s2] > 1) {
	    maskcolor = side->ui->colors[s2][1];
	} else {
	    maskcolor = side->ui->whitecolor;
	}
	/* Draw the mask. */
	XSetForeground(dpy, gc, maskcolor);
	XSetClipOrigin(dpy, gc, ex, ey);
	if (ximg != NULL && ximg->mask != None) {
	    XSetClipMask(dpy, gc, ximg->mask);
	    XFillRectangle(dpy, win, gc, ex, ey, ew, eh);
	} else {
	    XSetClipMask(dpy, gc, None);
	    XFillRectangle(dpy, win, gc, ex - 1, ey, ew + 1, eh + 1);
	}
	/* Now draw the emblem proper. */
	/* (should fix so some color emblems can be used with mono displays) */
	if (!side->ui->monochrome
	    && side->ui->dflt_color_embl_images
	    && ximg != NULL
	    && ximg->colr != None) {
	    /* Draw the color image. */
	    XCopyArea(dpy, ximg->colr, win, gc, 0, 0, ew, eh, ex, ey);
	} else {
	    XSetForeground(dpy, gc, imagecolor);
	    XSetClipMask(dpy, gc, (ximg != NULL ? ximg->mono : None));
	    XFillRectangle(dpy, win, gc, ex, ey, ew, eh);
	}
    }
}

static void
draw_country_border_line(side, win, sx, sy, dir, power)
Side *side;
Window win;
int sx, sy, dir, power;
{
    int wid = bwid[power];
    GC gc = side->ui->bdrygc;
    Display *dpy = side->ui->dpy;

    if (wid == 0)
      return;
    wid = max(1, wid / 2);
    XSetForeground(dpy, gc, side->ui->whitecolor);
    XSetClipMask(dpy, gc, None);
    XSetLineAttributes(dpy, gc, wid, LineSolid, CapButt, JoinMiter); 
    XDrawLine(dpy, win, gc,
	      sx + bsx[power][dir], sy + bsy[power][dir],
	      sx + bsx[power][dir+1], sy + bsy[power][dir+1]);
}

/* The cursor icon highlights a unit of interest - should be called a
   "selection highlight" or something similar. */

static void
draw_cursor_icon(side, win, sx, sy, sw, sh)
Side *side;
Window win;
int sx, sy, sw, sh;
{
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->unitgc;

    if (sw == 16 && sh == 16) {
	/* At 16x16, use a special pair of bitmaps. */
	XSetClipOrigin(dpy, gc, sx, sy);
	XSetForeground(dpy, gc, side->ui->whitecolor);
	XSetClipMask(dpy, gc, side->ui->boxmask);
	XFillRectangle(dpy, win, gc, sx, sy, sw, sh);
	XSetForeground(dpy, gc, side->ui->blackcolor);
	XSetClipMask(dpy, gc, side->ui->boxcurs);
	XFillRectangle(dpy, win, gc, sx, sy, sw, sh);
    } else {
	/* Box at larger sizes need to come in slightly, otherwise we
	   get "dirt" pixels in the hex grid. */
	if (sw > 16 && sh > 16)
	  sh -= 1;
	XSetClipMask(dpy, gc, None);
	XSetForeground(dpy, gc, side->ui->whitecolor);
	XDrawRectangle(dpy, win, gc, sx, sy, sw - 1, sh - 1);
	XSetForeground(dpy, gc, side->ui->blackcolor);
	XDrawRectangle(dpy, win, gc, sx + 1, sy + 1, sw - 3, sh - 3);
	/* If the box is not too small, draw an inner white rectangle. */
	if (sw > 4 && sh > 4) {
	    XSetForeground(dpy, gc, side->ui->whitecolor);
	    XDrawRectangle(dpy, win, gc, sx + 2, sy + 2, sw - 5, sh - 5);
	}
    }
}

/* Describe the state of the given unit, in maximal detail. */

void
draw_map_info(side, map)
Side *side;
Map *map;
{
    char infobuf[BUFSIZE], *featurename;
    int i, u, s, m, t, mrow, nums[MAXUTYPES];
    int x = map->curx, y = map->cury, uview;
    Unit *unit, *occ, *mainunit;
    Side *side2;

    XClearWindow(side->ui->dpy, map->infowin);
    unit = map->curunit;
    if (!in_play(unit)) {
	if (inside_area(x, y)) {
	    t = terrain_at(x, y);
	    u = NONUTYPE;
	    if (map->seeall || units_visible(side, x, y)) {
		unit = unit_at(x, y);
		if (unit != NULL) {
		    u = unit->type;
		    side2 = unit->side;
		}
	    } else {
		uview = unit_view(side, x, y);
		if (uview != EMPTY) {
		    u = vtype(uview);  s = vside(uview);
		    side2 = side_n(s);
		}
	    }
	    if (u != NONUTYPE) {
		sprintf(infobuf, "%s %s",
			       side_adjective(side2), u_type_name(u));
		draw_info_text(side, map, 0, 0, 40, infobuf);
	    }
	    location_desc(infobuf, side, NULL, u, x, y);
	    draw_info_text(side, map, 0, 1, 40, infobuf);
	} else {
	    /* (should never happen?) */
	}
	return;
    }
    u = unit->type;
    /* Say which unit this is. */
    sprintf(infobuf, "%s", unit_handle(side, unit));
    draw_info_text(side, map, 0, 0, 40, infobuf);
    location_desc(infobuf, side, unit, u, x, y);
    draw_info_text(side, map, 0, 1, 40, infobuf);
    /* Very briefly list the numbers and types of the occupants. */
    infobuf[0] = '\0';
    if (unit->occupant != NULL) {
	strcpy(infobuf, "Occ ");
	for_all_unit_types(i)
	  nums[i] = 0;
	for_all_occupants(unit, occ)
	  ++nums[occ->type];
	for_all_unit_types(i) {
	    if (nums[i] > 0) {
		tprintf(infobuf, "%d %1s  ", nums[i], utype_name_n(i, 1));
	    }
	}
    }
    draw_info_text(side, map, 0, 2, 40, infobuf);
    /* Display the "important" parameters. */
    /* (should say something about parts?) */
    hp_desc(infobuf, unit, TRUE);
    strcat(infobuf, "   ");
    acp_desc(tmpbuf, unit, TRUE);
    strcat(infobuf, tmpbuf);
    draw_info_text(side, map, 50, 0, -1, infobuf);
    /* (should display cxp and morale here also, if defined) */
    infobuf[0] = '\0';
    /* (should make generic, put into nlang.c) */
    mainunit = unit_at(x, y);
    if (mainunit != NULL && mainunit->nexthere != NULL) {
	strcpy(infobuf, "Others here ");
	for_all_unit_types(i)
	  nums[i] = 0;
	for_all_stack(x, y, occ)
	  if (occ != unit)
	    ++nums[occ->type];
	for_all_unit_types(i) {
	    if (nums[i] > 0) {
		tprintf(infobuf, "%d %1s  ", nums[i], utype_name_n(i, 1));
	    }
	}
    }
    if (strlen(infobuf) > 0)
      draw_info_text(side, map, 50, 1, -1, infobuf);
    /* Describe the state of all the supplies. */
    /* Start on the third line (leaving the second line blank, for
       readability), unless there are lots of supply types that we'll
       need to display. */
    mrow = 2; /* (nummtypes > 4 ? 1 : 2)  but need to account for also heres */
    infobuf[0] = '\0';
    for_all_material_types(m) {
	if (um_storage_x(u, m) > 0) {
	    tprintf(infobuf, "%s %d/%d  ",
		    m_type_name(m), unit->supply[m], um_storage_x(u, m));
	    if (m > 0 && m % 2 == 0) {
		draw_info_text(side, map, 50, mrow++, -1, infobuf);
		infobuf[0] = '\0';
	    }
	}
    }
    /* Do the last row of supply info, if any left to put out. */
    if (strlen(infobuf) > 0)
      draw_info_text(side, map, 50, mrow, -1, infobuf);
    /* Describe the current plan and task agenda. */
    if (unit->plan) {
	int row = 4;
	Task *task;

	plan_desc(infobuf, unit);
	draw_info_text(side, map, 0, 3, 40, infobuf);
	for (task = unit->plan->tasks; task != NULL; task = task->next) {
	    task_desc(infobuf, task);
	    draw_info_text(side, map, 0, row++, 40, infobuf);
	}
    }
}

/* Display improvement can be achieved by padding out lines with blanks,
   then the lines need not be cleared before redrawing. */

static void
draw_info_text(side, map, x, y, len, buf)
Side *side;
Map *map;
int x, y, len;
char *buf;
{
    int sx, sy;

    /* Translate a 0-100 value for x to pixels. */
    if (x == 50)
      sx = map->pxw / 2;
    else
      sx = 2;
    sy = y * side->ui->fh;
    if (len > 0 && strlen(buf) > len)
      buf[len-1] = '\0';
    draw_text(side, map->infowin, sx, sy, buf, side->ui->fgcolor);
}

/* Write onto the list of sides. */

void
draw_map_sides(side, map)
Side *side;
Map *map;
{
    Side *side2;

    if (map == NULL)
      return;
    for_all_sides(side2) {
	draw_side_info(side, map, side2);
    }
}

/* Show info about a single side to some other side. */

void
draw_side_info(side, map, side2)
Side *side, *side2;
Map *map;
{
    char tmpbuf[BUFSIZE];
    int sx, sy, fh = side->ui->fh;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->gc;

    if (map == NULL || side2 == NULL)
      return;
    sx = 2 + 16 + 2;
    sy = (side_number(side2) - 1) * map->sidespacing;
    draw_side_emblem(side, map->sideswin, 2 + 2, sy + 4, 8, 8,
		     side_number(side2), 1);
    /* Build up and write the textual description of the side. */
    tmpbuf[0] = '\0';
#ifdef DESIGNERS
    if (side2->designer)
      strcat(tmpbuf, "(designer)");
#endif /* DESIGNERS */
    strcat(tmpbuf, short_side_title(side2));
    if (side2->player) {
	strcat(tmpbuf, "(");
	short_player_title(tmpbuf+strlen(tmpbuf), side2->player, NULL);
	strcat(tmpbuf, ")");
    }
    draw_text(side, map->sideswin, sx, sy, tmpbuf,
    	      (side2 == side ? side->ui->bgcolor : side->ui->fgcolor));
    if (side_won(side2)) {
	/* what to do here? */
    } else if (side_lost(side2)) {
	/* Draw the line of ignominy through sides that have lost. */
	XSetForeground(dpy, gc, side->ui->fgcolor);
	XDrawLine(dpy, map->sideswin, gc,
		  0, sy + fh / 2, 500, sy + fh / 2);
    } else if (side2->ingame) {
	draw_side_progress(side, map, side2);
    } else {
	/* what to do here? */
    }
}

/* Display how far along the side is with moving its units. */

void
draw_side_progress(side, map, side2)
Side *side, *side2;
Map *map;
{
    int sx, sy, totacp, left = 0;
    Display *dpy = side->ui->dpy;
    GC gc = side->ui->gc;

    if (map == NULL || side2 == NULL)
      return;
    sx = 2 + 16 + 2;
    sy = (side_number(side2) - 1) * map->sidespacing;
    XSetClipMask(dpy, gc, None);
    totacp = side_initacp(side2);
    if (totacp > 0) {
	left = (100 * side_acp(side2)) / totacp;
	left = max(0, min(100, left));
    }
    /* Clear the bar. */
    XSetForeground(dpy, gc, side->ui->bgcolor);
    XFillRectangle(dpy, map->sideswin, gc,
		   sx + 1 + 0, sy + side->ui->fh + 2, 100, 10);

    /* And show what we haven't used yet... */
    if (left > 0) {
	XSetForeground(dpy, gc, side->ui->fgcolor);
	if (side2->finishedturn
	    || !(side_has_ai(side2) || side_has_display(side2))) {
	    XSetFillStyle(dpy, gc, FillOpaqueStippled);
	    XSetStipple(dpy, gc, side->ui->grays[gray]);
	}
	XFillRectangle(dpy, map->sideswin, gc,
		       sx + 1 + 0, sy + side->ui->fh + 2, left, 10);
	if (side2->finishedturn
	    || !(side_has_ai(side2) || side_has_display(side2))) {
	    XSetFillStyle(dpy, gc, FillSolid);
	}
    }

    /* Always frame the progress bar. */
    XSetForeground(dpy, gc, side->ui->fgcolor);
    XDrawRectangle(dpy, map->sideswin, gc,
		   sx, sy + side->ui->fh + 1, 100 + 1, 10 + 1);
}
