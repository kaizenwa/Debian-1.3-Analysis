/* Map graphics for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern void location_desc PARAMS ((char *buf, Side *side, Unit *unit, int u, int x, int y));
extern int supply_desc PARAMS ((char *buf, Unit *unit, int mrow));
#include "macconq.h"
extern int gamewinw;
extern int topunithgt;
extern void draw_unit_info(Map *map);
extern void draw_unit_blast(Map *map, Unit *unit, int blast);
extern void clear_unit_blast(Map *map, Unit *unit, int blast);

static void draw_borders(Map *map, int x, int y, int b);
static void draw_connections(Map *map, int x, int y, int c);
static void draw_units(Map *map, int x, int y);
static void draw_unit_and_occs(Map *map, Unit *unit, int sx, int sy, int sw, int sh);
static void draw_people_row(Map *map, int x0, int y0, int len);
static void draw_materials(Map *map, int x, int y);
static void draw_ai_region(Map *map, int x, int y);
static void draw_info_text(Map *map, int x, int y, int len, char *buf);

/* For these accessors, x must already be wrapped. */

#define cell_overlay(x, y)  \
  (tmpdrawlighting  \
   ? (night_at(x, y) ? -2 : (tmpdrawcoverage ? (cover(dside, x, y) == 0 ? -1 : 0) : 0))  \
   : (tmpdrawcoverage ? (cover(dside, x, y) == 0 ? -1 : 0) : 0))

#define cell_terrain(x, y, power)  \
  (dside->see_all ? terrain_at(x, y)  \
   : (terrain_visible(dside, x, y) ? vterrain(terrain_view(dside, x, y)) : NONTTYPE))

#define cell_style(x, y, power)  \
  ((terrain_visible(dside, x, y) || unseen_image != NULL) ? (power >= 4 ? usepolygons : useblocks) : dontdraw);

#define SHIFT_ORIGIN(m) {  \
  (m)->osx = (m)->vp->sx - (m)->conw;  (m)->osy = (m)->vp->sy - (m)->toph;  \
  SetOrigin((m)->osx, (m)->osy);  \
  }

#define RESET_ORIGIN(m) {  \
  (m)->osx = 0;  (m)->osy = 0;  \
  SetOrigin((m)->osx, (m)->osy);  \
  }

static void draw_cliffs(Map *map, int x, int y);
static void draw_contours(Map *map, int x, int y);

int tmpdrawlighting;

int tmpdrawcoverage;

/* The width of the left-side control panel. */

int conwid = 32;

/* The height of the top line. */

int tophgt = 16;

int mapnum = 1;

int nummaps = 0;

int lastmaph = -1, lastmapv = -1;

/* Handles of the pictures that display all the map control buttons. */

PicHandle tlcontrols = nil;
PicHandle blcontrols = nil;

char *mouseover = "something";

int animation_pattern_state;

/* This tests whether the given cell might possibly be visible on the given map.
   Not as precise as a real cliprect calculation. */

int
at_all_visible(Map *map, int x, int y)
{
	return cell_is_visible(map->vp, x, y);
}

/* Decide whether given location is away from the edge of the map's window. */

int
in_middle(Map *map, int x, int y)
{
	return cell_is_in_middle(map->vp, x, y);
}

/* Draw an individual detailed cell, as a row of one, on all maps. */

void  
update_cell_display(Side *side, int x, int y, int rightnow)
{
	int i;
	Unit *unit;
	GrafPtr oldport;
	Map *map;
	RgnHandle tmprgn;
	
	if (active_display(side)) {
		GetPort(&oldport);
		for_all_maps(map) {
			/* If update was only to report changes in undrawn info, maybe don't bother. */
			if ((rightnow == 34 && !map->drawtemperature) 
			    || (rightnow == 35 && !map->drawwinds)
			    || (rightnow == 36 && !map->drawcover)
			    )
			  continue;
			if (at_all_visible(map, x, y)) {
				/* Set up the drawing context. */
				SetPort(map->window);
				tmprgn = NewRgn();
				GetClip(tmprgn);
				/* Clip to the content area of the map's window. */
				ClipRect(&(map->contentrect));
				SHIFT_ORIGIN(map);
				/* Draw the cell. */
				draw_row(map, x, y, 1, TRUE);
				/* Draw any selections that are here. */
#if 1 /* (should eventually go away, when basic drawing does this right) */				
				for (i = 0; i < map->numselections; ++i) {
					unit = map->selections[i];
					if (unit && unit->x == x && unit->y == y) {
						draw_selected_unit(map, unit);
					}
				}
#endif
				RESET_ORIGIN(map);
				SetClip(tmprgn);
				DisposeRgn(tmprgn);
				if (map->numselections > 0) {
					unit = map->selections[0];
					if (in_play(unit) && unit->x == x && unit->y == y) {
						draw_unit_info(map);
					}
				}
			}
		}
		SetPort(oldport);
	}
}

/* Create a new map window and all its paraphernalia. */

Map *
create_map(int power)
{
	int m, t, i, x, y, mainwidth, mainheight;
	Rect vscrollrect, hscrollrect;
	Map *map;
	WindowPtr win;

	build_optional_terrain_type_menu();
	DGprintf("Creating map, mag power %d\n", power);
	map = (Map *) xmalloc(sizeof(Map));
	map->vp = new_vp();
	calc_vision(dside);
	set_map_power(map, power);
	/* Pick an appropriate focus of the view. */
	pick_a_focus(dside, &x, &y);
	set_view_focus(map->vp, x, y);
	/* Set default values for the display controls. */
	map->conw = conwid;
	map->toplineh = tophgt;
	map->topunith = topunithgt;
	map->toph = map->toplineh + map->topunith;
	map->drawterrain = TRUE;
	/* Display all types of borders and connections normally. */
	/* (auxterraintypes[t] == 0 implies not optional type - i can't overflow
	   drawauxterrain because at least one ttype must be cell terrain.) */
	/* (Addressing of map->auxterraintypes is 1-based instead of 0-based
	    because it corresponds to menu items.) */
	i = 1;
	for_all_terrain_types(t) {
		if (!t_is_cell(t)) {
			map->auxterraintypes[i++] = t;
			map->drawauxterrain[t] = TRUE;
		}
	}
	map->drawcellpats = TRUE;  /* should get from prefs */
	map->drawunits = TRUE;
	map->drawnames = default_draw_names;
	map->drawpeople = FALSE;
	map->drawelevations = FALSE;
	for_all_material_types(m) {
		map->drawmaterials[m] = FALSE;
	}
	map->nummaterialstodraw = 0;
	map->drawlighting = TRUE;
	map->drawtemperature = FALSE;
	map->drawwinds = FALSE;
	map->drawclouds = FALSE;
	map->drawstorms = TRUE;
	map->drawgrid = default_draw_grid;
	/* Display AI info by default if there is an AI present. */
	map->drawai = side_has_ai(dside);
	/* Don't indicate other maps by default - too confusing initially. */
	map->drawothermaps = FALSE;
	map->autoselect = defaultautoselect;
	map->moveonclick = defaultmoveonclick;
	map->numselections = 0;
	map->maxselections = max(100, numunits + numunits / 2);
	map->selections = (Unit **) xmalloc(map->maxselections * sizeof(Unit *));
	/* Newest map goes on the front of the list. */
	map->next = maplist;
	maplist = map;
	if (hasColorQD) {
		win = GetNewCWindow(wMap, nil, (WindowPtr) -1L);
	} else {
		win = GetNewWindow(wMap, nil, (WindowPtr) -1L);
	}
	map->window = win;
	stagger_window(win, &lastmaph, &lastmapv);
	if (first_windows) {
		get_main_screen_size(&mainwidth, &mainheight);
		SizeWindow(win,
				   mainwidth - gamewinw - 10,
				   mainheight - GetMBarHeight() - 16 - 150 - 7,
				   FALSE);
	}
	ShowWindow(win);
	SetPort(win);
	sprintf(spbuf, "Map %d", mapnum++);
	add_window_menu_item(spbuf, win);
	set_content_rect(map);
	m_center_on_focus(map);
	/* Make the scrollbars. */
	vscrollrect = map->window->portRect;
	vscrollrect.top -= 1;
	vscrollrect.bottom -= sbarwid - 1;
	vscrollrect.left = vscrollrect.right - sbarwid;
	vscrollrect.right += 1;
	map->vscrollbar =
		NewControl(win, &vscrollrect, "\p", 1,
			 map->vp->sy, 0, max(0, map->vp->totsh - map->vp->pxh), scrollBarProc, 0L);
	hscrollrect = win->portRect;
	hscrollrect.top = hscrollrect.bottom - sbarwid;
	hscrollrect.bottom += 1;
	hscrollrect.left += map->conw;
	hscrollrect.right -= sbarwid - 1;
	map->hscrollbar =
		NewControl(win, &hscrollrect, "\p", 1,
			 map->vp->sx, 0, max(0, map->vp->totsw - map->vp->pxw), scrollBarProc, 0L);
	set_map_scrollbars(map);
	++nummaps;
	return map;
}

/* Compute the content part of the map window. */

void
set_content_rect(Map *map)
{
	map->contentrect = map->window->portRect;
	map->contentrect.left += map->conw;  map->contentrect.top += map->toph;
	map->contentrect.right -= sbarwid;  map->contentrect.bottom -= sbarwid;
	set_view_size(map->vp,
				  map->contentrect.right - map->contentrect.left,
				  map->contentrect.bottom - map->contentrect.top);
	OffsetRect(&(map->contentrect), map->vp->sx - map->conw, map->vp->sy - map->toph);
}

void
m_focus_on_center(Map *map)
{
	focus_on_center(map->vp);
}

/* Put vcx,vcy in the middle of the map. */

void
m_center_on_focus(Map *map)
{
	center_on_focus(map->vp);
	set_content_rect(map);
}

/* Adjust the appearance and thumb of the scroll bars to reflect the map.  This is
   needed whenever the map is scrolled under program control, such as when magnifying
   or scrolling to a specified location. */

void
set_map_scrollbars(Map *map)
{
	int sx, sy, hilite;
	int hexadj = hexagon_adjust(map->vp);

	sx = map->vp->sx;  sy = map->vp->sy;
	if (map->vp->pxw < (map->vp->totsw - hexagon_adjust(map->vp))) {
		if (sx < hexadj)
		  sx = hexadj;
		/* Set horiz min so that leftmost cell in area is at edge of window. */
		SetCtlMin(map->hscrollbar, hexadj);
		/* Wrapped-around areas need extra room to look at cells on the seam. */
		SetCtlMax(map->hscrollbar, map->vp->totsw - (area.xwrap ? 0 : map->vp->pxw));
		SetCtlValue(map->hscrollbar, sx);
		hilite = TRUE;
	} else {
		/* Force sx to a default value and disable the scrollbar. */
		sx = hexadj;
		hilite = FALSE;
	}
	/* Adjust the hiliting of the scrollbar, but only if the window is in front,
	   otherwise the scrollbar should remain unhilited. */
	if (map->window == FrontWindow()) {
		HiliteControl(map->hscrollbar, (hilite ? 0 : 255));
	}
	DGprintf("Hscroll (%shilite) is %d -- %d -- %d\n",
			 (hilite ? "" : "no "), GetCtlMin(map->hscrollbar),
	         GetCtlValue(map->hscrollbar), GetCtlMax(map->hscrollbar));
	if (map->vp->pxh < map->vp->totsh) {
		/* Vertical scrollbar min is always zero. */
		SetCtlMax(map->vscrollbar, map->vp->totsh - map->vp->pxh);
		/* Constrain the scaled y position of the map.  This keeps the computed
		   y from exceeding what the scroll bar will allow (which happens because
		   the sy calcs don't take scroll bar limits into account, should be fixed). */
		if (sy > map->vp->totsh - map->vp->pxh)
		  sy = map->vp->totsh - map->vp->pxh;
		SetCtlValue(map->vscrollbar, sy);
		hilite = TRUE;
	} else {
		/* Force sy to the top of the map and disable the scrollbar. */
		sy = 0;
		hilite = FALSE;
	}
	if (map->window == FrontWindow()) {
		HiliteControl(map->vscrollbar, (hilite ? 0 : 255));
	}
	set_view_position(map->vp, sx, sy);
	set_content_rect(map);
	DGprintf("Vscroll (%shilite) is %d -- %d -- %d\n",
			 (hilite ? "" : "no "), GetCtlMin(map->vscrollbar),
	         GetCtlValue(map->vscrollbar), GetCtlMax(map->vscrollbar));
}

/* Given a magnification power, look up and/or calculate the sizes of everything,
   in pixels. */

void
set_map_power(Map *map, int power)
{
	set_view_power(map->vp, power);
	if (power >= 4 && cellrgns[power] == nil)
	  make_cell_clip(power);
}

/* Given a magnification power, compute the clipping regions to be used at
   that power. */

void
make_cell_clip(int power)
{
	int hw = hws[power], hh = hhs[power], delt = (hhs[power] - hcs[power]);
	PolyHandle poly;
	RgnHandle tmprgn;

	/* Make a hexagon region by drawing a polygon and then framing it while
	   a region is open. */
	poly = OpenPoly();
	MoveTo(hw / 2, 0);
	LineTo(hw, delt);
	LineTo(hw, hh - delt);
	LineTo(hw / 2, hh);
	LineTo(0, hh - delt);
	LineTo(0, delt);
	LineTo(hw / 2, 0);
	ClosePoly();
	cellrgns[power] = NewRgn();
	OpenRgn();
	FramePoly(poly);
	CloseRgn(cellrgns[power]);
	/* Make the grid-displaying version of the hexagon. */
	gridcellrgns[power] = NewRgn();
	CopyRgn(cellrgns[power], gridcellrgns[power]);
	/* Cut off a one-pixel line on the side. */
	tmprgn = NewRgn();
	SetRectRgn(tmprgn, hw - 1, 0, hw + 1, hh); 
	DiffRgn(gridcellrgns[power], tmprgn, gridcellrgns[power]);
	/* Now intersect with a region shifted upwards by one, which makes
	   the grid line along the bottom of the hex. */
	tmprgn = NewRgn();
	CopyRgn(cellrgns[power], tmprgn);
	OffsetRgn(tmprgn, 0, -1);
	SectRgn(gridcellrgns[power], tmprgn, gridcellrgns[power]);

	/* Similarly, but for cells at an angle. */
	/* (should only calc when angle view first requested) */
	/* First make a region as viewed at a 30-degree angle. */
	hh /= 2;  delt /= 2;
	poly = OpenPoly();
	MoveTo(hw / 2, 0);
	LineTo(hw, delt);
	LineTo(hw, hh - delt);
	LineTo(hw / 2, hh);
	LineTo(0, hh - delt);
	LineTo(0, delt);
	LineTo(hw / 2, 0);
	ClosePoly();
	cellrgns30[power] = NewRgn();
	OpenRgn();
	FramePoly(poly);
	CloseRgn(cellrgns30[power]);
	gridcellrgns30[power] = NewRgn();
	CopyRgn(cellrgns30[power], gridcellrgns30[power]);
	/* Cut off a one-pixel line on the side. */
	tmprgn = NewRgn();
	SetRectRgn(tmprgn, hw - 1, 0, hw + 1, hh/2); 
	DiffRgn(gridcellrgns30[power], tmprgn, gridcellrgns30[power]);
	/* Now intersect with a region shifted upwards by one, which makes
	   the grid line along the bottom of the hex. */
	tmprgn = NewRgn();
	CopyRgn(cellrgns30[power], tmprgn);
	OffsetRgn(tmprgn, 0, -1);
	SectRgn(gridcellrgns30[power], tmprgn, gridcellrgns30[power]);

	/* Now make a region as viewed at a 15-degree angle. */
	hh = hhs[power] / 4;
	delt = (hhs[power] - hcs[power]) / 4;
	poly = OpenPoly();
	MoveTo(hw / 2, 0);
	LineTo(hw, delt);
	LineTo(hw, hh - delt);
	LineTo(hw / 2, hh);
	LineTo(0, hh - delt);
	LineTo(0, delt);
	LineTo(hw / 2, 0);
	ClosePoly();
	cellrgns15[power] = NewRgn();
	OpenRgn();
	FramePoly(poly);
	CloseRgn(cellrgns15[power]);
	gridcellrgns15[power] = NewRgn();
	CopyRgn(cellrgns15[power], gridcellrgns15[power]);
	/* Cut off a one-pixel line on the side. */
	tmprgn = NewRgn();
	SetRectRgn(tmprgn, hw - 1, 0, hw + 1, hh/2); 
	DiffRgn(gridcellrgns15[power], tmprgn, gridcellrgns15[power]);
	/* Now intersect with a region shifted upwards by one, which makes
	   the grid line along the bottom of the hex. */
	tmprgn = NewRgn();
	CopyRgn(cellrgns15[power], tmprgn);
	OffsetRgn(tmprgn, 0, -1);
	SectRgn(gridcellrgns15[power], tmprgn, gridcellrgns15[power]);
}

/* Given a window, find the map that it belongs to. */

Map *
map_from_window(WindowPtr window)
{
	Map *map;
	
	if (dside == NULL)
	  return NULL;
	for_all_maps(map) {
		if (map->window == window)
		  return map;
	}
	return NULL;
}

/* Set the size of the map window and position its scrollbars correctly. */

void
grow_map(Map *map, int w, int h)
{
	int oldsx = map->vp->sx, oldsy = map->vp->sy;
	int oldpxw = map->vp->pxw, oldpxh = map->vp->pxh;
	Rect tmprect;
	WindowPtr mapwin = map->window;

	SizeWindow(mapwin, w, h, FALSE);
	adjust_map_decor(map);
	set_content_rect(map);
	set_map_scrollbars(map);
	if (map->vp->sx == oldsx && map->vp->sy == oldsy) {
		if (map->vp->pxh > oldpxh) {
		    SetRect(&tmprect, 0, oldpxh, map->vp->pxw, map->vp->pxh);
		    OffsetRect(&tmprect, map->conw, map->toph);
			InvalRect(&tmprect);
		}
		if (map->vp->pxw > oldpxw) {
		    SetRect(&tmprect, oldpxw, 0, map->vp->pxw, map->vp->pxh);
		    OffsetRect(&tmprect, map->conw, map->toph);
			InvalRect(&tmprect);
		}
	} else {
		/* Be conservative and update everything. */
		InvalRect(&mapwin->portRect);
	}
	/* Always need to update the controls. */
	SetRect(&tmprect, 0, 0, map->conw, mapwin->portRect.bottom);
	InvalRect(&tmprect);
	/* Always need to update the top area. */
	SetRect(&tmprect, 0, 0, mapwin->portRect.right, map->toph);
	InvalRect(&tmprect);
	draw_related_maps(map);
}

/* Map zooming actually does "rightsizing" if possible. */

void
zoom_map(Map *map, int part)
{
	WindowPtr mapwin = map->window;

	if (part == inZoomOut) {
		set_standard_state(mapwin,
						   area.width * map->vp->hw + map->conw + sbarwid + 1,
						   area.height * map->vp->hch + (map->vp->hh - map->vp->hch)
						     + sbarwid + map->toph + 1);
	}
	EraseRect(&mapwin->portRect);
	ZoomWindow(mapwin, part, true);
	adjust_map_decor(map);
	set_content_rect(map);
	/* (should try to minimize redraw here too) */
	InvalRect(&mapwin->portRect);
}

/* Move and size the controls to be correct for the map. */

void
adjust_map_decor(Map *map)
{				
	int w, h;
	WindowPtr mapwin = map->window;

	w = mapwin->portRect.right - mapwin->portRect.left;
	h = mapwin->portRect.bottom - mapwin->portRect.top;
/*	HideControl(map->hscrollbar); */
	MoveControl(map->hscrollbar, map->conw - 1, h - sbarwid);
	SizeControl(map->hscrollbar, w - map->conw - sbarwid + 2, sbarwid + 1);
/*	HideControl(map->vscrollbar); */
	MoveControl(map->vscrollbar, w - sbarwid, -1);
	SizeControl(map->vscrollbar, sbarwid + 1, h - sbarwid + 1 + 1);
}

/* Given a map and a cell, compute the pixel coords of the cell's UL corner.
   This is the core routine that relates cells and pixels. */

void
xform(map, x, y, sxp, syp)
Map *map;
int x, y, *sxp, *syp;
{
	xform_cell(map->vp, x, y, sxp, syp);
	/* Shift the basic viewport result to account for both origin-shift and map decor. */
	*sxp += map->osx + map->conw;  *syp += map->osy + map->toph;
}

void
m_xform_unit(map, unit, sxp, syp, swp, shp)
Map *map;
Unit *unit;
int *sxp, *syp, *swp, *shp;
{
	xform_unit(map->vp, unit, sxp, syp, swp, shp);
	/* Shift the basic viewport result to account for both origin-shift and map decor. */
	*sxp += map->osx + map->conw;  *syp += map->osy + map->toph;
}

void
m_xform_unit_self(map, unit, sxp, syp, swp, shp)
Map *map;
Unit *unit;
int *sxp, *syp, *swp, *shp;
{
	xform_unit_self(map->vp, unit, sxp, syp, swp, shp);
	/* Shift the basic viewport result to account for both origin-shift and map decor. */
	*sxp += map->osx + map->conw;  *syp += map->osy + map->toph;
}

void
m_xform_occupant(map, transport, unit, sx, sy, sw, sh, sxp, syp, swp, shp)
Map *map;
Unit *transport, *unit;
int sx, sy, sw, sh, *sxp, *syp, *swp, *shp;
{
	/* Transform the coordinates back to relative values. */
	sx -= map->osx + map->conw;  sy -= map->osy + map->toph;
	xform_occupant(map->vp, transport, unit, sx, sy, sw, sh, sxp, syp, swp, shp);
	/* Shift the basic viewport result to account for both origin-shift and map decor. */
	*sxp += map->osx + map->conw;  *syp += map->osy + map->toph;
}

/* Un-transform screen coordinates (as supplied by mouse perhaps) into
   map coordinates.  */

/* Note that only drawing is affected by SetOrigin - mouse locations are
   always window-relative, and thus only need to be adjusted by map decor
   before going to the generic routines. */

int
m_nearest_cell(Map *map, int sx, int sy, int *xp, int *yp)
{
	return nearest_cell(map->vp, sx - map->conw, sy - map->toph, xp, yp);
}

/* Find the closest direction of the closest boundary.  */

int
m_nearest_boundary(Map *map, int sx, int sy, int *xp, int *yp, int *dirp)
{
	return nearest_boundary(map->vp, sx - map->conw, sy - map->toph, xp, yp, dirp);
}

int
m_nearest_unit(Map *map, int sx, int sy, Unit **unitp)
{
	return nearest_unit(map->vp, sx - map->conw, sy - map->toph, unitp);
}

/* Display a map and all of its paraphernalia. */

void
draw_map(Map *map)
{
	Rect tmprect;
	WindowPtr mapwin = map->window;
	RgnHandle tmprgn;

	BackPat(QDPat(gray));
	EraseRect(&(map->window->portRect));
	/* Draw control panel and topline before clipping to inner part of window. */
	if (map->conw > 0)
	  draw_control_panel(map);
	if (map->toplineh > 0)
	  draw_top_line(map);
	if (map->topunith > 0)
	  draw_unit_info(map);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	draw_window_background(map);
	RESET_ORIGIN(map);
	/* Calculate shapes and sizes. */
	set_map_scrollbars(map);
	set_content_rect(map);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	draw_area_background(map);
	draw_map_content(map);
	RESET_ORIGIN(map);
	if (map->drawothermaps)
	  draw_other_maps(map);
	draw_selections(map);
#ifdef DEBUGGING
	/* Indicate where the focus is. */
	if (DebugG) {
		int sx, sy;

		ClipRect(&(map->contentrect));
		SHIFT_ORIGIN(map);
		xform(map, map->vp->vcx, map->vp->vcy, &sx, &sy);
		SetRect(&tmprect, sx, sy, sx + map->vp->hw, sy + map->vp->hh);
		InsetRect(&tmprect, -4, -4);
		InvertOval(&tmprect);
		InsetRect(&tmprect, 2, 2);
		InvertOval(&tmprect);
		RESET_ORIGIN(map);
	}
#endif /* DEBUGGING */
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
}

void
draw_window_background(Map *map)
{
	/* If part of the window is entirely outside the world, we draw its shape on
	   top of gray, otherwise window starts out all white. */
	if (area.width * map->vp->hw < 32000) {
		switch (bggray) {
			case blackgray:
				FillRect(&(map->contentrect), QDPat(black));  break;
			case darkgray:
				FillRect(&(map->contentrect), QDPat(dkGray));  break;
			case mediumgray:
				FillRect(&(map->contentrect), QDPat(gray));  break;
			case lightgray:
				FillRect(&(map->contentrect), QDPat(ltGray));  break;
			case whitegray:
				FillRect(&(map->contentrect), QDPat(white));  break;
		}
	} else {
		if (hasColorQD) {
			RGBForeColor((grid_matches_unseen ? &gridcolor : &unseencolor));
			PaintRect(&(map->contentrect));
			RGBForeColor(&blackcolor);
		} else {
			switch ((grid_matches_unseen ? gridgray : unseengray)) {
				case blackgray:
					FillRect(&(map->contentrect), QDPat(black));  break;
				case darkgray:
					FillRect(&(map->contentrect), QDPat(dkGray));  break;
				case mediumgray:
					FillRect(&(map->contentrect), QDPat(gray));  break;
				case lightgray:
					FillRect(&(map->contentrect), QDPat(ltGray));  break;
				case whitegray:
					FillRect(&(map->contentrect), QDPat(white));  break;
			}
		}
	}
}

/* Draw the actual map data.  This is basically a matter of drawing n rows of terrain,
   each of an appropriate length, but it's a bit of trouble to get the number and
   lengths right.  Actually, it's easy to get approximate sizes, but it's important
   to try to draw as few cells as humanly possible. */

void
draw_map_content(map)
Map *map;
{
	int y1, y2, y, x1, x2, xx1, yy1, xx2, yy2;
	int vx, vy, vw, vh;
	int halfheight = area.halfheight;
	int limitleft = FALSE, limitrite = FALSE;
	Rect bbox = (*(map->window->visRgn))->rgnBBox;
	Rect tmprect = map->contentrect;

	if (DebugG) {
		FillRgn(map->window->visRgn, QDPat(white));
	}
	/* Compute the size of the viewport.  Make sure it will extend past the edges
	   of the window, so that partial cells around the edges will be filled in. */
	vw = min(area.width, map->vp->pxw / map->vp->hw + 2);
	vh = min(area.height, map->vp->pxh / map->vp->hch + 2);
	/* Compute the bottom visible row. */
	vy = ((map->vp->totsh - map->vp->sy) / map->vp->hch) - vh;
	/* Now adjust the bottom row so it doesn't go outside the area. */
	if (vy < 0)
	  vy = 0;
	if (vy > area.height - vh)
	  vy = area.height - vh;
	/* Compute the leftmost "column". */
	vx = map->vp->sx / map->vp->hw - vy / 2 - 1;
	DGprintf("Set %dx%d viewport at %d,%d\n", vw, vh, vx, vy);
	/* Compute top and bottom rows to be displayed. */
	y1 = min(vy + vh, area.height - 1);
	y2 = vy;
	/* Find the top and bottom rows that are in the visRgn. */
	OffsetRect(&bbox, - map->vp->sx, - map->vp->sy);
	if (m_nearest_cell(map, bbox.left, bbox.top, &xx1, &yy1))
	  limitleft = TRUE;
	if (m_nearest_cell(map, bbox.right + map->vp->hw, bbox.bottom + map->vp->hh, &xx2, &yy2))
	  limitrite = TRUE;
	xx2 += 5;
	/* This fixes problem of missing rows - wish I knew why it worked :-( */
	yy2 -= 10;
	DGprintf("Map rows are %d - %d, update area rows are %d - %d\n", y2, y1, yy2, yy1);
	if (between(y2, yy1, y1))
	  y1 = yy1; 
	if (between(y2, yy2, y1))
	  y2 = yy2;
	/* Draw the rows, going from top to bottom. */
	for (y = y1; y >= y2; --y) {
		/* Adjust the right and left bounds to fill the viewport as
		   much as possible, without going too far (the drawing code
		   will clip, but clipped drawing is still expensive). */
		/* could test by drawing viewport rect as lines... */
		x1 = vx - (y - vy) / 2;
		x2 = x1 + vw + 2;
		/* If the area doesn't wrap, then we might have to stop
		   drawing before we reach the edge of the viewport. */
		if (area.xwrap) {
			/* (should clip to visrgn, but tricky to avoid wrapping bugs) */
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
			/* Clip the ends of the row to the visRgn. */
			if (limitleft && between(x1, xx1, x2))
			  x1 = xx1; 
			if (limitrite && between(x1, xx2, x2))
			  x2 = xx2;
		}
		draw_row(map, x1, y, x2 - x1, FALSE);
	}
}

/* This draws a hexagon or rectangle that covers the totality of the area, whether
   discovered or not. */

void 
draw_area_background(Map *map)
{
	int sx, sy, llx, lly, lrx, lry, rx, ry, urx, ury, ulx, uly, lx, ly;
	PolyHandle poly;
	Rect arearect;

	/* Don't bother if area magnified greatly. */
	/* (should fix to not try to draw giant rects, but still draw something reasonable.
	   note that otherwise grid color may be wrong) */
	if (area.width * map->vp->hw > 32000)
	  return;
	if (area.xwrap) {
		/* Area is cylinder; draw a rectangle. */
		xform(map, 0, area.height-1, &sx, &sy);
		arearect.left = 0;  arearect.top = sy;
		xform(map, 0, 0, &sx, &sy);
		arearect.right = area.width * map->vp->hw;  arearect.bottom = sy;
		/* Adjust so that edges of the rect pass through the middles of edge cells. */
		OffsetRect(&arearect, map->vp->hw/2, map->vp->hh/2);
		if (hasColorQD) {
			RGBForeColor(&gridcolor);
			PaintRect(&arearect);
			RGBForeColor(&blackcolor);
		} else {
			switch (gridgray) {
				case blackgray:
					FillRect(&arearect, QDPat(black));   break;
				case darkgray:
					FillRect(&arearect, QDPat(dkGray));  break;
				case mediumgray:
					FillRect(&arearect, QDPat(gray));    break;
				case lightgray:
					FillRect(&arearect, QDPat(ltGray));  break;
				case whitegray:
					FillRect(&arearect, QDPat(white));   break;
			}
		}
	} else {
		/* Area is hexagon; draw a hexagon. */
		/* (should make once and save?) */
		poly = OpenPoly();		
		xform(map, 0 + area.halfheight, 0, &llx, &lly);
		MoveTo(llx, lly);
		xform(map, area.width-1, 0, &lrx, &lry);
	 	LineTo(lrx, lry);
		xform(map, area.width-1, area.halfheight, &rx, &ry);
		LineTo(rx, ry);
 		xform(map, area.width-1 - area.halfheight, area.height-1, &urx, &ury);
		LineTo(urx, ury);
 		xform(map, 0, area.height-1, &ulx, &uly);
		LineTo(ulx, uly);
 		xform(map, 0, area.halfheight, &lx, &ly);
		LineTo(lx, ly);
		LineTo(llx, lly);
		ClosePoly();
		/* Adjust so that edges of the polygon pass through the middles of edge cells. */
		OffsetPoly(poly, map->vp->hw/2, map->vp->hh/2);
		if (hasColorQD) {
			RGBForeColor(&gridcolor);
			PaintPoly(poly);
			RGBForeColor(&blackcolor);
		} else {
			switch (gridgray) {
				case blackgray:
					FillPoly(poly, QDPat(black));   break;
				case darkgray:
					FillPoly(poly, QDPat(dkGray));  break;
				case mediumgray:
					FillPoly(poly, QDPat(gray));    break;
				case lightgray:
					FillPoly(poly, QDPat(ltGray));  break;
				case whitegray:
					FillPoly(poly, QDPat(white));   break;
			}
		}
		/* Free up the space for this hexagon. */
		KillPoly(poly);
	}
#if 0  /* The idea of a shaded border seems nice, but it doesn't look very good in practice. */
	for (x = 0; x < area.width; ++x) {
		xform(map, x, 0, &sx, &sy);
		draw_border_line(sx, sy, SW, map->vp->power, -1);
		draw_border_line(sx, sy, SE, map->vp->power, -2);
	}
	PenNormal();
#endif
}

/* Draw the map control panel as a pair of PICTs. */

void
draw_control_panel(map)
Map *map;
{
	int winhgt, basev;
	Rect tmprect;

	winhgt = (map->window->portRect).bottom - (map->window->portRect).top;
	SetRect(&tmprect, 0, 0, map->conw, winhgt);
	FillRect(&tmprect, QDPat(white));
	MoveTo(map->conw - 1, 0);  Line(0, winhgt);
	if (tlcontrols == nil) {
		tlcontrols = (PicHandle) GetResource('PICT', pMapControlsTL);
	}
	if (tlcontrols != nil) {
		SetRect(&tmprect, 0, 0,
				picture_width(tlcontrols), picture_height(tlcontrols));
		DrawPicture(tlcontrols, &tmprect);
	}
	if (blcontrols == nil) {
		blcontrols = (PicHandle) GetResource('PICT', pMapControlsBL);
	}
	if (blcontrols != nil) {
		SetRect(&tmprect, 0, winhgt - picture_height(blcontrols) + 1,
				picture_width(blcontrols), winhgt);
		DrawPicture(blcontrols, &tmprect);
	}
	if (map->moveonclick && map->autoselect) {
		SetRect(&tmprect, 4, 5, 26, 26);
		InvertRect(&tmprect);
	}
	/* (should modify appearance of top left arrow buttons to reflect abilities) */
	basev = 32 + 5*15 + 2 + 5/*why?*/ + 1;
	SetRect(&tmprect, 0, basev, 30, basev + 10);
	if (map->drawgrid) {
		InvertRect(&tmprect);
	}
	OffsetRect(&tmprect, 0, 11);
	if (map->drawnames) {
		InvertRect(&tmprect);
	}
	OffsetRect(&tmprect, 0, 11);
	if (map->drawpeople) {
		InvertRect(&tmprect);
	} else if (!people_sides_defined()) {
		gray_out_rect(&tmprect);
	}
	OffsetRect(&tmprect, 0, 11);
	if (map->drawplans) {
		InvertRect(&tmprect);
	}
	OffsetRect(&tmprect, 0, 11);
	if (map->drawai) {
		InvertRect(&tmprect);
	} else if (!side_has_ai(dside)) {
		/* (should ensure that this is updated when side gets an AI) */
		gray_out_rect(&tmprect);
	}
	OffsetRect(&tmprect, 0, 11);
	if (dside->may_set_see_all && dside->see_all) {
		InvertRect(&tmprect);
	} else if (!dside->may_set_see_all) {
		/* (should ensure that this is updated when side gets an AI) */
		gray_out_rect(&tmprect);
	}
	/* Draw state of bottom left control buttons. */
	if (map->vp->power <= 0) {
		SetRect(&tmprect, 0, winhgt - 15, 15, winhgt);
		gray_out_rect(&tmprect);
	}
	if (map->vp->power >= NUMPOWERS - 1) {
		SetRect(&tmprect, 16, winhgt - 15, 30, winhgt);
		gray_out_rect(&tmprect);
	}
}

void
draw_top_line(Map *map)
{
	int numchars, strwid;
	Rect tmprect;

	/* Clear the whole topline area. */
	SetRect(&tmprect, map->conw, 0, map->conw + map->vp->pxw, map->toplineh);
	FillRect(&tmprect, QDPat(white));
	/* Draw a line dividing this from the map content. */
	MoveTo(tmprect.left, tmprect.bottom - 1);
	Line(tmprect.right - tmprect.left, 0);
	/* Draw the current date. */
	TextSize(10);
	strwid = TextWidth(curdatestr, 1, curdatestr[0]);
	MoveTo(tmprect.right - strwid - 3, tmprect.top + 11);
	numchars = curdatestr[0];
	DrawText(curdatestr, 1, numchars);
	if (mouseover != NULL) {
		/* Draw description of what the mouse is over. */
		/* (should clip to avail space) */
		numchars = strlen(mouseover);
		MoveTo(map->conw + 3, tmprect.top + 11);
		DrawText(mouseover, 0, numchars);
	}
}

void
draw_unit_info(Map *map)
{
	int mrow, u, m;
	Rect tmprect;
	Unit *unit;
	char infobuf[100];
	GrafPtr oldport;
	RgnHandle tmprgn;

	if (map->topunith <= 0)
	  return;
 	GetPort(&oldport);
	SetPort(map->window);
	/* Save the current clipping region. */
	tmprgn = NewRgn();
	GetClip(tmprgn);
	SetRect(&tmprect, map->conw, map->toplineh, map->conw + map->vp->pxw, map->toph);
	ClipRect(&tmprect);
	FillRect(&tmprect, QDPat(white));
	/* Draw a line dividing this from the map content. */
	MoveTo(tmprect.left, tmprect.bottom - 1);
	Line(tmprect.right - tmprect.left, 0);
	if (map->numselections == 1) {
		unit = map->selections[0];
		if (in_play(unit)) {
			u = unit->type;
			sprintf(infobuf, "%s", unit_handle(dside, unit));
			draw_info_text(map, 0, 0, 0, infobuf);
			location_desc(infobuf, dside, unit, unit->type, unit->x, unit->y);
			draw_info_text(map, 0, 1, 0, infobuf);
    		hp_desc(infobuf, unit, TRUE);
    		strcat(infobuf, "   ");
    		acp_desc(tmpbuf, unit, TRUE);
    		strcat(infobuf, tmpbuf);
			draw_info_text(map, 50, 0, 0, infobuf);
			mrow = 0;
			while (supply_desc(infobuf, unit, mrow)) {
				draw_info_text(map, 50, mrow + 1, 0, infobuf);
				++mrow;
			}
		    /* Describe the current plan and task agenda. */
		    if (unit->plan) {
				int row = 2;
				Task *task;

				plan_desc(infobuf, unit);
				draw_info_text(map, 0, row++, 0, infobuf);
				for_all_tasks(unit->plan, task) {
					task_desc(infobuf, task);
					draw_info_text(map, 0, row++, 0, infobuf);
				}
		    }
		}
	} else if (map->numselections > 1) {
		draw_info_text(map, 0, 0, 0, "(multiple units)");
	}
	/* Restore clipping region. */
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	/* Restore grafport. */
	SetPort(oldport);
}

static void
draw_info_text(map, x, y, len, buf)
Map *map;
int x, y, len;
char *buf;
{
	int sx, sy;
	Rect tmprect;

	SetRect(&tmprect, map->conw, map->toplineh, map->conw + map->vp->pxw, map->toph);
    /* Translate a 0-100 value for x to pixels. */
    if (x == 50)
      sx = tmprect.left + (tmprect.right - tmprect.left) / 2;
    else
      sx = tmprect.left + 3;
    sy = tmprect.top + 11 + y * 15;
    if (len > 0 && strlen(buf) > len)
      buf[len-1] = '\0';
	MoveTo(sx, sy);
	DrawText(buf, 0, strlen(buf));
}

/* Draw an indication of the position of other maps relative to this one. */

void
draw_other_maps(map)
Map *map;
{
	Map *map2;

	for_all_maps(map2) {
		if (map != map2 /* && reasonable to show? */) {
			draw_other_map(map, map2);
		}
	}
}

void
draw_related_maps(map)
Map *map;
{
	Map *map2;
	GrafPtr oldport;

	for_all_maps(map2) {
		if (map != map2 && map2->drawothermaps /* && reasonable to show? */) {
			GetPort(&oldport);
			SetPort(map2->window);
			/* (also clipping?) */
			draw_other_map(map2, map);
			SetPort(oldport);
		}
	}
}

void
draw_other_map(map, map2)
Map *map, *map2;
{
	int sx, sy, sw, sh;
	Rect tmprect;

	scale_vp(map->vp, map2->vp, &sx, &sy, &sw, &sh);
	SetRect(&tmprect, sx, sy, sx + sw, sy + sh);
	OffsetRect(&tmprect, map->conw, map->toph);
	if (map->vp->hw < 8) PenSize(2, 2);
	PenMode(patXor);
	FrameRect(&tmprect);
	PenNormal();
}

/* x0 may be negative here. */

/* (should add a "draw interior only" version of this routine?) */

void
draw_row(map, x0, y0, len, clearit)
Map *map;
int x0, y0, len, clearit;
{
	int empty = FALSE;
	int i = 0, x, xw, sx, sy, t;

	if (!between(0, y0, area.height - 1))
	  return;
	if (!dside->see_all && !map->drawai && unseen_image == NULL) {
		empty = TRUE;
		/* Examine row to see if we can skip it entirely. */
		for (x = x0; x < x0 + len; ++x) {
			xw = wrapx(x);
			if (terrain_visible(dside, xw, y0)
				|| (numbordtypes > 0 && terrain_visible(dside, xw, y0 + 1))
				) {
				empty = FALSE;
				break;
			}
		}
	}
	if (empty && !clearit)
	  return;
	/* The terrain always comes first. */
	if (map->drawterrain) {
		draw_terrain_row(map, x0, y0, len);
		/* Maybe draw cliffs. */
		if (elevations_defined() && map->vp->angle != 90 && y0 > 0) {
			for (x = x0; x < x0 + len; ++x) {
				if (terrain_visible(dside, x, y0)) {
					draw_cliffs(map, x, y0);
				}
			}
		}
		if (elevations_defined() && map->drawelevations && map->vp->angle == 90) {
			for (x = x0; x < x0 + len; ++x) {
				if (terrain_visible(dside, x, y0)) {
					draw_contours(map, x, y0);
				}
			}
		}
		if (any_aux_terrain_defined()) {
			/* The relative ordering of these is quite important - connections
			   should always be drawn on top of borders. */
			if (bords_to_draw(map)) {
				for_all_terrain_types(t) {
					if (map->drawauxterrain[t] && t_is_border(t) && aux_terrain_defined(t)) {
						for (x = x0; x < x0 + len; ++x) {
							draw_borders(map, x, y0, t);
						}
					}
				}
			}
			if (conns_to_draw(map)) {
				for_all_terrain_types(t) {
					if (map->drawauxterrain[t] && t_is_connection(t) && aux_terrain_defined(t)) {
						for (x = x0; x < x0 + len; ++x) {
							draw_connections(map, x, y0, t);
						}
					}
				}
			}
		}
	}
	/* Although we had to draw the terrain on the edge, we can skip everything else,
	   since edge cells have no units, weather, etc. */
	/* Skip the top and bottom edge rows. */
	if (!between(1, y0, area.height - 2)) 
	  return;
	/* Shorten the row by one cell on each side, if those are edge cells. */
	if (!inside_area(x0 + len - 1, y0))
	  --len;
	if (!inside_area(x0, y0)) {
		++x0;
		--len;
	}
	if (len <= 0)
	  return;
	if (any_cell_materials_defined() && map->nummaterialstodraw > 0 && map->vp->hh > 20) {
		for (x = x0; x < x0 + len; ++x) {
			draw_materials(map, x, y0);
		}
	}
	/* (should be global to entire map drawing somehow?) */
	if (map->drawnames && map->vp->hh > 5) {
		for (x = x0; x < x0 + len; ++x) {
			draw_legend(map, x, y0);
		}
	}
	if (people_sides_defined() && bwid2[map->vp->power] > 0) {
		draw_people_row(map, x0, y0, len);
	}
	if (map->drawunits && map->vp->hw > 2) {
		for (x = x0; x < x0 + len; ++x) {
			draw_units(map, x, y0);
		}
	}
	if (map->drawai) {
		for (x = x0; x < x0 + len; ++x) {
			draw_ai_region(map, x, y0);
		}
	}
	/* If debugging, draw coverage on top of everything else. */
	if (DebugG && !all_see_all && map->vp->hw >= 16) {
		for (x = x0; x < x0 + len; ++x) {
			xform(map, x, y0, &sx, &sy);
			draw_coverage(sx, sy, map->vp->power, cover(dside, x, y0), alt_cover(dside, x, y0));
		}
	}
}

/* Is this just to see if a cell is visible? */

int
cell_update(map, x, y)
Map *map;
int x, y;
{
#if 0
	int sx, sy;
	Rect tmprect;

	xform(map, x, y, &sx, &sy);
	SetRect(&tmprect, sx, sy, sx + map->vp->hw, sy + map->vp->hh);
	return (RectInRgn(&tmprect, map->window->visRgn));
#else
	return TRUE;
#endif
}

/* Draw an entire row of terrain, possibly with a single rectangle fill. */
 
/* x0 may be negative. */

extern int sunx, suny;

void
draw_terrain_row(map, x0, y0, len)
Map *map;
int x0, y0, len;
{
	int x0w, x, xw, x1, x1w, sx, sy, i;
	int pwr = map->vp->power, ang = map->vp->angle;
	int dogrid = map->drawgrid, dofill = map->drawcellpats;
	int inarea, seginarea, style, segstyle, terr, segterr, over, segover, update, segupdate;

	tmpdrawlighting = map->drawlighting;
	tmpdrawcoverage = (!all_see_all && map->drawcover);
	x0w = wrapx(x0);
	i = 0;
	x1 = x0;
	x1w = wrapx(x1);
	seginarea = in_area(x0w, y0);
	segstyle = cell_style(x0w, y0, pwr);
	segterr = cell_terrain(x0w, y0, pwr);
	segover = cell_overlay(x0w, y0);
	segupdate = (seginarea ? cell_update(map, x0w, y0) : FALSE);
	for (x = x0; x < x0 + len + 1; ++x) {
		xw = wrapx(x);
		inarea = in_area(xw, y0);
		style = cell_style(xw, y0, pwr);
		terr = cell_terrain(xw, y0, pwr);
		over = cell_overlay(xw, y0);
		update = (inarea ? cell_update(map, xw, y0) : FALSE);
		/* Decide if the run is over and we need to dump some output. */
		if (x == x0 + len
			|| inarea != seginarea
			|| style != segstyle
			|| terr != segterr
			|| over != segover
			|| update != segupdate
/*			|| segstyle == useblocks  fixes, but poor performance */
			|| segstyle == usepictures
			|| segstyle == usepolygons
			|| ang != 90) {
			/* don't draw anything that would match the window's bg */
#if 0 /* use this for heavy-duty debugging only */
			DGprintf("Seg is %d,%d len=%d inarea=%d style=%d terr=%d over=%d update=%d\n",
					 x1, y0, i, seginarea, segstyle, segterr, segover, segupdate);
#endif
			if (seginarea && segupdate && segstyle != dontdraw) {
				xform(map, x1, y0, &sx, &sy);
				if (area.xwrap && sx > map->vp->totsw)
				  sx -= map->vp->totsw;
				switch (segstyle) {
					case useblocks:
						draw_cell_block(sx, sy, i, pwr, segterr, segover, ang);
						break;
					case usepictures:
						break;
					case usepolygons:
						draw_hex_region(sx, sy, pwr, map->drawgrid, segterr, segover, ang);
						/* Assume that only polygon scale can fit numbers. */
						if (0 && elevations_defined()
							&& map->drawelevations
							&& draw_elevation_here(x1w, y0)) {
							draw_elevation(sx, sy, pwr, elev_at(x1w, y0));
						}
						if (clouds_defined()
						    && map->drawclouds
						    && draw_clouds_here(x1w, y0)) {
							draw_clouds(sx, sy, pwr, cloud_view(dside, x1w, y0));
						}
						if (winds_defined()
						    && map->drawwinds
						    && draw_winds_here(x1w, y0)) {
							draw_winds(sx, sy, pwr, wind_view(dside, x1w, y0));
						}
						if (temperatures_defined()
						    && map->drawtemperature
						    && draw_temperature_here(x1w, y0)) {
							draw_temperature(sx, sy, pwr, temperature_view(dside, x1w, y0));
						}
				}
			}
			/* Set up for the next segment. */
			i = 0;
			x1 = x;
			x1w = wrapx(x1);
			seginarea = inarea;
			segstyle = style;
			segterr = terr;
			segover = over;
			segupdate = update;
		}
		++i;
	}
}

static void
draw_cliffs(Map *map, int x, int y)
{
	int elev, x1, y1, drop, t, t1;
	int sx, sy, sx1, sy1;
	PolyHandle poly;
	RGBColor hexcolor, oldcolor;

	t = cell_terrain(x, y, map->vp->power);
	elev = elev_at(x, y) + t_thickness(t);
	if (point_in_dir(x, y, SOUTHWEST, &x1, &y1)) {
		t1 = cell_terrain(x1, y1, map->vp->power);
		if (t1 != NONTTYPE) {
		  drop = elev - (elev_at(x1, y1) + t_thickness(t1));
		  if (drop > 0) {
			xform(map, x, y, &sx, &sy);
			xform(map, x1, y1, &sx1, &sy1);
			if (sy1 > sy + map->vp->hch + (map->drawgrid ? 1 : 0)) {
				poly = OpenPoly();
				MoveTo(sx, sy + map->vp->hch);
				LineTo(sx + map->vp->hw / 2, sy + map->vp->hh);
				LineTo(sx + map->vp->hw / 2, sy1 + (map->vp->hh - map->vp->hch));
				LineTo(sx, sy1);
				LineTo(sx, sy + map->vp->hch);
				ClosePoly();
				/* if (timg->colrpat != nil
				    && (minscreendepth > 1 || !timg->patdefined)) {
					FillCPoly(poly, timg->colrpat);
				} else */ if (tcolors[t] != NULL && tcolors[t]->defined && maxscreendepth > 1) {
					hexcolor.red   = (tcolors[t]->r / 2) << 8;
					hexcolor.green = (tcolors[t]->g / 2) << 8;
					hexcolor.blue  = (tcolors[t]->b / 2) << 8;
					RGBForeColor(&hexcolor);
					PaintPoly(poly);
					/* Restore the previous color. */
					oldcolor.red = oldcolor.green = oldcolor.blue = 0;
					RGBForeColor(&oldcolor);
				} else {
					hexcolor.red   = 128 << 8;
					hexcolor.green = 128 << 8;
					hexcolor.blue  = 128 << 8;
					RGBForeColor(&hexcolor);
					PaintPoly(poly);
					/* Restore the previous color. */
					oldcolor.red = oldcolor.green = oldcolor.blue = 0;
					RGBForeColor(&oldcolor);
				}
				KillPoly(poly);
			}
		  }
		}
	}
	if (point_in_dir(x, y, SOUTHEAST, &x1, &y1)) {
		t1 = cell_terrain(x1, y1, map->vp->power);
		if (t1 != NONTTYPE) {
		  drop = elev - (elev_at(x1, y1) + t_thickness(t1));
		  if (drop > 0) {
			xform(map, x, y, &sx, &sy);
			xform(map, x1, y1, &sx1, &sy1);
			if (sy1 > sy + map->vp->hch + (map->drawgrid ? 1 : 0)) {
				poly = OpenPoly();
				MoveTo(sx + map->vp->hw - (map->drawgrid ? 1 : 0),     sy + map->vp->hch);
				LineTo(sx + map->vp->hw - (map->drawgrid ? 1 : 0),     sy1);
				LineTo(sx + map->vp->hw / 2, sy1 + (map->vp->hh - map->vp->hch));
				LineTo(sx + map->vp->hw / 2, sy + map->vp->hh);
				LineTo(sx + map->vp->hw - (map->drawgrid ? 1 : 0),     sy + map->vp->hch);
				ClosePoly();
				/* if (timg->colrpat != nil
				    && (minscreendepth > 1 || !timg->patdefined)) {
					FillCPoly(poly, timg->colrpat);
				} else */ if (tcolors[t] != NULL && tcolors[t]->defined && maxscreendepth > 1) {
					hexcolor.red   = (tcolors[t]->r) << 8;
					hexcolor.green = (tcolors[t]->g) << 8;
					hexcolor.blue  = (tcolors[t]->b) << 8;
					RGBForeColor(&hexcolor);
					PaintPoly(poly);
					/* Restore the previous color. */
					oldcolor.red = oldcolor.green = oldcolor.blue = 0;
					RGBForeColor(&oldcolor);
				} else {
					hexcolor.red   = 128 << 8;
					hexcolor.green = 128 << 8;
					hexcolor.blue  = 128 << 8;
					RGBForeColor(&hexcolor);
					PaintPoly(poly);
					/* Restore the previous color. */
					oldcolor.red = oldcolor.green = oldcolor.blue = 0;
					RGBForeColor(&oldcolor);
				}
				KillPoly(poly);
			}
		  }
		}
	}
}

/* The theory of contour lines is that each hex can be considered as
   six triangles, each of which has a vertex at the center and two
   on adjacent corners of the hex.  The elevation of the center vertex
   is the overall elevation of the cell, the elevations of the corners
   are averages with the adjacent cells.  If a particular contour
   elevation is between any pair of vertex elevations, then the contour
   line must cross that side of the triangle - and one of the other two
   sides.  We decide which of the two it is, interpolate to get the
   actual positions of each endpoint of the line segment, then draw it. */

int num_contours;

int contour_interval = -1;

static void
draw_contours(Map *map, int x, int y)
{
	int el, dir, x1, y1, sum, n, lowest, liq, ecor[NUMDIRS], ec;
	int sx, sy, sxcor[NUMDIRS], sycor[NUMDIRS], sxc, syc;
	int power = map->vp->power;
	int ecorr, ecorl, sxcorr, sycorr, sxcorl, sycorl;
	int sx1, sy1, sx2, sy2;

	if (contour_interval < 0) {
		/* (should be user-settable map parm) */
		num_contours = min(20, area.maxelev - area.minelev);
		contour_interval = (area.maxelev - area.minelev) / num_contours;
	}
	if (contour_interval < 1)
	  return;
	el = elev_at(x, y);
	xform(map, x, y, &sx, &sy);
	sxc = sx + map->vp->hw / 2;  syc = sy + map->vp->hh / 2;
	for_all_directions(dir) {
		sum = el;
		n = 1;
		lowest = el;
		liq = t_liquid(terrain_at(x, y));
		if (point_in_dir(x, y, dir, &x1, &y1)) {
			sum += elev_at(x1, y1);
			++n;
			lowest = min(lowest, elev_at(x1, y1));
			if (t_liquid(terrain_at(x1, y1)))
			  liq = TRUE;
		}
		if (point_in_dir(x, y, left_dir(dir), &x1, &y1)) {
			sum += elev_at(x1, y1);
			++n;
			lowest = min(lowest, elev_at(x1, y1));
			if (t_liquid(terrain_at(x1, y1)))
			  liq = TRUE;
		}
		if (liq)
		  ecor[dir] = lowest;
		else
		  ecor[dir] = sum / n;
		sxcor[dir] = sx + bsx[power][dir];  sycor[dir] = sy + bsy[power][dir];
	}
	for (ec = area.minelev + contour_interval; ec < area.maxelev; ec += contour_interval) {
		for_all_directions(dir) {
			ecorr = ecor[dir];
			ecorl = ecor[left_dir(dir)];
			sxcorr = sxcor[dir];  sycorr = sycor[dir];
			sxcorl = sxcor[left_dir(dir)];  sycorl = sycor[left_dir(dir)];
			if (el != ecorr && between(min(el, ecorr), ec, max(el, ecorr))) {
				sx1 = sxc + ((sxcorr - sxc) * (ec - el)) / (ecorr - el);
				sy1 = syc + ((sycorr - syc) * (ec - el)) / (ecorr - el);
				if (el != ecorl && between(min(el, ecorl), ec, max(el, ecorl))) {
					sx2 = sxc + ((sxcorl - sxc) * (ec - el)) / (ecorl - el);
					sy2 = syc + ((sycorl - syc) * (ec - el)) / (ecorl - el);
					MoveTo(sx1, sy1);
					LineTo(sx2, sy2);
				} else if (ecorl != ecorr) {
					sx2 = sxcorr + ((sxcorl - sxcorr) * (ec - ecorr)) / (ecorl - ecorr);
					sy2 = sycorr + ((sycorl - sycorr) * (ec - ecorr)) / (ecorl - ecorr);
					MoveTo(sx1, sy1);
					LineTo(sx2, sy2);
				}
			}
			if (el != ecorl && between(min(el, ecorl), ec, max(el, ecorl))) {
				sx1 = sxc + ((sxcorl - sxc) * (ec - el)) / (ecorl - el);
				sy1 = syc + ((sycorl - syc) * (ec - el)) / (ecorl - el);
				if (ecorl != ecorr && between(min(ecorr, ecorl), ec, max(ecorr, ecorl))) {
					sx2 = sxcorr + ((sxcorl - sxcorr) * (ec - ecorr)) / (ecorl - ecorr);
					sy2 = sycorr + ((sycorl - sycorr) * (ec - ecorr)) / (ecorl - ecorr);
					MoveTo(sx1, sy1);
					LineTo(sx2, sy2);
				}
			}
		}
	}
}

static void
draw_borders(map, x, y, b)
Map *map;
int x, y, b;
{
	int xw = wrapx(x), dir, sx, sy, bitmask = 0;
	
 	if (!terrain_visible(dside, xw, y) || !any_borders_at(x, y, b))
 	  return;
	for_all_directions(dir) {
		if (border_at(x, y, dir, b) && borders_visible(dside, x, y, dir)) {
			bitmask |= 1 << dir;
		}
	}
	if (bitmask != 0) {
		xform(map, x, y, &sx, &sy);
		/* (should compute and pass in overlay) */
		draw_border_line_multiple(map->window, sx, sy, bitmask, map->vp->power, b, map->vp->angle);
	}
}

/* Draw all the connections of the given cell. */

static void
draw_connections(map, x, y, c)
Map *map;
int x, y, c;
{
	int xw = wrapx(x), dir, sx, sy, bitmask = 0;
	
	if (!terrain_visible(dside, xw, y) || !any_connections_at(x, y, c))
	  return;
	for_all_directions(dir) {
		if (connection_at(x, y, dir, c)) {
			bitmask |= 1 << dir;
		}
	}
	if (bitmask != 0) {
		xform(map, x, y, &sx, &sy);
		/* (should compute and pass in overlay) */
		draw_connection_line_multiple(map->window, sx, sy, bitmask, map->vp->power, c, map->vp->angle);
	}
}

/* Draw all the units visible in the given cell. */
/* (x is not wrapped) */

static void
draw_units(map, x, y)
Map *map;
int x, y;
{
	int xw = wrapx(x), sx, sy, sw, sh, uview, u, s;
	Unit *unit;
	Rect tmprect;
	extern PicHandle dotdotdotpicture;

	if (units_visible(dside, xw, y)) {
		unit = unit_at(xw, y);
		if (unit != NULL) {
			xform(map, x, y, &sx, &sy);
			if (map->vp->uw <= 16) {
				/* Adjust to unit part of cell. */
				sw = map->vp->uw;  sh = map->vp->uh;
				sx += (map->vp->hw - sw) / 2;  sy += (map->vp->hh - sw) / 2;
				if (unit->occupant != NULL
					&& sw >= 8
	    			&& (unit->side == dside || all_see_all || u_see_occupants(unit->type))) {
					/* Draw a "grouping box", in white, but with no occs drawn. */
					SetRect(&tmprect, sx, sy, sx + sw, sy + sh);
					FillRect(&tmprect, QDPat(white));
					FrameRect(&tmprect);
	    		}
				draw_unit_image(map->window, sx, sy, sw, sh,
								unit->type, side_number(unit->side), !completed(unit));
				/* Indicate if more than one stacked here. */
				if (unit->nexthere != NULL && sw > 8) {
					SetRect(&tmprect, sx + sw/2 - 6, sy + sh - 2,
									  sx + sw/2 + 6, sy + sh + 2);
					/* (should clip to fit in cell) */
					/* (should do a copybits) */
					DrawPicture(dotdotdotpicture, &tmprect);
				}
				if (map->drawnames)
				  draw_unit_name(unit, sx, sy, sw, sh);
			} else {
				for_all_stack(xw, y, unit) {
					if (1 /* doesnt work right? - side_sees_unit(dside, unit) */) {
						m_xform_unit(map, unit, &sx, &sy, &sw, &sh);
						draw_unit_and_occs(map, unit, sx, sy, sw, sh);
					}
				}
			}
		}
	} else {
		uview = unit_view(dside, xw, y);
		if (uview != EMPTY) {
			u = vtype(uview);  s = vside(uview);
			xform(map, x, y, &sx, &sy);
			/* Adjust to unit part of cell. */
			sx += (map->vp->hw - map->vp->uw) / 2;  sy += (map->vp->hh - map->vp->uh) / 2;
			draw_unit_image(map->window, sx, sy, map->vp->uw, map->vp->uh, u, s, 0);
		}
	}
}

static void
draw_unit_and_occs(map, unit, sx, sy, sw, sh)
Map *map;
Unit *unit;
int sx, sy, sw, sh;
{
	int u = unit->type, s = side_number(unit->side), sx2, sy2, sw2, sh2;
	Unit *occ;
	Rect tmprect;

	/* If an occupant's side is the same as its transport's, then there's
	   really no need to draw its side emblem, since the transport's emblem
	   will also be visible. */
	if (unit->transport && unit->side == unit->transport->side)
	  s = -1;
	if (unit->occupant == NULL
		|| sw <= 8
	    || (unit->side != dside && !all_see_all && !u_see_occupants(unit->type))) {
		draw_unit_image(map->window, sx, sy, sw, sh, u, s, !completed(unit));
		if (map->drawnames)
		  draw_unit_name(unit, sx, sy, sw, sh); 
	} else {
		/* Draw a sort of "grouping box", in white. */
		SetRect(&tmprect, sx, sy, sx + sw, sy + sh);
		FillRect(&tmprect, QDPat(white));
		FrameRect(&tmprect);
		/* Draw the transport in the UL quarter of the box. */
		m_xform_occupant(map, unit, unit, sx, sy, sw, sh, &sx2, &sy2, &sw2, &sh2);
		draw_unit_image(map->window, sx2, sy2, sw2, sh2, u, s, !completed(unit));
		if (map->drawnames)
		  draw_unit_name(unit, sx2, sy2, sw2, sh2);
		/* Draw all the occupants, in the bottom half of the box. */
		for_all_occupants(unit, occ) {
			m_xform_occupant(map, unit, occ, sx, sy, sw, sh, &sx2, &sy2, &sw2, &sh2);
			draw_unit_and_occs(map, occ, sx2, sy2, sw2, sh2);
		}
	}
}

/* Indicate what kind of people are living in the given row. */

/* (should optimize by sharing border drawing for max efficiency) */

static void
draw_people_row(map, x0, y, len)
Map *map;
int x0, y, len;
{
	int pop, xx, x, sx, sy, sw, sh, ex, ey, ew, eh, dir, x1, y1, pop1;
	int bitmask1, bitmask2, drawemblemhere;

	for (xx = x0; xx < x0 + len; ++xx) {
		x = wrapx(xx);
		if (!terrain_visible(dside, x, y))
		  continue;
		pop = people_side_at(x, y);
		bitmask1 = bitmask2 = 0;
		drawemblemhere = FALSE;
		/* Decide which edges are borders of the country. */
		for_all_directions(dir) {
			/* Don't do anything about edge cells. */
			if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
				if (terrain_visible(dside, x1, y1)) {
					pop1 = people_side_at(x1, y1);
					if (pop != pop1) {
						/* Borders with uninhabitated regions are drawn differently. */
						if (pop == NOBODY || pop1 == NOBODY) {
							bitmask2 |= 1 << dir;
						} else {
							bitmask1 |= 1 << dir;
						}
					}
				} else {
					/* Draw just people in the cells right at the edge of the known world. */
					drawemblemhere = TRUE;
				}
			}
		}
		/* Now draw both the edges and an emblem for the cell. */
		if ((bitmask1 | bitmask2) != 0 || (map->drawpeople && drawemblemhere)) {
			xform(map, x, y, &sx, &sy);
			if (bitmask1 != 0) {
				draw_country_borders(map->window, sx, sy, bitmask1, map->vp->power, 0, map->vp->angle);
			}
			if (bitmask2 != 0) {
				draw_country_borders(map->window, sx, sy, bitmask2, map->vp->power, 2, map->vp->angle);
			}
			/* Draw an emblem for the people in the cell. */
			if (map->drawpeople && pop != NOBODY) {
				sw = map->vp->uw;  sh = map->vp->uh;
				ew = min(sw, max(8, sw / 2));  eh = min(sh, max(8, sh / 2));
				ex = sx + (map->vp->hw - map->vp->uw) / 2 + sw / 2 - ew / 2;  ey = sy + (map->vp->hh - map->vp->uh) / 2 + sh / 2 - eh / 2;
				draw_side_emblem(map->window, ex, ey, ew, eh, pop, plain_emblem);
			}
		}
	}
}

/* This draws a small set of bar charts, one for each material type. */

static void
draw_materials(map, x, y)
Map *map;
int x, y;
{
	int m, t, sx, sy, mx, my, mw, mh, amt, maxamt, h;
	Rect graphrect;
	
	if (nummtypes == 0)
	  return;
	mw = map->vp->uw / nummtypes /* should be count of displayable materials... */;  mh = map->vp->uh;
	if (mw <= 2 || mh <= 2)
	  return;
	t = cell_terrain(x, y, map->vp->power);
	xform(map, x, y, &sx, &sy);
	mx = sx + (map->vp->hw - map->vp->uw) / 2;  my = sy + (map->vp->hh - map->vp->uh) / 2;
	for_all_material_types(m) {
		if (map->drawmaterials[m] && (maxamt = tm_storage_x(t, m)) > 0) {
			SetRect(&graphrect, mx + m * mw, my, mx + (m + 1) * mw, my + map->vp->uh);
			FrameRect(&graphrect);
			amt = material_at(x, y, m);
			h = (amt * mh) / maxamt;
			graphrect.top -= (mh - h);
			FillRect(&graphrect, QDPat(black));
		}
	}
}

static void
draw_ai_region(map, x, y)
Map *map;
int x, y;
{
	int thid, sx, sy, dir, x1, y1, thid1, bitmask = 0;

	thid = ai_region_at(dside, wrapx(x), y);
	/* Decide which edges are borders of the theater. */
	for_all_directions(dir) {
		/* Don't do anything about edge cells. */
		if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
			thid1 = ai_region_at(dside, x1, y1);
			if (thid != thid1) {
				bitmask |= 1 << dir;
			}
		}
	}
	if (bitmask != 0) {
		xform(map, x, y, &sx, &sy);
		if (bitmask != 0) {
			draw_ai_region_borders(map->window, sx, sy, bitmask, map->vp->power);
		}
	}
}

/* Draw any text that should be associated with this cell. */

/* (could precompute what the string will lap over and move or truncate str),
   should be deterministic for each mag, so redraw doesn't scramble */

void
draw_legend(map, x, y)
Map *map;
int x, y;
{
	int xw, sx, sy;
	char *str, buf[BUFSIZE];
	Feature *feature;

    xw = wrapx(x);
	/* Draw the name of a terrain feature. */
	/* (should limit to one cell of feature, preferably centering on quasi-centroid) */	
	if (terrain_visible(dside, x, y)) {
		feature = feature_at(x, y);
		if (feature != NULL) {
			if (feature->size > 0) {
				if ((feature->x == x && feature->y == y)
				    || (feature->x == 0 && feature->y == 0)
					|| 0 /* center far away */) {
					str = feature_desc(feature, buf);
					if (str != NULL) {
						xform(map, x, y, &sx, &sy);
						draw_legend_text(sx + map->vp->hw/2, sy + map->vp->hh/2, map->vp->uh, str, 0);
					}
				}
			}
		}
	}
}

void
draw_unit_blast(Map *map, Unit *unit, int blast)
{
	int sx, sy, sw, sh;
	RgnHandle tmprgn;

	tmprgn = NewRgn();
	GetClip(tmprgn);
	/* Clip to the content area of the map's window. */
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	m_xform_unit(map, unit, &sx, &sy, &sw, &sh);
	draw_blast_image(map->window, sx, sy, sw, sh, blast);
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
}

void
clear_unit_blast(Map *map, Unit *unit, int blast)
{
	int sx, sy, sw, sh;
	RgnHandle tmprgn;

	tmprgn = NewRgn();
	GetClip(tmprgn);
	/* Clip to the content area of the map's window. */
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	m_xform_unit(map, unit, &sx, &sy, &sw, &sh);
	clear_blast_image(map->window, sx, sy, sw, sh, blast);
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
}

/* Draw all the selections of all the units. */

void
draw_selections(Map *map)
{
	int i;
	GrafPtr oldport;
	RgnHandle tmprgn;
	Unit *unit;

 	GetPort(&oldport);
	SetPort(map->window);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	for (i = 0; i < map->numselections; ++i) {
		unit = map->selections[i];
		draw_selected_unit(map, unit);
	}
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	if (map->numselections > 0) {
		unit = map->selections[0];
		if (in_play(unit)) {
			draw_unit_info(map);
		}
	}
	SetPort(oldport);
}

/* Draw all the selected units in the given cell. */

void
draw_selections_at(map, x, y)
Map *map;
int x, y;
{
	int i;
	GrafPtr oldport;
	RgnHandle tmprgn;
	Unit *unit;

 	GetPort(&oldport);
	SetPort(map->window);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	for (i = 0; i < map->numselections; ++i) {
		unit = map->selections[i];
		if (unit && unit->x == x && unit->y == y) {
			draw_selected_unit(map, unit);
		}
	}
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	if (map->numselections > 0) {
		unit = map->selections[0];
		if (in_play(unit) && unit->x == x && unit->y == y) {
			draw_unit_info(map);
		}
	}
	SetPort(oldport);
}

void
draw_selected_unit_setport(map, unit)
Map *map;
Unit *unit;
{
	GrafPtr oldport;
	RgnHandle tmprgn;

 	GetPort(&oldport);
	SetPort(map->window);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	draw_selected_unit(map, unit);
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	if (map->numselections > 0) {
		unit = map->selections[0];
		if (in_play(unit)) {
			draw_unit_info(map);
		}
	}
	SetPort(oldport);
}

/* Draw a single selected unit on the given map.  Assumes that grafport already set. */

void
draw_selected_unit(map, unit)
Map *map;
Unit *unit;
{
	int sx, sy, sw, sh, size, wholecell = FALSE, drawmag = FALSE;
	int sx1, sy1, sw1, sh1;
	Rect tmprect;

	if (!in_play(unit))
	  return; /* unselect it too? */
	if (map->vp->uw >= 32) {
		m_xform_unit_self(map, unit, &sx, &sy, &sw, &sh);
		if (map->numselections == 1
			&& sw < 16
			&& unit->transport != NULL) {
			wholecell = TRUE;
			drawmag = TRUE;
			sx1 = sx;  sy1 = sy;  sw1 = sw;  sh1 = sh;
		}
	} else {
		wholecell = TRUE;
	}
	if (wholecell) {
		xform(map, unit->x, unit->y, &sx, &sy);
		/* Adjust to unit part of cell. */
		sx += (map->vp->hw - map->vp->uw) / 2;  sy += (map->vp->hh - map->vp->uh) / 2;
		sw = map->vp->uw;  sh = map->vp->uh;
	}
	if (0 /* not actually within visible area */)
	  return;
	/* Indicate a unit's plans/tasks in some useful fashion. */
	if (map->drawplans
		&& unit->plan
		&& unit->plan->tasks) {
		int sx2, sy2;
		Task *task = unit->plan->tasks, *nexttask;

		if (task != NULL) {
			if ((nexttask = task->next) != NULL) {
				switch (nexttask->type) {
					case TASK_MOVE_TO:
					case TASK_HIT_UNIT:
						if (in_area(nexttask->args[0], nexttask->args[1])) {
							xform(map, nexttask->args[0], nexttask->args[1], &sx2, &sy2);
							PenPat(QDPat(ltGray));
							MoveTo(sx + sw/2, sy + sh/2);
							LineTo(sx2 + map->vp->hw/2, sy2 + map->vp->hh/2);
							PenNormal();
						}
						/* else warn unobtrusively? tasks should not go outside world,
						   but not a fatal problem if they do... */
				}
			}
			switch (task->type) {
				case TASK_MOVE_TO:
				case TASK_HIT_UNIT:
					if (in_area(task->args[0], task->args[1])) {
						xform(map, task->args[0], task->args[1], &sx2, &sy2);
						PenPat(QDPat(dkGray));
						MoveTo(sx + sw/2, sy + sh/2);
						LineTo(sx2 + map->vp->hw/2, sy2 + map->vp->hh/2);
						PenNormal();
					}
			}
		}
	}
	/* Draw magnification lines pointing to the true location of the unit. */
	if (drawmag) {
		/* PenPat should already be black. */
		MoveTo(sx,      sy);       LineTo(sx1,       sy1);
		MoveTo(sx + sw, sy);       LineTo(sx1 + sw1, sy1);
		MoveTo(sx,      sy + sh);  LineTo(sx1,       sy1 + sh1);
		MoveTo(sx + sw, sy + sh);  LineTo(sx1 + sw1, sy1 + sh1);
	}
	/* Be sure the selected unit is drawn. */
	draw_unit_image(map->window, sx, sy, sw, sh,
					unit->type, side_number(unit->side), !completed(unit));
	/* Draw a highlighting rectangle. */
	SetRect(&tmprect, sx, sy, sx + sw, sy + sh);
	/* A hack to prevent leakage into the grid. */
	if (map->drawgrid && map->vp->power == 5) --tmprect.bottom;
	/* First, draw an outer frame, for contrast. */
	if (map->autoselect && unit->act && unit->act->initacp > 0 && unit->act->acp > 0) {
		PenPat(&animation_patterns[animation_pattern_state]);
	} else {
		PenPat(QDPat(white));
	}
	FrameRect(&tmprect);
	InsetRect(&tmprect, 1, 1);
	/* Black is for units that can still act, dark gray for actors, gray if the
	   unit can't do anything. */
	PenPat((unit->act && unit->act->initacp > 0) ?
			((unit->act->acp > 0) ? QDPat(black) : QDPat(dkGray)) : QDPat(gray));
	/* Wide border if awake, narrow if asleep or napping. */
	size = ((unit->plan && (unit->plan->asleep || unit->plan->reserve)) ? 1 : 2);
	PenSize(size, size);
	FrameRect(&tmprect);
	PenNormal();
	DGprintf("draw selection of %s at %d,%d\n", unit_desig(unit));
}

void
draw_selection_animation(map, unit)
Map *map;
Unit *unit;
{
	int sx, sy, sw, sh, wholecell = FALSE, drawmag = FALSE;
	int sx1, sy1, sw1, sh1;
	Rect tmprect;
	GrafPtr oldport;
	RgnHandle tmprgn;

	if (!in_play(unit))
	  return; /* unselect it too? */
 	GetPort(&oldport);
	SetPort(map->window);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	if (map->vp->uw >= 32) {
		m_xform_unit_self(map, unit, &sx, &sy, &sw, &sh);
		if (map->numselections == 1
			&& sw < 16
			&& unit->transport != NULL) {
			wholecell = TRUE;
			drawmag = TRUE;
			sx1 = sx;  sy1 = sy;  sw1 = sw;  sh1 = sh;
		}
	} else {
		wholecell = TRUE;
	}
	if (wholecell) {
		xform(map, unit->x, unit->y, &sx, &sy);
		/* Adjust to unit part of cell. */
		sx += (map->vp->hw - map->vp->uw) / 2;  sy += (map->vp->hh - map->vp->uh) / 2;
		sw = map->vp->uw;  sh = map->vp->uh;
	}
	/* Draw a highlighting rectangle. */
	SetRect(&tmprect, sx, sy, sx + sw, sy + sh);
	/* A hack to prevent leakage into the grid. */
	if (map->drawgrid && map->vp->power == 5) --tmprect.bottom;
	/* First, draw an outer white frame, for contrast. */
	if (unit->act && unit->act->initacp > 0 && unit->act->acp > 0) {
		PenPat(&animation_patterns[animation_pattern_state]);
	} else {
		PenPat(QDPat(white));
	}
	FrameRect(&tmprect);
	PenPat(QDPat(black));
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	SetPort(oldport);
}

/* (should only redraw any given cell once) */

void
erase_selections(map)
Map *map;
{
	int i;
	GrafPtr oldport;
	RgnHandle tmprgn;
	Unit *unit;

 	GetPort(&oldport);
	SetPort(map->window);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	for (i = 0; i < map->numselections; ++i) {
		unit = map->selections[i];
		draw_unselected_unit(map, unit);
	}
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	SetPort(oldport);
}

void
erase_selection(map, unit)
Map *map;
Unit *unit;
{
	GrafPtr oldport;
	RgnHandle tmprgn;

 	GetPort(&oldport);
	SetPort(map->window);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&(map->contentrect));
	SHIFT_ORIGIN(map);
	draw_unselected_unit(map, unit);
	RESET_ORIGIN(map);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	SetPort(oldport);
}

void
draw_unselected_unit(map, unit)
Map *map;
Unit *unit;
{
	if (!in_play(unit))
	  return;
	draw_row(map, unit->x, unit->y, 1, TRUE);
	DGprintf("erase selection of %s at %d,%d\n", unit_desig(unit));
}

void
force_map_update(map)
Map *map;
{
	force_update(map->window);
}

/* Remove and destroy the map object. */

void
destroy_map(map)
Map *map;
{
	Map *map2;
	
	if (maplist == map) {
		maplist = map->next;
	} else {
		for_all_maps(map2) {
			if (map2->next == map) {
				map2->next = map->next;
			}
		}
	}
	/* (should destroy substructs) */
	free(map);
}

void
activate_map(map, activate)
Map *map;
int activate;
{
	Rect growRect;

	if (activate) {
		HiliteControl(map->vscrollbar, 0);
		HiliteControl(map->hscrollbar, 0);
		/* Controls need to be redrawn on activation. */
		(*(map->vscrollbar))->contrlVis = 255;
		(*(map->hscrollbar))->contrlVis = 255;
		InvalRect(&(*(map->vscrollbar))->contrlRect);
		InvalRect(&(*(map->hscrollbar))->contrlRect);
		/* The growbox needs to be redrawn on activation. */
		growRect = map->window->portRect;
		/* adjust for the scrollbars */
		growRect.top = growRect.bottom - sbarwid;
		growRect.left = growRect.right - sbarwid;
		InvalRect(&growRect);
	} else {
		/* The scrollbars must be deactivated. */
		HiliteControl(map->vscrollbar, 255);
		HiliteControl(map->hscrollbar, 255);
#if 0  /* We don't want to hide them though, because the window bg is not white. */
		HideControl(map->vscrollbar);
		HideControl(map->hscrollbar);
#endif
		/* The growbox should be changed immediately on deactivation. */
		DrawGrowIcon(map->window);
	}
}

void
print_map(map)
Map *map;
{
/*	TPPrPort printport;
	extern THPrint printrecordhandle;

	printport = PrOpenDoc(printrecordhandle, nil, nil);
	PrCloseDoc(printport); */
}

/* (should be able to mention borders and conns also - share code with curses?) */
/* (could be a generic routine) */
void
oneliner(map, sx, sy)
Map *map;
int sx, sy;
{
	int x, y, t2, uview, u, s, ps = NOBODY, dep, sayin = FALSE;
	char *peopdesc = NULL, *sidedesc;
	char descbuf[80];
	Unit *unit;
	Side *side;
	char *mplayer_at_desig();

	if (!m_nearest_cell(map, sx, sy, &x, &y)) {
		strcpy(tmpbuf, "(nothing)");
		return;
	} else if (terrain_visible(dside, x, y)) {
		strcpy(tmpbuf, " ");
		/* Describe the side of the people here. */
		if (people_sides_defined()) {
			ps = people_side_at(x, y);
			if (ps != NOBODY) {
				side = side_n(ps);
				if (side == NULL) {
					peopdesc = "indep";
				} else if (side == dside) {
					peopdesc = "your";
				} else {
					peopdesc = side_adjective(side);
					if (peopdesc[0] == '\0') {
						sprintf(descbuf, "s%d", side->id);
						peopdesc = descbuf;
					}
				}
			}
		}
		if (units_visible(dside, x, y)) {
			m_nearest_unit(map, sx, sy, &unit);
			if (unit != NULL) {
				if (unit->side != dside) {
					sidedesc = side_adjective(unit->side);
					if (ps != NOBODY && ps == side_number(unit->side)) {
						peopdesc = "own";
					}
				} else {
					sidedesc = "your";
				}
				strcat(tmpbuf, sidedesc);
				if (unit->name) {
					strcat(tmpbuf, " ");
					strcat(tmpbuf, u_type_name(unit->type));
					strcat(tmpbuf, " ");
					strcat(tmpbuf, unit->name);
				} else if (unit->number > 0) {
					tprintf(tmpbuf, " %d%s %s",
							unit->number, ordinal_suffix(unit->number), u_type_name(unit->type));
				} else {
					strcat(tmpbuf, " ");
					strcat(tmpbuf, u_type_name(unit->type));
				}
				if (Debug || DebugG || DebugM) {
					tprintf(tmpbuf, " #%d", unit->id);
				}
				sayin = TRUE;
			}
		} else {
			if ((uview = unit_view(dside, x, y)) != EMPTY) {
				u = vtype(uview);  s = vside(uview);
				if (ps != NOBODY && ps == s) {
					peopdesc = "own";
				}
				strcat(tmpbuf, side_adjective(side_n(s)));
				strcat(tmpbuf, " ");
				strcat(tmpbuf, u_type_name(u));
				sayin = TRUE;
			}
		}
		if (sayin) {
			strcat(tmpbuf, " (in ");
		}
		if (peopdesc != NULL) {
			strcat(tmpbuf, peopdesc);
			strcat(tmpbuf, " ");
		}
		t2 = cell_terrain(x, y, 0);
		strcat(tmpbuf, t_type_name(t2));
		if (sayin) {
			strcat(tmpbuf, ")");
		}
		if (elevations_defined()) {
			tprintf(tmpbuf, " Elev %d", elev_at(x, y));
		}
		if (temperatures_defined()) {
			tprintf(tmpbuf, " T %d¡", temperature_at(x, y));
		}
		if (numcoattypes > 0) {
			for_all_terrain_types(t2) {
				if (t_is_coating(t2)
					&& aux_terrain_defined(t2)
					&& ((dep = aux_terrain_view(dside, x, y, t2)) > 0)) {
					tprintf(tmpbuf, " %s %d", t_type_name(t2), dep);
				}
			}
		}
	} else {
		sprintf(tmpbuf, "(unknown)");
	}
	tprintf(tmpbuf, " @%d,%d", x, y);
	if (terrain_visible(dside, x, y)) {
		Feature *feature;
		char *feature_desc();
		char *str, buf[BUFSIZE];

		feature = feature_at(x, y);
		if (feature != NULL) {
			if (feature->size > 0) {
				str = feature_desc(feature, buf);
				if (str != NULL) {
					tprintf(tmpbuf, " (%s)", str);
				}
			}
		}
	}
	if (map->drawai && side_has_ai(dside)) {
		strcat(tmpbuf, " ");
		strcat(tmpbuf, ai_at_desig(dside, x, y));
	}
}
