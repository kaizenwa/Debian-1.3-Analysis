/* Map interaction for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "macconq.h"
extern void do_add_terrain(void);
extern void do_remove_terrain(void);
extern void do_set_formation(void);
extern void set_position_modally(void);

static pascal void map_scroll_fn(ControlHandle control, short code);

int topunithgt = 60;

/* This is a temporary used by the map scroll proc. */

Map *curmap;

ControlActionUPP map_scroll_proc;

/* This scroll proc is shared by both the horizontal and vertical scrollbars. */

static pascal void
map_scroll_fn(ControlHandle control, short code)
{
	int curvalue, pagesize, jump;
	int oldsx, oldsy;
	RgnHandle tmprgn;
	Rect tmprect;

	/* The page jump should be most but not all of a screenful. */
	if (control == curmap->hscrollbar) {
		pagesize = (3 * curmap->vp->pxw) / 4;
	} else {
		pagesize = (3 * curmap->vp->pxh) / 4;
	}
	/* Adjust the pagesize to always be a multiple of 4. */
	pagesize = ((pagesize + 3) / 4) * 4;
	switch (code) {
		case inPageDown:
			jump = pagesize;
			break;
		case inDownButton:
			jump = 4;
			break;
		case inPageUp:
			jump = 0 - pagesize;
			break;
		case inUpButton:
			jump = -4;
			break;
		default:
			jump = 0;
			break;
	}
	/* Letting control's code do the max/min hacking. */
	SetCtlValue(control, GetCtlValue(control) + jump);
	curvalue = GetCtlValue(control);
	/* Tweak the map's own variables to match. */
	oldsx = curmap->vp->sx;  oldsy = curmap->vp->sy;
	if (control == curmap->hscrollbar) {
		set_view_position(curmap->vp, curvalue, curmap->vp->sy);
	} else {
		set_view_position(curmap->vp, curmap->vp->sx, curvalue);
	}
	m_focus_on_center(curmap);
	set_content_rect(curmap);
	/* Scroll the already-drawn bits. */
	tmprgn = NewRgn();
	SetRect(&tmprect, conwid, curmap->toph, conwid + curmap->vp->pxw, curmap->toph + curmap->vp->pxh);
	ScrollRect(&tmprect, oldsx - curmap->vp->sx, oldsy - curmap->vp->sy, tmprgn);
	InvalRgn(tmprgn);
	/* Do the update now, because we won't get back to the main event loop
	   until the mouse button is released. */
	update_window(curmap->window);
	DisposeRgn(tmprgn);
}

/* Handle a mouse down in the map window. */

void
do_mouse_down_map(Map *map, Point mouse, int mods)
{
	ControlHandle control;
	short part;
	int oldsx = map->vp->sx, oldsy = map->vp->sy;
	WindowPtr window = map->window;
	RgnHandle tmprgn;
	Rect tmprect;

	if (map_scroll_proc == NULL)
	  map_scroll_proc = NewControlActionProc(map_scroll_fn);

	part = FindControl(mouse, window, &control);
	if (control == map->hscrollbar) {
		/* Handle the horizontal scrollbar. */
		switch (part) {
			case inThumb:
				/* (should add power thumb ability?) */
				part = TrackControl(control, mouse, NULL);
				if (part == inThumb) {
					map->vp->sx = GetCtlValue(control);
					if (oldsx != map->vp->sx) {
						m_focus_on_center(map);
						set_content_rect(map);
						/* Scroll the already-drawn bits. */
						tmprgn = NewRgn();
						SetRect(&tmprect, conwid, map->toph, conwid + map->vp->pxw, map->toph + map->vp->pxh);
						ScrollRect(&tmprect, oldsx - map->vp->sx, oldsy - map->vp->sy, tmprgn);
						InvalRgn(tmprgn);
						DisposeRgn(tmprgn);
						draw_related_maps(map);
					}
				}
				break;
			default:
				curmap = map;
				part = TrackControl(control, mouse, map_scroll_proc);
				break;
		}
	} else if (control == map->vscrollbar) {
		/* Handle the vertical scrollbar. */
		switch (part) {
			case inThumb:
				/* (should add power thumb ability?) */
				part = TrackControl(control, mouse, NULL);
				if (part == inThumb) {
					map->vp->sy = GetCtlValue(control);
					if (oldsy != map->vp->sy) {
						m_focus_on_center(map);
						set_content_rect(map);
						/* Scroll the already-drawn bits. */
						tmprgn = NewRgn();
						SetRect(&tmprect, conwid, map->toph, conwid + map->vp->pxw, map->toph + map->vp->pxh);
						ScrollRect(&tmprect, oldsx - map->vp->sx, oldsy - map->vp->sy, tmprgn);
						InvalRgn(tmprgn);
						DisposeRgn(tmprgn);
						draw_related_maps(map);
					}
				}
				break;
			default:
				curmap = map;
				part = TrackControl(control, mouse, map_scroll_proc);
				break;
		}
	} else if (mouse.h <= conwid) {
		/* Interpret as a control panel hit. */
		do_mouse_down_map_control_panel(map, mouse.h, mouse.v, mods);
	} else {
		do_mouse_down_map_content(map, mouse.h, mouse.v, mods);
	}
}

void
do_mouse_down_map_control_panel(Map *map, int h, int v, int mods)
{
	int winh = map->window->portRect.bottom - map->window->portRect.top;

	/* (should better organize tests here) */
	if (between(winh - 2 * sbarwid, v, winh)) {
		switch ((winh - v) / sbarwid) {
			case 0:
				magnify_map(map, ((h < conwid / 2) ? -1 : 1));
				break;
#if 0
			/* This was too confusing here, so it's now flushed.  However, the
			   capability still seems worthwhile, so it should reappear elsewhere. */
			case 1:
				map_modal = ZOOM_MODAL;
				break;
#endif
		}
	} else if (v < 32) {
		toggle_survey(map);
	} else if ((v - 32) < 5 * 15) {
		switch ((v - 32) / 15) {
			case 0:
				if (h < conwid / 2) {
					select_previous_awake_mover(map);
				} else {
					select_next_awake_mover(map);
				}
				break;
			case 1:
				if (h < conwid / 2) {
					select_previous_mover(map);
				} else {
					select_next_mover(map);
				}
				break;
			case 2:
				if (h < conwid / 2) {
					select_previous_actor(map);
				} else {
					select_next_actor(map);
				}
				break;
			case 3:
				if (h < conwid / 2) {
					select_previous_unit(map);
				} else {
					select_next_unit(map);
				}
				break;
			case 4:
				beep();
				break;
		}
	} else if (v - 32 - 5*15 - 2 - 5/*why?*/ < 6 * 11) {
		switch ((v - 32 - 5*15 - 2 - 5/*why?*/) / 11) {
			case 0:
				toggle_map_grid(map);
				break;
			case 1:
				toggle_map_names(map);
				break;
			case 2:
				if (people_sides_defined()) {
					toggle_map_people(map);
				}
				break;
			case 3:
				toggle_map_plans(map);
				break;
			case 4:
				toggle_map_ai(map);
				break;
			case 5:
				if (dside->may_set_see_all) {
					dside->see_all = !dside->see_all;
					calc_vision(dside);
					force_map_update(map);
				}
				break;
		}
	} else {
		/* Unused area, ignore */
	}
}

void
toggle_survey(Map *map)
{
	int i;
	Unit *unit;

	map->moveonclick = !map->moveonclick;
	map->autoselect = !map->autoselect;
	draw_control_panel(map);
	if (map->autoselect) {
		if (map->numselections > 0) {
			for (i = 0; i < map->numselections; ++i) {
				unit = map->selections[i];
				if (unit != NULL) {
					map->curunit = autonext_unit(dside, unit);
					select_exactly_one_unit(map, map->curunit);
				}
			}
		}
	}
}

void
magnify_map(Map *map, int inout)
{
	set_map_mag(map, map->vp->power + inout);
}

/* This sets the map's magnification directly and updates it. */

void
set_map_mag(Map *map, int newpower)
{
	newpower = clip_to_limits(0, newpower, NUMPOWERS-1);
	if (map->vp->power != newpower) {
		set_map_power(map, newpower);
		m_center_on_focus(map);
		set_map_scrollbars(map);
		force_map_update(map);
		draw_related_maps(map);
	}
}

void
toggle_map_grid(Map *map)
{
	map->drawgrid = !map->drawgrid;
	/* (should not do a total redraw?) */
	force_map_update(map);
}

void
toggle_map_topline(Map *map)
{
	int oldtoph;
	Rect tmprect;
	RgnHandle tmprgn;
	GrafPtr oldport;

	oldtoph = map->toph;
	map->toplineh = (map->toplineh ? 0 : tophgt);
	map->toph = map->toplineh + map->topunith;
	set_content_rect(map);
	if (map->toplineh) {
 		GetPort(&oldport);
		SetPort(map->window);
		tmprgn = NewRgn();
		tmprect = map->window->portRect;
		tmprect.left += conwid;
		tmprect.right -= sbarwid;  tmprect.bottom -= sbarwid;
		ScrollRect(&tmprect, 0, map->toplineh, tmprgn);
		draw_top_line(map);
		InvalRgn(tmprgn);
		update_window(map->window);
		DisposeRgn(tmprgn);
		SetPort(oldport);
	} else {
		force_map_update(map);
	}
}

void
toggle_map_topunit(Map *map)
{
	int oldtoph;
	Rect tmprect;
	RgnHandle tmprgn;
	GrafPtr oldport;

	oldtoph = map->toph;
	map->topunith = (map->topunith ? 0 : topunithgt);
	map->toph = map->toplineh + map->topunith;
	set_content_rect(map);
	if (map->topunith) {
 		GetPort(&oldport);
		SetPort(map->window);
		tmprect = map->window->portRect;
		tmprect.left += conwid;  tmprect.top += oldtoph;
		tmprect.right -= sbarwid;  tmprect.bottom -= sbarwid;
		tmprgn = NewRgn();
		ScrollRect(&tmprect, 0, map->topunith, tmprgn);
		InvalRgn(tmprgn);
		update_window(map->window);
		DisposeRgn(tmprgn);
		draw_unit_info(map);
		SetPort(oldport);
	} else {
		force_map_update(map);
	}
}

void
toggle_map_other_maps(Map *map)
{
	map->drawothermaps = !map->drawothermaps;
	/* (should not do a total redraw) */
	force_map_update(map);
}

void
toggle_map_lighting(Map *map)
{
	map->drawlighting = !map->drawlighting;
	/* We have to do a total redraw. */
	force_map_update(map);
}

void
toggle_map_coverage(Map *map)
{
	map->drawcover = !map->drawcover;
	/* (should only change newly dimmed/lightened cells) */
	force_map_update(map);
}

void
toggle_map_names(Map *map)
{
	map->drawnames = !map->drawnames;
	/* (if now on, should draw names on top of everything, don't redraw everything) */
	if (map->vp->hh > 5) {
		force_map_update(map);
	} else {
		/* (should be a force update on control panel alone) */
		draw_control_panel(map);
	}
}

void
toggle_map_people(Map *map)
{
	map->drawpeople = !map->drawpeople;
	if (bwid2[map->vp->power] > 0) {
		force_map_update(map);
	} else {
		/* (should be a force update on control panel alone) */
		draw_control_panel(map);
	}
}

void
toggle_map_elevations(Map *map)
{
	map->drawelevations = !map->drawelevations;
	force_map_update(map);
}

void
toggle_map_materials(Map *map, int m)
{
	map->drawmaterials[m] = !map->drawmaterials[m];
	map->nummaterialstodraw += (map->drawmaterials[m] ? 1 : -1);
	force_map_update(map);
}

void
toggle_map_aux_terrain(Map *map, int t)
{
	map->drawauxterrain[t] = !map->drawauxterrain[t];
	force_map_update(map);
}

void
toggle_map_temperature(Map *map)
{
	map->drawtemperature = !map->drawtemperature;
	force_map_update(map);
}

void
toggle_map_winds(Map *map)
{
	map->drawwinds = !map->drawwinds;
	force_map_update(map);
}

void
toggle_map_clouds(Map *map)
{
	map->drawclouds = !map->drawclouds;
	force_map_update(map);
}

void
toggle_map_storms(Map *map)
{
	map->drawstorms = !map->drawstorms;
	force_map_update(map);
}

void
toggle_map_plans(Map *map)
{
	map->drawplans = !map->drawplans;
	if (map->numselections > 0) {
		force_map_update(map);
	} else {
		/* (should be a force update on control panel alone) */
		draw_control_panel(map);
	}
}

void
toggle_map_ai(Map *map)
{
	if (!side_has_ai(dside))
	  return;
	map->drawai = !map->drawai;
	force_map_update(map);
}

static int selrect;
static int downx, downy, downdir;

void drag_for_distance(Map *map, int h0, int v0);

void
do_mouse_down_map_content(Map *map, int h, int v, int mods)
{
	int i, rslt, anysuccess;
	Unit *unit;

	/* Remember this cell. */
	m_nearest_cell(map, h, v, &downx, &downy);
	/* Assume that last place clicked is a reasonable focus. */
	if (inside_area(downx, downy)) {
		map->vp->vcx = downx;  map->vp->vcy = downy;
	}
	if (map_modal > 0) {
		switch (map_modal) {
			case MOVE_TO_MODAL:
				do_move_to_command();
				/* Reset modality whether or not the command succeeded, otherwise
				   the player can get caught here if no fire commands can succeed. */
				map_modal = NO_MODAL;
				break;
			case FIRE_MODAL:
				do_fire_command();
				/* Reset modality whether or not the command succeeded, otherwise
				   the player can get caught here if no fire commands can succeed. */
				map_modal = NO_MODAL;
				break;
			case FIRE_INTO_MODAL:
				do_fire_into_command();
				/* Reset modality whether or not the command succeeded, otherwise
				   the player can get caught here if no fire commands can succeed. */
				map_modal = NO_MODAL;
				break;
			case SET_FORMATION_MODAL:
				do_set_formation();
				/* Reset modality whether or not the command succeeded, otherwise
				   the player can get caught here if no commands can succeed. */
				map_modal = NO_MODAL;
				break;
			case ADD_TERRAIN_MODAL:
				do_add_terrain();
				/* Reset modality whether or not the command succeeded, otherwise
				   the player can get caught here if no commands can succeed. */
				map_modal = NO_MODAL;
				break;
			case REMOVE_TERRAIN_MODAL:
				do_remove_terrain();
				/* Reset modality whether or not the command succeeded, otherwise
				   the player can get caught here if no commands can succeed. */
				map_modal = NO_MODAL;
				break;
			case DISTANCE_MODAL:
				drag_for_distance(map, h, v);
				/* Done being modal. */
				map_modal = NO_MODAL;
				break;
			case ZOOM_MODAL:
				select_area_and_zoom(map, h, v, mods);
				/* Done being modal. */
				map_modal = NO_MODAL;
				break;
			case GENERIC_MODAL:
				set_position_modally();
				map_modal = NO_MODAL;
				break;
			default:
				/* (should error out) */
				break;
		}
	} else if (mods & cmdKey) {
		if (map->moveonclick && map->autoselect) {
			unselect_all(map);
			m_nearest_unit(map, h, v, &unit);
			if (unit != NULL && (side_controls_unit(dside, unit) || endofgame)) {
				/* The nearest unit will become the "current unit". */
				map->curunit = unit;
				select_unit_on_map(map, unit);
				draw_selections(map);
				move_on_drag(map, unit, mods);
			} else {
				select_all_dragged_over(map, h, v, mods);
				/* Pick the first of the multiple selection as the "current unit". */
				if (map->numselections > 0) {
					map->curunit = map->selections[0];
				}
			}
		} else {
			anysuccess = FALSE;
			for (i = 0; i < map->numselections; ++i) {
				if ((unit = map->selections[i]) != NULL) {
					rslt = move_the_selected_unit(map, unit, h, v);
					if (rslt) anysuccess = TRUE;
				}
			}
			if (!anysuccess)
			  beep();
		}
	} else if (mods & optionKey) {
		for (i = 0; i < map->numselections; ++i) {
			if ((unit = map->selections[i]) != NULL) {
				fire_the_selected_unit(map, unit, h, v);
			}
		}
	} else if (mods & shiftKey) {
		m_nearest_unit(map, h, v, &unit);
		if (unit && side_sees_unit(dside, unit)) {
			/* Invert the selection status of the unit. */
			if (unit_is_selected(map, unit)) {
				unselect_unit_on_map(map, unit);
				erase_selection(map, unit);
			} else {
				select_unit_on_map(map, unit);
				draw_selections_at(map, unit->x, unit->y);
			}
		} else {
			select_all_dragged_over(map, h, v, mods);
		}
	} else {
		/* Interpret an unmodified mouse down. */
#ifdef DESIGNERS
		if (dside->designer && tooltype != notool) {
			apply_designer_tool(map, h, v, mods);
		} else
#endif /* DESIGNERS */
		if (map->moveonclick
#ifdef DESIGNERS
			&& !dside->designer
#endif /* DESIGNERS */
			) {
			/* Usually will only be one to move, but be general anyway. */
			for (i = 0; i < map->numselections; ++i) {
				if ((unit = map->selections[i]) != NULL) {
					move_the_selected_unit(map, unit, h, v);
				}
			}
			map->scrolltocurunit = TRUE;
		} else {
			unselect_all(map);
			m_nearest_unit(map, h, v, &unit);
			if (unit != NULL && (side_controls_unit(dside, unit) || endofgame)) {
				select_unit_on_map(map, unit);
				draw_selections(map);
				move_on_drag(map, unit, mods);
			} else {
				select_all_dragged_over(map, h, v, mods);
			}
		}
	}
}

void
select_all_dragged_over(Map *map, int h0, int v0, int mods)
{
	Point pt0, pt1, newmouse;
	int drawn = FALSE;
	Rect tmprect;

	SetPt(&pt0, h0, v0);
	SetPt(&pt1, h0, v0);
	SetRect(&tmprect, h0, v0, h0, v0);
	/* (should be a generic subr?) */
	PenMode(patXor);
	PenPat(QDPat(gray));
	while (WaitMouseUp()) {
		GetMouse(&newmouse);
		if (!EqualPt(pt1, newmouse) /* && PtInRect(newmouse, &(map->window->portRect)) */) {
			if (drawn) {
				tmprect.left = min(pt0.h, pt1.h);  tmprect.top = min(pt0.v, pt1.v);
				tmprect.right = max(pt0.h, pt1.h);  tmprect.bottom = max(pt0.v, pt1.v);
				FrameRect(&tmprect);
			}
			pt1 = newmouse;
			tmprect.left = min(pt0.h, pt1.h);  tmprect.top = min(pt0.v, pt1.v);
			tmprect.right = max(pt0.h, pt1.h);  tmprect.bottom = max(pt0.v, pt1.v);
			FrameRect(&tmprect);
			drawn = TRUE;
		}
	}
	if (drawn) {
		tmprect.left = min(pt0.h, pt1.h);  tmprect.top = min(pt0.v, pt1.v);
		tmprect.right = max(pt0.h, pt1.h);  tmprect.bottom = max(pt0.v, pt1.v);
		FrameRect(&tmprect);
	}
	PenNormal();
	select_all_units_in_rect(map, &tmprect);
}

void
select_area_and_zoom(Map *map, int h0, int v0, int mods)
{
	Point pt0, pt1, newmouse;
	int drawn = FALSE, x, y;
	Rect tmprect;

	SetPt(&pt0, h0, v0);
	SetPt(&pt1, h0, v0);
	/* (should be a generic subr) */
	PenMode(patXor);
	PenPat(QDPat(gray));
	while (WaitMouseUp()) {
		GetMouse(&newmouse);
		if (!EqualPt(pt1, newmouse) /* && PtInRect(newmouse, &(map->window->portRect)) */) {
			if (drawn) {
				tmprect.left = min(pt0.h, pt1.h);  tmprect.top = min(pt0.v, pt1.v);
				tmprect.right = max(pt0.h, pt1.h);  tmprect.bottom = max(pt0.v, pt1.v);
				FrameRect(&tmprect);
			}
			pt1 = newmouse;
			tmprect.left = min(pt0.h, pt1.h);  tmprect.top = min(pt0.v, pt1.v);
			tmprect.right = max(pt0.h, pt1.h);  tmprect.bottom = max(pt0.v, pt1.v);
			FrameRect(&tmprect);
			drawn = TRUE;
		}
	}
	if (drawn) {
		tmprect.left = min(pt0.h, pt1.h);  tmprect.top = min(pt0.v, pt1.v);
		tmprect.right = max(pt0.h, pt1.h);  tmprect.bottom = max(pt0.v, pt1.v);
		FrameRect(&tmprect);
	}
	PenNormal();
	m_nearest_cell(map, pt1.h, pt1.v, &x, &y);
	if (x != downx && y != downy) {
		magnify_to_fit(map, downx, downy, x, y);
	}
}

void
move_on_drag(Map *map, Unit *unit, int mods)
{
	int sx, sy, sw, sh, h0, v0, drawn = FALSE, x, y;
	Point pt0, pt1, newmouse;

	m_xform_unit_self(map, unit, &sx, &sy, &sw, &sh);
	h0 = sx + sw / 2;  v0 = sy + sh / 2;
	SetPt(&pt0, h0, v0);
	SetPt(&pt1, h0, v0);
	/* (should be a generic subr?) */
	PenMode(patXor);
	while (WaitMouseUp()) {
		GetMouse(&newmouse);
		/* should scroll, then abort if we drag outside the window */
		if (0 /* PtInRect(newmouse, &(map->window->portRect)) */) {
		}
		if (!EqualPt(pt1, newmouse)) {
			if (drawn) {
				MoveTo(h0, v0);  LineTo(pt1.h, pt1.v);
			}
			pt1 = newmouse;
			MoveTo(h0, v0);  LineTo(pt1.h, pt1.v);
			drawn = TRUE;
		}
	}
	/* Erase the last drawn line. */
	if (drawn) {
		MoveTo(h0, v0);  LineTo(pt1.h, pt1.v);
	}
	PenNormal();
	m_nearest_cell(map, pt1.h, pt1.v, &x, &y);
	if (x != downx || y != downy) {
		if (!move_the_selected_unit(map, unit, pt1.h, pt1.v))
		  beep();
	} else {
		/* (should try to enter another unit in this cell) */
	}
}

void
drag_for_distance(Map *map, int h0, int v0)
{
	Point pt0, pt1, newmouse;
	int drawn = FALSE, x, y;

	SetPt(&pt0, h0, v0);
	SetPt(&pt1, h0, v0);
	/* (should be a generic subr) */
	PenMode(patXor);
	PenPat(QDPat(gray));
	while (WaitMouseUp()) {
		GetMouse(&newmouse);
		/* should scroll, then abort if we drag outside the window */
		if (0 /* PtInRect(newmouse, &(map->window->portRect)) */) {
		}
		if (!EqualPt(pt1, newmouse)) {
			if (drawn) {
				MoveTo(h0, v0);  LineTo(pt1.h, pt1.v);
			}
			pt1 = newmouse;
			MoveTo(h0, v0);  LineTo(pt1.h, pt1.v);
			drawn = TRUE;
		}
	}
	/* Erase the last drawn line. */
	if (drawn) {
		MoveTo(h0, v0);  LineTo(pt1.h, pt1.v);
	}
	PenNormal();
	m_nearest_cell(map, pt1.h, pt1.v, &x, &y);
	notify(dside, "Distance from %d,%d to %d,%d is %d",
		   downx, downy, x, y, distance(downx, downy, x, y));
}

void
unselect_all(Map *map)
{
	int num = map->numselections;

	if (map->numselections > 0) {
		erase_selections(map);
		map->numselections = 0;
	}
}

/* Add the given unit to the array of units selected in the given map.  If we need
   more space, then grow the array by 50%. */

void
select_unit_on_map(Map *map, Unit *unit)
{
	if (map->numselections >= map->maxselections) {
		int newsize = map->maxselections + map->maxselections / 2;
		Unit **newarray = (Unit **) realloc((char *) map->selections, newsize * sizeof(Unit *));

		if (newarray == NULL) {
			run_warning("couldn't realloc map selection array");
			return;
		}
		map->maxselections = newsize;
		map->selections = newarray;
	}
	map->selections[map->numselections++] = unit;
}

int
unit_is_selected(Map *map, Unit *unit)
{
	int i;

	for (i = 0; i < map->numselections; ++i) {
		if (map->selections[i] == unit) return TRUE;
	}
	return FALSE;
}

void
unselect_unit_on_map(Map *map, Unit *unit)
{
	int i, j;

	for (i = 0; i < map->numselections; ++i) {
		if (map->selections[i] == unit) {
			/* Keep selection list contiguous, move other units down. */
			for (j = i + 1; j < map->numselections; ++j) {
				map->selections[j - 1] = map->selections[j];
			}
			--map->numselections;
			return;
		}
	}
}

/* Given a map and a rectangle in it, select all the units whose images touch on
   that rectangle. */

void
select_all_units_in_rect(Map *map, Rect *rectptr)
{
	int rectissmall = FALSE;
	int sx, sy, sw, sh;
	Unit *unit;
	Rect unitrect, tmprect;
	
	/* First see if we're selecting over a large area or within a single cell. */
	if (rectptr->right - rectptr->left < map->vp->hw
		&& rectptr->bottom - rectptr->top < map->vp->hh) rectissmall = TRUE;
	/* Now look at all the plausible units and see if any's image intersects the rect. */
	for_all_units(unit) {
		if (in_play(unit)
			&& (side_controls_unit(dside, unit) || endofgame)
			&& (rectissmall || unit->transport == NULL)) {
			m_xform_unit_self(map, unit, &sx, &sy, &sw, &sh);
			SetRect(&unitrect, sx, sy, sx + sw, sy + sh);
			if (SectRect(&unitrect, rectptr, &tmprect)) {
				select_unit_on_map(map, unit);
				/* (could do setport etc once...) */
				draw_selected_unit_setport(map, unit);
			}
		}
	}
}

/* This translates the user's "go to here" into appropriate tasks and/or actions. */

int
move_the_selected_unit(Map *map, Unit *unit, int h, int v)
{
	int x, y, rslt;
	Unit *other = NULL;

	m_nearest_cell(map, h, v, &x, &y);
	if (unit_at(x, y) != NULL) {
		m_nearest_unit(map, h, v, &other);
	}
	rslt = advance_into_cell(dside, unit, x, y, other);
	return rslt;
}

void
fire_the_selected_unit(Map *map, Unit *unit, int h, int v)
{
	int x, y;
	Unit *other;

	m_nearest_cell(map, h, v, &x, &y);
	if (x != unit->x || y != unit->y) {
		if (unit->act && unit->plan) { /* (should be more sophisticated test?) */
			if ((other = unit_at(x, y)) != NULL) {
				/* There's a unit to fire at. */
				if (other->side == unit->side) {
					beep();
				} else {
					prep_fire_at_action(unit, unit, other, -1);
				}
			} else {
				beep();
			}
		}
	}
}

void
select_exactly_one_unit(Map *map, Unit *unit)
{
    Unit *thisunit;

	if (map->numselections > 0) {		
		thisunit = map->selections[0];
		if (thisunit == unit) return;
	}
	unselect_all(map);
	select_unit_on_map(map, unit);
	scroll_to_unit(map, unit);
	draw_selections(map);
}

void
select_next_unit(Map *map)
{
	select_another(map, find_next_unit);
}

void
select_previous_unit(Map *map)
{
	select_another(map, find_prev_unit);
}

void
select_next_actor(Map *map)
{
	select_another(map, find_next_actor);
}

void
select_previous_actor(Map *map)
{
	select_another(map, find_prev_actor);
}

void
select_next_mover(Map *map)
{
	select_another(map, find_next_mover);
}

void
select_previous_mover(Map *map)
{
	select_another(map, find_prev_mover);
}

void
select_next_awake_mover(Map *map)
{
	select_another(map, find_next_awake_mover);
}

void
select_previous_awake_mover(Map *map)
{
	select_another(map, find_prev_awake_mover);
}

/* Given a map and a searching function, go find the "next" matching unit and select it. */

void
select_another(Map *map, Unit *(*fn)(Side *side, Unit *unit))
{
    Unit *thisunit, *nextunit;

	if (fn == NULL) {
		beep();
		return;
	}
	if (map->numselections > 0) {
		thisunit = map->selections[0];
	} else {
		thisunit = NULL;
	}
	nextunit = (*fn)(dside, thisunit);
	if (nextunit != NULL) {
		unselect_all(map);
		select_unit_on_map(map, nextunit);
		scroll_to_unit(map, nextunit);
		draw_selections(map);
		if (map->autoselect) {
			map->curunit = nextunit;
		}
	} else if (thisunit != NULL) {
		scroll_to_unit(map, thisunit);
		/* (should not be done this way, but how else?) */
		if (map->autoselect
			&& (thisunit->act && thisunit->act->acp > 0)
			&& (thisunit->plan && !thisunit->plan->asleep && !thisunit->plan->reserve && !thisunit->plan->delayed)) {
			map->curunit = thisunit;
		}
	}
}

void
scroll_best_map_to_unit(Unit *unit)
{
	Map *map, *bestmap;

	/* Find the "best" (highest power, unit already visible) map to scroll over
	   to the unit. */
	bestmap = maplist;
	for_all_maps(map) {
		if (map->vp->power > bestmap->vp->power
		    || (map->vp->power == bestmap->vp->power
		        && in_middle(map, unit->x, unit->y)
			 	&& !in_middle(bestmap, unit->x, unit->y))) {
			bestmap = map;
		}
	}
	/* We have a map, now make it show the unit. */
	if (!in_middle(bestmap, unit->x, unit->y)) {
		scroll_to_unit(bestmap, unit);
	}
	SelectWindow(bestmap->window);
	adjust_menus();
}

/* Scroll the given map over to display the given unit. */

void
scroll_to_unit(Map *map, Unit *unit)
{
	if (inside_area(unit->x, unit->y)) {
		if (!in_middle(map, unit->x, unit->y)) {
			set_view_focus(map->vp, unit->x, unit->y);
			m_center_on_focus(map);
			set_map_scrollbars(map);
			force_map_update(map);
		}
	}
}

/* This routine changes a map's viewport and magnification to fit the given rectangle. */

void
magnify_to_fit(Map *map, int x1, int y1, int x2, int y2)
{
	int wid, hgt, wanted, power;

	DGprintf("Magnifying map to fit in area %d,%d - %d,%d\n", x1, y1, x2, y2);
	/* (still need to do y/2 correction) */
	wid = abs(x2 - x1) + 1;  hgt = abs(y2 - y1) + 1;
	map->vp->vcx = min(x1, x2) + wid / 2;  map->vp->vcy = min(y1, y2) + hgt / 2;
	/* Compute the "ideal" size of a displayed cell. */
	wanted = min(map->vp->pxw / wid, map->vp->pxh / hgt);
	/* Search for the best approximation. */
	for (power = NUMPOWERS-1; power > 0; --power) {
		if (hws[power] < wanted) break;
	}
	set_map_mag(map, power);
}
