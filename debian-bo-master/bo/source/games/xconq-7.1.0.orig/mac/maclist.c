/* Unit lists for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "macconq.h"

static pascal void list_vscroll_fn(ControlHandle control, short code);
static pascal void list_hscroll_fn(ControlHandle control, short code);

/* (should be adjusted for expected range of values) */

int curactcolw = 10;
int imagecolw = 20;
int namecolw = 100;
int typecolw = 70;
int sidecolw = 70;
int hpcolw = 40;
int acpcolw = 60;
int poscolw = 60;
int supcolw = 40;
int notecolw = 50;

/* Sum of non-scrolling field widths. */

int fixedfieldwidth;

/* Sum of all field widths. */

int maxlistwidth;

/* The height of the headings lines at the top of the window. */

int listtoph = 20;

int listtopbaseline = 13;

/* The possible heights of each individual list entry. */

int smallentryspacing = 18;
int largeentryspacing = 34;

int listnum = 1;

int lastlisth = -1, lastlistv = -1;

ControlActionUPP list_vscroll_proc;
ControlActionUPP list_hscroll_proc;

/* Create a new list of units. */

void
create_list()
{
	int i;
	List *list = (List *) xmalloc(sizeof(List));
	Rect hscrollrect, vscrollrect;

	maxlistwidth = 0;
	maxlistwidth += curactcolw;
	maxlistwidth += imagecolw;
	maxlistwidth += namecolw;
	fixedfieldwidth = maxlistwidth;
	maxlistwidth += typecolw;
	maxlistwidth += sidecolw;
	maxlistwidth += hpcolw;
	maxlistwidth += acpcolw;
	maxlistwidth += poscolw;
	maxlistwidth += nummtypes * supcolw;
	maxlistwidth += notecolw;

	DGprintf("Creating a list\n");
	list->sides == -1L;  /* lists every side and indeps too */
	for (i = 0; i < MAXSORTKEYS; ++i) {
		list->sortkeys[i] = bynothing;
	}
	list->sortkeys[0] = byside;
	list->mainsortmi = miViewBySide;
	list->listglimpsed = FALSE;
	list->largeicons = FALSE; /* (should be a preference) */
	list->entryspacing = (list->largeicons ? largeentryspacing : smallentryspacing);
	list->firstvisible = 0;
	init_list_contents(list);
	organize_list_contents(list);
	list->next = listlist;
	listlist = list;
	/* Make a window for the list, give it scrollbars. */
	list->window = GetNewWindow(wList, nil, (WindowPtr) -1L);
	stagger_window(list->window, &lastlisth, &lastlistv);
	ShowWindow(list->window);
	SetPort(list->window);
	/* Create the scrollbars. */
	hscrollrect = list->window->portRect;
	hscrollrect.top = hscrollrect.bottom - sbarwid;
	hscrollrect.bottom += 1;
	hscrollrect.left = fixedfieldwidth - 1;
	hscrollrect.right -= sbarwid - 1;
	list->hscrollbar = NewControl(list->window, &hscrollrect, "\p", TRUE,
			 					  0, 0, 100, scrollBarProc, 0L);
	vscrollrect = list->window->portRect;
	vscrollrect.top = listtoph - 1;
	vscrollrect.bottom -= sbarwid - 1;
	vscrollrect.left = vscrollrect.right - sbarwid;
	vscrollrect.right += 1;
	list->vscrollbar = NewControl(list->window, &vscrollrect, "\p", TRUE,
			 					  0, 0, 100, scrollBarProc, 0L);
	/* Now set the scrollbars to their *real* limits. */
	set_list_scrollbars(list);
	sprintf(spbuf, "List %d", listnum++);
	add_window_menu_item(spbuf, list->window);
}

/* Make the list be empty. */

void
init_list_contents(List *list)
{
	/* Set up a unit vector with a first cut at needed space;
	   will adjust upwards automatically if necessary. */
	list->contents = make_unit_vector(numunits + 50);
	list->numunits = 0;
}

/* This takes the list and fills in the items it is to display. */

void
organize_list_contents(List *list)
{
	Side *side2;
	Unit *unit;

	/* Build up the array of units for this list. */
	list->numunits = 0;
	clear_unit_vector(list->contents);
	/* We always see our own units. */
	for_all_side_units(dside, unit) {
		add_unit_to_list(list, unit);
	}
	for_all_sides(side2) {
		if (dside != side2) {
			for_all_side_units(side2, unit) {
				if (side_sees_image(dside, unit)) {
					add_unit_to_list(list, unit);
				}
			}
		}
	}
	for_all_side_units(indepside, unit) {
		if (side_sees_image(dside, unit)) {
			add_unit_to_list(list, unit);
		}
	}
	/* Now sort the list according to its keys. */
	sort_list_contents(list);
}

void
sort_list_contents(List *list)
{
	int i;

	for (i = 0; i < MAXSORTKEYS; ++i) {
		tmpsortkeys[i] = list->sortkeys[i];
	}
	sort_unit_vector(list->contents);
}

void
add_unit_to_list(List *list, Unit *unit)
{
	if (alive(unit)) {
		list->contents = add_unit_to_vector(list->contents, unit, FALSE);
		/* (should apply other inclusion criteria too?) */
		++list->numunits;
	}
}

/* Calculate reasonable/valid values and maxima for the scrollbars,
   starting from the window size. */

void
set_list_scrollbars(List *list)
{
	int numvisunits, visfieldwidth, val, maxval;
	Rect winrect = list->window->portRect;

	/* Set the vertical scrollbar correctly. */	
	numvisunits = (winrect.bottom - winrect.top - sbarwid - listtoph) / list->entryspacing;
	maxval = max(0, list->numunits - numvisunits);
	val = GetCtlValue(list->vscrollbar);
	SetCtlMax(list->vscrollbar, maxval);
	if (val > maxval) {
		SetCtlValue(list->vscrollbar, maxval);
		list->firstvisible = maxval;
	}
	/* Set up the horizontal scrollbar. */
	visfieldwidth = winrect.right - winrect.left - sbarwid - fixedfieldwidth;
	val = GetCtlValue(list->hscrollbar);
	maxval = (maxlistwidth - fixedfieldwidth) - visfieldwidth;
	maxval = max(0, maxval);
	SetCtlMax(list->hscrollbar, maxval);
	if (val > maxval) {
		SetCtlValue(list->hscrollbar, maxval);
		list->firstvisfield = maxval;
	}
}

List *
list_from_window(WindowPtr window)
{
	List *list;
	
	if (dside == NULL) return NULL;
	for_all_lists(list) {
		if (list->window == window) return list;
	}
	return NULL;
}

void
draw_list(List *list)
{
	WindowPtr listwin = list->window;
	Rect tmprect, cliprect;
	RgnHandle tmprgn;

	tmprgn = NewRgn();
	GetClip(tmprgn);
	tmprect = listwin->portRect;
	tmprect.right -= sbarwid;
	tmprect.bottom -= sbarwid;
	BackPat(QDPat(white));
	EraseRect(&tmprect);
	/* Set up clipping for the contents of the list. */
	cliprect = listwin->portRect;
	cliprect.right -= sbarwid;
	tmprect.bottom -= sbarwid;
	ClipRect(&cliprect);
	TextSize(9);
	draw_list_contents(list);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
}

void
draw_list_contents(List *list)
{
	int line, numvisunits, viswidth;
	Rect winrect = list->window->portRect;

	/* Image is basically square, but add a bit of extra space on each side. */
	imagecolw = list->entryspacing + 2;
	/* Draw the selection and sorting as a sort of header. */
	draw_list_headings(list);
	/* Compute how many list elements are actually visible. */
	numvisunits = (winrect.bottom - winrect.top - listtoph - sbarwid) / list->entryspacing;
	numvisunits = min(numvisunits, list->numunits);
	list->lastvisible = list->firstvisible + numvisunits - 1;
	for (line = list->firstvisible; line <= list->lastvisible; ++line) {
		draw_unit_list_entry(list, line, FALSE);
	}
	HiliteControl(list->vscrollbar, ((numvisunits < list->numunits) ? 0 : 255));
	viswidth = winrect.right - winrect.left - sbarwid;
	HiliteControl(list->hscrollbar, ((viswidth < maxlistwidth) ? 0 : 255));
}

void
draw_list_headings(List *list)
{
	int x = 0, m;
	Str255 tmpstr;
	Rect cliprect, tmprect, winrect = list->window->portRect;
	RgnHandle tmprgn;

	/* Save the current clip region. */
	tmprgn = NewRgn();
	GetClip(tmprgn);
	cliprect = winrect;
	cliprect.right -= sbarwid;
	ClipRect(&cliprect);
	/* Clear the heading area. */
	SetRect(&tmprect, 0, 0, winrect.right - sbarwid, listtoph);
	EraseRect(&tmprect);
	/* Draw dividing lines that cross both fixed and scrolling fields. */
	MoveTo(0, listtoph - 3);
	Line(winrect.right, 0);
	MoveTo(0, listtoph - 1);
	Line(winrect.right, 0);
	/* (should underline sort keys with varying line heaviness) */
	/* We have to do MoveTo everywhere because DrawString moves the pen. */
	/* First draw headings for fields not affected by horizontal scrolling. */
	x += curactcolw;
	MoveTo(x, listtopbaseline);
	DrawString("\ps/L");
	x += imagecolw;
	MoveTo(x, listtopbaseline);
	DrawString("\pName/Number");
	/* Shift left by horiz scroll. */
	x -= list->firstvisfield;
	/* Now clip against the fixed fields' area. */
	cliprect = winrect;
	cliprect.right -= sbarwid;
	cliprect.left = fixedfieldwidth;
	ClipRect(&cliprect);
	x += namecolw;
	MoveTo(x, listtopbaseline);
	DrawString("\pType");
	x += typecolw;
	MoveTo(x, listtopbaseline);
	DrawString("\pSide");
	x += sidecolw;
	MoveTo(x, listtopbaseline);
	DrawString("\pHp");
	x += hpcolw;
	MoveTo(x, listtopbaseline);
	DrawString("\pAcp");
	x += acpcolw;
	MoveTo(x, listtopbaseline);
	DrawString("\pLoc");
	x += poscolw;
	for_all_material_types(m) {
		MoveTo(x, listtopbaseline);
		c2p(m_type_name(m), tmpstr);
		DrawString(tmpstr);
		x += supcolw;
	}
	MoveTo(x, listtopbaseline);
	DrawString("\pNotes");
	x += notecolw;
	/* Draw a gray line indicating the end of the data columns. */
	PenPat(QDPat(gray));
	MoveTo(x, 0);
	Line(0, listtoph - 4);
	PenNormal();
	/* Restore the clipping. */
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
}

/* This draws a one-line entry for the given unit. */

/* This routine does *not* save/restore the clip region, but does
   modify it, so it should not be used except in a safe context. */

void
draw_unit_list_entry(List *list, int n, int clearfirst)
{
	int u, m, x, y = (n - list->firstvisible) * list->entryspacing + listtoph;
	int texty = y + 15;
	char tmpnbuf[BUFSIZE];
	Side *side2;
	Rect entryrect, tmprect;
	Unit *unit = list->contents->units[n].unit;
	Rect cliprect;
	Rect bbox = (*(list->window->visRgn))->rgnBBox;

	SetRect(&entryrect, 0, y, list->window->portRect.right, y + list->entryspacing);
#if 0
	/* (should use rect intersection?) */
	if (!between(bbox.top, y, bbox.bottom)
	    && !between(bbox.top, y+list->entryspacing, bbox.bottom)) return;
#endif
	cliprect = list->window->portRect;
	cliprect.right -= sbarwid;
	ClipRect(&cliprect);

	if (clearfirst) {
		EraseRect(&entryrect);
	}
	if (unit == NULL || !alive(unit)) {
		/* We need to recalculate the list contents. */
		list->shouldreorg = TRUE;
		return;
	}
	u = unit->type;
	/* Draw whether the unit is awake or asleep. */
	if (unit->plan && completed(unit)) {
		SetRect(&tmprect, 0, y + list->entryspacing/2 - curactcolw/2,
				curactcolw, y + list->entryspacing/2 + curactcolw/2);
		InsetRect(&tmprect, 2, 2);
		/* (should draw the following analogously to map display) */
		if (unit->plan->asleep) {
			/* Leave rectangle blank. */
		} else if (unit->plan->reserve) {
			FillRect(&tmprect, QDPat(gray));
		} else {
			FillRect(&tmprect, QDPat(black));
		}
		FrameRect(&tmprect);
	}
	/* Draw an icon with side emblem for this unit. */
	draw_unit_image(list->window, curactcolw + 2, y + 1,
					(list->largeicons ? 32 : 16), (list->largeicons ? 32 : 16),
					u, side_number(unit->side), !completed(unit));
	/* Write the name or ordinal number. */
	name_or_number(unit, spbuf);
	spbuf[15] = '\0';  /* (should be clipping by pixels) */
	x = curactcolw + imagecolw;
	MoveTo(x, texty);
	DrawText(spbuf, 0, strlen(spbuf));
	/* Adjust according to the horizontal scroll. */
	x -= list->firstvisfield;
	cliprect = list->window->portRect;
	cliprect.right -= sbarwid;
	cliprect.left = fixedfieldwidth;
	ClipRect(&cliprect);
	/* Write the name of the unit's type. */
	x += namecolw;
	MoveTo(x, texty);
	sprintf(spbuf, "%s", u_type_name(u));
	spbuf[10] = '\0';
	DrawText(spbuf, 0, strlen(spbuf));
	/* Write the side of the unit. */
	x += typecolw;
	MoveTo(x, texty);
	side2 = unit->side;
	strcpy(tmpnbuf, shortest_side_title(side2, spbuf));
	tmpnbuf[15] = '\0'; /* truncate long side names */
	DrawText(tmpnbuf, 0, strlen(tmpnbuf));
	/* Draw the current hit points of the unit. */
	x += sidecolw;
	if (side_sees_unit(dside, unit)) {
		MoveTo(x, texty);
		hp_desc(spbuf, unit, FALSE);
		DrawText(spbuf, 0, strlen(spbuf));
	}
	x += hpcolw;
	if (side_sees_unit(dside, unit) && inside_area(unit->x, unit->y)) {
		MoveTo(x, texty);
		acp_desc(spbuf, unit, FALSE);
		if (strlen(spbuf) == 0)
		  strcpy(spbuf, "-");
		DrawText(spbuf, 0, strlen(spbuf));
	}
	/* Draw the location. */
	x += acpcolw;
	MoveTo(x, texty);
	if (inside_area(unit->x, unit->y)) {
		sprintf(spbuf, "%d,%d", unit->x, unit->y);
		if (unit->z != 0) {
			tprintf(spbuf, ",%d", unit->z);
		}
	} else {
		strcpy(spbuf, "  -  ");
	}
	DrawText(spbuf, 0, strlen(spbuf));
	/* Draw the state of all the supplies. */
	x += poscolw;
	if (side_sees_unit(dside, unit)) {
		for_all_material_types(m) {
			MoveTo(x, texty);
			sprintf(spbuf, "%d", unit->supply[m]);
			DrawText(spbuf, 0, strlen(spbuf));
			x += supcolw;
		}
	} else {
		x += nummtypes * supcolw;
	}
	spbuf[0] = '\0';
	if (side_sees_unit(dside, unit)
		&& !inside_area(unit->x, unit->y)
		&& unit_appear_turn(unit) >= 0) {
		tprintf(spbuf, "appear %s", absolute_date_string(unit_appear_turn(unit)));
	}
	if (strlen(spbuf) > 0) {
		MoveTo(x, texty);
		DrawText(spbuf, 0, strlen(spbuf));
	}
	/* Fix the clipping. */
	cliprect = list->window->portRect;
	cliprect.right -= sbarwid;
	ClipRect(&cliprect);
	/* Highlight this entry if it was selected. */
	if (list->contents->units[n].flag) {
		InvertRect(&entryrect);
	}
}

void
grow_list(List *list, int w, int h)
{
	WindowPtr listwin = list->window;

	SizeWindow(listwin, w, h, 1);
	adjust_list_decor(list);
	InvalRect(&listwin->portRect);
}

/* Zooming works like a list view in the Finder - it calculates a "perfect" size,
   showing as much as possible but with no wasted blank areas. */

void
zoom_list(List *list, int part)
{
	int maxh;
	WindowPtr listwin = list->window;

	if (part == inZoomOut) {
		maxh = listtoph + list->numunits * list->entryspacing + sbarwid;
		set_standard_state(listwin, maxlistwidth, maxh);
	}
	EraseRect(&listwin->portRect);
	ZoomWindow(listwin, part, true);
	adjust_list_decor(list);
}

/* Move and size the scrollbars to reflect the list's window. */

void
adjust_list_decor(List *list)
{
	int w, h;

	w = list->window->portRect.right - list->window->portRect.left;
	h = list->window->portRect.bottom - list->window->portRect.top;
	MoveControl(list->vscrollbar, w - sbarwid, listtoph - 1);
	SizeControl(list->vscrollbar, sbarwid + 1, h - listtoph - sbarwid + 1);
	MoveControl(list->hscrollbar, fixedfieldwidth - 1, h - sbarwid);
	SizeControl(list->hscrollbar, w - fixedfieldwidth - sbarwid + 1, sbarwid + 1);
	set_list_scrollbars(list);
}

/* Temporary used by scroll procs (saves looking up from the control). */

List *curlist;

static pascal void
list_vscroll_fn(ControlHandle control, short code)
{
	int curvalue, minvalue, maxvalue, oldvalue, pagesize, jump;
	RgnHandle tmprgn;
	Rect tmprect;
	WindowPtr listwin = curlist->window;

	curvalue = oldvalue = GetCtlValue(control);
	minvalue = GetCtlMin(control);
	maxvalue = GetCtlMax(control);
	pagesize = curlist->lastvisible - curlist->firstvisible + 1;
	switch (code) {
		case inPageDown:
			jump = max(1, pagesize - 1);
			break;
		case inDownButton:
			jump = 1;
			break;
		case inPageUp:
			jump = min(-1, - (pagesize - 1));
			break;
		case inUpButton:
			jump = -1;
			break;
		default:
			jump = 0;
			break;
	}
	curvalue = max(min(curvalue + jump, maxvalue), minvalue);
	curlist->firstvisible = curvalue;
	curlist->lastvisible = min(curlist->numunits, curlist->firstvisible + pagesize) - 1;
	SetCtlValue(control, curvalue);
	/* Scroll the already-drawn bits. */
	tmprgn = NewRgn();
	SetRect(&tmprect, 0, listtoph, listwin->portRect.right - sbarwid, listwin->portRect.bottom - sbarwid);
	ScrollRect(&tmprect, 0, (oldvalue - curvalue) * curlist->entryspacing, tmprgn);
	InvalRgn(tmprgn);
	/* We'll need to redraw the headings line. */
	draw_list_headings(curlist);
	/* Do the update now, because we won't get back to the main event loop
	   until the mouse button is released. */
	update_window(curlist->window);
	DisposeRgn(tmprgn);
}

static pascal void
list_hscroll_fn(ControlHandle control, short code)
{
	int curvalue, minvalue, maxvalue, oldvalue, pagesize, jump;
	RgnHandle tmprgn;
	Rect tmprect;
	WindowPtr listwin = curlist->window;

	oldvalue = GetCtlValue(control);
	minvalue = GetCtlMin(control);
	maxvalue = GetCtlMax(control);
	pagesize = curlist->lastvisfield - curlist->firstvisfield;
	switch (code) {
		case inPageDown:
			jump = pagesize - 10;
			break;
		case inDownButton:
			jump = 10;
			break;
		case inPageUp:
			jump = - (pagesize - 10);
			break;
		case inUpButton:
			jump = -10;
			break;
		default:
			jump = 0;
			break;
	}
	curvalue = max(min(oldvalue + jump, maxvalue), minvalue);
	curlist->firstvisfield = curvalue;
	curlist->lastvisfield = curlist->firstvisfield + pagesize;
	SetCtlValue(control, curvalue);
	/* Scroll the already-drawn bits. */
	tmprgn = NewRgn();
	SetRect(&tmprect, 0, listtoph, listwin->portRect.right - sbarwid, listwin->portRect.bottom - sbarwid);
	ScrollRect(&tmprect, 0, (oldvalue - curvalue) * curlist->entryspacing, tmprgn);
	InvalRgn(tmprgn);
	/* We'll need to redraw the headings line. */
	draw_list_headings(curlist);
	/* Do the update now, because we won't get back to the main event loop
	   until the mouse button is released. */
	update_window(curlist->window);
	DisposeRgn(tmprgn);
}

/* Handle a mouse down in the list.  Grafport already set, mouse coords are local. */

/* (mouse downs should select/deselect list elements) */

void
do_mouse_down_list(List *list, Point mouse, int mods)
{
	ControlHandle control;
	short part;
	int n, tmp;
	WindowPtr window = list->window;

	if (list_vscroll_proc == NULL)
	  list_vscroll_proc = NewControlActionProc(list_vscroll_fn);
	if (list_hscroll_proc == NULL)
	  list_hscroll_proc = NewControlActionProc(list_hscroll_fn);

	part = FindControl(mouse, window, &control);
	if (control == list->vscrollbar) {
		switch (part) {
			case inThumb:
				part = TrackControl(control, mouse, NULL);
				if (part == inThumb) {
					list->firstvisible = GetCtlValue(control);
					force_update(window);
				}
				break;
			default:
				curlist = list;
				part = TrackControl(control, mouse, list_vscroll_proc);
				list->firstvisible = GetCtlValue(control);
				break;
		}
	} else if (control == list->hscrollbar) {
		switch (part) {
			case inThumb:
				part = TrackControl(control, mouse, NULL);
				if (part == inThumb) {
					list->firstvisfield = GetCtlValue(control);
					force_update(window);
				}
				break;
			default:
				curlist = list;
				part = TrackControl(control, mouse, list_hscroll_proc);
				list->firstvisfield = GetCtlValue(control);
				break;
		}
	} else {
		if (mouse.v < listtoph) {
			if (between(curactcolw, mouse.h, curactcolw + imagecolw)) {
				toggle_list_large_icons(list);
			}
			/* do others eventually */
		} else {
			/* Figure out the selected unit. */
			n = (mouse.v - listtoph) / list->entryspacing + list->firstvisible;
			tmp = list->contents->units[n].flag;
			clear_selections(list);
			list->contents->units[n].flag = !tmp;
			redraw_unit_list_entry(list, n);
		}
	}
}

void
set_list_sorting(List *list, enum sortkeys newkey, int mi)
{
	int i;
	
	if (newkey != list->sortkeys[0]) {
		/* Push all the existing sortkeys back - this way they'll can be
		   used as tiebreakers for the new sort key. */
		for (i = MAXSORTKEYS - 1; i > 0; --i) {
			list->sortkeys[i] = list->sortkeys[i - 1];
		}
		/* Add the new one onto the front. */
		list->sortkeys[0] = newkey;
		sort_list_contents(list);
		force_update(list->window);
		/* Record the menu item so it can get a checkmark during menu adjust. */
		list->mainsortmi = mi;
	}
}

void
toggle_list_large_icons(List *list)
{
	list->largeicons = !list->largeicons;
	list->entryspacing = (list->largeicons ? largeentryspacing : smallentryspacing);
	force_update(list->window);
}

void
update_unit_in_lists(Unit *unit)
{
	int line;
	List *list;

	for_all_lists(list) {
		if ((line = unit_position_in_list(list, unit)) >= 0) {
			if (between(list->firstvisible, line, list->lastvisible)) {
				redraw_unit_list_entry(list, line);
			}
		} else {
			/* (should attempt to insert at the correct location) */
			add_unit_to_list(list, unit);
			force_update(list->window);
		}
	}
}

int
unit_position_in_list(List *list, Unit *unit)
{
	int i;
	
	for (i = 0; i < list->numunits; ++i) {
		if (unit == list->contents->units[i].unit) return i;
	}
	return (-1);
}

void
reorganize_list(List *list)
{
	organize_list_contents(list);
	force_update(list->window);
}

void
redraw_unit_list_entry(List *list, int n)
{
	WindowPtr listwin;
	Rect cliprect;
	GrafPtr oldport;
	RgnHandle tmprgn;

	if (!active_display(dside) || list == NULL) return;

	listwin = list->window;
 	GetPort(&oldport);
	SetPort(listwin);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	/* Set up clipping for the contents of the list. */
	cliprect = listwin->portRect;
	cliprect.right -= sbarwid;
	ClipRect(&cliprect);
	TextSize(9);
	draw_unit_list_entry(list, n, TRUE);
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	SetPort(oldport);
}

void
clear_selections(List *list)
{
	int i;
	
	for (i = 0; i < list->numunits; ++i) {
		if (list->contents->units[i].flag) {
			list->contents->units[i].flag = FALSE;
			redraw_unit_list_entry(list, i);
		}
	}
}

Unit *
selected_unit_in_list(List *list)
{
	int i;
	
	for (i = 0; i < list->numunits; ++i) {
		if (list->contents->units[i].flag) return list->contents->units[i].unit;
	}
	return NULL;
}

/* This finds a good map to scroll over to look at a unit mentioned in the list. */

void
scroll_to_selected_unit_in_list(List *list)
{
	Unit *unit;

	/* Beep and return if there are no maps open currently. */
	if (maplist == NULL) {
		beep();
		return;
	}
	unit = selected_unit_in_list(list);
	if (unit != NULL && inside_area(unit->x, unit->y))
	  scroll_best_map_to_unit(unit);
}

void
activate_list(List *list, int activate)
{
	Rect growRect;

	if (activate) {
		HiliteControl(list->vscrollbar, 0);
		HiliteControl(list->hscrollbar, 0);
#if 0
		/* the controls must be redrawn on activation: */
		(*(list->vscrollbar))->contrlVis = 255;
		(*(list->hscrollbar))->contrlVis = 255;
		InvalRect(&(*(list->vscrollbar))->contrlRect);
		InvalRect(&(*(list->hscrollbar))->contrlRect);
#endif
		/* The growbox needs to be redrawn on activation. */
		growRect = list->window->portRect;
		/* adjust for the scrollbars */
		growRect.top = growRect.bottom - sbarwid;
		growRect.left = growRect.right - sbarwid;
		InvalRect(&growRect);
	} else {
		/* The scrollbars must be hidden on deactivation. */
		HiliteControl(list->vscrollbar, 255);
		HiliteControl(list->hscrollbar, 255);
/*		HideControl(list->vscrollbar);
		HideControl(list->hscrollbar); */
		/* The growbox should be changed immediately on deactivation. */
		DrawGrowIcon(list->window);
	}
}

void
print_list(List *list)
{
#if 0
	TPPrPort printport;
	extern THPrint printrecordhandle;

	printport = PrOpenDoc(printrecordhandle, nil, nil);
	PrCloseDoc(printport);
#endif
}

/* Remove and destroy the list object. */

void
destroy_list(List *list)
{
	List *list2;
	
	if (listlist == list) {
		listlist = list->next;
	} else {
		for_all_lists(list2) {
			if (list2->next == list) {
				list2->next = list->next;
			}
		}
	}
	/* (should destroy substructs) */
	free(list);
}
