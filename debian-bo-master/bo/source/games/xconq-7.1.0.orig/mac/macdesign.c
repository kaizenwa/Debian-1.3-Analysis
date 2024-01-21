/* Game designer handling for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING. */

#include "conq.h"
#define make_raw_wind(d,f) (((f) << 3) | (d))
#include "macconq.h"

#ifdef DESIGNERS

#define numtooltypes 12

extern MenuHandle featuremenu;

WindowPtr designwin = nil;

ControlHandle ttypepopup = nil;
ControlHandle utypepopup = nil;
ControlHandle mtypepopup = nil;
ControlHandle sidepopup = nil;
ControlHandle feature_add_button = nil;
ControlHandle feature_remove_button = nil;
ControlHandle feature_edit_button = nil;
ControlHandle featurepopup = nil;

CursHandle paintcursors[numtooltypes];
CursHandle bordpaintor;
CursHandle connpaintor;
CursHandle coatpaintor;

/* This is the width and height of each design tool's entry. */

int dtoolw = 120;
int dtoolh = 36;

/* The type of designer tool currently in use. */

enum tooltype tooltype = notool;

/* All the state of the designer palette. */

short curbrushradius = 0;
short curttype = 0;
short curbgttype = 0;
short curdepth = 1;
short curutype = 0;
short cursidenumber = 0;
short curmtype = 0;
short curmamount = 0;
short curfid = 0;
Feature *curfeature = NULL;
short curelevation = 0;
short curtemperature = 0;
short curcloudtype = 0;
short curcloudbottom = 0;
short curcloudheight = 0;
short curwinddir = 0;
short curwindforce = 0;
short curtview;
short curuview;

/* These are globals used in drag painting. */

short painttype;
short paintpeop;
short paintfid;
short painttview;
short paintuview;

short enabledtooltype[20];

void
enable_designing(int forsure)
{
	extern int compromised;

	if (dside == NULL || dside->designer)
	  return;
	if (!forsure && !compromised) {
		switch (CautionAlert(aConfirmDesign, nil)) {
			case aiConfirmDesignOK:
				break;
			case aiConfirmDesignCancel:
				return;
		}
	}
	/* Actually change designer status, this will call back to alter all displays. */
	become_designer(dside);
	/* Create and add the designer's palette. */
	if (designwin == nil) {
		create_design_window();
	}
	if (designwin != nil) {
		position_design_window();
		ShowWindow(designwin);
/*		SelectWindow(designwin); */
	}
	/* Recache visibility flags. */
	calc_vision(dside);
}

void
disable_designing()
{
	if (dside == NULL || !dside->designer)
	  return;
	/* Hide (but don't destroy) the designer's palette. */
	if (designwin != nil) {
		HideWindow(designwin);
	}
	/* Actually change designer status, this will call back to alter all displays. */
	become_nondesigner(dside);
	/* Recache visibility flags. */
	calc_vision(dside);
}

/* Each type of design tool has a distinct cursor. */

void
init_design_cursors()
{
	paintcursors[terraintool] = GetCursor(cCell);
	bordpaintor = GetCursor(cBord);
	connpaintor = GetCursor(cConn);
	coatpaintor = GetCursor(cCoat);
	paintcursors[unittool] = GetCursor(cUnit);
	paintcursors[peopletool] = GetCursor(cPeople);
	paintcursors[materialtool] = GetCursor(cMaterial);
	paintcursors[featuretool] = GetCursor(cFeature);
	paintcursors[elevationtool] = GetCursor(cElevation);
	paintcursors[temperaturetool] = GetCursor(cTemperature);
	paintcursors[cloudstool] = GetCursor(cClouds);
	paintcursors[windstool] = GetCursor(cWinds);
	/* (should have a view-painting cursor) */
}

/* Adjust the cursor to reflect the current designer tool. */

CursPtr
adjust_designer_cursor(Point mouse, RgnHandle region)
{
	if (tooltype == terraintool && !t_is_cell(curttype)) {
		switch (t_subtype(curttype)) {
			case bordersubtype:
				return *bordpaintor;
			case connectionsubtype:
				return *connpaintor;
			case coatingsubtype:
				return *coatpaintor;
			default:
				terrain_subtype_warning("cursor adjust", curttype);
				return &QD(arrow);
		}
	}
	return *(paintcursors[tooltype]);
}

/* Create the designer tool window. */

void
create_design_window()
{
	if (hasColorQD) {
		designwin = GetNewCWindow(wDesign, NULL, (WindowPtr) -1L);
	} else {
		designwin = GetNewWindow(wDesign, NULL, (WindowPtr) -1L);
	}
	add_window_menu_item("Design", designwin);  /* until this becomes a windoid */
	/* Make the current side be the one with the display. */
	cursidenumber = side_number(dside);
	build_terrain_type_menu();
	build_unit_type_menu();
	build_side_menu();
	build_material_type_menu();
	build_feature_menu();
	ttypepopup = GetNewControl(mTerrainTypes, designwin);
	utypepopup = GetNewControl(mUnitTypes, designwin);
	sidepopup = GetNewControl(mSides, designwin);
	mtypepopup = GetNewControl(mMaterialTypes, designwin);
	featurepopup = GetNewControl(mFeatures, designwin);
	feature_add_button = GetNewControl(cFeatureAddButton, designwin);
	feature_remove_button = GetNewControl(cFeatureRemoveButton, designwin);
	feature_edit_button = GetNewControl(cFeatureEditButton, designwin);
	SizeWindow(designwin, 2 * dtoolw - 1, (numtooltypes / 2) * dtoolh - 1, 1);
	position_design_window();
	init_design_cursors();
	enabledtooltype[notool] = TRUE;
	enabledtooltype[terraintool] = TRUE;
	enabledtooltype[unittool] = TRUE;
	enabledtooltype[peopletool] = TRUE;
	enabledtooltype[featuretool] = TRUE;
	enabledtooltype[brushsizetool] = TRUE;
	enabledtooltype[materialtool] = any_materials_in_terrain;
	enabledtooltype[elevationtool] = !world_is_flat();
	enabledtooltype[temperaturetool] = any_temp_variation;
	enabledtooltype[cloudstool] = any_clouds;
	enabledtooltype[windstool] = any_wind_variation;
	enabledtooltype[viewtool] = !all_see_all;
	ShowWindow(designwin);
}

/* Try to put the palette alongside the frontmost window. */

void
position_design_window()
{
	Point pt;
	WindowPtr win;
	GrafPtr oldport;

	/* (should fix all this) */
	if ((win = FrontWindow()) != nil) {
		GetPort(&oldport);
		SetPort(win);
		SetPt(&pt, win->portRect.right + 3, win->portRect.top);
		LocalToGlobal(&pt);
		SetPort(oldport);
	} else {
		SetPt(&pt, 500, 50);
	}
	/* (should make sure is not off the edge of the screen) */
	if (pt.h + 2 * dtoolw > 640 /* is off screen entirely */) {
		SetPt(&pt, 640 - 2 * dtoolw - 2, 50);
	}
	MoveWindow(designwin, pt.h, pt.v, TRUE);
}

void
draw_design_window()
{
	int i;
	GrafPtr oldport;

	if (!active_display(dside) || designwin == nil) return;
	GetPort(&oldport);
	SetPort(designwin);
	/* Draw each tool's palette item. */
	for (i = 0; i < numtooltypes; ++i) {
		draw_design_window_tool(i);
	}
	DrawControls(designwin);
	SetPort(oldport);
}

void
draw_design_window_tool(enum tooltype tool)
{
	int enabled = TRUE, paintable = TRUE;
	char *toolname = NULL, *auxtoolname = NULL;
	Rect tmprect, imagerect;
	char imbuf[BUFSIZE];
	Str255 tmpstr;

	SetRect(&tmprect, 0, 0, dtoolw, dtoolh);
	OffsetRect(&tmprect, (tool / (numtooltypes/2)) * dtoolw, (tool % (numtooltypes/2)) * dtoolh);
	EraseRect(&tmprect);
	/* Confine the image to a square subrect on the left side of the window. */
	imagerect = tmprect;
	imagerect.right = imagerect.left + dtoolh;
	imbuf[0] = '\0';
	switch (tool) {
		case notool:
			toolname = "Normal";
			break;
		case terraintool:
			toolname = t_type_name(curttype);
			InsetRect(&imagerect, (dtoolh - hws[4]) / 2, (dtoolh - hhs[4]) / 2);
			/* Only do bg terrain type if painting cell terrain. */
			if (t_is_cell(curttype)) {
				auxtoolname = t_type_name(curbgttype);
				/* bg type is always cell subtype. */
				OffsetRect(&imagerect, 3, 3);
				draw_terrain_sample(designwin, imagerect, curbgttype);
				OffsetRect(&imagerect, -6, -6);
			}
			draw_terrain_sample(designwin, imagerect, curttype);
			break;
		case unittool:
			toolname = u_type_name(curutype);
			InsetRect(&imagerect, (dtoolh - 32) / 2, (dtoolh - 32) / 2);
		 	draw_unit_image(designwin, imagerect.left, imagerect.top, 32, 32,
		 					curutype, cursidenumber, 0);
			/* Gray out the unit if not allowed for the current side. */
			paintable = type_allowed_on_side(curutype, side_n(cursidenumber));
			break;
		case peopletool:
			toolname = shortest_side_title(side_n(cursidenumber), spbuf);
			InsetRect(&imagerect, (dtoolh - 16) / 2, (dtoolh - 16) / 2);
			draw_side_emblem(designwin, imagerect.left, imagerect.top, 16, 16,
							 cursidenumber, shadow_emblem);
			break;
		case featuretool:
			if (enabledtooltype[featuretool]) {
				if (curfeature == NULL) {
					curfeature = find_feature(curfid);
				}
				if (curfeature != NULL) {
					toolname = curfeature->name;
					auxtoolname = curfeature->typename;
				}
			} else {
				toolname = "Feature";
				enabled = FALSE;
			}
			break;
		case brushsizetool:
			OffsetRect(&imagerect, dtoolh/2 - curbrushradius, dtoolh/2 - curbrushradius);
			imagerect.right = imagerect.left + curbrushradius + 1;
			imagerect.bottom = imagerect.top + curbrushradius + 1;
			FillOval(&imagerect, QDPat(black));
			if (curbrushradius > 0) {
				sprintf(imbuf, "%d", curbrushradius);
			}
			toolname = "Brush";
			break;
		case materialtool:
			if (enabledtooltype[materialtool]) {
				toolname = m_type_name(curmtype);
				sprintf(imbuf, "%d", curmamount);
			} else {
				toolname = "Material";
				enabled = FALSE;
			}
			break;
		case elevationtool:
			if (enabledtooltype[elevationtool]) {
				sprintf(spbuf, "Elev %d", curelevation);
				toolname = spbuf;
			} else {
				toolname = "Elevation";
				enabled = FALSE;
			}
			break;
		case temperaturetool:
			if (enabledtooltype[temperaturetool]) {
				sprintf(spbuf, "Temp %d¡", curtemperature);
				toolname = spbuf;
			} else {
				toolname = "Temperature";
				enabled = FALSE;
			}
			break;
		case cloudstool:
			if (enabledtooltype[cloudstool]) {
				/* Black is not ideal here... */
				FillRect(&imagerect, QDPat(black));
				if (curcloudtype > 0) {
					InsetRect(&imagerect, (dtoolh - hws[4]) / 2, (dtoolh - hhs[4]) / 2);
					draw_clouds(imagerect.left, imagerect.top, 4, curcloudtype);
					sprintf(spbuf, "Cloudy (%d)", curcloudtype);
					toolname = spbuf;
				} else {
					toolname = "Clear";
				}
			} else {
				toolname = "Clouds";
				enabled = FALSE;
			}
			break;
		case windstool:
			if (enabledtooltype[windstool]) {
				InsetRect(&imagerect, (dtoolh - hws[4]) / 2, (dtoolh - hhs[4]) / 2);
				draw_winds(imagerect.left, imagerect.top, 4, make_raw_wind(curwinddir, curwindforce));
				if (curwindforce > 0) {
					sprintf(spbuf, "Winds %s, %d", dirnames[curwinddir], curwindforce);
					toolname = spbuf;
				} else {
					toolname = "Calm";
				}
			} else {
				toolname = "Winds";
				enabled = FALSE;
			}
			break;
		case viewtool:
			if (enabledtooltype[viewtool]) {
				toolname = "View";
			} else {
				toolname = "View";
				enabled = FALSE;
			}
			break;
		default:
			/* ??? */
			break;
	}
	TextSize(12);
	/* Draw a (short) text string in the image area. */
	if (strlen(imbuf) > 0) {
		/* (should center) */
		MoveTo(tmprect.left + (dtoolh - StringWidth(tmpstr)) / 2, tmprect.bottom - 5);
		DrawText(imbuf, 0, strlen(imbuf));
	}
	if (toolname != NULL) {
		MoveTo(tmprect.left + dtoolh, tmprect.top + (auxtoolname != NULL ? dtoolh / 4 + 4
														   : dtoolh / 2 + 5));
		if (tool == featuretool)
		  Move(- dtoolh, 0);
		DrawText(toolname, 0, strlen(toolname));
	}
	if (auxtoolname != NULL) {
		MoveTo(tmprect.left + dtoolh, tmprect.top + (dtoolh * 3) / 4);
		if (tool == featuretool)
		  Move(- dtoolh, 0);
		DrawText(auxtoolname, 0, strlen(auxtoolname));
	}
	if (!paintable) {
		gray_out_rect(&tmprect);
	}
	if (!enabled) {
		gray_out_rect(&tmprect);
	}
	/* Draw gray dividing lines. */
	PenPat(QDPat(gray));
	MoveTo(tmprect.right - 1, tmprect.top);  Line(0, dtoolh);
	MoveTo(tmprect.left, tmprect.bottom - 1);  Line(dtoolw, 0);
	PenNormal();
	/* Highlight the currently selected tool with a heavy outline rect. */
	if (tool == tooltype) {
		tmprect.bottom -= 1;  tmprect.right -= 1;
		InvertRect(&tmprect);
		InsetRect(&tmprect, 3, 3);
		InvertRect(&tmprect);
		if (!enabledtooltype[tool]) {
			InsetRect(&tmprect, -3, -3);
			gray_out_rect(&tmprect);
		}
	}
}

/* Respond to a mouse down in the designer's window. */

/* This macro implements cycling of a variable through a set of consecutive
   values, with direction controlled by the shift key.  If the limit is 0,
   then the cycling part is not done. */

#define OPTION_CYCLE(var, lo, hi, n, mods)  \
  if ((hi) - (lo) > 0) {  \
    (var) = (((var) + ((mods) & shiftKey ? -(n) : (n)) - (lo) + ((hi) - (lo))) % ((hi) - (lo))) + (lo);  \
  } else {  \
    (var) = ((var) + ((mods) & shiftKey ? -(n) : (n)));  \
  }

void
do_mouse_down_design(Point mouse, int mods)
{
	int oldtool, poppedtool, newutype, newbgttype, toolchoice;
	Feature *feature;
	Rect tmprect;
	ControlHandle control;
	long choice;
	extern int nextfid;

	tmprect.left = 0;  tmprect.right = dtoolw;
	oldtool = poppedtool = tooltype;
	toolchoice = (mouse.v / dtoolh) + (mouse.h > dtoolw ? (numtooltypes / 2) : 0);
	FindControl(mouse, designwin, &control);
	if (control == ttypepopup) {
		TrackControl(control, mouse, (void *) -1);
		choice = LoWord(GetCtlValue(control));
		if (choice > 0)
		  curttype = choice - 1;
		poppedtool = terraintool;
	} else if (control == utypepopup) {
		mark_allowed_unit_types();
		TrackControl(control, mouse, (void *) -1);
		choice = LoWord(GetCtlValue(control));
		if (choice > 0)
		  curutype = choice - 1;
		poppedtool = unittool;
	} else if (control == sidepopup) {
		mark_allowed_sides();
		TrackControl(control, mouse, (void *) -1);
		choice = LoWord(GetCtlValue(control));
		if (choice > 0) {
			cursidenumber = choice;
			if (cursidenumber > numsides)
			  cursidenumber = 0;
		}
		poppedtool = peopletool;
	} else if (control == mtypepopup) {
		TrackControl(control, mouse, (void *) -1);
		choice = LoWord(GetCtlValue(control));
		if (choice > 0) curmtype = choice - 1;
		poppedtool = materialtool;
	} else if (control == featurepopup) {
		TrackControl(control, mouse, (void *) -1);
		choice = LoWord(GetCtlValue(control));
		if (choice > 0) {
			curfid = choice - 1; /* not reliable */
			curfeature = find_feature(curfid);
		}
		poppedtool = featuretool;
	} else if (control == feature_add_button) {
		sprintf(spbuf, "%d", nextfid);
		feature = create_feature("feature", copy_string(spbuf));
		if (feature != NULL) {
			curfeature = feature;
			curfid = feature->id;
			feature_rename_dialog(feature);
			update_feature_menu(feature);
		}
		poppedtool = featuretool;
	} else if (control == feature_remove_button) {
		destroy_feature(curfeature);
		curfeature = NULL;
		curfid = 0;
		build_feature_menu();
	} else if (control == feature_edit_button) {
		feature_rename_dialog(curfeature);
		poppedtool = featuretool;
	} else if (enabledtooltype[toolchoice]) {
		/* Any other click selects the tool. */
		if (toolchoice != brushsizetool) tooltype = toolchoice;
		/* Now handle any shortcuts. */
		switch (toolchoice) {
			case notool:
				break;
			case terraintool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click cycle through all
					   the "foreground" terrain types. */
					OPTION_CYCLE(curttype, 0, numttypes, 1, mods);
				} else if ((mods & cmdKey) && t_is_cell(curttype)) {
					/* Cmd-click and Cmd-Shift-click cycle through all
					   the "background" terrain types. */
					newbgttype = curbgttype;
					do {
						OPTION_CYCLE(newbgttype, 0, numttypes, 1, mods);
						if (newbgttype == curbgttype)
						  break;
					} while (!t_is_cell(newbgttype));
					curbgttype = newbgttype;
				}
				break;
			case unittool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click cycle through all
					   the types allowed for the current side. */
					newutype = curutype;
					do {
						OPTION_CYCLE(newutype, 0, numutypes, 1, mods);
						if (newutype == curutype) break;
					} while (!type_allowed_on_side(newutype, side_n(cursidenumber)));
					curutype = newutype;
				}
				break;
			case peopletool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click cycle around all the sides. */
					OPTION_CYCLE(cursidenumber, 0, numsides + 1, 1, mods);
				}
				break;
			case featuretool:
				if (mods & optionKey && nextfid > 1) {
					/* Option-click and Option-Shift-click cycle around the features. */
					OPTION_CYCLE(curfid, 0, nextfid, 1, mods);
					curfeature = find_feature(curfid);
				}
				break;
			case brushsizetool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click cycle through brush sizes. */
					OPTION_CYCLE(curbrushradius, 0, 100, 1, mods);
				}
				break;
			case materialtool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click cycle around amounts. */
					OPTION_CYCLE(curmamount, 0, 10000, 1, mods);
				} else if (mods & cmdKey) {
					/* Command-click and Command-Shift-click adjust by 10. */
					OPTION_CYCLE(curmamount, 0, 10000, 10, mods);
				}
				break;
			case elevationtool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click adjust the elevation. */
					OPTION_CYCLE(curelevation, minelev, maxelev + 1, 1, mods);
				} else if (mods & cmdKey) {
					/* Command-click and Command-Shift-click adjust by 10. */
					OPTION_CYCLE(curelevation, minelev, maxelev + 1, 10, mods);
				}
				break;
			case temperaturetool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click adjust the temp. */
					OPTION_CYCLE(curtemperature, mintemp, maxtemp + 1, 1, mods);
				} else if (mods & cmdKey) {
					/* Command-click and Command-Shift-click adjust by 10. */
					OPTION_CYCLE(curtemperature, mintemp, maxtemp + 1, 10, mods);
				}
				break;
			case cloudstool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click adjust the cloud type. */
					OPTION_CYCLE(curcloudtype, 0, 4, 1, mods);
				}
				break;
			case windstool:
				if (mods & optionKey) {
					/* Option-click and Option-Shift-click adjust the wind force. */
					OPTION_CYCLE(curwindforce, minwindforce, maxwindforce, 1, mods);
				} else if (mods & cmdKey) {
					/* Cmd-click and Cmd-Shift-click adjust the direction. */
					OPTION_CYCLE(curwinddir, 0, NUMDIRS, 1, mods);
				}
				break;
			case viewtool:
				/* Nothing to do? */
				break;
			default:
				break;
		}
	}
	/* Draw the old and new tools. */
	if (oldtool != tooltype) {
		draw_design_window_tool(oldtool);
	}
	if (poppedtool != tooltype) {
		draw_design_window_tool(poppedtool);
	}
	draw_design_window_tool(tooltype);
	if (toolchoice != tooltype)
	  draw_design_window_tool(toolchoice);
	/* As a special case, redraw the unit tool if the side tool was touched. */
	if (tooltype == peopletool || poppedtool == peopletool) {
		draw_design_window_tool(unittool);
	}
	/* (should only draw controls in relevant tools) */
	DrawControls(designwin);
}

void
mark_allowed_unit_types()
{
	Side *side = side_n(cursidenumber);
	int u;

	for_all_unit_types(u) {
		EnableItem(utypemenu, u + 1);
		SetItemMark(utypemenu, u + 1,
					(type_allowed_on_side(u, side) ? diamondMark : noMark));
	}
}

void
mark_allowed_sides()
{
	Side *side;

	for_all_sides(side) {
		EnableItem(sidemenu, side_number(side));
		SetItemMark(sidemenu, side_number(side),
					(type_allowed_on_side(curutype, side) ? diamondMark : noMark));
	}
	EnableItem(sidemenu, numsides + 1);
	SetItemMark(sidemenu, numsides + 1,
				(type_allowed_on_side(curutype, NULL) ? diamondMark : noMark));
}

void
feature_rename_dialog(Feature *feature)
{
	short done = FALSE, ditem;
	char *newtypename, *newname;
	Str255 tmpstr;
	DialogPtr win;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (feature == NULL) return;
	win = GetNewDialog(dFeatureRename, NULL, (DialogPtr) -1L);
	/* Seed the text items with the original names. */
	newtypename = feature->typename;
	if (newtypename == NULL)
	  newtypename = "";
	GetDItem(win, diFeatureRenameType, &itemtype, &itemhandle, &itemrect);
	c2p(newtypename, tmpstr);
	SetIText(itemhandle, tmpstr);
	newname = feature->name;
	if (newname == NULL)
	  newname = "";
	GetDItem(win, diFeatureRenameName, &itemtype, &itemhandle, &itemrect);
	c2p(newname, tmpstr);
	SetIText(itemhandle, tmpstr);
	ShowWindow(win);
	while (!done) {
		/* Deactivate the front window. */
		activate_window(FrontWindow(), FALSE);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diRenameOK:
				GetDItem(win, diFeatureRenameType, &itemtype, &itemhandle, &itemrect);
				set_feature_type_name(feature, get_string_from_item(itemhandle));
				GetDItem(win, diFeatureRenameName, &itemtype, &itemhandle, &itemrect);
				set_feature_name(feature, get_string_from_item(itemhandle));
				/* Fall into next case. */
			case diRenameCancel:
				done = TRUE;
				break;
		}
	}
	DisposDialog(win);
}

/* Handling of mousedowns in the map when designing. */

void
apply_designer_tool(Map *map, int h, int v, int mods)
{
	int x, y, dir;
	int oldt, oldpeop, oldfid, oldtview, olduview;
	Unit *unit;

	m_nearest_boundary(map, h, v, &x, &y, &dir);
	switch (tooltype) {
		case terraintool:
			/* Dispatch on terrain subtype. */
			switch (t_subtype(curttype)) {
				case cellsubtype:
					/* Choose to paint fg or bg type, depending on what's already
					   there. */
					oldt = terrain_at(x, y);
					painttype = (curttype == oldt ? curbgttype : curttype);
					paint_cell(dside, x, y, curbrushradius, painttype);
					paint_on_drag(map, h, v, mods);
					break;
				case bordersubtype:
					/* Toggle border on first mouse down. */
					paint_border(dside, x, y, dir, curttype, -1);
					/* Dragging then adds or removes, depending on toggle's result. */
					border_on_drag(map, h, v, mods, border_at(x, y, dir, curttype));
					break;
				case connectionsubtype:
					/* Toggle connection on first mouse down. */
					paint_connection(dside, x, y, dir, curttype, -1);
					/* Dragging then adds or removes, depending on toggle's result. */
					connect_on_drag(map, h, v, mods, connection_at(x, y, dir, curttype));
					break;
				case coatingsubtype:
					paint_coating(dside, x, y, curbrushradius, curttype, curdepth);
					paint_on_drag(map, h, v, mods);
					break;
				default:
					terrain_subtype_warning("apply tool", curttype);
					break;
			}
			return;
		case unittool:
			/* A last check, should never(?) fail. */
			if (!type_allowed_on_side(curutype, side_n(cursidenumber))) {
				beep();
				return;
			}
			unit = designer_create_unit(dside, curutype, cursidenumber, x, y);
			if (unit != NULL) {
				/* Make the new unit automatically be the current selection. */
				unselect_all(map);
				select_unit_on_map(map, unit);
				draw_selections_at(map, unit->x, unit->y);
			} else {
				/* Beep if the creation failed for some reason. */
				beep();
			}
			/* No use for drag painting here, unlike most other tools. */
			break;
		case peopletool:
			/* Paint people or clear, inverting from what is already here. */
			oldpeop = people_side_at(x, y);
			paintpeop = (cursidenumber == oldpeop ? NOBODY : cursidenumber);
			paint_people(dside, x, y, curbrushradius, paintpeop);
			paint_on_drag(map, h, v, mods);
			break;
		case featuretool:
			oldfid = raw_feature_at(x, y);
			paintfid = (curfid == oldfid ? 0 : curfid);
			paint_feature(dside, x, y, curbrushradius, paintfid);
			paint_on_drag(map, h, v, mods);
			break;
		case materialtool:
			paint_material(dside, x, y, curbrushradius, curmtype, curmamount);
			paint_on_drag(map, h, v, mods);
			break;
		case elevationtool:
			paint_elevation(dside, x, y, curbrushradius, curelevation);
			paint_on_drag(map, h, v, mods);
			break;
		case temperaturetool:
			paint_temperature(dside, x, y, curbrushradius, curtemperature);
			paint_on_drag(map, h, v, mods);
			break;
		case cloudstool:
			paint_clouds(dside, x, y, curbrushradius, curcloudtype, curcloudbottom, curcloudheight);
			paint_on_drag(map, h, v, mods);
			break;
		case windstool:
			paint_winds(dside, x, y, curbrushradius, curwinddir, curwindforce);
			paint_on_drag(map, h, v, mods);
			break;
		case viewtool:
			if (dside->terrview == NULL || dside->unitview == NULL)
			  break;
			oldtview = terrain_view(dside, x, y);
			painttview = (curtview == oldtview ? UNSEEN : curtview);
			olduview = unit_view(dside, x, y);
			paintuview = (curuview == olduview ? EMPTY : curuview);
			paint_view(dside, x, y, curbrushradius, painttview, paintuview);
			paint_on_drag(map, h, v, mods);
			break;
		default:
			beep();
	}
}

void
paint_on_drag(Map *map, int h0, int v0, int mods)
{
	Point pt0, pt1, newmouse;
	int drawn = FALSE, x, y;
	Rect tmprect;

	SetPt(&pt0, h0, v0);
	SetPt(&pt1, h0, v0);
	SetRect(&tmprect, h0, v0, h0, v0);
	while (WaitMouseUp()) {
		GetMouse(&newmouse);
		if (!EqualPt(pt1, newmouse) /* && PtInRect(newmouse, &(map->window->portRect)) */) {
			pt1 = newmouse;
			if (m_nearest_cell(map, pt1.h, pt1.v, &x, &y)) {
				switch (tooltype) {
					case terraintool:
						/* Dispatch on terrain subtype. */
						switch (t_subtype(curttype)) {
							/* This sort of drag-paint only works for area fillers,
							   bords/conns use different algorithm. */
							case cellsubtype:
								paint_cell(dside, x, y, curbrushradius, painttype);
								break;
							case coatingsubtype:
								paint_coating(dside, x, y, curbrushradius, curttype, curdepth);
								break;
						}
						break;
					case peopletool:
						paint_people(dside, x, y, curbrushradius, paintpeop);
						break;
					case materialtool:
						paint_material(dside, x, y, curbrushradius, curmtype, curmamount);
						break;
					case featuretool:
						paint_feature(dside, x, y, curbrushradius, paintfid);
						break;
					case elevationtool:
						paint_elevation(dside, x, y, curbrushradius, curelevation);
						break;
					case temperaturetool:
						paint_temperature(dside, x, y, curbrushradius, curtemperature);
						break;
					case cloudstool:
						paint_clouds(dside, x, y, curbrushradius, curcloudtype, curcloudbottom, curcloudheight);
						break;
					case windstool:
						paint_winds(dside, x, y, curbrushradius, curwinddir, curwindforce);
						break;
					case viewtool:
						paint_view(dside, x, y, curbrushradius, curtview, curuview);
						break;
					default:
						break;
				}
			}
		}
	}
}

void
border_on_drag(Map *map, int h0, int v0, int mods, int paintmode)
{
	Point pt0, pt1, newmouse;
	int drawn = FALSE, x, y, dir;
	Rect tmprect;

	SetPt(&pt0, h0, v0);
	SetPt(&pt1, h0, v0);
	SetRect(&tmprect, h0, v0, h0, v0);
	while (WaitMouseUp()) {
		GetMouse(&newmouse);
		if (!EqualPt(pt1, newmouse) /* && PtInRect(newmouse, &(map->window->portRect)) */) {
			pt1 = newmouse;
			m_nearest_boundary(map, pt1.h, pt1.v, &x, &y, &dir);
			if (inside_area(x, y)) {
				paint_border(dside, x, y, dir, curttype, paintmode);
			}
		}
	}
}

void
connect_on_drag(Map *map, int h0, int v0, int mods, int paintmode)
{
	Point pt0, pt1, newmouse;
	int drawn = FALSE, x, y, dir;
	Rect tmprect;

	SetPt(&pt0, h0, v0);
	SetPt(&pt1, h0, v0);
	SetRect(&tmprect, h0, v0, h0, v0);
	while (WaitMouseUp()) {
		GetMouse(&newmouse);
		if (!EqualPt(pt1, newmouse) /* && PtInRect(newmouse, &(map->window->portRect)) */) {
			pt1 = newmouse;
			m_nearest_boundary(map, pt1.h, pt1.v, &x, &y, &dir);
			if (inside_area(x, y)) {
				paint_connection(dside, x, y, dir, curttype, paintmode);
			}
		}
	}
}

/* This allows the designer to choose which parts of a game to write out. */

int defunitids; /* should be module slot */

void
designer_save_dialog()
{
	int done = FALSE;
	short ditem;
	char namebuf[BUFSIZE];
	Str255 tmpstr;
	Point pnt;
	WindowPtr win;
	Module *module;
    SFReply reply;
	short itemtype;  Handle itemhandle;  Rect itemrect;
	
	win = GetNewDialog(dDesignerSave, NULL, (DialogPtr) -1L);
	module = create_game_module(NULL);
	module->title = "Designer-saved data";
	if (module == NULL) {
		beep();
		return;
	}
	init_module_reshape(module);
	/* Only rarely does the designer not want to compress all the area layers. */
	module->compress_layers = TRUE;
	GetDItem(win, diDesignerSaveCompress, &itemtype, &itemhandle, &itemrect);
	SetCtlValue((ControlHandle) itemhandle, module->compress_layers);
	while (!done) {
		/* Deactivate the front window. */
		activate_window(FrontWindow(), FALSE);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diDesignerSaveOK:
				get_string_from_ditem(diDesignerSaveName, namebuf);
				module->name = copy_string(namebuf);
				set_flag_from_ditem(diDesignerSaveTypes, module->def_types);
				set_flag_from_ditem(diDesignerSaveTables, module->def_tables);
				set_flag_from_ditem(diDesignerSaveGlobals, module->def_globals);
				set_flag_from_ditem(diDesignerSaveWorld, module->def_world);
				set_flag_from_ditem(diDesignerSaveAreas, module->def_areas);
				set_flag_from_ditem(diDesignerSaveAreaTerrain, module->def_area_terrain);
				set_flag_from_ditem(diDesignerSaveAreaMisc, module->def_area_misc);
				set_flag_from_ditem(diDesignerSaveAreaWeather, module->def_area_weather);
				set_flag_from_ditem(diDesignerSaveAreaMaterial, module->def_area_material);
				set_flag_from_ditem(diDesignerSaveSides, module->def_sides);
				set_flag_from_ditem(diDesignerSaveSideViews, module->def_side_views);
				set_flag_from_ditem(diDesignerSavePlayers, module->def_players);
				set_flag_from_ditem(diDesignerSaveUnits, module->def_units);
				set_flag_from_ditem(diDesignerSaveUnitIds, defunitids);
				set_flag_from_ditem(diDesignerSaveUnitProps, module->def_unit_props);
				set_flag_from_ditem(diDesignerSaveUnitMoves, module->def_unit_acts);
				set_flag_from_ditem(diDesignerSaveUnitPlans, module->def_unit_plans);
				set_flag_from_ditem(diDesignerSaveScoring, module->def_scoring);
				set_flag_from_ditem(diDesignerSaveHistory, module->def_history);
				set_flag_from_ditem(diDesignerSaveCompress, module->compress_layers);
				/* Collect the file and path to save to. */
				SetPt(&pnt, 100, 100);
				sprintf(spbuf, "%s.g", namebuf);
				c2p(spbuf, tmpstr);
				SFPutFile(pnt, "\p", tmpstr, /*(DlgHookProcPtr)*/ nil, &reply);
				if (reply.good) {
					/* Make the location of the file be the current volume. */
					SetVol(reply.fName, reply.vRefNum);
					p2c(((char *) reply.fName), namebuf);
					module->filename = copy_string(namebuf);
					SetCursor(*watchcursor);
					if (!write_game_module(module)) {
						run_warning("Couldn't write the module \"%s\"!", module->filename);
						/* Don't fall through, might be able to fix by changing save options. */
						break;
					}
					/* Mark as an Xconq game. */
					set_game_file_type(module->filename);
					SetCursor(&QD(arrow));
				} else {
					break;
				}
				/* Fall through to next case. */
			case diDesignerSaveCancel:
				done = TRUE;
				break;
			case diDesignerSaveTypes:
			case diDesignerSaveTables:
			case diDesignerSaveGlobals:
			case diDesignerSaveWorld:
			case diDesignerSaveAreas:
			case diDesignerSaveAreaTerrain:
			case diDesignerSaveAreaMisc:
			case diDesignerSaveAreaWeather:
			case diDesignerSaveAreaMaterial:
			case diDesignerSaveSides:
			case diDesignerSaveSideNames:
			case diDesignerSaveSideProps:
			case diDesignerSaveSideViews:
			case diDesignerSavePlayers:
			case diDesignerSaveUnits:
			case diDesignerSaveUnitIds:
			case diDesignerSaveUnitProps:
			case diDesignerSaveUnitMoves:
			case diDesignerSaveUnitPlans:
			case diDesignerSaveScoring:
			case diDesignerSaveHistory:
			case diDesignerSaveCompress:
				/* Toggle check boxes. */
				GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
				SetCtlValue((ControlHandle) itemhandle,
							!GetCtlValue((ControlHandle) itemhandle));
				break;
			case diDesignerSaveModule:
				/* (should bring up a dialog to set module properties) */
				break;
			case diDesignerSaveReshape:
				/* Bring up *another* modal dialog. */
				designer_reshape_dialog(module);
				break;
		}
	}
	DisposDialog(win);
}

/* A special dialog that allows for saving a world of different dimensions than is
   currently being used in the game. */

void
designer_reshape_dialog(Module *module)
{
	int done = FALSE;
	short ditem;
	WindowPtr win;
	Str255 tmpstr;
	short itemtype;  Handle itemhandle;  Rect itemrect;
	
	win = GetNewDialog(dDesignerReshape, NULL, (DialogPtr) -1L);
	while (!done) {
		/* Deactivate the front window. */
		activate_window(FrontWindow(), FALSE);
  		put_number_into_ditem(diDesignerReshapeOrigWidth, area.width);
  		put_number_into_ditem(diDesignerReshapeOrigHeight, area.height);
  		put_number_into_ditem(diDesignerReshapeOrigWorld, world.circumference);
  		put_number_into_ditem(diDesignerReshapeOrigSubWidth, module->subarea_width);
  		put_number_into_ditem(diDesignerReshapeOrigSubHeight, module->subarea_height);
  		put_number_into_ditem(diDesignerReshapeOrigSubX, module->subarea_x);
  		put_number_into_ditem(diDesignerReshapeOrigSubY, module->subarea_y);
  		put_number_into_ditem(diDesignerReshapeOutputSubWidth, module->final_subarea_width);
  		put_number_into_ditem(diDesignerReshapeOutputSubHeight, module->final_subarea_height);
  		put_number_into_ditem(diDesignerReshapeOutputSubX, module->final_subarea_x);
  		put_number_into_ditem(diDesignerReshapeOutputSubY, module->final_subarea_y);
  		put_number_into_ditem(diDesignerReshapeOutputWidth, module->final_width);
  		put_number_into_ditem(diDesignerReshapeOutputHeight, module->final_height);
  		put_number_into_ditem(diDesignerReshapeOutputWorld, module->final_circumference);
/*  		put_number_into_ditem(diDesignerReshapeFillTerrain, module->fill_terrain); */
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diDesignerReshapeOK:
		  		get_number_from_ditem(diDesignerReshapeOrigSubWidth, module->subarea_width);
		  		get_number_from_ditem(diDesignerReshapeOrigSubHeight, module->subarea_height);
		  		get_number_from_ditem(diDesignerReshapeOrigSubX, module->subarea_x);
		  		get_number_from_ditem(diDesignerReshapeOrigSubY, module->subarea_y);
		  		get_number_from_ditem(diDesignerReshapeOutputSubWidth, module->final_subarea_width);
		  		get_number_from_ditem(diDesignerReshapeOutputSubHeight, module->final_subarea_height);
		  		get_number_from_ditem(diDesignerReshapeOutputSubX, module->final_subarea_x);
		  		get_number_from_ditem(diDesignerReshapeOutputSubY, module->final_subarea_y);
		  		get_number_from_ditem(diDesignerReshapeOutputWidth, module->final_width);
		  		get_number_from_ditem(diDesignerReshapeOutputHeight, module->final_height);
		  		get_number_from_ditem(diDesignerReshapeOutputWorld, module->final_circumference);
/*		  		get_number_from_ditem(diDesignerReshapeFillTerrain, module->fill_terrain);  */
				/* Fall through to next case. */
			case diDesignerReshapeCancel:
				done = TRUE;
				break;
		}
	}
	DisposDialog(win);
}

#endif /* DESIGNERS */
