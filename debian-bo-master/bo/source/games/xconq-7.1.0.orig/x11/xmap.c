/* Map handling for the X11 interface to Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This file contains the high-level and general handling for map
   windows. */

#include "conq.h"
extern int num_features PARAMS ((void));
#include "xconq.h"
void update_unit_controls PARAMS ((Side *side, Map *map));

static void create_map_controls PARAMS ((Side *side, Map *map));

static void interp_key_command PARAMS ((Side *side, Map *map));

static void handle_map_click PARAMS ((Side *side, Map *map, int sx, int sy));

static void help_unit_type PARAMS ((Side *side, Map *map));

static void help_terrain_type PARAMS ((Side *side, Map *map));

static void mode_callback PARAMS ((Widget w, XtPointer client_data,
				  XtPointer call_data));
static void command_callback PARAMS ((Widget w, XtPointer client_data,
				     XtPointer call_data));
static void view_flag_callback PARAMS ((Widget w, XtPointer client_data,
				       XtPointer call_data));
static void zoom_callback PARAMS ((Widget w, XtPointer client_data,
				  XtPointer call_data));
static void map_help_callback PARAMS ((Widget w, XtPointer client_data,
				      XtPointer call_data));
static void unit_type_list_callback PARAMS ((Widget w, XtPointer client_data,
					    XtPointer call_data));
static void panner_callback PARAMS ((Widget w, XtPointer client_data,
				    XtPointer call_data));
static void porthole_callback PARAMS ((Widget w, XtPointer client_data,
				      XtPointer call_data));

static void panner_resize_handler PARAMS ((Widget w, XtPointer client_data,
					  XEvent *event, Boolean *cont));

static void MA_keypress PARAMS ((Widget w, XEvent *event, String *params,
				Cardinal *num_params));
static void MA_mapexpose PARAMS ((Widget w, XEvent *event, String *params,
				 Cardinal *num_params));
static void MA_setcoord PARAMS ((Widget w, XEvent *event, String *params,
				Cardinal *num_params));
static void MA_movelook PARAMS ((Widget w, XEvent *event, String *params,
				Cardinal *num_params));
static void MA_moveunit PARAMS ((Widget w, XEvent *event, String *params,
				Cardinal *num_params));
static void MA_distance PARAMS ((Widget w, XEvent *event, String *params,
				Cardinal *num_params));
static void MA_motionsetcoord PARAMS ((Widget w, XEvent *event, String *params,
				      Cardinal *num_params));
static void MA_toolaction PARAMS ((Widget w, XEvent *event, String *params,
				  Cardinal *num_params));
static void MA_toolselection PARAMS ((Widget w, XEvent *event, String *params,
				     Cardinal *num_params));
static void MA_movelook_ul PARAMS ((Widget wdgt, XEvent *event, String *params,
				   Cardinal *num_params));
static void MA_movelook_l  PARAMS ((Widget wdgt, XEvent *event, String *params,
				   Cardinal *num_params));
static void MA_movelook_dl PARAMS ((Widget wdgt, XEvent *event, String *params,
				   Cardinal *num_params));
static void MA_movelook_ur PARAMS ((Widget wdgt, XEvent *event, String *params,
				   Cardinal *num_params));
static void MA_movelook_r  PARAMS ((Widget wdgt, XEvent *event, String *params,
				   Cardinal *num_params));
static void MA_movelook_dr PARAMS ((Widget wdgt, XEvent *event, String *params,
				   Cardinal *num_params));
static void MA_center PARAMS ((Widget wdgt, XEvent *event, String *params,
			      Cardinal *num_params));
#if 0
static void MA_message PARAMS ((Widget wdgt, XEvent *event, String *params,
			       Cardinal *num_params));
static void SetMessageArea PARAMS ((Widget msgarea, char *msg));
#endif

static void movelook_one_cell PARAMS ((Widget w, int xdelta, int ydelta));

static void popup_ctrlpanel PARAMS ((Side *side, Map *map));
static void popdown_ctrlpanel PARAMS ((Side *side, Map *map));
static void create_ctrlpanel PARAMS ((Side *side, Map *map));
static void ctrlpanel_cancel_callback PARAMS ((Widget w, XtPointer client_data,
					      XtPointer call_data));
static void ctrlpanel_revert_callback PARAMS ((Widget w, XtPointer client_data,
					      XtPointer call_data));
static void ctrlpanel_apply_callback PARAMS ((Widget w, XtPointer client_data,
					     XtPointer call_data));
static void ctrlpanel_done_callback PARAMS ((Widget w, XtPointer client_data,
					    XtPointer call_data));

XtActionsRec map_actions_table[] = {
    { "keypress", MA_keypress },
    { "redraw", MA_mapexpose },
    { "select", MA_setcoord },
    { "movelook", MA_movelook },
    { "moveunit", MA_moveunit },
    { "distance", MA_distance },
    { "motionselect", MA_motionsetcoord },
    { "toolaction",  MA_toolaction },
    { "toolselection", MA_toolselection },
    { "movelook_ul", MA_movelook_ul },
    { "movelook_l", MA_movelook_l },
    { "movelook_dl", MA_movelook_dl },
    { "movelook_ur", MA_movelook_ur },
    { "movelook_r", MA_movelook_r },
    { "movelook_dr", MA_movelook_dr },
    { "center",	MA_center },
};

void
add_map_actions()
{
    extern XtAppContext thisapp;

    XtAppAddActions(thisapp, map_actions_table, XtNumber(map_actions_table));
}

/* Given the total width and height of a map window, calculate the sizes
   of the different subareas. */

static void
subdivide_map(side, map)
Side *side;
Map *map;
{
    /* Subdivide the window into right and left halves. */
    if (map->leftfrac == 0)
      map->leftfrac = 80;
    map->leftw = (map->leftfrac * map->totalw) / 100 ;
    /* Subdivide the left half. */
    map->toph = max(100, (20 * map->totalh / 100));
    map->pxw = map->leftw - 40 - 2 - 2;  /* this is dubious... */
    /* (should be able to tweak this) */
    map->infoh = 75;
    map->pxh = map->totalh - map->toph - map->infoh - 4;
    /* Subdivide the list area into upper and lower lists, plus panner
       space. */
    map->list1w = map->totalw - map->leftw;
    /* All of these have the same width. */
    map->list2w = map->panw = map->list1w;
    /* Compute the height of the panner by scaling by aspect ratio of
       the area. */
    map->panh = (area.height * map->panw) / area.width;
    if (map->panh > (map->totalh * 30) / 100) {
	/* Panner subarea is too big; shrink the width to fit the
	   available height. */
	map->panh = (map->totalh * 30) / 100;
	map->panw = (area.width * map->panh) / area.height;
    }
    /* Inset the panner a bit. */
    map->panw -= 8;  map->panh -= 8;
    /* Compute the side list height to be enough to show all of them. */
    map->list1h = numsides * map->sidespacing;
    /* ...but don't let it use more than 60% of available height. */
    map->list1h = min(map->list1h, (map->totalh * 60) / 100);
    /* Middle list only gets the leftovers. */
    map->list2h = map->totalh - map->list1h - map->panh;
}

/* Create a generic map object. */


#if 0
XtTranslations saved_translations;
#endif

Map *
create_map(side, power, geospec)
Side *side;
int power;
char *geospec;
{
    int x, y, w, h, m, i, sx, sy, scale;
    int fh = side->ui->fh;
    long flags;
    Map *map;
    Pixmap pic;
    char wbuff[BUFSIZE];
    Widget shell;
    Display *dpy = side->ui->dpy;

    DGprintf("Creating map, mag power %d\n", power);
    map = (Map *) xmalloc(sizeof(Map));

    /* 1000x750 pixels is "ideal" - big, but not really big. */
    map->totalw = 1000;  map->totalh = 750;
    if (!empty_string(geospec)) {
	/* This map has been given a preferred size - work to it. */
	flags = XParseGeometry(geospec, &sx, &sy, &w, &h);
	if (flags & WidthValue)
	  map->totalw = w;
	if (flags & HeightValue)
	  map->totalh = h;
	/* Note that we allow the caller to specify geometries
	   that may be larger than the available screen, on the
	   theory that people specifying geometries explicitly
	   know what they're doing. */
    } else {
	/* Shrink to fit display if the default is too big. */
	w = DisplayWidth(dpy, side->ui->screen);
	h = DisplayHeight(dpy, side->ui->screen);
	/* Leave a little room around the edges, but not too much;
	   Xconq needs all the screen space it can get. */
	map->totalw = min(map->totalw, w - w / 16);
	map->totalh = min(map->totalh, h - h / 16);
    }
    /* Compute the vertical spacing for the side list entries. */
    map->sidespacing = fh + 12;
    /* Add room to display per-side clocks if necessary. */
    if (g_rt_per_side() > 0) {
	map->sidespacing += fh;
    }
    /* Add room to display scorekeepers if necessary. */
    if (keeping_score()) {
	map->sidespacing += ((numscorekeepers + 1) / 2) * fh;
    }
    subdivide_map(side, map);

    if (power < 0) {
	/* We must want a world map; compute a power that will
	   be a good fit. */
	for (power = NUMPOWERS - 2; power > 0; --power) {
	    if (hws[power] * area.width < map->pxw
		&& hcs[power] * area.height < map->pxh
		&& hws[power+1] * area.width >= map->pxw
		&& hcs[power+1] * area.height >= map->pxh)
	      break;
	}
    }
    map->vp = new_vp();
    set_map_power(side, map, power);

    set_view_size(map->vp, map->pxw, map->pxh);
    pick_a_focus(side, &x, &y);
    set_view_focus(map->vp, x, y);
    /* Set default values for the display controls. */
    map->drawterrain = TRUE;
    map->drawgrid = TRUE;  /* should get from a resource */
    map->drawcellpats = side->ui->monochrome;
    map->drawunits = TRUE;
    map->drawnames = FALSE /* (map->hh >= 8) */;
    map->drawpeople = FALSE;
    map->drawelevations = FALSE;
    map->drawfeatureboundaries = FALSE;
    map->drawfeaturenames = FALSE;
    /* cache the highest feature number; don't forget to update this 
       if new featured are added (presently not implemented)  */
    side->ui->numfeatures = num_features();
    place_legends(side);
    for_all_material_types(m)
      map->drawresources[m] = FALSE;
    map->drawtemp = FALSE;
    map->drawweather = FALSE;
    map->seeall = all_see_all;
    /* Start the map off in generic move mode normally. */
    map->curtool = movetool;
    map->inpch = '\0';
    map->prefixarg = -1;
    map->inptype = NONUTYPE;
    /* Make the unit type string be empty. */
    map->ustr[0] = '\0';
    map->tstr[0] = '\0';
    map->savedcurx = map->savedcury = -1;
    /* Newest map goes on the front of the list. */
    map->next = side->ui->maps;
    side->ui->maps = map;

    if (map->next == NULL) {
	shell = side->ui->shell;
    } else {
	shell = XtVaCreatePopupShell("Map", topLevelShellWidgetClass,
				     side->ui->shell, 			      
				     XtNwidth, map->totalw,
				     XtNheight, map->totalh,
				     NULL);
/*	XSetWMProtocols(dpy, XtWindow(shell), &side->ui->kill_atom, 1);  */
    }

    /* Build up the complex of widgets that constitute the map window. */
    map->mainwidget =
      XtVaCreateManagedWidget("map", panedWidgetClass, shell,
			      XtNwidth, map->totalw,
			      XtNheight, map->totalh,
			      XtNorientation, XtorientHorizontal,
			      NULL);
    
    map->leftpane =
      XtVaCreateManagedWidget("leftPane", panedWidgetClass, map->mainwidget,
			      XtNwidth, map->leftw,
			      XtNmin, 50,
			      NULL);

    map->infoform =
      XtVaCreateManagedWidget("infoForm", panedWidgetClass, map->leftpane,
			      XtNmin, 150,
			      NULL);

    map->history =
      XtVaCreateManagedWidget("history", asciiTextWidgetClass, map->infoform,
			      XtNscrollHorizontal, XawtextScrollWhenNeeded,
			      XtNscrollVertical, XawtextScrollAlways,
			      XtNshowGrip, False,
			      NULL);

    /* It would be helpful sometimes to have a two-line input prompt window,
       but it chews valuable space.  Perhaps have two lines only if map
       is "large". */
    map->promptlabel =
      XtVaCreateManagedWidget("prompt", labelWidgetClass, map->infoform,
			      XtNlabel, " ",
			      XtNshowGrip, False,
			      XtNskipAdjust, True,
			      NULL);

    map->gamedate =
      XtVaCreateManagedWidget("gameDate", labelWidgetClass, map->infoform,
			      XtNlabel, " ",
			      XtNborderWidth, 0,
			      XtNshowGrip, False,
			      XtNskipAdjust, True,
			      NULL);
   /* (should split gamedate and put clock on same line...) */
   if (g_rt_per_turn() > 0 || g_rt_for_game() > 0)
     map->gameclock =
      XtVaCreateManagedWidget("gameClock", labelWidgetClass, map->infoform,
			      XtNlabel, " ",
			      XtNborderWidth, 0,
			      XtNshowGrip, False,
			      XtNskipAdjust, True,
			      NULL);
#if 0 /* this needs a home, but don't let it chew up screen space all the time */
    map->msgarea =
      XtVaCreateManagedWidget("message", labelWidgetClass, map->infoform,
			      /* Make it a two line label. */
			      XtNlabel, "one\ntwo",
			      XtNright, XtChainRight,
			      XtNleft, XtChainLeft,
			      XtNtop, XtChainBottom,
			      XtNbottom, XtChainBottom,
			      XtNfromVert, map->gameclock,
			      NULL);
#endif

    map->leftform =
      XtVaCreateManagedWidget("leftForm", panedWidgetClass, map->leftpane,
			      XtNmin, 50,
			      XtNorientation, XtorientHorizontal,
			      NULL);

    map->controlform =
      XtVaCreateManagedWidget("controlForm", formWidgetClass, map->leftform,
			      XtNmax, 150,
			      XtNmin, 50,
			      NULL);
    XawFormDoLayout(map->controlform, False);
    create_map_controls(side, map);
    XawFormDoLayout(map->controlform, True);

    map->mapform =
      XtVaCreateManagedWidget("mapForm", panedWidgetClass, map->leftform,
			      XtNmin, 50,
			      XtNorientation, XtorientVertical,
			      NULL);

    map->info =
      XtVaCreateManagedWidget("info", widgetClass, map->mapform,
			      XtNwidth, map->pxw,
			      XtNheight, map->infoh,
			      /* Insist that at least 2 lines be visible. */
			      XtNmin, 2 * fh,
			      /* ...might be useful to have large info window,
			         so don't limit its max size. */
			      NULL);
    XtAddEventHandler(map->info,
    		      KeyPressMask|ButtonPressMask|ButtonReleaseMask
		      |ExposureMask|StructureNotifyMask,
    		      False, handle_map_info_events, NULL);

    map->porthole =
      XtVaCreateManagedWidget("porthole", portholeWidgetClass, map->mapform,
			      XtNwidth, map->pxw,
			      XtNheight, map->pxh,
			      /* Don't let the map disappear completely. */
			      XtNmin, 50,
			      NULL);
    XtAddCallback(map->porthole, XtNreportCallback,
		  porthole_callback, NULL);
    map->portlabel =
      XtVaCreateManagedWidget("portholeLabel", labelWidgetClass, map->porthole,
			      XtNinternalHeight, 0,
			      XtNinternalWidth, 0,
			      XtNwidth, map->pxw,
			      XtNheight, map->pxh,
			      NULL);

    /* The right side of the window is dedicated to lists and to the
       map panner. */

    map->rightpane =
      XtVaCreateManagedWidget("rightPane", panedWidgetClass, map->mainwidget,
			      /* Let this side be small but not too small. */
			      XtNmin, 50,
			      NULL);

    map->sides =
      XtVaCreateManagedWidget("sides", widgetClass, map->rightpane,
			      XtNwidth, map->list1w,
			      XtNheight, map->list1h,
			      /* At least one side should be visible always. */
			      XtNmin, map->sidespacing,
			      /* No reason to grow to more than numsides. */
			      XtNmax, numsides * map->sidespacing,
			      NULL);
    XtAddEventHandler(map->sides,
    		      KeyPressMask|ButtonPressMask|ButtonReleaseMask
		      |ExposureMask|StructureNotifyMask,
    		      False, handle_map_sides_events, NULL);

    map->listview =
      XtVaCreateManagedWidget("listView", viewportWidgetClass, map->rightpane,
			      XtNallowVert, True,
			      NULL);
    map->listform =
      XtVaCreateManagedWidget("listForm", formWidgetClass, map->listview,
			      NULL);

    map->list_buttons = (Widget *) xmalloc(sizeof (Widget) * (numutypes + 1));
    for (i = 0; i <= numutypes; i++) {
	pic = XCreatePixmap(dpy, side->ui->rootwin,
			    min_w_for_unit_image, min_h_for_unit_image,
			    DefaultDepth(dpy, side->ui->screen));
	XSetForeground(dpy, side->ui->gc, side->ui->bgcolor);
	XFillRectangle(dpy, pic, side->ui->gc,
		       0, 0, min_w_for_unit_image, min_h_for_unit_image);
	XSetForeground(dpy, side->ui->gc, side->ui->fgcolor);
	if (i == 0) {
	    map->list_buttons[i] =
	      XtVaCreateManagedWidget("utype_", commandWidgetClass, map->listform,
				      XtNborderWidth, 0,
				      XtNheight, min_h_for_unit_image + 4,
				      XtNjustify, XtJustifyLeft,
				      XtNlabel, "  Num(Bld)  Gain  Loss",
				      XtNleftBitmap, pic,
				      XtNresize, False,
				      NULL);
#if 0
	    XtVaGetValues(map->list_buttons[i],
			  XtNtranslations, &saved_translations,
			  NULL);
#endif
	    /* We never do anything with this "button". */
	    XtUninstallTranslations(map->list_buttons[i]);
	} else {
	    /* Draw the unit image into the pixmap.
	       NOTE: The command widget expects the leftBitmap to be in
	       "bitmap" format - a 1 bit gets drawn in the foreground
	       color and a 0 bit in the background color.
	       That's why we pass in 1 & 0 to draw_unit_image() here for
	       fgcolor & bgcolor. */
	    draw_unit_image(side, pic, 0, 0,
			    min_w_for_unit_image, min_h_for_unit_image,
			    i - 1, -1, 1, 0);

	    /* Name the widget. */
	    build_name(wbuff, "utype_", u_type_name(i - 1));
	    map->list_buttons[i] =
	      XtVaCreateManagedWidget(wbuff, commandWidgetClass, map->listform,
				      /* 0 looks better, but use 1 for debugging. */
				      XtNborderWidth, 1,
				      XtNheight, min_h_for_unit_image + 4,
				      XtNhighlightThickness, 0,
				      XtNjustify, XtJustifyLeft,
				      XtNlabel, "                      ",
				      XtNleftBitmap, pic,
				      XtNresize, False,
				      /* Tie to next button up. */
				      XtNfromVert, map->list_buttons[i - 1],
				      NULL);
	    XtAddCallback(map->list_buttons[i], XtNcallback,
			  unit_type_list_callback, (XtPointer) i);
	    update_unit_type_list(side, map, i - 1);
	}
    }
    /* (should fix this calculation) */
    sx = (map->panw * 100) / (map->vp->totsw - hexagon_adjust(map->vp));
    sy = (map->panh * 100) / map->vp->totsh;
    scale = min(sx, sy);

    map->pannerbox =
      XtVaCreateManagedWidget("pannerBox", boxWidgetClass, map->rightpane,
			      XtNmin, map->panh + 8,
			      XtNmax, map->panh + 8,
			      XtNshowGrip, False,
			      XtNskipAdjust, True,
			      NULL);
    map->panner =
      XtVaCreateManagedWidget("panner", pannerWidgetClass, map->pannerbox,
			      XtNwidth, map->panw,
			      XtNheight, map->panh,
			      XtNcanvasWidth, map->vp->totsw - hexagon_adjust(map->vp),
			      XtNcanvasHeight, map->vp->totsh,
			      XtNdefaultScale, scale,
			      /* Don't let the panner's size change,
				 should recalculate the scale instead. */
			      XtNresize, False,
			      XtNrubberBand, True,
			      XtNsliderWidth, map->pxw,
			      XtNsliderHeight, map->pxh,
			      NULL);
    XtAddCallback(map->panner, XtNreportCallback,
		  panner_callback, NULL);

    XtAddEventHandler(map->panner, StructureNotifyMask, False,
		      panner_resize_handler, NULL);

    XtRealizeWidget(shell);

    x_center_on_focus(side, map);
    clear_prompt(side, map);

    /* Make the display appear. */
    XtPopup(shell, XtGrabNone);

    /* Set the cursor to match the current tool. */
    set_tool_cursor(side, map);

    /* Cache some windows into their own slots, for convenience later. */
    map->infowin = XtWindow(map->info);
    map->viewwin = XtWindow(map->portlabel);
    map->sideswin = XtWindow(map->sides);

    /* initialize panner pixmap */
    map->panner_pix = None;

    draw_map(side, map);

    flush_output(side);

#if 0  /* there seems not to be any way to flush out widget state tweaks
	  if this is done - feel free to prove me wrong! */
    for (i = 0; i <= numutypes; i++) {
	/* Don't do anything if mouse passes over. */
	XtUninstallTranslations(map->list_buttons[i]);
    }
#endif

    return map;
}

static void
create_map_controls(side, map)
Side *side;
Map *map;
{
    int wid = 80;

    map->controls = (Widget *) xmalloc(numcontrols * sizeof(Widget));

    map->controls[LOOK] =
      XtVaCreateManagedWidget("surveyMode", toggleWidgetClass, map->controlform,
			      XtNlabel, "Survey",
#if 1 /* X11R5 or later */
			      XtNleftBitmap, side->ui->controlpics[LOOK],
#endif
			      XtNresize, False,
			      XtNwidth, wid,
			      NULL);
    XtAddCallback(map->controls[LOOK], XtNcallback,
		  mode_callback, (XtPointer) LOOK);
    map->controls[MOVE] =
      XtVaCreateManagedWidget("moveMode", toggleWidgetClass, map->controlform,
			      XtNlabel, " Move ",
#if 1 /* X11R5 or later */
			      XtNleftBitmap, side->ui->controlpics[MOVE],
#endif
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[LOOK],
			      NULL);
    XtAddCallback(map->controls[MOVE], XtNcallback,
		  mode_callback, (XtPointer) MOVE);

    map->controls[UNIT_MOVE] =
      XtVaCreateManagedWidget("unitMove", commandWidgetClass, map->controlform,
			      XtNlabel, "MoveTo",
#if 1 /* X11R5 or later */
			      XtNleftBitmap, side->ui->controlpics[UNIT_MOVE],
#endif
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[MOVE],
			      XtNvertDistance, 10,
			      NULL);
    XtAddCallback(map->controls[UNIT_MOVE], XtNcallback,
		  command_callback, (XtPointer) UNIT_MOVE);
    map->controls[UNIT_SHOOT] =
      XtVaCreateManagedWidget("unitFire", commandWidgetClass, map->controlform,
			      XtNlabel, "Fire",
#if 1 /* X11R5 or later */
			      XtNleftBitmap, side->ui->controlpics[UNIT_SHOOT],
#endif
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[UNIT_MOVE],
			      NULL);
    XtAddCallback(map->controls[UNIT_SHOOT], XtNcallback,
		  command_callback, (XtPointer) UNIT_SHOOT);
    map->controls[UNIT_BUILD] =
      XtVaCreateManagedWidget("unitBuild", commandWidgetClass, map->controlform,
			      XtNlabel, "Build",
#if 1 /* X11R5 or later */
			      XtNleftBitmap, side->ui->controlpics[UNIT_BUILD],
#endif
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[UNIT_SHOOT],
			      NULL);
    XtAddCallback(map->controls[UNIT_BUILD], XtNcallback,
		  command_callback, (XtPointer) UNIT_BUILD);

    map->controls[SHOW_TERRAIN] =
      XtVaCreateManagedWidget("terrain", toggleWidgetClass, map->controlform,
			      XtNlabel, "Terrain",
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[UNIT_BUILD],
			      XtNvertDistance, 10,
			      NULL);
    XtAddCallback(map->controls[SHOW_TERRAIN], XtNcallback,
		  view_flag_callback, (XtPointer) SHOW_TERRAIN);
    map->controls[SHOW_GRID] =
      XtVaCreateManagedWidget("grid", toggleWidgetClass, map->controlform,
			      XtNlabel, "Grid",
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[SHOW_TERRAIN],
			      NULL);
    XtAddCallback(map->controls[SHOW_GRID], XtNcallback,
		  view_flag_callback, (XtPointer) SHOW_GRID);
    map->controls[SHOW_UNITS] =
      XtVaCreateManagedWidget("units", toggleWidgetClass, map->controlform,
			      XtNlabel, "Units",
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[SHOW_GRID],
			      NULL);
    XtAddCallback(map->controls[SHOW_UNITS], XtNcallback,
		  view_flag_callback, (XtPointer) SHOW_UNITS);
    map->controls[SHOW_NAMES] =
      XtVaCreateManagedWidget("names", toggleWidgetClass, map->controlform,
			      XtNlabel, "Names",
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[SHOW_UNITS],
			      NULL);
    XtAddCallback(map->controls[SHOW_NAMES], XtNcallback,
		  view_flag_callback, (XtPointer) SHOW_NAMES);
    map->controls[SHOW_PEOPLE] =
      XtVaCreateManagedWidget("people", toggleWidgetClass, map->controlform,
			      XtNlabel, "People",
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[SHOW_NAMES],
			      NULL);
    XtAddCallback(map->controls[SHOW_PEOPLE], XtNcallback,
		  view_flag_callback, (XtPointer) SHOW_PEOPLE);
    map->controls[ZOOM_OUT] =
      XtVaCreateManagedWidget("zoomOut", commandWidgetClass, map->controlform,
			      XtNbitmap, side->ui->controlpics[ZOOM_OUT],
			      XtNresize, False,
			      XtNwidth, wid / 2 - 3,
			      XtNfromVert, map->controls[SHOW_PEOPLE],
			      XtNvertDistance, 10,
			      NULL);
    XtAddCallback(map->controls[ZOOM_OUT], XtNcallback,
		  zoom_callback, (XtPointer) ZOOM_OUT);
    map->controls[ZOOM_IN] =
      XtVaCreateManagedWidget("zoomIn", commandWidgetClass, map->controlform,
			      XtNbitmap, side->ui->controlpics[ZOOM_IN],
			      XtNresize, False,
			      XtNwidth, wid / 2 - 3,
			      XtNfromVert, map->controls[SHOW_PEOPLE],
			      XtNfromHoriz, map->controls[ZOOM_OUT],
			      XtNvertDistance, 10,
			      NULL);
    XtAddCallback(map->controls[ZOOM_IN], XtNcallback,
		  zoom_callback, (XtPointer) ZOOM_IN);
    map->controls[SHOW_ALL] =
      XtVaCreateManagedWidget("all", toggleWidgetClass, map->controlform,
			      XtNlabel, "All",
			      XtNresize, False,
			      XtNsensitive, !all_see_all,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[ZOOM_OUT],
			      XtNvertDistance, 10,
			      NULL);
    XtAddCallback(map->controls[SHOW_ALL], XtNcallback,
		  view_flag_callback, (XtPointer) SHOW_ALL);
    map->controls[SHOW_MORE] =
      XtVaCreateManagedWidget("more", toggleWidgetClass, map->controlform,
			      XtNlabel, "More...",
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[SHOW_ALL],
			      NULL);
    XtAddCallback(map->controls[SHOW_MORE], XtNcallback,
		  view_flag_callback, (XtPointer) SHOW_MORE);

    map->controls[POPUP_HELP] =
      XtVaCreateManagedWidget("help", toggleWidgetClass, map->controlform,
			      XtNlabel, "Help",
			      XtNresize, False,
			      XtNwidth, wid,
			      XtNfromVert, map->controls[SHOW_MORE],
			      NULL);
    XtAddCallback(map->controls[POPUP_HELP], XtNcallback,
		  map_help_callback, NULL);


    update_controls(side, map);
}

void
set_map_power(side, map, power)
Side *side;
Map *map;
int power;
{
    set_view_power(map->vp, power);
    if (map->panner) {
	XtVaSetValues(map->panner,
		      XtNcanvasWidth, map->vp->totsw - hexagon_adjust(map->vp),
		      XtNcanvasHeight, map->vp->totsh,
		      /* what about slider size? */
		      NULL);
    }
}

void
x_center_on_focus(side, map)
Side *side;
Map *map;
{
    center_on_focus(map->vp);
    if (map->panner) {
	XtVaSetValues(map->panner,
		      XtNsliderX, map->vp->sx - hexagon_adjust(map->vp),
		      XtNsliderY, map->vp->sy,
		      NULL);
    }
}

static void
MA_setcoord(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int cellx, celly;
    Side *side;
    Map *map;
    XButtonEvent *btn = &event->xbutton;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    side->ui->mapdown = map;
    side->ui->sxdown = btn->x;  side->ui->sydown = btn->y;
    side->ui->cellxy_ok = x_nearest_cell(side, map, btn->x, btn->y, &cellx, &celly);
    if (side->ui->cellxy_ok) {
	side->ui->cellx = cellx;  side->ui->celly = celly;
    }
}

static void
MA_motionsetcoord(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int cellx, celly;
    Side *side;
    Map *map;
    XMotionEvent *mtn = &event->xmotion;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    side->ui->mapdown = map;
    side->ui->sxdown = mtn->x;  side->ui->sydown = mtn->y;
    side->ui->cellxy_ok = x_nearest_cell(side, map, mtn->x, mtn->y, &cellx, &celly);
    if (side->ui->cellxy_ok) {
	side->ui->cellx = cellx;  side->ui->celly = celly;
    }
}

static void
MA_movelook(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int cellx, celly;
    Unit *unit;
    Side *side;
    Map *map;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    if (side->ui->cellxy_ok) {
	cellx = side->ui->cellx;  celly = side->ui->celly;
	if (inside_area(cellx, celly)) {
	    x_nearest_unit(side, map, side->ui->sxdown, side->ui->sydown, &unit);
	    if (unit != NULL &&
		(side_controls_unit(side, unit) || map->seeall)) {
		set_current_unit(side, map, unit);
	    } else {
		set_current_xy(side, map, cellx, celly);
	    }
	} else {
	    beep(side);
	}
    }
}

static void
MA_movelook_ul(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    movelook_one_cell(w, dirx[NORTHWEST], diry[NORTHWEST]);
}

static void
MA_movelook_l(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    movelook_one_cell(w, dirx[WEST], diry[WEST]);
}

static void
MA_movelook_dl(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    movelook_one_cell(w, dirx[SOUTHWEST], diry[SOUTHWEST]);
}

static void
MA_movelook_ur(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    movelook_one_cell(w, dirx[NORTHEAST], diry[NORTHEAST]);
}

static void
MA_movelook_r(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    movelook_one_cell(w, dirx[EAST], diry[EAST]);
}

static void
MA_movelook_dr(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    movelook_one_cell(w, dirx[SOUTHEAST], diry[SOUTHEAST]);
}

static void
movelook_one_cell(w, xdelta, ydelta)
Widget w;
int xdelta, ydelta;
{
    Side *side;
    Map *map;
    Unit *unit;
    int nx, ny;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    nx = map->curx + xdelta;  ny = map->cury + ydelta;
    if (inside_area(nx, ny)) {
	unit = unit_at(nx, ny);
	if (unit != NULL &&
	    (side_controls_unit(side, unit) || map->seeall)) {
	    set_current_unit(side, map, unit);
	} else {
	    set_current_xy(side, map, nx, ny);
	}
    } else {
	beep(side);
    }
}

static void
MA_center(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    recenter(side, map, map->curx, map->cury);
}

static void
MA_moveunit(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    if (side->ui->cellxy_ok) {
	if (map->curunit && side_controls_unit(side, map->curunit)) {
	    move_the_selected_unit(side, map, map->curunit,
				   side->ui->sxdown, side->ui->sydown);
	} else {
	    beep(side);
	}
    }
}

static void
MA_distance(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;
    XButtonEvent *btn = &event->xbutton;
    int upcellx, upcelly;
    int dist;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;
    if (side->ui->cellxy_ok) {
	if (x_nearest_cell(side, map, btn->x, btn->y, &upcellx, &upcelly)) {
	    dist = distance(side->ui->cellx, side->ui->celly, upcellx, upcelly);
	    notify(side, "The distance is %d cells.", dist);
	} else {
	    beep(side);
	}
    }
}

static void
MA_keypress(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;
    XKeyEvent *key = &event->xkey;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    side->ui->mapdown = map;
    side->ui->sxdown = key->x;  side->ui->sydown = key->y;
    handle_key_event(side, map, event);
}

static void
MA_mapexpose(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;
    XEvent qevent;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    /* Do nothing if more expose events are following. */
    if (event->type == Expose && event->xexpose.count > 0)
      return;

    /* Clear the queue of other Expose event directed to viewwin. */
    while (XCheckTypedWindowEvent(side->ui->dpy, map->viewwin,
				  Expose, &qevent))
      ;

    draw_map(side, map);
}

static void
MA_toolaction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;
    XButtonEvent *btn = &event->xbutton;
    
    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

    handle_map_click(side, map, btn->x, btn->y);
}

/* Set the current designer paint/add/etc type from what's under the cursor. */

static void
MA_toolselection(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;
    XButtonEvent *btn = &event->xbutton;

    if (!find_side_and_map_via_porthole(w, &side, &map))
      return;

#ifdef DESIGNERS
    set_designer_cur_from_map(side, map, btn->x, btn->y);
#else
    beep(side);
#endif /* DESIGNERS */
}

int
find_side_and_map_via_control(w, sidep, mapp)
Widget w;
Side **sidep;
Map **mapp;
{
    Side *side;
    Map *map;

    for_all_sides(side) {
	if (active_display(side)) {
	    if (XtDisplay(w) == side->ui->dpy) {
		for_all_maps(side, map) {
		    if (map->controlform == XtParent(w)) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
		}
	    }
	}
    }
    return FALSE;
}

int
find_side_and_map_via_a_toplevel(w, sidep, mapp)
Widget w;
Side **sidep;
Map **mapp;
{
    Side *side;
    Map *map;

    for_all_sides(side) {
	if (active_display(side)) {
	    if (XtDisplay(w) == side->ui->dpy) {
		for_all_maps(side, map) {
		    if (side->ui->shell == w) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
		    if (side->ui->help_shell == w) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
#ifdef DESIGNERS
		    if (side->ui->design_shell == w) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
#endif /* DESIGNERS */
		}
	    }
	}
    }
    return FALSE;
}

int
find_side_and_map_via_listform(w, sidep, mapp)
Widget w;
Side **sidep;
Map **mapp;
{
    Side *side;
    Map *map;

    for_all_sides(side) {
	if (active_display(side)) {
	    if (XtDisplay(w) == side->ui->dpy) {
		for_all_maps(side, map) {
		    if (map->listform == XtParent(w)) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
		}
	    }
	}
    }
    return FALSE;
}

int
find_side_and_map_via_mapform(w, sidep, mapp)
Widget w;
Side **sidep;
Map **mapp;
{
    Side *side;
    Map *map;

    for_all_sides(side) {
       /* don't check side->ui->active, or we'll break porthole_callback; Massimo */ if (side && side->ui) {
	    if (XtDisplay(w) == side->ui->dpy) {
		for_all_maps(side, map) {
		    if (map->mapform == XtParent(w)) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
		}
	    }
	}
    }
    return FALSE;
}

int
find_side_and_map_via_porthole(w, sidep, mapp)
Widget w;
Side **sidep;
Map **mapp;
{
    Side *side;
    Map *map;

    for_all_sides(side) {
	if (active_display(side)) {
	    if (XtDisplay(w) == side->ui->dpy) {
		for_all_maps(side, map) {
		    if (map->porthole == XtParent(w)) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
		}
	    }
	}
    }
    return FALSE;
}

int
find_side_and_map_via_rightform(w, sidep, mapp)
Widget w;
Side **sidep;
Map **mapp;
{
    Side *side;
    Map *map;

    for_all_sides(side) {
	if (active_display(side)) {
	    if (XtDisplay(w) == side->ui->dpy) {
		for_all_maps(side, map) {
		    if (map->rightpane == XtParent(w)
		        || map->rightpane == XtParent(XtParent(w))) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
		}
	    }
	}
    }
    return FALSE;
}

/* Interpret a keyboard event, either as a single-key command or as
   part of some modal interaction. */

void
handle_key_event(side, map, evt)
Side *side;
Map *map;
XEvent *evt;
{
    char buf[4], ch;
    int nchar, cancelled;
    void (*fn) PARAMS ((Side *sidex, Map *mapx, int cancelledx));

    nchar = XLookupString(&(evt->xkey), buf, 4, NULL, NULL);
    if (nchar > 0) {
	/* Collect only the first char of a possible sequence. */
	ch = buf[0];
	DGprintf("%s typed char '%c'\n", side_desig(side), ch);
	/* so we can do ^L even while typing a string */
	if (ch == REDRAW_CHAR) {
	    redraw(side);
	} else if (map != NULL) {
	    /* Save the char, we'll need it. */
	    map->inpch = ch;
	    /* Call the modal handler if defined, giving it side
	       and cancel flag. */
	    if (map->modalhandler) {
		fn = map->modalhandler;
		cancelled = (ch == ESCAPE_CHAR);
		/* Remove the handler - will restore itself if needed. */
		map->modalhandler = NULL;
		(*fn)(side, map, cancelled);
		if (cancelled) {
		    /* Clean up any leftover interaction. */
		    clear_prompt(side, map);
		    notify(side, "Cancelled.");
		}
	    } else {
		interp_key_command(side, map);
	    }
	} else {
	    beep(side);
	}
    } else {
	/* pretty strange, ok to do nothing? */
    }
}

static void
interp_key_command(side, map)
Side *side;
Map *map;
{
    DGprintf("%s keyboard input: %c (%d)\n",
	     side_desig(side), map->inpch, map->prefixarg);
    if (isdigit(map->inpch)) {
	if (map->prefixarg < 0) {
	    map->prefixarg = 0;
	} else {
	    map->prefixarg *= 10;
	}
	map->prefixarg += (map->inpch - '0');
    	sprintf(map->prompt, "%d:", map->prefixarg);
	map->answer[0] = '\0';
	draw_prompt(side, map);
	return;
    } else if (map->inpch == BACKSPACE_CHAR || map->inpch == DELETE_CHAR) {
	beep(side);
    } else if (map->inpch == ESCAPE_CHAR) {
	map->prefixarg = -1;
	clear_prompt(side, map);
    } else {
	clear_prompt(side, map);
	map->frombutton = FALSE;
	execute_command(side, map);
	/* Reset the argument for next time. */
	map->prefixarg = -1;
    }
}

void
handle_map_sides_events (w, clientdata, evt, contdispatch)
Widget w;
XtPointer clientdata;
XEvent *evt;
Boolean *contdispatch;
{
    int s;
    Side *side;
    Map *map;

    if (find_side_and_map_via_rightform(w, &side, &map)) {
	switch (evt->type) {
	  case KeyPress:
	    handle_key_event(side, map, evt);
	    break;
	  case ButtonPress:
	    s = evt->xbutton.y / (side->ui->fh + 12) + 1;
	    if (s < 0)
	      s = 0;
	    if (s > numsides) 
	      s = numsides;
#ifdef DESIGNERS
	    if (side->designer && side->ui->design_shell) {
		side->ui->cursidenumber = s;
		update_cursidenumber(side);
	    }
#endif /* DESIGNERS */
	    break;
	  case Expose:
	    draw_map_sides(side, map);
	    break;
	  default:
	    DGprintf("Unhandled X event type %d, ignoring\n", evt->type);
	    break;
	}
    }
}

void
handle_map_info_events (w, clientdata, evt, contdispatch)
Widget w;
XtPointer clientdata;
XEvent *evt;
Boolean *contdispatch;
{
    Side *side;
    Map *map;

    if (find_side_and_map_via_mapform(w, &side, &map)) {
	switch (evt->type) {
	  case KeyPress:
	    handle_key_event(side, map, evt);
	    break;
	  case ButtonPress:
	    beep(side);
	    break;
	  case Expose:
	    draw_map_info(side, map);
	    break;
	  default:
	    DGprintf("Unhandled X event type %d, ignoring\n", evt->type);
	    break;
	}
    }
}

/* Prompt for a type of a unit from player, maybe only allowing some types
   to be accepted.  Also allow specification of no unit type.  We do this
   by scanning the vector, building a string of chars and a vector of
   unit types, so as to be able to map back when done. */

int
ask_unit_type(side, map, prompt, possibles, handler)
Side *side;
Map *map;
char *prompt;
int *possibles;
void (*handler)();
{
    int u, numtypes = 0;

    for_all_unit_types(u) {
	if (possibles == NULL || possibles[u]) {
	    map->uvec[numtypes] = u;
	    map->ustr[numtypes] = utype_name_n(u, 1)[0];
	    ++numtypes;
	    enable_in_unit_type_list(side, map, u, 1);
	} else {
	    enable_in_unit_type_list(side, map, u, -1);
	}
    }
    map->ustr[numtypes] = '\0';
    if (numtypes > 1) {
	sprintf(map->prompt, "%s [%s]", prompt, map->ustr);
	map->answer[0] = '\0';
	draw_prompt(side, map);
	map->modalhandler = handler;
    }
    return numtypes;
}

/* Do something with the char or unit type that the player entered. */

int
grok_unit_type(side, map, typep)
Side *side;
Map *map;
int *typep;
{
    int i, u;

    *typep = NONUTYPE;
    if (map->inptype != NONUTYPE) {
	*typep = map->inptype;
	/* Reset so doesn't affect subsequent unit type queries. */
	map->inptype = NONUTYPE;
    } else if (map->inpch != '\0') {
	if (map->inpch == '?') {
	    help_unit_type(side, map);
	    return FALSE;
	}
	i = iindex(map->inpch, map->ustr);
	if (i >= 0) {
	    *typep = map->uvec[i];
	} else {
	    notify(side, "Must type a unit type char from the list, or <esc>");
	    return FALSE;
	}
    } else {
	notify(side, "weird");
	return FALSE;
    }
    clear_prompt(side, map);
    /* Reset all the buttons in the unit type list. */
    for_all_unit_types(u) {
	enable_in_unit_type_list(side, map, u, 0);
    }
    /* Make the unit type string be empty. */
    map->ustr[0] = '\0';
    return TRUE;
}

static void
help_unit_type(side, map)
Side *side;
Map *map;
{
    int i;
    char helpbuf[BUFSIZE];

    for (i = 0; map->ustr[i] != '\0'; ++i) {
	if (i % 4 == 0) {
	    if (i > 0) {
		tprintf(helpbuf, "? for this help info"); 
		notify(side, "%s", helpbuf);
	    }
	    helpbuf[0] = '\0';
	}
	tprintf(helpbuf, "%c %s", map->ustr[i], u_type_name(map->uvec[i]));
	if (i > 0)
	  tprintf(helpbuf, ", ");
    }
}

void
enable_in_unit_type_list(side, map, u, flag)
Side *side;
Map *map;
int u, flag;
{
    switch (flag) {
      case -1:
	XtVaSetValues(map->list_buttons[u + 1],
		      XtNsensitive, False,
#if 0
		      XtNtranslations, saved_translations,
#endif
		      NULL);
	break;
      case 0:
	/* Tweak the button to how it should look when disabled. */
	XtVaSetValues(map->list_buttons[u + 1],
		      XtNsensitive, True,
		      NULL);
#if 0
	/* (need to force button state out somehow?) */
	XtUninstallTranslations(map->list_buttons[u + 1]);
#endif
	break;
      case 1:
	XtVaSetValues(map->list_buttons[u + 1],
		      XtNhighlightThickness, 2,
		      XtNsensitive, True,
#if 0
		      XtNtranslations, saved_translations,
#endif
		      NULL);
	break;
    }
}

int
ask_terrain_type(side, map, prompt, possibles, handler)
Side *side;
Map *map;
char *prompt;
int *possibles;
void (*handler) PARAMS ((Side *side, Map *map, int cancelled));
{
    int numtypes = 0, t;

    for_all_terrain_types(t) {
	if (possibles == NULL || possibles[t]) {
	    map->tvec[numtypes] = t;
	    map->tstr[numtypes] =
	      (!empty_string(t_char(t)) ? t_char(t)[0] : (t - 'a'));
	    ++numtypes;
	}
    }
    map->tstr[numtypes] = '\0';
    if (numtypes > 1) {
	sprintf(map->prompt, "%s [%s]", prompt, map->tstr);
	map->answer[0] = '\0';
	draw_prompt(side, map);
	map->modalhandler = handler;
    }
    return numtypes;
}

/* Do something with the char or terrain type that the player entered. */

int
grok_terrain_type(side, map, typep)
Side *side;
Map *map;
int *typep;
{
    int i;

    *typep = NONTTYPE;
    if (map->inpch == '?') {
	help_terrain_type(side, map);
	return FALSE;
    }
    i = iindex(map->inpch, map->tstr);
    if (i >= 0) {
	*typep = map->tvec[i];
	clear_prompt(side, map);
	return TRUE;
    } else {
	notify(side, "Must type a terrain type char or <esc>");
	return FALSE;
    }
}

static void
help_terrain_type(side, map)
Side *side;
Map *map;
{
    int i;
    char helpbuf[BUFSIZE];

    for (i = 0; map->tstr[i] != '\0'; ++i) {
	if (i % 4 == 0) {
	    if (i > 0) {
		tprintf(helpbuf, "? for this help info"); 
		notify(side, "%s", helpbuf);
	    }
	    helpbuf[0] = '\0';
	}
	tprintf(helpbuf, "%c %s", map->tstr[i], t_type_name(map->tvec[i]));
	if (i > 0)
	  tprintf(helpbuf, ", ");
    }
}

/* User is asked to pick a position on map.  This will iterate until the */
/* space bar designates the final position. */

/* (should change the cursor temporarily) */

void
ask_position(side, map, prompt, handler)
Side *side;
Map *map;
char *prompt;
void (*handler) PARAMS ((Side *side, Map *map, int cancelled));
{
    strcpy(map->prompt, prompt);
    map->answer[0] = '\0';
    draw_prompt(side, map);
    map->modalhandler = handler;
}

int
grok_position(side, map, xp, yp)
Side *side;
Map *map;
int *xp, *yp;
{
    
    if (in_area(map->curx, map->cury)) {
	*xp = map->curx;  *yp = map->cury;
	clear_prompt(side, map);
	return TRUE;
    } else {
	/* Make any possible usage attempts fail. */
	*xp = *yp = -1;
	return FALSE;
    }
}

/* Prompt for a yes/no answer with a settable default. */

void
ask_bool(side, map, question, dflt, handler)
Side *side;
Map *map;
char *question;
int dflt;
void (*handler) PARAMS ((Side *side, Map *map, int cancelled));
{
    sprintf(map->prompt, "%s [%s]", question, (dflt ? "yn" : "ny"));
    map->answer[0] = '\0';
    draw_prompt(side, map);
    map->tmpint = dflt;
    map->modalhandler = handler;
}

/* Figure out what the answer actually is, keeping the default in mind. */

int
grok_bool(side, map)
Side *side;
Map *map;
{
    int dflt = map->tmpint;
    char ch = map->inpch;

    if (dflt ? (lowercase(ch) == 'n') : (lowercase(ch) == 'y'))
      dflt = !dflt;
    clear_prompt(side, map);
    return dflt;
}

/* Read a string from the prompt window.  Deletion is allowed, and a
   text cursor (an underscore) is displayed. */

void
ask_string(side, map, prompt, dflt, handler)
Side *side;
Map *map;
char *prompt, *dflt;
void (*handler) PARAMS ((Side *side, Map *map, int cancelled));
{
    sprintf(map->prompt, "%s", prompt);
    /* Default must be non-NULL. */
    if (dflt == NULL)
      dflt = "";
    sprintf(map->answer, "%s", dflt);
    draw_prompt(side, map);
    map->modalhandler = handler;
}

/* Dig a character from the input and add it into the string.
   Keep returning FALSE until we get something, then make a copy
   of the result string and return TRUE. */

int
grok_string(side, map, strp)
Side *side;
Map *map;
char **strp;
{
    char ch = map->inpch;
    int len = strlen(map->answer);

    if (ch == '\r' || ch == '\n') {
	*strp = copy_string(map->answer);
	clear_prompt(side, map);
	return TRUE;
    } else {
	if (ch == BACKSPACE_CHAR || ch == DELETE_CHAR) {
	    if (len > 0)
	      --len;
	} else {
	    map->answer[len++] = ch;
	}
	map->answer[len] = '\0';
	draw_prompt(side, map);
	return FALSE;
    }
}

void
ask_side(side, map, prompt, dflt, handler)
Side *side;
Map *map;
char *prompt;
Side *dflt;
void (*handler) PARAMS ((Side *side, Map *map, int cancel));
{
    sprintf(map->prompt, "%s", prompt);
    sprintf(map->answer, "%s", side_desig(dflt));
    draw_prompt(side, map);
    map->modalhandler = handler;
}

int
grok_side(side, map, side2p)
Side *side;
Map *map;
Side **side2p;
{
    *side2p = NULL;
    return TRUE;
}

/* Given a pixel in a map, describe what's there. */

static void
move_look(side, map, sx, sy)
Side *side;
Map *map;
int sx, sy;
{
    int nx, ny;
    Unit *unit;

    if (x_nearest_cell(side, map, sx, sy, &nx, &ny)) {
	if (inside_area(nx, ny)) {
	    x_nearest_unit(side, map, sx, sy, &unit);
	    if (unit != NULL
		&& (side_controls_unit(side, unit) || map->seeall)) {
		set_current_unit(side, map, unit);
	    } else {
		set_current_xy(side, map, nx, ny);
	    }
	} else {
	    beep(side);
	}
    }
}

/* Set the "current unit" of a map - the one being displayed, moved, etc. */

void
set_current_unit(side, map, unit)
Side *side;
Map *map;
Unit *unit;
{
    if (unit == map->curunit)
      return;
    clear_current(side, map);
    if (unit == NULL || (in_play(unit) && side_controls_unit(side, unit))) {
	map->curunit = unit;
    }
    /* Always shift the current position to where the unit is,
       whether or not it's one of ours. */
    if (unit != NULL) {
	map->curx = unit->x;  map->cury = unit->y;
    }
    draw_current(side, map);
    draw_map_info(side, map);
    put_on_screen(side, map, map->curx, map->cury);
    update_unit_controls(side, map);
}

/* Set the "current position" on the map. */

void
set_current_xy(side, map, x, y)
Side *side;
Map *map;
int x, y;
{
    if (x == map->curx && y == map->cury)
      return;
    clear_current(side, map);
    if (in_area(x, y)) {
	map->curx = x;  map->cury = y;
    }
    draw_current(side, map);
    draw_map_info(side, map);
    put_on_screen(side, map, map->curx, map->cury);
    update_unit_controls(side, map);
}

void
clear_current(side, map)
Side *side;
Map *map;
{
    int lastx = -1, lasty = -1;
    Unit *lastunit = NULL;

    if (map->curunit) {
	lastunit = map->curunit;
    } else if (in_area(map->curx, map->cury)) {
	lastx = map->curx;  lasty = map->cury;
    }
    map->curunit = NULL;
    map->curx = map->cury = -1;
    erase_current(side, map, lastx, lasty, lastunit);
}

/* Save the "cur" slots so we can move around without losing their
   values. */

void
save_cur(side, map)
Side *side;
Map *map;
{
    map->savedcurx = map->curx;  map->savedcury = map->cury;
    map->savedcurunit = map->curunit;
}

/* Restore the saved "cur" slots. */

void
restore_cur(side, map)
Side *side;
Map *map;
{
    map->curx = map->savedcurx;  map->cury = map->savedcury;
    map->curunit = map->savedcurunit;
    /* Erase the saved values. */
    map->savedcurx = map->savedcury = -1;
    map->savedcurunit = NULL;
}

/* If a given tool (mode) has a special cursor, use it. */

void
set_tool_cursor(side, map)
Side *side;
Map *map;
{
    int tool = map->curtool;

    if (side->ui->toolcursors[tool] == None) {
	fprintf(stderr, "No cursor!\n");
	abort();
    }
    XDefineCursor(side->ui->dpy, XtWindow(map->porthole),
		  side->ui->toolcursors[tool]);
}

void
zoom_in_out(side, map, which)
Side *side;
Map *map;
int which;
{
    int newpower = map->vp->power + (which == ZOOM_OUT ? -1 : 1);

    if (newpower < 0)
      newpower = 0;
    if (newpower > NUMPOWERS - 1)
      newpower = NUMPOWERS - 1;
    if (newpower != map->vp->power) {
	set_map_power(side, map, newpower);
	x_center_on_focus(side, map);
	draw_map(side, map);
	update_controls(side, map);
    }
}

static void
mode_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int which = (int) client_data;
    Side *side;
    Map *map;

    if (!find_side_and_map_via_control(w, &side, &map))
      return;

    map->curtool = which;
    update_controls(side, map);
    set_tool_cursor(side, map);
}

static void
command_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int which = (int) client_data;
    Side *side;
    Map *map;

    if (!find_side_and_map_via_control(w, &side, &map))
      return;

    if (map->curunit == NULL) {
	beep(side);
	return;
    }

    map->frombutton = TRUE;
    switch (which) {
      case UNIT_MOVE:
	do_move_to(side, map);
	break;
      case UNIT_SHOOT:
	do_fire(side, map);
	break;
      case UNIT_BUILD:
	do_build(side, map);
	break;
      default:
	case_panic("not a valid command button", which);
	break;
    }
    /* Reset any prefix argument. */
    map->prefixarg = -1;
}

static void
view_flag_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int which = (int) client_data;
    int redraw = TRUE;
    Side *side;
    Map *map;

    if (!find_side_and_map_via_control(w, &side, &map))
      return;

    switch (which) {
      case SHOW_TERRAIN:
	map->drawterrain = !map->drawterrain;
	break;
      case SHOW_GRID:
	map->drawgrid = !map->drawgrid;
	break;
      case SHOW_UNITS:
	map->drawunits = !map->drawunits;
	break;
      case SHOW_NAMES:
	map->drawnames = !map->drawnames;
	break;
      case SHOW_PEOPLE:
	map->drawpeople = !map->drawpeople;
	break;
      case SHOW_ALL:
	if (!all_see_all)
	  map->seeall = !map->seeall;
	else
	  beep(side);
	break;
      case SHOW_ELEV:
	map->drawelevations = !map->drawelevations;
	break;
      case SHOW_MORE:
	if (!map->fullpanel)
	  popup_ctrlpanel(side, map);
	else
	  popdown_ctrlpanel(side, map);
	redraw = FALSE;
	break;
      default:
	break;
    }
    update_controls(side, map);
    /* Redraw the map to reflect the effect of the toggle. */
    if (redraw)
      draw_map(side, map);
}

static void
zoom_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int which = (int) client_data;
    Side *side;
    Map *map;

    if (!find_side_and_map_via_control(w, &side, &map))
      return;

    zoom_in_out(side, map, which);
}

static void
map_help_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;
    Map *map;

    if (!find_side_and_map_via_control(w, &side, &map))
      return;

    do_help(side, map);
    map->prefixarg = -1;
}

static void
unit_type_list_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int which = (int) client_data;
    Side *side;
    Map *map;
    void (*fn) PARAMS ((Side *sidex, Map *mapx, int cancelledx));
    
    if (!find_side_and_map_via_listform(w, &side, &map))
      return;

    /* Always ignore clicks on the List label. */
    if (which == 0)
      return;
    /* Call the modal handler if defined. */
    if (map->modalhandler) {
	/* Suppress any possible apparent keyboard input, and
	   supply a type directly instead. */
    	map->inpch = '\0';
	map->inptype = which - 1;
	fn = map->modalhandler;
	/* Remove the handler - will restore itself if needed. */
	map->modalhandler = NULL;
	(*fn)(side, map, FALSE);
#ifdef DESIGNERS
    } else if (side->designer && side->ui->design_shell) {
	side->ui->curutype = which - 1;
	update_curutype(side);
	notify(side, "will now be creating %s %s units",
	       side_adjective(side_n(side->ui->curusidenumber)),
	       u_type_name(side->ui->curutype));
#endif /* DESIGNERS */
    }
}

static void
panner_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;
    Map *map;
    XawPannerReport *prp = (XawPannerReport *) call_data;
    
    if (find_side_and_map_via_rightform(w, &side, &map)) {
	scroll_map_absolute(side, map, prp->slider_x + hexagon_adjust(map->vp), prp->slider_y);
    } else
      run_warning("Did not find side and map!\n");
}

static void
porthole_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;
    Map *map;
    XawPannerReport *prp = (XawPannerReport *) call_data;
    
    if (!find_side_and_map_via_mapform(w, &side, &map)) {
	run_warning("Did not find side and map!\n");
	return;
    }

    map->pxw = prp->slider_width;  map->pxh = prp->slider_height;
    set_view_size(map->vp, prp->slider_width, prp->slider_height);
    if (map->panner) {
	XtVaSetValues(map->panner,
		      XtNsliderWidth, map->pxw,
		      XtNsliderHeight, map->pxh,
		      NULL);
    }
}

/* Put the map at an absolute position. */

void
scroll_map_absolute(side, map, sx, sy)
Side *side;
Map *map;
int sx, sy;
{
    int oldsx = map->vp->sx, oldsy = map->vp->sy;

    set_view_position(map->vp, sx, sy);

    /* Redraw if the scroll position actually changed. */
    if (oldsx != map->vp->sx || oldsy != map->vp->sy)
      draw_map(side, map);
}

#if 0
void
scroll_map_relative(side, map, sx, sy)
Side *side;
Map *map;
int sx, sy;
{
    int oldsx = map->vp->sx, oldsy = map->vp->sy;

    /* (genericize into a ui.c routine?) */
    /* if we're doing horiz scroll */
    if (sx != 0) {
	int lx = hexagon_adjust(map->vp);
	int hx = map->vp->totsw - lx;

	/* if the map is bigger than the view */
	if (map->vp->pxw < hx) {
	    map->vp->sx += (sx * (map->vp->pxw - (2 * map->vp->hw)));
	    if (map->vp->sx < lx) {
		map->vp->sx = (area.xwrap ? hx - (lx - map->vp->sx) : lx);
	    } else if (map->vp->sx > hx) {
		map->vp->sx = (area.xwrap ? lx + (map->vp->sx - hx) : hx);
	    }
	}
    }
    /* if we're doing vert scroll and there's even reason to */
    if (sy != 0 && map->vp->pxh < map->vp->totsh) {
	    map->vp->sy += (sy * (map->vp->pxh - (2 * map->vp->hh)));
	    if (map->vp->sy < 0) {
		map->vp->sy = 0;
	    } else if (map->vp->sy > map->vp->totsh - map->vp->pxh) {
		map->vp->sy = map->vp->totsh - map->vp->pxh;
	    }
    }
    /* Redraw if the scroll position actually changed. */
    if (oldsx != map->vp->sx || oldsy != map->vp->sy)
      draw_map(side, map);
}
#endif

static void
handle_map_click(side, map, sx, sy)
Side *side;
Map *map;
int sx, sy;
{
    int ax, ay;
    Unit *unit2;

    if (map == NULL) {
	beep(side);
	return;
    }
    if (!x_nearest_cell(side, map, sx, sy, &ax, &ay)) {
	beep(side);
	return;
    }
    /* Assume that last place clicked is a reasonable focus. */
    if (inside_area(ax, ay)) {
	set_view_focus(map->vp, ax, ay);
    }
#ifdef DESIGNERS
    if (side->designer && side->ui->curdesigntool != looktool) {
	handle_designer_map_click(side, map, sx, sy);
	return;
    }
#endif /* DESIGNERS */
    if (map->modalhandler) {
       void (*fn) PARAMS ((Side *sidex, Map *mapx, int cancelledx));

       fn = map->modalhandler;
       map->modalhandler = NULL;
       move_look(side, map, sx, sy);
       (*fn)(side, map, 0);
       return;
    }

    switch (map->curtool) {
      case looktool:
	move_look(side, map, sx, sy);
	break;
      case movetool:
      case unitmovetool:
	if (map->curunit && side_controls_unit(side, map->curunit)) {
	    move_the_selected_unit(side, map, map->curunit, sx, sy);
	} else {
	    move_look(side, map, sx, sy);
	}
	break;
      case unitshoottool:
	if (map->curunit && side_controls_unit(side, map->curunit)) {
	    if (inside_area(ax, ay)) {
		if ((unit2 = unit_at(ax, ay)) != NULL) {
		    if (map->curunit != unit2) {
			prep_fire_at_action(map->curunit, map->curunit,
					    unit2, -1);
		    } else {
			/* don't attack ourselves */
		    }
		} else {
		    /* We're just shooting for the hell of it. */
		    prep_fire_into_action(map->curunit, map->curunit,
					  ax, ay, 0, 0);
		}
	    } else {
		beep(side);
	    }
	} else {
	}
	break;
      case unitbuildtool:
	/* (what should this do?) */
	beep(side);
	break;
     default:
	/* error eventually */
	break;
    }
}

/* (should use advance_into_cell here) */

void
move_the_selected_unit(side, map, unit, sx, sy)
Side *side;
Map *map;
Unit *unit;
int sx, sy;
{
    int x, y;
    Unit *other = NULL;

    x_nearest_cell(side, map, sx, sy, &x, &y);
    x_nearest_unit(side, map, sx, sy, &other);
#ifdef DESIGNERS
    /* Designers use this function to push units around, bound only by the
       limits on occupancy. */
    if (side->designer) {
	designer_teleport(unit, x, y, other);
	return;
    }
#endif
    if (x != unit->x || y != unit->y) {
	/* we're outa here... leaving the cell that is */

	if (unit->act && unit->plan) { /* (should be more sophisticated?) */

	    /* if it's far away, set up the task and boogie */
	    if (distance(unit->x, unit->y, x, y) > 1) {
		DGprintf("Ordering %s to move to %d,%d\n",
			 unit_desig(unit), x, y);
		set_move_to_task(unit, x, y);
		return;
	    }

	    /* If no one's home, try to move into it directly. */
	    if (unit_at(x, y) == NULL) {
		if (can_occupy_cell(unit, x, y)
		    && valid(check_move_action(unit, unit, x, y, 0))) {
		    prep_move_action(unit, unit, x, y, 0);
		} else {
		    beep(side);
		}
		return;
	    }

	    /* There are units at our desired destination. */
	    if (other == NULL) {
		if (can_occupy_cell(unit, x, y)) {
		    prep_move_action(unit, unit, x, y, 0);
		} else {
		    beep(side);
		}
	    } else if (other->side == unit->side) {
		/* One of ours, maybe get on it. */
		if (can_occupy(unit, other)) {
		    prep_enter_action(unit, unit, other);
		} else if (can_occupy(other, unit)) {
		    /* Have other unit do an enter action, then move. */
		    /* (not quite right, move should happen after other unit
		       is actually inside, in case it fills dest) */
		    prep_enter_action(other, other, unit);
		    set_move_to_task(unit, x, y);
		} else if (can_occupy_cell(unit, x, y)) {
		    prep_move_action(unit, unit, x, y, 0);
		} else {
		    beep(side);
		}
	    } else {
		/* Somebody else's unit, try to victimize it. */
		if (valid(check_capture_action(unit, unit, other))) {
		    prep_capture_action(unit, unit, other);
		} else if (valid(check_attack_action(unit, unit, other, 100))) {
		    prep_attack_action(unit, unit, other, 100);
		} else {
		    beep(side);
		}
	    }
	}
    } else {
	/* moving around within the current cell */
	if (other != NULL && other != unit) {
	    /* ok we're trying to hop onto another transport */
	    if (can_occupy(unit, other)) {
		prep_enter_action(unit, unit, other);
	    } else {
		/* maybe we should restack? */
		beep(side);
	    }
	}
    }
}

void
destroy_map(side, map)
Side *side;
Map *map;
{
    /* find map and remove it from the list of maps */
    /* also put some other map in front if this one was the front one */
}

static void
panner_resize_handler (w, client_data, event, cont)
Widget w; 
XtPointer client_data; 
XEvent *event; 
Boolean *cont;
{
    Side *side;
    Map *map;

    if (event->type == ConfigureNotify) {
	for_all_sides(side) {
	    if (active_display(side)) {
		if (XtDisplay(w) == side->ui->dpy) {
		    for_all_maps(side, map) {
			if (w == map->panner) {
			    goto found;
			}
		    }
		}
	    }
	}
	return;

      found:
	draw_view_in_panner(side, map);
    } else {
	return;
    }
}
 
/* draw the world view in the background of the panner */

void
draw_view_in_panner(side, map)
Side *side;
Map *map;
{
    int x, y, l, t, mx, my, depth, w, h, b;
    Display *dpy = side->ui->dpy;
    Pixel pixel, bg=0, unknown;
    XImage *img;
    char *dp, *data;
    GC gc;
    extern int numdisplays;

    if (map->panner == NULL)
      return;

    unknown = XBlackPixel(dpy, side->ui->screen);
    depth = DefaultDepth(dpy, side->ui->screen);
    if (depth % 8)
      return;

   /* we need w, h, b as int, otherwise some compiler will miscompile
   mx = ((2 * (x - b) * area.width) / (w - 2 * b) - my) / 2;
   while XtVaGetValues needs them as Dimension */
    {
	Dimension wd = 0, hd = 0, bd = 0;

	XtVaGetValues(map->panner,
		      XtNwidth, &wd,
		      XtNheight, &hd,
		      XtNinternalSpace, &bd,
		      XtNbackground, &bg,
		      NULL);
	w = wd;
	h = hd;
	b = bd;
    }

    /* b is the size of the internal border  */
    if (b <= 0 || h <= 2 * b || w <= 2 * b)
      return;

    /* Not using xmalloc because data will be freed within this function */
    data = (char *) malloc(w * h * depth / 8 * sizeof(char));

    dp = data;
    for (y = 0; y < h; y++) {
	if (y < b || y >= h - b) {
	    my = -1;
	} else {
	    my = ((h - 1 - b - y) * area.height) / (h - 2 * b);
	}
	for (x = 0; x < w; x++) {
	    if (x < b || x >= w - b) {
		mx = -1;
	    } else {
	        /* get the right rounding for the division by 2 */
		mx = ((2 * (x - b) * area.width) / (w - 2 * b) - my) / 2;
		if (area.xwrap) {
		    mx = wrapx(mx);
		} else {
		    mx += area.height / 4;
		}
	    }
	    if (mx >= 0 && my >= 0 && in_area(mx, my)) {
		t = terrain_seen_at(side, mx, my);
		if (t == NONTTYPE) {
		    pixel = unknown;
		} else {
		    pixel = side->ui->cellcolor[t];
		}
	    } else {
		pixel = bg;
	    }
	    for (l = depth - 8; l >= 0; l -= 8) {
		*dp = (pixel >> l) & 0xff;
		dp++;
	    }
	}
    }

    img = XCreateImage(dpy, DefaultVisual(dpy, side->ui->screen), depth,
		       ZPixmap, 0, data, w, h, 8, w * depth / 8);
    if (img == NULL) {
	free(data);
	return;
    }
    img->byte_order = MSBFirst;
    img->bitmap_bit_order = MSBFirst;

    /* clear the panner background before freeing the pixmap */    
#ifdef NO_PANNER_BGPIXMAP
    XSetWindowBackgroundPixmap(dpy, XtWindow(map->panner), None);
#else
    XtVaSetValues(map->panner, XtNbackgroundPixmap, None, NULL);
#endif

    if (map->panner_pix != None)
      XFreePixmap(dpy, map->panner_pix);
    map->panner_pix = XCreatePixmap(dpy, side->ui->rootwin, w, h, depth);
    if (!map->panner_pix || map->panner_pix == None) {
	XDestroyImage(img);
	/* XDestroyImage also frees data */
	return;
    }

    gc = XCreateGC(dpy, map->panner_pix, 0, NULL);
    XPutImage(dpy, map->panner_pix, gc, img, 0, 0, 0, 0, w, h);
    XFreeGC(dpy, gc);
    XDestroyImage(img);
    /* XDestroyImage also frees data */

#ifdef NO_PANNER_BGPIXMAP
    XSetWindowBackgroundPixmap(dpy, XtWindow(map->panner), map->panner_pix);
    /* at this point we force a redraw of the panner */
    XClearWindow(dpy, XtWindow(map->panner));
#else
    XtVaSetValues(map->panner, XtNbackgroundPixmap, map->panner_pix, NULL);
#endif
}

/* Popup control panel for maps, written by Massimo Campostrini. */

static void
popup_ctrlpanel(side, map)
Side *side;
Map *map;
{
    if (!map->ctrlpanel_shell)
      create_ctrlpanel(side, map);
    map->fullpanel = TRUE;
    XtPopup(map->ctrlpanel_shell, XtGrabNone);
}

int
find_side_and_map_via_ctrlpanel_form(w, sidep, mapp)
Widget w;
Side **sidep;
Map **mapp;
{
    Side *side;
    Map *map;

    for_all_sides(side) {
	if (active_display(side)) {
	    if (XtDisplay(w) == side->ui->dpy) {
		for_all_maps(side, map) {
		    if (map->ctrlpanel_form == w) {
			*sidep = side;
			*mapp = map;
			return TRUE;
		    }
		}
	    }
	}
    }
    return FALSE;
}

static void
create_ctrlpanel(side, map)
Side *side;
Map *map;
{
    Widget *buttons, label;

    map->ctrlpanel_shell =
      XtVaCreatePopupShell("controlPanel", topLevelShellWidgetClass, side->ui->shell,
			   NULL);

    map->ctrlpanel_form =
      XtVaCreateManagedWidget("form", boxWidgetClass, map->ctrlpanel_shell,
			      NULL);

    buttons = (Widget *) xmalloc(numcontrols * sizeof (Widget));
    map->ctrlpanel_buttons = buttons;

    label =
      XtVaCreateManagedWidget("---what---", labelWidgetClass, map->ctrlpanel_form,
			      XtNborderWidth, 0,
			      NULL);

    map->ctrlpanel_buttons[SHOW_TERRAIN] =
      XtVaCreateManagedWidget("Terrain", toggleWidgetClass, map->ctrlpanel_form,
			      NULL);
    map->ctrlpanel_buttons[SHOW_GRID] =
      XtVaCreateManagedWidget("Grid", toggleWidgetClass, map->ctrlpanel_form,
			      NULL);
    map->ctrlpanel_buttons[SHOW_UNITS] =
      XtVaCreateManagedWidget("Units", toggleWidgetClass, map->ctrlpanel_form,
			      NULL);
    map->ctrlpanel_buttons[SHOW_NAMES] =
      XtVaCreateManagedWidget("Names", toggleWidgetClass, map->ctrlpanel_form,
			      NULL);
    map->ctrlpanel_buttons[SHOW_FEATURE_NAMES] =
      XtVaCreateManagedWidget("featureNames",  toggleWidgetClass, map->ctrlpanel_form,
			       XtNlabel, "Feature Names", NULL);
    map->ctrlpanel_buttons[SHOW_FEATURE_BOUNDARIES] =
      XtVaCreateManagedWidget("featureBoundaries",  toggleWidgetClass, map->ctrlpanel_form,
			       XtNlabel, "Feature Bound.", NULL);
    map->ctrlpanel_buttons[SHOW_PEOPLE] =
      XtVaCreateManagedWidget("People", toggleWidgetClass, map->ctrlpanel_form,
			      NULL);
    map->ctrlpanel_buttons[SHOW_ELEV] =
      XtVaCreateManagedWidget("Elevations", toggleWidgetClass, map->ctrlpanel_form,
			      NULL);
    map->ctrlpanel_buttons[SHOW_TEMP] =
      XtVaCreateManagedWidget("Temperatures", toggleWidgetClass, map->ctrlpanel_form,
			      NULL);
    map->ctrlpanel_buttons[SHOW_ALL] =
      XtVaCreateManagedWidget("See All", toggleWidgetClass, map->ctrlpanel_form,
			      XtNsensitive, !all_see_all,
			      NULL);

    label =
      XtVaCreateManagedWidget("--color---", labelWidgetClass, map->ctrlpanel_form,
			      XtNborderWidth, 0,
			      NULL);

    map->ctrlpanel_buttons[COLR_TERR] =
     XtVaCreateManagedWidget("Terrain", toggleWidgetClass, map->ctrlpanel_form,
			      XtNsensitive, !side->ui->monochrome,
			      NULL);
    map->ctrlpanel_buttons[COLR_UNITS] =
     XtVaCreateManagedWidget(" Units ", toggleWidgetClass, map->ctrlpanel_form,
			      XtNsensitive, !side->ui->monochrome,
			      NULL);
    map->ctrlpanel_buttons[COLR_EMBL] =
     XtVaCreateManagedWidget("Emblems", toggleWidgetClass, map->ctrlpanel_form,
			      XtNsensitive, !side->ui->monochrome,
			      NULL);
    label =
      XtVaCreateManagedWidget("---mono---", labelWidgetClass, map->ctrlpanel_form,
			      XtNborderWidth, 0,
			      NULL);
    map->ctrlpanel_buttons[MONO_REVERSE] =
      XtVaCreateManagedWidget("Reverse", toggleWidgetClass, map->ctrlpanel_form,
			      XtNsensitive, side->ui->monochrome,
			      NULL);

    label =
      XtVaCreateManagedWidget("----------", labelWidgetClass, map->ctrlpanel_form,
			      XtNborderWidth, 0,
			      NULL);
    label =
      XtVaCreateManagedWidget("Cancel", commandWidgetClass, map->ctrlpanel_form,
			      NULL);
    XtAddCallback(label, XtNcallback, ctrlpanel_cancel_callback, NULL);
    label =
      XtVaCreateManagedWidget("Revert", commandWidgetClass, map->ctrlpanel_form,
			      NULL);
    XtAddCallback(label, XtNcallback, ctrlpanel_revert_callback, NULL);
    label =
      XtVaCreateManagedWidget("Apply", commandWidgetClass, map->ctrlpanel_form,
			      NULL);
    XtAddCallback(label, XtNcallback, ctrlpanel_apply_callback, NULL);
    label =
      XtVaCreateManagedWidget("Done", commandWidgetClass, map->ctrlpanel_form,
			      NULL);
    XtAddCallback(label, XtNcallback, ctrlpanel_done_callback, NULL);
}

static void 
ctrlpanel_cancel_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;
    Map *map;

    if (!find_side_and_map_via_ctrlpanel_form(XtParent(w), &side, &map))
      return;

    popdown_ctrlpanel(side, map);
}

static void 
ctrlpanel_revert_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;
    Map *map;

    if (!find_side_and_map_via_ctrlpanel_form(XtParent(w), &side, &map))
      return;

    update_controls(side, map);
}

int get_toggle_state PARAMS ((Widget w));

int
get_toggle_state(w)
Widget w;
{
    Boolean rslt;

    XtVaGetValues(w, XtNstate, &rslt, NULL);
    return rslt; 
}

static int set_from_controls PARAMS ((Side *side, Map *map));

static int
set_from_controls(side, map)
Side *side;
Map *map;
{
    int newval, anychanged = FALSE;

    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_TERRAIN]);
    if (newval != map->drawterrain)
      anychanged = TRUE;
    map->drawterrain = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_GRID]);
    if (newval != map->drawgrid)
      anychanged = TRUE;
    map->drawgrid = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_UNITS]);
    if (newval != map->drawunits)
      anychanged = TRUE;
    map->drawunits = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_NAMES]);
    if (newval != map->drawnames)
      anychanged = TRUE;
    map->drawnames = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_FEATURE_NAMES]);
    if (newval != map->drawfeaturenames)
      anychanged = TRUE;
    map->drawfeaturenames = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_FEATURE_BOUNDARIES]);
    if (newval != map->drawfeatureboundaries)
      anychanged = TRUE;
    map->drawfeatureboundaries = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_PEOPLE]);
    if (newval != map->drawpeople)
      anychanged = TRUE;
    map->drawpeople = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_ELEV]);
    if (newval != map->drawelevations)
      anychanged = TRUE;
    map->drawelevations = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_TEMP]);
    if (newval != map->drawtemp)
      anychanged = TRUE;
    map->drawtemp = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[SHOW_ALL]);
    if (newval != map->seeall)
      anychanged = TRUE;
    map->seeall = newval;
    /* (shouldn't be affecting all maps without redrawing all...) */
    newval = get_toggle_state(map->ctrlpanel_buttons[COLR_TERR]);
    if (newval != side->ui->dflt_color_terr_images)
      anychanged = TRUE;
    side->ui->dflt_color_terr_images = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[COLR_UNITS]);
    if (newval != side->ui->dflt_color_unit_images)
      anychanged = TRUE;
    side->ui->dflt_color_unit_images = newval;
    newval = get_toggle_state(map->ctrlpanel_buttons[COLR_EMBL]);
    if (newval != side->ui->dflt_color_embl_images)
      anychanged = TRUE;
    side->ui->dflt_color_embl_images = newval;
    return anychanged;
}

static void 
ctrlpanel_apply_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;
    Map *map;
    int changed;

    if (!find_side_and_map_via_ctrlpanel_form(XtParent(w), &side, &map))
      return;

    changed = set_from_controls(side, map);
    if (changed)
      draw_map(side, map);
}

static void 
ctrlpanel_done_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;
    Map *map;
    int changed;

    if (!find_side_and_map_via_ctrlpanel_form(XtParent(w), &side, &map))
      return;

    changed = set_from_controls(side, map);
    popdown_ctrlpanel(side, map);
    if (changed)
      draw_map(side, map);
}

static void
popdown_ctrlpanel(side, map)
Side *side;
Map *map;
{
    map->fullpanel = FALSE;
    if (map->ctrlpanel_shell)
      XtPopdown(map->ctrlpanel_shell);
}

void set_control_state PARAMS ((Side *side, Map *map, int, int, int));

void
update_controls(side, map)
Side *side;
Map *map;
{
    set_control_state(side, map, LOOK, TRUE, (map->curtool == looktool));
    set_control_state(side, map, MOVE, TRUE, (map->curtool == movetool));
    update_unit_controls(side, map);
    set_control_state(side, map, SHOW_TERRAIN, TRUE, map->drawterrain);
    set_control_state(side, map, SHOW_GRID, TRUE, map->drawgrid);
    set_control_state(side, map, SHOW_UNITS, TRUE, map->drawunits);
    set_control_state(side, map, SHOW_NAMES, TRUE, map->drawnames);
    set_control_state(side, map, SHOW_FEATURE_NAMES, TRUE, map->drawfeaturenames);
    set_control_state(side, map, SHOW_FEATURE_BOUNDARIES, TRUE, map->drawfeatureboundaries);
    set_control_state(side, map, SHOW_PEOPLE, people_sides_defined(), map->drawpeople);
    set_control_state(side, map, SHOW_ELEV, elevations_defined(), map->drawelevations);
    set_control_state(side, map, SHOW_TEMP, temperatures_defined(), map->drawtemp);
    set_control_state(side, map, SHOW_ALL, side->may_set_see_all, map->seeall);
    set_control_state(side, map, SHOW_MORE, TRUE, map->fullpanel);
    set_control_state(side, map, ZOOM_OUT, (map->vp->power > 0), FALSE);
    set_control_state(side, map, ZOOM_IN,  (map->vp->power < NUMPOWERS - 1), FALSE);
    set_control_state(side, map, COLR_TERR, !side->ui->monochrome,
		      side->ui->dflt_color_terr_images);
    set_control_state(side, map, COLR_UNITS, !side->ui->monochrome,
		      side->ui->dflt_color_unit_images);
    set_control_state(side, map, COLR_EMBL, !side->ui->monochrome,
		      side->ui->dflt_color_embl_images);
}

void
update_unit_controls(side, map)
Side *side;
Map *map;
{
    set_control_state(side, map, UNIT_MOVE,
		      (map->curunit != NULL && can_move_at_all(map->curunit)), FALSE);
    set_control_state(side, map, UNIT_SHOOT,
		      (map->curunit != NULL && can_fire(map->curunit)), FALSE);
    set_control_state(side, map, UNIT_BUILD,
    		      (map->curunit != NULL && can_build(map->curunit)), FALSE);
}

void
set_control_state(side, map, contype, active, state)
Side *side;
Map *map;
int contype, active, state;
{
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNsensitive, (Boolean) active);  nargs++;
    /* Note that the XtNstate property can be set for command widgets,
       and will simply have no effect. */
    XtSetArg(tmpargs[nargs], XtNstate, (Boolean) state);  nargs++;
    if (map->controls[contype])
      XtSetValues(map->controls[contype], tmpargs, nargs);
    if (map->fullpanel && map->ctrlpanel_shell && map->ctrlpanel_buttons[contype])
      XtSetValues(map->ctrlpanel_buttons[contype], tmpargs, nargs);
}

void
set_message_area(map, msg)
Map *map;
char *msg;
{
    /* Can we? */
    if (!map->msgarea)
      return;
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNlabel, msg);	nargs++;
    XtSetValues(map->msgarea, tmpargs, nargs);
}

/* Prompt line drawing. */

void
draw_prompt(side, map)
Side *side;
Map *map;
{
    char tmpbuf[BUFSIZE];

    if (map == NULL)
      return;
    sprintf(tmpbuf, "%s %s", map->prompt, map->answer);
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNlabel, tmpbuf);  nargs++;
    XtSetValues(map->promptlabel, tmpargs, nargs);
    /* should change the cursor so that the user knows that an
       answer is expected. */
}

void
clear_prompt(side, map)
Side *side;
Map *map;
{
    if (map == NULL)
      return;
    map->prompt[0] = '\0';
    map->answer[0] = '\0';
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNlabel, " ");  nargs++;
    XtSetValues(map->promptlabel, tmpargs, nargs);
}


void
draw_game_state(side, map)
Side *side;
Map *map;
{
    char tmpbuf[BUFSIZE];
    extern char *curseasonname;
    extern int paused;

    if (map == NULL || map->gamedate == None)
      return;
    sprintf(tmpbuf, "%s", absolute_date_string(g_turn()));
    if (curseasonname != NULL)
      tprintf(tmpbuf, " (%s)", curseasonname);
    if (paused)
      tprintf(tmpbuf, " PAUSED");
    XtVaSetValues(map->gamedate, XtNlabel, tmpbuf, NULL);
    draw_game_clocks(side, map);
    /* update the Info window; Massimo: */ draw_map_info(side, map);
}

/* This displays the gamewide clock(s) if defined. */

void
draw_game_clocks(side, map)
Side *side;
Map *map;
{
    int elapsed;
    time_t now;
    char tmpbuf[BUFSIZE];

    if (map == NULL || map->gameclock == None)
      return;
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNlabel, " ");  nargs++;
    XtSetValues(map->gameclock, tmpargs, nargs);
    if (g_rt_for_game() > 0) {
	time(&now);
	elapsed = idifftime(now, game_start_in_real_time);
	time_desc(tmpbuf, max(0, g_rt_for_game() - elapsed), g_rt_for_game());
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNlabel, tmpbuf);  nargs++;
	XtSetValues(map->gameclock, tmpargs, nargs);
    }
    /* (should not overwrite prev, if both defined) */
    if (g_rt_per_turn() > 0) {
	time(&now);
	elapsed = idifftime(now, turn_play_start_in_real_time);
	time_desc(tmpbuf, max(0, g_rt_per_turn() - elapsed), g_rt_per_turn());
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNlabel, tmpbuf);  nargs++;
	XtSetValues(map->gameclock, tmpargs, nargs);
    }
}

/* Alter the numbers for a single type of unit.  Should be called right
   after any changes.  Formatted to look nice, but kind of messy to set
   up correctly; display should not jump back and forth as the numbers
   change in size. */

void
update_unit_type_list(side, map, u)
Side *side;
Map *map;
int u;
{
    int num, changed = FALSE;

    if (!between(0, u, numutypes))
      return;
    spbuf[0] = '\0';
    /* Our unit total (right-justified) */
    num = num_units_in_play(side, u);
    if (num != map->last_num_in_play[u]) {
	map->last_num_in_play[u] = num;
	changed = TRUE;
    }
    if (num > 0)	{
	sprintf(tmpbuf, "%4d", num);
    } else {
	sprintf(tmpbuf, "    ");
    }
    strcat(spbuf, tmpbuf);
    /* Our units under construction (left-justified) */
    num = num_units_incomplete(side, u);
    if (num != map->last_num_incomplete[u]) {
	map->last_num_incomplete[u] = num;
	changed = TRUE;
    }
    if (num > 0) {
	sprintf(tmpbuf, "(%d)", num);
    } else {
	sprintf(tmpbuf, "    ");
    }
    strcat(spbuf, tmpbuf);
    /* (should do other columns too) */
    if (changed)
      XtVaSetValues(map->list_buttons[u + 1], XtNlabel, spbuf, NULL);
}

void
place_legends(side)
Side *side;
{
    int nf = side->ui->numfeatures;

    if (!features_defined() || nf <= 0)
      return;
    if (side->ui->legends == NULL)
      side->ui->legends = (Legend *) xmalloc((nf + 1) * sizeof(Legend));
    place_feature_legends(side->ui->legends, nf, side, 0, 1);
}
