/* Map graphics for the curses interface to Xconq.
   Copyright (C) 1986, 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "cconq.h"

static int xform PARAMS ((int x, int y, int *sxp, int *syp));
static void draw_divider PARAMS ((void));
static void draw_terrain_row PARAMS ((int x, int y, int len));
static void draw_units PARAMS ((int x, int y));
static void draw_people PARAMS ((int x, int y));
static void draw_legend PARAMS ((int x, int y));
static void draw_unit_details PARAMS ((Unit *unit));
static void describe_cell PARAMS ((int x, int y, int tview, char *filler,
				  char *buf));
static void organize_list_contents PARAMS ((void));
static void add_unit_to_list PARAMS ((Unit *unit));
static void draw_unit_list_entry PARAMS ((int line));
static void draw_type_list_entry PARAMS ((int line));

char *dashbuffer;

/* When true, draw the terrain character in both character positions
   of a cell; otherwise draw in just the left position and put a blank
   in the right position. */

int use_both_chars = TRUE;

/* When true, use standout mode to highlite mode lines and such.
   Standout mode increases visual clutter somewhat, so it's not
   obviously good or obviously bad. */

int use_standout = FALSE;

int drawlinear = TRUE;

char linear_char = '#';

char bord_char = '\0';

char conn_char = '\0';

int listtype = 0;

int firstvisible = 0;
int lastvisible = 30;
UnitVector *listvector = NULL;
int listnumunits = 0;

int first_visible_help_pos;
int last_visible_help_pos;

/* Completely redo a screen, making no assumptions about appearance.
   This is a last-gasp measure; most redrawing should be restricted to
   only the directly affected windows.  Also this shouldn't be done
   without the user's permission, since it will blow away impending
   input. */

void
redraw()
{
    if (active_display(dside)) {
	switch (mode) {
	  case SURVEY:
	  case MOVE:
	  case PROMPT:
	  case PROMPTXY:
	    clear();
	    draw_divider();
	    show_toplines();
	    show_game_date();
	    show_clock();
	    show_side_list();
	    show_list();
	    show_closeup();
	    show_map();
	    break;
	  case HELP:
	  case MORE:
	    show_help();
	    break;
	  default:
	    break;
	}
	show_cursor();
	refresh();
    }
}

static void
draw_divider()
{
    int i;

    for (i = 2; i < LINES; ++i) {
	mvaddstr(i, mw, "|");
    }
}

/* Decide whether given location is not too close to edge of screen.
   We do this because it's a pain to move units when half the adjacent
   cells aren't even visible.  This routine effectively places a lower
   limit of 5x5 for the map window. (I think) */

int
in_middle(x, y)
int x, y;
{
    int sx, sy;

    xform(x, y, &sx, &sy);
    return ((between (3, sx, mw - 3) || !between(2, x, area.width-3))
	    && (between (3, sy, mh - 3) || !between(2, y, area.height-3)));
}

static int
xform(x, y, sxp, syp)
int x, y, *sxp, *syp;
{
    xform_cell(mvp, x, y, sxp, syp);
    return TRUE;
}

void
set_scroll()
{
    int sx, sy, hexadj = hexagon_adjust(mvp);

    sx = mvp->sx;  sy = mvp->sy; 
    if (mw < (mvp->totsw - hexadj))
      sx = max(sx, hexadj);
    else
      sx = hexadj;
    if (mh >= mvp->totsh)
      sy = 0;
    set_view_position(mvp, sx, sy);
}

void
set_map_viewport()
{
    /* Compute the size of the viewport. */
    vw = min(area.width, mw / 2 + 1);
    vh = min(area.height, mh);
    /* Compute the bottom visible row. */
    vy = (mvp->totsh - mvp->sy) - vh;
    /* Adjust to keep its value from being outside the area. */
    vy = max(0, min(vy, area.height - vh));
    /* Compute the first visible column. */
    vx = mvp->sx / 2 - vy / 2 - 1;
    DGprintf("Set %dx%d viewport at %d,%d\n", vw, vh, vx, vy);
}

/* Display a map and all of its paraphernalia. */

void
show_map()
{
    int y1, y2, y, x1, x2;
    int halfheight = area.height / 2;

    clear_window(mapwin);
    set_map_viewport();
    /* Compute top and bottom rows to be displayed. */
    y1 = min(vy + vh, area.height - 1);
    y2 = vy;
    for (y = y1; y >= y2; --y) {
	/* Adjust the right and left bounds to fill the viewport as
	   much as possible, without going too far (the drawing code
	   will clip, but clipped drawing is still expensive). */
	x1 = vx - (y - vy) / 2;
	x2 = x1 + vw + 2;
	/* If the area doesn't wrap, then we might have to stop
	   drawing before we reach the edge of the viewport. */
	if (!area.xwrap) {
	    x1 = max(0, min(x1, area.width - 1));
	    x2 = max(0, min(x2, area.width));
	    if (x1 + y > area.width + halfheight)
	      continue;
	    if (x2 + y < halfheight)
	      continue;
	    if (x2 + y > area.width + halfheight)
	      x2 = area.width + halfheight - y;
	    if (x1 + y < halfheight)
	      x1 = halfheight - y;
	}
	draw_row(x1, y, x2 - x1);
    }
    /* Draw modeline in standout if desired and possible. */
    if (use_standout)
      standout();
    tmpbuf[0] = '\0';
    if (drawunits) {
	strcat(tmpbuf, "units");
    }
    if (drawnames) {
	if (strlen(tmpbuf) > 0)
	  strcat(tmpbuf, ",");
	strcat(tmpbuf, "names");
    }
    if (drawpeople) {
	if (strlen(tmpbuf) > 0)
	  strcat(tmpbuf, ",");
	strcat(tmpbuf, "people");
    }
    memcpy(spbuf, dashbuffer, mw);
    memcpy(spbuf+2, "Map", 3);
    memcpy(spbuf+7, (mode == SURVEY ? "Survey" : "-Move-"), 6);
    memcpy(spbuf+17, "(", 1);
    memcpy(spbuf+18, tmpbuf, strlen(tmpbuf));
    memcpy(spbuf+18+strlen(tmpbuf), ")", 1);
    spbuf[mw] = '\0';
    /* (should add followaction flag to status line?) */
    draw_text(mapwin, 0, mh, spbuf);
    if (use_standout)
      standend();
}

void
draw_row(x0, y0, len)
int x0, y0, len;
{
    int x;

    if (drawterrain) {
	draw_terrain_row(x0, y0, len);
    }
    /* Draw sparse things on top of the basic row. */
    for (x = x0; x < x0 + len; ++x) {
	if (!inside_area(x, y0))
	  continue;
	/* The relative ordering of these is quite important.  Note that
	   each should be prepared to run independently also, since the
	   other displays might have been turned off. */
	if (people_sides_defined() && drawpeople) {
	    draw_people(x, y0);
	}
	if (drawunits) {
	    draw_units(x, y0);
	}
    }
    /* Need a second loop so names are superimposed properly. */
    for (x = x0; x < x0 + len; ++x) {
	if (drawnames) {
	    draw_legend(x, y0);
	}
    }
}

/* Draw a single row of just terrain. */

static void
draw_terrain_row(x0, y0, len)
int x0, y0, len;
{
    char ch, ch2;
    int x, sx, sy, t, t2;

    xform(x0, y0, &sx, &sy);
    for (x = x0; x < x0 + len; ++x) {
	ch = ((x % 2 == 0 && y0 % 2 == 0) ? unseen_char_2 : unseen_char_1);
	ch2 = ' ';
	if (terrain_visible(dside, x, y0)) {
	    t = terrain_at(x, y0);
	    ch = terrchars[t];
	    ch2 = (use_both_chars ? ch : ' ');
	    /* Just provide a hint at presence of aux terrain. */
	    if (drawlinear && any_aux_terrain_defined()) {
		for_all_terrain_types(t2) {
		    if (aux_terrain_defined(t2)) {
			if (aux_terrain_at(x, y0, t2)) {
			    ch2 = linear_char;
			    break;
			}
		    }
		}
	    }
	}
	if (cur_at(mapwin, sx, sy))
	  addch(ch);
	if (cur_at(mapwin, sx + 1, sy))
	  addch(ch2);
	sx += 2;
    }
}

/* Draw a single unit char pair as appropriate. */

static void
draw_units(x, y)
int x, y;
{
    int draw = FALSE;
    int xw, sx, sy, uview, u, s;
    Unit *unit;

    xw = wrapx(x);    
    if (units_visible(dside, xw, y)) {
	if (in_play(curunit) && curunit->x == xw && curunit->y == y) {
	    unit = curunit;
	} else {
	    unit = unit_at(xw, y);
	}
	if (unit != NULL) {
	    u = unit->type;
	    s = side_number(unit->side);
	    draw = TRUE;
	}
    } else {
	uview = unit_view(dside, xw, y);
	if (uview != EMPTY) {
	    u = vtype(uview);
	    s = vside(uview);
	    draw = TRUE;
	}
    }
    if (draw) {
	xform(x, y, &sx, &sy);
	if (cur_at(mapwin, sx, sy)) {
	    addch(unitchars[u]);
	}
	if (cur_at(mapwin, sx + 1, sy)) {
	    if (between(1, s, 9))
	      addch(s + '0');
	    else if (s >= 10)
	      /* This could get weird if s > 36, but not much
		 chance of that because MAXSIDES < 31 always. */
	      addch(s - 10 + 'A');
	}
    }
}

/* Indicate what kind of people are living in the given cell. */

static void
draw_people(x, y)
int x, y;
{
    int pop, sx, sy;

    if (terrain_visible(dside, x, y)
	&& (pop = people_side_at(wrapx(x), y)) != NOBODY) {
	xform(x, y, &sx, &sy);
	if (cur_at(mapwin, sx + 1, sy))
	  addch(pop + '0');
    }
}

/* Draw any text that should be associated with this cell. */

/* (could precompute what the string will lap over and move or truncate str),
   should be deterministic for each mag, so redraw doesn't scramble */

/* do geofeatures, label at a cell with nothing else, and declared as the
   feature's "center" */

static void
draw_legend(x, y)
int x, y;
{
    int sx, sy, pixlen;
    char legend[100], *str;
    int uview = unit_view(dside, x, y);
    Feature *feature;
    Unit *unit;

    if (uview == EMPTY)
      return;
    /* Draw a unit's name or number. */
    /* (This will only do top unit in hex, what about others?) */
    if (drawunits
	&& (unit = unit_at(x, y)) != NULL) {
	/* still has a bug if types happen to match - need to use view date
	   instead? */
	if (vtype(uview) != unit->type)
	  return;
	if (unit->name != NULL) {
	    strcpy(legend, unit->name);
	} else {
	    /* Turns out we have nothing to make a legend with. */
	    /* Note that, unlike some other interfaces, this one
	       does not ever display unit numbers, since it makes the
	       screen too cluttered to read. */
	    return;
	}
	xform(x, y, &sx, &sy);
	draw_text(mapwin, sx + 2, sy, legend);
    } else {
	feature = feature_at(x, y);
	if (feature != NULL) {
	    if (feature->size == 1 && (str = feature_name_at(x, y)) != NULL) {
		xform(x, y, &sx, &sy);
		pixlen = strlen(str) / 2;
		draw_text(mapwin, sx - pixlen, sy, str);
	    }
	}
    }
}

/* (should change text[12] to an array) */

void
low_notify(side, str)
Side *side;
char *str;
{
    char *lastblank, extrabuf[BUFSIZE];

    if (!active_display(side))
      return;
    if (strlen(str) > toplineswin->w) {
	strcpy(extrabuf, str);
	lastblank = strchr(extrabuf + toplineswin->w - 10, ' '); 
	if (lastblank) {
	    strcpy(text2, lastblank + 1);
	    *lastblank = '\0';
	}
	strcpy(text1, extrabuf);
    } else {
	strcpy(text1, str);
    }
    DGprintf("%s\n", text1);
    DGprintf("%s\n", text2);
    show_toplines();
    /* Put cursor back to where it was. */
    show_cursor();
    refresh();
}

void
show_toplines()
{
    clear_window(toplineswin);
    draw_text(toplineswin, 0, 0, text1);
    draw_text(toplineswin, 0, 1, text2);
}

void
clear_toplines()
{
    text1[0] = '\0';
    text2[0] = '\0';
    show_toplines();
}

/* Display all the details of the currently-selected unit/cell. */

/* (much of this should be made into kernel routines shared by interfaces) */

void
show_closeup()
{
    int u, tview, uview;
    char *filler = "Empty ";
    Unit *unit = NULL;
    Side *side2;

    clear_window(closeupwin);
    if (inside_area(curx, cury)) {
	if (terrain_visible(dside, curx, cury)) {
	    tview = buildtview(terrain_at(curx, cury));
	} else {
	    tview = terrain_view(dside, curx, cury);
	}
	if (units_visible(dside, curx, cury)) {
	    unit = (in_play(curunit) ? curunit : unit_at(curx, cury));
	    if (in_play(unit)) {
		/* If there is a unit there, we can at least see basic info. */
		sprintf(tmpbuf, "%s", unit_handle(dside, unit));
		draw_text(closeupwin, 0, 0, tmpbuf);
		if (unit->side == dside) {
		    draw_unit_details(unit);
		}
		filler = "In ";
	    }
	} else {
	    uview = unit_view(dside, curx, cury);
	    if (uview != EMPTY) {
		filler = "In ";
		u = vtype(uview);
		side2 = side_n(vside(uview));
		sprintf(tmpbuf, "%s %s",
		    side_adjective(side2), u_type_name(u));
		draw_text(closeupwin, 0, 0, tmpbuf);
	    }
	}
	/* Describe the cell here. */
	describe_cell(curx, cury, tview, filler, tmpbuf);
	draw_text(closeupwin, 0, 1, tmpbuf);
    } else {
	sprintf(tmpbuf, "??? Off-area %d,%d ???", curx, cury);
	draw_text(closeupwin, 0, 1, tmpbuf);
    }
}

static void
describe_cell(x, y, tview, filler, buf)
int x, y, tview;
char *filler, *buf;
{
    int t;
    char *featname;

    if (tview != UNSEEN) {
	t = vterrain(tview);
	/* Now describe terrain and position. */
	sprintf(buf, "%s%s", filler, t_type_name(t));
	featname = feature_name_at(x, y);
	if (featname != NULL) {
	    strcat(buf, " ");
	    strcat(buf, featname);
	}
	elevation_desc(buf+strlen(buf), x, y);
	/* (should put all weather on own line) */
	temperature_desc(buf+strlen(buf), x, y);
	/* add clouds and winds desc here */
	linear_desc(buf+strlen(buf), x, y);
    } else {
	sprintf(buf, "<unknown>");
    }
    tprintf(buf, " at %d,%d", x, y);
}

/* Describe the state of the given unit, in maximal detail. */

static void
draw_unit_details(unit)
Unit *unit;
{
    int u = unit->type, u2, m, nums[MAXUTYPES];
    int terr = terrain_at(unit->x, unit->y);
    Unit *occ, *top;

    /* Say which unit this is. */
    sprintf(spbuf, "%s", unit_handle(dside, unit));
    /* Describe the "important" parameters like hit points and moves. */
    strcat(spbuf, "  ");
    hp_desc(tmpbuf, unit, TRUE);
    strcat(spbuf, tmpbuf);
    /* (should say something about parts here) */
    strcat(spbuf, "  ");
    acp_desc(tmpbuf, unit, TRUE);
    strcat(spbuf, tmpbuf);
    draw_text(closeupwin, 0, 0, spbuf);
    /* Mention transport and other units stacked here. */
    if (unit->transport != NULL) {
	sprintf(spbuf, "In %s", short_unit_handle(unit->transport));
    } else {
	describe_cell(unit->x, unit->y, buildtview(terr), "In ", spbuf);
	/* make this a "stacked_at" macro? */
	top = unit_at(unit->x, unit->y);
	if (top != NULL && top->nexthere != NULL) {
	    strcat(spbuf, ", ");
	    for_all_unit_types(u2)
	      nums[u2] = 0;
	    for_all_stack(unit->x, unit->y, occ)
	      ++nums[occ->type];
	    --nums[u];  /* don't count ourselves */
	    for_all_unit_types(u2) {
		if (nums[u2] > 0) {
		    tprintf(spbuf, "%d %1s  ", nums[u2], utype_name_n(u2, 1));
		}
	    }
	    strcat(spbuf, " here also");
	}
    }
    draw_text(closeupwin, 0, 1, spbuf);
    /* Very briefly list the numbers and types of the occupants. */
    spbuf[0] = '\0';
    if (unit->occupant != NULL) {
	strcpy(spbuf, "Occ ");
	/* (should be occ_desc in nlang.c?) */
	for_all_unit_types(u2)
	  nums[u2] = 0;
	for_all_occupants(unit, occ)
	  ++nums[occ->type];
	for_all_unit_types(u2) {
	    if (nums[u2] > 0) {
		tprintf(spbuf, "%d %1s  ", nums[u2], utype_name_n(u2, 1));
	    }
	}
    }
    draw_text(closeupwin, 0, 2, spbuf);
    /* Describe the state of all the supplies. */
    /* (should be supply_desc in kernel?) */
    spbuf[0] = '\0';
    for_all_material_types(m) {
	if (um_storage_x(u, m) > 0) {
	    sprintf(tmpbuf, "%s %d/%d  ",
		    m_type_name(m), unit->supply[m], um_storage_x(u, m));
	    strcat(spbuf, tmpbuf);
	}
    }
    draw_text(closeupwin, 0, 3, spbuf);
    /* Describe the current plans, tasks, etc. */
    /* (needs much improvement) */
    if (unit->plan) {
	int row = 5;
	Task *task;

	plan_desc(spbuf, unit);
	draw_text(closeupwin, 0, 4, spbuf);
	for (task = unit->plan->tasks; task != NULL; task = task->next) {
	    task_desc(spbuf, task);
	    draw_text(closeupwin, 0, row++, spbuf);
	}
    }
}

/* Basic routine that displays the list of sides. */

void
show_side_list()
{
    char ismoving, progress[20], *dpyname;
    int sy = 0, totacp;
    Side *side2;
    extern int curpriority;

    /* Ensure subwin is clear. */
    clear_window(sideswin);
    for_all_sides(side2) {
    	ismoving = ' ';
	if ((g_use_side_priority()
	     ? (curpriority == side2->priority)
	     : (!side2->finishedturn)))
	  ismoving = '*';
	if (side2->designer) {
	    strcpy(progress, "DESIGN  ");
	} else if (side2->ingame) {
	    totacp = side_initacp(side2);
	    if (totacp > 0) {
		sprintf(progress, "%3d%%", (100 * side_acp(side2)) / totacp);
		/* We get to see our actual total acp as well. */
		if (dside == side2) {
		    tprintf(progress, "/%-3d", totacp);
		} else {
		    strcat(progress, "    ");
		}
	    } else {
		strcpy(progress, "   --   ");
	    }
	} else {
	    if (side_lost(side2)) {
		strcpy(progress, " Lost   ");
	    } else if (side_won(side2)) {
		strcpy(progress, " Won!   ");
	    } else {
		strcpy(progress, " Gone   ");
	    }
	}
	dpyname = "";
	if (side2->player->displayname)
	  dpyname = side2->player->displayname;
	sprintf(spbuf, "%d%c %s %s (%s)",
		side_number(side2), ismoving, progress,
		side_name(side2), dpyname);
	draw_text(sideswin, 0, sy, spbuf);
	sy += 1;
    }
    /* Draw the modeline. */
    if (use_standout)
      standout();
    memcpy(spbuf, dashbuffer, lw);
    memcpy(spbuf+1, "Sides", 5);
    spbuf[lw] = '\0';
    draw_text(sideswin, 0, sh - 1, spbuf);
    if (use_standout)
      standend();
}

/* Display the date. */

void
show_game_date()
{
    clear_window(datewin);
    /* First line of the game state. */
    /* (should cache this date string a la Mac version?) */
    draw_text(datewin, 1, 0, absolute_date_string(g_turn()));
    /* (should use second line for something) */
}

/* General list display routine. */

/* (should track beginning/end of displayed list, draw only visible elts) */
/* (should add scrolling interaction) */

static void
organize_list_contents()
{
    Side *side2;
    Unit *unit;

    /* Build up the array of units for this list. */
    listnumunits = 0;
    clear_unit_vector(listvector);
    /* We always see our own units. */
    for_all_side_units(dside, unit) {
	add_unit_to_list(unit);
    }
    for_all_sides(side2) {
	if (dside != side2) {
	    for_all_side_units(side2, unit) {
		if (side_sees_image(dside, unit)) {
		    add_unit_to_list(unit);
		}
	    }
	}
    }
    for_all_side_units(indepside, unit) {
	if (side_sees_image(dside, unit)) {
	    add_unit_to_list(unit);
	}
    }
    /* Now sort the list according to its keys. */
    sort_unit_vector(listvector);
}

static void
add_unit_to_list(unit)
Unit *unit;
{
    if (alive(unit)) {
	add_unit_to_vector(listvector, unit, FALSE);
	/* (should apply other inclusion criteria too?) */
	++listnumunits;
    }
}

void
show_list()
{
    int i = 0, line = 1;
    char *maincat = "xxxxx", *filter = "?yyyy?";

    clear_window(listwin);
    tmpbuf[0] = '\0';
    switch (listtype) {
      case 0:
	if (listvector == NULL) {
	    listvector = make_unit_vector(1000);
	    listnumunits = 0;
	    for (i = 0; i < MAXSORTKEYS; ++i) {
		tmpsortkeys[i] = bynothing;
	    }
	    tmpsortkeys[0] = byside;
	    organize_list_contents();
	}
	for (line = firstvisible; line <= lastvisible; ++line) {
	    draw_unit_list_entry(line);
	}
	maincat = "Units";
	switch (listsides) {
	  case ourside:
	    filter = "-Own--";
	    break;
	  case ourallies:
	    filter = "Allied";
	    break;
	  case allsides:
	    filter = "-ALL--";
	    break;
	  default:
	    filter = "??????";
	    break;
	}
	    for (i = 0; i < MAXSORTKEYS; ++i) {
		if (i == 0) {
		    strcat(tmpbuf, "by ");
		} else if (tmpsortkeys[i] != bynothing) {
		    strcat(tmpbuf, ",");
		}
		switch (tmpsortkeys[i]) {
		  case byside:
		    strcat(tmpbuf, "side");
		    break;
		  case bytype:
		    strcat(tmpbuf, "type");
		    break;
		  case byname:
		    strcat(tmpbuf, "name");
		    break;
		  case byactorder:
		    strcat(tmpbuf, "act");
		    break;
		  case bylocation:
		    strcat(tmpbuf, "loc");
		    break;
		  case bynothing:
		    break;
		  default:
		    strcat(tmpbuf, "???");
		    break;
		}
	    }
	break;
      case 1:
	for_all_unit_types(line) {
	    draw_type_list_entry(line);
	} 
	maincat = "Types";
	filter = "------";
    }
    /* Draw the modeline, in standout if possible. */
    if (use_standout)
      standout();
    memcpy(spbuf, dashbuffer, lw);
    memcpy(spbuf+1, maincat, 5);
    memcpy(spbuf+7, filter, 6);
    memcpy(spbuf+14, tmpbuf, strlen(tmpbuf));
    spbuf[lw] = '\0';
    draw_text(listwin, 0, lh - 1, spbuf);
    if (use_standout)
      standend();
    refresh();
}

void
cycle_list_type()
{
    listtype = (listtype + 1) % 2;
}

void
cycle_list_filter()
{
    listsides = (listsides + 1) % 3;
}

void
cycle_list_order()
{
    tmpsortkeys[0] = (tmpsortkeys[0] + 1) % numsortkeytypes;
    sort_unit_vector(listvector);
}

/* Alter the numbers for a single type of unit.  Should be called right
   after any changes.  Formatted to look nice, but kind of messy to set
   up correctly; display should not jump back and forth as the numbers
   change in size. */

int firsttypevisible = 0;
int lasttypevisible = MAXUTYPES;

static void
draw_type_list_entry(line)
int line;
{
    int u, num;

    u = line + firsttypevisible;
    if (!between(0, u, numutypes))
      return;
    if (u > lasttypevisible)
      return;
    sprintf(spbuf, " %c ", unitchars[u]);
    /* Our unit total (right-justified) */
    num = num_units_in_play(dside, u);
    if (num > 0)	{
	sprintf(tmpbuf, "%4d", num);
    } else {
	sprintf(tmpbuf, "    ");
    }
    strcat(spbuf, tmpbuf);
    /* Our units under construction (left-justified) */
    num = num_units_incomplete(dside, u);
    if (num > 0) {
	sprintf(tmpbuf, "(%d)", num);
    } else {
	sprintf(tmpbuf, "    ");
    }
    strcat(spbuf, tmpbuf);
    draw_text(listwin, 1, line, spbuf);
}

/* (should add these back in somewhere?) */
#if 0
	/* Our total possessions over the game */
	/* Make sure that subsequent write goes into valid string area. */
	strcat(spbuf,  "            ");
	if (total_gain(side, u) > 0) {
	    sprintf(tmpbuf, "%4d", total_gain(side, u));
	    sprintf(spbuf+10, "%s", tmpbuf);
	}
	/* Our total losses over the game */
	strcat(spbuf,  "            ");
	if (total_loss(side, u) > 0) {
	    sprintf(tmpbuf, "- %d", total_loss(side, u));
	    sprintf(spbuf+15, "%s", tmpbuf);
	}
#endif

static void
draw_unit_list_entry(line)
int line;
{
    Unit *unit = listvector->units[line].unit;
    char tmpbuf[BUFSIZE];

    if (unit == NULL)
      return;
    if (unit == curunit) {
	draw_text(listwin, 0, line, "*");
    }
    if (alive(unit)) {
	name_or_number(unit, tmpbuf);
	sprintf(spbuf, "%c%d %-16s ",
		unitchars[unit->type], side_number(unit->side), tmpbuf);
	if (unit->act && unit->act->acp > 0) {
	    tprintf(spbuf, "%3d", unit->act->acp);
	}
	if (unit->plan && unit->plan->asleep) {
	    strcat(spbuf, "z");
	}
	if (unit->plan && unit->plan->reserve) {
	    strcat(spbuf, "r");
	}
	if (unit->plan && unit->plan->waitingfortasks) {
	    strcat(spbuf, "w");
	}
	/* do hp also? */
    } else {
	sprintf(spbuf, "--");
    }
    draw_text(listwin, 1, line, spbuf);
}

void
show_clock()
{
#if 0
    int time = 0;

    if (realtime_game()) {
	time_desc(spbuf, time);
	draw_text(clockwin, 0, 0, spbuf);
    }
#endif
}

/* General window clearing. */

void
clear_window(win)
struct ccwin *win;
{
    int i;

    if (win->x == 0
	&& win->y == 0
	&& win->w == COLS
	&& win->h == LINES) {
	clear();
    } else if (between(0, win->x, COLS-1)
	       && between(0, win->w, COLS-1)
	       && between(0, win->y, LINES-1)
	       && between(0, win->h, LINES-1)) {
	for (i = 0; i < win->w; ++i)
	  tmpbuf[i] = ' ';
	tmpbuf[win->w] = '\0';
	for (i = 0; i < win->h; ++i)
	  mvaddstr(win->y + i, win->x, tmpbuf);
    } else {
	printf("error: win %d is %dx%d @ %d,%d\n",
	       (int) win, win->w, win->h, win->x, win->y);
    }
}

#if 0
/* Draw a large blot over the area. */

void
draw_mushroom(x, y, i)
int x, y, i;
{
    int sx, sy;

    xform(x, y, &sx, &sy);
    if (cur_at(mapwin, sx, sy)) {
	addstr("##");
	refresh();
	if (i > 0) {
	    if (cur_at(mapwin, sx-1, sy+1))
	      addstr("####");
	    if (cur_at(mapwin, sx-2, sy))
	      addstr("######");
	    if (cur_at(mapwin, sx-1, sy-1))
	      addstr("####");
	    refresh();
	}
    }
}
#endif

/* Drawing text is easy, but we do need to do clipping manually. */

int
draw_text(win, x, y, str)
struct ccwin *win;
int x, y;
char *str;
{
    int i, slen, linestart = 0;

    if (y < 0)
      y = win->h - y;
    if (cur_at(win, x, y)) {
	slen = strlen(str);
	for (i = 0; i < slen; ++i) {
	    if (str[i] == '\n') {
		if (y < win->h) {
		    if (cur_at(win, x, ++y))
		      linestart = i;
		    else
		      break;
		} else {
		    /* Ran out of room */
		    return i;
		}
	    } else if (x + (i - linestart) < win->w) {
		addch(str[i]);
	    }
	}
    }
    return (-1);
}

/* Make a beep by writing ^G. */

void
xbeep()
{
#ifdef THINK_C /* the libcurses library already includes a beep function */
    beep();
#else
    putchar('\007');
#endif
}

/* (should break helpstring into lines before displaying, so scrolling
   calcs simpler) */

void
show_help()
{
    int i, slen, x = 0, y = 0, more = FALSE;
    char *str;

    clear();
    cur_at(helpwin, 4, y);
    if (use_standout)
      standout();
    else
      addstr("*** ");
    addstr(cur_help_node->key);
    if (use_standout)
      standend();
    else
      addstr(" ***");
    ++y;
    cur_at(helpwin, 0, y);
    str = get_help_text(cur_help_node);
    slen = strlen(str);
    for (i = first_visible_help_pos; i < slen; ++i) {
	if (str[i] == '\n' || x > helpwin->w - 2) {
	    if (y < helpwin->h - 2) {
		++y;
		cur_at(helpwin, 0, y);
		x = 0;
		if (!(str[i] == '\n')) {
		    addch(str[i]);
		    ++x;
		}
	    } else {
		more = TRUE;
		last_visible_help_pos = i;
		break;
	    }
	} else {
	    addch(str[i]);
	    ++x;
	}
    }
    y = helpwin->h - 1;
    cur_at(helpwin, (more ? 9 : 0), y);
    addstr(" ['n' for next, 'p' for prev, 'q' to end]");
    if (more) {
	cur_at(helpwin, 0, y);
	if (use_standout)
	  standout();
	addstr("--More--");
	if (use_standout)
	  standend();
    }
    /* Position the cursor. */
    cur_at(helpwin, (more ? 8 : 0), y);
}

/* Put the terminal's cursor at an appropriate place. */

void
show_cursor()
{
    int sx, sy;

    if (active_display(dside)) {
	switch (mode) {
	  case SURVEY:
	  case MOVE:
	    if (curunit != NULL
		&& in_play(curunit)
		&& !(curunit->x == curx && curunit->y == cury)) {
		curx = curunit->x;  cury = curunit->y;
	    }
	    if (!in_middle(curx, cury)) {
		set_view_focus(mvp, curx, cury);
		center_on_focus(mvp);
		set_map_viewport();
		show_map();
	    }
	    xform(curx, cury, &sx, &sy);
	    if (!cur_at(mapwin, sx, sy))
	      abort();
	    break;
	  case HELP:
	    cur_at(helpwin, 0, 0);
	    break;
	  case MORE:
	    xbeep();
	    break;
	  case PROMPT:
	    /* This doesn't account for two-line prompts. */
	    if (!cur_at(toplineswin, strlen(text1), 0))
	      abort();
	    break;
	  case PROMPTXY:
	    if (!in_middle(curx, cury)) {
		set_view_focus(mvp, curx, cury);
		center_on_focus(mvp);
		set_map_viewport();
		show_map();
	    }
	    xform(curx, cury, &sx, &sy);
	    if (!cur_at(mapwin, sx, sy))
	      abort();
	    break;
	  default:
	    abort();
	}
	refresh();
    }
}

/* Position the cursor, being careful to test for sensibility. */

int
cur_at(win, x, y)
struct ccwin *win;
int x, y;
{
    int sx, sy;

    if (x < 0 || x >= win->w || y < 0 || y >= win->h) {
	/* Just return false if something is wrong; the caller should
	   test this and react apropriately. */
	return FALSE;
    } else {
	sx = win->x + x;  sy = win->y + y;
	if (between(0, sx, COLS-1) && between(0, sy, LINES-1)) {
	    move(sy, sx);
	} else {
	    /* Bad. Very bad. */
	    abort();
	}
	return TRUE;
    }
}
