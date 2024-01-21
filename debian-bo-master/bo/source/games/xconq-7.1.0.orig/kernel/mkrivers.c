/* River generation for Xconq.
   Copyright 1991, 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This river generator requires elevations. */

#include "conq.h"

static void make_up_river_borders PARAMS ((int rivertype));
static int border_is_compatible PARAMS ((int x, int y, int dir, int b));
static int elev_in_dir PARAMS ((int x, int y, int dir));
static int elev_at_meet PARAMS ((int x, int y, int dir));
static void set_river_at PARAMS ((int x, int y, int dir, int t));
static void make_up_river_connections PARAMS ((int rivertype));

static int totalrivers;

/* Add rivers by picking a headwater randomly, then running downhill. */

/* Can do as either a border or connection type, depending on which
   terrain type is called "river". */

int
make_rivers(calls, runs)
int calls, runs;
{
    int x, y, t;

    totalrivers = 0;
    for_all_interior_cells(x, y) {
	totalrivers += t_river_chance(terrain_at(x, y));
    }
    if (totalrivers <= 0)
      return FALSE;
    totalrivers /= 10000;
    totalrivers = max(1, totalrivers);
    if (elevations_defined()) {
	for_all_terrain_types(t) {
	    if (t_subtype_x(t) == 10 /*keyword_value(K_RIVER_X)*/
		&& !aux_terrain_defined(t)) {
		if (t_is_border(t)) {
		    make_up_river_borders(t);
		} else if (t_is_connection(t)) {
		    make_up_river_connections(t);
		}
	    }
	}
    }
    return TRUE;
}

static int touchedwater;

/* Turn this on to record (as features) information about rivers
   are being constructed. */

static int DebugRiver = FALSE;

/* (should reindent) */
static void
make_up_river_borders(rivertype)
int rivertype;
{
    int j, x, y, dir, elev, x1, y1, d1, e1, x2, y2, d2, e2;
    int x0, y0, numrivers = 0, numrivs, numtouches;
    int low, lowdir;
    int t, watery[MAXTTYPES];
    Feature *rfeat;

    announce_lengthy_process("Creating rivers (as borders)");
    allocate_area_aux_terrain(rivertype);
    if (DebugRiver)
      init_features();
    for_all_interior_cells(x0, y0) {
	if (xrandom(10000) < t_river_chance(terrain_at(x0, y0))) {
	    ++numrivers;
	    touchedwater = FALSE;
	    if (numrivers % 5 == 0)
	      announce_progress((100 * numrivers) / totalrivers);
	    x = x0;  y = y0;
	    /* Set the initial river direction. */
	    lowdir = 0;
	    low = elev_at_meet(x, y, lowdir);
	    for_all_directions(dir) {
		if (elev_at_meet(x, y, dir) < low) {
		    lowdir = dir;
		    low = elev_at_meet(x, y, lowdir); 
		}
	    }
	    dir = lowdir;
	    /* If even the first bit of river is incompatible with the
	       surrounding terrain, blow this off and try the next location. */
	    if (!border_is_compatible(x, y, dir, rivertype))
	      continue;
	    /* Don't place this border if would join two other rivers;
	       detect by seeing river borders at both ends of this one. */
	    numtouches = 0;
	    if (point_in_dir(x, y, dir, &x1, &y1)) {
		if (border_at(x, y, left_dir(dir), rivertype)
		    || border_at(x1, y1, right_dir(opposite_dir(dir)), rivertype))
		  ++numtouches;
		if (border_at(x, y, right_dir(dir), rivertype)
		    || border_at(x1, y1, left_dir(opposite_dir(dir)), rivertype))
		  ++numtouches;
	    }
	    if (numtouches == 2)
	      continue;
	    set_river_at(x, y, dir, rivertype);
	    if (DebugRiver) {
		    char *featname;
		    Feature *feat;

		    sprintf(spbuf, "src%d%s", numrivers, dirnames[dir]);
		    featname = copy_string(spbuf);
		    feat = create_feature("source", featname);
		    feat->size = 1;
		    set_raw_feature_at(x, y, feat->id);

		    sprintf(spbuf, "riv#%d", numrivers);
		    featname = copy_string(spbuf);
		    rfeat = create_feature("river", featname);
		    rfeat->size = 0;
	    }
	    /* If our new bit of river already touches an existing river, just
	       stop now. */
	    if (numtouches == 1)
	      continue;
	    for (j = 0; j < area.maxdim; ++j) {
		if (!inside_area(x, y))
		  break;
		elev = elev_at_meet(x, y, dir);
		/* Compute cell and dir of the two possible ways to flow. */
		point_in_dir(x, y, right_dir(dir), &x1, &y1);
		d1 = left_dir(dir);
		e1 = elev_at_meet(x1, y1, d1);
		x2 = x;  y2 = y;
		d2 = right_dir(dir);
		e2 = elev_at_meet(x2, y2, d2);
		if (elev < e1 && elev < e2) {
		    init_warning("river going uphill??");
		    break;
		}
		/* Pick the lower of the two. */
		if (e1 < e2) {
		    x = x1;  y = y1;  dir = d1;
		} else {
		    x = x2;  y = y2;  dir = d2;
		}
		/* Don't add if might be between two edge cells. */
		if (!inside_area(x, y))
		  break;
		/* Rivers always follow the same paths, so if we see a river
		   here already, we're done. */
		if (border_at(x, y, dir, rivertype))
		  break;
		/* Make an actual piece of the river. */
		if (border_is_compatible(x, y, dir, rivertype)) {
		    set_river_at(x, y, dir, rivertype);
		    if (DebugRiver && rfeat != NULL && raw_feature_at(x, y) == 0) {
		    	set_raw_feature_at(x, y, rfeat->id);
		    	++(rfeat->size);
		    }
		}
	    }
	    rfeat = NULL;
	}
    }
    /* Cells surrounded by river should maybe be set specially. */
    if (g_river_sink_terrain() != NONTTYPE) {
	for_all_cells(x, y) {
	    if (inside_area(x, y)) {
		numrivs = 0;
		for_all_directions(dir) {
		    if (border_at(x, y, dir, rivertype)) ++numrivs;
		}
		if (numrivs >= NUMDIRS) {
		    set_terrain_at(x, y, g_river_sink_terrain());
		}
	    }
	}
    }
    /* Fix any bad adjacencies that got through. */
    for_all_terrain_types(t) {
    	watery[t] = t_liquid(t);
    }
    for_all_interior_cells(x, y) {
	    if (watery[(int) terrain_at(x, y)]) {
		for_all_directions(dir) {
		    set_border_at(x, y, dir, rivertype, FALSE);
		}
	    }
    }
    finish_lengthy_process();
}

static int
border_is_compatible(x, y, dir, b)
int x, y, dir, b;
{
    int x1, y1;

    return (tt_adj_terr_effect(terrain_at(x, y), b) < 0
            && point_in_dir(x, y, dir, &x1, &y1)
	    && tt_adj_terr_effect(terrain_at(x1, y1), b) < 0);
}

static int
elev_in_dir(x, y, dir)
int x, y, dir;
{
    int x1, y1;

    if (point_in_dir(x, y, dir, &x1, &y1)) {
	return elev_at(x1, y1);
    } else {
	/* It's possible that the algorithm will ask for the elevation
	   of a point not in the area, but it's an obscure case, and
	   doesn't hurt to return an arbitrary elevation. */
	return 0;
    }
}

/* The elevation at the junction of three cells is the lowest of the three. */

static int
elev_at_meet(x, y, dir)
int x, y, dir;
{
    int elev = elev_at(x, y);

    if (elev_in_dir(x, y, dir) < elev)
      elev = elev_in_dir(x, y, dir);
    if (elev_in_dir(x, y, right_dir(dir)) < elev)
      elev = elev_in_dir(x, y, right_dir(dir));
    return elev;
}

/* should have various conditions */

static void
set_river_at(x, y, dir, t)
int x, y, dir, t;
{
    int x1, y1;

    if (touchedwater) {
    } else if (t_liquid(terrain_at(x, y))) {
	touchedwater = TRUE;
    } else if (point_in_dir(x, y, dir, &x1, &y1)
	       && t_liquid(terrain_at(x1, y1))) {
	touchedwater = TRUE;
    } else {
	set_border_at(x, y, dir, t, TRUE);
    }
}

static void
make_up_river_connections(rivertype)
int rivertype;
{
    int x0, y0, i, x, y, x1, y1, dir;
    int numrivers = 0, lowdir, lowx, lowy, low;
    
    allocate_area_aux_terrain(rivertype);
    announce_lengthy_process("Creating rivers (as connections)");
    for_all_interior_cells(x0, y0) {
	if (probability(t_river_chance(terrain_at(x0, y0)) / 100)) {
	    ++numrivers;
	    if (numrivers % 5 == 0)
	      announce_progress((100 * numrivers) / totalrivers);
	    x = x0;  y = y0;
	    for (i = 0; i < 20; ++i) {
		/* Find the direction to the lowest adjacent cell. */
		lowdir = -1;
		low = elev_at(x, y);
		for_all_directions(dir) {
		    if (point_in_dir(x, y, dir, &x1, &y1)) {
			if (elev_at(x1, y1) <= low) {
			    lowx = x1;  lowy = y1;
			    lowdir = dir;
			    low = elev_at(x1, y1);
			}
		    }
		}
		if (lowdir < 0) break;
		if (!(t_liquid(terrain_at(x, y))
		      && t_liquid(terrain_at(lowx, lowy)))) {
		    set_connection_at(x, y, lowdir, rivertype, TRUE);
		}
		x = lowx;  y = lowy;
		if (!inside_area(x, y)) break;
	    }
	}
    }
    finish_lengthy_process();
}
