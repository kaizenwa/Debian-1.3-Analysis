/* Unit generation for Xconq.
   Copyright (C) 1986, 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"

static void set_people_on_side PARAMS ((int x, int y));
static void expand_country PARAMS ((int x, int y));
static int country_is_complete PARAMS ((Side *side, int runs));
static int average_numunits PARAMS ((int u));
static void count_cells PARAMS ((int x, int y));
static int good_place PARAMS ((int cx, int cy));
static void find_a_place PARAMS ((Side *side));
static int valid_unit_place PARAMS ((int x, int y));
static int possible_unit_place PARAMS ((int x, int y));
static int find_unit_place PARAMS ((int u, int cx, int cy, int *xp, int *yp));
static void mung_terrain PARAMS ((int x, int y, int u));
static Unit *find_occupant_place PARAMS ((Side *side, int u, int x0, int y0));
static int at_country_units_max PARAMS ((Side *side, int u));

/* Tmp array for counting terrain. */

static int *numcells;

static int *favorite;

static int *totnumcells;

/* This is the number of country placements that could not be found to meet
   all the constraints. */

static int badcountryplaces = 0;

/* This says whether terrain alteration is permissible to make country
   placements work.  This only happens when normal placement starts to fail. */

static int mungterrain;

static int baseradius;

static int tmpradius;

static int curmindistance;
static int curmaxdistance;

static int sideprogress;
int sidedeltahalf;

static int growth;

/* Set the people inside the country to be on the country's side.
   Flip a coin along the edges, to make the border more interesting.
   Also don't always overwrite existing people. */

static void
set_people_on_side(x, y)
int x, y;
{
    if ((distance(tmpside->startx, tmpside->starty, x, y) <= tmpradius
         || flip_coin())
        && probability(t_country_people(terrain_at(x, y)))
        && (people_side_at(x, y) == NOBODY || flip_coin())) {
	set_people_side_at(x, y, side_number(tmpside));
    }
}

/* Expand a country into the given cell. */

static void
expand_country(x, y)
int x, y;
{
    int u, t = terrain_at(x, y), ours, theirs, prob;
    Unit *unit;
    
    if (people_side_at(x, y) == NOBODY) {
	prob = t_country_growth(t);
    } else {
	prob = t_country_takeover(t);
    }
    if (probability(prob)) {
	/* Claim the cell, attempt to own it. */
	ours = theirs = FALSE;
	if (unit_at(x, y) == NULL) {
	    /* If completely empty, we claim it. */
	    ours = TRUE;
	    for_all_unit_types(u) {
	    	tmputype = u;
		if (xrandom(10000) < u_unit_growth(u)
		    && !at_country_units_max(tmpside, u)
		    && probability(ut_favored(u, t))
		    && valid_unit_place(x, y)) {
		    unit = create_unit(u, TRUE);
		    if (unit != NULL) {
			set_unit_side(unit, tmpside);
			set_unit_origside(unit, tmpside);
			init_supply(unit);
			enter_cell(unit, x, y);
		    	break;
		    }
		}
		if (xrandom(10000) < u_indep_growth(u)
		    && !at_country_units_max(tmpside, u)
		    && probability(ut_favored(u, t))
		    && valid_unit_place(x, y)) {
		    unit = create_unit(u, TRUE);
		    if (unit != NULL) {
			init_supply(unit);
			enter_cell(unit, x, y);
		    	break;
		    }
		}
	    }
	} else {
	    /* Somebody is here already. */
	    for_all_stack(x, y, unit) {
		if (unit->side != tmpside) {
		    if (unit->side != NULL) {
			/* Maybe take over another side's unit. */
			if (xrandom(10000) < u_unit_takeover(unit->type)
			    && !at_country_units_max(tmpside, unit->type)
			    ) {
			    set_unit_side(unit, tmpside);
			    set_unit_origside(unit, tmpside);
			    ours = TRUE;
			} else {
			    /* Indicate that we don't have possession of every
			       unit in this cell. */
			    theirs = TRUE;
			}
		    } else {
			/* Maybe take over an independent unit. */
			if (xrandom(10000) < u_indep_takeover(unit->type)
			    && !at_country_units_max(tmpside, unit->type)
			    ) {
			    set_unit_side(unit, tmpside);
			    set_unit_origside(unit, tmpside);
			    ours = TRUE;
			}
		    }
		}
	    }
	}
	/* If cell is claimed, and has no opposition, then country gets it. */
	if (ours && !theirs && probability(t_country_people(t))) {
	    set_people_side_at(x, y, side_number(tmpside));
	}
	/* Count this cell as one we expanded into. */
	++growth;
    }
    /* The cell should become known even if not occupied, but we can't
       do that here because view structures don't exist yet. */
}

#define announce_unit_progress(n) \
  announce_progress(sideprogress + sidedeltahalf + (sidedeltahalf * (n)) / totnumtodo)

/* Place all the units belonging to countries. */

int
make_countries(calls, runs)
int calls, runs;
{
    int x0, y0, u, t, i, tot, x, y;
    int advantage, sideadvantage, favor, x1, y1, dir;
    int canbeinopen[MAXUTYPES];  /* true if type need not be occ at start */
    int numtodo[MAXUTYPES], numindeptodo[MAXUTYPES], totnumtodo, totnumdone;
    int numleft[MAXUTYPES], numindepleft[MAXUTYPES], totleft, numlisted, numfails;
    int checkmins = FALSE;
    int dopeoplesides = FALSE, dopeopleindeps = FALSE;
    int maxradius;
    int numdone = 0;
    Unit *unit, *transport;
    Side *side;
    char tmpbuf2[BUFSIZE];

    /* Run this always, unless something important is missing. */
    if (!terrain_defined())
      return FALSE;
    /* Calculate whether the minimum required terrain types are present. */
    totnumcells = (int *) xmalloc(numttypes * sizeof(int));
    for_all_terrain_types(t) {
    	if (t_country_min(t) > 0)
    	  checkmins = TRUE;
    }
    if (checkmins) {
    	/* Edge cells are useless for country placement. */
    	for_all_interior_cells(x, y) {
    	    ++totnumcells[terrain_at(x, y)];
    	}
    	for_all_terrain_types(t) {
    	    if (t_country_min(t) * numsides > totnumcells[t]) {
    	    	init_warning("Not enough %s for all %d sides",
			     t_type_name(t), numsides);
    	    	/* Don't error out, might be extenuating circumstances. */
    	    }
    	}
    }
    announce_lengthy_process("Making countries");
    /* Precompute some important info */
    numcells = (int *) xmalloc(numttypes * sizeof(int));
    favorite = (int *) xmalloc(numutypes * sizeof(int));
    tot = 0;
    for_all_unit_types(u) {
	canbeinopen[u] = FALSE;
	favorite[u] = NONTTYPE;
	favor = 0;
	for_all_terrain_types(t) {
	    if (ut_favored(u, t) > 0)
	      canbeinopen[u] = TRUE;
	    if (ut_favored(u, t) > favor) {
		favorite[u] = t;
		favor = ut_favored(u, t);
	    }
	}
	if (canbeinopen[u]) {
	    tot += u_start_with(u) + u_indep_near_start(u);
	}
    }
    /* Make space for people sides if we're going to have any. */
    if (!people_sides_defined()) {
	for_all_terrain_types(t) {
	    if (t_country_people(t) > 0)
	      dopeoplesides = TRUE;
	    if (t_indep_people(t) > 0)
	      dopeopleindeps = TRUE;
	}
	if (dopeoplesides || dopeopleindeps) {
	    allocate_area_people_sides();
	}
    }
    if (g_radius_min() <= 0) {
	/* If no radius specified, pick something plausible. */
	baseradius = max(1, isqrt(3 * tot) / 2);
    } else {
	baseradius = g_radius_min();
    }
    curmindistance = g_separation_min();
    curmaxdistance = g_separation_max();
    badcountryplaces = 0;
    for_all_sides(side) {
	sideprogress = (100 * numdone++) / numsides;
	sidedeltahalf = (100 / numsides) / 2;
	announce_progress(sideprogress);
	if (!country_is_complete(side, runs)) {
	    sideadvantage = max(1, side->advantage);
	    advantage = (side->player ? side->player->advantage : sideadvantage);
	    tmpradius = baseradius;
	    if (advantage > 1)
	      tmpradius = (baseradius * isqrt(advantage * 100) + 9) / 10;
	    /* Discover or generate the country's center. */
	    mungterrain = FALSE;
	    find_a_place(side);
	    x0 = side->startx;  y0 = side->starty;
	    Dprintf("%s starts around %d,%d\n", side_desig(side), x0, y0);
	    announce_progress(sideprogress + sidedeltahalf);
	    totnumtodo = totnumdone = 0;
	    /* Calculate how many units of each type we will get, both the ones
	       that belong to the side, and the independents nearby. */
	    for_all_unit_types(u) {
		numtodo[u] = numindeptodo[u] = 0;
		if (type_allowed_on_side(u, side)) {
		    if (runs > 0) {
			/* If the side is just coming into the game, give it an
			   average number of each type that the existing sides
			   started out with, so that the new side has some parity. */
			if (u_start_with(u) > 0 || u_indep_near_start(u) > 0)
			  numtodo[u] = (average_numunits(u) * advantage) / sideadvantage;
		    } else {
			numtodo[u] = (u_start_with(u) * advantage) / sideadvantage;
		    }
		    totnumtodo += numtodo[u];
		}
		if (type_allowed_on_side(u, NULL)) {
		    numindeptodo[u] = (u_indep_near_start(u) * advantage) / sideadvantage;
		    totnumtodo += numindeptodo[u];
		}
		numleft[u] = numindepleft[u] = 0;
	    }
	    /* First do units actually belonging to the side initially. */
	    for_all_unit_types(u) {
		if (canbeinopen[u]) {
		    for (i = 0; i < numtodo[u]; ++i) {
			if (find_unit_place(u, x0, y0, &x, &y)) {
			    unit = create_unit(u, TRUE);
			    if (unit != NULL) {
				set_unit_side(unit, side);
				set_unit_origside(unit, side);
				init_supply(unit);
				enter_cell(unit, x, y);
	   			announce_unit_progress(++totnumdone);
			    }
			} else {
			    /* If can't find places for this type, give up. */
		            numleft[u] = numtodo[u] - i;
			    break;
			}
		    }
		}
	    }
	    /* Now do independents in the initial country area. */
	    for_all_unit_types(u) {
		for (i = 0; i < numindeptodo[u]; ++i) {
		    if (find_unit_place(u, x0, y0, &x, &y)) {
			unit = create_unit(u, TRUE);
			if (unit != NULL) {
			    init_supply(unit);
			    enter_cell(unit, x, y);
			    announce_unit_progress(++totnumdone);
			}
		    } else {
			/* If can't find places for this type, give up. */
			numindepleft[u] = numindeptodo[u] - i;
			break;
		    }
		}
	    }
	    /* Now do units that have to be occupants.  Note that if occupants
	       are allowed in independent units, then they might be
	       placed in an independent rather than an owned unit. */
	    for_all_unit_types(u) {
		if (!canbeinopen[u]) {
		    for (i = 0; i < numtodo[u]; ++i) {
			if ((transport = find_occupant_place(side, u, x0, y0))
			    != NULL) {
			    unit = create_unit(u, TRUE);
			    if (unit != NULL) {
				set_unit_side(unit, side);
				set_unit_origside(unit, side);
				init_supply(unit);
				enter_transport(unit, transport);
	   			announce_unit_progress(++totnumdone);
			    }
			} else {
		   	    numleft[u] = numtodo[u] - i;
			    break;
			}
		    }
		}
	    }
	    /* Now warn about what couldn't be placed. */
	    totleft = numlisted = numfails = 0;
	    tmpbuf2[0] = '\0';
	    for_all_unit_types(u) {
		totleft += numleft[u];
		if (numleft[u] > 0 && numlisted < 5) {
		    ++numfails;
		    strcat(tmpbuf2, " ");
		    strcat(tmpbuf2, u_type_name(u));
		}
	    }
	    if (totleft > 0) {
		char tmpbuf3[BUFSIZE];

		init_warning("could not put %d units in %s country (%s%s)",
			     totleft, shortest_side_title(side, tmpbuf3),
			     tmpbuf2, (numfails >= 5 ? " etc" : ""));
	    }
	}
	/* Now set the side of the people in this country. */
	tmpside = side;
	apply_to_area(side->startx, side->starty, tmpradius + 1,
		      set_people_on_side);
	/* Make sure each unit has people there, if allowed at all. */
	for_all_side_units(side, unit) {
	    if (inside_area(unit->x, unit->y)
	        && t_country_people(terrain_at(unit->x, unit->y)) > 0) {
		set_people_side_at(unit->x, unit->y, side_number(side));
	    }
	}
    }
    finish_lengthy_process();
    /* Grow each country out to a maximum radius. */
    announce_lengthy_process("Growing countries");
    maxradius = min(area.width, g_radius_max());
    for_all_sides(side)
      side->finalradius = maxradius;
    for (i = baseradius; i < maxradius; ++i) {
    	announce_progress((100 * (i - baseradius)) / (maxradius - baseradius));
	for_all_sides(side) {
	    /* If side still allowed to grow, expand it. */
	    if (side->finalradius == maxradius) {
	    	tmpside = side;
	    	growth = 0;
	    	apply_to_ring(side->startx, side->starty, i - 2, i,
			      expand_country);
		/* If no actual growth happened in a step, the country
		   might have reached its natural size. */
	    	if (growth == 0 && probability(g_growth_stop())) {
		    side->finalradius = i;
	    	}
	    }
	}
    }
    /* Do a couple "consolidation" steps to fill in "jaggies". */
    for (i = 0; i < 2; ++i) {
	for_all_sides(side) {
	    tmpside = side;
	    apply_to_ring(side->startx, side->starty,
			  baseradius, side->finalradius - 1,
			  expand_country);
	}
    }
    /* Also remove isolated populations surrounded by one other side. */
    if (people_sides_defined()) {
	for_all_interior_cells(x, y) {
	    int pop, pop2, majoritypop;
	    
	    if ((pop = people_side_at(x, y)) != NOBODY) {
		majoritypop = NOBODY;
		for_all_directions(dir) {
		    if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
			if ((pop2 = people_side_at(x1, y1)) == pop)
			  goto nextcell;
			if (majoritypop == NOBODY) {
			    majoritypop = pop2;
			} else {
			    if (pop2 != majoritypop)
			      goto nextcell;
			}
		    }
		}
		if (majoritypop != NOBODY) {
		    set_people_side_at(x, y, majoritypop);
		}
	    }
	  nextcell:
	    pop = pop;  /* a dummy statement */
	}
    }
    finish_lengthy_process();
    /* Warn about any difficulties encountered. */
    if (badcountryplaces > 0) {
    	init_warning("%d of %d sides have undesirable locations",
		     badcountryplaces, numsides);
    }
    return TRUE;
}

/* Test to see whether the side's pre-existing setup already suffices
   as a country in this game. */

static int
country_is_complete(side, runs)
Side *side;
int runs;
{
    int u, totunits, numunits[MAXUTYPES];
    Unit *unit;
    int sideadvantage = max(1, side->advantage);
    int advantage = (side->player ? side->player->advantage : sideadvantage);

    /* If side no longer in game, then we don't want to return country synth on it. */
    if (!side->ingame)
      return TRUE;
    totunits = 0;
    for_all_unit_types(u)
      numunits[u] = 0;
    /* Count up all and only the completed and present units. */
    for_all_side_units(side, unit) {
    	if (in_play(unit)) {
	    ++totunits;
	    ++numunits[unit->type];
	}
    }
    /* See if we're re-running side generation for the benefit of adding a
       new side, use the total counts of units to distinguish the new side
       from the old sides. */ 
    if (runs > 0)
      return (totunits > 0);
    for_all_unit_types(u) {
	if (numunits[u] < ((u_start_with(u) * advantage) / sideadvantage))
	  return FALSE;
    }
    return TRUE;
}

/* Compute the average number of units of a given type that are currently
   in the game. */

static int
average_numunits(u)
int u;
{
    int count = 0, nsides = 0;
    Unit *unit;
    Side *side;

    for_all_sides(side) {
	if (side->ingame) {
	    ++nsides;
	    for_all_side_units(side, unit) {
		if (in_play(unit) && unit->type == u)
		  ++count;
	    }
	}
    }
    return (count / (nsides > 1 ? nsides - 1 : 1));
}

/* Count the cell as being of a particular type. */

static void
count_cells(x, y)
int x, y;
{
    ++numcells[terrain_at(x, y)];
}

/* Test whether a given location is desirable for a country.  It should be
   in the right distance range from other countries, and have enough of
   the right sorts of terrain. */

static int
good_place(cx, cy)
int cx, cy;
{
    int toofar = TRUE, notfirst = FALSE, px, py, dist, t;
    Side *side;

    /* Check the candidate position against the other countries' positions. */
    for_all_sides(side) {
	px = side->startx;  py = side->starty;
	if (inside_area(px, py)) {
	    notfirst = TRUE;
	    dist = distance(cx, cy, px, py);
	    /* Min separation default allows any min separation. */
	    if (dist < curmindistance)
	      return FALSE;
	    /* Default max separation says pos can never be too far away. */
	    if (curmaxdistance < 0 || dist < curmaxdistance)
	      toofar = FALSE;
	}
    }
    if (toofar && notfirst)
      return FALSE;
    /* Count the types of terrain in the country. */
    for_all_terrain_types(t)
      numcells[t] = 0;
    apply_to_area(cx, cy, tmpradius, count_cells);
    /* Check against our upper and lower limits on terrain. */
    for_all_terrain_types(t) {
    	if (numcells[t] < t_country_min(t))
    	  return FALSE;
	if (t_country_max(t) >= 0
	    && numcells[t] > t_country_max(t))
	  return FALSE;
    }
    return TRUE;
}

/* Work hard to find a place for a side's country.  First make some random
   trials, then start searching from the center of the area outwards.
   Then just pick a place and plan to patch up the terrain later. */

static void
find_a_place(side)
Side *side;
{
    int tries, x, y, maxtries = area.numcells / 5;
    Unit *unit;

    Dprintf("%s country ", side_desig(side));
    /* First see if any of our units is already at a good location
       to put the whole country. */
    /* (should choose randomly from units first?) */
    for_all_side_units(side, unit) {
	if (in_play(unit) && good_place(unit->x, unit->y)) {
	    side->startx = unit->x;  side->starty = unit->y;
	    Dprintf("placed at unit %s\n", unit_desig(unit));
	    return;
	}
    }
    /* Then try any of the independent units.  Probability of choosing
       one is inversely proportional to the total number of units. */
    for_all_side_units(indepside, unit) {
	if (in_play(unit)
	    && good_place(unit->x, unit->y)
	    && probability(max(1, 100 / numunits))) {
	    side->startx = unit->x;  side->starty = unit->y;
	    Dprintf("placed at indep unit %s\n", unit_desig(unit));
	    return;
	}
    }
    /* Then try some random locations. */
    for (tries = 0; tries < maxtries; ++tries) {
	if (tries % 10 == 0) {
	    announce_progress(sideprogress +
			      ((sidedeltahalf / 2) * tries) / maxtries);
	}
	random_point(&x, &y);
	/* Filter out points that are right on the edge.  If that's
	   where the only valid starting places are, the exhaustive
	   search will still find them. */
	if (between(2, y, area.height - 2)
	    && between(2, x, area.width - 2)
	    && good_place(x, y)) { 
	    side->startx = x;  side->starty = y;
	    Dprintf("placed on try %d\n", tries);
	    return;
	}
    }
    /* Then search exhaustively, starting from the center of the area. */
    if (search_around(area.width / 2, area.halfheight,
		      area.width / 2 + area.halfheight,
		      good_place, &x, &y, 1)) {
	side->startx = x;  side->starty = y;
	Dprintf("placed after search\n");
	return;
    }
    /* This should be a place in the area no matter what. */
    random_point(&x, &y);
    side->startx = x;  side->starty = y;
    Dprintf("placed randomly\n");
    ++badcountryplaces;
    /* Since placement has become difficult, we get permission to alter
       the terrain if necessary, while placing units. */
    mungterrain = TRUE;
}    

/* The basic conditions that *must* be met by an initial unit placement. */

static int
valid_unit_place(x, y)
int x, y;
{
    int t = terrain_at(x, y);

    return (inside_area(x, y)
	    && ut_favored(tmputype, t) > 0
	    && !ut_vanishes_on(tmputype, t)
	    && !ut_wrecks_on(tmputype, t)
	    && type_can_occupy_cell(tmputype, x, y));
}

static int
possible_unit_place(x, y)
int x, y;
{
    return (inside_area(x, y) && unit_at(x, y) == NULL);
}

/* Find a place somewhere in the designated area, following constraints
   on terrain and adjacency.  Returns success of placement. */

static int
find_unit_place(u, cx, cy, xp, yp)
int u, cx, cy, *xp, *yp;
{
    int tries, x, y;
    int maxtries = tmpradius * tmpradius * 5;

    Dprintf("%s place found ", u_type_name(u));
    tmputype = u;
    for (tries = 0; tries < maxtries; ++tries) {
	if (random_point_near(cx, cy, tmpradius, &x, &y)) {
	    if (mungterrain
	        && !valid_unit_place(x, y)
	        && possible_unit_place(x, y)
		&& favorite[u] != NONTTYPE
		&& probability(ut_favored(u, favorite[u]))) {
		Dprintf("(by munging) ");
		mung_terrain(x, y, u);
	    }
	    if (valid_unit_place(x, y)
	        && probability(ut_favored(u, terrain_at(x, y)))) {
	    	*xp = x;  *yp = y;
		Dprintf("on try %d of %d\n", tries, maxtries);
		return TRUE;
	    }
	}
    }
    /* Random points aren't working, switch to exhaustive search. */
    if (search_around(cx, cy, tmpradius, valid_unit_place, &x, &y, 1)) {
	*xp = x;  *yp = y;
	Dprintf("after search\n");
	return TRUE;
    }
    /* Search again, just find a location that we can alter to suit. */
    if (favorite[u] != NONTTYPE) {
        if (search_around(cx, cy, tmpradius, possible_unit_place, &x, &y, 1)) {
            mung_terrain(x, y, u);
	    *xp = x;  *yp = y;
	    Dprintf("(by munging) after search\n");
	    return TRUE;
	}
    }
    Dprintf("- NOT!\n");
    return FALSE;
}

static void
mung_terrain(x, y, u)
int x, y, u;
{
    int dir, x1, y1;

    /* Alter the terrain to be compatible with the given unit type. */
    set_terrain_at(x, y, favorite[u]);
    /* Mung some empty adjacent cells also, improves appearance. */
    for_all_directions(dir) {
	if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
	    if (unit_at(x1, y1) == NULL && flip_coin()) {
		set_terrain_at(x1, y1, favorite[u]);
	    }
	}
    }
}

static Unit *
find_occupant_place(side, u, x0, y0)
Side *side;
int u, x0, y0;
{
    Unit *transport;

    /* (should cast about randomly first, then do full search) */
    for_all_side_units(side, transport) {
	if (is_unit(transport)
	    && inside_area(transport->x, transport->y)
	    && type_can_occupy(u, transport)) {
	    return transport;
	}
    }
    /* (should be able to look at indep units in country) */
    return NULL;
}

static int
at_country_units_max(side, u)
Side *side;
int u;
{
    int themax = u_country_units_max(u);

    if (themax < 0)
      return FALSE;
    /* (should adjust for advantage?) */
    return (themax <= side->numunits[u]);
}

/* A method that scatters independent units over the world. */

int
make_independent_units(calls, runs)
int calls, runs;
{
    int u, t, x, y;
    int doindeptype[MAXUTYPES], doanytype = FALSE, doindeppeople = FALSE;
    Unit *unit;

    /* Can't do anything without some terrain. */
    if (!terrain_defined())
      return FALSE;
    /* Decide which types will be put down. */
    for_all_unit_types(u) {
    	doindeptype[u] = FALSE;
    	for_all_terrain_types(t) {
    	    if (ut_indep_density(u, t) > 0 && !ut_vanishes_on(u, t)) {
    	    	doindeptype[u] = TRUE;
    	    	doanytype = TRUE;
    	    	break;
    	    }
    	}
    }
    /* Make space for people sides if we're going to have any. */
    if (!people_sides_defined()) {
	for_all_terrain_types(t) {
	    if (t_indep_people(t) > 0)
	      doindeppeople = TRUE;
	}
	if (doindeppeople) {
	    allocate_area_people_sides();
	}
    }
    /* If no units or peoples to do, don't waste time going through
       the world. */
    if (!doanytype && !doindeppeople)
      return FALSE;
    announce_lengthy_process("Making independents");
    /* Now apply the process to each cell individually. */
    for_all_interior_cells(x, y) {
    	/* Progress is approx proportional to "column" being worked on. */
    	if (y == 1 && x % 5 == 0)
    	  announce_progress((x * 100) / area.width);
	t = terrain_at(x, y);
	for_all_unit_types(u) {
	    if (doindeptype[u]) {
		if (ut_indep_density(u, t) > xrandom(10000)) {
	    	    if (!ut_vanishes_on(u, t)
			&& type_can_occupy_cell(u, x, y)) {
			unit = create_unit(u, TRUE);
			if (unit != NULL) {
			    init_supply(unit);
			    enter_cell(unit, x, y);
		    	}
		    }
		}
	    }
	}
	if (doindeppeople
	    /* Don't put indeps on top of people already on a side. */
	    && people_side_at(x, y) == NOBODY
	    && probability(t_indep_people(t))) {
	    /* 0 represents indep people present */
	    set_people_side_at(x, y, 0);
	}
    }
    finish_lengthy_process();
    return TRUE;
}
