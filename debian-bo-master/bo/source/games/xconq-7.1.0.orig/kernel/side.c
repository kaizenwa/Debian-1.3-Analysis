/* Sides in Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This file implements sides and functionality relating to sides in
   general. */

#include "conq.h"

#define checked_elev_at(x, y) (elevations_defined() ? elev_at(x, y) : 0)

static void init_visible_elevation PARAMS ((int x, int y));
static void init_visible_elevation_2 PARAMS ((int x, int y));
static void calc_visible_elevation PARAMS ((int x, int y));
static void calc_visible_elevation_2 PARAMS ((int x, int y));

/* Head of the list of all sides. */

Side *sidelist;

/* Pointer to the side representing independence. */

Side *indepside;

/* Pointer to the last side of the list. */

Side *lastside;

/* Temporary used in many places. */
     
Side *tmpside;

/* The actual number of sides in a game.  This number never decreases;
   sides no longer playing need to be around for recordskeeping purposes. */

int numsides;

/* Used to generate the id number of the side. */

int nextsideid;

/* Cached values of global vision vars. */

int all_see_all;

int any_los;

/* Pointer to buffer used for readable side description (for debugging). */

char *sidedesigbuf = NULL;

/* Pointer to the head of the list of players. */

Player *playerlist;

/* Pointer to the last player of the list, used to add players at end. */

Player *last_player;

/* The total number of players. */

int numplayers;

/* Use to generate the id number of each player. */

int nextplayerid;

char *playerdesigbuf = NULL;

/* The list of doctrine objects. */

Doctrine *doctrine_list;

Doctrine *last_doctrine;

int next_doctrine_id;

/* Init side machinery.   We always have an "independent" side hanging around;
   it will be at the head of the list of sides, but normal side iteration
   finesses this by starting from the second list element.  The independent
   side is also a good placeholder for random stuff. */

void
init_sides()
{
    /* Set up the list of sides. */
    sidelist = lastside = (Side *) xmalloc(sizeof(Side));
    /* The independent units' side will be the first one, is always created. */
    indepside = sidelist;
    /* Fill in some properties of the independents' side. */
    indepside->name = "- indep -";
    indepside->noun = "- indep -";
    indepside->pluralnoun = "- indeps -";
    indepside->adjective = "independent";
    indepside->colorscheme = NULL;
    /* By default, independent units are not identified by an emblem. */
    /* A game design can set this appropriately if desired, however. */
    indepside->emblemname = "none";
    indepside->id = 0;
    /* Put valid Lisp data into slots that need it. */
    indepside->symbol = lispnil;
    indepside->instructions = lispnil;
    indepside->rawscores = lispnil;
    indepside->aidata = lispnil;
    indepside->uidata = lispnil;
    /* The independent side is not counted in numsides. */
    numsides = 0;
    /* Regular side ids start at 1 and go up. */
    nextsideid = 1;
    /* Set up the list of players. */
    playerlist = last_player = NULL;
    numplayers = 0;
    nextplayerid = 1;
    /* Set up the player/side assignment array. */
    assignments = (Assign *) xmalloc(MAXSIDES * sizeof(Assign));
    /* Set up the list of doctrines. */
    doctrine_list = last_doctrine = NULL;
    next_doctrine_id = 1;
}

/* Create an object representing a side. */

Side *
create_side()
{
    int u;
    Side *newside;

    if (numsides >= g_sides_max()) {
	run_error("Cannot have more than %d sides total!", g_sides_max());
    }
    /* Allocate the side object proper. */
    newside = (Side *) xmalloc(sizeof(Side));
    /* Fill in various side slots.  Only those with non-zero/non-NULL
       defaults need have anything done to them. */
    newside->id = nextsideid++;
    /* Always start sides IN the game. */
    newside->ingame = TRUE;
    /* Note that "everingame" is only set at the beginning of a turn. */
    /* Set up the relationships with other sides. */
    newside->knows_about = ALLSIDES; /* for now */
    newside->trusts = (short *) xmalloc((g_sides_max() + 1) * sizeof(short));
    newside->trades = (short *) xmalloc((g_sides_max() + 1) * sizeof(short));
    /* Set up per-unit-type slots. */
    newside->counts = (short *) xmalloc(numutypes * sizeof(short));
    newside->tech = (short *) xmalloc(numutypes * sizeof(short));
    newside->inittech = (short *) xmalloc(numutypes * sizeof(short));
    newside->numunits = (short *) xmalloc(numutypes * sizeof(short));
    newside->numlive = (short *) xmalloc(numutypes * sizeof(short));
    for_all_unit_types(u) {
	/* Start unit numbering at 1, not 0. */
	newside->counts[u] = 1;
    }
    newside->priority = -1;
    newside->autofinish = TRUE;
    /* True by default, players should disable manually. */
    newside->willingtosave = TRUE;
    /* Put valid Lisp data into slots that need it. */
    newside->symbol = lispnil;
    newside->instructions = lispnil;
    newside->rawscores = lispnil;
    newside->aidata = lispnil;
    newside->uidata = lispnil;
    newside->startx = newside->starty = -1;
    newside->gaincounts = (short *) xmalloc(numutypes * num_gain_reasons * sizeof(short));
    newside->losscounts = (short *) xmalloc(numutypes * num_loss_reasons * sizeof(short));
    newside->atkstats = (long **) xmalloc(numutypes * sizeof(long *));
    newside->hitstats = (long **) xmalloc(numutypes * sizeof(long *));
    /* Link in at the end of the list of sides. */
    newside->next = NULL;
    lastside->next = newside;
    lastside = newside;
    init_side_unithead(newside);
    ++numsides;
    return newside;
}

/* To make the double links work, we have to have one pseudo-unit to serve
   as a head.  This unit should not be seen by any outside code. */

void
init_side_unithead(side)
Side *side;
{
    if (side->unithead == NULL) {
	side->unithead = create_bare_unit(0);
	side->unithead->next = side->unithead;
	side->unithead->prev = side->unithead;
    }
}

/* Quick check to see if we have units. */

/* This should be improved to not be fooled by dead units? */

int
side_has_units(side)
Side *side;
{
    return (side->unithead != NULL
	    && (side->unithead->next != side->unithead));
}

/* Set up doctrine structures. */

void
init_doctrine(side)
Side *side;
{
    int u;

    if (side->default_doctrine == NULL) {
	side->default_doctrine = new_doctrine(0);
	side->default_doctrine->everaskside = TRUE;
	side->default_doctrine->construction_run =
	  (short *) xmalloc (numutypes * sizeof(short));
    }
    if (side->udoctrine == NULL) {
	/* Make each individual unit doctrine point to the generic doctrine by default. */
	side->udoctrine = (Doctrine **) xmalloc(numutypes * sizeof(Doctrine *));
    }
    for_all_unit_types(u) {
	if (side->udoctrine[u] == NULL)
	  side->udoctrine[u] = side->default_doctrine;
    }
}

void
init_self_unit(side)
Side *side;
{
    Unit *unit;

    if ((g_self_required() /* || side prop? */)
	&& side->self_unit == NULL) {
	for_all_side_units(side, unit) {
	    if (u_can_be_self(unit->type)
		&& in_play(unit)
		&& completed(unit))
	      side->self_unit = unit;
	}
    }
}

/* Initialize basic viewing structures for a side.  This happens when the
   side is created (during/after reading but before synthesis). */

int
init_view(side)
Side *side;
{
    int terrainset = FALSE, t, u;

    /* (should check world validity/compatibility first?) */
    if (side->terrview == NULL) {
	side->terrview = malloc_area_layer(char);
	if (!g_see_terrain_always()) {
	    side->terrviewdate = malloc_area_layer(short);
	    /* The terrview also holds the "have we seen it" flag, so it
	       must always be allocated, but the aux terrain view need not be. */
	    if (numcelltypes < numttypes) {
		side->auxterrview = (char **) xmalloc(numttypes * sizeof(short *));
		for_all_terrain_types(t) {
		    if (!t_is_cell(t)) {
			side->auxterrview[t] = malloc_area_layer(char);
		    }
		}
		if (0 /* aux terrain seen separately from main terrain */) {
		    side->auxterrviewdate = (short **) xmalloc(numttypes * sizeof(short *));
		    for_all_terrain_types(t) {
			if (!t_is_cell(t)) {
			    side->auxterrviewdate[t] = malloc_area_layer(short);
			}
		    }
		}
	    }
	}
    } else {
	terrainset = TRUE;
    }
    if (side->unitview == NULL) {
	side->unitview = malloc_area_layer(short);
	side->unitviewdate = malloc_area_layer(short);
#if (MAXVIEWHISTORY > 0)
	side->viewhistory[MAXVIEWHISTORY] = malloc_area_layer(long);
#endif
    }
    /* Allocate weather view layers. */
    if (any_temp_variation && !g_see_weather_always()) {
	if (side->tempview == NULL) {
	    side->tempview = malloc_area_layer(short);
	}
	if (side->tempviewdate == NULL) {
	    side->tempviewdate = malloc_area_layer(short);
	}
    }
    if (any_clouds && !g_see_weather_always()) {
	if (side->cloudview == NULL) {
	    side->cloudview = malloc_area_layer(short);
	}
	if (side->cloudbottomview == NULL) {
	    side->cloudbottomview = malloc_area_layer(short);
	}
	if (side->cloudheightview == NULL) {
	    side->cloudheightview = malloc_area_layer(short);
	}
	if (side->cloudviewdate == NULL) {
	    side->cloudviewdate = malloc_area_layer(short);
	}
    }
    if (any_wind_variation && !g_see_weather_always()) {
	if (side->windview == NULL) {
	    side->windview = malloc_area_layer(short);
	}
	if (side->windviewdate == NULL) {
	    side->windviewdate = malloc_area_layer(short);
	}
    }
    /* Allocate the vision coverage cache if needed. */
    if (side->coverage == NULL) {
	side->coverage = malloc_area_layer(short);
    }
    /* (should put this calc elsewhere?) */
    any_los = FALSE;
    for_all_unit_types(u) {
	if (u_vision_bend(u) != 100) {
	    any_los = TRUE;
	    break;
	}
    }
    /* Allocate vision altitude coverage if needed */
    if (side->alt_coverage == NULL && any_los) {
	side->alt_coverage = malloc_area_layer(short);
    }
    return terrainset;
}

/* Calculate the centroid of all the starting units. */

void
calc_start_xy(side)
Side *side;
{
    int num = 0, sumx = 0, sumy = 0;
    Unit *unit;

    for_all_side_units(side, unit) {
	if (in_play(unit)) {
	    sumx += unit->x;  sumy += unit->y;
	    ++num;
	}
    }
    if (num > 0) {
	side->startx = wrapx(sumx / num);  side->starty = sumy / num;
    }
}

/* Given a side, get its "number", which is same as its "id". */

int
side_number(side)
Side *side;
{
    return (side == NULL ? indepside->id : side->id);
}

/* The inverse function - given a number, figure out which side it is.
   Returns NULL for independent side number and for any failures. */

Side *
side_n(n)
int n;
{
    Side *side;

    if (n < 1)
      return NULL;
    for_all_sides(side)
      if (side->id == n)
        return side;
    return NULL;
}

Side *
find_side_by_name(str)
char *str;
{
    Side *side;

    if (empty_string(str))
      return NULL;
    for_all_sides(side) {
	if (!empty_string(side->name) && strcmp(side->name, str) == 0)
	  return side;
	if (!empty_string(side->noun) && strcmp(side->noun, str) == 0)
	  return side;
	if (!empty_string(side->pluralnoun) && strcmp(side->pluralnoun, str) == 0)
	  return side;
	if (!empty_string(side->adjective) && strcmp(side->adjective, str) == 0)
	  return side;
    }
    return NULL;
}

Side *
parse_side_spec(str)
char *str;
{
    int s;
    char *reststr;

    if (isdigit(str[0])) {
	s = strtol(str, &reststr, 10);
	if (between(1, s, numsides)) {
	    return side_n(s);
	}
    } else {
	return find_side_by_name(str);
    }
    return NULL;
}

/* This is true when one side controls another. */

int
side_controls_side(side, side2)
Side *side, *side2;
{
    if (side == NULL || side2 == NULL)
      return FALSE;
    return (side == side2 || side2->controlled_by == side);
}

short *max_control_ranges;

static int controller_here PARAMS ((int x, int y));

static int
controller_here(x, y)
int x, y;
{
    Unit *unit2;

    if (distance(x, y, tmpunit->x, tmpunit->y) < 2)
      return FALSE;
    for_all_stack(x, y, unit2) {
	if (side_controls_unit(tmpside, unit2)
	    && probability(uu_control(unit2->type, tmpunit->type)))
	  return TRUE;
    }
    return FALSE;
}

/* This is true if the given side may operate on the given unit. */

int
side_controls_unit(side, unit)
Side *side;
Unit *unit;
{
    int dir, x1, y1;
    Unit *unit2;

    if (side == NULL || unit == NULL)
      return FALSE;
#ifdef DESIGNERS
    if (side->designer)
      return TRUE;
#endif
    if (side_controls_side(side, unit->side)) {
	/* should check if type is controllable and within control coverage */
	/* should check if unit's cmdr is controlled */
	/* The *unit* side must have the tech to use the unit, the controlling
	   side would have to actually take over the unit if it wants to use
	   its tech level to control the unit. */
	if (unit->side != NULL && unit->side->tech[unit->type] < u_tech_to_use(unit->type))
	  return FALSE;
	if (u_direct_control(unit->type) || unit == side->self_unit)
	  return TRUE;
	/* Unit is not under direct control of the side; look for a controlled unit
	   that can control this unit. */
	if (max_control_ranges == NULL) {
	    int u1, u2;

	    max_control_ranges = (short *) xmalloc(numutypes * sizeof(short));
	    for_all_unit_types(u2) {
		max_control_ranges[u2] = -1;
		for_all_unit_types(u1) {
		    max_control_ranges[u2] =
		      max(max_control_ranges[u2], uu_control_range(u1, u2));
		}
	    }
	}
	if (max_control_ranges[unit->type] >= 0) {
	    for_all_stack(unit->x, unit->y, unit2) {
		if (unit != unit2
		    && side_controls_unit(side, unit2)
		    && probability(uu_control_at(unit2->type, unit->type)))
		  return TRUE;
	    }
	    /* (what about occupants that could be controllers?) */
	}
	if (max_control_ranges[unit->type] >= 1) {
	    for_all_directions(dir) {
		if (interior_point_in_dir(unit->x, unit->y, dir, &x1, &y1)) {
		    for_all_stack(x1, y1, unit2) {
			if (side_controls_unit(side, unit2)
			    && probability(uu_control_adj(unit2->type, unit->type)))
			  return TRUE;
		    }
		}
	    }
	}
	if (max_control_ranges[unit->type] >= 2) {
	    tmpside = side;
	    tmpunit = unit;
	    return search_around(unit->x, unit->y, max_control_ranges[unit->type],
	    			 controller_here, &x1, &y1, 1);
	}
    }
    /* (should add something for control for non-owned units?) */
    return FALSE;
}

/* This is true if the given side may examine the given unit. */

int
side_sees_unit(side, unit)
Side *side;
Unit *unit;
{
    if (side == NULL || unit == NULL)
      return FALSE;
#ifdef DESIGNERS
    if (side->designer)
      return TRUE;
#endif /* DESIGNERS */
    if (side_controls_side(side, unit->side))
      return TRUE;
    return FALSE;
}

int
side_sees_image(side, unit)
Side *side;
Unit *unit;
{
    if (side == NULL || unit == NULL)
      return FALSE;
#ifdef DESIGNERS
    if (side->designer)
      return TRUE;
#endif /* DESIGNERS */
    if (all_see_all)
      return TRUE;
    if (side_controls_side(side, unit->side))
      return TRUE;
    if (in_area(unit->x, unit->y)
	&& side->coverage != NULL
	&& cover(side, unit->x, unit->y) > 0)
      return TRUE;
    return FALSE;
}

int
num_units_in_play(side, u)
Side *side;
int u;
{
    int num = 0;
    Unit *unit;

    if (side != NULL && side->ingame) {
      for_all_side_units(side, unit) {
	if (unit->type == u
	    && in_play(unit)
	    && completed(unit))
	  ++num;
      }
    }
    return num;
}

int
num_units_incomplete(side, u)
Side *side;
int u;
{
    int num = 0;
    Unit *unit;

    if (side != NULL && side->ingame) {
      for_all_side_units(side, unit) {
	if (unit->type == u && alive(unit) && !completed(unit)) ++num;
      }
    }
    return num;
}

Unit *
find_next_unit(side, prevunit)
Side *side;
Unit *prevunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (prevunit == NULL)
	  prevunit = side->unithead;
	for (unit = prevunit->next; unit != prevunit; unit = unit->next) {
	    if (is_unit(unit) && unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)) {
		return unit;
	    }
	}
    }
    return NULL;
}

Unit *
find_prev_unit(side, nextunit)
Side *side;
Unit *nextunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (nextunit == NULL) nextunit = side->unithead;
	for (unit = nextunit->prev; unit != nextunit; unit = unit->prev) {
	    if (is_unit(unit) && unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)) {
		return unit;
	    }
	}
    }
    return NULL;
}

Unit *
find_next_actor(side, prevunit)
Side *side;
Unit *prevunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (prevunit == NULL) prevunit = side->unithead;
	for (unit = prevunit->next; unit != prevunit; unit = unit->next) {
	    if (is_unit(unit) && unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)
		&& (unit->act && unit->act->initacp)) {
		return unit;
	    }
	}
    }
    return NULL;
}

Unit *
find_prev_actor(side, nextunit)
Side *side;
Unit *nextunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (nextunit == NULL)
	  nextunit = side->unithead;
	for (unit = nextunit->prev; unit != nextunit; unit = unit->prev) {
	    if (is_unit(unit) && unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)
		&& (unit->act && unit->act->initacp)) {
		return unit;
	    }
	}
    }
    return NULL;
}

Unit *
find_next_mover(side, prevunit)
Side *side;
Unit *prevunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (prevunit == NULL)
	  prevunit = side->unithead;
	for (unit = prevunit->next; unit != prevunit; unit = unit->next) {
	    if (is_unit(unit) && unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)
		&& (unit->act && unit->act->acp > 0)) {
		return unit;
	    }
	}
    }
    return NULL;
}

Unit *
find_prev_mover(side, nextunit)
Side *side;
Unit *nextunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (nextunit == NULL)
	  nextunit = side->unithead;
	for (unit = nextunit->prev; unit != nextunit; unit = unit->prev) {
	    if (is_unit(unit) && unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)
		&& (unit->act && unit->act->acp > 0)) {
		return unit;
	    }
	}
    }
    return NULL;
}

Unit *
find_next_awake_mover(side, prevunit)
Side *side;
Unit *prevunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (prevunit == NULL)
	  prevunit = side->unithead;
	for (unit = prevunit->next; unit != prevunit; unit = unit->next) {
	    if (is_unit(unit)
		&& unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)
		&& (unit->act && unit->act->acp > 0)
		&& (unit->plan
		    && !unit->plan->asleep
		    && !unit->plan->reserve
		    && !unit->plan->delayed)) {
		return unit;
	    }
	}
    }
    return NULL;
}

Unit *
find_prev_awake_mover(side, nextunit)
Side *side;
Unit *nextunit;
{
    Unit *unit = NULL;

    if (side != NULL) {
	if (nextunit == NULL) nextunit = side->unithead;
	for (unit = nextunit->prev; unit != nextunit; unit = unit->prev) {
	    if (is_unit(unit) && unit->id > 0
		&& alive(unit)
		&& inside_area(unit->x, unit->y)
				&& (unit->act && unit->act->acp > 0)
				&& (unit->plan && !unit->plan->asleep && !unit->plan->reserve && !unit->plan->delayed)) {
		return unit;
	    }
	}
    }
    return NULL;
}

/* Compute the total number of action points available to the side at the
   beginning of the turn. */

int
side_initacp(side)
Side *side;
{
    int totacp = 0;
    Unit *unit;

    for_all_side_units(side, unit) {
	if (alive(unit) && unit->act) {
	    totacp += unit->act->initacp;
	}
    }
    return totacp;
}

/* Return the total of acp still unused by the side. */

int
side_acp(side)
Side *side;
{
    int acpleft = 0;
    Unit *unit;

    for_all_side_units(side, unit) {
	if (alive(unit) && unit->act) {
	    acpleft += unit->act->acp;
	}
    }
    return acpleft;
}

int
side_acp_reserved(side)
Side *side;
{
    int acpleft = 0;
    Unit *unit;

    for_all_side_units(side, unit) {
	if (alive(unit) && unit->act) {
		if (unit->plan
		    && (unit->plan->reserve || unit->plan->asleep)) {
	    	acpleft += unit->act->acp;
	    }
	}
    }
    return acpleft;
}

#if 0  /* not presently needed, but possibly useful */
/* Return the total of acp still expected to be used by a human player. */

int
side_acp_human(side)
Side *side;
{
    int acpleft = 0;
    Unit *unit;

    if (!side_has_display(side))
      return acpleft;

    for_all_side_units(side, unit) {
	if (unit != NULL
	    && alive(unit)
	    && inside_area(unit->x, unit->y)
	    && (unit->act
		&& unit->act->acp > 0)
	    && (unit->plan
		&& !unit->plan->asleep
		&& !unit->plan->reserve
		&& unit->plan->tasks == NULL)) {
		acpleft += unit->act->acp;
	}
    }
    return acpleft;
}
#endif

/* (note that technology could be "factored out" of a game if all sides
   reach max tech at some point) */
/* (otherwise should compute once and cache) */
/* (note that once tech factors out, can never factor in again) */

int
using_tech_levels()
{
    int u;
    Side *side;

    for_all_sides(side) {
	for_all_unit_types(u) {
	    if (side->tech[u] < u_tech_max(u)) return TRUE;
	}
    }
    return FALSE;
}

/* Take the given side out of the game entirely.  This does not imply
   winning or losing, nor does it take down the side's display or AI. */

void
remove_side_from_game(side)
Side *side;
{
    Side *side2;

    /* Officially flag this side as being no longer in the game. */
    side->ingame = FALSE;
    /* Update everybody on this. */
    for_all_sides(side2) {
	update_side_display(side2, side, TRUE);
    }
    /* Note that we no longer try to remove any images from other sides'
       views, because even with the side gone, the images may be useful
       information about where its units had gotten to.  For instance,
       if a unit had been captured shortly before the side lost, then its
       image might still correspond to an actual unit, with only its side
       changed, and other sides may want to investigate for themselves. */
}

int
num_displayed_sides()
{
    int n = 0;
    Side *side;

    for_all_sides(side) {
	if (side_has_display(side))
	  ++n;
    }
    return n;
}

void
set_side_name(side, side2, newname)
Side *side, *side2;
char *newname;
{
    side2->name = newname;
    /* (should inform all other sides!) */
    update_side_display(side, side2, TRUE);
}

#ifdef DESIGNERS

void
become_designer(side)
Side *side;
{
    Side *side2;

    if (side->designer)
      return;
    side->designer = TRUE;
    ++numdesigners;
    /* Designers have godlike power in the game, so mark it (permanently)
       as no longer a normal game. */
    compromised = TRUE;
    side->may_set_see_all = TRUE;
    /* See everything by default - can turn off manually if desired. */
    side->see_all = TRUE;
    update_everything();
    for_all_sides(side2) {
	update_side_display(side2, side, TRUE);
    }
}

void
become_nondesigner(side)
Side *side;
{
    Side *side2;

    if (!side->designer)
      return;
    side->designer = FALSE;
    --numdesigners;
    side->may_set_see_all = FALSE;
    side->see_all = all_see_all;
    update_everything();
    for_all_sides(side2) {
	update_side_display(side2, side, TRUE);
    }
}

#endif /* DESIGNERS */

int
trusted_side(side1, side2)
Side *side1, *side2;
{
    if (side1 == side2)
      return TRUE;
    if (side1 == NULL || side2 == NULL || side1->trusts == NULL)
      return FALSE;
    return (side1->trusts[side_number(side2)]);
}

void
set_trust(side, side2, val)
Side *side, *side2;
int val;
{
    int oldval;
    Side *side3;

    if (side == NULL || side2 == NULL || side == side2)
      return;
    if (side->trusts == NULL)
      return;
    oldval = side->trusts[side_number(side2)];
    side->trusts[side_number(side2)] = val;
    /* This is a major change that all other sides will know about. */
    if (oldval != val) {
	for_all_sides(side3) {
	    if (active_display(side3)) {
		notify(side3, "%s %s %s",
		       side_name(side),
		       (val ? "now trusts" : "no longer trusts"),
		       side_name(side2));
		update_side_display(side3, side, FALSE);
		update_side_display(side3, side2, TRUE);
	    }
	}
	/* (should update views, list of units known about, etc) */
	/* (if cell goes from exact to only recorded, update display anyhow) */
    }
}

void
set_mutual_trust(side, side2, val)
Side *side, *side2;
int val;
{
    int oldval, oldval2;
    Side *side3;

    if (side == NULL || side2 == NULL || side == side2)
      return;
    if (side->trusts == NULL || side2->trusts == NULL)
      return;
    oldval = side->trusts[side_number(side2)];
    oldval2 = side2->trusts[side_number(side)];
    side->trusts[side_number(side2)] = val;
    side2->trusts[side_number(side)] = val;
    if (oldval != val || oldval2 != val) {
	for_all_sides(side3) {
	    update_side_display(side3, side, FALSE);
	    update_side_display(side3, side2, TRUE);
	}
	/* (should update views, list of units known about, etc) */
    }
}

/* What interfaces should use to tweak the autofinish flag. */

void
set_autofinish(side, value)
Side *side;
int value;
{
    side->autofinish = value;
}

/* Being at war requires only ones of the sides to consider itself so. */

/* (Should the other side's relationships be tweaked also?) */

int
enemy_side(s1, s2)
Side *s1, *s2;
{
    if (trusted_side(s1, s2))
      return FALSE;
    return TRUE;
}

/* A formal alliance requires the agreement of both sides. */

int
allied_side(s1, s2)
Side *s1, *s2;
{
    if (trusted_side(s1, s2))
      return TRUE;
    return FALSE;
}

/* Neutralness is basically anything else. */

int
neutral_side(s1, s2)
Side *s1, *s2;
{
    return (!enemy_side(s1, s2) && !allied_side(s1, s2));
}

void
set_willing_to_save(side, flag)
Side *side;
int flag;
{
    int oldflag = side->willingtosave;
    Side *side2;

    if (flag != oldflag) {
	side->willingtosave = flag;
	/* Inform everybody of our willingness to save. */
	for_all_sides(side2) {
	    if (active_display(side2)) {
		notify(side2, "%s is%s willing to save the game.",
			      side_name(side), (flag ? "" : " not"));
		update_side_display(side2, side, TRUE);
	    }
	}
    }
}

void
set_willing_to_draw(side, flag)
Side *side;
int flag;
{
    int oldflag = side->willingtodraw;
    Side *side2;

    if (flag != oldflag) {
	side->willingtodraw = flag;
	/* Inform everybody of our willingness to draw. */
	for_all_sides(side2) {
	    if (active_display(side2)) {
		notify(side2, "%s is%s willing to declare the game a draw.",
			      side_name(side), (flag ? "" : " not"));
		update_side_display(side2, side, TRUE);
	    }
	}
    }
}

/* Set the self-unit of the given side.  This is only called when done at
   the direction of the side, and may fail if the side can't change its
   self-unit voluntarily.  Return success or failure of change. */

int
set_side_self_unit(side, unit)
Side *side;
Unit *unit;
{
    if (!in_play(unit))
      return FALSE;
    if (side->self_unit
        && in_play(side->self_unit)
        && !u_self_changeable(side->self_unit->type))
      return FALSE;
    side->self_unit = unit;
    /* (should update some part of display?) */
    return TRUE;
}

/* Message-forwarding function. */

void
send_message(side, sidemask, str)
Side *side;
SideMask sidemask;
char *str;
{
    Side *side2;

    for_all_sides(side2) {
	if (side2 != side && (sidemask & (1 << side2->id))) {
	    receive_message(side2, side, str);
	}
    }
}

/* Handle the receipt of a message.  Some messages may result in specific
   actions, but the default is just to forward to AIs and displays. */

void
receive_message(side, sender, str)
Side *side, *sender;
char *str;
{
    /* First look for specially-recognized messages. */
    if (strcmp("%reveal", str) == 0) {
	reveal_side(sender, side, NULL);
    } else {
	/* Give the message to interface if present. */
	if (side_has_display(side)) {
	    update_message_display(side, sender, str, TRUE);
	}
	/* Also give the message to any AI. */
	if (side_has_ai(side)) {
	    ai_receive_message(side, sender, str);
	}
    }
}

/* General method for passing along info about one side to another. */

void
reveal_side(sender, recipient, types)
Side *sender, *recipient;
int *types;
{
    int x, y;
    Unit *unit;

    if (all_see_all)
      return;
    if (!g_terrain_seen()) {
	for_all_cells(x, y) {
	    if (terrain_view(sender, x, y) != UNSEEN
	        && terrain_view(recipient, x, y) == UNSEEN) {
	        set_terrain_view(recipient, x, y, terrain_view(sender, x, y));
	        /* (should update unit views also) */
		update_cell_display(recipient, x, y, TRUE);
	    }
	}
    }
    for_all_side_units(sender, unit) {
	if (in_play(unit) && (types == NULL || types[unit->type])) {
	    see_exact(recipient, unit->x, unit->y);
	    update_cell_display(recipient, unit->x, unit->y, TRUE);
	}
    }
}

/* Vision. */

void
calc_vision(side)
Side *side;
{
}

/* What happens when a unit appears on a given cell. */

/* An always-seen unit has builtin spies/tracers to inform everybody else of
   all its movements.  When such a unit occupies a cell, coverage is turned
   on and remains on until the unit leaves that cell. */

/* If this unit with onboard spies wanders into unknown territory,
   shouldn't that territory become known as well?  I think the unseen
   test should only apply during initialization. */
/* But if always-seen unit concealed during init, will magically appear
   when it first moves! */

void
all_see_occupy(unit, x, y, inopen)
Unit *unit;
int x, y;
int inopen;
{
    Side *side;
    int always = u_see_always(unit->type);
    
    for_all_sides(side) {
	if (side->see_all) {
	    update_cell_display(side, x, y, TRUE);
	} if (side_sees_unit(side, unit)) {
	    see_cell(side, x, y);
	} else {
	    if (always && terrain_view(side, x, y) != UNSEEN) {
		add_cover(side, x, y, 1);
		set_alt_cover(side, x, y, 0);
	    }
	    if (inopen || always) {
		see_cell(side, x, y);
	    }
	}
    }
    {
	int dir, x1, y1;
	Unit *unit2, *unit3;
	    	
	for_all_directions(dir) {
	    if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
	   	for_all_stack(x1, y1, unit2) {
	   	    if (unit2->side != NULL
	   	    	&& unit2->side != unit->side
	   	    	&& units_visible(unit2->side, x, y)
	   	    	&& !unit_trusts_unit(unit2, unit)) {
	   	    	wake_unit(unit2, FALSE, 0, NULL);
	   	    }
	   	    for_all_occupants(unit2, unit3) {
			if (unit3->side != NULL
			    && unit3->side != unit->side
			    && units_visible(unit3->side, x, y)
			    && !unit_trusts_unit(unit3, unit)) {
			    wake_unit(unit3, FALSE, 0, NULL);
			}
	   	    }
	   	}
	    }
	}
    }
}

/* Some highly visible unit types cannot leave a cell without everybody
   knowing about the event.  The visibility is attached to the unit, not
   the cell, so first the newly-empty cell is viewed, then view coverage
   is decremented. */

void
all_see_leave(unit, x, y, inopen)
Unit *unit;
int x, y;
int inopen;
{
    Side *side;
    int always = u_see_always(unit->type);
    int olduview;

    for_all_sides(side) {
	if (side->see_all) {
	    update_cell_display(side, x, y, TRUE);
	} else if (side_sees_unit(side, unit)) {
	    see_cell(side, x, y);
	} else {
	    if (always && terrain_view(side, x, y) != UNSEEN) {
		see_cell(side, x, y);
		if (cover(side, x, y) > 0)
		  add_cover(side, x, y, -1);
		if (side->alt_coverage)
		  /* (should recalc alt coverage, since was 0) */;
	    }
	    /* Won't be called twice, because view coverage is 0 now. */
	    if (inopen) {
		see_cell(side, x, y);
	    }
	    /* special hack to flush images we *know* are garbage */
	    if (cover(side, x, y) < 1) {
	    	olduview = unit_view(side, x, y);
	    	if (vside(olduview) == side_number(side)) {
	    	    set_unit_view(side, x, y, EMPTY);
	    	    set_unit_view_date(side, x, y, g_turn());
		    update_cell_display(side, x, y, FALSE);
	    	}
	    }
	}
    }
}

static int tmpx0, tmpy0, tmpz0;
static int tmpnx, tmpny, tmpnz;

static void
init_visible_elevation(x, y)
int x, y;
{
    int elev = checked_elev_at(x, y);

    set_tmp2_at(x, y, elev - tmpz0);
}

static void
init_visible_elevation_2(x, y)
int x, y;
{
    int elev = checked_elev_at(x, y);

    set_tmp3_at(x, y, elev - tmpnz);
}

/* (should reindent) */
static void
calc_visible_elevation(x, y)
int x, y;
{
    int dir, x1, y1, elev, tmp, tmpa, tmpb, adjelev = 9999, viselev, dist, dist1;
    int cellwid = area.cellwidth;

    elev = checked_elev_at(x, y);
    dist = distance(x, y, tmpx0, tmpy0);
    if (cellwid <= 0)
      cellwid = 1;
    for_all_directions(dir) {
	if (point_in_dir(x, y, dir, &x1, &y1)) {
	  dist1 = distance(x1, y1, tmpx0, tmpy0);
	  if (dist1 < dist) {
	    tmpa = tmp2_at(x1, y1);
	    /* Account for the screening effect of the elevation difference. */
	    /* (dist1 will never be zero) */
	    tmpa = (tmpa * dist * cellwid) / (dist1 * cellwid);
	    tmpb = checked_elev_at(x1, y1)
	           + t_thickness(terrain_at(x1, y1))
	           - tmpz0;
	    tmpb = (tmpb * dist * cellwid) / (dist1 * cellwid);
	    tmp = max(tmpa, tmpb);
	    adjelev = min(adjelev, tmp + tmpz0);
	  }
	}
    }
    viselev = max(adjelev, elev);
    set_tmp2_at(x, y, viselev - tmpz0);
}

/* (should reindent) */
static void
calc_visible_elevation_2(x, y)
int x, y;
{
    int dir, x1, y1, elev, tmp, tmpa, tmpb, adjelev = 9999, viselev, dist, dist1;
    int cellwid = area.cellwidth;

    elev = checked_elev_at(x, y);
    dist = distance(x, y, tmpnx, tmpny);
    if (cellwid <= 0)
      cellwid = 1;
    for_all_directions(dir) {
	if (point_in_dir(x, y, dir, &x1, &y1)) {
	  dist1 = distance(x1, y1, tmpnx, tmpny);
	  if (dist1 < dist) {
	    tmpa = tmp3_at(x1, y1);
	    /* Account for the screening effect of the elevation difference. */
	    /* (dist1 will never be zero) */
	    tmpa = (tmpa * dist * cellwid) / (dist1 * cellwid);
	    tmpb = checked_elev_at(x1, y1)
	           + t_thickness(terrain_at(x1, y1))
	           - tmpnz;
	    tmpb = (tmpb * dist * cellwid) / (dist1 * cellwid);
	    tmp = max(tmpa, tmpb);
	    adjelev = min(adjelev, tmp + tmpnz);
	  }
	}
    }
    viselev = max(adjelev, elev);
    set_tmp3_at(x, y, viselev - tmpnz);
}

/* Unit's beady eyes are shifting from one location to another.
   Since new things may be coming into view, we have to check and maybe
   draw lots of cells (but only need the one output flush, fortunately). */

/* (LOS comes in here, to make irregular coverage areas) */

void
cover_area(side, unit, x0, y0, nx, ny)
Side *side;
Unit *unit;
int x0, y0, nx, ny;
{
    int u = unit->type, range0, nrange, range, x, y, x1, y1, x2, y2, y1c, y2c, xw, cov, los, r;
    int xmin, ymin, xmax, ymax, oldcov, newcov, anychanges;
    extern int daynight, sunx, suny;

    if (side == NULL
	|| side->coverage == NULL
    	|| indep(unit)
	|| all_see_all
	|| !completed(unit)
	)
      return;
    range0 = nrange = u_vision_range(u);
    /* Adjust for the effects of nighttime on vision range. */
    if (in_area(x0, y0)) {
	if (night_at(x0, y0)) {
	    range0 =
	      (range0 * ut_vision_night_effect(u, terrain_at(x0, y0))) / 100;
	}
    } else {
	range0 = 0;
    }
    if (in_area(nx, ny)) {
	if (night_at(nx, ny)) {
	    nrange =
	      (nrange * ut_vision_night_effect(u, terrain_at(nx, ny))) / 100;
	}
    } else {
	nrange = 0;
    }
    range = max(range0, nrange);
    allocate_area_scratch(1);
    anychanges = FALSE;
    /* First, set the union of the from and to areas to the existing
       coverage. */
    /* These may be outside the area - necessary since units may be able
       to see farther in x than the height of the area. */
    /* Compute the maximum bounds that may be affected. */
    if (y0 >= 0) {
	if (ny >= 0) {
	    ymin = min(y0, ny);
	    ymax = max(y0, ny);
	} else {
	    ymin = ymax = y0;
	}
    } else if (ny >= 0) {
	ymin = ymax = ny;
    }
    if (x0 >= 0) {
	if (nx >= 0) {
	    xmin = min(x0, nx);
	    xmax = max(x0, nx);
	} else {
	    xmin = xmax = x0;
	}
    } else if (nx >= 0) {
	xmin = xmax = nx;
    }
    if (any_los) {
	/* Need extra scratch layers, will be used for visible
           elevation cache. */
	allocate_area_scratch(3);
    }
    los = FALSE;
    /* (should also adjust for effect of clouds here) */
    if (u_vision_bend(u) != 100) {
	los = TRUE;
	/* Compute the minimum elevation for visibility at each cell. */
	if (in_area(x0, y0)) {
	    tmpx0 = x0;  tmpy0 = y0;
	    tmpz0 = checked_elev_at(x0, y0)
		+ unit_alt(unit)
		+ ut_eye_height(u, terrain_at(x0, y0));
	    apply_to_area(x0, y0, range0, init_visible_elevation);
	    /* Leave own and adj cells alone, they will always be visible. */
	    for (r = 2; r <= range0; ++r) {
		apply_to_ring(x0, y0, r, r, calc_visible_elevation);
	    }
	    /* We now have a layer indicating how high things must be
               to be visible. */
	}
	if (in_area(nx, ny)) {
	    tmpnx = nx;  tmpny = ny;
	    tmpnz = checked_elev_at(nx, ny)
		+ unit_alt(unit)
		+ ut_eye_height(u, terrain_at(nx, ny));
	    apply_to_area(nx, ny, nrange, init_visible_elevation_2);
	    /* Leave own and adj cells alone, they will always be visible. */
	    for (r = 2; r <= nrange; ++r) {
		apply_to_ring(nx, ny, r, r, calc_visible_elevation_2);
	    }
	    /* We now have another layer, indicating how high things must be
               to be visible from the new location. */
	}
    }
    /* Copy the current coverage into the tmp layer. */
    y1 = y1c = ymin - range;
    y2 = y2c = ymax + range;
    /* Clip the iteration bounds. */
    if (y1c < 0)
      y1c = 0;
    if (y2c > area.height - 1)
      y2c = area.height - 1;
    for (y = y1c; y <= y2c; ++y) {
	x1 = xmin - range;
	x2 = xmax + range;
	for (x = x1; x <= x2; ++x) {
	    if (in_area(x, y)) {
		xw = wrapx(x);
		set_tmp1_at(xw, y, cover(side, xw, y));
	    }
	}
    }
    /* Decrement coverage around the old location. */
    if (in_area(x0, y0)) {
	y1 = y1c = y0 - range0;
	y2 = y2c = y0 + range0;
	/* Clip the iteration bounds. */
	if (y1c < 0)
	  y1c = 0;
	if (y2c > area.height - 1)
	  y2c = area.height - 1;
	for (y = y1c; y <= y2c; ++y) {
	    x1 = x0 - (y < y0 ? (y - y1) : range0);
	    x2 = x0 + (y > y0 ? (y2 - y) : range0);
	    for (x = x1; x <= x2; ++x) {
		if (in_area(x, y)) {
		    xw = wrapx(x);
		    if (!los
			|| ((tmp2_at(xw, y) + tmpz0)
			    <= (checked_elev_at(xw, y)
				+ t_thickness(terrain_at(xw, y))))) {
			cov = tmp1_at(xw, y) - 1;
			/* Should never go negative, detect if so. */
			if (cov < 0) {
			    Dprintf("Negative coverage for %s at %d,%d\n",
				    side_desig(side), xw, y);
			}
			set_tmp1_at(xw, y, cov);
		    }
		    if (los && (alt_cover(side, xw, y) == (tmp2_at(xw, y) + tmpz0)))
		      /* this unit set the min, should recalc alt coverage now */;
		}
	    }
	}
    }
    /* Increment coverage around the new location. */
    if (in_area(nx, ny)) {
	y1 = y1c = ny - nrange;
	y2 = y2c = ny + nrange;
	/* Clip the iteration bounds. */
	if (y1c < 0)
	  y1c = 0;
	if (y2c > area.height - 1)
	  y2c = area.height - 1;
	for (y = y1c; y <= y2c; ++y) {
	    x1 = nx - (y < ny ? (y - y1) : nrange);
	    x2 = nx + (y > ny ? (y2 - y) : nrange);
	    for (x = x1; x <= x2; ++x) {
		if (in_area(x, y)) {
		    xw = wrapx(x);
		    if (!los
			|| ((tmp3_at(xw, y) + tmpnz)
			    <= (checked_elev_at(xw, y)
				+ t_thickness(terrain_at(xw, y))))) {
			cov = tmp1_at(xw, y) + 1;
			set_tmp1_at(xw, y, cov);
		    }
		    if (los)
		      set_alt_cover(side, xw, y, min(alt_cover(side, xw, y), (tmp3_at(xw, y) + tmpnz)));
		}
	    }
	}
    }
    /* Now update the actual coverage.  Do this over an area that includes
       both the decrement and increment changes. */
    y1 = y1c = ymin - range;
    y2 = y2c = ymax + range;
    /* Clip the iteration bounds. */
    if (y1c < 0)
      y1c = 0;
    if (y2c > area.height - 1)
      y2c = area.height - 1;
    for (y = y1c; y <= y2c; ++y) {
	x1 = xmin - range;
	x2 = xmax + range;
	for (x = x1; x <= x2; ++x) {
	    if (in_area(x, y)) {
		xw = wrapx(x);
		oldcov = cover(side, xw, y);
		newcov = tmp1_at(xw, y);
		if (newcov != oldcov) {
		    set_cover(side, xw, y, newcov);
		    if (newcov > oldcov && see_cell(side, xw, y))
		      react_to_seen_unit(side, unit, xw, y);
		    if ((newcov > 0 && oldcov == 0)
			|| (newcov == 0 && oldcov > 0))
		      update_cell_display(side, xw, y, 36);
		    anychanges = TRUE;
		}
	    }
	}
    }
    /* If we're seeing new things, make sure they're on the display. */
    if (anychanges)
      flush_display_buffers(side);
}

/* Use this to clear out garbled view coverage. */

void
reset_coverage()
{
    Side *side;

    if (all_see_all)
      return;
    for_all_sides(side)
      calc_coverage(side);
}

/* Calculate/recalculate the view coverage layers of a side. */

void
calc_coverage(side)
Side *side;
{
    int x, y, s2, pop, visible[MAXSIDES];
    Unit *unit;

    if (side->coverage == NULL)
      return;
    Dprintf("Calculating all view coverage for %s\n", side_desig(side));
    /* Either init all cells to 0, or use populations to decide. */
    if (people_sides_defined()) {
	for (s2 = 0; s2 <= numsides; ++s2)
	  visible[s2] = (trusted_side(side_n(s2), side) ? 1 : 0);
	/* (should add controlled sides too) */
	for_all_cells(x, y) {
	    pop = people_side_at(x, y);
	    set_cover(side, x, y, ((pop != NOBODY && visible[pop]) ? 1 : 0));
	}
    } else {
	for_all_cells(x, y) {
	    set_cover(side, x, y, 0);
	}
    }
    /* Add coverage by, and of, the units already in place. */
    for_all_units(unit) {
	if (in_play(unit)) {
	    if (unit->side != NULL && trusted_side(unit->side, side)) {
		cover_area(side, unit, -1, -1, unit->x, unit->y);
	    } else if (u_see_always(unit->type)
		       && terrain_view(side, unit->x, unit->y) != UNSEEN) {
		add_cover(side, unit->x, unit->y, 1);
	    }
	}
    }
}

void
reset_all_views()
{
    Side *side;

    if (all_see_all)
      return;
    for_all_sides(side) {
	reset_view(side);
    }
}

void
reset_view(side)
Side *side;
{
    int x, y, uview;

    for_all_cells(x, y) {
	if (cover(side, x, y) == 0
	    && ((uview = unit_view(side, x, y)) != EMPTY)
	    && vside(uview) == side_number(side)) {
	    set_unit_view(side, x, y, EMPTY);
	}
    }
}

void
react_to_seen_unit(side, unit, x, y)
Side *side;
Unit *unit;
int x, y;
{
    int uview, eu;
    Unit *eunit;
    Side *es;

    if (all_see_all /* see real unit */) {
    	/* (should look at all of stack if can be mixed) */
	if ((eunit = unit_at(x, y)) != NULL) {
	    if (react_to_enemies(unit) && !allied_side(eunit->side, side)) {
		/* should do a more general alarm */
	        wake_unit(unit, TRUE, 0, NULL);
	    }
	}
    } else if (side->coverage != NULL) {
	uview = unit_view(side, x, y);
    	if (uview != EMPTY) {
    	    eu = vtype(uview);  es = side_n(vside(uview));
    	    /* react only to certain utypes? */
	    if (react_to_enemies(unit) && !allied_side(es, side)) {
		/* should do a more general alarm */
		wake_unit(unit, TRUE, 0, NULL);
	    }
    	}
    } else {
    	/* ??? */
    }
}

extern void compute_see_chances PARAMS ((void));

int any_see_chances = -1;

int any_people_see_chances = -1;

int max_see_chance_range;

/* Determine whether there is any possibility of an uncertain sighting,
   and cache the conclusion. */

void
compute_see_chances()
{
    int u1, u2, m1;

    any_see_chances = FALSE;
    any_people_see_chances = FALSE;
    max_see_chance_range = -1;
    for_all_unit_types(u2) {
	for_all_unit_types(u1) {
	    if (uu_see_at(u1, u2) != 100) {
		any_see_chances = TRUE;
		max_see_chance_range = max(max_see_chance_range, 0);
	    }
	    if (uu_see_adj(u1, u2) != 100) {
		any_see_chances = TRUE;
		max_see_chance_range = max(max_see_chance_range, 1);
	    }
	    if (uu_see(u1, u2) != 100) {
		any_see_chances = TRUE;
		max_see_chance_range = max(max_see_chance_range, u_vision_range(u1));
	    }
	}
	for_all_material_types(m1) {
	    if (um_people_see(u2, m1)) {
		any_people_see_chances = TRUE;
	    }
	}
    }
}

int test_for_viewer PARAMS ((int x, int y));

static Unit *tmpunittosee, *tmpunitseen;

int
test_for_viewer(x, y)
int x, y;
{
    int u2 = tmpunittosee->type, x2 = tmpunittosee->x, y2 = tmpunittosee->y, dist, chance;
    Unit *unit;

    dist = distance(x, y, x2, y2);
    for_all_stack(x, y, unit) {
	if (dist == 1)
	  chance = uu_see_adj(unit->type, u2);
	else
	  /* (should interpolate according to distance?) */
	  chance = uu_see(unit->type, u2);
	chance = (chance * ut_visibility(u2, terrain_at(x2, y2))) / 100;
	if (chance >= 100 || probability(chance)) {
	    tmpunitseen = tmpunittosee;
	    return TRUE;
	}
    }
    return FALSE;
}

/* Update the view of this cell for everybody's benefit.  May have to write
   to many displays. */

void
all_see_cell(x, y)
int x, y;
{
    register Side *side;

    for_all_sides(side) {
	see_cell(side, x, y);
    }
}

/* Look at the given position, possibly not seeing anything.  Return true if
   a unit was spotted. */

static int see_weather PARAMS ((Side *side, int x, int y));

int
see_cell(side, x, y)
Side *side;
int x, y;
{
    int update, chance, t, curview, curuview, newuview, x1, y1, m, wupdate;
    Unit *unit, *unitseen;

    if (side == NULL || side == indepside || !in_area(x, y))
      return FALSE;
    update = FALSE;
    unitseen = NULL;
    /* If we see everything, just pass through to updating the display. */
    if (side->see_all) {
    	update = TRUE;
    	unitseen = unit_at(x, y);
    } else if (cover(side, x, y) > 0) {
    	/* Always update our knowledge of the terrain. */
    	curview = terrain_view(side, x, y);
 	if (curview == UNSEEN || !g_see_terrain_always()) {
	    set_terrain_view(side, x, y, buildtview(terrain_at(x, y)));
	    if (!g_see_terrain_always())
	      set_terrain_view_date(side, x, y, g_turn());
	    update = TRUE;
	}
	if (temperatures_defined() || clouds_defined() || winds_defined()) {
	    wupdate = see_weather(side, x, y);
	    if (wupdate)
	      update = TRUE;
	}
	if (any_see_chances < 0)
	  compute_see_chances();
   	curuview = unit_view(side, x, y);
   	for_all_stack(x, y, unit) {
	    if (side_sees_unit(side, unit)) {
		unitseen = unit;
		break;
	    } else if (any_see_chances) {
	        /* (should always check for viewers at x,y first) */
		if (max_see_chance_range > 0) {
		    tmpunittosee = unit;
		    if (search_around(x, y, max_see_chance_range, test_for_viewer,
		    		      &x1, &y1, 1)) {
		    	unitseen = tmpunitseen;
		    	break;
		    }
		}
	    } else {
		t = terrain_at(x, y);
		chance = ut_visibility(unit->type, t);
		if (chance >= 100 || probability(chance)) {
		    unitseen = unit;
		    break;
		}
	    }
	}
	/* If there is still a unit to be seen at this location, then we know
	   that there was a stack before, so update to draw just the units left. */
	if (unitseen)
	  update = TRUE;
	/* See if any people in the cell see something. */
	if (any_people_see_chances
	    && unitseen == NULL
	    && any_cell_materials_defined()) {
   	    for_all_stack(x, y, unit) {
		for_all_material_types(m) {
		    if (cell_material_defined(m)
			&& material_at(x, y, m) > 0) {
			chance = um_people_see(unit->type, m);
			if (probability(chance)) {
			    unitseen = unit;
			    break;
			}
		    }
		}
		if (unitseen != NULL)
		  break;
   	    }
	}
	if (unitseen) {
	    newuview = builduview(side_number(unitseen->side), unitseen->type);
	    if (curuview != newuview) {
		/* We're seeing something different from what used to be there. */
		set_unit_view(side, x, y, newuview);
	    	set_unit_view_date(side, x, y, g_turn());
		update = TRUE;
	    } else {
	    	/* Same as we already know, so not considered a change. */
	    	unitseen = NULL;
	    }
	} else if (curuview != EMPTY) {
	    set_unit_view(side, x, y, EMPTY);
	    set_unit_view_date(side, x, y, g_turn());
	    update = TRUE;
	}
    }
    /* If there was any change in what was visible, tell the display. */
    if (update) {
	update_cell_display(side, x, y, FALSE);
    }
    return (unitseen != NULL);
}

/* "Bare-bones" viewing, for whenever you know exactly what's there.
   This is the lowest level of all viewing routines, and executed a lot. */

void
see_exact(side, x, y)
Side *side;
int x, y;
{
    register int olduview, oldtview, newtview, newuview, update, wupdate;
    register Unit *unit;

    if (side == NULL || side == indepside || !in_area(x, y))
      return;
    if (all_see_all) {
	/* It may not really be necessary to do anything to the display, but
	   the kernel doesn't know if the interface is drawing all the units
	   that are visible, or is only drawing "interesting" ones, or whatever.
	   It would be up to the interface to decide that, say, its magnification
	   power for a map is such that only one unit is being displayed, and
	   that the update from here doesn't result in any visible changes to
	   what's already been drawn on the screen. */
    	update = TRUE;
    } else {
	update = FALSE;
    	oldtview = terrain_view(side, x, y);
    	newtview = buildtview(terrain_at(x, y));
    	set_terrain_view(side, x, y, newtview);
	set_terrain_view_date(side, x, y, g_turn());
	if (oldtview != newtview)
	  update = TRUE;
	wupdate = see_weather(side, x, y);
	if (wupdate)
	  update = TRUE;
    	olduview = unit_view(side, x, y);
    	newuview = EMPTY;
    	unit = unit_at(x, y);
	if (unit != NULL)
	  newuview = builduview(side_number(unit->side), unit->type);
	set_unit_view(side, x, y, newuview);
	set_unit_view_date(side, x, y, g_turn());
	if (olduview != newuview)
	  update = TRUE;
    }
    /* If there was any change in what was visible, tell the display. */
    if (update) {
	update_cell_display(side, x, y, FALSE);
    }
}

static int
see_weather(side, x, y)
Side *side;
int x, y;
{
    int curview, update;

    update = FALSE;
    if (temperatures_defined()) {
	see_weather(side, x, y);
	curview = temperature_view(side, x, y);
	if (curview != temperature_at(x, y)) {
	    set_temperature_view(side, x, y, temperature_at(x, y));
	    update = TRUE;
	}
	/* Even if the data didn't change, our information is
	   now up-to-date. */
	if (!g_see_weather_always())
	  set_temperature_view_date(side, x, y, g_turn());
    }
    if (clouds_defined()) {
	curview = cloud_view(side, x, y);
	if (curview != raw_cloud_at(x, y)) {
	    set_cloud_view(side, x, y, raw_cloud_at(x, y));
	    update = TRUE;
	}
	curview = cloud_bottom_view(side, x, y);
	if (curview != raw_cloud_bottom_at(x, y)) {
	    set_cloud_bottom_view(side, x, y, raw_cloud_bottom_at(x, y));
	    update = TRUE;
	}
	curview = cloud_height_view(side, x, y);
	if (curview != raw_cloud_height_at(x, y)) {
	    set_cloud_height_view(side, x, y, raw_cloud_height_at(x, y));
	    update = TRUE;
	}
	/* Even if the data didn't change, our information is
	   now up-to-date. */
	if (!g_see_weather_always())
	  set_cloud_view_date(side, x, y, g_turn());
	/* Only need one date for the three layers of view data. */
    }
    if (winds_defined()) {
	curview = wind_view(side, x, y);
	if (curview != raw_wind_at(x, y)) {
	    set_wind_view(side, x, y, raw_wind_at(x, y));
	    update = TRUE;
	}
	/* Even if the data didn't change, our information is
	   now up-to-date. */
	if (!g_see_weather_always())
	  set_wind_view_date(side, x, y, g_turn());
    }
    return update;
}

/* A border has been seen if the cell on either side has been seen. */

int
seen_border(side, x, y, dir)
Side *side;
int x, y, dir;
{
    int x1, y1;

    if (side->see_all)
      return TRUE;
    if (terrain_view(side, x, y) != UNSEEN)
      return TRUE;
    if (point_in_dir(x, y, dir, &x1, &y1))
      if (terrain_view(side, x1, y1) != UNSEEN)
	return TRUE;
    return FALSE;
}

/* Make a printable identification of the given side. */

char *
side_desig(side)
Side *side;
{
    if (sidedesigbuf == NULL)
      sidedesigbuf = xmalloc(BUFSIZE);
    if (side != NULL) {
	sprintf(sidedesigbuf, "s%d (%s)", side_number(side), side_name(side));
	if (side->self_unit) {
	    tprintf(sidedesigbuf, "(self is #%d)", side->self_unit->id);
	}
    } else {
	sprintf(sidedesigbuf, "nullside");
    }
    return sidedesigbuf;
}

/* Add a player into the list of players.  All values are defaults here. */

Player *
add_player()
{
    Player *player = (Player *) xmalloc(sizeof(Player));

    player->id = nextplayerid++;
    /* Note that all names and suchlike slots are NULL. */
    ++numplayers;
    /* Add this one to the end of the player list. */
    if (last_player == NULL) {
	playerlist = last_player = player;
    } else {
	last_player->next = player;
	last_player = player;
    }
    Dprintf("Added a player\n");
    return player;
}

Player *
find_player(n)
int n;
{
    Player *player;

    for_all_players(player) {
       if (player->id == n)
	return player;
    }
    return NULL;
}

/* Transform a player object into a regularized form. */

void
canonicalize_player(player)
Player *player;
{
    if (player == NULL)
      return;
    if (empty_string(player->displayname))
      player->displayname = NULL;
    if (empty_string(player->aitypename))
      player->aitypename = NULL;
    /* This seems like a logical place to canonicalize an AI type
       of "ai" into a specific type, but we don't necessarily know
       the best default until the game is closer to starting. */
}

/* Make a printable identification of the given player. */

char *
player_desig(player)
Player *player;
{
    if (playerdesigbuf == NULL)
      playerdesigbuf = xmalloc(BUFSIZE);
    if (player != NULL) {
	sprintf(playerdesigbuf, "%s,%s/%s@%s+%d",
		(player->name ? player->name : ""),
		(player->aitypename ? player->aitypename : ""),
		(player->configname ? player->configname : ""),
		(player->displayname ? player->displayname : ""),
		player->advantage);
    } else {
	sprintf(playerdesigbuf, "nullplayer");
    }
    return playerdesigbuf;
}

/* Doctrine handling. */

Doctrine *
new_doctrine(id)
int id;
{
    Doctrine *doctrine = (Doctrine *) xmalloc(sizeof(Doctrine));

    if (id <= 0)
      id = next_doctrine_id++;
    doctrine->id = id;
    /* Add the new doctrine to the end of the list of doctrines. */
    if (last_doctrine != NULL) {
	last_doctrine->next = doctrine;
	last_doctrine = doctrine;
    } else {
	doctrine_list = last_doctrine = doctrine;
    }
    return doctrine;
}

Doctrine *
find_doctrine_by_name(name)
char *name;
{
    Doctrine *doctrine;

    if (name == NULL)
      return NULL;
    for (doctrine = doctrine_list; doctrine != NULL; doctrine = doctrine->next) {
	if (doctrine->name != NULL && strcmp(name, doctrine->name) == 0)
	  return doctrine;
    }
    return NULL;
}

Doctrine *
find_doctrine(id)
int id;
{
    Doctrine *doctrine;

    for (doctrine = doctrine_list; doctrine != NULL; doctrine = doctrine->next) {
	if (doctrine->id == id)
	  return doctrine;
    }
    return NULL;
}

Doctrine *
clone_doctrine(olddoc)
Doctrine *olddoc;
{
    int tmpid;
    Doctrine *newdoc, *tmpnext;

    newdoc = new_doctrine(0);
    tmpid = newdoc->id;
    tmpnext = newdoc->next;
    memcpy(newdoc, olddoc, sizeof(Doctrine));
    newdoc->id = tmpid;
    if (newdoc->construction_run) {
	newdoc->construction_run =
	  (short *) xmalloc (numutypes * sizeof(short));
	memcpy(newdoc->construction_run, olddoc->construction_run, numutypes * sizeof(short));
    }
    newdoc->next = tmpnext;
    return newdoc;
}

/* Standing order handling. */

StandingOrder *
new_standing_order()
{
    StandingOrder *sorder;

    sorder = (StandingOrder *) xmalloc(sizeof(StandingOrder));
    sorder->types = xmalloc(numutypes);
    return sorder;
}

int order_conds_match PARAMS ((StandingOrder *sorder, StandingOrder *sorder2));

void
add_standing_order(side, sorder, pos)
Side *side;
StandingOrder *sorder;
int pos;
{
    StandingOrder *sorder2;

    if (sorder->task == NULL) {
	if (side->orders == NULL) {
	    /* No orders, so nothing to do. */
	    return;
	} else if (order_conds_match(sorder, side->orders)) {
	    /* Delete the first order in the list. */
	    if (side->last_order == side->orders)
	      side->last_order = side->orders->next;
	    side->orders = side->orders->next;
	} else {
	    for (sorder2 = side->orders; sorder2->next != NULL; sorder2 = sorder2->next) {
		if (order_conds_match(sorder, sorder2->next)) {
		    if (side->last_order == sorder2->next)
		      side->last_order = sorder2->next->next;
		    sorder2->next = sorder2->next->next;
		    return;
		}
	    }
	    /* If we're here, no match; might be user error, but can't be sure,
	       so don't say anything. */
	}
    } else if (pos == 0) {
	sorder->next = side->orders;
	side->orders = sorder;
	if (side->last_order == NULL)
	  side->last_order = sorder;
    } else if (side->last_order != NULL) {
	side->last_order->next = sorder;
	side->last_order = sorder;
    } else {
	side->orders = side->last_order = sorder;
    }
}

int
order_conds_match(sorder, sorder2)
StandingOrder *sorder, *sorder2;
{
    return (sorder->etype == sorder2->etype
	    && sorder->a1 == sorder2->a1
	    && sorder->a2 == sorder2->a2
	    && sorder->a3 == sorder2->a3);
}

int
parse_standing_order(side, cmdstr)
Side *side;
char *cmdstr;
{
    StandingOrder *sorder;

    sorder = new_standing_order();
    cmdstr = parse_unit_types(side, cmdstr, sorder->types);
    if (cmdstr == NULL)
      return (-1);
    cmdstr = parse_order_cond(side, cmdstr, sorder);
    if (cmdstr == NULL)
      return (-2);
    cmdstr = parse_task(side, cmdstr, &(sorder->task));
    if (cmdstr == NULL)
      return (-3);
    add_standing_order(side, sorder, 0);
    return 0;
}

char *
parse_unit_types(side, str, utypevec)
Side *side;
char *str, *utypevec;
{
    char *arg, substr[BUFSIZE], *rest;
    int u;

    rest = get_next_arg(str, substr, &arg);
    u = utype_from_name(arg);
    if (u != NONUTYPE) {
	utypevec[u] = 1;
    } else if (strcmp(arg, "all") == 0) {
	for_all_unit_types(u)
	  utypevec[u] = 1;
    } else {
	notify(side, "Unit type \"%s\" not recognized", arg);
	return NULL;
    }
    return rest;
}

char *
parse_order_cond(side, str, sorder)
Side *side;
char *str;
StandingOrder *sorder;
{
    int x = 0, y = 0, dist = 0;
    char *arg, *arg2, substr[BUFSIZE], *rest;
    Unit *unit;

    rest = get_next_arg(str, substr, &arg);
    if (strcmp(arg, "at") == 0 || strcmp(arg, "@") == 0) {
	sorder->etype = 1;
    } else if (strcmp(arg, "in") == 0) {
	sorder->etype = 2;
    } else if (strcmp(arg, "within") == 0 || strcmp(arg, "near") == 0) {
	sorder->etype = 3;
    } else {
	notify(side, "Condition type \"%s\" not recognized", arg);
	return NULL;
    }
    if (sorder->etype == 3) {
	rest = get_next_arg(rest, substr, &arg);
	dist = strtol(arg, &arg2, 10);
	sorder->a3 = dist;
    }
    rest = get_next_arg(rest, substr, &arg);
    x = strtol(arg, &arg2, 10);
    if (arg != arg2 && *arg2 == ',') {
	y = strtol(arg2 + 1, &arg, 10);
	sorder->a1 = x;  sorder->a2 = y;
	return rest;
    } else if ((unit = find_unit_by_name(arg)) != NULL) {
	if (sorder->etype == 1 || sorder->etype == 3) {
	    sorder->a1 = x;  sorder->a2 = y;
	} else if (sorder->etype == 2) {
	    sorder->a1 = unit->id;
	} else {
	    return NULL;
	}
	return rest;
    } else {
	notify(side, "Condition argument \"%s\" not recognized", arg);
	return NULL;
    }
}

/* Collect the next whitespace-separated argument. */

char *
get_next_arg(str, buf, rsltp)
char *str, *buf, **rsltp;
{
    char *p;

    strcpy(buf, str);
    p = buf;
    /* Skip past any leading whitespace. */
    while (isspace(*p))
      ++p;
    if (*p == '"') {
	++p;
	*rsltp = p;
	while (*p != '"' && *p != '\0')
	  ++p;
	*p = '\0';
	/* Increment so later scanning looks past the closing quote. */ 
	++p;
    } else {
	*rsltp = p;
	while (!isspace(*p) && *p != '\0')
	  ++p;
	*p = '\0';
    }
    return str + (p - buf);
}

/* Agreement handling here for now. */

Agreement *agreementlist = NULL;

Agreement *lastagreement;

int numagreements = 0;

char *agreementdesigbuf;

void
init_agreements()
{
    agreementlist = lastagreement = NULL;
    numagreements = 0;
}

Agreement *
create_agreement(id)
int id;
{
    Agreement *ag = (Agreement *) xmalloc(sizeof(Agreement));

    ag->id = id;
    ag->terms = lispnil;
    ag->next = NULL;
    if (agreementlist != NULL) {
	lastagreement->next = ag;
    } else {
	agreementlist = lastagreement = ag;
    }
    ++numagreements;
    return ag;
}

char *
agreement_desig(ag)
Agreement *ag;
{
    if (agreementdesigbuf == NULL)
      agreementdesigbuf = xmalloc(BUFSIZE);
    sprintf(agreementdesigbuf, "<ag %s %s %s>",
	    (ag->typename ? ag->typename : "(null)"),
	    (ag->name ? ag->name : "(null)"),
	    (ag->terms == lispnil ? "(no terms)" : "...terms..."));
    return agreementdesigbuf;
}

#ifdef DESIGNERS

static void paint_view_1 PARAMS ((int x, int y));

static int tmptview;
static int tmpuview;

/* Paint the side's view with given values. */

void
paint_view(side, x, y, r, tview, uview)
Side *side;
int x, y, r, tview, uview;
{
    tmpside = side;
    tmptview = tview;
    tmpuview = uview;
    apply_to_area_plus_edge(x, y, r, paint_view_1);
}

static void
paint_view_1(x, y)
int x, y;
{
    int oldtview = terrain_view(tmpside, x, y);
    int olduview = unit_view(tmpside, x, y);

    if (oldtview != tmptview || olduview != tmpuview) {
	set_terrain_view(tmpside, x, y, tmptview);
	set_unit_view(tmpside, x, y, tmpuview);
	see_exact(tmpside, x, y);
    }
}

#endif /* DESIGNERS */
