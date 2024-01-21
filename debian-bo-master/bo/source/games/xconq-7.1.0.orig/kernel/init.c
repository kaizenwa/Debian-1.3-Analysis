/* General Xconq initialization.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Initialization is complicated, because Xconq needs lots of setup for
   maps, units, sides, and the like.  The data must also be able to come
   from saved games, scenarios, bare maps in files, type definitions,
   or be synthesized if necessary. */

/* The general theory is that everything gets set to a known empty state,
   then all files are read, then all the synthesis methods get run.
   Files/readers and synth methods are each responsible for preventing
   fatal conflicts. */

#include "conq.h"
extern void add_library_path PARAMS ((char *path));
extern void prealloc_debug PARAMS ((void));
#include "version.h"

static void cache_possible_types PARAMS ((void));
static void trial_assign_player PARAMS ((int i, Player *player));
static void init_view_2 PARAMS ((Side *side, int terrainset));
static void init_view_cell PARAMS ((int x, int y));
static void maybe_init_view_cell PARAMS ((int x, int y));
static int adj_seen_terrain PARAMS ((int x, int y, Side *side));

/* This is true when the set of types has been defined.  Subsequently only
   scenarios based on those types can be loaded. */

int typesdefined = FALSE;

/* This is true after the game is totally synthesized, but players may
   not all be in the game yet. */

int gamedefined = FALSE;

/* These are handy tmp vars, usually used with function args. */

short tmputype;
short tmpmtype;
short tmpttype;

/* The array of player/side matchups. */

Assign *assignments = NULL;

/* The list of pathnames to library directories. */

LibraryPath *xconq_libs;

LibraryPath *last_user_xconq_lib;

/* The name of the default game module. */

char *standard_game_name;

/* This is the main string printing buffer.  It is statically allocated
   because it is used even before the main data structures are inited. */

char spbuf[BUFSIZE];

/* This is an auxiliary buffer.  Also needs to be statically allocated. */

char tmpbuf[BUFSIZE];

/* This is true if the interface was requested to put up a new game
   dialog (kind of a random file for this, but *is* init-related). */

int option_popup_new_game_dialog;

#ifdef DESIGNERS

/* This is true if all sides with displays should become designers
   automatically. */

int allbedesigners = FALSE;

/* This is the current count of how many sides are designers. */

int numdesigners = 0;

#endif /* DESIGNERS */

int max_zoc_range;

short *u_possible;

/* The table of all available synthesis methods. */

struct synthtype {
    int key;
    int (*fn) PARAMS ((int calls, int runs));
    int dflt;
    int calls;
    int runs;
} synthmethods[] = {
    { K_MAKE_FRACTAL_PTILE_TERRAIN, make_fractal_terrain, TRUE, 0, 0 },
    { K_MAKE_RANDOM_TERRAIN, make_random_terrain, FALSE, 0, 0 },
    { K_MAKE_EARTHLIKE_TERRAIN, make_earthlike_terrain, FALSE, 0, 0 },
    { K_MAKE_MAZE_TERRAIN, make_maze_terrain, FALSE, 0, 0 },
    { K_MAKE_RIVERS, make_rivers, TRUE, 0, 0 },
    { K_MAKE_COUNTRIES, make_countries, TRUE, 0, 0 },
    { K_MAKE_INDEPENDENT_UNITS, make_independent_units, TRUE, 0, 0 },
    { K_MAKE_INITIAL_MATERIALS, make_initial_materials, TRUE, 0, 0 },
    { K_NAME_UNITS_RANDOMLY, name_units_randomly, TRUE, 0, 0 },
    { K_NAME_GEOGRAPHICAL_FEATURES, name_geographical_features, TRUE, 0, 0 },
    { K_MAKE_ROADS, make_roads, TRUE, 0, 0 },
    { K_MAKE_WEATHER, make_weather, TRUE, 0, 0 },
    { K_MAKE_RANDOM_DATE, make_random_date, FALSE, 0, 0 },
    { -1, NULL, 0, 0, 0 }
};

/* Allow environment vars to override compiled-in library and game. */

void
init_library_path(path)
char *path;
{
    char *xconqlib;

    xconqlib = getenv("XCONQLIB");
    if (empty_string(xconqlib)) {
	if (!empty_string(path)) {
	    xconqlib = path;
	} else {
	    xconqlib = default_library_filename();
	}
    }
    xconq_libs = (LibraryPath *) xmalloc(sizeof(LibraryPath));
    xconq_libs->path = copy_string(xconqlib);
    last_user_xconq_lib = NULL;
    standard_game_name = getenv("XCONQSTANDARDGAME");
    if (empty_string(standard_game_name))
      standard_game_name = STANDARD_GAME;
}

void
add_library_path(path)
char *path;
{
    LibraryPath *lib;

    /* A NULL path indicates that all the existing paths should go away. */
    if (path == NULL) {
	xconq_libs = last_user_xconq_lib = NULL;
	return;
    }
    lib = (LibraryPath *) xmalloc(sizeof(LibraryPath));
    lib->path = copy_string(path);
    /* Insert this after the previous user-specified library, if there
       was any, but before the default library places. */
    if (last_user_xconq_lib != NULL) {
	lib->next = last_user_xconq_lib->next;
	last_user_xconq_lib->next = lib;
    } else {
	lib->next = xconq_libs;
	xconq_libs = lib;
    }
    last_user_xconq_lib = lib;
}

/* This is a general init that prepares data structures to be filled in
   by a game definition.  It should run *before* command line parsing. */

void
init_data_structures()
{
    init_xrandom(-1);
    init_lisp();
    init_types();
    init_globals();
    init_namers();
    init_world();
    init_history();
    init_sides();
    init_agreements();
    init_units();
    init_nlang();
    init_help();
    init_scorekeepers();
    init_actions();
    init_tasks();
    init_ai_types();
    init_write();
}

/* Build the default list of synthesis methods. */

void
set_g_synth_methods_default()
{
    int i;
    Obj *synthlist = lispnil, *synthlistend = lispnil, *tmp;

    for (i = 0; synthmethods[i].key >= 0; ++i) {
	if (synthmethods[i].dflt) {
	    tmp = cons(intern_symbol(keyword_name(synthmethods[i].key)),
		       lispnil);
	    if (synthlist == lispnil) {
		synthlist = synthlistend = tmp;
	    } else {
		set_cdr(synthlistend, tmp);
		synthlistend = tmp;
	    }
	}
    }
    /* Now alter the global variable to contain this list. */
    set_g_synth_methods(synthlist);
}

/* Run a doublecheck on plausibility of game parameters.  Additional
   checks are performed elsewhere as needed, for instance during random
   generation.  Serious mistakes exit now, since they can cause all sorts
   of strange behavior and core dumps.  It's a little more friendly to only
   exit at the end of the tests, so all the mistakes can be found at once. */

/* In theory, if a game passes these tests, then Xconq will never crash. */

/* (This should always be run, might find errors in compiled game.) */

void
check_game_validity()
{
    int failed = FALSE, movers = FALSE, actors = FALSE;
    int u1, u2, m1, t1, t2;

    /* We must have at least one kind of unit. */
    if (numutypes < 1) {
	init_error("no unit types have been defined");
	failed = TRUE;
    }
    /* OK not to have any types of materials. */
    /* We must have at least one kind of terrain. */
    if (numttypes < 1) {
	init_error("no terrain types have been defined");
	failed = TRUE;
    }
    /* Make sure inter-country distances relate correctly. */
    if (g_separation_min() >= 0
	&& g_separation_max() >= 0
	&& !(g_separation_min() <= g_separation_max())) {
	init_warning("country separations %d to %d screwed up",
		     g_separation_min(), g_separation_max());
    }

    /* (Need more general game checks.) */

    max_zoc_range = -1;
    /* Check that all the unit names and chars are distinct. */
    for_all_unit_types(u1) {
	for_all_unit_types(u2) {
	    /* Only do "upper triangle" of utype x utype matrix. */
	    if (u1 < u2) {
		if (strcmp(u_type_name(u1), u_type_name(u2)) == 0) {
		    init_warning(
		     "unit types %d and %d are both named \"%s\"",
				 u1, u2, u_type_name(u1));
		    /* This is bad but not disastrous, so don't fail. */
		}
	    }
	    if (uu_zoc_range(u1, u2) > max_zoc_range) {
		max_zoc_range = uu_zoc_range(u1, u2);
	    }
	}
    }
    /* Large ZOC ranges are not implemented. */
    if (max_zoc_range > 1) {
	init_warning("ZOC range goes up to %d, may be very inefficient", max_zoc_range);
    }
    /* (Eventually check material types also.) */
    /* Check that all terrain names and chars are distinct. */
    for_all_terrain_types(t1) {
	for_all_terrain_types(t2) {
	    /* Only do "upper triangle" of ttype x ttype matrix. */
	    if (t1 < t2) {
		if (strcmp(t_type_name(t1), t_type_name(t2)) == 0) {
		    init_warning(
		     "terrain types %d and %d are both named \"%s\"",
		     t1, t2, t_type_name(t1));
		    /* This is bad but not disastrous, so don't fail. */
		}
		/* Should check that colors are different from each other
		   and from builtin colors? */
	    }
	}
    }
    /* Check various unit type properties. */
    for_all_unit_types(u1) {
	/* Can't make use of this yet, so error out if anybody tries. */
	if (u_available(u1) != 1) {
	    init_error("unit type %d must always be available", u1);
	    failed = TRUE;
	}
	/* Can't make use of this yet, so error out if anybody tries. */
	if (u_action_priority(u1) != 0) {
	    init_error("unit type %d cannot have a nonzero action priority", u1);
	    failed = TRUE;
	}
	/* should be part of general bounds check */
	if (u_cp(u1) <= 0) {
	    init_error("unit type %d has nonpositive cp", u1);
	    failed = TRUE;
	}
	if (u_hp(u1) <= 0) {
	    init_error("unit type %d has nonpositive hp", u1);
	    failed = TRUE;
	}
	if (u_parts(u1) <= 0) {
	    init_error("unit type %d has nonpositive part count.", u1);
	    failed = TRUE;
	}
	if (u_parts(u1) > 0 && u_hp(u1) % u_parts(u1) != 0) {
	    init_error("unit type %d hp not a multiple of its parts.", u1);
	    failed = TRUE;
	}
	if (u_speed(u1) > 0) {
	    movers = TRUE;
	}
	if (u_acp(u1) > 0) {
	    actors = TRUE;
	}
    }
    /* Check various material type properties. */
    for_all_material_types(m1) {
	/* Can't make use of this yet, so error out if anybody tries. */
	if (m_available(m1) != 1) {
	    init_error("material type %d not always available", m1);
	    failed = TRUE;
	}
    }
    /* Check various terrain type properties. */
    for_all_terrain_types(t1) {
	/* Can't make use of this yet, so error out if anybody tries. */
	if (t_available(t1) != 1) {
	    init_error("terrain type %d not always available", t1);
	    failed = TRUE;
	}
    }
    /* If nothing can move and nothing can build, this will probably be
       a really dull game, but there may be such games, do don't say
       anything normally. */
    if (numutypes > 0 && !actors) {
	Dprintf("No actors have been defined.\n");
    }
    if (numutypes > 0 && !movers) {
	Dprintf("No movers have been defined.\n");
    }
    if (numttypes > 0 && numcelltypes == 0) {
	init_error("no terrain type has been allowed for cells");
	failed = TRUE;
    }
    /* This is a really bad game definition, leave before we crash.  This would
       only be executed on systems where init_error doesn't exit immediately. */
    if (failed) {
	exit(0);
    }
    Dprintf("Finished checking game design.\n");
    Dprintf("It defines %d unit types, %d material types, %d terrain types.\n",
	    numutypes, nummtypes, numttypes);
}

/* Calculate the values of global variables that are used everywhere. */

void
calculate_globals()
{
    /* The game is now completely defined; no further user-specified changes can
       occur. */
    gamedefined = TRUE;
    calculate_world_globals();
    /* This needs to be precalculated instead of as-needed, since range can be
       validly both negative and positive, so no way to distinguish uninitialized. */
    {
	int u1, u2, range;
    	extern int max_detonate_on_approach_range;

	max_detonate_on_approach_range = -1;
	for_all_unit_types(u1) {
	    for_all_unit_types(u2) {
		range = uu_detonate_approach_range(u1, u2);
		max_detonate_on_approach_range = max(range, max_detonate_on_approach_range);
	    }
	}
    }	
}

/* Clean up all the objects and cross-references. */

void
patch_object_references()
{
    int numhere, numoccs;
    Unit *unit, *transport;
    Side *side;
    Obj *utref, *utorig;

    /* Use read-in ids to fill in side slots that point to other objects. */
    for_all_sides(side) {
	if (side->playerid > 0) {
	    side->player = find_player(side->playerid);
	}
	if (side->controlled_by_id > 0) {
	    side->controlled_by = side_n(side->controlled_by_id);
	}
	if (side->self_unit_id > 0) {
	    side->self_unit = find_unit(side->self_unit_id);
	}
    }
    for_all_units(unit) {
	/* It's possible that dead units got read in, so check. */
	if (alive(unit)) {
	    if (unit->transport != NULL) {
		transport = NULL;
		utref = utorig = (Obj *) unit->transport;
		/* For safety's sake, null out the slot. */
		unit->transport = NULL;
		/* We have a Lisp object; use it to identify a particular
		   unit as the transport. */
		if (symbolp(utref) && boundp(utref)) {
		    utref = symbol_value(utref);
		}
		if (numberp(utref)) {
		    transport = find_unit(c_number(utref));
		    if (transport == NULL)
		      init_warning("could not find a transport id %d for %s",
				   c_number(utref), unit_desig(unit));
		} else if (stringp(utref)) {
		    transport = find_unit_by_name(c_string(utref));
		    if (transport == NULL)
		      init_warning("could not find a transport named \"%s\" for %s",
				   c_string(utref), unit_desig(unit));
		} else {
		    /* not a recognized way to refer to a unit */
		    sprintlisp(tmpbuf, utorig);
		    init_warning("could not find transport %s for %s",
				 tmpbuf, unit_desig(unit));
		}
		/* it is important to make sure that unit->x, and unit->y 
		   are negative at this point.  Otherwise, the coverage will
		   be messed up for units put into transports that have not yet
		   been placed.  They will be covered for entering the cell,
		   and again when the transport enters the cell. */
		if (transport != NULL) {
		    /* (also check that this is a valid transport type?) */
		    enter_transport(unit, transport);
		} else {
		    /* (could let the unit enter the cell, or could
		       make it infinitely postponed) */
		}
	    } else {
		/* Check that the unit's location is meaningful. */
		if (!inside_area(unit->x, unit->y)) {
		    if (inside_area(unit->prevx, unit->prevy)) {
			if (can_occupy_cell(unit, unit->prevx, unit->prevy)) {
			    enter_cell(unit, unit->prevx, unit->prevy);
			} else {
			    numhere = numoccs = 0;
			    /* Search this cell for units to enter. */
			    for_all_stack(unit->prevx, unit->prevy, transport) {
				++numhere;
				if (unit->side == transport->side
				    && can_occupy(unit, transport)) {
				    enter_transport(unit, transport);
				    break;
				}
				if (unit->side == transport->side
				    && can_occupy(transport, unit))
				  ++numoccs;
			    }
#if 0
			    /* Try having all the existing units enter the transport. */
			    /* (but doesn't work if only some units should go into transport) */
			    if (!inside_area(unit->x, unit->y) && numoccs == numhere) {
				for_all_stack(unit->prevx, unit->prevy, unit2) {
				    enter_transport(unit2, unit);
				}
				enter_cell(unit, unit->prevx, unit->prevy);
			    }
#endif
			    if (!inside_area(unit->x, unit->y)) {
				init_warning("No room for %s at %d,%d",
					     unit_desig(unit),
					     unit->prevx, unit->prevy);
			    }
			}
			/* This prevents attempts to do a second enter_cell
			   during initialization. */
			unit->prevx = unit->prevy = -1;
		    } else if (unit->prevx == -1 && unit->prevy == -1) {
		    	/* This will be left alone - should have pos filled in later. */
		    } else if (unit->cp >= 0) {
			/* Warn, but only if there's no good reason for the unit to have
			   an offworld position. */
			if (area.fullwidth == 0) {
			    init_warning("%s is at offworld location, left there",
					 unit_desig(unit));
			}
			/* This will make it be a reinforcement. */
			unit->cp = -1;
		    }
		}
	    }
	    /* Make sure that side numbering will use only new numbers. */
	    if (unit->side != NULL) {
		if (unit->number > 0) {
		    (unit->side->counts)[unit->type] =
		      max((unit->side->counts)[unit->type], 1 + unit->number);
		}
	    } else {
		/* Trash the numbers on indep units. */
		unit->number = 0;
	    }
	    if (completed(unit)) {
		init_unit_actorstate(unit, TRUE);
		/* Restore acp that wasn't written out because it was the most normal
		   value. */
		if (unit->act && unit->act->acp < u_acp_min(unit->type))
		  unit->act->acp = u_acp(unit->type);
		/* Restore initacp that wasn't written out because it was the normal
		   value. */
		if (unit->act
		    && unit->act->acp > 0
		    && unit->act->initacp == 0)
		  unit->act->initacp = u_acp(unit->type);
		/* Might already have a plan, leave alone if so. */
		if (unit->plan == NULL) {
		    init_unit_plan(unit);
		}
	    }
	} else {
	    /* Dead units need to be disentangled from anything that might
	       have been done to them.  For instance, a module might include
	       a standard collection of units, but then follow up by removing
	       some of those units, and can do it by setting hp == 0.  We want
	       this to work consistently and reliably. */
	    /* Null this out, any possible unit reference is useless. */
	    unit->transport = NULL;
	    if (inside_area(unit->x, unit->y)) {
		leave_cell(unit);
	    }
	}
    }
}

/* Make up a proposed side/player assignment, creating sides and players
   as necessary.  Lock down any assignments that should not be changed,
   but leave everything to be changed as desired. */

/* (when does locking get done?) */

void
make_trial_assignments()
{
    int i = 0;
    Side *side;
    Player *player;

    /* Fill in the side's predefined default and range of initial advantage. */
    for_all_sides(side) {
    	init_side_advantage(side);
    }
    /* Ensure we have as many sides as will be required. */
    while (numsides < g_sides_min()) {
	make_up_a_side();
    }
    while (numsides < min(numplayers, g_sides_max())) {
	make_up_a_side();
    }
    /* Put in all the sides. */
    for_all_sides(side) {
	assignments[i++].side = side;
    }
    /* If no players have been created, make one that is human-run,
       presumably by the person who started up this program. */
    if (numplayers == 0) {
	add_default_player();
    }
    if (numsides < numplayers) {
	/* We have too many players. */
	init_warning("too many players (%d) for %d sides, ignoring extra",
		     numplayers, numsides);
    }
    /* Make any prespecified assignments. */
    for (i = 0; i < numsides; ++i) {
	if (assignments[i].side != NULL
	    && assignments[i].side->player != NULL
	    && assignments[i].player == NULL) {
	    trial_assign_player(i, assignments[i].side->player);
	    /* (should this assignment be locked?) */
	}
    }
    /* Assign any remaining players. */
    player = playerlist;
    for (i = 0; i < numsides; ++i) {
	if (assignments[i].side != NULL
	    && assignments[i].side->ingame
	    && assignments[i].player == NULL) {
	    for (; player != NULL; player = player->next) {
		if (player->side == NULL) {
		    trial_assign_player(i, player);
		    break;
		}
	    }
	}
    }
    /* Add default players for sides with none (in separate loop so player
       creation doesn't confuse player list traversal above). */
    for (i = 0; i < numsides; ++i) {
	if (assignments[i].side != NULL
	    && assignments[i].player == NULL) {
	    player = add_player();
	    /* Default players are always AIs (at least for now). */
	    player->aitypename = "mplayer";
	    trial_assign_player(i, player);
	}
    }
    for_all_players(player) {
	if (player->side == NULL) {
	    init_warning("Player %s not given a side", player_desig(player));
	}
    }
    /* At this point, we have matching sides and players, ready to be
       rearranged if desired. */
}

static void
trial_assign_player(i, player)
int i;
Player *player;
{
    assignments[i].player = player;
    player->side = assignments[i].side;
    (assignments[i].side)->player = player;
    /* Set the player's advantage to be the side's advantage, if defined. */
    if (assignments[i].player != NULL
	&& assignments[i].player->advantage == 0) {
	assignments[i].player->advantage = assignments[i].side->advantage;
    }
    Dprintf("Tentatively assigned %s to %s%s\n",
	    side_desig(assignments[i].side),
	    player_desig(assignments[i].player),
	    (assignments[i].locked ? " (locked)" : ""));
}

/* Create a random side with default characteristics. */

Side *
make_up_a_side()
{
	extern Obj *side_defaults;

    Side *side = create_side();

    if (side == NULL) {
	run_error("could not create a side");
	return NULL;
    }
    fill_in_side(side, side_defaults, FALSE);
    make_up_side_name(side);
    init_side_advantage(side);
    /* A newly-created side starts out in the game, can drop out later. */
    side->ingame = TRUE;
    Dprintf("Made up a side %s\n", side_desig(side));
    return side;
}

/* If undefined, seed a side's advantage and allowable range from the
   global values. */

void
init_side_advantage(side)
Side *side;
{
    /* Set up the default and range of initial advantages. */
    if (side->advantage == 0)
      side->advantage = g_advantage_default();
    if (side->minadvantage == 0)
      side->minadvantage = g_advantage_min();
    if (side->maxadvantage == 0)
      side->maxadvantage = g_advantage_max();
}

/* Add a side and a player to go with it (used by interfaces). */

int
add_side_and_player()
{
    int n;
    Side *side;
    Player *player;

    side = make_up_a_side();
    if (side == NULL)
      return FALSE;
    n = numsides - 1;
    assignments[n].side = side;
    player = add_player();
    assignments[n].player = player;
    player->side = assignments[n].side;
    (assignments[n].side)->player = player;
    /* Set the player's advantage to be the side's advantage, if not
       already set. */
    if (player->advantage == 0) {
	player->advantage = side->advantage;
    }
    return TRUE;
}

/* This can be used by interfaces to exchange players between one side and
   another. */

int
exchange_players(n, n2)
int n, n2;
{
    int i;
    Player *tmpplayer = assignments[n].player;

    if (n < 0)
      n = 0;
    if (n2 < 0) {
	for (i = n + 1; i <= numsides + n; ++i) {
	    n2 = i % numsides;
	    if (assignments[n2].side && (assignments[n2].side)->ingame)
	      break;
	}
	/* No sides to exchange with, return. */
    	if (i == numsides + n)
	  return -1;
    }
    assignments[n].player = assignments[n2].player;
    assignments[n2].player = tmpplayer;
    /* Doesn't seem like these should be needed, but they are. */
    assignments[n].player->side = assignments[n].side;
    assignments[n].side->player = assignments[n].player;
    assignments[n2].player->side = assignments[n2].side;
    assignments[n2].side->player = assignments[n2].player;
    return n2;
}

int
remove_side_and_player()
{
    /* (how to do this?) */
    /* (would need to renumber sides etc) */
    return FALSE;
}

/* Synthesis methods fill in whatever is not fixed by game modules or by
   the player(s). */

void
run_synth_methods()
{
    int i, methkey, found = FALSE, rslt;
    Obj *synthlist, *methods, *method, *parms;

    synthlist = g_synth_methods();
    Dprintf("Will run syntheses ");
    Dprintlisp(synthlist);
    Dprintf("\n");
    for (methods = synthlist; methods != lispnil; methods = cdr(methods)) {
	method = car(methods);
	if (symbolp(method)) {
	    methkey = keyword_code(c_string(method));
	    for (i = 0; synthmethods[i].key >= 0; ++i) {
		if (methkey == synthmethods[i].key) {
		    found = TRUE;
		    rslt = (*synthmethods[i].fn)(synthmethods[i].calls, synthmethods[i].runs);
		    ++(synthmethods[i].calls);
		    if (rslt)
		      ++(synthmethods[i].runs);
		    break;
		}
	    }
	} else if (consp(method)) {
	    parms = cdr(method);
	    if (stringp(car(method))) {
		/* External program. */
		/* (should format parms, compose outputfile redirection,
		    call program with os.c function, check return code,
		    open and read output file) */
		run_error("No external synth programs yet");
	    }
	}
	if (!found) {
	    sprintlisp(spbuf, method);
	    init_warning("bad synthesis method %s, ignoring", spbuf);
	}
    }
}

int
get_synth_method_uses(methkey, calls, runs)
int methkey, *calls, *runs;
{
    int i;

    for (i = 0; synthmethods[i].key >= 0; ++i) {
	if (methkey == synthmethods[i].key) {
	    *calls = synthmethods[i].calls;
	    *runs = synthmethods[i].runs;
	    return TRUE;
	}
    }
    return FALSE;
}

int
make_weather(calls, runs)
int calls, runs;
{
    int x, y, winddir;
    extern int maxwindforce;	

    if (maxwindforce > 0) {
	if (!winds_defined()) {
	    allocate_area_winds();
	    if (g_wind_mix_range() > 0) {
		/* Make all winds start in the same direction. */
		/* Initial variation will randomize. */
		winddir = random_dir();
		for_all_cells(x, y) {
		    set_wind_at(x, y, winddir,
				t_wind_force_avg(terrain_at(x, y)));
		}
	    } else {
		for_all_cells(x, y) {
		    set_wind_at(x, y, random_dir(),
				t_wind_force_avg(terrain_at(x, y)));
		}
	    }
	}
    }
    return TRUE;
}

/* Set the starting date/time to a random value within a given range. */

int
make_random_date(calls, runs)
int calls, runs;
{
    int n;

    /* Don't bother if the initial date has been set explicitly. */
    if (!empty_string(g_initial_date()))
      return FALSE;
    if (empty_string(g_initial_date_min()))
      return FALSE;
    if (empty_string(g_initial_date_max()))
      return FALSE;
    /* Use the low end of the date range as a starting place for date
       calculations. */
    set_initial_date(g_initial_date_min());
    n = turns_between(g_initial_date_min(), g_initial_date_max());
    /* Now bump the starting date to its random place. */
    set_initial_date(absolute_date_string(xrandom(n) + 1));
    return TRUE;
}

/* The final init cleans up various stuff. */

void
final_init()
{
    Side *side;

    /* Fill in any empty doctrines. */
    for_all_sides_plus_indep(side) {
	init_doctrine(side);
    }
    /* Make sure each side has a self-unit if it needs one. */
    for_all_sides(side) {
	init_self_unit(side);
    }
    /* At this point we should be ready to roll.  Any inconsistencies
       hereafter will be fatal. */
    check_consistency();
    cache_possible_types();
    /* Count the units initially present. */
    init_side_balance();
    /* Fix up garbled view data. */
    init_all_views();
    configure_sides();
    /* Check again, just to be sure. */
    check_consistency();
    create_game_help_nodes();
    /* Set up the scores to be attached to each side. */
    init_scores();
    /* Calculate which random event methods will be run. */
    init_random_events();
    final_init_world();
    /* Start the recording of history. */
    start_history();
#ifdef DEBUGGING
    /* Make sure that any debugging-related allocation is done. */
    if (Debug || DebugG || DebugM)
      prealloc_debug();
    /* Report on memory consumption. */
    Dprintf("One side is %d bytes.\n", sizeof(Side));
    Dprintf("One unit is %d bytes, one plan is %d bytes.\n",
	    sizeof(Unit), sizeof(Plan));
    if (Debug)
      report_malloc();
#endif /* DEBUGGING */
}

/* Load up any player-specified configuration data. */

void
configure_sides()
{
    Side *side;

    for_all_sides(side) {
	load_side_config(side);
    }
}

/* Calculate what each side knows about the world. */

void
init_all_views()
{
    int x, y, i = 0, todo = max(1, numsides * area.numcells);
    int terrainset;
    Side *side;

    /* Snapshot the value here, so is permanently on. */
    all_see_all = g_see_all();
    /* Set up the basic view structures for all sides first. */
    for_all_sides(side) {
	if (all_see_all) {
	    side->see_all = TRUE;
	} else {
	    side->see_all = FALSE;
	    terrainset = init_view(side);
	    init_view_2(side, terrainset);
	}
	/* We're not normally allowed to choose whether to see all or not. */
	side->may_set_see_all = FALSE;
	/* Can't think of any other place to put this... */
	calc_start_xy(side);
    }
    /* Nothing more to do here. */
    if (all_see_all)
      return;
    announce_lengthy_process("Computing current view at each location");
    /* (coverage is also done at this point) */
    for_all_sides(side) {
	for_all_cells(x, y) {
	    ++i;
    	    if (i % 100 == 0)
    	      announce_progress((100 * i) / todo);
	    see_cell(side, x, y);
	}
    }
    finish_lengthy_process();
    init_area_views();
}

static void
init_view_2(side, terrainset)
Side *side;
int terrainset;
{
    int x, y;

    calc_coverage(side);
    tmpside = side;
    if (g_terrain_seen()) {
	for_all_cells(x, y) {
	    init_view_cell(x, y);
	}
    } else {
	for_all_cells(x, y) {
	    if (!terrainset)
	      set_terrain_view(side, x, y, UNSEEN);
	    set_unit_view(side, x, y, EMPTY);
	    /* View date is 0, which is what we want. */
	}
    }
}

/* This is a helper for the following routine.  The helper is applied to
   each cell, decides what is visible in that cell. */

/* This only works from already_seen, does not account for coverage. */

/* (should only be run once/cell/side, use a scratch layer to keep track?) */

static void
init_view_cell(x, y)
int x, y;
{
    int u, chance;
    Unit *unit;

    /* Guaranteed to see the terrain accurately. */
    set_terrain_view(tmpside, x, y, buildtview(terrain_at(x, y)));
    /* If this cell is under observation, don't need to do anything special. */
    if (cover(tmpside, x, y) > 0)
      return;
    /* Scan all the units here to see if any are visible. */
    for_all_stack(x, y, unit) {
    	if (in_play(unit)) {
    	    u = unit->type;
	    if (u_see_always(u)) {
	    	see_exact(tmpside, x, y);
	    	/* or flag unit as spotted? */
		set_cover(tmpside, x, y, 1);
	    	return;
	    }
	    chance = (indep(unit) ? u_already_seen_indep(u)
		      : u_already_seen(u));
	    if (probability(chance)) {
	    	see_exact(tmpside, x, y);
	    	/* This view might be overwritten by a view of
	    	   another might-be-seen unit at this location,
	    	   so don't return just yet. */
	    }
	}
    }
    /* We get to see the weather conditions here. */
    set_wind_view(tmpside, x, y, raw_wind_at(x, y));
}

static void
maybe_init_view_cell(x, y)
int x, y;
{
    int dir, x1, y1;

    if (adj_seen_terrain(x, y, tmpside) && flip_coin()) {
	init_view_cell(x, y);
	for_all_directions(dir) {
	    if (point_in_dir(x, y, dir, &x1, &y1)) {
		init_view_cell(x1, y1);
	    }
	}
    }
}

static int
adj_seen_terrain(x, y, side)
int x, y;
Side *side;
{
    int dir, x1, y1;

    if (!inside_area(x, y) || side == NULL)
      return FALSE;
    for_all_directions(dir) {
	if (point_in_dir(x, y, dir, &x1, &y1)) {
	    if (terrain_view(side, x1, y1) != UNSEEN)
	      return TRUE;
	}
    }
    return FALSE;
}

/* Do ranged initial views from units. */

void
init_area_views()
{
    int rad, x, y, pop, dir, x1, y1, i = 0;
    Unit *unit;
    Side *side, *side2;

    /* Don't run if nothing exists to look at. */
    if (!terrain_defined())
      return;
    /* Skip if everything already known, side creation got these cases. */
    if (g_see_all() || g_terrain_seen())
      return;
    announce_lengthy_process("Computing ranged and people views");
    /* Compute the view for each side. */ 
    for_all_sides(side) {
	/* Set this so the helper fn has a side to use. */
	tmpside = side;
	/* View from our own and other units. */
	for_all_units(unit) {
	    if (trusted_side(unit->side, side)) {
		/* The unit always sees itself. */
		see_exact(side, unit->x, unit->y);
		/* It may also see things nearby. */
		rad = u_seen_radius(unit->type);
		if (rad >= area.maxdim) {
		    /* Special optimization - view the whole area. */
		    for_all_cells(x, y) {
			init_view_cell(x, y);
		    }
		    /* Note that we're not done; other units may be able to
		       supply more exact views of their vicinities than
		       would init_view_cell from a distant unit. */
		} else if (rad >= 0) {
		    apply_to_area(unit->x, unit->y, rad, init_view_cell);
		}
	    }
	}
	/* The people see everything in the cells that they are in, plus the
	   normally visible things in adjacent cells. */
	if (people_sides_defined()) {
	    for_all_interior_cells(x, y) {
		pop = people_side_at(x, y);
		side2 = side_n(pop);
		if (pop != NOBODY && trusted_side(side2, side)) {
		    see_exact(side, x, y);
		    for_all_directions(dir) {
			if (point_in_dir(x, y, dir, &x1, &y1)) {
			    init_view_cell(x1, y1);
			}
		    }
		}
	    }
	}
	if (side->finalradius > 0) {
	    /* (should also view terrain adj to each of these cells, since the
	       viewing represents exploration) */
	    apply_to_ring(side->startx, side->starty,
			  1, side->finalradius - 1,
			  init_view_cell);
	    apply_to_ring(side->startx, side->starty,
			  side->finalradius - 2, side->finalradius + 2,
			  maybe_init_view_cell);
	}
	announce_progress((100 * i++) / numsides);
    }
    finish_lengthy_process();
}

/* Method to give all units and terrain a basic stockpile of supply. */

int
make_initial_materials(calls, runs)
int calls, runs;
{
    int m, t, amts[MAXTTYPES], doany, x, y;

    /* Go over each material and terrain type, looking for nonzero
       material in terrain possibilities, then alloc and fill in layers
       as needed. */
    for_all_material_types(m) {
	doany = FALSE;
	for_all_terrain_types(t) {
	    amts[t] = min(tm_storage_x(t, m), tm_initial(t, m));
	    if (amts[t] > 0)
	      doany = TRUE;
	}
	if (doany) {
	    allocate_area_material(m);
	    for_all_cells(x, y) {
		t = terrain_at(x, y);
		set_material_at(x, y, m, amts[t]);
	    }
	}
    }
    return TRUE;
}

/* Give the unit what it is declared to have stockpiled
   at the start of a game. */

void
init_supply(unit)
Unit *unit;
{
    int m, u = unit->type;

    for_all_material_types(m) {
	unit->supply[m] = min(um_storage_x(u, m), um_initial(u, m));
    }
}

/* Quicky test needed in a couple places. */

int
saved_game()
{
    FILE *fp;

    fp = fopen(saved_game_filename(), "r");
    if (fp != NULL) {
	fclose(fp);
	return TRUE;
    } else {
	return FALSE;
    }
}

/* Count all the initial units in each side's balance sheet, but only
   if all the balance sheets are completely blank. */

void
init_side_balance()
{
    int u, anything = FALSE;
    Unit *unit;
    Side *side;

    for_all_sides(side) {
	for_all_unit_types(u) {
	    if (total_gain(side, u) > 0)
	      return;
	}
    }
    for_all_units(unit) {
	count_gain(unit->side, unit->type, initial_gain);
    }
}

/* This routine does a set of checks to make sure that Xconq's data
   is in a valid state.  This is particularly important after init,
   since the combination of files and synthesis methods may have
   caused some sort of disaster. */

void
check_consistency()
{
    int x, y;

    /* If no terrain, make a flat area of all ttype 0. */
    if (!terrain_defined()) {
	init_warning("No terrain defined, substituting type 0");
	allocate_area_terrain();
	for_all_cells(x, y) {
	    if (inside_area(x, y)) {
		set_terrain_at(x, y, 0);
	    }
	}
	add_edge_terrain();
    }
    if (numsides <= 0) {
	init_error("There are no player sides at all in this game");
    } else if (numsides < numplayers) {
	init_warning("Only made %d of the %d sides requested",
		     numsides, numplayers);
    }
    /* Need any kind of unit checks? */
}

static void
cache_possible_types()
{
    int u, u2, rescan;
    Unit *unit;

    u_possible = (short *) xmalloc(numutypes * sizeof(short));
    for_all_units(unit) {
	u_possible[unit->type] = TRUE;
    }
    rescan = TRUE;
    while (rescan) {
	rescan = FALSE;
	for_all_unit_types(u) {
	    if (u_possible[u]) {
		for_all_unit_types(u2) {
		    if (uu_acp_to_create(u, u2) > 0
			&& !u_possible[u2]) {
			u_possible[u2] = TRUE;
			rescan = TRUE;
		    }
		}
	    }
	}
    }
}

/* This does the actual assignment of players to sides, and initializes the
   side structures appropriately. */

void
assign_players_to_sides()
{
    int i, numdisplays = 0, numdisplayswanted = 0, numais = 0, n = 0;
    Side *side;
    Player *player;

    announce_lengthy_process("Assigning players to sides");
    for (i = 0; i < numsides; ++i) {
	announce_progress((100 * i) / numsides);
	side = assignments[i].side;
	player = assignments[i].player;
	canonicalize_player(player);
	/* Fix any mistaken advantages. */
	/* This is a warning here because properly-done interfaces shouldn't
	   allow any mistaken advantages to get this far. */
	if (player->advantage < side->minadvantage) {
	    init_warning("Requested advantage of %d for %s is too low, will be set to %d",
			 player->advantage, player_desig(player), side->minadvantage);
	    player->advantage = side->minadvantage;
	}
	if (player->advantage > side->maxadvantage) {
	    init_warning("Requested advantage of %d for %s is too high, will be set to %d",
			 player->advantage, player_desig(player), side->maxadvantage);
	    player->advantage = side->maxadvantage;
	}
	/* Call the interface code to initialize the side's display, if
	   it wants to use one (the interface has to decide). */
	if (side_wants_display(side)) {
	    ++numdisplayswanted;
	    init_ui(side);
	    if (side_has_display(side)) {
		++numdisplays;
	    }
	}
	/* Count the desired AIs, for setup below. */
	if (side_wants_ai(side)) {
	    ++numais;
	}
	Dprintf("Assigned %s to %s\n",
		 side_desig(side), player_desig(player));
    }
    finish_lengthy_process();
    if (numdisplays < numdisplayswanted) {
	if (numdisplays < 1) {
	    init_warning("None of the %d requested displays opened",
			 numdisplayswanted);
	} else {
	    init_warning("Only %d of %d requested displays opened",
			 numdisplays, numdisplayswanted);
	}
    } else if (numdisplays == 0) {
	init_warning("Need at least one display to run");
    }
#ifdef DESIGNERS
    /* Make each displayed side into a designer if it was requested. */
    if (allbedesigners) {
    	for_all_sides(side) {
	    if (side_has_display(side)) {
		become_designer(side);
	    }
    	}
    }
#endif /* DESIGNERS */
    if (numais > 0) {
	announce_lengthy_process("Setting up AIs");
	for (i = 0; i < numsides; ++i) {
	    if (numais > 1)
	      announce_progress((100 * n++) / numais);
	    side = assignments[i].side;
	    if (side_wants_ai(side)) {
		init_ai(side);
	    }
	}
	finish_lengthy_process();
    }
}

/* Make sure any debugging I/O routines have allocated their space.
   Usually routines like side_desig allocate their working space on
   demand, but if the first demand occurs during a game being saved
   because allocation fails, then we're in big trouble.  So this
   routine, which need only be be called when debugging is turned
   on, calls allocation-needing things in such a way to cause them
   to allocate their working space. */

void
prealloc_debug()
{
    side_desig(NULL);
    player_desig(NULL);
    unit_desig(NULL);
    shortest_unique_name(0);
}

/* Return the program version. */

char *
version_string()
{
    return VERSION;
}

/* Return the copyright notice. */

char *
copyright_string()
{
    return COPYRIGHT;
}

/* Return the license string. */

char *
license_string()
{
    return "\
Xconq is free software and you are welcome to distribute copies of it\n\
under certain conditions; type \"o copying\" to see the conditions.\n\
There is absolutely no warranty for Xconq; type \"o warranty\" for details.\n\
";
}

/* This comment is a fake reference to K_NO_X, which is a keyword used
   to clear the subtype-x property of terrain, but is not actually
   mentioned in the code - besides here anyway. :-) */
