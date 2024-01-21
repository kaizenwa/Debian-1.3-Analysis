/* The main simulation-running code in Xconq.
   Copyright (C) 1986, 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This is the main simulation-running code. */

#include "conq.h"
extern void all_see_cell_weather PARAMS ((int x, int y));
extern int stop_apply;
extern int execute_standing_order PARAMS ((Unit *unit, int addtask));
extern void add_to_unit_hp PARAMS ((Unit *unit, int hp));
extern void try_sharing PARAMS ((Unit *from, Unit *to, int m));

static void run_turn_start PARAMS ((void));
static void run_restored_turn_start PARAMS ((void));
static void add_new_sides_to_game PARAMS ((void));
static void init_movement PARAMS ((void));
static void init_actionvectors PARAMS ((void));
static void run_tech_leakage PARAMS ((void));
static void cache_init_tech_levels PARAMS ((void));
static void run_tooling_attrition PARAMS ((void));
static void reset_all_reserves PARAMS ((void));
static void compute_moves PARAMS ((void));
static void compose_actionvectors PARAMS ((void));
static int move_some_units PARAMS ((int lim));
static int side_move_some_units PARAMS ((Side *side, int lim));
static int unit_still_acting PARAMS ((Unit *unit, int checkwaiting));
static int move_one_unit_multiple PARAMS ((Unit *unit, int lim));
static void finish_movement PARAMS ((void));
static void test_agreements PARAMS ((void));
static void compute_sun PARAMS ((void));
static void run_sun PARAMS ((void));
static void compute_season PARAMS ((void));
static void run_environment PARAMS ((void));
static void mix_winds PARAMS ((void));
static void run_spies PARAMS ((void));
static void run_random_events PARAMS ((void));
static int init_accidents PARAMS ((void));
static void run_accidents PARAMS ((void));
static int init_attrition PARAMS ((void));
static void run_attrition PARAMS ((void));
static int init_revolts PARAMS ((void));
static void run_revolts PARAMS ((void));
static void unit_revolt PARAMS ((Unit *unit));
static int init_surrenders PARAMS ((void));
static void run_surrenders PARAMS ((void));
static void unit_surrender PARAMS ((Unit *unit));
static void maybe_surrender_to PARAMS ((Unit *unit, Unit *unit2));
static int excess_left PARAMS ((int x, int y));
static void try_transfer_to_cell PARAMS ((int x, int y));
static int sharable_left PARAMS ((int x, int y));
static void try_sharing_with_cell PARAMS ((int x, int y));
static void run_economy PARAMS ((void));
static int base_production PARAMS ((Unit *unit, int m));
static void try_transfer PARAMS ((Unit *from, Unit *to, int r));
static void try_transfer_aux PARAMS ((Unit *from, Unit *to, int r));
static int can_satisfy_need PARAMS ((Unit *unit, int r, int need));
static void run_turn_end PARAMS ((void));
static void run_people_consumption PARAMS ((void));
static void run_cell_consumption PARAMS ((void));
static void run_unit_base_consumption PARAMS ((void));
static void unit_consumes PARAMS ((Unit *unit));
static int in_supply PARAMS ((Unit *unit, int m));
static void run_self_builds PARAMS ((void));
static void run_environment_effects PARAMS ((void));
static void damage_unit_with_temperature PARAMS ((Unit *unit, int n));
static void run_people_side_changes PARAMS ((void));
static void update_cell_display_all_sides PARAMS ((int x, int y, int rightnow));
static void run_appearances PARAMS ((void));
static void run_disappearances PARAMS ((void));
static void run_hp_recovery PARAMS ((void));
static void run_auto_repair PARAMS ((void));
static void auto_repair_from_here PARAMS ((int x, int y));
static void auto_repair_unit PARAMS ((Unit *unit, Unit *unit2));
static int season_effect PARAMS ((int u));
static void run_detonation_accidents PARAMS ((void));
static void run_people_limits PARAMS ((void));
static void maybe_detonate_accidently PARAMS ((Unit *unit));
static void spy_on_location PARAMS ((int x, int y));

/* The number of the current turn within a year. */

int curyearpart = -1;

/* The season name for the current turn. */

char *curseasonname = NULL;

/* Priority of sides that are now moving. */

int curpriority;

/* Priority of units that are now moving. */

int curunitpriority;

/* The table of all types of random events. */

struct randomeventtype {
    int key;
    int (*initfn) PARAMS ((void));
    void (*fn) PARAMS ((void));
} randomeventmethods[] = {
    { K_ACCIDENTS_IN_TERRAIN, init_accidents, run_accidents },
    { K_ATTRITION_IN_TERRAIN, init_attrition, run_attrition },
    { K_UNITS_REVOLT, init_revolts, run_revolts },
    { K_UNITS_SURRENDER, init_surrenders, run_surrenders },
    { 0, NULL, NULL }
};

int numrandomevents;

int randomeventindices[10]; /* this must be >= number of diff methods */

int maintimeout = -1;

int paused = FALSE;

/* State variables. */
/* (I don't think all of these are strictly necessary) */

/* This becomes TRUE the first time run_game is executed. */

int gameinited = FALSE;

/* This is true only before the game actually starts. */

int beforestart = TRUE;

/* This is true only at the beginning of a turn. */

int at_turn_start = FALSE;

/* This is true after the game is over. */

int endofgame = FALSE;

/* How often to do saves while playing. */

int checkpointinterval = 0;

/* This is set FALSE whenever the game state changes, and TRUE whenever
   the game has been saved. */

int gamestatesafe = TRUE;

/* This is TRUE after the designer has been mucking around, or if
   networked versions are found to be inconsistent. */

int compromised = FALSE;

/* The number of new sides that have been requested to be added. */

int new_sides_requested;

char **players_requested;

/* True whenever the game has both day and night. */

int daynight = FALSE;

/* The location of the sun, as a position relative to the area.  The
   actual values may be far outside the area. */

int sunx, suny;

/* The sun's previous location. */

int lastsunx = -1, lastsuny = -1;

/* The time at which the game actually starts. */

time_t game_start_in_real_time;

/* The point in the turn at which players can actually do things. */

time_t turn_play_start_in_real_time;

int planexecs;

int taskexecs;

/* The rate at which AIs play when acting more slowly (so as
   not to overwhelm human players), expressed as plan executions
   per minute.  0 means "as fast as possible". */

int slow_play_rate = 240;

/* The rate at which AIs play when acting more quickly (such
   as when all humans are done with their moves). */

int fast_play_rate = 0;

/* The current rate at which AIs are playing. */

int current_play_rate;

/* Flags indicating whether various sets of calculations need to be done. */

int any_tooling_attrition = -1;

int any_self_builds = -1;

int any_appearances = -1;

int any_people_side_changes = -1;

int *any_people_surrenders = NULL;

int any_hp_recovery = -1;

int any_auto_repair = -1;

int any_tech_leakage = -1;

int any_detonation_accidents = -1;

int any_unit_production = -1;

int any_terrain_production = -1;

int any_people_production = -1;

int any_people_consumption = -1;

int any_cell_consumption = -1;

int any_unit_base_consumption = -1;

int any_people_max = -1;

int any_spying = -1;

int any_disappearances = -1;

static short *will_be_auto_repaired;

static short *auto_repair_range_max;

/* This function does a (small, usually) amount of simulation, then returns.
   It can be run multiple times at any time, will not go "too far".
   It returns the number of actions that were actually performed. Other
   important state changes (such a side finishing its turn or the turn
   ending) are also counted as actions, so that this function's callers
   will know that something was done. */

int
run_game(maxactions)
int maxactions;
{
    int numacted, runtime;
    time_t rungamestart, rungameend;
    Side *side;

    gameinited = TRUE;
    time(&rungamestart);
    numacted = 0;
    if (beforestart) {
	/* If we haven't started yet, see if it's time. */
	test_for_game_start();
	Dprintf("run_game: tested for game start.\n");
    } else if (endofgame) {
	/* Nothing to do except wait for users to do exit commands. */
    	Dprintf("run_game: at end of game.\n");
    } else if (paused) {
	/* Don't do anything if we're paused. */
    	Dprintf("run_game: paused.\n");
    } else {
	if (at_turn_start) {
	    if (midturnrestore)
	      run_restored_turn_start();
	    else
	      run_turn_start();
	    check_all_units();
	    compose_actionvectors();
	    init_movement();
	    update_all_progress_displays("", -1);
	    /* Game might have been ended by new turn init. */
	    if (endofgame) {
	    	Dprintf("run_game: game ended by new turn init.\n");
	    	return 0;
	    }
	    /* (should adjust this by recorded elapsed turn time) */
	    time(&turn_play_start_in_real_time);
	    at_turn_start = FALSE;
	    /* Count turn start as an action. */
	    ++numacted;
	}
	for_all_sides(side) {
	    /* If this is running in realtime, update all clock displays. */
	    if (side->ingame && side_has_display(side) && realtime_game()) {
		update_clock_display(side, TRUE);
	    }
	    /* Non-participating sides are automatically "finished". */
	    if (side->ingame
	        && !side->finishedturn
		&& !side_has_ai(side)
		&& !side_has_display(side)) {
		Dprintf("run_game: %s finished - no AI or display.\n",
			side_desig(side));
		finish_turn(side);
		/* Count this as an action. */
		++numacted;
	    }
	}
	/* If all sides are done acting, end the turn.  This won't be true
	   right at the start of a turn. */
	if (all_sides_finished() || exceeded_rt_per_turn()) {
	    run_turn_end();
	    Dprintf("run_game: at turn end.\n");
	    at_turn_start = TRUE;
	    /* Count this as an action. */
	    ++numacted;
	} else {
	    /* Move some units around. */
	    numacted += move_some_units(maxactions);
	    /* Possibly finish some sides' turns. */
	    for_all_sides(side) {
		if (!side->finishedturn) {
		    if (side_has_ai(side)) {
			ai_analyze_after_moves(side, numacted);
		    }
		    /* Display and/or AI might have vanished? */
		    if (!side_has_ai(side) && !side_has_display(side)) {
		    	Dprintf("run_game: %s finished - AI and/or display gone.\n",
				side_desig(side));
			finish_turn(side);
			/* Count this as an action. */
			++numacted;
		    }
		    /* See if any sides auto-finish. */
		    if (!units_still_acting(side)
			&& side->autofinish
#ifdef DESIGNERS
			&& !side->designer
#endif /* DESIGNERS */
			) {
		    	Dprintf("run_game: %s auto-finishes.\n", side_desig(side));
			finish_turn(side);
			/* Count this as an action. */
			++numacted;
		    }
		}
	    }
	    if (Debug) {
	    	Dprintf("run_game: %d/%d actions.", numacted, maxactions);
		if (planexecs > 0)
		  Dprintf(" (%d plan execs)", planexecs);
		if (taskexecs > 0)
		  Dprintf(" (%d task execs)", taskexecs);
		/* (also number of units considered?) */
	    	Dprintf("\n");
	    }
	}
	check_realtime();
	test_for_game_end();
    }
    time(&rungameend);
    runtime = idifftime(rungameend, rungamestart);
    if (runtime > 0)
      Dprintf("run_game: took %d seconds\n", runtime);
    return numacted;
}

/* See if game is ready to get underway for real.  Note that displays will
   work before the game has started, but game time doesn't move. */

void
test_for_game_start()
{
    int anydisplays = FALSE;
    Side *side;

    /* We must have at least one unit on a side that is being displayed
       before the game can start for real. */
    for_all_sides(side) {
	if (side_has_display(side)) {
	    anydisplays = TRUE;
	}
	if (side_has_units(side) && side_has_display(side)) {
	    /* Now we're really ready to roll. */
	    beforestart = FALSE;
	    at_turn_start = TRUE;
	    if (midturnrestore) {
		record_event(H_GAME_RESTARTED, ALLSIDES);
	    } else {
		record_event(H_GAME_STARTED, ALLSIDES);
		set_g_elapsed_time(0);
	    }
	    /* Record the game as starting NOW in real time. */
	    time(&game_start_in_real_time);
	    /* Adjust by any recorded elapsed time. */
	    game_start_in_real_time -= g_elapsed_time();
	    /* No need to look at any more sides, just get on with the game. */
	    return;
	}
    }
    if (!anydisplays) {
	init_warning("No sides have a display");
    }
}

/* This routine looks to see if the game is completely over. */

void
test_for_game_end()
{
    Side *side;

    /* Declare a draw if everybody is amenable. */
    if (all_others_willing_to_quit(NULL)) {
    	/* (should remove all sides first?) */
	end_the_game();
    }
    for_all_sides(side) {
    	/* If we have an active side being displayed, we're not done yet. */
	if (side->ingame && side_has_display(side))
	  return;
	/* (If no displayed sides have units, turns will whiz by) */
    }
    end_the_game();
}

/* This is true when all participating sides have finished their turn. */

int
all_sides_finished()
{
    Side *side;

    for_all_sides(side) {
	if (side->ingame
	    && !side->finishedturn) {
	    return FALSE;
	}
    }
    return TRUE;
}

/* This is true when AIs should move more slowly. */

int
all_human_only_sides_finished()
{
    Side *side;

    for_all_sides(side) {
	if (side->ingame
	    && side_has_display(side)
	    && !side_has_ai(side)
	    && !side->finishedturn) {
	    return FALSE;
	}
    }
    return TRUE;
}

/* Call this from interfaces to check on realtime details without actually
   going into run_game.  Will call back to interface if necessary. */

void
check_realtime()
{
    Side *side;

    if (!realtime_game())
      return;
    if (exceeded_rt_for_game())
      end_the_game();
    if (g_rt_per_side() > 0) {
	for_all_sides(side) {
	    if (side->ingame && side->totaltimeused > g_rt_per_side()) {
		remove_side_from_game(side);
	    }
	}
    }
}

int
exceeded_rt_for_game()
{
    time_t now;

    if (g_rt_for_game() <= 0)
      return FALSE;
    time(&now);
    /* Note that the game start time is already adjusted for any
       elapsed time recorded when the game was last saved. */
    return (idifftime(now, game_start_in_real_time) > g_rt_for_game());
}

int
exceeded_rt_per_turn()
{
    time_t now;

    if (g_rt_per_turn() <= 0)
      return FALSE;
    time(&now);
    return (idifftime(now, turn_play_start_in_real_time) > g_rt_per_turn());
}

/* This returns true if the given side is still wanting to do stuff. */

int
units_still_acting(side)
Side *side;
{
    int curactor;
    Unit *unit;

    if (!side->ingame)
      return FALSE;
    for (curactor = 0; curactor < side->actionvector->numunits; ++curactor) {
	unit = (side->actionvector->units)[curactor].unit;
	if (unit_still_acting(unit, FALSE) && side_controls_unit(side, unit)) {
	    return TRUE;
	}
    }
    return FALSE;
}

/* Do everything that would happen before movement in a turn. */

static void
run_turn_start()
{
    int curturn;
    time_t turncalcstart, turncalcend;
    Side *side;

    /* Increment the turn number. */
    curturn = g_turn();
    ++curturn;
    set_g_turn(curturn);
    /* See if we've hit the preset end of the game. */
    if (curturn > g_last_turn() && !probability(g_extra_turn())) {
	end_the_game();
	/* The game is over, don't bother with the other calcs. */
	return;
    }
    add_new_sides_to_game();
    time(&turncalcstart);
    update_all_progress_displays("turn start calcs", -1);
    compute_season();
    Dprintf("##### TURN %d (%s) #####\n",
	    curturn, absolute_date_string(curturn));
    for_all_sides(side) {
	side->finishedturn = FALSE;
	update_turn_display(side, TRUE);
	if (realtime_game()) {
	    update_clock_display(side, TRUE);
	}
    }
    run_sun();
    run_environment();
    run_economy();
    run_hp_recovery();
    run_auto_repair();
    run_self_builds();
    run_appearances();
    run_random_events();
    run_detonation_accidents();
    sort_units();
    init_actionvectors();
    compute_moves();
    run_spies();
    run_tech_leakage();
    run_tooling_attrition();
    cache_init_tech_levels();
    reset_all_reserves();
    gamestatesafe = FALSE;
    if ((checkpointinterval > 0) && (curturn % checkpointinterval == 0)) {
	write_entire_game_state(checkpoint_filename());
    }
    time(&turncalcend);
    Dprintf("%d seconds to calc at turn start\n",
	    idifftime(turncalcend, turncalcstart));
}

/* Do computations to start the first turn of a restored game. */

static void
run_restored_turn_start()
{
    Side *side;

    Dprintf("##### TURN %d (%s) #####\n",
	    g_turn(), absolute_date_string(g_turn()));
    for_all_sides(side) {
	update_turn_display(side, TRUE);
	if (realtime_game()) {
	    update_clock_display(side, TRUE);
	}
    }
    compute_sun();
    sort_units();
    init_actionvectors();
    /* We're done with restore-specific tweaks, turn the flag off. */
    midturnrestore = FALSE;
}

static void
init_actionvectors()
{
    Side *side;

    for_all_sides_plus_indep(side) {
	if (side->actionvector == NULL)
	  side->actionvector = make_unit_vector(max(numunits, 100));
	clear_unit_vector(side->actionvector);
    }
}

/* Interfaces should call this to have another side added to the current
   game.  The actual addition happens during turn setup, so as not to
   risk confusing list traversals or AI calculations. */

/* (should take a player spec as arg?) */

int
request_additional_side(playerspec)
char *playerspec;
{
    if (numsides + new_sides_requested + 1 <= g_sides_max()) {
	if (players_requested == NULL)
	  players_requested = (char **) xmalloc(MAXSIDES * sizeof(char *));
	if (empty_string(playerspec))
	  playerspec = ",ai";
	players_requested[new_sides_requested++] = copy_string(playerspec);
	notify_all("Will add a new side (player \"%s\") at the start of the next turn",
		   playerspec);
	return TRUE;
    } else {
	notify_all("Additional side requested, but not possible to add");
	return FALSE;
    }
}

/* Add the requested number of new sides into an ongoing game. */

static void
add_new_sides_to_game()
{
    int i;
    Side *side, *side2;
    Player *player;
    Unit *unit;

    if (new_sides_requested > 0) {
	for (i = 0; i < new_sides_requested; ++i) {
	    if (numsides >= g_sides_max())
	      break;
	    /* Grow side-referencing objects. */
	    for_all_units(unit) {
		if (unit->opinions != NULL) {
		    init_unit_opinions(unit, numsides + 1);
		}
	    }
	    side = make_up_a_side();
	    player = add_player();
	    parse_player_spec(player, players_requested[i]);
	    player->side = side;
	    side->player = player;
	    /* Set the player's advantage to be the side's advantage, if not
	       already set. */
	    if (player->advantage == 0) {
		player->advantage = side->advantage;
	    }
	    run_synth_methods();
	    init_doctrine(side);
	    init_self_unit(side);
	    if (g_use_side_priority()) {
		int maxpri = 0;

		for_all_sides(side2) {
		    if (side2->priority > maxpri)
		      maxpri = side2->priority;
		}
		side->priority = maxpri + 1;
		/* If the indepside's priority was set automatically to be
		   one up from regular sides', bump it up to be past the new
		   side's priority also. */
		if (indepside->priority == side->priority)
		  ++(indepside->priority);
	    }
	    /* Count all of the new side's units as initial gain. */
	    for_all_side_units(side, unit) {
		count_gain(side, unit->type, initial_gain);
	    }
	    if (side_wants_ai(side)) {
		init_ai(side);
	    }
	    for_all_sides(side2) {
		/* Give all other AIs a chance to re-evaluate the situation. */
		if (side_has_ai(side2) && side2 != side) {
		    ai_react_to_new_side(side2, side);
		}
		/* Add the new side to displays. */
		update_side_display(side2, side, TRUE);
		notify(side2, "A new side %s (played by %s) is in the game.",
		       side_desig(side), player_desig(player));
	    }
	}
	if (i > 0) {
	    /* Recalculate all view info for all sides; simpler than
	       trying to figure out what's really changed. */
	    reset_coverage();
	    reset_all_views();
	    compute_all_feature_centroids();
	    /* Redraw everything that's displayed. */
	    update_everything();
	}
	if (i < new_sides_requested) {
	    notify_all("Cannot create %d of the requested new sides",
		       new_sides_requested - i);
	}
    }
    /* We've handled everything we're going to, reset the counter. */
    new_sides_requested = 0;
}

/* Parse the syntax "[username][,ai][/config][@display][+advantage]". */

void
parse_player_spec(player, spec)
Player *player;
char *spec;
{
    int commapos, slashpos, atpos, pluspos;

    if (spec != NULL && strcmp(spec, "*") != 0) {
	/* Extract (destructively) a trailing advantage specification. */
	pluspos = iindex('+', spec);
	if (pluspos >= 0) {
	    player->advantage = max(1, atoi(&(spec[pluspos + 1])));
	    spec[pluspos] = '\0';
	}
	/* Extract a displayname if given. */
	atpos = iindex('@', spec);
	if (atpos >= 0) {
	    player->displayname = copy_string(spec + atpos + 1);
	    spec[atpos] = '\0';
	}
	/* Extract a configuration name if given. */
	slashpos = iindex('/', spec);
	if (slashpos >= 0) {
	    player->configname = copy_string(spec + slashpos + 1);
	    spec[slashpos] = '\0';
	}
	/* Extract an AI type if given. */
	commapos = iindex(',', spec);
	if (commapos >= 0) {
	    player->aitypename = copy_string(spec + commapos + 1);
	    spec[commapos] = '\0';
	}
	/* Just a plain old string left. */
	if (strlen(spec) > 0) {
	    if (atpos >= 0) {
		/* Display given separately, so this is a name. */
		player->name = copy_string(spec);
	    } else {
		player->displayname = copy_string(spec);
	    }
	}
    }
    canonicalize_player(player);
}

static void
init_movement()
{
    int i;
    Side *side, *side2;

    i = 1;
    curpriority = 9999;
    curunitpriority = 0;
    /* In the absence of any idea about how to handle actions by independent units,
       force their turn to be finished immediately. */
    indepside->finishedturn = TRUE;
    for_all_sides(side) {
	if (side->ingame) {
	    /* Record that this side was active during at least one turn. */
	    side->everingame = TRUE;
	    /* No units are waiting for orders initially. */
	    side->numwaiting = 0;
	}
	side->turnstarttime = time(0);
	/* Didn't really do input, but useful to pretend so. */
	side->lasttime = time(0);
	/* Calculate side priorities; do here so future versions can
	   set priorities dynamically. */
	if (g_use_side_priority()) {
	    if (side->priority < 0) {
		side->priority = i++;
	    }
	}
	if (side_has_ai(side))
	  ai_init_turn(side);
	side->busy = FALSE;
	if (side_has_display(side))
	  update_action_display(side, TRUE);
    }
    /* Set independent units to move after units on sides. */
    if (g_use_side_priority()) {
	if (indepside->priority < 0) {
	    indepside->priority = i;
	}
	for_all_sides_plus_indep(side) {
	    if (!side->finishedturn && side->priority < curpriority)
	      curpriority = side->priority;
	}
    }
    /* Inform sides with displays that all units are ready to act. */
    for_all_sides(side) {
	if (side_has_display(side)) {
	    for_all_sides(side2) {
		update_side_display(side, side2, TRUE);
	    }
	}
    }
}

/* Compute the leakage of technology from one side to another. */

static void
run_tech_leakage()
{
    int u;
    Side *side, *side2;

    if (any_tech_leakage < 0) {
	any_tech_leakage = FALSE;
	for_all_unit_types(u) {
	    if (u_tech_leakage(u) > 0) {
		any_tech_leakage = TRUE;
		break;
	    }
	}
    }
    if (!any_tech_leakage)
      return;
    Dprintf("Running tech leakage\n");
    for_all_sides(side) {
	for_all_sides(side2) {
	    if (side != side2 /* and some contact between sides */) {
		for_all_unit_types(u) {
		    if (side->tech[u] < side2->tech[u]
			&& u_tech_leakage(u) > 0) {
			side->tech[u] += prob_fraction(u_tech_leakage(u));
		    }
		}
	    }
	}
    }
}

/* Remember each side's tech levels before it does any research actions
   during the turn.  This can be used to keep tech level from going up too
   fast if the player has lots of units doing research. */

static void
cache_init_tech_levels()
{
    int u;
    Side *side;

    if (using_tech_levels()) {
	for_all_sides(side) {
	    for_all_unit_types(u) {
		side->inittech[u] = side->tech[u];
	    }
	}
    }
}

/* Reduce some units' construction tooling randomly. */

static void
run_tooling_attrition()
{
    int u, u2, att;
    Unit *unit;

    /* Test whether tooling attrition is ever possible. */
    if (any_tooling_attrition < 0) {
	any_tooling_attrition = FALSE;
	for_all_unit_types(u) {
	    for_all_unit_types(u2) {
		if (uu_tp_attrition(u, u2) > 0) {
		    any_tooling_attrition = TRUE;
		    break;
		}
	    }
	    if (any_tooling_attrition)
	      break;
	}
    }
    if (!any_tooling_attrition)
      return;
    for_all_units(unit) {
	if (is_active(unit) && unit->tooling != NULL) {
	    for_all_unit_types(u2) {
		att = uu_tp_attrition(unit->type, u2);
		if (att > 0) {
		    unit->tooling[u2] -= prob_fraction(att);
		}
		if (unit->tooling[u2] < 0) unit->tooling[u2] = 0;
	    }
	}
    }
}

static void
reset_all_reserves()
{
    Unit *unit;

    for_all_units(unit) {
	if (unit->plan != NULL) {
	    unit->plan->reserve = FALSE;
	}
    }
}

/* Compute moves and actions for all the units at once, put everybody that
   can do anything into a list. */

static void
compute_moves()
{
    int curturn = g_turn();
    Unit *unit;
    Side *side;

    for_all_sides(side) {
	side->numacting = 0;
	side->numfinished = 0;
	for_all_side_units(side, unit) {
	    if (unit->act) {
		/* Unit acp is set to -1 to indicate uninitialization,
		   but acp is computed by adding to the previous acp,
		   so when starting a new game (as opposed to
		   restoring an old one), acp should be inited to
		   zero.  (This could maybe be done better.) */
		if (curturn == 1)
		  unit->act->acp = 0;
		compute_acp(unit);
		update_unit_acp_display(side, unit, FALSE);
	    }
	}
    }
}

static void
compose_actionvectors()
{
    int priority;
    Unit *unit;
    Side *side;

    for_all_sides_plus_indep(side) {
	for_all_side_units(side, unit) {
	    if (unit->act && unit->act->initacp > 0) {
		priority = unit_priority(unit);
		if (between(curunitpriority, priority, curunitpriority + 99)) {
		    side->actionvector = add_unit_to_vector(side->actionvector, unit, 0);
		    /* Clear all delay flags. */
		    if (unit->plan)
		      unit->plan->delayed = FALSE;
		}
	    }
	    if (unit->plan) {
		unit->plan->execs_this_turn = 0;
	    }
	}
	/* (should sort vector by unit priority) */
	Dprintf("Action vector for %s has %d units, at priority %d\n",
		side_desig(side), side->actionvector->numunits, curunitpriority);
    }
}

int
unit_priority(unit)
Unit *unit;
{
    /* (should look for individual unit priority hook also) */
    return u_action_priority(unit->type);
}

/* Compute the action points available to the unit this turn. */

void
compute_acp(unit)
Unit *unit;
{
    int u = unit->type, t, acp, tempeff, maxacp, minacp, err;
    Unit *occ;

    /* Units still under construction or off-area can't do anything. */
    if (!completed(unit) || !inside_area(unit->x, unit->y)) {
	unit->act->initacp = unit->act->acp = unit->act->actualactions = 0;
	return;
    }
    /* First compute how many action points are available. */
    /* Start with basic acp, normal or damaged as appropriate. */
    /* (Should fix - small multi-part unit would be considered damaged) */
    if (unit->hp < u_hp_max(u) && u_acp_damage_effect(u) != lispnil) {
	acp = damaged_acp(unit, u_acp_damage_effect(u));
    } else {
	acp = u_acp(u);
    }
    /* Adjust for occupants. */
    for_all_occupants(unit, occ) {
	if (is_active(occ)) {
	    acp = (acp * uu_acp_occ_effect(occ->type, u)) / 100;
	}
    }
    /* Adjust for night time. */
    if (night_at(unit->x, unit->y)) {
    	/* (should account for unit being on a road at night, etc) */
	t = terrain_at(unit->x, unit->y);
    	acp = (acp * ut_acp_night_effect(u, t)) / 100;
    }
    /* Adjust for temperature. */
    if (temperatures_defined() && u_acp_temp_effect(u) != lispnil) {
	err = interpolate_in_list(temperature_at(unit->x, unit->y), u_acp_temp_effect(u), &tempeff);
	if (err == 0) {
	    acp = (acp * tempeff) / 100;
	} else {
	    /* (should complain?) */
	}
    }
    /* Adjust for season. */
    if (u_acp_season_effect(u) != lispnil) {
    	acp = (acp * season_effect(u)) / 100;
    }
    /* Clip to upper and lower acp-per-turn limits. */
    acp = max(acp, u_acp_turn_min(u));
    if (u_acp_turn_max(u) >= 0)
      acp = min(acp, u_acp_turn_max(u));
    /* Increment the unit's available acp by the acp we get for this turn. */
    unit->act->initacp = unit->act->acp + acp;
    /* Now clip the unit's accumulated acp to its limits. */
    minacp = u_acp_min(u);
    unit->act->initacp = max(unit->act->initacp, minacp);
    maxacp = (u_acp_max(u) < 0 ? acp : u_acp_max(u));
    unit->act->initacp = min(unit->act->initacp, maxacp);
    /* Current acp is now the initial acp. */
    unit->act->acp = unit->act->initacp;
    /* Zero the counts of what actually got done. */
    unit->act->actualactions = unit->act->actualmoves = 0;
}

/* Compute and return the acp of a damaged unit, using a list of (hp acp) pairs
   and interpolating between them. */

int
damaged_acp(unit, effect)
Unit *unit;
Obj *effect;
{
    int u, err, rslt;

    u = unit->type;
    err = interpolate_in_list_ext(unit->hp, effect, 0, 0, 0, 0, u_hp(u), u_acp(u), &rslt);
    if (err != 0) {
	run_warning("cannot get damaged acp for %s at hp %d, using %d",
		    u_type_name(u), u_acp(u));
	rslt = u_acp(u);
    }
    return rslt;
}

static void
compute_season()
{
    Obj *names, *rest, *elt;

    curseasonname = NULL;
    if (world.yearlength > 1) {
	curyearpart = (g_turn() + g_initial_year_part()) % world.yearlength;
	/* Determine the name of the season, if defined. */
	names = g_season_names();
	if (names != NULL && names != lispnil && consp(names)) {
	    for (rest = names; rest != lispnil; rest = cdr(rest)) {
		elt = car(rest);
		if (consp(elt)
		    && numberp(car(elt))
		    && numberp(cadr(elt))
		    && between(c_number(car(elt)), curyearpart, c_number(cadr(elt)))
		    && stringp(car(cddr(elt))))
		  curseasonname = c_string(car(cddr(elt)));
	    }
	}
    } else {
	curyearpart = 0;
    }
}

static int
season_effect(u)
int u;
{
    int err, rslt;

    if (curyearpart < 0)
      compute_season();
    err = interpolate_in_list(curyearpart, u_acp_season_effect(u), &rslt);
    if (err != 0) {
	rslt = 100;
    }
    return rslt;
}

/* Do some number of actions. */

static int
move_some_units(lim)
int lim;
{
    int num = 0, sidenum;
    Side *side;

    for_all_sides_plus_indep(side) {
	if ((g_use_side_priority() ?
	     curpriority == side->priority :
	     TRUE)) {
	    sidenum = side_move_some_units(side, lim);
	    num = max(num, sidenum);
	}
    }
    return num;
}

/* Do some number of actions. */

static int
side_move_some_units(side, lim)
Side *side;
int lim;
{
    int num, foundanytomove, curactor, numdelayed;
    Unit *unit;

    num = 0;
  tryagain:
    foundanytomove = FALSE;
    numdelayed = 0;
    for (curactor = 0; curactor < side->actionvector->numunits; ++curactor) {
	unit = (side->actionvector->units)[curactor].unit;
	if (unit->plan && unit->plan->delayed) {
	    ++numdelayed;
	    continue;
	}
	current_play_rate = slow_play_rate;
	/* AIs should play as fast as possible if turns are sequential or if the
	   human players are all done. */
	if (g_use_side_priority() || all_human_only_sides_finished()) {
	    current_play_rate = fast_play_rate;
	}
	/* If the unit is keeping formation, then give it a chance to
	   adjust its position, even if it's not "still acting". */
	if (is_active(unit)
	    && (unit->side ?
		(unit->side->ingame && !unit->side->finishedturn) : TRUE)
	    && (unit->act && unit->act->acp > 0)
	    && (unit->plan && unit->plan->formation)) {
	    num += move_one_unit_multiple(unit, lim - num);
	    foundanytomove = TRUE;
	}
	if (unit->side
	    && unit->side->orders
	    && unit->plan
	    && unit->plan->tasks == NULL
	    && execute_standing_order(unit, FALSE)) {
	    /* We're not waiting because standing order execution will
	       shortly be allowed to fill in a task for real. */
	    unit->plan->waitingfortasks = FALSE;
	    num += move_one_unit_multiple(unit, lim - num);
	    foundanytomove = TRUE;
	}
	if (unit_still_acting(unit, TRUE)) {
	    num += move_one_unit_multiple(unit, lim - num);
	    foundanytomove = TRUE;
	}
	if (unit_still_acting(unit, FALSE)) {
	    foundanytomove = TRUE;
	}
	if (num >= lim) 
	  return num;
    }
    if (!foundanytomove && numdelayed > 0) {
	for (curactor = 0; curactor < side->actionvector->numunits; ++curactor) {
	    unit = (side->actionvector->units)[curactor].unit;
	    if (unit->plan)
	      unit->plan->delayed = FALSE;
	}
	goto tryagain;
    }
    if (!foundanytomove && 0 /* not at max priority */) {
	/* should recompose action vector for new priority? */
    }
    return num;
}

static int
unit_still_acting(unit, checkwaiting)
Unit *unit;
int checkwaiting;
{
    return (is_active(unit)
	    && (unit->side
		&& unit->side->ingame
		&& !unit->side->finishedturn)
	    && (unit->act
		&& unit->act->acp > 0)
	    && ((unit->plan
		&& !unit->plan->asleep
		&& !unit->plan->reserve
		&& (checkwaiting ? !unit->plan->waitingfortasks : TRUE))
		|| has_pending_action(unit)));
}

/* Do a single unit's actions, up to the given limit or until it runs
   out of things it wants to do (or something happens to it). */

int lastexecution = 0;

static int
move_one_unit_multiple(unit, lim)
Unit *unit;
int lim;
{
    int num = 0, buzz = 0, acp1;
    int rslt;

    if (unit->act == NULL || unit->act->initacp < 1)
      return 0;
    acp1 = unit->act->acp;
    while (is_active(unit)
	   && (unit->act
	       && unit->act->acp > u_acp_min(unit->type))
	   && ((unit->plan
		&& !unit->plan->asleep
		&& !unit->plan->reserve
		&& !unit->plan->delayed)
	       || has_pending_action(unit))
	   && num < lim
	   && buzz < lim) {
	if (has_pending_action(unit)) {
	    /* Execute the action directly. */
	    rslt = execute_action(unit, &(unit->act->nextaction));
	    /* Clear the action.  Note that the unit might have changed
	       to a non-acting type, so we have to check for act struct. */
	    if (unit->act)
	      unit->act->nextaction.type = ACTION_NONE;
	    /* In any case, the game state is irrevocably altered. */
	    gamestatesafe = FALSE;
	    ++num;
	} else if (unit->plan) {
	    if (unit->side
		&& unit->side->orders
		&& unit->plan->tasks == NULL
		&& execute_standing_order(unit, TRUE)) {
		execute_plan(unit, 1);
		gamestatesafe = FALSE;
		++buzz;
	    }
	    if (unit->plan->formation && move_into_formation(unit)) {
		execute_plan(unit, 1);
		gamestatesafe = FALSE;
		++buzz;
	    }
	    if (unit->plan->waitingfortasks
		|| unit->plan->asleep
		|| unit->plan->reserve
		|| unit->plan->delayed)
	      break;
	    if (unit->side
		&& side_has_ai(unit->side)
		&& current_play_rate > 0
		&& !n_ms_elapsed(60000 / current_play_rate)
		)
	      break;
	    execute_plan(unit, 1);
	    record_ms();
	    gamestatesafe = FALSE;
	    ++buzz;
	} else {
	    run_warning("Planless \"%s\" was asked to act", unit_desig(unit));
	    ++buzz;
	}
	/* This should never happen. */
	if (unit->act && unit->act->acp == acp1 && num > 1) {
	    /* Blast the action. */
	    unit->act->nextaction.type = ACTION_NONE;
	    /* Blast the plan. */
	    if (unit->plan)
	      unit->plan->type = PLAN_NONE;
	    if (unit->plan && probability(5))
	      unit->plan->asleep = TRUE;
	    run_warning("%s was confused, resetting plans", unit_desig(unit));
	}
    }
    return num;
}

/* This explicitly finishes out a side's activity for the turn. */

void
finish_turn(side)
Side *side;
{
    int nextpriority;
    Side *side2, *side3;

    /* Flag the side as being done for this turn. */
    side->finishedturn = TRUE;
    /* Stop counting down our time consumption. */
    side->totaltimeused += (time(0) - side->turnstarttime);
    if (g_use_side_priority()) {
	nextpriority = 9999;
	for_all_sides_plus_indep(side2) {
	    if (!side2->finishedturn
/*		&& side2->priority > curpriority */
		&& side2->priority < nextpriority) {
		nextpriority = side2->priority;
	    }
	    if (!side2->finishedturn && side2->priority < curpriority)
	      run_warning("%s not finished, but priority is %d, less than current %d",
			  side_desig(side2), side2->priority, curpriority);
	}
	if (nextpriority > curpriority)
	  curpriority = nextpriority;
    }
    /* Clue everybody in. */
    if (g_use_side_priority()) {
	/* Several sides may change, if current priority changes. */
	for_all_sides(side2) {
	    for_all_sides(side3) {
		update_side_display(side2, side3, TRUE);
	    }
	}
    } else {
	/* Only the turn-finishing side changes. */
	for_all_sides(side2) {
	    update_side_display(side2, side, TRUE);
	}
    }
    Dprintf("%s finished its turn.\n", side_desig(side));
}

void
set_play_rate(slow, fast)
int slow, fast;
{
    if (slow < 0 || fast < 0 || fast < slow) {
	run_warning("Bad play rates slow=%d fast=%d, ignoring", slow, fast);
	return;
    }
    slow_play_rate = slow;
    fast_play_rate = fast;
}

/* Take care of details that no longer require any interaction, at least
   none that can't wait until the next turn. */

static void
finish_movement()
{
    int lostacp;
    Unit *unit;
    Side *side, *side2;

    for_all_sides (side) {
	if (Debug) {
	    lostacp = 0;
	    for_all_side_units(side, unit) {
		if (is_active(unit) && unit->act && unit->act->acp > 0) {
		    lostacp += unit->act->acp;
		}
	    }
	    if (lostacp > 0) {
		Dprintf("%s forfeited %d acp overall.\n",
			side_desig(side), lostacp);
	    }
	}
	if (side_has_ai(side)) {
	    ai_finish_movement(side);
	}
    }
    for_all_sides(side)  {
	for_all_sides(side2) {
	    update_side_display(side, side2, TRUE);
	}
    }
}

/* See how any agreements' terms are holding up. */

static void
test_agreements()
{
    Agreement *ag;

    for_all_agreements(ag) {
	if (ag->state == in_force) {
		/* what? */
	}
    }
}

/* Compute sun-related data. */

static void
compute_sun()
{
    int curtime, highest;

    switch (world.daylength) {
      case 0:
	/* Sun is at a fixed position. */
	daynight = TRUE;
	/* (should be possible to set explicitly somehow? implicitly from lat/long?) */
	sunx = area.width / 2;  suny = area.halfheight;
	break;
      case 1:
	/* No sun effects at all, every place uniformly lit. */
      	daynight = FALSE;
	break;
      default:
	/* Normal days and nights. */
	daynight = TRUE;
	/* If world has a appropriate circumference, the sun moves over
	   it at a regular pace. */
	if (world.circumference >= area.width) {
	    lastsunx = sunx;  lastsuny = suny;
	    curtime = (g_turn() * 100 + g_initial_day_part()) % (world.daylength * 100);
	    sunx = (curtime * world.circumference) / (world.daylength * 100) + area.width / 2;
	    /* Adjust for the world's axialtilt.  The approximation is basically a triangle
	       wave instead of a more correct sinusoid. */
	    highest = ((world.circumference / 4) * world.axialtilt) / 90;
	    if (curtime < ((world.daylength * 100) / 2))
	      suny = area.halfheight - highest + (curtime * 2 * highest) / ((world.daylength * 100) / 2);
	    else
	      suny = area.halfheight + highest - ((curtime - ((world.daylength * 100) / 2)) * 2 * highest) / ((world.daylength * 100) / 2);
	    suny -= area.latitude;
	}
	break;
    }
    if (daynight) {
	Dprintf("Sun is now at %d,%d\n", sunx, suny);
    }
}
/* Compute the position of the sun for this turn. */

static void
run_sun()
{
    int x, y;
    Side *side;

    compute_sun();
    if (world.daylength > 1 && world.circumference >= area.width) {
	/* Update the appearance of any cells whose lighting has changed. */
	/* Note that we do this from top to bottom, purely for esthetic
	   reasons. */
	for (y = area.height - 1; y >= 0; --y) {
	    for (x = 0; x < area.width; ++x) {
		if (in_area(x, y)) {
		    if (lighting(x, y, sunx, suny) != lighting(x, y, lastsunx, lastsuny)) {
			for_all_sides(side) {
			    update_cell_display(side, x, y, FALSE);
			}
		    }
	    	}
	    }
	}
    }
}

int any_annual_temp_change = -1;

static int num_key_points;

struct a_key_point {
    int x, y;
    int temp;
} *key_points;

void calc_key_point_temps PARAMS ((int yearpart));

int interpolate_temperature PARAMS ((int x, int y));

void
calc_key_point_temps(yearpart)
int yearpart;
{
    int i, err, rslt;
    Obj *lis, *item, *loc;

    num_key_points = length(g_temp_year());
    if (num_key_points > 0 && key_points == NULL) {
	key_points =
	  (struct a_key_point *) xmalloc(num_key_points * sizeof(struct a_key_point));
    }
    i = 0;
    for (lis = g_temp_year(); lis != lispnil; lis = cdr(lis)) {
	item = car(lis);
	loc = car(item);
	if (consp(loc)) {
	    key_points[i].x = c_number(car(loc));
	    key_points[i].y = c_number(car(cdr(loc)));
	}
	err = interpolate_in_list(yearpart, cdr(item), &rslt);
	if (err != 0) {
	    /* (should complain) */
	    rslt = 0;
	}
	key_points[i].temp = rslt;
	++i;
    }
}

int
interpolate_temperature(x, y)
int x, y;
{
    int d0, d1, t0, t1, i, besti, nextbesti, d;

    if (num_key_points == 1) {
	return key_points[0].temp;
    } else if (num_key_points == 2) {
	d0 = distance(x, y, key_points[0].x, key_points[0].y);
	t0 = key_points[0].temp;
	d1 = distance(x, y, key_points[1].x, key_points[1].y);
	t1 = key_points[1].temp;
	return ((t0 * d1 + t1 * d0) / (d0 + d1));
    } else if (num_key_points > 2) {
	besti = 0;
	nextbesti = -1;
	d0 = distance(x, y, key_points[besti].x, key_points[besti].y);
	for (i = 2; i < num_key_points; ++i) {
	    d = distance(x, y, key_points[i].x, key_points[i].y);
	    if (d < d0) {
		nextbesti = besti;
		d1 = d0;
		besti = i;
		d0 = distance(x, y, key_points[besti].x, key_points[besti].y);
	    } else if ((nextbesti < 0 || d < d1) && i != besti) {
		nextbesti = i;
		d1 = distance(x, y, key_points[nextbesti].x, key_points[nextbesti].y);
	    }
	}
	d1 = distance(x, y, key_points[nextbesti].x, key_points[nextbesti].y);
	t0 = key_points[besti].temp;
	t1 = key_points[nextbesti].temp;
	return ((t0 * d1 + t1 * d0) / (d0 + d1));
    } else {
	run_error("???");
	return 0;
    }
}

/* Compute environment changes. */

static void
run_environment()
{
    int yrlen = world.yearlength, x, y, dir, t, celltemp;
    int anychanges = FALSE;

    if (mintemp == maxtemp && !any_wind_variation_in_layer)
      return;
    if (mintemp != maxtemp && !temperatures_defined()) {
        allocate_area_temperatures();
        allocate_area_scratch(3);
    }
    if (any_annual_temp_change < 0) {
	any_annual_temp_change = FALSE;
	if (yrlen > 0 && g_temp_year() != lispnil) {
	    any_annual_temp_change = TRUE;
	}
    }
    if (any_annual_temp_change) {
	calc_key_point_temps(curyearpart);
    }
    if (any_wind_variation_in_layer && !winds_defined()) {
        allocate_area_winds();
        allocate_area_scratch(2);
    }
    /* The tmp1 record will record where any changes occur. */
    for_all_cells(x, y) {
	set_tmp1_at(x, y, FALSE);
    }
    if (mintemp != maxtemp) {
	/* Compute the average temperature at each point in the world. */
	for (y = area.height - 1; y >= 0; --y) {
	    for (x = 0; x < area.width; ++x) {
		if (!in_area(x, y))
		  continue;
		/* Save the prev temp. */
		set_tmp2_at(x, y, temperature_at(x, y));
		t = terrain_at(x, y);
		if (any_annual_temp_change)
		  celltemp = interpolate_temperature(x, y);
		else
		  celltemp = t_temp_avg(t);
		/* Add in a random variation if specified. */
		if (t_temp_variability(t) > 0) {
		    celltemp += (xrandom(t_temp_variability(t))
				 - t_temp_variability(t)/2);
		}
		/* Higher elevations can be much colder. */
		/* (In this pos, will influence lowlands via moderation -
		   realistic?) */
		if (elevations_defined()
		    && g_temp_floor_elev() != 0
		    && elev_at(x, y) < g_temp_floor_elev()) {
		    celltemp -=
		      ((celltemp - g_temp_floor()) * elev_at(x, y))
			/ g_temp_floor_elev();
		}
		/* Clip to terrain type's limits. */
		if (celltemp < t_temp_min(t))
		  celltemp = t_temp_min(t);
		if (celltemp > t_temp_max(t))
		  celltemp = t_temp_max(t);
		/* Record the (unmoderated) temperature of the cell. */
		set_temperature_at(x, y, celltemp);
	    }
	}
	/* Sometimes the scale of the world is such that neighboring cells
	   influence each other's temperatures. */
	if (g_temp_mod_range() > 0) {
	    /* only doing a range of 1... */
	    for_all_interior_cells(x, y) {
		set_tmp3_at(x, y, temperature_at(x, y));
		for_all_directions(dir) {
		    set_tmp3_at(x, y,
				(tmp3_at(x, y)
				 + temperature_at(x+dirx[dir], y+diry[dir])));
		}
	    }
	    for_all_interior_cells(x, y) {
		celltemp = tmp3_at(x, y) / (NUMDIRS + 1);
		t = terrain_at(x, y);
		/* Clip to terrain type's limits. */
		if (celltemp < t_temp_min(t))
		  celltemp = t_temp_min(t);
		if (celltemp > t_temp_max(t))
		  celltemp = t_temp_max(t);
		set_temperature_at(x, y, celltemp);
	    }
	}
 	for_all_cells(x, y) {
	    if (temperature_at(x, y) != tmp2_at(x, y))
	      set_tmp1_at(x, y, TRUE);
	    anychanges = TRUE;
	}
    }
    /* Do wind changes. */
    if (any_wind_variation_in_layer) {
	/* Save the previous state. */
	for_all_interior_cells(x, y) {
	    set_tmp2_at(x, y, raw_wind_at(x, y));
	}
	for_all_interior_cells(x, y) {
	    int winddir = wind_dir_at(x, y);
	    int windforce = wind_force_at(x, y);
	    int t = terrain_at(x, y);
	    int anychange;

	    anychange = FALSE;
	    if (probability(t_wind_variability(t))) {
		winddir = (flip_coin() ? right_dir(winddir) : left_dir(winddir));
		anychange = TRUE;
	    }
	    if (probability(t_wind_force_variability(t))) {	
		windforce += (flip_coin() ? 1 : -1);
		windforce = max(windforce, t_wind_force_min(t));
		windforce = min(windforce, t_wind_force_max(t));
		anychange = TRUE;
	    }
	    if (anychange) {
		set_wind_at(x, y, winddir, windforce);
	    }
	}
	if (g_wind_mix_range() > 0) {
	    mix_winds();
	}
	for_all_cells(x, y) {
	    if (raw_wind_at(x, y) != tmp2_at(x, y))
	      set_tmp1_at(x, y, TRUE);
	    anychanges = TRUE;
	}
    }
    /* See if any displays should change and report if so. */
    if (anychanges) {
	for_all_cells(x, y) {
	    if (tmp1_at(x, y)) {
		all_see_cell_weather(x, y);
	    }
	}
    }
}

static void
mix_winds()
{
    int num, i, x, y, dir, x1, y1, wdir, wforce, sumx, sumy, n, t;

    num = (area.width * area.height) / 6;

    for (i = 0; i < num; ++i) {
	random_point(&x, &y);
	wdir = wind_dir_at(x, y);
	wforce = wind_force_at(x, y);
	sumx = dirx[wdir] * wforce;  sumy = diry[wdir] * wforce;
	n = 1;
	for_all_directions(dir) {
	    if (point_in_dir(x, y, dir, &x1, &y1)) {
		wdir = wind_dir_at(x1, y1);
		wforce = wind_force_at(x1, y1);
		sumx += dirx[wdir] * wforce;  sumy += diry[wdir] * wforce;
		++n;
	    }
	}
	sumx = sumx / n;  sumy = sumy / n;
	wdir = approx_dir(sumx, sumy);
	wforce = distance(0, 0, sumx, sumy);
	t = terrain_at(x, y);
	wforce = max(wforce, t_wind_force_min(t));
	wforce = min(wforce, t_wind_force_max(t));
	if (wforce < 0)
	  wforce = 0;
	set_wind_at(x, y, wdir, wforce);
    }
}

/* Update all sides with all weather changes that have happened at
   the given location. */

void
all_see_cell_weather(x, y)
int x, y;
{
    int oldview;
    Side *side;

    for_all_sides(side) {
	if (g_see_weather_always()
	    ? (terrain_view(side, x, y) != UNSEEN)
	    : (cover(side, x, y) > 0)) {
	    oldview = temperature_view(side, x, y);
	    if (oldview != temperature_at(x, y)) {
		set_temperature_view(side, x, y, temperature_at(x, y));
		update_cell_display(side, x, y, 34);
	    }
	    /* (should update cloud view here too) */
	    oldview = wind_view(side, x, y);
	    if (oldview != raw_wind_at(x, y)) {
		set_wind_view(side, x, y, raw_wind_at(x, y));
		update_cell_display(side, x, y, 35);
	    }
	}
    }
}

/* Given that the spying unit is going to get info about other units at this
   location, figure out just what it is going to see. */

static void
spy_on_location(x, y)
int x, y;
{
    int qual;
    Unit *unit2, *occ;

    for_all_stack(x, y, unit2) {
    	if (unit2->side != tmpunit->side) {
    	    qual = uu_spy_quality(tmpunit->type, unit2->type);
    	    if (probability(qual)) {
    	    	/* Spy got something, report it. */
    	    	/* (should be more worked-out, dunno exactly how) */
    	    	see_exact(tmpunit->side, x, y);
    	    	for_all_occupants(unit2, occ) {
    	    	    /* (should get info about occupants) */
    	    	}
    	    }
    	}
    }
}

/* Certain kinds of units can do spying for the side they're on. */

static void
run_spies()
{
    int chance;
    Unit *unit;

    if (any_spying < 0) {
	int u;

	any_spying = FALSE;
	for_all_unit_types(u) {
	    if (u_spy_chance(u) > 0) {
		any_spying = TRUE;
		break;
	    }
	}
	if (all_see_all)
	  any_spying = FALSE;  /* override */
    }
    if (!any_spying)
      return;
    Dprintf("Running spies\n");
    for_all_units(unit) {
	if (is_active(unit)) {
	    chance = u_spy_chance(unit->type);
	    if (chance > 0) {
		if (xrandom(10000) < chance) {
		    /* Spying is successful, decide how much was seen. */
		    tmpunit = unit;
		    apply_to_area(unit->x, unit->y, u_spy_range(unit->type),
				  spy_on_location);
		}
	    }
	}
    }
}

/* Figure out ahead of time which random event methods to run. */

void
init_random_events()
{
    int i, k;
    Obj *randomeventlist, *rest, *evttype;

    numrandomevents = 0;
    randomeventlist = g_random_events();
    if (randomeventlist == NULL || randomeventlist == lispnil)
      return;
    for (rest = randomeventlist; rest != lispnil; rest = cdr(rest)) {
	evttype = car(rest);
	if (symbolp(evttype)) {
	    k = keyword_code(c_string(evttype));
	    for (i = 0; randomeventmethods[i].key != 0; ++i) {
		if (k == randomeventmethods[i].key
		    && randomeventmethods[i].fn != NULL
		    && (randomeventmethods[i].initfn == NULL
			|| (*(randomeventmethods[i].initfn))())) {
		    randomeventindices[numrandomevents++] = i;
		}
	    }
	}
    }
}

/* Run the current cache of random event methods. */

static void
run_random_events()
{
    int i;

    if (numrandomevents > 0) {
	Dprintf("Running random events\n");
	for (i = 0; i < numrandomevents; ++i) {
	    (*(randomeventmethods[randomeventindices[i]].fn))();
	}
    }
}

/* Test whether accidents can happen in this game. */

static int
init_accidents()
{
    int u, t;
    
    for_all_unit_types(u) {
    	for_all_terrain_types(t) {
    	    if (ut_accident_vanish(u, t) > 0
		|| ut_accident_hit(u, t) > 0)
	      return TRUE;
    	}
    }
    return FALSE;
}

/* Test each unit that is out in the open to see if a terrain-related
   accident happens to it.  Accidents can either kill the unit instantly or
   just damage it. */

static void
run_accidents()
{
    int t;
    Unit *unit;

    for_all_units(unit) {
	if (in_play(unit) && unit->transport == NULL) {
	    t = terrain_at(unit->x, unit->y);
	    if (xrandom(10000) < ut_accident_vanish(unit->type, t)) {
	    	/* Kill the unit outright. */
		kill_unit(unit, H_UNIT_VANISHED);
		/* should make a hevt */
	    } else if (xrandom(10000) < ut_accident_hit(unit->type, t)) {
		/* Damage the unit. */
		unit->hp2 -= ut_accident_damage(unit->type, t);
		/* (should be able to pass reason to damage_unit) */
		damage_unit(unit, accident_dmg);
	    }
	}
    }
}

/* Test whether attrition can ever happen in this game. */

static int
init_attrition()
{
    int u, t;
    
    for_all_unit_types(u) {
    	for_all_terrain_types(t) {
    	    if (ut_attrition(u, t) > 0)
	      return TRUE;
    	}
    }
    return FALSE;
}

/* Attrition only takes out a few hp at a time, but can be deadly... */

static void
run_attrition()
{
    int u, dmg;
    Unit *unit;

    for_all_units(unit) {
	if (in_play(unit)) {
	    u = unit->type;
	    dmg = prob_fraction(ut_attrition(u, terrain_at(unit->x, unit->y)));
	    /* This is like hit_unit but doesn't have other effects. */
	    unit->hp2 -= dmg;
	    /* (should be able to pass reason to damage_unit) */
	    damage_unit(unit, accident_dmg);
	}
    }
}

/* Test whether revolts can ever happen in this game. */

static int
init_revolts()
{
    int u;

    for_all_unit_types(u) {
	if (u_revolt(u) > 0)
	  return TRUE;
    }
    return FALSE;
}

/* Check each unit to see whether it revolts spontaneously.  While
   surrender is influenced by nearby units, revolt takes only the
   overall state of the world into account. */

static void
run_revolts()
{
    Unit *unit;

    for_all_units(unit) {
	if (in_play(unit) && u_revolt(unit->type) > 0) {
	    unit_revolt(unit);
	}
    }
}

static void
unit_revolt(unit)
Unit *unit;
{
    int u = unit->type, ux = unit->x, uy = unit->y, chance, count;
    Side *oldside = unit->side, *newside;

    chance = u_revolt(u);
    if (0 /* affected by politics */) {
    }
    if (xrandom(10000) < chance) {
	if (0 /* leanings towards various sides */) {
	    /* (should want to change to best-liked side) */
	} else {
	    count = 0;
	    while (count++ < 200) {
		newside = side_n(xrandom(numsides + 1));
		if (unit_allowed_on_side(unit, newside)
		    && newside != oldside) {
		    break;
		}
	    }
	}
	/* Might not have been much of a revolt. :-) */
	if (newside == oldside)
	  return;
	change_unit_side(unit, newside, H_UNIT_REVOLTED, NULL);
	see_exact(oldside, ux, uy);
	update_cell_display(oldside, ux, uy, TRUE);
	all_see_cell(ux, uy);
    }
}

/* Test whether surrenders can happen in this game. */

int *surrender_ranges;

static int
init_surrenders()
{
    int u1, u2, u3, range, rslt = FALSE;
    
    for_all_unit_types(u1) {
    	for_all_unit_types(u2) {
    	    if (uu_surrender_chance(u1, u2) > 0) {
		rslt = TRUE;
		if (surrender_ranges == NULL)
		  surrender_ranges = (int *) xmalloc(MAXUTYPES * sizeof(int));
		for_all_unit_types(u3) surrender_ranges[u3] = -1;
		range = uu_surrender_range(u1, u2);
		surrender_ranges[u1] = max(range, surrender_ranges[u1]);
    	    }
    	}
    }
    return rslt;
}

static void
run_surrenders()
{
    Unit *unit;

    for_all_units(unit) {
	if (in_play(unit)) {
	    unit_surrender(unit);
	}
    }
}

/* Units may surrender to enemy units that are visible nearby.
   Independents have to be treated specially, since they don't have a view
   to work from.  We sort of compute the view "on the fly". */

static void
unit_surrender(unit)
Unit *unit;
{
    int u = unit->type, dir, x1, y1, range /*, surrounded = TRUE */;
    Unit *unit2;

    range = surrender_ranges[u];
    if (range < 0) {
	/* This unit won't surrender, nothing to do. */
    } else if (range > 1) {
	/* (should write general case) */
    } else {
	/* Range is 0 or 1; check other units in this cell. */
	for_all_stack(unit->x, unit->y, unit2) {
	    if (in_play(unit2)
		&& unit2->side != unit->side
		&& uu_surrender_chance(u, unit2->type) > 0
		&& visible_to(unit, unit2)) {
		maybe_surrender_to(unit, unit2);
	    }
	}
	/* Check on adjacent units. */
        if (range == 1) {
	  for_all_directions(dir) {
	    if (interior_point_in_dir(unit->x, unit->y, dir, &x1, &y1)) {
		for_all_stack(unit->x, unit->y, unit2) {
		    if (in_play(unit2)
			&& unit2->side != unit->side
			&& uu_surrender_chance(u, unit2->type) > 0
			&& visible_to(unit, unit2)) {
			maybe_surrender_to(unit, unit2);
		    }
		}
	    }
	  }
	}
    }
}

/* Calculate whether one unit is visible to another, even if the other is independent. */

int
visible_to(unit, unit2)
Unit *unit, *unit2;
{
    int uview;

    if (all_see_all) {
	return TRUE;
    } else if (unit->side != NULL) {
    	uview = unit_view(unit->side, unit2->x, unit2->y);
	return (uview != EMPTY && vtype(uview) == unit2->type);
    } else {
	/* (should be more careful to check see-chances) */
    	if (distance(unit->x, unit->y, unit2->x, unit2->y) <= u_vision_range(unit->type))
    	  return TRUE;
    	else
    	  return FALSE;
    }
}

static void
maybe_surrender_to(unit, unit2)
Unit *unit, *unit2;
{
    int chance;

    chance = uu_surrender_chance(unit->type, unit2->type);
    if (xrandom(10000) < chance) {
	/* (should be able to indicate that this was surrender, not capture */
	capture_unit(unit, unit2);
    }
}

int tmpexcess;

/* We've "found what we were searching for" when the excess to distribute
   is gone. */

static int
excess_left(x, y)
int x, y;
{
    return (tmpexcess > 0);
}

static void
try_transfer_to_cell(x, y)
int x, y;
{
    Unit *unit2, *occ;

    if (tmpexcess <= 0)
      return;
    for_all_stack(x, y, unit2) {
	if (in_play(unit2) && unit2->side == tmpunit->side) {
	    try_transfer(tmpunit, unit2, tmpmtype);
	}
    }
    for_all_stack(x, y, unit2) {
	if (in_play(unit2) && unit2->side == tmpunit->side) {
	    for_all_occupants(unit2, occ) {
		if (in_play(occ) && occ->side == tmpunit->side) {
		    try_transfer(tmpunit, occ, tmpmtype);
		}
	    }
	}
    }
}

static int
sharable_left(x, y)
int x, y;
{
    return tmpunit->supply[tmpmtype] > (um_storage_x(tmpunit->type, tmpmtype) / 2);
}

static void
try_sharing_with_cell(x, y)
int x, y;
{
    Unit *unit2, *occ;

    if (!sharable_left(x, y))
      return;
    for_all_stack(x, y, unit2) {
	if (in_play(unit2) && unit2->side == tmpunit->side) {
	    try_sharing(tmpunit, unit2, tmpmtype);
	}
    }
    for_all_stack(x, y, unit2) {
	if (in_play(unit2) && unit2->side == tmpunit->side) {
	    for_all_occupants(unit2, occ) {
		if (in_play(occ) && occ->side == tmpunit->side) {
		    try_sharing(tmpunit, occ, tmpmtype);
		}
	    }
	}
    }
}

/* The main routine does production, distribution, and discarding in order. */

static void
run_economy()
{
    int u, m, t, amt, dist, x, y, x1, y1, m1, m2;
    int prod, ptivity, stor, oldamt, newamt;
    int ttotals[MAXMTYPES], utotals[MAXMTYPES];
    Unit *unit;
    
    if (nummtypes == 0)
      return;
    if (any_unit_production < 0) {
	any_unit_production = FALSE;
	for_all_unit_types(u) {
	    for_all_material_types(m) {
		if (max(um_base_production(u, m),
			um_occ_production(u, m)) > 0) {
		    any_unit_production = TRUE;
		    break;
		}
	    }
	}
    }
    if (any_terrain_production < 0) {
	any_terrain_production = FALSE;
	for_all_terrain_types(t) {
	    for_all_material_types(m) {
		if (tm_production(t, m) > 0) {
		    any_terrain_production = TRUE;
		    break;
		}
	    }
	}
    }
    if (any_people_production < 0) {
	any_people_production = FALSE;
	for_all_material_types(m1) {
	    if (m_people(m1) > 0) {
		for_all_material_types(m2) {
		    if (mm_people_production(m1, m2) > 0) {
			any_people_production = TRUE;
			break;
		    }
		}
	    }
	}
    }
    if (!any_unit_production
	&& !any_terrain_production
	&& !any_people_production)
      return;
    /* (should find other reasons not to run this) */
    Dprintf("Running economy\n");
    for_all_material_types(m)
      ttotals[m] = utotals[m] = 0;
    /* Make new materials but don't clip to storage capacities yet. */
    if ((any_terrain_production || any_people_production)
	&& any_cell_materials_defined()) {
	for_all_material_types(m) {
	    if (cell_material_defined(m)) {
		for_all_cells(x, y) {
		    if (any_terrain_production) {
			t = terrain_at(x, y);
			prod = tm_production(t, m);
			if (prod > 0) {
			    oldamt = material_at(x, y, m);
			    newamt = oldamt + prod;
			    set_material_at(x, y, m, newamt);
			    if (Debug) {
				stor = tm_storage_x(t, m);
				if (newamt > stor)
				  ttotals[m] += (newamt - stor);
			    }
			}
		    }
		    if (any_people_production) {
			for_all_material_types(m1) {
			    if (m_people(m1) > 0) {
				prod = mm_people_production(m1, m);
				if (prod > 0) {
				    oldamt = material_at(x, y, m);
				    newamt = oldamt + prod;
				    set_material_at(x, y, m, newamt);
				    if (Debug) {
					stor = tm_storage_x(t, m);
					if (newamt > stor)
					  ttotals[m] += (newamt - stor);
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
    if (any_unit_production) {
	for_all_units(unit) {
	    if (in_play(unit) && completed(unit)) {
		u = unit->type;
		for_all_material_types(m) {
		    t = terrain_at(unit->x, unit->y);
		    prod = base_production(unit, m);
		    if (prod > 0) {
			ptivity = ut_productivity(u, t);
			/* Note that we've switched to hundredths. */
			ptivity = max(ptivity, um_productivity_min(u, m));
			ptivity = min(ptivity, um_productivity_max(u, m));
			amt = prob_fraction(prod * ptivity);
			unit->supply[m] += amt;
			if (Debug && unit->supply[m] > um_storage_x(u, m))
			  utotals[m] += (unit->supply[m] - um_storage_x(u, m));
		    }
		}
	    }
	}
    }
    Dprintf("Overflow is:");
    Dprintf("  (for terrain)");
    for_all_material_types(m) Dprintf(" %d", ttotals[m]);
    Dprintf("  (for units)");
    for_all_material_types(m) Dprintf(" %d", utotals[m]);
    Dprintf("\n");
    /* Move stuff around - try to get rid of any excess. */
    /* (should also do cell-cell, cell-unit, unit-cell xfers) */
    for_all_units(unit) {
	if (in_play(unit) && !indep(unit)) {
	    u = unit->type;
	    for_all_material_types(m) {
		stor = um_storage_x(u, m);
		if (unit->supply[m] > stor) {
		    dist = um_outlength(u, m);
		    if (dist >= 0) {
		    	tmpunit = unit;
		    	tmpmtype = m;
		    	tmpexcess = unit->supply[m] - stor;
			search_and_apply(unit->x, unit->y, dist, excess_left,
					 &x1, &y1, 1,
					 try_transfer_to_cell, 999999);
		    }
		}
	    }
	}
    }
    /* Throw away excess that can't be stored anywhere. */
    for_all_material_types(m)
      ttotals[m] = utotals[m] = 0;
    if (any_cell_materials_defined()) {
	for_all_material_types(m) {
	    if (cell_material_defined(m)) {
		for_all_cells(x, y) {
		    t = terrain_at(x, y);
		    stor = tm_storage_x(t, m);
		    oldamt = material_at(x, y, m);
		    newamt = min(oldamt, stor);
		    set_material_at(x, y, m, newamt);
		    if (Debug && newamt < oldamt)
		      ttotals[m] += (oldamt - newamt);
		}
	    }
	}
    }
    for_all_units(unit) {
	u = unit->type;
	for_all_material_types(m) {
	    stor = um_storage_x(u, m);
	    oldamt = unit->supply[m];
	    newamt = min(oldamt, stor);
	    unit->supply[m] = newamt;
	    if (Debug && newamt < oldamt)
	      utotals[m] += (oldamt - newamt);
	}
    }
    Dprintf("Discarded ");
    Dprintf("  (for terrain)");
    for_all_material_types(m) Dprintf(" %d", ttotals[m]);
    Dprintf("  (for units)");
    for_all_material_types(m) Dprintf(" %d", utotals[m]);
    Dprintf("\n");
    /* This next phase is for sharing of scarcer supplies. */
    for_all_units(unit) {
	if (in_play(unit) && !indep(unit)) {
	    u = unit->type;
	    for_all_material_types(m) {
		dist = um_outlength(u, m);
		if (dist >= 0) {
		    tmpunit = unit;
		    tmpmtype = m;
		    search_and_apply(unit->x, unit->y, dist, sharable_left,
				     &x1, &y1, 1,
				     try_sharing_with_cell, 999999);
		}
	    }
	}
    }
    /* Finally, reset supply alarms. */
    for_all_units(unit) {
	if (in_play(unit) && unit->plan != NULL) {
	    /* (should probably be a subr) */
	    if (unit->plan->supply_is_low
		&& !past_halfway_point(unit)) {
		unit->plan->supply_alarm = TRUE;
		unit->plan->supply_is_low = FALSE;
    		update_unit_display(unit->side, unit, TRUE); 
	    }
	}
    }
}

static int
base_production(unit, m)
Unit *unit;
int m;
{
    int u = unit->type, occprod;

    if (unit->transport) {
	occprod = um_occ_production(u, m);
	return (occprod >= 0 ? occprod : um_base_production(u, m));
    } else {
	return um_base_production(u, m);
    }
}

/* Give away supplies, but save enough to stay alive for a couple turns. */

static void
try_transfer(from, to, m)
Unit *from, *to;
int m;
{
    int oldsupply = from->supply[m];

    try_transfer_aux(from, to, m);
    tmpexcess -= (oldsupply - from->supply[m]);
}

/* Note that this may be called on newly-completed units during a turn. */

void
try_sharing(from, to, m)
Unit *from, *to;
int m;
{
    try_transfer_aux(from, to, m);
}

/* Material redistribution uses this routine to move supplies around
   between units far apart or on the same cell. Try to do reasonable
   things with the materials.  Net producers are much more willing to
   give away supplies than net consumers. */

static void
try_transfer_aux(from, to, m)
Unit *from, *to;
int m;
{
    int nd, u = from->type, u2 = to->type, fromrate, torate;

    if (from != to &&
	um_inlength(u2, m) >= distance(from->x, from->y, to->x, to->y)) {
	if (completed(to)) {
	    /* Try for the transfer only if we're below capacity. */
	    nd = um_storage_x(u2, m) - to->supply[m];
	    if (nd  > 0) {
		if ((um_base_production(u, m) > um_base_consumption(u, m))
		    || (survival_time(to) < 3)
		    || (um_storage_x(u, m) * 4 >= um_storage_x(u2, m))) {
		    if (can_satisfy_need(from, m, nd)) {
			transfer_supply(from, to, m, nd);
		    } else if (can_satisfy_need(from, m, max(1, nd/2))) {
			transfer_supply(from, to, m, max(1, nd/2));
		    } else if (from->supply[m] > um_storage_x(u, m)) {
			transfer_supply(from, to, m,
					(from->supply[m]
					 - um_storage_x(u, m)));
		    }
		} else {
		    fromrate = u_speed(u) * um_consumption_per_move(u, m) * 3;
		    fromrate = max(1, fromrate);
		    torate = u_speed(u2) * um_consumption_per_move(u2, m) * 3;
		    torate = max(1, torate);
		    if ((from->supply[m] / fromrate)
			> (to->supply[m] / torate)) {
			transfer_supply(from, to, m,
					min(nd, (8 + from->supply[m]) / 9));
		    }
		}
	    }
	} else {
	    /* Incomplete units don't need supply, but they are a
	       handy overflow mepository. */
	    if (from->supply[m] > um_storage_x(u, m)
		&& to->supply[m] < um_storage_x(u2, m)) {
		/* Calculate the limit on how much we can transfer usefully. */
		nd = min(um_storage_x(u2, m) - to->supply[m],
			 from->supply[m] - um_storage_x(u, m));
		transfer_supply(from, to, m, nd);
	    }
	}
    }
}

/* This estimates if a need can be met.  Note that total transfer of
   requested amount is not a good idea, since the supplies might be
   essential to the unit that has them first.  If we're more than half
   full, or the request is very small, then we can spare it. */

/* (should replace with doctrine/plan controls, check underlying terrain) */

static int
can_satisfy_need(unit, m, need)
Unit *unit;
int m, need;
{
    int supp = unit->supply[m];
    int stor = um_storage_x(unit->type, m);

    return (((2 * supp > stor) && (need < ((supp * 4) / 5)))
	    || (need < stor / 5));
}

/* Do everything associated with the end of a turn. */

static void
run_turn_end()
{
    finish_movement();
    run_people_consumption();
    run_cell_consumption();
    run_unit_base_consumption();
    run_environment_effects();
    run_people_side_changes();
    /* This should come after other people-related computations,
       since this only constrains generic overcrowding. */
    run_people_limits();
    flush_dead_units();
    check_post_turn_scores();
    test_agreements();
    run_disappearances();
#ifdef DEBUGGING
    if (Debug)
      report_malloc();
#endif /* DEBUGGING */
}

/* Handle consumption by people. */

static void
run_people_consumption()
{
    int x, y, m1, m2, t, consum, oldamt, newamt, newtype;

    /* Precompute whether any people consumption ever happens. */
    if (any_people_consumption < 0) {
	int mm1, mm2;

	any_people_consumption = FALSE;
	for_all_material_types(mm1) {
	    for_all_material_types(mm2) {
	    	if (mm_people_consumption(mm1, mm2) > 0) {
		    any_people_consumption = TRUE;
		    break;
	    	}
	    }
	    if (any_people_consumption)
	      break;
	}
	Dprintf("Have consumption by people.\n");
    }
    if (!any_people_consumption)
      return;
    Dprintf("Running people consumption\n");
    if (any_cell_materials_defined()) {
	for_all_material_types(m1) {
	    if (cell_material_defined(m1)) {
		for_all_material_types(m2) {
		    if (cell_material_defined(m2)) {
			consum = mm_people_consumption(m1, m2);
			if (consum > 0) {
			    for_all_cells(x, y) {
				oldamt = material_at(x, y, m2);
				newamt = oldamt - consum;
				if (newamt < 0) {
				    newamt = 0;
				    /* Check for exhaustion. */
				    /* (should share with cell consumption) */
				    t = terrain_at(x, y);
				    if (probability(tm_change_on_exhaust(t, m2)) &&
					tm_exhaust_type(t, m2) != NONTTYPE) {
					newtype = tm_exhaust_type(t, m2);
					/* Change the terrain's type. */
					change_terrain_type(x, y, newtype);
				    }
				}
				set_material_at(x, y, m2, newamt);
			    }
			}
		    }
		}
	    }
	}
    }
}

/* Handle consumption by terrain. */

static void
run_cell_consumption()
{
    int x, y, t, m, consum, oldamt, newamt, willchange, newtype;

    /* Precompute whether any cell base consumption ever happens. */
    if (any_cell_consumption < 0) {
	int t2, m2;

	any_cell_consumption = FALSE;
	for_all_terrain_types(t2) {
	    for_all_material_types(m2) {
	    	if (tm_consumption(t2, m2) > 0) {
		    any_cell_consumption = TRUE;
		    break;
	    	}
	    }
	    if (any_cell_consumption)
	      break;
	}
	Dprintf("Have consumption by cells.\n");
    }
    if (!any_cell_consumption)
      return;
    Dprintf("Running cell consumption\n");
    if (any_cell_materials_defined()) {
	for_all_cells(x, y) {
	    t = terrain_at(x, y);
	    willchange = FALSE;
	    for_all_material_types(m) {
		if (cell_material_defined(m)) {
		    consum = tm_consumption(t, m);
		    oldamt = material_at(x, y, m);
		    newamt = oldamt - consum;
		    if (newamt < 0) {
			newamt = 0;
			/* Check for exhaustion. */
			if (!willchange &&
			    probability(tm_change_on_exhaust(t, m)) &&
			    tm_exhaust_type(t, m) != NONTTYPE) {
			    willchange = TRUE;
			    newtype = tm_exhaust_type(t, m);
			}
		    }
		    set_material_at(x, y, m, newamt);
		}
	    }
	    if (willchange) {
	    	/* Change the terrain's type. */
	    	change_terrain_type(x, y, newtype);
	    }
	}
    }
}

/* Handle base consumption by units. */

static void
run_unit_base_consumption()
{
    Unit *unit;

    /* Precompute whether any base consumption ever happens. */
    if (any_unit_base_consumption < 0) {
	int u, m;

	any_unit_base_consumption = FALSE;
	for_all_unit_types(u) {
	    for_all_material_types(m) {
	    	if (um_base_consumption(u, m) > 0) {
		    any_unit_base_consumption = TRUE;
		    break;
	    	}
	    }
	    if (any_unit_base_consumption)
	      break;
	}
	Dprintf("Have consumption by units.\n");
    }
    if (!any_unit_base_consumption)
      return;
    Dprintf("Running unit consumption\n");
    for_all_units(unit) {
	if (is_active(unit)) {
	    unit_consumes(unit);
	}
    }
}

/* Consume the constant overhead part of supply consumption. */
/* Usage by movement is subtracted from overhead first. */

static void
unit_consumes(unit)
Unit *unit;
{
    int u = unit->type, m, usedup, consump, tempeff, checkstarve = FALSE;
    Obj *effect;

    if (alive(unit)) {    
	for_all_material_types(m) {
	    if (um_base_consumption(u, m) > 0 &&
		!(unit->transport != NULL
		  && um_consumption_as_occupant(u, m) == 0)) {
		/* Calculate what was already consumed by movement. */
		usedup = 0;
		if (unit->act != NULL)
		  usedup = unit->act->actualmoves * um_consumption_per_move(u, m);
		consump = um_base_consumption(u, m);
		/* If being transported, modify the base consumption. */
		if (unit->transport != NULL)
		  consump = (consump * um_consumption_as_occupant(u, m)) / 100;
		/* Modify consumption if temperature effects. */
		effect = u_consume_temp_effect(u);
#if 0		/* (should think about this code a bit more) */
		if (temperatures_defined() && effect != lispnil) {
		    tempeff = interpolate_in_list(temperature_at(unit->x, unit->y), effect, FALSE, 999);
		    consump = (consump * tempeff) / 100;
		}
#endif
		/* Subtract consumption that was not already used up in movement. */
		if (usedup < consump)
		  unit->supply[m] -= (consump - usedup);
		/* Don't let supply go below zero. */
		if (unit->supply[m] <= 0) {
		    unit->supply[m] = 0;
		    checkstarve = TRUE;
		}
	    }
	}
    }
    if (checkstarve)
      maybe_starve(unit, TRUE);
    if (alive(unit)
    	&& unit->plan
    	&& !unit->plan->supply_is_low
    	&& past_halfway_point(unit)
    	) {
    	unit->plan->supply_is_low = TRUE;
    	update_unit_display(unit->side, unit, TRUE); 
    }
}

/* What happens to a unit that runs out of supplies.  If it can survive
   on nothing, then there may be a few turns of grace, depending on
   how the dice roll... */

void
maybe_starve(unit, partial)
Unit *unit;
int partial;
{
    int u = unit->type, m, starv, oneloss, hploss = 0;

    for_all_material_types(m) {
	if (unit->supply[m] <= 0 && !in_supply(unit, m)) {
	    starv = um_hp_per_starve(u, m);
	    if (starv > 0) {
		oneloss = prob_fraction(starv);
		hploss = max(hploss, oneloss);
	    }
	}
    }
    if (hploss > 0) {
	if (hploss >= unit->hp) {
	    /* (should let occupants try to escape first) */
	    kill_unit(unit, H_UNIT_STARVED);
	} else if (partial) {
	    unit->hp -= hploss;
	    unit->hp2 -= hploss;
	    /* (should do other hp loss consequences) */
	    /* (use generic damage routine?) */
	}
    }
}

/* Check if the unit has ready access to a source of supplies. */

/* (should be more sophisticated and account for supply lines etc) */

static int
in_supply(unit, m)
Unit *unit;
int m;
{
    if (unit->transport != NULL) {
    	if (unit->transport->supply[m] > 0)
	  return TRUE;
    }
    return FALSE;
}

/* Some types of units can become completed and grow to full size
   automatically when they get to a certain point. */

static void
run_self_builds()
{
    int u, cpper;
    Unit *unit;

    if (any_self_builds < 0) {
	any_self_builds = FALSE;
	for_all_unit_types(u) {
	    if (u_cp_per_self_build(u) > 0) {
		any_self_builds = TRUE;
		break;
	    }
	}
    }
    if (!any_self_builds)
      return;
    Dprintf("Running self builds\n");
    for_all_units(unit) {
	u = unit->type;
	if (in_play(unit)
	    && !fullsized(unit)
	    && (cpper = u_cp_per_self_build(u)) > 0
	    && unit->cp >= u_cp_to_self_build(u)) {
	    unit->cp += cpper;
	    if (unit->cp > u_cp(u)) unit->cp = u_cp(u);
	    if (completed(unit)) {
		make_unit_complete(unit);
	    } else {
	    	/* Let the player know that progress was made. */
		update_unit_display(unit->side, unit, TRUE);
	    }
	}
    }
}

static void
run_environment_effects()
{
    int err, dmg;
    Unit *unit;
    Obj *attrition;

    if (!temperatures_defined())
      return;
    /* (should check if temperature attrition ever non-nil) */
    Dprintf("Running environmental effects\n");
    for_all_units(unit) {
	if (is_active(unit)) {
	    attrition = u_temp_attrition(unit->type);
	    if (attrition != lispnil) {
		err = interpolate_in_list(temperature_at(unit->x, unit->y), attrition, &dmg);
		if (err != 0) {
		    dmg = 0;
		}
		damage_unit_with_temperature(unit, dmg);
	    }
	    /* (should check for storm damage here?) */
	}
    }
}

static void
damage_unit_with_temperature(unit, dmg)
Unit *unit;
int dmg;
{
    int n;

    n = prob_fraction(dmg);
    if (n >= unit->hp) {
	notify(unit->side, "%s dies from excessive temperatures",
	       unit_handle(unit->side, unit));
	/* should have a different hevt type - this will be recorded as a combat death */
	kill_unit(unit, H_UNIT_DIED_FROM_TEMPERATURE);
    } else if (n > 0) {
	notify(unit->side, "%s loses %d HP due to excessive temperatures",
		unit_handle(unit->side, unit), n);
	unit->hp -= n;
	unit->hp2 -= n;
	update_unit_display(unit->side, unit, TRUE);
    }
}

static void
run_people_side_changes()
{
    int x, y, u, t;
    Unit *unit;

    if (!people_sides_defined())
      return;
    if (any_people_side_changes < 0) {
	any_people_surrenders = (int *) xmalloc(numutypes * sizeof(int));
	any_people_side_changes = FALSE;
	for_all_unit_types(u) {
	    for_all_terrain_types(t) {
		if (ut_people_surrender(u, t) > 0) {
		    any_people_side_changes = TRUE;
		    any_people_surrenders[u] = TRUE;
		    break;
		}
	    }
	}
    }
    if (!any_people_side_changes)
      return;
    for_all_cells(x, y) {
	if (unit_at(x, y) != NULL) {
	    for_all_stack(x, y, unit) {
		/* The people here may change sides. */
	        u = unit->type;
		if (any_people_surrenders[u]
		    && probability(people_surrender_chance(u, x, y))) {
		    change_people_side_around(x, y, u, unit->side);
		}
	    }
	} else {
	    /* Unoccupied cells might see population revert. */
	    /* (this would need multiple-loyalty pops) */
	}
    }
}

int
people_surrender_chance(u, x, y)
int u, x, y;
{
    int m, chance, peop;

    chance = ut_people_surrender(u, terrain_at(x, y));
    /* Modify the basic chance according to people types, if present. */
    if (any_cell_materials_defined()) {
	for_all_material_types(m) {
	    if (m_people(m) > 0
		&& cell_material_defined(m)) {
		peop = material_at(x, y, m);
		if (peop > 0) {
		    chance = (chance * um_people_surrender(u, m)) / 100;
		}
	    }
	}
    }
    return chance;
}

void
change_people_side_around(x, y, u, side)
int x, y, u;
Side *side;
{
    int pop = people_side_at(x, y), s = side_number(side), dir, x1, y1;
    Side *oldside;

    if (pop != NOBODY
        && pop != s
        && !trusted_side(side, side_n(pop))) {
        oldside = side_n(pop);
	set_people_side_at(x, y, s);
	update_cell_display_all_sides(x, y, TRUE);
	for_all_directions(dir) {
	    if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
		update_cell_display_all_sides(x1, y1, TRUE);
	    }
	}
	/* Previous side loses its free coverage. */
	if (oldside) {
	    add_cover(oldside, x, y, -1);
	    /* Update coverage display. */
	    update_cell_display(oldside, x, y, 36);
	}
    }
    /* (should add ability to change adjacent cells also) */
}

/* See if the numbers of individuals in a cell exceeds the max, and migrate or
   remove so as to bring the numbers back in line. */

static void
run_people_limits()
{
    int m, t, x, y, num, ratio, amt, newamt;
    
    if (any_people_max < 0) {
	any_people_max = FALSE;
	for_all_terrain_types(t) {
	    if (t_people_max(t) >= 0) {
		any_people_max = TRUE;
		break;
	    }
	}
    }
    if (!any_people_max)
      return;
    if (!any_cell_materials_defined())
      return;
    for_all_cells(x, y) {
	t = terrain_at(x, y);
	if (t_people_max(t) >= 0) {
	    num = num_people_at(x, y);
	    if (num > t_people_max(t)) {
		/* Too many people here, trim them down. */
		/* Compute the ratio of limit to actual number.
		   (Note that actual number is guaranteed to be nonzero.) */
		ratio = (t_people_max(t) * 100) / num;
		for_all_material_types(m) {
		    if (m_people(m) > 0
			&& cell_material_defined(m)) {
			amt = material_at(x, y, m);
			if (amt > 0) {
			    newamt = (amt * ratio) / 100;
			    set_material_at(x, y, m, newamt);
			    /* (should update sides?) */
			}
		    }
		}
	    }
	}
    }
}

/* (generic routine) */

int
num_people_at(x, y)
int x, y;
{
    int m, num;

    num = 0;
    for_all_material_types(m) {
	if (cell_material_defined(m)) {
	    num += material_at(x, y, m) * m_people(m);
	}
    }
    return num;
}

static void
update_cell_display_all_sides(x, y, rightnow)
int x, y, rightnow;
{
    Side *side;

    for_all_sides(side) {
	/* (Testing for unit visibility is not quite right, but close
	   enough for now.) */
	if (side->ingame && units_visible(side, x, y)) {
	    update_cell_display(side, x, y, rightnow);
	}
    }
}

/* See if it's time for any scheduled arrivals to appear. */

static void
run_appearances()
{
    int curturn, nx, ny, nw, nh, nx1, ny1;
    Unit *unit, *transport;

    if (any_appearances < 0) {
    	any_appearances = FALSE;
	for_all_units(unit) {
	    if (unit->cp < 0 && unit_appear_turn(unit) >= 0) {
		any_appearances = TRUE;
		break;
	    }
	}
    }
    if (!any_appearances)
      return;
    Dprintf("Running appearances\n");
    curturn = g_turn();
    for_all_units(unit) {
    	/* See if now time for a unit to appear. */
    	if (unit->cp < 0 && unit_appear_turn(unit) >= 0) {
	    if (unit_appear_turn(unit) <= curturn) {
		/* Set the unit to its correct cp. */
		unit->cp = (- unit->cp);
		/* Get the base location at which it will appear. */
		nx = (- unit->prevx);  ny = (- unit->prevy);
		if (unit_appear_x(unit) >= 0 && unit_appear_y(unit) >= 0) {
		    /* Appear at a random location around nx,ny. */
		    nw = unit_appear_x(unit);  nh = unit_appear_y(unit);
		    if (random_point_in_area(nx, ny, nw, nh, &nx1, &ny1)) {
			nx = nx1;  ny = ny1;
		    }
		}
		/* Do the usual steps to place the unit. */
		/* (should be able to retry with diff loc if nw or nh > 0) */
		if (inside_area(nx, ny)) {
		    if (can_occupy_cell(unit, nx, ny)) {
			enter_cell(unit, nx, ny);
		    } else {
			/* Search this cell for units to enter. */
			for_all_stack(nx, ny, transport) {
			    if (unit->side == transport->side
				&& can_occupy(unit, transport)) {
				enter_transport(unit, transport);
				break;
			    }
			}
			/* We've got a problem, make the unit wait for next turn;
			   since the appearance turn test is "<=", will just try again. */
			unit->cp = (- unit->cp);
		    }
		} else {
		    /* loc of reinforcement is messed up */
		}
		init_unit_actorstate(unit, FALSE);
		init_unit_plan(unit);
	    }
    	}
    }
}

static void
run_disappearances()
{
    int curturn;
    Unit *unit;

    if (any_disappearances < 0) {
    	any_disappearances = FALSE;
	for_all_units(unit) {
	    if (unit_disappear_turn(unit) >= 0) {
		any_disappearances = TRUE;
		break;
	    }
	}
    }
    if (!any_disappearances)
      return;
    Dprintf("Running disappearances\n");
    curturn = g_turn();
    for_all_units(unit) {
    	/* See if now time for a unit to disappear. */
    	if (in_play(unit) && unit_disappear_turn(unit) >= 0) {
	    if (unit_disappear_turn(unit) <= curturn) {
		/* (should eject occupants first if possible) */
		kill_unit(unit, H_UNIT_LEFT_WORLD);
	    }
	}
    }
}

/* Some types of units recover lost hp spontaneously. */

static void
run_hp_recovery()
{
    int u, hprecovery, hpmax, oldhp;
    Unit *unit;

    if (any_hp_recovery < 0) {
	any_hp_recovery = FALSE;
    	for_all_unit_types(u) {
	    if (u_hp_recovery(u) > 0) {
		any_hp_recovery = TRUE;
		break;
	    }
	}
    }
    if (!any_hp_recovery)
      return;
    Dprintf("Running hp recovery\n");
    for_all_units(unit) {
	if (is_active(unit)) {
	    u = unit->type;
	    hprecovery = u_hp_recovery(u);
	    hpmax = u_hp(u);
	    /* (should only do for one part of multi-part unit?) */
	    if (hprecovery > 0 && unit->hp < hpmax) {
		oldhp = unit->hp;
		add_to_unit_hp(unit, prob_fraction(hprecovery));
		/* Inform the player if the unit's hp changed. */
		if (unit->hp != oldhp) {
		    update_unit_display(unit->side, unit, TRUE);
		}
	    }
	}
    }
}

/* Some types of units can repair others without doing actions. */

static void
run_auto_repair()
{
    int u1, u2, u, hpmax, oldhp;
    Unit *unit, *occ;

    if (any_auto_repair < 0) {
	any_auto_repair = FALSE;
    	for_all_unit_types(u1) {
	    for_all_unit_types(u2) {
		if (uu_auto_repair(u2, u1) > 0) {
		    any_auto_repair = TRUE;
		    break;
		}
	    }
	}
    }
    if (!any_auto_repair)
      return;
    if (will_be_auto_repaired == NULL) {
	will_be_auto_repaired =
	  (short *) xmalloc(numutypes * sizeof(short));
	auto_repair_range_max =
	  (short *) xmalloc(numutypes * sizeof(short));
    	for_all_unit_types(u1) {
	    will_be_auto_repaired[u1] = FALSE;
	    auto_repair_range_max[u1] = -1;
	    for_all_unit_types(u2) {
		if (uu_auto_repair(u2, u1) > 0) {
		    will_be_auto_repaired[u1] = TRUE;
		    auto_repair_range_max[u1] =
		      max(auto_repair_range_max[u1], uu_auto_repair_range(u2, u1));
		}
	    }
	}
    }
    Dprintf("Running auto repair\n");
    for_all_units(unit) {
	if (is_active(unit)) {
	    u = unit->type;
	    hpmax = u_hp(u);
	    /* (should only do for one part of multi-part unit?) */
	    if (unit->hp < u_hp(u) && will_be_auto_repaired[u]) {
		oldhp = unit->hp;
		if (auto_repair_range_max[u] < 0) {
		    if (unit->transport && uu_auto_repair(unit->transport->type, u) > 0) {
			auto_repair_unit(unit->transport, unit);
		    } else {
			for_all_occupants(unit, occ) {
			    if (is_active(occ) && uu_auto_repair(occ->type, u) > 0) {
				auto_repair_unit(occ, unit);
			    }
			}
		    }
		} else {
		    tmpunit = unit;
		    apply_to_area(unit->x, unit->y, auto_repair_range_max[u], auto_repair_from_here);
		}
		/* Inform the player if the unit's hp changed. */
		if (unit->hp != oldhp) {
		    update_unit_display(unit->side, unit, TRUE);
		}
	    }
	}
    }
}

/* Try to auto-repair using anything found at the given location. */

static void
auto_repair_from_here(x, y)
int x, y;
{
    int dist;
    Unit *unit2;

    /* Skip out if we're all repaired. */
    if (tmpunit->hp == u_hp(tmpunit->type)) {
	stop_apply = TRUE;
	return;
    }
    for_all_stack(x, y, unit2) {
	if (unit2 != tmpunit
	    && trusted_side(unit2->side, tmpunit->side)
	    && uu_auto_repair(unit2->type, tmpunit->type) > 0) {
	    dist = distance(tmpunit->x, tmpunit->y, unit2->x, unit2->y);
	    if (dist <= uu_auto_repair_range(unit2->type, tmpunit->type)) {
		auto_repair_unit(unit2, tmpunit);
	    }
	}
    }
}

/* Do the actual auto-repair. */

static void
auto_repair_unit(unit, unit2)
Unit *unit, *unit2;
{
    int u = unit->type, u2 = unit2->type, m, repair;

    /* Check the basic restrictions on repair. */
    if (unit->hp < uu_hp_to_repair(u, u2))
      return;
    for_all_material_types(m) {
	if (unit->supply[m] < um_to_repair(u, m))
	  return;
    }
    repair = uu_auto_repair(u, u2);
    add_to_unit_hp(unit2, prob_fraction(repair));
}

void
add_to_unit_hp(unit, hp)
Unit *unit;
int hp;
{
    int hpmax;

    unit->hp += hp;
    hpmax = u_hp(unit->type);
    if (unit->hp > hpmax)
      unit->hp = hpmax;
    unit->hp2 += hp;
    if (unit->hp2 > hpmax)
      unit->hp2 = hpmax;
}

static void
run_detonation_accidents()
{
    int u, t, x, y, z, chance;
    Unit *unit;

    if (any_detonation_accidents < 0) {
	any_detonation_accidents = FALSE;
	for_all_unit_types(u) {
	    for_all_terrain_types(t) {
		if (ut_detonation_accident(u, t) > 0) {
		    any_detonation_accidents = TRUE;
		    break;
		}
	    }
	}
    }
    if (!any_detonation_accidents)
      return;
    for_all_units(unit) {
	if (in_play(unit) && completed(unit)) {
	    x = unit->x;  y = unit->y;  z = unit->z;
	    t = terrain_at(x, y);
	    chance = ut_detonation_accident(unit->type, t);
	    if (chance > 0) {
	    	maybe_detonate_accidently(unit);
	    }
	}
    }
}

static void
maybe_detonate_accidently(unit)
Unit *unit;
{
    int x = unit->x, y = unit->y, chance, t;
    extern int max_u_detonate_effect_range;

    t = terrain_at(x, y);
    chance = ut_detonation_accident(unit->type, t);
    if (xrandom(10000) < chance) {
	/* Detonate the unit right where it is. */
	detonate_unit(unit, x, y, unit->z);
	reckon_damage_around(x, y, max_u_detonate_effect_range);
    }
}

/* Resignation, possibly giving away any remaining units. */

void
resign_game(side, side2)
Side *side, *side2;
{
    /* Nothing to do if we're not in the game. */
    if (!side->ingame)
      return;
    notify_all_of_resignation(side, side2);
    side_loses(side, side2, -1);
}

/* This is true if there is any kind of realtime limit on the game. */

int
realtime_game()
{
    return (g_rt_for_game() > 0
    	    || g_rt_per_side() > 0
    	    || g_rt_per_turn() > 0);
}

/* Pass NULL to see if all sides are now willing to save the game. */

int
all_others_willing_to_save(side)
Side *side;
{
    Side *side2;

    for_all_sides(side2) {
	if (side != side2 && !side2->willingtosave)
	  return FALSE; 
    }
    return TRUE;
}

/* Pass NULL to see if all sides are now willing to declare a draw. */

int
all_others_willing_to_quit(side)
Side *side;
{
    Side *side2;

    for_all_sides(side2) {
	if (side != side2 && !side2->willingtodraw)
	  return FALSE; 
    }
    return TRUE;
}

/* This forces an end to the game directly. */

void
end_the_game()
{
    Side *side;

    Dprintf("The game is over.\n");
    /* Make sure everybody sees this. */
    notify_all("END OF THE GAME!");
    record_event(H_GAME_ENDED, ALLSIDES);
    /* Set the global that indicates the game is over for everybody. */
    endofgame = TRUE;
    end_history();
    record_into_scorefile();
    dump_statistics();
    /* Done with internal state change, now echo onto displays. */
    for_all_sides(side) {
    	/* (should there be any other effects on sides, like final scoring?) */
    	if (side_has_display(side)) {
    	    update_turn_display(side, TRUE);
    	    update_side_display(side, side, TRUE);
    	}
    }
}
