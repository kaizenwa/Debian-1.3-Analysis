/* Implementation of the "mplayer" AI in Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern int xmalloc_warnings;

/* Limit on the number of goals that a side may have. */

#define MAXGOALS 10

/* Limit on the number of theaters a single side may have. */

#define MAXTHEATERS 98

/* what does the game look like? */
typedef enum a_game_class {
    gc_none,
    gc_standard,
    gc_time
} GameClass;

static GameClass game_class = gc_none;

/* Strategy is what a side uses to make decisions. */

typedef struct a_strategy {
    int type;			/* placeholder */
    int trytowin;
    int creationdate;
    short strengths[MAXSIDES][MAXUTYPES];  /* estimated numbers of units */
    short points[MAXSIDES];	/* estimated point value */
    short alstrengths[MAXSIDES][MAXUTYPES];  /* numbers in alliances */
    short alpoints[MAXSIDES];	/* points in alliances */
    short initial_strengths_computed;
    short strengths0[MAXSIDES][MAXUTYPES];  /* initial estimated numbers of units */
    short points0[MAXSIDES];	/* initial estimated point value */
    short alstrengths0[MAXSIDES][MAXUTYPES];  /* initial numbers in alliances */
    short alpoints0[MAXSIDES];	/* initial points in alliances */
    short contacted[MAXSIDES+1];
    short homefound[MAXSIDES+1];
    int analyzegame;
    struct a_theater *theaters;
    struct a_theater **theatertable;
    short numtheaters;
    char *areatheaters;
    struct a_theater *homefront;
    struct a_theater *perimeters[NUMDIRS];
    struct a_theater *midranges[NUMDIRS];
    struct a_theater *remotes[NUMDIRS];
    int numgoals;
    struct a_goal *goals[MAXGOALS];
    /* Exploration and search slots. */
    int zonewidth, zoneheight;
    int numzonex, numzoney;     /* dimensions of search zone array */
    int numzones;
    struct a_searchzone *searchzones;
    short *explorertypes;
    short explorersneeded;
    short *terrainguess;
    short cx, cy;               /* "centroid" of all our units */
    short *demand;              /* worth of each utype w.r.t. strategy */
    int explore_priority;
    int defend_priority;
    int attack_priority;
    struct a_unit *unitlist[MAXUTYPES];   /* lists to help mplay efficiency */
    short unitlistcount[MAXUTYPES];  /* counts of above lists */
    short *actualmix;
    short *expectedmix;
    short *idealmix;
    short *research_status;  /* specific to the "time" game */
    short *research_on;      /* specific to the "time" game */
    Obj *writable_state;
} Strategy;

/* utype-specific research status codes for the "time" game */
#define RS_RESEARCH_NEEDED 4
#define RS_RESEARCH_ASSIGNED 3
#define RS_UPGRADE_NEEDED 1

#define mplayer(s) ((Strategy *) (s)->ai)

/* A Theater is a sub-area that can be planned for all at once. */

/* To save space in theater layer, no more than 127 theaters may exist at once.
   This should be sufficient, even a Napoleon would have trouble keeping track
   of that much activity. */

typedef struct a_theater {
    short id;
    char *name;			/* an informative name for this theater */
    short x, y;			/* center of the theater */
    short xmin, ymin;		/*  */
    short xmax, ymax;		/*  */
    int size;			/* number of cells in the theater */
    short importance;		/* 0 = shrug, 100 = critical */
    Goal *maingoal;
    short allied_units;		/* How many units on our side here. */
    short makers;		/* Total number of makers */
    short unexplored;		/* number of unseen cells in theater */
    short allied_bases;		/* total number of our bases, includes towns */
    short border;		/* true if this is a border theater. */
    short reinforce;		/* priority on request for units. */
    short numassigned[MAXUTYPES];  /* num of each type assigned to theater */
    short numneeded[MAXUTYPES];  /* units we should move to theater. */
    short numtotransport[MAXUTYPES];  /* types needing transportation. */
    short numenemies[MAXUTYPES];
    short numsuspected[MAXUTYPES];
    short numsuspectedmax[MAXUTYPES];
    int *people;		/* number of populated cells seen */
    int enemystrengthmin;	/* estimate of enemy unit strength */
    int enemystrengthmax;	/* estimate of enemy unit strength */
    short units_lost;		/* How many units have we lost here. */
    struct a_theater *next;
} Theater;

#define for_all_theaters(s,th)  \
  for ((th) = mplayer(s)->theaters; (th) != NULL; (th) = (th)->next)

#define theater_at(s,x,y)  \
  (mplayer(s)->theatertable[mplayer(s)->areatheaters[(x)+area.width*(y)]])

#define set_theater_at(s,x,y,th)  \
  ((mplayer(s)->areatheaters[(x)+area.width*(y)]) = (th)->id)

#define for_all_cells_in_theater(s,x,y,th)  \
  for ((x) = theater->xmin; (x) < theater->xmax; ++(x))  \
    for ((y) = theater->ymin; (y) < theater->ymax; ++(y))  \
      if (theater_at((s), (x), (y)) == (th)

#define unit_theater(unit) ((Theater *) (unit)->aihook)

#define set_unit_theater(unit,theater) ((unit)->aihook = (char *) (theater))

#define can_see_actual_units(side, x, y) (all_see_all || cover((side), (x), (y)) > 0)

/* Local function declarations. */

static void mplayer_init PARAMS ((Side *side));
static void mplayer_init_turn PARAMS ((Side *side));
static void create_strategy PARAMS ((Side *side));
static void reset_strategy PARAMS ((Side *side));
static void analyze_the_game PARAMS ((Side *side));
static void determine_subgoals PARAMS ((Side *side));
static void review_theaters PARAMS ((Side *side));
static void create_initial_theaters PARAMS ((Side *side));
static Theater *create_theater PARAMS ((Side *side));
static void remove_theater PARAMS ((Side *side, Theater *theater));
static void remove_small_theaters PARAMS ((Side *side));
static void compute_theater_bounds PARAMS ((Side *side));
static void review_goals PARAMS ((Side *side));
static void review_units PARAMS ((Side *side));
static void update_side_strategy PARAMS ((Side *side));
static void decide_theater_needs PARAMS ((Side *side, Theater *theater));
static void update_unit_plans PARAMS ((Side *side));
static void update_unit_plans_randomly PARAMS ((Side *side));
static void estimate_strengths PARAMS ((Side *side));
static void decide_resignation PARAMS ((Side *side));
static void give_up PARAMS ((Side *side));
static void add_goal PARAMS ((Side *side, Goal *goal));
static Goal *has_goal PARAMS ((Side *side, GoalType goaltype));
static Goal *has_unsatisfied_goal PARAMS ((Side *side, GoalType goaltype));
static void mplayer_decide_plan PARAMS ((Side *side, Unit *unit));
static int need_this_type_to_explore PARAMS ((Side *side, int u));
/* static int compare_weights PARAMS ((struct weightelt *w1, struct weightelt *w2)); */
static void assign_to_exploration PARAMS ((Side *side, Unit *unit));
static void assign_explorer_to_theater PARAMS ((Side *side, Unit *unit, Theater *theater));
static int need_this_type_to_build_explorers PARAMS ((Side *side, int u));
static void assign_to_explorer_construction PARAMS ((Side *side, Unit *unit));
static void assign_to_offense PARAMS ((Side *side, Unit *unit));
static void assign_to_offense_support PARAMS ((Side *side, Unit *unit));
static int type_can_build_attackers PARAMS ((Side *side, int u));
static int mplayer_preferred_build_type PARAMS ((Side *side, Unit *unit, int plantype));
static int select_by_weight PARAMS ((int *arr, int numvals));
static int need_more_transportation PARAMS ((Side *side));
static void assign_to_defense_support PARAMS ((Side *side, Unit *unit));
static int assign_to_research_on PARAMS ((Side *side, Unit *unit, int u2));
static int can_research_on PARAMS ((int u, int u2));
static int needs_research PARAMS ((Side *side, int u));
static int mplayer_planning_to_capture PARAMS ((Side *side, int u, int x, int y));
static int mplayer_guide_explorer PARAMS ((Side *side, Unit *unit));
static int probably_explorable PARAMS ((Side *side, int x, int y, int u));
static int build_base_for_self PARAMS ((Side *side, Unit *unit));
static int build_base_for_others PARAMS ((Side *side, Unit *unit));
static int build_depot_for_self PARAMS ((Side *side, Unit *unit));
static void mplayer_react_to_action_result PARAMS ((Side *side, Unit *unit, int rslt));
static void mplayer_react_to_task_result PARAMS ((Side *side, Unit *unit, Task *task, TaskOutcome rslt));
static void change_to_adjacent_theater PARAMS ((Side *side, Unit *unit));
static int desired_direction_impassable PARAMS ((Unit *unit, int x, int y));
static int could_be_ferried PARAMS ((Unit *unit, int x, int y));
static int carryable PARAMS ((int u));
static int accelerable PARAMS ((int u));
static int blocked_by_enemy PARAMS ((Unit *unit, int x, int y));
static void attack_blockage PARAMS ((Side *side, Unit *unit, int x, int y));
static void mplayer_react_to_new_side PARAMS ((Side *side, Side *side2));
static void mplayer_analyze_after_moves PARAMS ((Side *side, int numacted));
static void mplayer_finish_movement PARAMS ((Side *side));
static Unit *search_for_available_transport PARAMS ((Unit *unit));
static void rethink_plan PARAMS ((Unit *unit));
static int enemy_close_by PARAMS ((Side *side, Unit *unit, int dist, int *xp, int *yp));
static void mplayer_receive_message PARAMS ((Side *side, Side *sender, char *str));
static char *mplayer_at_desig PARAMS ((Side *side, int x, int y));
static int mplayer_theater_at PARAMS ((Side *side, int x, int y));
static int mplayer_read_strengths PARAMS ((Side *side));
static Obj *mplayer_save_state PARAMS ((Side *side));

static void mplayer_react_to_unit_loss PARAMS ((Side *side, Unit *unit));

static int compare_weights PARAMS ((const void *w1, const void *w2));

/* This is the set of operations that generic code will use. */

AI_ops mplayer_ops = {
    "mplayer",			/* name */
    NULL,			/* to_test_compat */		
    mplayer_init,		/* to_init */
    mplayer_init_turn,		/* to_init_turn */
    mplayer_decide_plan,	/* to_decide_plan */
    mplayer_react_to_unit_loss,	/* to_react_to_unit_loss */
    mplayer_react_to_action_result,	/* to_react_to_action_result */
    mplayer_react_to_task_result,	/* to_react_to_task_result */
    mplayer_react_to_new_side,	/* to_react_to_new_side */
    mplayer_planning_to_capture,	/* planning_to_capture */
    mplayer_guide_explorer,	/* to_guide_explorer */
    mplayer_preferred_build_type,	/* preferred_build_type */
    mplayer_analyze_after_moves,	/* to_analyze_after_moves */
    mplayer_finish_movement,	/* to_finish_movement */
    mplayer_receive_message,	/* to_receive_message */
    mplayer_save_state,		/* to_save_state */
    mplayer_theater_at,		/* region_at */
    mplayer_at_desig,		/* at_desig */
    -1				/* dummy */
};

/* Flag to detect when shared mplayer init has been done. */

static int mplayerinited = FALSE;

static Theater *tmptheater;

static Side *anewside;

/* Determine game type from name of included modules. */

static GameClass
find_game_class()
{
    Module *m;

    for_all_modules(m) {
	if (strcmp(m->name, "time") == 0
	    || (m->origmodulename && strcmp(m->origmodulename, "time") == 0))
	  return gc_time;
    }

    return gc_standard;
}

static void
mplayer_init(side)
Side *side;
{
    Unit *unit;

    if (game_class == gc_none) {
	game_class = find_game_class();
    }

    /* (should do this only when absolutely needed - mplayer might
       never actually be used) */
    if (!mplayerinited) {
	ai_init_shared();
	mplayerinited = TRUE;
	Dprintf("One mplayer AI is %d bytes.\n", sizeof(Strategy));
    }
    /* Make sure a strategy object exists. */
    if (mplayer(side) == NULL)
      create_strategy(side);
    /* If the side has no units at the moment, it doesn't really need to
       plan. */
    if (!side_has_units(side))
      return;
    /* Compute an initial estimation of units on each side. */
    /* (Needed for save/restore consistency, otherwise not
       critical to do here.) */
    estimate_strengths(side);
    /* Study the scorekeepers and such, decide how to play the game. */
    analyze_the_game(side);
    /* Reset plans of any units that were not doing anything. */
    for_all_side_units(side, unit) {
    	if (in_play(unit) && unit->plan && unit->plan->aicontrol) {
	    unit->plan->asleep = FALSE;
	    unit->plan->reserve = FALSE;
	    /* We might want to mess with the unit now, so clear all
	       delays. */
	    unit->plan->delayed = FALSE;
	    if (unit->plan->waitingfortasks)
	      --(side->numwaiting);
	    unit->plan->waitingfortasks = FALSE;
	    if (unit->plan->type == PLAN_PASSIVE) {
		unit->plan->type = PLAN_NONE;
	    }
    	}
    }
}

/* At the beginning of each turn, make plans and review the situation. */

static void
mplayer_init_turn(side)
Side *side;
{
    int u, u2;

    /* Cases where we no longer need to run. */
    if (!side->ingame)
      return;
    /* A side without units hasn't got anything to do but wait. */
    /* (should account for possible units on controlled sides) */
    if (!side_has_units(side))
      return;
    /* Mplayers in a hacked game will not play,
       unless they're being debugged. */
    if (compromised && !DebugM)
      return;
    update_all_progress_displays("ai turn init start", side->id);
    DMprintf("%s mplayer init turn\n", side_desig(side));
    /* Make sure a strategy object exists. */
    if (mplayer(side) == NULL)
      create_strategy(side);
    /* Look over the game design we're playing with. */
    analyze_the_game(side);

    /* code specific to the "time" game */
    if (game_class == gc_time) {
	for_all_unit_types(u) {
	    if (mplayer(side)->research_status[u] == RS_RESEARCH_ASSIGNED) {
		u2 = mplayer(side)->research_on[u];
		if (!needs_research (side, u2)) {
		    /* research done, start upgrading */
		    DMprintf("%s has completed research on %s\n",
			     side_desig(side), u_type_name(u2));
		    mplayer(side)->research_status[u] = RS_UPGRADE_NEEDED;
		}
	    }
	}
    }

    /* If this game is one that can be won, as opposed to
       just dinking around, figure how to win it. */
    if (mplayer(side)->trytowin) {
	/* Check out the current goal tree first. */
	review_goals(side);
	/* Goal analysis might have triggered resignation. */
	if (!side->ingame)
	  goto done;
	/* Check out all the theaters. */
	review_theaters(side);
	/* Check out all of our units. */
	review_units(side);
	/* Decide on the new current plan. */
	update_side_strategy(side);
	/* Propagate this to individual unit plans. */
	update_unit_plans(side);
    } else {
	update_unit_plans_randomly(side);
    }
  done:
    update_all_progress_displays("", side->id);
    DMprintf("%s mplayer init turn done\n", side_desig(side));
}

/* Create and install an entirely new strategy object for the side. */

static void
create_strategy(side)
Side *side;
{
    Strategy *strategy = (Strategy *) xmalloc(sizeof(Strategy));

    /* Put the specific structure into a generic slot. */
    side->ai = (struct a_ai *) strategy;
    strategy->type = mplayertype;
    /* Allocate a table of pointers to theaters, for access via small numbers
       rather than full pointers. */
    strategy->theatertable = (Theater **) xmalloc(127 * sizeof(Theater *));
    /* Allocate a layer of indexes into the theater table. */
    strategy->areatheaters = malloc_area_layer(char);
    /* Allocate random things. */
    /* Arrays for unit types. */
    strategy->actualmix = (short *) xmalloc(numutypes * sizeof(short));
    strategy->expectedmix = (short *) xmalloc(numutypes * sizeof(short));
    strategy->idealmix = (short *) xmalloc(numutypes * sizeof(short));
    strategy->research_status = (short *) xmalloc(numutypes * sizeof(short));
    strategy->research_on     = (short *) xmalloc(numutypes * sizeof(short));
    /* Arrays for terrain types. */
    strategy->terrainguess = (short *) xmalloc(numttypes * sizeof(short));
    strategy->writable_state = lispnil;
    /* Set everything to correct initial values. */
    reset_strategy(side);
}

/* Put all the right initial values into the strategy, but don't allocate anything. */

static void
reset_strategy(side)
Side *side;
{
    int u, u2, t, dir;
    Strategy *strategy = (Strategy *) side->ai;

    /* Remember when we did this. */
    strategy->creationdate = g_turn();
    /* Null out various stuff. */
    strategy->numgoals = 0;
    strategy->theaters = NULL;
    /* Actually we start with no theaters, but it's convenient to leave entry 0
       in the theater table pointing to NULL. */
    strategy->numtheaters = 1;
    /* Clear pointers to special-purpose theaters. */
    strategy->homefront = NULL;
    for_all_directions(dir) {
    	strategy->perimeters[dir] = NULL;
    	strategy->midranges[dir] = NULL;
    	strategy->remotes[dir] = NULL;
    }
    strategy->explorersneeded = 0;
    /* Reset the summation of our exploration needs. */
    for_all_unit_types(u) {
	strategy->actualmix[u] = 0;
	strategy->expectedmix[u] = 0;
	strategy->idealmix[u] = 0;
	strategy->research_status[u] = 0;
	strategy->research_on[u] = 0;

	/* code specific to the "time" game */
	if (game_class == gc_time) {
	    for_all_unit_types(u2) {
		if (needs_research (side, u2) && can_research_on(u, u2)) {
		    strategy->research_status[u] = RS_RESEARCH_NEEDED;
		    strategy->research_on[u] = u2;
		    DMprintf("%s can research on %s (to level %d)\n",
			     u_type_name(u), u_type_name(u2),
			     u_tech_to_build(u2));
		}
	    }
	}
    }
    for_all_terrain_types(t) {
	strategy->terrainguess[t] = 0;
    }
    strategy->analyzegame = TRUE;
    /* Analyze the game and decide our basic goals. */
    analyze_the_game(side);
}

/* Look over the game design and decide what we're supposed to be doing,
   if anything at all.  This just sets up toplevel goals based on the
   game design, does not evaluate goals or any such. */

static void
analyze_the_game(side)
Side *side;
{
    int maybedraw, i;
    Goal *goal;

    if (mplayer(side)->analyzegame) {
	if (should_try_to_win(side)) {
	    mplayer(side)->trytowin = TRUE;
	    /* This is our whole purpose in the game. */
	    goal = create_goal(GOAL_WON_GAME, side, TRUE);
	    add_goal(side, goal);
	    /* Now figure what exactly we have to do in order to win. */
	    determine_subgoals(side);
	    /* Machine will want to keep playing as long as it thinks
	       it has a chance to win. */
	    maybedraw = FALSE;
	} else {
	    mplayer(side)->trytowin = FALSE;
	    /* Since the side is not trying to win anything, it will be
	       pretty laidback about whether to keep the game going. */
	    maybedraw = TRUE;
	}
	/* Be trusting about game saves, at least for now. (The problem
	   is that a human player could escape fate by saving the game
	   and then either editing the saved game or just throwing it
	   away.) */
	set_willing_to_save(side, TRUE);
	set_willing_to_draw(side, maybedraw);
	mplayer(side)->analyzegame = FALSE;
	/* Summarize our analysis of this game. */
	DMprintf("%s will try to %s this game\n",
		 side_desig(side),
		 mplayer(side)->trytowin ? "win" : "have fun in");
	for (i = 0; i < mplayer(side)->numgoals; ++i) {
	    goal = mplayer(side)->goals[i];
	    DMprintf("%s has %s\n", side_desig(side), goal_desig(goal));
	}
    }
}

static void
determine_subgoals(side)
Side *side;
{
    int numvicgoals;
    Unit *unit;
    Side *side2;
    Scorekeeper *sk;
    Goal *goal;

    /* Look at each scorekeeper and decide on appropriate goals. */
    for_all_scorekeepers(sk) {
        if (match_keyword(sk->body, K_LAST_SIDE_WINS)) {
	    /* We want to "kick butt" - *everybody* else's butt. */
	    for_all_sides(side2) {
		if (!trusted_side(side, side2) && side2->ingame) {
		    /* Our goals include preventing other sides from accomplishing
		       theirs. */
		    goal = create_goal(GOAL_WON_GAME, side2, FALSE);
		    add_goal(side, goal);
		    /* (should add "search-and-destroy" as corollaries) */
		}
	    }
	    /* Add goals to protect our own units. */
	    numvicgoals = 0;
	    for_all_side_units(side, unit) {
		if (point_value(unit) > 0  /* (should be "n most valuable") */
		    && in_play(unit)
		    && numvicgoals < 10) {
		    goal = create_goal(GOAL_VICINITY_HELD, side, TRUE);
		    goal->args[0] = unit->x;  goal->args[1] = unit->y;
		    goal->args[2] = goal->args[3] = 2;
		    add_goal(side, goal);
		    ++numvicgoals;
		}
	    }
        } else {
	    DMprintf("Don't understand a scorekeeper!");
        }
    }
    /* We might develop a sudden interest in exploration. */
    /* (but should only be if information is really important to winning) */
    if (!all_see_all) {
	if (!g_terrain_seen()) {
	    add_goal(side, create_goal(GOAL_WORLD_KNOWN, side, TRUE));
	}
	/* It will be important to keep track of other sides' units
	   as much as possible. */
	for_all_sides(side2) {
	    if (side != side2) {
		goal = create_goal(GOAL_POSITIONS_KNOWN, side, TRUE);
		goal->args[0] = (long) side2;
		add_goal(side, goal);
	    }
	}
	/* Also add the general goal of knowing where indeps are. */
	goal = create_goal(GOAL_POSITIONS_KNOWN, side, TRUE);
	goal->args[0] = (long) NULL;
	add_goal(side, goal);
    }
}

/* Do a combination of analyzing existing theaters and creating new ones. */

static void
review_theaters(side)
Side *side;
{
    int x, y, u, s, pop, totnumunits;
    int firstcontact = FALSE;
    int homefound = FALSE;
    short view;
    Unit *unit;
    Side *firstcontactside, *homefoundside, *otherside, *side2;
    Theater *theater;

    /* Create some theaters if none exist. */
    if (mplayer(side)->theaters == NULL) {
	create_initial_theaters(side);
    }
    for_all_theaters(side, theater) {
	theater->allied_units = 0;
	theater->makers = 0;
	theater->unexplored = 0;
	theater->border = FALSE;
	theater->allied_bases = 0;
	for_all_unit_types(u) {
	    theater->numassigned[u] = 0;
	    theater->numneeded[u] = 0;
	    theater->numenemies[u] = 0;
	    theater->numsuspected[u] = theater->numsuspectedmax[u] = 0;
	    theater->numtotransport[u] = 0;
	}
	if (people_sides_defined()) {
	    for (s = 0; s <= numsides; ++s)
	      theater->people[s] = 0;
	}
	theater->units_lost /= 2;
    }
    compute_theater_bounds(side);
    /* Now look at all the units that we can. */
    for_all_side_units(side, unit) {
    	if (in_play(unit)) {
	    theater = unit_theater(unit);
	    if (theater != NULL) {
		++(theater->allied_units);
		(theater->numassigned[unit->type])++;
		if (isbase(unit))
		  theater->allied_bases++;
	    }
	}
    }
    /* (should also analyze allies etc) */
    /* Now look at the whole world. */
    for_all_interior_cells(x, y) {
	theater = theater_at(side, x, y);
	if (theater != NULL) {
	    if (can_see_actual_units(side, x, y)) {
	    	for_all_stack(x, y, unit) {
	    	    /* what about occupants? */
	    	    if (in_play(unit)
	    	    	&& !trusted_side(side, unit->side)
	    	    	&& (!indep(unit)
	    	    	    || u_point_value(unit->type) > 0)) {
			if (enemy_side(side, unit->side))
			  ++(theater->numenemies[unit->type]);
	    	    	if (mplayer(side)->contacted[side_number(unit->side)] == 0) {
			    mplayer(side)->contacted[side_number(unit->side)] = 1;
			    if (!indep(unit)) {
				firstcontact = TRUE;
				firstcontactside = unit->side;
			    }
	    	    	}
	    	    	if (mplayer(side)->homefound[side_number(unit->side)] == 0
	    	    	    && !mobile(unit->type)) {
			    mplayer(side)->homefound[side_number(unit->side)] = 1;
			    if (!indep(unit)) {
				homefound = TRUE;
				homefoundside = unit->side;
			    }
	    	    	}
	    	    }
	    	}
		if (people_sides_defined()) {
		    pop = people_side_at(x, y);
		    if (pop != NOBODY) {
			++(theater->people[pop]);
	    	    	if (mplayer(side)->homefound[pop] == 0) {
			    mplayer(side)->homefound[pop] = 1;
			    if (pop != 0) {
				homefound = TRUE;
				homefoundside = side_n(pop);
			    }
	    	    	}
		    }
		}
	    } else {
		if (terrain_view(side, x, y) == UNSEEN) {
		    ++(theater->unexplored);
		} else {
		    view = unit_view(side, x, y);
		    if (view != EMPTY) {
			side2 = side_n(vside(view));
			if (side2 == NULL) {
			    u = vtype(view);
			    /* (should rate by value of capture) */
			    if (u_point_value(u) > 0) {
				++(theater->numsuspected[u]);
				++(theater->numsuspectedmax[u]);
			    }
			} else if (enemy_side(side, side2)) {
			    u = vtype(view);
			    if (u_point_value(u) > 0) {
				++(theater->numsuspected[u]);
				++(theater->numsuspectedmax[u]);
			    }
			}
		    }
		    if (people_sides_defined()) {
			pop = people_side_at(x, y);
			if (pop != NOBODY) {
			    ++(theater->people[pop]);
			}
		    }
		}
	    }
	}
    }
    for_all_theaters(side, theater) {
    	theater->x = (theater->xmin + theater->xmax) / 2;
    	theater->y = (theater->ymin + theater->ymax) / 2;
    	theater->enemystrengthmin = theater->enemystrengthmax = 0;
    	for_all_unit_types(u) {
	    theater->enemystrengthmin +=
	      theater->numenemies[u] + theater->numsuspected[u];
	}
	theater->enemystrengthmax = theater->enemystrengthmin;
    }
    if (firstcontact || homefound) {
    	for_all_side_units(side, unit) {
	    if (unit->plan && unit->plan->aicontrol) {
		unit->plan->maingoal = NULL;
		unit->plan->formation = NULL;
		unit->plan->funit = NULL;
		/* Force a replan. */
		unit->plan->type = PLAN_NONE;
		unit->plan->asleep = FALSE;
		unit->plan->reserve = FALSE;
		if (unit->plan->waitingfortasks)
		  --(side->numwaiting);
		unit->plan->waitingfortasks = FALSE;
		set_unit_theater(unit, NULL);
		update_unit_display(side, unit, TRUE);
	    }
	}
    }
    for_all_theaters(side, theater) {
	DMprintf("%s theater \"%s\" at %d,%d from %d,%d to %d,%d (size %d)\n",
		 side_desig(side), theater->name, theater->x, theater->y,
		 theater->xmin, theater->ymin, theater->xmax, theater->ymax,
		 theater->size);
	/* Summarize what we know about the theater. */
	DMprintf("%s theater \"%s\"", side_desig(side), theater->name);
	if (!all_see_all && theater->unexplored > 0) {
	    DMprintf(" unexplored %d", theater->unexplored);
	}
	DMprintf(" enemy %d", theater->enemystrengthmin);
	if (theater->enemystrengthmin != theater->enemystrengthmax) {
	    DMprintf("-%d", theater->enemystrengthmax);
	}
	for_all_unit_types(u) {
	    if (theater->numenemies[u] + theater->numsuspected[u] > 0) {
	    	DMprintf(" %3s %d", u_type_name(u), theater->numenemies[u]);
	    	if (theater->numsuspected[u] > 0) {
		    DMprintf("+%d", theater->numsuspected[u]);
		}
	    }
	}
	if (people_sides_defined()) {
	    DMprintf(" people");
	    for (s = 0; s <= numsides; ++s) {
		if (theater->people[s] > 0) {
		    DMprintf(" s%d %d", s, theater->people[s]);
		}
	    }
	}
	DMprintf("\n");
	totnumunits = 0;
	for_all_unit_types(u) {
	    totnumunits +=
	      (theater->numassigned[u] + theater->numneeded[u] + theater->numtotransport[u]);
	}
	if (totnumunits > 0) {
	    /* Summarize the status of our own units in this theater. */
	    DMprintf("%s theater \"%s\" has ", side_desig(side), theater->name);
	    for_all_unit_types(u) {
		if (theater->numassigned[u] + theater->numneeded[u] + theater->numtotransport[u] > 0) {
		    DMprintf(" %d %3s", theater->numassigned[u], u_type_name(u));
			if (theater->numneeded[u] > 0) {
			    DMprintf(" (of %d needed)", theater->numneeded[u]);
			}
			if (theater->numtotransport[u] > 0) {
			    DMprintf(" (%d awaiting transport)", theater->numtotransport[u]);
			}
		}
	    }
	    DMprintf("\n");
	}
    }
    /* Also summarize contacts. */
    for_all_sides(otherside) {
    	if (otherside != side) {
	    if (mplayer(side)->contacted[otherside->id]) {
		DMprintf("%s contacted s%d", side_desig(side), otherside->id);
		if (mplayer(side)->homefound[otherside->id]) {
		    DMprintf(", home found");
		}
		DMprintf("\n");
	    }
    	}
    }
}

/* Set up the initial set of theaters. */

static void
create_initial_theaters(side)
Side *side;
{
    int x, y, dir, dist, i, j;
    int xmin, ymin, xmax, ymax;
    int homeradius, perimradius, midradius, xxx;
    int numthx, numthy, thwid, thhgt;
    Unit *unit;
    Theater *homefront, *enemyarea, *theater;
    Theater *gridtheaters[8][8];
    Strategy *strategy = mplayer(side);
    
    for (i = 0; i < 8; ++i) {
	for (j = 0; j < 8; ++j) {
	    gridtheaters[i][j] = NULL;
	}
    }
    /* Compute bbox of initial (should also do enemy?) units. */
    xmin = area.width;  ymin = area.height;  xmax = ymax = 0;
    for_all_side_units(side, unit) {
	if (alive(unit) /* and other preconditions? */) {
	    if (unit->x < xmin)
	      xmin = unit->x;
	    if (unit->y < ymin)
	      ymin = unit->y;
	    if (unit->x > xmax)
	      xmax = unit->x;
	    if (unit->y > ymax)
	      ymax = unit->y;
	}
    }
    /* Most games start with each side's units grouped closely together.
       If this is not the case, do something else. */
    if (xmax - xmin > area.width / 4 && ymax - ymin > area.height / 4) {
	/* (should do some sort of clustering of units) */
	if (0 /*people_sides_defined()*/) {
	    homefront = create_theater(side);
	    homefront->name = "Home Front";
	    enemyarea = create_theater(side);
	    enemyarea->name = "Enemy Area";
	    for_all_interior_cells(x, y) {
	        if (people_side_at(x, y) == side->id) {
		    set_theater_at(side, x, y, homefront);
	        } else {
		    set_theater_at(side, x, y, enemyarea);
	        }
	    }
	} else {
	    /* Divide the world up along a grid. */
	    numthx = (area.width  > 60 ? (area.width  > 120 ? 7 : 5) : 3);
	    numthy = (area.height > 60 ? (area.height > 120 ? 7 : 5) : 3);
	    thwid = max(8, area.width / numthx);
	    thhgt = max(8, area.height / numthy);
	    for_all_interior_cells(x, y) {
		i = x / thwid;  j = y / thhgt;
		if (gridtheaters[i][j] == NULL) {
		    theater = create_theater(side);
		    sprintf(spbuf, "Grid %d,%d", i, j);
		    theater->name = copy_string(spbuf);
		    theater->x = x;  theater->y = y;
		    gridtheaters[i][j] = theater;
		} else {
		    theater = gridtheaters[i][j];
		}
		set_theater_at(side, x, y, theater);
	    }
	}
	return;
    } else {
	/* Always create a first theater that covers the starting area. */
	homefront = create_theater(side);
	homefront->name = "Home Front";
	/* Calculate startxy if not already available. */
	if (side->startx < 0 && side->starty < 0)
	  calc_start_xy(side);
	homefront->x = side->startx;  homefront->y = side->starty;
	strategy->homefront = homefront;
	homeradius = max(5, g_radius_min());
	perimradius = max(homeradius + 5, g_separation_min() - homeradius);
	midradius = max(perimradius + 10, g_separation_min() * 2);
	xxx = max((side->startx - perimradius), (area.width - side->startx - perimradius));
	xxx /= 2;
	midradius = min(midradius, perimradius + xxx);
	for_all_interior_cells(x, y) {
	    if (people_sides_defined()
		&& people_side_at(x, y) == side->id) {
		set_theater_at(side, x, y, homefront);
	    } else {
		dist = distance(x, y, side->startx, side->starty);
		if (dist < homeradius) {
		    set_theater_at(side, x, y, homefront);
		} else {
		    dir = approx_dir(x - side->startx, y - side->starty);
		    if (dist < perimradius) {
			if (strategy->perimeters[dir] == NULL) {
			    theater = create_theater(side);
			    sprintf(spbuf, "Perimeter %s", dirnames[dir]);
			    theater->name = copy_string(spbuf);
			    theater->x = x;  theater->y = y;
			    strategy->perimeters[dir] = theater;
			} else {
			    theater = strategy->perimeters[dir];
			}
		    } else if (dist < midradius) {
			if (strategy->midranges[dir] == NULL) {
			    theater = create_theater(side);
			    sprintf(spbuf, "Midrange %s", dirnames[dir]);
			    theater->name = copy_string(spbuf);
			    theater->x = x;  theater->y = y;
			    strategy->midranges[dir] = theater;
			} else {
			    theater = strategy->midranges[dir];
			}
		    } else {
			if (strategy->remotes[dir] == NULL) {
			    theater = create_theater(side);
			    sprintf(spbuf, "Remote %s", dirnames[dir]);
			    theater->name = copy_string(spbuf);
			    theater->x = x;  theater->y = y;
			    strategy->remotes[dir] = theater;
			} else {
			    theater = strategy->remotes[dir];
			}
		    }
		    set_theater_at(side, x, y, theater);
	    	}
	    }
	}  
    }
    remove_small_theaters(side);
    /* Assign all units to the theater they're currently in. */
    /* (how do reinforcements get handled? mplayer should get hold of perhaps) */
    for_all_side_units(side, unit) {
	if (in_play(unit) /* and other preconditions? */) {
	    set_unit_theater(unit, theater_at(side, unit->x, unit->y));
	}
    }
}

/* Create a single theater object and link it into the list of
   theaters. */

/* (should be able to re-use theaters in already in theater table) */

static Theater *
create_theater(side)
Side *side;
{
    Theater *theater = (Theater *) xmalloc(sizeof(Theater));
    
    if (mplayer(side)->numtheaters > MAXTHEATERS)
      return NULL;
    theater->id = (mplayer(side)->numtheaters)++;
    theater->name = "?";
    theater->maingoal = NULL;
    theater->people = (int *) xmalloc ((numsides + 1) * sizeof(int));
    /* (should alloc other array slots too) */
    /* Connect theater into a linked list. */
    theater->next = mplayer(side)->theaters;
    mplayer(side)->theaters = theater;
    /* Install it into the theater table also. */
    mplayer(side)->theatertable[theater->id] = theater;
    return theater;
}

/* Clear all references to the theater and remove it from the list.
   Note that the theater size must already be zero. */

static void
remove_theater(side, theater)
Side *side;
Theater *theater;
{
    int dir;
    Theater *prev;

    if (mplayer(side)->homefront == theater)
      mplayer(side)->homefront = NULL;
    for_all_directions(dir) {
	if (mplayer(side)->perimeters[dir] == theater)
	  mplayer(side)->perimeters[dir] = NULL;
	if (mplayer(side)->midranges[dir] == theater)
	  mplayer(side)->midranges[dir] = NULL;
	if (mplayer(side)->remotes[dir] == theater)
	  mplayer(side)->remotes[dir] = NULL;
    }
    if (mplayer(side)->theaters == theater)
      mplayer(side)->theaters = theater->next;
    else {
	prev = NULL;
	for_all_theaters(side, prev) {
	    if (prev->next == theater) {
		prev->next = theater->next;
		break;
	    }
	}
	/* If prev still null, badness */
    }
    --(mplayer(side)->numtheaters);
}

static void
move_theater_cell(x, y)
int x, y;
{
    int dir, x1, y1;
    Theater *theater2;

    if (theater_at(tmpside, x, y) == tmptheater) {
	for_all_directions(dir) {
	    if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
		theater2 = theater_at(tmpside, x1, y1);
		if (theater2 != NULL && theater2 != tmptheater) {
		    set_theater_at(tmpside, x, y, theater2);
		    ++(theater2->size);
		    /* (should recompute bbox too) */
		    --(tmptheater->size);
		}
	    }
	}
    }
}

static void
remove_small_theaters(side)
Side *side;
{
    int domore;
    Theater *theater;

    compute_theater_bounds(side);
    domore = TRUE;
    while (domore) {
	domore = FALSE;
	for_all_theaters(side, theater) {
	    if (between(1, theater->size, 5)) {
		tmpside = side;
		tmptheater = theater;
		apply_to_area(theater->x, theater->y, 6, move_theater_cell);
		if (theater->size == 0) {
		    remove_theater(side, theater);
		    /* Have to start over now. */
		    domore = TRUE;
		    break;
		}
	    }
	}
    }
    /* Redo, many random changes to bounds. */
    compute_theater_bounds(side);
}

/* Compute the size and bounding box of each theater.  This should be run
   each time theaters change in size or shape. */

static void
compute_theater_bounds(side)
Side *side;
{
    int x, y;
    Theater *theater;

    for_all_theaters(side, theater) {
	theater->size = 0;
	theater->xmin = theater->ymin = -1;
	theater->xmax = theater->ymax = -1;
    }
    for_all_interior_cells(x, y) {
	theater = theater_at(side, x, y);
	if (theater != NULL) {
	    ++(theater->size);
	    /* Compute bounding box of theater if not already done. */
	    if (theater->xmin < 0 || x < theater->xmin)
	      theater->xmin = x;
	    if (theater->ymin < 0 || y < theater->ymin)
	      theater->ymin = y;
	    if (theater->xmax < 0 || x > theater->xmax)
	      theater->xmax = x;
	    if (theater->ymax < 0 || y > theater->ymax)
	      theater->ymax = y;
	}
    }
}

/* Examine the goals to see what has been accomplished and what still needs
   to be done. */

static void
review_goals(side)
Side *side;
{
    int i;
    Scorekeeper *sk;
    Goal *goal;
    Side *side2;
    Strategy *strategy = mplayer(side);

    /* First check on our friends and enemies. */
    for_all_sides(side2) {
	/* If they're not trusting us, we don't want to trust them. */
	/* (should be able to update this immediately after other side changes trust) */
	if (!trusted_side(side2, side))
	  set_trust(side, side2, FALSE);
    }
    for (i = 0; i < strategy->numgoals; ++i) {
	goal = strategy->goals[i];
	DMprintf("%s has %s\n", side_desig(side), goal_desig(goal));
    }
    /* Should look at certainty of each goal and decide whether to keep or
       drop it, and mention in debug output also. */
    /* Also think about resigning. */
    if (keeping_score()) {
	for_all_scorekeepers(sk) {
	    if (symbolp(sk->body)
		&& match_keyword(sk->body, K_LAST_SIDE_WINS)) {
		decide_resignation(side);
	    }
	}
    }
}

static void
estimate_strengths(side)
Side *side;
{
    int u, sn1, x, y, uview;
    Side *side1, *side2;
    Strategy *strategy = mplayer(side);
    Unit *unit;
 
    for_all_sides(side1) {
	sn1 = side_number(side1);
	for_all_unit_types(u) {
	    strategy->strengths[sn1][u] = 0;
	}
	/* this lets us count even semi-trusted allies' units accurately... */
	if (side1 == side || allied_side(side, side1)) {
	    for_all_side_units(side1, unit) {
		/* Note that we count off-area units, since they are reinforcements
		   usually. */
		if (alive(unit) && completed(unit)) {
		    ++(strategy->strengths[sn1][unit->type]);
		}
	    }
	}
    }
    if (all_see_all) {
	/* If we can see everything, we can add up units accurately. */
	for_all_cells(x, y) {
	    for_all_stack(x, y, unit) {
		side2 = unit->side;
		if (side2 != NULL && !(side2 == side || allied_side(side, side2))) {
		    if (completed(unit)) {
		        ++(strategy->strengths[side2->id][unit->type]);
		    }
		}
	    }
	}
    } else {
	/* Look at the current view to get enemy strength. */
	/* This is too easily faked, and doesn't know about hiding units... */
	/* Should also discount old data. */
	for_all_cells(x, y) {
	    uview = unit_view(side, x, y);
	    if (uview != UNSEEN && uview != EMPTY) {
		side2 = side_n(vside(uview));
		/* Count only units on other sides. */
		if (side2 != NULL && !(side2 == side || allied_side(side, side2))) {
		    ++(strategy->strengths[side2->id][vtype(uview)]);
		}
	    }
	}
    }
    /* Estimate point values. */
    /* (should try to account for individual units with special point values) */
    for_all_sides(side1) {
	sn1 = side1->id;
	strategy->points[sn1] = 0;
	for_all_unit_types(u) {
	    strategy->points[sn1] +=
	      strategy->strengths[sn1][u] * u_point_value(u);
	}
    }
    /* Estimate point values, and how many of each type in allied group. */
    for_all_sides(side1) {
	sn1 = side1->id;
	for_all_unit_types(u) {
	    strategy->alstrengths[sn1][u] = strategy->strengths[sn1][u];
	    for_all_sides(side2) {
		if (side1 != side2 && allied_side(side1, side2)) {
		    strategy->alstrengths[sn1][u] +=
		      strategy->strengths[side2->id][u];
		}
	    }
	}
	strategy->alpoints[sn1] = strategy->points[sn1];
	for_all_sides(side2) {
	    if (side1 != side2 && allied_side(side1, side2)) {
		strategy->alpoints[sn1] += strategy->points[side2->id];
	    }
	}
    }
    if (!strategy->initial_strengths_computed) {
	if (!mplayer_read_strengths(side)) {
	    for_all_sides(side1) {
		sn1 = side1->id;
		strategy->points0[sn1] = strategy->points[sn1];
		strategy->alpoints0[sn1] = strategy->alpoints[sn1];
		for_all_unit_types(u) {
		    strategy->strengths0[sn1][u] =
		      strategy->strengths[sn1][u];
		    strategy->alstrengths0[sn1][u] =
		      strategy->alstrengths[sn1][u];
		}
	    }
	}
	ai_save_state(side);
	strategy->initial_strengths_computed = TRUE;
    }
    /* If we're calling strength estimation because a new side has
       come into existence, use the current strengths as the new
       side's initial strengths. */
    if (anewside != NULL) {
	sn1 = anewside->id;
	strategy->points0[sn1] = strategy->points[sn1];
	strategy->alpoints0[sn1] = strategy->alpoints[sn1];
	for_all_unit_types(u) {
	    strategy->strengths0[sn1][u] =
	      strategy->strengths[sn1][u];
	    strategy->alstrengths0[sn1][u] =
	      strategy->alstrengths[sn1][u];
	}
	/* Have to redo the saveable state also; force this by blasting any
	   existing recordable state (it should all be re-creatable from
	   mplayer's internal state). */
	strategy->writable_state = lispnil;
	ai_save_state(side);
    }
    /* Dump out a detailed listing of our estimates. */
    if (DebugM) {
	for_all_sides(side1) {
	    sn1 = side1->id;
	    DMprintf("%s ", side_desig(side));
	    DMprintf("est init streng of %s: ", side_desig(side1));
	    for_all_unit_types(u) {
		DMprintf(" %d", strategy->strengths0[sn1][u]);
	    }
	    DMprintf(" (%d points)\n", strategy->points0[sn1]);
	    DMprintf("%s ", side_desig(side));
	    DMprintf("est curr streng of %s: ", side_desig(side1));
	    for_all_unit_types(u) {
		DMprintf(" %d", strategy->strengths[sn1][u]);
	    }
	    DMprintf(" (%d points)\n", strategy->points[sn1]);
	    DMprintf("%s ", side_desig(side));
	    DMprintf("est init allied of %s: ", side_desig(side1));
	    for_all_unit_types(u) {
		DMprintf(" %d", strategy->alstrengths0[sn1][u]);
	    }
	    DMprintf(" (%d points)\n", strategy->alpoints0[sn1]);
	    DMprintf("%s ", side_desig(side));
	    DMprintf("est curr allied of %s: ", side_desig(side1));
	    for_all_unit_types(u) {
		DMprintf(" %d", strategy->alstrengths[sn1][u]);
	    }
	    DMprintf(" (%d points)\n", strategy->alpoints[sn1]);
	}
    }
}

/* Sometimes there is no point in going on, but be careful not to be too
   pessimistic.  Right now we only give up if no hope at all.  Currently
   this is only used if there is a last-side-wins scorekeeper; it would
   need to be modified considerably to be useful with scorekeepers in
   general. */

static void
decide_resignation(side)
Side *side;
{
    int ratio, ratio0, chance = 0, sn1;
    Side *side1;
    Strategy *strategy = mplayer(side);

    estimate_strengths(side);
    /* If our estimate of our own points is zero, then we're about to lose the
       game anyway, so just return and avoid screwing up ratio calcs below. */
    if (strategy->alpoints[side->id] <= 0)
      return;
    for_all_sides(side1) {
	if (side != side1 && side1->ingame && !allied_side(side, side1)) {
	    sn1 = side1->id;
	    /* Note that ratio calculations always scale up by 100, so that we
	       can do finer comparisons without needing floating point. */
	    ratio = (strategy->alpoints[sn1] * 100) / strategy->alpoints[side->id];
	    /* code specific to the "time" game */
	    /* the mplayer can severely underestimates its own strength */
	    if (game_class == gc_time) {
		ratio /= 3;
	    }

	    if (strategy->alpoints0[side->id] > 0) {
		ratio0 = (strategy->alpoints0[sn1] * 100) / strategy->alpoints0[side->id];
		/* If we estimated 0 points for some side's initial strength,
		   then our estimate is bad; assume parity. */
		if (ratio0 <= 0)
		  ratio0 = 100;
		/* This formula basically calls for no resignation if ratio is no more
		   than twice what it was initially, 50% chance if ratio is four times
		   what it was (if we started out even, then we're outnumbered 4 to 1),
		   and interpolates for ratios in between. */
		chance = (((ratio * 100) / ratio0) - 200) / 5;
		chance = max(chance, 0);
		chance = min(chance, 90);
	    } else {
		/* work by absolute ratios */
		if (ratio > 400) {
		    chance = ratio / 10;
		}
		chance = min(chance, 90);
	    }
	}
    }

    /* Whether or not we actually resign, we may be willing to
       go for a draw if other players want to. */
    /* (Note that toggling this flag is not exactly poker-faced
       behavior, but I doubt human players will be able to derive
       much advantage, since they'll already have a pretty good
       idea if the AI is in trouble or not.) */
    set_willing_to_draw(side, (chance > 0));
    /* Maybe resign. */
    if (chance > 0) {
	if (probability(chance)) {
	    give_up(side);
	}
    }
}

/* If an mplayer resigns, it tries to help its friends. */

static void
give_up(side)
Side *side;
{
    Side *side1;

    /* If there is a human player, allow the human to make the final decision. */
    /* (should have a way to delegate this also, at least for testing purposes) */
    if (side_has_display(side)) {
	notify(side, "Your AI mplayer recommends resignation");
	return;
    }
    /* (should have to give units to controlling side if there is one) */
    /* (should controlling side be able to disallow resignation?) */
    /* Try to give away all of our units to an ally. */
    for_all_sides(side1) {
	if (side != side1 && allied_side(side, side1) && side1->ingame) {
	    resign_game(side, side1);
	    return;
	}
    }
    /* (should give to any positively-regarded side?) */
    /* No allies left in game, let everything become independent. */
    /* (should disband all units that can be disbanded?) */
    resign_game(side, NULL);
}

/* Go through all our units (and allied ones?). */

static void
review_units(side)
Side *side;
{
    Unit *unit, *occ, *unit2;
    Plan *plan;
    Theater *oldtheater, *theater;
    int u, u2, cp, cpmin, any;

    /* This code is specific to the "time" game. */
    if (game_class == gc_time) {
	for_all_unit_types(u) {
	    u2 = mplayer(side)->research_on[u];
	    if (mplayer(side)->research_status[u] == RS_RESEARCH_ASSIGNED) {
		/* is anyone researching? */
		unit2 = NULL;
		for_all_side_units(side, unit) {
		    if (unit->type==u && in_play(unit) &&
			unit->plan && unit->plan->aicontrol) {
			if (unit->plan->tasks &&
			    unit->plan->tasks->type == TASK_RESEARCH &&
			    unit->plan->tasks->args[0] == u2) 
			  unit2 = unit;
		    }
		}
		if (unit2 != NULL) {
		    DMprintf("%s is researching for %s on %s (level %d/%d)\n",
			     unit_desig(unit2), side_desig(side),
			     u_type_name(u2),
			     side->tech[u2], u_tech_to_build(u2));
		} else {
		    DMprintf("no %s is researching for %s on %s!\n",
			     u_type_name(u), side_desig(side),
			     u_type_name(u2));
		    mplayer(side)->research_status[u] = RS_RESEARCH_NEEDED;
		}
	    }
	    if (mplayer(side)->research_status[u] == RS_RESEARCH_NEEDED
		&& needs_research (side, u2)) {
		/* pick for research a unit not building; 
		   if all are building, choose the one which started last */
		unit2 = NULL;
		cpmin = 9999;
		any = 0;
		for_all_side_units(side, unit) {
		    if (unit->type == u
			&& in_play(unit)
			&& unit->plan
			&& unit->plan->aicontrol) {
			any = 1;
			cp = 0;
			occ = NULL;
			if ((unit->plan->tasks != NULL
			     && unit->plan->tasks->type == TASK_BUILD))
			  occ = find_unit_to_complete(unit, unit->plan->tasks);
			if (occ != NULL) {
			    cp = occ->cp - uu_creation_cp(u,occ->type);
			    if (uu_cp_per_build(u,u2) > 0)
			      cp /= uu_cp_per_build(u,u2);
			}
			if (cp < cpmin) {
			    unit2 = unit;
			    cpmin = cp;
			}
		    }
		}
		if (unit2 == NULL) {
		    if (any)
		      DMprintf("no %s is available to research for %s on %s!\n",
			       u_type_name(u), side_desig(side),
			       u_type_name(u2));
		} else {
		    if (assign_to_research_on(side, unit2, u2)) {
			mplayer(side)->research_status[u] =
			  RS_RESEARCH_ASSIGNED;
		    }
		}
	    }
	}
    }

    for_all_side_units(side, unit) {
	if (in_play(unit) && unit->plan && unit->plan->aicontrol) {

	    /* code specific to the "time" game */
	    if (game_class == gc_time) {
		u = unit->type;
		u2 = mplayer(side)->research_on[u];

		/* should we upgrade? */
		if (mplayer(side)->research_status[u] == RS_UPGRADE_NEEDED) {
		    cp = 0;
		    occ = NULL;
		    if ((unit->plan->tasks != NULL &&
			 unit->plan->tasks->type == TASK_BUILD))
		      occ = find_unit_to_complete(unit, unit->plan->tasks);
		    if (occ != NULL) {
			cp = occ->cp - uu_creation_cp(u,occ->type);
			if (uu_cp_per_build(u,u2)>0)
			  cp /= uu_cp_per_build(u,u2);
		    }
		    if (occ != NULL && occ->type==u2) {
			/* already upgrading */
			DMprintf("%s is upgrading to %s (%d/%d cp)\n",
				 unit_desig(unit), u_type_name(u2),
				 occ->cp, u_cp(occ->type));
		    } else if (cp >= u_cp(u2)/4) { /* rule-of-thumb... */
			/* complete unit under construction */
			DMprintf("%s will complete %s (now %d/%d cp) before upgrading to %s\n",
				 unit_desig(unit), u_type_name(occ->type),
				 occ->cp, u_cp(occ->type), u_type_name(u2));
		    } else {
			/* start upgrading */
			if (occ != NULL && !fullsized(occ)) {
			    DMprintf("%s will drop work on %s (%d/%d cp) and immediately start upgrading to %s\n",
				 unit_desig(unit), u_type_name(occ->type),
				 occ->cp, u_cp(occ->type), u_type_name(u2));
			} else {
			    DMprintf("%s will start upgrading to %s\n",
				     unit_desig(unit), u_type_name(u2));
			}
			unit->plan->type = PLAN_PASSIVE;
			clear_task_agenda(unit->plan);
			set_construction(unit, u2, 1);
		    }
		}
	    }

	    plan = unit->plan;
	    oldtheater = unit_theater(unit);
	    /* Goal might have become satisfied. */
	    if (plan->maingoal) {
		if (goal_truth(side, plan->maingoal) == 100) {
		    DMprintf("%s %s satisfied, removing\n",
			     unit_desig(unit), goal_desig(plan->maingoal));
		    plan->maingoal = NULL;
		    /* Force a replan. */
		    plan->type = PLAN_NONE;
		    set_unit_theater(unit, NULL);
		}
	    }
	    /* Theater might have become explored enough (90% known). */
	    if (plan->type == PLAN_EXPLORATORY
	        && (theater = unit_theater(unit)) != NULL
	        && theater->unexplored < theater->size / 10) {
		    DMprintf("%s theater %s is mostly known\n",
			     unit_desig(unit), theater->name);
		    plan->maingoal = NULL;
		    /* Force a replan. */
		    plan->type = PLAN_NONE;
		    set_unit_theater(unit, NULL);
	    }
	    theater = unit_theater(unit);
	    DMprintf("%s currently assigned to %s",
	    	     unit_desig(unit),
		     (theater ? theater->name : "no theater"));
	    if (oldtheater != theater) {
	    	DMprintf(" (was %s)",
			 (oldtheater ? oldtheater->name : "no theater"));
	    }
	    DMprintf("\n");
	}
    }
    /* Could notify display about unit plan mix? */
}

/* Look at our current overall strategy and hack it as needed. */

static void
update_side_strategy(side)
Side *side;
{
    Theater *theater;

    Dprintf("%s updating strategy\n", side_desig(side));
    /* Add something to add/update theaters as things open up. (?) */
    for_all_theaters(side, theater) {
	decide_theater_needs(side, theater);
    }
}

/* Figure out how many units to request for each area. */

static void
decide_theater_needs(side, theater)
Side *side;
Theater *theater;
{
    if (theater->unexplored > 0) {
    	/* Exploration is less important when 90% of a theater is known. */
    	if (theater->unexplored > (theater->size / 10)) {
		++(mplayer(side)->explorersneeded);
    	}
	/* Should look for good exploration units. */
	theater->importance = 50;  /* should depend on context */
/*	theater->reinforce = EXPLORE_AREA;  */
#if 0
    } else if (0 /* few enemies? */) {
	if (theater->allied_makers == 0
	    && theater->makers > 0
	    && theater->nearby) {
	    theater->reinforce = GUARD_BORDER_TOWN + 2 * theater->makers;
	} else if (theater->makers > 0) {
	    theater->reinforce = (theater->border ? GUARD_BORDER_TOWN :
				  GUARD_TOWN) + 2 * theater->allied_makers;
	} else if (theater->allied_bases > 0) {
	    theater->reinforce = (theater->border ? GUARD_BORDER: GUARD_BASE);
	} else if (theater->border) {
	    theater->reinforce = NO_UNITS;
	} else {
	    theater->reinforce = NO_UNITS;
	}
    } else {
	if (theater->allied_makers > 0) {
	    theater->reinforce = DEFEND_TOWN + 5 * theater->makers;
	} else if (theater->allied_bases > 0) {
	    theater->reinforce = DEFEND_BASE + theater->allied_bases;
	} else {
	    theater->reinforce = 0 /* DEFEND_AREA */;
	}
#endif
    }
}

/* For each unit, decide what it should be doing (if anything).  This is
   when a side takes the initiative;  a unit can also request info from
   its side when it is working on its individual plan. */

static void
update_unit_plans(side)
Side *side;
{
    Unit *unit;

    for_all_side_units(side, unit) {
	if (is_active(unit) && unit->plan != NULL) {
	    mplayer_decide_plan(side, unit);
	}
    }
}

/* Randomly change a unit's plans.  (This is really more for
   debugging, exercising plan execution code in novel ways.) */

static void
update_unit_plans_randomly(side)
Side *side;
{
    Unit *unit;

    for_all_side_units(side, unit) {
	if (is_active(unit) && unit->plan && unit->plan->aicontrol) {
	    if (probability(10)) {
		DMprintf("Randomly changed %s plan %s",
			 unit_desig(unit), plan_desig(unit->plan));
		unit->plan->type = xrandom((int) NUMPLANTYPES);
		DMprintf("to plan %s\n", plan_desig(unit->plan));
	    }
	    /* (should add/remove goals randomly) */
	    if (probability(10)) {
		unit->plan->reserve = FALSE;
	    }
	    if (probability(10)) {
		unit->plan->asleep = FALSE;
	    }
	}
    }
}

/* Push a new goal onto the side's list of goals. */

/* (this should only add goals that are not already present) */

static void
add_goal(side, goal)
Side *side;
Goal *goal;
{
    if (mplayer(side)->numgoals < MAXGOALS) {
	mplayer(side)->goals[(mplayer(side)->numgoals)++] = goal;
    }
}

static Goal *
has_goal(side, goaltype)
Side *side;
GoalType goaltype;
{
    int i;
    Goal *goal;

    for (i = 0; i < mplayer(side)->numgoals; ++i) {
	goal = mplayer(side)->goals[i];
	if (goal != NULL && goal->type == goaltype) {
	    return goal;
	}
    }
    return NULL;
}

static Goal *
has_unsatisfied_goal(side, goaltype)
Side *side;
GoalType goaltype;
{
    int i;
    Goal *goal;

    for (i = 0; i < mplayer(side)->numgoals; ++i) {
	goal = mplayer(side)->goals[i];
	if (goal != NULL && goal->type == goaltype && goal_truth(side, goal) < 100) {
	    return goal;
	}
    }
    return NULL;
}

/* This is for when a unit needs a plan and asks its side for one. */

static void
mplayer_decide_plan(side, unit)
Side *side;
Unit *unit;
{
    Plan *plan = unit->plan;
    int u = unit->type;

    if (plan == NULL || !plan->aicontrol)
      return;

    /* code specific to the "time" game */
    /* don't mess up with units researching or upgrading */
    if (game_class == gc_time) {
	if (mplayer(side)->research_status[u] == RS_UPGRADE_NEEDED
	    && plan->tasks != NULL
	    && plan->tasks->type == TASK_BUILD)
	  return;
	if (mplayer(side)->research_status[u] == RS_RESEARCH_ASSIGNED
	    && plan->tasks != NULL
	    && plan->tasks->type == TASK_RESEARCH)
	  return;
    }

    if (!mplayer(side)->trytowin) {
	plan->type = PLAN_RANDOM;
	clear_task_agenda(unit->plan);
	return;
    }
    switch (plan->type) {
      case PLAN_PASSIVE:
      case PLAN_NONE:
	if (mobile(unit->type)) {
	    /* Maybe assign to exploration. */
	    if (has_goal(side, GOAL_WORLD_KNOWN)) {
		if (need_this_type_to_explore(side, unit->type)) {
		    /* also limit to a total percentage, in case
		       exploration needs are very high */
		    assign_to_exploration(side, unit);
		    return;
		}
	    }
	    if (type_can_attack(unit->type) || type_can_fire(unit->type)) {
		/* (A more precise test would be "can attack types known to be or
		    likely to be in the current game".) */
		assign_to_offense(side, unit);
	    } else {
		assign_to_offense_support(side, unit);
	    }
	    /* when should mobile units be assigned to defense?? */
	} else {
	    if (has_unsatisfied_goal(side, GOAL_VICINITY_HELD)
		&& type_can_build_attackers(side, unit->type)
		) {
	    	assign_to_offense_support(side, unit);
	    } else if (has_goal(side, GOAL_WORLD_KNOWN)
	        && need_this_type_to_build_explorers(side, unit->type)) {
		assign_to_explorer_construction(side, unit);
	    } else if (type_can_build_attackers(side, unit->type)) {
	    	assign_to_offense_support(side, unit);
	    } else {
		assign_to_defense_support(side, unit);
	    }
	}
	break;
      case PLAN_OFFENSIVE:
	/* leave plan alone */
	break;
      case PLAN_EXPLORATORY:
	/* leave plan alone */
	break;
      case PLAN_DEFENSIVE:
	/* leave plan alone */
	break;
      default:
	break;
    }
}

static int
need_this_type_to_explore(side, u)
Side *side;
int u;
{
    int s, numcontacted = 0, numfound = 0;

    if (!mobile(u))
      return FALSE;
    for (s = 1; s <= numsides; ++s) {
    	if (s == side->id)
    	  continue;
	if (mplayer(side)->contacted[s])
	  ++numcontacted;
	if (mplayer(side)->homefound[s])
	  ++numfound;
    }
    if (numcontacted == 0) {
	return TRUE;
    } else if (numfound == 0) {
	return probability(50);
    } else if (numfound < numsides - 1) {
	return probability(10);
    } else {
	return FALSE;
    }
}

struct weightelt {
    int weight;
    long data;
};

static int
compare_weights(w1, w2)
CONST void *w1, *w2;
{
    return (((struct weightelt *) w2)->weight - ((struct weightelt *) w1)->weight);
}

/* Set the unit up as an explorer and let it go. */

static void
assign_to_exploration(side, unit)
Side *side;
Unit *unit;
{
    int numweights = 0, weight, i, dist;
    struct weightelt weights[MAXTHEATERS];
    Theater *theater;

    /* Unit's goal in life will be to see that the world is all known. */
    unit->plan->type = PLAN_EXPLORATORY;
    set_unit_theater(unit, NULL);
    /* Find the theater most in need of exploration. */
    for_all_theaters(side, theater) {
    	if (theater->size > 0 && theater->unexplored > 0) {
	    /* Weight by percentage of theater that is unknown. */
	    weight = (100 * theater->unexplored) / theater->size;
	    /* Downrate theaters that are far away. */
	    dist = distance(unit->x, unit->y, theater->x, theater->y)
	      - isqrt(theater->size) / 2;
	    if (dist < 0)
	      dist = 0;
	    weight /= max(1, (4 * dist) / area.maxdim);
	    /* Uprate the home front by a lot. */
	    if (theater == mplayer(side)->homefront)
	      weight *= 4;
	    /* Flatten out 10% variations. */
	    weight = 10 * (weight / 10);
	    weights[numweights].weight = weight;
	    weights[numweights].data = theater->id;
	    ++numweights;
    	}
    }
    if (numweights > 0) {
    	qsort(weights, numweights, sizeof(struct weightelt), compare_weights);
    	/* Choose randomly among theaters of equal weight. */
    	for (i = 0; i < numweights; ++i)
	  if (weights[i].weight < weights[0].weight)
	    break;
    	theater = mplayer(side)->theatertable[weights[xrandom(i)].data];
    } else {
    	theater = NULL;
    }
    assign_explorer_to_theater(side, unit, theater);
}

static int
probably_explorable(side, x, y, u)
Side *side;
int x, y, u;
{
    int dir, x1, y1, tview, t;

    for_all_directions(dir) {
	if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
	    tview = terrain_view(side, x, y);
	    if (tview == UNSEEN)
	      return TRUE;
	    t = vterrain(tview);
	    if (could_be_on(u, t) && could_live_on(u, t))
	      return TRUE;
	}
    }
    return FALSE;
}

static void
assign_explorer_to_theater(side, unit, theater)
Side *side;
Unit *unit;
Theater *theater;
{
    int sq, x, y, tries = 0;
    Goal *goal;
    
    if (theater != NULL) {
	set_unit_theater(unit, theater);
	++(theater->numassigned[unit->type]);
	goal = create_goal(GOAL_VICINITY_KNOWN, side, TRUE);
	sq = isqrt(theater->size);
	while (++tries < 100) {
	    /* Select a random point within the theater. */
	    x = theater->xmin;  y = theater->ymin;
	    x += (xrandom(theater->xmax - theater->xmin)
		  + xrandom(theater->xmax - theater->xmin)) / 2;
	    y += (xrandom(theater->ymax - theater->ymin)
		  + xrandom(theater->ymax - theater->ymin)) / 2;
	    if (theater_at(side, x, y) == theater
		&& terrain_view(side, x, y) == UNSEEN
		&& probably_explorable(side, x, y, unit->type))
	      break;
	}
	goal->args[0] = x;  goal->args[1] = y;
	goal->args[2] = goal->args[3] = sq / 2;
	unit->plan->maingoal = goal;
	DMprintf("%s now assigned to exploration in %s, around %d,%d\n",
		 unit_desig(unit), theater->name, x, y);
    }
}

static int
need_this_type_to_build_explorers(side, u)
Side *side;
int u;
{
    int s, u2;
	
    for (s = 1; s <= numsides; ++s) {
	if (mplayer(side)->contacted[s])
	  return FALSE;
	if (mplayer(side)->homefound[s])
	  return FALSE;
    }
    for_all_unit_types(u2) {
	if (mobile(u2)
	    /* should also check u2 is a useful explorer */
	    && uu_acp_to_create(u, u2) > 0)
	  return TRUE;
    }
    return FALSE;
}

/* Explorer constructors concentrate on building types that are good for
   exploration. */

static void
assign_to_explorer_construction(side, unit)
Side *side;
Unit *unit;
{
    /* Unit's goal in life will be to help see that the world is all known. */
    unit->plan->type = PLAN_EXPLORATORY;
    DMprintf("%s assigned to explorer construction\n", unit_desig(unit));
}

static void
assign_to_offense(side, unit)
Side *side;
Unit *unit;
{
    int numweights = 0, weight;
    struct weightelt weights[MAXTHEATERS];
    Goal *goal;
    Theater *homefront, *theater;

    unit->plan->type = PLAN_OFFENSIVE;
    clear_task_agenda(unit->plan);
    /* If our home area is being threatened, assign the unit to it. */
    homefront = mplayer(side)->homefront;
    if (homefront != NULL && homefront->enemystrengthmin > 0) {
	set_unit_theater(unit, homefront);
	goal = create_goal(GOAL_VICINITY_HELD, side, TRUE);
	goal->args[0] = homefront->x;  goal->args[1] = homefront->y;
	goal->args[2] = goal->args[3] = isqrt(homefront->size);
	unit->plan->maingoal = goal;
	DMprintf("%s assigned to offensive in the home front\n",
		 unit_desig(unit));
	return;
    }
    /* If the theater the unit is currently in is being threatened, assign the unit to it. */
    /* (should just increase it weight in next calculation?) */
    theater = theater_at(side, unit->x, unit->y);
    if (theater != NULL && theater->enemystrengthmin > 0) {
	set_unit_theater(unit, theater);
	goal = create_goal(GOAL_VICINITY_HELD, side, TRUE);
	/* (should randomize?) */
	goal->args[0] = theater->x;  goal->args[1] = theater->y;
	goal->args[2] = (theater->xmax - theater->xmin) / 2;
	goal->args[3] = (theater->ymax - theater->ymin) / 2;
	unit->plan->maingoal = goal;
	DMprintf("%s assigned to offensive in the theater where it's at now\n",
		 unit_desig(unit));
	return;
    }
    for_all_theaters(side, theater) {
    	if (theater->enemystrengthmin > 0 || theater->unexplored > 0) {
	    /* (should weight by strength relative to own units already there) */
	    weight = theater->enemystrengthmax * 20;
	    /* Prefer not to send unit to farther-away theaters. */
	    if (distance(unit->x, unit->y, theater->x, theater->y) > area.maxdim / 2) {
		weight /= 2;
	    }
	    /* Prefer theaters with more unknown territory. */
	    weight += (10 * theater->unexplored) / max(1, theater->size);
	    weights[numweights].weight = weight;
	    weights[numweights].data = theater->id;
	    ++numweights;
    	}
    }
    if (numweights > 0) {
    	qsort(weights, numweights, sizeof(struct weightelt), compare_weights);
    	theater = mplayer(side)->theatertable[weights[0].data];
    } else {
    	theater = theater_at(side, unit->x, unit->y);
    }
    set_unit_theater(unit, theater);
    if (theater != NULL) {
	++(theater->numassigned[unit->type]);
	goal = create_goal(GOAL_VICINITY_HELD, side, TRUE);
	/* (should randomize?) */
	goal->args[0] = theater->x;  goal->args[1] = theater->y;
	goal->args[2] = (theater->xmax - theater->xmin) / 2;
	goal->args[3] = (theater->ymax - theater->ymin) / 2;
	unit->plan->maingoal = goal;
	DMprintf("%s now assigned to offensive in %s",
		 unit_desig(unit), theater->name);
	if (numweights > 1) {
	    DMprintf(" (weight %d; runnerup was %s, weight %d)",
		     weights[0].weight,
		     (mplayer(side)->theatertable[weights[1].data])->name,
		     weights[1].weight);
	}
	DMprintf("\n");
    } else {
	DMprintf("%s now assigned to offensive in no theater",
		 unit_desig(unit));
    }
}

static void
assign_to_offense_support(side, unit)
Side *side;
Unit *unit;
{
    unit->plan->type = PLAN_OFFENSIVE;
    clear_task_agenda(unit->plan);
}

static int
type_can_build_attackers(side, u)
Side *side;
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (mobile(u2)
	    && (type_can_attack(u2) || type_can_fire(u2))
	    && uu_acp_to_create(u, u2) > 0)
	  return TRUE;
    }
    return FALSE;
}

/* For the given side and unit and plan, calculate the right type of
   unit to build. */

static int
mplayer_preferred_build_type(side, unit, plantype)
Side *side;
Unit *unit;
int plantype;
{
    int u = unit->type, u2, u3, t;
    int x, y, dir, x1, y1, blockedallaround, uview, est, rslt;
    int prefs[MAXUTYPES], knownterrain[MAXTTYPES];
    int fringeterrain[MAXTTYPES], sumfringe, totfringe;
    int enemytypes[MAXUTYPES];
    int numtotransport[MAXUTYPES];
    Unit *unit2, *occ;
    Theater *theater;

    if (plantype == PLAN_EXPLORATORY) {
	/* Calculate the amount of each type of terrain at the edges
	   of the known world. */
	for_all_terrain_types(t)
	  knownterrain[t] = fringeterrain[t] = 0;
	for_all_interior_cells(x, y) {
	    if (terrain_view(side, x, y) != UNSEEN) {
		++(knownterrain[(int) terrain_at(x, y)]);
		for_all_directions(dir) {
		    point_in_dir(x, y, dir, &x1, &y1);
		    if (terrain_view(side, x1, y1) == UNSEEN) {
			++(fringeterrain[(int) terrain_at(x, y)]);
			break;
		    }
		}
	    }
	}
    } else {
	/* should use estimated strengths instead? */
    	for_all_unit_types(u2)
    	  enemytypes[u2] = 0;
    	for_all_interior_cells(x, y) {
	    if (can_see_actual_units(side, x, y)) {
		for_all_stack(x, y, unit2) {
		    if (!trusted_side(side, unit2->side))
		      ++enemytypes[unit2->type];
		    /* (should count occ types recursively also) */
		    for_all_occupants(unit2, occ) {
			++enemytypes[occ->type];
		    }
		}
	    } else {
		uview = unit_view(side, x, y);
		if (uview != EMPTY) {
		    if (!trusted_side(side, side_n(vside(uview))))
		      ++enemytypes[vtype(uview)];
		}
	    }
    	}
    }
    /* Calculate a basic preference for each possible type. */
    for_all_unit_types(u2) {
	prefs[u2] = 0;
	est = est_completion_time(unit, u2);
	if (could_create(u, u2)
	    /* tmp hack until mplayer can do research */
	    && (u_tech_to_build(u2) > 0 ? side->tech[u2] >= u_tech_to_build(u2) : TRUE)
	    && est >= 0
	    && type_allowed_on_side(u2, side)) {
	    if (0 /* any demand in this unit's theater */) {
	    } else if (need_more_transportation(side)) {
    		for_all_unit_types(u3) {
		    numtotransport[u3] = 0;
    		}
	    	for_all_theaters(side, theater) {
		    for_all_unit_types(u3) {
			numtotransport[u3] += theater->numtotransport[u3];
		    }
	    	}
    		for_all_unit_types(u3) {
		    if (numtotransport[u3] > 0
			&& mobile(u2)
			&& could_carry(u2, u3)) {
			prefs[u2] += numtotransport[u3];
		    }
	    	}
	    } else {
		/* Prefer units by overall suitability for general plan. */
		if (plantype == PLAN_EXPLORATORY) {
		    /* Weight unit types by suitability for exploration around
		       the edges of the known area. */
		    sumfringe = totfringe = 0;
		    for_all_terrain_types(t) {
			totfringe += fringeterrain[t];
			if (!terrain_always_impassable(u2, t))
			  sumfringe += fringeterrain[t];
		    }
		    if (totfringe < 1)
		      sumfringe = totfringe = 1;
		    /* Scale - so 5% diffs in amt of crossable terrain
		       don't affect result. */
		    prefs[u2] = (20 * sumfringe) / totfringe;
		    /* Prefer types that are quicker to build. */
		    prefs[u2] /= max(1, est / 8);
		} else {
		    /* Weight unit types by effectiveness against known enemies. */
		    for_all_unit_types(u3) {
			if (enemytypes[u3] > 0) {
			    if (uu_zz_bhw(u2, u3) > 0) {
				prefs[u2] += uu_zz_bhw(u2, u3) * enemytypes[u3];
			    }
			    if (uu_zz_bcw(u2, u3) > 0) {
				prefs[u2] += uu_zz_bcw(u2, u3) * enemytypes[u3];
			    }
			}
		    }
		    /* Prefer types that are quicker to build. */
		    prefs[u2] /= max(1, est / 8);
		}
		if (prefs[u2] < 1)
		  prefs[u2] = 1;
	    }
	}
    }
    /* Units that can't even get out of the builder get their preference
       cut.  This helps prevent the construction of large ships in Denver. */
    /* (should allow if units would have some other way to leave) */
    if (1 /* plantype == PLAN_EXPLORATORY */) {
	for_all_unit_types(u2) {
	    if (prefs[u2] > 0) {
		blockedallaround = TRUE;
		for_all_directions(dir) {
		    point_in_dir(unit->x, unit->y, dir, &x1, &y1);
		    if (!terrain_always_impassable(u2, terrain_at(x1, y1))) {
		    	blockedallaround = FALSE;
		    	break;
		    }
#if 0  /* should replace this with a more useful test */
		    if (unit_at(x1, y1) != NULL) {
		    	blockedallaround = FALSE;
		    	break;
		    }
#endif
		}
		if (blockedallaround)
		  prefs[u2] = 0;
	    }
	}
    }
    DMprintf("%s preferred build type is ", unit_desig(unit));
    /* Look for an existing incomplete occupant and prefer to build its type,
       if it is in the choices in the typelist. */
    for_all_occupants(unit, occ) {
	if (in_play(occ) && !completed(occ)) {
	    if (prefs[occ->type] > 0 && flip_coin()) {
		rslt = occ->type;
		DMprintf("%s (incomplete occupant)\n", u_type_name(rslt));
		return rslt;
	    }
	}
    }
    for_all_unit_types(u2)
      if (prefs[u2] < 0)
        prefs[u2] = 0;
    rslt = select_by_weight(prefs, numutypes);
    if (!is_unit_type(rslt))
      rslt = NONUTYPE;
    if (DebugM) {
	if (is_unit_type(rslt)) {
	    DMprintf("%s (choices were", u_type_name(rslt));
	    for_all_unit_types(u2) {
		if (prefs[u2] > 0)
		  DMprintf(" %s,%d", utype_name_n(u2, 1), prefs[u2]);
	    }
	    DMprintf(")");
	} else {
	    DMprintf("nothing (no choices)");
	}
	DMprintf("\n");
    }
    return rslt;
}

/* (should make this a generic routine) */

static int
select_by_weight(arr, numvals)
int *arr, numvals;
{
    int sum = 0, i, n;

    sum = 0;
    for (i = 0; i < numvals; ++i) {
	sum += arr[i];
    }
    if (sum == 0)
      return -1;
    /* We now know the range, make a random index into it. */
    n = xrandom(sum);
    /* Go through again to figure out which choice the index refs. */
    sum = 0;
    for (i = 0; i < numvals; ++i) {
	sum += arr[i];
	if (sum >= n) {
	    return i;
	}
    }
    run_error("Ooh weird");
    return -1;
}

static int
need_more_transportation(side)
Side *side;
{
    int u3, u2, anytransport;
    Theater *theater;

    for_all_theaters(side, theater) {
	for_all_unit_types(u3) {
	    if (theater->numtotransport[u3] > 0) {
		anytransport = FALSE;
		for_all_unit_types(u2) {
		    if (theater->numassigned[u2] > 0
			&& mobile(u2)
			&& could_carry(u2, u3))
		      anytransport = TRUE;
		}
		if (!anytransport)
		  return TRUE;
	    }
	}
    }
    return FALSE;
}

static void
assign_to_defense_support(side, unit)
Side *side;
Unit *unit;
{
    unit->plan->type = PLAN_DEFENSIVE;
    clear_task_agenda(unit->plan);
}

static int
can_research_on(u, u2)
int u;
int u2;
{
    int acp_res = uu_acp_to_research(u, u2);

    if (acp_res < 1 || acp_res > u_acp(u))
      return 0;

    return 1;
}

/* code specific to the "time" game */
static int 
needs_research (side, u)
Side *side;
int u;
{
    int u2, i;

    if (game_class != gc_time)
      return 0;

    if (side->tech[u] >= u_tech_to_build(u) ||
	u_tech_max(u)  > u_tech_to_build(u))
      return 0;
    
    i = 0;
    for_all_unit_types(u2) {
	i += uu_acp_to_create(u, u2) > 0;
    }
    if (i < 2)
      return 0;

    return 1;
}

static int
assign_to_research_on(side, unit, u2)
Side *side;
Unit *unit;
int u2;
{
    int lev = u_tech_to_build(u2);

    if (!can_research_on(unit->type, u2)) {
	DMprintf("%s cannot research on %s!\n",
	     unit_desig(unit), u_type_name(u2));
	return 0;
    }

    DMprintf("%s will research for %s on %s (to level %d)\n",
	     unit_desig(unit), side_desig(side), u_type_name(u2), lev);

    unit->plan->type = PLAN_PASSIVE;
    push_research_task(unit, u2, lev);
    return 1;
}

/* This is called by individual plans to see if a particular unit type at a
   particular location is scheduled to be captured. */

static int
mplayer_planning_to_capture(side, u, x, y)
Side *side;
int u, x, y;
{
    Task *task;
    Unit *unit;

    for_all_side_units(side, unit) {
	if (in_play(unit)
	    && unit->plan) {
	    for_all_tasks(unit->plan, task) {
		if (task->type == TASK_CAPTURE
		    && task->args[0] == x && task->args[1] == y
		    && (task->args[2] == NONUTYPE || task->args[2] == u))
		  return TRUE;
		if (task->type == TASK_HIT_UNIT
		    && task->args[0] == x && task->args[1] == y
		    && (task->args[2] == NONUTYPE || task->args[2] == u)
		    && uu_capture(unit->type, u) > 0)
		  return TRUE;
	    }
	}
    }
    return FALSE;
}

/* This is called when an exploring unit gets confused about what to do. */

static int
mplayer_guide_explorer(side, unit)
Side *side;
Unit *unit;
{
    if (probability(10) && build_base_for_self(side, unit))
      return TRUE;
    if (probability(10) && build_base_for_others(side, unit))
      return TRUE;
    return FALSE;
}

/* Decide for the unit whether it should build a base for its own benefit. */

static int
build_base_for_self(side, unit)
Side *side;
Unit *unit;
{
    int u = unit->type, u2, cando = FALSE;

    for_all_unit_types(u2) {
	if (uu_acp_to_create(u, u2) > 0
	    && (uu_creation_cp(u, u2) >= u_cp(u2)
	        || uu_acp_to_build(u, u2) > 0)
	    /* (should check if any advantage to building) */
	   ) {
	   cando = TRUE;
	   break;
	}
    }
    if (cando) {
	DMprintf("%s building %s as a base for itself\n",
		     unit_desig(unit), u_type_name(u2));
	set_construction(unit, u2, 1);
	return TRUE;
    }
    return FALSE;
}

/* Decide for the unit whether it should build a base to help other units. */

static int
build_base_for_others(side, unit)
Side *side;
Unit *unit;
{
    int u = unit->type, u2, cando = FALSE;

    for_all_unit_types(u2) {
	if (uu_acp_to_create(u, u2) > 0
	    && (uu_creation_cp(u, u2) >= u_cp(u2)
	        || uu_acp_to_build(u, u2) > 0)
	    /* (should check if any advantage to building) */
	   ) {
	   cando = TRUE;
	   break;
	}
    }
    if (cando) {
	DMprintf("%s building %s as a base for others\n",
		     unit_desig(unit), u_type_name(u2));
	set_construction(unit, u2, 1);
	return TRUE;
    }
    return FALSE;
}

static int
build_depot_for_self(side, unit)
Side *side;
Unit *unit;
{
    int u = unit->type, u2, cando = FALSE;

    for_all_unit_types(u2) {
	if (uu_acp_to_create(u, u2) > 0
	    && (uu_creation_cp(u, u2) >= u_cp(u2)
	        || uu_acp_to_build(u, u2) > 0)
	    /* (should check if any advantage to building) */
	   ) {
	   cando = TRUE;
	   break;
	}
    }
    if (cando) {
	DMprintf("%s building %s as a depot for itself\n",
		     unit_desig(unit), u_type_name(u2));
	set_construction(unit, u2, 1);
	return TRUE;
    }
    return FALSE;
}

static void
mplayer_react_to_action_result(side, unit, rslt)
Side *side;
Unit *unit;
int rslt;
{
    if (unit->plan
	&& unit->plan->supply_is_low) {
	resupply_if_low(unit);
    }
}

/* This is a hook that runs after each task is executed. */

static void
mplayer_react_to_task_result(side, unit, task, rslt)
Side *side;
Unit *unit;
Task *task;
TaskOutcome rslt;
{
    int dx, dy, x1, y1, fact;
    Unit *occ;
    Theater *theater;

    /* React to an apparent blockage. */
    if (rslt == TASK_FAILED
	&& task != NULL
	&& task->type == TASK_MOVE_TO
	&& task->retrynum > 2) {
	if (desired_direction_impassable(unit, task->args[0], task->args[1])) {
	    if (could_be_ferried(unit, task->args[0], task->args[1])) {
		if (unit->plan->type == PLAN_EXPLORATORY && flip_coin()) {
	      	    /* (could also change task within the same theater) */
		    DMprintf("%s blocked while exploring, changing theaters\n",
			     unit_desig(unit));
		    change_to_adjacent_theater(side, unit);
		    return;
		} else if (flip_coin()) {
		    DMprintf("%s blocked, will wait for transport\n",
			     unit_desig(unit));
		    theater = theater_at(side, unit->x, unit->y);
		    if (theater != NULL) {
			++(theater->numtotransport[unit->type]);
		    }
		    unit->plan->reserve = TRUE;
		    unit->plan->waitingfortransport = TRUE;
		    return;
		}
	    } else {
	    	if (unit->occupant) {
		    DMprintf("%s blocked while transporting, will sit briefly\n",
			     unit_desig(unit));
		    unit->plan->reserve = TRUE;
		    for_all_occupants(unit, occ) {
		    	wake_unit(occ, FALSE, -1, NULL);
		    }
		    return;
	    	}
		/* Another option is to transfer to another theater.
		   This is especially useful when exploring. */
		if (unit->plan->type == PLAN_EXPLORATORY && flip_coin()) {
		    DMprintf("%s blocked while exploring, changing theaters\n",
			     unit_desig(unit));
		    change_to_adjacent_theater(side, unit);
		    return;
		}
	    }
	    /* Try moving sideways. */
	    if (probability(80)) {
		dx = task->args[0] - unit->x;  dy = task->args[1] - unit->y;
		fact = (flip_coin() ? 50 : -50);
		x1 = unit->x - ((fact * dy) / 100);  y1 = unit->y + ((fact * dx) / 100);
		if (inside_area(x1, y1))
		  push_move_near_task(unit, x1, y1, 1);
	    }
	    return;
	} else if (blocked_by_enemy(unit, task->args[0], task->args[1])) {
	    /* (should decide if allowable risk to passengers) */
	    DMprintf("%s blocked by enemy\n", unit_desig(unit));
	    attack_blockage(side, unit, task->args[0], task->args[1]); 
	}
	return;
    }
    /* React to inability to resupply by trying to build a base. */
    if (rslt == TASK_FAILED
	&& task != NULL
	&& task->type == TASK_RESUPPLY
	&& task->retrynum > 2) {
    	set_unit_reserve(side, unit, FALSE, FALSE);
    	build_depot_for_self(side, unit);
    }
#if 0    /* I'm not sure the following is really necessary - */
    /* - if the task fails, but a good victim is to be seen nearby, then the AI will
       eventually assign a unit to attack, quite likely the one whose task just failed. */
    /* React to a target unit trying to get away; if we can still see it nearby
       somewhere, adjust to pursue. */
    if (rslt == TASK_FAILED
	&& task != NULL
	&& task->type == TASK_HIT_UNIT
	) {
	int u = unit->type, tx, ty, dist, enemythere, uview, u2, s2;
	Unit *unit2, *target;

	tx = task->args[0];  ty = task->args[1];
	tu = task->args[2];  ts = task->args[3];
	if (can_see_actual_units(side, tx, ty)) {
		for_all_stack(tx, ty, unit2) {
	    	    if (unit2->side != side
			&& (tu == NONUTYPE || tu == unit2->type)
			) {
			/* A target is still there; task must have failed for some
			   other reason. */
			return;
		    }
		}
		/* They're gone! */
	} else {
		/* Assess old image or emptiness. */
		uview = unit_view(side, tx, ty);
		if (uview != EMPTY) {
		    if (tu == NONUTYPE || tu == vtype(uview)) {
		    	/* Target is still here. */
		    	return;
		    }
		}
		/* They're gone! */
	}
	/* If we get here, the target's location is empty. */
	return;
    }
#endif
}

static void
change_to_adjacent_theater(side, unit)
Side *side;
Unit *unit;
{
    int dir;
    Theater *theater;

    theater = unit_theater(unit);
    if (theater != NULL) {
   	for_all_directions(dir) {
   	    if (theater == mplayer(side)->perimeters[dir]) {
   	    	assign_explorer_to_theater(side, unit,
   	    	    mplayer(side)->perimeters[flip_coin() ? left_dir(dir) : right_dir(dir)]);
   	    	break;
   	    }
   	    if (theater == mplayer(side)->midranges[dir]) {
   	    	assign_explorer_to_theater(side, unit,
   	    	    mplayer(side)->midranges[flip_coin() ? left_dir(dir) : right_dir(dir)]);
   	    	break;
   	    }
   	    if (theater == mplayer(side)->remotes[dir]) {
   	    	assign_explorer_to_theater(side, unit,
   	    	    mplayer(side)->remotes[flip_coin() ? left_dir(dir) : right_dir(dir)]);
   	    	break;
   	    }
   	}
    }
}

/* (should account for impassability because of borders, etc) */

static int
desired_direction_impassable(unit, x, y)
Unit *unit;
int x, y;
{
    int dirs[NUMDIRS], numdirs, i, x1, y1, t, numbaddirs = 0;

    numdirs = choose_move_dirs(unit, x, y, TRUE, NULL, NULL, dirs);
    for (i = 0; i < numdirs; ++i) {
	point_in_dir(unit->x, unit->y, dirs[i], &x1, &y1);
	t = terrain_at(x1, y1);
	if (terrain_always_impassable(unit->type, t))
	  ++numbaddirs;
    }
    return (numbaddirs == numdirs);
}

static int
could_be_ferried(unit, x, y)
Unit *unit;
int x, y;
{
    int dirs[NUMDIRS], numdirs, i, x1, y1, t, u2;

    if (!carryable(unit->type))
      return FALSE;
    numdirs = choose_move_dirs(unit, x, y, FALSE, NULL, NULL, dirs);
    for (i = 0; i < numdirs; ++i) {
	point_in_dir(unit->x, unit->y, dirs[i], &x1, &y1);
	t = terrain_at(x1, y1);
	/* See if there is a type that can carry us through via this direction. */
	for_all_unit_types(u2) {
	    if (could_carry(u2, unit->type)
	        && mobile(u2)
	        && !terrain_always_impassable(u2, t)
	        /* should also have "and this type is avail to us" */
	        ) {
	        return TRUE;
	    }
	}
    }
    return FALSE;
}

/* Note the recursion - should precalc this property. */

static int
carryable(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (could_carry(u2, u)
	    && (mobile(u2) /* || carryable(u2) */ ))
	  return TRUE;
    }
    return FALSE;
}

int *accelerables = NULL;

static int
accelerable(u)
int u;
{
    int u1, u2;

    if (accelerables == NULL) {
	accelerables = (int *) xmalloc(numutypes * sizeof(int));
	for_all_unit_types(u1) {	
	    for_all_unit_types(u2) {
		if (could_carry(u2, u1)
		    && mobile(u2)) {
		    if (u_acp(u2) * u_speed(u2) > u_acp(u1) * u_speed(u1)) {
#if 0
			sameterrain = TRUE;
			for_all_terrain_types(t) {
			    if (terrain_always_impassable(u2, t)
				&& !terrain_always_impassable(u1, t)) {
				return FALSE;
			    }
			}
#endif
			accelerables[u1] = TRUE;
			break;
		    }
		}
	    }
	}
    }
    return accelerables[u];
}

static int
blocked_by_enemy(unit, x, y)
Unit *unit;
int x, y;
{
    int dirs[NUMDIRS], numdirs, i, x1, y1, t, numbaddirs = 0;
    Unit *unit2;

    numdirs = choose_move_dirs(unit, x, y, TRUE, NULL, NULL, dirs);
    for (i = 0; i < numdirs; ++i) {
	point_in_dir(unit->x, unit->y, dirs[i], &x1, &y1);
	t = terrain_at(x1, y1);
	if (terrain_always_impassable(unit->type, t)) {
	    ++numbaddirs;
	    continue;
	}
	unit2 = unit_at(x1, y1);
	if (in_play(unit2) && !trusted_side(unit->side, unit2->side))
	  ++numbaddirs;
    }
    return (numbaddirs == numdirs);
}

static void
attack_blockage(side, unit, x, y)
Side *side;
Unit *unit;
int x, y;
{
    int dirs[NUMDIRS], numdirs, i, x1, y1, t;
    Unit *unit2;

    numdirs = choose_move_dirs(unit, x, y, TRUE, NULL, NULL, dirs);
    for (i = 0; i < numdirs; ++i) {
	point_in_dir(unit->x, unit->y, dirs[i], &x1, &y1);
	t = terrain_at(x1, y1);
	if (terrain_always_impassable(unit->type, t))
	  continue;
	unit2 = unit_at(x1, y1);
	if (!trusted_side(unit->side, unit2->side)) {
	    push_hit_task(unit, x1, y1);
	    return;
	}
    }
}

static void
mplayer_react_to_new_side(side, side2)
Side *side, *side2;
{
    /* (Assumes we call this right after adding each new side) */
    int oldnumsides = numsides - 1;
    int *newpeople, s;
    Theater *theater;

    for_all_theaters(side, theater) {
	/* Grow any people count arrays if present. */
	if (theater->people != NULL) {
	    newpeople = (int *) xmalloc ((numsides + 1) * sizeof(int));
	    for (s = 0; s <= oldnumsides; ++s)
	      newpeople[s] = theater->people[s];
	    free(theater->people);
	    theater->people = newpeople;
	}
    }
    anewside = side2;
    estimate_strengths(side);
    anewside = NULL;
}

static int analyze_move_count;

/* This runs after every few moves in run_game, so performance is
   important.  We only look for wedged units either when there is
   clear evidence of such, or every couple hundred calls, just in case. */

static void
mplayer_analyze_after_moves(side, numacted)
Side *side;
int numacted;
{
    Unit *unit;

    if (numacted == 0 && (side->numwaiting > 0 || ((++analyze_move_count % 200) == 0))) {
	for_all_side_units(side, unit) {
	    if (unit->plan
		&& !unit->plan->asleep
		&& !unit->plan->reserve
		&& unit->plan->waitingfortasks
		&& unit->plan->aicontrol) {
		/* This case really ought never to occur, but it's relatively
		   benign, so no need to make an actual warning for this. */
		Dprintf("AI-controlled unit %s waiting for tasks? - putting into reserve",
			unit_desig(unit));
		reserve_unit(side, unit);
	    }
	}
    }
}

/* At the end of a turn, re-evaluate the plans of some units in case
   the situation changed. */

static void
mplayer_finish_movement(side)
Side *side;
{
    int u;
    Unit *unit;
    Theater *theater;

    for_all_theaters(side, theater) {
	for_all_unit_types(u) {
	    if (theater->numtotransport[u] > 0) {
		/* Find a unit needing transport. */
		for_all_side_units(side, unit) {
		    if (is_active(unit)
			&& unit->plan
			&& unit->plan->aicontrol
			&& unit->plan->waitingfortransport
			&& theater_at(side, unit->x, unit->y) == theater) {
			search_for_available_transport(unit);
			unit->plan->waitingfortransport = FALSE;
		    }
		}
		break;
	    }
	}
    }
    for_all_side_units(side, unit) {
	if (is_active(unit)
	    && unit->plan
	    && unit->plan->aicontrol) {
	    rethink_plan(unit);
	}
    }
}

static Unit *
search_for_available_transport(unit)
Unit *unit;
{
    int dist, closestdist = area.maxdim;
    Unit *transport, *closesttransport = NULL;
    Theater *theater = unit_theater(unit);

    /* (more efficient to search adjacent cells first?) */
    for_all_side_units(unit->side, transport) {
	if (is_active(transport)
	    && mobile(transport->type)
	    && could_carry(transport->type, unit->type)
	    && transport->occupant == NULL /* (should be "has room") */
	    && transport->act != NULL /* not quite correct, but to fix bug */) {
	    /* Don't grab at units being moved manually. */
	    if (transport->plan
		&& !transport->plan->aicontrol)
	      continue;
	    /* Maybe this one is already coming to get somebody. */
	    if (transport->plan
		&& transport->plan->tasks != NULL
		&& transport->plan->tasks->type == TASK_PICKUP) {
		if (transport->plan->tasks->args[0] == unit->id)
		  return transport;
		/* Picking up somebody else - don't hijack. */
		continue;
	    }
	    if (transport->plan
		&& transport->plan->tasks != NULL
		&& transport->plan->tasks->type == TASK_MOVE_TO
		&& transport->plan->tasks->next != NULL
		&& transport->plan->tasks->next->type == TASK_PICKUP) {
		if (transport->plan->tasks->next->args[0] == unit->id)
		  return transport;
		/* Picking up somebody else - don't hijack. */
		continue;
	    }
	    dist = distance(unit->x, unit->y, transport->x, transport->y);
	    if (dist < closestdist || (dist == closestdist && flip_coin())) {
		closesttransport = transport;
		closestdist = dist;
	    }
	    /* If transport already adjacent, no need to keep looking. */
	    if (closestdist <= 1)
	      break;
	}
    }
    if (closesttransport != NULL && closesttransport->plan != NULL) {
	clear_task_agenda(closesttransport->plan);
	/* (could inherit unit's goal, but not needed) */
	closesttransport->plan->maingoal = NULL;
	push_pickup_task(closesttransport, unit);
	push_move_near_task(closesttransport, unit->x, unit->y, 1);
	/* No longer count this unit as needing transport. */
	if (theater != NULL) {
	    --(theater->numtotransport[unit->type]);
	    set_unit_theater(closesttransport, theater);
	}
	DMprintf("%s will be picked up by closest transport %s\n",
	         unit_desig(unit), unit_desig(closesttransport));
	return closesttransport;
    }
    return NULL;
}

/* For units with plans and that are under AI control, consider changing the
   current plan/tasks. */

static void
rethink_plan(unit)
Unit *unit;
{
    int dist, x1, y1;
    Task *toptask = unit->plan->tasks, *nexttask = NULL;
    Plan *plan = unit->plan;
    Unit *transport;

    if (toptask)
      nexttask = toptask->next;
    /* If we have a long ways to go, see if there is a transport available that
       can get us there faster.  */
    if (toptask != NULL
	&& (toptask->type == TASK_HIT_UNIT
	    || (toptask->type == TASK_MOVE_TO
		&& nexttask != NULL
		&& nexttask->type == TASK_HIT_UNIT))
        && !plan->reserve
        && !plan->asleep
        && !plan->waitingfortransport
        && (unit->transport == NULL || !mobile(unit->transport->type))
        && ((dist = distance(unit->x, unit->y,
			     toptask->args[0], toptask->args[1]))
            >= 4 * u_acp(unit->type))
        && accelerable(unit->type)
        ) {
        DMprintf("%s looking for transport to accelerate with;\n", unit_desig(unit));
        transport = search_for_available_transport(unit);
        if (transport != NULL) {
	    /* push_sentry_task(unit, max(1, dist / max(1, u_acp(transport->type)))); */
	    plan->reserve = TRUE;
	    plan->waitingfortransport = FALSE;
        } else {
	    DMprintf("  found nothing\n");
        }
    }
    if (unit->plan->type == PLAN_OFFENSIVE
        && toptask != NULL
        && toptask->type == TASK_MOVE_TO
        && distance(unit->x, unit->y, toptask->args[0], toptask->args[1])
		>= min(2, u_acp(unit->type))
        && enemy_close_by(unit->side, unit, 1 /* 2 would be better? */, &x1, &y1)
        ) {
	push_hit_task(unit, x1, y1);
	DMprintf("%s sees enemy close by, will attack it\n", unit_desig(unit));
    }
    /* (should also notice fire opportunities) */
    /* If we see somebody that could be captured and help us explore, set up
       to produce capturers. */
    if (!mobile(unit->type)
	&& (unit->plan->type == PLAN_EXPLORATORY || unit->plan->type == PLAN_OFFENSIVE)
	) {
	int range = 4, rslt, x, y;

	DMprintf("%s searching for useful capture within %d in order to choose build; found ",
	     	 unit_desig(unit), range);
	tmpunit = unit;
	rslt = search_around(unit->x, unit->y, range, useful_captureable_here,
			     &x, &y, 1);
	if (rslt) {
	    DMprintf("one at %d,%d\n", x, y);
	    if (toptask != NULL
		&& toptask->type == TASK_BUILD
		&& uu_capture(toptask->args[0], tmputype)
		) {
		/* already doing the right thing */
	    } else {
		/* (should find best type that can capture quickly,
		    schedule to build it) */
	    }
	} else {
	    DMprintf("nothing\n");
	}
    }
}

static int victimx, victimy, victimrating;

static int
enemy_close_by(side, unit, dist, xp, yp)
Side *side;
Unit *unit;
int dist, *xp, *yp;
{
    int x = unit->x, y = unit->y, dir, x1, y1;

    victimrating = -9999;
    tmpunit = unit;
    victim_here(x, y);
    for_all_directions(dir) {
	if (point_in_dir(x, y, dir, &x1, &y1)) {
	    victim_here(x, y);
	}
    }
    if (victimrating > -9999) {
	*xp = victimx;  *yp = victimy;
	return TRUE;
    } else {
	return FALSE;
    }
}

static void
mplayer_react_to_unit_loss(side, unit)
Side *side;
Unit *unit;
{
    int x = unit->x, y = unit->y;
    Theater *th;

    if (!inside_area(x, y)) {
	x = unit->prevx;  y = unit->prevy;
    }
    if (!inside_area(x, y))
      return;
    if (mplayer(side) && (th = theater_at(side, x, y)) != NULL) {
    	++(th->units_lost);
    }
}

static void
mplayer_receive_message(side, sender, str)
Side *side, *sender;
char *str;
{
    /* First detect standard messages. */
    if (strcmp(str, "Eh?") == 0) {
	/* Don't respond, otherwise we might infinitely recurse. */
    } else if (allied_side(side, sender)) {
    } else {
	/* Detect insults and respond appropriately. */
	if (strstr(str, "idiot")) {
	    send_message(side, add_side_to_set(sender, NOSIDES), "Loser!");
	} else {
	    /* No idea what the message was, be puzzled. */
	    send_message(side, add_side_to_set(sender, NOSIDES), "Eh?");
	}
    }
}

/* This is used by interfaces to display the theater in use at a given point. */

static char *
mplayer_at_desig(side, x, y)
Side *side;
int x, y;
{
    Theater *theater;

    if (mplayer(side) == NULL)
      return "";
    theater = theater_at(side, x, y);
    return (theater ? theater->name : "<no theater>");
}

static int
mplayer_theater_at(side, x, y)
Side *side;
int x, y;
{
    Theater *theater;

    if (mplayer(side) == NULL)
      return 0;
    theater = theater_at(side, x, y);
    return (theater ? theater->id : 0);
}

static int
mplayer_read_strengths(side)
Side *side;
{
    int sn1, u, found = FALSE;
    char *propname;
    Obj *props, *bdg, *rest, *sidebdg, *urest;
    Side *side1;
    Strategy *strategy = mplayer(side);

    props = find_at_key(side->aidata, "mplayer");
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	propname = c_string(car(bdg));
	if (strcmp(propname, "strengths0") == 0) {
	    found = TRUE;
	    rest = cdr(bdg);
	    for_all_sides(side1) {
		sn1 = side1->id;
		sidebdg = car(rest);
		urest = cadr(sidebdg);
		for_all_unit_types(u) {
		    strategy->strengths0[sn1][u] = c_number(car(urest));
		    urest = cdr(urest);
		}
		rest = cdr(rest);
	    }
	} else if (strcmp(propname, "alstrengths0") == 0) {
	    found = TRUE;
	    rest = cdr(bdg);
	    for_all_sides(side1) {
		sn1 = side1->id;
		sidebdg = car(rest);
		urest = cadr(sidebdg);
		for_all_unit_types(u) {
		    strategy->alstrengths0[sn1][u] = c_number(car(urest));
		    urest = cdr(urest);
		}
		rest = cdr(rest);
	    }
	} else if (strcmp(propname, "points0") == 0) {
	    found = TRUE;
	    rest = cdr(bdg);
	    for_all_sides(side1) {
		sn1 = side1->id;
		strategy->points0[sn1] = c_number(car(rest));
		rest = cdr(rest);
	    }
	} else if (strcmp(propname, "alpoints0") == 0) {
	    found = TRUE;
	    rest = cdr(bdg);
	    for_all_sides(side1) {
		sn1 = side1->id;
		strategy->alpoints0[sn1] = c_number(car(rest));
		rest = cdr(rest);
	    }
	} else {
	}
    }
    return found;
}

/* Write out any state that the mplayer must preserve.  We don't actually write;
   instead we build a Lisp object and pass that back to the writing routines. */

static Obj *
mplayer_save_state(side)
Side *side;
{
    int sn1, u;
    Obj *rslt, *vallist, *uvallist;
    Side *side1;
    Strategy *strategy = mplayer(side);

    rslt = lispnil;
    /* Just return last result if it's already been computed. */
    if (strategy->writable_state != lispnil || xmalloc_warnings)
      return strategy->writable_state;
    /* We're pushing bindings onto a list, so do in reverse of desired order. */
    vallist = lispnil;
    for_all_sides(side1) {
	sn1 = side1->id;
	uvallist = lispnil;
	for_all_unit_types(u) {
	    uvallist = cons(new_number(strategy->alstrengths0[sn1][u]), uvallist);
	}
	uvallist = reverse(uvallist);
	push_binding(&vallist, new_number(sn1), uvallist);
    }
    vallist = reverse(vallist);
    push_cdr_binding(&rslt, intern_symbol("alstrengths0"), vallist);
    vallist = lispnil;
    for_all_sides(side1) {
	sn1 = side1->id;
	uvallist = lispnil;
	for_all_unit_types(u) {
	    uvallist = cons(new_number(strategy->strengths0[sn1][u]), uvallist);
	}
	uvallist = reverse(uvallist);
	push_binding(&vallist, new_number(sn1), uvallist);
    }
    vallist = reverse(vallist);
    push_cdr_binding(&rslt, intern_symbol("strengths0"), vallist);
    vallist = lispnil;
    for_all_sides(side1) {
	sn1 = side1->id;
	vallist = cons(new_number(strategy->alpoints0[sn1]), vallist);
    }
    vallist = reverse(vallist);
    push_cdr_binding(&rslt, intern_symbol("alpoints0"), vallist);
    vallist = lispnil;
    for_all_sides(side1) {
	sn1 = side1->id;
	vallist = cons(new_number(strategy->points0[sn1]), vallist);
    }
    vallist = reverse(vallist);
    push_cdr_binding(&rslt, intern_symbol("points0"), vallist);
    strategy->writable_state = rslt;
    return rslt;
}
