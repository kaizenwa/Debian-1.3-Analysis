/* Functions common to all AIs.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"

/* The number of AI types defined.  Increment for each new AI type
   that is compiled in. */

#define numaitypes 2

/* Declarations of all AI types. */

extern AI_ops mplayer_ops;

AI_ops **all_ai_ops;

void
init_ai_types()
{
    /* Fill in the table of pointers to AI types. */
    all_ai_ops = (AI_ops **) xmalloc(numaitypes * sizeof(AI_ops *));
    all_ai_ops[0] = (AI_ops *) xmalloc(sizeof(AI_ops));
    all_ai_ops[1] = &mplayer_ops;
}

void
init_ai(side)
Side *side;
{
    int i;
    char *typename;
    int (*test) PARAMS ((void));
    void (*fn) PARAMS ((Side *side));

    if (side_wants_ai(side)) {
	if (strcmp(side->player->aitypename, "ai") == 0) {
	    /* (should use the "best" default for this game and side) */
	    side->player->aitypename = "mplayer";
	}
	for (i = 0; i < numaitypes; ++i) {
	    typename = (all_ai_ops[i])->name;
	    if (typename != NULL
		&& strcmp(typename, side->player->aitypename) == 0) {
		test = (all_ai_ops[i])->to_test_compat;
		if (test == NULL || (*test)()) {
		    side->aitype = i;
		    fn = (all_ai_ops[i])->to_init;
		    if (fn)
		      (*fn)(side);
		} else {
		    /* desired aitype not avail - should complain */
		}
	    }
	}
	if (!side_has_ai(side)) {
	    init_warning("could not make an AI (type %s) for %s",
			 side->player->aitypename, side_desig(side));
	}
    }
}

/* Change the AI running a side.  This has to clean up if the AI is
   being turned off. */

void
set_side_ai(side, typename)
Side *side;
char *typename;
{
    Unit *unit;

    if (!empty_string(typename) && strcmp(typename, "ai") == 0) {
	/* (should use the "best" default for this game and side) */
	typename = "mplayer";
    }
    side->player->aitypename = typename;
    for_all_side_units(side, unit) {
	unit->aihook = NULL;
	if (unit->plan && unit->plan->aicontrol) {
	    unit->plan->type = PLAN_NONE;
	    unit->plan->maingoal = NULL;
	    unit->plan->formation = NULL;
	    unit->plan->funit = NULL;
	    clear_task_agenda(unit->plan);
	    if (typename == NULL) {
		unit->plan->asleep = FALSE;
		unit->plan->reserve = FALSE;
		/* Don't touch delay, let action looping clear it eventually. */
		unit->plan->aicontrol = FALSE;
	    } else {
		unit->plan->aicontrol = TRUE;
	    }
	}
	/* Still let units finish their currently buffered action. */
    }
    if (typename == NULL) {
	/* (should just deactivate) */
	side->ai = NULL;
	/* Clear out everything that was set up by the AI. */
    } else {
	init_ai(side);
    }
}

void
ai_init_turn(side)
Side *side;
{
    void (*fn) PARAMS ((Side *side));

    fn = (all_ai_ops[side->aitype])->to_init_turn;
    if (fn)
      (*fn)(side);
}

void
ai_decide_plan(side, unit)
Side *side;
Unit *unit;
{
    void (*fn) PARAMS ((Side *side, Unit *unit));

    fn = ((all_ai_ops[side->aitype])->to_decide_plan);
    if (fn)
      (*fn)(side, unit);
}

void
ai_react_to_unit_loss(side, unit)
Side *side;
Unit *unit;
{
    void (*fn) PARAMS ((Side *side, Unit *unit));

    fn = ((all_ai_ops[side->aitype])->to_react_to_unit_loss);
    if (fn)
      (*fn)(side, unit);
}

/* Forward an action result to the appropriate AI routine. */

void
ai_react_to_action_result(side, unit, rslt)
Side *side;
Unit *unit;
int rslt;
{
    void (*fn) PARAMS ((Side *side, Unit *unit, int rslt));

    fn = ((all_ai_ops[side->aitype])->to_react_to_action_result);
    if (fn)
      (*fn)(side, unit, rslt);
}

/* Forward a task result to the appropriate AI routine. */

void
ai_react_to_task_result(side, unit, task, rslt)
Side *side;
Unit *unit;
Task *task;
TaskOutcome rslt;
{
    void (*fn) PARAMS ((Side *side, Unit *unit, Task *task, TaskOutcome rslt));

    fn = ((all_ai_ops[side->aitype])->to_react_to_task_result);
    if (fn)
      (*fn)(side, unit, task, rslt);
}

void
ai_react_to_new_side(side, side2)
Side *side, *side2;
{
    void (*fn) PARAMS ((Side *side, Side *side2));

    fn = ((all_ai_ops[side->aitype])->to_react_to_new_side);
    if (fn)
      (*fn)(side, side2);
}

int
ai_planning_to_capture(side, u, x, y)
Side *side;
int u, x, y;
{
    int (*fn) PARAMS ((Side *side, int u, int x, int y));

    fn = ((all_ai_ops[side->aitype])->planning_to_capture);
    if (fn)
      return (*fn)(side, u, x, y);
    else
      return FALSE;
}

int
ai_guide_explorer(side, unit)
Side *side;
Unit *unit;
{
    int (*fn) PARAMS ((Side *side, Unit *unit));

    fn = ((all_ai_ops[side->aitype])->to_guide_explorer);
    if (fn)
      return (*fn)(side, unit);
    else
      return FALSE;
}

int
ai_preferred_build_type(side, unit, plantype)
Side *side;
Unit *unit;
int plantype;
{
    int (*fn) PARAMS ((Side *side, Unit *unit, int plantype));

    fn = ((all_ai_ops[side->aitype])->preferred_build_type);
    if (fn)
      return (*fn)(side, unit, plantype);
    else
      return NONUTYPE;
}

void
ai_analyze_after_moves(side, numacted)
Side *side;
int numacted;
{
    void (*fn) PARAMS ((Side *side, int numacted));

    fn = ((all_ai_ops[side->aitype])->to_analyze_after_moves);
    if (fn)
      (*fn)(side, numacted);
}

void
ai_finish_movement(side)
Side *side;
{
    void (*fn) PARAMS ((Side *side));

    fn = ((all_ai_ops[side->aitype])->to_finish_movement);
    if (fn)
      (*fn)(side);
}

/* Forward a textual message to the appropriate AI routine. */

void
ai_receive_message(side, sender, str)
Side *side, *sender;
char *str;
{
    void (*fn) PARAMS ((Side *side, Side *sender, char *str));

    fn = ((all_ai_ops[side->aitype])->to_receive_message);
    if (fn)
      (*fn)(side, sender, str);
}

void
ai_save_state(side)
Side *side;
{
    Obj *(*fn) PARAMS ((Side *side)), *state = lispnil;

    fn = ((all_ai_ops[side->aitype])->to_save_state);
    if (fn)
      state = (*fn)(side);
    /* Don't bother if there's no AI state to mess with. */
    if (side->aidata == lispnil && state == lispnil)
      return;
    side->aidata = replace_at_key(side->aidata, side->player->aitypename, state);
}

int
ai_region_at(side, x, y)
Side *side;
int x, y;
{
    int (*fn) PARAMS ((Side *side, int x, int y));

    fn = ((all_ai_ops[side->aitype])->region_at);
    if (fn)
      return (*fn)(side, x, y);
    else
      return 0;
}

char *
ai_at_desig(side, x, y)
Side *side;
int x, y;
{
    char *(*fn) PARAMS ((Side *side, int x, int y));

    fn = ((all_ai_ops[side->aitype])->at_desig);
    if (fn)
      return (*fn)(side, x, y);
    else
      return NULL;
}

/* (should go elsewhere eventually?) */

/* Goal handling. */

GoalDefn goaldefns[] = {

#undef  DEF_GOAL
#define DEF_GOAL(NAME,code,ARGTYPES) { NAME, ARGTYPES },

#include "goal.def"

    { NULL, NULL }
};


/* General handling of goals. */

Goal *
create_goal(type, side, tf)
GoalType type;
Side *side;
int tf;
{
    Goal *goal = (Goal *) xmalloc(sizeof(Goal));

    goal->type = type;
    goal->side = side;
    goal->tf = tf;
    return goal;
}

int
cell_unknown(x, y)
int x, y;
{
    return (!all_see_all && !tmpside->see_all && terrain_view(tmpside, x, y) == UNSEEN);
}

int
enemies_present(x, y)
int x, y;
{
    if (units_visible(tmpside, x, y)) {
    	return (unit_at(x, y) != NULL && !trusted_side(tmpside, unit_at(x, y)->side));
    } else if (terrain_view(tmpside, x, y) != UNSEEN) {
    	return (vside(unit_view(tmpside, x, y)) != side_number(tmpside));
    } else {
    	return FALSE;
    }
}

/* Test a goal to see if it is true for side, as specified. */

int
goal_truth(side, goal)
Side *side;
Goal *goal;
{
    int x, y;
    Side *side2 = NULL;

    if (goal == NULL) return 0;
    switch (goal->type) {
      case GOAL_WON_GAME:
	side2 = goal->side;
	return (side2 ? (side_won(side2) ? 100 : -100) : 0);
      case GOAL_LOST_GAME:
	side2 = goal->side;
	return (side2 ? (side_lost(side2) ? 100 : -100) : 0);
      case GOAL_POSITIONS_KNOWN:
	/* what if no enemies present? then this is undefined? */
	/* should goals have preconditions or prerequisites? */
	return 0;
      case GOAL_WORLD_KNOWN:
	tmpside = side;
	for_all_interior_cells(x, y) {
	    if (cell_unknown(x, y)) return -100;
	}
	return 100;
      case GOAL_VICINITY_KNOWN:
	tmpside = side;
	if (search_around(goal->args[0], goal->args[1], goal->args[2],
			  cell_unknown, &x, &y, 1)) {
	    return -100;
	} else {
	    return 100;
	}
      case GOAL_VICINITY_HELD:
      	tmpside = side;
	if (search_around(goal->args[0], goal->args[1], goal->args[2],
			  enemies_present, &x, &y, 1)) {
	    return -100;
	} else {
	    return 100;
	}
      case GOAL_CELL_OCCUPIED:
	return 0;
      case GOAL_HAS_UNIT_TYPE:
	return 0;
      case GOAL_HAS_UNIT_TYPE_NEAR:
	return 0;
      case GOAL_HAS_MATERIAL_TYPE:
	return 0;
      default:
	case_panic("goal type", goal->type);
	return 0;
    }
}

/* (might eventually want another evaluator that guesses at another
   side's goals) */

char *goalbuf = NULL;

char *
goal_desig(goal)
Goal *goal;
{
    int numargs, i, arg;
    char *argtypes;

    if (goal == NULL)
      return "<null goal>";
    if (goalbuf == NULL)
      goalbuf = xmalloc(BUFSIZE);
    sprintf(goalbuf, "<goal s%d %s%s",
	    side_number(goal->side), (goal->tf ? "" : "not "),
	    goaldefns[goal->type].name);
    argtypes = goaldefns[goal->type].argtypes;
    numargs = strlen(argtypes);
    for (i = 0; i < numargs; ++i) {
	arg = goal->args[i];
	switch (argtypes[i]) {
	  case 'h':
	    tprintf(goalbuf, "%d", arg);
	    break;
	  case 'm':
	    if (is_material_type(arg))
	      tprintf(goalbuf, " %s", m_type_name(arg));
	    else
	      tprintf(goalbuf, " m%d?", arg);
	    break;
	  case 'S':
	    tprintf(goalbuf, " `%s'", side_desig(side_n(arg)));
	    break;
	  case 'u':
	    if (is_unit_type(arg))
	      tprintf(goalbuf, " %s", u_type_name(arg));
	    else
	      tprintf(goalbuf, " m%d?", arg);
	    break;
	  case 'U':
	    tprintf(goalbuf, " `%s'", unit_desig(find_unit(arg)));
	    break;
	  case 'w':
	    tprintf(goalbuf, " %dx", arg);
	    break;
	  case 'x':
	    tprintf(goalbuf, " %d,", arg);
	    break;
	  case 'y':
	    tprintf(goalbuf, "%d", arg);
	    break;
	  default:
	    tprintf(goalbuf, " %d", arg);
	    break;
	}
    }
    strcat(goalbuf, ">");
    return goalbuf;
}


/* General collections of numbers used by all machine players. */

/* Init used by all machine players.  Precompute useful information
   relating to unit types in general, and that usually gets referenced
   in inner loops. */

void
ai_init_shared()
{
    int u, u1, u2, t, m1, numbuilders, tmp;
    
    /* Need 3 scratch layers for routefinding. */
    allocate_area_scratch(3);

    /* Recognize unit types that are bases */
    for_all_unit_types(u1) {
	set_u_is_base(u1, FALSE);
	tmp = FALSE;
	for_all_material_types(m1) {
	    if (um_base_production(u1, m1) > 0) {
		tmp = TRUE;
		break;
	    }
	}
	if (tmp) {
	    for_all_unit_types(u2) {
		if ((u1 != u2) && could_carry(u1,u2)) {
		    set_u_is_base(u1, TRUE);
		    continue;
		}
	    }
	}
    }
    /* Note that is_base_builder is set to the type of base that can */
    /* be built.  That means that unit zero can not be a base which */
    /* can be built. */
    for_all_unit_types(u1) {
	set_u_is_transport(u1, FALSE);
	set_u_is_carrier(u1, FALSE);
	set_u_is_base_builder(u1, FALSE);
	set_u_can_make(u1, FALSE);
	set_u_can_capture(u1, FALSE);
	numbuilders = 0;
/*	ave_build_time[u1] = 0;  */
	for_all_unit_types(u2) {
	    if (u_is_base(u2) &&
		could_create(u1, u2) &&
		1 /* can be made quickly? */) {
		set_u_is_base_builder(u1, u2);
	    }
	    if (u_speed(u1) > 0 && could_carry(u1, u2)) {
		set_u_is_transport(u1, TRUE);
	    }
	    if (could_create(u2, u1)) {
		numbuilders++;
/*		ave_build_time[u1] += uu_make(u2,u1);  */
		set_u_can_make(u2, TRUE);
	    }
	    if (uu_capture(u1, u2) > 0 || uu_indep_capture(u1, u2) > 0) {
		set_u_can_capture(u1, TRUE);
	    }
	}
/*	if (numbuilders > 0)
	  ave_build_time[u1] /= numbuilders;  */
    }
    /* a carrier is a unit that is a mobile base, but that cannot
       move a passenger anywhere the passenger could not go itself. */
    for_all_unit_types(u1) {
	if (u_is_transport(u1)) {
	    set_u_is_carrier(u1, TRUE);
	    for_all_unit_types(u2) {
		if (could_carry(u1, u2)) {
		    for_all_terrain_types(t) {
			if (could_be_on(u1, t) && !could_be_on(u2, t))
			  set_u_is_carrier(u1, FALSE);
		    }
		}
	    }
	}
    }
    for_all_unit_types(u) {
	set_u_bw(u, basic_worth(u));
    }
    for_all_unit_types(u) {
	for_all_unit_types(u2) {
	    set_uu_bhw(u, u2, basic_hit_worth(u, u2));
	    set_uu_bfw(u, u2, basic_fire_worth(u, u2));
	    set_uu_bcw(u, u2, basic_capture_worth(u, u2));
	    set_uu_btw(u, u2, basic_transport_worth(u, u2));
	}
    }
    /* Tell how things rated. */
    if (DebugM)
      display_assessment();
}

int basic_transport_worth PARAMS ((int u1, int u2));
void set_uu_btw PARAMS ((int u1, int u2, int v));

#define DICE(N,NUMDICE,SPOTS,OFFSET)  \
  (((N) >> 14 == 0 || (N) >> 14 == 3) ?  \
   (NUMDICE = 0, SPOTS = 0, OFFSET = (N)) :  \
   (NUMDICE = ((N) >> 11) & 0x07, SPOTS = ((N) >> 7) & 0x0f, OFFSET = (N) & 0x7f))

/* A crude estimate of the worth of having one type of unit. */

int
basic_worth(u)
int u;
{
    int worth = 0, u2, r, range;
  
    worth += u_hp(u) * 10;
    for_all_unit_types(u2) {
	if (could_create(u, u2))
	  worth += (u_bw(u2) * (50)) / 1 /* uu_make(u, u2) */;
	/* (should account for shared capacity) */
	if (could_carry(u, u2))
	  worth += (1 + u_speed(u)) * uu_capacity_x(u, u2) *
	    (u_is_base(u) ? 10 : 1) * u_bw(u2) / 30;
    }
    range = 12345;
    for_all_material_types(r) {
	worth += um_base_production(u, r) * (u_is_base(u) ? 4 : 1);
	if (um_consumption_per_move(u, r) > 0)
	  range = min(range, um_storage_x(u, r) / max(1, um_consumption_per_move(u, r)));
	if (um_base_consumption(u, r) > 0) 
	  range =
	    min(range, u_speed(u) * um_storage_x(u, r) / max(1, um_base_consumption(u, r)));
    }
    worth += u_speed(u) * u_hp(u);
    worth += (range == 12345 ? area.maxdim : range)
      * u_hp(u) / max(1, 10 - u_speed(u));
    for_all_unit_types(u2) {
	worth += (worth * uu_capture(u, u2)) / 150;
    }
    worth = isqrt(worth);
    DMprintf("unit type %s worth %d \n ", u_type_name(u), worth);
    return worth;
}

/* A basic estimate of the payoff of one unit type attacking another type
   directly.  This is "context-free", does not account for overall goals etc. */

/* (should account for number of attacks possible in one turn) */

int
basic_hit_worth(u, e)
int u, e;
{
    int dam, numdice, numspots, offset, avgdamage, worth = 0, anti = 0;

    if (uu_acp_to_attack(u, e) < 1)
      return -9999;
    dam = uu_damage(u, e);
    DICE(dam, numdice, numspots, offset);
    avgdamage = offset + (numdice * numspots) / 2;
    if (avgdamage > u_hp(e))
      avgdamage = u_hp(e);
    worth = (uu_hit(u, e) * avgdamage) / u_hp(e);
    if (1 /* strength of counterattack */) {
	dam = uu_damage(e, u);
	DICE(dam, numdice, numspots, offset);
	avgdamage = offset + (numdice * numspots) / 2;
	if (avgdamage > u_hp(u))
	  avgdamage = u_hp(u);
	anti = (uu_hit(e, u) * avgdamage) / u_hp(u);
    }
    return worth - (anti * 9) / 10;
}

int
basic_fire_worth(u, e)
int u, e;
{
    int hitchance, dam, numdice, numspots, offset, avgdamage, worth = 0;

    if (u_acp_to_fire(u) < 1)
      return -9999;
    if (uu_fire_hit(u, e) != -1)
      hitchance = uu_fire_hit(u, e);
    else
      hitchance = uu_hit(u, e);
    if (uu_fire_damage(u, e) != -1)
      dam = uu_fire_damage(u, e);
    else
      dam = uu_damage(u, e);
    DICE(dam, numdice, numspots, offset);
    avgdamage = offset + (numdice * numspots) / 2;
    if (avgdamage > u_hp(e))
      avgdamage = u_hp(e);
    worth = (hitchance * avgdamage) / u_hp(e);
    return worth;
}

/* A crude estimate of the payoff of one unit type trying to capture. */

int
basic_capture_worth(u, e)
int u, e;
{
    int worth1 = 0, worth2 = 0;

    if (uu_acp_to_capture(u, e) < 1 && uu_acp_to_attack(u, e) < 1)
      return -9999;
    if (uu_capture(u, e) > 0) {
	worth1 = uu_capture(u, e) * u_acp(u) /* divided by acp/attack */;
    }
    if (uu_indep_capture(u, e) > 0) {
	worth2 = uu_indep_capture(u, e) * u_acp(u) /* divided by acp/attack */;
    }
    return max(worth1, worth2);
}

int
basic_transport_worth(u, u2)
int u, u2;
{
    int worth = 0;

    if (could_carry(u, u2)) {
	worth += 1;
    }
    return worth;
}

/* Some notion of the unit's "strength"? */

int
unit_strength(u)
int u;
{
    return 1;
}

/* Display the results of our calculations. */

void
display_assessment()
{
    int t, u, u2;

    DMprintf("\nUnit Attributes:\n");
    for_all_unit_types(u) {
	DMprintf(" %-3.3s : base %d, transport %d, carrier %d, worth %d\n",
	       shortest_unique_name(u), u_is_base(u),
	       u_is_transport(u), u_is_carrier(u), u_bw(u));
	DMprintf("    Operate between ranges %d and %d\n", operating_range_worst(u), operating_range_best(u));
    }
    DMprintf("\nUnit vs Unit Combat:\n");
    for_all_unit_types(u) {
	DMprintf(" %-3.3s:", shortest_unique_name(u));
	for_all_unit_types(u2)
	  DMprintf("%5d", uu_zz_bhw(u, u2));
	DMprintf("\n");
    }
    DMprintf("\nUnit vs Unit Fire:\n");
    for_all_unit_types(u) {
	DMprintf(" %-3.3s:", shortest_unique_name(u));
	for_all_unit_types(u2)
	  DMprintf("%5d", uu_zz_bfw(u, u2));
	DMprintf("\n");
    }
    DMprintf("\nUnit vs Unit Capture:\n");
    for_all_unit_types(u) {
	DMprintf(" %-3.3s:", shortest_unique_name(u));
	for_all_unit_types(u2)
	  DMprintf(" %4d", uu_zz_bcw(u, u2));
	DMprintf("\n");
    }
    DMprintf("\nUnit vs Unit Transport:\n");
    for_all_unit_types(u) {
	DMprintf(" %-3.3s:", shortest_unique_name(u));
	for_all_unit_types(u2)
	  DMprintf(" %4d", uu_zz_btw(u, u2));
	DMprintf("\n");
    }
    DMprintf("\n");
}

int
is_base_for(u1, u2)
int u1, u2;
{
    return (u_speed(u1) == 0
	    && (uu_capacity_x(u2, u1) > 0
		|| (uu_size(u2, u1) <= u_capacity(u1))));
}

int
is_carrier_for(u1, u2)
int u1, u2;
{
    return (u_speed(u1) > 0
	    && (uu_capacity_x(u2, u1) > 0
		|| (uu_size(u2, u1) <= u_capacity(u1))));
}

/* Since *.def parameters don't have setters usually, we have to supply
   some here.  These are very sensitive to how the parameters are organized,
   and they don't do any checking, so you have to careful about using them. */

void set_u_is_base(u, n) int u, n; {  utypes[u].is_base = n;  }
void set_u_is_transport(u, n) int u, n; {  utypes[u].is_transport = n;  }
void set_u_is_carrier(u, n) int u, n; {  utypes[u].is_carrier = n;  }
void set_u_is_base_builder(u, n) int u, n; {  utypes[u].is_base_builder = n;  }
void set_u_can_make(u, n) int u, n; {  utypes[u].can_make = n;  }
void set_u_can_capture(u, n) int u, n; {  utypes[u].can_capture = n;  }
void set_u_bw(u, n) int u, n; {  utypes[u].bw = n;  }

int bhwtab = -1;
int bfwtab = -1;
int bcwtab = -1;
int btwtab = -1;

void
set_uu_bhw(u1, u2, v)
int u1, u2, v;
{
    if (bhwtab < 0) {
	for (bhwtab = 0; tabledefns[bhwtab].name != NULL; ++bhwtab) {
	    if (strcmp("zz-basic-hit-worth", tabledefns[bhwtab].name) == 0) {
		allocate_table(bhwtab, FALSE);
		break;
	    }
	}
    }
    if (tabledefns[bhwtab].table == NULL)
      run_error("no bhw table allocated");
    (*(tabledefns[bhwtab].table))[numutypes * u1 + u2] = v;
}

void
set_uu_bfw(u1, u2, v)
int u1, u2, v;
{
    if (bfwtab < 0) {
	for (bfwtab = 0; tabledefns[bfwtab].name != NULL; ++bfwtab) {
	    if (strcmp("zz-basic-fire-worth", tabledefns[bfwtab].name) == 0) {
		allocate_table(bfwtab, FALSE);
		break;
	    }
	}
    }
    if (tabledefns[bfwtab].table == NULL)
      run_error("no bfw table allocated");
    (*(tabledefns[bfwtab].table))[numutypes * u1 + u2] = v;
}

void
set_uu_bcw(u1, u2, v)
int u1, u2, v;
{
    if (bcwtab < 0) {
	for (bcwtab = 0; tabledefns[bcwtab].name != NULL; ++bcwtab) {
	    if (strcmp("zz-basic-capture-worth", tabledefns[bcwtab].name) == 0) {
		allocate_table(bcwtab, FALSE);
		break;
	    }
	}
    }
    if (tabledefns[bcwtab].table == NULL)
      run_error("no bcw table allocated");
    (*(tabledefns[bcwtab].table))[numutypes * u1 + u2] = v;
}


void
set_uu_btw(u1, u2, v)
int u1, u2, v;
{
    if (btwtab < 0) {
	for (btwtab = 0; tabledefns[btwtab].name != NULL; ++btwtab) {
	    if (strcmp("zz-basic-transport-worth", tabledefns[btwtab].name) == 0) {
		allocate_table(btwtab, FALSE);
		break;
	    }
	}
    }
    if (tabledefns[btwtab].table == NULL)
      run_error("no btw table allocated");
    (*(tabledefns[btwtab].table))[numutypes * u1 + u2] = v;
}
