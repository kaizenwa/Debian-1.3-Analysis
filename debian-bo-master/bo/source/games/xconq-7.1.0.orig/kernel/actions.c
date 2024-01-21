/* Implementations of Xconq actions.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* The general theory of actions is that interface or AI code calls, for
   an action foo, the routine prep_foo_action, which just records the action
   for later execution.  The action loop in run_game eventually calls
   do_foo_action, which first calls check_foo_action to confirm that the
   action will succeed.  If check_foo_action does not find any errors, then
   the action cannot fail.  The main body of do_foo_action then implements
   the effects of the action.  Interfaces may call check_foo_action freely,
   but should never call do_foo_action directly. */

#include "conq.h"
extern int tmphevtdata1;
extern int construction_possible PARAMS ((int u2));
extern void broadcast_next_action PARAMS ((Unit *unit));
extern void low_send PARAMS ((int id, char *buf));
extern void add_to_unit_hp PARAMS ((Unit *unit, int hp));
extern void try_sharing PARAMS ((Unit *from, Unit *to, int m));

extern void change_cell PARAMS ((Unit *unit, int x, int y));

extern int max_detonate_on_approach_range;
extern int max_u_detonate_effect_range;
extern int max_t_detonate_effect_range;

int numremotes;

extern int number_member PARAMS ((int x, Obj *lis));
extern int wind_value PARAMS ((Unit *unit, int angle, int force, Obj *effect, int maxval));

static void try_detonate_on_approach PARAMS ((int x, int y));
static void set_created_unit_props PARAMS ((Unit *unit, int u2, Side *side));
static void play_action_messages PARAMS ((Unit *unit, Action *action));

/* We can't declare all the action functions as static because some of them
   are in other files, but don't let them be visible to all files. */

#undef  DEF_ACTION
#define DEF_ACTION(name,code,args,prepfn,DOFN,checkfn,ARGDECL,doc)  \
  extern int DOFN PARAMS (ARGDECL);

#include "action.def"

/* The table of all the types of actions. */

ActionDefn actiondefns[] = {

#undef  DEF_ACTION
#define DEF_ACTION(NAME,CODE,ARGS,prepfn,DOFN,CHECKFN,argdecl,doc)  \
    { CODE, NAME, ARGS, DOFN, CHECKFN },

#include "action.def"

    { -1, NULL, NULL, NULL }
};

/* This is used to indicate that a move is a retreat; for normal movement it will
   always be false. */

int retreating;

/* This is a specific type of unit that the retreater is running away from. */

int retreating_from = NONUTYPE;

char *actiondesigbuf = NULL;

/* Do any action-related initialization. */

void
init_actions()
{
}

void
broadcast_next_action(unit)
Unit *unit;
{
    int i, j, atype, n;
    char buf[BUFSIZE], smallbuf[32];

    if (numremotes <= 0)
      return;
    atype = unit->act->nextaction.type;
    sprintf(buf, "A %d", unit->id);
    if (unit->id != unit->act->nextaction.actee) {
	sprintf(smallbuf, "/%d", unit->act->nextaction.actee);
	strcat(buf, smallbuf);
    }
    sprintf(smallbuf, " %d", unit->act->nextaction.type);
    strcat(buf, smallbuf);
    n = strlen(actiondefns[atype].argtypes);
    for (j = 0; j < n; ++j) {
	sprintf(smallbuf, " %d", unit->act->nextaction.args[j]);
	strcat(buf, smallbuf);
    }
    /* Send the formatted description of the action to all the remotes. */
    for (i = 1; i < numremotes + 1; ++i) {
	low_send(i, buf);
    }
}

/* Just a placeholder action, so not much to do here. */

int
prep_none_action(unit, unit2)
Unit *unit, *unit2;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_NONE;
    unit->act->nextaction.actee = unit2->id;
    return TRUE;
}

int
do_none_action(unit, unit2)
Unit *unit, *unit2;
{
    return A_ANY_DONE;
}

int
check_none_action(unit, unit2)
Unit *unit, *unit2;
{
    return A_ANY_DONE;
}

/* Movement actions. */

/* Record a move action as the next to do. */

int
prep_move_action(unit, unit2, x, y, z)
Unit *unit, *unit2;
int x, y, z;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_MOVE;
    unit->act->nextaction.args[0] = x;
    unit->act->nextaction.args[1] = y;
    unit->act->nextaction.args[2] = z;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

/* The basic act of moving.  This attempts to move and maybe fails, but
   takes no corrective action.  Note that this requires space in the
   destination cell, will not board, attack, etc - all that is task- and
   plan-level behavior. */

int
do_move_action(unit, unit2, nx, ny, nz)
Unit *unit, *unit2;
int nx, ny, nz;
{
    int u, u2, nu2, t, rslt, speed, mpcost = 0, acpcost, ox, oy, oz;

    u = unit->type;  u2 = unit2->type;
    t = terrain_at(nx, ny);
    ox = unit2->x;  oy = unit2->y;  oz = unit2->z;
    speed = 100;
    mpcost = 1;
    if (!inside_area(nx, ny)) {
	kill_unit(unit2, H_UNIT_LEFT_WORLD);
	rslt = A_ANY_DONE;
    } else if (ut_vanishes_on(u2, t) && !can_move_via_conn(unit2, nx, ny)) {
	kill_unit(unit2, H_UNIT_VANISHED);
	rslt = A_ANY_DONE; /* should return something else :-) */
    } else if (ut_wrecks_on(u2, t) && !can_move_via_conn(unit2, nx, ny)) {
	if (u_wrecked_type(u2) == NONUTYPE) {
	    /* Occupants always die if the wrecked unit disappears. */
	    kill_unit(unit, H_UNIT_WRECKED);
	} else {
	    /* Wreck the unit.  Note that we want to do the wrecking even
	       if the unit will vanish, so that occupants can escape if
	       allowed for. */
	    if (!ut_vanishes_on(u_wrecked_type(u2), t)) {
		speed = unit_speed(unit2, nx, ny);
		mpcost = move_unit(unit2, nx, ny);
	    }
	    /* Change the unit's type at its new location. */
	    change_unit_type(unit2, u_wrecked_type(u2), H_UNIT_WRECKED);
	    nu2 = unit2->type;
	    /* Restore to default hp for the new type. */
	    unit2->hp = unit2->hp2 = u_hp(nu2);
	    /* Get rid of occupants if now overfull. */
	    eject_excess_occupants(unit2);
	    /* Now make it go away, taking unlucky occupants with. */
	    if (ut_vanishes_on(nu2, t)) {
		kill_unit(unit2, H_UNIT_VANISHED);
	    }
	}
	rslt = A_ANY_DONE;
    } else {
	speed = unit_speed(unit2, nx, ny);
	mpcost = move_unit(unit2, nx, ny);
	/* ZOC move cost is added after action is done. */
	mpcost += zoc_move_cost(unit2, ox, oy, oz);
	rslt = A_ANY_DONE;
    }
    if (alive(unit)) {
        if (speed > 0) {
		acpcost = (mpcost * 100) / speed;
	} else {
		acpcost = 1;
	}
	acpcost = max(acpcost, u_acp_to_move(u2));
	if (acpcost < 1)
	  acpcost = 1;
	use_up_acp(unit, acpcost);
    }
    /* Count the unit as having actually moved. */
    if (alive(unit2) && unit2->act)
      ++(unit2->act->actualmoves);
    return rslt;
}

int
check_move_action(unit, unit2, nx, ny, nz)
Unit *unit, *unit2;
int nx, ny, nz;
{
    int u, u2, u3, ox, oy, oz, acp, acpavail, mpavail, totcost, m, speed;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    /* Note that edge cell dests (used to leave the world) are allowed. */
    if (!in_area(nx, ny))
      return A_ANY_ERROR;
    u = unit->type;  u2 = unit2->type;
    ox = unit2->x;  oy = unit2->y;  oz = unit2->z;
    acp = u_acp_to_move(u2);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    acpavail = unit->act->acp;
    /* If this action is a part of retreating, add more acp to represent the
       motivational power of needing to run away... */
    if (retreating && unit == unit2) {
	if (retreating_from != NONUTYPE) {
	    acpavail += uu_acp_retreat(u2, retreating_from);
	}
    }
    /* (should not have to modify the unit, but succeeding calls need this) */
    unit->act->acp = acpavail;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    if (!has_supply_to_act(unit2))
      return A_ANY_NO_MATERIAL;
    /* Destination is outside the world and we're not allowed to leave. */
    if (!inside_area(nx, ny) && u_mp_to_leave_world(u2) < 0)
	return A_MOVE_CANNOT_LEAVE_WORLD;
    /* Check if the destination is within our move range. */
    /* (also check for and maybe allow border slides here) */
    if (distance(ox, oy, nx, ny) > u_move_range(u2))
      return A_ANY_TOO_FAR;
    if (nz > 0)
      return A_ANY_TOO_FAR;
    /* Check if the destination is in a blocking ZOC. */
    if (in_blocking_zoc(unit, nx, ny, nz))
      return A_ANY_ERROR;
    /* Now start looking at the move costs. */
    u3 = (unit2->transport ? unit2->transport->type : NONUTYPE);
    totcost = total_move_cost(u2, u3, ox, oy, oz, nx, ny, nz);
    speed = unit_speed(unit2, nx, ny);
    mpavail = (unit->act->acp * speed) / 100;
    /* take into account acp-min in computing mpavail; Massimo */
    if (u_acp_min(u2) < 0)
      mpavail = ((unit->act->acp - u_acp_min(u2)) * speed) / 100;
    /* Zero mp always disallows movement, unless intra-cell. */
    if (mpavail <= 0 && !(ox == nx && oy == ny && oz == nz))
      return A_MOVE_NO_MP;
    /* The free mp might get us enough moves, so add it before comparing. */
    if (mpavail + u_free_mp(u2) < totcost)
      return A_MOVE_NO_MP;
    /* If destination is too small or already full, we can't move into it. */
    if ((nz & 1) == 0) {
	if (!can_occupy_cell(unit2, nx, ny))
	  return A_MOVE_DEST_FULL;
    } else {
	if (!can_occupy_conn(unit2, nx, ny, nz))
	  return A_MOVE_DEST_FULL;
    }
    /* We have to have a minimum level of supply to be able to move. */
    for_all_material_types(m) {
	if (unit2->supply[m] < um_to_move(u2, m))
	  return A_ANY_NO_MATERIAL;
    }
    return A_ANY_OK;
}

int
can_move_via_conn(unit, nx, ny)
Unit *unit;
int nx, ny;
{
    int c, dir;

    if (numconntypes == 0)
      return FALSE;
    for_all_terrain_types(c) {
	    if (t_is_connection(c)
		&& aux_terrain_defined(c)
		&& (dir = closest_dir(nx - unit->x, ny - unit->y)) >= 0
		&& connection_at(unit->x, unit->y, dir, c)
		&& !ut_vanishes_on(unit->type, c)
		&& !ut_wrecks_on(unit->type, c)) {
		return TRUE;
	    }
    }
    return FALSE;
}

int
unit_speed(unit, nx, ny)
Unit *unit;
int nx, ny;
{
    int u = unit->type, speed, x = unit->x, y = unit->y, angle, windval;
    int occeff, totocceff, totoccdenom;
    Unit *occ;
    
    speed = u_speed(u);
    if (unit->hp < u_hp_max(u) && u_speed_damage_effect(u) != lispnil) {
	speed = damaged_value(unit, u_speed_damage_effect(u), speed);
    }
    if (winds_defined() && u_speed_wind_effect(u) != lispnil) {
	angle = angle_with(closest_dir(nx - x, nx - y), wind_dir_at(x, y));
	windval = wind_value(unit, angle, wind_force_at(x, y), u_speed_wind_effect(u), 10000);
	speed = (speed * windval) / 100;
    }
    if (unit->occupant /* and any occupant speed effects */) {
        totocceff = 100;
        totoccdenom = 100;
    	for_all_occupants(unit, occ) {
	    if (completed(occ)) {
		occeff = uu_speed_occ_effect(u, occ->type);
		if (occeff != 100) {
		    totocceff *= occeff;
		    totoccdenom *= 100;
		}
	    }
    	}
    	speed = (speed * totocceff) / totoccdenom;
    }
    /* Clip to limits. */
    speed = max(speed, u_speed_min(u));
    speed = min(speed, u_speed_max(u));
    return speed;
}

/* Compute and return value for a damaged unit, using a list of (hp val) pairs
   and interpolating between them. */

int
damaged_value(unit, effect, maxval)
Unit *unit;
Obj *effect;
int maxval;
{
    int u, err, rslt;

    u = unit->type;
    err = interpolate_in_list_ext(unit->hp, effect, 0, 0, 0, 0, u_hp(u), maxval, &rslt);
    if (err != 0) {
	run_warning("cannot get damaged speed for %s at hp %d, using 100",
		    u_type_name(u), unit->hp);
	rslt = 100;
    }
    return rslt;
}

/* Compute and return the wind's effect on a unit, using a list of lists
   and interpolating between them. */

int
wind_value(unit, angle, force, effect, maxval)
Unit *unit;
int angle, force, maxval;
Obj *effect;
{
    int err, rslt;
    Obj *rest, *head, *key, *val;

    for (rest = effect; rest != lispnil; rest = cdr(rest)) {
	head = car(rest);
	key = car(head);
	if ((numberp(key) && angle == c_number(key))
	    || (symbolp(key))
	    || (consp(key) && number_member(angle, key))) {
	    val = cadr(head);
	    if (numberp(val))
	      return c_number(val);
	    else {
		err = interpolate_in_list(force, val, &rslt);
		if (err == 0) {
		    return rslt;
		} else {
		    run_warning("no value for wind angle=%d force=%d", angle, force);
		    return maxval;
		}
	    }
	}
    }
    return maxval;
}

int
number_member(x, lis)
int x;
Obj *lis;
{
    Obj *rest;

    if (lis == lispnil) {
	return FALSE;
    } else if (!consp(lis)) {
	/* should probably be an error of some sort */
	return FALSE;
    }
    for (rest = lis; rest != lispnil; rest = cdr(rest)) {
	if (numberp(car(rest)) && x == c_number(car(rest)))
	  return TRUE;
    }
    return FALSE;
}


/* Conduct the actual move (used in both normal moves and some combat).
   Note that the new x,y may be the same as the old; this will happen
   if an occupant is getting off a transport but staying in the same cell. */

int
move_unit(unit, nx, ny)
Unit *unit;
int nx, ny;
{
    int u = unit->type, u3, ox = unit->x, oy = unit->y, oz = unit->z;
    int nz = oz;

    u3 = (unit->transport ? unit->transport->type : NONUTYPE);
    /* Disappear from the old location and appear at the new one. */
    if (unit->transport == NULL /* should be unconditional, but bugs still */)
      change_cell(unit, nx, ny);
    else {
      leave_cell(unit);
      enter_cell(unit, nx, ny);
    }
    /* Movement may set off other people's alarms. */
    maybe_react_to_move(unit, ox, oy);
    /* Might have auto-detonations in response. */
    if (max_detonate_on_approach_range >= 0) {
	detonate_on_approach_around(unit);
	/* A detonation might have been fatal, get out now if so. */
	if (!alive(unit))
	  return 1;
    }
    /* The people at the new location may change sides immediately. */
    if (people_sides_defined()
	&& any_people_side_changes
	&& probability(people_surrender_chance(u, nx, ny))) {
	change_people_side_around(nx, ny, u, unit->side);
    }
    /* Use up supplies as directed. */
    consume_move_supplies(unit);
    /* a hack */
    update_cell_display(unit->side, ox, oy, TRUE);
    /* Always return the mp cost, even if the mover died. */
    return total_move_cost(u, u3, ox, oy, oz, nx, ny, nz);
}

int
can_move_at_all(unit)
Unit *unit;
{
    return u_speed(unit->type) > 0;
}

/* This is true if the given location is in a blocking zoc for the unit. */

int
in_blocking_zoc(unit, x, y, z)
Unit *unit;
int x, y, z;
{
    int u = unit->type, t = terrain_at(x, y), dir, x1, y1, u2, range;
    Unit *unit2;

    if (max_zoc_range < 0)
      return FALSE;
    if (max_zoc_range >= 0) {
	for_all_stack(x, y, unit2) {
	    range = zoc_range(unit2, u);
	    if (range >= 0
		&& unit_blockable_by(unit, unit2)
		&& ut_zoc_into(unit2->type, t)
		&& ut_zoc_from_terrain(unit2->type, t) > 0)
	      return TRUE;
	}
    }
    if (max_zoc_range >= 1) {
	for_all_directions(dir) {
	    if (point_in_dir(x, y, dir, &x1, &y1)) {
		for_all_stack(x, y, unit2) {
		    u2 = unit2->type;
		    range = zoc_range(unit2, u);
		    if (range >= 1
			&& unit_blockable_by(unit, unit2)
			&& ut_zoc_into(u2, t))
		      return TRUE;
		}
	    }
	}
    }
    if (max_zoc_range >= 2) {
	run_warning("Max zoc range >= 2 not implemented");
    }
    return FALSE;
}

/* This is true if unit2 wants to block unit from moving. */

int
unit_blockable_by(unit, unit2)
Unit *unit, *unit2;
{
    return (!trusted_side(unit2->side, unit2->side) /* should make a better test */
	    && uu_mp_to_enter_zoc(unit->type, unit2->type) < 0);
}

/* Compute the number of move points that will be needed to do the given
   move. */

int
total_move_cost(u, u2, x1, y1, z1, x2, y2, z2)
int u, u2, x1, y1, z1, x2, y2, z2;
{
    int cost, ferry, b, c, conncost, dist, dir;

    if (z1 != 0 || z2 != 0) {
    	/* should write these calcs eventually */
    }
    dist = distance(x1, y1, x2, y2);
    if (dist == 0) {
	if (z2 != z1) {
	    /* (should have parms for up/down in same cell) */
	    return 1;
	} else {
    	    /* Unit is leaving a transport and moving into the open here;
    	       free of charge. */
    	    return 0;
    	}
    } else if (dist == 1) {
    	/* (fall through) */
    } else {
    	/* Border slide or multiple cell move. */
    	/* (should implement) */
    	return 9999;
    }
    cost = 0;
    ferry = 0;
    if (u2 != NONUTYPE) {
	/* Charge for leaving the transport. */
    	cost += uu_mp_to_leave(u, u2);
    	/* See what type of ferrying we're going to get. */
    	ferry = uu_ferry_on_leave(u2, u);
    }
    if (ferry < 1) {
    	cost += ut_mp_to_leave(u, terrain_at(x1, y1));
    }
    if (numbordtypes > 0 && ferry < 2) {
	/* Add any necessary border crossing costs. */
	dir = closest_dir(x2 - x1, y2 - y1);
	if (dir >= 0) {
	    for_all_terrain_types(b) {
		if (t_is_border(b)
		    && aux_terrain_defined(b)
		    && border_at(x1, y1, dir, b)) {
		    cost += ut_mp_to_enter(u, b);
		}
	    }
	}
    }
    if (ferry < 3) {
	cost += ut_mp_to_enter(u, terrain_at(x2, y2));
    }
    /* Use a connection traversal if it would be cheaper.  This is
       only automatic if the connection on/off costs are small enough,
       otherwise the unit has to do explicit actions to get on the
       connection and off again. */
    if (numconntypes > 0) {
	/* Try each connection type to see if it's better. */
	dir = closest_dir(x2 - x1, y2 - y1);
	if (dir >= 0) {
	    for_all_terrain_types(c) {
		if (t_is_connection(c)
		    && aux_terrain_defined(c)
		    && connection_at(x1, y1, dir, c)) {
		    conncost = ut_mp_to_enter(u, c)
		      + ut_mp_to_traverse(u, c)
			+ ut_mp_to_leave(u, c);
		    cost = min(cost, conncost);
		}
	    }
	}
    }
    /* The cost of leaving the world is always an addon. */
    if (!inside_area(x2, y2)) {
    	cost += u_mp_to_leave_world(u);
    }
    /* Any (inter-cell) movement must always cost at least 1 mp. */
    if (cost < 1)
      cost = 1;
    return cost;
}

int
zoc_range(unit, u2)
Unit *unit;
int u2;
{
    int u = unit->type;

    return (uu_zoc_range(u, u2)
	    * ut_zoc_from_terrain(u, terrain_at(unit->x, unit->y))) / 100;
}

int
zoc_move_cost(unit, ox, oy, oz)
Unit *unit;
int ox, oy, oz;
{
    int u = unit->type, u2, t1, t2, cost, mpcost, dir, x1, y1, range;
    Unit *unit2;

    /* If this is negative, ZOCs are not part of this game. */
    if (max_zoc_range < 0)
      return 0;
    if (!in_play(unit))
      return 0;
    mpcost = 0;
    t1 = terrain_at(ox, oy);
    t2 = terrain_at(unit->x, unit->y);
    if (max_zoc_range == 0 || max_zoc_range == 1) {
        /* ZOCs of units in old cell. */
	for_all_stack(ox, oy, unit2) {
	    u2 = unit2->type;
            range = zoc_range(unit2, u);
	    if (in_play(unit2) /* should be is_active? */
		&& unit2->side != unit->side
		&& range >= 0
		&& ut_zoc_into(u2, t1)
		/* should account for from-terrain also */
		)
	      mpcost = max(mpcost, uu_mp_to_leave_zoc(u, u2));
	}
	/* ZOCs of units in new cell. */
	for_all_stack(unit->x, unit->y, unit2) {
	    u2 = unit2->type;
            range = zoc_range(unit2, u);
	    if (in_play(unit2) /* should be is_active? */
		&& unit2->side != unit->side
		&& range >= 0
		&& ut_zoc_into(u2, t2))
	      mpcost = max(mpcost, uu_mp_to_enter_zoc(u, u2));
	}
    }
    if (max_zoc_range > 0) {
      if (max_zoc_range == 1) {
        /* ZOCs may be into adjacent cells. */
        /* Look for everybody that was exerting ZOC into the old location. */
        /* (should calc with stacked units also) */
	for_all_directions(dir) {
	    if (point_in_dir(ox, oy, dir, &x1, &y1)) {
		for_all_stack(x1, y1, unit2) {
		    u2 = unit2->type;
            	    range = zoc_range(unit2, u);
		    if (in_play(unit2) /* should be is_active? */
			&& unit2->side != unit->side  /* and unfriendly */
			&& range >= 1
			&& ut_zoc_into(u2, t1)) {
			if (1 /* leaving zoc */) {
			    cost = uu_mp_to_leave_zoc(u, u2);
			} else {
			    cost = uu_mp_to_traverse_zoc(u, u2);
			}
			mpcost = max(mpcost, cost);
		    }
		    /* (and occupants?) */
		}
	    }
	}
        /* Look for everybody that is now exerting ZOC into the new location. */
        /* (should calc with stacked units also) */
	for_all_directions(dir) {
	    if (point_in_dir(unit->x, unit->y, dir, &x1, &y1)) {
		for_all_stack(x1, y1, unit2) {
		    u2 = unit2->type;
            	    range = zoc_range(unit2, u);
		    if (in_play(unit2) /* should be is_active? */
			&& unit2->side != unit->side  /* and unfriendly */
			&& range >= 1
			&& ut_zoc_into(u2, t2)) {
			if (1 /* entering zoc */) {
			    cost = uu_mp_to_enter_zoc(u, u2);
			} else {
			    cost = uu_mp_to_traverse_zoc(u, u2);
			}
			mpcost = max(mpcost, cost);
		    }
		    /* (and occupants?) */
		}
	    }
	}
      } else {
	/* General case. */
        /* (should write this case - bleah, complicated) */
        run_error("No zoc ranges > 1 allowed");
      }
    }
    return mpcost;
}

/* This is a hook to handle any reactions to the unit's successful move. */

int
maybe_react_to_move(unit, ox, oy)
Unit *unit;
int ox, oy;
{
    return 0;
}

static void
try_detonate_on_approach(x, y)
int x, y;
{
    int dist;
    Unit *unit;

    dist = distance(tmpunit->x, tmpunit->y, x, y);
    for_all_stack(x, y, unit) {
	if (unit->side != tmpunit->side
	    && dist <= uu_detonate_approach_range(unit->type, tmpunit->type)
	    /* (should make doctrine-based decision about whether to go off) */
	    && !was_detonated(unit)
	    ) {
	    detonate_unit(unit, unit->x, unit->y, unit->z);
	}
    }
}

void
detonate_on_approach_around(unit)
Unit *unit;
{
    int maxrange;

    tmpunit = unit;
    apply_to_area(unit->x, unit->y, max_detonate_on_approach_range, try_detonate_on_approach);
    maxrange = max(max_u_detonate_effect_range, max_t_detonate_effect_range) + max_detonate_on_approach_range;
    reckon_damage_around(unit->x, unit->y, maxrange);
}

/* Use up the supply consumed by a successful move.  Also, the move might
   have used up essentials and left the unit without its survival needs,
   so check for this case and maybe hit/kill the unit. */

void  
consume_move_supplies(unit)
Unit *unit;
{
    int u = unit->type, m, checkstarve = FALSE;
    
    for_all_material_types(m) {
	if (um_consumption_per_move(u, m) > 0) {
	    unit->supply[m] -= um_consumption_per_move(u, m);
	    /* Don't let supply go below zero. */
	    if (unit->supply[m] <= 0) {
		unit->supply[m] = 0;
		checkstarve = TRUE;
	    }
	}
    }
    if (checkstarve)
      maybe_starve(unit, FALSE);
    /* Trigger any supply alarms. */
    if (alive(unit)
    	&& unit->plan
    	&& !unit->plan->supply_is_low
    	&& past_halfway_point(unit)
    	) {
    	unit->plan->supply_is_low = TRUE;
    	update_unit_display(unit->side, unit, TRUE); 
    }
}

/* Movement into another unit. */

/* Record an enter action as the next to do. */

int
prep_enter_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    if (unit3 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_ENTER;
    unit->act->nextaction.args[0] = unit3->id;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_enter_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    int u2, u3, u4, ox, oy, oz, nx, ny, nz, speed, acpcost, mpcost;

    u2 = unit2->type;
    ox = unit2->x;  oy = unit2->y;  oz = unit2->z;
    u3 = unit3->type;
    nx = unit3->x;  ny = unit3->y;  nz = unit3->z;
    /* Change the unit's position. */
    leave_cell(unit2);
    enter_transport(unit2, unit3);
    /* Calculate how much acp has been used up. */
    u4 = (unit2->transport ? unit2->transport->type : NONUTYPE);
    mpcost = total_entry_cost(u2, u4, ox, oy, oz, u3, nx, ny, nz);
    if (alive(unit)) {
	speed = u_speed(u2);
	if (speed > 0) {
	    acpcost = (mpcost * 100) / speed;
	} else {
	    acpcost = 1;
	}
	use_up_acp(unit, acpcost + uu_acp_to_enter(u2, u3));
    }
    return A_ANY_DONE;
}

int
check_enter_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    int u, u2, u3, u4, u2x, u2y, u3x, u3y, totcost, speed, mpavail;
    int ox, oy, oz, nx, ny, nz;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!in_play(unit3))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    u3 = unit3->type;
    if (uu_acp_to_enter(u2, u3) < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, uu_acp_to_enter(u2, u3)))
      return A_ANY_CANNOT_DO;
    /* Can't enter self. */
    if (unit2 == unit3)
      return A_ANY_ERROR;
    u2x = unit2->x;  u2y = unit2->y;
    u3x = unit3->x;  u3y = unit3->y;
    ox = unit2->x;  oy = unit2->y;  oz = unit2->z;
    nx = unit3->x;  ny = unit3->y;  nz = unit3->z;
    if (!between(0, distance(ox, oy, nx, ny), 1))
      return A_ANY_ERROR;
    if (!sides_allow_entry(unit2, unit3))
      return A_ANY_ERROR;
    if (!can_occupy(unit2, unit3))
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, uu_acp_to_enter(u2, u3)))
      return A_ANY_NO_ACP;
    u4 = (unit2->transport ? unit2->transport->type : NONUTYPE);
    totcost = total_entry_cost(u2, u4, ox, oy, oz, u3, nx, ny, nz);
    speed = u_speed(u2);
    /* (should generalize!) */
    if (winds_defined() && u_speed_wind_effect(u2) != lispnil) {
	speed *= wind_force_at(u3x, u3y);
    }
    if (speed > 0 && unit->act) {
	mpavail = (unit->act->acp * speed) / 100;
    } else {
	mpavail = 0;
    }
    /* If transport picks up the unit itself, no need to check mp. */
    if (uu_ferry_on_enter(u3, u2) < 3) {
	/* Zero mp always disallows movement. */
	if (mpavail <= 0)
	  return A_MOVE_NO_MP;
	/* The free mp might get us enough moves, so add it before comparing. */
	if (mpavail + u_free_mp(u2) < totcost)
	  return A_MOVE_NO_MP;
    }
    /* (should check materials available) */
    return A_ANY_OK;
}

/* This tests whether the relationship between the sides of a unit
   and a prospective transport allows for entry of the unit. */

int
sides_allow_entry(unit, transport)
Unit *unit, *transport;
{
    if (unit->side == NULL) {
    	if (transport->side == NULL) {
    	    return TRUE;
    	} else {
    	    return uu_can_enter_indep(unit->type, transport->type);
    	}
    } else {
    	if (transport->side == NULL) {
    	    return uu_can_enter_indep(unit->type, transport->type);
    	} else {
	    /* Note that because this is for an explicit action, the unit
	       must trust the transport, so the only test is whether the
	       transports trusts the unit enough to have it as an occupant. */
    	    return unit_trusts_unit(transport, unit);
    	}
    }
}

/* This computes the total mp cost of entering a transport. */

int
total_entry_cost(u1, u3, x1, y1, z1, u2, x2, y2, z2)
int u1, u3, x1, y1, z1, u2, x2, y2, z2;
{
    int cost = 0, ferryout, ferryin, t1, t2, b, dir, conncost, c;

    ferryout = 0;
    ferryin = uu_ferry_on_enter(u2, u1);
    if (u3 != NONUTYPE) {
	/* Charge for leaving the transport. */
    	cost += uu_mp_to_leave(u1, u3);
    	ferryout = uu_ferry_on_leave(u3, u1);
    }
    /* (should include possibility of using conn to cross terrain) */
    /* Maybe add cost to leave terrain of own cell. */
    if (ferryout < 1 && ferryin < 3) {
	t1 = terrain_at(x1, y1);
    	cost += ut_mp_to_leave(u1, t1);
    }
    /* Maybe add cost to cross one (or more) borders. */
    if (numbordtypes > 0 && ferryout < 2 && ferryin < 2) {
	dir = closest_dir(x2 - x1, y2 - y1);
	if (dir >= 0) {
	    for_all_terrain_types(b) {
		if (t_is_border(b)
		    && aux_terrain_defined(b)
		    && border_at(x1, y1, dir, b)) {
		    cost += ut_mp_to_enter(u1, b);
		}
	    }
	}
    }
    /* Maybe even have to pay cost of crossing destination's terrain. */
    if (ferryout < 3 && ferryin < 1) {
	t2 = terrain_at(x2, y2);
    	cost += ut_mp_to_enter(u1, t2);
    }
    /* Use a connection traversal if it would be cheaper.  This is
       only automatic if the connection on/off costs are small enough,
       otherwise the unit has to do explicit actions to get on the
       connection and off again. */
    if (numconntypes > 0) {
	/* Try each connection type to see if it's better. */
	dir = closest_dir(x2 - x1, y2 - y1);
	if (dir >= 0) {
	    for_all_terrain_types(c) {
		if (t_is_connection(c)
		    && aux_terrain_defined(c)
		    && connection_at(x1, y1, dir, c)) {
		    conncost = ut_mp_to_enter(u1, c)
		      + ut_mp_to_traverse(u1, c)
			+ ut_mp_to_leave(u1, c);
		    cost = min(cost, conncost);
		}
	    }
	}
    }
    /* Add the actual cost of entry. */
    cost += uu_mp_to_enter(u1, u2);
    /* Movement must always cost at least 1 mp. */
    if (cost < 1)
      cost = 1;
    return cost;
}

/* Material actions. */

/* Explicit material production. */

int
prep_produce_action(unit, unit2, m, n)
Unit *unit, *unit2;
int m, n;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_PRODUCE;
    unit->act->nextaction.args[0] = m;
    unit->act->nextaction.args[1] = n;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_produce_action(unit, unit2, m, n)
Unit *unit, *unit2;
int m, n;
{
    int amt;

    amt = min(n, um_material_per_production(unit2->type, m));
    unit2->supply[m] += n;
    use_up_acp(unit, um_acp_to_produce(unit2->type, m));
    return A_ANY_DONE;
}

int
check_produce_action(unit, unit2, m, n)
Unit *unit, *unit2;
int m, n;
{
    int acp, m2;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!is_material_type(m))
      return A_ANY_ERROR;
    acp = um_acp_to_produce(unit2->type, m);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    if (um_material_per_production(unit2->type, m) < 1)
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    /* Check that the unit has any required supplies. */
    for_all_material_types(m2) {
	if (unit2->supply[m2] < um_to_produce(unit2->type, m2))
	  return A_ANY_NO_MATERIAL;
    }
    return A_ANY_OK;
}

/* Transfer action. */

/* This action transfers material from one unit to another. */

int
prep_transfer_action(unit, unit2, m, n, unit3)
Unit *unit, *unit2, *unit3;
int m, n;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    if (unit3 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_TRANSFER;
    unit->act->nextaction.args[0] = m;
    unit->act->nextaction.args[1] = n;
    unit->act->nextaction.args[2] = unit3->id;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_transfer_action(unit, unit2, m, n, unit3)
Unit *unit, *unit2, *unit3;
int m, n;
{
    int actual;

    if (n > 0) {
    	actual = transfer_supply(unit2, unit3, m, n);
    } else {
    	actual = transfer_supply(unit3, unit2, m, -n);
    }
    use_up_acp(unit, 1);
    if (actual == n) {
	return A_ANY_DONE;
    } else if (actual == 0) {
	return A_ANY_ERROR;
    } else {
    	/* (should be able to say that action did not do all that was requested) */
	return A_ANY_DONE;
    }
}

int
check_transfer_action(unit, unit2, m, n, unit3)
Unit *unit, *unit2, *unit3;
int m, n;
{
    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!is_material_type(m))
      return A_ANY_ERROR;
    if (n == 0)
      return A_ANY_ERROR;
    if (!in_play(unit3))
      return A_ANY_ERROR;
    if (um_acp_to_unload(unit2->type, m) < 1)
      return A_ANY_CANNOT_DO;
    if (unit3->act && um_acp_to_load(unit3->type, m) < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, 1))
      return A_ANY_CANNOT_DO;
    if (n > 0) {
	if (unit2->supply[m] <= 0)
	  return A_ANY_ERROR;
	if (um_storage_x(unit3->type, m) == 0)
	  return A_ANY_ERROR;
    } else {
	if (unit3->supply[m] <= 0)
	  return A_ANY_ERROR;
	if (um_storage_x(unit2->type, m) == 0)
	  return A_ANY_ERROR;
    }
    if (!has_enough_acp(unit, 1))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Move supply from one unit to another.  Don't move more than is possible;
   check both from and to amounts and capacities. */

int
transfer_supply(from, to, m, amount)
Unit *from, *to;
int m, amount;
{
    int origfrom = from->supply[m], origto = to->supply[m];

    amount = min(amount, origfrom);
    amount = min(amount, um_storage_x(to->type, m) - origto);
    if (um_unload_max(from->type, m) >= 0) {
	amount = min(amount, um_unload_max(from->type, m));
    }
    if (um_load_max(to->type, m) >= 0) {
	amount = min(amount, um_load_max(to->type, m));
    }
    from->supply[m] -= amount;
    to->supply[m] += amount;
    /* Make sure any displays of supply levels see the transfer. */
    update_unit_display(from->side, from, TRUE); 
    update_unit_display(to->side, to, TRUE); 
    Dprintf("%s (had %d) transfers %d %s to %s (had %d)\n",
	    unit_desig(from), origfrom, amount, m_type_name(m),
	    unit_desig(to), origto);
    return amount;
}

/* Research action. */

/* If a side's tech level is under its max, research can increase it. */

int
prep_research_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_RESEARCH;
    unit->act->nextaction.args[0] = u3;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_research_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    int u2 = unit2->type, lim;
    Side *side = unit2->side;

    side->tech[u3] += prob_fraction(uu_tech_per_research(u2, u3));
    /* Silently apply the per-side-per-turn limit on tech gains. */
    lim =  side->inittech[u3] + u_tech_per_turn_max(u3);
    if (side->tech[u3] > lim)
      side->tech[u3] = lim;
    /* Adjust the tech levels of any related unit types to match. */
    adjust_tech_crossover(side, u3);
    /* (should notify side about changes and/or thresholds reached?) */
    use_up_acp(unit, uu_acp_to_research(u2, u3));
    return A_ANY_DONE;
}

int
check_research_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    int u, u2;
    Side *side;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!is_unit_type(u3))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    side = unit->side;
    /* Independent units don't do research. */
    if (side == NULL)
      return A_ANY_ERROR;
    /* This unit must be of a type that can research the given type. */
    if (uu_acp_to_research(u2, u3) < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, uu_acp_to_research(u2, u3)))
      return A_ANY_CANNOT_DO;
    /* Max tech level means there's nothing more to learn. */
    if (side->tech[u3] >= u_tech_max(u3))
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, uu_acp_to_research(u2, u3)))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* For all unit types, bring their tech level up to match the crossovers
   from the given unit type. */

void
adjust_tech_crossover(side, u)
Side *side;
int u;
{
    int u2, cross;

    for_all_unit_types(u2) {
	if (u2 != u) {
	    /* (should be "as ratio of max levels for each type") */
	    cross = (uu_tech_crossover(u, u2) * side->tech[u2]) / 100;
	    if (cross > side->tech[u2]) side->tech[u2] = cross;
	}
    }
}

/* Toolup action. */

/* Before a unit can build another, it may need to take some time to prepare by
   "tooling up". */

int
prep_toolup_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_TOOL_UP;
    unit->act->nextaction.args[0] = u3;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_toolup_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    int tp;

    if (unit2->tooling == NULL)
      init_unit_tooling(unit2);
    /* Increase the tooling, clipping to its max. */
    tp = unit2->tooling[u3];
    tp += uu_tp_per_toolup(unit2->type, u3);
    tp = min(tp, uu_tp_max(unit2->type, u3));
    unit2->tooling[u3] = tp;
    /* Adjust any related toolings. */
    adjust_tooling_crossover(unit2, u3);
    /* Consume acp. */
    use_up_acp(unit, uu_acp_to_toolup(unit2->type, u3));
    return A_ANY_DONE;
}

int
check_toolup_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    int acp, tp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!is_unit_type(u3))
      return A_ANY_ERROR;
    acp = uu_acp_to_toolup(unit2->type, u3);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    tp = (unit2->tooling ? unit2->tooling[u3] : 0);
    /* Check if tooling is already at its max. */
    if (tp >= uu_tp_max(unit2->type, u3))
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* For all unit types, bring their tooling level up to match the crossovers
   from the given unit type. */

void
adjust_tooling_crossover(unit, u2)
Unit *unit;
int u2;
{
    int u3, uucross, cross, tp2, tp3;

    /* Perhaps nothing to cross over with. */
    if (unit->tooling == NULL)
      return;
    for_all_unit_types(u3) {
	if (u3 != u2) {
	    uucross = uu_tp_crossover(u2, u3);
	    if (uucross > 0) {
		tp2 = unit->tooling[u2];
		tp3 = unit->tooling[u3];
		/* (should be "as ratio of max levels for each type") */
		cross = (uucross * tp2) / 100;
		if (cross > tp3)
		  unit->tooling[u3] = cross;
	    }
	}
    }
}

/* Create-in action. */

/* This action creates the (incomplete) unit. */

int
prep_create_in_action(unit, unit2, u3, dest)
Unit *unit, *unit2, *dest;
int u3;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    if (dest == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_CREATE_IN;
    unit->act->nextaction.args[0] = u3;
    unit->act->nextaction.args[1] = dest->id;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_create_in_action(unit, unit2, u3, dest)
Unit *unit, *unit2, *dest;
int u3;
{
    int u2 = unit2->type, m;
    Unit *newunit;

    /* Make the new unit. */
    newunit = create_unit(u3, FALSE);
    if (newunit != NULL) {
	/* Fill in various properties. */
	set_created_unit_props(newunit, u2, unit->side);
	/* Put the new unit inside the designated transport. */
	enter_transport(newunit, dest);
	/* Unit might have started out complete. */
	if (completed(newunit)) {
	    garrison_unit(unit2, newunit);
	    make_unit_complete(newunit);
	} else {
	    record_event(H_UNIT_CREATED, add_side_to_set(unit2->side, NOSIDES),
			 side_number(unit2->side), newunit->id);
	}
	if (alive(unit2)) {
	    count_gain(unit2->side, newunit->type, build_gain);
	    /* Consume the creator's supplies as specified. */
	    for_all_material_types(m) {
		unit2->supply[m] -= um_consumption_on_creation(u3, m);
	    }
	}
	use_up_acp(unit, uu_acp_to_create(u2, u3));
	return A_ANY_DONE;
    } else {
	/* We've hit a max number of units, nothing to be done. */
	return A_ANY_ERROR;
    }
}

int
check_create_in_action(unit, unit2, u3, dest)
Unit *unit, *unit2, *dest;
int u3;
{
    int u, u2, m, tp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!is_unit_type(u3))
      return A_ANY_ERROR;
    if (!in_play(dest))
      return A_ANY_ERROR;
    u = unit->type;  u2 = unit2->type;
    if (uu_acp_to_create(u2, u3) < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, uu_acp_to_create(u2, u3)))
      return A_ANY_CANNOT_DO;
    /* Check the tech level of the side. */
    if (u_tech_to_build(u3) > 0) {
	if (unit->side == NULL)
	  return A_ANY_ERROR;
	if (unit->side->tech[u3] < u_tech_to_build(u3))
	  return A_ANY_ERROR;
    }
    /* Check the tooling. */
    tp = (unit2->tooling ? unit2->tooling[u3] : 0);
    if (tp < uu_tp_to_build(u2, u3))
      return A_ANY_ERROR;
    if (distance(unit2->x, unit2->y, dest->x, dest->y) > uu_create_range(u2, u3))
      return A_ANY_TOO_FAR;
    if (unit2->transport != NULL
        && !uu_occ_can_build(unit2->transport->type, u2))
      return A_ANY_ERROR;
    if (!type_can_occupy(u3, dest))
      return A_ANY_ERROR;
    for_all_material_types(m) {
    	if (unit2->supply[m] < um_to_create(u3, m))
    	  return A_ANY_NO_MATERIAL;
    	if (unit2->supply[m] < um_consumption_on_creation(u3, m))
    	  return A_ANY_NO_MATERIAL;
    }
    if (!has_enough_acp(unit, uu_acp_to_create(u2, u3)))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

static void
set_created_unit_props(newunit, u2, side)
Unit *newunit;
int u2;
Side *side;
{
    int u3 = newunit->type, m, amt;

    newunit->hp = newunit->hp2 = 1;
    newunit->cp = uu_creation_cp(u2, u3);
    if (unit_allowed_on_side(newunit, side)) {
	set_unit_side(newunit, side);
	set_unit_origside(newunit, side);
    }
    /* Always number the unit when first created. */
    assign_unit_number(newunit);
    /* Set all supplies to their just-created levels. */
    for_all_material_types(m) {
	amt = newunit->supply[m];
	amt = max(amt, um_created_supply(u3, m));
	/* Clip to capacity. */
	amt = min(amt, um_storage_x(u3, m));
	newunit->supply[m] = amt;
    }
}

/* Create-at action. */

int
prep_create_at_action(unit, unit2, u3, x, y, z)
Unit *unit, *unit2;
int u3, x, y, z;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_CREATE_AT;
    unit->act->nextaction.args[0] = u3;
    unit->act->nextaction.args[1] = x;
    unit->act->nextaction.args[2] = y;
    unit->act->nextaction.args[3] = z;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_create_at_action(unit, unit2, u3, x, y, z)
Unit *unit, *unit2;
int u3, x, y, z;
{
    int u2 = unit2->type, m;
    Unit *newunit;

    /* Make the new unit. */
    newunit = create_unit(u3, FALSE);
    if (newunit != NULL) {
	/* Fill in various properties. */
	set_created_unit_props(newunit, u2, unit->side);
	/* Put it at a correct location. */
	if (can_occupy_cell(newunit, x, y)) {
	    enter_cell(newunit, x, y);
	} else if (can_occupy_cell_without(newunit, x, y, unit2)
		   && can_occupy(unit2, newunit)) {
	    /* Let the builder occupy its incomplete work. */
	    leave_cell(unit2);
	    enter_cell(newunit, x, y);
	    enter_transport(unit2, newunit);
	} else {
	    /* This should never happen. */
	    run_error("construction/occupation complications");
	}
	/* and set its altitude? */
	/* Unit might be complete right away. */
	if (completed(newunit)) {
    	    garrison_unit(unit2, newunit);
	    make_unit_complete(newunit);
	} else {
	    record_event(H_UNIT_CREATED, add_side_to_set(unit2->side, NOSIDES),
			 side_number(unit2->side), newunit->id);
	}
	if (alive(unit2)) {
	    count_gain(unit2->side, newunit->type, build_gain);
	    /* Consume the creator's supplies as specified. */
	    for_all_material_types(m) {
		unit2->supply[m] -= um_consumption_on_creation(u3, m);
	    }
	}
	use_up_acp(unit, uu_acp_to_create(u2, u3));
	return A_ANY_DONE;
    } else {
	/* We've hit a max number of units, nothing to be done. */
	return A_ANY_ERROR;
    }
}

int
check_create_at_action(unit, unit2, u3, x, y, z)
Unit *unit, *unit2;
int u3, x, y, z;
{
    int u, u2, m, tp;

    /* (should share code in create_in) */
    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!is_unit_type(u3))
      return A_ANY_ERROR;
    if (!inside_area(x, y))
      return A_ANY_ERROR;
    u = unit->type;  u2 = unit2->type;
    if (uu_acp_to_create(u2, u3) < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, uu_acp_to_create(u2, u3)))
      return A_ANY_CANNOT_DO;
    if (u_tech_to_build(u3) > 0) {
	if (unit->side == NULL)
	  return A_ANY_ERROR;
	if (unit->side->tech[u3] < u_tech_to_build(u3))
	  /* (should have an "insufficient tech" error for this case) */
	  return A_ANY_ERROR;
    }
    if (distance(unit2->x, unit2->y, x, y) > uu_create_range(u2, u3))
      return A_ANY_TOO_FAR;
    /* Check the tooling. */
    tp = (unit2->tooling ? unit2->tooling[u3] : 0);
    if (tp < uu_tp_to_build(u2, u3))
      /* (should have an "insufficient tooling" error for this case) */
      return A_ANY_ERROR;
    if (unit2->transport != NULL
        && !uu_occ_can_build(unit2->transport->type, u2))
      return A_ANY_ERROR;
    /* (should check for room and safety of terrain) */
    if (!(type_can_occupy_cell(u3, x, y)
          || (can_occupy_type(unit2, u3)
              && type_can_occupy_cell_without(u3, x, y, unit2))))
                return A_ANY_ERROR;
    /* (should check that unit limit not hit yet) */
    for_all_material_types(m) {
    	if (unit2->supply[m] < um_to_create(u3, m))
    	  return A_ANY_NO_MATERIAL;
    	if (unit2->supply[m] < um_consumption_per_build(u3, m))
    	  return A_ANY_NO_MATERIAL;
    }
    if (!has_enough_acp(unit, uu_acp_to_create(u2, u3)))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Build action. */

/* This action makes progress on a construction effort, possibly completing
   the new unit and making it available. */

int build_step_consumption PARAMS ((int u2, int u3, int m, int cp));

int
prep_build_action(unit, unit2, newunit)
Unit *unit, *unit2, *newunit;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_BUILD;
    unit->act->nextaction.args[0] = newunit->id;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_build_action(unit, unit2, newunit)
Unit *unit, *unit2, *newunit;
{
    int u, u2, u3, m;

    u = unit->type;
    u2 = unit2->type;
    u3 = newunit->type;
    for_all_material_types(m) {
    	unit2->supply[m] -= build_step_consumption(u2, u3, m, newunit->cp);
    }
    newunit->cp += uu_cp_per_build(u2, u3);
    if (completed(newunit)) {
    	garrison_unit(unit2, newunit);
	make_unit_complete(newunit);
    }
    update_unit_display(newunit->side, newunit, TRUE);
    use_up_acp(unit, uu_acp_to_build(u2, u3));
    return A_ANY_DONE;
}

/* Consumption per build is in terms of total material to complete,
   this routine computes the consumption for a particular build action.
   This is basically division of the total by the number of completion
   points, but is careful about not missing any needed consumption
   if the division rounds down. */

int
build_step_consumption(u2, u3, m, cp)
int u2, u3, m, cp;
{
    int consumtot = um_consumption_per_build(u3, m);
    int cpinc = uu_cp_per_build(u2, u3);
    int consum, rslt;

    consum = (cpinc * consumtot * 100) / (u_cp(u3));
    rslt = consum / 100;
    if (((cp + cpinc) * 100) / (u_cp(u3)) <= consum % 100)
      ++rslt;
    return rslt;
}

void
garrison_unit(unit, unit2)
Unit *unit, *unit2;
{
    int u = unit->type, u2 = unit2->type, x = unit->x, y = unit->y;
    Unit *transport = NULL, *occ, *nextocc;

    /* Maybe get rid of the building unit if it is to be the garrison. */
    if (uu_hp_to_garrison(u, u2) >= unit->hp) {
	/* But first get the about-to-be-killed garrisoning unit
	   disconnected from everything. */
	leave_cell(unit);
	/* Put new unit in place of the garrisoning one, if it was an occupant. */
	if (unit2->transport == unit) {
	    leave_transport(unit2);
	    if (transport != NULL) { /* some other unit that could be transport? */
		enter_transport(unit2, transport);
	    } else {
		enter_cell(unit2, x, y);
	    }
	}

	if (unit2->transport != NULL && unit2->transport != unit)
	  transport = unit2->transport;
	/* for_all_occupants will not work here, 
	   since leave_transport changes occ->nexthere */
	for (occ = unit->occupant; occ != NULL; occ = nextocc) {
	    nextocc = occ->nexthere;
	    /* Move the other occupants anywhere we can find. */
	    if (can_occupy(occ, unit2)) {
		/* leave_cell won't work here, since "unit" already left cell */
		leave_transport(occ);
		update_unit_display(unit->side, unit, TRUE);
		enter_transport(occ, unit2);
	    } else if (transport != NULL && can_occupy(occ, transport)) {
		leave_transport(occ);
		update_unit_display(unit->side, unit, TRUE);
		enter_transport(occ, transport);
	    } else if (can_occupy_cell(occ, x, y)) {
		leave_transport(occ);
		update_unit_display(unit->side, unit, TRUE);
		enter_cell(occ, x, y);
	    }
	    /* Otherwise the occupant has to die along with the garrison. */
	    /* (should also do something with sub-occs of doomed occs?) */
	}
	/* Now we can get rid of the garrisoning unit without scrambling
	   anything else. */
	tmphevtdata1 = unit2->id;
	kill_unit(unit, H_UNIT_GARRISONED);
    } else {
	/* Note that if this all happens before damage is reckoned,
	   hp and hp2 might be different. */
	unit->hp -= uu_hp_to_garrison(u, u2);
	unit->hp2 -= uu_hp_to_garrison(u, u2);
	/* (should record loss of hp as garrison event?) */
    }
}

int
check_build_action(unit, unit2, newunit)
Unit *unit, *unit2, *newunit;
{
    int u, u2, u3, acpcost, m, tp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!in_play(newunit))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    u3 = newunit->type;
    acpcost = uu_acp_to_build(u2, u3);
    if (acpcost < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acpcost))
      return A_ANY_CANNOT_DO;
    if (uu_cp_per_build(u2, u3) <= 0)
      return A_ANY_ERROR;
    /* Can't finish building a unit until we have the technology. */
    if (u_tech_to_build(u3) > 0) {
	if (unit->side == NULL)
	  return A_ANY_ERROR;
	if (unit->side->tech[u3] < u_tech_to_build(u3))
	  return A_ANY_ERROR;
    }
    /* Check the tooling. */
    tp = (unit2->tooling ? unit2->tooling[u3] : 0);
    if (tp < uu_tp_to_build(u2, u3))
      return A_ANY_ERROR;
    /* Check the distance to the unit being worked on. */
    if (distance(unit->x, unit->y, newunit->x, newunit->y)
	> uu_build_range(u, u3))
      return A_ANY_ERROR;
    /* Note that we should be able to build when inside the incomplete
       unit we're building. */
    if (unit2->transport != NULL
	&& completed(unit2->transport)
        && !uu_occ_can_build(unit2->transport->type, u2))
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acpcost))
      return A_ANY_NO_ACP;
    for_all_material_types(m) {
    	if (unit2->supply[m] < um_to_build(u2, m))
    	  return A_ANY_NO_MATERIAL;
    	if (unit2->supply[m] < build_step_consumption(u2, u3, m, newunit->cp))
    	  return A_ANY_NO_MATERIAL;
    }
    return A_ANY_OK;
}

/* Do all the little things to make a fully operational unit. */

void
make_unit_complete(unit)
Unit *unit;
{
    int u = unit->type, m;
    SideMask observers;
    Side *side;

    /* Make this a "complete" but not a "fullsized" unit. */
    unit->cp = max(unit->cp, u_cp(u) / u_parts(u));
    unit->hp = unit->hp2 = u_hp(u) / u_parts(u);
    /* Christen our new unit. Its serial number (if it is a type that has
       one) was assigned just after its creation. */
    make_up_unit_name(unit);
    /* It also starts viewing its surroundings. */
    if (unit->transport == NULL
	|| uu_occ_can_see(unit->type, unit->transport->type)) {
	cover_area(unit->side, unit, -1, -1, unit->x, unit->y);
    }
    /* Set all the supplies up to their unit-just-completed levels. */
    for_all_material_types(m) {
	unit->supply[m] = max(unit->supply[m], um_completed_supply(u, m));
	unit->supply[m] = min(unit->supply[m], um_storage_x(u, m));
    }
    /* Also see if anybody here is willing to share to make up any
       deficiencies before the end of the turn. */
    for_all_material_types(m) {
	if (unit->transport)
	  try_sharing(unit->transport, unit, m);
    }
    init_unit_actorstate(unit, FALSE);
    init_unit_plan(unit);
    /* Put this unit into action immediately, at full acp. */
    if (unit->act) {
	compute_acp(unit);
	if (unit->act->initacp > 0) {
	    side = (unit->side ? unit->side : indepside);
	    side->actionvector = add_unit_to_vector(side->actionvector, unit, 0);
	}
    }
    observers = add_side_to_set(unit->side, NOSIDES);
    /* (should add any other sides that might see this) */
    record_event(H_UNIT_COMPLETED, observers, side_number(unit->side), unit->id);
    /* (should add to any per-side tallies) */
    Dprintf("%s is completed\n", unit_desig(unit));
}

/* Repair action. */

int
prep_repair_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    if (unit3 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_REPAIR;
    unit->act->nextaction.args[0] = unit3->id;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_repair_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    int u, u2, u3, rep, m;

    u = unit->type;  u2 = unit2->type;  u3 = unit3->type;
    rep = uu_repair(u2, u3);
    /* Add to the repairee's hit points. */
    add_to_unit_hp(unit3, prob_fraction(rep));
    /* Eat supplies used up by repair. */
    for_all_material_types(m) {
	unit2->supply[m] -= um_consumption_per_repair(u3, m);
    }
    use_up_acp(unit, uu_acp_to_repair(u2, u3));
    return A_ANY_DONE;
}

int
check_repair_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    int u, u2, u3, acp, m;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!in_play(unit3))
      return A_ANY_ERROR;
    u = unit->type;  u2 = unit2->type;  u3 = unit3->type;
    acp = uu_acp_to_repair(u2, u3);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    if (uu_repair(u2, u3) <= 0)
      return A_ANY_ERROR;
    if (unit3->hp >= u_hp(u3))
      return A_ANY_ERROR;
    if (unit2->hp < uu_hp_to_repair(u2, u3))
      return A_ANY_ERROR;
    for_all_material_types(m) {
	if (unit2->supply[m] < um_to_repair(u2, m))
	  return A_ANY_NO_MATERIAL;
	if (unit2->supply[m] < um_consumption_per_repair(u3, m))
	  return A_ANY_NO_MATERIAL;
    }
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Disband action. */

/* The disband action destroys a unit in an "orderly" fashion, and can be
   undertaken voluntarily. */

int
prep_disband_action(unit, unit2)
Unit *unit, *unit2;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_DISBAND;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_disband_action(unit, unit2)
Unit *unit, *unit2;
{
    int u2, m, amt, disb;

    u2 = unit2->type;
    /* Recover some percentage of the unit's supply. */
    for_all_material_types(m) {
    	if (um_supply_per_disband(u2, m) > 0 && unit2->supply[m] > 0) {
    	    amt = (unit2->supply[m] * um_supply_per_disband(u2, m)) / 100;
    	    /* Unit always loses the amount, whether or not distributed. */
    	    unit2->supply[m] -= amt;
    	    distribute_material(unit2, m, amt);
    	}
    }
    /* Remove hit points or kill the unit directly. */
    disb = u_hp_per_disband(u2);
    if (disb < unit2->hp) {
	unit2->hp -= disb;
	unit2->hp2 = unit2->hp;
    } else {
    	/* Pass around whatever we can get out of the unit itself. */
    	for_all_material_types(m) {
    	    if (um_recycleable(u2, m) > 0) {
    	    	distribute_material(unit2, m, um_recycleable(u2, m));
    	    }
    	}
    	/* should ensure vanish */
	kill_unit(unit2, H_UNIT_DISBANDED);
    }
    use_up_acp(unit, u_acp_to_disband(u2));
    return A_ANY_DONE;
}

/* Given a unit and a quantity of material, pass it out to nearby units. */

void
distribute_material(unit, m, amt)
Unit *unit;
int m, amt;
{
    /* Distribute to transport first. */
    if (amt > 0 && unit->transport != NULL) {
    	/* (should clip to capacity etc) */
    	unit->transport->supply[m] += amt;
    	amt = 0;
    }
    /* Then to any unit in the cell. */
    if (amt > 0) {
    }
    /* Then to any unit in an adjacent cell. */
    if (amt > 0) {
    }
    /* (should note anything that went to waste?) */
}

int
check_disband_action(unit, unit2)
Unit *unit, *unit2;
{
    int u, u2, acp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    u = unit->type;  u2 = unit->type;
    acp = u_acp_to_disband(u2);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    if (u_hp_per_disband(unit2->type) <= 0)
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Transfer-part action. */

/* Create a new unit that is similar to the original one, and give it
   some of the parts of the original unit. */
/* (New unit in same cell if possible or else in random adjacent cell.) */

int
prep_transfer_part_action(unit, unit2, parts, unit3)
Unit *unit, *unit2, *unit3;
int parts;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_TRANSFER_PART;
    unit->act->nextaction.args[0] = parts;
    unit->act->nextaction.args[1] = (unit3 ? unit3->id : 0);
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_transfer_part_action(unit, unit2, parts, unit3)
Unit *unit, *unit2, *unit3;
int parts;
{
    int u2 = unit2->type, acp, part_hp;

    part_hp = u_hp(u2) / u_parts(u2);
    if (unit3 == NULL) {
	/* Create a new unit with parts from unit2. */
	unit3 = create_unit(u2, TRUE);
	if (unit3 != NULL) {
	    unit3->hp = parts * part_hp;
	    /* (Cap the hp now - occupancy calcs below might use unit parts
	        to determine available capacity) */
	    unit3->hp = min(unit3->hp, u_hp(unit3->type));
	    unit3->hp2 = unit3->hp;
	    if (unit_allowed_on_side(unit3, unit->side)) {
		set_unit_side(unit3, unit->side);
		set_unit_origside(unit3, unit->origside);
	    }
	    /* Always number the unit when first created. */
	    assign_unit_number(unit3);
	    /* (should fill in more slots of new unit, such as supply?) */
	    if (can_occupy_cell(unit3, unit2->x, unit2->y)) {
		enter_cell(unit3, unit2->x, unit2->y);
	    } else {
	    	/* (should add code to enter something else here) */
		/* This should never happen. */
		run_warning("transfer_part complications, leaving unit offworld");
	    }
	} else {
	    /* We have a problem. */
	    return A_ANY_ERROR;
	}
    } else {
	/* Increase the unit3's hp by what's in this unit, and cap it. */
	add_to_unit_hp(unit3, parts * part_hp);
    }
    /* Need to tweak parts in unit2 also */
    if (parts * part_hp >= unit2->hp) {
	/* (should transfer occs to unit3) */
	kill_unit(unit2, -1);  /* should add a merge kill-reason */
    } else {
	unit2->hp -= parts * part_hp;
	unit2->hp2 = unit2->hp;
    }
    if (alive(unit2))
      update_unit_display(unit2->side, unit2, TRUE);
    update_unit_display(unit3->side, unit3, TRUE);
    acp = u_acp_to_transfer_part(u2);
    use_up_acp(unit, acp);
    return A_ANY_DONE;
}

int
check_transfer_part_action(unit, unit2, parts, unit3)
Unit *unit, *unit2, *unit3;
int parts;
{
    int u2, acp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (parts <= 0)
      return A_ANY_ERROR;
    /* unit3 can be null. */
    u2 = unit2->type;
    acp = u_acp_to_transfer_part(u2);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    if (u_parts(u2) <= 1)
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Change-type action. */

int
prep_change_type_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_CHANGE_TYPE;
    unit->act->nextaction.args[0] = u3;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

/* Actually change the type of a unit. */

int
do_change_type_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    int u, u2;

    u = unit->type;
    u2 = unit2->type;
    change_unit_type(unit2, u3, H_UNIT_TYPE_CHANGED);
    update_unit_display(unit2->side, unit2, TRUE);
    /* (should consume materials) */
    use_up_acp(unit, uu_acp_to_change_type(u2, u3));
    return A_ANY_DONE;
}

int
check_change_type_action(unit, unit2, u3)
Unit *unit, *unit2;
int u3;
{
    int u, u2, acp, m;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!is_unit_type(u3))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    acp = uu_acp_to_change_type(u2, u3);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    /* should check if still on allowable side */
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    /* Check that the unit has any required supplies. */
    for_all_material_types(m) {
	if (unit2->supply[m] < um_to_change_type(u2, m))
	  return A_ANY_NO_MATERIAL;
    }
    return A_ANY_OK;
}

/* Change-side action. */

/* Tell a unit to change to a given side. */

/* (what about occs, garrisons, plans?) */

int
prep_change_side_action(unit, unit2, side)
Unit *unit, *unit2;
Side *side;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_CHANGE_SIDE;
    unit->act->nextaction.args[0] = side_number(side);
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_change_side_action(unit, unit2, side)
Unit *unit, *unit2;
Side *side;
{
    int rslt;

    if (side_controls_unit(unit->side, unit2)) {
	/* If we own it, we can just change it. */
	change_unit_side(unit2, side, H_UNIT_ACQUIRED, NULL);
	rslt = A_ANY_DONE;
    } else {
	rslt = A_ANY_ERROR;
    }
    use_up_acp(unit, u_acp_to_change_side(unit2->type));
    return rslt;
}

int
check_change_side_action(unit, unit2, side)
Unit *unit, *unit2;
Side *side;
{
    int u, u2, acp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!side_in_play(side))
      return A_ANY_ERROR;
    if (unit2->side == side)
      return A_ANY_ERROR;
    if (!unit_allowed_on_side(unit2, side))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    acp = u_acp_to_change_side(u2);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Alter-terrain action. */

/* Change the terrain in the cell to something else. */

/* We don't need to ensure that the unit can exist on the new terrain
   type, because the designer is presumed to have set things up sensibly,
   because the unit might be in an appropriate transport, or because
   there is some actual use in such a bizarre shtick. */

/* What if engineers dig hole underneath enemy unit?  Should this be
   possible, or should there be a "can-dig-under-enemy" parm?  */

int
prep_alter_cell_action(unit, unit2, x, y, t)
Unit *unit, *unit2;
int x, y, t;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_ALTER_TERRAIN;
    unit->act->nextaction.args[0] = x;
    unit->act->nextaction.args[1] = y;
    unit->act->nextaction.args[2] = t;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_alter_cell_action(unit, unit2, x, y, t)
Unit *unit, *unit2;
int x, y, t;
{
    int u, u2, oldt, acpr, acpa;
    Side *side;

    u = unit->type;  u2 = unit2->type;
    oldt = terrain_at(x, y);
    /* Change the terrain to the new type. */
    change_terrain_type(x, y, t);
    /* Note that we still charge acp even if terrain type doesn't change. */
    acpr = ut_acp_to_remove_terrain(u2, oldt);
    acpa = ut_acp_to_add_terrain(u2, t);
    use_up_acp(unit, acpr + acpa);
    return A_ANY_DONE;
}

int
check_alter_cell_action(unit, unit2, x, y, t)
Unit *unit, *unit2;
int x, y, t;
{
    int u, u2, oldt, acpr, acpa;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!in_area(x, y))
      return A_ANY_ERROR;
    if (!is_terrain_type(t))
      return A_ANY_ERROR;
    if (!t_is_cell(t))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    oldt = terrain_at(x, y);
    acpr = ut_acp_to_remove_terrain(u2, oldt);
    acpa = ut_acp_to_add_terrain(u2, t);
    if (acpr < 1 || acpa < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acpr + acpa))
      return A_ANY_CANNOT_DO;
    if (distance(unit2->x, unit2->y, x, y) > ut_alter_range(u2, t))
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acpr + acpa))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Add-terrain action. */

/* Add terrain; border, connection, or coating. */

int
prep_add_terrain_action(unit, unit2, x, y, dir, t)
Unit *unit, *unit2;
int x, y, dir, t;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_ADD_TERRAIN;
    unit->act->nextaction.args[0] = x;
    unit->act->nextaction.args[1] = y;
    unit->act->nextaction.args[2] = dir;
    unit->act->nextaction.args[3] = t;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_add_terrain_action(unit, unit2, x, y, dir, t)
Unit *unit, *unit2;
int x, y, dir, t;
{
    int u = unit->type, oldval, newval, x1, y1;
    Side *side;

    switch (t_subtype(t)) {
      case cellsubtype:
      	/* Will never happen. */
      	break;
      case bordersubtype:
      	oldval = border_at(x, y, dir, t);
      	newval = TRUE;
	set_border_at(x, y, dir, t, newval);
      	break;
      case connectionsubtype:
      	oldval = connection_at(x, y, dir, t);
      	newval = TRUE;
	set_connection_at(x, y, dir, t, newval);
      	break;
      case coatingsubtype:
    	oldval = aux_terrain_at(x, y, t);
      	/* Interpret "dir" as depth of coating to add. */
    	newval = min(oldval + dir, tt_coat_max(terrain_at(x, y), t));
    	set_aux_terrain_at(x, y, t, newval);
      	break;
    }
    /* Let everybody see what has happened. */
    for_all_sides(side) {
	if (active_display(side)) {
	    if (terrain_visible(side, x, y)) {
		update_cell_display(side, x, y, TRUE);
	    }
	    if (t_subtype(t) != coatingsubtype) {
		if (point_in_dir(x, y, dir, &x1, &y1) && terrain_visible(side, x1, y1))
		  update_cell_display(side, x1, y1, TRUE);
    	    }
    	}
    }
    use_up_acp(unit, (newval != oldval ? ut_acp_to_add_terrain(u, t) : 1));
    return A_ANY_DONE;
}

int
check_add_terrain_action(unit, unit2, x, y, dir, t)
Unit *unit, *unit2;
int x, y, dir, t;
{
    int u, u2, acp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!inside_area(x, y))
      return A_ANY_ERROR;
    if (!between(0, dir, NUMDIRS - 1))
      return A_ANY_ERROR;
    if (!is_terrain_type(t))
      return A_ANY_ERROR;
    if (t_is_cell(t))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    acp = ut_acp_to_add_terrain(u2, t);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
     if (!can_have_enough_acp(unit, acp))
       return A_ANY_CANNOT_DO;
   if (distance(unit->x, unit->y, x, y) > ut_alter_range(u2, t))
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Remove-terrain action. */

/* Remove a border, connection, or coating. */

int
prep_remove_terrain_action(unit, unit2, x, y, dir, t)
Unit *unit, *unit2;
int x, y, dir, t;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_REMOVE_TERRAIN;
    unit->act->nextaction.args[0] = x;
    unit->act->nextaction.args[1] = y;
    unit->act->nextaction.args[2] = dir;
    unit->act->nextaction.args[3] = t;
    unit->act->nextaction.actee = unit2->id;
    broadcast_next_action(unit);
    return TRUE;
}

int
do_remove_terrain_action(unit, unit2, x, y, dir, t)
Unit *unit, *unit2;
int x, y, dir, t;
{
    int u = unit->type, oldval, newval, x1, y1;
    Side *side;

    switch (t_subtype(t)) {
      case cellsubtype:
      	/* Will never happen. */
      	break;
      case bordersubtype:
	oldval = border_at(x, y, dir, t);
	newval = FALSE;
	set_border_at(x, y, dir, t, newval);
      	break;
      case connectionsubtype:
	oldval = connection_at(x, y, dir, t);
	newval = FALSE;
	set_connection_at(x, y, dir, t, newval);
      	break;
      case coatingsubtype:
    	oldval = aux_terrain_at(x, y, t);
       	/* Interpret "dir" as depth of coating to remove. */
   	newval = max(oldval - dir, 0);
   	/* If newval drops below the min coating depth, coating will vanish. */
    	if (newval < tt_coat_min(terrain_at(x, y), t))
    	  newval = 0;
    	set_aux_terrain_at(x, y, t, newval);
      	break;
    }
    /* Let everybody see what has happened. */
    for_all_sides(side) {
	if (active_display(side)) {
	    if (terrain_visible(side, x, y)) {
		update_cell_display(side, x, y, TRUE);
	    }
	    if (t_subtype(t) != coatingsubtype) {
		if (point_in_dir(x, y, dir, &x1, &y1) && terrain_visible(side, x1, y1))
		  update_cell_display(side, x1, y1, TRUE);
    	    }
    	}
    }
    use_up_acp(unit, (newval != oldval ? ut_acp_to_remove_terrain(u, t) : 1));
    return A_ANY_DONE;
}

int
check_remove_terrain_action(unit, unit2, x, y, dir, t)
Unit *unit, *unit2;
int x, y, dir, t;
{
    int u, u2, acp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!inside_area(x, y))
      return A_ANY_ERROR;
    if (!between(0, dir, NUMDIRS - 1))
      return A_ANY_ERROR;
    if (!is_terrain_type(t))
      return A_ANY_ERROR;
    if (t_is_cell(t))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    acp = ut_acp_to_remove_terrain(u2, t);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!can_have_enough_acp(unit, acp))
      return A_ANY_CANNOT_DO;
    if (distance(unit->x, unit->y, x, y) > ut_alter_range(u2, t))
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

/* Execute a given action on a given unit. */

/* (assumes unit can act in the first place - valid?) */

int
execute_action(unit, action)
Unit *unit;
Action *action;
{
    char *argtypestr;
    int u = unit->type, rslt = A_ANY_ERROR, n, i;
    long args[4];
    Unit *unit2, *argunit;
    Side *argside, *side2;
#ifdef THINK_C
    /* Think C can be excessively picky sometimes. */
    int (*checkfn) PARAMS ((Unit *unit, Unit *unit2, ...));
    int (*dofn) PARAMS ((Unit *unit, Unit *unit2, ...));
#else
    int (*checkfn) ();
    int (*dofn) ();
#endif
    extern int numsoundplays;

    Dprintf("%s doing %s with %d acp left\n",
	    unit_desig(unit), action_desig(action), unit->act->acp);

    if (!alive(unit) || !unit->act || unit->act->acp < u_acp_min(u))
      return A_ANY_ERROR;

    argtypestr = actiondefns[(int) action->type].argtypes;
    n = strlen(argtypestr);
    for (i = 0; i < n; ++i) {
	switch (argtypestr[i]) {
	  case 'n':
	  case 'u':
	  case 'm':
	  case 't':
	  case 'x':
	  case 'y':
	  case 'z':
	  case 'd':
	    args[i] = action->args[i];
	    break;
	  case 'U':
	    argunit = find_unit(action->args[i]);
	    if (argunit == NULL) {
		/* an error, should do run_warning */
	    }
	    args[i] = (long) argunit;
	    break;
	  case 'S':
	    argside = side_n(action->args[i]);
	    args[i] = (long) argside;
	    break;
	  default:
	    /* should warn */
	    break;
	}
    }
    checkfn = actiondefns[(int) action->type].checkfn;
    dofn = actiondefns[(int) action->type].dofn;
    if (action->actee == 0) {
	unit2 = unit;
    } else {
	unit2 = find_unit(action->actee);
    }
    if (unit2 == NULL) {
	return A_ANY_ERROR;
    }
    switch (n) {
      case 0:
	rslt = (*checkfn)(unit, unit2);
	break;
      case 1:
	rslt = (*checkfn)(unit, unit2, args[0]);
	break;
      case 2:
	rslt = (*checkfn)(unit, unit2, args[0], args[1]);
	break;
      case 3:
	rslt = (*checkfn)(unit, unit2, args[0], args[1], args[2]);
	break;
      case 4:
	rslt = (*checkfn)(unit, unit2, args[0], args[1], args[2], args[3]);
	break;
      default:
	case_panic("action arg count", n);
    }
    numsoundplays = 0; /* kind of a hack */
    if (valid(rslt)) {
	if (g_action_messages() != lispnil)
	  play_action_messages(unit, action);
	switch (n) {
	  case 0:
	    rslt = (*dofn)(unit, unit2);
	    break;
	  case 1:
	    rslt = (*dofn)(unit, unit2, args[0]);
	    break;
	  case 2:
	    rslt = (*dofn)(unit, unit2, args[0], args[1]);
	    break;
	  case 3:
	    rslt = (*dofn)(unit, unit2, args[0], args[1], args[2]);
	    break;
	  case 4:
	    rslt = (*dofn)(unit, unit2, args[0], args[1], args[2], args[3]);
	    break;
	  default:
	    case_panic("action arg count", n);
	}
	Dprintf("%s action %s result is %s, %d acp left\n",
		unit_desig(unit), action_desig(action), hevtdefns[rslt].name,
		(unit->act ? unit->act->acp : -9999));
	if (unit->plan) {
	    unit->plan->lastaction = *action;
	    unit->plan->lastresult = rslt;
	}
	if (unit->side && side_has_ai(unit->side)) {
	    ai_react_to_action_result(unit->side, unit, rslt);
	}
	if (unit->side && side_has_display(unit->side)) {
	    update_action_result_display(unit->side, unit, rslt, TRUE);
	}
	/* Show other sides that some action has occurred. */
	for_all_sides(side2) {
	    if (side_has_display(side2)) {
		update_side_display(side2, unit->side, TRUE);
	    }
	}
	/* If of a type that might be spotted if it does anything, check
	   each side to see if they notice.  Note that the type is from
	   *before* the action, not after (some actions may cause type changes). */
	if (u_spot_action(u)
	    && !all_see_all
	    && !u_see_always(u)
	    && in_area(unit->x, unit->y)) {
	    for_all_sides(side2) {
		if (cover(side2, unit->x, unit->y) > 0) {
		    /* (should call some sort of "glimpsing" routine) */
		}
	    }
	}
	/* Check any scorekeepers that run after each action. */
	if (any_post_action_scores) {
	    check_post_action_scores(unit, action, rslt);
	}
    } else {
	if (unit->plan) {
	    unit->plan->lastaction = *action;
	    unit->plan->lastresult = rslt;
	}
	if (unit->side && side_has_ai(unit->side)) {
	    ai_react_to_action_result(unit->side, unit, rslt);
	}
	Dprintf("%s action %s can't be done, result is %s\n",
		unit_desig(unit), action_desig(action), hevtdefns[rslt].name);
    }
    /* Return success/failure so caller can use. */
    return rslt;
}

static void
play_action_messages(unit, action)
Unit *unit;
Action *action;
{
    int found = FALSE;
    char *soundname;
    Obj *rest, *head, *parms, *msgdesc;

    for_all_list(g_action_messages(), rest) {
	head = car(rest);
	if (consp(head)
	    && symbolp(car(head))
	    && strcmp(c_string(car(head)),
		      actiondefns[(int) action->type].name) == 0) {
	    found = TRUE;
	    break;
	}
	if (consp(head)
	    && consp(car(head))
	    && symbolp(car(car(head)))
	    && strcmp(c_string(car(car(head))),
		      actiondefns[(int) action->type].name) == 0) {
	    parms = cdr(car(head));
	    if (parms == lispnil) {
		found = TRUE;
		break;
	    }
	    if (((symbolp(car(parms))
		   && strcmp(c_string(car(parms)),
			     u_type_name(unit->type)) == 0)
		  || match_keyword(car(parms), K_USTAR)
		  || (symbolp(car(parms))
		      && boundp(car(parms))
		      && ((symbolp(symbol_value(car(parms)))
		          && strcmp(c_string(symbol_value(car(parms))),
				    u_type_name(unit->type)) == 0)
		          || (numberp(symbol_value(car(parms)))
		              && c_number(symbol_value(car(parms)))
			      == unit->type)))
		  )) {
		found = TRUE;
		break;
	    }
	    /* (should be able to match on particular action parms also) */
	}
    }
    /* If we have a match, do something with it. */
    if (found) {
	msgdesc = cadr(head);
	if (stringp(msgdesc)) {
	    notify(unit->side, "%s", c_string(msgdesc));
	} else if (consp(msgdesc)
		   && symbolp(car(msgdesc))
		   && strcmp(c_string(car(msgdesc)), "sound") == 0
		   && stringp(cadr(msgdesc))) {
	    soundname = c_string(cadr(msgdesc));
	    /* (should not be passing ptrs to schedule_movie) */
	    schedule_movie(unit->side, movie_extra_0, soundname);
	    play_movies(add_side_to_set(unit->side, NOSIDES));
	} else {
	}
    }
}

/* Basic check that unit has sufficient acp to do an action. */

int
can_have_enough_acp(unit, acp)
Unit *unit;
int acp;
{
    int u = unit->type, maxacp, minacp;

    maxacp = u_acp(u);
    if (u_acp_turn_max(u) >= 0)
      maxacp = min(maxacp, u_acp_turn_max(u));
    maxacp = (u_acp_max(u) < 0 ? maxacp : u_acp_max(u));
    minacp = u_acp_min(u);
    return (maxacp - acp >= minacp);
}

int
has_enough_acp(unit, acp)
Unit *unit;
int acp;
{
    if (unit->act == NULL)
      return FALSE;
    return ((unit->act->acp - acp) >= u_acp_min(unit->type));
}

/* This is true iff the unit has enough of each sort of supply to act. */

int
has_supply_to_act(unit)
Unit *unit;
{
    int m;

    for_all_material_types(m) {
	if (unit->supply[m] < um_to_act(unit->type, m))
	  return FALSE;
    }
    return TRUE;
}

/* Make the consumed acp disappear, but not go below the minimum possible. */

void
use_up_acp(unit, acp)
Unit *unit;
int acp;
{
    int oldacp, newacp, acpmin;

    /* This can sometimes be called on dead or non-acting units,
       so check first. */
    if (alive(unit) && unit->act && acp > 0) {
    	oldacp = unit->act->acp;
	newacp = oldacp - acp;
	acpmin = u_acp_min(unit->type);
	unit->act->acp = max(newacp, acpmin);
	/* Maybe modify the unit's display. */
	if (oldacp != unit->act->acp) {
		update_unit_display(unit->side, unit, TRUE);
	}
    }
}

/* Functions returning general abilities of a unit. */

int
can_research(unit)
Unit *unit;
{
    return type_can_research(unit->type);
}

int
type_can_research(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_research(u, u2) > 0)
	  return TRUE;
    }
    return FALSE;
}

int
can_toolup(unit)
Unit *unit;
{
    return type_can_toolup(unit->type);
}

int
type_can_toolup(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_toolup(u, u2) > 0)
	  return TRUE;
    }
    return FALSE;
}

int
can_create(unit)
Unit *unit;
{
    return type_can_create(unit->type);
}

int
type_can_create(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_create(u, u2) > 0
	    && uu_tp_max(u, u2) >= uu_tp_to_build(u, u2))
	  return TRUE;
    }
    return FALSE;
}

int
can_complete(unit)
Unit *unit;
{
    return type_can_complete(unit->type);
}

int
type_can_complete(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_build(u, u2) > 0
	    && uu_tp_max(u, u2) >= uu_tp_to_build(u, u2))
	  return TRUE;
    }
    return FALSE;
}

/* This tests whether the given unit is capable of doing repair. */

int
can_repair(unit)
Unit *unit;
{
    return type_can_repair(unit->type);
}

int
type_can_repair(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_repair(u, u2) > 0)
	  return TRUE;
    }
    return FALSE;
}

/* This tests whether the given unit is capable of doing repair. */

int
can_change_type(unit)
Unit *unit;
{
    return type_can_change_type(unit->type);
}

int
type_can_change_type(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_change_type(u, u2) > 0)
	  return TRUE;
    }
    return FALSE;
}

int
can_disband(unit)
Unit *unit;
{
    return (type_can_disband(unit->type) || !completed(unit));
}

int
type_can_disband(u)
int u;
{
    return (u_acp_to_disband(u) > 0);
}

int
side_can_disband(side, unit)
Side *side;
Unit *unit;
{
#ifdef DESIGNERS
    if (side->designer)
      return TRUE;
#endif /* DESIGNERS */
    return (side_controls_unit(side, unit)
	    && can_disband(unit));
}

/* This tests whether the given unit is capable of adding terrain. */

int
can_add_terrain(unit)
Unit *unit;
{
    return type_can_add_terrain(unit->type);
}

int
type_can_add_terrain(u)
int u;
{
    int t;
	
    for_all_terrain_types(t) {
	if (ut_acp_to_add_terrain(u, t) > 0)
	  return TRUE;
    }
    return FALSE;
}

/* This tests whether the given unit is capable of removing terrain. */

int
can_remove_terrain(unit)
Unit *unit;
{
    return type_can_remove_terrain(unit->type);
}

int
type_can_remove_terrain(u)
int u;
{
    int t;
	
    for_all_terrain_types(t) {
	if (ut_acp_to_remove_terrain(u, t) > 0)
	  return TRUE;
    }
    return FALSE;
}

/* The following is generic code. */

int
any_construction_possible()
{
    int u, u2;
    static int any_construction = -1;

    if (any_construction < 0) {
	any_construction = FALSE;
	for_all_unit_types(u) {
	    for_all_unit_types(u2) {
		if (uu_acp_to_create(u, u2) > 0) {
		    any_construction = TRUE;
		    return any_construction;
		}
	    }
	}
    }
    return any_construction;
}

int
construction_possible(u2)
int u2;
{
    int u;

    for_all_unit_types(u) {
	if (uu_acp_to_create(u, u2) > 0
	    && uu_tp_max(u, u2) >= uu_tp_to_build(u, u2))
	  return TRUE;
    }
}

/* Compose a legible description of a given action. */

char *
action_desig(act)
Action *act;
{
    int i, slen;
    char ch, *str;

    if (act == NULL)
      return "?null action?";
    if (act->type == ACTION_NONE)
      return "[]";
    if (actiondesigbuf == NULL)
      actiondesigbuf = xmalloc(BUFSIZE);
    str = actiondesigbuf;
    sprintf(str, "[%s", actiondefns[act->type].name);
    slen = strlen(actiondefns[act->type].argtypes);
    for (i = 0; i < slen; ++i) {
	ch = (actiondefns[act->type].argtypes)[i];
	switch (ch) {
	  case 'U':
	    tprintf(str, " \"%s\"",
		    unit_desig(find_unit(act->args[i])));
	    break;
	  default:
	    tprintf(str, " %d", act->args[i]);
	}
    }
    if (act->actee != 0) {
	tprintf(str, " (#%d)", act->actee);
    }
    strcat(str, "]");
    return actiondesigbuf;
}
