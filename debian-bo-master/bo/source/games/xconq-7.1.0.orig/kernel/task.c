/* Unit task execution and general task functions.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"

/* This is the number of tasks to allocate initially.  More will always be
   allocated as needed, so this should be a "reasonable" value. */

#ifndef INITMAXTASKS
#define INITMAXTASKS 100
#endif

#define CLEAR_AGENDA 99

static int compare_directions PARAMS ((const void *a0, const void *a1));
static int test_for_buildable PARAMS ((int x, int y));

/* Declare all the task functions. */

#undef  DEF_TASK
#define DEF_TASK(name,code,argtypes,FN)  \
  static TaskOutcome FN PARAMS ((Unit *unit, Task *task));

#include "task.def"

/* Array of descriptions of task types. */

TaskDefn taskdefns[] = {

#undef  DEF_TASK
#define DEF_TASK(NAME,code,ARGTYPES,FN) { NAME, ARGTYPES, FN },

#include "task.def"

    { NULL, NULL, NULL }
};

/* The list of available task objects. */

Task *freetasks;

/* Pointer to a buffer that task debug info goes into. */

char *taskbuf;

static int tmpbuildutype;

static Unit *tmpbuilder, *tmpbuildunit;

/* Allocate an initial collection of task objects. */

void
init_tasks()
{
    allocate_task_block();
}

/* Allocate a new block of tasks. */

void
allocate_task_block()
{
    int i;

    freetasks = (Task *) xmalloc(INITMAXTASKS * sizeof(Task));
    /* Chain the tasks together. */
    for (i = 0; i < INITMAXTASKS; ++i) {
	freetasks[i].next = &freetasks[i+1];
    }
    freetasks[INITMAXTASKS-1].next = NULL;
}

/* Create and return a new task. */

Task *
create_task(type)
TaskType type;
{
    int i;
    Task *task;

    /* Scarf up some more memory if we need it. */
    if (freetasks == NULL) {
	allocate_task_block();
    }
    /* Peel off a task from the free list. */
    task = freetasks;
    freetasks = task->next;
    /* Reset its slots. */
    task->type = type;
    task->execnum = 0;
    task->retrynum = 0;
    for (i = 0; i < MAXTASKARGS; ++i)
      task->args[i] = 0;
    task->next = NULL;
    return task;
}

Task *
clone_task(oldtask)
Task *oldtask;
{
    int i;
    Task *newtask;

    newtask = create_task(oldtask->type);
    /* Probably not a good idea to copy exec/retry counts, skip them. */
    for (i = 0; i < MAXTASKARGS; ++i)
      newtask->args[i] = oldtask->args[i];
    return newtask;
}

/* The empty task always succeeds immediately. */

static TaskOutcome
do_none_task(unit, task)
Unit *unit;
Task *task;
{
    return TASK_IS_COMPLETE;
}

static int
test_for_buildable(x, y)
int x, y;
{
    Unit *unit;

    for_all_stack(x, y, unit) {
	if (in_play(unit)
	    && !fullsized(unit)
	    && unit->type == tmpbuildutype
	    && unit->side == tmpbuilder->side) {
	    tmpbuildunit = unit;
	    return TRUE;
	}
    }
    return FALSE;
}

/* The build task handles the research, tooling up, creation, and completion
   for a given number of units of a given type. */

static TaskOutcome
do_build_task(unit, task)
Unit *unit;
Task *task;
{
    int u = unit->type, dir, nx, ny, tp;
    int x = unit->x, y = unit->y;
    int u2 = task->args[0], run = task->args[3];
    Unit *unit2 = NULL;
    Side *us = unit->side;

    /* First see if we've already built all the units requested. */
    if (task->args[2] >= run) {
        return TASK_IS_COMPLETE;
    }
    /* See if our technology needs improvement in order to build this type. */
    if (is_unit_type(u2)
	&& u_tech_to_build(u2) > 0
        && us->tech[u2] < u_tech_to_build(u2)) {
        if (uu_acp_to_research(u, u2) > 0) {
	    if (valid(check_research_action(unit, unit, u2))) {
		prep_research_action(unit, unit, u2);
		return TASK_PREPPED_ACTION;
	    } else {
		/* We get three trys to research before giving up. */
	    	return (task->execnum < 3 ? TASK_IS_INCOMPLETE : TASK_FAILED);
	    }
        } else {
	    /* Can't do the necessary research. */
	    return TASK_FAILED;
        }
    }
    /* See if we need to toolup to work on this type. */
    if (is_unit_type(u2)) {
	tp = (unit->tooling ? unit->tooling[u2] : 0);
	if (tp < uu_tp_to_build(u, u2)) {
	    if (uu_acp_to_toolup(u, u2) > 0) {
		if (valid(check_toolup_action(unit, unit, u2))) {
		    prep_toolup_action(unit, unit, u2);
		    return TASK_PREPPED_ACTION;
		} else {
		    /* We get three trys to toolup before giving up. */
		    return (task->execnum < 3 ? TASK_IS_INCOMPLETE : TASK_FAILED);
		}
	    } else {
		/* Can't do the necessary toolup. */
		return TASK_FAILED;
	    }
	}
    }
    /* Check out the unit supposedly in progress. */
    if (task->args[1] != 0) {
	unit2 = find_unit(task->args[1]);
	if (in_play(unit2) && unit2->type == u2) {
	    if (fullsized(unit2)) {
		++(task->args[2]);
		if (task->args[2] >= run) {
		    return TASK_IS_COMPLETE;
		}
	    }
	}
    }

    /* Find something to work on */
    unit2 = find_unit_to_complete(unit, task);

    /* No incomplete unit found, so try to create one. */
    if (unit2 == NULL) {
	if (valid(check_create_in_action(unit, unit, u2, unit))) {
	    prep_create_in_action(unit, unit, u2, unit);
	    return TASK_PREPPED_ACTION;
	} else if (valid(check_create_at_action(unit, unit, u2, x, y, 0))) {
	    prep_create_at_action(unit, unit, u2, x, y, 0);
	    return TASK_PREPPED_ACTION;
	} else {
	    /* Try creating in an adjacent cell. */
	    for_all_directions(dir) {
		if (interior_point_in_dir(x, y, dir, &nx, &ny)
		    && valid(check_create_at_action(unit, unit, u2, nx, ny, 0))) {
		    prep_create_at_action(unit, unit, u2, nx, ny, 0);
		    return TASK_PREPPED_ACTION;
		}
	    }
	    return TASK_FAILED;
	}
    } else {
	/* Record the unit's id for use the next time around. */
	task->args[1] = unit2->id;
    }
    /* We have an incomplete unit to work on, try to do a build action. */
    if (valid(check_build_action(unit, unit, unit2))) {
	prep_build_action(unit, unit, unit2);
	return TASK_PREPPED_ACTION;
    } else {
	/* Try three times before giving up. */
	return (task->execnum < 3 ? TASK_IS_INCOMPLETE : TASK_FAILED);
    }
}

Unit *
find_unit_to_complete(unit, task)
Unit *unit;
Task *task;
{
    Unit *occ;
    int u = unit->type, nx, ny, range;
    int u2 = task->args[0];
    int x = unit->x, y = unit->y;

    /* Check out the unit supposedly in progress. */
    if (task->args[1] != 0) {
	occ = find_unit(task->args[1]);
	if (in_play(occ) && occ->type == u2 && !fullsized(occ)) 
	  return occ;
    }
    /* Maybe search for any appropriate incomplete occupants. */
    for_all_occupants(unit, occ) {
	if (in_play(occ)
	    && !fullsized(occ)
	    && occ->type == u2
	    && occ->side == unit->side) 
	  return(occ);
    }
    /* Or else search for any appropriate incomplete units in this cell. */
    for_all_stack(x, y, occ) {
	if (in_play(occ)
	    && !fullsized(occ)
	    && occ->type == u2
	    && occ->side == unit->side) {
	    return (occ);
    	}
    }
    /* Or else search nearby area. */
    if (is_unit_type(u2)) {
	range = uu_build_range(u, u2);
	if (range > 0) {
	    tmpbuilder = unit;
	    tmpbuildutype = u2;
	    if (search_around(x, y, range, test_for_buildable, &nx, &ny, 1)) {
	    	return(tmpbuildunit);
	    }
	}
    }
    /* nothing found */
    return NULL;
}

/* This is a "pure research" task, with the sole objective of increasing
   technology. */

static TaskOutcome
do_research_task(unit, task)
Unit *unit;
Task *task;
{
    int u = unit->type;
    int u2 = task->args[0], lev = task->args[1];
    Side *us = unit->side;

    /* Independents can never ever do research. */
    if (us == NULL)
      return TASK_FAILED;
    if (us->tech[u2] > u_tech_max(u2))
      return TASK_FAILED; /* actually an error */
    if (us->tech[u2] >= lev)
      return TASK_IS_COMPLETE;
    if (uu_acp_to_research(u, u2) <= 0)
      return TASK_FAILED;
    if (valid(check_research_action(unit, unit, u2))) {
	prep_research_action(unit, unit, u2);
	return TASK_PREPPED_ACTION;
    } else {
	/* We get three tries to research before giving up. */
	return (task->execnum < 3 ? TASK_IS_INCOMPLETE : TASK_FAILED);
    }
}

/* This is to capture a given type/side of unit at a given place. */

static TaskOutcome
do_capture_task(unit, task)
Unit *unit;
Task *task;
{
    int u = unit->type, tx, ty, tu2, ts2, dist, movedist, rslt;
    Unit *unit2;
    Side *us = unit->side;

    /* (should be able to say how hard to try) */
    tx = task->args[0];  ty = task->args[1];
    tu2 = task->args[2];
    ts2 = task->args[3];
    dist = distance(tx, ty, unit->x, unit->y);
    switch (dist) {
      case 0:
      case 1:
	for_all_stack(tx, ty, unit2) {
	    if ((ts2 >= 0 ?
		 (side_number(unit2->side) == ts2) :
		 (unit2->side != us /* should be "not a friendly side" */))
		&& (tu2 == NONUTYPE || tu2 == unit->type)) {
		if (valid(check_capture_action(unit, unit, unit2))) {
		    prep_capture_action(unit, unit, unit2);
		    return TASK_PREPPED_ACTION;
		} else if (valid(check_attack_action(unit, unit, unit2, 100))) {
		    prep_attack_action(unit, unit, unit2, 100);
		    return TASK_PREPPED_ACTION;
		} else {
		    /* We get several tries to capture before giving up. */
		    unit->plan->reserve = TRUE;
		    return (task->execnum < 5 ? TASK_IS_INCOMPLETE : TASK_FAILED);
		}
	    }
	}
	/* Nothing was here to capture. */
	return TASK_IS_COMPLETE;
      default:
	/* If on mobile transport, let it handle things. */
	if (unit->transport != NULL
	    && mobile(unit->transport->type)
	    /* and the transport is not blocked */
	    && flip_coin()) {
	    return TASK_IS_INCOMPLETE;
	}
	/* If out of range and can move, push a task to get closer (maybe). */
	if (mobile(u) && flip_coin()) {
	    movedist = max(1 /* attack range */, u_range(u));
	    if (dist > movedist + u_acp(u) /* or dist that could be covered in 1-2 turns */) {
		movedist = dist - max(1, (dist - movedist) / 4);
		/* We're too far away to capture directly, add a move-to
		   task. */
		push_move_near_task(unit, tx, ty, movedist);
		return TASK_IS_INCOMPLETE;
	    }
	}
	return TASK_FAILED;
    }
}

/* The disband task's purpose is to make the unit disappear, so just keep doing
   actions; when the unit goes away, so will the task. */

static TaskOutcome
do_disband_task(unit, task)
Unit *unit;
Task *task;
{
    if (valid(check_disband_action(unit, unit))) {
	prep_disband_action(unit, unit);
	return TASK_PREPPED_ACTION;
    } else {
	/* (should try to find a nearby unit to do it?) */
	return (task->execnum < 5 ? TASK_IS_INCOMPLETE : TASK_FAILED);
    }
}

/* This task just attempts to do the explicitly specified action repeatedly. */

static TaskOutcome
do_action_task(unit, task)
Unit *unit;
Task *task;
{
    int i;

    if (task->args[0] > 0) {
	--(task->args[0]);
    	if (unit->act == NULL)
    	  return TASK_FAILED;
    	unit->act->nextaction.type = task->args[1];
    	for (i = 0; i < MAXACTIONARGS; ++i) {
	    unit->act->nextaction.args[i] = task->args[i + 2];
    	}
    	/* act on self always? */
    	unit->act->nextaction.actee = unit->id;
	return TASK_PREPPED_ACTION;
    } else {
	return TASK_IS_COMPLETE;
    }
}

static TaskOutcome
do_hit_position_task(unit, task)
Unit *unit;
Task *task;
{
    int u = unit->type, tx, ty, dist;

    /* This is to hit a given place. */
    /* (ask for a number of hits?) */
    tx = task->args[0];  ty = task->args[1];
    dist = distance(tx, ty, unit->x, unit->y);
    if (valid(check_fire_into_action(unit, unit, tx, ty, 0, -1))) {
	prep_fire_into_action(unit, unit, tx, ty, 0, -1);
	return TASK_PREPPED_ACTION;
    } else if (mobile(u) && flip_coin()) {
	/* We're too far away to shoot directly, add a move-to task. */
	push_move_near_task(unit, tx, ty, max(1 /* attack range */, u_range(u)));
	return TASK_IS_INCOMPLETE;
    }
    return TASK_FAILED;
}

static TaskOutcome
do_hit_unit_task(unit, task)
Unit *unit;
Task *task;
{
    int u = unit->type, tx, ty, dist, movedist, enemythere, uview, tu, ts;
    Unit *unit2;
    Side *us = unit->side;

    /* This is to hit a (given type/side of) unit at a given place. */
    /* (should add spec for number of hits to attempt?) */
    tx = task->args[0];  ty = task->args[1];
    tu = task->args[2];  ts = task->args[3];
    dist = distance(tx, ty, unit->x, unit->y);
    if (dist <= 1 /* direct attack range */) {
	for_all_stack(tx, ty, unit2) {
	    if (!trusted_side(us, unit2->side)
	    	&& (tu == NONUTYPE || unit2->type == tu)
	    	/* (should check designated side to hit, if defined) */
	    	) {
		if (valid(check_attack_action(unit, unit, unit2, 100))) {
		    prep_attack_action(unit, unit, unit2, 100);
		    return TASK_PREPPED_ACTION;
		}
	    }
	}
	/* but might still be able to fire! */
	return TASK_FAILED;
    }
    if (dist < u_range_min(u)) {
	/* should move further away */
	return TASK_FAILED;
    }
    if (dist < u_range(u)) {
	if (units_visible(us, tx, ty)) {
	  for_all_stack(tx, ty, unit2) {
	    if (!trusted_side(us, unit2->side)
	    	&& (tu == NONUTYPE || unit2->type == tu)
	    	/* (should check designated side to hit, if defined) */
	    	) {
		if (valid(check_fire_at_action(unit, unit, unit2, -1))
		    && fire_can_damage(unit, unit2)) {
		    prep_fire_at_action(unit, unit, unit2, -1);
		    return TASK_PREPPED_ACTION;
		}
	    }
	  }
	}
	return TASK_FAILED;	    
    }
    if (units_visible(us, tx, ty)) {
	enemythere = FALSE;
	for_all_stack(tx, ty, unit2) {
	    if (!trusted_side(us, unit2->side)
	    	&& (tu == NONUTYPE || unit2->type == tu)
	    	/* (should check designated side to hit, if defined) */
	    	) {
	    	enemythere = TRUE;
	    	break;
	    }
	}
	if (!enemythere) {
	    if (tu != NONUTYPE) {
		return TASK_FAILED;
	    } else {
		return TASK_IS_COMPLETE;
	    }
	}
    } else {
	/* Have to assess old image or non-image. */
	uview = unit_view(us, tx, ty);
	if (uview != EMPTY) {
	    if (tu == NONUTYPE) {
	    	/* Just keep going. */
	    }
	} else {
	    /* Not clear if disappearance of target means success or failure,
	       but go with failure. */
	    return TASK_FAILED;
	}
    }
    /* If on mobile transport, let it handle things. */
    if (unit->transport != NULL
        && mobile(unit->transport->type)
	/* and the transport is not blocked */
        && flip_coin()) {
        return TASK_IS_INCOMPLETE;
    }
    /* If out of range and can move, push a task to get closer (maybe). */
    if (mobile(u) && flip_coin()) {
	movedist = max(1 /* attack range */, u_range(u));
	if (dist > movedist + u_acp(u) /* or dist that could be covered in 1-2 turns */) {
	    movedist = dist - max(1, (dist - movedist) / 4);
	}
	push_move_near_task(unit, tx, ty, movedist);
	return TASK_IS_INCOMPLETE;
    }
    return TASK_FAILED;
}

/* Return true if the unit can actually damage the other unit. */

int
fire_can_damage(unit, unit2)
Unit *unit, *unit2;
{
    if (!alive(unit) || !alive(unit2))
      return FALSE;
    /* (should check for right kind of ammo) */
    if (uu_hit(unit->type, unit2->type) <= 0)
      return FALSE;
    /* (this is dubious - a no-damage attack could still consume acp) */
    if (uu_damage(unit->type, unit2->type) <= 0)
      return FALSE;
    /* (should check if victim hp might be under damage low bounds) */
    return TRUE;
}

static TaskOutcome
do_move_dir_task(unit, task)
Unit *unit;
Task *task;
{
    int dir, tx, ty;
    Unit *unit2;

    if ((task->args[1])-- > 0) {
	dir = task->args[0];
	if (!point_in_dir(unit->x, unit->y, dir, &tx, &ty)) {
		return TASK_FAILED;
	}
	if (unit_at(tx, ty)) {
	    for_all_stack(tx, ty, unit2) {
	    }
	    return TASK_FAILED;
	} else if (valid(check_move_action(unit, unit, tx, ty, 0))) {
	    prep_move_action(unit, unit, tx, ty, 0);
	    return TASK_PREPPED_ACTION;
	} else {
	    return TASK_FAILED;
	}
    } else {
	return TASK_IS_COMPLETE;
    }
}

enum choicestate {
    eitherway,
    leftthenright,
    rightthenleft,
    leftonly,
    rightonly
};

static TaskOutcome do_approach_subtask PARAMS ((Unit *unit, Task *task, int tx, int ty, short *statep));

/* The move-to task is the main way for units to get from point A to point B.
   In addition to the destination, the task has a required distance, so it will
   succeed if the unit is within that distance to the nominal destination. */

static TaskOutcome
do_move_to_task(unit, task)
Unit *unit;
Task *task;
{
    int dist, tx, ty;
    Unit *unit2, *occ;

    /* This task is to get to a designated location somehow. */
    tx = task->args[0];  ty = task->args[1];
    dist = distance(tx, ty, unit->x, unit->y);
    if (dist <= task->args[2]) {
	return TASK_IS_COMPLETE;
    }
    switch (dist) {
      case 0:
	/* We're there already, nothing more to do. */
	return TASK_IS_COMPLETE;
      case 1:
	/* Adjacent cell, do a single move. */
	if (unit_at(tx, ty)) {
	    for_all_stack(tx, ty, unit2) {
		if (can_occupy(unit, unit2)) {
		    if (valid(check_enter_action(unit, unit, unit2))) {
			prep_enter_action(unit, unit, unit2);
			return TASK_PREPPED_ACTION;
		    } else {
			continue;
		    }
		} else if (!trusted_side(unit->side, unit2->side)) {
		    /* This is probably not a good idea, combat odds not
		       taken into account. */
		    if (valid(check_attack_action(unit, unit, unit2, 100))) {
			prep_attack_action(unit, unit, unit2, 100);
			return TASK_PREPPED_ACTION;
		    } else {
			continue;
		    }
		} else {
		    /* We could find room in an occupant... */
		    for_all_occupants(unit2, occ) {
			if (can_occupy(unit, occ)) {
			    if (valid(check_enter_action(unit, unit, occ))) {
				prep_enter_action(unit, unit, occ);
				return TASK_PREPPED_ACTION;
			    }
			}
		    }
		}
	    }
	    return TASK_FAILED;
	}
	if (valid(check_move_action(unit, unit, tx, ty, unit->z))) {
	    /* Moving into an empty cell. */
	    prep_move_action(unit, unit, tx, ty, unit->z);
	    return TASK_PREPPED_ACTION;
	} else {
	    return TASK_FAILED;
	}
	break;
      default:
	/* Still some distance away, pick a way to go. */
	return do_approach_subtask(unit, task, tx, ty, &(task->args[4]));
    }
    return TASK_FAILED;
}

static TaskOutcome
do_approach_subtask(unit, task, tx, ty, statep)
Unit *unit;
Task *task;
int tx, ty;
short *statep;
{
    int nx, ny, dirs[NUMDIRS], numdirs, i, numdirs2;
    Unit *unit2;

    /* If on mobile transport, let it handle things. */
    if (unit->transport != NULL
	&& mobile(unit->transport->type)
	/* and the transport is not blocked */
	&& flip_coin()) {
	unit->plan->reserve = TRUE;
	return TASK_IS_INCOMPLETE;
    }
    numdirs = choose_move_dirs(unit, tx, ty, TRUE,
			       plausible_move_dir, sort_directions, dirs);
    for (i = 0; i < numdirs; ++i) {
	point_in_dir(unit->x, unit->y, dirs[i], &nx, &ny);
	for_all_stack(nx, ny, unit2) {
	    if (can_occupy(unit, unit2)) {
		if (valid(check_enter_action(unit, unit, unit2))) {
		    prep_enter_action(unit, unit, unit2);
		    /* We (probably) made forward progress, so reopen choice of dirs. */
		    *statep = eitherway;
		    return TASK_PREPPED_ACTION;
		} else {
		    continue;
		}
	    } else if (!trusted_side(unit->side, unit2->side)) {
		if (unit->occupant) {
		    /* More important to find a way through. */
		    continue;
		} else {
		    /* This will encourage some re-evaluation. */
		    return TASK_FAILED;
		}
#if 0 /* the following is rarely a good idea */
		if (valid(check_attack_action(unit, unit, unit2, 100))) {
		    prep_attack_action(unit, unit, unit2, 100);
		    /* We (probably) made forward progress, so reopen choice of dirs. */
		    *statep = eitherway;
		    return TASK_PREPPED_ACTION;
		} else {
		    continue;
		}
#endif
	    }
	}
	if (valid(check_move_action(unit, unit, nx, ny, unit->z))) {
	    prep_move_action(unit, unit, nx, ny, unit->z);
	    /* We (probably) made forward progress, so reopen choice of dirs. */
	    *statep = eitherway;
	    return TASK_PREPPED_ACTION;
	}
    }
    /* Get both right and left non-decreasing dirs. */
    numdirs  = choose_move_dirs(unit, tx, ty, TRUE, NULL, NULL, dirs);
    numdirs2 = choose_move_dirs(unit, tx, ty, FALSE, NULL, NULL, dirs);
    for (i = numdirs; i < numdirs2; ++i) {
	if (plausible_move_dir(unit, dirs[i])) {
	    switch (*statep) {
	      case eitherway:
		if (i == numdirs)
		  *statep = leftonly /* leftthenright */;
		if (i == numdirs+1)
		  *statep = rightonly /* rightthenleft */;
		break;
#if 0
	      case leftthenright:
		if (i == numdirs)
		  *statep = rightonly;
		if (i == numdirs+1)
		  *statep = rightonly;
		continue;
		break;
	      case rightthenleft:
		if (i == numdirs+1)
		  *statep = leftonly;
		continue;
		break;
#endif
	      case leftonly:
		if (i == numdirs+1)
		  continue;
		break;
	      case rightonly:
		if (i == numdirs)
		  continue;
		break;
	      default:
		run_warning("Weird right/left state %d", *statep);
		*statep = leftonly;
		break;
	    }
	} else {
	    switch (*statep) {
	      case eitherway:
		if (i == numdirs)
		  *statep = rightonly;
		if (i == numdirs+1)
		  *statep = leftonly;
		continue;
		break;
#if 0
	      case leftthenright:
		if (i == numdirs)
		  *statep = rightonly;
		if (i == numdirs+1)
		  *statep = rightonly;
		continue;
		break;
	      case rightthenleft:
		if (i == numdirs+1)
		  *statep = leftonly;
		continue;
		break;
#endif
	      case leftonly:
		if (i == numdirs)
		  return TASK_FAILED;
		if (i == numdirs+1)
		  continue;
		break;
	      case rightonly:
		if (i == numdirs)
		  continue;
		if (i == numdirs+1)
		  return TASK_FAILED;
		break;
	      default:
		run_warning("Weird right/left state %d", *statep);
		*statep = leftonly;
		break;
	    }
	}
	point_in_dir(unit->x, unit->y, dirs[i], &nx, &ny);
	for_all_stack(nx, ny, unit2) {
	    if (can_occupy(unit, unit2)) {
		if (valid(check_enter_action(unit, unit, unit2))) {
		    prep_enter_action(unit, unit, unit2);
		    return TASK_PREPPED_ACTION;
		} else {
		    continue;
		}
	    } else if (!trusted_side(unit->side, unit2->side)) {
		if (unit->occupant) {
		    /* More important to find a way through. */
		    continue;
		} else {
		    /* This will encourage some re-evaluation. */
		    return TASK_FAILED;
		}
#if 0				/* the following is rarely a good idea */
		if (valid(check_attack_action(unit, unit, unit2, 100))) {
		    prep_attack_action(unit, unit, unit2, 100);
		    return TASK_PREPPED_ACTION;
		} else {
		    continue;
		}
#endif
	    }
	}
	if (valid(check_move_action(unit, unit, nx, ny, unit->z))) {
	    prep_move_action(unit, unit, nx, ny, unit->z);
	    return TASK_PREPPED_ACTION;
	}
    }
    return TASK_FAILED;
}

static TaskOutcome
do_occupy_task(unit, task)
     Unit *unit;
     Task *task;
{
    int dist;
    Unit *transport = find_unit(task->args[0]);

    if (!in_play(transport))
      return TASK_FAILED;
    /* (should also fail if we don't know where transport is anymore) */
    if (unit->transport == transport) {
	return TASK_IS_COMPLETE;
    }
    dist = distance(unit->x, unit->y, transport->x, transport->y);
    if (dist <= 1) {
	if (valid(check_enter_action(unit, unit, transport))) {
	    prep_enter_action(unit, unit, transport);
	    return TASK_PREPPED_ACTION;
	} else {
	    /* Try a couple times, then fail if not working. */
	    return (task->execnum < 3 ? TASK_IS_INCOMPLETE : TASK_FAILED);
	}
    } else {
	/* Still some distance away, pick a way to go. */
	return do_approach_subtask(unit, task, transport->x, transport->y, &(task->args[1]));
    }
}

/* Wait around for a particular unit.  Give up if the unit is not forthcoming. */

static TaskOutcome
do_pickup_task(unit, task)
Unit *unit;
Task *task;
{
    Unit *occupant = find_unit(task->args[0]);

    if (!in_play(occupant))
      return TASK_FAILED;
    wake_unit(occupant, FALSE, 0, NULL);
    if (occupant->transport == unit) {
	return TASK_IS_COMPLETE;
    } else if (task->execnum > 10) {
	/* Waiting around isn't working for us, give up.  If the
	   prospective occupant still needs us, we'll get another
	   call. */
	return TASK_FAILED;
    } else {
	if (valid(check_enter_action(occupant, occupant, unit))) {
	    prep_enter_action(occupant, occupant, unit);
	    return TASK_PREPPED_ACTION;
	} else if (valid(check_enter_action(unit, occupant, unit))) {
	    prep_enter_action(unit, occupant, unit);
	    return TASK_PREPPED_ACTION;
	} else {
	    return (task->execnum < 5 ? TASK_IS_INCOMPLETE : TASK_FAILED);
	}
    }
}

static TaskOutcome
do_produce_task(unit, task)
Unit *unit;
Task *task;
{
    int m, tot, sofar, amt;

    m = task->args[0];
    tot = task->args[1];
    sofar = task->args[2];
    if (sofar >= tot)
      return TASK_IS_COMPLETE;
    amt = um_material_per_production(unit->type, m);
    if (valid(check_produce_action(unit, unit, m, amt))) {
	prep_produce_action(unit, unit, m, amt);
	return TASK_PREPPED_ACTION;
    }
    return TASK_FAILED;
}

Unit *
repair_here(x, y)
int x, y;
{
    Unit *unit;

    for_all_stack(x, y, unit) {
	/* what about allies? */
	if (unit->side == tmpside && can_carry(unit, tmpunit)) {
	    /* this should be controlled by doctrine? */
	    /* shouldn't wake up, should get a new task to "wait up"
	       or even approach if possible */
	/*    wake_unit(unit, FALSE, WAKEOWNER, NULL);  */
	    return unit;
	}
	/* should look at occupants in stack too */
    }
    return NULL;
}

int
repair_test(x, y)
int x, y;
{
    return (repair_here(x, y) != NULL);
}

static TaskOutcome
do_repair_task(unit, task)
Unit *unit;
Task *task;
{
    int x, y, u = unit->type, m, range = area.maxdim;
    int ux = unit->x, uy = unit->y;
    Unit *unit2;

    for_all_material_types(m) {
	if (um_consumption_per_move(u, m) > 0) {
	    range = min(range, unit->supply[m] / um_consumption_per_move(u, m));
	}
    }
    tmpside = unit->side;
    tmpunit = unit;
    if (unit->hp == u_hp(u)) {  /* what if unit is multi-part? */
    	return TASK_IS_COMPLETE;
    } else if (unit->transport != NULL) {
	set_unit_reserve(unit->side, unit, TRUE, FALSE);
    	return TASK_IS_INCOMPLETE;
    } else if ((unit2 = repair_here(ux, uy)) != NULL
    	&& unit2 != unit->transport) {	
    	prep_enter_action(unit, unit, unit2);
	return TASK_PREPPED_ACTION;
    } else if (search_around(ux, uy, range, repair_test, &x, &y, 1)) {
    	/* (should collect actual unit and chase it directly) */
	push_move_to_task(unit, x, y);
	return TASK_IS_INCOMPLETE;
    } else {
    	/* (should be able to signal interface usefully somehow) */
	return TASK_FAILED;
    }
}

static int *lowm = NULL, numlow; 

Unit *
aux_resupply_here(unit)
Unit *unit;
{
    int i, enough = TRUE;
    Unit *occ;

    /* what about allies? */
    if (unit_trusts_unit(unit, tmpunit)
	&& can_carry(unit, tmpunit)) {
	for (i = 0; i < numlow; ++i) {
	    if (unit->supply[lowm[i]] == 0)
	      enough = FALSE;
	}
	/* this should be controlled by doctrine? */
	/* shouldn't wake up, should get a new task to "wait up"
	   or even approach if possible */
	/*    wake_unit(unit, FALSE, WAKEOWNER, NULL);  */
	if (enough)
	  return unit;
    }
    for_all_occupants(unit, occ) {
	if (aux_resupply_here(occ)) {
	    return occ;
	}
    }
    return NULL;
}

Unit *
resupply_here(x, y)
int x, y;
{
    Unit *unit, *resupplier;

    for_all_stack(x, y, unit) {
    	resupplier = aux_resupply_here(unit);
    	if (resupplier)
    	  return resupplier;
    }
    return NULL;
}

int
resupply_test(x, y)
int x, y;
{
    return (resupply_here(x, y) != NULL);
}

/* Replenish our supplies, using one of several strategies, which as usual
   depends on the game, unit, terrain, etc.  Strategies include 1) wait for
   supply line or own production to replenish, 2) move to productive terrain
   and then wait, 3) move within range of a supplier, and 4) request a supplier
   to approach. */

/* (should see if production actions would resupply, prep those actions) */

static TaskOutcome
do_resupply_task(unit, task)
Unit *unit;
Task *task;
{
    int x, y, u = unit->type, m, range;
    int ux = unit->x, uy = unit->y;
    Unit *unit2;

    tmpside = unit->side;
    tmpunit = unit;
    if (lowm == NULL)
      lowm = (int *) xmalloc(nummtypes * sizeof(int));
    numlow = 0;
    if (task->args[0] == NONMTYPE) {
	for_all_material_types(m) {
	    if (unit->supply[m] < um_storage_x(u, m)) {
		lowm[numlow++] = m;
	    }
	}
    } else {
	m = task->args[0];
	if (unit->supply[m] < um_storage_x(u, m)) {
	    lowm[numlow++] = m;
	}
    }
    /* We're all full up, must be OK. */
    if (numlow == 0) {
    	return TASK_IS_COMPLETE;
    } else if (can_auto_resupply_self(unit, lowm, numlow)) {
	set_unit_reserve(unit->side, unit, TRUE, FALSE);
    	return (probability(10) ? TASK_FAILED : TASK_IS_INCOMPLETE);
    } else if (unit->transport != NULL) {
	/* (should fix this test, transport is not necessarily of any help) */
    	/* (could attempt to resupply via direct action) */
	set_unit_reserve(unit->side, unit, TRUE, FALSE);
    	return (probability(10) ? TASK_FAILED : TASK_IS_INCOMPLETE);
    } else if ((unit2 = resupply_here(ux, uy)) != NULL
    	&& unit2 != unit->transport) {	
    	prep_enter_action(unit, unit, unit2);
	return TASK_PREPPED_ACTION;
    } else {
	/* Compute how far out to look for a resupply point; be a little
	   optimistic. */
	range = operating_range_best(u);
	/* (should reduce range if materials are really low) */
	if (search_around(ux, uy, range, resupply_test, &x, &y, 1)) {
    	    /* (should collect actual unit and chase it directly) */
    	    /* (should only need to get within outlength of needed supplies) */
	    push_move_to_task(unit, x, y);
	    return TASK_IS_INCOMPLETE;
	} else {
	    /* Failure - sometimes just sit, but usually try something else. */
	    if (probability(10))
	      set_unit_reserve(unit->side, unit, TRUE, FALSE);
    	    /* (should be able to signal interface usefully somehow) */
	    return TASK_FAILED;
	}
    }
}


/* Return true if our own automatic material production is *greater*
   than our consumption, for the given list of materials. */

int
can_auto_resupply_self(unit, materials, numtypes)
Unit *unit;
int *materials, numtypes;
{
    int u = unit->type, i, m, rslt = TRUE, t = terrain_at(unit->x, unit->y);

    for (i = 0; i < numtypes; ++i) {
	m = materials[i];
	if (um_base_production(u, m) * ut_productivity(u, t) <= um_base_consumption(u, m))
	  rslt = FALSE;
    }
    return rslt;
}

static TaskOutcome
do_sentry_task(unit, task)
Unit *unit;
Task *task;
{
    if (task->args[0] > 0) {
	unit->plan->reserve = TRUE;
	--(task->args[0]);
	return TASK_IS_INCOMPLETE;
    } else {
	/* Unit won't necessarily wake up, may just replan and
	   continue sleeping. */
	return TASK_IS_COMPLETE;
    }
}

/* This is the main routine for a unit to execute a task.  It basically
   consists of a dispatch to the execution code for each task type, and
   handling for a task's several possible outcomes.  Note that a task
   does *not* directly invoke any actions; instead it will schedule
   ("prep") an action, which will be executed later by execute_action.
   Therefore, it is possible for a task to succeed but the action to
   fail, although each task type's code tries to reduce the chances
   of this happening (not possible to prevent entirely - unit may
   become damaged and unable to do perform an action after the task
   had decided on that action). */

TaskOutcome
execute_task(unit)
Unit *unit;
{
    Plan *plan = unit->plan;
    TaskOutcome rslt;
    Task *task;

    /* This should never happen. */
    if (unit->plan == NULL)
      run_error("???");
    task = plan->tasks;
    rslt = execute_task_aux(unit, task);
    DMprintf("%s did task %s: ", unit_desig(unit), task_desig(task));
    /* Now look at what happened with task execution. */
    switch (rslt) {
      case TASK_UNKNOWN:
	DMprintf("???unknown outcome???");
	break;
      case TASK_FAILED:
        ++task->retrynum;
	DMprintf("failed try %d, ", task->retrynum);
	if (probability(20) || task->retrynum > 5) {
	    pop_task(plan);
	    DMprintf("removed it");
	    /* We might be buzzing, so maybe go into reserve. */
	    if (probability(20)) {
	    	plan->reserve = TRUE;
	    	DMprintf(" and went into reserve");
	    }
	} else {
	    DMprintf("will retry");
	}
	break;
      case TASK_IS_INCOMPLETE:
	/* Leave the task alone. */
	DMprintf("incomplete");
	break;
      case TASK_PREPPED_ACTION:
	/* Mention the action that was prepared to execute. */
	DMprintf("prepped action %s", action_desig(&(unit->act->nextaction)));
	break;
      case TASK_IS_COMPLETE:
	DMprintf("completed after %d executions", task->execnum);
	pop_task(plan);
	break;
      default:
	break;
    }
    DMprintf("\n");
    /* Give AIs a chance to decide what to do with the result of a task. */
    if (unit->side != NULL && side_has_ai(unit->side)) {
    	ai_react_to_task_result(unit->side, unit, task, rslt);
    }
    return rslt;
}

/* Perform a single given task. */

TaskOutcome
execute_task_aux(unit, task)
Unit *unit;
Task *task;
{
    if (!alive(unit) || task == NULL)
      return TASK_UNKNOWN;
    DMprintf("%s doing task %s\n", unit_desig(unit), task_desig(task));
    /* Count this execution. */
    ++task->execnum;
    /* (should use function pointer table...) */
    switch (task->type) {
      case TASK_NONE:
	/* This is a no-op, useful as a placeholder.  Always "succeeds". */
	return TASK_IS_COMPLETE;
      case TASK_BUILD:
	return do_build_task(unit, task);
      case TASK_CAPTURE:
	return do_capture_task(unit, task);
      case TASK_DISBAND:
	return do_disband_task(unit, task);
      case TASK_DO_ACTION:
	return do_action_task(unit, task);
      case TASK_HIT_POSITION:
	return do_hit_position_task(unit, task);
      case TASK_HIT_UNIT:
	return do_hit_unit_task(unit, task);
      case TASK_MOVE_DIR:
	return do_move_dir_task(unit, task);
      case TASK_MOVE_TO:
	return do_move_to_task(unit, task);
      case TASK_OCCUPY:
        return do_occupy_task(unit, task);
      case TASK_PICKUP:
        return do_pickup_task(unit, task);
      case TASK_PRODUCE:
        return do_produce_task(unit, task);
      case TASK_REPAIR:
        return do_repair_task(unit, task);
      case TASK_RESEARCH:
	return do_research_task(unit, task);
      case TASK_RESUPPLY:
        return do_resupply_task(unit, task);
      case TASK_SENTRY:
        return do_sentry_task(unit, task);
      default:
	/* This is bad, but not necessarily a reason to die instantly. */
	run_warning("Unknown task type %d", task->type);
    	return TASK_FAILED;
    }
}

/* This weird-looking routine computes next directions for moving to a
   given spot.  The number of directions ranges from 1 to 4, depending
   on whether there is a straight-line path to the dest, and whether we are
   required to take a direct path or are allowed to move in dirs that don't
   the unit any closer (we never increase our distance though).
   Some trickinesses:  if area wraps, must resolve ambiguity about
   getting to the same place going either direction (we pick shortest). */

int
choose_move_dirs(unit, tx, ty, shortest, dirtest, dirsort, dirs)
Unit *unit;
int tx, ty, shortest, *dirs;
int (*dirtest) PARAMS ((Unit *, int));
void (*dirsort) PARAMS ((Unit *, int *, int));
{
    int dx, dxa, dy, dist, d1, d2, d3, d4, axis = -1, hextant = -1;
    int numdirs = 0, shortestnumdirs;

    dist = distance(unit->x, unit->y, tx, ty);
    dx = tx - unit->x;  dy = ty - unit->y;

    if (area.xwrap) {
	dxa = (tx + area.width) - unit->x;
	if (ABS(dx) > ABS(dxa))
	  dx = dxa;
	dxa = (tx - area.width) - unit->x;
	if (ABS(dx) > ABS(dxa))
	  dx = dxa;
    }
    if (dx == 0 && dy == 0) {
	return -1;
    }
    axis = hextant = -1;
    if (dx == 0) {
	axis = (dy > 0 ? NORTHEAST : SOUTHWEST);
    } else if (dy == 0) {
	axis = (dx > 0 ? EAST : WEST);
    } else if (dx == (0 - dy)) {
	axis = (dy > 0 ? NORTHWEST : SOUTHEAST);
    } else if (dx > 0) {
	hextant = (dy > 0 ? EAST :
		   (ABS(dx) > ABS(dy) ? SOUTHEAST : SOUTHWEST));
    } else {
	hextant = (dy < 0 ? WEST :
		   (ABS(dx) > ABS(dy) ? NORTHWEST : NORTHEAST));
    }
    if (axis >= 0) {
	d1 = d2 = axis;
	if (dirtest == NULL || (*dirtest)(unit, d1)) {
	    dirs[numdirs++] = d1;
	}
    }
    if (hextant >= 0) {
	d1 = left_dir(hextant);
	d2 = hextant;
	if (dirtest == NULL || (*dirtest)(unit, d1)) {
	    dirs[numdirs++] = d1;
	}
	if (dirtest == NULL || (*dirtest)(unit, d2)) {
	    dirs[numdirs++] = d2;
	}
    }
    /* Check on other properties of the two choices. */
    if (numdirs > 1 && dirsort != NULL) {
    	(*dirsort)(unit, dirs, numdirs);
    }
    if (dist > 1 && !shortest) {
	shortestnumdirs = numdirs;
    	d3 = left_dir(d1);
    	d4 = right_dir(d2);
	if (dirtest == NULL || (*dirtest)(unit, d3)) {
	    dirs[numdirs++] = d3;
	}
	if (dirtest == NULL || (*dirtest)(unit, d4)) {
	    dirs[numdirs++] = d4;
	}
	if (numdirs > shortestnumdirs + 1 && dirsort != NULL) {
	    (*dirsort)(unit, dirs + shortestnumdirs, numdirs - shortestnumdirs);
	}
    }
    return numdirs;
}

/* A heuristic test for whether the given direction is a good one
   to move in. */

int
plausible_move_dir(unit, dir)
Unit *unit;
int dir;
{
    int u = unit->type, ux = unit->x, uy = unit->y, nx, ny, t, c;

    point_in_dir(ux, uy, dir, &nx, &ny);
    if (unit_at(nx, ny))
      return TRUE;
    t = terrain_at(nx, ny);
    if ((ut_vanishes_on(u, t)
         || ut_wrecks_on(u, t))
        && !can_move_via_conn(unit, nx, ny))
      return FALSE;
    if (ut_mp_to_enter(u, t) <= u_acp(u))
      return TRUE;
    if (numconntypes > 0) {
	/* Try each connection type to see if it works. */
	for_all_terrain_types(c) {
	    if (t_is_connection(c)
		&& aux_terrain_defined(c)
		&& connection_at(ux, uy, dir, c)) {
		if ((ut_mp_to_enter(u, c)
		     + ut_mp_to_traverse(u, c)
		     + ut_mp_to_leave(u, c)) <= u_acp(u))
		  return TRUE;
	    }
	}
    }
    return FALSE;
}

/* This compares the desirability of two different directions.  This is
   somewhat tricky, because it should return < 0 if i0 designates a BETTER
   direction than i1. */

int xs[NUMDIRS];
int ys[NUMDIRS];
int terrs[NUMDIRS];

static int
compare_directions(a0, a1)
CONST void *a0, *a1;
{
    int i0, i1;
    int u = tmputype, t0, t1;
    int ux = tmpunit->x, uy = tmpunit->y, u2 = NONUTYPE;
    int cost0 = 0, cost1 = 0, s, ps0, ps1, surr0, surr1, rslt;
    extern int *any_people_surrenders;

    i0 = *((int *) a0);  i1 = *((int *) a1);
    t0 = terrs[i0];  t1 = terrs[i1];
    if (tmpunit->transport)
      u2 = tmpunit->transport->type;
    /* Check the overall movement cost of each direction. */
    cost0 = total_move_cost(u, u2, ux, uy, 0, xs[i0], ys[i0], 0);
    cost1 = total_move_cost(u, u2, ux, uy, 0, xs[i1], ys[i1], 0);
    if (cost0 != cost1) {
	return cost0 - cost1;
    }
    if (1 /* not in supply */) {
	if ((rslt = ut_productivity(u, t1) - ut_productivity(u, t0)) != 0) {
	    return rslt;
	}
    }
    if ((rslt = ut_mp_to_leave(u, t1) - ut_mp_to_leave(u, t0)) != 0) {
	return rslt;
    }
    /* Chooser the safer terrain. */
    if ((rslt = ut_accident_hit(u, t1) - ut_accident_hit(u, t0)) != 0) {
	return rslt;
    }
    /* Choose the better-concealing terrain. */
    /* (should only do if limited visibility) */
    if ((rslt = ut_visibility(u, t1) - ut_visibility(u, t0)) != 0) {
	return rslt;
    }
    /* Prefer to go over cells that we can change to our side. */
    if (any_people_surrenders != NULL && any_people_surrenders[u]) {
    	s = side_number(tmpunit->side);
    	ps0 = people_side_at(xs[i0], ys[i0]);
    	ps1 = people_side_at(xs[i1], ys[i1]);
    	surr0 = ut_people_surrender(u, t0)
	  * ((ps0 != NOBODY && s != ps0) ? 1 : 0);
    	surr1 = ut_people_surrender(u, t1)
	  * ((ps1 != NOBODY && s != ps1) ? 1 : 0);
    	if (surr0 != surr1) {
	    return surr1 - surr0;
    	}
    }
    return 0;
}

void
sort_directions(unit, dirs, numdirs)
Unit *unit;
int *dirs, numdirs;
{
    int i, tmp, i0 = 0, i1 = 1, compar;

    for (i = 0; i < numdirs; ++i) { 
	point_in_dir(unit->x, unit->y, dirs[i], &(xs[i]), &(ys[i]));
	terrs[i] = terrain_at(xs[i], ys[i]);
    }
    tmpunit = unit;
    tmputype = unit->type;
    if (numdirs == 2) {
	compar = compare_directions(&i0, &i1);
    	if (compar > 0 || (compar == 0 && flip_coin())) {
    	    tmp = dirs[0];  dirs[0] = dirs[1];  dirs[1] = tmp;
    	}
    } else if (numdirs > 2) {
    	qsort(dirs, numdirs, sizeof(int), compare_directions);
	if (compare_directions(&i0, &i1) == 0 && flip_coin()) {
	    tmp = dirs[0];  dirs[0] = dirs[1];  dirs[1] = tmp;
	}
    }
}

/* Put the given task back onto the list of free tasks. */

void 
free_task(task)
Task *task;
{
    task->next = freetasks;
    freetasks = task;
}

void
add_task(unit, pos, task)
Unit *unit;
int pos;
Task *task;
{
    int numcleared;

    if (!in_play(unit) || unit->plan == NULL) {
	run_warning("Trying to do %s task with bad %s",
		    task_desig(task), unit_desig(unit));
	return;
    }
    Dprintf("To %s task agenda, add %s",
	    unit_desig(unit), task_desig(task));
    if (pos == CLEAR_AGENDA) {
	numcleared = clear_task_agenda(unit->plan);
	Dprintf(" (cleared %d existing tasks)", numcleared);
    }
    Dprintf("\n");
    switch (pos) {
      case 0:
      case CLEAR_AGENDA:
	/* Put the task on the front of the agenda. */
	task->next = unit->plan->tasks;
	unit->plan->tasks = task;
	break;
      default:
	run_error("can't do this yet");
	break;
    }
    /* Shouldn't be asleep any longer. */
    unit->plan->asleep = FALSE;
    /* We're not in reserve. */
    unit->plan->reserve = FALSE;
    /* Presumably we're no longer waiting to be told what to do. */
    if (unit->plan->waitingfortasks && unit->side)
      --(unit->side->numwaiting);
    unit->plan->waitingfortasks = FALSE;
    /* Reflect all this on displays. */
    update_unit_display(unit->side, unit, FALSE);
}

Task *
create_move_to_task(x, y)
int x, y;
{
    Task *task = create_task(TASK_MOVE_TO);

    task->args[0] = x;  task->args[1] = y;
    task->args[2] = 0;
    task->args[3] = 0;
    task->args[4] = eitherway;
    return task;
}

void
push_move_to_task(unit, x, y)
Unit *unit;
int x, y;
{
    if (!in_area(x, y)) {
    	run_warning("Trying to move %s to %d,%d", unit_desig(unit), x, y);
    	return;
    }
    add_task(unit, 0, create_move_to_task(x, y));
}

/* Give the unit a task to move to a given place, erasing every other task. */

void
set_move_to_task(unit, x, y)
Unit *unit;
int x, y;
{
    add_task(unit, CLEAR_AGENDA, create_move_to_task(x, y));
}

Task *
create_move_near_task(x, y, dist)
int x, y, dist;
{
    Task *task = create_task(TASK_MOVE_TO);

    task->args[0] = x;  task->args[1] = y;
    task->args[2] = 0;
    task->args[3] = dist;
    task->args[4] = eitherway;
    return task;
}

void
set_move_near_task(unit, x, y, dist)
Unit *unit;
int x, y, dist;
{
    add_task(unit, CLEAR_AGENDA, create_move_near_task(x, y, dist));
}

void
push_move_near_task(unit, x, y, dist)
Unit *unit;
int x, y, dist;
{
    add_task(unit, 0, create_move_near_task(x, y, dist));
}

/* Create a task to move in a given direction for a given distance. */

Task *
create_move_dir_task(dir, n)
int dir, n;
{
    Task *task = create_task(TASK_MOVE_DIR);

    task->args[0] = dir;
    task->args[1] = n;
    return task;
}

/* Fill in the given unit with direction-moving orders. */

void
set_move_dir_task(unit, dir, n)
Unit *unit;
int dir, n;
{
    add_task(unit, CLEAR_AGENDA, create_move_dir_task(dir, n));
}

/* This routine sets up a task to build a unit of the given type. */

Task *
create_build_task(u2, run)
int u2, run;
{
    Task *task = create_task(TASK_BUILD);

    task->args[0] = u2;
    task->args[3] = run;
    return task;
}

void
push_build_task(unit, u2, run)
Unit *unit;
int u2, run;
{
    add_task(unit, 0, create_build_task(u2, run));
}

/* This routine sets up a task to research a unit of the given type. */

Task *
create_research_task(u2, n)
int u2, n;
{
    Task *task = create_task(TASK_RESEARCH);

    task->args[0] = u2;
    task->args[1] = n;
    return task;
}

void
push_research_task(unit, u2, n)
Unit *unit;
int u2, n;
{
    add_task(unit, 0, create_research_task(u2, n));
}

void
set_hit_task(unit, x, y)
Unit *unit;
int x, y;
{
    Task *task = create_task(TASK_HIT_UNIT);

    task->args[0] = x;  task->args[1] = y;
    task->args[2] = NONUTYPE;
    task->args[3] = -1;
    add_task(unit, CLEAR_AGENDA, task);
}

void
push_specific_hit_task(unit, x, y, u, s)
Unit *unit;
int x, y, u, s;
{
    Task *task = create_task(TASK_HIT_UNIT);

    task->args[0] = x;  task->args[1] = y;
    task->args[2] = u;
    task->args[3] = s;
    add_task(unit, 0, task);
}

void
set_specific_hit_task(unit, x, y, u, s)
Unit *unit;
int x, y, u, s;
{
    Task *task = create_task(TASK_HIT_UNIT);

    task->args[0] = x;  task->args[1] = y;
    task->args[2] = u;
    task->args[3] = s;
    add_task(unit, CLEAR_AGENDA, task);
}

void
push_hit_task(unit, x, y)
Unit *unit;
int x, y;
{
    Task *task = create_task(TASK_HIT_UNIT);

    task->args[0] = x;  task->args[1] = y;
    task->args[2] = NONUTYPE;
    task->args[3] = -1;
    add_task(unit, 0, task);
}

Task *
create_capture_task(x, y)
int x, y;
{
    Task *task = create_task(TASK_CAPTURE);

    task->args[0] = x;  task->args[1] = y;
    task->args[2] = NONUTYPE;
    task->args[3] = -1;
    return task;
}

void
set_capture_task(unit, x, y)
Unit *unit;
int x, y;
{
    add_task(unit, CLEAR_AGENDA, create_capture_task(x, y));
}

void
push_capture_task(unit, x, y)
Unit *unit;
int x, y;
{
    add_task(unit, 0, create_capture_task(x, y));
}

void
set_disband_task(unit)
Unit *unit;
{
    add_task(unit, CLEAR_AGENDA, create_task(TASK_DISBAND));
}

Task *
create_resupply_task(m)
int m;
{
    Task *task = create_task(TASK_RESUPPLY);

    task->args[0] = m;
    return task;
}

void
set_resupply_task(unit, m)
Unit *unit;
int m;
{
    add_task(unit, CLEAR_AGENDA, create_resupply_task(m));
}

Task *
create_occupy_task(transport)
Unit *transport;
{
    Task *task = create_task(TASK_OCCUPY);

    task->args[0] = transport->id;
    task->args[1] = eitherway;
    /* add a waiting period also? */
    return task;
}

void
push_occupy_task(unit, transport)
Unit *unit, *transport;
{
    add_task(unit, 0, create_occupy_task(transport));
}

Task *
create_pickup_task(occ)
Unit *occ;
{
    Task *task = create_task(TASK_PICKUP);

    task->args[0] = occ->id;
    /* add a waiting period also? */
    return task;
}

void
push_pickup_task(unit, occ)
Unit *unit, *occ;
{
    add_task(unit, 0, create_pickup_task(occ));
}

Task *
create_produce_task(m, n)
int m, n;
{
    Task *task = create_task(TASK_PRODUCE);

    task->args[0] = m;
    task->args[1] = n;
    /* Third arg is amount produced, which starts at 0. */
    return task;
}

void
push_produce_task(unit, m, n)
Unit *unit;
int m, n;
{
    add_task(unit, 0, create_produce_task(m, n));
}

Task *
create_sentry_task(n)
int n;
{
    Task *task = create_task(TASK_SENTRY);

    task->args[0] = n;
    return task;
}

void
set_sentry_task(unit, n)
Unit *unit;
int n;
{
    add_task(unit, CLEAR_AGENDA, create_sentry_task(n));
}

void
push_sentry_task(unit, n)
Unit *unit;
int n;
{
    add_task(unit, 0, create_sentry_task(n));
}

extern int parse_location PARAMS ((Side *side, char *arg, int *xp, int *yp));

extern Unit *parse_unit PARAMS ((Side *side, char *arg));

/* Find a unit with the given name, either alive or dead. */

Unit *
parse_unit(side, nm)
Side *side;
char *nm;
{
    Unit *unit;

    if (empty_string(nm))
      return NULL;
    for_all_side_units(side, unit) {
	if (alive(unit) && unit->name != NULL && strcmp(unit->name, nm) == 0)
	  return unit;
    }
    /* Under some circumstances, we can refer to other sides' units by name. */
    for_all_units(unit) {
	if (alive(unit)
	    && unit->side != side
	    && unit->name != NULL
	    && strcmp(unit->name, nm) == 0
	    && (side->see_all
		|| side_sees_image(side, unit)))
	  return unit;
    }
    return NULL;
}

/* Given a textual description of a location, compute an x,y for it
   if possible. */

int
parse_location(side, arg, xp, yp)
Side *side;
char *arg;
int *xp, *yp;
{
    char *arg2;
    Unit *unit;

    *xp = strtol(arg, &arg2, 10);
    if (arg != arg2 && *arg2 == ',') {
	*yp = strtol(arg2 + 1, &arg, 10);
	if (arg2 + 1 != arg) {
	    return TRUE;
	} else if ((unit = parse_unit(side, arg)) != NULL) {
	    *xp = unit->x;  *yp = unit->y;
	    return TRUE;
	}
    }
    notify(side, "location \"%s\" not recognized", arg);
    return FALSE;
}

/* Given a string describing a task that has been entered in
   by a player, generate a task object and return the rest
   of the string, if NULL if failure. */

char *
parse_task(side, str, taskp)
Side *side;
char *str;
Task **taskp;
{
    int tasktype, i, x, y, n, dir, u, taskargs[MAXTASKARGS], numargs, rslt;
    char *arg, *arg2, substr[BUFSIZE], *rest, *argtypes;
    Unit *unit;

    rest = get_next_arg(str, substr, &arg);
    /* Recognize special cases of task types first. */
    if (strcmp(arg, "nil") == 0 || strcmp(arg, "nothing") == 0) {
	/* NULL task but non-NULL return indicates order cancellation. */
	*taskp = NULL;
	return rest;
    } else if (strcmp(arg, "move-near") == 0) {
	rest = get_next_arg(rest, substr, &arg);
	if (parse_location(side, arg, &x, &y)) {
	    rest = get_next_arg(rest, substr, &arg);
	    n = strtol(arg, &arg2, 10);
	    *taskp = create_move_near_task(x, y, n);
	    return rest;
	}
    }
    tasktype = lookup_task_type(arg);
    if (tasktype < 0) {
	notify(side, "task type \"%s\" not recognized", arg);
	return NULL;
    }
    switch (tasktype) {
      case TASK_MOVE_TO:
	rest = get_next_arg(rest, substr, &arg);
	if (parse_location(side, arg, &x, &y)) {
	    *taskp = create_move_to_task(x, y);
	    return rest;
	} else {
	    return NULL;
	}
	break;
      default:
	argtypes = taskdefns[tasktype].argtypes;
	numargs = strlen(argtypes);
	for (i = 0; i < numargs; ++i)
	  taskargs[i] = 0;
	rest = get_next_arg(rest, substr, &arg);
	for (i = 0; i < numargs; ++i) {
	    if (argtypes[i] == 'x' && argtypes[i+1] == 'y') {
		/* If there are two arguments that are together a position,
		   interpret both together. */
		if (parse_location(side, arg, &x, &y)) {
		    taskargs[i] = x;  taskargs[i + 1] = y;
		    ++i;
		} else {
		    return NULL;
		}
	    } else if (argtypes[i] == 'd') {
		char *mydirchars = "ulnbhy"; /* (a local copy of ui.c thing) */
		/* Match on names or chars for directions. */
		for_all_directions(dir) {
		    if (strcmp(arg, dirnames[dir]) == 0) {
			taskargs[i] = dir;
			goto nextarg;
		    }
		    if (strlen(arg) == 1 && arg[0] == mydirchars[dir]) {
			taskargs[i] = dir;
			goto nextarg;
		    }
		}
		notify(side, "direction \"%s\" not recognized", arg);
	    } else if (argtypes[i] == 'u') {
		u = utype_from_name(arg);
		if (u != NONUTYPE) {
		    taskargs[i] = u;
		} else {
		    notify(side, "unit type \"%s\" not recognized", arg);
		}
	    } else if (argtypes[i] == 'U') {
		unit = parse_unit(side, arg);
		if (unit != NULL) {
		    taskargs[i] = unit->id;
		} else {
		    notify(side, "unit called \"%s\" not recognized", arg);
		}
	    } else {
		/* Just collect an integer and stuff it. */
		taskargs[i] = strtol(arg, &arg2, 10);
		if (arg == arg2) {
		    notify(side, "argument \"%s\" not recognized", arg);
		}
	    }
	  nextarg:
	    rest = get_next_arg(str, substr, &arg);
	    /* (should check for end of command or not?) */
	}
	*taskp = create_task(tasktype);
	for (i = 0; i < numargs; ++i) {
	    (*taskp)->args[i] = taskargs[i];
	}
	return rest;
    }
}

int
lookup_task_type(name)
char *name;
{
    int i;

    for (i = 0; taskdefns[i].name != NULL; ++i)
      if (strcmp(name, taskdefns[i].name) == 0)
	return i; /* should get real enum? */
    return -1;
}

/* Describe a task succinctly - use for debugging only. */

char *
task_desig(task)
Task *task;
{
    int i, slen;
    char *argtypes;

    if (taskbuf == NULL)
      taskbuf = xmalloc(BUFSIZE);
    if (task) {
	sprintf(taskbuf, "{%s", taskdefns[task->type].name);
	argtypes = taskdefns[task->type].argtypes;
	slen = strlen(argtypes);
	for (i = 0; i < slen; ++i) {
	    tprintf(taskbuf, "%c%d", (i == 0 ? ' ' : ','), task->args[i]);
	}
	tprintf(taskbuf, " x %d", task->execnum);
	if (task->retrynum > 0) {
	    tprintf(taskbuf, " fail %d", task->retrynum);
	}
	strcat(taskbuf, "}");
    } else {
	sprintf(taskbuf, "no task");
    }
    return taskbuf;
}

