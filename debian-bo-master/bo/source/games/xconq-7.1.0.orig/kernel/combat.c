/* The combat-related actions of Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Rules of combat: the attacker hits the defender ("other") unit and its
   occupants, but the damage does not take effect right away.  If counter
   attacks are possible in this period, the defender always does so, with
   the same odds.  If the defender dies, then the attacker moves into the
   cell.  If the attacker dies, nothing happens.  If both survive, then the
   attacker may attempt to capture the defender. */

#include "conq.h"
extern int damaged_terrain_type PARAMS ((int t));

static int one_attack PARAMS ((Unit *atker, Unit *defender));
static void fire_on_unit PARAMS ((Unit *atker, Unit *other));
static void attack_unit PARAMS ((Unit *atker, Unit *other));
static void maybe_hit_unit PARAMS ((Unit *atker, Unit *other, int fire, int fallsoff));
static void hit_unit PARAMS ((Unit *unit, int hit, Unit *atker));

static void reckon_damage_here PARAMS ((int x, int y));
static void report_damage PARAMS ((Unit *unit, Unit *atker, Unit *mainunit));
static int retreat_unit PARAMS ((Unit *unit, Unit *atker));
static int retreat_in_dir PARAMS ((Unit *unit, int dir));
static void attempt_to_capture_unit PARAMS ((Unit *atker, Unit *other));
static void capture_unit_2 PARAMS ((Unit *unit, Unit *pris,
				    Side *prevside));
static void capture_occupant PARAMS ((Unit *unit, Unit *pris, Unit *occ, Side *newside));
static void detonate_on_cell PARAMS ((int x, int y));
static void notify_combat PARAMS ((Unit *unit, Unit *atker, char *str));

#undef  DEF_ACTION
#define DEF_ACTION(name,code,args,prepfn,DOFN,checkfn,ARGDECL,doc)  \
  extern int DOFN PARAMS (ARGDECL);

#include "action.def"

int max_u_detonate_effect_range = -1;

int max_t_detonate_effect_range = -1;

int max_detonate_on_approach_range = -1;

/* Remember what the main units involved are, so display is handled relative
   to them and not to any occupants. */

static Unit *amain, *omain;


int numsoundplays;

/* Attack action. */

/* This is an attack on a given unit at a given level of commitment. */

int
prep_attack_action(unit, unit2, defender, n)
Unit *unit, *unit2, *defender;
int n;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_ATTACK;
    unit->act->nextaction.args[0] = defender->id;
    unit->act->nextaction.args[1] = n;
    unit->act->nextaction.actee = unit2->id;
    return TRUE;
}

int
do_attack_action(unit, unit2, defender, n)
Unit *unit, *unit2, *defender;
int n;
{
    int u2 = unit2->type, u3 = defender->type;
    int withdrawchance, surrenderchance;

    action_point(unit2->side, defender->x, defender->y);
    action_point(defender->side, defender->x, defender->y);
    /* Defender might be a type that can sneak away to avoid attack. */
    withdrawchance = uu_withdraw_per_attack(u2, u3);
    if (withdrawchance > 0) {
	if (probability(withdrawchance)) {
	    if (retreat_unit(defender, unit2)) {
    		if (alive(unit))
    		  use_up_acp(unit, uu_acp_to_attack(u2, u3));
		return A_ANY_DONE;
	    }
	}
    }
    /* Defender might instead choose to surrender right off. */
    surrenderchance = uu_surrender_per_attack(u2, u3);
    if (surrenderchance > 0) {
	if (probability(surrenderchance)) {
	    capture_unit(unit2, defender);
	    if (alive(unit))
	      use_up_acp(unit, uu_acp_to_attack(u2, u3));
	    return A_ANY_DONE;
	}
    }
    /* Carry out a normal attack. */
    one_attack(unit, defender);
    if (alive(unit))
      use_up_acp(unit, uu_acp_to_attack(u2, u3));
    /* The defender in an attack has to take time to defend itself. */
    if (alive(defender)) 
      use_up_acp(defender, uu_acp_to_defend(u2, u3));
    return A_ANY_DONE;
}

int
check_attack_action(unit, unit2, defender, n)
Unit *unit, *unit2, *defender;
int n;
{
    int u, u2, u3, acp, u2x, u2y, dfx, dfy, dist, m;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!in_play(defender))
      return A_ANY_ERROR;
    /* We can't attack ourselves. */
    if (unit2 == defender)
      return A_ANY_ERROR;
    if (unit2->side != NULL && unit2->side == defender->side)
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    u3 = defender->type;
    acp = uu_acp_to_attack(u2, u3);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    /* Check whether we can attack from inside a transport. */
    if (unit2->transport && uu_occ_combat(u2, unit2->transport->type) == 0)
      return A_ANY_ERROR;
    u2x = unit2->x;  u2y = unit2->y;
    dfx = defender->x;  dfy = defender->y;
    dist = distance(u2x, u2y, dfx, dfy);
    if (dist < uu_attack_range_min(u2, u3))
      return A_ANY_TOO_NEAR;
    if (dist > uu_attack_range(u2, u3))
      return A_ANY_TOO_FAR;
    if (uu_hit(u2, u3) <= 0)
      return A_ANY_ERROR;
    /* We have to have a minimum level of supply to be able to attack. */
    for_all_material_types(m) {
	if (unit2->supply[m] < um_to_fight(u2, m))
	  return A_ANY_NO_MATERIAL;
    }
    /* (should prorate ammo needs by intensity of attack) */
    if (!enough_ammo(unit2, defender))
      return A_ANY_ERROR;
    /* Allow attacks even if zero damage, this amounts to "harassment". */
    return A_ANY_OK;
}

/* Overrun action. */

/* Overrun is an attempt to occupy a given cell that may include attempts
   to attack and/or capture any units in the way. */

int
prep_overrun_action(unit, unit2, x, y, z, n)
Unit *unit, *unit2;
int x, y, z, n;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_OVERRUN;
    unit->act->nextaction.args[0] = x;
    unit->act->nextaction.args[1] = y;
    unit->act->nextaction.args[2] = z;
    unit->act->nextaction.args[3] = n;
    unit->act->nextaction.actee = unit2->id;
    return TRUE;
}

int
do_overrun_action(unit, unit2, x, y, z, n)
Unit *unit, *unit2;
int x, y, z, n;
{
    int u, u2, u3, acpused, mpcost, acpcost, speed, ox, oy, oz;
    Unit *defender;

    u = unit->type;  u2 = unit2->type;
    ox = unit2->x;  oy = unit2->y;  oz = unit2->z;
    action_point(unit2->side, x, y);
    acpused = 0;
    /* Attack every defender in turn. */
    for_all_stack(x, y, defender) {
	u3 = defender->type;
	/* Don't attack any of our buddies. */
	if (unit_trusts_unit(unit2, defender))
	  continue;
	action_point(defender->side, x, y);
	one_attack(unit2, defender);
	if (alive(unit))
	  use_up_acp(unit, uu_acp_to_attack(u2, u3));
	acpused += uu_acp_to_attack(u2, u3);
	/* The target of an attack has to take time to defend itself. */
	if (alive(defender)) {
	    use_up_acp(defender, uu_acp_to_defend(u, u3));
	}
    }
    if (!alive(unit2))
      return A_OVERRUN_FAILED;
    if (in_blocking_zoc(unit2, x, y, z))
      return A_OVERRUN_FAILED;
    /* Try to enter the cleared cell now - might still have
       friendlies filling it up already, so check first. */
    if (can_occupy_cell(unit2, x, y)) {
	mpcost = move_unit(unit2, x, y);
	/* Note that we'll say the action succeeded even if
	   the cell did not have enough room for us to actually
	   be in it, which is a little weird. */
	/* Now add up any extra movement costs of entering the new cell. */
	mpcost += zoc_move_cost(unit2, ox, oy, oz);
	acpcost = 0;
	speed = unit_speed(unit2, x, y);
        if (speed > 0)
	  acpcost = (mpcost * 100) / speed;
	acpcost -= acpused;
	if (acpcost > 0) {
	    /* Take the movement cost out of the moving unit if possible. */
	    use_up_acp(unit2, acpcost);
	}
	/* Count the unit as having actually moved. */
	if (unit2->act)
	  ++(unit2->act->actualmoves);
    }
    return A_OVERRUN_SUCCEEDED;
}

int
check_overrun_action(unit, unit2, x, y, z, n)
Unit *unit, *unit2;
int x, y, z, n;
{
    int u, u2, u2x, u2y, u2z, u3, totcost, speed, mpavail, m;
    Unit *defender;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!inside_area(x, y))
      return A_ANY_ERROR;
    if (n == 0)
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    /* (should think about this some more - overrunning into a cell with no
        resistance should work perhaps, even for noncombat units) */
    if (!type_can_attack(u))
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, 1))
      return A_ANY_NO_ACP;
    /* Check whether we can attack from inside a transport. */
    /* (although this might be legit if unit is assumed to leave transport first) */
    if (unit2->transport && uu_occ_combat(u2, unit2->transport->type) == 0)
      return A_ANY_ERROR;
    u2x = unit2->x;  u2y = unit2->y;  u2z = unit2->z;
    /* We have to be in the same cell or an adjacent one. */
    if (!between(0, distance(u2x, u2y, x, y), 1))
      return A_ANY_TOO_FAR;
    /* Now start looking at the move costs. */
    u3 = (unit2->transport ? unit2->transport->type : NONUTYPE);
    totcost = total_move_cost(u2, u3, u2x, u2y, u2z, x, y, u2z);
    speed = unit_speed(unit2, x, y);
    mpavail = (unit->act->acp * speed) / 100;
    /* Zero mp always disallows movement, unless intra-cell. */
    if (mpavail <= 0 && !(u2x == x && u2y == y && u2z == u2z))
      return A_MOVE_NO_MP;
    /* The free mp might get us enough moves, so add it before comparing. */
    if (mpavail + u_free_mp(u2) < totcost)
      return A_MOVE_NO_MP;
    /* We have to have a minimum level of supply to be able to attack. */
    for_all_material_types(m) {
	if (unit2->supply[m] < um_to_fight(u2, m))
	  return A_ANY_NO_MATERIAL;
    }
    for_all_stack(x, y, defender) {
	/* (should test if units here can be attacked en masse) */
    	/* (should prorate ammo needs by intensity of overrun) */
    	if (!enough_ammo(unit2, defender))
    	  return A_ANY_NO_MATERIAL;
    }
    return A_ANY_OK;
}

/* Return true if the attacker defeated the defender, and can therefore
   try to move into the defender's old position. */

static int
one_attack(atker, defender)
Unit *atker, *defender;
{
    int ax = atker->x, ay = atker->y, ox = defender->x, oy = defender->y;
    int counter;
    Side *as = atker->side, *os = defender->side;

    amain = atker;  omain = defender;
    attack_unit(atker, defender);
    /* Do a counterattack if appropriate. */
    counter = uu_counterattack(atker->type, defender->type);
    if (counter > 0) {
	/* (should use value to set strength/commitment of counterattack) */
	attack_unit(defender, atker);
    }
    reckon_damage();
    see_exact(as, ax, ay);
    see_exact(as, ox, oy);
    see_exact(os, ax, ay);
    see_exact(os, ox, oy);
    update_cell_display(as, ax, ay, TRUE);
    update_cell_display(as, ox, oy, TRUE);
    update_cell_display(os, ax, ay, TRUE);
    update_cell_display(os, ox, oy, TRUE);
    all_see_cell(ax, ay);
    all_see_cell(ox, oy);
    attempt_to_capture_unit(atker, defender);
    /* If the defender was not captured, it might turn the tables! */
    /* (Note that we cannot cache the attacker's and defender's types,
       because the type might have changed due to damage, and we want
       to know if the new type might countercapture.) */
    if (alive(defender)
	&& alive(atker)
	&& defender->side == os
	&& uu_countercapture(atker->type, defender->type) > 0) {
	attempt_to_capture_unit(defender, atker);
    }
    return (alive(atker) && unit_at(ox, oy) == NULL);
}

/* Fire-at action. */

/* Shooting at a given unit. */

int
prep_fire_at_action(unit, unit2, defender, m)
Unit *unit, *unit2, *defender;
int m;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_FIRE_AT;
    unit->act->nextaction.args[0] = defender->id;
    unit->act->nextaction.args[1] = m;
    unit->act->nextaction.actee = unit2->id;
    return TRUE;
}

int
do_fire_at_action(unit, unit2, defender, m)
Unit *unit, *unit2, *defender;
int m;
{
    int ux = unit->x, uy = unit->y, ox, oy, oz;

    action_point(unit2->side, defender->x, defender->y);
    action_point(defender->side, defender->x, defender->y);
    update_fire_at_display(unit2->side, unit, defender, m, TRUE);
    update_fire_at_display(defender->side, unit, defender, m, TRUE);
    ox = defender->x;  oy = defender->y;  oz = defender->z;
    amain = unit;  omain = defender;
    fire_on_unit(unit, defender);
    reckon_damage();
    if (alive(unit))
      use_up_acp(unit, u_acp_to_fire(unit2->type));
    /*	if (alive(defender)) use_up_acp(defender, 1); */
    /* Each side sees what happened to its own unit. */
    update_unit_display(unit2->side, unit2, TRUE);
    if (unit != unit2)
      update_unit_display(unit->side, unit, TRUE);
    update_unit_display(defender->side, defender, TRUE);
    /* The attacking side also sees the remote cell. */
    update_cell_display(unit2->side, ox, oy, TRUE);
    update_cell_display(defender->side, ox, oy, TRUE);
    /* Victim might see something in attacker's cell. */
    update_cell_display(defender->side, ux, uy, TRUE);
    /* Actually, everybody might be seeing the combat. */
    all_see_cell(ux, uy);
    all_see_cell(ox, oy);
    /* Always expend the ammo (but only if m is a valid mtype). */
    return A_ANY_DONE;
}

/* Test a fire action for plausibility. */

int
check_fire_at_action(unit, unit2, unit3, m)
Unit *unit, *unit2, *unit3;
int m;
{
    int u, u2, u3, ux, uy, uz, acp, dist, m2;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!in_play(unit3))
      return A_ANY_ERROR;
    /* We can't attack ourselves. */
    if (unit2 == unit3)
      return A_ANY_ERROR;
    u = unit->type; u2 = unit2->type;  u3 = unit3->type;
    ux = unit->x;  uy = unit->y;  uz = unit->z;
    acp = u_acp_to_fire(u2);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    /* Check whether we can attack from inside a transport. */
    if (unit2->transport && uu_occ_combat(u2, unit2->transport->type) == 0)
      return A_ANY_ERROR;
    /* Check that target is in range. */
    dist = distance(ux, uy, unit3->x, unit3->y);
    if (dist > u_range(u2))
      return A_ANY_TOO_FAR;
    if (dist < u_range_min(u2))
      return A_ANY_TOO_NEAR;
    if (unit2->side != NULL && unit2->side == unit3->side)
      return A_ANY_ERROR;
    /* Check intervening elevations. */
    if (found_blocking_elevation(u2, ux, uy, uz, u3, unit3->x, unit3->y, unit3->z))
      return A_ANY_ERROR;
    /* We have to have a minimum level of supply to be able to attack. */
    for_all_material_types(m2) {
	if (unit2->supply[m2] < um_to_fight(u2, m2))
	  return A_ANY_NO_MATERIAL;
    }
    /* Check for enough of right kind of ammo. */
    if (is_material_type(m)) {
    	if (unit->supply[m] == 0)
    	  return A_ANY_NO_MATERIAL;
    } else {
    	if (!enough_ammo(unit2, unit3))
    	  return A_ANY_NO_MATERIAL;
    }
    return A_ANY_OK;
}

/* Fire-into action. */

/* Shooting at a given location. */

int
prep_fire_into_action(unit, unit2, x, y, z, m)
Unit *unit, *unit2;
int x, y, z, m;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_FIRE_INTO;
    unit->act->nextaction.args[0] = x;
    unit->act->nextaction.args[1] = y;
    unit->act->nextaction.args[2] = z;
    unit->act->nextaction.args[3] = m;
    unit->act->nextaction.actee = unit2->id;
    return TRUE;
}

/* One can always shoot, if the cell is visible, but there might not
   not be anything to hit!  No counterattacks when shooting, and the
   results might not be visible to the shooter. */

int
do_fire_into_action(unit, unit2, x, y, z, m)
Unit *unit, *unit2;
int x, y, z, m;
{
    int ux = unit->x, uy = unit->y, ox, oy, oz;
    SideMask sidemask;
    Unit *other;
    Side *side;

    /* Show the firing unit doing its attack. */
    update_fire_into_display(unit->side, unit2, x, y, z, m, TRUE);
    /* Make up the list of sides that will see the incoming fire. */
    sidemask = NOSIDES;
    for_all_stack(x, y, other) {
	if (other->side)
	  sidemask = add_side_to_set(other->side, sidemask);
    }
    for_all_sides(side) {
	if (side_in_set(side, sidemask))
    	  update_fire_into_display(side, unit2, x, y, z, m, TRUE);
    }
    /* If any units at target, hit them. */
    for_all_stack(x, y, other) {
	ox = other->x;  oy = other->y;  oz = other->z;
	amain = unit;  omain = other;
	fire_on_unit(unit2, other);
	reckon_damage();
	/* Each side sees what happened to its unit that is being hit. */
	update_unit_display(other->side, other, TRUE);
	/* The attacking side also sees the remote cell. */
	update_cell_display(unit->side, ox, oy, TRUE);
	update_cell_display(other->side, ox, oy, TRUE);
	/* Victim might see something in attacker's cell. */
	update_cell_display(other->side, ux, uy, TRUE);
	/* Actually, everybody might be seeing the combat. */
	all_see_cell(ux, uy);
	all_see_cell(ox, oy);
	/* don't take moves though! */
    }
    /* Firing side gets just one update. */
    update_unit_display(unit2->side, unit2, TRUE);
    if (unit != unit2)
      update_unit_display(unit->side, unit, TRUE);
    if (alive(unit))
      use_up_acp(unit, u_acp_to_fire(unit2->type));
    /* Always expend the ammo (but only if m is a valid material). */
    /* We're always "successful", even though the bombardment may have
       had little or no actual effect. */
    return A_ANY_DONE;
}

/* Test a shoot action for plausibility. */

int
check_fire_into_action(unit, unit2, x, y, z, m)
Unit *unit, *unit2;
int x, y, z, m;
{
    int u, u2, u2x, u2y, u2z, acp, dist, m2;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    u2x = unit2->x;  u2y = unit2->y;  u2z = unit2->z;
    /* Check that target location is meaningful. */
    if (!inside_area(x, y))
      return A_FIRE_INTO_OUTSIDE_WORLD;
    u = unit->type;
    u2 = unit2->type;
    acp = u_acp_to_fire(u2);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    /* Check whether we can attack from inside a transport. */
    if (unit2->transport && uu_occ_combat(u2, unit2->transport->type) == 0)
      return A_ANY_ERROR;
    /* Check that target is in range. */
    dist = distance(u2x, u2y, x, y);
    if (dist > u_range(u2))
      return A_ANY_TOO_FAR;
    if (dist < u_range_min(u2))
      return A_ANY_TOO_NEAR;
    /* Check intervening elevations and terrain. */
    if (found_blocking_elevation(u2, u2x, u2y, u2z, NONUTYPE, x, y, z))
      return A_ANY_ERROR;
    /* We have to have a minimum level of supply to be able to attack. */
    for_all_material_types(m2) {
	if (unit2->supply[m2] < um_to_fight(u2, m2))
	  return A_ANY_NO_MATERIAL;
    }
    /* Check for enough of right kind of ammo. */
    if (is_material_type(m)) {
    	if (unit->supply[m] == 0)
    	  return A_ANY_NO_MATERIAL;
    } else {
    	/* should just assume amount is appropriate? */
    }
    return A_ANY_OK;
}

int
found_blocking_elevation(u, ux, uy, uz, u2, u2x, u2y, u2z)
int u, ux, uy, uz, u2, u2x, u2y, u2z;
{
/*    int maxrise = u_elev_at_max_range(u); */

    if (world_is_flat())
      return FALSE;
    /* Adjacent cells can't be screened by elevation. */
    /* (should accommodate possibility that target is at top of
       cliff in adj and back away from its edge, thus screened) */
    if (distance(ux, uy, u2x, u2y) <= 1)
      return FALSE;
    /* (should add scan of path) */
    return FALSE;
}
 
static void
fire_on_unit(atker, other)
Unit *atker, *other;
{
    int m, dist = distance(atker->x, atker->y, other->x, other->y);

    if (alive(atker) && alive(other)) {
	if (enough_ammo(atker, other)) {
	    maybe_hit_unit(atker, other, TRUE, (dist > u_hit_falloff_range(atker->type)));
	    for_all_material_types(m) {
		if (um_hit_by(other->type, m) > 0) {
		    atker->supply[m] -= um_consumption_per_attack(atker->type, m);
		}
	    }
	    /* The *victim* can lose acp. */
	    use_up_acp(other, uu_acp_to_be_fired_on(other->type, atker->type));
	}
    }
    /* (should ping victim to see if it wants to respond to the attack?) */
}

/* Test to see if enough ammo is available to make the attack.
   Need enough of *all* types - semi-bogus but too complicated otherwise? */

int
enough_ammo(unit, other)
Unit *unit, *other;
{
    int m;

    for_all_material_types(m) {
	if (um_hit_by(other->type, m) > 0 &&
	    unit->supply[m] < um_consumption_per_attack(unit->type, m))
	  return FALSE;
    }
    return TRUE;
}

/* Single attack, no counterattack.  Check and use ammo - usage independent
   of outcome, but types used depend on unit types involved. */

static void
attack_unit(atker, other)
Unit *atker, *other;
{
    int m;

    if (alive(atker) && alive(other)) {
	if (enough_ammo(atker, other)) {
	    maybe_hit_unit(atker, other, FALSE, FALSE);
	    for_all_material_types(m) {
		if (um_hit_by(other->type, m) > 0) {
		    atker->supply[m] -= um_consumption_per_attack(atker->type, m);
		}
	    }
	}
    }
    /* (should ping victim to see if it wants to respond to the attack?) */
}

/* Make a single hit and maybe hit some passengers also.  Power of hit
   is constant, but chance is affected by terrain, quality,
   and occupants' protective abilities.  If a hit is successful, it may
   have consequences on the defender's occupants, but limited by the
   protection that the transport provides. */

static void
maybe_hit_unit(atker, other, fire, fallsoff)
Unit *atker, *other;
int fire, fallsoff;
{
    int chance, t, hit = 0, a = atker->type, o = other->type;
    int cxpeffect, cxpmax, effect, prot;
    int dist, disthit, rangedelta, hitdelta, rangeamt;
    int dmgspec;
    int hitmovietype;
    Unit *occ, *unit2;
    Side *side3;
    
    Dprintf("%s tries to hit %s", unit_desig(atker), unit_desig(other));
    if (fire && uu_fire_hit(a, o) != -1)
      chance = uu_fire_hit(a, o);
    else
      chance = uu_hit(a, o);
    /* Combat experience tends to raise the hit chance, so do that first. */
    cxpmax = u_cxp_max(a);
    if (cxpmax > 0 && atker->cxp > 0) {
	cxpeffect = uu_hit_cxp(a, o);
	if (cxpeffect != 100) {
	    effect = 100 + (atker->cxp * (cxpeffect - 100)) / cxpmax;
	    chance = (chance * effect) / 100;
	}
    }
    /* (should modify due to cxp of defender too) */
    /* Account for terrain effects. */
    t = terrain_at(atker->x, atker->y);
    chance = (chance * ut_attack_terrain_effect(a, t)) / 100;
    t = terrain_at(other->x, other->y);
    chance = (chance * ut_defend_terrain_effect(o, t)) / 100;
    /* Account for protective units nearby. */
    for_all_occupants(other, occ) {
	if (in_play(occ) && completed(occ)) {
	    prot = uu_protection(occ->type, o);
	    if (prot != 100)
	      chance = (chance * prot) / 100;
	}
    }
    if (other->transport
	&& in_play(other->transport)
	&& completed(other->transport)) {
	prot = uu_protection(other->transport->type, o);
	if (prot != 100)
	  chance = (chance * prot) / 100;
    }
    for_all_stack(other->x, other->y, unit2) {
	if (unit2 != other
	    && in_play(unit2)
	    && completed(unit2)
	    && unit2->side == other->side) {
	    prot = uu_stack_protection(unit2->type, o);
	    if (prot != 100)
	      chance = (chance * prot) / 100;
	}
    }
    if (fallsoff) {
	dist = distance(atker->x, atker->y, other->x, other->y);
    	disthit = uu_hit_max_range_effect(a, o);
    	rangedelta = u_range(a) - u_hit_falloff_range(a);
    	rangeamt = dist - u_hit_falloff_range(a);
    	hitdelta = uu_hit(a, o) - disthit;
    	chance = uu_hit(a, o)
	  - ((uu_hit(a, o) - disthit) * rangeamt) / rangedelta;
    }
    Dprintf(", probability of hit is %d%%", chance);
    /* Compute the hit itself. */
    if (probability(chance)) {
	if (fire && uu_fire_damage(a, o) != -1)
    	  dmgspec = uu_fire_damage(a, o);
    	else
    	  dmgspec = uu_damage(a, o);
    	/* Account for attacker's experience. */
	if (cxpmax > 0 && atker->cxp > 0) {
	    cxpeffect = uu_damage_cxp(a, o);
	    if (cxpeffect != 100) {
		effect = 100 + (atker->cxp * (cxpeffect - 100)) / cxpmax;
		dmgspec = multiply_dice(dmgspec, effect);
	    }
	}
	hit = roll_dice(dmgspec);
    }
    if (hit > 0) {
    	Dprintf(", damage will be %d hp", hit);
    } else {
    	Dprintf(", missed");
    }
    /* (should record a raw statistic?) */
    /* Ablation is a chance for occupants or stack to take part of a hit themselves. */
    if (hit > 0) {
	/* (should decide how ablation computed) */
    }
    if (hit > 0) {
	chance = uu_retreat_chance(a, o);
	/* (should adjust chance by morale etc) */
	if (probability(chance)) {
	    if (retreat_unit(other, atker)) {
		notify_combat(other, atker, "%s retreats!");
		hit = 0; /* should only be reduced hit, may still be > 0 */
	    }
	}
    }
    hit_unit(other, hit, atker);
    for_all_sides(side3) {
    	if (active_display(side3)
	    && (all_see_all
    	    	|| (side3 == atker->side || side3 == other->side))) {
	    hitmovietype = ((hit >= other->hp) ? movie_death :
			    ((hit > 0) ? movie_hit : movie_miss));
	    schedule_movie(side3, hitmovietype, other->id);
    	}
    }
    Dprintf("\n");
    /* Recurse into occupants, maybe hit them too.  */
    for_all_occupants(other, occ) {
	if (probability(100 - uu_protection(o, occ->type))) {
	    maybe_hit_unit(atker, occ, fire, fallsoff);
	}
    }
    /* We get combat experience only if there could have been some damage. */
    if (chance > 0) {
    	if (atker->cxp < u_cxp_max(a))
    	  atker->cxp += uu_cxp_per_combat(a, o);
    	if (other->cxp < u_cxp_max(o))
    	  other->cxp += uu_cxp_per_combat(o, a);
    	/* Occupants already gained their experience in the recursive call. */
    }
}

/* Do the hit itself. */

static void
hit_unit(unit, hit, atker)
Unit *unit, *atker;
int hit;
{
    int u = unit->type, hpmin;

    /* Some units might detonate automatically upon being hit. */
    if (hit > 0
        && atker != NULL
        && probability(uu_detonate_on_hit(u, atker->type))
        && !was_detonated(unit)) {
    	detonate_unit(unit, unit->x, unit->y, unit->z);
    	/* If the detonating unit still exists, then continue
    	   on to normal damage computation. */
    }
    /* Record the loss of hp. */
    unit->hp2 -= hit;
    /* Attacker might not be able to do any more damage.  Note that the
       positioning of this code is such that all the usual side effects
       of combat happen, but the victim doesn't get any more worse off
       than it is already. */
    if (atker != NULL) {
	hpmin = uu_hp_min(atker->type, u);
	if (hpmin > 0 && hpmin > unit->hp2) {
	    unit->hp2 = hpmin;
	}
    }
    /* Maybe record for statistical analysis. */
    /* (this is only useful if code always goes through here - is that true?) */
    if (atker != NULL && atker->side != NULL) {
	if (atker->side->atkstats[atker->type] == NULL)
	  atker->side->atkstats[atker->type] = (long *) xmalloc(numutypes * sizeof(long));
	if (atker->side->hitstats[atker->type] == NULL)
	  atker->side->hitstats[atker->type] = (long *) xmalloc(numutypes * sizeof(long));
	++((atker->side->atkstats[atker->type])[u]);
	(atker->side->hitstats[atker->type])[u] += hit;
    }
    /* Some units may detonate automatically just before dying. */
    if (hit > 0
        && unit->hp2 <= 0
        && probability(u_detonate_on_death(u))
        && !was_detonated(unit)) {
	detonate_unit(unit, unit->x, unit->y, unit->z);
    }
}

/* Hits on the main units have to be done later, so that mutual
   destruction works properly.  This function also does all the notifying. */

/* (What if occupants change type when killed, but transport vanishes?) */

void
reckon_damage()
{
    /* Entertain everybody. */
    play_movies(ALLSIDES);
    /* Report the damage in more detail, now before the actual damage
       is taken (which may cause many units to disappear). */
    /* Normally we report the defender and then the attacker's damage,
       but if the defender dies, we report its counterattack results
       first and its death second. */
    if (!(omain->hp2 <= 0 && amain->hp2 > 0)) {
	report_damage(omain, amain, omain);
	report_damage(amain, omain, amain);
    } else {
	report_damage(amain, omain, amain);
	report_damage(omain, amain, omain);
    }
    damage_unit(omain, combat_dmg);
    damage_unit(amain, combat_dmg);
}

static void
reckon_damage_here(x, y)
int x, y;
{
    Unit *unit;

    for_all_stack(x, y, unit) {
	damage_unit(unit, combat_dmg);
    }
}

void
reckon_damage_around(x, y, r)
int x, y, r;
{
    if (r > 0) {
	apply_to_area(x, y, r, reckon_damage_here);
    } else {
	reckon_damage_here(x, y);
    }
}

static void
report_damage(unit, atker, mainunit)
Unit *unit, *atker, *mainunit;
{
    Unit *occ;

    if (unit->hp2 <= 0) {
	if (unit == mainunit) {
	    notify_combat(atker, unit, "%s destroys %s!");
	} else {
	    notify_combat(unit, atker, "  (and destroys occupant %s!)");
	}
    } else if (unit->hp2 < unit->hp) {
	if (unit == mainunit) {
	    notify_combat(atker, unit, "%s hits %s!");
	} else {
	    notify_combat(unit, atker, "  (and hits occupant %s!)");
	}
    } else {
	/* messages about misses not too useful */
    }
    for_all_occupants(unit, occ) {
	report_damage(occ, atker, mainunit);
    }
}

/* Make the intended damage become real, and do any consequences. */

void
damage_unit(unit, dmgreason)
Unit *unit;
enum damage_reasons dmgreason;
{
    int newacp;
    HistEventType hevttype;
    Obj *dameff;
    Unit *occ;

    /* Process all the occupants first. */
    for_all_occupants(unit, occ) {
	damage_unit(occ, dmgreason);
    }
    /* If no damage was recorded, just return. */
    if (unit->hp2 == unit->hp)
      return;
    /* If unit is to die, do the consequences. */
    if (unit->hp2 <= 0) {
	if (u_wrecked_type(unit->type) == NONUTYPE) {
	    /* (should let occupants escape now?) */
	    hevttype = (dmgreason == combat_dmg ? H_UNIT_KILLED :
			(dmgreason == accident_dmg ? H_UNIT_DIED_IN_ACCIDENT :
			 0));
	    kill_unit(unit, hevttype);
	} else {
	    hevttype = (dmgreason == combat_dmg ? H_UNIT_WRECKED :
			(dmgreason == accident_dmg ? H_UNIT_WRECKED_IN_ACCIDENT :
			 0));
	    change_unit_type(unit, u_wrecked_type(unit->type), hevttype);
	    /* Restore to default hp for the new type. */
	    unit->hp = unit->hp2 = u_hp(unit->type);
	    /* Get rid of occupants if overfull. */
	    eject_excess_occupants(unit);
	    /* change_unit_type already reported the wrecking as an event,
	       do we need any additional reportage? */
	}
    } else {
	record_event(H_UNIT_DAMAGED, add_side_to_set(unit->side, NOSIDES),
		     unit->id, unit->hp, unit->hp2);
	/* Change the unit's hp. */
	unit->hp = unit->hp2;
	/* Perhaps adjust the acp down. */
	if (unit->act != NULL
	    && unit->act->acp > 0
	    && (dameff = u_acp_damage_effect(unit->type)) != lispnil) {
	    newacp = damaged_acp(unit, dameff);
	    /* The damaged acp limits the remaining acp, rather than trying
	       to do some sort of proportional adjustment, which would be
	       hard to get right. */
	    /* (should account for occupant effects on acp) */
	    unit->act->acp = min(unit->act->acp, newacp);
	}
    }
    /* Clear any detonation flag that might have been set. */
    if (alive(unit))
      set_was_detonated(unit, FALSE);
    /* Let the unit's owner know about all this. */
    update_unit_display(unit->side, unit, TRUE);
}

/* Retreat is a special kind of movement that a unit uses to avoid
   damage during combat. It bypasses some of the normal move rules. */

static int
retreat_unit(unit, atker)
Unit *unit, *atker;
{
    int dir;
    extern int retreating_from;

    retreating_from = atker->type;
    if (unit->x == atker->x && unit->y == atker->y) {
    	dir = random_dir();
    } else {
    	dir = approx_dir(unit->x - atker->x, unit->y - atker->y);
    }
    if (retreat_in_dir(unit, dir))
      return TRUE;
    if (flip_coin()) {
    	if (retreat_in_dir(unit, left_dir(dir)))
    	  return TRUE;
    	if (retreat_in_dir(unit, right_dir(dir)))
    	  return TRUE;
    } else {
    	if (retreat_in_dir(unit, right_dir(dir)))
    	  return TRUE;
    	if (retreat_in_dir(unit, left_dir(dir)))
    	  return TRUE;
    }
    retreating_from = NONUTYPE;
    return FALSE;
}

static int
retreat_in_dir(unit, dir)
Unit *unit;
int dir;
{
    int nx, ny, rslt;
    extern int retreating;
    extern int retreating_from;

    /* (should it be possible for a unit to retreat out of the world?) */
    if (!interior_point_in_dir(unit->x, unit->y, dir, &nx, &ny))
      return FALSE;
    retreating = TRUE;
    rslt = check_move_action(unit, unit, nx, ny, unit->z);
    if (!valid(rslt))
      return FALSE;
    do_move_action(unit, unit, nx, ny, unit->z);
    retreating = FALSE;
    retreating_from = NONUTYPE;
    return TRUE;
}

/* Capture action. */

/* Prepare a capture action to be executed later. */

int
prep_capture_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_CAPTURE;
    unit->act->nextaction.args[0] = unit3->id;
    unit->act->nextaction.actee = unit2->id;
    return TRUE;
}

/* Execute a capture action. */

int
do_capture_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    int rslt;

    attempt_to_capture_unit(unit2, unit3);
    use_up_acp(unit, uu_acp_to_capture(unit2->type, unit3->type));
    if (unit3->side == unit2->side)
      rslt = A_CAPTURE_SUCCEEDED;
    else
      rslt = A_CAPTURE_FAILED;
    return rslt;
}

/* Check the validity of a capture action. */

int
check_capture_action(unit, unit2, unit3)
Unit *unit, *unit2, *unit3;
{
    int u, u2, u3, acp, m;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!in_play(unit3))
      return A_ANY_ERROR;
    /* We can't capture ourselves. */
    if (unit2 == unit3)
      return A_ANY_ERROR;
    /* We can't capture units on our side. */
    if (unit2->side == unit3->side)
      return A_ANY_ERROR;    
    u = unit->type;
    u2 = unit2->type;
    u3 = unit3->type;
    acp = uu_acp_to_capture(u2, u3);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    if (capture_chance(u2, u3, unit3->side) == 0)
      return A_ANY_CANNOT_DO;
    if (distance(unit2->x, unit2->y, unit3->x, unit3->y) > 1)
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    /* We have to have a minimum level of supply to be able to capture. */
    for_all_material_types(m) {
	if (unit2->supply[m] < um_to_fight(u2, m))
	  return A_ANY_NO_MATERIAL;
    }
    return A_ANY_OK;
}

/* Handle capture possibility and repulse/slaughter. */

/* The chance to capture an enemy is modified by several factors.
   Neutrals have a different chance to be captured, and presence of
   occupants should also has an effect.  Can't capture anything that is
   on a kind of terrain that the capturer can't go on, unless victim has
   "bridge effect". */

/* (Need a little better treatment of committed assaults, where lack of
   success == death.) */

static void
attempt_to_capture_unit(atker, other)
Unit *atker, *other;
{
    int a = atker->type, o = other->type, chance, prot;
    int ox = other->x, oy = other->y;
    Unit *occ;
    Side *as = atker->side, *os = other->side;
    
    chance = capture_chance(a, o, other->side);
    if (alive(atker) && alive(other) && chance > 0) {
	if (impassable(atker, ox, oy) && !uu_bridge(o, a))
	  return;
	/* Can possibly detonate on *any* attempt to capture! */
	if (probability(uu_detonate_on_capture(o, a))
	    && !was_detonated(other)) {
	    detonate_unit(other, other->x, other->y, other->z);
	    /* Might not be possible to capture anything anymore. */
	    if (!alive(atker) || !alive(other))
	      return;
	    /* Types of units might have changed, recalc things. */
	    a = atker->type;  o = other->type;
	    as = atker->side;  os = other->side;
	    chance = capture_chance(a, o, other->side);
	}
	/* Occupants can protect the transport. */
	for_all_occupants(other, occ) {
	    if (is_active(occ)) {
	    	prot = uu_protection(occ->type, o);
		chance = (chance * prot) / 100;
	    }
	}
	/* Test whether the capture actually happens. */
	if (probability(chance)) {
	    capture_unit(atker, other);
	} else if (atker->transport != NULL && 
		   (impassable(atker, ox, oy) ||
		    impassable(atker, atker->x, atker->y))) {
	    /* was the capture attempt a one-way trip? */
	    /* (should fix the test above - needs to be more accurate) */
	    notify_combat(other, atker, "%s resists capture; %s slaughtered!");
	    kill_unit(atker, H_UNIT_KILLED /* should be something appropriate */);
	} else {
	    notify_combat(other, atker, "%s throws back %s!");
	    /* (should record failed attempt to capture?) */
	}
	if (chance > 0) {
	    if (atker->cxp < u_cxp_max(a))
	      atker->cxp += uu_cxp_per_capture(a, o);
	    /* (should not increment if side just changed?) */
	    if (other->cxp < u_cxp_max(o))
	      other->cxp += uu_cxp_per_capture(o, a);
	}
    }
}

int
capture_chance(u, u2, side2)
int u, u2;
Side *side2;
{
    int chance, indepchance;

    chance = uu_capture(u, u2);
    if (side2 != NULL)
      return chance;
    indepchance = uu_indep_capture(u, u2);
    return (indepchance < 0 ? chance : indepchance);
}

/* There are many consequences of a unit being captured. */

void
capture_unit(unit, pris)
Unit *unit, *pris;
{
    int u = unit->type, px = pris->x, py = pris->y;
    Unit *occ;
    Side *ps = pris->side, *us = unit->side, *newside;

    newside = unit->side;
    /* Return a unit to its original side if we are buds with that side. */
    if (pris->origside != newside && trusted_side(us, pris->origside))
      newside = pris->origside;
    if (probability(uu_scuttle(pris->type, unit->type))) {
	/* (should add terrain effect on success too) */
	/* (should characterize as a scuttle) */
	kill_unit(pris, H_UNIT_DISBANDED);
    }
    if (!unit_allowed_on_side(pris, newside)) {
	kill_unit(pris, H_UNIT_KILLED);
    }
    if (alive(pris)) {
	if (newside == pris->origside)
	  notify_combat(unit, pris, "%s liberates %s!");
	else
	  notify_combat(unit, pris, "%s captures %s!");
	/* Decide the fate of each occupant of our prisoner. */
	for_all_occupants(pris, occ) {
	    capture_occupant(unit, pris, occ, newside);
	}
	/* The change of side itself.  This happens recursively to any
	   remaining occupants as well. */
	change_unit_side(pris, newside, H_UNIT_CAPTURED, unit);
	/* Garrison the newly-captured unit with hp from the capturing unit. */
	garrison_unit(unit, pris);
	capture_unit_2(unit, pris, ps);
	/* The people at the new location may change sides immediately. */
	if (people_sides_defined()
	    && any_people_side_changes
	    && probability(people_surrender_chance(pris->type, px, py))) {
	    change_people_side_around(px, py, pris->type, unit->side);
	}
    }
    /* Update everybody's view of the situation. */
    see_exact(ps, px, py);
    update_cell_display(ps, px, py, TRUE);
    all_see_cell(px, py);
}

/* Given that the main unit is going to be captured, decide what each occupant
   will do. */

static void
capture_occupant(unit, pris, occ, newside)
Unit *unit, *pris, *occ;
Side *newside;
{
    int u = unit->type;
    Unit *subocc;

    if (probability(uu_occ_escape(u, occ->type))) {
	/* The occupant escapes, along with all its suboccupants. */
	/* (should impl by moving to nearby cells?) */
    } else if (0 /* (should allow for occ scuttling also) */) {
    } else if (/* u_change_side(occ->type) || */ capture_chance(u, occ->type, occ->side) > 0) {
	/* Side change will actually happen later. */
	for_all_occupants(occ, subocc) {
	    capture_occupant(unit, occ, subocc, newside);
	}
    } else {
	/* Occupant can't live as a prisoner, but suboccs might. */
	for_all_occupants(occ, subocc) {
	    capture_occupant(unit, occ, subocc, newside);
	}
	/* Any suboccupants that didn't escape will die. */
	/* (what if subocc captured tho? should move elsewhere) */
	kill_unit(occ, H_UNIT_KILLED);
    }
}

static void
capture_unit_2(unit, pris, prevside)
Unit *unit, *pris;
Side *prevside;
{
    Unit *occ;

    /* Our new unit's experience might be higher or lower, depending on what
       capture really means (change of crew perhaps). */
    pris->cxp = (pris->cxp * u_cxp_on_capture(pris->type)) / 100;
    pris->cxp = min(unit->cxp, u_cxp_max(pris->type));
    /* Clear any actions and plans. */
    /* Don't use init_unit_actorstate!  We just want to cancel any pending
       action, but leave all the acp values untouched. */
    if (pris->act)
      pris->act->nextaction.type = ACTION_NONE;
    /* (should probably adjust side's acp sums by new unit's amounts) */
    init_unit_plan(pris);
    /* Assign a number to this unit if it should have one. */
    assign_unit_number(pris);
    /* Likewise for occupants. */
    for_all_occupants(pris, occ) {
	capture_unit_2(unit, occ, prevside);
    }
}

/* A detonate action just blasts the vicinity. */

int
prep_detonate_action(unit, unit2, x, y, z)
Unit *unit, *unit2;
int x, y, z;
{
    if (unit == NULL || unit->act == NULL)
      return FALSE;
    if (unit2 == NULL)
      return FALSE;
    unit->act->nextaction.type = ACTION_DETONATE;
    unit->act->nextaction.args[0] = x;
    unit->act->nextaction.args[1] = y;
    unit->act->nextaction.args[2] = z;
    unit->act->nextaction.actee = unit2->id;
    return TRUE;
}

int
do_detonate_action(unit, unit2, x, y, z)
Unit *unit, *unit2;
int x, y, z;
{
    int u2 = unit2->type;

    detonate_unit(unit2, x, y, z);
    /* Note that if the maxrange is further than the actual range of this
       detonation, only just-damaged units will be looked at. */
    reckon_damage_around(x, y, max_u_detonate_effect_range);
    /* Unit might have detonated outside its range of effect, so need this to make
       its own damage is accounted for. */
    if (alive(unit2))
      reckon_damage_around(unit2->x, unit2->y, 0);
    use_up_acp(unit, u_acp_to_detonate(u2));
    return A_ANY_DONE;
}

int
check_detonate_action(unit, unit2, x, y, z)
Unit *unit, *unit2;
int x, y, z;
{
    int u, u2, acp;

    if (!in_play(unit))
      return A_ANY_ERROR;
    if (!in_play(unit2))
      return A_ANY_ERROR;
    if (!inside_area(x, y))
      return A_ANY_ERROR;
    u = unit->type;
    u2 = unit2->type;
    /* The unit must actually be able to detonate. */
    acp = u_acp_to_detonate(u2);
    if (acp < 1)
      return A_ANY_CANNOT_DO;
    /* Can only detonate in our own or an adjacent cell. */
    /* (In other words, the detonating unit doesn't get to teleport
       its detonation effects to any desired faraway location.) */
    if (distance(unit2->x, unit2->y, x, y) > 1)
      return A_ANY_ERROR;
    if (!has_enough_acp(unit, acp))
      return A_ANY_NO_ACP;
    return A_ANY_OK;
}

static int tmpdetx, tmpdety;

/* Actual detonation may occur by explicit action or automatically; this
   routine makes the detonation effects happen, pyrotechnics and all. */

int
detonate_unit(unit, x, y, z)
Unit *unit;
int x, y, z;
{
    int u = unit->type, dir, x1, y1, dmg, t, maxrange;
    Unit *unit2;

    if (max_u_detonate_effect_range < 0) {
	int u2, u3, range;

	for_all_unit_types(u2) {
	    for_all_unit_types(u3) {
		range = uu_detonation_range(u2, u3);
		max_u_detonate_effect_range =
		  max(range, max_u_detonate_effect_range);
	    }
	}
    }
    if (max_t_detonate_effect_range < 0) {
	int u2, t, range;

	for_all_unit_types(u2) {
	    for_all_terrain_types(t) {
		range = ut_detonation_range(u2, t);
		max_t_detonate_effect_range =
		  max(range, max_t_detonate_effect_range);
	    }
	}
    }
    notify_combat(unit, unit, "%s detonates!");
    set_was_detonated(unit, TRUE);
    /* Hit the detonating unit first. */
    hit_unit(unit, u_hp_per_detonation(u), NULL);
    /* Hit units at ground zero. */
    for_all_stack(x, y, unit2) {
    	if (unit2 != unit) {
	    hit_unit(unit2, uu_detonation_damage_at(u, unit2->type), unit);
	}
    }
    damage_terrain(u, x, y);
    /* Hit units and/or terrain in adjacent cells, if this is defined. */
    if (max_u_detonate_effect_range >= 1) {
        for_all_directions(dir) {
	    if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
		for_all_stack(x1, y1, unit2) {
		    dmg = uu_detonation_damage_adj(u, unit2->type);
		    hit_unit(unit2, dmg, unit);
		}
	    }
	}
    }
    if (max_t_detonate_effect_range >= 1) {
        for_all_directions(dir) {
	    if (point_in_dir(x, y, dir, &x1, &y1)) {
		damage_terrain(u, x1, y1);
	    }
	}
    }
    /* Hit units that are further away. */
    maxrange = max(max_u_detonate_effect_range, max_t_detonate_effect_range);
    if (maxrange >= 2) {
	tmpunit = unit;
	tmpdetx = x;  tmpdety = y;
	apply_to_area(x, y, maxrange, detonate_on_cell);
    }
    /* (should test compatibility of any new terrain types with each other;
        only after changes over with) */
    return TRUE;
}

static void
detonate_on_cell(x, y)
int x, y;
{
    int dist, dmg, sdmg, t;
    Unit *unit2;

    dist = distance(tmpdetx, tmpdety, x, y);
    if (dist > 1 && dist <= max_u_detonate_effect_range) {
	for_all_stack(x, y, unit2) {
	    if (dist <= uu_detonation_range(tmpunit->type, unit2->type)) {
		dmg = uu_detonation_damage_adj(tmpunit->type, unit2->type);
		/* Reduce by inverse square of the distance. */
		sdmg = (dmg * 100) / (dist * dist);
		dmg = prob_fraction(sdmg);
		hit_unit(unit2, dmg, tmpunit);
	    }
	}
    }
    if (dist > 1 && dist <= max_t_detonate_effect_range) {
	damage_terrain(tmpunit->type, x, y);
    }
}

void
damage_terrain(u, x, y)
int u, x, y;
{
    int t, t2, dir, x1, y1;

    /* Damage the cell's terrain. */
    t = terrain_at(x, y);
    if (probability(ut_detonation_damage(u, t))) {
	t2 = damaged_terrain_type(t);
	if (t2 == NONTTYPE) {
	    run_error("bad damaged type?");
	    return;
	} else if (t2 != t) {
	    change_terrain_type(x, y, t2);
	}
    }
    /* Apply to auxiliary terrain also. */
    if (1 /* if any aux terrain */) {
	for_all_terrain_types(t) {
	    switch (t_subtype(t)) {
	      case cellsubtype:
		/* We already did this one. */
	        break;
	      case bordersubtype:
		for_all_directions(dir) {
		    if (1) {
			if (border_at(x, y, dir, t)
			    && probability(ut_detonation_damage(u, t))) {
			    t2 = damaged_terrain_type(t);
			    if (t2 == NONTTYPE) {
				set_border_at(x, y, dir, t, FALSE);
			    } else if (t2 != t) {
				set_border_at(x, y, dir, t, FALSE);
				set_border_at(x, y, dir, t2, TRUE);
				/* There is potentially a problem with some
				   game designs here; if the new type t2 can
				   also be damaged by the detonation, then
				   the loop here will damage it in turn, at
				   least if t2 *follows* t in the list of terrain
				   types.  Preventing this would require a lot of
				   buffering, so it's left as a limitation for now. */
			    }
			}
		    }
		}
	        break;
	      case connectionsubtype:
		for_all_directions(dir) {
		    if (1) {
			if (connection_at(x, y, dir, t)
			    && probability(ut_detonation_damage(u, t))) {
			    t2 = damaged_terrain_type(t);
			    if (t2 == NONTTYPE) {
				set_connection_at(x, y, dir, t, FALSE);
			    } else if (t2 != t) {
				set_connection_at(x, y, dir, t, FALSE);
				set_connection_at(x, y, dir, t2, TRUE);
			    }
			}
		    }
		}
	        break;
	      case coatingsubtype:
		/* don't know how to damage coatings yet */
	        break;
	    }
	}
    }
}

int
damaged_terrain_type(t)
int t;
{
    int t2, tot, othertot, test, sum, rslt;

    tot = othertot = 0;
    for_all_terrain_types(t2) {
	if (t_subtype(t2) == t_subtype(t)) {
	    tot += tt_damaged_type(t, t2);
	} else {
	    othertot += tt_damaged_type(t, t2);
	}
    }
    rslt = NONTTYPE;
    if ((tot + othertot) > 0) {
    	test = xrandom(tot + othertot);
    	sum = 0;
    	for_all_terrain_types(t2) {
	    if (t_subtype(t2) == t_subtype(t)) {
	    	sum += tt_damaged_type(t, t2);
		if (test < sum) {
		    rslt = t2;
		    break;
		}
	    }
    	}
    }
    /* Random values between tot and othertot will have
       fallen through the loop, and the rslt is NONTTYPE,
       which indicates that the terrain must be removed
       if possible. */
    /* Paranoia check */
    if (rslt != NONTTYPE && t_subtype(rslt) != t_subtype(t))
      run_error("badness in damaged_terrain_type");
    return rslt;
}

static void
notify_combat(unit1, unit2, str)
Unit *unit1, *unit2;
char *str;
{
    char buf1[BUFSIZE], buf2[BUFSIZE];
    Side *side3;

    for_all_sides(side3) {
	if (active_display(side3)
	    && (all_see_all || side3 == unit1->side || side3 == unit2->side)) {
	    strcpy(buf1, unit_handle(side3, unit1));
	    strcpy(buf2, unit_handle(side3, unit2));
	    notify(side3, str, buf1, buf2);
	}
    }
}

/* Tests for whether given units or types can do particular types of actions. */

int
can_attack(unit)
Unit *unit;
{
    return type_can_attack(unit->type);
}

int
type_can_attack(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_attack(u, u2) > 0 && uu_hit(u, u2) > 0 && uu_damage(u, u2) > 0)
	  return TRUE;
    }
    return FALSE;
}

int
can_fire(unit)
Unit *unit;
{
    return type_can_fire(unit->type);
}

int
type_can_fire(u)
int u;
{
    int u2, fhit, fdam;
	
    if (u_acp_to_fire(u) == 0)
      return FALSE;
    for_all_unit_types(u2) {
	fhit = uu_fire_hit(u, u2);
	fdam = uu_fire_damage(u, u2);
	if ((fhit == -1 ? (uu_hit(u, u2) > 0) : (fhit > 0))
	     && (fdam == -1 ? (uu_damage(u, u2) > 0) : (fdam > 0)))
	  return TRUE;
    }
    return FALSE;
}

int
type_can_capture(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_acp_to_capture(u, u2) > 0
	    && (uu_capture(u, u2) > 0 || uu_indep_capture(u, u2) > 0))
	  return TRUE;
    }
    return FALSE;
}

int
can_detonate(unit)
Unit *unit;
{
    return (u_acp_to_detonate(unit->type) > 0);
}
