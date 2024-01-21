// "intel.C"

/*    Copyright (C) 1994,1995,1996  Steve Hardt

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Steve Hardt 
    hardts@mit.edu (valid until Nov. 1996)
    hardts@netscape.com
    hardts@alum.mit.edu
    http://graphics.lcs.mit.edu/~hardts/xevil.html
*/

#ifndef NO_PRAGMAS
#pragma implementation "intel.h"
#endif


// Include Files
extern "C" {
  #include <string.h>
}

#include "utils.h"
#include "physical.h"
#include "intel.h"
#include "actual.h"



// Defines
#define TARGET_RANGE 500
#define REFLEXES_TIME 4
#define FIGHT_RANGE 45  // [50-40]
#define FIGHT_RANGE_2 (FIGHT_RANGE * FIGHT_RANGE)
#define WANDER_WIDTH 1000
#define WANDER_HEIGHT 600
#define SHOT_CUTOFF 9
#define LADDER_JUMP_TIME 4
#define FTHROWER_RANGE_2 (90 * 90)
#define HEALTH_ATTACK_PERCENT .1
#define TRANSMOGIFIER_PERCENT .15
#define MASTER_MIN_DIST 100 //50
#define MASTER_MIN_DIST_2 (MASTER_MIN_DIST * MASTER_MIN_DIST)
#define MASTER_MAX_DIST  600  //   350 
#define MASTER_MAX_DIST_2 (MASTER_MAX_DIST * MASTER_MAX_DIST)
// Strategy change times for different strategies.
#define TO_AWAY_TARGET_TIME 30
#define TO_POS_TIME 100
#define TO_MASTER_TIME 100


const IntelOptions Intel::intelOptionsDefault = 
{
  True, // classFriends
  False, // harmless
  False, // psychotic
  False, // ignoreItems
  False, // ignoreLemmings
};



Intel::Intel(WorldP w,LocatorP l,char *name,int lives,
	     const IntelOptions *ops,ITmask opMask)
{
  intelStatusChanged = True;
  living = True;
  world = w;
  locator = l;

  intelOptions = intelOptionsDefault;
  if (opMask & ITharmless)
    intelOptions.harmless = ops->harmless;
  if (opMask & ITclassFriends)
    intelOptions.classFriends = ops->classFriends;
  if (opMask & ITpsychotic)
    intelOptions.psychotic = ops->psychotic;
  if (opMask & ITignoreItems)
    intelOptions.ignoreItems = ops->ignoreItems;
  if (opMask & ITignoreLemmings)
    intelOptions.ignoreLemmings = ops->ignoreLemmings;

  // intelStatus
  strcpy(intelStatus.name,name);
  intelStatus.classId = A_None;
  strcpy(intelStatus.className,"none");
  intelStatus.health = 0;
  intelStatus.mass = 0;
  intelStatus.weaponClassId = A_None;
  strcpy(intelStatus.weapon,"none");
  intelStatus.weaponReady = False;
  intelStatus.ammo = PH_AMMO_UNLIMITED;
  intelStatus.itemClassId = A_None;
  strcpy(intelStatus.item,"none");
  intelStatus.itemCount = 0;
  if (lives != IT_INFINITE_LIVES)
    intelStatus.lives = lives - 1; // One life is used for current life.  
  else 
    intelStatus.lives = IT_INFINITE_LIVES;
  intelStatus.humanKills = 0;
  intelStatus.enemyKills = 0;
  intelStatus.soups = 0;
}



Boolean Intel::is_human()
{
  return False;
}


Boolean Intel::is_enemy()
{
  return False;
}



Boolean Intel::is_lemming_intel()
{
  return False;
}



Boolean Intel::reincarnate_me()
{
  return !living && 
    (intelStatus.lives == IT_INFINITE_LIVES || intelStatus.lives > 0);
}



void Intel::add_human_kill()
{
  intelStatus.humanKills++; 
  intelStatusChanged = True;
}



void Intel::add_enemy_kill()
{
  intelStatus.enemyKills++; 
  intelStatusChanged = True;
}



void Intel::add_soup()
{
  intelStatus.soups++; 
  intelStatusChanged = True;
}



ITcommand Intel::dir_to_command(Dir dir)
{
  assert(dir == CO_air || dir >= CO_R);
  ITcommand ret = IT_CENTER;
  if (dir != CO_air)
    ret = (ITcommand)((dir - CO_R) * 0.5 + IT_R);
  
  return ret;
}



ITcommand Intel::dir_to_command_weapon(Dir dir)
{
  assert(dir == CO_air || dir >= CO_R);
  ITcommand ret = IT_WEAPON_CENTER;
  if (dir != CO_air)
    ret = (ITcommand)((dir - CO_R) * 0.5 + IT_WEAPON_R);
  
  return ret;
}



Dir Intel::command_weapon_to_dir_4(ITcommand command)
{
  switch (command) {
  case IT_WEAPON_R:
    return CO_R;
  case IT_WEAPON_DN:
    return CO_DN;
  case IT_WEAPON_L:
    return CO_L;
  case IT_WEAPON_UP:
    return CO_UP;
  };
  
  return CO_air;
}



Dir Intel::command_weapon_to_dir_8(ITcommand command)
{
  switch (command) {
  case IT_WEAPON_R:
    return CO_R;
  case IT_WEAPON_DN_R:
    return CO_DN_R;
  case IT_WEAPON_DN:
    return CO_DN;
  case IT_WEAPON_DN_L:
    return CO_DN_L;
  case IT_WEAPON_L:
    return CO_L;
  case IT_WEAPON_UP_L:
    return CO_UP_L;
  case IT_WEAPON_UP:
    return CO_UP;
  case IT_WEAPON_UP_R:
    return CO_UP_R;
  };

  return CO_air;
}




ITcommand Intel::center_pos_to_command(const Pos &pos)
{
  if (pos.y > pos.x)
    {
      if (pos.y > -pos.x)
	return IT_DN;
      else 
	return IT_L;
    }
  else
    {
      if (pos.y > -pos.x)
	return IT_R;
      else
	return IT_UP;
    }
}



void Intel::die()
{
  intelStatus.health = -1;  
  intelStatus.classId = A_None;
  strcpy(intelStatus.className,"tormented spirit");
  intelStatus.mass = 0;
  intelStatus.weaponClassId = A_None;
  strcpy(intelStatus.weapon,"none");
  intelStatus.weaponReady = False;
  intelStatus.ammo = PH_AMMO_UNLIMITED;
  intelStatus.itemClassId = A_None;
  strcpy(intelStatus.item,"none");

  intelStatusChanged = True; 

  Id invalid;
  id = invalid;
  living = False;
}



void Intel::reincarnate()
{
  if (intelStatus.lives != IT_INFINITE_LIVES)
    {
      assert (intelStatus.lives > 0);
      intelStatus.lives--;
      intelStatusChanged = True;
    }
  living = True;
}



void Intel::clock(PhysicalP p)
{
  assert(intelStatus.lives == IT_INFINITE_LIVES || intelStatus.lives >= 0);
  assert (p->get_id(id) == PH_NO_SIG);

  // classId, class
  ClassId classId = p->get_class_id();
  if (classId != intelStatus.classId)
    {
      intelStatus.classId = classId;
      strcpy(intelStatus.className,p->get_class_name());
      intelStatusChanged = True;
    }

  // health
  Health health = p->get_health();
  if ((health >= 0) && (health != intelStatus.health))
    {
      intelStatus.health = health;
      intelStatusChanged = True;
    }
  
  // mass
  Mass mass = p->get_mass();
  if (mass != intelStatus.mass)
    {
      intelStatus.mass = mass;
      intelStatusChanged = True;
    }

  // weaponClassId, weapon
  const char *weapon;
  ClassId wClassId = p->get_weapon_string(weapon);
  if (wClassId != intelStatus.weaponClassId) {
    intelStatus.weaponClassId = wClassId;
    strcpy(intelStatus.weapon,(char *)weapon);
    intelStatusChanged = True;
  }

  if (p->is_user())
    {
      PhysicalP user = p;

      // weaponReady
      PhysicalP weapon = user->get_weapon_current();
      Boolean weaponReady = weapon ? ((WeaponP)weapon)->ready() : 
      (p->is_fighter() || (p->is_built_in() && p->ready()));
      if (intelStatus.weaponReady != weaponReady)
	{
	  intelStatus.weaponReady = weaponReady;
	  intelStatusChanged = True;
	}

      // ammo
      int ammo = weapon ? ((WeaponP)weapon)->get_ammo() : PH_AMMO_UNLIMITED;
      if (intelStatus.ammo != ammo)
	{
	  intelStatus.ammo = ammo;
	  intelStatusChanged = True;
	}
      
      // itemClassId, item
      PhysicalP item = user->get_item_current();
      ClassId itemClassId = item ? item->get_class_id() : A_None;
      if (intelStatus.itemClassId != itemClassId)
	{
	  intelStatus.itemClassId = itemClassId;
	  strcpy(intelStatus.item,item ? item->get_class_name() : "none");
	  intelStatusChanged = True;
	}

      int itemCount = user->get_item_count();
      if (intelStatus.itemCount != itemCount)
	{
	  intelStatus.itemCount = itemCount;
	  intelStatusChanged = True;
	}
    }
  else // Not User.
    {
      Boolean weaponReady = 
	p->is_fighter() || (p->is_built_in() && p->ready());
      if (intelStatus.weaponReady != weaponReady)
	{
	  intelStatus.weaponReady = weaponReady;
	  intelStatusChanged = True;
	}

      if (intelStatus.ammo != PH_AMMO_UNLIMITED)
	{
	  intelStatus.ammo = PH_AMMO_UNLIMITED;
	  intelStatusChanged = True;
	}

      if (p->is_suicide())
	{
	  if (intelStatus.itemClassId != A_SuicideButton)
	    {
	      intelStatus.itemClassId = A_SuicideButton;
	      strcpy(intelStatus.item,"suicide button");
	      intelStatusChanged = True;
	    }
	}
      else if (intelStatus.itemClassId != A_None)
	{
	  intelStatus.itemClassId = A_None;
	  strcpy(intelStatus.item,"none");
	  intelStatusChanged = True;
	}
    }

  // Kills does not depend on the physical.
}



Human::Human(WorldP w,LocatorP l,char *name,int lives,ColorNum cNum) :
       Intel(w,l,name,lives,NULL,ITnone)
{
  command = IT_NO_COMMAND;
  colorNum = cNum;
}



Boolean Human::is_human()
{
  return True;
}



void Human::clock(PhysicalP p)
{ 
  assert(alive());
  p->set_command(command); 
  command = IT_NO_COMMAND; 
  Intel::clock(p);
}



Machine::Machine(WorldP w,LocatorP l,char *name,const IntelOptions *ops,
		 ITmask opMask,IntelP master) 
: Intel(w,l,name,1,ops,opMask)
{
  // strategyChange uses default initializer.
  Timer mTimer(REFLEXES_TIME);
  reflexes = mTimer;
  Timer oTimer(LADDER_JUMP_TIME);
  ladderJump = oTimer;
  strategy = doNothing;
  if (master)
    {
      masterIntelId = master->get_intel_id(); 
      masterSet = True;
    }
  else
    masterSet = False;
}



IntelId Machine::get_master_intel_id()
{
  if (masterSet)
    return masterIntelId;
  else
    {
      IntelId invalid;
      return invalid;
    }
}



void Machine::add_human_kill()
{
  if (masterSet)
    {
      LocatorP l = get_locator();
      IntelP intel = l->lookup(masterIntelId);
      if (intel)
	{
	  intel->add_human_kill();
	  return;
	}
    }
  Intel::add_human_kill();
}



void Machine::add_enemy_kill()
{
  if (masterSet)
    {
      LocatorP l = get_locator();
      IntelP intel = l->lookup(masterIntelId);
      if (intel)
	{
	  intel->add_enemy_kill();
	  return;
	}
    }
  Intel::add_enemy_kill();
}



void Machine::add_soup()
{
  assert(0);
}



void Machine::clock(PhysicalP p)
{
  assert(alive() && p->is_creature());
  const IntelOptions &ops = get_intel_options();
  LocatorP locator = get_locator();
  Boolean commandSet = False;
  
  if (reflexes.ready())
    {
      // Use MedKit.
      if (!commandSet && 
	  p->is_user() && 
	  (p->get_health() < p->get_health_max()) && 
	  (has_item(p,A_MedKit)))
	{
	  PhysicalP item = p->get_item_current();
	  if (item && (item->get_class_id() == A_MedKit))
	    p->set_command(IT_ITEM_USE);
	  else
	    p->set_command(IT_ITEM_CHANGE);
	  commandSet = True;
	}
      
      // Use Transmogifier.
      if (!commandSet && p->is_user() && 
	  (p->get_health() < TRANSMOGIFIER_PERCENT * p->get_health_max()) &&
	  (has_item(p,A_Transmogifier)))
	{
	  PhysicalP item = p->get_item_current();
	  if (item && (item->get_class_id() == A_Transmogifier))
	    p->set_command(IT_ITEM_USE);
	  else
	    p->set_command(IT_ITEM_CHANGE);
	  commandSet = True;
	}

      // Use Shield.
      if (!commandSet && p->is_user() && p->is_moving() && has_shield(p))
	{
	  PhysicalP protectionP = 
	    locator->lookup(((MovingP)p)->get_protection());
	  if (!protectionP)
	    {
	      PhysicalP item = p->get_item_current();
	      if (item && item->is_shield())
		p->set_command(IT_ITEM_USE);
	      else
		p->set_command(IT_ITEM_CHANGE);
	      commandSet = True;
	    }
	}

      // Use Doppel.
//      if (!commandSet && p->is_user() && p->is_moving() && 
//	  has_item(p,A_Doppel))
//	{
//	  PhysicalP item = p->get_item_current();
//	  if (item && item->get_class_id() == A_Doppel)
//	    p->set_command(IT_ITEM_USE);
//	  else
//	    p->set_command(IT_ITEM_CHANGE);
//	  commandSet = True;
//	}
      

      PhysicalP target = NULL;
      
      // Make sure master is not to close or too far away.
      PhysicalP masterP = NULL; // meaningful iff masterSet (may be NULL).
      int master_dist_2; // meaningful iff (masterSet && masterP).
      if (masterSet)
	{
	  IntelP master = locator->lookup(masterIntelId);
	  if (master)
	    {
	      Id masterId = master->get_id();
	      masterP = locator->lookup(masterId);
	      PhysicalP p = locator->lookup(get_id());
	      if (masterP && p)
		{
		  const Area &masterArea = masterP->get_area();
		  const Area &area = p->get_area();
		  Pos masterMiddle = masterArea.get_middle();
		  Pos middle = area.get_middle();
		  Size diff = masterMiddle - middle;
		  master_dist_2 = diff.abs_2();
		}
	      else
		masterP = NULL; // To show that master_dist_2 is invalid.
	    }
	}

      // Wandering too far from the master.
      if (masterSet && masterP && master_dist_2 > MASTER_MAX_DIST_2)
	{
	  strategyChange.set(TO_MASTER_TIME);
	  strategy = toMaster;
	  // target = masterP;
	  targetId = masterP->get_id();
	}


      // New strategy.
      // Make sure target is non-NULL for strategies that require it.
      if (strategyChange.ready())
	{
	  Boolean isEnemy;
	  target = new_target_id(isEnemy,p,masterP); // Sets targetId.
	  
	  if (target)
	    {
	      if (isEnemy)
		{
		  if ((ops.psychotic ||
		       (((p->is_user() && (has_gun(p) || has_cutter(p))) ||
			 p->is_fighter() || 
			 p->is_built_in() ||
			 p->prickly()) &&
			(p->get_health() > 
			 HEALTH_ATTACK_PERCENT * p->get_health_max()))) &&
		      !ops.harmless)
		    strategy = toTarget;
		  else
		    strategy = awayTarget;
		}
	      else
		strategy = toTarget;
	      strategyChange.set(TO_AWAY_TARGET_TIME);
	    }
	  else
	    strategy = doNothing;
	}
      else 
	// Make sure that target still exists.
	if (strategy == toTarget || strategy == awayTarget
	    || strategy == toMaster)
	  {
	    target = locator->lookup(targetId);
	    if (!target)
	      strategy = doNothing;
	  }

      // If no target, change strategy to toPos.
      if (strategy == doNothing)
	 /* && !masterSet */
	{
	  strategy = toPos;
	  strategyChange.set(TO_POS_TIME);
	  
	  const Area &area = p->get_area();
	  Pos middle = area.get_middle();
	  
	  targetPos.x = middle.x + Utils::choose(WANDER_WIDTH) - 
	    WANDER_WIDTH / 2;
	  targetPos.y = middle.y + Utils::choose(WANDER_HEIGHT) - 
	    WANDER_HEIGHT / 2;
	}


// Disabled
#if 0
      // If nothing better to do, go to master.
      if (strategy == doNothing && masterSet && masterP)
	{
	  strategy = toMaster;
	  strategyChange.set(1);
	  target = masterP;
	  targetId = masterP->get_id();
	}
#endif
      

      // Act on current strategy.
      switch (strategy) {
      case toPos:
	if (!commandSet)
	  commandSet = move_pos(p,targetPos,ladderJump);
	break;
	
      case toTarget:
	{
	  assert(target);

	  // Check is_creature so won't attack items.
	  if (!commandSet && !ops.harmless && target->is_creature())
	    commandSet = attack_target(p,target);
	  
	  if (!commandSet)
	    commandSet = move_target(p,target,ladderJump);
	}
	break;
	
      case awayTarget:
	{
	  assert(target);

	  const IntelOptions &ops = get_intel_options();
	  // Check is_creature so won't attack items.
	  if (!commandSet && !ops.harmless && target->is_creature())
	    commandSet = attack_target(p,target);

	  if (!commandSet)
	    commandSet = away_target(p,target,ladderJump);
	}
	break;
	
      case toMaster:
	assert(target);
	if (!commandSet)
	  commandSet = move_target(p,target,ladderJump);
	if (masterSet && masterP && master_dist_2 < MASTER_MIN_DIST_2)
	  strategyChange.set(1);
	break;
	
      case doNothing:
	if (!commandSet)
	  {
	    p->set_command(IT_CENTER);
	    commandSet = True;
	  }
	break;
      };

      reflexes.set();
    }
  
  reflexes.clock();
  strategyChange.clock();
  ladderJump.clock();
  Intel::clock(p);
}



Boolean Machine::attack_target(PhysicalP p,PhysicalP target)
{
  // Fire a gun if available.
  Boolean useGun = p->is_user() && has_gun(p);
  if (useGun)
    {
      if (weapon_current_is_gun(p))
	{
	  PhysicalP weapon = p->get_weapon_current();
	  if (weapon->ready())
	    {
	      const Area &area = p->get_area();
	      Pos pos = area.get_middle();
	      const Area &targetArea = target->get_area();
	      Pos targetPos = targetArea.get_middle();
	      
	      Size rel = targetPos - pos;
	      Boolean distOk = True;
	      
	      if (weapon->get_class_id() == A_FThrower)
		distOk = rel.abs_2() < FTHROWER_RANGE_2;
	      
	      if (distOk)
		{
		  Dir weaponDir = rel.get_dir();
		  
		  const Vel *unitVels = p->get_unit_vels();
		  float z = rel.cross(unitVels[weaponDir]);
		      
		  if (fabs(z) <= SHOT_CUTOFF)
		    {
		      p->set_command(dir_to_command_weapon(weaponDir));
		      return True;
		    }
		}
	    }
	}
      else /* if (weapon_current_is_gun(p)) */
	{
	  p->set_command(IT_WEAPON_CHANGE);
	  return True;
	}
    } /* if (useGun) */
  
  
  // Use cutter if available.
  Boolean useCutter = !useGun && p->is_user() && has_cutter(p);
  if (useCutter)
    {
      if (!weapon_current_is_cutter(p))
	{
	  p->set_command(IT_WEAPON_CHANGE);
	  return True;
	}
      // else move toward target.
    }
  

  // Use built in weapon if available.
  Boolean useBuiltIn = !useGun && !useCutter && p->is_built_in();
  if (useBuiltIn)
    {
      if (!p->get_weapon_current())
	{
	  if (p->ready())
	    {
	      const Area &area = p->get_area();
	      Pos pos = area.get_middle();
	      const Area &targetArea = target->get_area();
	      Pos targetPos = targetArea.get_middle();
		  
	      Size rel = targetPos - pos;
	      Dir weaponDir = rel.get_dir();
	      const Vel *unitVels = p->get_unit_vels();
	      float z = rel.cross(unitVels[weaponDir]);
		      
	      if (fabs(z) <= SHOT_CUTOFF)
		{
		  p->set_command(dir_to_command_weapon(weaponDir));
		  return True;
		}
	    }
	}
      else
	{
	  p->set_command(IT_WEAPON_CHANGE);
	  return True;
	}
    }

  
  // Close range fighting
  if (!useGun && !useCutter && !useBuiltIn && p->is_fighter())
    {
      if (!p->get_weapon_current())
	{
	  const Area &area = p->get_area();
	  const Area &targetArea = target->get_area();
	  const Pos middle = area.get_middle();
	  const Pos targetMiddle = targetArea.get_middle();
	  
	  if (middle.distance_2(targetMiddle) <= FIGHT_RANGE_2)
	    {
	      //  Dir dirTo = area.dir_to(targetArea);
	      Size rel = targetMiddle - middle;
	      Dir dirTo = rel.get_dir();

	      p->set_command(dir_to_command_weapon(dirTo));
	      return True;
	    }
	}
      else
	{
	  p->set_command(IT_WEAPON_CHANGE);
	  return True;
	}
    }


  return False;
}




Boolean Machine::move_target(PhysicalP physical,PhysicalP target,
			     Timer &ladderJump)
{
  const Area &area = physical->get_area();
  Dir toTarget = area.dir_to(target->get_area());
  
  return move_dir(physical,toTarget,ladderJump);
}



Boolean Machine::away_target(PhysicalP physical,PhysicalP target,
			     Timer &ladderJump)
{
  const Area &area = physical->get_area();
  Dir awayTarget = Coord::dir_opposite(area.dir_to(target->get_area()));
  
  return move_dir(physical,awayTarget,ladderJump);
}



Boolean Machine::move_pos(PhysicalP physical,const Pos &targetPos,
			  Timer &ladderJump)
{
  const Area &area = physical->get_area();
  Dir toPos = area.dir_to(targetPos);

  return move_dir(physical,toPos,ladderJump);
}



Boolean Machine::move_dir(PhysicalP physical,Dir dir,
			  Timer &ladderJump)
{
  assert(physical->is_creature());
  CreatureP creature = (CreatureP) physical;
  ITcommand command = IT_NO_COMMAND;
  
  if (creature->is_flying())
    command = dir_to_command(dir);
  else // Sticky, Walking, Grounded, Hopping
    {
      Stance stance = creature->get_stance();
      Touching touching = creature->get_touching_area();
      const Hanging &hanging = creature->get_hanging();
      Boolean canClimb = creature->can_climb();
      
      switch (dir) {
      case CO_R:
	switch (stance) {
	case CO_center:
	case CO_dn:
	case CO_l:
	case CO_up:
	  if ((touching != CO_R) || creature->is_sticky())
	    command = IT_R;
	  break;
	case CO_climb:
	  if ((touching != CO_R) || creature->is_sticky())
	    {
	      command = IT_UP_R;
	      ladderJump.set();
	    }
	  break;
	};
	break;
	
      case CO_DN_R:
	if (canClimb && touching != CO_dn && ladderJump.ready())
	  {
	    command = IT_DN;
	    break;
	  }
	switch (stance) {
	case CO_center:
	case CO_dn:
	case CO_up:
	  command = IT_R;
	  break;
	case CO_r:
	  command = IT_DN;
	  break;
	case CO_l:
	  command = IT_DN_R;
	  break;
	case CO_climb:
	  if (touching != CO_dn)
	    command = IT_DN;
	  else
	    {
	      command = IT_R;
	      ladderJump.set();
	    }
	  break;
	}
	break;
	
      case CO_DN:
	switch (stance) {
	case CO_r:
	case CO_up:
	case CO_l:
	case CO_climb:
	  if ((touching != CO_dn) || canClimb ||
	      (hanging.corner == CO_dn_R) || (hanging.corner == CO_dn_L))
	    command = IT_DN;
	  break;
	case CO_center:
	  if (canClimb)
	    command = IT_DN;
	  break;
	}
	break;
	
      case CO_DN_L:
	if (canClimb && touching != CO_dn && ladderJump.ready())
	  {
	    command = IT_DN;
	    break;
	  }
	switch (stance) {
	case CO_center:
	case CO_dn:
	case CO_up:
	  command = IT_L;
	  break;
	case CO_l:
	  command = IT_DN;
	  break;
	case CO_r:
	  command = IT_DN_L;
	  break;
	case CO_climb:
	  if (touching != CO_dn && ladderJump.ready())
	    command = IT_DN;
	  else
	    {
	      command = IT_L;
	      ladderJump.set();
	    }
	  break;
	}
	break;
	
      case CO_L:
	switch (stance) {
	case CO_center:
	case CO_dn:
	case CO_r:
	case CO_up:
	  if ((touching != CO_L) || (creature->is_sticky()))
	    command = IT_L;
	  break;
	case CO_climb:
	  if ((touching != CO_L) || (creature->is_sticky()))
	    {
	      command = IT_UP_L;
	      ladderJump.set();
	    }
	  break;
	};
	break;
	
      case CO_UP_L:
	if (canClimb && touching != CO_up && ladderJump.ready())
	  {
	    command = IT_UP;
	    break;
	  }
	switch (stance) {
	case CO_center:
	case CO_dn:
	case CO_up:
	  command = IT_L;
	  break;
	case CO_r:
	  command = IT_UP;
	  break;
	case CO_l:
	  command = IT_UP_L;
	  break;
	case CO_climb:
	  if (touching != CO_up)
	    command = IT_UP;
	  else
	    {
	      command = IT_L;
	      ladderJump.set();
	    }
	  break;
	};
	break;
	
      case CO_UP:
	switch (stance) {
	case CO_center:
	case CO_air:
	case CO_dn:
	case CO_r:
	case CO_l:
	case CO_climb:
	  if ((touching != CO_up) || canClimb || 
	      (hanging.corner == CO_up_R) || (hanging.corner == CO_up_L))
	    command = IT_UP;
	  break;
	};
	break;
	
      case CO_UP_R:
	if (canClimb && touching != CO_up && ladderJump.ready())
	  {
	    command = IT_UP;
	    break;
	  }
	switch (stance) {
	case CO_center:
	case CO_dn:
	case CO_up:
	  command = IT_R;
	  break;
	case CO_r:
	case CO_l:
	  command = IT_UP_R;
	  break;
	case CO_climb:
	  if (touching != CO_up)
	    command = IT_UP;
	  else
	    {
	      command = IT_R;
	      ladderJump.set();
	    }
	  break;
	};
	break;
      }
      
      if ((command == IT_NO_COMMAND) && !canClimb)
	command = IT_CENTER;
    }
  
  if (command != IT_NO_COMMAND)
    {
      physical->set_command(command);
      return True;
    }

  return False;
}



Boolean Machine::has_gun(PhysicalP p)
{
  for (int n = 0; n < p->get_weapons_num(); n++)
    {
      PhysicalP gun = p->get_weapon(n);
      if (gun && gun->is_gun())
	return True;
    }
  return False;
}



Boolean Machine::has_cutter(PhysicalP p)
{
  for (int n = 0; n < p->get_weapons_num(); n++)
    {
      PhysicalP w = p->get_weapon(n);
      if (w && w->is_cutter())
	return True;
    }
  return False;
}



Boolean Machine::has_shield(PhysicalP p)
{
  for (int n = 0; n < p->get_items_num(); n++)
    {
      PhysicalP w = p->get_item(n);
      if (w && w->is_shield())
	return True;
    }
  return False;
}



Boolean Machine::has_item(PhysicalP p,ClassId classId)
{
  for (int n = 0; n < p->get_items_num(); n++)
    {
      PhysicalP item = p->get_item(n);
      if (item && item->get_class_id() == classId)
	return True;
    }
  return False;
}



Boolean Machine::weapon_current_is_gun(PhysicalP p)
{
  PhysicalP weapon = p->get_weapon_current();
  return weapon && weapon->is_gun();
}



Boolean Machine::weapon_current_is_cutter(PhysicalP p)
{
  PhysicalP weapon = p->get_weapon_current();
  return weapon && weapon->is_cutter();
}



PhysicalP Machine::new_target_id(Boolean &isEnemy,PhysicalP p,
				 PhysicalP masterP)
{
  LocatorP locator = get_locator();
  
  PhysicalP nearby[OL_NEARBY_MAX];
  int nearbyNum;
  locator->get_nearby(nearby,nearbyNum,p,TARGET_RANGE);

  PhysicalP items[OL_NEARBY_MAX];
  int itemsNum = 0;
  PhysicalP enemies[OL_NEARBY_MAX];
  int enemiesNum = 0;


  // Pull out possible items and enemies from nearby.
  const IntelOptions &ops = get_intel_options();
  ClassId classId = p->get_class_id();
  for (int n = 0; n < nearbyNum; n++)
    {
      IntelP intel = nearby[n]->get_intel();
      
      if (
	  // Not your master.
	  nearby[n] != masterP &&
	  // Must be a creature
	  nearby[n]->is_creature() && 
	  // Must be alive
	  nearby[n]->alive() && 
	  // Must have intelligence
	  intel && 
	  // Don't want to have slaves of same master fighting each other.
	  !(masterSet && !intel->is_human() && 
	    ((MachineP)intel)->masterSet && 
	    ((MachineP)intel)->masterIntelId == masterIntelId) &&
	  // Check if ignoring lemmings
	  !(ops.ignoreLemmings && nearby[n]->get_class_id() == A_Lemming) &&
	  // Machines ignoring machines of same class.
	  (intel->is_human() ||
	   !(ops.classFriends && nearby[n]->get_class_id() == classId)) &&
	  // Huggers and aliens don't attack huggers.
	  !((p->get_class_id() == A_Alien || p->is_hugger()) && 
	    nearby[n]->is_hugger()) &&
	  // Huggers don't attack aliens
	  !(p->is_hugger() && nearby[n]->get_class_id() == A_Alien) &&
	  // Ignore invisible targets (Can cast to MovingP because is_creature())
	  !((MovingP)nearby[n])->is_invisible())
	{
	  enemies[enemiesNum] = nearby[n];
	  enemiesNum++;
	}
      else if (
	       // Not ignoring items
	       !ops.ignoreItems &&
	       // Must be user
	       p->is_user() && 
	       // Must be item and can take it.
	       nearby[n]->is_item() && 
	       ((ItemP)nearby[n])->can_take() &&
	       // Ignore invisible targets (Can cast to MovingP because is_item().)
	       !((MovingP)nearby[n])->is_invisible())
	{
	  items[itemsNum] = nearby[n];
	  itemsNum++;
	}
    }

  if (enemiesNum)
    {
      PhysicalP target = enemies[Utils::choose(enemiesNum)];
      targetId = target->get_id();
      isEnemy = True;
      return target;
    }
  
  if (itemsNum)
    {
      PhysicalP target = items[Utils::choose(itemsNum)];
      targetId = target->get_id();
      isEnemy = False;
      return target;
    }

  return NULL;
}



Boolean Enemy::is_enemy()
{
  return True;
}



Boolean Neutral::is_enemy()
{
  return False;
}



LemmingIntel::LemmingIntel(WorldP w,LocatorP l,char *name,const Id &home_id)
:Neutral(w,l,name,NULL,ITnone)
{
  Timer oTimer(LADDER_JUMP_TIME);
  ladderJump = oTimer;
  homeId = home_id;
}



Boolean LemmingIntel::is_lemming_intel()
{
  return True;
}



void LemmingIntel::clock(PhysicalP p)
{
  assert(alive() && p->is_creature());
  LocatorP l = get_locator();

  PhysicalP home = l->lookup(homeId);
  assert(home);
  const Area &area = home->get_area();
  Pos middle = area.get_middle();

  move_pos(p,middle,ladderJump);
  ladderJump.clock();

  // NOTE: Skipping Machine::clock.
  Intel::clock(p);
}
