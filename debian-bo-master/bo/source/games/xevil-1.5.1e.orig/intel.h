// "intel.h"

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

#ifndef INTEL_H
#define INTEL_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
#include "utils.h"
#include "coord.h"
#include "id.h"
#include "locator.h"
#include "world.h"



// Defines
#define IT_STRING_LENGTH 80
#define IT_INFINITE_LIVES -1


class Physical;
typedef Physical *PhysicalP;


/* Possible commands to be given to objects.
   Order of directions is guaranteed.  (See ui.h and game.C) */
  enum {
    IT_CENTER,
    IT_R, IT_DN_R, IT_DN, IT_DN_L,
    IT_L, IT_UP_L, IT_UP, IT_UP_R,
    IT_WEAPON_CENTER,IT_WEAPON_CHANGE,IT_WEAPON_DROP,
    IT_ITEM_USE,IT_ITEM_CHANGE,IT_ITEM_DROP,
    
    IT_WEAPON_R,IT_WEAPON_DN_R,IT_WEAPON_DN,IT_WEAPON_DN_L,
    IT_WEAPON_L,IT_WEAPON_UP_L,IT_WEAPON_UP,IT_WEAPON_UP_R,

    IT_NO_COMMAND};
typedef int ITcommand;
#define IT_COMMAND_MAX IT_NO_COMMAND



// IntelOptions
typedef unsigned long ITmask;
#define ITnone 0L
#define ITharmless (1L<<0)
#define ITclassFriends (1L<<1)
#define ITpsychotic (1L<<2)
#define ITignoreItems (1L<<3)
#define ITignoreLemmings (1L<<4)

struct IntelOptions {
  Boolean classFriends; // Do not attack members of same class.
  Boolean harmless; // Will not attack even if it has the ability to do so.
  Boolean psychotic; // Will never run away.  harmless overrides psychotic.
  Boolean ignoreItems; // Don't try to pick up items.
  Boolean ignoreLemmings; // Don't attack Lemmings.
};



// IntelStatus used by ui.
// Remember Intel::die
struct IntelStatus {
  // strings and ClassIds must be kept consistent.

  char name[IT_STRING_LENGTH];
  ClassId classId;
  char className[IT_STRING_LENGTH];
  Health health; // -1 means dead.
  Mass mass;

  ClassId weaponClassId;  
  char weapon[IT_STRING_LENGTH];
  Boolean weaponReady;
  int ammo;

  ClassId itemClassId;
  char item[IT_STRING_LENGTH];
  int itemCount; // Number of the item.

  int lives; // Can be IT_INFINITE_LIVES.
  int humanKills;
  int enemyKills;
  int soups; // Human killed by something other than a human.  Only looked at
             // when game style is UIsettings::DUEL.
};

  

// Class Definitions.
class Intel {
 public:
  Intel(WorldP w,LocatorP l,char *name,int lives,
	const IntelOptions *ops,ITmask opMask); 
  /* EFFECTS: Create a new intel with the options specified in opMakse that are
     in ops.  ops will never be referenced if opMask is ITnone.  lives are
     decremented as human is created.  lives can also be set to 
     IT_INFINTE_LIVES. */

  virtual Boolean is_human();
  /* NOTE: !is_human() iff Intel is a Machine. */
  virtual Boolean is_enemy();
  virtual Boolean is_lemming_intel();

  Boolean alive() {return living;}
  WorldP get_world() {return world;}
  LocatorP get_locator() {return locator;}

  const Id &get_id() {return id;}
  /* NOTE: If dead, return an invalid id. */

  Boolean reincarnate_me();
  /* EFFECTS: Should *this be reincarnated. */

  Boolean intel_status_changed() {return intelStatusChanged;}

  const IntelStatus *get_intel_status() 
    {intelStatusChanged = False; return &intelStatus;}

  const char *get_name() {return intelStatus.name;}
  int get_lives() {return intelStatus.lives;} // Can be IT_INFINITE_LIVES.
  int get_human_kills() {return intelStatus.humanKills;}
  int get_enemy_kills() {return intelStatus.enemyKills;}
  int get_soups() {return intelStatus.soups;}
  /* NOTE: Could achieve the same thing with get_intel_status.  Convenience 
   functions. */

  IntelId get_killer_intel_id() {return killerIntelId;}

  IntelId get_intel_id() {return intelId;}

  void set_id(const Id &i) {id = i;}
  /* REQUIRES: i is valid. */
  /* NOTE: Only needed for Physical::set_intel.  id is updated on every 
     clock. */

  void set_lives(int lives) 
    {intelStatus.lives = lives; intelStatusChanged = True;}
  /* NOTE: Can be IT_INFINITE_LIVES. */

  virtual void add_human_kill();

  virtual void add_enemy_kill();

  virtual void add_soup();

  void set_killer_intel_id(const IntelId &k) {killerIntelId = k;}

  void set_intel_id(const IntelId &i) {intelId = i;}
  /* NOTE: Should only be used by the Locator. */

  static ITcommand dir_to_command(Dir dir);
  /* REQUIRES: dir is a "pure" direction or CO_air. */
  /* NOTE: CO_air -> IT_CENTER. */

  static ITcommand dir_to_command_weapon(Dir dir);

  static Dir command_weapon_to_dir_4(ITcommand);

  static Dir command_weapon_to_dir_8(ITcommand);
  /* EFFECTS: If command is a weapon command return the corresponding Dir. */
  /* NOTE: Like Gun::compute_weapon_dir. */

  static Boolean is_command_weapon(ITcommand command)
    {return (command >= IT_WEAPON_R && command <= IT_WEAPON_UP_R) || 
       command == IT_WEAPON_CENTER;}

  static ITcommand center_pos_to_command(const Pos &pos);
  /* EFFECTS: Returns the command corresponding to the direction from (0,0) to
     pos. */
  /* NOTE: pos can have negative components. */
     

  void die(); 
  /* EFFECTS: Tell *this that it no longer has anything clocking it. */
  /* NOTE: This MUST be called at death.  Game requires it to refill enemy
     players. */

  void reincarnate();
  /* EFFECTS: Tell *this that it will be clocked again. */

  virtual void clock(PhysicalP p);
  /* NOTE: Should be called by Intel's children. */


#ifndef PROTECTED_IS_PUBLIC
 protected:
#endif
  const IntelOptions &get_intel_options() {return intelOptions;}


 private:
  IntelStatus intelStatus;
  Boolean intelStatusChanged; // Since last get_intel_status.
  Boolean living;
  WorldP world;
  LocatorP locator;
  Id id;
  IntelId killerIntelId;  // Intel that most recently attacked the Intel.
  IntelId intelId;
  IntelOptions intelOptions;

  const static IntelOptions intelOptionsDefault;
};
typedef Intel *IntelP;



// Simply buffers input from a user interface.
class Human: public Intel {
 public: 
  Human(WorldP w,LocatorP l,char *name,int lives,ColorNum colorNum);
  /* NOTE: Uses all default options.  intelOptions should be meaningless for 
     a human. */

  ColorNum get_color_num() {return colorNum;}

  virtual Boolean is_human();
    
  void set_command(ITcommand c) {command = c;}
  /* EFFECTS: Asynchronously set the command for the current turn to be c.
     Overrides previous settings. */

  virtual void clock(PhysicalP p);
  /* EFFECTS: Sets the command for p. */


 private:
  ITcommand command;
  ColorNum colorNum;
};
typedef Human *HumanP;



class Machine: public Intel {
  enum Strategy {doNothing, toPos, toTarget, awayTarget, toMaster};
  
public:
  Machine(WorldP w,LocatorP l,char *name,
	  const IntelOptions *ops,ITmask opMask,
	  IntelP master = NULL);
  
  Boolean is_slave() {return !masterSet;}

  IntelId get_master_intel_id();
  /* EFFECTS: If a slave, return the IntelId of the master.  Otherwise, 
     return an invalid IntelId. */

  virtual void add_human_kill();

  virtual void add_enemy_kill();

  virtual void add_soup();

  virtual void clock(PhysicalP);
  /* NOTE: Not called by Machine's children. */


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  static Boolean attack_target(PhysicalP p,PhysicalP target);
  static Boolean move_target(PhysicalP p,PhysicalP target,Timer &ladderJump);
  static Boolean away_target(PhysicalP p,PhysicalP target,Timer &ladderJump);
  static Boolean move_pos(PhysicalP p,const Pos &targetPos,Timer &ladderJump);
  static Boolean move_dir(PhysicalP p,Dir d,Timer &ladderJump);
  /* REQUIRES: p and target must be non-NULL. */
  /* REQUIRES: d is an element of {CO_R..CO_UP_R,CO_air} */
  /* EFFECTS: The above are all utility functions that attempt to set the
     command for p to do something.  Returns whether the command was set or
     not. */
  /* IMPLEMENTATION NOTE: Declared static to ensure that no data members of 
     Machine are used in these utility functions.  We */


  static Boolean has_gun(PhysicalP);
  static Boolean has_cutter(PhysicalP);
  static Boolean has_shield(PhysicalP);
  static Boolean has_item(PhysicalP,ClassId);
  static Boolean weapon_current_is_gun(PhysicalP);
  static Boolean weapon_current_is_cutter(PhysicalP);
  /* EFFECTS: Return info about a Physical. */


private:
  PhysicalP new_target_id(Boolean &isEnemy,PhysicalP p,PhysicalP masterP);
  /* MODIFIES: isItem */
  /* EFFECTS: Get a new target for p.  Sets targetId, sets isEnemy to True iff
     returned pointer is an enemy (as opposed to an item).
     Returns NULL if no suitable target found.  masterP is the master's
     Physical or NULL. */


  IntelId masterIntelId; // Meaningful iff masterSet.
  Boolean masterSet; // Is the Machine a slave to another Intel.
  Strategy strategy;
  Pos targetPos;
  Id targetId;
  Timer strategyChange;
  Timer reflexes;
  Timer ladderJump; // don't get back on ladder immediately after jumping off.
};
typedef Machine *MachineP;



class Enemy: public Machine {
public:
  Enemy(WorldP w,LocatorP l,char *name,
	const IntelOptions *ops,ITmask opMask,
	IntelP master = NULL)
    :Machine(w,l,name,ops,opMask,master) {}
  
  virtual Boolean is_enemy();
};
typedef Enemy *EnemyP;




class Neutral: public Machine {
public:
  Neutral(WorldP w,LocatorP l,char *name,
	const IntelOptions *ops,ITmask opMask,
	IntelP master = NULL)
    :Machine(w,l,name,ops,opMask,master) {}
  
  virtual Boolean is_enemy();
};
typedef Neutral *NeutralP;




class LemmingIntel: public Neutral {
public:
  LemmingIntel(WorldP w,LocatorP l,char *name,const Id &homeId);
  
  virtual Boolean is_lemming_intel();

  virtual void clock(PhysicalP);
  /* NOTE: Does not call Neutral::clock. */
  

 private:
  Timer ladderJump; // don't get back on ladder immediately after jumping off.
  Id homeId;
};
#endif

