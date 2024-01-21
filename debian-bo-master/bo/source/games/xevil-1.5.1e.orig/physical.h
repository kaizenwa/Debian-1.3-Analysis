// "physical.h"  
// Interior nodes of the physical object tree.  These classes
// are never instantiated.


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

#ifndef PHYSICAL_H
#define PHYSICAL_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
#include "utils.h"
#include "coord.h"
#include "area.h"
#include "world.h"
#include "id.h"
#include "intel.h"
#include "locator.h"


// Defines
#define PH_ANIM_MAX 4
#define PH_FRAME_MAX 7
#define PH_WEAPONS_MAX 10
#define PH_ITEMS_MAX 20
#define PH_AMMO_UNLIMITED -1



// Other declarations
enum PHsig {PH_NO_SIG, PH_NOT_SET, PH_ID_CHANGED};



// Class declarations
class Protection;
typedef Protection *ProtectionP;


////////// Physical
/* Physical is the parent class of all physical objects. */

/* PhysicalContext plays a double role.  Like all the other "contexts", 
   MovingContext, CreatureContext, etc., it is a place to specify paramaters
   for a specific class.  E.g. if you look at the PhysicalContext in 
   ninja.bitmaps you will see where the mass, max health, etc, for a Ninja is
   specified.
   
   But, it also plays a role for the Object Locator.  All classes of Physical
   objects (not just the objects, but the classes) are registered with
   Locator::register_context().  So, the Locator has a list of all the classes
   in the game.  For example, if the Locator wanted to create one of each type
   of object, it could just go through the list of registed classes and 
   call the PhysicalContext->create() method on each.
   Only PhysicalContext is used for this extra functionality.  MovingContext,
   ShotContext, etc, only play the first role.
   */
struct PhysicalContext {
  Health health;
  Mass mass;
  ClassId classId;
  const char *className; // Used to be called "clas".


  /* Global fields for the Game as a whole. */

  // Can a Transmogifier turn something into this class of object.
  Boolean transmogifyTarget;
  // Can objects of this class use doppelgangers.  E.g. we don't want 
  // FireDemons to start duplicating themselves.  This is independent of 
  // whether this class is derived from class User.  
  Boolean doppelUser;
  // Can a Human player start out as an object of this class.
  Boolean potentialHuman;
  // Can an Enemy(Machine) player start out as an object of this class.
  Boolean potentialEnemy;
  // If potentialEnemy is true, this gives the likelihood that an enemy will
  // be created of this class.  For reference Ninja is 3.  E.g. if 
  // enemyWeight is 1, then there will be one third as many of these objects
  // as there are Ninjas.
  int enemyWeight;
  // Is this a weapon or item that can magically fall out of the sky.
  // Only one of potentialWeapon, potentialOtherItem should be True.
  Boolean potentialWeapon;
  Boolean potentialOtherItem;
  // If the Physical is a potential item or weapon, this gives a measure of
  // how many of the object will be created.  You can think of this as the 
  // "percent" of the World that is covered by the item/weapon.
  float objectWorldPercent;
  // A pointer to a function that gives the maximum size of the Physical
  // corresponding to this context.
  Size (*get_size_max)();
  // A pointer to a function that creates an instance of the Physical 
  // corresponding to this context.
  PhysicalP (*create)(WorldP w,LocatorP l,const Pos &pos);
  // Flags that determine what statistics will be printed out when XEvil is
  // exited.  You probably only want to set one of these, if any.
  // E.g. For Shot, we are interested in the number that were 
  // created (fired), so statsCreations would be True.  E.g. For MedKit, 
  // we care more about how many were used than how many were created, so we
  // set statsUses.  For Ninja, the most interesting thing is how many of 
  // got killed (and average lifespan), so we set statsDeath.  E.g. for Rock
  // we don't set any of them, (who cares about a rock).
  Boolean statsCreations;
  Boolean statsUses;
  Boolean statsDeaths;
  // Return the stats for the class.  This will be NULL if the class does not
  // have stats.  I.e. a NULL function, NOT a function that returns NULL.
  // This is non-NULL iff any of the stats{Creations,Uses,Deaths} flags
  // are set.
  // 
  // I could probably have put the actual Stats here instead of
  // every class having to have its own stats.  But, I really wanted to keep
  // all the "Contexts" constant, and Stats obviously have to change during
  // game play.
  const Stats &(*get_stats)();
};


class Physical {
public:
  Physical(const PhysicalContext &,WorldP,LocatorP);
  /* EFFECTS: Create a new, mapped physical with no Id with undefined area. */

  Physical();
  /* NOTE: Should never be called. */

  virtual ~Physical();

  virtual const Area &get_area() = 0;

  virtual const Area &get_area_next() = 0;
  /* NOTE: Should only be used by the Locator. */

  Health get_health() {return health;}

  Health get_health_next() {return healthNext;}
  /* NOTE: Should only be used by the Locator to prepare for death.
     Somewhat of a hack. */

  Health get_health_max() {return pc->health;}
  Mass get_mass() {return mass;}
  virtual Vel get_vel();
  virtual Dir get_dir();

  ClassId get_class_id() {return pc->classId;}
  const char *get_class_name() {return pc->className;}

  virtual int get_drawing_level();

  const PhysicalContext *get_context() {return pc;}

  Boolean delete_me() {return deleteMe;}

  Boolean alive() {assert (health >= 0 || dieCalled); return health >= 0;}
  /* NOTE: Is publicly known that (health >= 0) <=> alive.  So this function is
   just for convenience. */

  Boolean die_called() 
//  {assert (healthNext < 0 || !dieCalled);  return dieCalled;}
    {return dieCalled;}
  /* EFFECTS: Returns whether die() has been called or not.  Not clocked. */

  /* Should only be used for abstract classes.  Actual classes can be tested
     for with get_class_id(). */
  virtual Boolean is_moving();
  virtual Boolean is_shot();
  virtual Boolean is_item(); 
  virtual Boolean is_shield();
  virtual Boolean is_bomb();
  virtual Boolean is_weapon();
  virtual Boolean is_cutter();
  virtual Boolean is_gun();
  virtual Boolean is_creature(); 
  virtual Boolean is_suicide();
  virtual Boolean is_user();
  virtual Boolean is_fighter();
  virtual Boolean is_walking(); 
  virtual Boolean is_sticky(); 
  virtual Boolean is_flying();
  virtual Boolean is_built_in();
  virtual Boolean is_hugger();
  virtual Boolean is_auto_use();

  Boolean get_mapped() {return mapped;}

  virtual Boolean prickly();
  /* NOTE: Prickly means that others are hurt by touching this Physical. */

  virtual Boolean collidable();
  /* NOTE: This value never changes for an object. */

  const Acc *get_unit_accs() {return unitAccs;}
  const Vel *get_unit_vels() {return unitVels;}

  Id get_id() {assert(idValid); return id;}

  PHsig get_id(Id &id);
  /* MODIFIES: id */
  /* EFFECTS: Set id to be the Id and return PH_NO_SIG if set.  Otherwise, 
     return PH_NOT_SET. */

  PhysicalP get_dont_collide() {return dontCollide;}
  /* EFFECTS: If there is another object that *this is not allowed to collide
     with, return it.  Otherwise return NULL; */

  IntelP get_intel() {return intel;}
  /* NOTE: Can be NULL. */

  Boolean get_flash() {return !flashTimer.ready();}

  Boolean get_quiet_death() {return quietDeath;}

  virtual int get_built_in_weapon_coolness();
  /* EFFECTS: Returns the coolness of having weapon set to none. */
  /* NOTE: Should be non-negative. */

  void set_command(ITcommand c) {command = c;}
  /* EFFECTS: Sets the command to be c, overrides any previous command 
     setting. */
  /* NOTE: command is not clocked. */

  PHsig set_id(const Id &id);
  /* EFFECTS: Set the Id to be id.  Return PH_NO_SIG if there was no previous
     id.  Return PH_ID_CHANGED if there was a previous id.  The id is set in 
     either case. */

  void set_dont_collide(PhysicalP other) {dontCollide = other;}
  /* EFFECTS: *this will not be allowed to collide with other.  Any previous 
     value will be overridden.  A setting of NULL disables this feature. */

  void set_intel(IntelP i) {intel = i;  if (i) i->set_id(id);}
  /* REQUIRES: Object has been added to locator (has valid id.) */
  /* NOTE: Can be NULL. */

  void set_health_next(Health h) {assert(alive()); healthNext = h;}

  virtual void heal();
  /* EFFECTS: Called by MedKit. */
  /* NOTE: Calls up the tree. */
  
  virtual void set_mapped_next(Boolean val);
  /* NOTE: Should be ok to set the value to the previous value. */
  /* NOTE: idempotent */
  /* Calls up the tree. */

  void set_delete_me() {deleteMe = True;}
  /* NOTE: Should only be called by self and Locator. */

  void flash();
  /* EFFECTS: Object will be drawn for FLASH_TIME turns. */

  void set_no_death_delete() {noDeathDelete = True;}

  virtual void corporeal_attack(PhysicalP killer,int damage); 
  virtual void heat_attack(PhysicalP,int heat,Boolean secondary = False);
  /* NOTE: Sometimes call up the tree. */
  /* NOTE: killer is the one responsible for causing the damage.  Can be 
     NULL.  Adds kills to the killer. */
  /* NOTE: Only the last call before the update cycle takes effect. */

  virtual Boolean swap_protect();
  virtual Boolean frog_protect();
  /* EFFECTS: Returns whether the Physical is protected from a swap or frog
     attack.  May have side effects, so call only once per attack. */
  /* NOTE: Using a Transmogifier is considered a swap attack. */
  
  virtual void avoid(PhysicalP);
  virtual void collide(PhysicalP);
  /* EFFECTS: Collision procedures.  avoid is called on the lighter of the two
     objects.  collide is called on both objects. */
  /* NOTE: Not always called up the tree. */

  void intelligence() {if (intel) intel->clock(this);}

  void kill_self() {healthNext = -1;}
  /* NOTE: Called in act/collision_checks phase.  Do not call this in the 
     die phase. (or update phase?) */

  virtual void set_quiet_death();
  /* EFFECTS: When this dies, do not do any funny things like leaving corpses
     or exploding or any other type of physical evidence. */
  /* WARNING: When a User dies, it drops all weapons and items even if 
     set_quiet_death() has been called.  This is somewhat of a hack. */
  /* NOTE: Calls up the tree. */

  void virtual act();
  /* EFFECTS: Action phase.  All next variables must be set here.  Commands 
     are interpreted here.*/

  void virtual update(); 
  /* EFFECTS: Set current variables to be the next ones.  No interactions 
     between physical objects. */
  
  void virtual draw(Drawable buffer,Xvars &xvars,int dpyNum,
		    const Area &area) = 0;
  /* REQUIRES: buffer is at least as big as area. */
  /* EFFECTS: Draw the physical object in buffer.  buffer represents area. */
  /* NOTE: Does not check for overlap */
  /* NOTE: X variables initialized in draw.  Thus, if draw is never called for
     a base class, the X variables never need to be initialized. */

  void virtual die();
  /* EFFECTS:  If the *this dies a natural death (I.e. health < 0), then this
     function is called.  Not called if *this is 
     destroyed for any other reason.  E.g. end of game.  The default is to 
     kill the intel and set_delete_me. */
  /* NOTE: Called only in the die phase. */
  /* NOTE: Calls up the tree. */
  /* NOTE: Guaranteed to be called only once. */


  virtual int get_weapons_num();
  virtual int get_items_num(); 
  /* NOTE: Returned value is not valid after current turn. */
  
  virtual PhysicalP get_weapon(int);
  virtual PhysicalP get_item(int); 
  virtual PhysicalP get_weapon_current();
  virtual PhysicalP get_item_current();
  /* NOTE: Can return NULL. */

  virtual int get_item_count();
  /* EFFECTS: If get_item() is non-NULL, return the number of that item
     the Physical is carrying.  Elese return 0. */

  virtual ClassId get_weapon_string(const char *&str);
  /* EFFECTS: str is set to point to a static string containing the string
     for ClassId. */
  /* NOTE: BuiltIn returns something for get_weapon_string(), but NULL
     for get_weapon_current. */

  virtual Boolean ready();



#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  WorldP get_world() {return world;}

  Boolean alive_next() {return healthNext >= 0;}

  LocatorP get_locator() {return locator;}

  virtual ITcommand get_command();
  /* EFFECTS: Gets the command. */
  /* NOTE: command is not clocked. */

  Boolean get_mapped_next() {return mappedNext;}

  void set_mass_next(Mass mss) {massNext = mss;}


private:
  void init_static();

  static Boolean staticValid;
  static Acc unitAccs[CO_DIR_MAX]; 
  static Vel unitVels[CO_DIR_MAX]; 
  Boolean idValid;
  Id id;
  WorldP world;
  LocatorP locator;
  ITcommand command;
  const PhysicalContext *pc; 
  Health health, healthNext;
  Mass mass, massNext;
  PhysicalP dontCollide;
  IntelP intel;
  Boolean deleteMe;
  Boolean mapped,mappedNext;
  Boolean noDeathDelete; // Should set_delete_me be called at death.
  Boolean dieCalled;
  int heat, heatNext;
  Boolean previousHeatWasSecondary;
  Timer flashTimer;
  Boolean quietDeath;
};
// PhysicalP defined in locator.h



////////// Protection
// Parent: Physical
// Created by a Shield.  Protects a Moving against various types of attacks.
// Not a subclass of Moving because we don't want to worry about hitting 
// walls.
struct ProtectionContext {
  char *colorName;
  PhysicalContext physicalContext;
};


class ProtectionXdata {
public:
  ProtectionXdata() {valid = False;}

  Pixel color[Xvars::DISPLAYS_MAX];
  Boolean valid;
};


class Protection : public Physical {
public:
  Protection(const ProtectionContext &,ProtectionXdata &,
	     WorldP,LocatorP,const Area &);

  virtual Boolean collidable();
  virtual const Area &get_area();
  virtual const Area &get_area_next();

  void follow(const Area &);
  /* NOTE: Can be called multiple times in one turn. */
  /* EFFECTS: Called in act() phase by the Moving being protected. 
   aNext is the next Area of the Moving.  Can be called before or after
   this->act(). */

  virtual Boolean corporeal_protect(int damage) = 0;
  virtual Boolean heat_protect(int heat,Boolean secondary) = 0;
  /* EFFECTS: The Moving has received an attack.  Return wheter the Protection
     will protect against the attack. */

  virtual void draw(Drawable,Xvars &,int,const Area &);

  virtual void act();
  virtual void update();


private:
  enum {DELTA_MIN = -2,DELTA_MAX = 5};  // DELTA_MIN <= delta < DELTA_MAX

  virtual void init_x(Xvars &);

  const ProtectionContext *prc;
  ProtectionXdata *pXdata;
  int delta;  // When drawn, how far the Protection sticks out.
  Area areaBaseNext;
  Area area,areaNext;
};
// ProtectionP defined above.



////////// Moving
// Parent: Physical
// Has all 19 directions.  Multiple pixmaps.  Can change size and position. 
// Top speed is VEL_MAX.

/* Only sizes[CO_air] and offsets[CO_air] are required to be set.  
   Gives initial size and offset. */
struct MovingContext {
  char *foreColorName;
  Boolean foreWhiteDefault;
  const char *backColorName;
  Boolean backWhiteDefault;
  int animMax[CO_DIR_MAX];
  Size sizes[CO_DIR_MAX];
  Size offsets[CO_DIR_MAX];
  char *pixmapBits[CO_DIR_MAX][PH_ANIM_MAX];
  char *maskBits[CO_DIR_MAX][PH_ANIM_MAX];
  PhysicalContext physicalContext;
};


class MovingXdata {
public:
  MovingXdata() {valid = False;}
  
  Boolean valid;
  Pixmap pixmaps[Xvars::DISPLAYS_MAX][CO_DIR_MAX][PH_ANIM_MAX],
  masks[Xvars::DISPLAYS_MAX][CO_DIR_MAX][PH_ANIM_MAX];
};


class Moving: public Physical {
public:
  Moving(const MovingContext &m_c,
	 MovingXdata &x_data,
	 WorldP world,
	 LocatorP l,
	 const Pos &rawPos,
	 Dir dirInitial = CO_air);

  Moving();
  /* NOTE: Should never be called. */

  virtual Boolean is_moving();

  virtual const Area &get_area();

  virtual const Area &get_area_next();
  /* NOTE: Should only be used by the Locator or in a protected context. */

  Boolean is_invisible() {return invisible;}

  virtual Boolean ignore_walls();
  /* NOTE: Override this to make something able to "walk through walls." */

  virtual Vel get_vel();
  const Pos &get_raw_pos() {return rawPos;}
  virtual Dir get_dir();

  virtual Id get_protection();

  void set_vel_next(const Vel &vel) {velNext = vel;}
  void set_vel_next(int zero) {assert (zero == 0); velNext.set_zero();}
  /* EFFECTS: Sets the next velocity for the object to be vel.  Can be called
     multiple times before update, only the last call is used. */

  void set_middle_next(const Pos &pos);
  /* EFFECTS: Sets the middle of pos according to the current (not next) values
     of dir, and area (for size). */
  /* NOTE: May be called before or after act phase. */

  void relocate(const Pos &p);
  /* EFFECTS: Moves the raw position of the Moving to p. Can and must be 
     called outside of Locator::clock. */
  /* REQUIRES: Must be relocated to an open area of the World. */
  /* NOTE: Sets current and next values. */

  virtual void set_mapped_next(Boolean val);

  void set_extra_vel_next(const Vel &vel) 
  {extraVelNext = vel; extraVelNextSet = True;}
  void set_extra_vel_next(int zero) 
    {assert (zero == 0); velNext.set_zero(); extraVelNextSet = True;}

  virtual void set_protection(const Id &);

  void set_invisible_next(Boolean val) {invisibleNext = val;}

  void set_attack_hook(void (*hook)(PhysicalP)) {attackHook = hook;}
  /* NOTE: Overrides previous value if any. */

  void attack_hook() {if (attackHook) attackHook(this);}
  /* EFFECTS: Should be called whenever *this attacks anything. */

  virtual void corporeal_attack(PhysicalP killer,int damage); 
  virtual void heat_attack(PhysicalP,int heat,Boolean secondary = False);

  virtual void act();
  virtual void update();
  virtual void draw(Drawable,Xvars &,int,const Area &);
  virtual void avoid(PhysicalP);
  virtual void collide(PhysicalP);
  virtual void die();


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  Boolean hit_wall() {return hitWall;}
  Boolean hit_wall_next() {return hitWallNext;}

  Dir get_dir_next() {return dirNext;}

  const MovingContext *get_moving_context() {return mc;}
  Vel get_vel_next() {return velNext;}

  virtual int get_anim_time();
  /* EFFECTS: Override this to make objects animate faster or slower.
     Returns the number of turns per frame.  Default is MOVING_ANIM_TIME. */

  void set_vel(const Vel &v) {vel = v;}
  void set_vel(int zero) {assert (zero == 0); vel.set_zero();}

  void set_dir(const Dir &d) {dir = d;}
  /* NOTE: Only used by Lance for initialization. */

  void set_dir_next(const Dir &d) {dirNext = d;}

  void set_raw_pos_next(const Pos &rpos)
    {rawPosNext = rpos; rawPosChanged = True;}

  void update_next();
  /* EFFECTS: Compute areaNext and hitWallNext.  May modify rawPosNext or 
     dirNext. */
  /* NOTE: May be called more than once per turn. */

  virtual void get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			       Dir dir,int animNum);
  /* MODIFIES: pixmap, mask */
  /* NOTE: Only used so that children of Moving can affect Moving's actions. */
  
  virtual void get_size_offset_next(Size &size,Size &offset,Dir dirNext);
  /* MODIFIES: size, offset */
  /* NOTE: Only used so that children of Moving can affect Moving's actions. */

  virtual void init_x(Xvars &);
  /* NOTE: Now called up the tree. */


private:
  Boolean context_valid();
  /* EFFECTS: Returns True if this->cx is valid, False otherwise. */

  float compute_collision(Mass m1,float v1,Mass m2,float v2);


  MovingXdata *movingXdata;
  int movingAnimNum;
  Timer animTimer;
  const MovingContext *mc;
  Pos rawPos,rawPosNext; Boolean rawPosChanged;
  Area area,areaNext;   
  Dir dir,dirNext;
  Vel vel,velNext; 
  Boolean extraVelNextSet;
  Vel extraVel,extraVelNext; // Follows clock in non-standard way.
  Boolean hitWall,hitWallNext;
  Id protection;
  Boolean invisible,invisibleNext; 
  void (*attackHook)(PhysicalP);
};
typedef Moving *MovingP;



////////// Shot
// Parent: Moving

struct ShotContext {
  int damage; 
  Speed speed;
  MovingContext movingContext;
};

typedef MovingXdata ShotXdata ;

class Shot: public Moving {
public:
  Shot(const ShotContext &,ShotXdata &,WorldP,LocatorP,
       const Pos &,const Id &shooter,
       Dir shotDir,Dir movingDir = CO_air);
  
  const Id &get_shooter() {return shooter;}

  virtual Boolean is_shot();

  virtual void avoid(PhysicalP other);
  virtual void collide(PhysicalP other);

  virtual void act();


#ifndef PROTECTED_IS_PUBLIC
 protected:
#endif
  int get_damage() {return context->damage;}


private:
  Id shooter;
  const ShotContext *context;
};



////////// Falling
// Parent: Moving
// Moving with gravity.  Falls until it is blocked by the world.  

struct FallingContext {
  MovingContext movingContext;
};

typedef MovingXdata FallingXdata;

class Falling: public Moving {
public:
  Falling(const FallingContext &h_c,FallingXdata &x_data,
	  WorldP world,LocatorP l,const Pos &rawPos,
	  Dir dirInitial = CO_air);
  
  virtual void act();
};



////////// Touchable
// Parent: Falling
// Remembers if it has ever been touched by a Human controlled Physical.

struct TouchableContext {
  FallingContext fallingContext;
};

typedef FallingXdata TouchableXdata;

class Touchable: public Falling {
 public:
  Touchable(const TouchableContext &,TouchableXdata &x_data,
	    WorldP,LocatorP,const Pos &);

  virtual int get_drawing_level();
  /* EFFECTS:  Touchable is drawn behind everything else. */
  
  Boolean wasTouched() {return touched;}
  /* EFFECTS:  Has a Physical with Human intelligence touched the Xit. */
  
  virtual Boolean collidable();
  virtual void act();


private:
  Boolean touched;
  const TouchableContext *context;
};
typedef Touchable *TouchableP;



//////////// Heavy
// Parent: Falling
// Does damage to things it lands on.

struct HeavyContext {
  int damage;
  FallingContext fallingContext;
};

typedef FallingXdata HeavyXdata;

class Heavy: public Falling {
 public:
  Heavy(const HeavyContext &h_c,
	HeavyXdata &x_data,
	WorldP world,
	LocatorP l,
	const Pos &rawPos);
  
  virtual void collide(PhysicalP);
  /* EFFECTS: Crush things it falls on. */
  

 private:
  const HeavyContext *context;
};



//////////// Item
// Parent: Falling
// 
struct ItemContext {
  Boolean persists; 
  // Provides ranking for weapons/items when switching.  
  // No two weapons should have the same coolness and no two items
  // should have the same coolness.  
  // Negative coolness means that the item should never be automatically 
  // selected.
  int coolness; 
  FallingContext fallingContext;
};
typedef FallingXdata ItemXdata;


class Item: public Falling {
public:
  Item(const ItemContext &c_x,
       ItemXdata &x_data,
       WorldP w,
       LocatorP l,
       const Pos &pos);
  
  Boolean is_item() {return True;}

  Boolean is_held() {return held;}
  
  Boolean can_take() {return canTake.ready() && !held && !cantTake;}
  /* EFFECTS: Returns whether the object can be picked up. */

  virtual int get_drawing_level();
  /* NOTE: Items are at drawing level 1. */

  Id get_holder_id();
  /* EFFECTS: Returns the holder Id if held or an invalid Id if not held.  */

  int get_coolness() {return context->coolness;}

  void set_quiet_death();

  Boolean persists() {return context->persists;}

  virtual void follow_user(const Pos &userMiddle,Dir userDir);

  void taken(PhysicalP);
  /* EFFECTS:  The object has been taken by another Physical. */
  /* NOTE: Changes immediate externally visible state.  Should only be called
     in the collision phase. */

  void dropped(PhysicalP);
  /* EFFECTS:  The object has been dropped by another Physical. */
  /* NOTE: Called by another object in the act phase. */
  /* NOTE: Called in either the act or update phase. */
  /* NOTE: Calls up the tree. */

  virtual void use(PhysicalP);
  /* EFFECTS: p uses *this. */
  /* NOTES: Called by another object in act phase.  Can also be called in
     collide phase. (see AutoUse::collide)  Calls up the tree. */

  virtual void act();
  
  virtual void die();
  
  
#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  void set_cant_take() {cantTake = True;}
  
  
private:
  enum MESSAGE {NONE,USED,DESTROYED};

  Boolean held;
  Id holderId; // Valid iff held.
  Boolean dieMessage;
  Timer canTake;
  Boolean cantTake;
  const ItemContext *context;
};
typedef Item *ItemP;



//////////// AutoUse
// Parent: Item
// Automatically gets used when it collides with a non-user creature.
// WARNING: All children of this class must be able to be used by a non-user.

struct AutoUseContext {
  Boolean humansOnly; // Only automatically used by humans.
  ItemContext itemContext;
};
typedef ItemXdata AutoUseXdata;

class AutoUse : public Item {
public:
  AutoUse(const AutoUseContext &c_x,
	  AutoUseXdata &x_data,
	  WorldP w,
	  LocatorP l,
	  const Pos &pos);

  virtual Boolean is_auto_use();
  
  virtual void collide(PhysicalP);


private:
  const AutoUseContext *context;
};



//////////// Shield
// Parent: AutoUse
struct ShieldContext {
  AutoUseContext autoUseContext;
};
typedef AutoUseXdata ShieldXdata;

class Shield: public AutoUse {
public:
  Shield(const ShieldContext &,ShieldXdata &,
	 WorldP w,LocatorP l,const Pos &pos);
  virtual Boolean is_shield();
  
  virtual void use(PhysicalP);

  
#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  virtual ProtectionP create_protection(const Area&) = 0;
  /* EFFECTS: Create the type of Protection specific to this type of Shield. */
};



//////////// Animated
// Parent: Item
//
struct AnimatedContext {
  char *colorName;
  Size size;
  int animMax[PH_FRAME_MAX];
  char *pixmapBits[PH_FRAME_MAX][PH_ANIM_MAX];
  char *maskBits[PH_FRAME_MAX][PH_ANIM_MAX];
  ItemContext itemContext;
};


class AnimatedXdata {
public:
  AnimatedXdata() {valid = False;}
  Boolean valid;
  Pixmap pixmaps[Xvars::DISPLAYS_MAX][PH_FRAME_MAX][PH_ANIM_MAX],
  masks[Xvars::DISPLAYS_MAX][PH_FRAME_MAX][PH_ANIM_MAX];
  ItemXdata itemXdata;
}; 


class Animated: public Item {
public:
  Animated(const AnimatedContext &,AnimatedXdata &,
	   WorldP,LocatorP,const Pos &);


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  void set_frame(Frame fr) {frame = fr;}
  void set_frame_next(Frame fr) {frameNext = fr;}
  Frame get_frame() {return frame;}
  
  virtual void get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			       Dir dir,int animNum);
  virtual void get_size_offset_next(Size &size,Size &offset,
				    Dir dirNext);
  
  virtual void init_x(Xvars &);
  
  virtual void update();


private:
  Boolean context_valid();

  AnimatedXdata *animatedXdata;
  const AnimatedContext *ac;
  Frame frame,frameNext;
  int animatedAnimNum;
};



//////////// Weapon
// Parent: Item
// 
struct WeaponContext {
  // Defaultable has been superceeded by using negative values for
  // ItemContext::coolness.
  ItemContext itemContext;
};
typedef ItemXdata WeaponXdata;
class Weapon;
typedef Weapon *WeaponP;


class Weapon: public Item {
public:
  Weapon(const WeaponContext &c_x,
	 WeaponXdata &x_data,
	 WorldP w,
	 LocatorP l,
	 const Pos &pos);
  
  Boolean is_weapon() {return True;}

  virtual Boolean ready();
  /* EFFECTS: Can the weapon be fired now. */
  /* NOTE: Sometimes calls up the tree.*/

  //  Boolean defaultable() {return wc->defaultable;}
  /* EFFECTS: Is this a type of weapon that can safely be set automatically as
     the current weapon.  E.g. You do not want to set a soul-swapper as the 
     current weapon unless the user explicitly says so. */

  virtual int get_ammo() = 0;
  virtual int get_ammo_max() = 0;
  /* NOTE: Can return PH_AMMO_UNLIMITED. */

  virtual void fire(const Id &id,ITcommand command);
  /* REQUIRES: command is a weapon command. */
  /* EFFECTS: Fire the weapon according to the specified command.  id is the
     physical firing the weapon.  */

  virtual void enter_scope_next(PhysicalP user);
  virtual void leave_scope_next(PhysicalP user);
  /* NOTE: Called during act(collide) or update phase. Should be just 
     act(). */
  /* NOTE: Calls up the tree. */
  /* NOTE: ok to call multiple times in same turn, but must enter/leave scope
     in proper order. */

  void take_ammo_from(WeaponP other);
  /* EFFECTS: Take as much ammo as possible from the other weapon. */


#ifndef PROTECTED_IS_PUBLIC
 protected:
#endif
  Boolean entered_scope() {return enteredScope;}

  virtual void set_ammo(int) = 0;


private:
  const WeaponContext *wc;
  Boolean enteredScope; // not clocked
};
// typedef Weapon *WeaponP; Defined above.



//////////// Cutter
// Parent: Weapon
// NOTE: Uses CO_center for cutter directly in front of user.
struct CutterContext {
  int damage;  // per turn
  Size offsets[CO_DIR_MAX];  // From User's middle to Cutter's middle.

  char *unheldColorName;
  Size unheldSize;
  char *unheldPixmapBits;
  char *unheldMaskBits;
  WeaponContext weaponContext;
};


class CutterXdata {
 public:
  CutterXdata() {valid = False;}

  Boolean valid;
  Pixmap unheldPixmap[Xvars::DISPLAYS_MAX],unheldMask[Xvars::DISPLAYS_MAX];
  WeaponXdata weaponXdata;
};


class Cutter: public Weapon {
public:
  Cutter(const CutterContext &c_x,CutterXdata &x_data,
	 WorldP w,LocatorP l,const Pos &pos);
  
  virtual Boolean is_cutter();

  virtual Boolean ready();

  virtual int get_ammo();
  virtual int get_ammo_max();

  virtual void get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			       Dir dir,int animNum);
  
  virtual void get_size_offset_next(Size &size,Size &offset,Dir dirNext);

  virtual Boolean ignore_walls();
  /* NOTE: We don't want cutters that are held to get bounced through walls. */

  virtual void set_ammo(int);

  virtual void follow_user(const Pos &,Dir);

  virtual void enter_scope_next(PhysicalP);
  virtual void leave_scope_next(PhysicalP);

  virtual void collide(PhysicalP);

  virtual void update();


#ifndef PROTECTED_IS_PUBLIC
 protected: 
#endif
  virtual void init_x(Xvars &);


private:
  Dir dir_4_from_user_dir(Dir);

  Boolean inScope,inScopeNext;
  Id killerId;  // Valid iff inScope.
  CutterXdata *cutterXdata;
  const CutterContext *context;
};




////////// Creature
// Parent: Moving
// Moving with stances.  Can be affected by gravity.
struct CreatureContext {
  Speed crawlSpeed;
  Speed centerSpeed; 
  Speed airSpeed;
  Speed climbSpeed;
  Speed jump;
  Speed acceleration;
  Health corpseHealth; // A positive number.

  Boolean biological;

  Size deadSize;
  Size deadOffset;
  char *deadPixmapBits;
  char *deadMaskBits;
  
  MovingContext movingContext;
};


struct CreatureXdata
{
  CreatureXdata() {valid = False;}

  Pixmap deadPixmap[Xvars::DISPLAYS_MAX],deadMask[Xvars::DISPLAYS_MAX];
  Boolean valid;
  MovingXdata movingXdata;
};


class Creature: public Moving {
public:
  Creature(const CreatureContext &m_c,
	   CreatureXdata &x_data,
	   WorldP world,
	   LocatorP l,
	   const Pos &rawPos);

  Creature();
  /* NOTE: Should never be called. */

  virtual Boolean is_creature();
  Stance get_stance() {return stance;}
  Boolean can_climb() {return canClimb;}

  virtual ITcommand get_command();
  /* NOTE: Will return ITnone if the Creature is stunned. */

  const Hanging &get_hanging() {return hanging;}
  /* EFFECTS: Get the corner that the object is hanging off of, or CO_AIR if
     not hanging off a corner. */

  const Touching &get_touching_area() {return touching;}
  /* EFFECTS: The object's actual touching.  I.e. according to its area. */

  Grav get_grav() {return grav;}

  Boolean get_alien_immune() {return !alienImmune.ready();}
  /* EFFECTS: Is Creature immune to Alien and Hugger attacks. */

  Boolean stunned() {return !stunTimer.ready();}

  Boolean biological() {return cc->biological;}
  /* EFFECTS: Is the Creature a biological organism.  E.g. as opposed to a 
     machine. */

  Boolean on_door();
  /* NOTE: Convenience function to say whether *this is on top of a door. */

  Boolean get_want_climb() {return wantClimb;}
  void set_want_climb(Boolean val) {wantClimb = val;}
  /* NOTE: Actually, ALL climbing should be in Creature. */

  void stun_next(int time);
  /* EFFECTS: Stun the creature for time turns. */

  void set_alien_immune(int immTime) {alienImmune.set(immTime);}
  /* EFFECTS: Will be immune to alien and hugger attacks for immTime turns. */
  /* NOTE: Immediate. */

  virtual void set_quiet_death();
  /* NOTE: Calling set_quiet_death on a corpse destroys it. */

  virtual void corporeal_attack(PhysicalP,int damage); 
  virtual void heat_attack(PhysicalP,int heat,Boolean secondary); 

  virtual void act();
  virtual void update();
  virtual void die();

  static const Stats &get_stats() {return stats;}


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  Stance get_stance_next() {return stanceNext;}

  const CreatureContext *get_creature_context() {return cc;}

  Touching get_touching_stance();
  /* EFFECTS: The object's touching according to its stance. */

  time_t get_birth_time() {return birthTime;}

  virtual void get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			       Dir dir,int animNum);
  /* MODIFIES: pixmap, mask */
  
  virtual void get_size_offset_next(Size &size,Size &offset,Dir dirNext);
  /* MODIFIES: size, offset */

  Boolean stunned_next() {return stunNext ? True : False;}

  void set_stance(const Stance &st) {stance = st;}

  void set_stance_next(const Stance &stance);
  /* EFFECTS: Sets the next stanceection for the object to be stance.  Can be
     called multiple times before update, only the last call is used. */

  void set_grav_next(const Grav &g)
    {gravNext = g;}
  /* EFFECTS: Sets the pull of gravity for the next turn to be grav. */

  void center_wsquare_x_next(const Loc &loc);
  /* REQUIRES: loc overlaps with the area, stanceNext set to CO_climb */
  /* EFFECTS: Tries to center the x position object on the wsquare at loc.  
     (May be bumped a bit if a wall prevents exact centering.) */

  void corner(const Hanging &hanging);
  /* REQUIRES: stanceNext is set to correspond to h.corner. */
  /* EFFECTS: Tries to move the object around the corner it is hanging off of.
     Note that hanging.corner is the initial corner, not the desired final 
     one. */

  virtual void init_x(Xvars &);


private:
  static Dir compute_dir(const Stance &stance,const Vel &vel);
  /* USES: nothing */
  /* EFFECTS: Return the dir corresponding to stance and vel. */

  Boolean context_valid();

  Touching touching;
  Hanging hanging;
  Boolean canClimb;
  Stance stance,stanceNext; 
  Grav grav,gravNext; 
  const CreatureContext *cc;
  Timer corpseTimer;
  CreatureXdata *creatureXdata;
  Boolean wantClimb;  // actually, ALL climbing should be in Creature.
  //  Id killerId;   Now in Intel.
  time_t birthTime;
  static Stats stats;

  // If non-zero, indicates how many turns Creature should be stunned for.
  int stunNext; 
  Timer stunTimer;
  Timer alienImmune;
};
typedef Creature *CreatureP;



/*//////// Grounded
//  Parent: Creature
Sticks to the ground only.  Affected by gravity.  Can't climb or jump. */
struct GroundedContext 
{
  int dummy;
}; 

struct GroundedXdata {};

class Grounded: public virtual Creature {
 public:
  Grounded(const GroundedContext &,GroundedXdata &);
  
  virtual void act();
  void _act();
};



/*//////// Suicide
// Parent: Creature
Kills self if the command IT_ITEM_USE is seen. */
struct SuicideContext
{
  int dummy;
};

struct SuicideXdata {};

class Suicide: public virtual Creature {
 public:
  Suicide(const SuicideContext &,SuicideXdata &);

  virtual Boolean is_suicide();
  
  virtual void act();
  void _act();
};



////////// Hopping
// Parent: Creature
// 
struct HoppingContext 
{
  int dummy;
}; 

struct HoppingXdata {};

class Hopping: public virtual Creature {
 public:
  Hopping(const HoppingContext &,HoppingXdata &);

  virtual void act();
};



////////// User
// Parent: Creature
// Holds and uses items and weapons.  The items and weapons are made to follow
// the User. 
struct UserContext 
{
  Boolean usesWeapons;
  Boolean usesItems;
};  

struct UserXdata {};


class User: public virtual Creature {
public:
  User(const UserContext &,UserXdata &);

  virtual Boolean is_user();

  virtual int get_weapons_num();
  virtual int get_items_num(); 
  /* NOTE: Returned value is not valid after current turn. */
  
  virtual PhysicalP get_weapon(int);
  virtual PhysicalP get_item(int); 
  virtual PhysicalP get_weapon_current();
  virtual PhysicalP get_item_current();
  /* NOTE: Can return NULL. */

  virtual int get_item_count();

  virtual ClassId get_weapon_string(const char *&str);
  /* EFFECTS: str is set to point to a static string containing the string
     for ClassId. */

  virtual void set_mapped_next(Boolean val);
  /* EFFECTS: Bring current weapon in/out of scope and then change mapped. */

  virtual void act();
  void _act();

  virtual void update();
  void _update();

  virtual void collide(PhysicalP other);
  Boolean _collide(PhysicalP);

  virtual void die();
  void _die();


#ifndef PROTECTED_IS_PUBLIC
 protected:
#endif
  Boolean has_weapon(Weapon **weapon,ClassId classId);
  /* MODIFIES: weapon */
  /* EFFECTS: Like has_weapon(ClassId) except will return the 
     weapon in weapon if not NULL. */


private:
  int coolest_weapon();
  /* EFFECTS: Return index of weapon with highest coolness or weaponsNum if 
     builtIn weapon is coolest.  Only consider weapons with nonNegative 
     coolness. */

  int coolest_item();
  /* EFFECTS: Return index of coolest item or itemsNum. */

  int next_coolest_weapon();
  /* EFFECTS: Return the coolest weapon (or builtIn weapon) that is less cool
     than the current weapon. Can be non-negative coolness.  If none found, 
     return coolest_weapon(). */

  int next_coolest_item();
  /* EFFECTS: Return coolest item less cool than current item.  Cycle around
     to coolest_item() if none found. */

  //  int defaultable_weapon();
  /* REQUIRES: weaponsNum and weapons are properly updated. */
  /* EFFECTS: Returns the index of a wepon in weapons that is defaultable or
     weaponsNum if there is none. */

  void weapon_use(ITcommand command);
  void weapon_change();
  void weapon_drop();
  void item_use();
  void item_change();
  void item_drop();
  /* NOTE: Called in User::_act(). */

  Id weapons[PH_WEAPONS_MAX];
  int weaponsNum;
  // weaponCurrent == weaponsNum if using builtIn weapon or no weapon.
  int weaponCurrent; 
  // If weaponCycleTimer is not ready(), cycle to next weapon on IT_WEAPON_CHANGE.
  // If ready(), start again from coolest_weapon().
  Timer weaponCycleTimer;
  //  Memory of previous weapon dir is now in ui.

  Id items[PH_ITEMS_MAX];
  int itemsNum;
  // itemCurrent == itemsNum if no selected item.
  int itemCurrent; 
  // Like weaponCycleTimer.
  Timer itemCycleTimer;
  // How many of the current item the User has.
  int itemCount,itemCountNext;
  
  const UserContext *context;
};
typedef User *UserP;



////////// Fighter
// Parent: Creature
// Can attack in different directions.
struct FighterContext {
  char *colorName;
  int slide;
  int jumpHorizontal;
  int jumpVertical;
  int damageStuck;
  int damageFree;

  Size sizes[CO_DIR_MAX];
  Size offsets[CO_DIR_MAX];
  Size hotSpots[CO_DIR_MAX];  // Must add in offset to use.
  char *pixmapBits[CO_DIR_MAX];
  char *maskBits[CO_DIR_MAX];
};


class FighterXdata {
public:
  FighterXdata() {valid = False;}

  Boolean valid;
  Pixmap pixmaps[Xvars::DISPLAYS_MAX][CO_DIR_MAX],
  masks[Xvars::DISPLAYS_MAX][CO_DIR_MAX];
};


class Fighter: public virtual Creature {
public:
  Fighter(const FighterContext &f_c,FighterXdata &x_data);

  virtual Boolean is_fighter();

  virtual void get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			       Dir dir,int animNum);
  
  virtual void get_size_offset_next(Size &size,Size &offset,Dir dirNext);

  virtual void act();
  void _act();

  virtual void update();
  void _update();

  virtual void collide(PhysicalP other);
  Boolean _collide(PhysicalP);


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  virtual void init_x(Xvars &);


private:
  Attack dir_to_attack(Dir dir);

  void attack_stuck(Dir dir,Stance stance);
  void attack_free_horizontal(Dir dir);
  /* NOTE: Does not zero vertical velocity. */

  void attack_free_vertical(Dir dir);
  /* NOTE: Vertical velocity only. */

  FighterXdata *fighterXdata;
  const FighterContext *fc;
  Attack attack, attackNext;
  Timer stuckTimer;
};



////////// Walking
// Parent: Creature
// Sticks to the ground only.  Affected by gravity.
struct WalkingContext 
{
  int dummy;
}; 

struct WalkingXdata {};

class Walking: public virtual Creature {
public:
  Walking(const WalkingContext &,WalkingXdata &);

  virtual Boolean is_walking() {return True;}

  virtual void act();
  void _act();


private:
  Pos rawPosPrev;
  Vel velPrev;
};



////////// Sticky
// Parent: Creature
// Sticks to and walks on flat surfaces in four directions.  Affected by 
// gravity.
struct StickyContext 
{
  int dummy;
};  

struct StickyXdata {};

class Sticky: public virtual Creature {
public:
  Sticky(const StickyContext &,StickyXdata &);
  
  virtual Boolean is_sticky();

  virtual void act();
  void _act();


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  Boolean want_corner(const Corner &corner);
  /* EFFECTS: Returns True if the object should go around the given corner,
     False otherwise.  corner is the initial, not final corner.*/

  Stance cornered_stance(const Hanging &h);
  /* EFFECTS: Returns the stance to take in order to go around the corner
     of h. */

  void set_want_corner(const Corner &c1) 
    {wantCorner1 = c1; wantCorner2 = CO_air;}
  void set_want_corner(const Corner &c1,const Corner &c2) 
    {wantCorner1 = c1; wantCorner2 = c2;}
  /* EFFECTS: Tell the object that it should go around a corner if possible.
     The given values are in effect until changed. */
  /* NOTE: set_want_corner(CO_air) should be used to disable cornering. */


private:
  Corner wantCorner1,wantCorner2;
};



////////// Flying
// Parent: Creature
// Floats around.  Can walk on the ground.
struct FlyingContext 
{
  int gravTime;
};  

struct FlyingXdata {};

class Flying: public virtual Creature {
public:
  Flying(const FlyingContext &,FlyingXdata &);
  
  virtual Boolean is_flying();

  virtual void act();
  void _act();


private:
  Timer gravTimer;
  const FlyingContext *context;
};



////////// BuiltIn
// Parent: Creature
// Has a directional built in weapon.
struct BuiltInContext 
{
  int shotTime;
  ClassId weaponClassId;
  const char *weaponStr;
  int coolness; // of the builtIn weapon.
};  

struct BuiltInXdata {};

class BuiltIn: public virtual Creature {
public:
  BuiltIn(const BuiltInContext &,BuiltInXdata &);
  
  virtual Boolean is_built_in();

  virtual ClassId get_weapon_string(const char *&str);
  /* EFFECTS: str is set to point to a static string containing the string
     for ClassId. */

  virtual int get_built_in_weapon_coolness();

  virtual Boolean ready();
  /* EFFECTS: Is the BuiltIn ready to fire. */

  virtual void act();
  void _act();


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  virtual Dir compute_weapon_dir(ITcommand);
  /* EFFECTS:  Returns the direction the weapon should be fired, 
     {CO_R ... CO_UP_R}, or CO_air. */

  virtual Size get_shot_size(Dir) = 0;

  virtual PhysicalP create_shot(const Pos &,Dir) = 0;


private:
  Timer shotTimer;
  const BuiltInContext *context;
};
typedef BuiltIn *BuiltInP;



////////// Hugger
// Parent: Creature
// Hugs onto any creature it collides with (except A_Alien and A_Hugger)
// and stuns them, then turns them into an Alien.
struct HuggerContext 
{
  // Which intel, the hugger's or the huggee's intel is implanted in the new
  // Alien.
  Boolean useHuggeeIntel; 
};  

struct HuggerXdata {};

class Hugger: public virtual Creature {
public:
  Hugger(const HuggerContext &,HuggerXdata &);

  virtual Boolean is_hugger();

  virtual int get_drawing_level();

  virtual void act();
  void _act();

  virtual void collide(PhysicalP other);


private:
  Id huggeeId;
  const HuggerContext *context;
};

#endif


