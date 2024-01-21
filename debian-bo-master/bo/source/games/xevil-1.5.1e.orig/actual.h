// "actual.h"
// Actual objects to be instantiated.

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

#ifndef ACTUAL_H
#define ACTUAL_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
#include "utils.h"
#include "coord.h"
#include "world.h"
#include "locator.h"
#include "physical.h"



// Class Declarations
class Explosion: public Physical {
 public:
  Explosion(WorldP w,LocatorP l,const Pos &middle,const Id &bomber,int radius,
	    int damageMax);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);
  
  virtual Boolean collidable();
  virtual const Area &get_area();
  virtual const Area &get_area_next();
  virtual void draw(Drawable buffer,Xvars &xvars,int,const Area &area);
  virtual void act();

  const static PhysicalContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  Id bomber;
  Area area; 
  Pos middle; // Same as in area.
  int radius;
  int damageMax;
};



class Fire: public Physical {
public:
  Fire(WorldP w,LocatorP l,const Pos &middle,Boolean coll = True);
  /* NOTE: If coll is False, the Fire will not collide with anything.
     I.e. Just for decoration. */
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);
  
  static int get_damage();
  virtual const Area &get_area();
  virtual const Area &get_area_next();

  virtual Boolean collidable();

  virtual void draw(Drawable buffer,Xvars &xvars,int,const Area &area);

  virtual void avoid(PhysicalP other);
  virtual void collide(PhysicalP other);
  virtual void corporeal_attack(PhysicalP,int);
  virtual void heat_attack(PhysicalP,int,Boolean);

  virtual void act();
  virtual void update();

  const static PhysicalContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  Boolean isCollidable;

  //  static PhysicalXdata xdata;
  Area area,areaNext;
};



// Protects from N_PROTECTION_N corporeal attacks.
class NProtection: public Protection {
public:
  NProtection(WorldP,LocatorP,const Area &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static ProtectionContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual Boolean corporeal_protect(int);
  virtual Boolean heat_protect(int heat,Boolean);

  int n; // Number of hits that it can still take.
  static ProtectionXdata xdata;
};



// Protects from heat or corporeal attacks for T_PROTECTION_TIME turns.
class TProtection: public Protection {
public:
  TProtection(WorldP,LocatorP,const Area &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void act();

  const static ProtectionContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual Boolean corporeal_protect(int);
  virtual Boolean heat_protect(int heat,Boolean);

  Timer timer;
  static ProtectionXdata xdata;
};



class Rock: public Heavy {
 public:
  Rock(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static HeavyContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static HeavyXdata xdata;
};



class Weight: public Heavy {
 public:
  Weight(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static HeavyContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static HeavyXdata xdata;
};



class AltarOfSin: public Heavy {
 public:
  AltarOfSin(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void corporeal_attack(PhysicalP,int); 

  virtual void heat_attack(PhysicalP,int,Boolean);

  virtual void collide(PhysicalP);

  virtual void update();

  const static HeavyContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  Boolean turnTaken; // Only do one interesting thing per turn.

  static HeavyXdata xdata;
};



class Doppel: public AutoUse {
  // Not a AutoUse because Lemmings and other funny things get doppelganged.
 public:
  Doppel(WorldP,LocatorP,const Pos &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);
  
  virtual void use(PhysicalP);
  /* NOTE: Only certain classes may be used as doppelgangers.  If the
     User is not of one of these classes, the doppelganger class will
     be chosen randomly. */

  static const Stats &get_stats() {return stats;}

  const static AutoUseContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  PhysicalP create_physical(const Area &,PhysicalP original);
  /* EFFECTS: Create a new Physical which is of the same class as original. */

  static AutoUseXdata xdata;
  static Stats stats;
};



class Cloak: public AutoUse {
public:
  Cloak(WorldP,LocatorP,const Pos &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void use(PhysicalP);

  static const Stats &get_stats() {return stats;}

  const static AutoUseContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  static void attack_hook(PhysicalP p);

  static AutoUseXdata xdata;
  static Stats stats;
};  



class Transmogifier: public AutoUse {
 public:
  Transmogifier(WorldP,LocatorP,const Pos &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);
  
  virtual void use(PhysicalP);

  static const Stats &get_stats() {return stats;}

  const static AutoUseContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  PhysicalP new_physical(const Pos &middle);
  /* EFFECTS:  Create a new randomly chosen Physical centered at middle
     for the object using the Transmogifier. */

  static Boolean transmogify_target_filter(const PhysicalContext *);
  /* EFFECTS: Filter those classes whose transmogifyTarget flag is True. */

  static AutoUseXdata xdata;
  static Stats stats;
};
  


class MedKit: public AutoUse {
 public:
  MedKit(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void use(PhysicalP);

  static const Stats &get_stats() {return stats;}

  const static AutoUseContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static AutoUseXdata xdata;
  static Stats stats;
};



class NShield: public Shield {
public:
  NShield(WorldP,LocatorP,const Pos &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void use(PhysicalP);

  static const Stats &get_stats() {return stats;}

  const static ShieldContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */

  
private:
  virtual ProtectionP create_protection(const Area &);

  static ShieldXdata xdata;
  static Stats stats;
};



class TShield: public Shield {
public:
  TShield(WorldP,LocatorP,const Pos &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void use(PhysicalP);

  static const Stats &get_stats() {return stats;}

  const static ShieldContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual ProtectionP create_protection(const Area &);

  static ShieldXdata xdata;
  static Stats stats;
};



class Bomb: public Animated {
 public:
  Bomb(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void use(PhysicalP p);
  /* NOTE: p can be NULL. */

  virtual void act();

  virtual Boolean is_bomb();

  virtual void set_quiet_death();

  virtual void die();
  /* EFFECTS: Create an explosion. */

  static const Stats &get_stats() {return stats;}

  const static AnimatedContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static AnimatedXdata xdata;
  Timer timer;
  Boolean active;
  Id bomber; // Only valid if active.
  Frame frame; 
  Boolean defused;
  static Stats stats;
};



class Shell: public Shot {
 public:
  Shell(WorldP w,LocatorP l,const Pos &pos,const Id &shooter,
	Dir dir);
  static Size get_size();  // doubles as get_size_max().
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  static const Stats &get_stats() {return stats;}

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static ShotXdata xdata;
  static Stats stats;
};



class SwapShell: public Shot {
 public:
  SwapShell(WorldP w,LocatorP l,const Pos &pos,
	    const Id &shooter,const Id& swapper,Dir dir);
  /* NOTE: shooter is the person who fired it.  swapper is the swapper the 
     shell came from. */
  static Size get_size();  // Doubles as get_size_max().
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void collide(PhysicalP);
  /* NOTE: Does not call up the tree. */

  static const Stats &get_stats() {return stats;}

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static ShotXdata xdata;
  Id swapper;
  static Stats stats;
};



class Lance: public Shot {
 public:
  Lance(WorldP w,LocatorP l,const Pos &pos,const Id &shooter,
	Dir dir);
  static Size get_size(Dir);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  static Dir compute_weapon_dir(ITcommand);
  /* EFFECTS: Weapons firing a Lance shoot diagonal shots different than
     most weapons. */

  static const Stats &get_stats() {return stats;}

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static ShotXdata xdata;
  static Stats stats;
};



class Laser: public Shot {
 public:
  Laser(WorldP w,LocatorP l,const Pos &pos,const Id &shooter,
	Dir dir);
  static Size get_size(Dir);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  static Dir compute_weapon_dir(ITcommand);
  /* EFFECTS: Weapons firing a Laser shoot diagonal shots different than
     most weapons. */

  static const Stats &get_stats() {return stats;}

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static ShotXdata xdata;
  static Stats stats;
};



class FrogShell: public Shot {
 public:
  FrogShell(WorldP w,LocatorP l,const Pos &pos,const Id &shooter,
	    const Id &frogGun,Dir dir);
  static Size get_size(); // Doubles as get_size_max().
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void collide(PhysicalP);
  /* NOTE: Does not call up the tree. */

  static const Stats &get_stats() {return stats;}

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static ShotXdata xdata;
  Id frogGun;
  static Stats stats;
};



class Fireball: public Shot {
 public:
  Fireball(WorldP w,LocatorP l,const Pos &pos,const Id &shooter,
	   Dir dir,int heat = -1,int time = -1);
  /* NOTE: Will use heat and time instead of FIREBALL_HEAT and FIREBALL_TIME
     if they are specified. */
  static Size get_size(); // Doubles as get_size_max().
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void draw(Drawable buffer,Xvars &xvars,int,const Area &area);
  virtual void collide(PhysicalP other);
  virtual void act();

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static ShotXdata xdata;
  Timer timer;
  int heat;
};



// Only seeks after creatures.
class Missile: public Shot {
 public:
  Missile(WorldP w,LocatorP l,const Pos &pos,const Id &shooter,Dir dir);
  static Size get_size(Dir);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void act();

  static const Stats &get_stats() {return stats;}

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  Boolean hasTarget;
  Id targetId;
  Timer timer;
  Timer rotate;
  Id shooterId;

  static ShotXdata xdata;
  static Stats stats;
};



class Star: public Shot {
 public:
  Star(WorldP w,LocatorP l,const Pos &pos,const Id &shooter,
	Dir dir);
  static Size get_size(); // Doubles as get_size_max.
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  static const Stats &get_stats() {return stats;}

  const static ShotContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static ShotXdata xdata;
  static Stats stats;
};



class Trapdoor: public Moving {
public:
  enum {LEMMINGS_MAX = 40};

  Trapdoor(WorldP w,LocatorP l,const Pos &pos,const Id &homeId);
  static Size get_size(); // Doubles as get_size_max().
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean collidable();

  virtual int get_drawing_level();
  /* NOTE: Trapdoor drawn behind everything else. */

  int get_lemmings_out(IntelId *&lem) {lem = lemmings; return lemmingsNum;}
  /* EFFECTS: Set lem to be an array of all the IntelIds of the lemmings that 
     have been generated by this trapdoor,  i.e are "out".  Return the size of
     the array. */

  virtual void act();

  const static MovingContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  Timer timer;
  IntelId lemmings[LEMMINGS_MAX];
  int lemmingsNum;
  Id homeId; // Generated lemmings will seek this Home.
  
  static MovingXdata xdata;
};
typedef Trapdoor *TrapdoorP;



class Home: public Moving {
public:
  Home(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size(); // Doubles as get_size_max().
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean collidable();


  virtual int get_drawing_level();
  /* NOTE: Home drawn behind everything else. */

  int get_lemmings_safe() {return lemmingsSafe;}

  virtual void act();

  const static MovingContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  int lemmingsSafe;
  
  static MovingXdata xdata;
};
typedef Home *HomeP;



class Grenade: public Falling {
public:
  Grenade(WorldP w,LocatorP l,const Pos &pos,const Id &sh,
	  Dir dir,Speed speed);
  Grenade(WorldP,LocatorP,const Pos &,const Id &,const Vel &);
  static Size get_size(Dir dir);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void set_quiet_death();

  virtual void act();

  virtual void die();

  static const Stats &get_stats() {return stats;}

  const static FallingContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  Timer timer;
  Id shooter;
  Boolean defused;

  static FallingXdata xdata;
  static Stats stats;
};



class Egg: public Falling {
public:
  Egg(WorldP w,LocatorP l,const Pos &pos,
      const IntelOptions &ops,ITmask opMask);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void act();

  const static FallingContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  Boolean open;
  
  // The intel options for the new Hugger.  Should be the same as for an
  // Alien.
  IntelOptions intelOptions;
  ITmask intelOpMask;

  static FallingXdata xdata;
};



// NOTE: Called Xit instead of Exit to avoid naming conflicts with the C
// function exit().
class Xit: public Touchable {
public:
  Xit(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static TouchableContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */

  
 private:
  static TouchableXdata xdata;
};



class Flag: public Touchable {
public:
  Flag(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static TouchableContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */

  
 private:
  static TouchableXdata xdata;
};



class Chainsaw: public Cutter {
 public:
  Chainsaw(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static CutterContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static CutterXdata xdata;
};



//////////// Gun
// Parent: Weapon
// Shoots shells.  Has timer between allowed shots.
struct GunContext {
  int shotTime;
  int ammoInitial;
  int ammoMax;  
  WeaponContext weaponContext;
};
typedef WeaponXdata GunXdata;


class Gun: public Weapon {
public:
  Gun(const GunContext &,GunXdata &,WorldP,LocatorP,const Pos &);

  virtual Boolean is_gun();
  virtual Boolean ready();
  int get_ammo();
  int get_ammo_max();

  virtual void update();


#ifndef PROTECTED_IS_PUBLIC
protected:
#endif
  void set_ammo(int);
  void set_shot_timer() {timer.set();}

  virtual Size get_shot_size(Dir);

  virtual Dir compute_weapon_dir(ITcommand c);
  /* EFFECTS:  Returns the direction the weapon should be fired, 
     {CO_R ... CO_UP_R}, or CO_air. */

  virtual void fire(const Id &id,ITcommand command);
  void _fire(const Id &id,Dir dir,Boolean setTimer = True,
	     Boolean costsAmmo = True);
  /* NOTE: Only the first of these two functions should be overridden. */
  /* EFFECTS: Gun is fired by the shooter id with ITcommand command or in Dir
     dir.  setTimer says whether or not to force a delay between shots. 
     costsAmmo says whether a unit of ammo is subtracted for the shot.*/
  
  virtual PhysicalP create_shot(PhysicalP shooter,WorldP world,
				LocatorP locator,const Pos &pos,Dir dir);
  /* EFFECTS: Create the object to be fired out of the gun.  E.g. shell, 
     swap_shell. shooter is the object firing the gun. */


private:
  Timer timer;
  int ammo;
  const GunContext *gc;
};



class Pistol: public Gun {
 public:
  Pistol(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static GunXdata xdata;
};



// Machine Gun.
class MGun: public Gun {
 public:
  MGun(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static GunXdata xdata;
};



class Lancer: public Gun {
 public:
  Lancer(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  virtual Size get_shot_size(Dir);
  virtual Dir compute_weapon_dir(ITcommand);
  virtual PhysicalP create_shot(PhysicalP,WorldP,LocatorP,const Pos &,Dir dir);

  static GunXdata xdata;
};



class FThrower: public Gun {
 public:
  FThrower(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual PhysicalP create_shot(PhysicalP,WorldP,LocatorP,const Pos &,Dir dir);

  virtual void fire(const Id &,ITcommand);

  virtual void act();
  
  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  virtual Size get_shot_size(Dir);

  Timer stopFiring;
  Id killerId; // Valid iff isFiring.
  ITcommand fireCommand;   // Valid iff isFiring.
  Boolean isFiring;

  static GunXdata xdata;
};



class Launcher: public Gun {
 public:
  Launcher(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual Size get_shot_size(Dir);
  virtual PhysicalP create_shot(PhysicalP,WorldP,LocatorP,const Pos &,Dir dir);

  static GunXdata xdata;
};



class Grenades: public Gun {
 public:
  Grenades(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual Size get_shot_size(Dir);
  virtual PhysicalP create_shot(PhysicalP,WorldP,LocatorP,const Pos &,Dir dir);

  static GunXdata xdata;
};



#if 0
class Shotgun: public Gun {
 public:
  Shotgun(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  void fire(const Id &,ITcommand);

  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  static GunXdata xdata;
};
#endif



class Stars: public Gun {
 public:
  Stars(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static GunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  virtual Size get_shot_size(Dir);
  virtual PhysicalP create_shot(PhysicalP,WorldP,LocatorP,const Pos &,Dir dir);
  
  virtual void fire(const Id &id,ITcommand command);

  static GunXdata xdata;
};



//////////// SingleGun
// Parent: Gun
struct SingleGunContext {
  GunContext gunContext;
};
typedef GunXdata SingleGunXdata;


class SingleGun: public Gun {
public:
  SingleGun(const SingleGunContext &,SingleGunXdata &,
	    WorldP,LocatorP,const Pos &);

  virtual Boolean ready();
  

private:
  virtual void fire(const Id &id,ITcommand command);
  
  Id shotId;
};



class Swapper: public SingleGun {
 public:
  Swapper(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static SingleGunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual Size get_shot_size(Dir);
  virtual PhysicalP create_shot(PhysicalP,WorldP,LocatorP,const Pos &,Dir dir);

  static SingleGunXdata xdata;
};



class FrogGun: public SingleGun {
 public:
  FrogGun(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  const static SingleGunContext context;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual Size get_shot_size(Dir);
  virtual PhysicalP create_shot(PhysicalP,WorldP,LocatorP,const Pos &,Dir dir);

  static SingleGunXdata xdata;
};



class Enforcer: public virtual Creature, public Grounded, public Suicide {
 public:
  Enforcer(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_suicide();

  virtual Boolean prickly();

  virtual void collide(PhysicalP p);

  virtual void act();

  static const Stats &get_stats() {return stats;}

  virtual void die();

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  const static GroundedContext groundedContext;
  const static SuicideContext suicideContext;
  static CreatureXdata creatureXdata;
  static GroundedXdata groundedXdata;
  static SuicideXdata suicideXdata;
  static Stats stats;
};



class Frog: public virtual Creature, public Hopping {
 public:
  Frog(WorldP w,LocatorP l,const Pos &pos,const Id &unmapped);
  /* NOTE: Can create a frog with no unmapped by giving an invalid Id. */
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual void act();

  virtual void die();
  /* EFFECTS: If unmapped exists, have it die where the frog is. 
     Otherwise, let Creature leave a dead frog. */

  static const Stats &get_stats() {return stats;}

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  const static HoppingContext hoppingContext;
  static CreatureXdata creatureXdata;
  static HoppingXdata hoppingXdata;

  Boolean unmappedSet;
  Id unmapped;
  Timer timer; // Only meaningful if unmappedSet.
  static Stats stats;
};



class Hero: public virtual Creature, public Walking, public Fighter, 
public User 
{
 public:
  Hero(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_fighter();
  virtual Boolean is_walking();
  virtual Boolean is_user();

  virtual void get_pixmap_mask(int dpyNum,Pixmap &,Pixmap &,Dir,int);
  virtual void get_size_offset_next(Size &size,Size &offset,Dir dirNext);

  virtual int get_weapons_num();
  virtual int get_items_num(); 
  virtual PhysicalP get_weapon(int);
  virtual PhysicalP get_item(int); 
  virtual PhysicalP get_weapon_current();
  virtual PhysicalP get_item_current();
  virtual int get_item_count();
  virtual void set_mapped_next(Boolean val);

  virtual ClassId get_weapon_string(const char *&str);

  virtual void heal();
  
  virtual void act();

  virtual void update();
  
  virtual void die();

  virtual void collide(PhysicalP other);

  static const Stats &get_stats() {return stats;}
  
  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  Timer healTimer;
  Health healthMin;

  virtual void init_x(Xvars &);

  static CreatureXdata creatureXdata;
  static WalkingXdata walkingXdata;
  static FighterXdata fighterXdata;
  static UserXdata userXdata;

  const static WalkingContext walkingContext;
  const static FighterContext fighterContext;
  const static UserContext userContext;
  static Stats stats;
};



class Ninja: public virtual Creature, public Sticky, public Fighter, 
public User {
 public:
  Ninja(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);
  
  virtual Boolean is_fighter();
  virtual Boolean is_sticky();
  virtual Boolean is_user();

  virtual void get_pixmap_mask(int dpyNum,Pixmap &,Pixmap &,Dir,int);
  virtual void get_size_offset_next(Size &size,Size &offset,Dir dirNext);

  virtual int get_weapons_num();
  virtual int get_items_num(); 
  virtual PhysicalP get_weapon(int);
  virtual PhysicalP get_item(int); 
  virtual PhysicalP get_weapon_current();
  virtual PhysicalP get_item_current();
  virtual int get_item_count();
  virtual void set_mapped_next(Boolean val);

  virtual ClassId get_weapon_string(const char *&str);
  
  virtual void act();

  virtual void update();
  
  virtual void die();

  virtual void collide(PhysicalP other);
  
  static const Stats &get_stats() {return stats;}

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


private:
  virtual void init_x(Xvars &);

  static CreatureXdata creatureXdata;
  static StickyXdata stickyXdata;
  static FighterXdata fighterXdata;
  static UserXdata userXdata;
  
  const static StickyContext stickyContext;
  const static FighterContext fighterContext;
  const static UserContext userContext;
  static Stats stats;
};



class Alien: public virtual Creature, public Sticky {
 public:
  Alien(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_sticky();

  virtual Boolean prickly();

  virtual void collide(PhysicalP p);

  virtual void act();

  virtual void die();

  static const Stats &get_stats() {return stats;}

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  const static StickyContext stickyContext;
  static CreatureXdata creatureXdata;
  static StickyXdata stickyXdata;
  static Stats stats;
};



class RedHugger: public virtual Creature, public Sticky, public Hugger {
 public:
  RedHugger(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_sticky();
  virtual Boolean is_hugger();

  virtual void act();

  virtual void collide(PhysicalP other);

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */

  
 private:
  const static StickyContext stickyContext;
  const static HuggerContext huggerContext;

  static CreatureXdata creatureXdata;
  static StickyXdata stickyXdata;
  static HuggerXdata huggerXdata;
};



class GreenHugger: public virtual Creature, public Sticky, public Hugger {
 public:
  GreenHugger(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_sticky();
  virtual Boolean is_hugger();

  virtual void act();

  virtual void collide(PhysicalP other);

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */

  
 private:
  const static StickyContext stickyContext;
  const static HuggerContext huggerContext;

  static CreatureXdata creatureXdata;
  static StickyXdata stickyXdata;
  static HuggerXdata huggerXdata;
};



class ChopperBoy: public virtual Creature, public Flying, public User {
 public:
  ChopperBoy(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_flying();

  virtual Boolean is_user();
  virtual int get_weapons_num();
  virtual int get_items_num(); 
  virtual PhysicalP get_weapon(int);
  virtual PhysicalP get_item(int); 
  virtual PhysicalP get_weapon_current();
  virtual PhysicalP get_item_current();
  virtual int get_item_count();
  virtual void set_mapped_next(Boolean val);
  
  virtual ClassId get_weapon_string(const char *&str);

  virtual void act();

  virtual void update();
  
  virtual void die();

  virtual void collide(PhysicalP other);

  static const Stats &get_stats() {return stats;}
  
  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */

 private:
  static CreatureXdata creatureXdata;
  static FlyingXdata flyingXdata;
  static UserXdata userXdata;
  
  const static FlyingContext flyingContext;
  const static UserContext userContext;
  static Stats stats;
};



class Lemming: public virtual Creature, public Grounded, public Suicide {
 public:
  Lemming(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_suicide();

  virtual void act();

  static const Stats &get_stats() {return stats;}

  virtual void die();

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


 private:
  const static GroundedContext groundedContext;
  const static SuicideContext suicideContext;
  static CreatureXdata creatureXdata;
  static GroundedXdata groundedXdata;
  static SuicideXdata suicideXdata;
  static Stats stats;
};



class FireDemon: public virtual Creature, public Flying, public BuiltIn {
 public:
  FireDemon(WorldP w,LocatorP l,const Pos &pos);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);
  
  virtual Boolean is_flying();

  virtual Boolean is_built_in();

  virtual ClassId get_weapon_string(const char *&str);

  virtual Boolean ready();

  virtual void heat_attack(PhysicalP,int heat,Boolean secondary = False);

  virtual Boolean swap_protect();

  virtual Boolean frog_protect();

  virtual void act();

  virtual void collide(PhysicalP other);

  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */
  
  
#ifndef PROTECTED_IS_PUBLIC
 protected:
#endif
  virtual Size get_shot_size(Dir);

  virtual PhysicalP create_shot(const Pos &,Dir);
 
 
 private:
  int swapResistance;
  
  static CreatureXdata creatureXdata;
  static FlyingXdata flyingXdata;
  static BuiltInXdata builtInXdata;
  
  const static FlyingContext flyingContext;
  const static BuiltInContext builtInContext;
};



class Walker: public virtual Creature, public Walking, public User,
public BuiltIn
{
public:
  Walker(WorldP,LocatorP,const Pos &);
  static Size get_size_max();
  static PhysicalP create(WorldP,LocatorP,const Pos &);

  virtual Boolean is_walking();
  virtual Boolean is_user();
  virtual Boolean is_built_in();
  
  virtual int get_weapons_num();
  virtual int get_items_num(); 
  virtual PhysicalP get_weapon(int);
  virtual PhysicalP get_item(int); 
  virtual PhysicalP get_weapon_current();
  virtual PhysicalP get_item_current();
  virtual int get_item_count();
  virtual void set_mapped_next(Boolean val);

  virtual ClassId get_weapon_string(const char *&str);

  virtual void act();

  virtual void update();
  
  virtual void die();

  virtual void collide(PhysicalP other);

  static const Stats &get_stats() {return stats;}
  
  const static CreatureContext creatureContext;
  /* NOTE: Making this public is a bit of a hack.  Locator::register_contexts()
     needs to get to it and I don't want to have to make a separate accessor
     function for every class. */


#ifndef PROTECTED_IS_PUBLIC
 protected:
#endif
  virtual int get_anim_time();

  virtual Dir compute_weapon_dir(ITcommand);

  virtual Size get_shot_size(Dir);

  virtual PhysicalP create_shot(const Pos &,Dir);

  
private:
  static CreatureXdata creatureXdata;
  static WalkingXdata walkingXdata;
  static BuiltInXdata builtInXdata;
  static UserXdata userXdata;
  
  const static WalkingContext walkingContext;
  const static BuiltInContext builtInContext;
  const static UserContext userContext;
  static Stats stats;
};
#endif
