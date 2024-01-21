// "actual.C"
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

#ifndef NO_PRAGMAS
#pragma implementation "actual.h"
#endif


// Include Files
#include <strstream.h>

#include "utils.h"
#include "coord.h"
#include "world.h"
#include "physical.h"
#include "actual.h"

// Bitmap files.  Also contain constants and some simple methods.
// Most importantly, they contain the "contexts".
#include "bitmaps/explosion/explosion.bitmaps"
#include "bitmaps/fire/fire.bitmaps"
#include "bitmaps/n_protection/n_protection.bitmaps"
#include "bitmaps/t_protection/t_protection.bitmaps"

#include "bitmaps/shell/shell.bitmaps"
#include "bitmaps/swap_shell/swap_shell.bitmaps"
#include "bitmaps/lance/lance.bitmaps"
#include "bitmaps/laser/laser.bitmaps"
#include "bitmaps/frog_shell/frog_shell.bitmaps"
#include "bitmaps/fireball/fireball.bitmaps"
#include "bitmaps/missile/missile.bitmaps"
#include "bitmaps/star/star.bitmaps"

#include "bitmaps/trapdoor/trapdoor.bitmaps"
#include "bitmaps/home/home.bitmaps"
#include "bitmaps/grenade/grenade.bitmaps"
#include "bitmaps/egg/egg.bitmaps"
#include "bitmaps/xit/xit.bitmaps"
#include "bitmaps/flag/flag.bitmaps"

#include "bitmaps/rock/rock.bitmaps"
#include "bitmaps/weight/weight.bitmaps"
#include "bitmaps/altar_of_sin/altar_of_sin.bitmaps"
#include "bitmaps/doppel/doppel.bitmaps"
#include "bitmaps/cloak/cloak.bitmaps"
#include "bitmaps/transmogifier/transmogifier.bitmaps"
#include "bitmaps/med_kit/med_kit.bitmaps"
#include "bitmaps/n_shield/n_shield.bitmaps"
#include "bitmaps/t_shield/t_shield.bitmaps"
#include "bitmaps/bomb/bomb.bitmaps"

#include "bitmaps/chainsaw/chainsaw.bitmaps"
#include "bitmaps/pistol/pistol.bitmaps"
#include "bitmaps/m_gun/m_gun.bitmaps"
#include "bitmaps/lancer/lancer.bitmaps"
#include "bitmaps/f_thrower/f_thrower.bitmaps"
#include "bitmaps/launcher/launcher.bitmaps"
#include "bitmaps/grenades/grenades.bitmaps"
//#include "bitmaps/shotgun/shotgun.bitmaps"
#include "bitmaps/stars/stars.bitmaps"
#include "bitmaps/swapper/swapper.bitmaps"
#include "bitmaps/frog_gun/frog_gun.bitmaps"

#include "bitmaps/enforcer/enforcer.bitmaps"
#include "bitmaps/frog/frog.bitmaps"
#include "bitmaps/hero/hero.bitmaps"
#include "bitmaps/ninja/ninja.bitmaps"
#include "bitmaps/alien/alien.bitmaps"
#include "bitmaps/hugger/hugger.bitmaps"
#include "bitmaps/chopper_boy/chopper_boy.bitmaps"
#include "bitmaps/lemming/lemming.bitmaps"
#include "bitmaps/fire_demon/fire_demon.bitmaps"
#include "bitmaps/walker/walker.bitmaps"



Explosion::Explosion(WorldP w,LocatorP l,const Pos &mid,
		     const Id &bomb_er,int r,int dMax)
     : Physical(context,w,l) 
{ 
  bomber = bomb_er;
  Pos pos;
  pos.x = mid.x - EXPLOSION_VISIBLE_RADIUS;
  pos.y = mid.y - EXPLOSION_VISIBLE_RADIUS;

  Size size;
  size.width = size.height = 2 * EXPLOSION_VISIBLE_RADIUS;

  Area narea(AR_RECT,pos,size);
  area = narea;
  middle = mid;
  radius = r;
  damageMax = dMax;
}



Boolean Explosion::collidable()
{
  return False;
}



const Area &Explosion::get_area()
{
  return area;
}



const Area &Explosion::get_area_next()
{
  return area;
}



void Explosion::draw(Drawable buffer,Xvars &xvars,int dpyNum,
		     const Area &bufArea)
{
  Pos pos;
  Size size;
  area.get_rect(pos,size);
  Size offset = area - bufArea;

  XFillArc(xvars.dpy[dpyNum],buffer,xvars.gc[dpyNum],
	   offset.width,offset.height,
	   size.width,size.height,
	   0,23040);
/*  XFillRectangle(xvars.dpy,buffer,xvars.gc,
		 offset.width,offset.height,
		 size.width,size.height); */
}



void Explosion::act() 
{
  PhysicalP nearby[OL_NEARBY_MAX];
  int nitems;
  LocatorP locator = get_locator();
  locator->get_nearby(nearby,nitems,this,radius);
  
  PhysicalP p = locator->lookup(bomber);
  int hit = 0;
  
  for (int n = 0; n < nitems; n++)
    if (nearby[n]->collidable())
      { 
	const Area &otherArea = nearby[n]->get_area();
	int distance = middle.distance(otherArea.get_middle());
	assert (distance >= 0);
	nearby[n]->corporeal_attack(p,damageMax*(radius-distance)/radius);
	hit++;
      }

  ostrstream msg;
  msg << "Explosion hits " << hit << " objects." << ends;
  locator->message_enq(msg.str());

  kill_self();
  Physical::act();
}



Fire::Fire(WorldP w,LocatorP l,const Pos &mid,Boolean coll)
: Physical(context,w,l)
{
  Pos pos;
  pos.x = mid.x - FIRE_DIAMETER_INIT / 2;
  pos.y = mid.y - FIRE_DIAMETER_INIT / 2;
  Size size;
  size.width = size.height = FIRE_DIAMETER_INIT;

  Area nArea(AR_RECT,pos,size);
  area = areaNext = nArea;
  isCollidable = coll;
}



int Fire::get_damage() 
{
  return FIRE_DAMAGE;
}



const Area &Fire::get_area()
{
  return area;
}



const Area &Fire::get_area_next()
{
  return areaNext;
}



Boolean Fire::collidable()
{
  return isCollidable;
}



void Fire::draw(Drawable buffer,Xvars &xvars,int dpyNum,const Area &bufArea)
{
  const Area &area = get_area();
  Pos pos;
  Size size;
  area.get_rect(pos,size);
  Size offset = area - bufArea;

  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],xvars.red[dpyNum]);
  XFillArc(xvars.dpy[dpyNum],buffer,xvars.gc[dpyNum],
	   offset.width,offset.height,
	   size.width,size.height,
	   0,23040);
  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],xvars.black[dpyNum]);
}



void Fire::avoid(PhysicalP)
{}



void Fire::collide(PhysicalP other)
{
  if (other->is_shot())
    return;
  other->heat_attack(NULL,FIRE_HEAT,True);
}



void Fire::corporeal_attack(PhysicalP,int)
{}



void Fire::heat_attack(PhysicalP,int,Boolean)
{}



void Fire::act()
{
  Pos pos;
  Size size;
  area.get_rect(pos,size);
  
  size.width -= (FIRE_RADIUS_DELTA * 2);
  size.height = size.width;
  if (size.width <= 0)
    kill_self();
  else
    {
      pos.x += FIRE_RADIUS_DELTA;
      pos.y += FIRE_VEL_Y;
      Area nArea(AR_RECT,pos,size);
      areaNext = nArea;
    }
  Physical::act();
}



void Fire::update()
{
  area = areaNext;
  Physical::update();
}



NProtection::NProtection(WorldP w,LocatorP l,const Area &area)
  : Protection(context,xdata,w,l,area)
{
  n = N_PROTECTION_N;
}



Boolean NProtection::corporeal_protect(int)
{
  assert(n >= 0);
  if (n == 0)
    return False;
  n--;
  if (n == 0 )
    kill_self();
  return True;
}



Boolean NProtection::heat_protect(int,Boolean)
{
  assert(n >= 0);
  return False;
}



TProtection::TProtection(WorldP w,LocatorP l,const Area &area)
  : Protection(context,xdata,w,l,area)
{
  timer.set(T_PROTECTION_TIME);
}



void TProtection::act()
{
  if (timer.ready())
    kill_self();
  timer.clock();

  Protection::act();
}



Boolean TProtection::corporeal_protect(int)
{
  return True;
}



Boolean TProtection::heat_protect(int,Boolean)
{
  return True;
}



Rock::Rock(WorldP w,LocatorP l,const Pos &pos) 
     : Heavy(context,xdata,w,l,pos) 
{}



Weight::Weight(WorldP w,LocatorP l,const Pos &pos) 
     : Heavy(context,xdata,w,l,pos) 
{}



AltarOfSin::AltarOfSin(WorldP w,LocatorP l,const Pos &pos)
     : Heavy(context,xdata,w,l,pos)  
{
  turnTaken = False;
}



void AltarOfSin::corporeal_attack(PhysicalP other,int)
{
  /* Don't fuck with the Altar of Sin. */

  if (!turnTaken)
    {
      IntelP intel;
      if (other && (other->get_class_id() != A_AltarOfSin) &&
	  (intel = other->get_intel()))
	{
	  LocatorP locator = get_locator();
	  ostrstream str;
	  
	  if (Utils::coinFlip() && other->is_moving())  
	    /* Turned into a frog. */
	    { 
	      str << intel->get_name() << " attacks the Altar of Sin and is "
		<< "turned into a frog." << ends;
	      locator->message_enq(str.str());
	      
	      other->set_intel(NULL);
	      if (!other->get_mapped())
		cerr << "Warning:AltarOfSin::corporeal_attack: object "
		  << "should be mapped." << endl;
	      other->set_mapped_next(False);
	      
	      const Area &area = other->get_area();
	      Pos pos = area.get_middle() - 0.5 * Frog::get_size_max();
	      
	      PhysicalP frog = new Frog(get_world(),locator,pos,other->get_id());
	      assert(frog);
	      frog->set_intel(intel);
	      locator->add(frog);
	    }
	  else
	    /* Lose all health. */
	    {
	      str << "BLASPHMER!  " << intel->get_name() << 
		" loses health for daring to attack the Altar of Sin." << ends;
	      locator->message_enq(str.str());
	      
	      int damage = other->get_health();
	      other->corporeal_attack(this,damage);
	    }
	}
      turnTaken = True;
    }
}



void AltarOfSin::heat_attack(PhysicalP,int,Boolean)
{}



void AltarOfSin::collide(PhysicalP other)
{
  IntelP intel;
  if ((intel = other->get_intel()) && intel->is_human() && !turnTaken)
    {
      int lives = intel->get_lives();
      LocatorP locator = get_locator();
      ostrstream msg;

      // If human has infinite lives, give him some points(kills) instead.
      if (lives == IT_INFINITE_LIVES)
	{
	  for (int n = 0; n < ALTAR_OF_SIN_KILLS; n++)
	    intel->add_human_kill();
	  
	  msg << intel->get_name() << " sells soul for " 
	    << ALTAR_OF_SIN_KILLS << " kills." << ends;
	}
      else
	{
	  intel->set_lives(lives + 1);
	  msg << intel->get_name() << " sells soul for an extra life." << ends;
	}

      locator->message_enq(msg.str());
      kill_self();
      turnTaken = True;
    }
  else
    Heavy::collide(other);
}



void AltarOfSin::update()
{
  turnTaken = False;
  Heavy::update();
}



Doppel::Doppel(WorldP w,LocatorP l,const Pos &pos)
: AutoUse(context,xdata,w,l,pos)
{}



void Doppel::use(PhysicalP p)
{
  assert(p && p->get_intel());

  // If p is not allowed to use doppelgangers, just return.
  const PhysicalContext *usersPC = p->get_context();
  if (!usersPC->doppelUser)
    return;


  stats.add_use();
  WorldP world = get_world();
  LocatorP locator = get_locator();
  
  PhysicalP obj = create_physical(p->get_area(),p);

  IntelOptions ops;
  ops.psychotic = True;
  ops.classFriends = False;
  ops.ignoreItems = True;
  ops.ignoreLemmings = True;
  NeutralP neutral = 
    new Neutral(world,locator,"Slave",&ops,
		ITpsychotic|ITclassFriends|ITignoreItems|ITignoreLemmings,
		p->get_intel());
  assert(neutral);
  locator->register_neutral(neutral);

  locator->add(obj);
  obj->set_intel(neutral);
  obj->set_dont_collide(p);
  
  kill_self();
  Item::use(p);
}


// New version which uses Locator::filter_contexts.
// More general and easy to extend.
PhysicalP Doppel::create_physical(const Area &area,PhysicalP original)
{
  WorldP world = get_world();
  LocatorP locator = get_locator();
  Pos middle = area.get_middle();
  PhysicalP obj;
  
  // Get the PhysicalContext(description of its class) of the original
  const PhysicalContext *originalContext = original->get_context();

  // Use the original's PhysicalContext to create a new object.
  obj = originalContext->create(world,locator,
				middle - 0.5 * originalContext->get_size_max());

  assert(obj);
  return obj;
}



#if 0
PhysicalP Doppel::create_physical(const Area &area,ClassId classId)
{
  WorldP world = get_world();
  LocatorP locator = get_locator();
  PhysicalP obj;
  Pos middle = area.get_middle();
  
  switch (classId)
    {
    case A_Ninja:
      obj = new Ninja(world,locator,middle - 0.5 * Ninja::get_size_max());
      break;
    case A_Hero:
      obj = new Hero(world,locator,middle - 0.5 * Hero::get_size_max());
      break;
    case A_ChopperBoy:
      obj = new ChopperBoy(world,locator,
			   middle - 0.5 * ChopperBoy::get_size_max());
      break;
    case A_Walker:
      obj = new Walker(world,locator,
		       middle - 0.5 * Walker::get_size_max());
      break;
    case A_Alien:
      obj = new Alien(world,locator,middle - 0.5 * Alien::get_size_max());
      break;
    case A_RedHugger:
      obj = new RedHugger(world,locator,middle - 0.5 * RedHugger::get_size_max());
      break;
    default:
      // choose randomly
      switch (Utils::choose(4)) {
      case 0: 
	return create_physical(area,A_Ninja);
      case 1: 
	return create_physical(area,A_Hero);
      case 2: 
	return create_physical(area,A_ChopperBoy);
      case 3: 
	return create_physical(area,A_Walker);
//      case 3: return create_physical(area,A_Alien);
      };
    };

  assert(obj);
  return obj;
}
#endif



Stats Doppel::stats;



Cloak::Cloak(WorldP w,LocatorP l,const Pos &pos)
: AutoUse(context,xdata,w,l,pos)
{}



void Cloak::use(PhysicalP p)
{
  assert(p->is_moving());

  // Destroy any protection p may be using.
  LocatorP l = get_locator();
  PhysicalP prot = l->lookup(((MovingP)p)->get_protection());
  if (prot)
    prot->kill_self();

  // Don't bother doing anything on an already invisible user.
  if (!((MovingP)p)->is_invisible())
    {
      ((MovingP)p)->set_invisible_next(True);

      // So p will stop being invisible when p attacks something.
      ((MovingP)p)->set_attack_hook(attack_hook);
    }

  stats.add_use();
  kill_self();
  Item::use(p);
}



void Cloak::attack_hook(PhysicalP p)
{
  assert(p->is_moving());
  ((MovingP)p)->set_invisible_next(False);
}



Stats Cloak::stats;



Transmogifier::Transmogifier(WorldP w,LocatorP l,const Pos &pos)
: AutoUse(context,xdata,w,l,pos)
{}



void Transmogifier::use(PhysicalP other)
{
  assert(other->alive());
  stats.add_use();

  IntelP intel = other->get_intel();

  // Must check that other does not have protection against swapping.
  if (intel && !other->swap_protect())
    {
      LocatorP locator = get_locator();
      const Area &area = other->get_area();
      PhysicalP p = new_physical(area.get_middle());
      locator->add(p);

      p->set_intel(intel);
      other->set_intel(NULL);
      other->set_quiet_death();
      other->kill_self();
    }
  kill_self();
  AutoUse::use(other);
}



// New version which uses Locator::filter_contexts.
// More general and easy to extend.
PhysicalP Transmogifier::new_physical(const Pos &middle)
{
  PhysicalContext *list[A_CLASSES_NUM];
  int size;
  LocatorP l = get_locator();
  WorldP w = get_world();

  // Get list of all classes that are potential transmogify targets.
  size = l->filter_contexts(list,NULL,transmogify_target_filter);

  // Something is wrong if size == 0, because there are no classes to 
  // transmogify into.
  assert(size);
  
  // Randomly choose.
  int which = Utils::choose(size);

  // Create object centered on middle.
  Pos pos = middle - 0.5 * list[which]->get_size_max();
  PhysicalP obj = list[which]->create(w,l,pos);

  return obj;
}



#if 0
PhysicalP Transmogifier::new_physical(const Pos &middle)
{
  PhysicalP obj;
  LocatorP locator = get_locator();
  WorldP world = get_world();
  
  // Either a Frog, Hero, Ninja, ChopperBoy, Alien, or RedHugger.
  switch (Utils::choose(6)) {
  case 0:
    {
      Id invalid;
      Pos pos = middle - 0.5 * Frog::get_size_max();
      obj = new Frog(world,locator,pos,invalid);
    }
    break;
  case 1:
    {
      Pos pos = middle - 0.5 * Hero::get_size_max();
      obj = new Hero(world,locator,pos);
    }
    break;
  case 2:
    {
      Pos pos = middle - 0.5 * Ninja::get_size_max();
      obj = new Ninja(world,locator,pos);
    }
    break;
  case 3:
    {
      Pos pos = middle - 0.5 * ChopperBoy::get_size_max();
      obj = new ChopperBoy(world,locator,pos);
    }
    break;
  case 4:
    {
      Pos pos = middle - 0.5 * Alien::get_size_max();
      obj = new Alien(world,locator,pos);
    }
    break;
  case 5:
    {
      Pos pos = middle - 0.5 * RedHugger::get_size_max();
      obj = new RedHugger(world,locator,pos);
    }
    break;
  default:
    assert(0);
  };

  assert(obj);
  return obj;
}
#endif



Boolean Transmogifier::transmogify_target_filter(const PhysicalContext *pc)
{
  return pc->transmogifyTarget;
}



Stats Transmogifier::stats;



MedKit::MedKit(WorldP w,LocatorP l,const Pos &pos) 
     : AutoUse(context,xdata,w,l,pos) 
{}



void MedKit::use(PhysicalP p)
{
  stats.add_use();
  assert(p->alive());
  p->heal();
  kill_self();
  AutoUse::use(p);
}



Stats MedKit::stats;



NShield::NShield(WorldP w,LocatorP l,const Pos &pos) 
     : Shield(context,xdata,w,l,pos) 
{}



void NShield::use(PhysicalP p)
{
  stats.add_use();
  Shield::use(p);
}



ProtectionP NShield::create_protection(const Area &area)
{
  ProtectionP pr = new NProtection(get_world(),get_locator(),area);
  assert(pr);
  return pr;
}



Stats NShield::stats;



TShield::TShield(WorldP w,LocatorP l,const Pos &pos) 
     : Shield(context,xdata,w,l,pos) 
{}



void TShield::use(PhysicalP p)
{
  stats.add_use();
  Shield::use(p);
}



ProtectionP TShield::create_protection(const Area &area)
{
  ProtectionP pr = new TProtection(get_world(),get_locator(),area);
  assert(pr);
  return pr;
}



Stats TShield::stats;



Bomb::Bomb(WorldP w,LocatorP l,const Pos &p) :
       Animated(context,xdata,w,l,p) 
{
  set_frame(BOMB_FRAME_INACTIVE);
  set_frame_next(BOMB_FRAME_INACTIVE);
  frame = BOMB_FRAME_INACTIVE;

  Timer ntimer(BOMB_TIME); 
  timer = ntimer;
  active = False;
  defused = False;
}



Boolean Bomb::is_bomb()
{
  return True;
}



void Bomb::use(PhysicalP bomberP)
{
  if (!active) 
    {
      frame = BOMB_FRAME_ACTIVE;
      timer.set();
      active = True;
      if (bomberP)
	bomber = bomberP->get_id();
      LocatorP locator = get_locator();
      ostrstream msg;
      msg << frame << ends;
      locator->message_enq(msg.str());

      set_cant_take();
    }
  Animated::use(bomberP);
}



void Bomb::act()
{
  if (active)
    timer.clock();
  
  if (active && timer.ready())
    {
      if (! frame)
	kill_self();
      else
	{
	  timer.set();
	  frame--;
	  
	  LocatorP locator = get_locator();
	  ostrstream msg;
	  msg << frame << ends;
	  locator->message_enq(msg.str());
	}
    }
  
  set_frame_next(frame);

 Animated::act();
}



void Bomb::set_quiet_death()
{
  defused = True;
  Animated::set_quiet_death();
}



void Bomb::die()
{
  if (!defused)
    {
      // Kind of a hack, recording bombs that exploded, not bombs that 
      // were used.
      stats.add_use();

      WorldP world = get_world();
      LocatorP locator = get_locator();
      const Area area = get_area();
      
      PhysicalP explosion = 
	new Explosion(world,locator,area.get_middle(),bomber,
		      BOMB_EXPLOSION_RADIUS,BOMB_EXPLOSION_DAMAGE_MAX);
      assert (explosion);
      locator->add(explosion);
    }  

  Animated::die();
}



Stats Bomb::stats;



Trapdoor::Trapdoor(WorldP w,LocatorP l,const Pos &pos,const Id &home_id)
: Moving(context,xdata,w,l,pos)
{
  Timer nTimer(TRAPDOOR_TIME);
  timer = nTimer;
  timer.set();
  
  lemmingsNum = 0;
  homeId = home_id;
}



Boolean Trapdoor::collidable()
{
  return False;
}



int Trapdoor::get_drawing_level()
{
  return 0;
}



void Trapdoor::act()
{
  if (timer.ready() && lemmingsNum < LEMMINGS_MAX)
    {
      LocatorP l = get_locator();
      WorldP w = get_world();
      const Area &area = get_area();
      Pos pos = area.get_middle();
      Size lemmingSize = Lemming::get_size_max();
      pos.x -= lemmingSize.width / 2;
      pos.y += lemmingSize.height;
      PhysicalP lemming = new Lemming(w,l,pos);
      assert(lemming);

      char lemmingStr[20];
      ostrstream str(lemmingStr,20);
      str << "lemming-" << lemmingsNum << ends;
      NeutralP lemmingIntel = new LemmingIntel(w,l,lemmingStr,homeId);
      lemming->set_intel(lemmingIntel);
      l->register_neutral(lemmingIntel);
      lemmings[lemmingsNum] = lemmingIntel->get_intel_id();

      l->add(lemming);
      
      lemmingsNum++;
      timer.set();
    }

  timer.clock();
  Moving::act();
}



Home::Home(WorldP w,LocatorP l,const Pos &pos)
: Moving(context,xdata,w,l,pos)
{
  lemmingsSafe = 0;
}



Boolean Home::collidable()
{
  return False;
}



int Home::get_drawing_level()
{
  return 0;
}



void Home::act()
{
  PhysicalP nearby[OL_NEARBY_MAX];
  int nItems;
  LocatorP locator = get_locator();
  locator->get_nearby(nearby,nItems,this,HOME_RADIUS);

  for (int n = 0; n < nItems; n++)
    {
      IntelP intel = nearby[n]->get_intel();
      if (intel && intel->is_lemming_intel())
	{
	  nearby[n]->set_quiet_death();
	  nearby[n]->kill_self();
	  lemmingsSafe++;
	}
    }
  Moving::act();
}



Shell::Shell(WorldP w,LocatorP l,const Pos &p,const Id &shooter,
	     Dir d) 
     : Shot(context,xdata,w,l,p,shooter,d) 
{
  stats.add_creation();
}



Stats Shell::stats;



SwapShell::SwapShell(WorldP w,LocatorP l,const Pos &p,
		     const Id &sh, const Id &sw,Dir d)
     : Shot(context,xdata,w,l,p,sh,d)
{
  swapper = sw;
  stats.add_creation();
}



void SwapShell::collide(PhysicalP other)
{
  if (other->is_shot())
    return;

  LocatorP locator = get_locator();
  const Id &shooter = get_shooter();
  PhysicalP shooterP;

  // Do nothing if shooter no longer exists.
  if (!other->swap_protect() && 
      (shooterP = locator->lookup(shooter)))
    { 
      // Swap souls.
      IntelP tmp = other->get_intel();
      /* Don't swap unless both shooter and other have intelligence. */
      if (tmp && shooterP->get_intel()) 
	{
	  other->set_intel(shooterP->get_intel());
	  shooterP->set_intel(tmp);
	  
	  // Destroy swapper.
	  PhysicalP swapperP;
	  if (locator->lookup(swapperP,swapper) == OL_NO_SIG)
	    swapperP->kill_self();
	}
    }

  kill_self();
}



Stats SwapShell::stats;



Lance::Lance(WorldP w,LocatorP l,const Pos &p,const Id &shooter,Dir d)
       : Shot(context,xdata,w,l,p,shooter,d,d) 
{
  stats.add_creation();
}



// This function is also used by Laser::compute_weapon_dir().
Dir Lance::compute_weapon_dir(ITcommand command)
{
  switch (command) {
  case IT_WEAPON_R:
    return CO_R;
  case IT_WEAPON_DN_R:
    return CO_DN_R_R;
  case IT_WEAPON_DN:
    return CO_DN;
  case IT_WEAPON_DN_L:
    return CO_DN_L_L;
  case IT_WEAPON_L:
    return CO_L;
  case IT_WEAPON_UP_L:
    return CO_UP_L_L;
  case IT_WEAPON_UP:
    return CO_UP;
  case IT_WEAPON_UP_R:
    return CO_UP_R_R;
  };
  return CO_air;
}



Stats Lance::stats;



Laser::Laser(WorldP w,LocatorP l,const Pos &p,const Id &shooter,Dir d)
       : Shot(context,xdata,w,l,p,shooter,d,d) 
{
  stats.add_creation();
}



Dir Laser::compute_weapon_dir(ITcommand command)
{
  // Kind of a hack.
  return Lance::compute_weapon_dir(command);
}



Stats Laser::stats;



FrogShell::FrogShell(WorldP w,LocatorP l,const Pos &p,
		     const Id &shooter,const Id &frog_gun,Dir d) 
     : Shot(context,xdata,w,l,p,shooter,d) 
{
  frogGun = frog_gun;
  stats.add_creation();
}



void FrogShell::collide(PhysicalP other)
{
  // Be careful not to frog a Frog.
  if (other->is_shot() || other->get_class_id() == A_Frog)
    return;

  IntelP intel = other->get_intel();
  if (!other->frog_protect() && intel && other->is_moving())
    {
      other->set_intel(NULL);
      assert(other->get_mapped());
      other->set_mapped_next(False);
      
      const Area &area = other->get_area();
      Pos pos = area.get_middle() - 0.5 * Frog::get_size_max();

      LocatorP locator = get_locator();
      PhysicalP frog = new Frog(get_world(),locator,pos,other->get_id());
      assert(frog);
      frog->set_intel(intel);
      locator->add(frog);

      // Destroy frogGun.
      PhysicalP p;
      if (p = locator->lookup(frogGun))
	p->kill_self();
    }

  kill_self();
}



Stats FrogShell::stats;



Fireball::Fireball(WorldP w,LocatorP l,const Pos &p,const Id &shooter,
		   Dir d,int h,int t) 
     : Shot(context,xdata,w,l,p,shooter,d) 
{
  if (t == -1)
    timer.set(FIREBALL_TIME);
  else
    timer.set(t);

  if (h == -1)
    heat = FIREBALL_HEAT;
  else
    heat = h;
}



void Fireball::collide(PhysicalP other)
{
  // Fireballs are not destroyed by shots.
  if (other->is_shot() && other->get_class_id() != A_Missile)
    return;

  LocatorP locator = get_locator();
  const Id &shooter = get_shooter();
  PhysicalP p = locator->lookup(shooter);

  if (p)
    {
      // Special case.  Fireballs fired by FireDemons hurt other FireDemons.
      if (p->get_class_id() == A_FireDemon && 
	  other->get_class_id() == A_FireDemon)
	{
	  other->corporeal_attack(p,FIREBALL_FIRE_DEMON_DAMAGE);
	  kill_self();
	}
      else
	other->heat_attack(p,heat);
    }
  else
    other->heat_attack(NULL,heat);
}



void Fireball::act()
{
  if (timer.ready())
    kill_self();

  timer.clock();
  Shot::act();
}



void Fireball::draw(Drawable buffer,Xvars &xvars,int dpyNum,const Area &bufArea)
{
  const Area &area = get_area();
  Pos pos;
  Size size;
  area.get_rect(pos,size);
  Size offset = area - bufArea;

  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],xvars.red[dpyNum]);
  XFillArc(xvars.dpy[dpyNum],buffer,xvars.gc[dpyNum],
	   offset.width,offset.height,
	   size.width,size.height,
	   0,23040);
  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],xvars.black[dpyNum]);
}



Missile::Missile(WorldP w,LocatorP l,const Pos &p,const Id &sh,Dir d)
: Shot(context,xdata,w,l,p,sh,d,d) 
{
  timer.set(MISSILE_DESTRUCT_TIME);
  Timer nTimer(MISSILE_ROTATE_TIME);
  rotate = nTimer;
  hasTarget = False;
  shooterId = sh;
  stats.add_creation();
}



void Missile::act()
{
  LocatorP locator = get_locator();

  // Find a target.
  if (!hasTarget)
    {
      PhysicalP nearby[OL_NEARBY_MAX];
      int nearbyNum;
      // Should work even though this not fully constructed.
      locator->get_nearby(nearby,nearbyNum,this,MISSILE_RADIUS);
      
      // Search through list for best possible candidate.
      const Area &area = get_area();
      Pos middle = area.get_middle();
      int distance_2 = -1;  // Best so far.
      
      for (int n = 0; n < nearbyNum; n++)
	if (nearby[n]->alive() &&
	    nearby[n]->is_creature() &&
	    nearby[n]->get_id() != shooterId)
	  {
	    const Area &area = nearby[n]->get_area();
	    int dist_2 = middle.distance_2(area.get_middle());
	    
	    if (distance_2 == -1 || dist_2 < distance_2)
	      {
		targetId = nearby[n]->get_id();
		distance_2 = dist_2;
		hasTarget = True;
	      }
	  }
    }

  if (rotate.ready())
    {
      rotate.set();
      
      PhysicalP target;
      if (locator->lookup(target,targetId) == OL_NO_SIG)
	{
	  Boolean noChange = False;
	  Dir dir = get_dir();
	  const Area &area = get_area();
	  const Area &targetArea = target->get_area();
	  Dir dirTo = area.dir_to(targetArea);
	  if (dir > dirTo)
	    {
	      if (dir - dirTo > (CO_DIR_PURE / 2))
		dir++;
	      else
		dir--;
	    }
	  else if (dirTo > dir)
	    {
	      if (dirTo - dir > (CO_DIR_PURE / 2))
		dir--;
	      else
		dir++;
	    }
	  else 
	    noChange = True;
	  
	  if (!noChange)
	    {
	      if (dir >= CO_DIR_MAX)
		dir -= CO_DIR_PURE;
	      if (dir < (CO_DIR_MAX - CO_DIR_PURE))
		dir += CO_DIR_PURE;
	      
	      set_dir_next(dir);
	      const Vel *unitVels = get_unit_vels();
	      set_vel_next(context.speed * unitVels[dir]);
	    }
	}
    }
  
  if (timer.ready())
    kill_self();
  
  timer.clock();
  rotate.clock();
  Shot::act();
}



Stats Missile::stats;



Star::Star(WorldP w,LocatorP l,const Pos &p,const Id &sh,Dir d)
: Shot(context,xdata,w,l,p,sh,d) 
{
  stats.add_creation();
}



Stats Star::stats;



Grenade::Grenade(WorldP w,LocatorP l,const Pos &pos,const Id &sh,
		 Dir dir,Speed speed)
: Falling(context,xdata,w,l,pos,dir)
{
  assert(dir != CO_air);
  const Vel *unitVels = get_unit_vels();
  set_vel_next(speed * unitVels[dir]);
  
  timer.set(GRENADE_TIME);
  shooter = sh;
  defused = False;
}



Grenade::Grenade(WorldP w,LocatorP l,const Pos &pos,const Id &sh,
		 const Vel &vel)
: Falling(context,xdata,w,l,pos,vel.get_dir())
{
  stats.add_creation();

  set_vel_next(vel);
  timer.set(GRENADE_TIME);
  shooter = sh;
  defused = False;
}



void Grenade::set_quiet_death()
{
  defused = True;
  Falling::set_quiet_death();
}



void Grenade::act()
{
  if (timer.ready())
    kill_self();
  
  timer.clock();
  Falling::act();
}



void Grenade::die()
{
  if (!defused)
    {
      WorldP world = get_world();
      LocatorP locator = get_locator();
      const Area area = get_area();
  
      PhysicalP explosion = 
	new Explosion(world,locator,area.get_middle(),shooter,
		      GRENADE_EXPLOSION_RADIUS,GRENADE_EXPLOSION_DAMAGE_MAX);
      assert (explosion);
      locator->add(explosion);
    }
  Falling::die();
}



Stats Grenade::stats;



Egg::Egg(WorldP w,LocatorP l,const Pos &pos,
	 const IntelOptions &ops,ITmask opMask)
: Falling(context,xdata,w,l,pos)
{
  intelOptions = ops;
  intelOpMask = opMask;
  open = False;
}



void Egg::act()
{
  if (!open)
    {
      LocatorP l = get_locator();
      PhysicalP nearby[OL_NEARBY_MAX];
      int nItems;
      l->get_nearby(nearby,nItems,this,EGG_RADIUS);
      // Check for nearby humans.
      for (int n = 0; n < nItems; n++)
	{
	  IntelP intel = nearby[n]->get_intel();
	  if (intel && 
	      intel->is_human() &&
	      !nearby[n]->is_hugger() &&
	      nearby[n]->get_class_id() != A_Alien)
	    // Trigger the egg.
	    {
	      PhysicalP hugger;
	      IntelP intel;
	      WorldP w = get_world();
	      
	      // Hugger initial position is above the egg.
	      const Area &area = get_area();
	      Pos eggMiddle = area.get_middle();
	      
	      // Decide whether to create a red or green face hugger.
	      Boolean gHugger = 
		Utils::choose(EGG_GREEN_HUGGER_PERCENT) == 0;
	      
	      Size huggerSize = gHugger ? GreenHugger::get_size_max() :
	                                  RedHugger::get_size_max();
	      Pos p(eggMiddle.x - huggerSize.width / 2,
		    eggMiddle.y - EGG_HUGGER_EJECT_HEIGHT);
	      
	      // Create a Green or Red Hugger.
	      if (gHugger) {
		// Create a GreenHugger, a neutral intelligence.
		hugger = new GreenHugger(w,l,p);
		intel = new Neutral(w,l,"Hugger",
				    &intelOptions,intelOpMask);
		l->register_neutral((NeutralP)intel);
	      }
	      else {
		// Create a RedHugger, an enemy intelligence.
		hugger = new RedHugger(w,l,p);
		intel = new Enemy(w,l,"Hugger",
				  &intelOptions,intelOpMask);
		l->register_enemy((EnemyP)intel);
	      }
	      assert(hugger);
	      assert(intel);
	      hugger->set_intel(intel);
	      l->add(hugger);

	      // Don't want Hugger to bump into the egg.
	      hugger->set_dont_collide(this);    
	      
	      // Make egg open.
	      open = True;
	      set_dir_next(CO_center); // The pixmap for an open egg.
	      break;
	    }
	}
    }
  Falling::act();
}



Xit::Xit(WorldP w,LocatorP l,const Pos &pos) 
: Touchable(context,xdata,w,l,pos)
{}



Flag::Flag(WorldP w,LocatorP l,const Pos &pos) 
: Touchable(context,xdata,w,l,pos)
{}



Chainsaw::Chainsaw(WorldP w,LocatorP l,const Pos &p) 
: Cutter(context,xdata,w,l,p) 
{}



Gun::Gun(const GunContext &g_c,
	 GunXdata &x_data,
	 WorldP w,
	 LocatorP l,
	 const Pos &p) :
       Weapon(g_c.weaponContext,x_data,w,l,p) 
{
  assert(g_c.ammoInitial <= g_c.ammoMax);

  gc = &g_c;
  Timer ntimer(g_c.shotTime); 
  timer = ntimer;
  ammo = g_c.ammoInitial;
}



Boolean Gun::is_gun()
{
  return True;
}



Boolean Gun::ready()
{
  return timer.ready();
}



int Gun::get_ammo()
{
  return ammo;
}



int Gun::get_ammo_max()
{
  return gc->ammoMax;
}



void Gun::update()
{
  timer.clock(); 
  Weapon::update();
}



void Gun::set_ammo(int val)
{
  assert(val <= gc->ammoMax && val >= 0);
  ammo = val;
}



Size Gun::get_shot_size(Dir)
{
  return Shell::get_size();
}



Dir Gun::compute_weapon_dir(ITcommand command)
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



void Gun::fire(const Id &id,ITcommand command)
{
  Dir dir = compute_weapon_dir(command);
  _fire(id,dir);
}



void Gun::_fire(const Id &id,Dir dir,Boolean setTimer,Boolean costsAmmo)
/* NOTE: PH_AMMO_UNLIMITED is not currently tested. */
{
  assert((ammo == PH_AMMO_UNLIMITED) ||
	 ((ammo <= gc->ammoMax) && (ammo >= 0)));

  // Person who fired weapon may not exist anymore. (See FThrower)
  LocatorP locator = get_locator();
  PhysicalP p;
  if (p = locator->lookup(id))
    {
      if ((dir != CO_air) && ready() && 
	  ((ammo == PH_AMMO_UNLIMITED) || (ammo > 0)))
	{
	  const Area &area = p->get_area();
	  
//	  Pos newPos = area.adjacent_rect(get_shot_size(dir),dir);
 	  Pos newPos = Coord::shot_initial_pos(area,get_shot_size(dir),dir);
	  WorldP world = get_world();
	  LocatorP locator = get_locator();
	  
	  PhysicalP shot = create_shot(p,world,locator,newPos,dir);
	  shot->set_dont_collide(p);
	  locator->add(shot);
	  if (setTimer)
	    timer.set();
	  
	  if (costsAmmo && ammo != PH_AMMO_UNLIMITED)
	    ammo--;
	}
    }  
}



PhysicalP Gun::create_shot(PhysicalP shooter,WorldP world,LocatorP locator,
			   const Pos &pos,Dir dir)
{
  PhysicalP shot = new Shell(world,locator,pos,shooter->get_id(),dir);
  assert (shot);
  return shot;
}



SingleGun::SingleGun(const SingleGunContext &c,
		     SingleGunXdata &x_data,
		     WorldP w,LocatorP l,const Pos &p) 
    : Gun(c.gunContext,x_data,w,l,p) 
{}



Boolean SingleGun::ready()
{
  // Can only fire if previous shot has been destroyed.
  PhysicalP shot;
  LocatorP locator = get_locator();

  return (locator->lookup(shot,shotId) == OL_NOT_FOUND) && Gun::ready();
}



void SingleGun::fire(const Id &id,ITcommand command)
{
  assert(get_ammo() == PH_AMMO_UNLIMITED);

  LocatorP locator = get_locator();
  PhysicalP p;
  if (locator->lookup(p,id) == OL_NO_SIG)
    {
      Dir dir = compute_weapon_dir(command);
      if ((dir != CO_air) && ready())
	{
	  const Area &area = p->get_area();
	  
//	  Pos newPos = area.adjacent_rect(get_shot_size(dir),dir);
 	  Pos newPos = Coord::shot_initial_pos(area,get_shot_size(dir),dir);
	  WorldP world = get_world();
	  
	  LocatorP locator = get_locator();
	  PhysicalP shot = create_shot(p,world,locator,newPos,dir);
	  shot->set_dont_collide(p);
	  locator->add(shot);
	  
	  shotId = shot->get_id();
	}
    }
}



Pistol::Pistol(WorldP w,LocatorP l,const Pos &p) 
     : Gun(context,xdata,w,l,p) 
{}



MGun::MGun(WorldP w,LocatorP l,const Pos &p) 
     : Gun(context,xdata,w,l,p) 
{}



Swapper::Swapper(WorldP w,LocatorP l,const Pos &p) :
       SingleGun(context,xdata,w,l,p) 
{}



Size Swapper::get_shot_size(Dir)
{
  return SwapShell::get_size();
}



PhysicalP Swapper::create_shot(PhysicalP shooter,WorldP w,LocatorP l,
			       const Pos &pos,Dir d)
{
  PhysicalP swapShell = new SwapShell(w,l,pos,shooter->get_id(),get_id(),d);
  assert (swapShell);
  return swapShell;
}



Lancer::Lancer(WorldP w,LocatorP l,const Pos &p) 
     : Gun(context,xdata,w,l,p) 
{}



Size Lancer::get_shot_size(Dir dir)
{
  return Lance::get_size(dir);
}



Dir Lancer::compute_weapon_dir(ITcommand command)
{
  return Lance::compute_weapon_dir(command);
}



PhysicalP Lancer::create_shot(PhysicalP shooter,WorldP w,LocatorP l,
			      const Pos &pos,Dir d)
{
  PhysicalP lance = new Lance(w,l,pos,shooter->get_id(),d);
  assert (lance);
  return lance;
}



FrogGun::FrogGun(WorldP w,LocatorP l,const Pos &p) :
       SingleGun(context,xdata,w,l,p) 
{}



Size FrogGun::get_shot_size(Dir)
{
  return FrogShell::get_size();
}



PhysicalP FrogGun::create_shot(PhysicalP shooter,WorldP w,LocatorP l,
			       const Pos &pos,Dir d)
{
  PhysicalP shot = new FrogShell(w,l,pos,shooter->get_id(),get_id(),d);
  assert (shot);
  return shot;
}



FThrower::FThrower(WorldP w,LocatorP l,const Pos &p) :
       Gun(context,xdata,w,l,p) 
{
  Timer nTimer(F_THROWER_FIRE_TIME);
  stopFiring = nTimer;
  isFiring = False;
}



PhysicalP FThrower::create_shot(PhysicalP shooter,WorldP w,LocatorP l,
			      const Pos &pos,Dir d)
{
  PhysicalP fireball = new Fireball(w,l,pos,shooter->get_id(),d);
  assert (fireball);
  return fireball;
}



void FThrower::fire(const Id &id,ITcommand command)
{
  killerId = id;
  fireCommand = command;
  stopFiring.set();
  isFiring = True;
}



void FThrower::act()
{
  if (isFiring && stopFiring.ready())
    isFiring = False;

  if (isFiring)
    Gun::fire(killerId,fireCommand);
  
  stopFiring.clock();
  Gun::act();
}



Size FThrower::get_shot_size(Dir)
{
  return Fireball::get_size();
}



Launcher::Launcher(WorldP w,LocatorP l,const Pos &p) 
: Gun(context,xdata,w,l,p) 
{}



Size Launcher::get_shot_size(Dir dir)
{
  return Missile::get_size(dir);
}



PhysicalP Launcher::create_shot(PhysicalP shooter,WorldP world,
				LocatorP locator,const Pos &pos,Dir dir)
{
  PhysicalP shot = new Missile(world,locator,pos,shooter->get_id(),dir);
  assert (shot);
  return shot;
}



Grenades::Grenades(WorldP w,LocatorP l,const Pos &p) 
: Gun(context,xdata,w,l,p) 
{}



Size Grenades::get_shot_size(Dir dir)
{
  return Grenade::get_size(dir);
}



PhysicalP Grenades::create_shot(PhysicalP shooter,WorldP world,
				LocatorP locator,const Pos &pos,Dir dir)
{
  Speed speed;
  if (dir == CO_UP_L || dir == CO_UP || dir == CO_UP_R)
    speed = GRENADES_TOSS_SPEED;
  else if (dir == CO_R || dir == CO_DN_R || dir == CO_DN_L || dir == CO_L)
    speed = GRENADES_ROLL_SPEED;
  else if (dir == CO_DN)
    speed = 0;
  else
    assert(0);

  assert(shooter->is_moving());
  const Vel *unitVels = get_unit_vels();
  Vel vel = ((MovingP)shooter)->get_vel() + speed * unitVels[dir];

//  PhysicalP shot = new Grenade(world,locator,pos,shooter->get_id(),dir,speed);
  PhysicalP shot = new Grenade(world,locator,pos,shooter->get_id(),vel);
  assert (shot);
  return shot;
}



#if 0
Shotgun::Shotgun(WorldP w,LocatorP l,const Pos &p) 
     : Gun(context,xdata,w,l,p) 
{}



void Shotgun::fire(const Id &id,ITcommand command)
{
  Dir dir = compute_weapon_dir(command);
  LocatorP l = get_locator();
  PhysicalP shooter;
  int ammo;

  if ((shooter = l->lookup(id)) && dir != CO_air && ready() && 
      (ammo = get_ammo()) > 0)
    {
      const Area &shooterArea = shooter->get_area();
	  
      PhysicalP nearby[OL_NEARBY_MAX];
      int nearbyNum;
      l->get_nearby(nearby,nearbyNum,shooter,SHOTGUN_RADIUS);
      const Vel *unitVels = get_unit_vels();

      for (int n = 0; n < nearbyNum; n++)
	{
	  Size diff = nearby[n]->get_area() - shooterArea;

	  if (diff.get_dir() == dir)
	    {
	      if (nearby[n]->is_moving())
		((MovingP)nearby[n])->
		  set_extra_vel_next((SHOTGUN_SLAM_MOMENTUM / 
				      nearby[n]->get_mass()) *
				     unitVels[dir]);
//	      ((MovingP)nearby[n])->set_vel_next(0);
	      nearby[n]->corporeal_attack(shooter,SHOTGUN_DAMAGE);
	    }
	}

      if (shooter->is_moving())
	((MovingP)shooter)->
	  set_extra_vel_next((SHOTGUN_RECOIL_MOMENTUM /
			      shooter->get_mass()) * 
			     unitVels[Coord::dir_opposite(dir)]);
      
      set_ammo(ammo - 1);
      set_shot_timer();
    }
}
#endif



Stars::Stars(WorldP w,LocatorP l,const Pos &p) 
     : Gun(context,xdata,w,l,p) 
{}



Size Stars::get_shot_size(Dir)
{
  return Star::get_size();
}



PhysicalP Stars::create_shot(PhysicalP shooter,WorldP world,LocatorP locator,
			     const Pos &pos,Dir dir)
{
  PhysicalP shot = new Star(world,locator,pos,shooter->get_id(),dir);
  assert (shot);
  return shot;
}



void Stars::fire(const Id &shooterId,ITcommand command)
{
  LocatorP locator = get_locator();
  PhysicalP shooter = locator->lookup(shooterId);
  if (shooter)
    {
      // Ninjas firing stars give a wider spread.
      int delta = (shooter->get_class_id() == A_Ninja) ? 2 : 1;

      // Fire a spread of shots around center.
      Dir center = Intel::command_weapon_to_dir_8(command);
      if (center != CO_air)
	for (Dir baseDir = center - delta; baseDir <= center + delta; baseDir++)
	  {
	    Dir dir;
	    if (baseDir >= CO_DIR_MAX)
	      dir = baseDir - CO_DIR_PURE;
	    else if (baseDir < CO_DIR_MAX - CO_DIR_PURE)
	      dir = baseDir + CO_DIR_PURE;
	    else
	      dir = baseDir;

	    // Only set the timer on the last shot.
	    Boolean lastOne = (baseDir == center + delta);
	    Gun::_fire(shooterId,dir,lastOne,lastOne);
	  }
    }
}



Enforcer::Enforcer(WorldP w,LocatorP l,const Pos &rawPos)
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Grounded(groundedContext,groundedXdata),
       Suicide(suicideContext,suicideXdata)
{}



Boolean Enforcer::is_suicide()
{
  return True;
}



Boolean Enforcer::prickly()
{
  return True;
}



void Enforcer::collide(PhysicalP other)
{
  if (
      // Dead Enforcers don't hurt.
      alive() &&

      // Use an AutoUse, don't destroy it.  Would be better to put this check
      // somewhere else.
      !other->is_auto_use())
    {
      other->corporeal_attack(this,ENFORCER_DAMAGE);
      attack_hook();
    }

  Grounded::collide(other);
}



void Enforcer::act()
{
  Grounded::_act();
  Suicide::_act();
  Creature::act();
}



void Enforcer::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());
  
  Grounded::die();
}


Stats Enforcer::stats;



Frog::Frog(WorldP w,LocatorP l,const Pos &rawPos,const Id &un_mapped) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Hopping(hoppingContext,hoppingXdata)
{
  PhysicalP p;
  if (l->lookup(p,un_mapped) == OL_NO_SIG)
    {
      unmapped = un_mapped;
      assert (p->get_intel() == NULL);

      timer.set(FROG_TIME);
      unmappedSet = True;
    }
  else
    unmappedSet = False;
}



void Frog::act()
{
  // alive() may not be necessary.
  if (alive() && unmappedSet)
    {
      LocatorP locator = get_locator();
      PhysicalP p = locator->lookup(unmapped);
      if (p && p->alive())
	{
	  assert(p->is_moving());
	  const Area &area = get_area();
	  ((MovingP)p)->set_middle_next(area.get_middle());
	  
	  if (timer.ready())
	    {
	      p->set_intel(get_intel());
	      p->set_mapped_next(True);
	      set_intel(NULL);
	      set_quiet_death();
	      kill_self();
	      
	      unmappedSet = False;
	    }
	}
    }
  
  if (unmappedSet)
    timer.clock();
  Hopping::act();
}



void Frog::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());
  
  if (unmappedSet)
    {
      LocatorP locator = get_locator();
      PhysicalP p;
      if ((p = locator->lookup(unmapped)) && p->alive())
	{
	  assert(p->get_intel() == NULL);
	  p->set_intel(get_intel());
	  set_intel(NULL);

	  if (get_quiet_death())
	    {
	      p->set_quiet_death();
	    }
	  else
	    {
	      p->set_mapped_next(True);
	      const Area &area = get_area();
	      assert(p->is_moving());
	      ((MovingP)p)->set_middle_next(area.get_middle());
	    }

	  if (!p->die_called())
	    {
	      /* Partial hack to prevent corpse from getting knocked out of
		 the world. */
	      const Area &area = p->get_area_next();
	      WorldP world = get_world();
	      if (!world->inside(area.middle_wsquare()))
		p->set_quiet_death();

	      p->kill_self();
	      p->die();
	    }
	  
	  set_quiet_death();
	}
    }
  Hopping::die();
}



Stats Frog::stats;



Hero::Hero(WorldP w,LocatorP l,const Pos &rawPos) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Walking(walkingContext,walkingXdata),
       Fighter(fighterContext,fighterXdata),
       User(userContext,userXdata)
{
  healthMin = get_health();
  Timer t(HERO_HEAL_TIME);
  healTimer = t;
  healTimer.set();
}



Boolean Hero::is_fighter() 
{
  return True;
}



Boolean Hero::is_walking() 
{
  return True;
}



Boolean Hero::is_user() 
{
  return True;
}



void Hero::get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,Dir dir,
			   int animNum)
{
  Fighter::get_pixmap_mask(dpyNum,pixmap,mask,dir,animNum);
}



void Hero::get_size_offset_next(Size &size,Size &offset,Dir dirNext)
{
  Fighter::get_size_offset_next(size,offset,dirNext);
}



int Hero::get_weapons_num()
{
  return User::get_weapons_num();
}



int Hero::get_items_num()
{
  return User::get_items_num();
}



PhysicalP Hero::get_weapon(int n)
{
  return User::get_weapon(n);
}



PhysicalP Hero::get_item(int n)
{
  return User::get_item(n);
}



PhysicalP Hero::get_weapon_current()
{
  return User::get_weapon_current();
}



PhysicalP Hero::get_item_current()
{
  return User::get_item_current();
}



int Hero::get_item_count()
{
  return User::get_item_count();
}



void Hero::set_mapped_next(Boolean val)
{
  User::set_mapped_next(val);
}



ClassId Hero::get_weapon_string(const char *&str)
{
  return User::get_weapon_string(str);
}



void Hero::heal()
{
  // Somewhat of a hack.  Skipping a level in calling up the tree. 
  Creature::heal();

  healthMin = get_health_max();
}



void Hero::act()
{
  // Code for healing.
  // alive_next() checks if something else has already killed this.
  if (alive() && alive_next())
    {
      // Heal Hero up to HERO_HEAL_MULTIPLIER * min health.
      Health health = get_health();
      if (health < healthMin)
	healthMin = health;
      
      if (healTimer.ready())
	{
	  Health healthBest = 
	    min(get_health_max(),((int)(healthMin * HERO_HEAL_MULTIPLIER)));
	  Health healthNext = health + HERO_HEAL;
	  if (healthNext > healthBest)
	    healthNext = healthBest;
	  
	  set_health_next(healthNext);
	  healTimer.set();
	}
      healTimer.clock();
    }

  Walking::_act();
  Fighter::_act();
  Creature::act();
  User::_act();
  // NOTE: Creature::act() before User::act();
}



void Hero::update()
{
//  Walking::_update();
  Fighter::_update();
  User::_update();
  Creature::update();
}



void Hero::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());

  // Walking::_die();
  // Fighter::_die();
  User::_die();
  Creature::die();
}



void Hero::collide(PhysicalP other) 
{
  if (!Fighter::_collide(other) && !User::_collide(other))
    Creature::collide(other);
}



void Hero::init_x(Xvars &xvars)
{
  Fighter::init_x(xvars);
}



Stats Hero::stats;



Ninja::Ninja(WorldP w,LocatorP l,const Pos &rawPos) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Sticky(stickyContext,stickyXdata),
       Fighter(fighterContext,fighterXdata),
       User(userContext,userXdata)
{}



Boolean Ninja::is_fighter() 
{
  return True;
}



Boolean Ninja::is_sticky() 
{
  return True;
}



Boolean Ninja::is_user() 
{
  return True;
}



void Ninja::get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,Dir dir,int animNum)
{
  Fighter::get_pixmap_mask(dpyNum,pixmap,mask,dir,animNum);
}



void Ninja::get_size_offset_next(Size &size,Size &offset,Dir dirNext)
{
  Fighter::get_size_offset_next(size,offset,dirNext);
}



int Ninja::get_weapons_num()
{
  return User::get_weapons_num();
}



int Ninja::get_items_num()
{
  return User::get_items_num();
}



PhysicalP Ninja::get_weapon(int n)
{
  return User::get_weapon(n);
}



PhysicalP Ninja::get_item(int n)
{
  return User::get_item(n);
}




PhysicalP Ninja::get_weapon_current()
{
  return User::get_weapon_current();
}



PhysicalP Ninja::get_item_current()
{
  return User::get_item_current();
}



int Ninja::get_item_count()
{
  return User::get_item_count();
}



void Ninja::set_mapped_next(Boolean val)
{
  User::set_mapped_next(val);
}



ClassId Ninja::get_weapon_string(const char *&str)
{
  return User::get_weapon_string(str);
}



void Ninja::act()
{
  Sticky::_act();
  Fighter::_act();
  Creature::act();
  User::_act();
  // NOTE: Creature::act() before User::act();
}



void Ninja::update()
{
  // Sticky::_update();
  Fighter::_update();
  User::_update();
  Creature::update();
}



void Ninja::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());

  // Sticky::_die();
  User::_die();
  // User::_die();
  Creature::die();
}



void Ninja::collide(PhysicalP other) 
{
  if (!Fighter::_collide(other) && !User::_collide(other))
    Creature::collide(other);
}



void Ninja::init_x(Xvars &xvars)
{
  Fighter::init_x(xvars);
}



Stats Ninja::stats;



Alien::Alien(WorldP w,LocatorP l,const Pos &rawPos) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Sticky(stickyContext,stickyXdata)
{}



Boolean Alien::is_sticky()
{
  return True;
}



Boolean Alien::prickly()
{
  return True;
}



void Alien::collide(PhysicalP other)
{
  if (
      // Dead Aliens don't hurt.
      alive() &&

      // Don't hurt huggers or eggs.
      !other->is_hugger() &&
      other->get_class_id() != A_Egg &&

      // Check that the other is not immune to Alien attacks.
      !(other->is_creature() && ((CreatureP)other)->get_alien_immune()) &&

      // Use an AutoUse, don't destroy it.  Would be better to put this check
      // somewhere else.
      !other->is_auto_use())
    {
      other->corporeal_attack(this,ALIEN_DAMAGE);
      attack_hook();
    }

  Sticky::collide(other);
}



void Alien::act()
{
  Sticky::act();
}



void Alien::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());

  Sticky::die();
}



Stats Alien::stats;



RedHugger::RedHugger(WorldP w,LocatorP l,const Pos &rawPos) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Sticky(stickyContext,stickyXdata),
       Hugger(huggerContext,huggerXdata)
{}



Boolean RedHugger::is_sticky()
{
  return True;
}



Boolean RedHugger::is_hugger()
{
  return True;
}



void RedHugger::act()
{
  Hugger::_act();
  Sticky::_act();
  Creature::act();
}



void RedHugger::collide(PhysicalP other)
{
  Hugger::collide(other);
}



GreenHugger::GreenHugger(WorldP w,LocatorP l,const Pos &rawPos) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Sticky(stickyContext,stickyXdata),
       Hugger(huggerContext,huggerXdata)
{}



Boolean GreenHugger::is_sticky()
{
  return True;
}



Boolean GreenHugger::is_hugger()
{
  return True;
}



void GreenHugger::act()
{
  Hugger::_act();
  Sticky::_act();
  Creature::act();
}



void GreenHugger::collide(PhysicalP other)
{
  Hugger::collide(other);
}



ChopperBoy::ChopperBoy(WorldP w,LocatorP l,const Pos &rawPos) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Flying(flyingContext,flyingXdata),
       User(userContext,userXdata)
{}



Boolean ChopperBoy::is_flying() 
{
  return True;
}



Boolean ChopperBoy::is_user() 
{
  return True;
}



int ChopperBoy::get_weapons_num()
{
  return User::get_weapons_num();
}



int ChopperBoy::get_items_num()
{
  return User::get_items_num();
}



PhysicalP ChopperBoy::get_weapon(int n)
{
  return User::get_weapon(n);
}



PhysicalP ChopperBoy::get_item(int n)
{
  return User::get_item(n);
}




PhysicalP ChopperBoy::get_weapon_current()
{
  return User::get_weapon_current();
}



PhysicalP ChopperBoy::get_item_current()
{
  return User::get_item_current();
}



int ChopperBoy::get_item_count()
{
  return User::get_item_count();
}



void ChopperBoy::set_mapped_next(Boolean val)
{
 User::set_mapped_next(val);
}



ClassId ChopperBoy::get_weapon_string(const char *&str)
{
  return User::get_weapon_string(str);
}



void ChopperBoy::act()
{
  Flying::_act();
  Creature::act();
  User::_act();
  // NOTE: Creature::act() before User::act();
}



void ChopperBoy::update()
{
  User::_update();
  Creature::update();
}



void ChopperBoy::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());

  User::_die();
  Creature::die();
}



void ChopperBoy::collide(PhysicalP other) 
{
  if (!User::_collide(other))
    Creature::collide(other);
}



Stats ChopperBoy::stats;



Lemming::Lemming(WorldP w,LocatorP l,const Pos &rawPos)
: Creature(creatureContext,creatureXdata,w,l,rawPos),
  Grounded(groundedContext,groundedXdata),
  Suicide(suicideContext,suicideXdata)
{}



Boolean Lemming::is_suicide()
{
  return True;
}



void Lemming::act()
{
  Grounded::_act();
  Suicide::_act();
  Creature::act();
}



void Lemming::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());
  
  Grounded::die();
}



Stats Lemming::stats;



FireDemon::FireDemon(WorldP w,LocatorP l,const Pos &rawPos) 
     : Creature(creatureContext,creatureXdata,w,l,rawPos),
       Flying(flyingContext,flyingXdata),
       BuiltIn(builtInContext,builtInXdata)
{
  swapResistance = FIRE_DEMON_SWAP_RESISTANCE;
}



Boolean FireDemon::is_flying() 
{
  return True;
}



Boolean FireDemon::is_built_in()
{
  return True;
}



ClassId FireDemon::get_weapon_string(const char *&str)
{
  return BuiltIn::get_weapon_string(str);
}



Boolean FireDemon::ready()
{
  return BuiltIn::ready();
}



void FireDemon::heat_attack(PhysicalP,int,Boolean)
{}



Boolean FireDemon::swap_protect()
{
  if (swapResistance > 0)
    {
      swapResistance--;
      return True;
    }
  return False;
}



Boolean FireDemon::frog_protect()
{
  return True;
}



Size FireDemon::get_shot_size(Dir)
{
  return Fireball::get_size();
}



PhysicalP FireDemon::create_shot(const Pos &pos,Dir dir)
{
  PhysicalP shot = 
    new Fireball(get_world(),get_locator(),pos,get_id(),dir,
		 FIRE_DEMON_FIREBALL_HEAT,FIRE_DEMON_FIREBALL_TIME);
  assert(shot);
  return shot;
}



void FireDemon::act()
{
  const Area &area = get_area();
  Pos pos;
  Size size;
  area.get_rect(pos,size);
  LocatorP locator = get_locator();

  // Generate fire.
  for (int n = 0; n < FIRE_DEMON_FIRE; n++)
    {
      Pos firePos(pos.x + Utils::choose(size.width),
		  pos.y + Utils::choose((int)(.8 * size.height))); 
      
      PhysicalP fire = new Fire(get_world(),locator,firePos,False);
      locator->add(fire);
    }

  Flying::_act();
  BuiltIn::_act();
  Creature::act();
}



void FireDemon::collide(PhysicalP other)
{
  if (alive() && !other->is_shot())
    {
      other->heat_attack(this,FIRE_DEMON_HEAT);
      attack_hook();
    }
  
  Flying::collide(other);
}



Walker::Walker(WorldP w,LocatorP l,const Pos &rawPos) 
: Creature(creatureContext,creatureXdata,w,l,rawPos),
  Walking(walkingContext,walkingXdata),
  User(userContext,userXdata),
  BuiltIn(builtInContext,builtInXdata)
{}



Boolean Walker::is_walking()
{
  return True;
}



Boolean Walker::is_user()
{
  return True;
}



Boolean Walker::is_built_in()
{
  return True;
}



int Walker::get_weapons_num()
{
  return User::get_weapons_num();
}



int Walker::get_items_num()
{
  return User::get_items_num();
}



PhysicalP Walker::get_weapon(int n)
{
  return User::get_weapon(n);
}



PhysicalP Walker::get_item(int n)
{
  return User::get_item(n);
}



PhysicalP Walker::get_weapon_current()
{
  return User::get_weapon_current();
}



PhysicalP Walker::get_item_current()
{
  return User::get_item_current();
}



int Walker::get_item_count()
{
  return User::get_item_count();
}



void Walker::set_mapped_next(Boolean val)
{
  User::set_mapped_next(val);
}



ClassId Walker::get_weapon_string(const char *&str)
{
  // I'd prefer to have this functionality higher in the class hierarchy.
  ClassId id = User::get_weapon_string(str);
  
  if (id == A_None)
    return BuiltIn::get_weapon_string(str);
  else
    return id;
}



void Walker::act()
{
  Walking::_act();
  BuiltIn::_act();
  Creature::act();
  User::_act();
  // NOTE: Creature::act() before User::act();
}



void Walker::update()
{
  // Walking::_update();
  User::_update();
  // BuiltIn::_update();
  Creature::update();
}



void Walker::die()
{
  if (!get_quiet_death())
    stats.add_death(get_birth_time());

  // Walking::_die();
  User::_die();
  // BuiltIn::_die();
  Creature::die();
}



void Walker::collide(PhysicalP other)
{
  User::collide(other);
}



int Walker::get_anim_time()
{
  return WALKER_ANIM_TIME;
}



Dir Walker::compute_weapon_dir(ITcommand command)
{
  return Laser::compute_weapon_dir(command);
}



Size Walker::get_shot_size(Dir dir)
{
  return Laser::get_size(dir);
}



PhysicalP Walker::create_shot(const Pos &pos,Dir dir)
{
  PhysicalP shot =
    new Laser(get_world(),get_locator(),pos,get_id(),dir);
  assert(shot);
  return shot;
}



Stats Walker::stats;
