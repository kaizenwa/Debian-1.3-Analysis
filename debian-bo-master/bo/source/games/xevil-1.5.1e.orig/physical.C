// "physical.C"

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
#pragma implementation "physical.h"
#endif


// Include Files
#include <iostream.h>

extern "C" {
#include <string.h>
}

#include <strstream.h>

#include "utils.h"
#include "coord.h"
#include "area.h"
#include "world.h"
#include "id.h"
#include "intel.h"
#include "locator.h"
#include "physical.h"
#include "actual.h"


// Defines
#define GRAVITY 2
#define PUSH_OFF 3
#define VEL_SMALL 1
#define MOVING_ANIM_TIME 2
#define MOVING_EXTRA_VEL_MAX 9
#define FALLING_VEL_MAX 12
#define HEAVY_DAMAGE_VEL 3
#define ITEM_CAN_TAKE_TIME 15
#define ITEM_DROP_OFFSET_WIDTH 20
#define ITEM_DROP_OFFSET_HEIGHT 20
#define EXTRA_VEL_DAMP 1
#define DROP_SPEED 2
#define FIGHTER_SLIDE_TIME 4
#define SHOT_OFFSET 19
#define HEAT_MAX 25
#define FLYING_GRAV_VEL_CUTOFF 1
#define CORPSE_TIME 140
#define FLASH_TIME 5
#define TOUCHABLE_RADIUS 20 //11 //10
#define HUGGER_TIME 90
#define USER_CYCLE_TIME 50


//// Functions
Physical::Physical(const PhysicalContext &p_c,WorldP w,LocatorP l) 
{
  idValid = False;
  world = w;
  locator = l;
  command = IT_NO_COMMAND;
  pc = &p_c;
  health = healthNext = pc->health;
  mass = massNext = pc->mass;
  dontCollide = NULL;
  intel = NULL;
  if (! staticValid)
    init_static();
  deleteMe = False;
  mapped = mappedNext = True;
  noDeathDelete = False;
  dieCalled = False;
  heat = heatNext = 0;
  previousHeatWasSecondary = False;

  // Add 1 to FLASH_TIME to draw for proper number of turns.
  Timer t(FLASH_TIME + 1);
  flashTimer = t;
  quietDeath = False;
}



 Physical::Physical()
{
  assert(0);
}



Physical::~Physical() 
{}



Dir Physical::get_dir() 
{
  return CO_air;
}



int Physical::get_drawing_level()
{
  return 1;
}



Vel Physical::get_vel() // Returns (0,0)
{
  Vel vel; 
  return vel;
} 



Boolean Physical::is_moving() 
{
  return False;
}



Boolean Physical::is_shot() 
{
  return False;
}



Boolean Physical::is_item() 
{
  return False;
}



Boolean Physical::is_shield()
{
  return False;
}



Boolean Physical::is_bomb()
{
  return False;
}



Boolean Physical::is_weapon() 
{
  return False;
}



Boolean Physical::is_cutter() 
{
  return False;
}



Boolean Physical::is_gun() 
{
  return False;
}



Boolean Physical::is_creature() 
{
  return False;
}



Boolean Physical::is_suicide()
{
  return False;
}



Boolean Physical::is_user() 
{
  return False;
}



Boolean Physical::is_fighter() 
{
  return False;
}



Boolean Physical::is_walking() 
{
  return False;
}



Boolean Physical::is_sticky() 
{
  return False;
}



Boolean Physical::is_flying()
{
  return False;
}



Boolean Physical::is_built_in()
{
  return False;
}



Boolean Physical::is_hugger()
{
  return False;
}



Boolean Physical::is_auto_use()
{
  return False;
}



Boolean Physical::prickly()
{
  return False;
}



Boolean Physical::collidable() 
{
  return True;
}



PHsig Physical::get_id(Id &outside_id)
{
  if (! idValid)
    return PH_NOT_SET;

  outside_id = id;
  return PH_NO_SIG;
}



int Physical::get_built_in_weapon_coolness()
{
  return 0;
}



PHsig Physical::set_id(const Id &new_id)
{
  id = new_id;

  if (idValid)
    return PH_ID_CHANGED;

  idValid = True;
  return PH_NO_SIG;
}



void Physical::heal()
{
  set_health_next(get_health_max());
}



void Physical::set_mapped_next(Boolean val)
{
  mappedNext = val;
}



void Physical::flash()
{
  flashTimer.set();
}



void Physical::corporeal_attack(PhysicalP,int damage) 
{
  // alive_next() checks if something else has already killed this.
  if (alive() && alive_next())
    healthNext = health - damage;
}



void Physical::heat_attack(PhysicalP,int h,Boolean secondary)
{
  assert(h >= 0);
  if (! (secondary && previousHeatWasSecondary)) // Don't propagate fires.
    {
      heatNext = heat + h;
      
      if (heatNext > HEAT_MAX)
	heatNext = HEAT_MAX;
      
      if (heat == 0 && heatNext > 0)
	{
	  LocatorP l = get_locator();
	  ostrstream str;
	  str << get_class_name() << " is on fire." << ends;
	  locator->message_enq(str.str());
	}
    }
  previousHeatWasSecondary = secondary;
}



Boolean Physical::swap_protect()
{
  return False;
}



Boolean Physical::frog_protect()
{
  return False;
}



void Physical::avoid(PhysicalP) {}



void Physical::collide(PhysicalP) {}



void Physical::act()
{  
  const Area &area = get_area();

/* This check is now done inside the locator.
  if (alive() && !world->inside(area.middle_wsquare()))
    {
      LocatorP locator = get_locator();
      ostrstream msg;
      msg << get_class_name() << " has been knocked out of the world so is dead." <<
	ends;
      locator->message_enq(msg.str()); 
      
      kill_self();
      } */

  if (heatNext > 0)
    {
      Pos pos;
      Size size;
      area.get_rect(pos,size);
      pos.x += Utils::choose(size.width);
      // Start fire in upper-half.
      if (size.height > 1)
	pos.y += Utils::choose((int)(size.height * .5)); 

      LocatorP locator = get_locator();
      PhysicalP fire = new Fire(get_world(),locator,pos);
      fire->set_dont_collide(this);
      locator->add(fire);
      heatNext--;

      // alive_next() checks if something else has already killed this.
      if (alive() && alive_next())
	healthNext -= Fire::get_damage();
    }
}



void Physical::set_quiet_death()
{
  quietDeath = True;
}



void Physical::update()
{
  mass = massNext;

  command = IT_NO_COMMAND;

  // Fire damage is now in Physical::act

  health = healthNext;
  mapped = mappedNext;
  heat = heatNext;
  flashTimer.clock();

}



void Physical::die() 
{
  assert(!dieCalled && healthNext < 0 && health >= 0);
  dieCalled = True;

  if (intel) 
    intel->die();
  intel = NULL;

  if (!noDeathDelete)
    set_delete_me();
}



int Physical::get_weapons_num()
{
  return 0;
}



int Physical::get_items_num()
{
  return 0;
}



PhysicalP Physical::get_weapon(int)
{
  return NULL;
}



PhysicalP Physical::get_item(int)
{
  return NULL;
}



PhysicalP Physical::get_weapon_current()
{
  return NULL;
}



PhysicalP Physical::get_item_current()
{
  return NULL;
}



int Physical::get_item_count()
{
  return 0;
}



ClassId Physical::get_weapon_string(const char *&str)
{
  static const char noneStr[] = "none";
  str = noneStr;
  return A_None;
}



Boolean Physical::ready()
{
  return False;
}



ITcommand Physical::get_command()
{
  return command;
}



void Physical::init_static()
{
  // CO_air_* is not really a unit.

  unitAccs[CO_center_R].ddx = 1.0;
  unitAccs[CO_center_L].ddx = -1.0;
  unitAccs[CO_air_R].ddx = .4;
  unitAccs[CO_air_L].ddx = -.4;
  unitAccs[CO_air_R].ddy = unitAccs[CO_air_L].ddy = -1.0;  // -0.95
  unitAccs[CO_air_UP].ddy = -1.0;
  unitAccs[CO_air_DN].ddy = 1.0;

  unitAccs[CO_center_R].ddy = unitAccs[CO_center_L].ddy = 
      unitAccs[CO_air].ddy = unitAccs[CO_air].ddx = 
	unitAccs[CO_center].ddx = 
      unitAccs[CO_center].ddy = unitAccs[CO_air_UP].ddx = 
	unitAccs[CO_air_DN].ddx = 
	  0.0;

  unitAccs[CO_r].ddx = unitAccs[CO_r].ddy =
      unitAccs[CO_dn].ddx = unitAccs[CO_dn].ddy =
	unitAccs[CO_l].ddx = unitAccs[CO_l].ddy =
	  unitAccs[CO_up].ddx = unitAccs[CO_up].ddy =
	    0.0;
  
  unitAccs[CO_r_DN].ddx = unitAccs[CO_r_UP].ddx =
    unitAccs[CO_dn_R].ddy = unitAccs[CO_dn_L].ddy =
      unitAccs[CO_l_DN].ddx = unitAccs[CO_l_UP].ddx =
	unitAccs[CO_up_R].ddy = unitAccs[CO_up_L].ddy =
	  unitAccs[CO_climb_DN].ddx = unitAccs[CO_climb_UP].ddx =
	    0.0;
  
  unitAccs[CO_r_DN].ddy = unitAccs[CO_l_DN].ddy = unitAccs[CO_climb_DN].ddy = 
    0.5;
  
  unitAccs[CO_r_UP].ddy = unitAccs[CO_l_UP].ddy = unitAccs[CO_climb_UP].ddy =
    -0.5;
  
  unitAccs[CO_dn_R].ddx = unitAccs[CO_up_R].ddx = 0.5;
  
  unitAccs[CO_dn_L].ddx = unitAccs[CO_up_L].ddx = -0.5;
  
  int n;
  for (n = 0; n < 16; n++)
    {
      float theta = M_PI * n / 8.0;

      unitAccs[CO_R + n].ddx = cos(theta);
      unitAccs[CO_R + n].ddy = sin(theta);
    }

  for (n = 0; n < CO_DIR_MAX; n++)
    unitVels[n] = unitAccs[n];

  staticValid = True;
}



Boolean Physical::staticValid = False;
Acc Physical::unitAccs[CO_DIR_MAX];
Vel Physical::unitVels[CO_DIR_MAX];



Protection::Protection(const ProtectionContext &pr_c,ProtectionXdata &x_data,
		       WorldP w,LocatorP l,const Area &ar)
  : Physical(pr_c.physicalContext,w,l)
{
  area = areaNext = areaBaseNext = ar;
  delta = 0;
  pXdata = &x_data;
  prc = &pr_c;
}



Boolean Protection::collidable()
{
  return False;
}



const Area &Protection::get_area()
{
  return area;
}



const Area &Protection::get_area_next()
{
  return areaNext;
}



void Protection::follow(const Area &aBaseNext)
{
  areaBaseNext = aBaseNext;

  Pos basePos;
  Size baseSize;
  areaBaseNext.get_rect(basePos,baseSize);
  Pos nPos(basePos.x - delta,basePos.y - delta);
  Size nSize;
  nSize.width = baseSize.width + 2 * delta;
  nSize.height = baseSize.height + 2 * delta;

  if (nSize.width < 0)
    nSize.width = 0;
  if (nSize.height < 0)
    nSize.height = 0;

  Area nArea(AR_RECT,nPos,nSize);
  areaNext = nArea;
}



void Protection::draw(Drawable buffer,Xvars &xvars,int dpyNum,const Area &bufArea)
{
  if (!pXdata->valid)
    init_x(xvars);

  Pos pos;
  Size size;
  area.get_rect(pos,size);
  Size offset = area - bufArea;

  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],pXdata->color[dpyNum]);
  XDrawRectangle(xvars.dpy[dpyNum],buffer,xvars.gc[dpyNum],
		 offset.width,offset.height,
		 size.width - 1,size.height - 1);
  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],xvars.black[dpyNum]);
}



void Protection::act()
{
  delta++;
  if (delta == DELTA_MAX)
    delta = DELTA_MIN;

  // Updates areaNext.
  follow(areaBaseNext);

  Physical::act();
}



void Protection::update()
{
  area = areaNext;
  
  Physical::update();
}



void Protection::init_x(Xvars &xvars)
{
  assert(!pXdata->valid);
  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    pXdata->color[dpyNum] = 
      xvars.allocNamedColor(dpyNum,prc->colorName,xvars.black[dpyNum]);
  pXdata->valid = True;
}



Moving::Moving(const MovingContext &m_c,
		MovingXdata &x_data,
		WorldP w,	       
		LocatorP l,
		const Pos &raw_pos,
		Dir dirInitial)
     : Physical(m_c.physicalContext,w,l)
{
  movingXdata = &x_data;
  mc = &m_c;

  rawPos = rawPosNext = raw_pos;
  rawPosChanged = False;
  dir = dirNext = dirInitial; /* Don't change.  See Shell. */

  if (m_c.animMax[dir])
    movingAnimNum = Utils::choose(m_c.animMax[dir]);
  else
    movingAnimNum = 0;
  
  // offsets[CO_air] and sizes[CO_air] are guaranteed to be set.
  Area nArea(AR_RECT,raw_pos + mc->offsets[dir],mc->sizes[dir]);
  area = areaNext = nArea;

  WorldP world = get_world();
  hitWall = hitWallNext = !world->open(area);
  
  extraVelNextSet = False;

  assert(context_valid());

  invisible = invisibleNext = False;

  attackHook = NULL;

  // NOTE: protection initializes itself.
};



Moving::Moving()
{
  assert(0);
}



Boolean Moving::is_moving()
{
  return True;
}



const Area &Moving::get_area() 
{
  return area;
}



const Area &Moving::get_area_next() 
{
  return areaNext;
}



Vel Moving::get_vel()
{
  return vel;
}



Boolean Moving::ignore_walls()
{
  return False;
}



Dir Moving::get_dir() 
{
  return dir;
}



Id Moving::get_protection()
{
  return protection;
}



void Moving::set_middle_next(const Pos &middleNext)
{
  Pos pos;
  Size size;
  area.get_rect(pos,size);

  Pos rPosNext = middleNext - mc->offsets[dir] - 0.5 * size;

  /* Important to set rawPosChanged if called before act. */
  set_raw_pos_next(rPosNext);  

  /* Only matters if called after act.  Otherwise is just redundant. */
  update_next(); 
}



void Moving::relocate(const Pos &rPos)
{
  assert(area == areaNext);
  assert(dir == dirNext);
  assert(vel == velNext);
  assert(rawPos == rawPosNext);
//  assert(extraVel == extraVelNext);
  assert(hitWall == hitWallNext);

  Pos pos;
  Size size;
  area.get_rect(pos,size);
  Size delta = pos - rawPos;
  
  rawPos = rawPosNext = rPos;
  Area nArea(AR_RECT,rawPos + delta,size);
  area = areaNext = nArea;

  hitWall = hitWallNext = False;
}



void Moving::set_mapped_next(Boolean val)
{
  LocatorP locator = get_locator();
  PhysicalP p = locator->lookup(protection);
  if (p)
    p->set_mapped_next(val);

  Physical::set_mapped_next(val);
}



void Moving::set_protection(const Id &id)
{
  protection = id;
}



void Moving::corporeal_attack(PhysicalP killer,int damage)
{
  // Protection from corporeal attacks.
  LocatorP l = get_locator();
  ProtectionP p = (ProtectionP)l->lookup(protection);
  if (p && p->corporeal_protect(damage))
    return;

  Physical::corporeal_attack(killer,damage);
}



void Moving::heat_attack(PhysicalP killer,int h,Boolean secondary)
{
  // Protection from heat.
  LocatorP l = get_locator();
  ProtectionP p = (ProtectionP)l->lookup(protection);
  if (p && p->heat_protect(h,secondary))
    return;

  Physical::heat_attack(killer,h,secondary);
}



void Moving::act()
{
  // Note: no restrictions on vel.
  extraVel.limit(MOVING_EXTRA_VEL_MAX);
  Vel sumVel = velNext + extraVel;
  extraVel.damp(EXTRA_VEL_DAMP);
  
  if (!rawPosChanged)
    rawPosNext = rawPos + sumVel;
  
  update_next();
  
 Physical::act();
}



void Moving::update()
{
  rawPos = rawPosNext;  
  rawPosChanged = False;
  
  area = areaNext;

  if(mc->animMax[dir])
    {
      // Timer so that one frame may be shown over multiple turns.
      if (animTimer.ready())
	{
	  movingAnimNum = (movingAnimNum + 1) % mc->animMax[dir];
	  animTimer.set(get_anim_time());
	}
      animTimer.clock();
    }

  // If direction changed, reset animation counter.
  if (dir != dirNext)
    movingAnimNum = 0;
  dir = dirNext;
  
  hitWall = hitWallNext;
  vel = velNext;  

  if (extraVelNextSet)
    {
      extraVel = extraVelNext;
      extraVelNextSet = False;
    }

  invisible = invisibleNext;
  
  Physical::update();
}



void Moving::draw(Drawable buffer,Xvars &xvars,int dpyNum,const Area &bufArea)
{    
  // Initialize X variables if necessary.
  if (!movingXdata->valid)
    init_x(xvars);
  
  Pos pos;
  Size size;
  area.get_rect(pos,size);
  Size offset = area - bufArea;

  Pixmap scratch;
  if (invisible)
    {
      LocatorP locator = get_locator();
      scratch = locator->get_scratch_buffer(dpyNum);
  
      // Which way to distort the image.
      // We will draw the image twice to get the wrap-around.
      // Copy portion of area into scratch buffer.
      XCopyArea(xvars.dpy[dpyNum],buffer,scratch,xvars.gc[dpyNum],
		offset.width,offset.height,
		size.width,size.height,
		0,size.height-3);
      XCopyArea(xvars.dpy[dpyNum],buffer,scratch,xvars.gc[dpyNum],
		offset.width,offset.height,
		size.width,size.height,
		0,-3);
    }
  
  Pixmap pixmap,mask;
  get_pixmap_mask(dpyNum,pixmap,mask,dir,movingAnimNum);
  
  XSetClipMask(xvars.dpy[dpyNum],xvars.gc[dpyNum],mask);
  XSetClipOrigin(xvars.dpy[dpyNum],xvars.gc[dpyNum],
		 offset.width,offset.height);
  
  if (invisible)
    // Use scratch buffer.
    XCopyArea(xvars.dpy[dpyNum],scratch,
	      buffer,xvars.gc[dpyNum],0,0,
	      size.width,size.height,
	      offset.width,offset.height);
  else 
    // Use supplied pixmap.
    XCopyArea(xvars.dpy[dpyNum],pixmap,
	      buffer,xvars.gc[dpyNum],0,0,
	      size.width,size.height,
	      offset.width,offset.height);
  
  // Restore gc to initial state.
  XSetClipMask(xvars.dpy[dpyNum],xvars.gc[dpyNum],None);
  XSetClipOrigin(xvars.dpy[dpyNum],xvars.gc[dpyNum],0,0);

  // Don't draw outline if invisible.
  if (!invisible)
    {
      // Draw outline around object if controlled by a Human or if a slave to
      // a Human master.
      IntelP intel = get_intel();
      IntelP master = NULL;
      Boolean drawOutline = False;
      if (intel)
	{
	  if (intel->is_human())
	    drawOutline = True;
	  else
	    {
	      LocatorP locator = get_locator();
	      master = 
		locator->lookup(((MachineP)intel)->get_master_intel_id());
	      if (master && master->is_human())
		drawOutline = True;
	    }
	}
      if (drawOutline)
	{
	  ColorNum colorNum;
	  if (intel->is_human())
	    colorNum = ((HumanP)intel)->get_color_num();
	  else
	    {
	      assert(master && master->is_human());
	      colorNum = ((HumanP)master)->get_color_num();
	    }
	  
	  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],
			 xvars.humanColors[dpyNum][colorNum]);
	  XPoint points[12];

	  points[0].x = offset.width;
	  points[0].y = offset.height;
	  
	  points[1].x = offset.width + 1;
	  points[1].y = offset.height;
	  
	  points[2].x = offset.width;
	  points[2].y = offset.height + 1;
	  

	  points[3].x = offset.width + size.width - 1;
	  points[3].y = offset.height;
	  
	  points[4].x = offset.width + size.width - 2;
	  points[4].y = offset.height;
	  
	  points[5].x = offset.width + size.width - 1;
	  points[5].y = offset.height + 1;
	  

	  points[6].x = offset.width;
	  points[6].y = offset.height + size.height - 1;
	  
	  points[7].x = offset.width;
	  points[7].y = offset.height + size.height - 2;
	  
	  points[8].x = offset.width + 1;
	  points[8].y = offset.height + size.height - 1;
	  

	  points[9].x = offset.width + size.width - 1;
	  points[9].y = offset.height + size.height - 1;

	  points[10].x = offset.width + size.width - 2;
	  points[10].y = offset.height + size.height - 1;

	  points[11].x = offset.width + size.width - 1;
	  points[11].y = offset.height + size.height - 2;

	  XDrawPoints(xvars.dpy[dpyNum],buffer,xvars.gc[dpyNum],points,
		      12,CoordModeOrigin);
	  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],
			 xvars.black[dpyNum]);
	}
    }
}



#if 0
void Moving::draw(Drawable buffer,Xvars &xvars,int dpyNum,const Area &bufArea)
{    
  if (!movingXdata->valid)
    init_x(xvars);

  Pos pos;
  Size size;
  area.get_rect(pos,size);
  Size offset = area - bufArea;

  Pixmap pixmap,mask;
  get_pixmap_mask(dpyNum,pixmap,mask,dir,movingAnimNum);
  
  XSetClipMask(xvars.dpy[dpyNum],xvars.gc[dpyNum],mask);
  XSetClipOrigin(xvars.dpy[dpyNum],xvars.gc[dpyNum],offset.width,offset.height);

  XCopyArea(xvars.dpy[dpyNum],pixmap,
	    buffer,xvars.gc[dpyNum],0,0,
	    size.width,size.height,
	    offset.width,offset.height);
  
  // Restore gc to initial state.
  XSetClipMask(xvars.dpy[dpyNum],xvars.gc[dpyNum],None);
  XSetClipOrigin(xvars.dpy[dpyNum],xvars.gc[dpyNum],0,0);


  // Draw outline around object if controlled by a Human or if a slave to
  // a Human master.
  IntelP intel = get_intel();
  IntelP master = NULL;
  Boolean drawOutline = False;
  if (intel)
    {
      if (intel->is_human())
	drawOutline = True;
      else
	{
	  LocatorP locator = get_locator();
	  master = 
	    locator->lookup(((MachineP)intel)->get_master_intel_id());
	  if (master && master->is_human())
	    drawOutline = True;
	}
    }
  if (drawOutline)
    {
      ColorNum colorNum;
      if (intel->is_human())
	colorNum = ((HumanP)intel)->get_color_num();
      else
	{
	  assert(master && master->is_human());
	  colorNum = ((HumanP)master)->get_color_num();
	}
      
      XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],
		     xvars.humanColors[dpyNum][colorNum]);
      XPoint points[12];

      points[0].x = offset.width;
      points[0].y = offset.height;
      
      points[1].x = offset.width + 1;
      points[1].y = offset.height;
      
      points[2].x = offset.width;
      points[2].y = offset.height + 1;
      

      points[3].x = offset.width + size.width - 1;
      points[3].y = offset.height;
      
      points[4].x = offset.width + size.width - 2;
      points[4].y = offset.height;
      
      points[5].x = offset.width + size.width - 1;
      points[5].y = offset.height + 1;
      

      points[6].x = offset.width;
      points[6].y = offset.height + size.height - 1;
      
      points[7].x = offset.width;
      points[7].y = offset.height + size.height - 2;
      
      points[8].x = offset.width + 1;
      points[8].y = offset.height + size.height - 1;
      

      points[9].x = offset.width + size.width - 1;
      points[9].y = offset.height + size.height - 1;

      points[10].x = offset.width + size.width - 2;
      points[10].y = offset.height + size.height - 1;

      points[11].x = offset.width + size.width - 1;
      points[11].y = offset.height + size.height - 2;

      XDrawPoints(xvars.dpy[dpyNum],buffer,xvars.gc[dpyNum],points,
		  12,CoordModeOrigin);
      XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],xvars.black[dpyNum]);
    }
}
#endif



void Moving::avoid(PhysicalP other)
{
  const Area &otherArea = other->get_area();
  Size offset = area.avoid_no_up(otherArea);
  set_raw_pos_next(rawPos + offset);
}



void Moving::collide(PhysicalP other)
{
  Mass m1 = this->get_mass();
  Vel v1 = this->get_vel();
  Mass m2 = other->get_mass();
  Vel v2 = other->get_vel();

  if (m1 > 0 && m2 > 0) // Don`t have collisions with objects of zero mass.
    {
      Vel newVel(compute_collision(m1,v1.dx,m2,v2.dx),
		 compute_collision(m1,v1.dy,m2,v2.dy)); 
      set_vel_next(0);
      extraVel = newVel;
    }
}



void Moving::die()
{
  LocatorP locator = get_locator();
  PhysicalP p = locator->lookup(protection);
  // Must be careful about killing p because we are in the die() phase.
  if (p && !p->die_called())
    {
      p->kill_self();
      p->die();
    }
  Physical::die();
} 



int Moving::get_anim_time()
{
  return MOVING_ANIM_TIME;
}



void Moving::update_next()
{
  // Compute areaNext and hitWallNext.  May modify rawPosNext or dirNext.
  Size size,off_set;
  get_size_offset_next(size,off_set,dirNext);
  Area first_guess(AR_RECT,rawPosNext + off_set,size);
    
  if (ignore_walls())
    {
      areaNext = first_guess;
      hitWallNext = False;
    }
  else
    {
      WorldP world = get_world();
      Size offset;
      
      // Should we use velNext or vel?
      switch (world->open_offset(offset,first_guess,vel)) {
      case W_NO_SIG:
	areaNext = first_guess;
	hitWallNext = False;
	break;
	
      case W_CLOSE:
	rawPosNext = rawPosNext + offset;
	areaNext = first_guess;
	areaNext.shift(offset);
	hitWallNext = True;
	/* Doesn't set velocity to 0. */
	break;
	
      case W_CLOSE_BAD:
	rawPosNext = rawPosNext + offset;
	areaNext = first_guess;
	areaNext.shift(offset);
	hitWallNext = True;
	set_vel_next(0); // ????
	break;
	
      case W_FAILURE:
	/* Abort all changes to area, rawPos, or dir. */
	areaNext = area;
	rawPosNext = rawPos;
	dirNext = dir; 
	hitWallNext = True;
	set_vel_next(0); // ??? Will help? 8/18/94
	break;
      }
    }
  
  LocatorP locator = get_locator();
  ProtectionP protectionP = (ProtectionP)locator->lookup(protection);
  if (protectionP)
    protectionP->follow(areaNext);
}
  


void Moving::get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			     Dir dir,int animNum)
{
  pixmap = movingXdata->pixmaps[dpyNum][dir][animNum];
  mask = movingXdata->masks[dpyNum][dir][animNum];
}



void Moving::get_size_offset_next(Size &size,Size &offset,Dir)
{
  size = mc->sizes[dirNext];
  offset = mc->offsets[dirNext];
}



void Moving::init_x(Xvars &xvars) 
{
  assert(movingXdata->valid == False);

  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    for (int n = 0; n < CO_DIR_MAX; n++)
      for (int m = 0; m < mc->animMax[n]; m++)
	{
	  movingXdata->pixmaps[dpyNum][n][m] = 
	    XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
					mc->pixmapBits[n][m],
					mc->sizes[n].width,mc->sizes[n].height,
					xvars.
					allocNamedColor(dpyNum,
							mc->foreColorName,
							mc->foreWhiteDefault ? 
							xvars.white[dpyNum]:
							xvars.black[dpyNum]),
					xvars.
					allocNamedColor(dpyNum,
							mc->backColorName,
							mc->backWhiteDefault ?
							xvars.white[dpyNum]: 
							xvars.black[dpyNum]),
					xvars.depth[dpyNum]);
	  movingXdata->masks[dpyNum][n][m] = 
	    XCreateBitmapFromData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				  mc->maskBits[n][m],
				  mc->sizes[n].width,mc->sizes[n].height);
	}
  movingXdata->valid = True;
}



Boolean Moving::context_valid()
{
/*  if (!(mc->sizes[CO_climb] == mc->sizes[CO_climb_UP]) ||
      !(mc->sizes[CO_climb] == mc->sizes[CO_climb_DN]))
    return False; */ /* frog.bitmaps */

  // See Moving::corner
  if (!(mc->sizes[CO_r] == mc->sizes[CO_r_UP]) ||
      !(mc->sizes[CO_r] == mc->sizes[CO_r_DN]) ||
      !(mc->sizes[CO_dn_R] == mc->sizes[CO_dn_L]) ||
      !(mc->sizes[CO_l] == mc->sizes[CO_l_UP]) ||
      !(mc->sizes[CO_l] == mc->sizes[CO_l_DN]) ||
      !(mc->sizes[CO_up_R] == mc->sizes[CO_up_L]))
    return False;
  // No longer true that CO_dn or CO_up have to be same size as CO_dn_[RL] and
  // CO_up_[RL], respectively.  Hugger.

  
  // Objects do not need CO_air


  for (int dir = 0; dir < CO_DIR_MAX; dir++)
    {
      if (mc->animMax[dir] < 0)
	return False;
      for (int m = 0; m < mc->animMax[dir]; m++)
	if ((mc->pixmapBits[dir][m] == 0) || (mc->maskBits[dir][m] == 0))
	  return False;
    }
  return True;
}



float Moving::compute_collision(Mass m1,float v1,Mass m2,float v2)
{
  assert(m1 > 0 && m2 > 0);

  float A = m1 * v1 + m2 * v2;
  float B = 0.5 * m1 * v1 * v1 + 0.5 * m2 * v2 * v2;
  
  float a = (m1*m2 + m1*m1);
  float b = -2 * m1 * A;
  float c = A * A - 2 * m2 * B;

  float disc = b * b - 4 * a * c;

  /* Hack.  If the discriminant is negative for some reason, returning 0 
     shouldn't do too much damage. */
  if (disc < 0)
    return 0;

  float v1_1 = (-b + sqrt(disc))/ (2 * a);
  float v1_2 = (-b - sqrt(disc))/ (2 * a);
  
  return fabs(v1_1 - v1) > fabs(v1_2 - v1) ? v1_1 : v1_2;
}



Shot::Shot(const ShotContext &s_c,ShotXdata &xdata,WorldP w,LocatorP l,
	   const Pos &p,const Id &shoot_er,
	   Dir shotDir,Dir movingDir) 
     : Moving(s_c.movingContext,xdata,w,l,p,movingDir)
{
  context = &s_c;
  shooter = shoot_er;

  const Vel *unitVels = get_unit_vels();
  set_vel_next(context->speed * unitVels[shotDir]);
  if (hit_wall())
    kill_self();
}



Boolean Shot::is_shot()
{
  return True;
}



void Shot::avoid(PhysicalP) {}



void Shot::collide(PhysicalP other)
{
  if (other->is_shot())
    return;

  // If we start out in a wall, don't hurt anyone.
  // Will kill_self() in ::act().
  if (hit_wall())
    return;

  PhysicalP p;
  LocatorP locator = get_locator();
  if (locator->lookup(p,shooter) == OL_NO_SIG)
    other->corporeal_attack(p,context->damage);
  else
    other->corporeal_attack(NULL,context->damage);

  kill_self();
}



void Shot::act()
{
  if (hit_wall())  // Used to be hit_wall_next() but lances died too soon.
    kill_self();

  Moving::act();
}



Falling::Falling(const FallingContext &f_c,FallingXdata &x_data,
		 WorldP w,LocatorP l,const Pos &raw_pos,Dir dirInitial) :
       Moving(f_c.movingContext,x_data,w,l,raw_pos,dirInitial) 
{}



void Falling::act()
{
  Touching touching;
  Hanging hanging;

  const Area &area = get_area();
  WorldP world = get_world();
  world->touchingHanging(touching,hanging,area);

  // Account for gravity.  
  Vel velNext = get_vel_next();
  if (touching != CO_dn) 
    velNext.dy += GRAVITY; 
  else 
    velNext.dy = 0;

  if (touching == CO_r || touching == CO_l)
    velNext.dx = 0;

  velNext.limit(FALLING_VEL_MAX);
  set_vel_next(velNext);

  Moving::act();
}



Touchable::Touchable(const TouchableContext &t_c,TouchableXdata &x_data,
		     WorldP w,LocatorP l,const Pos &raw_pos) 
: Falling(t_c.fallingContext,x_data,w,l,raw_pos) 
{
  touched = False;
  context = &t_c;
}



int Touchable::get_drawing_level()
{
  return 0;
}



Boolean Touchable::collidable()
{
  return False;
}



void Touchable::act()
{
  if (!touched)
    {
      PhysicalP nearby[OL_NEARBY_MAX];
      int nItems;
      LocatorP l = get_locator();
      l->get_nearby(nearby,nItems,this,TOUCHABLE_RADIUS);
      const Area &area = get_area();
      
      for (int n = 0; n < nItems; n++)
	{
	  IntelP intel = nearby[n]->get_intel();
	  if (intel && intel->is_human())
	    {
	      const Area &otherArea = nearby[n]->get_area();
	      if (area.overlap(otherArea))
		touched = True;
	    }
	}
    }

  Falling::act();
}



Heavy::Heavy(const HeavyContext &h_c,HeavyXdata &x_data,
	     WorldP w,LocatorP l,const Pos &raw_pos) 
     : Falling(h_c.fallingContext,x_data,w,l,raw_pos) 
{
  context = &h_c;
}



void Heavy::collide(PhysicalP other)
{
  const Vel &vel = get_vel();
  if (vel.dy >= HEAVY_DAMAGE_VEL)
    {
      const Area &area = get_area();
      const Area &otherArea = other->get_area();
      const Pos &otherMiddle = otherArea.get_middle();

      Dir dirTo = area.dir_to(otherMiddle);
      if ((dirTo == CO_DN_R) || (dirTo == CO_DN) || (dirTo == CO_DN_L))
	other->corporeal_attack(this,context->damage);
    }
  Falling::collide(other);
}



Item::Item(const ItemContext &c_x,
	   ItemXdata &x_data,
	   WorldP w,
	   LocatorP l,
	   const Pos &pos) :
       Falling(c_x.fallingContext,x_data,w,l,pos) 
{
  Timer ntimer(ITEM_CAN_TAKE_TIME);
  canTake = ntimer;
  dieMessage = DESTROYED;
  held = False;
  context = &c_x;
  cantTake = False;
}



int Item::get_drawing_level()
{
  return 2;
}



Id Item::get_holder_id()
{
  if (held)
    return holderId;
  else
    {
      Id invalid;
      return invalid;
    }
}



void Item::set_quiet_death()
{
  dieMessage = NONE;
  Falling::set_quiet_death();
}



void Item::follow_user(const Pos &pos,Dir)
{
  set_middle_next(pos);
}



void Item::taken(PhysicalP p) 
{
  assert(!held);
  assert(!cantTake);
  held = True; 
  holderId = p->get_id();
  set_mapped_next(False);
}



void Item::dropped(PhysicalP dropper)
{
  // Can happen in update phase (from User::die).  

  LocatorP locator = get_locator();
  assert(held);
  assert(locator->lookup(holderId) == dropper);

  held = False; 
  const Area area = dropper->get_area();

  Pos middleNext = area.get_middle(); 

  // Decide whether to drop to side or below.
  if (dropper->is_creature())
    {
      Touching touching = ((CreatureP)dropper)->get_touching_area();
      if (touching == CO_dn)
	{
	  switch(dropper->get_dir()) {
	  case CO_center_R:
	  case CO_dn_R:
	    middleNext.x -= ITEM_DROP_OFFSET_WIDTH;
	    break;
	  case CO_center_L:
	  case CO_dn_L:
	    middleNext.x += ITEM_DROP_OFFSET_WIDTH;
	    break;
	  default:
	    middleNext.x += (Utils::coinFlip() ? ITEM_DROP_OFFSET_WIDTH : 
			     - ITEM_DROP_OFFSET_WIDTH);
	  };
	}
      else
	middleNext.y += ITEM_DROP_OFFSET_HEIGHT;
    }
  else
    middleNext.x += 
      (Utils::coinFlip() ? ITEM_DROP_OFFSET_WIDTH : - ITEM_DROP_OFFSET_WIDTH);

  set_middle_next(middleNext);
  canTake.set();
  set_mapped_next(True);
}



void Item::use(PhysicalP)
{
  dieMessage = USED;
}



void Item::act()
{
  canTake.clock();
  Falling::act();
}



void Item::die()
{
  LocatorP locator = get_locator();
  ostrstream msg;
  switch (dieMessage) {
  case NONE:
    break;
  case USED:
    msg << get_class_name() << " has been used." << ends;
    locator->message_enq(msg.str());
    break;
  case DESTROYED:
    msg << get_class_name() << " is destroyed." << ends;
    locator->message_enq(msg.str());
    break;
  };

 Falling::die();
}



AutoUse::AutoUse(const AutoUseContext &c,
		 AutoUseXdata &x_data,
		 WorldP w,
		 LocatorP l,
		 const Pos &pos) :
Item(c.itemContext,x_data,w,l,pos) 
{
  context = &c;
}



Boolean AutoUse::is_auto_use()
{
  return True;
}



void AutoUse::collide(PhysicalP other)
{
  IntelP otherIntel = other->get_intel();

  // First, ways in which AutoUse doesn't automatically use() itself.
  if (!other->is_creature() || 
      other->is_user() || 
      !otherIntel ||
      (!otherIntel->is_human() && context->humansOnly))
    Item::collide(other);
  else
    use(other);
}



Shield::Shield(const ShieldContext &s_c,
	       ShieldXdata &x_data,
	       WorldP w,
	       LocatorP l,
	       const Pos &pos) 
: AutoUse(s_c.autoUseContext,x_data,w,l,pos) 
{}



Boolean Shield::is_shield()
{
  return True;
}



void Shield::use(PhysicalP p)
{
  assert(p->is_moving());

  LocatorP l = get_locator();
  Id id = ((MovingP)p)->get_protection();
  ProtectionP oldProtection = (ProtectionP) l->lookup(id);
  if (oldProtection)
    oldProtection->kill_self();

  ProtectionP protection = create_protection(p->get_area());
  LocatorP locator = get_locator();
  locator->add(protection);
  ((MovingP)p)->set_protection(protection->get_id());

  kill_self();
  AutoUse::use(p);
}



Animated::Animated(const AnimatedContext &a_c,
		   AnimatedXdata &x_data,
		   WorldP w,
		   LocatorP l,
		   const Pos &pos) :
       Item(a_c.itemContext,x_data.itemXdata,w,l,pos) 
{
  animatedXdata = &x_data;
  ac = &a_c;
  frame = frameNext = 0;
  animatedAnimNum = Utils::choose(ac->animMax[frame]);

  assert(context_valid());
}



void Animated::get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,Dir,int)
{
  pixmap = animatedXdata->pixmaps[dpyNum][frame][animatedAnimNum];
  mask = animatedXdata->masks[dpyNum][frame][animatedAnimNum];
}



void Animated::get_size_offset_next(Size &size,Size &offset,Dir)
{
  size = ac->size;
  offset.width = offset.height = 0;
}



void Animated::init_x(Xvars &xvars) 
{
  assert(animatedXdata->valid == False);

  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    for (int f = 0; f < PH_FRAME_MAX; f++)
      for (int n = 0; n < ac->animMax[f]; n++)
	{
	  animatedXdata->pixmaps[dpyNum][f][n] = 
	    XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
					ac->pixmapBits[f][n],
					ac->size.width,ac->size.height,
					xvars.black[dpyNum],
					xvars.allocNamedColor(dpyNum,
							      ac->colorName),
					xvars.depth[dpyNum]);
	  animatedXdata->masks[dpyNum][f][n] = 
	    XCreateBitmapFromData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				  ac->maskBits[f][n],
				  ac->size.width,ac->size.height);
      }
  animatedXdata->valid = True;
  
  Item::init_x(xvars);
}



void Animated::update()
{
  animatedAnimNum = (animatedAnimNum + 1) % ac->animMax[frame];
  
  if (frame != frameNext)
    animatedAnimNum = 0;
  frame = frameNext;

 Item::update();
}



Boolean Animated::context_valid()
{
  for (int f = 0; f < PH_FRAME_MAX; f++)
    {
      if (ac->animMax[f] < 0)
	return False;
      for (int m = 0; m < ac->animMax[f]; m++)
	if ((ac->pixmapBits[f][m] == 0) || (ac->maskBits[f][m] == 0))
	  return False;
    }

  return True;
}



Weapon::Weapon(const WeaponContext &c_x,
	       WeaponXdata &x_data,
	       WorldP w,
	       LocatorP l,
	       const Pos &pos) 
     : Item(c_x.itemContext,x_data,w,l,pos) 
{
  enteredScope = False;
  wc = &c_x;
}



Boolean Weapon::ready()
{
  assert(0);
  return False;
}



void Weapon::fire(const Id &,ITcommand)
{}



void Weapon::enter_scope_next(PhysicalP)
{
/*  #ifdef PRINT_ERRORS
  if (enteredScope)
    cerr << "Warning:: " << get_class_name() << " already entered scope." << endl;
    #endif */
  enteredScope = True;
}



void Weapon::leave_scope_next(PhysicalP)
{
/* #ifdef PRINT_ERRORS
  if (!enteredScope)
    cerr << "Warning:: " << get_class_name() << " not in scope." << endl;
    #endif */
  enteredScope = False;
}



void Weapon::take_ammo_from(WeaponP other)
{
  int thisAmmo = get_ammo();
  int otherAmmo = other->get_ammo();

  if ((thisAmmo != PH_AMMO_UNLIMITED) && (otherAmmo != PH_AMMO_UNLIMITED))
    {
      int maxTake = get_ammo_max() - thisAmmo;
      int trans = min(maxTake,otherAmmo);
      
      set_ammo(thisAmmo + trans);
      other->set_ammo(otherAmmo - trans);
    }
}



Cutter::Cutter(const CutterContext &c_x,CutterXdata &x_data,
       WorldP w,LocatorP l,const Pos &pos)
     : Weapon(c_x.weaponContext,x_data.weaponXdata,w,l,pos)
{
  inScope = inScopeNext = False;
  context = &c_x;
  cutterXdata = &x_data;
}



Boolean Cutter::is_cutter()
{
  return True;
}



Boolean Cutter::ready()
{
  return False;
}



int Cutter::get_ammo()
{
  return PH_AMMO_UNLIMITED;
}



int Cutter::get_ammo_max()
{
  return PH_AMMO_UNLIMITED;
}



void Cutter::get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			     Dir dir,int animNum)
{
  if (!inScope)
    {
      pixmap = cutterXdata->unheldPixmap[dpyNum];
      mask = cutterXdata->unheldMask[dpyNum];
    }
  else
    Weapon::get_pixmap_mask(dpyNum,pixmap,mask,dir,animNum);
}


  
void Cutter::get_size_offset_next(Size &size,Size &offset,
				  Dir dirNext)
{
  if (!inScope)
    {
      size = context->unheldSize;
      offset = context->unheldSize;
    }
  else
    Weapon::get_size_offset_next(size,offset,dirNext);
}



Boolean Cutter::ignore_walls()
{
  return is_held();
}


			     
void Cutter::set_ammo(int)
{
  assert(0);
}



void Cutter::follow_user(const Pos &userMiddle,Dir userDir)
{
  if (inScope)
    {
      set_dir_next(userDir);
      set_middle_next(userMiddle + context->offsets[userDir]);
    }
  else
    set_middle_next(userMiddle);
}



void Cutter::enter_scope_next(PhysicalP user)
{
  if(!entered_scope())
    {
      set_dont_collide(user);
      set_mapped_next(True);
      killerId = user->get_id();
      set_dir_next(CO_center);
      inScopeNext = True;
      
      Weapon::enter_scope_next(user);
    }
#ifdef PRINT_ERRORS
  else
    cerr << "Warning:: " << get_class_name() << " entered scope twice." << endl;
#endif
}



void Cutter::leave_scope_next(PhysicalP user)
{
  if (entered_scope())
    {
      set_dont_collide(NULL);
      set_mapped_next(False);
      set_dir_next(CO_air);
      inScopeNext = False;
      
      Weapon::leave_scope_next(user);
    }
#ifdef PRINT_ERRORS
  else
    cerr << "Warning:: " << get_class_name() << " left scope twice." << endl;
#endif
}



void Cutter::collide(PhysicalP other)
{
  if (inScope)
    {
      PhysicalP p;
      LocatorP locator = get_locator();
      if (locator->lookup(p,killerId) == OL_NO_SIG)
	other->corporeal_attack(p,context->damage);
      else
	other->corporeal_attack(NULL,context->damage);
    }
  else
    Weapon::collide(other);
}



void Cutter::init_x(Xvars &xvars)
{
  assert(cutterXdata->valid == False);

  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    {
      cutterXdata->unheldPixmap[dpyNum] = 
	XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				    context->unheldPixmapBits,
				    context->unheldSize.width,
				    context->unheldSize.height,
				    xvars.black[dpyNum],
				    xvars.allocNamedColor(dpyNum,
							  context->unheldColorName),
				    xvars.depth[dpyNum]);
      cutterXdata->unheldMask[dpyNum] = 
	XCreateBitmapFromData(xvars.dpy[dpyNum],xvars.root[dpyNum],
			      context->unheldMaskBits,
			      context->unheldSize.width,
			      context->unheldSize.height);
  
      cutterXdata->valid = True;
    }
  Weapon::init_x(xvars);
}



void Cutter::update()
{
  inScope = inScopeNext;
  Weapon::update();
}



Creature::Creature(const CreatureContext &c_c,
		   CreatureXdata &x_data,
		   WorldP w,	       
		   LocatorP l,
		   const Pos &raw_pos)
     : Moving(c_c.movingContext,x_data.movingXdata,w,l,raw_pos)
{
  touching = CO_air;
  canClimb = False;
  stance = stanceNext = CO_air;
  grav = gravNext = 0;
  cc = &c_c;
  Timer nTimer(CORPSE_TIME);
  corpseTimer = nTimer;
  assert(context_valid());
  creatureXdata = &x_data;
  birthTime = time(NULL);
  stunNext = 0;
  wantClimb = False;
}



Creature::Creature()
{
  assert(0);
}



Boolean Creature::is_creature()
{
  return True;
}



ITcommand Creature::get_command()
{
  ITcommand mCommand = Moving::get_command();
  if (stunned()) {
    // The only commands allowed when stunned are
    // IT_WEAPON_CHANGE, IT_WEAPON_DROP, IT_ITEM_USE, IT_ITEM_CHANGE, 
    // and IT_ITEM_DROP.  Can still whip out your chainsaw to cut up huggers.
    if (mCommand == IT_WEAPON_CHANGE ||
	mCommand == IT_WEAPON_DROP ||
	mCommand == IT_ITEM_USE ||
	mCommand == IT_ITEM_CHANGE ||
	mCommand == IT_ITEM_DROP)
      return mCommand;
    else
      return IT_NO_COMMAND;
  }
  else 
    return mCommand;
}



Boolean Creature::on_door()
{
  const Area &area = get_area();
  Loc dest;
  WorldP world = get_world();
  return world->check_door(area.middle_wsquare(),dest);
}



void Creature::stun_next(int time)
{
  stunNext = time;
#if 0
  stunTimer.set(time);

  // Kind of a hack, mucking with 
  set_vel(0);
  set_vel_next(0);
  set_extra_vel_next(0);
  set_stance_next(CO_air);
#endif
}



void Creature::set_quiet_death()
{
  // Get rid of corpses.
  if (!alive())
    set_delete_me();

  Moving::set_quiet_death();
}



void Creature::corporeal_attack(PhysicalP killer,int damage) 
{
  // Does not take the possible existence of a Protection into account for
  // figuring out killer.

  IntelP kIntel;
  IntelId kIntelId; // Starts out invalid.
  if (killer && (kIntel = killer->get_intel()))
    kIntelId = kIntel->get_intel_id();
  
  IntelP intel = get_intel();
  if (intel)
    intel->set_killer_intel_id(kIntelId);
  
  Moving::corporeal_attack(killer,damage);
}



void Creature::heat_attack(PhysicalP killer,int heat,Boolean secondary)
{
  // Does not take the possible existence of a Protection into account for
  // figuring out killer.

  IntelP kIntel;
  IntelId kIntelId; // Starts out invalid.
  if (killer && (kIntel = killer->get_intel()))
    kIntelId = kIntel->get_intel_id();
  
  IntelP intel = get_intel();
  if (intel)
    intel->set_killer_intel_id(kIntelId);

  Moving::heat_attack(killer,heat,secondary);
}



void Creature::act()
{
  Vel velNext = get_vel_next();
  // Account for gravity.
  velNext.dy += gravNext; 
  
  // Limit velocities.
  switch (stanceNext) {
  case CO_air:
    velNext.limit(cc->airSpeed);
    break;
  case CO_center:
    velNext.limit(cc->centerSpeed);
    break;
  case CO_climb:
    velNext.limit(cc->climbSpeed);
    break;
  default:
    velNext.limit(cc->crawlSpeed);
    break;
  }    
  set_vel_next(velNext);
  set_dir_next(compute_dir(stanceNext,velNext));

  
  // Check for doors.
  ITcommand command = get_command();
  WorldP world = get_world();
  Loc dest;
  const Area &area = get_area();
  Loc loc = area.middle_wsquare();
  const Pos &rawPos = get_raw_pos();
  // If on a door and command is IT_UP, go through the door.
  if (command == IT_UP && world->check_door(loc,dest))
    {
      Pos p;
      p.x = rawPos.x - loc.c * WSQUARE_WIDTH + dest.c * WSQUARE_WIDTH;
      p.y = rawPos.y - loc.r * WSQUARE_HEIGHT + dest.r * WSQUARE_HEIGHT;
      set_raw_pos_next(p);
    }

  // If marked for wanting to climb and we can do so, then climb.
  if (canClimb && wantClimb) {
    set_stance_next(CO_climb);
    set_vel_next(0);
    set_grav_next(0);
    center_wsquare_x_next(area.middle_wsquare());
    set_want_climb(False);
  }
  
  if (! alive())
    {
      if (get_intel())
	cerr << "Warning: Creature::act(): Corpse with non-NULL intelligence."
	  << endl;
      corpseTimer.clock();
    }

 Moving::act();
}



void Creature::update()
{
  stance = stanceNext;
  grav = gravNext;
  const Area area = get_area_next();
  
  WorldP world = get_world();
  world->touchingHanging(touching,hanging,area);
  canClimb = world->canClimb(area.middle_wsquare());

  // Remove corpse after time limit or if damaged enough.
  if (!get_quiet_death() && !alive() && 
      (corpseTimer.ready() || (get_health() < - cc->corpseHealth)))
    {
      LocatorP locator = get_locator();
      ostrstream msg;
      if (corpseTimer.ready())
	{
	  msg << get_class_name() << " corpse has decomposed." << ends;
	  locator->message_enq(msg.str());
	}
      else
	{
	  msg << get_class_name() << " corpse has been destroyed." << ends;
	  locator->message_enq(msg.str());
	}
      set_delete_me();
    }

  if (stunNext) 
    {
      stunTimer.set(stunNext);

      // All before Moving::update.
      set_vel_next(0);
      set_extra_vel_next(0);
      set_stance_next(CO_air);

      stunNext = 0;
    }

  stunTimer.clock();
  alienImmune.clock();
  Moving::update();
}



void Creature::die()
{
  if (!get_quiet_death())
    stats.add_death(birthTime);

  // Prepare for being a corpse.
  LocatorP locator = get_locator();
  if (!get_quiet_death())
    {    
      corpseTimer.set();
      
      // Set current and next values because called in update phase.
      Vel velNew(0,DROP_SPEED);
      set_vel_next(velNew);
      set_stance_next(CO_air);
      
      ostrstream msg;
      msg << get_class_name() << " has died." << ends;
      locator->message_enq(msg.str());

      set_no_death_delete();
    }


  // Deal with awarding kills.
  IntelP intel = get_intel();
  IntelId killerIntelId;
  if (intel)
    killerIntelId = intel->get_killer_intel_id();
  IntelP killerIntel;
  if (killerIntel = locator->lookup(killerIntelId))
    {
      // Compute soups.  I.e. A human killed by something other than a 
      // different human.
      if (intel && intel->is_human() && 
	  (!killerIntel || !killerIntel->is_human() || killerIntel == intel))
	intel->add_soup();

      // Award killer with a kill of the appropriate type.  Don't get kills
      // for taking yourself out.
      if (intel && killerIntel && intel != killerIntel)
	{
	  if (intel->is_human())
	    killerIntel->add_human_kill();
	  else if (((MachineP)intel)->is_enemy())
	    killerIntel->add_enemy_kill();
	}
    }
  else if (intel && intel->is_human())
    intel->add_soup();
  
  Moving::die();
}



Touching Creature::get_touching_stance()
{
  if (stance == CO_center)
    return  CO_dn;
  else if (stance == CO_climb)
    return  CO_air;
  else
    return (Touching)stance;
}



void Creature::center_wsquare_x_next(const Loc &loc)
{
  const Area area = get_area();
  const MovingContext *cc = get_moving_context();
  const Pos rawPos = get_raw_pos();
  assert (stanceNext == CO_climb);
  assert (area.overlap(loc));

  Pos rpos(loc.c * WSQUARE_WIDTH + 
	   (WSQUARE_WIDTH - cc->sizes[CO_climb].width) / 2 -
	   cc->offsets[CO_climb].width,
	   rawPos.y);

  set_raw_pos_next(rpos);
}



void Creature::corner(const Hanging &hanging)
{
  Pos rpos;
  const Loc loc = hanging.loc;
  const MovingContext *mc = get_moving_context();


  switch (hanging.corner) {
  case CO_up_L:  // Final is CO_r_DN.
    assert(stanceNext == CO_r);
    rpos.x = loc.c * WSQUARE_WIDTH - mc->sizes[CO_r].width - 
      mc->offsets[CO_r].width;
    rpos.y = (loc.r + 1) * WSQUARE_HEIGHT - 
      (int)ceil(0.5 * mc->sizes[CO_r].height) - mc->offsets[CO_r].height;
    break;

  case CO_dn_L:  // Final is CO_r_UP.
    assert(stanceNext == CO_r);
    rpos.x = loc.c * WSQUARE_WIDTH - mc->sizes[CO_r].width - 
      mc->offsets[CO_r].width;
    rpos.y = loc.r * WSQUARE_HEIGHT - (int)floor(0.5 * mc->sizes[CO_r].height) -
      mc->offsets[CO_r].height;
    break;
    
  case CO_l_UP:  // Final is CO_dn_R.
    assert(stanceNext == CO_dn);
    rpos.x = (loc.c + 1) * WSQUARE_WIDTH - 
      (int)ceil(0.5 * mc->sizes[CO_dn].width) - mc->offsets[CO_dn].width;
    rpos.y = loc.r * WSQUARE_HEIGHT - mc->sizes[CO_dn].height - 
      mc->offsets[CO_dn].height;
    break;

  case CO_r_UP:  // Final is CO_dn_L.
    assert(stanceNext == CO_dn);
    rpos.x = loc.c * WSQUARE_WIDTH - (int)floor(0.5 * mc->sizes[CO_dn].width) -
      mc->offsets[CO_dn].width;
    rpos.y = loc.r * WSQUARE_HEIGHT - mc->sizes[CO_dn].height - 
      mc->offsets[CO_dn].height;
    break;

  case CO_up_R:  // Final is CO_l_DN.
    assert(stanceNext == CO_l);
    rpos.x = (loc.c + 1) * WSQUARE_WIDTH - mc->offsets[CO_l].width;
    rpos.y = (loc.r + 1) * WSQUARE_HEIGHT - 
      (int)ceil(0.5 * mc->sizes[CO_l].height) - mc->offsets[CO_l].height;
    break;

  case CO_dn_R:  // Final is CO_l_UP.
    assert(stanceNext == CO_l);
    rpos.x = (loc.c + 1) * WSQUARE_WIDTH - mc->offsets[CO_l].width;
    rpos.y = loc.r * WSQUARE_HEIGHT - 
      (int)floor(0.5 * mc->sizes[CO_l].height) - mc->offsets[CO_l].height;
    break;

  case CO_l_DN:  // Final is CO_up_R.
    assert(stanceNext == CO_up);
    rpos.x = (loc.c + 1) * WSQUARE_WIDTH - 
      (int)ceil(0.5 * mc->sizes[CO_up].width) - mc->offsets[CO_up].width;
    rpos.y = (loc.r + 1) * WSQUARE_HEIGHT - mc->offsets[CO_up].height;
    break;

  case CO_r_DN:  // Final is CO_up_L.
    assert(stanceNext == CO_up);
    rpos.x = loc.c * WSQUARE_WIDTH - (int)floor(0.5 * mc->sizes[CO_up].width) -
      mc->offsets[CO_up].width;
    rpos.y = (loc.r + 1) * WSQUARE_HEIGHT - mc->offsets[CO_up].height;
    break;

  default:
    assert(0);
  }
  
  set_raw_pos_next(rpos);
}



void Creature::get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			       Dir dir,int animNum)
{
  if (! alive())
    {
      pixmap = creatureXdata->deadPixmap[dpyNum];
      mask = creatureXdata->deadMask[dpyNum];
    }
  else
    Moving::get_pixmap_mask(dpyNum,pixmap,mask,dir,animNum);
}



void Creature::get_size_offset_next(Size &size,Size &offset,Dir dir)
{
  if (! alive_next())
    {
      size = cc->deadSize;
      offset = cc->deadOffset;
    }
  else
    Moving::get_size_offset_next(size,offset,dir);
}



void Creature::set_stance_next(const Stance &stance)
{
  assert((stance == CO_r) || (stance == CO_dn) || (stance == CO_l) ||
	 (stance == CO_up) || (stance == CO_air) || 
	 (stance == CO_center) || (stance == CO_climb)); 
  stanceNext = stance; 
}



void Creature::init_x(Xvars &xvars)
{
  assert(creatureXdata->valid == False);

  const MovingContext *mc = get_moving_context();

  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    {
      creatureXdata->deadPixmap[dpyNum] = 	 
	XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				    cc->deadPixmapBits,
				    cc->deadSize.width,cc->deadSize.height,
				    xvars.allocNamedColor(dpyNum,
							  mc->foreColorName,
	       mc->foreWhiteDefault ? xvars.white[dpyNum] : xvars.black[dpyNum]),
				    xvars.allocNamedColor(dpyNum,
							  mc->backColorName,
	   mc->backWhiteDefault ? xvars.white[dpyNum] : xvars.black[dpyNum]),
				    xvars.depth[dpyNum]);
      creatureXdata->deadMask[dpyNum] = 
	XCreateBitmapFromData(xvars.dpy[dpyNum],xvars.root[dpyNum],
			      cc->deadMaskBits,
			      cc->deadSize.width,cc->deadSize.height);
    }

  creatureXdata->valid = True;
  Moving::init_x(xvars);
}



Dir Creature::compute_dir(const Stance &st,const Vel &v)
{
  Dir ret = CO_center;
  
  switch (st) {
  case CO_r:
    if (fabs(v.dy) < VEL_SMALL)
      ret = CO_r;
    else
      ret = v.dy > 0 ? CO_r_DN : CO_r_UP;
    break;
    
  case CO_dn:
    if (fabs(v.dx) < VEL_SMALL)
      ret = CO_dn;
    else
      ret = v.dx > 0 ? CO_dn_R : CO_dn_L;
    break;

  case CO_l:
    if (fabs(v.dy) < VEL_SMALL)
      ret = CO_l;
    else
      ret = v.dy > 0 ? CO_l_DN : CO_l_UP;
    break;
    
  case CO_up:
    if (fabs(v.dx) < VEL_SMALL)
      ret = CO_up;
    else
      ret = v.dx > 0 ? CO_up_R : CO_up_L;
    break;

  case CO_center:
    if (fabs(v.dx) < VEL_SMALL)
      ret = CO_center;
    else
      ret = v.dx > 0 ? CO_center_R : CO_center_L;
    break;

  // If diagonal, will give horizontal direction.
  case CO_air:
    if (fabs(v.dx) < VEL_SMALL)
      {
	if (fabs(v.dy) < VEL_SMALL)
	  ret = CO_air;
	else
	  // Changed
	  ret = v.dy > 0 ? CO_air_DN : CO_air_UP;
      }
    else
      ret = v.dx > 0 ? CO_air_R : CO_air_L;
    break;

  // If diagonal, will give horizontal direction.
  case CO_climb:
    if (fabs(v.dx) < VEL_SMALL)
      {
	if (fabs(v.dy) < VEL_SMALL)
	  ret = CO_climb;
	else
	  // Changed
	  ret = v.dy > 0 ? CO_climb_DN : CO_climb_UP;
      }
    else
      ret = v.dx > 0 ? CO_climb_R : CO_climb_L;
    break;
    
  default:
    assert (0);
    break;
  }

  return ret;
}



Boolean Creature::context_valid()
{
  if ((cc->crawlSpeed < 0) || (cc->centerSpeed < 0) || (cc->airSpeed < 0) || 
      (cc->climbSpeed < 0) || (cc->jump < 0) || (cc->acceleration < 0))
    return False;
  
  return True;
}



Stats Creature::stats;



Grounded::Grounded(const GroundedContext &,GroundedXdata &)
{}



void Grounded::act()
{
  _act();
  Creature::act();
}



void Grounded::_act()
{
  // Methods of Moving or Creature.
  const CreatureContext *cc = get_creature_context();
  const Stance stance = get_stance();
  const Touching touchingArea = get_touching_area();
  const Touching touchingStance = get_touching_stance();
  const Vel vel = get_vel();
  const Acc *unitAccs = get_unit_accs();
  const ITcommand command = get_command(); 
  
  
  if (touchingArea != touchingStance)
    {
      // Hit the ground.
      if (touchingArea == CO_dn) 
	{
	  set_stance_next(CO_center);
	  Vel velNew(vel.dx,0);
	  set_vel_next(velNew);
	}
      // Hit side walls.
      else if ((touchingArea == CO_r) || (touchingArea == CO_l))
	{
	  Vel velNew(0,vel.dy);
	  set_vel_next(velNew);
	}
      // In the air.
      else if (touchingArea == CO_air)
	set_stance_next(CO_air);
    }
  else if (touchingArea == CO_dn) 
    {
      Vel velNew(vel.dx,0);
      set_vel_next(velNew);
    }
  
  // Set gravity.
  set_grav_next((touchingArea == CO_dn) ? 0 : GRAVITY);
  
  /* Interpret commands from controlling intelligence.  May override some of 
     above settings. */
  switch (command) {
  case IT_CENTER:
    if (stance == CO_center)
      set_vel_next(0);
    break;
    
  case IT_R:
    if (stance == CO_center)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_R]);
    break;
    
  case IT_L:
    if (stance == CO_center)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_L]);
    
    break;
  };    
}



Suicide::Suicide(const SuicideContext &,SuicideXdata &)
{}



Boolean Suicide::is_suicide()
{
  return True;
}



void Suicide::act()
{
  _act();
  Creature::act();
}



void Suicide::_act()
{
  if (get_command() == IT_ITEM_USE)
    kill_self();
}



Hopping::Hopping(const HoppingContext &,HoppingXdata &)
{}
     
     

void Hopping::act()
{
  // Methods of Moving or Creature.
  const CreatureContext *cc = get_creature_context();
  const Stance stance = get_stance();
  const Touching touchingArea = get_touching_area();
  const Touching touchingStance = get_touching_stance();
  const Vel vel = get_vel();
  const Vel *unitVels = get_unit_vels();
  const Boolean canClimb = can_climb();
  const Area area = get_area();
  const ITcommand command = get_command(); 
  
  
  // Default set gravity.
  set_grav_next((touchingArea == CO_dn) || 
		(canClimb && (stance == CO_climb) && alive()) ? 0 : GRAVITY);

  
  // Change stance if necessary.
  if (stance != CO_climb) 
    {
      if (touchingArea != touchingStance)
	{
	  set_want_climb(False);

	  // Hit the ground.
	  if (touchingArea == CO_dn) 
	    {
	      set_stance_next(CO_center);
	      set_vel_next(0);
	    }
	  // Side walls.
	  else if (touchingArea == CO_r)
	    {
	      Vel velNew(0,vel.dy);
	      set_vel_next(velNew);
	    }
	  else if (touchingArea == CO_l)
	    {
	      Vel velNew(0,vel.dy);
	      set_vel_next(velNew);
	    }
	  else if (touchingArea == CO_air)
	    set_stance_next(CO_air);
	  else if (touchingArea != CO_up)
	    assert(0);
	}
      else if (touchingArea == CO_dn) 
	{
	  Vel velNew(vel.dx,0);
	  set_vel_next(velNew);
	}
    }
  else // stance == CO_climb
    {
      // Stop climbing if not in climbing square. 
      if (!canClimb)  
	set_stance_next(CO_air);
      else if (! vel.is_zero())
	set_vel_next(0);
    }
  

  /* Interpret commands from controlling intelligence.  May override some of 
     above settings. */
  switch (command) {
  case IT_CENTER:
    if (stance == CO_air)
      set_want_climb(True);
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_grav_next(0);
	set_vel_next(0);
	center_wsquare_x_next(area.middle_wsquare());
      }
    break;
    
  case IT_R:
    if ((stance == CO_center) || (stance == CO_climb))
      {
	set_vel_next(cc->jump * unitVels[CO_UP_R_R]);
	set_stance_next(CO_air);
      }
    break;
    
  case IT_DN_R:
    if (stance == CO_climb)
      {
	set_vel_next(cc->jump * unitVels[CO_DN_R]);
	set_stance_next(CO_air);
      }
    break;
    
  case IT_DN:
    if (stance == CO_air)
      set_want_climb(True);
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_grav_next(0);
	set_vel_next(0);
	center_wsquare_x_next(area.middle_wsquare());
      }
    else if (stance == CO_climb)
      set_vel_next(cc->jump * unitVels[CO_climb_DN]);
    break;
    
  case IT_DN_L:
    if (stance == CO_climb) 
      {
	set_vel_next(cc->jump * unitVels[CO_DN_L]);
	set_stance_next(CO_air);
      }
    break;
    
  case IT_L:
    if ((stance == CO_center) || (stance == CO_climb)) 
      {
	set_vel_next(cc->jump * unitVels[CO_UP_L_L]);
	set_stance_next(CO_air);
      }
    break;
    
  case IT_UP_L:
    if ((stance == CO_center) || (stance == CO_climb)) 
      {
	set_vel_next(cc->jump * unitVels[CO_air_L]);
	set_stance_next(CO_air);
      }
    break;
    
  case IT_UP:
    // Doors are handled by Creature.
    if (on_door())
      break;
    if (stance == CO_air)
      set_want_climb(True);
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_vel_next(0);
	set_grav_next(0);
	center_wsquare_x_next(area.middle_wsquare());
      }
    else if (stance == CO_center) // Jump up.
      {
	set_vel_next(cc->jump * unitVels[CO_air_UP]);
	set_stance_next(CO_air);
      }
    else if (stance == CO_climb)
      set_vel_next(cc->jump * unitVels[CO_climb_UP]);
    break;
    
  case IT_UP_R:
    // Jump right.
    if ((stance == CO_center) || (stance == CO_climb)) 
      {
	set_vel_next(cc->jump * unitVels[CO_air_R]);
	set_stance_next(CO_air);
      }
    break;
  };

 Creature::act();
}



User::User(const UserContext &cx,UserXdata &)
{
  weaponsNum = weaponCurrent = itemsNum = itemCurrent = 0; 
  context = &cx;

  Timer dummy(USER_CYCLE_TIME);
  weaponCycleTimer = dummy;
  itemCycleTimer = dummy;
  itemCount = 0;
}



Boolean User::is_user()
{
  return True;
}



int User::get_weapons_num()
{
  return weaponsNum;
}



int User::get_items_num()
{
  return itemsNum;
}



PhysicalP User::get_weapon(int n)
{
  assert(n >= 0);
  if (n < weaponsNum)
    {
      LocatorP locator = get_locator();
      PhysicalP ret;
      if (ret = locator->lookup(weapons[n]))
	return ret;
      else
	return NULL;
    }
  else
    return NULL;
}



PhysicalP User::get_item(int n)
{
  assert(n >= 0);
  if (n < itemsNum)
    {
      LocatorP locator = get_locator();
      PhysicalP ret;
      if (ret = locator->lookup(items[n]))
	return ret;
      else
	return NULL;
    }
  else
    return NULL;
}



PhysicalP User::get_weapon_current()
{
  assert (weaponCurrent <= weaponsNum);
  if (weaponCurrent != weaponsNum)
    {
      LocatorP locator = get_locator();
      PhysicalP ret;
      if (ret = locator->lookup(weapons[weaponCurrent]))
	return ret;
      else
	return NULL;
    }
  else
    return NULL;
}



ClassId User::get_weapon_string(const char *&str)
{
  PhysicalP p = get_weapon_current();
  if (p)
    {
      str = p->get_class_name();
      return p->get_class_id();
    }
  else
    return Creature::get_weapon_string(str);
}



PhysicalP User::get_item_current()
{
  assert (itemCurrent <= itemsNum);
  if (itemCurrent != itemsNum)
    {
      PhysicalP ret;

      LocatorP locator = get_locator();
      if (ret = locator->lookup(items[itemCurrent]))
	return ret;
      else
	return NULL;
    }
  else
    return NULL;
}



int User::get_item_count()
{
  return itemCount;
}



void User::set_mapped_next(Boolean val)
{
  if (get_mapped_next() != val)
    {
      LocatorP locator = get_locator();
      PhysicalP weapon;
      if ((weaponCurrent != weaponsNum) && 
	  (weapon = locator->lookup(weapons[weaponCurrent])))
	{
	  if (val)
	    ((WeaponP)weapon)->enter_scope_next(this);
	  else
	    ((WeaponP)weapon)->leave_scope_next(this);
	}
    }

  Creature::set_mapped_next(val);
}



void User::act() 
{
  Creature::act(); 
  _act();
}



void User::_act()
{
  assert(weaponCurrent <= weaponsNum);
  assert(itemCurrent <= itemsNum);
  // Make sure that don't have any weapons or items if can't use them.
  assert((context->usesWeapons || weaponsNum == 0) &&
	 (context->usesItems || itemsNum == 0));

  LocatorP locator = get_locator();
  ITcommand command = get_command();


  if (Intel::is_command_weapon(command))
    // Fire weapon.  
    weapon_use(command);
  else
    switch (command) {
    case IT_WEAPON_CHANGE:
      weapon_change();
      break;
    case IT_WEAPON_DROP:
      weapon_drop();
      break;
    case IT_ITEM_USE:
      item_use();
      break;
    case IT_ITEM_CHANGE:
      item_change();
      break;
    case IT_ITEM_DROP:
      item_drop();
      break;
    }
  
  assert(weaponCurrent <= weaponsNum);
  assert(itemCurrent <= itemsNum);
  

  /* Go through list of weapons and items, moving them along with the object
     or deleting them if they no longer exist or have no ammo. */

  const Area &areaNext = get_area_next();
  const Pos middleNext = areaNext.get_middle();
  Dir dirNext = get_dir_next();

  // Traverse list of weapons.
  int n;
  for (n = 0; n < weaponsNum;)
    {
      assert(weaponCurrent <= weaponsNum);

      PhysicalP p;
      if (p = locator->lookup(weapons[n]))
	{
	  if ((((WeaponP)p)->get_ammo() > 0) || 
	      (((WeaponP)p)->get_ammo() == PH_AMMO_UNLIMITED))
	    ((WeaponP)p)->follow_user(middleNext,dirNext);
	  else
	    p->kill_self();
	  n++;
	}
      else
	{
	  weapons[n] = weapons[weaponsNum - 1];
	  weaponsNum--;

	  if (weaponCurrent == weaponsNum && n != weaponsNum)
	    weaponCurrent = n; // Keep same current weapon.
	  else if (weaponCurrent == weaponsNum + 1)
	    weaponCurrent = weaponsNum;  // So still no weapon.
	  else if (weaponCurrent == n)
	    weaponCurrent = coolest_weapon(); // Still some weapon active.

	  PhysicalP weapon;
	  if ((weaponCurrent != weaponsNum) && 
	      (weapon = locator->lookup(weapons[weaponCurrent])))
	    ((WeaponP)weapon)->enter_scope_next(this);
	}
    }


  // Traverse list of items.

  // Count number of current item that User has.  Set itemCountNext.
  // NOTE: This might not be completely accurate, as itemCurrent can change
  // during this traversal.  But, who cares.  It'll be updated next turn.
  itemCountNext = 0;
  ClassId itemCurrentClassId = A_None;
  PhysicalP item;
  if (itemCurrent != itemsNum && 
      (item = locator->lookup(items[itemCurrent])))
    itemCurrentClassId = item->get_class_id();

  for (n = 0; n < itemsNum;)
    {
      assert(itemCurrent <= itemsNum);

      PhysicalP p;
      if (p = locator->lookup(items[n]))
	{
	  // Increment itemCountNext.
	  if (p->get_class_id() == itemCurrentClassId)
	    itemCountNext++;
	  // Make item follow user.
	  ((ItemP)p)->follow_user(middleNext,dirNext);
	  n++;
	}
      else
	{
	  items[n] = items[itemsNum - 1];
	  itemsNum--;

	  if (itemCurrent == itemsNum && n != itemsNum)
	    itemCurrent = n; // Keep same current item.
	  else if (itemCurrent == itemsNum + 1)
	    itemCurrent = itemsNum;  // So still no item.
	  else if (itemCurrent == n)
	    itemCurrent = coolest_item();  // Some item or itemsNum.
	}
    }

  assert(weaponCurrent <= weaponsNum);
  assert(itemCurrent <= itemsNum);
  
  weaponCycleTimer.clock();
  itemCycleTimer.clock();
}



void User::update()
{
  _update();
  Creature::update();
}



void User::_update()
{
  itemCount = itemCountNext;
}



void User::collide(PhysicalP other) 
{
  if (!_collide(other)) 
    Creature::collide(other);
}



Boolean User::_collide(PhysicalP other)
{ 
  if (alive() && other->is_item() && ((ItemP)other)->can_take())
    {
      // Pick up weapon.
      if (other->is_weapon()) {
	if (context->usesWeapons &&
	    weaponsNum < PH_WEAPONS_MAX)
	  {
	    ((ItemP)other)->taken(this);
	    
	    WeaponP weapon;
	    if (has_weapon(&weapon,other->get_class_id()))
	      { 
		// Take ammo from other and kill other.
		weapon->take_ammo_from((WeaponP)other);
		other->set_quiet_death();
		other->kill_self();
	      }
	    else  
	      {
		weapons[weaponsNum] = other->get_id();

		// Increment weaponCurrent along with weaponsNum, so still
		// no weapon selected.
		if (weaponCurrent == weaponsNum)
		  weaponCurrent++;
		weaponsNum++;

		if (weaponsNum == 1)  // Used to be 0.
		  {
		    // Will set it to new weapon or builtIn weapon.
		    weaponCurrent = coolest_weapon();
		    // If new weapon is selected.
		    if (weaponCurrent == 0)
		      ((WeaponP)other)->enter_scope_next(this);
		  }
		
#if 0
		/* Automatically switch to weapon if no previous weapon. */ 
		if (weaponCurrent == weaponsNum - 1)
		  {
		    // if (((WeaponP)other)->defaultable())
		    ((WeaponP)other)->enter_scope_next(this);
		    // else
		    // weaponCurrent = weaponsNum;
		  }
#endif
	      } // has weapon
	  }
	else // not usesWeapon or already full of weapons 
	  {
	    other->set_quiet_death();
	    other->kill_self();
	  }
      } // is weapon

      // Pick up item.
      else if (context->usesItems &&
	       !other->is_weapon() &&  // don't want weapons in item slot
	       itemsNum < PH_ITEMS_MAX)
	{
	  ((ItemP)other)->taken(this);

	  items[itemsNum] = other->get_id();
	  itemsNum++;

	  // itemCurrent is 0 if no previous item.
	}

      return True;
    }
  else
    return False;
}



void User::die()
{
  _die();
  Creature::die();
}



void User::_die()
{
  LocatorP locator = get_locator();

  // Drop all weapons.
  int n;     
  for (n = 0; n < weaponsNum; n++)
    {
      PhysicalP weapon;
      if (locator->lookup(weapon,weapons[n]) == OL_NO_SIG)
	{
	  if (n == weaponCurrent)
	    ((WeaponP)weapon)->leave_scope_next(this);

	  if (((ItemP)weapon)->persists())
	    ((ItemP)weapon)->dropped(this);
	  else if (!weapon->die_called())
	    {
	      weapon->set_quiet_death();
	      weapon->kill_self();
	      weapon->die();
	    }
	}
    }
  weaponCurrent = 0;
  weaponsNum = 0;


  // Drop all items.
  itemCurrent = 0;
  for (n = 0; n < itemsNum; n++)
    {
      PhysicalP item;
      if (locator->lookup(item,items[n]) == OL_NO_SIG)
	{
	  if (((ItemP)item)->persists())
	    ((ItemP)item)->dropped(this);
	  else if (!item->die_called())
	    {
	      item->set_quiet_death();
	      item->kill_self();
	      item->die();
	    }
	}
    }
  itemsNum = 0;
}



Boolean User::has_weapon(Weapon **weaponOut,ClassId classId)
{
  for (int n = 0; n < weaponsNum; n++)
    {
      PhysicalP weapon;
      LocatorP locator = get_locator();

      // Clean up deleted weapons elsewhere.
      if (weapon = locator->lookup(weapons[n]))
	if (weapon->get_class_id() == classId)
	  {
	    if (weaponOut)
	      *weaponOut = (WeaponP)weapon;
	    return True;
	  }
    }
  return False;
}



#if 0
int User::defaultable_weapon()
{
  for (int n = 0; n < weaponsNum; n++)
    {
      LocatorP locator = get_locator();
      WeaponP weapon = (WeaponP)locator->lookup(weapons[n]);
      if (weapon && weapon->defaultable())
	return n;
    }
  return weaponsNum;
}
#endif



int User::coolest_weapon()
{
  LocatorP locator = get_locator();
  int index = weaponsNum;
  int coolnessMax = get_built_in_weapon_coolness();

  // Your builtIn weapon must be defaultable.
  assert(coolnessMax >= 0);

  int n;
  for (n = 0; n < weaponsNum; n++)
    {
      WeaponP weapon = (WeaponP)locator->lookup(weapons[n]);
      if (weapon && 
	  weapon->get_coolness() > coolnessMax &&
	  // Only weapons of nonnegative coolness.
	  weapon->get_coolness() >= 0)
	{
	  coolnessMax = weapon->get_coolness();
	  index = n;
	}
    }
  
  return index;
}



/* Mostly copied from User::coolest_weapon. */
int User::coolest_item()
{
  LocatorP locator = get_locator();
  int index = itemsNum;
  int coolnessMax = 0;

  // Your builtIn item must be defaultable.
  assert(coolnessMax >= 0);

  int n;
  for (n = 0; n < itemsNum; n++)
    {
      ItemP item = (ItemP)locator->lookup(items[n]);
      if (item && 
	  item->get_coolness() > coolnessMax &&
	  // Only items of nonnegative coolness.
	  item->get_coolness() >= 0)
	{
	  coolnessMax = item->get_coolness();
	  index = n;
	}
    }
  
  return index;
}



int User::next_coolest_weapon()
{
  LocatorP locator = get_locator();

  // Find coolness of current weapon or of builtIn weapon.
  int currentCoolness;
  if (weaponCurrent != weaponsNum)
    {
      WeaponP weapon = (WeaponP)locator->lookup(weapons[weaponCurrent]);
      if (weapon)
	currentCoolness = weapon->get_coolness();
      else
	// Somehow, the current weapon got destroyed, just return coolest
	// of all weapons.
	return coolest_weapon();
    }
  else
    currentCoolness = get_built_in_weapon_coolness();


  // Look for weapon of maximum coolness less than currentCoolness.
  Boolean found = False;
  int maxx;
  int index;
  int n;
  for (n = 0; n < weaponsNum; n++)
    {
      WeaponP weapon = (WeaponP)locator->lookup(weapons[n]);
      if (weapon && 
	  (!found || weapon->get_coolness() > maxx) &&
	  weapon->get_coolness() < currentCoolness)
	{
	  maxx = weapon->get_coolness();
	  index = n;
	  found = True;
	}
    }

  // See if builtIn weapon is the best fit.
  if (get_built_in_weapon_coolness() < currentCoolness &&
      (!found || get_built_in_weapon_coolness() > maxx))
    {
      maxx = get_built_in_weapon_coolness();
      index = weaponsNum;
      found = True;
    }
  
  if (!found)
    // Cycle around.
    return coolest_weapon();
  else
    return index;
}



/*  Mostly copied from User::next_coolest_weapon. */
int User::next_coolest_item()
{
  LocatorP locator = get_locator();

  // Find coolness of current item.
  int currentCoolness;
  if (itemCurrent != itemsNum)
    {
      ItemP item = (ItemP)locator->lookup(items[itemCurrent]);
      if (item)
	currentCoolness = item->get_coolness();
      else
	// Somehow, the current item got destroyed, just return coolest
	// of all items.
	return coolest_item();
    }
  else
    currentCoolness = 0;


  // Look for item of maximum coolness less than currentCoolness.
  Boolean found = False;
  int maxx;
  int index;
  int n;
  for (n = 0; n < itemsNum; n++)
    {
      ItemP item = (ItemP)locator->lookup(items[n]);
      if (item && 
	  (!found || item->get_coolness() > maxx) &&
	  item->get_coolness() < currentCoolness)
	{
	  maxx = item->get_coolness();
	  index = n;
	  found = True;
	}
    }

  if (!found)
    // Cycle around.
    return coolest_item();
  else
    return index;
}



void User::weapon_use(ITcommand command)
{
  if (weaponCurrent != weaponsNum)
    {
      LocatorP locator = get_locator();
      PhysicalP p;
      if (p = locator->lookup(weapons[weaponCurrent]))
	{
	  ((WeaponP)p)->fire(get_id(),command);
	  
	  // Inform Creature that an attack occurred.
	  attack_hook();
	}
    }
}



void User::weapon_change()
{
  LocatorP locator = get_locator();

  PhysicalP weapon = NULL;
  if ((weaponCurrent != weaponsNum) && 
      (weapon = locator->lookup(weapons[weaponCurrent])))
    ((WeaponP)weapon)->leave_scope_next(this);

  int coolest = coolest_weapon();
  // If its been awhile since the last weapon change AND
  // we're not already on the coolest weapon,
  if (weaponCycleTimer.ready() && weaponCurrent != coolest)
    weaponCurrent = coolest;
  else
    // If we changed weapons recently, cycle to the next coolest weapon.
    weaponCurrent = next_coolest_weapon();
  
  // weapon was just changed
  weaponCycleTimer.set();
  
  if ((weaponCurrent != weaponsNum) && 
      (weapon = locator->lookup(weapons[weaponCurrent])))
    {
      ((WeaponP)weapon)->enter_scope_next(this);
      weapon->flash();
    }
}



void User::weapon_drop()
{
  LocatorP locator = get_locator();

  if (weaponCurrent != weaponsNum) // Some weapon is current.
    {
      // Clean up dead weapons elsewhere.
      
      PhysicalP weapon;
      if (weapon = locator->lookup(weapons[weaponCurrent]))
	{
	  ((WeaponP)weapon)->leave_scope_next(this);
	  ((ItemP)weapon)->dropped(this);
	  weaponsNum--;
	  weapons[weaponCurrent] = weapons[weaponsNum];
	  
	  // Some weapon or weaponCurrent == weaponsNum.
	  weaponCurrent = coolest_weapon();
	  
	  PhysicalP weapon;
	  if ((weaponCurrent != weaponsNum) &&
	      (weapon = locator->lookup(weapons[weaponCurrent])))
	    ((WeaponP)weapon)->enter_scope_next(this);
	}
    }
  else  // No current weapon.
    /* Cycle to next available weapon, so can keep dropping. */
    weapon_change();
}



void User::item_use()
{
  if (itemCurrent != itemsNum)
    {
      LocatorP locator = get_locator();
      PhysicalP p;
      if (p = locator->lookup(items[itemCurrent]))
	((ItemP)p)->use(this);
    }
}



void User::item_change()
{
  int coolest = coolest_item();
  // If its been awhile since the last item change AND
  // we're not already on the coolest item,
  if (itemCycleTimer.ready() && itemCurrent != coolest)
    itemCurrent = coolest;
  else
    // If we changed items recently, cycle to the next coolest item.
    itemCurrent = next_coolest_item();
  
  // item was just changed
  itemCycleTimer.set();

  PhysicalP p;
  LocatorP locator = get_locator();
  if ((itemCurrent != itemsNum) &&
      (p = locator->lookup(items[itemCurrent])))
    p->flash();
}



void User::item_drop()
{
  LocatorP locator = get_locator();

  if (itemCurrent != itemsNum) // Some item is current.
    {
      // Clean up dead items elsewhere.

      PhysicalP item;
      if (item = locator->lookup(items[itemCurrent]))
	{
	  ((ItemP)item)->dropped(this);
	  itemsNum--;
	  items[itemCurrent] = items[itemsNum];
	  
	  // Some item current.
	  itemCurrent = coolest_item();
	}
    }
  else  // No current item.
    /* Cycle to next available item, so can keep dropping. */
    item_change();
}



Fighter::Fighter(const FighterContext &f_c,
		 FighterXdata &x_data)
{
  Timer nTimer(FIGHTER_SLIDE_TIME);
  stuckTimer = nTimer;

  fc = &f_c;
  fighterXdata = &x_data;
  attack = attackNext = attackNone;
}



Boolean Fighter::is_fighter()
{
  return True;
}



void Fighter::get_pixmap_mask(int dpyNum,Pixmap &pixmap,Pixmap &mask,
			      Dir dir,int animNum)
{
  // Alive check is somewhat of a hack.
  if (alive() && attack && dir_to_attack(dir))
    {
      pixmap = fighterXdata->pixmaps[dpyNum][dir];
      mask = fighterXdata->masks[dpyNum][dir];
    }
  else
    Creature::get_pixmap_mask(dpyNum,pixmap,mask,dir,animNum);
}


  
void Fighter::get_size_offset_next(Size &size,Size &offset,
					   Dir dirNext)
{
  // Alive check is somewhat of a hack.
  if (alive_next() && attackNext && dir_to_attack(dirNext))
    {
      size = fc->sizes[dirNext];
      offset = fc->offsets[dirNext];
    }
  else
    Creature::get_size_offset_next(size,offset,dirNext);
}


			     
void Fighter::act() 
{
  _act(); 
  Creature::act();
}



void Fighter::_act()
{
  ITcommand command = get_command();
  const Dir dir = get_dir();

  if (attack && (attack != dir_to_attack(dir)))
    {
      attackNext = attackNone;
      set_vel_next(0);
    }

  if (attack == attackStuck) 
    {
      if (stuckTimer.ready())
	{
	  attackNext = attackNone;
	  set_vel_next(0);
	}
      else
	stuckTimer.clock();
    }


  // Start new attack.
  if (!attack && !get_weapon_current() && Intel::is_command_weapon(command))
    {
      Boolean didAttack = True;
      
      // Choose attack depending on stance.
      switch (get_stance()) {
      case CO_center:
	switch (command) {
	case IT_WEAPON_R:
	  attack_stuck(CO_center_R,CO_center);
	  break;
	case IT_WEAPON_DN_R:
	  attack_stuck(CO_dn_R,CO_dn);
	  break;
	case IT_WEAPON_DN_L:
	  attack_stuck(CO_dn_L,CO_dn);
	  break;
	case IT_WEAPON_L:
	  attack_stuck(CO_center_L,CO_center);
	  break;
	case IT_WEAPON_UP_L:
	  attack_free_horizontal(CO_UP_L);  
	  break;
	case IT_WEAPON_UP:
	  attack_free_vertical(CO_air_UP);
	  break;
	case IT_WEAPON_UP_R:
	  attack_free_horizontal(CO_UP_R); 
	  break;
	default:
	  didAttack = False;
	};
	break;

      case CO_air: 
	if (command == IT_WEAPON_DN)
	  attack_free_vertical(CO_air_DN);
	else
	  didAttack = False;
	break;
	
      case CO_r:
	switch (command) {
	case IT_WEAPON_L:
	case IT_WEAPON_DN_L:
	  attack_free_horizontal(CO_center_L);
	  break;
	case IT_WEAPON_UP_L:
	  attack_free_horizontal(CO_UP_L);
	  break;
	case IT_WEAPON_UP:
	case IT_WEAPON_UP_R:
	  attack_stuck(CO_r_UP,CO_r);
	  break;
	case IT_WEAPON_DN:
	case IT_WEAPON_DN_R:
	  attack_stuck(CO_r_DN,CO_r);
	  break;
	default:
	  didAttack = False;
	};
	break;

      case CO_dn:
	switch (command) {
	case IT_WEAPON_UP_R:
	  attack_free_horizontal(CO_UP_R);
	  break;
	case IT_WEAPON_UP_L:
	  attack_free_horizontal(CO_UP_L);
	  break;
	case IT_WEAPON_UP:
	  attack_free_vertical(CO_air_UP);
	  break;
	case IT_WEAPON_R:
	case IT_WEAPON_DN_R:
	  attack_stuck(CO_dn_R,CO_dn);
	  break;
	case IT_WEAPON_L:
	case IT_WEAPON_DN_L:
	  attack_stuck(CO_dn_L,CO_dn);
	  break;
	default:
	  didAttack = False;
	};
	break;

      case CO_l:
	switch (command) {
	case IT_WEAPON_R:
	case IT_WEAPON_DN_R:
	  attack_free_horizontal(CO_center_R);
	  break;
	case IT_WEAPON_UP_R:
	  attack_free_horizontal(CO_UP_R);
	  break;
	case IT_WEAPON_UP:
	case IT_WEAPON_UP_L:
	  attack_stuck(CO_l_UP,CO_l);
	  break;
	case IT_WEAPON_DN:
	case IT_WEAPON_DN_L:
	  attack_stuck(CO_l_DN,CO_l);
	  break;
	default:
	  didAttack = False;
	};
	break;

      case CO_up:
	switch (command) {
	case IT_WEAPON_DN:
	  attack_free_vertical(CO_air_DN);
	  break;
	case IT_WEAPON_R:
	case IT_WEAPON_DN_R:
	case IT_WEAPON_UP_R:
	  attack_stuck(CO_up_R,CO_up);
	  break;
	case IT_WEAPON_L:
	case IT_WEAPON_DN_L:
	case IT_WEAPON_UP_L:
	  attack_stuck(CO_up_L,CO_up);
	  break;
	default:
	  didAttack = False;
	};
	break;

      case CO_climb:
	switch (command) {
	case IT_WEAPON_R:
	case IT_WEAPON_DN_R:
	  attack_free_horizontal(CO_center_R);
	  break;
	case IT_WEAPON_DN:
	  attack_free_vertical(CO_air_DN);
	  break;
	case IT_WEAPON_L:
	case IT_WEAPON_DN_L:
	  attack_free_horizontal(CO_center_L);
	  break;
	case IT_WEAPON_UP_L:
	  attack_free_horizontal(CO_UP_L);
	  break;
	case IT_WEAPON_UP:
	  attack_free_vertical(CO_air_UP);
	  break;
	case IT_WEAPON_UP_R:
	  attack_free_horizontal(CO_UP_R);
	  break;
	default:
	  didAttack = False;
	};
	break;

      default:
	didAttack = False;
	break;
      };


      // Inform Creature that an attack occurred.
      if (didAttack)
	attack_hook();
    }  
}



void Fighter::update() 
{
  _update(); 
  Creature::update();
}



void Fighter::_update()
{
  attack = attackNext;
}



void Fighter::collide(PhysicalP other)
{
  if (!_collide(other)) 
    Creature::collide(other);
}



Boolean Fighter::_collide(PhysicalP other)
{
  if (attack)
    {
      Dir dir = get_dir();
      const Area &area = get_area();
      
      Pos pos;
      Size size;
      area.get_rect(pos,size);
      
      const Area &otherArea = other->get_area();
      Attack attackDir = dir_to_attack(dir);


      if (attackDir && otherArea.overlap(pos + fc->hotSpots[dir]))
	{
	  // Move just enough to get out of the way.
	  Size offset = area.avoid(otherArea);
	  const Pos &rawPos = get_raw_pos();
	  set_raw_pos_next(rawPos + offset);

	  other->corporeal_attack(this,
				  (attackDir == attackStuck) ? 
				  fc->damageStuck : fc->damageFree);
	  attackNext = attackNone;
	  return True;
	}
    }
  
  return False;
}



void Fighter::init_x(Xvars &xvars)
{
  assert(fighterXdata->valid == False);

  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    for (int n = 0; n < CO_DIR_MAX; n++)
      if (dir_to_attack(n) && fc->pixmapBits[n] && fc->maskBits[n])
	{
	  fighterXdata->pixmaps[dpyNum][n] = 
	    XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
					fc->pixmapBits[n],
					fc->sizes[n].width,fc->sizes[n].height,
					xvars.black[dpyNum],
					xvars.allocNamedColor(dpyNum,fc->colorName),
					xvars.depth[dpyNum]);
	  fighterXdata->masks[dpyNum][n] = 
	    XCreateBitmapFromData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				  fc->maskBits[n],
				  fc->sizes[n].width,fc->sizes[n].height);
	}
  
  fighterXdata->valid = True;
  Creature::init_x(xvars);
}



Attack Fighter::dir_to_attack(Dir dir)
{
  switch (dir) {
  case CO_center_R:
  case CO_center_L:
  case CO_r_DN:
  case CO_r_UP:
  case CO_dn_R:
  case CO_dn_L:
  case CO_l_DN:
  case CO_l_UP:
  case CO_up_R:
  case CO_up_L:
    return attackStuck;

  case CO_air_R:
  case CO_air_DN: 
  case CO_air_L:
  case CO_air_UP:
    return attackFree;
  };

  return attackNone;
}



void Fighter::attack_stuck(Dir dir,Stance stance)
{
  const Vel *unitVels = get_unit_vels();

  set_vel_next(fc->slide * unitVels[dir]);
  set_stance_next(stance);
  stuckTimer.set();
  attackNext = attackStuck;
}



void Fighter::attack_free_horizontal(Dir dir)
{
  const Acc *unitAccs = get_unit_accs();

  set_vel_next(get_vel() + fc->jumpHorizontal * unitAccs[dir]);
  set_stance_next(CO_air);
  attackNext = attackFree;
}



void Fighter::attack_free_vertical(Dir dir)
{
  const Acc *unitAccs = get_unit_accs();
  Vel vel = get_vel();

  Vel velNext(0,vel.dy + fc->jumpVertical * unitAccs[dir].ddy);
  set_vel_next(velNext);
  set_stance_next(CO_air);
  attackNext = attackFree;
}



Walking::Walking(const WalkingContext &,WalkingXdata &)
{}



void Walking::act()
{
  _act();
  Creature::act();
}



void Walking::_act()
{
  const Vel vel = get_vel();
  const Pos &rawPos = get_raw_pos();

  // Avoid getting stuck.
/*  if (alive() && velPrev.dy > 0 && rawPosPrev == rawPos)
    {
      Vel velNext(0,vel.dy);
      set_vel_next(velNext);
#ifdef PRINT_ERRORS
      cerr << "Walking::_act()  trying to get unstuck." << endl;
#endif
      return;
    }
    
  velPrev = vel;
  rawPosPrev = rawPos;
  */

  // Methods of Moving or Creature.
  const CreatureContext *cc = get_creature_context();
  const Stance stance = get_stance();
  const Touching touchingArea = get_touching_area();
  const Touching touchingStance = get_touching_stance();
  const Acc *unitAccs = get_unit_accs();
  const Boolean canClimb = can_climb();
  const Area area = get_area();
  const ITcommand command = get_command(); 
  
  
  // Default set gravity.
  set_grav_next((touchingArea == CO_dn) || 
		(canClimb && (stance == CO_climb) && 
		 alive()) ? 0 : GRAVITY);

  
  // Change stance if necessary.
  if (stance != CO_climb) 
    {
      if (touchingArea != touchingStance)
	{
	  set_want_climb(False);

	  // Hit the ground.
	  if (touchingArea == CO_dn) 
	    {
	      set_stance_next(CO_center);
	      Vel velNew(vel.dx,0);
	      set_vel_next(velNew);
	    }
	  // Side walls.
	  else if (touchingArea == CO_r)
	    {
	      Vel velNew(1,vel.dy);
	      set_vel_next(velNew);
	    }
	  else if (touchingArea == CO_l)
	    {
	      Vel velNew(-1,vel.dy);
	      set_vel_next(velNew);
	    }
	  // In the air.
	  else if (touchingArea == CO_air)
	    {
	      set_stance_next(CO_air);
	    }
	  else if (touchingArea != CO_up)
	    assert(0);
	}
      else if (touchingArea == CO_dn) 
	{
	  Vel velNew(vel.dx,0);
	  set_vel_next(velNew);
	}
    }
  // Stop climbing if not in climbing square. 
  else if (!canClimb)  // stance == CO_climb
    set_stance_next(CO_air);
  
  
  /* Interpret commands from controlling intelligence.  May override some of 
     above settings. */
  switch (command) {
  case IT_CENTER:
    if (stance == CO_air)
      set_want_climb(True);
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_vel_next(0);
	set_grav_next(0);
	center_wsquare_x_next(area.middle_wsquare());
	set_want_climb(False);
      }
    else if (stance != CO_air)
      set_vel_next(0);
    break;
    
  case IT_R:
    if ((stance == CO_dn) || (stance == CO_center) || // Regular walking.
	(stance == CO_climb)) 
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_R]);
    break;
    
  case IT_DN_R:
    if (stance == CO_dn)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_R]);
    else if (stance == CO_center) // Crawling.
      {
	set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_R]);
	set_stance_next(CO_dn);
      }
    else if (stance == CO_climb)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_R]);
    break;
    
  case IT_DN:
    if (stance == CO_air)
      set_want_climb(True);
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_vel_next(0);
	set_grav_next(0);
	center_wsquare_x_next(area.middle_wsquare());
	set_want_climb(False);
      }
    // slow down.
    else if (stance == CO_dn) 
      set_vel_next(0);
    else if (stance == CO_center) 
      set_stance_next(CO_dn);
    else if (stance == CO_climb)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_climb_DN]);
    break;
    
  case IT_DN_L:
    if (stance == CO_dn) // Crawling.
      set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_L]);
    else if (stance == CO_center) // Crawling.
      {
	set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_L]);
	set_stance_next(CO_dn);
      }
    else if (stance == CO_climb) 
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_L]);
    break;
    
  case IT_L:
    if ((stance == CO_dn) || (stance == CO_center) || // Regular walking.
	(stance == CO_climb)) 
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_L]);
    break;
    
  case IT_UP_L:
    if ((stance == CO_dn) || (stance == CO_center) || (stance == CO_climb)) 
      { // Jump left.
	set_vel_next(vel + cc->jump * unitAccs[CO_air_L]);
	set_stance_next(CO_air);
      }
    break;
    
  case IT_UP:
    if (stance == CO_air)
      set_want_climb(True);
    // Doors are handled by Creature.
    if (on_door())
      break;
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_vel_next(0);
	set_grav_next(0);
	center_wsquare_x_next(area.middle_wsquare());
	set_want_climb(False);
      }
    else if (stance == CO_dn)
      set_stance_next(CO_center);
    else if (stance == CO_center) // Jump up.
      {
	set_vel_next(vel + cc->jump * unitAccs[CO_air_UP]);
	set_stance_next(CO_air);
      }
    else if (stance == CO_climb)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_climb_UP]);
    break;
    
  case IT_UP_R:
    // Jump right.
    if ((stance == CO_dn) || (stance == CO_center) || (stance == CO_climb)) 
      { 
	set_vel_next(vel + cc->jump * unitAccs[CO_air_R]);
	set_stance_next(CO_air);
      }
    break;
  }
}



Sticky::Sticky(const StickyContext &,StickyXdata &)
{
  wantCorner1 = wantCorner2 = CO_air;
}



Boolean Sticky::is_sticky()
{
  return True;
}



void Sticky::act() 
{
  _act(); 
  Creature::act();
}



void Sticky::_act()
{
  // Methods of Moving or Creature.
  const CreatureContext *cc = get_creature_context();
  const Stance stance = get_stance();
  const Touching touchingArea = get_touching_area();
  const Touching touchingStance = get_touching_stance();
  const Hanging hanging = get_hanging();
  const Vel vel = get_vel();
  const Acc *unitAccs = get_unit_accs();
  const Boolean canClimb = can_climb();
  const Area area = get_area();
  const ITcommand command = get_command(); 
  
  /* Set gravity.  Don't lock on walls if dead. */
  set_grav_next((!alive() && (touchingArea != CO_dn)) ||
		((touchingArea == CO_air) && 
		 !(canClimb && (stance == CO_climb))) 
		? GRAVITY : 0);
  
  /* Make stance correspond to touching.  I.e. attach / detach to wall.
     Do not attach to wall if climbing. */
  if ((stance != CO_climb) || !canClimb)
    {
      if ((touchingArea != touchingStance) || 
	  ((stance == CO_climb) && !canClimb))
	{
	  set_want_climb(False);

	  if (touchingArea == CO_dn)
	    {
	      set_stance_next(CO_center);
	      Vel velNew(vel.dx,0);
	      set_vel_next(velNew);
	    }
	  else if (touchingArea == CO_r)
	    {
	      set_stance_next(CO_r);
	      set_vel_next(0);
	    }
	  else if (touchingArea == CO_l)
	    {
	      set_stance_next(CO_l);
	      set_vel_next(0);
	    }
	  else if (touchingArea == CO_up)
	    {
	      set_stance_next(CO_up);
	      set_vel_next(0);
	    }
	  else if (touchingArea == CO_air)
	    {
	      set_stance_next(CO_air);
	    }
	  else
	    assert(0);
	}
      else if ((touchingArea == CO_dn) || (touchingArea == CO_up))
	{
	  Vel velNew(vel.dx,0);
	  set_vel_next(velNew);
	}
      else if ((touchingArea == CO_r) || (touchingArea == CO_l))
	{
	  Vel velNew(0,vel.dy);
	  set_vel_next(velNew);
	}
    }
  // Stop climbing if not in climbing square. 
//  else if (! canClimb) // stance == CO_climb
//    set_stance_next(CO_air);
  

  /* Interpret commands from controlling intelligence.  May override some of 
     above settings. */
  switch (command) {
  case IT_CENTER:
    if (stance == CO_air)
      set_want_climb(True);
    set_want_corner(CO_air);  
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_vel_next(0);
	set_grav_next(0);
	center_wsquare_x_next(area.middle_wsquare());
      }
    else if (stance != CO_air)
      set_vel_next(0);
    break;
    
  case IT_R:
    set_want_corner(CO_air);  
    if (stance == CO_r) // On right wall.
      {
	set_vel_next(0);
	set_want_corner(CO_r_UP,CO_r_DN);
      }
    else if ((stance == CO_dn) || (stance == CO_center) || // Regular walking.
	     (stance == CO_climb)) 
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_R]);
    else if (stance == CO_l) // On left wall.
      set_vel_next(vel + PUSH_OFF * unitAccs[CO_R]);
    else if (stance == CO_up) // On ceiling.
      set_vel_next(vel + cc->acceleration * unitAccs[CO_up_R]);
    break;
    
  case IT_DN_R:
    set_want_corner(CO_air);  
    if (stance == CO_r) // On right wall.
      {
	set_want_corner(CO_r_DN);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_r_DN]);
      }
    else if (stance == CO_dn)
      {
	set_want_corner(CO_dn_R);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_R]);
      }
    else if ((stance == CO_l) || (stance == CO_up)) 
      set_vel_next(vel + PUSH_OFF * unitAccs[CO_DN_R]);
    else if (stance == CO_center) // Crawling.
      {
	set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_R]);
	set_stance_next(CO_dn);
      }
    else if (stance == CO_climb)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_R]);
    break;
    
  case IT_DN:
    if (stance == CO_air)
      set_want_climb(True);
    set_want_corner(CO_air);  
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_vel_next(0);
	set_grav_next(0);
	center_wsquare_x_next(area.middle_wsquare());
      }
    else if (stance == CO_dn) 
      {
	set_vel_next(0);
	set_want_corner(CO_dn_R,CO_dn_L);
      }
    else if (stance == CO_center)  // duck
      set_stance_next(CO_dn);
    else if (stance == CO_up)
      set_vel_next(vel + PUSH_OFF * unitAccs[CO_DN]);
    else if (stance == CO_r)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_r_DN]);
    else if (stance == CO_l)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_l_DN]);    
    else if (stance == CO_climb)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_climb_DN]);
    break;
    
  case IT_DN_L:
    set_want_corner(CO_air);  
    if ((stance == CO_r) || (stance == CO_up)) // On other walls.
      set_vel_next(vel + PUSH_OFF * unitAccs[CO_DN_L]);
    else if (stance == CO_dn) // Crawling.
      {
	set_want_corner(CO_dn_L);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_L]);
      }
    else if (stance == CO_l) // On left wall.
      {
	set_want_corner(CO_l_DN);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_l_DN]);
      }
    else if (stance == CO_center) // Crawling.
      {
	set_vel_next(vel + cc->acceleration * unitAccs[CO_dn_L]);
	set_stance_next(CO_dn);
      }
    else if (stance == CO_climb) 
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_L]);
    break;
    
  case IT_L:
    set_want_corner(CO_air);  
    if (stance == CO_l) // On left wall.
      {
	set_want_corner(CO_l_DN,CO_l_UP);
	set_vel_next(0);
      }
    else if ((stance == CO_dn) || (stance == CO_center) || // Regular walking.
	     (stance == CO_climb)) 
      set_vel_next(vel + cc->acceleration * unitAccs[CO_center_L]);
    else if (stance == CO_r) // On right wall.
      set_vel_next(vel +  PUSH_OFF * unitAccs[CO_L]);
    else if (stance == CO_up) // On ceiling.
      set_vel_next(vel + cc->acceleration * unitAccs[CO_up_L]);
    break;
    
  case IT_UP_L:
    set_want_corner(CO_air);  
    if ((stance == CO_r) || (stance == CO_dn) || (stance == CO_center) ||
	(stance == CO_climb)) 
      { // Jump left.
	set_vel_next(vel + cc->jump * unitAccs[CO_air_L]);
	set_stance_next(CO_air);
      }
    else if (stance == CO_l) // On left wall.
      {
	set_want_corner(CO_l_UP);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_l_UP]);
      }
    else if (stance == CO_up) // On ceiling.
      {
	set_want_corner(CO_up_L);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_up_L]);
      }
    break;
    
  case IT_UP:
    // Doors are handled by Creature.
    if (on_door())
      break;
    if (stance == CO_air)
      set_want_climb(True);
    set_want_corner(CO_air);  
    if ((stance != CO_climb) && canClimb)
      {
	set_stance_next(CO_climb);
	set_vel_next(0);
	set_grav_next(0);
	center_wsquare_x_next(area.middle_wsquare());
      }
    else if (stance == CO_up) // On ceiling.
      {
	set_want_corner(CO_up_R,CO_up_L);
	set_vel_next(0);
      }
    else if (stance == CO_dn) 
      set_stance_next(CO_center);
    else if (stance == CO_center) // Jump up.
      {
	set_vel_next(vel + cc->jump * unitAccs[CO_air_UP]);
	set_stance_next(CO_air);
      }
    else if (stance == CO_r) // On right wall.
      set_vel_next(vel + cc->acceleration * unitAccs[CO_r_UP]);
    else if (stance == CO_l) // On left wall.
      set_vel_next(vel + cc->acceleration * unitAccs[CO_l_UP]);
    else if (stance == CO_climb)
      set_vel_next(vel + cc->acceleration * unitAccs[CO_climb_UP]);
    break;
    
  case IT_UP_R:
    set_want_corner(CO_air);  
    if (stance == CO_r) // On right wall.
      {
	set_want_corner(CO_r_UP);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_r_UP]);
      }
    else if ((stance == CO_dn) || (stance == CO_l) || (stance == CO_center) ||
	     (stance == CO_climb)) 
      { /* Jump right. */
	set_vel_next(vel + cc->jump * unitAccs[CO_air_R]);
	set_stance_next(CO_air);
      }
    else if (stance == CO_up) // On ceiling.
      {
	set_want_corner(CO_up_R);
	set_vel_next(vel + cc->acceleration * unitAccs[CO_up_R]);
      }
    break;
  }
  
  // Go around outside corners. 
  if (want_corner(hanging.corner))
    {
      set_stance_next(cornered_stance(hanging));
      set_vel_next(0);
      corner(hanging);
      set_want_corner(CO_air);
    }
}



Boolean Sticky::want_corner(const Corner &corner)
{
  return (corner != CO_air) && 
    ((corner == wantCorner1) || (corner == wantCorner2));
}



Stance Sticky::cornered_stance(const Hanging &hanging)
{
  switch (hanging.corner) {
  case CO_r_DN:
    return CO_up;
  case CO_r_UP:
    return CO_dn;
  case CO_dn_R:
    return CO_l;
  case CO_dn_L:
    return CO_r;
  case CO_l_DN:
    return CO_up;
  case CO_l_UP:
    return CO_dn;
  case CO_up_R:
    return CO_l;
  case CO_up_L:
    return CO_r;
  default:
    assert(0);
    return CO_air;
  }
}



Flying::Flying(const FlyingContext &cx,FlyingXdata &)
{
  context = &cx;
  Timer nTimer(cx.gravTime);
  gravTimer = nTimer;
}



Boolean Flying::is_flying()
{
  return True;
}



void Flying::act() 
{
  _act(); 
  Creature::act();
}



void Flying::_act()
{
  // Methods of Moving or Creature.
  const CreatureContext *cc = get_creature_context();
  const Stance stance = get_stance();
  const Touching touchingArea = get_touching_area();
  const Touching touchingStance = get_touching_stance();
  const Vel vel = get_vel();
  const Acc *unitAccs = get_unit_accs();
  const ITcommand command = get_command(); 
  assert(stance == CO_air || stance == CO_center);

  /* Deal with gravity here instead of in Creature because falling less than
     one pixel per clock. */
  // Limits on velNext imposed by Creature.
  Vel velNext = vel;
  if (touchingArea != CO_dn && gravTimer.ready() && 
      vel.dy <= FLYING_GRAV_VEL_CUTOFF)
    {
      velNext.dy += 1;
      gravTimer.set();
    }

  if (touchingArea != touchingStance)
    {
      if (touchingArea == CO_dn)
	{
	  set_stance_next(CO_center);
	  velNext.dy = 0;
	}
      else if (touchingArea == CO_air)
	set_stance_next(CO_air);
    }  

  /* Interpret commands from controlling intelligence.  May override some of 
     above settings. */
  switch (command) {
  case IT_CENTER:
    velNext.set_zero();
    break;

  case IT_R:
    velNext = velNext + cc->acceleration * unitAccs[CO_R];
    break;
    
  case IT_DN_R:
    if (stance == CO_air)
      velNext = velNext + cc->acceleration * unitAccs[CO_DN_R];
    else
      velNext = velNext + cc->acceleration * unitAccs[CO_center_R];
    break;
    
  case IT_DN:
    if (stance == CO_air)
      velNext = velNext + cc->acceleration * unitAccs[CO_DN];
    else
      velNext.set_zero();
    break;
    
  case IT_DN_L:
    if (stance == CO_air)
      velNext = velNext + cc->acceleration * unitAccs[CO_DN_L];
    else
      velNext = velNext + cc->acceleration * unitAccs[CO_center_L];
    break;
    
  case IT_L:
    velNext = velNext + cc->acceleration * unitAccs[CO_L];
    break;
    
  case IT_UP_L:
    if (stance == CO_air)
      velNext = velNext + cc->acceleration * unitAccs[CO_air_L];
    else
      {
	velNext = velNext + cc->jump * unitAccs[CO_air_L];
	set_stance_next(CO_air);
      }
    break;

  case IT_UP:
    // Doors are handled by Creature.
    if (on_door())
      break;
    if (stance == CO_air)
      velNext = velNext + cc->acceleration * unitAccs[CO_air_UP];
    else
      {
	velNext = velNext + cc->jump * unitAccs[CO_air_UP];
	set_stance_next(CO_air);
      }
    break;

  case IT_UP_R:
    if (stance == CO_air)
      velNext = velNext + cc->acceleration * unitAccs[CO_air_R];
    else
      {
	velNext = velNext + cc->jump * unitAccs[CO_air_R];
	set_stance_next(CO_air);
      }
    break;
  };

  set_vel_next(velNext);

  gravTimer.clock();
}



BuiltIn::BuiltIn(const BuiltInContext &cx,BuiltInXdata &)
{
  context = &cx;
  Timer nTimer(cx.shotTime);
  shotTimer = nTimer;
}



Boolean BuiltIn::is_built_in()
{
  return True;
}



ClassId BuiltIn::get_weapon_string(const char *&str)
{
  str = context->weaponStr;
  return context->weaponClassId;
}



int BuiltIn::get_built_in_weapon_coolness()
{
  return context->coolness;
}



Boolean BuiltIn::ready()
{
  return shotTimer.ready();
}



void BuiltIn::act()
{
  _act();
  Creature::act();
}



void BuiltIn::_act()
{
  Dir shotDir = compute_weapon_dir(get_command());
  if (shotTimer.ready() && 
      shotDir != CO_air &&
      !get_weapon_current())
    {
      const Area &area = get_area();
      LocatorP locator = get_locator();
      Pos shotPos = 
	Coord::shot_initial_pos(area,get_shot_size(shotDir),shotDir);
      PhysicalP shot = create_shot(shotPos,shotDir);
      shot->set_dont_collide(this);
      locator->add(shot);
      shotTimer.set();

      // Signal that we attacked something.
      attack_hook();
    } 
  
  shotTimer.clock();
}



Dir BuiltIn::compute_weapon_dir(ITcommand command)
{
  return Intel::command_weapon_to_dir_8(command);
}



Hugger::Hugger(const HuggerContext &cx,HuggerXdata &)
{
  context = &cx;
}



Boolean Hugger::is_hugger()
{
  return True;
}



int Hugger::get_drawing_level()
{
  return 2;
}



void Hugger::act() 
{
  _act();
  Creature::act();
}



void Hugger::_act()
{
  LocatorP locator = get_locator();
  PhysicalP huggee = locator->lookup(huggeeId);
  
  if (alive() && huggee) 
    {
      if (!huggee->alive())
	{
	  // Someone else killed the huggee while being hugged.
	  Id invalid;
	  huggeeId = invalid;
	}
      else if (!stunned() && !stunned_next())
	{
	  // Create new alien.
	  const Area &area = huggee->get_area();
	  Pos middle = area.get_middle();
	  Size alienSize = Alien::get_size_max();
	  Pos alienPos(middle.x - alienSize.width / 2,
		       middle.y - alienSize.height / 2);
	  PhysicalP alien = new Alien(get_world(),locator,alienPos);
	  assert(alien);
	  locator->add(alien);
	  // Set the intelligence of the new Alien.
	  if (context->useHuggeeIntel) 
	    {
	      // Use intelligence of huggee.  
	      alien->set_intel(huggee->get_intel()); 
	      huggee->set_intel(NULL);
	    }
	  else
	    {
	      // Use own intelligence.  Huggee's intel dies with the huggee's
	      // physical body.
	      alien->set_intel(get_intel());
	      set_intel(NULL);
	    }
	  
	  // kill huggee
	  huggee->kill_self();
	  
	  // kill hugger
	  kill_self();
	}
      else  // huggee is alive() but hugger is stunned.
	// Stay locked onto hugee's face.
	{
	  const Area &area = huggee->get_area();
	  Pos p;
	  Size s;
	  area.get_rect(p,s);
	  // Guess where hugee's face is, upper-left + (width/2,height*.3).
	  Pos pNext(p.x + (int) (0.5 * s.width),
		    p.y + (int) (0.3 * s.height));
	  set_middle_next(pNext);
	}

    } // if (alive() && huggee)
}



void Hugger::collide(PhysicalP other)
{
  LocatorP l = get_locator();

  if (
      // Dead Huggers don't hug.
      alive() &&

      // Make sure not already hugging something.
      !l->lookup(huggeeId) &&  

      // Only hug living biological creatures.
      other->alive() &&
      other->is_creature() &&
      ((CreatureP)other)->biological() &&

      // Don't hug aliens or other huggers.
      other->get_class_id() != A_Alien && 
      !other->is_hugger() &&

      // Don't hug alien immune creatures.
      !((CreatureP)other)->get_alien_immune()
      ) 
    {
      // Kind of a hack, stun self as well as huggee.
      stun_next(HUGGER_TIME);

      // Stun other for one turn longer so that we have one turn to 
      // turn other into an alien.
      ((CreatureP)other)->stun_next(HUGGER_TIME + 1);

      // Don't want other to get killed or hugged when already being hugged.
      // Need two extra turns, one to turn other into Alien, and one for
      // synchronization as alienImmune is immediate and stun is not.
      ((CreatureP)other)->set_alien_immune(HUGGER_TIME + 2);
      
      huggeeId = other->get_id();
    }
  
  Creature::collide(other);
}
