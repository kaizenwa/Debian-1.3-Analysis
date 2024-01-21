// "locator.C"

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
#pragma implementation "locator.h"
#endif


// Include Files
#include "utils.h"
#include "coord.h"
#include "id.h"
#include "area.h"
#include "physical.h"
#include "locator.h"
#include "actual.h"



// Defines
#define EXTRA_DRAW_ROWS 1  // What are these for??
#define EXTRA_DRAW_COLS 1



// Functions
const Area &OLgridEntry::get_area() 
{
  assert(prev);
  return entry->physical->get_area();
}



int OLgridEntry::get_drawing_level() 
{
  assert(entry->physical && prev); 
  return entry->physical->get_drawing_level();
}



void OLgridEntry::insert(OLgrid grid,const GLoc &gl)
{
  assert((prev == NULL) && (next == NULL));

  assert(Locator::valid(gl));
  entry->gloc = gl;

  next = grid[gl.vert][gl.horiz]->next;
  prev = grid[gl.vert][gl.horiz];
  if (grid[gl.vert][gl.horiz]->next)
    grid[gl.vert][gl.horiz]->next->prev = this;
  grid[gl.vert][gl.horiz]->next = this;
}



void OLgridEntry::remove()
{
  assert(prev);

  if (next)
    next->prev = prev;
  prev->next = next;
  prev = next = NULL;
}



void OLshadowEntry::insert(OLshadows shadows,const GLoc &gl)
{
  assert((prev == NULL) && (next == NULL));

  entry->shadowGloc = gl;
  assert(Locator::valid(gl));

  next = shadows[gl.vert][gl.horiz]->next;
  prev = shadows[gl.vert][gl.horiz];
  if (shadows[gl.vert][gl.horiz]->next)
    shadows[gl.vert][gl.horiz]->next->prev = this;
  shadows[gl.vert][gl.horiz]->next = this;
}



void OLshadowEntry::remove()
{
  assert (prev);

  if (next)
    next->prev = prev;
  prev->next = next;
  prev = next = NULL;
}



HumanP Incarnator::operator () ()
{
  while (n < locator->humansNum && 
	 !(locator->reincarnating[n] &&
	   locator->reincarnateTimers[n].ready()))
    n++;

  if (n == locator->humansNum)
    return NULL;
  else
    {
      // Extract human.
      locator->reincarnating[n] = False;
      return locator->humans[n];
    }
}



PhysicalP PhysicalIter::operator () ()
{
  while (n < locator->listMax && !locator->list[n].valid)
    n++;
  
  assert(n <= locator->listMax);
  if (n == locator->listMax)
    return NULL;
  else
    {
      n++;
      return locator->list[n-1].physical;
    }
}



Locator::Locator(WorldP w)
{
  contextCount = 0;
  world  = w;

  register_contexts();

  uniqueGen = 0;

  listMax = 0;

  for (int c = 0; c < OL_GRID_HORIZ_MAX; c++)
    for (int r = 0; r < OL_GRID_VERT_MAX; r++)
      {
	grid[r][c] = new OLgridEntry(NULL);
	assert(grid[r][c]);
	shadows[r][c] = new OLshadowEntry();
	assert(shadows[r][c]);
      }

  addNum = 0;
  shadowDelNum = 0;

  xValid = False;

  head = tail = 0;
  messagesIgnore = False;

  humansNum = enemiesNum = neutralsNum = 0;
  for (int n = 0; n < HUMANS_MAX; n++)
    {
      Timer nTimer(REINCARNATE_TIME);
      reincarnateTimers[n] = nTimer;
    }
}



void Locator::add(PhysicalP p)
{
  if (addNum >= OL_LIST_MAX)
    {
      cerr << "Object locator is full.  Can only have " << OL_LIST_MAX 
	   << " objects in the game." << endl;
      assert(0);
    }
  addList[addNum] = p;
  addNum++;
  p->set_id(reserve_list_entry(p));
}



void Locator::get_nearby(PhysicalP nearby[OL_NEARBY_MAX],int &nitems,
			 PhysicalP p,int radius)
{
  int glocRadius = (int)ceil(radius / OL_GRID_SIZE_MAX);
  int radius_2 = radius * radius;
  nitems = 0;

  const Id id = p->get_id();
  GLoc pGLoc = list[id.index].gloc;
  const Area &pArea = p->get_area();
  Pos pPos = pArea.get_middle();

  GLoc gloc;
  for (gloc.horiz = pGLoc.horiz - glocRadius; 
       gloc.horiz <= pGLoc.horiz + glocRadius; gloc.horiz++)
    for (gloc.vert = pGLoc.vert - glocRadius; 
	 gloc.vert <= pGLoc.vert + glocRadius; 
	 gloc.vert++)
      if (valid(gloc))
	{
	  // Skip the header.
	  OLgridEntry *ge = grid[gloc.vert][gloc.horiz]->get_next(); 
	  while (ge)
	    {
	      PhysicalP p2 = ge->get_physical();
	      if (p != p2)
		{
		  const Area& area = p2->get_area();
		  if (pPos.distance_2(area.get_middle()) <= radius_2)
		    {
		      assert(nitems < OL_NEARBY_MAX);
		      nearby[nitems] = p2;
		      nitems++;
		    }
		}
	      ge = ge->get_next();
	    }
	}
}



OLsig Locator::lookup(PhysicalP &p, const Id &id)
{
  p = lookup(id);
  return p ? OL_NO_SIG : OL_NOT_FOUND;
}



PhysicalP Locator::lookup(const Id &id)
{
  if ((id.index != Id::INVALID) &&
      (list[id.index].valid || list[id.index].reserved))
    {
      Id testId = list[id.index].physical->get_id();
      if (testId == id)
	return list[id.index].physical;
    }
  return NULL;
}



IntelP Locator::lookup(const IntelId &iId)
{
  if (iId.index == IntelId::INVALID)
    return NULL;

  // Check humans.
  if (iId.index < humansNum)
    {
      IntelId testId = humans[iId.index]->get_intel_id();
      if (testId == iId)
	return humans[iId.index];
    }

  // Check enemies.
  if (iId.index < enemiesNum && enemies[iId.index]->alive())
    {
      IntelId testId = enemies[iId.index]->get_intel_id();
      if (testId == iId)
	return enemies[iId.index];
    }

  // Check neutrals.
  if (iId.index < neutralsNum && neutrals[iId.index]->alive())
    {
      IntelId testId = neutrals[iId.index]->get_intel_id();
      if (testId == iId)
	return neutrals[iId.index];
    }

  return NULL;
}



void Locator::draw(Drawable window,Xvars &xvars,int dpyNum,const Box &room)
{
  /* Don't draw if outside the world. */
  if (world->overlap(room)) 
    {
      if (!xValid)
	init_x(xvars);
      
      // Draw new stuff
      GLoc gridStart; 
      gridStart.horiz = 
	(int)floor((float)room.loc.c / (float)OL_GRID_COL_MAX) - 1;
      gridStart.vert = 
	(int)floor((float)room.loc.r / (float)OL_GRID_ROW_MAX) - 1;
      
      GLoc gridFinish; 
      gridFinish.horiz = (int)ceil((float)(room.loc.c + room.dim.colMax) / 
				   (float) OL_GRID_COL_MAX) + 1;
      gridFinish.vert = (int)ceil((float)(room.loc.r + room.dim.rowMax) /
				  (float) OL_GRID_ROW_MAX) + 1;
      
      // Go through all shadows in room.
	GLoc gloc;
      for (gloc.vert = gridStart.vert; gloc.vert < gridFinish.vert; 
	   gloc.vert++)
	for (gloc.horiz = gridStart.horiz; gloc.horiz < gridFinish.horiz; 
	     gloc.horiz++)
	  if (valid(gloc))
	    {
	      OLshadowEntry *se = shadows[gloc.vert][gloc.horiz]->get_next();
		  
	      while (se)
		{
		  const Area &shadowArea = se->get_area();
		  if (se->get_orphaned())
		    draw_area(window,xvars,dpyNum,room,shadowArea,gloc);
		  else
		    {
		      OLgridEntry *ge = se->get_grid_entry();
		      if (shadowArea.overlap(ge->get_area()))
			se->set_draw_self(False);
		      else
			{
			  se->set_draw_self(True);
			  draw_area(window,xvars,dpyNum,room,
				    shadowArea,gloc);
			}
		    }
		  se = se->get_next();
		}
	    }
	  
      // go through all grid entries in room.
      for (gloc.vert = gridStart.vert; gloc.vert < gridFinish.vert; 
	   gloc.vert++)
	for (gloc.horiz = gridStart.horiz; gloc.horiz < gridFinish.horiz; 
	     gloc.horiz++)
	  if (valid(gloc))
	    {
	      OLgridEntry *ge = grid[gloc.vert][gloc.horiz]->get_next();
	      
	      while (ge)
		{
		  OLentry *ent = ge->get_entry();
		  assert (ent->gloc == gloc);

		  if (ge->get_mapped() || ge->get_flash())
		    {
		      OLshadowEntry *se = ge->get_shadow_entry();
		      
		      const Area &shadowArea = se->get_area();
		      if (se->draw_self() ||
			  // Don't try to combine areas if the shadow is 
			  // not in the room.
			  // Edge effects taken care of by extra +1 in 
			  // looking at grid squares of the room.
			  !shadowArea.overlap(room))
			draw_area(window,xvars,dpyNum,room,ge->get_area(),
				  gloc);
		      else
			draw_area(window,xvars,dpyNum,room,
				  shadowArea.combine(ge->get_area()),gloc);
		    }
		  ge = ge->get_next();
		}
	    }
    }
}

  

void Locator::reset()
{
  // Destroy the addList.
  int n;
  for (n = 0; n < addNum; n++)
    delete addList[n];
  addNum = 0;

  for (n = 0; n < listMax; n++)
    if (list[n].valid)
      {
	del_now(list[n].physical);
	delete list[n].physical;
      }
  listMax = 0;

  uniqueGen = 0;
  
  // Check grid integrity.  There should only be the headers remaining.
  for (int c = 0; c < OL_GRID_HORIZ_MAX; c++)
    for (int r = 0; r < OL_GRID_VERT_MAX; r++)
      assert( grid[r][c] && (grid[r][c]->get_next() == NULL));

  // Delete all messages remaining in the queue.
  while (head != tail)
    {
      delete messages[head];
      head = (head + 1) % OL_MESSAGES_MAX;
    }

  for (n = 0; n < humansNum; n++)
    delete humans[n];

  for (n = 0; n < enemiesNum; n++)
    delete enemies[n];

  for (n = 0; n < neutralsNum; n++)
    delete neutrals[n];

  humansNum = enemiesNum = neutralsNum = 0;
}



void Locator::clock()
{
  // Kill shadow delete list.
  int n;       
  for (n = 0; n < shadowDelNum; n++)
    {
      shadowDelList[n]->remove();
      delete shadowDelList[n];
    }
  shadowDelNum = 0;

  // Move shadows to previous area.
  for (n = 0; n < listMax; n++)
    if (list[n].valid && (list[n].mapped || list[n].flash))
      {
	list[n].shadowEntry->set_area(list[n].physical->get_area());
	GLoc realGloc = compute_gloc(list[n].shadowEntry->get_area());

	if (list[n].shadowGloc != realGloc)
	  {
	    list[n].shadowEntry->remove();
	    list[n].shadowEntry->insert(shadows,realGloc);
	  }
      }

  /* Collision phase.  Physical::collide will be called at most once for any
     object. */
  collision_checks();

  /* Intelligence phase and action phase.  Set next variables.  Externally 
     visible state should not change. 
     Exceptions to this: canTake. */
  for (n = 0; n < listMax; n++)
    if (list[n].valid)
      {
	// Think before you act.
	list[n].physical->intelligence();
	list[n].physical->act();
      }

  /* Kill objects outside world. */
  for (n = 0; n < listMax; n++)
    if (list[n].valid && (list[n].mapped || list[n].flash))
      {
	const Area &area = list[n].physical->get_area_next();
	if (!world->inside(area.middle_wsquare()))
	  //	    (&& !list[n].physical->delete_me())
	  {
	    list[n].physical->set_quiet_death();
	    list[n].physical->kill_self();

#ifdef PRINT_ERRORS
	    cerr << list[n].physical->get_class_name() << 
	      " knocked out of world." << endl; 
#endif
	  }
      }
  
  // Check for death.  I.e. healthNext < 0.
  /* NOTE: If using kill_self in this phase, an object must manually 
     call die and check for die_called().  Hack in Frog::die. */
  for (n = 0; n < listMax; n++)
    if (list[n].valid &&
	list[n].physical->get_health_next() < 0 && 
	list[n].physical->get_health() >= 0 &&
	!list[n].physical->die_called())
      list[n].physical->die();
  
  // Update externally visable state.
  for (n = 0; n < listMax; n++)
    if (list[n].valid) 
      list[n].physical->update();
  
  /* Update the locator grid, shifting gridEntries around if necessary. */
  for (n = 0; n < listMax; n++)
    if (list[n].valid)
      {
	allign_flash_and_mapped(n);
	if (list[n].mapped || list[n].flash)
	  {
	    const Area &area = list[n].physical->get_area();
	    if (world->inside(area.middle_wsquare()))
	      {
		GLoc realGLoc = compute_gloc(list[n].physical);
		
		if (list[n].gloc != realGLoc)
		  {
		    list[n].gridEntry->remove();
		    list[n].gridEntry->insert(grid,realGLoc);
		  }
	      }
	    else
	      assert(list[n].physical->die_called() && 
		     list[n].physical->delete_me());
	  }

	list[n].collided = False;
      }

  // Add all objects on the list.
  for (n = 0; n < addNum; n++)
    add_now(addList[n]);
  addNum = 0;

  // Delete objects that are dead.
  for (n = 0; n < listMax; n++)
    if (list[n].valid && list[n].physical->delete_me())
      {
	del_now(list[n].physical);
	delete list[n].physical;
      }
  
  reincarnate_clock();
}



void Locator::message_enq(char *msg)
{
  if (messagesIgnore)
    {
      delete msg;
      return;
    }

  if ((tail + 1) % OL_MESSAGES_MAX != head)
    {
      messages[tail] = msg;
      tail = (tail + 1) % OL_MESSAGES_MAX;
    }
  else
    {
      delete msg;
//    cout << "Warning: Message queue overflow.  Discarding message." << endl;
    }
}



char *Locator::message_deq()
{
  if (head == tail)
    return NULL;

  int ret = head;
  head = (head + 1) % OL_MESSAGES_MAX;
  
  return messages[ret];
}



void Locator::register_human(HumanP h)
{
  // Assign h an IntelId.
  assert(humansNum < HUMANS_MAX);
  IntelId iId = h->get_intel_id();
  assert(iId.index == IntelId::INVALID);
  iId.index = humansNum;
  iId.unique = uniqueGen;
  uniqueGen++;
  h->set_intel_id(iId);

  // Add to the array of Humans.
  humans[humansNum] = h;
  reincarnating[humansNum] = False;
  reincarnateTimers[humansNum].reset();
  humansNum++;
}



void Locator::register_enemy(EnemyP m)
{
  IntelId iId = m->get_intel_id();
  assert(iId.index == IntelId::INVALID);
  iId.unique = uniqueGen;
  uniqueGen++;

  // Try inserting in current array.
  for (int n = 0; n < enemiesNum; n++)
    if (!enemies[n]->alive())
      {
	delete enemies[n];
	enemies[n] = m;
	iId.index = n;
	m->set_intel_id(iId);
	return;
      }

  // Else add to array.
  assert(enemiesNum < ENEMIES_MAX);
  enemies[enemiesNum] = m;
  iId.index = enemiesNum;
  m->set_intel_id(iId);
  enemiesNum++;
}



void Locator::register_neutral(NeutralP m)
{
  IntelId iId = m->get_intel_id();
  assert(iId.index == IntelId::INVALID);
  iId.unique = uniqueGen;
  uniqueGen++;

  // Try inserting in current array.
  for (int n = 0; n < neutralsNum; n++)
    if (!neutrals[n]->alive())
      {
	delete neutrals[n];
	neutrals[n] = m;
	iId.index = n;
	m->set_intel_id(iId);
	return;
      }

  // Else add to array.
  assert(neutralsNum < NEUTRALS_MAX);
  neutrals[neutralsNum] = m;
  iId.index = neutralsNum;
  m->set_intel_id(iId);
  neutralsNum++;
}



int Locator::humans_playing()
{
  int n,ret;
  for (n = 0, ret = 0; n < humansNum; n++)
    if (humans[n]->alive() || humans[n]->reincarnate_me())
      ret++;
  return ret;
}



int Locator::enemies_alive()
{
  int n,ret;
  for (n = 0, ret = 0; n < enemiesNum; n++)
    if (enemies[n]->alive())
      ret++;
  return ret;
}



int Locator::neutrals_alive()
{
  int n,ret;
  for (n = 0, ret = 0; n < neutralsNum; n++)
    if (neutrals[n]->alive())
      ret++;
  return ret;
}



int Locator::filter_contexts(const PhysicalContext *contextList[A_CLASSES_NUM],
			     ClassId idList[A_CLASSES_NUM],
			     Boolean (* filter)(const PhysicalContext *))
{
  assert(list);
  int numFound = 0;
  
  int n;
  for (n = 0; n < A_CLASSES_NUM; n++)
    if (contexts[n] && filter(contexts[n]))
      {
	assert(numFound < A_CLASSES_NUM);
	if (contextList)
	  contextList[numFound] = contexts[n];
	if (idList)
	  // We guarantee that the index of a context in Locator::contexts
	  // is the ClassId of that context.
	  idList[numFound] = (ClassId)n;
	numFound ++;
      }
  return numFound;
}



void Locator::add_now(PhysicalP p)
{
  Id id = p->get_id();
  int n = id.index;

  assert (list[n].physical == p);
  list[n].valid = True;
  list[n].collided = False;
  list[n].collidable = p->collidable();
  list[n].mapped = p->get_mapped();
  list[n].flash = p->get_flash();

  if (list[n].mapped || list[n].flash)
    {
      // Create and add shadow and grid entries.
      list[n].gridEntry = new OLgridEntry(&list[n]);
      assert(list[n].gridEntry);
      list[n].shadowEntry = new OLshadowEntry(p->get_area(),&list[n]);
      assert(list[n].shadowEntry);
      GLoc gloc = compute_gloc(list[n].physical);
      list[n].gridEntry->insert(grid,gloc);
      list[n].shadowEntry->insert(shadows,gloc);
    }
}



void Locator::del_now(PhysicalP p)
{
  Id id = p->get_id();
  list[id.index].valid = False;
  list[id.index].reserved = False;

  if (list[id.index].mapped || list[id.index].flash)
    {
      list[id.index].gridEntry->remove();
      delete list[id.index].gridEntry;

      shadowDelList[shadowDelNum] = list[id.index].shadowEntry;
      list[id.index].shadowEntry->set_orphaned();
      shadowDelNum++;
    }

  // Decrease effective size of list.
  if (id.index == listMax - 1)
    listMax--;
}



void Locator::allign_flash_and_mapped(int n)
{
  Boolean flash = list[n].physical->get_flash();
  Boolean mapped = list[n].physical->get_mapped();

  // Add new OLgridEntry and shadowEntry.
  if (!(list[n].mapped || list[n].flash) && (mapped || flash))
    {
      list[n].gridEntry = new OLgridEntry(&list[n]);
      assert(list[n].gridEntry);
      list[n].shadowEntry = 
	new OLshadowEntry(list[n].physical->get_area(),&list[n]);
      assert(list[n].shadowEntry);
      GLoc gloc = compute_gloc(list[n].physical);
      list[n].gridEntry->insert(grid,gloc);      
      list[n].shadowEntry->insert(shadows,gloc);
    }

  // Destroy grid entry and mark shadow entry for deletion.
  if ((list[n].mapped || list[n].flash) && !(mapped || flash))
    {
      list[n].gridEntry->remove();
      delete list[n].gridEntry;
      shadowDelList[shadowDelNum] = list[n].shadowEntry;
      list[n].shadowEntry->set_orphaned();
      shadowDelNum++;
    }
  
  list[n].mapped = mapped;
  list[n].flash = flash;
}



void Locator::init_x(Xvars &xvars)
{
  assert(xValid == False);

  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    {
      xdata.buffer[dpyNum] = 
	XCreatePixmap(xvars.dpy[dpyNum],xvars.root[dpyNum],
		      (OL_GRID_COL_MAX + 2 * EXTRA_DRAW_COLS) 
		      * WSQUARE_WIDTH,
		      (OL_GRID_ROW_MAX + 2 * EXTRA_DRAW_ROWS) 
		      * WSQUARE_HEIGHT,
		      xvars.depth[dpyNum]);
  
      xdata.scratchBuffer[dpyNum] = 
	XCreatePixmap(xvars.dpy[dpyNum],xvars.root[dpyNum],
		      OL_GRID_COL_MAX * WSQUARE_WIDTH,
		      OL_GRID_ROW_MAX * WSQUARE_HEIGHT,
		      xvars.depth[dpyNum]); 
    }
  xValid = True;
}



Id Locator::reserve_list_entry(PhysicalP p)
{
  Id dummy;
  assert(p->get_id(dummy) == PH_NOT_SET);

  for (int n = 0; ; n++)
    {
      assert(n <= listMax); 

      // Increase effective size of list.
      if (n == listMax)
	{
	  assert(n < OL_LIST_MAX); // Object locator list is full.
	  list[n].valid = False;
	  list[n].reserved = False;
	  listMax++;
	}

      if (!list[n].valid && !list[n].reserved)
	{
	  Id ret;
	  ret.index = n;
	  ret.unique = uniqueGen;
	  uniqueGen++;
	  list[n].reserved = True;
	  list[n].physical = p;
	  return ret;
	}
    }
}



void Locator::collision_checks()
{
  for (int n = 0; n < listMax; n++)
    if (list[n].valid && !list[n].collided && list[n].mapped && 
	list[n].collidable &&
	list[n].physical->alive())   // So corpses don't collide, see below.
      {
	PhysicalP p1 = list[n].physical;
	GLoc gloc;

	for (gloc.vert = list[n].gloc.vert - 1; 
	     gloc.vert <= list[n].gloc.vert + 1; 
	     gloc.vert++)
	  for (gloc.horiz = list[n].gloc.horiz - 1; 
	       gloc.horiz <= list[n].gloc.horiz + 1; 
	       gloc.horiz++)
	    if (valid(gloc))
	      {
		// Skip the header.
		OLgridEntry *ge = grid[gloc.vert][gloc.horiz]->get_next(); 
		while (ge)
		  {
		    PhysicalP p2 = ge->get_physical(); 
		    if ((! ge->get_collided()) &&
			ge->get_mapped() &&
			ge->get_collidable() &&
			(p1 != p2) &&
			(p2->get_dont_collide() != p1) &&
			(p1->get_dont_collide() != p2) &&
			p2->alive()) // So corpses don't collide, see below.
		      {
			const Area &a1 = p1->get_area();
			if (a1.overlap(p2->get_area()))
			  {
			    if (p1->get_mass() > p2->get_mass())
			      p2->avoid(p1);
			    else if (p2->get_mass() > p1->get_mass())
			      p1->avoid(p2);
			    else if (Utils::coinFlip())
			      p1->avoid(p2);
			    else
			      p2->avoid(p1);
			    
			    p1->collide(p2);
			    p2->collide(p1);
			    list[n].collided = True;
			    ge->set_collided(True);
			  }
		      }
		    ge = ge->get_next();
		  }
	      }
      }
}



GLoc Locator::compute_gloc(PhysicalP p)
{
  const Area &area = p->get_area();
  Pos middle = area.get_middle(); 
  GLoc gloc;
  gloc.horiz = middle.x / OL_GRID_WIDTH;
  gloc.vert = middle.y / OL_GRID_HEIGHT;
  return gloc;
}



GLoc Locator::compute_gloc(const Area &area)
{
  Pos middle = area.get_middle(); 
  GLoc gloc;
  gloc.horiz = middle.x / OL_GRID_WIDTH;
  gloc.vert = middle.y / OL_GRID_HEIGHT;
  return gloc;
}



void Locator::draw_area(Drawable window,Xvars &xvars,int dpyNum,
			const Box &room,const Area &area,const GLoc &gloc)
{
  // Trivial reject.
  if (!area.overlap(room))
    return;

  world->draw(xdata.buffer[dpyNum],xvars,dpyNum,area);

  for (int dLevel = 0; dLevel < DRAWING_LEVELS; dLevel++)
    {
      GLoc check;
      for (check.horiz = gloc.horiz - 1; 
	   check.horiz <= gloc.horiz + 1; 
	   check.horiz++)
	for (check.vert = gloc.vert - 1; 
	     check.vert <= gloc.vert + 1; 
	     check.vert++)
	  if (valid(check))
	    {
	      OLgridEntry *checkGe = 
		grid[check.vert][check.horiz]->get_next(); 
	      
	      while (checkGe)
		{
		  if ((checkGe->get_drawing_level() == dLevel) && 
		      (checkGe->get_flash() || checkGe->get_mapped()))
		    {
		      PhysicalP checkP = checkGe->get_physical();
		      const Area &checkArea = checkP->get_area();
		      
		      if (checkArea.overlap(area))
			checkP->draw(xdata.buffer[dpyNum],xvars,dpyNum,area);
		    }				
		  checkGe = checkGe->get_next();
		}
	    }
    }

  Pos pos;
  Size size;
  area.get_rect(pos,size);

  XCopyArea(xvars.dpy[dpyNum],xdata.buffer[dpyNum],window,xvars.gc[dpyNum],
	    0,0,size.width,size.height,
	    pos.x - room.loc.c * WSQUARE_WIDTH,
	    pos.y - room.loc.r * WSQUARE_HEIGHT);
}



void Locator::reincarnate_clock()
{
  for (int n = 0; n < humansNum; n++)
    {
      if (!reincarnating[n] && humans[n]->reincarnate_me())
	{
	  reincarnating[n] = True;
	  reincarnateTimers[n].set();
	}
      reincarnateTimers[n].clock();
    }
}



void Locator::register_contexts()
{
  int n;
  for (n = 0; n < A_CLASSES_NUM; n++)
    contexts[n] = NULL;

  // Here we go...
  register_context(&Explosion::context);
  register_context(&Fire::context);
  register_context(&NProtection::context.physicalContext);
  register_context(&TProtection::context.physicalContext);
  register_context(&Trapdoor::context.physicalContext);
  register_context(&Home::context.physicalContext);
  register_context(&Shell::context.movingContext.physicalContext);
  register_context(&SwapShell::context.movingContext.physicalContext);
  register_context(&Lance::context.movingContext.physicalContext);
  register_context(&Laser::context.movingContext.physicalContext);
  register_context(&FrogShell::context.movingContext.physicalContext);
  register_context(&Fireball::context.movingContext.physicalContext);
  register_context(&Missile::context.movingContext.physicalContext);
  register_context(&Star::context.movingContext.physicalContext);
  register_context(&Grenade::context.movingContext.physicalContext);
  register_context(&Egg::context.movingContext.physicalContext);
  register_context(&Xit::context.fallingContext.movingContext.physicalContext);
  register_context(&Flag::context.fallingContext.movingContext.physicalContext);
  register_context(&Rock::context.fallingContext.movingContext.physicalContext);
  register_context(&Weight::context.fallingContext.movingContext.physicalContext);
  register_context(&AltarOfSin::context.fallingContext.movingContext.physicalContext);
  register_context(&Doppel::context.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Cloak::context.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Transmogifier::context.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&MedKit::context.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&NShield::context.autoUseContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&TShield::context.autoUseContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Bomb::context.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Chainsaw::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Pistol::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&MGun::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Lancer::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&FThrower::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Launcher::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Grenades::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  //  register_context(&Shotgun::context.weaponContext.itemContext.fallingContext.
  //		   movingContext.physicalContext);
  register_context(&Stars::context.weaponContext.itemContext.fallingContext.
		   movingContext.physicalContext);
  register_context(&Swapper::context.gunContext.weaponContext.itemContext.
		   fallingContext.movingContext.physicalContext);
  register_context(&FrogGun::context.gunContext.weaponContext.itemContext.
		   fallingContext.movingContext.physicalContext);
  register_context(&Enforcer::creatureContext.movingContext.physicalContext);
  register_context(&Frog::creatureContext.movingContext.physicalContext);
  register_context(&Hero::creatureContext.movingContext.physicalContext);
  register_context(&Ninja::creatureContext.movingContext.physicalContext);
  register_context(&Alien::creatureContext.movingContext.physicalContext);
  register_context(&RedHugger::creatureContext.
		   movingContext.physicalContext);
  register_context(&GreenHugger::creatureContext.
		   movingContext.physicalContext);
  register_context(&ChopperBoy::creatureContext.movingContext.physicalContext);
  register_context(&Lemming::creatureContext.movingContext.physicalContext);
  register_context(&FireDemon::creatureContext.movingContext.physicalContext);
  register_context(&Walker::creatureContext.movingContext.physicalContext);
  


  assert(contextCount <= A_CLASSES_NUM);

  // Some sanity checking, see if something is wrong with the
  // newly programmed PhysicalContexts.
  for (n = 0; n < A_CLASSES_NUM; n++) {
    const PhysicalContext *cx = contexts[n];
    if (cx) {
      // Make sure that the index of a context is the same as it's classId.
      assert(cx->classId == n);

      // Iff potentialEnemy, enemyWeight is greater than zero.
      assert(cx->potentialEnemy == (cx->enemyWeight > 0));
      assert(cx->enemyWeight >= 0);

      assert(cx->get_size_max && cx->create);

      // Should have a get_stats() method iff one of the stats* flags is 
      // set.
      assert((cx->statsCreations || cx->statsUses || cx->statsDeaths) ==
	     (cx->get_stats != NULL));
      
      // Can't act as bot a weapon and otherItem.
      assert(!(cx->potentialWeapon && cx->potentialOtherItem));

      // If Physical could be created as either a weapon or otherItem, make
      // sure objectWorldPercent is greater than zero.
      assert((cx->potentialWeapon || 
	      cx->potentialOtherItem) ==
	     (cx->objectWorldPercent > 0));

      assert(cx->objectWorldPercent >= 0);
    }
  }


  if (contextCount < A_CLASSES_NUM)
    cout << "ERROR: Locator::register_contexts(): Not all classes have been "
      << "registered" << endl;
}



void Locator::register_context(const PhysicalContext *context)
{
  contexts[context->classId] = context;
  contextCount++;
}
