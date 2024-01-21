// "world.C"

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
#pragma implementation "world.h"
#endif


// Include Files.
#include <iostream.h>

#include "utils.h"
#include "coord.h"
#include "area.h"
#include "world.h"
#include "bitmaps/world/world.bitmaps"



// Defines.
#define HANGING_PERCENT 0.40 // The % of the edge hanging off the corner.
#define STRAND_PERCENT .50 // Gives number of boundaries between rooms.
#define STRAND_LENGTH 8 // Length of strands in Blueprints::fill_random.
#define EMPTY_TRIES_MAX 5000
#define WALL_LENGTH 30
#define WALLS_PERCENT .01
#define WALLS_HORIZ_CHANCE 4 // 2 gives equal chance
#define LADDER_CHANCE 10
#define MAP_PRINT_DEFAULT False
#define UP_DOWN_CHANCE 4
#define OPEN_ITERATIONS 2

enum {UN_POSTER,UN_DOOR};

#define POSTERS_ACTUAL_PERCENT .0003  // Percentage of dim.
#define DOORS_ACTUAL_PERCENT .1  // Pairs of doors as percentage of rooms. 

// Used to determine how much space to leave between walls.
#define OBJECT_COL_MAX 2
#define OBJECT_ROW_MAX 2


// The title screen.
#define W_TITLE_COL_MAX 40
#define W_TITLE_ROW_MAX 16
static char world_title[][W_COL_MAX_MAX] = {
  {3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,},
  {3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,},
  {3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,6,0,0,0,0,0,6,0,6,6,6,6,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,6,0,0,0,0,0,6,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,0,6,0,0,0,6,0,0,6,0,0,0,0,0,0,0,0,0,0,0,4,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,0,0,6,0,6,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,0,0,0,6,0,0,0,0,6,6,6,0,0,6,0,0,0,6,0,0,6,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,0,0,6,0,6,0,0,0,6,0,0,0,0,6,0,0,0,6,0,0,6,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,0,6,0,0,0,6,0,0,6,0,0,0,0,0,6,0,6,0,0,0,6,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,6,0,0,0,0,0,6,0,6,0,0,0,0,0,6,0,6,0,0,0,6,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,6,0,0,0,0,0,6,0,6,6,6,6,0,0,0,6,0,0,0,0,6,0,0,6,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,},
  {3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,},
  {3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,},
  {3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,},
};




// Blueprints class.  A helper for World.
class Blueprints {
 public:
  Blueprints(WorldP world,char map[W_ROW_MAX_MAX][W_COL_MAX_MAX]);
  /* EFFECTS: Create a blueprints object with room boundaries defined. */

  int get_middle_row(int down);
  int get_middle_col(int across);
  /* NOTES: Offset from edge of room, in WSQUARES.  Floor at middleRow.
     Ladder at [middleCol ... middleCol + OBJECT_COL_MAX). */

  void fill_map();
  /* MODIFIES: map */
  /* EFFECTS: Using the defined room boundaries, create a map of the world. */

  void print();
  /* EFFECTS: Print the room boundaries to cout. */

  Boolean blocked_right(const RoomIndex &ri);
  Boolean blocked_bottom(const RoomIndex &ri);
  Boolean blocked_left(const RoomIndex &ri);
  Boolean blocked_top(const RoomIndex &ri);
  /* EFFECTS: Returns whether ri has a blocking wall on the edge of the 
     room. */


 private:
  void fill_random();
  Boolean open(const RoomIndex &ri);
  void has_right(const RoomIndex &ri,const Loc &rl);
  void missing_right(const RoomIndex &ri,const Loc &rl);
  void has_bottom(const RoomIndex &ri,const Loc &rl);
  void missing_bottom(const RoomIndex &ri,const Loc &rl);
  void has_top(const RoomIndex &ri,const Loc &rl);
  void missing_top(const RoomIndex &ri,const Loc &rl);
  void has_left(const RoomIndex &ri,const Loc &rl);
  void missing_left(const RoomIndex &ri,const Loc &rl);

  void ladder(const Loc &loc);
  /* EFFECTS: Draw a ladder going up and down from loc until a wall is hit. */

  Boolean wall_below(int rTop,int cTop,int num);
  /* EFFECTS: Return whether there is a Wwall or WupDown in the vertical 
     column of num blocks whose top is (rTop,cTop). */

  void up_downs();
  /* EFFECTS: Change all walls on edge of ladders to up_downs. */

  WorldP world;
  Rooms worldRooms;
  Dim worldDim;
  char (* map)[W_COL_MAX_MAX];
  int middleRows[W_DOWN_MAX_MAX];
  int middleCols[W_ACROSS_MAX_MAX];
  Boolean horiz[W_DOWN_MAX_MAX + 1][W_ACROSS_MAX_MAX];
  Boolean vert[W_DOWN_MAX_MAX][W_ACROSS_MAX_MAX + 1];
};



Blueprints::Blueprints(WorldP w,char m[W_ROW_MAX_MAX][W_COL_MAX_MAX])
{
  world = w;
  worldRooms = world->get_rooms();
  worldDim = world->get_dim();
  map = m;

  RoomIndex ri;

  // Boundaries only at the edges of the world.
  for (ri.across = 0; ri.across < worldRooms.acrossMax + 1; ri.across++)
    for (ri.down = 0; ri.down < worldRooms.downMax + 1; ri.down++)
      {
	if (ri.across < worldRooms.acrossMax)
	  {
	    if ((ri.down == 0) || (ri.down == worldRooms.downMax))
	      horiz[ri.down][ri.across] = True;
	    else
	      horiz[ri.down][ri.across] = False;
	  }
	if (ri.down < worldRooms.downMax)
	  {
	    if ((ri.across == 0) || (ri.across == worldRooms.acrossMax))
	      vert[ri.down][ri.across] = True;
	    else
	      vert[ri.down][ri.across] = False;
	  }
      }

  fill_random();


  assert(W_ROOM_ROW_MAX - 2 * (OBJECT_ROW_MAX + 1) > 0);
  assert(W_ROOM_COL_MAX - 2 * (OBJECT_COL_MAX + 1) > 0);

  // Compute middles
  for (int riR = 0; riR < worldRooms.downMax; riR++)
    middleRows[riR] = OBJECT_ROW_MAX + 1 +
      Utils::choose(W_ROOM_ROW_MAX - 2 * (OBJECT_ROW_MAX + 1));

  for (int riC = 0; riC < worldRooms.acrossMax; riC++)
    middleCols[riC] = OBJECT_COL_MAX + 1 +
      Utils::choose(W_ROOM_COL_MAX - 2 * (OBJECT_COL_MAX + 1));
}



int Blueprints::get_middle_row(int down)
{
  assert (0 <= down && down < worldRooms.downMax);
  return middleRows[down];
}



int Blueprints::get_middle_col(int across)
{
  assert (0 <= across && across < worldRooms.acrossMax);
  return middleCols[across];
}



void Blueprints::fill_map()
{
  RoomIndex ri;
  Loc rl;


  // Walls between rooms.
  for (ri.across = 0, rl.c = 0;
       ri.across < worldRooms.acrossMax;
       ri.across++, rl.c += W_ROOM_COL_MAX)
    for (ri.down = 0, rl.r = 0;
	 ri.down < worldRooms.downMax;
	 ri.down++, rl.r += W_ROOM_ROW_MAX)
      {
	if (! horiz[ri.down][ri.across])
	  missing_top(ri,rl);
	else
	  has_top(ri,rl);
	if (! horiz[ri.down + 1][ri.across])
	  missing_bottom(ri,rl);
	else
	  has_bottom(ri,rl);
	if (! vert[ri.down][ri.across])
	  missing_left(ri,rl);
	else
	  has_left(ri,rl);
	if (! vert[ri.down][ri.across + 1])
	  missing_right(ri,rl);
	else
	  has_right(ri,rl);
      }


  // Extra walls.
  for (int walls = 0;
       walls < worldDim.rowMax * worldDim.colMax * WALLS_PERCENT;
       walls ++)
    {
      Boolean ok = True;
      Loc loc;
      loc.r = Utils::choose(worldDim.rowMax);
      loc.c = Utils::choose(worldDim.colMax);
      int delta = Utils::coinFlip() ? 1 : -1;
      int horiz = Utils::choose(WALLS_HORIZ_CHANCE);

      Loc check;
      for (check.c = loc.c - OBJECT_COL_MAX;
	   check.c <= loc.c + OBJECT_COL_MAX;
	   check.c++)
	for (check.r = loc.r - OBJECT_ROW_MAX;
	     check.r <= loc.r + OBJECT_ROW_MAX;
	     check.r++)
	  if (!world->open(check,True))
	    ok = False;

      // Horizontal extra wall.
      if (horiz)
	while (ok && Utils::choose(WALL_LENGTH))
	  {
	    for (check.c = loc.c;
		 check.c != loc.c + delta * (OBJECT_COL_MAX + 1);
		 check.c += delta)
	      for (check.r = loc.r - OBJECT_ROW_MAX;
		   check.r <= loc.r + OBJECT_ROW_MAX;
		   check.r++)
		if (!world->open(check,True))
		  ok = False;

	    if (ok)
	      {
		if (! Utils::choose(LADDER_CHANCE))
		  ladder(loc);
		else
		  map[loc.r][loc.c] = Wwall;

		loc.c += delta;

		if (! Utils::choose(UP_DOWN_CHANCE))
		  loc.r += (Utils::coinFlip() ? 1 : -1);
	      }
	  }
      else  // Vertical wall.
	while (ok && Utils::choose(WALL_LENGTH))
	  {
	    for (check.r = loc.r;
		 check.r != loc.r + delta * (OBJECT_ROW_MAX + 1);
		 check.r += delta)
	      for (check.c = loc.c - OBJECT_COL_MAX;
		   check.c <= loc.c + OBJECT_COL_MAX;
		   check.c++)
		if (!world->open(check,True))
		  ok = False;
	    if (ok)
	      {
		map[loc.r][loc.c] = Wwall;
		loc.r += delta;
	      }
	  }
    }


  up_downs();
}



void Blueprints::print()
{
  RoomIndex ri;

  for (ri.down = 0; ri.down <= worldRooms.downMax; ri.down++)
    {
      // Horizontal boundaries.
      for (ri.across = 0; ri.across < worldRooms.acrossMax; ri.across++)
	cout << ((horiz[ri.down][ri.across]) ? "**" : "*-");
      cout << "*" << endl;

      // Vertical boundaries.
      if (ri.down < worldRooms.downMax)
	for (ri.across = 0; ri.across <= worldRooms.acrossMax; ri.across++)
	  cout << ((vert[ri.down][ri.across]) ? "* " : "| ");

      cout << endl;
    }
}



Boolean Blueprints::blocked_right(const RoomIndex &ri)
{
  assert(ri.down < worldRooms.downMax && ri.down >= 0);
  assert(ri.across < worldRooms.acrossMax && ri.across >= 0);

  return vert[ri.down][ri.across + 1];
}



Boolean Blueprints::blocked_bottom(const RoomIndex &ri)
{
  assert(ri.down < worldRooms.downMax && ri.down >= 0);
  assert(ri.across < worldRooms.acrossMax && ri.across >= 0);

  return horiz[ri.down + 1][ri.across];
}



Boolean Blueprints::blocked_left(const RoomIndex &ri)
{
  assert(ri.down < worldRooms.downMax && ri.down >= 0);
  assert(ri.across < worldRooms.acrossMax && ri.across >= 0);

  return vert[ri.down][ri.across];
}



Boolean Blueprints::blocked_top(const RoomIndex &ri)
{
  assert(ri.down < worldRooms.downMax && ri.down >= 0);
  assert(ri.across < worldRooms.acrossMax && ri.across >= 0);

  return horiz[ri.down][ri.across];
}



void Blueprints::fill_random()
{
  /* Don't do maze algorithm if the world is too small.  */
  if ((worldRooms.acrossMax >= 2) && (worldRooms.downMax >= 2))
    for (int n = 0; n < worldRooms.acrossMax * worldRooms.downMax * STRAND_PERCENT; n++)
      {
	RoomIndex ri;
	ri.across = Utils::choose(worldRooms.acrossMax - 1) + 1;
	ri.down = Utils::choose(worldRooms.downMax - 1) + 1;

	Boolean ok = open(ri);
	while (Utils::choose(STRAND_LENGTH) && ok)
	  { // open implies that ri must not be on outer boundaries.
	      if (Utils::coinFlip())
		{
		  if (Utils::coinFlip())
		    {
		      horiz[ri.down][ri.across] = True;
		      ri.across++;
		      ok = open(ri);
		    }
		  else
		    {
		      horiz[ri.down][ri.across - 1] = True;
		      ri.across--;
		      ok = open(ri);
		    }
		}
	      else
		{
		  if (Utils::coinFlip())
		    {
		      vert[ri.down][ri.across] = True;
		      ri.down++;
		      ok = open(ri);
		    }
		  else
		    {
		      vert[ri.down - 1][ri.across] = True;
		      ri.down--;
		      ok = open(ri);
		    }
		}
	  }
      }
}



Boolean Blueprints::open(const RoomIndex &ri)
{
  assert((ri.across >= 0) && (ri.across <= worldRooms.acrossMax) &&
	 (ri.down >= 0) && (ri.down <= worldRooms.downMax));

  if ((ri.across > 0) && horiz[ri.down][ri.across - 1])
    return False;

  if ((ri.across < worldRooms.acrossMax) && horiz[ri.down][ri.across])
    return False;

  if ((ri.down > 0) && vert[ri.down - 1][ri.across])
    return False;

  if ((ri.down < worldRooms.downMax) && vert[ri.down][ri.across])
    return False;

  return True;
}



void Blueprints::has_top(const RoomIndex &,const Loc &roomLoc)
{
  for (int c = roomLoc.c; c < roomLoc.c + W_ROOM_COL_MAX; c++)
    map[roomLoc.r][c] = Wwall;
}



void Blueprints::missing_top(const RoomIndex &roomIndex,
				const Loc &roomLoc)
{
  for (int c = roomLoc.c + middleCols[roomIndex.across];
       c < roomLoc.c + middleCols[roomIndex.across] + OBJECT_COL_MAX;
       c++)
    for (int r = roomLoc.r; r < roomLoc.r + W_ROOM_ROW_MAX - 1; r++)
      map[r][c] = Wladder;
}



void Blueprints::has_bottom(const RoomIndex &,
			    const Loc &roomLoc)
{
  for (int c = roomLoc.c; c < roomLoc.c + W_ROOM_COL_MAX; c++)
    map[roomLoc.r + W_ROOM_ROW_MAX - 1][c] = Wwall;
}



void Blueprints::missing_bottom(const RoomIndex &roomIndex,
				const Loc &roomLoc)
{
  for (int c = roomLoc.c + middleCols[roomIndex.across];
       c < roomLoc.c + middleCols[roomIndex.across] + OBJECT_COL_MAX;
       c++)
    for (int r = roomLoc.r + middleRows[roomIndex.down] - OBJECT_ROW_MAX;
	 r < roomLoc.r + W_ROOM_ROW_MAX;
	 r++)
      map[r][c] = Wladder;
}



void Blueprints::has_left(const RoomIndex &,
			  const Loc &roomLoc)
{
  for (int r = roomLoc.r; r < roomLoc.r + W_ROOM_ROW_MAX; r++)
    map[r][roomLoc.c] = Wwall;
}



void Blueprints::missing_left(const RoomIndex &roomIndex,
			       const Loc &roomLoc)
{
  for (int c = roomLoc.c; c < roomLoc.c + middleCols[roomIndex.across]; c++)
    map[roomLoc.r + middleRows[roomIndex.down]][c] = Wwall;
}



void Blueprints::has_right(const RoomIndex &,
			   const Loc &roomLoc)
{
  for (int r = roomLoc.r; r < roomLoc.r + W_ROOM_ROW_MAX; r++)
    map[r][roomLoc.c + W_ROOM_COL_MAX - 1] = Wwall;
}



void Blueprints::missing_right(const RoomIndex &roomIndex,
			       const Loc &roomLoc)
{
  for (int c = roomLoc.c + middleCols[roomIndex.across] + OBJECT_COL_MAX;
       c < roomLoc.c + W_ROOM_COL_MAX; c++)
    map[roomLoc.r + middleRows[roomIndex.down]][c] = Wwall;
}



void Blueprints::ladder(const Loc &init)
{
  // delta is only -1 and 1.
  for (int delta = -1; delta <= 1; delta += 2)
    {
      Loc loc = init;

      // To avoid hitting init twice.
      if (delta == 1)
	loc.r++;

      while (world->open(loc,True)) // Running directly into wall or ladder.
	{
	  map[loc.r][loc.c] = Wladder;

	  // Stop when there is a wall to the left or right.
	  Loc left,right;
	  right.r = left.r = loc.r;
	  left.c = loc.c - 1;
	  right.c = loc.c + 1;
	  if ((!world->open(left) || !world->open(right)))
	    {
	      // Extra extension at top.
	      if (delta == -1)
		for (int extra = 1; extra <= OBJECT_ROW_MAX; extra++)
		  {
		    Loc extraLoc;
		    extraLoc.c = loc.c;
		    extraLoc.r = loc.r - extra;
		    if (world->open(extraLoc,True))
		      map[extraLoc.r][extraLoc.c] = Wladder;
		    else
		      break;
		  }
	      break;
	    }

	  loc.r += delta;
	}
    }
}



Boolean Blueprints::wall_below(int rTop,int cTop,int num)
{
  Loc loc;
  loc.c = cTop;

  for (loc.r = rTop; loc.r < rTop + num; loc.r++)
    if (world->inside(loc) && 
	(map[loc.r][loc.c] == Wwall || map[loc.r][loc.c] == WupDown))
      return True;
  
  return False;
}



void Blueprints::up_downs()
{
  Loc loc;

  // Change Wwall to WupDown if a ladder is immediately to left or right.
  for (loc.c = 0; loc.c < worldDim.colMax; loc.c++)
    for (loc.r = 0; loc.r < worldDim.rowMax; loc.r++)
      if ((map[loc.r][loc.c] == Wwall) &&
	  (((loc.c + 1 < worldDim.colMax) &&
	    (map[loc.r][loc.c + 1] == Wladder)) ||
	   ((loc.c - 1 >= 0) &&
	    (map[loc.r][loc.c - 1] == Wladder))))
	map[loc.r][loc.c] = WupDown;


  // Change Wwall to WupDown if doesn't have a wall to right AND left.
  for (loc.c = 0; loc.c < worldDim.colMax; loc.c++)
    for (loc.r = 0; loc.r < worldDim.rowMax; loc.r++)
      if (map[loc.r][loc.c] == Wwall &&
	  !((wall_below(loc.r - 1,loc.c - 1,3) &&
	     wall_below(loc.r - 1,loc.c + 1,3)) ||
	    wall_below(loc.r - 1,loc.c,1) ||
	    wall_below(loc.r + 1,loc.c,1)))
	map[loc.r][loc.c] = WupDown;
}




// Functions for World class.
World::World(Boolean pol_correct)
{
  mapPrint = MAP_PRINT_DEFAULT;
  rooms.acrossMax = 1;
  rooms.downMax = 1;
  roomsNext = rooms;
  dim_size_update();
  blueprints = NULL;

  xValid = False;
  Loc loc;
  for (loc.r = 0; loc.r < W_ROW_MAX_MAX; loc.r++)
    for (loc.c = 0; loc.c < W_COL_MAX_MAX; loc.c++)
      unionSquares[loc.r][loc.c] = NULL;

  // Initialize posterDims
  for (int n = 0; n < W_POSTERS_NUM; n++)
    {
      assert(posterSizes[n].width % WSQUARE_WIDTH == 0 &&
	     posterSizes[n].height % WSQUARE_HEIGHT == 0);
      posterDims[n].colMax = posterSizes[n].width / WSQUARE_WIDTH;
      posterDims[n].rowMax = posterSizes[n].height / WSQUARE_HEIGHT;
    }

  polCorrect = pol_correct;
  title_map();
}



Dim World::get_room_dim()
{
  Dim ret(W_ROOM_ROW_MAX,W_ROOM_COL_MAX);
  return ret;
}



Size World::get_room_size()
{
  Size ret;
  ret.width = W_ROOM_COL_MAX * WSQUARE_WIDTH;
  ret.height = W_ROOM_ROW_MAX * WSQUARE_HEIGHT;
  return ret;
}



Boolean World::check_door(const Loc &loc,Loc &dest)
{
  if (unionSquares[loc.r][loc.c] &&
      unionSquares[loc.r][loc.c]->type == UN_DOOR)
    {
      dest = unionSquares[loc.r][loc.c]->dSquare.dest;
      return True;
    }
  return False;
}



void World::set_rooms_next(const Rooms &r)
{
  roomsNext.acrossMax = min(W_ACROSS_MAX_MAX,r.acrossMax);
  roomsNext.downMax = min(W_DOWN_MAX_MAX,r.downMax);
}



Boolean World::overlap(const Box &box)
{
  return ((box.loc.c < dim.colMax) &&
	  (box.loc.c + box.dim.colMax > 0) &&
	  (box.loc.r < dim.rowMax) &&
	  (box.loc.r + box.dim.rowMax > 0));
}



void World::draw(Drawable buffer,Xvars &xvars,int dpyNum,const Box &box)
{
  if (!xValid)
    init_x(xvars);

  Loc l, buffer_l, finish;
  finish.r = box.loc.r + box.dim.rowMax;
  finish.c = box.loc.c + box.dim.colMax;

  for (l.r = box.loc.r, buffer_l.r = 0; l.r < finish.r; buffer_l.r++, l.r++)
    for (l.c = box.loc.c, buffer_l.c = 0; l.c < finish.c; buffer_l.c++, l.c++)
      draw_square(buffer,xvars,dpyNum,l,
		  buffer_l.c * WSQUARE_WIDTH,buffer_l.r * WSQUARE_HEIGHT);
}



void World::draw(Drawable buffer,Xvars &xvars,int dpyNum,const Area &area)
{
  if (!xValid)
    init_x(xvars);

  Pos aPos;
  Size aSize;
  area.get_rect(aPos,aSize);

  Loc loc;
  for (loc.c = aPos.x / WSQUARE_WIDTH;
       loc.c < ceil((float)(aPos.x + aSize.width) / (float)WSQUARE_WIDTH);
       loc.c++)
    for (loc.r = aPos.y / WSQUARE_HEIGHT;
	 loc.r < ceil((float)(aPos.y + aSize.height)/(float)WSQUARE_HEIGHT);
	 loc.r++)
      draw_square(buffer,xvars,dpyNum,loc,
		  loc.c * WSQUARE_WIDTH - aPos.x,
		  loc.r * WSQUARE_HEIGHT - aPos.y);
}



Boolean World::open(const Loc &loc,Boolean laddersClosed,Boolean postersClosed,
		    Boolean doorsClosed)
{
  if (! inside(loc))
    return False;
  else
    return (
	    // map contains an open square at loc
	    (map[loc.r][loc.c] == Wempty ||
	     map[loc.r][loc.c] == WtextSquare || 
	     map[loc.r][loc.c] == Wsquanch ||
	     (map[loc.r][loc.c] == Wladder && !laddersClosed)) &&
	    
	    // a poster isn't closed at loc
	    (!postersClosed || 
	     !(unionSquares[loc.r][loc.c] && 
	       unionSquares[loc.r][loc.c]->type == UN_POSTER)) &&

	    // a door isn't "closed" at loc
	    (!doorsClosed ||
	     !(unionSquares[loc.r][loc.c] &&
	       unionSquares[loc.r][loc.c]->type == UN_DOOR)));
}



Boolean World::open(const Area &area,Boolean laddersClosed,
		    Boolean postersClosed,Boolean doorsClosed)
{
  /* Avoid a certain type of crash when area is far out of the world.
     I.e. when area.pos is near overflow. */
  if (!inside(area.middle_wsquare()))
    return False;

  Loc list[AR_WSQUARES_MAX];
  int nsquares;
  area.wsquares(list,nsquares);
  for (int n = 0; n < nsquares; n++)
    if (! open(list[n],laddersClosed,postersClosed,doorsClosed))
      return False;
  
  return True;
}



Boolean World::open(const Box &box,Boolean laddersClosed,Boolean postersClosed,
		    Boolean doorsClosed)
{
  Loc loc;
  for (loc.c = box.loc.c; loc.c < box.loc.c + box.dim.colMax; loc.c++)
    for (loc.r = box.loc.r; loc.r < box.loc.r + box.dim.rowMax; loc.r++)
      if (!open(loc,laddersClosed,postersClosed,doorsClosed))
	return False;
  return True;
}



Wsig World::open_offset(Size &offset,const Area &areaNew,
			const Vel &vel,Boolean emptyOnly)
{
  if (open(areaNew,emptyOnly))
    return W_NO_SIG;

  Vel opp = -1 * vel;

  Dir best[4];
  Dir others[4];
  int bestNum,othersNum;
  opp.get_dirs_4(best,others,bestNum,othersNum);

  if (open_try_dirs(offset,areaNew,best,bestNum,emptyOnly))
    return W_CLOSE;

  if (open_try_dirs(offset,areaNew,others,othersNum,emptyOnly))
    return W_CLOSE;

  if (open_try_diagonals(offset,areaNew,emptyOnly))
    return W_CLOSE_BAD;

#ifdef PRINT_ERRORS
  cerr << "open_offset fails." << endl;
#endif

  return W_FAILURE;
}



void World::touchingHanging(Touching &touching, Hanging &hanging,
			    const Area &area)
{
  Pos apos;
  Size asize;
  area.get_rect(apos,asize);

  touching = CO_air;
  hanging.corner = CO_air;

  TouchingList tList;
  area.touching_wsquares(tList);

  th_helper(touching,hanging,tList.r,True,apos.y,asize.height,
	    WSQUARE_HEIGHT,CO_r,CO_r_UP,CO_r_DN);
  th_helper(touching,hanging,tList.l,True,apos.y,asize.height,
	    WSQUARE_HEIGHT,CO_l,CO_l_UP,CO_l_DN);
  th_helper(touching,hanging,tList.dn,False,apos.x,asize.width,
	    WSQUARE_WIDTH,CO_dn,CO_dn_L,CO_dn_R);
  th_helper(touching,hanging,tList.up,False,apos.x,asize.width,
	    WSQUARE_WIDTH,CO_up,CO_up_L,CO_up_R);
}



void World::reset()
{
  // Delete old unionSquares.
  Loc loc;
  for (loc.r = 0; loc.r < dim.rowMax; loc.r++)
    for (loc.c = 0; loc.c < dim.colMax; loc.c++)
      if (unionSquares[loc.r][loc.c])
	{
	  delete unionSquares[loc.r][loc.c];
	  unionSquares[loc.r][loc.c] = NULL;
	}
  
  // Change rooms,dim,size of world.
  rooms = roomsNext;
  dim_size_update();

  // Clear map.
  for (loc.r = 0; loc.r < dim.rowMax; loc.r++)
    for (loc.c = 0; loc.c < dim.colMax; loc.c++)
      map[loc.r][loc.c] = Wempty;


  // Generate new map.
  if (blueprints)
    delete blueprints;
  blueprints = new Blueprints(this,map);
  assert(blueprints);
  if (mapPrint)
    blueprints->print();
  blueprints->fill_map();

  add_doors();

  add_posters();
}



Pos World::empty_rect(const Size &s)
{
  int count = 0; // Guard against infinite loop.

  while (True)
    {
      assert (count < EMPTY_TRIES_MAX);
      count++;

      Pos ret;
      ret.x = Utils::choose(size.width);
      ret.y = Utils::choose(size.height);

      Area area(AR_RECT,ret,s);
      if (open(area,True,False,True))
	return ret;
    }
}



Pos World::empty_touching_rect(const Size &s)
{
  int count = 0; // Guard against infinite loop.

  while (True)
    {
      assert (count < EMPTY_TRIES_MAX);
      count++;

      Pos ret;
      ret.x = Utils::choose(size.width);
      ret.y = WSQUARE_HEIGHT * Utils::choose(dim.rowMax) - s.height;

      Area area(AR_RECT,ret,s);
      Touching touching;
      Hanging hanging;
      touchingHanging(touching,hanging,area);
      if (open(area,True,False,True) && (touching == CO_dn))
	return ret;
    }
}



Pos World::empty_accessible_rect(const Size &s)
{
  Blueprints *bl = blueprints;
  assert(bl);
  int count = 0; // Guard against infinite loop.

  while (True)
    {
      assert (count < EMPTY_TRIES_MAX);
      count++;

      // Choose the room.
      RoomIndex r(Utils::choose(rooms.downMax),
		  Utils::choose(rooms.acrossMax));

      Pos ret;
      // Start at right edge of room.
      ret.x = r.across * W_ROOM_COL_MAX * WSQUARE_WIDTH;

      // Choose x coord so that on right or left side of ladder, whereever 
      // there is a floor.
      if (bl->blocked_right(r))
	{ 
	  if (bl->blocked_left(r)) 
	    // Bail out, no suitable rect.
	    continue;
	  else
	    // Left half of room.
	    ret.x += Utils::choose(WSQUARE_WIDTH * 
				   bl->get_middle_col(r.across));
	}
      else
	{
	  if (bl->blocked_left(r)) 
	    // Right half of room.
	    ret.x += 
	      WSQUARE_WIDTH *
		(bl->get_middle_col(r.across) + OBJECT_COL_MAX) +
		  Utils::choose(WSQUARE_WIDTH * 
				(W_ROOM_COL_MAX - 
				 bl->get_middle_col(r.across) -
				 OBJECT_COL_MAX));
	  else
	    {
	      if (Utils::coinFlip())
		// Left half of room.
		ret.x += Utils::choose(WSQUARE_WIDTH * 
				       bl->get_middle_col(r.across));
	      else
		// Right half of room.
		ret.x += 
		  WSQUARE_WIDTH *
		    (bl->get_middle_col(r.across) + OBJECT_COL_MAX) +
		      Utils::choose(WSQUARE_WIDTH * 
				    (W_ROOM_COL_MAX - 
				     bl->get_middle_col(r.across) -
				     OBJECT_COL_MAX));
	    }
	}
      
      // y coord so that area is just touching middle floor.
      ret.y = (r.down * W_ROOM_ROW_MAX + bl->get_middle_row(r.down))
	* WSQUARE_HEIGHT - s.height;
      
      Area area(AR_RECT,ret,s);
      if (open(area,True,False,True))
	return ret;

    } // while
}



Boolean World::empty_box(Loc &loc,const Dim &d,Boolean laddersClosed,
			 Boolean postersClosed,Boolean doorsClosed)
{
  int count = 0; // Guard against infinite loop.

  while (True)
    {
      if (count >= EMPTY_TRIES_MAX)
	return False;
      count++;

      loc.c = Utils::choose(dim.colMax);
      loc.r = Utils::choose(dim.rowMax);

      Box box(loc,d);
      if (open(box,laddersClosed,postersClosed,doorsClosed))
	return True;
    }
}



Boolean World::empty_touching_box(Loc &loc,const Dim &d,
				  Boolean laddersClosed,
				  Boolean postersClosed,
				  Boolean doorsClosed)
{
  int count = 0; // Guard against infinite loop.
  
  while (True)
    {
      if (count >= EMPTY_TRIES_MAX)
	return False;
      count++;

      loc.c = Utils::choose(dim.colMax);
      loc.r = Utils::choose(dim.rowMax);

      Area area(AR_RECT,loc,d);
      Touching touching;
      Hanging hanging;
      touchingHanging(touching,hanging,area);
      if (open(area,laddersClosed,postersClosed,doorsClosed) &&
	  touching == CO_dn)
	return True;
    }
}



void World::draw_square(Drawable buffer,Xvars &xvars,int dpyNum,const Loc &l,
			  int x,int y)
{
  if (inside(l))
    {
      if (unionSquares[l.r][l.c])
	{
	  assert(map[l.r][l.c] == Wempty);

	  // Draw poster.
	  if (unionSquares[l.r][l.c]->type == UN_POSTER)
	    {
	      PosterSquare *pSquare = &unionSquares[l.r][l.c]->pSquare;
	      Pixmap pix = xdata.posterPixmaps[dpyNum][pSquare->poster]
		[posterDims[pSquare->poster].colMax * pSquare->loc.r +
		 pSquare->loc.c];
	      assert(pix);
	      XCopyArea(xvars.dpy[dpyNum],pix,
			buffer,xvars.gc[dpyNum],0,0,
			WSQUARE_WIDTH,WSQUARE_HEIGHT,
			x,y);
	    }
	  // Draw door.
	  else if (unionSquares[l.r][l.c]->type == UN_DOOR)
	    {
	      int topBottom = unionSquares[l.r][l.c]->dSquare.topBottom;
	      XCopyArea(xvars.dpy[dpyNum],
			xdata.doorPixmaps[dpyNum][topBottom],
			buffer,xvars.gc[dpyNum],0,0,
			WSQUARE_WIDTH,WSQUARE_HEIGHT,
			x,y);
	    }
	  else
	    assert(0);
	}
      else
	// Draw regular square.
	XCopyArea(xvars.dpy[dpyNum],
		  xdata.pixmaps[dpyNum][map[l.r][l.c]],
		  buffer,xvars.gc[dpyNum],0,0,
		  WSQUARE_WIDTH,WSQUARE_HEIGHT,
		  x,y);
    }
  else
    // If outside world.
    XCopyArea(xvars.dpy[dpyNum],
	      xdata.pixmaps[dpyNum][Woutside],
	      buffer,xvars.gc[dpyNum],0,0,
	      WSQUARE_WIDTH,WSQUARE_HEIGHT,
	      x,y);
}



void World::dim_size_update()
{
  dim.colMax = rooms.acrossMax * W_ROOM_COL_MAX;
  dim.rowMax = rooms.downMax * W_ROOM_ROW_MAX;
  size.width = dim.colMax * WSQUARE_WIDTH;
  size.height = dim.rowMax * WSQUARE_HEIGHT;
}



void World::init_x(Xvars &xvars)
{
  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    {
      xdata.background[dpyNum] = 
	xvars.allocNamedColor(dpyNum,Xvars_BACKGROUND);
      
      // Regular wsquare pixmaps.
      int n;
      for (n = 0; n < W_BLOCKS_NUM; n++)
	xdata.pixmaps[dpyNum][n] =
	  XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				      pixmapBits[n],
				      empty_width,empty_height,
				      xvars.allocNamedColor(dpyNum,
							    colorNames[n],
							    xvars.black[dpyNum]),
				      xdata.background[dpyNum],
				      xvars.depth[dpyNum]);

      // Create posterPixmaps.
      for (n = 0; n < W_POSTERS_NUM; n++)
	{
	  Pixmap tempPixmap =
	    XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
					posterPixmapBits[n],
					posterSizes[n].width,
					posterSizes[n].height,
					xvars.allocNamedColor(dpyNum,
							      posterForegrounds[n],
							      xvars.black[dpyNum]),
					xvars.allocNamedColor(dpyNum,
							      posterBackgrounds[n],
							      xvars.white[dpyNum]),
					xvars.depth[dpyNum]);
	  assert(tempPixmap);
	  Pixmap tempMask =
	    XCreateBitmapFromData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				  posterMaskBits[n],
				  posterSizes[n].width,
				  posterSizes[n].height);
	  assert(tempMask);

	  xdata.posterPixmaps[dpyNum][n] =
	    new Pixmap [posterDims[n].rowMax * posterDims[n].colMax];
	  assert(xdata.posterPixmaps[dpyNum][n]);

	  // Create a separate pixmap for every wsquare in the poster.
	  Loc l;
	  for (l.r = 0; l.r < posterDims[n].rowMax; l.r++)
	    for (l.c = 0; l.c < posterDims[n].colMax; l.c++)
	      {
		xdata.posterPixmaps[dpyNum][n]
		  [posterDims[n].colMax * l.r + l.c] =
		    XCreatePixmap(xvars.dpy[dpyNum],xvars.root[dpyNum],
				  WSQUARE_WIDTH,WSQUARE_HEIGHT,
				  xvars.depth[dpyNum]);
		assert(xdata.posterPixmaps[dpyNum][n]
		       [posterDims[n].colMax * l.r + l.c]);


		// Draw background.
		XCopyArea(xvars.dpy[dpyNum],xdata.pixmaps[dpyNum][Wempty],
			  xdata.posterPixmaps[dpyNum][n]
			  [posterDims[n].colMax * l.r + l.c],
			  xvars.gc[dpyNum],0,0,
			  WSQUARE_WIDTH,WSQUARE_HEIGHT,0,0);
		
		XSetClipMask(xvars.dpy[dpyNum],xvars.gc[dpyNum],tempMask);
		XSetClipOrigin(xvars.dpy[dpyNum],xvars.gc[dpyNum],
			       -l.c * WSQUARE_WIDTH,-l.r * WSQUARE_HEIGHT);
		XCopyArea(xvars.dpy[dpyNum],tempPixmap,
			  xdata.posterPixmaps[dpyNum][n]
			  [posterDims[n].colMax * l.r + l.c],
			  xvars.gc[dpyNum],
			  l.c * WSQUARE_WIDTH,l.r * WSQUARE_HEIGHT,
			  WSQUARE_WIDTH,WSQUARE_HEIGHT,0,0);
		XSetClipMask(xvars.dpy[dpyNum],xvars.gc[dpyNum],None);
		XSetClipOrigin(xvars.dpy[dpyNum],xvars.gc[dpyNum],0,0);
	      }
	  XFreePixmap(xvars.dpy[dpyNum],tempPixmap);
	  XFreePixmap(xvars.dpy[dpyNum],tempMask);
	}

      // Create doorPixmaps.
      for (n = 0; n < 2; n++)
	xdata.doorPixmaps[dpyNum][n] = 
	  XCreatePixmapFromBitmapData(xvars.dpy[dpyNum],xvars.root[dpyNum],
				      doorPixmapBits[n],
				      WSQUARE_WIDTH,WSQUARE_HEIGHT,
				      xvars.
				      allocNamedColor(dpyNum,
						      W_DOOR_FOREGROUND,
						      xvars.black[dpyNum]),
				      xvars.
				      allocNamedColor(dpyNum,
						      W_DOOR_BACKGROUND),
				      xvars.depth[dpyNum]);
    }
  xValid = True;
}



void World::title_map()
{
  Loc titleOrigin;
  titleOrigin.c = (W_ROOM_COL_MAX - W_TITLE_COL_MAX) / 2;
  titleOrigin.r = (W_ROOM_ROW_MAX - W_TITLE_ROW_MAX) / 2;
  Dim titleDim(W_TITLE_ROW_MAX,W_TITLE_COL_MAX);
  Box title(titleOrigin,titleDim);

  Loc loc;
  for (loc.r = 0; loc.r < dim.rowMax; loc.r++)
    for (loc.c = 0; loc.c < dim.colMax; loc.c++)
      if (title.overlap(loc))
	map[loc.r][loc.c] =
	  world_title[loc.r - titleOrigin.r][loc.c - titleOrigin.c];
      else
	map[loc.r][loc.c] = Woutside;
}



void World::th_helper(Touching &touching,Hanging &hanging,
		      const TouchingListItem &item,
		      Boolean r_c, // row/column
		      int coord,int length,
		      int wsquare_length,
		      Touching iftouching,
		      Corner ifsmall, Corner iflarge)
{
  Boolean touchingOk = False;

  for (int n = 0; n < item.num; n++)
    if (! open(item.list[n]))
      {
	touching = iftouching;
	touchingOk = True;
      }

  if (touchingOk) // Only compute hanging if touching.
    {
      int hangCutoff = (int)floor(HANGING_PERCENT * length);

      int n;
      for (n = 0; n < item.num; n++)
	if (! open(item.list[n]))
	  break;

      // item.list[n] is first blocked wsquare.
      if ((n < item.num) &&
	  ((r_c ? item.list[n].r:item.list[n].c) * wsquare_length - coord
	   >= hangCutoff))
	{
	  hanging.corner = ifsmall;
	  hanging.loc = item.list[n];
	}

      for (n = item.num - 1; n >= 0; n--)
	if (! open(item.list[n]))
	  break;

      if ((n >= 0) &&
	  (coord + length -
	   (r_c ? item.list[n].r + 1:item.list[n].c + 1) * wsquare_length
	   >= hangCutoff))
	{
	  hanging.corner = iflarge;
	  hanging.loc = item.list[n];
	}
    }
}



Boolean World::open_iter(Area &area,int &changed,Dir dir,Boolean emptyOnly)
{
  int offOne = 0;

  Loc list[AR_WSQUARES_MAX];
  int nsquares;
  area.wsquares(list,nsquares);
  for (int n = 0; n < nsquares; n++)
    {
      // Support for up_downs.
      if (inside(list[n]) && (map[list[n].r][list[n].c] == WupDown) &&
	  ((dir == CO_R) || (dir == CO_L)))
	return False;

      if (! open(list[n],emptyOnly))
	{
	  int temp = area.avoid_wsquare_dir(list[n],dir);
	  offOne = max(offOne,temp);
	}
  }
  if (offOne == 0)
    return True;

  changed += offOne;
  area.shift(open_size(offOne,dir));
  return False;
}



Size World::open_size(int offset,Dir dir)
{
  Size ret;
  ret.width = ret.height = 0;

  switch (dir) {
  case CO_R:
    ret.width = offset;
    break;
  case CO_DN:
    ret.height = offset;
    break;
  case CO_L:
    ret.width = -offset;
    break;
  case CO_UP:
    ret.height = -offset;
    break;
  };

  return ret;
}



Size World::open_size(int offset1,Dir dir1,int offset2,Dir dir2)
{
  Size ret;
  ret.width = ret.height = 0;

  switch (dir1) {
  case CO_R:
    ret.width = offset1;
    break;
  case CO_DN:
    ret.height = offset1;
    break;
  case CO_L:
    ret.width = -offset1;
    break;
  case CO_UP:
    ret.height = -offset1;
    break;
  };

  switch (dir2) {
  case CO_R:
    ret.width = offset2;
    break;
  case CO_DN:
    ret.height = offset2;
    break;
  case CO_L:
    ret.width = -offset2;
    break;
  case CO_UP:
    ret.height = -offset2;
    break;
  };

  return ret;
}



Boolean World::open_try_dirs(Size &offset,const Area &area,const Dir dirs[4],
			     int dirsNum,Boolean emptyOnly)
{
  if (!dirsNum)
    return False;

  Area areas[4];
  int offsets[4];
  for (int n = 0; n < dirsNum; n++)
    {
      areas[n] = area;
      offsets[n] = 0;
    }

  for (int iter = 0; iter < OPEN_ITERATIONS; iter++)
    {
      Boolean oks[4];
      int n;
      for (n = 0; n < dirsNum; n++)
	oks[n] = open_iter(areas[n],offsets[n],dirs[n],emptyOnly);

      int offsetOkMin = Utils::minimum(offsets,oks,dirsNum);

      for (n = 0; n < dirsNum; n++)
	if (oks[n] && (offsets[n] == offsetOkMin))
	  {
	    offset = open_size(offsets[n],dirs[n]);
	    return True;
	  }
    }

  return False;
}



Boolean World::open_try_diagonals(Size &offset,const Area &area,
				 Boolean emptyOnly)
{
  Dir dirs[4];
  dirs[0] = CO_R;
  dirs[1] = CO_DN;
  dirs[2] = CO_L;
  dirs[3] = CO_UP;

  int offsets[4];
  int n;
  for (n = 0; n < 4; n++)
    offsets[n] = 0;

  Loc list[AR_WSQUARES_MAX];
  int nsquares;
  area.wsquares(list,nsquares);
  for (n = 0; n < nsquares; n++)
    if (! open(list[n],emptyOnly))
      {
	for (int dIndex = 0; dIndex < 4; dIndex++)
	  {
	    int dOffset = area.avoid_wsquare_dir(list[n],dirs[dIndex]);
	    offsets[dIndex] = max(dOffset,offsets[dIndex]);
	  }
      }

  for (int dIndex = 0; dIndex < 4; dIndex++)
    {
      Area areaTest = area;
      offset = open_size(offsets[dIndex],dirs[dIndex],
			 offsets[(dIndex + 1) % 4],dirs[(dIndex + 1) % 4]);
      areaTest.shift(offset);
      if (open(areaTest,emptyOnly))
	return True;
    }

  return False;
}



void World::add_posters()
{
  // Put in posters.
  if (!polCorrect)
    {
      for (int n = 0; 
	   n < POSTERS_ACTUAL_PERCENT * dim.rowMax * dim.colMax; 
	   n++)
	{
	  int p = Utils::choose(W_POSTERS_NUM);
	  Loc pLoc;
	  if (!empty_box(pLoc,posterDims[p],True,True,True))
	    break;
	  
	  Loc loc;
	  for (loc.c = pLoc.c; loc.c < pLoc.c + posterDims[p].colMax; loc.c++)
	    for (loc.r = pLoc.r; loc.r < pLoc.r + posterDims[p].rowMax; loc.r++)
	      {
		if (! unionSquares[loc.r][loc.c])
		  unionSquares[loc.r][loc.c] = new UnionSquare;
		assert(unionSquares[loc.r][loc.c]);
		unionSquares[loc.r][loc.c]->type = UN_POSTER;
		unionSquares[loc.r][loc.c]->pSquare.poster = p;
		unionSquares[loc.r][loc.c]->pSquare.loc.c = loc.c - pLoc.c;
		unionSquares[loc.r][loc.c]->pSquare.loc.r = loc.r - pLoc.r;
	      }
	}
    }
}



void World::add_doors()
{
  // Put in doors.
  for (int n = 0; 
       n < DOORS_ACTUAL_PERCENT * rooms.downMax * rooms.acrossMax;
       n++) 
    {
      Loc loc1;
      Loc loc2;
      Dim doorDim(2,1);  // Hardwired size of doors.
      if (!empty_touching_box(loc1,doorDim,True,True,True))
	break;
      if (!empty_touching_box(loc2,doorDim,True,True,True))
	break;

      // Don't want doors on top of each other.
      Area area1(AR_RECT,loc1,doorDim);
      Area area2(AR_RECT,loc2,doorDim);
      if (area1.overlap(area2))
	break;
      
      // Lower half of doors.
      Loc loc1d = loc1.move(1,0);
      Loc loc2d = loc2.move(1,0);

      // We actually create 4 doors, two pairs.  Each apparent door is
      // actually two DoorSquares.

      // Cross connect two new doors at loc1 and loc2.
      unionSquares[loc1.r][loc1.c] = new UnionSquare;
      assert(unionSquares[loc1.r][loc1.c]);
      unionSquares[loc1.r][loc1.c]->type = UN_DOOR;
      unionSquares[loc1.r][loc1.c]->dSquare.dest = loc2;
      unionSquares[loc1.r][loc1.c]->dSquare.topBottom = W_DOOR_TOP;

      unionSquares[loc2.r][loc2.c] = new UnionSquare;
      assert(unionSquares[loc2.r][loc2.c]);
      unionSquares[loc2.r][loc2.c]->type = UN_DOOR;
      unionSquares[loc2.r][loc2.c]->dSquare.dest = loc1;
      unionSquares[loc2.r][loc2.c]->dSquare.topBottom = W_DOOR_TOP;
      
      // Cross connect two new doors at loc1d and loc2d.
      unionSquares[loc1d.r][loc1d.c] = new UnionSquare;
      assert(unionSquares[loc1d.r][loc1d.c]);
      unionSquares[loc1d.r][loc1d.c]->type = UN_DOOR;
      unionSquares[loc1d.r][loc1d.c]->dSquare.dest = loc2d;
      unionSquares[loc1d.r][loc1d.c]->dSquare.topBottom = W_DOOR_BOTTOM;

      unionSquares[loc2d.r][loc2d.c] = new UnionSquare;
      assert(unionSquares[loc2d.r][loc2d.c]);
      unionSquares[loc2d.r][loc2d.c]->type = UN_DOOR;
      unionSquares[loc2d.r][loc2d.c]->dSquare.dest = loc1d;
      unionSquares[loc2d.r][loc2d.c]->dSquare.topBottom = W_DOOR_BOTTOM;
    }
}
