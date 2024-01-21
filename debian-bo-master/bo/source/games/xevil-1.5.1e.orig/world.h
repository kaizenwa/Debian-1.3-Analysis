// "world.h"

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

/* Overview:  A world object is a rectangular region composed of WSQUARES.
   Internally, it is divided into rooms of constant size.  It's upper left 
   corner is at WSQUARE (0,0). 
   World should be draw completely once before just drawing with changes. */

#ifndef WORLD_H
#define WORLD_H

#ifndef NO_PRAGMAS
#pragma interface
#endif

// Include Files
#include "utils.h"
#include "coord.h"
#include "area.h"


// Defines.
// WSQUARES in a room.
#define W_ROOM_COL_MAX 40//36
#define W_ROOM_ROW_MAX 16//15 

/* Maximum rooms in world.  World may actually be smaller.  World starts out 
   at the size of the title screen. */
#define W_ACROSS_MAX_MAX 15
#define W_DOWN_MAX_MAX 15

// WSQUARES in world.
#define W_COL_MAX_MAX (W_ROOM_COL_MAX * W_ACROSS_MAX_MAX)
#define W_ROW_MAX_MAX (W_ROOM_ROW_MAX * W_DOWN_MAX_MAX)

/* The maximum number of wsquares that can be set changed/unchanged in one 
  clock cycle. */
#define W_CHANGES_MAX 5000

/* The blocks composing the world.  Represented as integers so that the world
   map can be initialized easily. */
enum { Wempty, Wwall, Wladder, Woutside, Wsquanch, WupDown, WtextSquare};
#define W_BLOCKS_NUM 7

enum Wsig {W_NO_SIG, W_CLOSE, W_CLOSE_BAD, W_FAILURE};


enum {W_DOOR_TOP, W_DOOR_BOTTOM};

// No more Wmap

#define W_POSTERS_NUM 13
class Blueprints;



// Class Declarations
class World {
  struct Wxdata {
    Pixel background[Xvars::DISPLAYS_MAX]; 
    Pixmap pixmaps[Xvars::DISPLAYS_MAX][W_BLOCKS_NUM];
    // Array of pixmaps for a poster with columns changing fastest.
    Pixmap *posterPixmaps[Xvars::DISPLAYS_MAX][W_POSTERS_NUM];  
    Pixmap doorPixmaps[Xvars::DISPLAYS_MAX][2];
  };

  struct PosterSquare {
    int type;
    int poster;
    Loc loc; // Inside poster.
  };

  struct DoorSquare {
    int type;
    int topBottom; 
    Loc dest;
  };

  union UnionSquare {
    int type; // UN_POSTER or UN_DOOR.
    PosterSquare pSquare;
    DoorSquare dSquare;
  };


 public:
  World(Boolean pol_correct);
  /* EFFECTS: Create the title screen world.  If politically correct, 
   there will be no posters and the title screen will be blank. */

  Size get_size() {return size;}
  /* EFFECTS: Size of the world in pixels. */

  Dim get_dim() {return dim;}
  /* EFFECTS: Dimensions of the world in wsquares. */

  Dim get_room_dim();
  /* EFFECTS: Return the dimensions of a room in wsquares. */

  Size get_room_size();
  /* EFFECTS: The size of a single room in pixels. */

  Rooms get_rooms() {return rooms;}
  /* EFFECTS: The max number of rooms in across and down direction. */

  Boolean check_door(const Loc &loc1,Loc &dest);
  /* EFFECTS:  Return whether there is a door at loc1.  If there is, return
     the location of the doors destination in dest. */

  void set_rooms_next(const Rooms &r);
  /* EFFECTS: After the next reset, the world will have as many rooms as r.
   If r is bigger than the maximum, the min of the maximum size and r is 
   used. */

  void set_map_print(Boolean val) {mapPrint = val;}
  /* EFFECTS: Sets whether to print each new map of the world at reset. */

  Boolean overlap(const Box &box);
  /* EFFECTS: Does any of box overlap with the wsquares of the world. */

  void draw(Drawable buffer,Xvars &xv,int dpyNum,const Box &box);
  /* EFFECTS: Draw all of the box that is in the world. */

  void draw(Drawable buffer,Xvars &xv,int dpyNum,const Area &area);
  /* EFFECTS: Draw all of area that is in the world. */

  Boolean inside(const Loc &l)
    {return l.r >= 0 && l.c >= 0 && l.r < dim.rowMax && l.c < dim.colMax;}
  /* EFFECTS: Returns True if loc is a wsquare inside the world, otherwise
   returns False. */

  Boolean open(const Loc &loc,Boolean laddersClosed = False,
	       Boolean postersClosed = False,
	       Boolean doorsClosed = False);
  /* EFFECTS: Returns True if loc is an open square inside the world.  Returns
     False otherwise.  Note that wsquares outside the world are treated as if 
     they were closed wsquares. */

  Boolean open(const Area &area,Boolean laddersClosed = False,
	       Boolean postersClosed = False,Boolean doorsClosed = False);
  Boolean open(const Box &box,Boolean laddersClosed = False,
	       Boolean postersClosed = False,Boolean doorsClosed = False);
  /* EFFECTS: Is the portion of the world covered by area (or box) blocked? 
     laddersClosed implies that ladders and other non-walls are considered to be
     blocking. */

  

  Wsig open_offset(Size &offset,const Area &area,
		   const Vel &vel,Boolean laddersClosed = False);
  /* MODIFIES: offset  */
  /* EFFECTS: Returns W_NO_SIG if the portion of the world covered by initial 
     is open.  prev is the previous area occupied before.  If the area is 
     blocked, but a nearby area is not, returns
     W_CLOSE_OK and sets offset to be the offset necessary to unblock initial.
     Retruns W_CLOSE_BLOCKED if a nearby area is blocked.  Stills sets offset
     for the nearby area.  Returns W_FAILURE if initial and all nearby areas 
     are blocked.  offset may be modified in any case. */

  void touchingHanging(Touching &touching, Hanging &hanging, 
		       const Area &area);
  /* REQUIRES: area is AR_RECT. */
  /* MODIFIES: touching, hanging */
  /* EFFECTS: If area is touching a blocked wsquare, sets touching to be the 
     direction from area to the wsquare.  If the area is touching in both the
     vertical and horizontal direction, the vertical takes precedence.  Sets
     touching to be CO_air if not touching in any direction.  If the area is
     hanging off of the edge of some blocked wsquares, sets hanging 
     appropriately.  Otherwise, sets hanging.corner to be CO_air. */
  
  Boolean canClimb(const Loc &loc)
    {return inside(loc) ? (map[loc.r][loc.c] == Wladder) : False;}
  /* EFFECTS: Returns True if loc is a climable square inside the world.  I.e.
     a ladder.  False otherwise. */

  Pos empty_rect(const Size &size);
  /* EFFECTS: Return the upper-left position of a randomly choosen empty 
     rectangle of size s.  Ladders and doors are not considered empty.  
     Posters are considered empty. */

  Pos empty_touching_rect(const Size &size);
  /* EFFECTS: Like empty_rect except the returned rect will be touching the 
     ground. */

  Pos empty_accessible_rect(const Size &size);
  /* EFFECTS: Like empty_rect except that it guarantees that the rectangle can
     be reached by all Creatures.  Also, the returned rectangle will be 
     touching CO_dn. */
  /* REQUIRES: reset() has been called. */

  Boolean empty_box(Loc &loc,const Dim &dim,Boolean laddersClosed,
		    Boolean postersClosed,Boolean doorsClosed);
  /* MODIFIES: loc */
  /* EFFECTS: Returns True iff an empty box of dimension dim can be found and
     returns its upper-left Loc. */

  Boolean empty_touching_box(Loc &loc,const Dim &dim,Boolean laddersClosed,
			     Boolean postersClosed,Boolean doorsClosed);
  /* MODIFIES: loc */
  /* EFFECTS: Like empty_box, except the returned box will be touching the
     ground. */
  
  void reset();
  /* EFFECTS:  Prepare the world for a new game.  Create a new map. */

  void clock() {} 
  

 private:
  void draw_square(Drawable buffer,Xvars &xvars,int dpyNum,const Loc &loc,
		   int x,int y);
  /* EFFECTS: Draw the appropriate square from loc to (x,y) on buffer. */

  void dim_size_update();
  /* EFFECTS: Updates dim and size from the current value of rooms. */

  void init_x(Xvars &);
  /* EFFECTS: Initialize all x dependencies. */

  void title_map();
  /* EFFECTS: Draw the title screen on the map. */

  void th_helper(Touching &touching,Hanging &hanging,
		 const TouchingListItem &item,
		 Boolean r_c,
		 int coord,int length,
		 int wsquare_length,
		 Touching iftouching,
		 Corner ifsmall, Corner iflarge);
  /* MODIFIES: touching, hanging */
  /* EFFECTS: Helper for World::touching.  Does touching, hanging for one 
     side. */

  Boolean open_iter(Area &area,int &changed,Dir dir,Boolean laddersClosed);
  /* MODIFIES: area, changed */
  /* EFFECTS: Move area as much in dir as necessary to avoid all its wsquares.
     Increment changed by the distance moved.  Return True if the original 
     area is open and nothing is changed.*/

  Size open_size(int offset,Dir dir);
  Size open_size(int offset1,Dir dir1,int offset2,Dir dir2);
  /* REQUIRES: dir is in {CO_R, CO_DN, CO_L, CO_UP} */
  /* EFFECTS: Return the Size corresponding to an offset in one or two 
     directions. */

  Boolean open_try_dirs(Size &offset,const Area &area,const Dir dirs[4],
			       int dirsNum,Boolean laddersClosed);
  /* MODIFIES: offset */
  /* EFFECTS: Sets offset to be the minimum necessary to shift area to an
     open area in one of the directions of dirs.  Returns whether it 
     succeeded.  offset may be changed in any case. */
  
  Boolean open_try_diagonals(Size &offset,const Area &area,
				   Boolean laddersClosed);
  /* MODIFIES: offset */
  /* EFFECTS: Set offset to shift area to a nearby open one.  Returns whether
     it succeeded.  offset may be changed in any case. */

  void add_posters();
  void add_doors();
  /* NOTES: Helpers for World::reset. */


  Rooms rooms,roomsNext;
  Size size;  /* For convenience only. */
  Dim dim;    /* For convenience only. */

  char map[W_ROW_MAX_MAX][W_COL_MAX_MAX];
  Boolean xValid;
  Wxdata xdata;
  Boolean mapPrint;
  Dim posterDims[W_POSTERS_NUM]; 
  UnionSquare *unionSquares[W_ROW_MAX_MAX][W_COL_MAX_MAX];
  Boolean polCorrect;
  Blueprints *blueprints;

  // Defined in world.bitmaps.
  static char *posterPixmapBits[W_POSTERS_NUM];
  static char *posterMaskBits[W_POSTERS_NUM];
  static const Size posterSizes[W_POSTERS_NUM];
  static const char *posterForegrounds[W_POSTERS_NUM];
  static const char *posterBackgrounds[W_POSTERS_NUM];
  static char *doorPixmapBits[2];
  static char *pixmapBits[W_BLOCKS_NUM];
  static const char *colorNames[W_BLOCKS_NUM];
};

			   

// Typdefs
  typedef World *WorldP;
#endif
