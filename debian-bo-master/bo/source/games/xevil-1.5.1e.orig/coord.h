// "coord.h"  Coordinates and directions and the like.

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

#ifndef COORD_H
#define COORD_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
#include "utils.h"



// Defines
#define WSQUARE_WIDTH 16
#define WSQUARE_HEIGHT 16
#define WSQUARE_WIDTH_INV (1.0 / WSQUARE_WIDTH)
#define WSQUARE_HEIGHT_INV (1.0 / WSQUARE_HEIGHT)


// Order of directions IS guaranteed.
#define CO_center_R 0
#define CO_center_L 1
#define CO_air_R 2
#define CO_air_L 3
#define CO_air 4
#define CO_center 5

#define CO_r 6
#define CO_r_DN 7 
#define CO_r_UP 8
#define CO_dn 9
#define CO_dn_R 10
#define CO_dn_L 11
#define CO_l 12
#define CO_l_DN 13
#define CO_l_UP 14
#define CO_up 15
#define CO_up_R 16
#define CO_up_L 17

#define CO_climb 18
#define CO_climb_DN 19
#define CO_climb_UP 20
#define CO_air_UP 21
#define CO_air_DN 22
#define CO_climb_R 23
#define CO_climb_L 24

#define CO_R 25
#define CO_DN_R_R 26
#define CO_DN_R 27
#define CO_DN_DN_R 28
#define CO_DN 29
#define CO_DN_DN_L 30
#define CO_DN_L 31
#define CO_DN_L_L 32
#define CO_L 33
#define CO_UP_L_L 34
#define CO_UP_L 35
#define CO_UP_UP_L 36
#define CO_UP 37
#define CO_UP_UP_R 38
#define CO_UP_R 39
#define CO_UP_R_R 40

#define CO_DIR_MAX 41

#define CO_DIR_PURE 16  // "Pure" does not include CO_center.  Use CO_air.

typedef int ClassId;
// Possible values for a ClassId.  There should be an entry for every class
// of Physical objects in the game.
// Also used as an index into Locator::contexts.
//
// Note that A_CLASSES_NUM is public and accessible by anyone.
//
enum {A_Explosion,A_Fire,
      A_NProtection,A_TProtection,
      A_Trapdoor,A_Home,
      A_Shell,A_SwapShell,A_Lance,A_Laser,A_FrogShell,A_Fireball,A_Missile,
      A_Star,
      A_Grenade,A_Egg,
      A_Xit,A_Flag,
      A_Rock,A_Weight,A_AltarOfSin,A_Doppel,A_Cloak,
      A_Transmogifier,A_MedKit,A_NShield,A_TShield,A_Bomb,
      A_Chainsaw,A_Pistol,A_MGun,A_Lancer,A_FThrower,A_Launcher,A_Grenades,
      //      A_Shotgun,
      A_Stars,
      A_Swapper,A_FrogGun,
      A_Enforcer,A_Frog,A_Hero,A_Ninja,A_Alien,A_RedHugger,A_GreenHugger,
      A_ChopperBoy,A_Lemming,
      A_FireDemon,A_Walker,

      // Identifiers for non-existent classes.
      A_CLASSES_NUM,
      A_Fireballs, A_Lasers,
      A_SuicideButton,
      A_None
      }; 

typedef int Dir;
typedef int Touching;
typedef int Stance;
typedef int Corner;
typedef int Mass;
typedef int Health;
typedef int Frame;
typedef int Speed;
typedef long Quanta;



class Area;



// Data Structures
class Stats {
 public:
  Stats() {creations = uses = deaths = 0; aveLifespan = 0;}

  long get_creations() const {return creations;}
  long get_uses() const {return uses;}
  long get_deaths() const {return deaths;}

  float get_ave_lifespan() const {return aveLifespan;}
  /* NOTE: Value returned is in seconds. */

  void add_creation() { if (enabled) creations++;}
  void add_use() {if (enabled) uses++;}
  void add_death(time_t birthTime);
  
  static void enable() {enabled = True;}
  /* EFFECTS: Stats will only be recorded after the call to enable(). */
  /* NOTE: Used so that stats will not be recorded during the demo screen. */

 private:
  long creations;
  long uses; // use, explosion.
  long deaths;
  float aveLifespan; // Valid iff deaths > 0.

  Boolean static enabled;
};



struct Pos {
  Pos() {x = y = 0;}
  Pos(int xx,int yy) {x = xx; y = yy;}

  int distance(const Pos &) const;
  /* EFFECTS: Returns distance between two points. */

  int distance_2(const Pos&) const;
  /* EFFECTS: Returns square of distance between two points. */
  
  int x; int y;
}; // In pixels



struct Vel {
  Vel() {dx = dy = 0.0;}
  Vel(float x,float y) {dx = x; dy = y;}
  void set_zero() {dx = dy = 0.0;}
  Vel shrink(float k) const;
  void damp(float k);
  Boolean is_zero() const;
  Dir get_dir() const;
  void limit(float k);
  /* REQUIRES: k >= 0 */
  /* EFFECTS: Force dx and dy to be <= k. */

  void get_dirs_4(Dir in[4],Dir out[4],int &inNum,int &outNum);
  /* MODIFIES: in, out, inNum, outNum */
  /* EFFECTS: Partitions {CO_R, CO_DN, CO_L, CO_UP} into in and out.  inNum 
     and outNum are set to the sizes of the respective sets. */

  float dx; float dy;
};



struct Dim {
  Dim() {rowMax = colMax = 0;}
  Dim(int rm,int cm) {rowMax = rm; colMax = cm;}

  int rowMax, colMax; // In WSQUARES.
}; 



struct Size {
  Dir get_dir();
  /* EFFECTS: Returns one of {CO_R..CO_UP_R,CO_air}. */

  void get_dirs_4(Dir &d1,Dir &d2);
  /* MODIFIES: d1, d2 */
  /* EFFECTS: Gets the two directions of {CO_R,CO_DN,CO_L,CO_UP} that 
     correspond to *this.  If there is only one, it is returned as d1 and 
     d2.  If *this has zero size, d1 == d2 == CO_air on return. */

  void set(int w,int h){width = w; height = h;}

  void set(const Dim &dim) {width = dim.colMax * WSQUARE_WIDTH;
			    height = dim.rowMax * WSQUARE_HEIGHT;}

  float cross(const Vel &v);
  /* EFFECTS: z component of the cross product of the size and the Vel. */

  void set_zero() {width = height = 0;}

  int abs_2() {return width*width + height*height;}

  int width; int height; // In pixels.
}; 



// Constructors mess with TouchingList in area.h
struct Loc {
  Loc move(int dr,int dc) 
    {Loc ret; ret.r = r + dr; ret.c = c + dc; return ret;}
  /* EFFECTS: Return new Loc that is *this + (dr,dc) */

  int r,c; // In WSQUARES.
}; 



struct Box {
  Box() {};
  Box(const Loc &l,const Dim &d) {loc = l; dim = d;}
  Boolean overlap(const Loc &);

  Loc loc; Dim dim; // In WSQUARES.
}; 



struct GLoc {
  int vert, horiz;
};



struct RoomIndex {
  RoomIndex() : down(0), across(0) {}
  RoomIndex(int d,int a) : down(d), across(a) {}

  // In rooms.
  int down; 
  int across; 
}; 

struct Rooms {
  Rooms() {downMax = acrossMax = 0;}
  Rooms(int dn,int acc) {downMax = dn; acrossMax = acc;}

  int downMax, acrossMax;
}; 



struct Acc {
  operator Vel()
    {Vel ret(ddx,ddy); return ret;}
  /* EFFECTS: Converts from an acceleration to velocity.  Assumes initial 
     velocity is 0. */

  float ddx, ddy;
};



struct Hanging {
  Hanging() {corner = CO_air;}

  Corner corner;
  Loc loc; // Not meaningful if corner == CO_air.
};



typedef int Grav;

enum Attack {attackNone, attackStuck, attackFree};



// Function Prototypes
Boolean operator == (const Loc &l1, const Loc &l2);
Boolean operator == (const Pos &p1, const Pos &p2);
Boolean operator == (const Vel &, const Vel &);
Boolean operator == (const Size &s1, const Size &s2);
Boolean operator == (const GLoc &,const GLoc &);
Boolean operator != (const GLoc &,const GLoc &);
Pos operator + (const Pos &pos,const Size &size);
Pos operator - (const Pos &pos,const Size &size);
Size operator - (const Pos &p1,const Pos &p2);
Pos operator + (const Pos &pos, const Vel &vel);
Size operator * (float k,const Size &size);
Vel operator + (const Vel &,const Vel &);
Vel operator + (const Vel &, const Acc &acc);
Vel operator * (float k,const Vel &vel);
Vel operator / (float k,const Vel &vel);
Vel operator + (float k,const Vel &vel);
Acc operator * (int k,const Acc &acc);


class Coord
{
public:
  static Dir dir_opposite(Dir dir);
  /* REQUIRES: dir must be a pure dir. */

  static Dir movement_dir_4(Dir dir);
  /* EFFECTS: Returns a dir in {CO_air,CO_R,CO_DN,CO_L,CO_UP}.  CO_air if
     the dir corresponds to one where the object is not moving.  Otherwise, 
     one of the others. */

  static Pos shot_initial_pos(const Area &area,
			      const Size &shotSize,Dir shotDir);
  /* EFFECTS: Compute the starting position of the shot.  The shot should be
     flush on the inside of the area, if possible.  If the shot is too big,
     make sure it doesn't stick out the opposite side. */
};
#endif

  
