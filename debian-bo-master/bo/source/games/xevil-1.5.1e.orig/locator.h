// "locator.h" The object locator.  

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

/* Overview: 
The locator can be seen as the collection of all physical objects in
the game.  It draws all objects with the draw method.  The clock
method goes through all the phases of one turn for all objects.  This
is the only entity in the game that has a list of all the objects.
Objects can be mapped and/or collidable.  An unmapped object is
neither drawn nor collided.  An uncollidable object is drawn (if
mapped) but not collided.  There is a message queue that objects can
use to communicate with the ui object.  The locator does not need to
be clocked for the queue to work. 
The locator grid is the maximum size of the world.  I.e. it can be larger than
the world. */


#ifndef LOCATOR_H
#define LOCATOR_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
#include <iostream.h>

#include "utils.h"
#include "coord.h"
#include "id.h"
#include "world.h"

class Intel;
typedef Intel *IntelP;
class Physical;
typedef Physical *PhysicalP;
class PhysicalContext;
class Human;
typedef Human *HumanP;
class Enemy;
typedef Enemy *EnemyP;
class Neutral;
typedef Neutral *NeutralP;


// Defines
#define OL_LIST_MAX 1000
#define OL_NEARBY_MAX OL_LIST_MAX
#define OL_MESSAGES_MAX 300

#define OL_GRID_COL_MAX 4
#define OL_GRID_ROW_MAX 4
#define OL_GRID_WIDTH (OL_GRID_COL_MAX * WSQUARE_WIDTH) // In pixels.
#define OL_GRID_HEIGHT (OL_GRID_ROW_MAX * WSQUARE_HEIGHT) // In pixels.
#define OL_GRID_SIZE_MAX (max(OL_GRID_WIDTH,OL_GRID_HEIGHT))
#define OL_GRID_HORIZ_MAX (W_COL_MAX_MAX / OL_GRID_COL_MAX + 1)// grid squares.
#define OL_GRID_VERT_MAX (W_ROW_MAX_MAX / OL_GRID_ROW_MAX + 1) // grid squares.

// Data Structures.
enum OLsig {OL_NO_SIG, OL_ALREADY_MANAGED, OL_NOT_FOUND};



// Class Declarations
class Locator;
class OLgridEntry;
class OLshadowEntry;


class OLentry {
  friend class Locator;
  friend class OLgridEntry; 
  friend class OLshadowEntry;
  friend class PhysicalIter;
  
  OLgridEntry *gridEntry; /* Only valid if mapped || flash. */
  GLoc gloc; /* Set by OLgridEntry::insert.  Not nec. valid. */
  OLshadowEntry *shadowEntry; /* Only valid if mapped || flash. */
  GLoc shadowGloc; /* Set by OLshadowEntry::insert.  Not nec. valid. */


  Boolean valid; 
  Boolean reserved; // Meaningful when !valid.
  PhysicalP physical; // Set when reserved.
  
  Boolean collided;
  Boolean mapped; // Redundant.  Must be same as in object. 
  Boolean flash; // Redundant.  Must be same as in object.
  Boolean collidable;
};



class OLgridEntry {
 public:
  OLgridEntry(OLentry *e) {entry = e; prev = next = NULL;}
  
  OLentry *get_entry() {return entry;}
  PhysicalP get_physical() {return entry->physical;}
  Boolean get_collided() {return entry->collided;}
  Boolean get_mapped() {return entry->mapped;}
  Boolean get_flash() {return entry->flash;}
  OLshadowEntry *get_shadow_entry() {return entry->shadowEntry;}
  const Area &get_area(); 

  int get_drawing_level();

  Boolean get_collidable() {return entry->collidable;}
  OLgridEntry *get_next() {return next;}

  void set_collided(Boolean val) {entry->collided = val;}

  void insert(OLgridEntry *grid[OL_GRID_VERT_MAX][OL_GRID_HORIZ_MAX],
	      const GLoc &gl);

  void remove();

  
 private:
  OLgridEntry *prev, *next;
  OLentry *entry;
};
// This typedef not always used.  OLgridEntry::insert
typedef OLgridEntry *OLgrid[OL_GRID_VERT_MAX][OL_GRID_HORIZ_MAX];



class OLshadowEntry {
 public:
  OLshadowEntry() {entry = NULL; prev = next = NULL; orphaned = False;}
  OLshadowEntry(const Area &a,OLentry *e) 
    {area = a; entry = e; prev = next = NULL; orphaned = False;}
  Boolean get_orphaned() {return orphaned;}

  OLentry *get_entry() {assert(entry); return entry;}
  OLgridEntry *get_grid_entry() {assert(entry); return entry->gridEntry;}
  const Area &get_area() {return area;}
  Boolean draw_self() {return drawSelf;}
  OLshadowEntry *get_next() {return next;}
  /* NOTE: Several require that the shadowEntry is not orphaned. */

  void set_area(const Area &a) {area = a;}
  void set_draw_self(Boolean val) {drawSelf = val;}
  void set_orphaned() {orphaned = True; entry = NULL;}


  void insert(OLshadowEntry *shadows[OL_GRID_VERT_MAX][OL_GRID_HORIZ_MAX],
	      const GLoc &gl);
  void remove();


 private:
  Boolean orphaned;
  Area area;
  Boolean drawSelf;
  OLentry *entry;
  OLshadowEntry *prev, *next;
};
typedef OLshadowEntry *OLshadows[OL_GRID_VERT_MAX][OL_GRID_HORIZ_MAX];



// Iterator to extract all humans to be reincarnated.
class Incarnator {
public:
  Incarnator(Locator &l) {n = 0; locator = &l;}
  
  HumanP operator () ();
  /* EFFECTS: Extract next object to be reincarnated.  Return NULL if none.
     The returned HumanP can only be extracted once. */

  
private:
  int n;
  Locator *locator;
};



class PhysicalIter {
 public:
  PhysicalIter(Locator &l) {n = 0; locator = &l;}

  PhysicalP operator() ();
  /* EFFECTS: Yield all Physicals managed by the Locator. */
  /* REQUIRES: Must be called outside of Locator::clock.  The Locator must 
     not be modified while the PhysicalIter is being used. */


 private:
  int n;
  Locator *locator;
};



class Locator {
  struct OLxdata 
    {
      Pixmap buffer[Xvars::DISPLAYS_MAX];
      Pixmap scratchBuffer[Xvars::DISPLAYS_MAX];
    };

  friend class Incarnator;
  friend class PhysicalIter;


 public:
  enum { HUMANS_MAX = Xvars::HUMAN_COLORS_NUM, // <= UI_VIEWPORTS_MAX.
	 ENEMIES_MAX = 500, NEUTRALS_MAX = 400, DRAWING_LEVELS = 3,
       };

  Locator(WorldP world);
  /* EFFECTS: Create a new object locator with no managed objects. */

  static Boolean valid(const GLoc &gl) 
    {return ((gl.vert >= 0) && (gl.horiz >= 0) && 
	     (gl.vert < OL_GRID_VERT_MAX) && (gl.horiz < OL_GRID_HORIZ_MAX));} 
  /* EFFECTS:  Is gl a loc in the grid. */

  void add(PhysicalP p);
  /* REQUIRES: p is not already managed. */
  /* EFFECTS: p will be added to the list of managed physicals at the beginning
     of the next clock.  p is initially mapped.  canCollide is set from p.
     If called inside clock, object will not be added until beginning of next
     clock.  You should not add something twice.  p is given a valid Id 
     immediately (not at the next clock). */

  void get_nearby(PhysicalP nearby[OL_NEARBY_MAX],int &nitems,
		  PhysicalP p,int radius);
  /* MODIFIES: nearby,nitems. */
  /* EFFECTS:  Returns all objects that have their middles within radius of 
     p's middle.  (I.e <= ) */  
  /* NOTE: Expensive. */

  OLsig lookup(PhysicalP &p, const Id &id);
  /* MODIFIES: p */
  /* EFFECTS: Set p to be the object corresponding to id if it is managed 
     and return OL_NO_SIG.  Otherwise return OL_NOT_FOUND. */

  PhysicalP lookup(const Id &id);
  /* EFFECTS: Same as above except return NULL iff not found. */

  IntelP lookup(const IntelId &iId);
  /* EFFECTS: Return the human or machine (enemy or neutral) Intel for iId if 
     it exists, is registered, and is alive or reincarnating.  (Note that only 
     Humans can be reincarnating.)  Otherwise, return NULL. */

  void draw(Drawable window,Xvars &xvars,int dpyNum,const Box &box);
  /* REQUIRES: win is the same size as Box. (Adjusted for size of WSQUAREs.) */

  void reset();

  void clock();


  // Message service.
  void message_enq(char *msg);
  /* EFFECTS:  Enqueue msg in the queue of messages for the ui.  The ui will 
     free msg when it is done with it. */

  char *message_deq();
  /* EFFECTS: Dequeue the next message of the message queue or return NULL if
     the queue is empty. */

  void set_messages_ignore(Boolean msgIg) {messagesIgnore = msgIg;}
  /* EFFECTS: While set to True, all enques messages will be immediately discarded. 
   Default is False. */


  // Player registry service.  Takes responsibility for reincarnation.
  void register_human(HumanP); // Kept until reset.
  void register_enemy(EnemyP); // Deleted as needed.
  void register_neutral(NeutralP); // Deleted as needed.
  
  int humans_playing();
  int enemies_alive();
  int neutrals_alive();
  
  HumanP get_human(int n) {assert(n < humansNum);  return humans[n];}
  /* REQUIRES: n < the number of humans registered */
  /* NOTE: Does not guarantee any particular order. */

  Pixmap get_scratch_buffer(int dpyNum) {return xdata.scratchBuffer[dpyNum];}
  /* NOTE: The scratch buffer is exported by the locator for any drawing tricks
     that objects may want to use to draw themselves.  E.g. Cloaks use it. 
     Guaranteed to be as large as the largest object. */


  // Class registry service.
  // Note: To iterate over all classes, you can also loop from 0 to 
  // A_CLASSES_NUM, calling get_context() on each.  Make sure to test for NULL.
  const PhysicalContext *get_context(ClassId classId) 
    {assert(classId >= 0 && classId < A_CLASSES_NUM); return contexts[classId];}
  /* EFFECTS: Return the PhyscialContext of the class with the given 
     ClassId. */
  
  int filter_contexts(const PhysicalContext *contextList[A_CLASSES_NUM],
		      ClassId idList[A_CLASSES_NUM],
		      Boolean (* filter)(const PhysicalContext *));
  /* MODIFIES:  */
  /* EFFECTS: Create a list of all the classes/ids of object for which filter
     returns True.  list should be an array of size A_CLASSES_NUM.  
     Returns the size of list.  Either of contextList, idList can be NULL,
     if one is, it will not be filled in. */

  
 private:
  enum { REINCARNATE_TIME = 70 };

  void add_now(PhysicalP);
  void del_now(PhysicalP);

  void allign_flash_and_mapped(int n);
  /* EFFECTS: Checks to see if physical n has changed its mapped or flash 
     attribute.  If so, adjust the mapped attribute in *this. */
  /* REQUIRES: n is a valid list index. */

  void init_x(Xvars &);

  Id reserve_list_entry(PhysicalP p);
  /* EFFECTS: Reserve an entry in the list for p and return an Id for the
     entry. */
  
  void collision_checks();
  /* EFFECTS: Do collision checks between all registered objects. */

  GLoc compute_gloc(PhysicalP);
  GLoc compute_gloc(const Area &);
  /* EFFECTS: Compute the location in the grid for a physical or an area. */
  /* NOTE: Ok to return invalid gloc. */

  void draw_area(Drawable window,Xvars &xvars,int dpyNum,const Box &room,
		 const Area &area,const GLoc &gloc);
  /* REQUIRES: area is centered at gloc. window is the size of room*/
  /* EFFECTS: Draws world and all objects overlapping area. */
  /* NOTE: box does not have to be valid. */

  void reincarnate_clock();


  // Class registry service.
  void register_contexts();
  void register_context(const PhysicalContext *context); 
  /* EFFECTS: All classes of Physical objects must be registered with
     register_context in order to be seen by the Game. */


  // Data.
  WorldP world;
  int uniqueGen;
  int listMax; // All valid entries in the list have index < listMax.
  OLentry list[OL_LIST_MAX];
  OLgrid grid;

  OLshadows shadows;
  OLshadowEntry *shadowDelList[OL_LIST_MAX];
  int shadowDelNum;

  PhysicalP addList[OL_LIST_MAX];
  int addNum;

  Boolean xValid;
  OLxdata xdata;

  // Message service.
  char *messages[OL_MESSAGES_MAX];
  int head,tail; 
  Boolean messagesIgnore;

  // Player registry service.
  HumanP humans[HUMANS_MAX];
  EnemyP enemies[ENEMIES_MAX];
  NeutralP neutrals[NEUTRALS_MAX];
  int humansNum;
  int enemiesNum;
  int neutralsNum;
  Boolean reincarnating[HUMANS_MAX]; // Just for humans.
  Timer reincarnateTimers[HUMANS_MAX]; // Just for humans.

  // List of all classes of Physicals.  Indexed by the values for ClassId.
  // E.g. contexts[A_ChopperBoy] is the PhysicalContext for the class
  // ChopperBoy.
  const PhysicalContext *contexts[A_CLASSES_NUM];
  // contextCount is for verification purposes, make sure all the classes are
  // registered.
  int contextCount;
};
typedef Locator *LocatorP;
#endif
