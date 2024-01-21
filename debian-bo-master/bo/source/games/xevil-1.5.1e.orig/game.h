// "game.h"

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

#ifndef GAME_H
#define GAME_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
extern "C" {
#include <time.h>
}
#include <iostream.h>
#include "utils.h"
#include "ui.h"
#include "world.h"
#include "locator.h"
#include "actual.h"



// Class Declarations
class Game;
typedef Game *GameP;

class GameStats {
 public:
  GameStats() {reset();}
  long num_turns() {return numTurns;}
  void reset() {aveTime = 0; numTurns = 0;}
  void report();
  void clock();
  

 private:
  long numTurns;
  time_t prevTime;
  float aveTime; /* In seconds */
};



// New scalable, way using Locator::filter_contexts.
class GameObjects {
 public:
  GameObjects(WorldP, LocatorP);

  void game_reset(const char *oneItem,Boolean noItems,
		  Boolean oneEach);
  /* EFFECTS: Initialize  values for a new game.  Does not 
     actually create objects.  oneItem not empty means there will be only one 
     item in the world, the value of oneItem.  Otherwise, noItems means there 
     will be no items.  Otherwise regular items.  oneEach will cause
     one of each item to be used. */
  /* NOTE: Must be called once before GameObjects::refill. */
  /* REQUIRES: The memory for oneItem is not copied internally, so don't 
     ever delete it. */

  void level_reset(const Dim &worldDim);
  /* EFFECTS: Choose how many of each class of objects to use for the 
     next level. */
  /* IMPLEMENTATION NOTE: Deal with memory for ids[]. */

  void refill();
  /* EFFECTS: Create and add objects as necessary. */
  /* REQUIRES: GameObjects::game_reset has already been called. */


private:
  void refill_helper(const PhysicalContext *cx,int which);
  /* EFFECTS:  Make it so that actuals[which] of the Item exist.  I.e. create
     (actuals[which] - <current number>) new Items. */

  void compute_actuals(const PhysicalContext *weapons[],int weaponsNum,
		       const PhysicalContext *oItems[],int oItemsNum);
  /* EFFECTS: Compute actuals from maximums.  weapons and oItems are the lists
   of potential weapons and potential otherItems. */
  
  static Boolean potential_weapon_filter(const PhysicalContext *);
  static Boolean potential_other_item_filter(const PhysicalContext *);
  /* NOTE: Used for Locator::filter_context. */

  WorldP world;
  LocatorP locator;

  Boolean resetCalled;
  Boolean altarOfSinAlready; // Has an AltarOfSin already been created.
  int maximums[A_CLASSES_NUM];
  int actuals[A_CLASSES_NUM];
  Id *ids[A_CLASSES_NUM];

  const char *oneItem;
  Boolean noItems;
  Boolean oneEach;
};

// Old way
#if 0
class GameObjects {
 public:
  GameObjects(Boolean pol_correct);

  void game_reset(const char *oneItem,Boolean noItems,
		  Boolean oneEach);
  /* EFFECTS: Initialize  values for a world of size worldDim.  Does not 
     actually create objects.  oneItem not empty means there will be only one 
     item in the world, the value of oneItem.  Otherwise, noItems means there 
     will be no items.  Otherwise regular items.  oneEach will cause
     one of each item to be used. */
  /* NOTE: Must be called once before GameObjects::refill. */
  /* REQUIRES: The memory for oneItem must never be deleted. */

  void level_reset(const Dim &worldDim);
  /* EFFECTS: Choose how many of each class of objects to use for the 
     next level. */
  
  void refill(WorldP w);
  /* EFFECTS: Create and add objects as necessary. */
  /* REQUIRES: GameObjects::game_reset has already been called. */


private:
  enum { PISTOLS,CHAINSAWS,M_GUNS,SWAPPERS,LANCERS,FROG_GUNS,F_THROWERS,
	 LAUNCHERS,GRENADESS,STARSS,W_HIGHEST};
  enum { ROCKS = W_HIGHEST,WEIGHTS,MEDKITS,BOMBS,N_SHIELDS,
	 T_SHIELDS,TRANSMOGIFIERS,DOPPELS,CLOAKS,
	 HIGHEST};

  void compute_actuals();
  
  Boolean resetCalled;
  Boolean altarOfSinAlready;
  int maximums[HIGHEST];
  int actuals[HIGHEST];
  Id *ids[HIGHEST];
  Boolean polCorrect;

  const char *oneItem;
  Boolean noItems;
  Boolean oneEach;
};
#endif



class Game {
 public:
  enum {
    // In turns.
    REPORT_TIME = 200,  
  };
  
  enum Scenario {EXTERMINATE,BONUS,HIVE,FLAG,LEMMINGS,FIRE_DEMON,HIGHEST};
  /* NOTE: Actually should be private.  Lame-ass Silicon Graphics compiler. */


  Game(int *, char **);

  Display *get_dpy(int dpyNum) {return ui.get_dpy(dpyNum);}
  int get_dpy_max() {return ui.get_dpy_max();}

  Quanta get_quanta() {return quanta;}
  /* EFFECTS: How often should the game be clocked. (microseconds) */

  Boolean show_stats() {return showStats;}
  
  void clock();
  /* EFFECTS: Do one cycle of game play. */
  
  void process_event(int dpyNum,XEvent *event) 
    {ui.process_event(dpyNum,event);}

  static ClassId parse_class_name(LocatorP l,const char *name);
  /* EFFECTS: Given the class name corresponding to a registered 
     PhysicalContext, return the ClassId of the PhysicalContext, or 
     A_None if not found. */
  /* NOTE: Pass in Locator so that we can export this as a static function. */

  
private:
  enum { WITTY_SAYINGS_NUM = 118,
	 INTEL_NAMES_NUM  = 66,
	 SCENARIO_BONUS_FROGS = 12,
	 SCENARIO_FLAG_FLAGS = 15,
	 SCENARIO_LEMMINGS_LEMMINGS = Trapdoor::LEMMINGS_MAX,
	 SCENARIO_LEMMINGS_NEED = 15,
         DEFAULT_CHOOSABLE_NUM = 7,};

  void ui_settings_check(Boolean &shouldReset);
  /* EFFECTS: Process any commands that the Ui has received. */
  
  void reincarnations_check();
  
  void game_over_check(int humansPlaying,int enemiesPlaying);
  /* EFFECTS: Set extraTimer if the game over conditions for the current style
     have been met.  Call end_game() when the Timer expires. */
  
  void new_level_check(int enemiesPlaying);
  /* EFFECTS: Called in clock().  Sets newLevelTimer if the level has been 
     completed.  Call new_level() when the Timer expires. */

  void refill_check();
  /* EFFECTS: Refill items in the game.  Depends on style and scenario. */
  
  void demo_check();
  
  static Boolean potential_human_filter(const PhysicalContext *pc);
  /* EFFECTS: Filter those classes whose potentialHuman flag is True. */
  
  PhysicalP human_physical(int humanNum = 0);
  /* USES: Game::humanClass */
  /* EFFECTS: Create a Physical appropriate for a Human. */
  /* NOTE: humanNum is something of a hack. */
  
  static Boolean potential_enemy_filter(const PhysicalContext *pc);
  /* EFFECTS: Filter those classes whose potentialEnemy flag is True. */

  PhysicalP enemy_physical(ClassId *classIds = NULL,int num = 0);
  /* REQUIRES: All elements of classIds are in Game::defaultChoosable. 
     (Needed for weights.) */
  /* EFFECTS: Create a Physical of a class in classIds or choose randomly if
     num is zero.  Otherwise, num is the length of classIds. */

  PhysicalP enemy_physical(ClassId classId);
  /* EFFECTS: Create a Physical of class classId. */
			     
  Scenario choose_scenario();
  /* EFFECTS: Randomly choose a scenario for the next level. */

  void end_game();
  /* EFFECTS: End the game immediately. */

  void new_level(Boolean fromReset = False);
  /* EFFECTS: Clean up after previous level and start a new level. 
  fromReset tells whether new_level() is being called from Game::reset
  or not. */
  /* NOTE: Not just used in UIsettings::LEVELS, also in UIsettings::DUEL. */

  void reset();
  /* EFFECTS: End the game and start a new one. */

  void process_x_resources(int *argc,char **argv);
  /* EFFECTS: Read in x resources. Sets default keyset for the Ui. */

  void parse_args(int *argc,char **argv);
  /* EFFECTS: Parses the command line, setting all options except
   politically correct. */

  Boolean is_pol_correct(int *argc,char **argv);
  /* EFFECTS: Checks for the politically correct command line option. */

  char **display_names(int *argc,char **argv);
  /* EFFECTS: Returns an array of length UI_VIEWPORTS_MAX of strings 
     containing the display names for all possible viewports.  
     "" means use the local display.  Must delete both the array and the 
     strings contained in it when done. */

  void humans_reset();
  /* EFFECTS: Create new human players and create/destroy viewports and 
     register the human playsers as necessary. */

  void style_reset();
  /* EFFECTS: Functionality for reset() that is specific to specific game
     styles. */

  void intro();
  /* EFFECTS: Put info messages on cout and on the Locator message
     queue. */ 

  static Boolean stats_creations_filter(const PhysicalContext *);
  static Boolean stats_uses_filter(const PhysicalContext *);
  static Boolean stats_deaths_filter(const PhysicalContext *);
  /* NOTE: Helpers for print_stats(). */

  void print_stats();
  /* EFFECTS: Print out current statistics for the Game to cout. */

  void refill_enemies();
  /* EFFECTS: Bring dead enemy players back to life. */

  void hive_refill_enemies();
  /* EFFECTS: refill_enemies for HIVE scenario. */

  void flag_refill_enemies();
  /* EFFECTS: refill_enemies for FLAG scenario. */

  void lemmings_refill_enemies();
  /* EFFECTS: refill_enemies for LEMMINGS scenario. */

  PhysicalP create_enemy(PhysicalP p = NULL);
  /* EFFECTS: Create enemy player, give it a Physical, register_enemy
     and add to locator.  The Physical will be p if non-NULL.  If p is NULL,
     use enemy_physical() to create a enemy.  Return the Physical. */

  ITmask intel_options_for(IntelOptions &ops,ClassId classId);
  /* EFFECTS: Return the default IntelOptions and mask for an (Enemy) 
     Intelligence in a body of class classId. */
  /* MODIFIES: ops */
 
  void demo_setup();
  /* EFFECTS: Create enemy players for the demo. */
  
  void off_clock_kill(PhysicalP);
  /* EFFECTS: Kill a Physical when control is not in Locator::clock().
     I.e. not during regular game play. */
     

  static char *wittySayings[WITTY_SAYINGS_NUM];
  static char *intelNames[INTEL_NAMES_NUM];
#if 0
  static ClassId defaultChoosable[DEFAULT_CHOOSABLE_NUM];
  static int defaultChoosableWeights[DEFAULT_CHOOSABLE_NUM];
#endif


  Quanta quanta;  // Time between each turn in milliseconds.
  Boolean showStats;
  World world;
  Rooms worldRooms; // Not used by all styles and scenerios.
  Locator locator; // Initialized after world.
  Ui ui; // Must be initialized after the other two.
  GameStats stats;
  int *argc;
  char **argv; // Careful, pointer to original argv, not a copy.
  int viewportsNum;
  int humansNum, humansNumNext; // Next applies to next reset.
  // From -name command line argument.  Each will be chosen randomly if "".  
  char *humanNames[Locator::HUMANS_MAX]; 
  int enemiesNum, // Used in different ways by different styles.
  enemiesNumNext; // What appears on the Ui.
  Boolean gameOn; // gameOn is still True while extraTimer is running.
  Timer extraTimer; // Extra time before ending the game.
  Boolean extraTime;

  // Don't start game immediately after prev game ends if input received.
  Timer otherInputReset; 
  Boolean intelHarmless;
  int humansPlayingPrev; // Just used for displaying info.
  int enemiesPlayingPrev;
  Boolean noItems; // Mostly for testing.
  Boolean oneEach;
  const char *oneItem; // Only initialize one item if not "".
  ClassId humanClass; // Choose randomly if A_None.
  Boolean enemiesRefill,enemiesRefillNext; // Keep regenerating enemies.
  GameObjects gameObjects;
  Timer refillTimer;
  Boolean pause;  // Is the game paused.
  UIsettings::Style style,styleNext;

  Scenario scenario; // Only meaningful for SCENARIOS game style.
  Scenario scenarioOverride; // Ignore if set to HIGHEST, from command line.
  Id frogs[SCENARIO_BONUS_FROGS]; // Only for BONUS scenario.
  int frogsRemaining;
  Id xitId; // Only meaningful for HIVE scenario.
  Id flagIds[SCENARIO_FLAG_FLAGS]; // Only meaningful for FLAG scenario.
  int flagsRemaining; // Only meaningful for FLAG scenario.
  Id homeId;  // Only for LEMMINGS.
  Id trapdoorId; // Only for LEMMINGS.
  int lemmingsSafe,lemmingsOut,lemmingsActive; // Only for LEMMINGS.
  Boolean dontAdvance;
  
  int enemyNameCount; // Number for next enemy name.  E.g. "enemy-8"

  /* For LEVELS, means time after killing enemies to next level.  All 
    others, means the total time for each level. */
  Timer newLevelTimer; 
  Boolean newLevelTime;

  int level; // Current level.
  int levelHighest; // Over all games.
  Timer levelTitleTimer; // Time for displaying the level title screen. 
  Boolean levelTitleTime;
  Boolean polCorrect; // Politically correct mode.
  Boolean demoOn; // Is a demo currently running.
  Timer demoExtraTimer; // Extra time before ending the demo.
  Boolean demoExtraTime;
  Timer demoCutoffTimer; // Maximum time for each demo.
};
#endif


