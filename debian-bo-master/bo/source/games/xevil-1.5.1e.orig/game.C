// "game.C"

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
#pragma implementation "game.h"
#endif


// Include Files
extern "C" {
#include <string.h>
#include <stdio.h>
}

#include <strstream.h>
#include <iomanip.h>

#include "utils.h"
#include "coord.h"
#include "world.h"
#include "id.h"
#include "intel.h"
#include "physical.h"
#include "actual.h"
#include "locator.h"
#include "ui.h"
#include "game.h"

extern "C" {
#include <X11/Xresource.h>
}



// Defines
#define VERSION "1.5.1e"

#ifdef XEVIL_KEYSET
#define KEYSET_DEFAULT XEVIL_KEYSET
#else
#define KEYSET_DEFAULT UIdecmips
#endif

#ifndef XEVIL_CLASS
#define XEVIL_CLASS "XEvil"
#endif

#define WORLD_ACROSS_MAX_DEFAULT 4
#define WORLD_DOWN_MAX_DEFAULT 2

#define QUANTA_DEFAULT 40 // In milliseconds
#define QUANTA_MAX 500
#define EXTRA_TURNS 50
#define DEMO_EXTRA_TURNS EXTRA_TURNS
#define OTHER_INPUT_RESET 80
#define HUMANS_NUM_DEFAULT 1
#define ENEMIES_NUM_DEFAULT 0 // 3
#define HUMAN_LIVES 3
#define HUMAN_LIVES_SCENARIOS 10
#define REFILL_TIME 200
#define EXTRA_ITEMS_MULTIPLIER 8
#define LEVEL_TIME 3000
#define NEW_LEVEL_TIME 50
#define LEVEL_ENEMIES_INITIAL 3 // Number of enemies on first level.
#define LEVEL_ENEMIES_INCR 2
#define LEVEL_TITLE_TIME 40
#define DEMO_CUTOFF_TIME 400
#define SCENARIO_BONUS_TIME 650
#define SCENARIO_HIVE_ALIENS 13
#define SCENARIO_HIVE_EGGS 8
#define SCENARIO_FLAG_ENEMIES 15
#define SCENARIO_LEMMINGS_ENEMIES 3

#define ALTAR_OF_SIN_CHANCE     10


// Old lame way.
#if 0
#define PISTOLS_PERCENT         .0004 
#define CHAINSAWS_PERCENT       .0002
#define M_GUNS_PERCENT          .0002
#define SWAPPERS_PERCENT        .0002
#define LANCERS_PERCENT         .0002
#define FROG_GUNS_PERCENT       .0002
#define F_THROWERS_PERCENT      .0002
#define LAUNCHERS_PERCENT       .0002
#define GRENADESS_PERCENT       .0002
#define STARSS_PERCENT          .0002
#define ROCKS_PERCENT           .001 
#define WEIGHTS_PERCENT         .0002
#define MED_KITS_PERCENT        .0003
#define BOMBS_PERCENT           .0006
#define N_SHIELDS_PERCENT       .00008
#define T_SHIELDS_PERCENT       .00008
#define TRANSMOGIFIERS_PERCENT  .00002
#define DOPPELS_PERCENT         .00002 
#define CLOAKS_PERCENT          .00002
#endif


char *Game::wittySayings[WITTY_SAYINGS_NUM] = {
  "If it moves it's a threat.  If it doesn't move it's a potential threat.",
    "Happy, happy.  Joy, joy.",
    "For the mother country!!!",
    "Hi ho.  Hi ho.  It's off to kill we go.",
    "Well, do you feel lucky, Punk?",
    "Wake up, time to die.",
    "Let's rock.",
    "I love the smell of napalm in the morning.",
    "How's it feel to be hunted?",
    "Go ahead, make my day.",
    "I'll be back.",
    "Won't you be my neighbor?",
    "3E.  We bring good things to death.",
    "We come in peace.  (Shoot to kill.  Shoot to kill.)",
    "Hey.  You can't do that.",
    "I'm here to chew bubble-gum and kick ass.  And I'm all out of bubble-gum.",
    "You're a useless asshole, Pogo-Man.",
    "Settle down, Beavis.",
    "Ho, ho, ho.  Now I have a machine gun.",
    "Aren't you dead yet?",
    "I don't give a fuck what you know.  I'm going to torture you anyway.",
    "I'm hungry.  Let's get a taco.",
    "I'm sorry, Dave.  I can't do that.",
    "Violence is the only true form of communication.",
    "Let the flames purify your soul!!!",
    "Fuck me gently with a chainsaw.",
    "He doesn't look like a quivering ball of white-hot canine fury.",
    "Hackito ergo sum.",
    "I'm singing in the rain...",
    "Washin' the dog!!  Washin' the dog!!",
    "ooooh.  That's gotta hurt.",
    "Kids, don't try this at home.  We're trained professionals.",
    "Oh, you want some of this?!  Fuck you!",
    "Well, he shoulda' armed himself!",
    "Fuck art.  Let's kill.",
    "If you can't eat it or fuck it, kill it.",
    "Skrotus lives in my fridge.",
    "I love you.  You love me.  We're a happy family.",
    "You always were an asshole, Gorman.",
    "Change it dude.  This sucks.",
    "The game of death, doom, and (more or less) ultimate destruction.",
    "I think I'll just Dieeeeeee.",
    "You know he'll feel that one in the morning.",
    "I've had this ice-cream bar since I was a child.",
    "Shut up or I will kill you.  I will physically kill you.",
    "Better stop before someone gets hurt.",
    "What??",
    "Whoa!!",
    "Jesus will love you if you give me money.",
    "Evil.  It's not just for breakfast anymore.",
    "XEvil.  The carpal tunnel game.",
    "Violence.  It's not just the best policy.  It's the only policy.",
    "Got ... to get ... to ... my arm.",
    "Who's the lucky one?",
    "Don't blink.  It would really suck if you blinked.",
    "Here kitty, kitty, kitty.",
    "Stay on target...",
    "Do not taunt Happy-Fun-Ball.",
    "Bring out the gimp.                            Gimp's sleepin.\nWell, I guess you'll just have to go wake him up now, won't you.",
    "That's easy.",
    "It's just a rabbit.",
    "Ignorance is the number one cause of happiness.",
    "What the heck's going on?  I thought I was a normal guy.",
    "Oh, no.  It's K-k-k-ken c-c-c-coming to k-k-k-kill me.",
    "You bloated sack of protoplasm.",
    "The weak exist only to amuse the strong.",
    "We're not hitch-hiking anymore.  We're riding.",
    "No, sir.  I didn't like it.",
    "Are we there yet?",
    "Kurt Cobain spoke for my generation.",
    "We're on a mission from God.",
    "Oh, I'm a lumberjack and I'm ok...",
    "I'm sorry.  You must have mistaken me for the clown that gives a damn.",
    "Another visitor.  Stay awhile.  STAY FOREVER!!!!",
    "I laughed.  I cried.  It was better than Cats.\nI'd see it again and again.",
    "SPOON!!!",
    "Only life can kill you.",
    "That which does not kill us, hurts.",
    "Hey, baby.  Wanna wrestle.",
    "Fuck you and the horse you rode in on.",
    "Spin, spin, spin, spin, spin, spin, spin...",
    "Dope will get you through times of no money\nbetter than money will get you through times of no dope.",
    "What he said.",
    "Like, take off, eh.",
    "Lord God, Leroy, that was the toughest meanest hippie I ever saw.",
    "Happy thoughts.  Happy thoughts. Happy thoughts.",
    "M*cr*s*ft:  Who need quality when you have marketing.",
    "The proof is left as an exercise.",
    "Ummm...  We have a problem.",
    "Come see the violence inherent in the system.\nHelp! Help! I'm being repressed.",
    "Hey, man, you a sniper?                                           Shut up.\nYou sure look like a sniper.                                      I said, shut up.",
    "Oh, I'm sorry.  Did that hurt?",
    "What me worry?",
    "Nobody expects the Spanish Inquisition!!!",
    "We are, we are, we are, we are, we are the engineers.\nWe can, we can, we can, we can demolish forty beers.",
    "When in doubt, shoot the lawyer.",
    "All that hate's gonna burn you up, boy.\nKeeps me warm.",
    "He lives in that piece of paper?\nNo, you idiot.  He lives in a regular house.",
    "I'll give you something to cry about.",
    "It's not a bug, it's a feature.",
    "Don't hate me because I'm beautiful.",
    "What a great game of cards this is.",
    "You're ugly and your mother dresses you funny.",
    "I can eat fifty eggs.",
    "What we have here is failure to communicate.",
    "Jesus loves me more than you.",
    "Are you threatening me!!!??",
    "I need TP for my bunghole.",
    "You're immune to both romance and mirth.  You must be a... a...\nThat's right.  I'm an engineer.",
    "Out Out!!  You demons of stupidity!!",
    "To infinity, and beyond!!!",
    "I love Bomb. It's nice.",
    "Marines!  We are leaving.",
    "Toad Sexing kicks ass.",
    "Let me try that again.",
    "I is a college graduate.",
    "FRIDAY!!! ... IN STEREO!!! ...",
    "I'll get you, my pretty.  And you're little dog, too!",
};



char *Game::intelNames[INTEL_NAMES_NUM] = {
  "Dr. Pain",
  "Steve",
  "hardts",
  "James E. Tetazoo",
  "Arnold",
  "Victor the Cleaner",
  "Hudson",
  "Hicks",
  "Twinkie Boy",
  "Bob",
  "Ben Bitdiddle",
  "Beavis",
  "Butt-Head",
  "Fred",
  "Mulch-Man",
  "Mike",
  "Tim",
  "Peter",
  "Alan",
  "Neal",
  "Tony",
  "Asshole",
  "Crazy Hairy",
  "Spaceman Spiff",
  "Kitty-Wench",
  "Juker",
  "Shrieks",
  "Albert Lin",
  "Jim Gouldstone",
  "Steph",
  "Paining",
  "Dim",
  "The Man",  
  "Twinkie Girl",
  "Mr. Buzzcut",  
  "Sensei Steve",
  "Ren",
  "Stimpy",
  "Mr. Horse",
  "Dilbert",
  "Dogbert",
  "Gromit",
  "The Tick",
  "Fat Freddie",
  "Phineas",
  "Freewheelin Franklin",
  "Doctor Who",
  "Celery-Head-Man",
  "Roo",
  "Shwang",
  "Leo",
  "Smoothie",
  "Hawaii Chick",
  "Cowboy Steve",
  "Outlaw Jim",
  "Prick",
  "Smack",
  "Hello, Kitty",
  "Wedge",
  "Redshirt",
  "Mistress Leah",
  "e^x Man",
  "John Rusnak",
  "Mr. Bonk",
  "Pixie of Pass/Fail",
  "Josh the 10-yr Old",
};




// Functions     
void GameStats::report()
{
  if (numTurns % Game::REPORT_TIME == 0)
    cout << "Turn " << numTurns << ":  Average time of turn:  " << 
      aveTime << endl;
}



void GameStats::clock()
{
  time_t oldPrevTime = prevTime;
  time(&prevTime);    

  if (numTurns)
    aveTime = (numTurns * aveTime + (prevTime - oldPrevTime)) / 
      (numTurns + 1); 
  
  numTurns++;
}



GameObjects::GameObjects(WorldP w,LocatorP l)
: world(w),locator(l)
{
  resetCalled = False;
  for (int n = 0; n < A_CLASSES_NUM; n++) {
    maximums[n] = 0;
    actuals[n] = 0;
    ids[n] = NULL;
  }
}



void GameObjects::game_reset(const char *one_item,
			     Boolean no_items,Boolean one_each)
{
  oneItem = one_item;
  noItems = no_items;
  oneEach = one_each;
  resetCalled = True;
  altarOfSinAlready = noItems; // If noItems, don't want an Altar of Sin.
  
//  level_reset(worldDim);
}



void GameObjects::level_reset(const Dim &worldDim)
{
  // WARNING: GameObjects forgets about old items.  Not a memory leak, but 
  // this may end up with more and more items coming into existence.

  // Initialize arrays.
  int n;
  for (n = 0; n < A_CLASSES_NUM; n++)
    {
      maximums[n] = 0;
      actuals[n] = 0;
      if (ids[n]) {
	delete [] ids[n];
	ids[n] = NULL;
      }
    }
  
  
  // Get arrays of potential weapons and potential otherItems.
  int weaponsNum;
  PhysicalContext *weapons[A_CLASSES_NUM];
  weaponsNum = locator->filter_contexts(weapons,NULL,
					potential_weapon_filter);
  int oItemsNum;
  PhysicalContext *oItems[A_CLASSES_NUM];
  oItemsNum = locator->filter_contexts(oItems,NULL,
				       potential_other_item_filter);
  

  // Normal case.
  if (!strlen(oneItem) && !noItems && !oneEach)
    {
      // Area of the world.
      int area = worldDim.rowMax * worldDim.colMax;

      // Set maximums[n] for all potential weapons, n.
      for (n = 0; n < weaponsNum; n++) {
	// Check not already set.
	assert(maximums[weapons[n]->classId] == 0);

	maximums[weapons[n]->classId] = 
	  (int)ceil(area * weapons[n]->objectWorldPercent);
      }

      // Set maximums[n] for all potential weapons, n.
      for (n = 0; n < oItemsNum; n++) {
	// Check not already set.
	assert(maximums[oItems[n]->classId] == 0);

	maximums[oItems[n]->classId] = 
	  (int)ceil(area * oItems[n]->objectWorldPercent);
      }
    }

  // If only one item in the Game.
  else if (strlen(oneItem))
    {
      ClassId classId = Game::parse_class_name(locator,oneItem);
      if (classId != A_None)
	maximums[classId] = 1;
    }

  // One of each object.
  else if (oneEach) {
    for (n = 0; n < weaponsNum; n++) {
	maximums[weapons[n]->classId] = 1;
      }

    for (n = 0; n < oItemsNum; n++) {
      maximums[oItems[n]->classId] = 1;
    }
  }

  // Compute actuals from maximums.
  compute_actuals(weapons,weaponsNum,oItems,oItemsNum);

  // Somewhat of a hack.  Always want maximum number when using oneItem or
  // oneEach.
  if (strlen(oneItem) || oneEach)
    for (n = 0; n < A_CLASSES_NUM; n++)
      actuals[n] = maximums[n];

  // Create the arrays of ids for each object type.
  // Only need space for actuals[n].
  for (n = 0; n < A_CLASSES_NUM; n++)
    if (actuals[n]) {
      Id invalid;
      ids[n] = new Id[actuals[n]];
      assert(ids[n]);
      for (int m = 0; m < actuals[n]; m++)
	ids[n][m] = invalid;
      // Explicitly initialize ids because different C++ compilers follow
      // different rules about constructing elements of an array.
    }
}



void GameObjects::refill()
{
  assert(resetCalled);

  // Create an AltarOfSin.
  if (!altarOfSinAlready && 
      Utils::choose(ALTAR_OF_SIN_CHANCE) == 0)
    {
      Pos pos = world->empty_rect(AltarOfSin::get_size_max());
      PhysicalP p = new AltarOfSin(world,locator,pos); 
      assert(p); 
      locator->add(p); 
      altarOfSinAlready = True;
    }

  int n;
  for (n = 0; n < A_CLASSES_NUM; n++)
    if (actuals[n]) {
      // We know that the actuals is indexed by classId.
      const PhysicalContext *cx = locator->get_context((ClassId)n);
      assert(cx);
      refill_helper(cx,n);
    }
}



// Ahh, no more #define REFILL_HELPER().  It's a real function now.
void GameObjects::refill_helper(const PhysicalContext *cx,int which)
{
  int existsNum = 0;
  int m;
  for (m = 0; m < actuals[which]; m++)
    if (locator->lookup(ids[which][m]))
      existsNum++;
  int newOnes = actuals[which] - existsNum;
  for (m = 0; newOnes > 0; m++)
    {
      assert(m < actuals[which]);
      if (!locator->lookup(ids[which][m]))
	{ 
	  Pos pos = world->empty_rect(cx->get_size_max());
	  PhysicalP p = cx->create(world,locator,pos);
	  locator->add(p);
	  ids[which][m] = p->get_id();
	  newOnes--;
	}
    }
}



void GameObjects::compute_actuals(const PhysicalContext *weapons[],
				  int weaponsNum,
				  const PhysicalContext *oItems[],
				  int oItemsNum)
{
  // Initialize all actuals to 0.
  int n;       
  for (n = 0; n < A_CLASSES_NUM; n++)
    actuals[n] = 0;

  // Number of weapon and otherItems classes to include in this level.
  int weaponsLevel = 2 + Utils::choose(5);  // 2 <= weaponsLevel <= 6
  int oItemsLevel = 1 + Utils::choose(5);  // 1 <= oItemsLevel <= 5
  assert(weaponsLevel <= weaponsNum && 
	 oItemsLevel <= oItemsNum);

  // Rank the weapon and other classes.
  int *weaponsRank = new int[weaponsNum];
  Utils::randomList(weaponsRank,weaponsNum);
  int *oItemsRank = new int[oItemsNum];
  Utils::randomList(oItemsRank,oItemsNum);

  // Set actuals to be non-zero for weaponsLevel number of classes.  Choose
  // those ranked highest according to weaponsRank.
  for (n = 0; n < weaponsLevel; n++)
    {
      int which = weapons[weaponsRank[n]]->classId;
      actuals[which] = 
	maximums[which] ? (Utils::choose(maximums[which]) + 1) : 0;
    }

  // Set actuals to be non-zero for oItemsLevel number of classes.  Choose
  // those ranked highest according to oItemsRank.
  for (n = 0; n < oItemsLevel; n++)
    {
      int which = oItems[oItemsRank[n]]->classId;
      actuals[which] = 
	maximums[which] ? (Utils::choose(maximums[which]) + 1) : 0;
    }


  delete [] weaponsRank;
  delete [] oItemsRank;
}



Boolean GameObjects::potential_weapon_filter(const PhysicalContext *cx)
{
  return cx->potentialWeapon;
}



Boolean GameObjects::potential_other_item_filter(const PhysicalContext *cx)
{
  return cx->potentialOtherItem;
}



// Old lame way.
#if 0
GameObjects::GameObjects(Boolean pol_correct)
{
  resetCalled = False;
  for (int n = 0; n < HIGHEST; n++)
    ids[n] = NULL;
  polCorrect = pol_correct;
}
void GameObjects::game_reset(const char *one_item,
			     Boolean no_items,Boolean one_each)
{
  oneItem = one_item;
  noItems = no_items;
  oneEach = one_each;
  resetCalled = True;
  altarOfSinAlready = noItems; // If noItems, don't want an Altar of Sin.
  
//  level_reset(worldDim);
}
void GameObjects::level_reset(const Dim &worldDim)
{
  // WARNING: GameObjects forgets about old items.  Not a memory leak, but 
  // this may end up with more and more items coming into existence.

  // Initialization.
  int n;
  for (n = 0; n < HIGHEST; n++)
    {
      maximums[n] = 0;
      if (ids[n])
	delete [] ids[n];
    }

  // Normal case.
  if (!strlen(oneItem) && !noItems && !oneEach)
    {
      int area = worldDim.rowMax * worldDim.colMax;

      maximums[PISTOLS] = (int)ceil(area * PISTOLS_PERCENT);
      maximums[CHAINSAWS] = (int)ceil(area * CHAINSAWS_PERCENT);
      maximums[M_GUNS] = (int)ceil(area * M_GUNS_PERCENT);
      maximums[SWAPPERS] = (int)ceil(area * SWAPPERS_PERCENT);
      maximums[LANCERS] = (int)ceil(area * LANCERS_PERCENT);
      maximums[FROG_GUNS] = (int)ceil(area * FROG_GUNS_PERCENT);
      maximums[F_THROWERS] = (int)ceil(area * F_THROWERS_PERCENT);
      maximums[LAUNCHERS] = (int)ceil(area * LAUNCHERS_PERCENT);
      maximums[GRENADESS] = (int)ceil(area * GRENADESS_PERCENT);
      maximums[STARSS] = (int)ceil(area * STARSS_PERCENT);
      maximums[ROCKS] = (int)ceil(area * ROCKS_PERCENT);
      maximums[WEIGHTS] = (int)ceil(area * WEIGHTS_PERCENT);
      maximums[MEDKITS] = (int)ceil(area * MED_KITS_PERCENT);
      maximums[BOMBS] = (int)ceil(area * BOMBS_PERCENT);
      maximums[N_SHIELDS] = (int)ceil(area * N_SHIELDS_PERCENT);
      maximums[T_SHIELDS] = (int)ceil(area * T_SHIELDS_PERCENT);
      maximums[TRANSMOGIFIERS] = (int)ceil(area * TRANSMOGIFIERS_PERCENT);
      maximums[DOPPELS] = (int)ceil(area * DOPPELS_PERCENT);
      maximums[CLOAKS] = (int)ceil(area * CLOAKS_PERCENT);
    }
  // Only one item in the Game.
  else if (strlen(oneItem))
    {
      if (!strcmp("rock",oneItem))
	maximums[ROCKS] = 1;
      else if (!strcmp("weight",oneItem))
	maximums[WEIGHTS] = 1;
      else if (!strcmp("med-kit",oneItem))
	maximums[MEDKITS] = 1;
      else if (!strcmp("bomb",oneItem))
	maximums[BOMBS] = 1;
      else if (!strcmp("chainsaw",oneItem))
	maximums[CHAINSAWS] = 1;
      else if (!strcmp("pistol",oneItem))
	maximums[PISTOLS] = 1;
      else if (!strcmp("machine-gun",oneItem))
	maximums[M_GUNS] = 1;
      else if (!strcmp("soul-swapper",oneItem))
	maximums[SWAPPERS] = 1;
      else if (!strcmp("lancer",oneItem))
	maximums[LANCERS] = 1;
      else if (!strcmp("frog-gun",oneItem))
	maximums[FROG_GUNS] = 1;
      else if (!strcmp("flame-thrower",oneItem))
	maximums[F_THROWERS] = 1;
      else if (!strcmp("launcher",oneItem))
	maximums[LAUNCHERS] = 1;
      else if (!strcmp("grenades",oneItem))
	maximums[GRENADESS] = 1;
      else if (!strcmp("stars",oneItem))
	maximums[STARSS] = 1;
      else if (!strcmp("n-shield",oneItem))
	maximums[N_SHIELDS] = 1;
      else if (!strcmp("t-shield",oneItem))
	maximums[T_SHIELDS] = 1;
      else if (!strcmp("transmogifier",oneItem))
	maximums[TRANSMOGIFIERS] = 1;
      else if (!strcmp("doppelganger",oneItem))
	maximums[DOPPELS] = 1;
      else if (!strcmp("cloak",oneItem))
	maximums[CLOAKS] = 1;
    }
  // One of each object.
  else if (oneEach)
    for (n = 0; n < HIGHEST; n++)
      maximums[n] = 1;

  // Create the arrays of ids for each object type.
  for (n = 0; n < HIGHEST; n++)
    {
      Id invalid;
      ids[n] = new Id[maximums[n]];
      assert(ids[n]);
      for (int m = 0; m < maximums[n]; m++)
	ids[n][m] = invalid;
      // Explicitly initialize ids because different C++ compilers follow
      // different rules about constructing elements of an array.
    }

  // Compute actuals from maximums.
  compute_actuals();

  // Somewhat of a hack.  Always want maximum number when using oneItem or
  // oneEach.
  if (strlen(oneItem) || oneEach)
    for (n = 0; n < HIGHEST; n++)
      actuals[n] = maximums[n];
}
/* EFFECTS:  Make it so that maximums[which] of the Item exist.  I.e. create
   (maximums[which] - <current number>) new Items. */
#define REFILL_HELPER(CLASS,which)\
{\
  int existsNum = 0;\
  PhysicalP dummy;\
  int m;\
  for (m = 0; m < maximums[which]; m++)\
    if (locator->lookup(dummy,ids[which][m]) == OL_NO_SIG)\
      existsNum++;\
  int newOnes = actuals[which] - existsNum;\
  for (m = 0; newOnes > 0; m++)\
    {\
      assert(m < maximums[which]);\
      if (locator->lookup(dummy,ids[which][m]) == OL_NOT_FOUND)\
	{ \
	  Pos pos = world->empty_rect(CLASS::get_size_max());\
	  PhysicalP p = new CLASS(world,locator,pos);\
	  assert(p);\
	  locator->add(p);\
	  ids[which][m] = p->get_id();\
	  newOnes--;\
	}\
    }\
}
void GameObjects::refill(WorldP world,LocatorP locator)
{
  assert(resetCalled);

  if (!polCorrect && !altarOfSinAlready && 
      Utils::choose(ALTAR_OF_SIN_CHANCE) == 0)
    {
      Pos pos = world->empty_rect(AltarOfSin::get_size_max());
      PhysicalP p = new AltarOfSin(world,locator,pos); 
      assert(p); 
      locator->add(p); 
      altarOfSinAlready = True;
    }
      
  REFILL_HELPER(Rock,ROCKS);
  REFILL_HELPER(Weight,WEIGHTS);
  REFILL_HELPER(MedKit,MEDKITS);
  REFILL_HELPER(Bomb,BOMBS);
  REFILL_HELPER(Chainsaw,CHAINSAWS);
  REFILL_HELPER(Pistol,PISTOLS);
  REFILL_HELPER(MGun,M_GUNS);
  REFILL_HELPER(Swapper,SWAPPERS);
  REFILL_HELPER(Lancer,LANCERS);
  REFILL_HELPER(FrogGun,FROG_GUNS);
  REFILL_HELPER(FThrower,F_THROWERS);
  REFILL_HELPER(Launcher,LAUNCHERS);
  REFILL_HELPER(Grenades,GRENADESS);
  REFILL_HELPER(Stars,STARSS);
  REFILL_HELPER(NShield,N_SHIELDS);
  REFILL_HELPER(TShield,T_SHIELDS);
  REFILL_HELPER(Transmogifier,TRANSMOGIFIERS);
  REFILL_HELPER(Doppel,DOPPELS);
  REFILL_HELPER(Cloak,CLOAKS);
}
#undef REFILL_HELPER
void GameObjects::compute_actuals()
{
  const int O_HIGHEST = HIGHEST - W_HIGHEST;

  // Initialize all actuals to 0.
  int n;       
  for (n = 0; n < HIGHEST; n++)
    actuals[n] = 0;

  // Number of weapon and other classes to include in this level.
  int wClassesNum = 2 + Utils::choose(5);  // 2 <= wClassesNum <= 6
  int oClassesNum = 1 + Utils::choose(5);  // 1 <= oClassesNum <= 5
  assert(wClassesNum < W_HIGHEST && oClassesNum < O_HIGHEST);

  // Rank the weapon and other classes.
  int wRank[W_HIGHEST];
  Utils::randomList(wRank,W_HIGHEST);
  int oRank[O_HIGHEST];
  Utils::randomList(oRank,O_HIGHEST);

  // Set actuals to be non-zero for the first wClassesNum in wRank.
  for (n = 0; n < wClassesNum; n++)
    {
      int which = wRank[n];
      actuals[which] = 
	maximums[which] ? (Utils::choose(maximums[which]) + 1) : 0;
    }

  // Set actuals to be non-zero for the first oClassesNum in oRank.
  for (n = 0; n < oClassesNum; n++)
    {
      int which = oRank[n] + W_HIGHEST;
      actuals[which] = 
	maximums[which] ? (Utils::choose(maximums[which]) + 1) : 0;
    }
}
#endif // Old lame way.



Game::Game(int *arg_c,char **arg_v)
     : world(is_pol_correct(arg_c,arg_v)), 
       locator(&world),
       ui(arg_c,arg_v,&world,&locator,display_names(arg_c,arg_v),
	  is_pol_correct(arg_c,arg_v)),
       gameObjects(&world,&locator)
{ 
  polCorrect = is_pol_correct(arg_c,arg_v);

  argc = arg_c;
  argv = arg_v;

  gameOn = False;
  demoOn = True;

  Timer t1(EXTRA_TURNS);
  extraTimer = t1;
  extraTime = False;

  Timer t11(DEMO_EXTRA_TURNS);
  demoExtraTimer = t11;
  demoExtraTime = False;

  Timer t2(OTHER_INPUT_RESET);
  otherInputReset = t2;

  Timer t3(REFILL_TIME);
  refillTimer = t3;
  refillTimer.set();

  newLevelTime = False;

  Timer t5(LEVEL_TITLE_TIME);
  levelTitleTimer = t5;
  levelTitleTime = False;

  Timer t6(DEMO_CUTOFF_TIME);
  demoCutoffTimer = t6;

  pause = False;
  style = styleNext = UIsettings::LEVELS;
  ui.set_style(style);

  scenarioOverride = HIGHEST; // I.e. ignore the override. 

  humansNum = 0;
  humansNumNext = HUMANS_NUM_DEFAULT;
  ui.set_humans_num(humansNumNext);
  // Before parse_args.
  for (int n = 0; n < Locator::HUMANS_MAX; n++)
    humanNames[n] = "";
  
  enemiesNum = 0;
  // Must be before parse_args.
  enemiesNumNext = ENEMIES_NUM_DEFAULT;
  ui.set_enemies_num(enemiesNumNext);

  enemiesRefill = enemiesRefillNext = False;
  ui.set_enemies_refill(False);

  quanta = QUANTA_DEFAULT;
  ui.set_quanta(quanta);

  humansPlayingPrev = enemiesPlayingPrev = 0;
  enemyNameCount = 0;

  worldRooms.downMax = WORLD_DOWN_MAX_DEFAULT;
  worldRooms.acrossMax = WORLD_ACROSS_MAX_DEFAULT;

  intelHarmless = False;
  showStats = False;
  dontAdvance = False;

  noItems = False;
  oneEach = False;
  oneItem = "";

  humanClass = A_None;

  parse_args(argc,argv);

  // Now in Game::reset.
  // world.set_rooms_next(worldRooms);

  // Must be before first call to Ui::set_input.
  process_x_resources(argc,argv);			
  ui.set_input(0,UI_KEYS_RIGHT);

  intro();
  locator.set_messages_ignore(True); // Must be after intro().

  demoCutoffTimer.set();
  demo_setup();
}



void Game::clock()
{
  Boolean shouldReset = False;

  ui_settings_check(shouldReset);
  
  // Must be before ui.clock().
  if (!pause && !gameOn && otherInputReset.ready() && ui.other_input())
    shouldReset = True;
  
  ui.clock();
  
  if (!pause)
    {
      /* Put after ui.clock so doesn't draw screen twice on reset.  Intel needs
	 to be clocked to get id so that ui can follow the id of the intel. */
      if (shouldReset)
	reset();
      
      if (gameOn)
	{
	  if (levelTitleTime)
	    {
	      levelTitleTimer.clock();
	      if (levelTitleTimer.ready())
		{
		  ui.unset_level_title();
		  levelTitleTime = False;
		}
	    }
	  else
	    {
	      world.clock();
	      locator.clock();  
	      
	      reincarnations_check();
	      
	      int humansPlaying = locator.humans_playing();
	      int enemiesPlaying = locator.enemies_alive();
	      
	      if (humansPlaying != humansPlayingPrev)
		{
		  humansPlayingPrev = humansPlaying;
		  ui.set_humans_playing(humansPlaying);
		}
	      
	      if (enemiesPlaying != enemiesPlayingPrev)
		{
		  enemiesPlayingPrev = enemiesPlaying;
		  ui.set_enemies_playing(enemiesPlaying);
		}

	      game_over_check(humansPlaying,enemiesPlaying);
	      
	      new_level_check(enemiesPlaying);
	      
	      if (showStats)
		stats.report();
	      stats.clock();

	      refill_check();

	    } /* if (levelTitleTime) */
	} /* if (gameOn) */
      else
	{
	  otherInputReset.clock();
	  demo_check();
	}
    } /* if (!pause) */
}



ClassId Game::parse_class_name(LocatorP l,const char *name)
{
  // Linear search.
  ClassId n;
  for (n = 0; n < A_CLASSES_NUM; n++)
    {
      const PhysicalContext *context = l->get_context(n);
      if (context && !strcmp(context->className,name))
	return n;
    }
  return A_None;
}



void Game::ui_settings_check(Boolean &shouldReset)
{
  if (ui.settings_changed())
    {
      UIsettings settings;
      UImask mask = ui.get_settings(settings);
      
      if (mask & UInewGame)
	shouldReset = True;

      if (mask & UIquit)
	{
	  print_stats();
	  exit(0);
	}

      if (mask & UIhumansNum)
	{
	  humansNumNext = min(settings.humansNum,Locator::HUMANS_MAX);
	  ui.set_humans_num(humansNumNext);
	}

      if (mask & UIenemiesNum)
	{
	  enemiesNumNext = min(settings.enemiesNum,Locator::ENEMIES_MAX);
	  ui.set_enemies_num(enemiesNumNext);
	}

      if (mask & UIenemiesRefill)
	{
	  enemiesRefillNext = settings.enemiesRefill;
	  ui.set_enemies_refill(enemiesRefillNext);
	}

      if (mask & UIpause)
	{
	  pause = settings.pause;
	  ui.set_pause(pause);
	}

      if (mask & UIstyle)
	{
	  ostrstream str;
	  styleNext = settings.style;
	  ui.set_style(settings.style);
	  switch (styleNext) {
	  case UIsettings::SCENARIOS:
	    str << "A number of different scenarios." << "\n"
		<< "You must complete each scenario to continue on to the "
		<< "next one." << ends;
	    break;
	  case UIsettings::LEVELS:
	    str << "Human player(s) fights through increasing levels of "
		<< "difficulty." << "\n" 
		<< "To complete a level you must kill all enemy players." 
		<< ends;
	    break;
	  case UIsettings::KILL:
	    str << "Every human and machine for him/her/itself." << ends;
	    break;
	  case UIsettings::DUEL:
	    str << "Human vs. human battle to the death." 
		<< "\n" << "Each human has 3 lives." << ends;
	    break;
	  case UIsettings::EXTENDED:
	    str << "Human vs. human battle to the death." 
		<< "\n" << "Unlimited lives." << ends;
	    break;
	  case UIsettings::TRAINING:
	    str << "One human plays alone.  Useful for learning the "
		<< "controls." << ends;
	    break;
	  };
	  locator.message_enq(str.str());
	}
      
      if (mask & UIquanta)
	{
	  quanta = settings.quanta;
	  if (quanta < 0)
	    quanta = 0;
	  if (quanta > QUANTA_MAX)
	    quanta = QUANTA_MAX;
	  ui.set_quanta(quanta);
	}
    }
}



void Game::reincarnations_check()
{
  Incarnator iter(locator);
  HumanP human;
  while (human = iter())
    {
      PhysicalP obj = human_physical();
      locator.add(obj);
      human->reincarnate();
      obj->set_intel(human);
      
      ostrstream msg;
      msg << human->get_name() << " is back from the dead." << ends;
      locator.message_enq(msg.str());
    }
}



void Game::game_over_check(int humansPlaying,int enemiesPlaying)
{
  if (!extraTime && 
      (((style == UIsettings::SCENARIOS || 
	 style == UIsettings::LEVELS) && 
	humansPlaying == 0) ||
       (style == UIsettings::KILL && 
	humansPlaying + enemiesPlaying <= 1) ||
       ((style == UIsettings::DUEL || 
	 style == UIsettings::EXTENDED) &&
	humansPlaying <= 1) ||
       (style == UIsettings::TRAINING &&
	humansPlaying == 0)))
    {
      // End the game.
      extraTime = True;
      extraTimer.set();
    }

  if (extraTime && extraTimer.ready())
    end_game();
  if (extraTime)
    extraTimer.clock();
}      



void Game::new_level_check(int enemiesPlaying)
{
  if (style == UIsettings::SCENARIOS)
    {
      if (!newLevelTime && scenario == EXTERMINATE && enemiesPlaying == 0)
	{
	  newLevelTime = True;
	  newLevelTimer.set(NEW_LEVEL_TIME);
	}
      else if (scenario == BONUS)
	{
	  // Count number of Frogs still alive.
	  int remaining = 0;
	  for (int n = 0; n < SCENARIO_BONUS_FROGS; n++)
	    {
	      PhysicalP p = locator.lookup(frogs[n]);
	      if (p && p->alive())
		remaining++;
	    }
	  
	  // Update Ui.
	  if (remaining != frogsRemaining)
	    {
	      frogsRemaining = remaining;
	      ostrstream str;
	      str << "[" << level << "] BONUS LEVEL\nfrogs remaining: " 
		<< remaining << ends;
	      ui.set_level(str.str());
	      delete str.str();

	      // Will only be called once per level.
	      if (frogsRemaining == 0)
		{
		  ostrstream ostr;
		  ostr << "***** You must feel proud of yourself for killing all those defenseless frogs. *****" << ends;
		  locator.message_enq(ostr.str());
		  assert(newLevelTime);
		  newLevelTimer.set(NEW_LEVEL_TIME);
		}
	    }	  
	}
      else if (!newLevelTime && scenario == HIVE)
	{
	  PhysicalP xit = locator.lookup(xitId);
	  assert(xit); // The Xit should never be destroyed.
	  if (((TouchableP)xit)->wasTouched())
	    {
	      newLevelTimer.set(0);
	      newLevelTime = True;
	    }
	}
      else if (!newLevelTime && scenario == FLAG)
	{
	  int fRemaining = 0;
	  for (int n = 0; n < SCENARIO_FLAG_FLAGS; n++)
	    {
	      PhysicalP p = locator.lookup(flagIds[n]);
	      if (p)
		{
		  fRemaining++;
		  if (((TouchableP)p)->wasTouched())
		    off_clock_kill(p);
		}
	    }
	  if (fRemaining == 0) // All flags are gone, so end level.
	    {
	      newLevelTime = True;
	      newLevelTimer.set(0);
	    }
	  if (fRemaining != flagsRemaining)
	    {
	      flagsRemaining = fRemaining;
	      ostrstream levelStr;
	      levelStr << "[" << level << "] COLLECT " << SCENARIO_FLAG_FLAGS
		<< " FLAGS\nremaining: " << flagsRemaining << ends;
	      ui.set_level(levelStr.str()); 
	      delete levelStr.str();
	    }
	}
      else if (!newLevelTime && scenario == LEMMINGS)
	{
	  // NOTE: Lemming counts may be a little screwy if other Physicals
	  // have soul-swapped with some of the Lemmings.  
	  
	  HomeP home = (HomeP)locator.lookup(homeId);
	  TrapdoorP trapdoor = (TrapdoorP)locator.lookup(trapdoorId);
	  assert(home && trapdoor);

	  // Count lemmings out, active, and safe.
	  IntelId *lemmings; // Care about LemmingIntels, not Lemmings.
	  int lemOut = trapdoor->get_lemmings_out(lemmings);
	  int lemSafe = home->get_lemmings_safe();
	  int lemActive = 0;
	  for (int n = 0; n < lemOut; n++)
	    {
	      IntelP lemmingIntel = locator.lookup(lemmings[n]);	      
	      if (lemmingIntel)
		{
		  PhysicalP lemming = locator.lookup(lemmingIntel->get_id());
		  if (lemming && lemming->alive())
		    lemActive++;
		}
	    }

	  // Number Dead depends only on Out, Safe, and Active, so don't bother
	  // keeping it from turn to turn.  I.e. no Game::lemmingsDead.
	  int lemDead = lemOut - lemSafe - lemActive;
	  
	  // Update the Ui if necessary.
	  if (lemSafe != lemmingsSafe || lemOut != lemmingsOut ||
	      lemActive != lemmingsActive)
	    {
	      lemmingsSafe = lemSafe;
	      lemmingsOut = lemOut;
	      lemmingsActive = lemActive;
	      ostrstream levelStr;
	      levelStr << "[" << level << "] SAVE " 
		<< SCENARIO_LEMMINGS_NEED << " LEMS\n" 
		  << "out: " << lemmingsOut << " safe: " << lemmingsSafe
		    << " dead: " << lemDead
		      << ends;
	      ui.set_level(levelStr.str()); 
	      delete levelStr.str();
	    }

	  // New level if enough lemmings are safe or too many are dead.
//	  if ((lemOut == SCENARIO_LEMMINGS_LEMMINGS && lemActive == 0) ||
//	      lemSafe >= SCENARIO_LEMMINGS_NEED)
	  if (lemDead > SCENARIO_LEMMINGS_LEMMINGS - SCENARIO_LEMMINGS_NEED
	      || lemSafe >= SCENARIO_LEMMINGS_NEED)
	    {
	      if (lemSafe < SCENARIO_LEMMINGS_NEED)
		{
		  ostrstream msg;
		  msg << "YOU FAILED TO SAVE " << SCENARIO_LEMMINGS_NEED
		    << " LEMS.\nTry this level again." << ends;
		  locator.message_enq(msg.str());

		  dontAdvance = True;
		}

	      newLevelTime = True;
	      newLevelTimer.set(NEW_LEVEL_TIME);
	    }	      
	}
      else if (!newLevelTime && scenario == FIRE_DEMON 
	       && enemiesPlaying == 0)
	{
	  newLevelTime = True;
	  newLevelTimer.set(NEW_LEVEL_TIME);
	}
    }


  // LEVELS game style.
  if (!newLevelTime && style == UIsettings::LEVELS && 
      enemiesPlaying == 0)
    {
      newLevelTime = True;
      newLevelTimer.set(NEW_LEVEL_TIME);
    }


  // Call new_level() when newLevelTimer has expired.
  if (newLevelTime)
    {
      if (newLevelTimer.ready())
	{
	  newLevelTime = False;
	  new_level();
	}
      else
	newLevelTimer.clock();
    }
}



void Game::refill_check()
{
  if (refillTimer.ready())
    {
      if (!(style == UIsettings::SCENARIOS && scenario == BONUS))
	gameObjects.refill();
      
      if (enemiesRefill &&
	  (style == UIsettings::KILL || 
	   style == UIsettings::DUEL
	   || style == UIsettings::EXTENDED))
	refill_enemies();
      else if (style == UIsettings::SCENARIOS && scenario == HIVE)
	hive_refill_enemies();
      else if (style == UIsettings::SCENARIOS && scenario == FLAG)
	flag_refill_enemies();
      else if (style == UIsettings::SCENARIOS && scenario == LEMMINGS)
	lemmings_refill_enemies();

      refillTimer.set();
    }
  refillTimer.clock();
}



void Game::demo_check()
{
  if (demoOn)
    {
      if (!demoExtraTime && locator.enemies_alive() <= 1)
	{
	  demoExtraTimer.set();
	  demoExtraTime = True;
	}
      
      // Start new demo.
      if (demoCutoffTimer.ready() || 
	  (demoExtraTime && demoExtraTimer.ready()))
	{
	  demoCutoffTimer.set();
	  demoExtraTime = False;
	  locator.reset();
	  ui.reset();
	  demo_setup();
	}
      
      demoCutoffTimer.clock();
      demoExtraTimer.clock();
      world.clock();
      locator.clock();
    }		
}



Boolean Game::potential_human_filter(const PhysicalContext *pc)
{
  return pc->potentialHuman;
}



// New version which uses Locator::filter_contexts.
// More general and easy to extend.
PhysicalP Game::human_physical(int humanNum)
{
  // The class of the human to be created.
  const PhysicalContext *theContext;

  if (humanClass == A_None ||
      // Hack so that nonzero numbered humans are always chosen randomly.
      humanNum != 0) 
    {
      // Choose class randomly.

      // Get list of all classes that are potential Human classes.
      PhysicalContext *list[A_CLASSES_NUM];
      int size = locator.filter_contexts(list,NULL,potential_human_filter);
      assert(size);
      theContext = list[Utils::choose(size)];
    }
  else 
    {
      // Class has already been chosen.
      theContext = locator.get_context(humanClass);
    }


  // Create an object of the class corresponding to theContext.
  Pos pos = world.empty_rect(theContext->get_size_max());
  PhysicalP obj = theContext->create(&world,&locator,pos);
  assert(obj);
  return obj;
}



#if 0
PhysicalP Game::human_physical(int humanNum)
{
  PhysicalP obj;
  int which = -1;
  if (humanClass == A_None ||
      // Hack so that nonzero numbered humans are always chosen randomly.
      humanNum != 0) 
    which = Utils::choose(4);
    // The argument to Utils::choose is the number of human classes that
    // can be randomly selected from.  There are more possibilities
    // if the "-human_class" switch is used.

  if (humanClass == A_Enforcer)
    {
      Pos pos = world.empty_rect(Enforcer::get_size_max());
      obj = new Enforcer(&world,&locator,pos);
    }
  else if (humanClass == A_Frog)
    {
      Id invalid;
      Pos pos = world.empty_rect(Frog::get_size_max());
      obj = new Frog(&world,&locator,pos,invalid);
    }
  else if (humanClass == A_Hero || which == 0)
    {
      Pos pos = world.empty_rect(Hero::get_size_max());
      obj = new Hero(&world,&locator,pos);
    }
  else if (humanClass == A_Ninja || which == 1)
    {
      Pos pos = world.empty_rect(Ninja::get_size_max());
      obj = new Ninja(&world,&locator,pos);
    }
  else if (humanClass == A_ChopperBoy || which == 2)
    {
      Pos pos = world.empty_rect(ChopperBoy::get_size_max());
      obj = new ChopperBoy(&world,&locator,pos);
    }
  else if (humanClass == A_Alien)
    {
      Pos pos = world.empty_rect(Alien::get_size_max());
      obj = new Alien(&world,&locator,pos);
    }
  else if (humanClass == A_FireDemon)
    {
      Pos pos = world.empty_rect(FireDemon::get_size_max());
      obj = new FireDemon(&world,&locator,pos);
    }
  else if (humanClass == A_RedHugger)
    {
      Pos pos = world.empty_rect(RedHugger::get_size_max());
      obj = new RedHugger(&world,&locator,pos);
    }
  else if (humanClass == A_GreenHugger)
    {
      Pos pos = world.empty_rect(GreenHugger::get_size_max());
      obj = new GreenHugger(&world,&locator,pos);
    }
  else if (humanClass == A_Walker || which == 3)
    {
      Pos pos = world.empty_rect(Walker::get_size_max());
      obj = new Walker(&world,&locator,pos);
    }
  else
    assert(0);


  assert(obj);
  return obj;
}
#endif



Boolean Game::potential_enemy_filter(const PhysicalContext *pc)
{
  return pc->potentialEnemy;
}



// New version which uses Locator::filter_contexts.
// More general and easy to extend.
PhysicalP Game::enemy_physical(ClassId *choosable,int choosableNum)
{
  ClassId localChoosable[A_CLASSES_NUM];

  if (choosableNum == 0) 
    {
      // Ask the Locator for potential enemies since none were passed in.
      assert(!choosable);
      choosable = localChoosable;
      choosableNum = locator.filter_contexts(NULL,choosable,
					     potential_enemy_filter);
    }

  // Fill in weights.
  int weights[A_CLASSES_NUM];
  int n;
  for (n = 0; n < choosableNum; n++)
    {
      const PhysicalContext *context = locator.get_context(choosable[n]);
      assert(context);
      weights[n] = context->enemyWeight;
    }

  // Sum of all weights.
  int weightsSum = 0;
  for (n = 0; n < choosableNum; n++)
    weightsSum += weights[n];
  
  // Choose random weight in [0,weightsSun).
  int choice = Utils::choose(weightsSum);

  // weightBelow is always the total amount of weight of the classes from
  // 0 to n.  When this exceeds the chosen weight, create a Physical of 
  // class n.
  int weightBelow = 0;
  for (n = 0; n < choosableNum; n++)
    {
      weightBelow += weights[n];
      if (choice < weightBelow)
	return enemy_physical(choosable[n]);
    }
  assert(0);
  return NULL;
}



#if 0
// The default set of classes to be used by enemy_physical.
ClassId Game::defaultChoosable[DEFAULT_CHOOSABLE_NUM] =
{ A_Ninja,A_Hero,A_ChopperBoy,A_Enforcer,A_Alien,A_FireDemon,A_Walker};

int Game::defaultChoosableWeights[DEFAULT_CHOOSABLE_NUM] =
{ 3,      3,     2,           1,         1,      0,          2,      };


PhysicalP Game::enemy_physical(ClassId *choosable,int choosableNum)
{
  int localWeights[DEFAULT_CHOOSABLE_NUM];
  int *weights;

  if (choosableNum == 0) // Use defaults
    {
      choosable = defaultChoosable;
      choosableNum = DEFAULT_CHOOSABLE_NUM;
      weights = defaultChoosableWeights;
    }
  else
    {
      assert(choosable);
      weights = localWeights;
      // Get weights from defaultChoosableWeights.
      for (int n = 0; n < choosableNum; n++)
	for (int m = 0; m < DEFAULT_CHOOSABLE_NUM; m++)
	  if (choosable[n] == defaultChoosable[m])
	    weights[n] = defaultChoosableWeights[m];
    }

  int weightsSum = 0;
  int n;
  for (n = 0; n < choosableNum; n++)
    weightsSum += weights[n];
  
  int choice = Utils::choose(weightsSum);
  int weightBelow = 0;
  for (n = 0; n < choosableNum; n++)
    {
      weightBelow += weights[n];
      if (choice < weightBelow)
	return enemy_physical(choosable[n]);
    }
  assert(0);
  return NULL;
}
#endif



// New more general version.
PhysicalP Game::enemy_physical(ClassId classId)
{
  const PhysicalContext *context = locator.get_context(classId);
  assert(context);

  Pos pos = world.empty_rect(context->get_size_max());
  PhysicalP obj = context->create(&world,&locator,pos);
  return obj;
}



#if 0
// Old version
PhysicalP Game::enemy_physical(ClassId classId)
{
  assert(classId == A_Ninja || classId == A_Hero 
	 || classId == A_ChopperBoy || classId == A_Enforcer 
	 || classId == A_Alien || classId == A_Frog 
	 || classId == A_FireDemon || classId == A_Walker);

  PhysicalP obj;
  switch (classId) {
  case A_Ninja:
    {
      Pos pos = world.empty_rect(Ninja::get_size_max());
      obj = new Ninja(&world,&locator,pos);
    }
    break;
  case A_Hero:
    {
      Pos pos = world.empty_rect(Hero::get_size_max());
      obj = new Hero(&world,&locator,pos);
    }
    break;
  case A_ChopperBoy:
    {
      Pos pos = world.empty_rect(ChopperBoy::get_size_max());
      obj = new ChopperBoy(&world,&locator,pos);
    }
    break;
  case A_Enforcer:
    {
      Pos pos = world.empty_rect(Enforcer::get_size_max());
      obj = new Enforcer(&world,&locator,pos);
    }
    break;
  case A_Alien:
    {
      Pos pos = world.empty_rect(Alien::get_size_max());
      obj = new Alien(&world,&locator,pos);
    }
    break;
  case A_Frog:
    {
      Id invalid;
      Pos pos = world.empty_rect(Frog::get_size_max());
      obj = new Frog(&world,&locator,pos,invalid);
    }
    break;
  case A_FireDemon:
    {
      Pos pos = world.empty_rect(FireDemon::get_size_max());
      obj = new FireDemon(&world,&locator,pos);
    }
    break;
  case A_Walker:
    {
      Pos pos = world.empty_rect(Walker::get_size_max());
      obj = new Walker(&world,&locator,pos);
    }
    break;
  default:
    assert(0);
  }

  assert(obj);
  return obj;
}
#endif


Game::Scenario Game::choose_scenario()
{
  if (scenarioOverride != HIGHEST)
    return scenarioOverride;
  else
    return (Scenario)Utils::choose(HIGHEST);
}



void Game::end_game()
{
  gameOn = False;

  ostrstream winners;
  winners << "Game Over.  Winners: ";
  int winnersNum = 0;
  for (int n = 0; n < humansNum; n++)
    {
      HumanP human = locator.get_human(n);

      if (human->alive() || human->get_lives() == IT_INFINITE_LIVES 
	  || human->get_lives() > 0)
	{
	  winners << human->get_name() << "(" << human->get_human_kills() 
	    << " human(s), " << human->get_enemy_kills() << " machine(s)) ";
	  winnersNum++;
	}
    }
  if (winnersNum)
    {
      winners << ends;
      locator.message_enq(winners.str());
    }
  else if (locator.enemies_alive())
    {
      ostrstream str;
      if (humansNum)
	str << "Ha, ha.  You puny fleshlings can never defeat your " <<
	  "computer masters." << ends;
      else
	str << "The game is over." << ends;
      locator.message_enq(str.str());
    }
  else
    {
      ostrstream str;
      if (polCorrect)
	str << "Game Over.  Everybody is dead.  You all suck." << ends;
      else
	str << "Game Over.  Everybody is dead." << ends;
      locator.message_enq(str.str());
    }
  
  otherInputReset.set();
}



void Game::new_level(Boolean fromReset)
{
  // dontAdvance should only be used for SCENARIOS game style.
  assert(!dontAdvance || style == UIsettings::SCENARIOS);

  if (!dontAdvance)
    level++;

  if (level > levelHighest)
    levelHighest = level;

  ostrstream lStr;       // For level box on the side of the Ui.
  ostrstream lTitleStr;  // For Ui title screen.

  // Switch on the current game style.
  switch(style) {
  case UIsettings::SCENARIOS:
    {
      if (!dontAdvance)
	scenario = choose_scenario();
      else
	dontAdvance = False;

      // Switch on the possible scenarios.
      switch(scenario) {
      case EXTERMINATE:
	world.set_rooms_next(worldRooms);
	break;
      case BONUS:
	{
	  Rooms rooms(2,1);
	  world.set_rooms_next(rooms);
	}
	break;
      case HIVE:
	{
	  Rooms rooms(5,5);
	  world.set_rooms_next(rooms);
	}
	break;
      case FLAG:
	{
	  Rooms rooms(3,6);
	  world.set_rooms_next(rooms);
	}
	break;
      case LEMMINGS:
	{
	  Rooms rooms(1,4);
	  world.set_rooms_next(rooms);
	}
	break;
      case FIRE_DEMON:
	{
	  Rooms rooms(2,2);
	  world.set_rooms_next(rooms);
	}
	break;
      default:
	assert(0);
	break;
      };

      world.reset();
      

      // Clean up all Physicals in the Game.  World must be reset so 
      // that human players can be moved to the inside of the world.
      PhysicalIter pIter(locator);
      PhysicalP p;
      while (p = pIter())
	{
	  IntelP intel = p->get_intel();
	  Boolean saveMe = False;

	  // Save Human controlled physicals.
	  if (intel && intel->is_human())
	    saveMe = True;

	  // Save Machines that are slaves of Humans.
	  if (intel && !intel->is_human())
	    {
	      IntelP master = 
		locator.lookup(((MachineP)intel)->get_master_intel_id());
	      if (master && master->is_human())
		saveMe = True;
	    }

	  // Move to a new location inside the World.
	  if (saveMe)
	    {
	      assert(p->is_moving());
	      const Area &area = p->get_area();
	      Size size;
	      Pos pos;
	      area.get_rect(pos,size);
	      ((MovingP)p)->relocate(world.empty_rect(size));
	    }
	  // Kill Items not held by Human players.
	  else if (p->is_item())
	    {
	      Id holderId = ((ItemP)p)->get_holder_id();
	      PhysicalP holder = locator.lookup(holderId);
	      if (!(holder && (intel = holder->get_intel())
		    && intel->is_human()))
		off_clock_kill(p);
	    }
	  // Quietly kill everything else, ha, ha, ha.
	  else
	    off_clock_kill(p);
	}


      // Switch on the possible scenarios.
      switch(scenario) {
      case EXTERMINATE:
	{
	  lTitleStr << "[" << level << "] EXTERMINATE" << ends;
	  lStr << "[" << level << "] EXTERMINATE\nKill all machines." << ends;
	  enemiesNum += LEVEL_ENEMIES_INCR;
	  for (int m = 0; m < enemiesNum; m++)
	    create_enemy();
	}
	break;
      case BONUS:
	{
	  for (int n = 0; n < SCENARIO_BONUS_FROGS; n++)
	    {
	      PhysicalP p = create_enemy(enemy_physical(A_Frog));
	      frogs[n] = p->get_id();
	    }
	  frogsRemaining = SCENARIO_BONUS_FROGS;
	  newLevelTimer.set(SCENARIO_BONUS_TIME);
	  newLevelTime = True;
	  lTitleStr << "[" << level << "] BONUS LEVEL" << ends;
	  lStr << "[" << level << "] BONUS LEVEL\nfrogs remaining: " 
	    << SCENARIO_BONUS_FROGS << ends;
	}
	break;
      case HIVE:
	{
	  // Aliens are created in Game::clock.
	  lTitleStr << "[" << level << "] HIVE" << ends;
	  lStr << "[" << level << "] HIVE.\nFind the exit." << ends;
	  
	  // Create the Xit.
	  Pos pos = world.empty_accessible_rect(Xit::get_size_max());
	  PhysicalP p = new Xit(&world,&locator,pos);
	  assert(p);
	  locator.add(p);
	  xitId = p->get_id();

	  // Create some eggs.
	  IntelOptions eggOptions;
	  ITmask eggMask = intel_options_for(eggOptions,A_Alien);

	  for (int n = 0; n < SCENARIO_HIVE_EGGS; n++)
	    {
	      Pos pos = world.empty_rect(Egg::get_size_max());
	      PhysicalP egg = new Egg(&world,&locator,pos,
				      eggOptions,eggMask);
	      assert(egg);
	      locator.add(egg);
	    }
	}
	break;
      case FLAG:
	{
	  for (int n = 0; n < SCENARIO_FLAG_FLAGS; n++)
	    {
	      Pos pos = world.empty_accessible_rect(Flag::get_size_max());
	      PhysicalP p = new Flag(&world,&locator,pos);
	      assert(p);
	      locator.add(p);
	      flagIds[n] = p->get_id();
	    }
	  flagsRemaining = SCENARIO_FLAG_FLAGS;
	  lTitleStr << "[" << level << "] CAPTURE THE FLAG" << ends;
	  lStr << "[" << level << "] COLLECT " << SCENARIO_FLAG_FLAGS 
	    << " FLAGS.\nremaining: " << SCENARIO_FLAG_FLAGS << ends;
	}
	break;
      case LEMMINGS:
	{
	  // Know these area is open because they are within the 
	  // (OBJECT_COL_MAX,OBJECT_ROW_MAX) margain around the World edge.
	  Size worldSize = world.get_size();
	  Size homeSize = Home::get_size();
	  Pos homePos(worldSize.width - 12 * WSQUARE_WIDTH - homeSize.width,
		      worldSize.height - WSQUARE_HEIGHT - homeSize.height);
	  PhysicalP home = 
	    new Home(&world,&locator,homePos);
	  assert(home);
	  locator.add(home);
	  homeId = home->get_id();
	  

	  // Create Trapdoor after Home because it needs homeId.
	  Pos trapdoorPos(3*WSQUARE_WIDTH,WSQUARE_HEIGHT);
	  PhysicalP trapdoor = 
	    new Trapdoor(&world,&locator,trapdoorPos,homeId);
	  locator.add(trapdoor);
	  trapdoorId = trapdoor->get_id();
	  
	  lemmingsSafe = lemmingsOut = lemmingsActive = 0;
	  lTitleStr << "[" << level << "] LEMS" << ends;
	  lStr << "[" << level << "] SAVE " 
	    << SCENARIO_LEMMINGS_NEED << " LEMS\n" 
	      << "out: 0 safe: 0 dead: 0" << ends;
	}
	break;
      case FIRE_DEMON:
	{
	  lTitleStr << "[" << level << "] FIRE DEMON" << ends;
	  lStr << "[" << level << "] Kill the Fire Demon." << ends;
	  create_enemy(enemy_physical(A_FireDemon));
	}
	break;
      default:
	assert(0);
	break;
      };
    }
    break;
    
  case UIsettings::LEVELS:
    {
      // World stays the same size.
      if (!fromReset)
	world.reset();
      lTitleStr << "Level: " << level << ends;
      lStr << "Level: " << level << ends;
      enemiesNum += LEVEL_ENEMIES_INCR;
      for (int m = 0; m < enemiesNum; m++)
	create_enemy();
    }
    break;

  case UIsettings::KILL:
  case UIsettings::DUEL:
  case UIsettings::EXTENDED:
  case UIsettings::TRAINING:
    {
      // World stays the same size.
      if (!fromReset)
	world.reset();
      lTitleStr << "Level: " << level << ends;
      lStr << "Level: " << level << ends;
      newLevelTimer.set(LEVEL_TIME);
      newLevelTime = True;

      // NOTE: Do not kill off extra Physical objects for these styles.
    }
    break;
  };
  
  gameObjects.level_reset(world.get_dim());

  // No gameObjects for BONUS scenario.
  if (!(style == UIsettings::SCENARIOS && scenario == BONUS))
    gameObjects.refill();

  ui.set_level_title(lTitleStr.str());
  ui.set_level(lStr.str());
  delete lTitleStr.str();
  delete lStr.str();

  levelTitleTime = True;
  levelTitleTimer.set();
}



void Game::reset()
{
  demoOn = False;
  Stats::enable();
  locator.set_messages_ignore(False);

  if (gameOn)
    end_game();

  extraTime = False;
  levelTitleTime = False;
  newLevelTime = False;
  enemyNameCount = 0;
  enemiesRefill = enemiesRefillNext;
  style = styleNext;
  if (style == UIsettings::TRAINING)
    humansNum = 1;
  else
    humansNum = humansNumNext;
  enemiesNum = enemiesNumNext;
  level = 0; // So will start out at 1.
  levelHighest = 0;

  locator.reset();
  ui.reset();

  if (style != UIsettings::SCENARIOS)
    {
      world.set_rooms_next(worldRooms);
      world.reset();
    }
  // else the world is reset in new_level().
  
  if (!polCorrect)
    {
      ostrstream msg;
      msg << wittySayings[Utils::choose(WITTY_SAYINGS_NUM)] << ends;
      locator.message_enq(msg.str());
    }

  /* Don't need to call Ui::set_* because the new values originally came 
     from Ui. */

  gameObjects.game_reset(oneItem,noItems,oneEach);
  style_reset();
  new_level(True);
  humans_reset(); // After new_level because world must be reset.

  gameOn = True;

  // Force Game::clock to set Ui::set_intels_playing().
  humansPlayingPrev = enemiesPlayingPrev = -1;  // Used to be 0.
}



void Game::process_x_resources(int *,char **)
{
  for (int dpyNum = 0; dpyNum < ui.get_dpy_max(); dpyNum++)
    if (!ui.keyset_set(dpyNum))
      {
	const char * const *keysNames = ui.get_keys_names();
	
	KeySym right[UI_KEYS_MAX][2],left[UI_KEYS_MAX][2];
	
	for (int n = 0; n < UI_KEYS_MAX; n++)
	  for (int which = 0; which < 2; which++)
	    {
	      { // Right keys.
		right[n][which] = 0;
		
		ostrstream strm;
		if (which == 0)
		  strm << "right_" << keysNames[n] << ends;
		else
		  strm << "right_" << keysNames[n] << "_2" << ends;
		char *option = strm.str();
		
		// Should we free value??
		char *value = XGetDefault(ui.get_dpy(0),XEVIL_CLASS,option);
		if (value)
		  {
		    KeySym keysym = XStringToKeysym(value);
		    if (keysym != NoSymbol)
		      right[n][which] = keysym;
		  }
		delete option;
	      }
	      
	      { // Left Keys.
		left[n][which] = 0;
		
		ostrstream strm;
		if (which == 0)
		  strm << "left_" << keysNames[n] << ends;
		else
		  strm << "left_" << keysNames[n] << "_2" << ends;
		char *option = strm.str();
		
		// Should we free value??
		char *value = XGetDefault(ui.get_dpy(0),XEVIL_CLASS,option);
		if (value)
		  {
		    KeySym keysym = XStringToKeysym(value);
		    if (keysym != NoSymbol)
		      left[n][which] = keysym;
		  }
		delete option;
	      }
	    }
	ui.set_keyset(dpyNum,KEYSET_DEFAULT,right,left);
      }
}



void Game::parse_args(int *argc,char **argv)
{
  // Create a bunch of "-namr<x>" strings for comparing with command-line 
  // args.
  ostrstream dashName[Locator::HUMANS_MAX];
  int n;
  for (n = 0; n < Locator::HUMANS_MAX; n++)
    dashName[n] << "-name" << n << ends;
  
  for (n = 0; n < *argc; n++)
    {
      if (! strcmp("-duel",argv[n]))
	{
	  style = styleNext = UIsettings::DUEL;
	  ui.set_style(style);
	}
      else if (! strcmp("-extended",argv[n]))
	{
	  style = styleNext = UIsettings::EXTENDED;
	  ui.set_style(style);
	}
      else if ((! strcmp("-h",argv[n])) || (! strcmp("-help",argv[n])))
	{
	  cout 
	    << "usage: " << argv[0] 
	    << " [-display display_name] [-d<x> display_name | "
	    << "-display<x> display_name] "
	    << "[-duel] [-extended] [-h | -help] [-humans number] [-info] "
	    << "[-kill] "
	    << "[-keys alpha | decmips | iris | mac | ncd | rsaix | sun3 | sun4 "
	    << "| sun4_sparc | tektronix] [-levels] [-machines number] [-map] "
	    << "[-name<x> <name_of_player_x>] "
	    << "[-pc] [-regenerate_machines] [-rooms acrossxdown] "
	    << "[-speed time-in-ms] [-synchronous] [-training]" << endl;
	  exit(0);
	}
      else if (! strcmp("-harmless",argv[n]))
	intelHarmless = True;
      else if (!strcmp("-human_class",argv[n]) && (n + 1 < *argc))
	{
	  assert(humanClass == A_None);
	  n++;
	  
	  // If not found, humanClass will just remain as A_None.
	  humanClass = parse_class_name(&locator,argv[n]);

// Old brain-dead way.
#if 0
	  if (!strcmp("enforcer",argv[n]))
	    humanClass = A_Enforcer;
	  else if (!strcmp("frog",argv[n]))
	    humanClass = A_Frog;
	  else if (!strcmp("hero",argv[n]))
	    humanClass = A_Hero;
	  else if (!strcmp("ninja",argv[n]))
	    humanClass = A_Ninja;
	  else if (!strcmp("chopper-boy",argv[n]))
	    humanClass = A_ChopperBoy;
	  else if (!strcmp("alien",argv[n]))
	    humanClass = A_Alien;
	  else if (!strcmp("fire-demon",argv[n]))
	    humanClass = A_FireDemon;
	  else if (!strcmp("red-hugger",argv[n]))
	    humanClass = A_RedHugger;
	  else if (!strcmp("green-hugger",argv[n]))
	    humanClass = A_GreenHugger;
	  else if (!strcmp("walker",argv[n]))
	    humanClass = A_Walker;
#endif
	}
      else if ((! strcmp("-humans",argv[n])) && (n + 1 < *argc))
	{
	  humansNumNext = atoi(argv[n+1]);
	  humansNumNext = min(humansNumNext,Locator::HUMANS_MAX);
	  ui.set_humans_num(humansNumNext);
	  n++;
	}
      else if (! strcmp("-info",argv[n]))
	{
	  cout << 
	  "   Copyright (C) 1994,1995  Steve Hardt " << endl << 
	  endl << 
	  "    This program is free software; you can redistribute it and/or modify" << endl <<
	  "    it under the terms of the GNU General Public License as published by" << endl <<
	  "    the Free Software Foundation; either version 1, or (at your option)" << endl <<
	  "    any later version." << endl <<
	  endl <<
	  "    This program is distributed in the hope that it will be useful," << endl <<
	  "    but WITHOUT ANY WARRANTY; without even the implied warranty of" << endl <<
	  "    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" << endl << 
	  "    GNU General Public License for more details." << endl <<
	  endl <<
	  "    You should have received a copy of the GNU General Public License" << endl <<
	  "    along with this program; if not, write to the Free Software" << endl <<
	  "    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA." << endl <<
	  endl <<
	  "    Design and programming:" << endl <<
	  "    Steve Hardt" << endl << 
          "    hardts@mit.edu (valid until Nov. 1996)" << endl <<
          "    hardts@netscape.com" << endl <<
	  "    hardts@alum.mit.edu" << endl <<
	  endl <<
	  "    The XEvil distribution is available via anonymous ftp at" << endl <<
	  "    ftp.x.org under /contrib/games/xevil." << endl <<
	  "    http://graphics.lcs.mit.edu/~hardts/xevil.html" << endl;
	  exit(0);
	}
      else if (! strcmp("-kill",argv[n]))
	{
	  style = styleNext = UIsettings::KILL;
	  ui.set_style(style);
	}
      else if ((! strcmp("-keys",argv[n])) && (n + 1 < *argc))
	{
	  // Sets the keyset for all displays to the same value.
	  n++;
	  for (int dpyNum = 0; dpyNum < ui.get_dpy_max(); dpyNum++)
	    {
	      if (! strcmp("alpha",argv[n]))
		ui.set_keyset(dpyNum,UIalpha);
	      if (! strcmp("decmips",argv[n]))
		ui.set_keyset(dpyNum,UIdecmips);
	      if (! strcmp("iris",argv[n]))
		ui.set_keyset(dpyNum,UIiris);
	      if (! strcmp("mac",argv[n]))
		ui.set_keyset(dpyNum,UImac);
	      if (! strcmp("ncd",argv[n]))
		ui.set_keyset(dpyNum,UIncd);
	      if (! strcmp("rsaix",argv[n]))
		ui.set_keyset(dpyNum,UIrsaix);
	      if (! strcmp("sun3",argv[n]))
		ui.set_keyset(dpyNum,UIsun3);
	      if (! strcmp("sun4",argv[n]))
		ui.set_keyset(dpyNum,UIsun4);
	      if (! strcmp("sun4_sparc",argv[n]))
		ui.set_keyset(dpyNum,UIsun4_sparc);
	      if (! strcmp("tektronix",argv[n]))
		ui.set_keyset(dpyNum,UItektronix);
	    }
	}
      else if (!strcmp("-levels",argv[n]))
	{
	  style = styleNext = UIsettings::LEVELS;
	  ui.set_style(style);
	}
      else if ((! strcmp("-machines",argv[n])) && (n + 1 < *argc))
	{
	  enemiesNumNext = atoi(argv[n+1]);
	  enemiesNumNext = min(enemiesNumNext,Locator::ENEMIES_MAX);
	  ui.set_enemies_num(enemiesNumNext);
	  n++;
	}
      else if (! strcmp("-map",argv[n]))
	world.set_map_print(True);
      else if (! strcmp("-no_items",argv[n]))
	noItems = True;
      else if (! strcmp("-one_each",argv[n]))
	oneEach = True;
      else if ((! strcmp("-one_item",argv[n])) && (n + 1 < *argc))
	{
	  n++;
	  oneItem = argv[n];
	}
      /* "-pc" command line option handled by Game::is_pol_correct. */
      else if (! strcmp("-regenerate_machines",argv[n]))
	{
	  enemiesRefillNext = True;
	  ui.set_enemies_refill(True);
	}
      else if (! strcmp("-rooms",argv[n]) && (n + 1 < *argc))
	{
	  Rooms temp;
	  if (sscanf(argv[n+1],"%dx%d",
		     &temp.acrossMax,&temp.downMax) == 2)
	    worldRooms = temp;
	  n++;
	}
      else if ((! strcmp("-scenario",argv[n])) && (n + 1 < *argc))
	{
	  n++;
	  if (!(strcmp("exterminate",argv[n])))
	    scenarioOverride = EXTERMINATE;
	  else if (!(strcmp("flag",argv[n])))
	    scenarioOverride = FLAG;
	  else if (!(strcmp("bonus",argv[n])))
	    scenarioOverride = BONUS;
	  else if (!(strcmp("hive",argv[n])))
	    scenarioOverride = HIVE;
	  else if (!(strcmp("lems",argv[n])))
	    scenarioOverride = LEMMINGS;
	  else if (!(strcmp("fire-demon",argv[n])))
	    scenarioOverride = FIRE_DEMON;
	}
      else if (!strcmp("-scenarios",argv[n]))
	{
	  style = styleNext = UIsettings::SCENARIOS;
	  ui.set_style(style);
	}
      else if ((! strcmp("-speed",argv[n])) && (n + 1 < *argc))
	{
	  quanta = atoi(argv[n+1]);
	  ui.set_quanta(quanta);
	  n++;
	}
      else if (! strcmp("-stats",argv[n]))
	showStats = True;
      else if (! strcmp("-synchronous",argv[n]))
	for (int m = 0; m < ui.get_dpy_max(); m++)
	  XSynchronize(ui.get_dpy(m),True);
      else if (! strcmp("-training",argv[n]))
	{
	  style = styleNext = UIsettings::TRAINING;
	  ui.set_style(style);
	}

      // Check "-name<x>" arguments.
      for (int j = 0; j < Locator::HUMANS_MAX; j++)
	if ((! strcmp(dashName[j].str(),argv[n])) && (n + 1 < *argc))
	  {
	    humanNames[j] = argv[n+1];
	    n++;
	  }
    } // for


  // Delete memory for "-name" strings.
  for (n = 0; n < Locator::HUMANS_MAX; n++)
    delete dashName[n].str();
}



Boolean Game::is_pol_correct(int *argc,char **argv)
{
  for (int n = 0; n < *argc; n++)
    if (! strcmp("-pc",argv[n]))
      return True;

  return False;
}



char **Game::display_names(int *argc,char **argv)
{
  ostrstream dashDisplay[UI_VIEWPORTS_MAX][2];
  typedef char *charP;
  char **displayNames = new charP [UI_VIEWPORTS_MAX];

  int n;
  for (n = 0; n < UI_VIEWPORTS_MAX; n++)
    {
      displayNames[n] = new char [Xvars::DISPLAY_NAME_LENGTH];
      strcpy(displayNames[n],"");
      dashDisplay[n][0] << "-display" << n << ends;
      dashDisplay[n][1] << "-d" << n << ends;
    }

  // Loop through all command line arguments.
  for (n = 0; n < *argc - 1 ; n++)
    {
      // Set display name for all viewports.
      if (!strcmp(argv[n],"-display") || !strcmp(argv[n],"-d"))
	{
	  assert(strlen(argv[n+1]) < Xvars::DISPLAY_NAME_LENGTH);
	  for (int vNum = 0; vNum < UI_VIEWPORTS_MAX; vNum++)
	    strcpy(displayNames[vNum],argv[n+1]);
	}
      
      // Set display name for one viewport.
      for (int m = 0; m < UI_VIEWPORTS_MAX; m++)
	for (int which = 0; which < 2; which++)
	  if (!strcmp(argv[n],dashDisplay[m][which].str()))
	    {
	      assert(strlen(argv[n+1]) < Xvars::DISPLAY_NAME_LENGTH);
	      strcpy(displayNames[m],argv[n+1]);
	    }
    }

  for (n = 0; n < UI_VIEWPORTS_MAX; n++)
    for (int which = 0; which < 2; which++)
      delete dashDisplay[n][which].str();

  return displayNames;
}



void Game::humans_reset()
{
  assert(humansNum <= Xvars::HUMAN_COLORS_NUM);
  assert(humansNum <= Locator::HUMANS_MAX);

  // Doesn't really have to be done if polCorrect.
  assert(humansNum <= INTEL_NAMES_NUM);
  int intelNamesIndices[INTEL_NAMES_NUM];
  Utils::randomList(intelNamesIndices,INTEL_NAMES_NUM);


  // Create humans along with their viewports, if necessary.
  for (int vn = 0; vn < humansNum; vn++)
    {
      assert(vn <= ui.get_viewports_num());
      if (vn == ui.get_viewports_num())
	{
	  int vNum = ui.add_viewport();
	  assert(vn == vNum);
	  int v = ui.get_viewports_num_on_dpy(ui.get_dpy_num(vNum));
	  if (v == 1)
	    ui.set_input(vNum,UI_KEYS_RIGHT);
	  else if (v == 2)
	    ui.set_input(vNum,UI_KEYS_LEFT);
	}
      ostrstream namePc;
      if (polCorrect)
	namePc << "Human-" << vn << ends;

      // Choose random name if -name was not specified for this player.
      char *nameNonPc = strlen(humanNames[vn]) ? humanNames[vn] :
      intelNames[intelNamesIndices[vn]];

      // Give infinite lives for UIsettings::EXTENDED.
      // Not infinite lives for Uisettings::DUEL.
      int lives = (style == UIsettings::EXTENDED) ? IT_INFINITE_LIVES : 
      (style == UIsettings::SCENARIOS ? HUMAN_LIVES_SCENARIOS :
       HUMAN_LIVES);

      HumanP human = new Human(&world,&locator,
			       (polCorrect ? namePc.str() : nameNonPc),
			       lives,vn);
      
      assert(human);
      if (polCorrect)
	delete namePc.str();

      locator.register_human(human);
      ui.register_intel(vn,human);

      // Actual objects for humans.
      PhysicalP obj= human_physical(vn);
      locator.add(obj);
      obj->set_intel(human);
    }

  int delNum = ui.get_viewports_num() - humansNum;
  for (int n = 0; n < delNum; n++)
    if (ui.get_viewports_num() > 1)
      ui.del_viewport();
}



void Game::style_reset()
{
  // So first level will start out with LEVEL_ENEMIES_INITIAL.
  if (style == UIsettings::SCENARIOS || style == UIsettings::LEVELS)
    enemiesNum = LEVEL_ENEMIES_INITIAL - LEVEL_ENEMIES_INCR;
    
  if (style == UIsettings::KILL || style == UIsettings::DUEL 
       || style == UIsettings::EXTENDED)
    for (int m = 0; m < enemiesNum; m++)
      create_enemy();

  
  if ((style == UIsettings::DUEL || style == UIsettings::EXTENDED) 
      && humansNum < 2)
    {
      ostrstream str;
      str << "!!!!!!!! YOU NEED AT LEAST 2 HUMAN PLAYERS FOR A "
	  << "DUEL !!!!!!!!" << ends;
      locator.message_enq(str.str());
    }

  if (style == UIsettings::KILL && (humansNum + enemiesNum) < 2)
    {
      ostrstream str;
      str << "!!!!!!!! YOU NEED AT LEAST 2 HUMAN OR MACHINE PLAYERS TO "
	<< "PLAY KILL, KILL, KILL !!!!!!!!" << ends;
      locator.message_enq(str.str());
    }
}



void Game::intro()
{
  ostrstream msg;
  msg << "XEvil version " << VERSION 
    << ", Copyright (C) 1994,1995.1996 Steve Hardt (hardts@mit.edu)" 
    << "\n" 
    << "http://graphics.lcs.mit.edu/~hardts/xevil.html" 
    << "  (hardts@netscape.com)" << ends;
  locator.message_enq(msg.str());
  
  cout << "XEvil version " << VERSION << 
    ", Copyright (C) 1994,1995 Steve Hardt" << endl <<
    "hardts@mit.edu (valid until Nov. 1996)" << endl <<
    "hardts@netscape.com" << endl <<
    "hardts@alum.mit.edu" << endl;
  cout << "XEvil comes with ABSOLUTELY NO WARRANTY." << endl;
  
  cout << "Type 'xevil -info' for information on redistribution and " 
    << "on NO WARRANTY." << endl;
  cout << "The XEvil distribution can be found at ftp.x.org under /contrib/games." << endl;
  cout << "http://graphics.lcs.mit.edu/~hardts/xevil.html" << endl;
}



// Used in Game::print_stats().
Boolean Game::stats_creations_filter(const PhysicalContext *cx) {
  return cx->statsCreations;
}



Boolean Game::stats_uses_filter(const PhysicalContext *cx) {
  return cx->statsUses;
}



Boolean Game::stats_deaths_filter(const PhysicalContext *cx) {
  return cx->statsDeaths;
}



// New way using Locator::filter_contexts.
void Game::print_stats()
{
  cout << setprecision(3) << endl
  << "-----------------------STATISTICS-----------------------" << endl;


  int contextsNum,n;
  const PhysicalContext *contexts[A_CLASSES_NUM];


  // Creations.
  contextsNum = 
    locator.filter_contexts(contexts,NULL,stats_creations_filter);

  for (n = 0; n < contextsNum; n++) {
    const Stats &stats = contexts[n]->get_stats();
    cout << contexts[n]->className << ":  created:  " 
    << stats.get_creations() << endl;
  }
  cout << endl;


  // Uses.
  contextsNum = 
    locator.filter_contexts(contexts,NULL,stats_uses_filter);

  for (n = 0; n < contextsNum; n++) {
    const Stats &stats = contexts[n]->get_stats();
    cout << contexts[n]->className << ":  used: " 
    << stats.get_uses() << endl;
  }
  cout << endl;


  // Deaths.
  contextsNum = 
    locator.filter_contexts(contexts,NULL,stats_deaths_filter);

  for (n = 0; n < contextsNum; n++) {
    const Stats &stats = contexts[n]->get_stats();
    cout << contexts[n]->className << ":  number killed: "
    << stats.get_deaths() 
    << "  average lifespan: " << stats.get_ave_lifespan() << " seconds" 
    << endl;
  }


  // Total for all Creatures.
  const Stats &creature = Creature::get_stats();
  cout 
  << "Total creatures killed: " 
  << creature.get_deaths() 
  << "  average lifespan: " << creature.get_ave_lifespan() << " seconds" 
  << endl << endl;


  // Figure this one out yourself.
  cout 
  << "Highest level: " << levelHighest << endl;
}



// Old way
#if 0
void Game::print_stats()
{
  const Stats &shot = Shot::get_stats();
  const Stats &grenade = Grenade::get_stats();
  const Stats &doppel = Doppel::get_stats();
  const Stats &cloak = Cloak::get_stats();
  const Stats &transmogifier = Transmogifier::get_stats();
  const Stats &medKit = MedKit::get_stats();
  const Stats &shield = Shield::get_stats();
  const Stats &bomb = Bomb::get_stats();
  const Stats &creature = Creature::get_stats();
  const Stats &enforcer = Enforcer::get_stats();
  const Stats &frog = Frog::get_stats();
  const Stats &hero = Hero::get_stats();
  const Stats &ninja = Ninja::get_stats();
  const Stats &alien = Alien::get_stats();
  const Stats &chopperBoy = ChopperBoy::get_stats();
  const Stats &lemming = Lemming::get_stats();
  const Stats &walker = Walker::get_stats();

  cout << setprecision(3) << endl
  << "-----------------------STATISTICS-----------------------" << endl
  << "Shots fired:                               " 
  << shot.get_creations() << endl
  << "Grenades thrown:                           " 
  << grenade.get_creations() << endl
  << "Doppelgangers used:                        " 
  << doppel.get_uses() << endl
  << "Cloaks used:                               "
  << cloak.get_uses() << endl
  << "Transmogifiers used:                       " 
  << transmogifier.get_uses() << endl
  << "MedKits used:                              " 
  << medKit.get_uses() << endl
  << "Shields used:                              " 
  << shield.get_uses() << endl
  << "Bombs exploded:                            " 
  << bomb.get_uses() << endl
  << "Creatures killed (average lifespan):       " 
  << creature.get_deaths() << "  (" << creature.get_ave_lifespan() 
  << " sec)" << endl
  << "   Enforcers:                              " 
  << enforcer.get_deaths() 
  << "  (" << enforcer.get_ave_lifespan() << " sec)" << endl
  << "   Frogs:                                  " << frog.get_deaths()
  << "  (" << frog.get_ave_lifespan() << " sec)" << endl
  << "   Heros:                                  " << hero.get_deaths()
  << "  (" << hero.get_ave_lifespan() << " sec)" << endl
  << "   Ninjas:                                 " << ninja.get_deaths()
  << "  (" << ninja.get_ave_lifespan() << " sec)" << endl
  << "   Aliens:                                 " << alien.get_deaths()
  << "  (" << alien.get_ave_lifespan() << " sec)" << endl
  << "   ChopperBoys:                            " 
  << chopperBoy.get_deaths()
  << "  (" << chopperBoy.get_ave_lifespan() << " sec)" << endl
  << "   Lems:                                   " 
  << lemming.get_deaths()
  << "  (" << lemming.get_ave_lifespan() << " sec)" << endl
  << "   Walkers:                                " 
  << walker.get_deaths()
  << "  (" << walker.get_ave_lifespan() << " sec)" << endl
  << "Highest level:                             " << levelHighest << endl;
}
#endif



void Game::refill_enemies()
{
  int diff = enemiesNum - locator.enemies_alive();
  assert(diff >= 0);
  
  for (int n = 0; n < diff; n++)
    create_enemy();
}



void Game::hive_refill_enemies()
{
  int diff = SCENARIO_HIVE_ALIENS - locator.enemies_alive();

  // diff might be less than zero if some RedHuggers have come into existence.
  //assert(diff >= 0);

  for (int n = 0; n < diff; n++)
    create_enemy(enemy_physical(A_Alien));
}



void Game::flag_refill_enemies()
{
  int diff = SCENARIO_FLAG_ENEMIES - locator.enemies_alive();
  assert(diff >= 0);
  
  for (int n = 0; n < diff; n++)
    create_enemy();
}



void Game::lemmings_refill_enemies()
{
  int diff = SCENARIO_LEMMINGS_ENEMIES - locator.enemies_alive();
  assert(diff >= 0);
  
  // Change the num argument to enemy_physical if this list is changed.
  int list[] = {A_Ninja,A_Hero,A_ChopperBoy};

  for (int n = 0; n < diff; n++)
    create_enemy(enemy_physical(list,3));
}



PhysicalP Game::create_enemy(PhysicalP obj)
{
  if (!obj)
    obj = enemy_physical();

  IntelOptions ops;
  ITmask opMask = intel_options_for(ops,obj->get_class_id());

  ostrstream name;
  name << "Machine-" << (enemyNameCount++) << ends;
  EnemyP enemy = new Enemy(&world,&locator,name.str(),&ops,opMask);
  assert(enemy);
  delete name.str();
  locator.register_enemy(enemy);

  locator.add(obj);
  obj->set_intel(enemy);

  return obj;
}



ITmask Game::intel_options_for(IntelOptions &ops,ClassId classId)
{
  ops.harmless = intelHarmless;

  ops.classFriends = (style != UIsettings::KILL);

  ops.psychotic = 
    classId == A_Enforcer || 
      classId == A_Alien ||
	classId == A_FireDemon;
  
  return ITharmless | ITclassFriends | ITpsychotic;
}



void Game::demo_setup()
{
  // Choose between different demos.
  switch (Utils::choose(6))  
    {
    case 0:  // A bunch of Heros and an Alien.
      {
	for (int n = 0; n < 10; n++)
	  {
	    ostrstream name;
	    name << "Enemy-" << n << ends;
	    IntelOptions ops;
	    ops.harmless = True;
	    EnemyP enemy = new Enemy(&world,&locator,name.str(),
					   &ops,ITharmless);
	    assert(enemy);
	    delete name.str();
	    locator.register_enemy(enemy);
	    
	    Pos pos = world.empty_rect(Hero::get_size_max());
	    PhysicalP obj = new Hero(&world,&locator,pos);
	    assert(obj);
	    locator.add(obj);
	    obj->set_intel(enemy);
	  }
	IntelOptions ops;
	ops.psychotic = True;
	EnemyP enemy = new Enemy(&world,&locator,"killer",&ops,ITpsychotic);
	assert(enemy);
	locator.register_enemy(enemy);
	Pos pos = world.empty_rect(Alien::get_size_max());
	PhysicalP obj = new Alien(&world,&locator,pos);
	assert(obj);
	locator.add(obj);
	obj->set_intel(enemy);
      }
      break;
    case 1: // Hero, FThrower, and a bunch of Frogs (does not mean Frenchmen).
      {
	for (int n = 0; n < 15; n++)
	  {
	    ostrstream name;
	    name << "Enemy-" << n << ends;
	    IntelOptions ops;
	    ops.psychotic = Utils::coinFlip();
	    EnemyP enemy = new Enemy(&world,&locator,name.str(),
					   &ops,ITpsychotic);
	    assert(enemy);
	    delete name.str();
	    locator.register_enemy(enemy);
	    
	    Pos pos = world.empty_rect(Frog::get_size_max());
	    Id invalid;
	    PhysicalP obj = new Frog(&world,&locator,pos,invalid);
	    assert(obj);
	    locator.add(obj);
	    obj->set_intel(enemy);
	  }

	EnemyP enemy = new Enemy(&world,&locator,"killer",NULL,0);
	assert(enemy);
	locator.register_enemy(enemy);
	Pos pos = world.empty_rect(Hero::get_size_max());
	PhysicalP obj = new Hero(&world,&locator,pos);
	assert(obj);
	locator.add(obj);
	obj->set_intel(enemy);
	
//	pos = world.empty_rect(FThrower::get_size_max());
	PhysicalP fThrower = new FThrower(&world,&locator,pos);
	locator.add(fThrower);
      }
      break;
    case 2: // A bunch of Enforcers.
      {
	for (int n = 0; n < 10; n++)
	  {
	    ostrstream name;
	    name << "Enemy-" << n << ends;
	    IntelOptions ops;
	    ops.classFriends = False;
	    ops.psychotic = True;
	    EnemyP enemy = new Enemy(&world,&locator,name.str(),
					   &ops,ITclassFriends|ITpsychotic);
	    assert(enemy);
	    delete name.str();
	    locator.register_enemy(enemy);
	    
	    Pos pos = world.empty_rect(Enforcer::get_size_max());
	    PhysicalP obj = new Enforcer(&world,&locator,pos);
	    assert(obj);
	    locator.add(obj);
	    obj->set_intel(enemy);
	  }
      }
      break;
    case 3: // A bunch of Ninjas.
      {
	for (int n = 0; n < 10; n++)
	  {
	    ostrstream name;
	    name << "Enemy-" << n << ends;
	    IntelOptions ops;
	    ops.classFriends = False;
	    EnemyP enemy = new Enemy(&world,&locator,name.str(),
					   &ops,ITclassFriends);
	    assert(enemy);
	    delete name.str();
	    locator.register_enemy(enemy);
	    
	    Pos pos = world.empty_rect(Ninja::get_size_max());
	    PhysicalP obj = new Ninja(&world,&locator,pos);
	    assert(obj);
	    locator.add(obj);
	    obj->set_intel(enemy);
	  }
	Pos pos = world.empty_rect(Chainsaw::get_size_max());
	PhysicalP obj = new Chainsaw(&world,&locator,pos);
	locator.add(obj);
      }
      break;
    case 4: // drop weights 
      {
	for (int n = 0; n < 36; n++)
	  {
	    Pos pos(32 + 16 * n,16 * 6);
	    Pos pos2(32 + 16 * n,32);
	    PhysicalP p = new Weight(&world,&locator,pos);
	    PhysicalP p2 = new Bomb(&world,&locator,pos2);
	    ((ItemP)p2)->use(NULL);

	    Vel vel(0,-12);
	    ((MovingP)p)->set_vel_next(vel);
	    locator.add(p);
	    locator.add(p2);
	  }	    

	for (int m = 0; m < 10; m++)
	  {
	    ostrstream name;
	    name << "Enemy-" << m << ends;
	    EnemyP enemy = new Enemy(&world,&locator,name.str(),
					   NULL,ITnone);
	    assert(enemy);
	    delete name.str();
	    locator.register_enemy(enemy);
	    
	    Pos pos(32 + Utils::choose(16 * 36),16 * 12);
	    PhysicalP obj;
	    switch (Utils::choose(3)) {
	    case 0:
	      obj = new Ninja(&world,&locator,pos);
	      break;
	    case 1:
	      obj = new Hero(&world,&locator,pos);
	      break;
	    case 2:
	      {
		Id invalid;
		obj = new Frog(&world,&locator,pos,invalid);
	      }
	      break;
	    };
	    assert(obj);
	    locator.add(obj);
	    obj->set_intel(enemy);
	  }
      }
      break;
    case 5: // Ninjas and ChopperBoys.
      {
	int n;
	for (n = 0; n < 10; n++)
	  {
	    ostrstream name;
	    name << "Enemy-" << n << ends;
	    IntelOptions ops;
	    ops.classFriends = False;
	    EnemyP enemy = new Enemy(&world,&locator,name.str(),
					   &ops,ITclassFriends);
	    assert(enemy);
	    delete name.str();
	    locator.register_enemy(enemy);
	    
	    PhysicalP obj;
	    Pos pos;
	    if (n % 2)
	      {
		pos = world.empty_rect(Ninja::get_size_max());
		obj = new Ninja(&world,&locator,pos);
	      }
	    else
	      {
		pos = world.empty_rect(ChopperBoy::get_size_max());
		obj = new ChopperBoy(&world,&locator,pos);
	      }
	    assert(obj);
	    locator.add(obj);
	    obj->set_intel(enemy);
	  }
	for (n = 0; n < 5; n++)
	  {
	    Pos pos = world.empty_rect(Launcher::get_size_max());
	    PhysicalP obj = new Launcher(&world,&locator,pos);
	    assert(obj);
	    locator.add(obj);
	  }

      }
      break;
    default:
      assert(0);
    };
}



void Game::off_clock_kill(PhysicalP p)
{
//  cout << "attempting to kill " << p->get_class_name() << endl;

  p->set_quiet_death();
  if (!p->die_called())
    {
      p->kill_self();
      p->die();
    }
}
