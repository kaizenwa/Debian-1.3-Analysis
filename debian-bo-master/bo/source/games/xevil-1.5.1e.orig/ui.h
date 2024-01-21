// "ui.h"  Header for user interface module.  

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
   Draws the world.  Manages viewports.  Holds x variables.  Follows an intel
if one is registered.  Sends commands to the intel if it is human.  For weapon
commands, tapping key gives most recently pressed direction or IT_CENTER if 
there is none. */

/* NOTE: A Ui is never destroyed.  Allocates memory for transfering to 
	 callbacks and event handlers. */

#ifndef UI_H  //Make sure that file is not included more than once.
#define UI_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
#include "utils.h"
#include "coord.h"
#include "world.h"
#include "locator.h"
#include "id.h"
#include "intel.h"



// Defines
#define UI_VIEWPORTS_MAX 10
#define UI_KEYS_MAX IT_WEAPON_R
#define UI_STRING_LENGTH 600 // For panels.
#define UI_STATUSES_NUM 8  // Not 9, because ammo is with weapon.
#define UI_MENUS_PRIMARY_NUM 15
#define UI_MENUS_SECONDARY_NUM 2
#define UI_SHORT_STRING_LENGTH 80


// Data Structures
enum UIinput {UI_KEYS_RIGHT,UI_KEYS_LEFT};
enum UIkeyset {UIsun3, UIdecmips, UIiris, UIncd, UItektronix, UIsun4, UIrsaix,
	       UIsun4_sparc,UImac,UIalpha};

class Ui;
typedef Ui *UiP;

// First index is an IT_COMMAND, sedond is one of two.
typedef KeySym UIkeymap[UI_KEYS_MAX][2];

typedef unsigned long UImask;
#define UInone 0L
#define UInewGame (1L<<0)
#define UIquit (1L<<1)
#define UIhumansNum (1L<<2)
#define UIenemiesNum (1L<<3)
#define UIenemiesRefill (1L<<4)
#define UIpause (1L<<6)
#define UIstyle (1L<<7)
#define UIquanta (1L<<8)

class UIsettings {
public:
  enum Style {SCENARIOS,LEVELS,KILL,DUEL,EXTENDED,TRAINING};

  int humansNum;
  int enemiesNum;
  Boolean enemiesRefill;
  Boolean pause;
  Style style;
  Quanta quanta;
};



// Class Declarations
class TextPanel;
class TogglePanel;
class KeyPressPanel;

class Ui {
  struct UIxdata {
    Window controls[Xvars::DISPLAYS_MAX];
    
    // The top level shell containing the KeyPressPanel.
    Window learnControls[Xvars::DISPLAYS_MAX]; 
    Window toplevels[UI_VIEWPORTS_MAX];
    Window arenas[UI_VIEWPORTS_MAX];
    Cursor arenaCursor[Xvars::DISPLAYS_MAX];
    Pixmap icon[Xvars::DISPLAYS_MAX];
  };
  
  
 public:
  Ui(int *argc,char **argv,WorldP w,LocatorP l,
     char **displayNames,Boolean polCorrect);  
  /* EFFECTS: Create user interface with one viewport (number 0) with world w 
     and locator l.  The memory pointed to by displayNames becomes the
     property of the Ui. */
  /* NOTE: Must set_keyset on all displays before first clock. 
     Must manually set_* all desired parameters before first reset. */
 
  int get_viewports_num() {return viewportsNum;}
  /* EFFECTS: Total number of viewports in the Ui. */

  int get_viewports_num_on_dpy(int dpyNum) {return vIndexNum[dpyNum];}
  int get_viewport_on_dpy(int dpyNum,int v) {return vIndex[dpyNum][v];}
  /* EFFECTS: Get viewports and number of viewports on a single display.
     The viewports on a display, dpyNum, range from 0 to 
     get_viewports_num_on(dpyNum) - 1. */

  Display *get_dpy(int dpyNum) {return xvars.dpy[dpyNum];}
  int get_dpy_max() {return xvars.dpyMax;}
  int get_dpy_num(int viewportNum) {return dpyIndex[viewportNum];}
  /* EFFECTS: Display number from viewport Number. */

  UImask get_settings(UIsettings &s);
  /* MODIFIES: s */
  /* EFFECTS:  Get all of the settings that are stored in the Ui.  Mostly the
     menubar stuff.  settingsChanges gives the fields in s that have changed
     since the last call of get_settings.  Other fields are not guaranteed to
     be meaningful.  Initially, nothing is changed. */

  const char * const *get_keys_names() 
  {return (const char * const *)keysNames;}

  Boolean settings_changed() {return settingsChanges != UInone;}
  /* EFFECTS:  Tells whether the the settings have changed since the last 
     call of get_settings().  Initially is False. */

  Boolean keyset_set(int dpyNum) {return keysetSet[dpyNum];}

  Boolean command_repeatable(ITcommand command);
  /* EFFECTS: Return whether the command is a repeating command.  I.e. if the
     user holds down the key that caused this command will it be dispatched 
     repeatedly.  The user has to keep tapping the keyboard to send non-
     repeatable commands repeatedly. */

  void set_humans_num(int);
  void set_enemies_num(int);
  void set_enemies_refill(Boolean);
  void set_style(UIsettings::Style);
  void set_quanta(Quanta);

  void set_humans_playing(int);
  void set_enemies_playing(int);

  void set_level(const char *);

  Boolean other_input() {return otherInput;}
  /* EFFECTS: Tells whether there has been any keyboard or mouse press other
     than a valid control since the last clock. */

  void set_input(int vNum,UIinput input);
  /* EFFECTS: Set the input device for viewport num. */

  void set_keyset(int dpyNum,UIkeyset keyset);
  void set_keyset(int dpyNum,UIkeyset basis,KeySym right[UI_KEYS_MAX][2],
		  KeySym left[UI_KEYS_MAX][2]);
  /* REQUIRES: Must be called at least once before the first clock. */
  /* IMPLEMENTATION NOTE: Sets keycodes[0 and 1] */

  void set_pause(Boolean);
  /* EFFECTS: Pause/unpause. */

  void set_level_title(char *msg);
  void unset_level_title();
  /* EFFECTS: When set, the Ui will draw msg in every viewport,
     until unset.  The world and locator are not drawn again until 
     unset. */
  /* NOTE: Takes care of Ui::set_redraw. */

  int add_viewport();
  /* EFFECTS: Add another viewport and return its number. */

  void del_viewport();
  /* EFFECTS: Delete the highest numbered viewport. */

  void register_intel(int n, IntelP intel);
  /* REQUIRES: intel is not already registered with a different viewport. */
  /* EFFECTS: Registers the intel on viewport n.  The viewport will now 
     follow the object represented by intel->get_id() and send commands to 
     it if it is human. */

  void unregister_intel(int n);
  /* REQUIRES: n is a valid viewport */
  /* EFFECTS: Unregister the intel associated with viewport n, if any.  
     Otherwise do nothing. */

  void reset();
  /* EFFECTS: Clear out all registered intels.  Does NOT remove viewports. */

  void set_redraw();
  /* EFFECTS: Next turn, the Ui must redraw everything. */

  void process_event(int dpyNum,XEvent *event);
  /* NOTE: Call callback last so that callback can make changes to self. */

  void clock();
  /* EFFECTS: Follows the object of the registered intel if any and redraws 
     the world and locator. */

  /* Must use static functions for callbacks because of problems with
     pointers to member functions in some compilers. */
  static void menu_quit_CB(UiP,int,int);
  static void menu_new_game_CB(UiP,int,int);
  static void menu_humans_num_CB(UiP,const char *value);
  static void menu_enemies_num_CB(UiP,const char *value);
  static void menu_enemies_refill_CB(UiP,TogglePanel *,Boolean val);
  static void menu_controls_CB(UiP,TogglePanel *,Boolean);
  static void menu_learn_controls_CB(UiP,TogglePanel *,Boolean);
  static void menu_scenarios_CB(UiP,TogglePanel *,Boolean);
  static void menu_levels_CB(UiP,TogglePanel *,Boolean);
  static void menu_kill_CB(UiP,TogglePanel *,Boolean);
  static void menu_duel_CB(UiP,TogglePanel *,Boolean);
  static void menu_extended_CB(UiP,TogglePanel *,Boolean);
  static void menu_training_CB(UiP,TogglePanel *,Boolean);
  static void menu_quanta_CB(UiP,const char *value);

  static void status_weapon_CB(UiP,int,int);
  static void status_item_CB(UiP,int,int);

  static void learn_controls_CB(UiP,KeyPressPanel *,XEvent *);


 private:
  void set_message(const char *message);
  /* EFECTS: Places message on all the viewports' message bars.  */

  void draw(int viewportNum, Boolean changedOnly = False);
  /* REQUIRES: viewportNum is valid. */
  /* EFFECTS: Draw everything in viewport number num. */

  void init_x();
  /* EFFECTS: Initialize the X Window variables in the Ui. */
  /* NOTE: Also sets vIndex,vIndexMax,meunusNum,dpyIndex. */

  void init_sizes();

  Boolean viewport_to_loc(int n,const Loc &l);
  /* REQUIRES: n is a valid viewport num.  l is a location in the world. */
  /* EFFECTS: Moves viewport n so that l is in it.  Returns True iff the
   viewport has moved. */

  void dispatch(int v,ITcommand command);
  /* EFFECTS: Let viewport v deal with the given command.  I.e. Send it to a 
     intel or move the viewport around. */

  int get_viewport_num(int dpyNum,Window window);
  /* EFFECTS: If window is one of the arenas, returns the viewport num.  
     Otherwise, returns -1. */

  void create_controls();

  void create_learn_controls();

  void create_toplevel(int viewportNum);
  /* EFFECTS: Create a top-level window, set the input mask, set standard WM
     properties, and map it.  Creates menus, arena, statuses, intelsPlaying,
     and messageBar.  If viewportNum == 0 then it creates the menus. */

  void create_menus(int viewportNum);
  void create_arena(int viewportNum);
  void create_statuses(int viewportNum);
  void create_intels_playing(int viewportNum);
  void create_message_bar(int viewportNum);
  void create_levels(int viewportNum);

  void update_statuses(int viewportNum);

  void controls_redraw(int dpyNum);

  // Events taking place on viewports.
  void viewport_expose(int viewportNum); 
  void viewport_key_press(int dpyNum,XEvent *event);
  void viewport_key_release(int dpyNum,XEvent *event);
  void viewport_button_press(int viewportNum,XEvent *event);
  Boolean viewport_button_press_helper(Dir &dir,int viewportNum,
				       XEvent *event);
  Dir key_press_to_dir(int dpyNum,XEvent *event);

  void controls_expose(int dpyNum,XEvent *) {controls_redraw(dpyNum);}


  // Variables.
  static char *keysNames[UI_KEYS_MAX];

  char **argv;  // Warning: Exposing the rep.
  int argc;
  Xvars xvars;
  UIxdata xdata;
  char **displayNames;  /* Array of [UI_VIEWPORTS_MAX]. 
			   Warning: Exposing the rep. */

  int viewportsNum;  // Total number of viewports on all displays.
  int dpyIndex[UI_VIEWPORTS_MAX]; // Map from viewports to displays.

  // Map from displays to viewports, a one to many map.
  int vIndex[Xvars::DISPLAYS_MAX][UI_VIEWPORTS_MAX]; 
  int vIndexNum[Xvars::DISPLAYS_MAX]; //Current number of viewports on a display.
  int vIndexMax[Xvars::DISPLAYS_MAX]; //Possible number of viewports on a display.

  WorldP world;
  LocatorP locator;
  Box viewportBoxes[UI_VIEWPORTS_MAX];
  Boolean intelsSet[UI_VIEWPORTS_MAX];
  IntelP intels[UI_VIEWPORTS_MAX];

  // Meaningful iff keysetSet.
  unsigned int keycodes[Xvars::DISPLAYS_MAX][2][UI_KEYS_MAX][2]; 
  Boolean keysetSet[Xvars::DISPLAYS_MAX];

  UIinput inputs[UI_VIEWPORTS_MAX];  // Meaningful iff inputsSet[].
  Boolean inputsSet[UI_VIEWPORTS_MAX];

  Dim roomDim;
  UIsettings settings;
  UImask settingsChanges;
  Boolean otherInput;
  Boolean cursorDefined[UI_VIEWPORTS_MAX];
  Boolean weaponKeyDown[UI_VIEWPORTS_MAX];
  ITcommand commandRepeat[UI_VIEWPORTS_MAX]; // For key repeat.

  ITcommand weaponCommandDefault[UI_VIEWPORTS_MAX];

  Boolean neverReset; /* So can't scroll with title screen. */

  Size menusSize[Xvars::DISPLAYS_MAX];
  Size roomSize[Xvars::DISPLAYS_MAX];
  Size arenaSize[Xvars::DISPLAYS_MAX];
  Size statusesSize[Xvars::DISPLAYS_MAX];
  Size intelsPlayingSize[Xvars::DISPLAYS_MAX];
  Size messageBarSize[Xvars::DISPLAYS_MAX];
  Size viewportSize[Xvars::DISPLAYS_MAX];

  // Must redraw all Panels after Pause/Unpause.
  TextPanel *menus[UI_VIEWPORTS_MAX][UI_MENUS_PRIMARY_NUM];
  /* Takes advantage of fact that menuControls and menuLearnControls are
     the first two elements of the enumeration. */
  int menusNum[UI_VIEWPORTS_MAX]; 
  TextPanel *statuses[UI_VIEWPORTS_MAX][UI_STATUSES_NUM];
  TextPanel *humansPlaying[UI_VIEWPORTS_MAX];
  TextPanel *enemiesPlaying[UI_VIEWPORTS_MAX];
  TextPanel *messageBars[UI_VIEWPORTS_MAX];
  TextPanel *levels[UI_VIEWPORTS_MAX];
  Timer messageTimer;
  Boolean redrawAll[UI_VIEWPORTS_MAX]; 
  Boolean pause;
  Boolean levelTitle; // The Ui is displaying the title of a level.
  char levelTitleString[UI_SHORT_STRING_LENGTH];
  Boolean polCorrect;
  // The actual control learning panel.
  KeyPressPanel *learnControls[Xvars::DISPLAYS_MAX];  

  struct LControls {
    int input,key,which;
  };
  LControls lControls[Xvars::DISPLAYS_MAX];
};



class Panel {
 public:
  Panel(UiP ui,int dpyNum,int viewportNum,const Xvars &,
	Window,const Pos &,const Size &,
	unsigned int eventMask = ExposureMask);
  /* EFFECTS: Creatue a new panel at position pos of size size.  The positio
     and size includes the border.  Color is initially black.  Use a 
     viewportNum of -1 if there is no associated viewportNum. */
  /* NOTE: Must explicity set dpyNum as a Panel may be created with 
     viewportNum = -1. */

  UiP get_ui() {return ui;}

  Pixel get_foreground() {return foreground;}
  Pixel get_background() {return background;}
  Size get_size() {return size;}
  Window get_window() {return window;}
  const Xvars &get_xvars() {return xvars;}

  int get_viewport_num() {assert(viewportNum >= 0); return viewportNum;}
  int get_dpy_num() {return dpyNum;}

  void set_foreground(Pixel c,Boolean re_draw = True);
  void set_background(Pixel c,Boolean re_draw = True);
  /* EFFECTS: Changes the color for the panel.  Redraws iff re_draw. */

  virtual Boolean process_event(int dpyNum,XEvent *event);
  /* EFFECTS: Panel deals with the event if it applies.  Returns True iff it
     used the event. */
  /* NOTE: Only calls up the tree if event is not processed at current 
     level. */

  virtual void redraw() = 0;
  virtual void clear() = 0;


 private:
  const Xvars &xvars;
  int dpyNum; // redundant.  Same as ui->get_dpy_num(viewportNum).
  Window window;
  int viewportNum;
  Size size; // Inside the border.
  Pixel foreground,background;
  UiP ui;
};



class TextPanel: public Panel {
 public:
  TextPanel(UiP ui,int dpyNum,int viewportNum,const Xvars &xvars,
	    Window parent,const Pos &pos,const Size &size,
	    const char *msg = NULL,unsigned int eventMask = ExposureMask);
  
  void set_message(const char *msg);
  /* NOTE:  Makes its own copy.  Redraws. */

  static Size get_unit(XFontStruct *,int cols,int rows = 1);

  virtual void redraw();
  virtual void clear();
  
  void set_sensitive(Boolean val);
  Boolean get_sensitive() {return sensitive;}


 private:
  char message[UI_STRING_LENGTH];
  Boolean sensitive;
};



class WritePanel : public TextPanel {
/* OVERVIEW: Accepts text input.  Displays "<prompt><input>_" as its 
   message. */

 public:
  WritePanel(UiP ui,int dpyNum,int viewportNum,const Xvars &xvars,
	     Window parent,
	     const Pos &pos,const Size &size,
	     void (*callback)(UiP,const char *),
	     const char *prompt = NULL,
	     unsigned int eventMask = 
	     ExposureMask | ButtonPressMask | KeyPressMask);
  /* NOTE: prompt appears before the user's input text. */
  
  const char *get_value() {return value;}
  void set_value(const char *value);

  virtual Boolean process_event(int dpyNum,XEvent *event);

  
 private:
  void update_message();

  Boolean active;
  char prompt[UI_STRING_LENGTH];
  char value[UI_STRING_LENGTH];
  void (*callback)(UiP,const char *);
};



class KeyPressPanel: public TextPanel {
 public:
  KeyPressPanel(UiP ui,int dpyNum,int viewportNum,const Xvars &xvars,
		Window parent,
		const Pos &pos,const Size &size,
		void (*callback)(UiP,KeyPressPanel *,XEvent *),
		const char *msg = NULL,
		unsigned int eventMask = ExposureMask | KeyPressMask);
  
  virtual Boolean process_event(int dpyNum,XEvent *event);

  
 private:
  void (*callback)(UiP,KeyPressPanel *,XEvent *);
};



class ButtonPanel: public TextPanel {
 public:
  ButtonPanel(UiP ui,int dpyNum,int viewportNum,const Xvars &xvars,
	      Window parent,
	      const Pos &pos,const Size &size,
	      void (*callback)(UiP,int,int),
	      const char *msg = NULL,
	      unsigned int eventMask = ExposureMask | ButtonPressMask);
  
  virtual Boolean process_event(int dpyNum,XEvent *event);


 private:
  void (*callback)(UiP,int vNum,int button);
};



class TogglePanel: public TextPanel {
 public:
  TogglePanel(UiP ui,int dpyNum,int viewportNum,const Xvars &xvars,
	      Window parent,
	      const Pos &pos,const Size &size,
	      void (*callback)(UiP,TogglePanel *,Boolean),
	      const char *msg,
	      unsigned int eventMask = ExposureMask | ButtonPressMask);
  
  Boolean get_value() {return set;}
  void set_value(Boolean);

  virtual Boolean process_event(int dpyNum,XEvent *event);


 private:
  Boolean set;
  void (*callback)(UiP,TogglePanel *,Boolean);
};
#endif
