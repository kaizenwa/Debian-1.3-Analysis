// "ui.C"
// Also has class Panel.

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
#pragma implementation "ui.h"
#endif



// Include Files
#include "utils.h"
extern "C"
{
#include <string.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
}

#include <iostream.h>
#include <strstream.h>

#include "coord.h"
#include "ui.h"
#include "world.h"
#include "locator.h"
#include "id.h"
#include "physical.h"
#include "bitmaps/ui/ui.bitmaps"



// Defines
#define ROW_SHIFT 5 // For viewports with no object.
#define COL_SHIFT 4 // For viewports with no object.
#define ARENA_BORDER 4
#define FONT_NAME "9x15"
#define MESSAGE_TIME 3
#define RED_COLOR_NAME "red"
#define GREEN_COLOR_NAME "green4"
#define KEYSYM_BUFFER 10

// Number of characters across for each menu button.
#define QUIT_LINE_LENGTH 5
#define NEW_GAME_LINE_LENGTH 9
#define HUMANS_NUM_LINE_LENGTH 9
#define ENEMIES_NUM_LINE_LENGTH 13
#define ENEMIES_REFILL_LINE_LENGTH 19
#define CONTROLS_LINE_LENGTH 13
#define LEARN_CONTROLS_LINE_LENGTH 12
#define STATUS_LINE_LENGTH 25
#define STYLE_LINE_LENGTH 11
#define SCENARIOS_LINE_LENGTH 9
#define LEVELS_LINE_LENGTH 6
#define KILL_LINE_LENGTH 16
#define DUEL_LINE_LENGTH 4
#define EXTENDED_LINE_LENGTH 13
#define TRAINING_LINE_LENGTH 8
#define QUANTA_LINE_LENGTH 14

#define CONTROLS_COLS 90
#define CONTROLS_ROWS (UI_KEYS_MAX + 5)  // + 8
#define LEARN_CONTROLS_COLS 72
#define LEARN_CONTROLS_ROWS 10


enum {menuControls,menuLearnControls, // These two MUST be first.
      menuQuit,menuNewGame,menuHumansNum,menuEnemiesNum,menuEnemiesRefill,
      menuStyle,menuScenarios,menuLevels,menuKill,menuDuel,menuExtended,
      menuTraining,menuQuanta};

enum {statusName,statusClassName,statusHealth,statusMass,statusWeapon,
      statusItem,statusLivesHKills,statusKillsMKills};


const static UIkeymap keys_right_sun3 = 
{
  {XK_R11},{XK_Right},{XK_R15},{XK_Down},{XK_R13},
  {XK_Left},{XK_R7},{XK_Up},{XK_R9},
  {XK_R1},{XK_R2},{XK_R3},
  {XK_R4},{XK_R5},{XK_R6},
};

const static UIkeymap keys_right_decmips =
{
  {XK_KP_5},{XK_KP_6},{XK_KP_3},{XK_KP_2},{XK_KP_1},
  {XK_KP_4},{XK_KP_7},{XK_KP_8},{XK_KP_9},
  {XK_Find},{XK_Insert},{268500736},
  {XK_Select},{XK_Prior},{XK_Next},
}; 

const static UIkeymap keys_right_iris =
{
  {XK_KP_5},{XK_KP_6},{XK_KP_3},{XK_KP_2},{XK_KP_1},
  {XK_KP_4},{XK_KP_7},{XK_KP_8},{XK_KP_9},
  {XK_Insert},{XK_Home},{XK_Prior},
  {XK_Delete},{XK_End},{XK_Next},
}; 

const static UIkeymap keys_right_ncd =
{
  {XK_KP_5},{XK_KP_6},{XK_KP_3},{XK_KP_2},{XK_KP_1},
  {XK_KP_4},{XK_KP_7},{XK_KP_8},{XK_KP_9},
  {XK_Left},{XK_Down},{XK_Right},
  {XK_KP_F1},{XK_KP_F2},{XK_KP_F3},
}; 

const static UIkeymap keys_right_sun4 =
{
  {XK_KP_5},{XK_KP_6},{XK_KP_3},{XK_KP_2},{XK_KP_1},
  {XK_KP_4},{XK_KP_7},{XK_KP_8},{XK_KP_9},
  {XK_Insert},{XK_Home},{XK_Prior},
  {XK_Delete,XK_BackSpace},{XK_End},{XK_Next},
}; 

const static UIkeymap keys_right_sun4_sparc =
{
  {XK_F31},{XK_Right},{XK_F35},{XK_Down},{XK_F33},
  {XK_Left},{XK_F27},{XK_Up},{XK_F29},
  {XK_F21},{XK_F22},{XK_F23},
  {XK_F24},{XK_F25},{XK_F26},
}; 


const static UIkeymap keys_right_mac =
{
  {XK_KP_5},{XK_KP_6},{XK_KP_3},{XK_KP_2},{XK_KP_1},
  {XK_KP_4},{XK_KP_7},{XK_KP_8},{XK_KP_9},
  {XK_Help},{XK_Home},{XK_Prior},
  {XK_Select},{XK_End},{XK_Next},
}; 


const static UIkeymap keys_left_all =
  {
    {XK_l,XK_l},{XK_semicolon,XK_semicolon},{XK_slash,XK_slash},{XK_period,XK_period},
    {XK_m,XK_comma},
    {XK_k,XK_k},{XK_i,XK_i},{XK_o,XK_o},{XK_p,XK_bracketleft},
    {XK_a},{XK_s},{XK_d},
    {XK_z},{XK_x},{XK_c},
  };


const static UIkeymap *keymaps_preset[][2] = 
{
  {&keys_right_sun3, &keys_left_all},
  {&keys_right_decmips, &keys_left_all},
  {&keys_right_iris, &keys_left_all},
  {&keys_right_ncd, &keys_left_all},
  {&keys_right_iris, &keys_left_all},
  {&keys_right_sun4, &keys_left_all},
  {&keys_right_sun4, &keys_left_all},
  {&keys_right_sun4_sparc, &keys_left_all},
  {&keys_right_mac,&keys_left_all},
  {&keys_right_decmips,&keys_left_all},
};  



// Functions
Ui::Ui(int *agc, char **agv, WorldP w, LocatorP l,char **d_names,
       Boolean pol_correct) 
{
  argc = *agc;
  argv = agv;
  displayNames = d_names;
  polCorrect = pol_correct;

  world = w;
  locator = l;

  roomDim = world->get_room_dim();

  intelsSet[0] = False;

  inputsSet[0] = False;
  
  viewportBoxes[0].loc.r = 0;
  viewportBoxes[0].loc.c = 0;
  viewportBoxes[0].dim = roomDim;
  cursorDefined[0] = False;
  weaponKeyDown[0] = False;
  weaponCommandDefault[0] = IT_CENTER;
  commandRepeat[0] = IT_NO_COMMAND;

  settingsChanges = UInone;
  otherInput = False;
  pause = False;

  for (int dpyNum = 0; dpyNum < Xvars::DISPLAYS_MAX; dpyNum++)
    vIndexNum[dpyNum] = vIndexMax[dpyNum] = 0;
  init_x();

  for (int n = 0; n < xvars.dpyMax; n++)
    keysetSet[n] = False;

  init_sizes();
  // Ui::world must be valid.

  create_controls();
  create_learn_controls();
  
  create_toplevel(0);
  vIndexNum[0] = 1;
  viewportsNum = 1;

  neverReset = True;

  Timer nTimer(MESSAGE_TIME);
  messageTimer = nTimer;

  redrawAll[0] = True;
  levelTitle = False;
}



int Ui::add_viewport() 
{
  assert(viewportsNum < UI_VIEWPORTS_MAX);
  
  inputsSet[viewportsNum] = False;
  intelsSet[viewportsNum] = False;

  viewportBoxes[viewportsNum].loc.r = 0;
  viewportBoxes[viewportsNum].loc.c = 0;
  viewportBoxes[viewportsNum].dim = roomDim;
  cursorDefined[viewportsNum] = False;
  weaponKeyDown[viewportsNum] = False;
  weaponCommandDefault[viewportsNum] = IT_CENTER;
  commandRepeat[viewportsNum] = IT_NO_COMMAND;
  redrawAll[viewportsNum] = True;

  create_toplevel(viewportsNum);

  // Update maps between displays and viewports.
  int dpyNum = dpyIndex[viewportsNum];
  assert(vIndex[dpyNum][vIndexNum[dpyNum]] == viewportsNum);
  assert(vIndexNum[dpyNum] < vIndexMax[dpyNum]);
  vIndexNum[dpyNum]++;
  viewportsNum++;
  return viewportsNum - 1;
}



void Ui::set_pause(Boolean val)
{
  if (pause != val)
    {
      for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
	{
	  if (val)
	    {
	      XClearWindow(xvars.dpy[dpyNum],xdata.controls[dpyNum]);
	      
	      learnControls[dpyNum]->clear();
	      
	      for (int v = 0; v < vIndexNum[dpyNum]; v++)
		{
		  XClearWindow(xvars.dpy[dpyNum],
			       xdata.arenas[vIndex[dpyNum][v]]);
		  for (int mm = 0; mm < menusNum[vIndex[dpyNum][v]]; mm++)
		    menus[vIndex[dpyNum][v]][mm]->clear();
		  for (int s = 0; s < UI_STATUSES_NUM; s++)
		    statuses[vIndex[dpyNum][v]][s]->clear();
		  humansPlaying[vIndex[dpyNum][v]]->clear();
		  enemiesPlaying[vIndex[dpyNum][v]]->clear();
		  messageBars[vIndex[dpyNum][v]]->clear();
		  levels[vIndex[dpyNum][v]]->clear();
		}
	    }
	  else
	    {
	      controls_redraw(dpyNum);
	      
	      learnControls[dpyNum]->redraw();
	      
	      for (int v = 0; v < vIndexNum[dpyNum]; v++)
		{
		  redrawAll[vIndex[dpyNum][v]] = True;
		  for (int mm = 0; mm < menusNum[vIndex[dpyNum][v]]; mm++)
		    menus[vIndex[dpyNum][v]][mm]->redraw();
		  for (int s = 0; s < UI_STATUSES_NUM; s++)
		    statuses[vIndex[dpyNum][v]][s]->redraw();
		  humansPlaying[vIndex[dpyNum][v]]->redraw();
		  enemiesPlaying[vIndex[dpyNum][v]]->redraw();
		  messageBars[vIndex[dpyNum][v]]->redraw();
		  levels[vIndex[dpyNum][v]]->redraw();
		}
	    }
	} // for
      pause = val;
    }
}



void Ui::set_level_title(char *msg)
{
  assert(strlen(msg) < UI_SHORT_STRING_LENGTH);
  strcpy(levelTitleString,msg);
  levelTitle = True;
  set_redraw();
}



void Ui::unset_level_title()
{
  levelTitle = False;
  set_redraw();
}



void Ui::del_viewport()
{
  assert(viewportsNum > 1);
  viewportsNum--;

  int dpyNum = dpyIndex[viewportsNum];
  vIndexNum[dpyNum]--;
  assert(vIndex[dpyNum][vIndexNum[dpyNum]] == viewportsNum);

  XDestroyWindow(xvars.dpy[dpyNum],
		 xdata.toplevels[viewportsNum]);
}



void Ui::set_humans_num(int val)
{
  settings.humansNum = val;

  ostrstream str;
  str << val << ends;
  ((WritePanel *)menus[0][menuHumansNum])->set_value(str.str());
  delete str.str();
}



Boolean Ui::command_repeatable(ITcommand command)
{
  switch (command) {
  case IT_CENTER:
  case IT_R:
  case IT_DN_R:
  case IT_DN:
  case IT_DN_L:
  case IT_L:
  case IT_UP_L:
  case IT_UP:
  case IT_UP_R:
    return True;
  default:
    return False;
  }
}



void Ui::set_enemies_num(int val)
{
  settings.enemiesNum = val;

  ostrstream str;
  str << val << ends;
  ((WritePanel *)menus[0][menuEnemiesNum])->set_value(str.str());
  delete str.str();
}



void Ui::set_enemies_refill(Boolean val)
{
  settings.enemiesRefill = val;

  ((TogglePanel *)menus[0][menuEnemiesRefill])->set_value(val);
}



void Ui::set_style(UIsettings::Style style)
{
  settings.style = style;

  ((TogglePanel *)menus[0][menuScenarios])->
    set_value(style == UIsettings::SCENARIOS);
  ((TogglePanel *)menus[0][menuLevels])->
    set_value(style == UIsettings::LEVELS);
  ((TogglePanel *)menus[0][menuKill])->set_value(style == UIsettings::KILL);
  ((TogglePanel *)menus[0][menuDuel])->set_value(style == UIsettings::DUEL);
  ((TogglePanel *)menus[0][menuExtended])->
    set_value(style == UIsettings::EXTENDED);
  ((TogglePanel *)menus[0][menuTraining])->
    set_value(style == UIsettings::TRAINING);

  menus[0][menuHumansNum]->set_sensitive(style != UIsettings::TRAINING);
  menus[0][menuEnemiesNum]->set_sensitive(style == UIsettings::KILL || 
					   style == UIsettings::DUEL ||
					   style == UIsettings::EXTENDED);
  menus[0][menuEnemiesRefill]->set_sensitive(style == UIsettings::KILL ||
					      style == UIsettings::DUEL ||
					      style == UIsettings::EXTENDED);
}



void Ui::set_quanta(Quanta quanta)
{
  settings.quanta = quanta;

  ostrstream str;
  str << quanta << ends;
  ((WritePanel *)menus[0][menuQuanta])->set_value(str.str());
  delete str.str();
}



void Ui::set_humans_playing(int val)
{
  ostrstream msg;
  msg << "Humans remaining: " << val << ends; 
  char *msg_str = msg.str();

  for (int n = 0; n < viewportsNum; n++)
    humansPlaying[n]->set_message(msg_str);
  delete msg_str;
}



void Ui::set_enemies_playing(int val)
{
  ostrstream msg;
  msg << "Machines remaining: " << val << ends; 
  char *msg_str = msg.str();

  for (int n = 0; n < viewportsNum; n++)
    enemiesPlaying[n]->set_message(msg_str);
  delete msg_str;
}



void Ui::set_level(const char *val)
{
  for (int n = 0; n < viewportsNum; n++)
    levels[n]->set_message(val);
}



void Ui::set_input(int n,UIinput input)
{
  assert(n >= 0 && n < viewportsNum);
  inputs[n] = input; 
  inputsSet[n] = True;
}



void Ui::set_keyset(int dpyNum,UIkeyset ks)
{
  for (int input = 0; input < 2; input++)
    for (int n = 0; n < UI_KEYS_MAX; n++)
      for (int which = 0; which < 2; which++)
	keycodes[dpyNum][input][n][which] = 
	  XKeysymToKeycode(xvars.dpy[dpyNum],
			   (*keymaps_preset[ks][input])[n][which]);

  keysetSet[dpyNum] = True;
}



void Ui::set_keyset(int dpyNum,UIkeyset ks,
		    KeySym right[][2],KeySym left[][2])
{
  int n;
  for (n = 0; n < UI_KEYS_MAX; n++)
    if (right[n][0])
      {
	keycodes[dpyNum][UI_KEYS_RIGHT][n][0] = 
	  XKeysymToKeycode(xvars.dpy[dpyNum],right[n][0]);
	if (right[n][1])
	  keycodes[dpyNum][UI_KEYS_RIGHT][n][1] = 
	    XKeysymToKeycode(xvars.dpy[dpyNum],right[n][1]);
	else
	  keycodes[dpyNum][UI_KEYS_RIGHT][n][1] = 
	    XKeysymToKeycode(xvars.dpy[dpyNum],right[n][0]);
      }
    else
      {
	keycodes[dpyNum][UI_KEYS_RIGHT][n][0] = 
	  XKeysymToKeycode(xvars.dpy[dpyNum],
			   (*keymaps_preset[ks][UI_KEYS_RIGHT])[n][0]);
	keycodes[dpyNum][UI_KEYS_RIGHT][n][1] = 
	  XKeysymToKeycode(xvars.dpy[dpyNum],
			   (*keymaps_preset[ks][UI_KEYS_RIGHT])[n][1]);
      }

  for (n = 0; n < UI_KEYS_MAX; n++)
    if (left[n][0])
      {
	keycodes[dpyNum][UI_KEYS_LEFT][n][0] = 
	  XKeysymToKeycode(xvars.dpy[dpyNum],left[n][0]);
	if (left[n][1])
	  keycodes[dpyNum][UI_KEYS_LEFT][n][1] = 
	    XKeysymToKeycode(xvars.dpy[dpyNum],left[n][1]);
	else
	  keycodes[dpyNum][UI_KEYS_LEFT][n][1] = 
	    XKeysymToKeycode(xvars.dpy[dpyNum],left[n][0]);
      }
    else
      {
	keycodes[dpyNum][UI_KEYS_LEFT][n][0] = 
	  XKeysymToKeycode(xvars.dpy[dpyNum],
			   (*keymaps_preset[ks][UI_KEYS_LEFT])[n][0]);
	keycodes[dpyNum][UI_KEYS_LEFT][n][1] = 
	  XKeysymToKeycode(xvars.dpy[dpyNum],
			   (*keymaps_preset[ks][UI_KEYS_LEFT])[n][1]);
      }
  keysetSet[dpyNum] = True;
}



UImask Ui::get_settings(UIsettings &s)
{
  s = settings; 
  UImask tmp = settingsChanges;
  settingsChanges = UInone;
  return tmp;
}



void Ui::register_intel(int n, IntelP intel)
{
  assert(n >= 0 && n < viewportsNum);
  
  intelsSet[n] = True;
  intels[n] = intel;

  if (intel->is_human())
    {
      // Set colors of status panels to be those of the human.
      Pixel pixel = 
	xvars.humanColors[dpyIndex[n]][((HumanP)intel)->get_color_num()];
      for (int num = 0; num < UI_STATUSES_NUM; num++)
	statuses[n][num]->set_foreground(pixel);
    }
}



void Ui::unregister_intel(int n)
{
  assert((n >= 0) && (n < UI_VIEWPORTS_MAX));
  intelsSet[n] = False;
}



void Ui::reset()
{
  for (int n = 0; n < viewportsNum; n++)
    {
      intelsSet[n] = False;
      redrawAll[n] = True;
      for (int m = 0; m < UI_STATUSES_NUM; m++)
	{
	  statuses[n][m]->set_message("");
	  statuses[n][m]->set_foreground(xvars.black[dpyIndex[n]]);
	}
      viewportBoxes[n].loc.r = viewportBoxes[n].loc.c = 0;
    }
  neverReset = False;
  levelTitle = False;
}



void Ui::set_redraw()
{
  for (int n = 0; n < viewportsNum; n++)
    redrawAll[n] = True;
}



void Ui::process_event(int dpyNum,XEvent *event)
{
  if (!pause)
    {
      // Check arenas before others for efficiency.
      int vNum = get_viewport_num(dpyNum,event->xany.window);

      if (vNum != -1)
	{
	  assert(dpyIndex[vNum] == dpyNum);
	  switch(event->type) {
	  case Expose:
	    viewport_expose(vNum);
	    break;
	  case KeyPress:
	    viewport_key_press(dpyNum,event);
	    break;
	  case KeyRelease:
	    viewport_key_release(dpyNum,event);
	    break;
	  case ButtonPress:
	    viewport_button_press(vNum,event);
	    break;
	  default:
	    cerr << "Warning: Unexpected event of type" << event->type 
	      << endl;
	    break;
	  };
	  return;
	}
      
      if (event->xany.window == xdata.controls[dpyNum] && 
	  event->type == Expose)
	{
	  controls_expose(dpyNum,event);
	  return;
	}
	  
      if (learnControls[dpyNum]->process_event(dpyNum,event))
	return;
      
      for (int v = 0; v < vIndexNum[dpyNum]; v++)
	{
	  int m;
	  for (m = 0; m < menusNum[vIndex[dpyNum][v]]; m++)
	    if (menus[vIndex[dpyNum][v]][m]->process_event(dpyNum,event))
	      return;
	  
	  for (m = 0; m < UI_STATUSES_NUM; m++)
	    if (statuses[vIndex[dpyNum][v]][m]->process_event(dpyNum,event))
	      return;
	  
	  if (humansPlaying[vIndex[dpyNum][v]]->process_event(dpyNum,event))
	    return;
	  
	  if (enemiesPlaying[vIndex[dpyNum][v]]->process_event(dpyNum,event))
	    return;
	  
	  if (messageBars[vIndex[dpyNum][v]]->process_event(dpyNum,event))
	    return;

	  if (levels[vIndex[dpyNum][v]]->process_event(dpyNum,event))
	    return;
	}
    }
  else if (event->type == KeyPress || event->type == ButtonPress)
    {
      settingsChanges |= UIpause;
      settings.pause = False;
    }
}



void Ui::clock()
{
  int dpyNum;
  for (dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    assert(keysetSet[dpyNum]);
  
  if (!pause)
    {
      otherInput = False;
      
      // Deal with keyboard repeated commands.
// DISABLED
//      for (int vn = 0; vn < viewportsNum; vn++)
//	if (commandRepeat[vn] != IT_NO_COMMAND)
//	  dispatch(vn,commandRepeat[vn]);
      
      // Follow intel and draw.
      for (int n = 0; n < viewportsNum; n++)
	{
	  PhysicalP p;
	  
	  // Before check for death.
	  update_statuses(n);
	  
	  if (intelsSet[n])
	    {
	      if (locator->lookup(p,intels[n]->get_id()) == OL_NO_SIG)
		{
		  Area a = p->get_area();
		  if (viewport_to_loc(n,a.middle_wsquare()))
		    redrawAll[n] = True;
		}
	      else if (!intels[n]->alive() && intels[n]->get_lives() == 0)
		unregister_intel(n);
	    }
	  
	  draw(n,!redrawAll[n]);
	}    
      
      
      // Update message bar.
      if (messageTimer.ready())
	{
	  char *msg;
	  if (msg = locator->message_deq())
	    {
	      set_message(msg);
	      delete msg;
	      messageTimer.set();
	    }
	}
      messageTimer.clock();
    }      
  
  for (dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    if (vIndexNum[dpyNum] > 0)
      XSync(xvars.dpy[dpyNum],False);
}



void Ui::menu_quit_CB(UiP ui,int vNum,int)
{
  assert(vNum == 0);
  ui->settingsChanges |= UIquit;
}



void Ui::menu_new_game_CB(UiP ui,int vNum,int)
{
  assert(vNum == 0);
  ui->settingsChanges |= UInewGame;
}



void Ui::menu_humans_num_CB(UiP ui,const char *value)
{
  ui->settingsChanges |= UIhumansNum; 
  ui->settings.humansNum = atoi(value);
}



void Ui::menu_enemies_num_CB(UiP ui,const char *value)
{
  ui->settingsChanges |= UIenemiesNum; 
  ui->settings.enemiesNum = atoi(value);
}



void Ui::menu_enemies_refill_CB(UiP ui,TogglePanel *,Boolean val)
{
  ui->settingsChanges |= UIenemiesRefill;
  ui->settings.enemiesRefill = val;
}



void Ui::menu_controls_CB(UiP ui,TogglePanel *toggle,Boolean val)
{
  int dpyNum = toggle->get_dpy_num();
  if (val)
    XMapWindow(ui->xvars.dpy[dpyNum],ui->xdata.controls[dpyNum]);
  else
    XUnmapWindow(ui->xvars.dpy[dpyNum],ui->xdata.controls[dpyNum]);
}



void Ui::menu_learn_controls_CB(UiP ui,TogglePanel *toggle,Boolean active)
{
  int dpyNum = toggle->get_dpy_num();
  if (active)
    {
      ui->lControls[dpyNum].input = ui->lControls[dpyNum].key = 
	ui->lControls[dpyNum].which = 0;
      
      ostrstream str;
      str << "There are 2 sets of controls for the players on the right and" 
	  << "\n"
	  << "left sides of the keyboard, respectively." << "\n"
	  << "Each player has " << UI_KEYS_MAX << " commands.  "
	  << "Two physical keys on the keyboard can" << "\n"
	  << "be mapped to each command.  This is useful when two keys "
	  << "are physically" 
	  << "\n"
	  << "close to each other and you would like both of them to perform" 
	  << "\n" << "the same action." << "\n"
	  << "\n"
	  << "Place the cursor in this window and type the prompted key."
	  << "\n" << "\n"
	  << (ui->lControls[dpyNum].input ? "Left" : "Right") 
	  << " player enter the " 
	  << (ui->lControls[dpyNum].which ? "second" : "first") 
	  << " key for <" << ui->keysNames[ui->lControls[dpyNum].key] 
	  << ">." << ends;
      
      ui->learnControls[dpyNum]->set_message(str.str());
      delete str.str();
      XMapWindow(ui->xvars.dpy[dpyNum],ui->xdata.learnControls[dpyNum]);
    }
  else
    XUnmapWindow(ui->xvars.dpy[dpyNum],ui->xdata.learnControls[dpyNum]);
}



void Ui::menu_scenarios_CB(UiP ui,TogglePanel *t,Boolean val)
{
  if (!val)
    t->set_value(True); // Gives radio box behavior.
  else
    {
      ui->settingsChanges |= UIstyle;
      ui->settings.style = UIsettings::SCENARIOS;
    }
}



void Ui::menu_levels_CB(UiP ui,TogglePanel *t,Boolean val)
{
  if (!val)
    t->set_value(True); // Gives radio box behavior.
  else
    {
      ui->settingsChanges |= UIstyle;
      ui->settings.style = UIsettings::LEVELS;
    }
}



void Ui::menu_kill_CB(UiP ui,TogglePanel *t,Boolean val)
{
  if (!val)
    t->set_value(True);
  else
    {
      ui->settingsChanges |= UIstyle;
      ui->settings.style = UIsettings::KILL;
    }
}



void Ui::menu_duel_CB(UiP ui,TogglePanel *t,Boolean val)
{
  if (!val)
    t->set_value(True);
  else
    {
      ui->settingsChanges |= UIstyle;
      ui->settings.style = UIsettings::DUEL;
    }
}



void Ui::menu_extended_CB(UiP ui,TogglePanel *t,Boolean val)
{
  if (!val)
    t->set_value(True);
  else
    {
      ui->settingsChanges |= UIstyle;
      ui->settings.style = UIsettings::EXTENDED;
    }
}



void Ui::menu_training_CB(UiP ui,TogglePanel *t,Boolean val)
{
  if (!val)
    t->set_value(True);
  else
    {
      ui->settingsChanges |= UIstyle;
      ui->settings.style = UIsettings::TRAINING;
    }
}



void Ui::menu_quanta_CB(UiP ui,const char *value)
{
  ui->settingsChanges |= UIquanta; 
  ui->settings.quanta = atoi(value);
}



void Ui::status_weapon_CB(UiP ui,int viewportNum,int button)
{
  switch (button) {
  case Button1:
    ui->dispatch(viewportNum,IT_WEAPON_CENTER);
    break;
  case Button2:
    ui->dispatch(viewportNum,IT_WEAPON_CHANGE);
    break;
  case Button3:
    ui->dispatch(viewportNum,IT_WEAPON_DROP);
    break;
  };
}



void Ui::status_item_CB(UiP ui,int viewportNum,int button)
{
  switch (button) {
  case Button1:
    ui->dispatch(viewportNum,IT_ITEM_USE);
    break;
  case Button2:
    ui->dispatch(viewportNum,IT_ITEM_CHANGE);
    break;
  case Button3:
    ui->dispatch(viewportNum,IT_ITEM_DROP);
    break;
  };
}



void Ui::learn_controls_CB(UiP ui,KeyPressPanel *panel,XEvent *event)
{
  int dpyNum = panel->get_dpy_num();

  // Viewport containing the [Learn Controls] button.  panel has no viewport.
  int vNum = ui->vIndex[dpyNum][0]; 

  assert(event->type == KeyPress && 
	 ((TogglePanel *)ui->menus[vNum][menuLearnControls])->get_value());
  ostrstream str;
  str << (ui->lControls[dpyNum].input ? "Left" : "Right") 
      << " player: The "
      << (ui->lControls[dpyNum].which ? "second" : "first") << " key for <" 
      << ui->keysNames[ui->lControls[dpyNum].key] << "> is "
      << XKeysymToString(XKeycodeToKeysym(ui->xvars.dpy[dpyNum],
					  event->xkey.keycode,0))
      << "." << "\n" << "\n";
  ui->keycodes[dpyNum][ui->lControls[dpyNum].input]
    [ui->lControls[dpyNum].key][ui->lControls[dpyNum].which] =
      event->xkey.keycode;
  
  if (ui->lControls[dpyNum].which < 1)
    ui->lControls[dpyNum].which++;
  else if (ui->lControls[dpyNum].key < (UI_KEYS_MAX - 1))
    {
      ui->lControls[dpyNum].key++;
      ui->lControls[dpyNum].which = 0;
    }
  else if (ui->lControls[dpyNum].input < 1)
    {
      ui->lControls[dpyNum].input++;
      ui->lControls[dpyNum].key = 0;
      ui->lControls[dpyNum].which = 0;
    }
  else
    {
      ((TogglePanel *)ui->menus[vNum][menuLearnControls])->set_value(False);
      XUnmapWindow(ui->xvars.dpy[dpyNum],ui->xdata.learnControls[dpyNum]);
    }
  
  str << (ui->lControls[dpyNum].input ? "Left" : "Right") 
      << " player enter the " 
      << (ui->lControls[dpyNum].which ? "second" : "first") 
      << " key for <" << keysNames[ui->lControls[dpyNum].key] 
	<< ">." << ends;
  ui->learnControls[dpyNum]->set_message(str.str());
  delete str.str();

  ui->controls_redraw(dpyNum);
}



void Ui::set_message(const char *message)
{
  for (int n = 0; n < viewportsNum; n++)
    messageBars[n]->set_message(message);
}



void Ui::draw(int viewportNum,Boolean changedOnly)
{
  assert((viewportNum >= 0) && (viewportNum < viewportsNum));
  int dpyNum = dpyIndex[viewportNum];

  if (levelTitle)
    {
      if (!changedOnly)
	{
	  XClearWindow(xvars.dpy[dpyNum],xdata.arenas[viewportNum]);

	  ostrstream str;
	  str << levelTitleString << ends;
	  Size fontSize;
	  fontSize.width = xvars.font[dpyNum]->max_bounds.width;
	  fontSize.height = xvars.font[dpyNum]->max_bounds.ascent 
	    + xvars.font[dpyNum]->max_bounds.descent;

	  XDrawString(xvars.dpy[dpyNum],xdata.arenas[viewportNum],
		      xvars.gc[dpyNum],
		      (arenaSize[dpyNum].width - 
		       (fontSize.width * strlen(str.str()))) / 2,
		      (arenaSize[dpyNum].height 
		       - fontSize.height) / 2 + 
		      xvars.font[dpyNum]->max_bounds.ascent,
		      str.str(),strlen(str.str()));
	  delete str.str();
	}
    }
  else
    {
      if (!changedOnly)
	world->draw(xdata.arenas[viewportNum],xvars,dpyNum,
		    viewportBoxes[viewportNum]); 
      locator->draw(xdata.arenas[viewportNum],xvars,dpyNum,
		    viewportBoxes[viewportNum]);  
    }  
  if (!changedOnly)
    redrawAll[viewportNum] = False;
}



void Ui::init_x() 
{
  // Initialize Xvars.
  xvars.dpyMax = 0;

  // Assign a display to all viewports.
  for (int vNum = 0; vNum < UI_VIEWPORTS_MAX; vNum++)
    {
      Boolean found = False;
      for (int m = 0; m < vNum && !found; m++)
	// Use already opened display.
	if (!strcmp(displayNames[m],displayNames[vNum]))
	  {
	    menusNum[vNum] = 0;
	    dpyIndex[vNum] = dpyIndex[m];
	    vIndex[dpyIndex[vNum]][vIndexMax[dpyIndex[vNum]]] = vNum;
	    vIndexMax[dpyIndex[vNum]]++;
	    found = True;
	  }
      // Open new display.
      if (!found)
	{
	  if (xvars.dpyMax >= Xvars::DISPLAYS_MAX)
	    {
	      cerr << "Can only open " << Xvars::DISPLAYS_MAX <<
		" different displays." << endl;
	      cerr << "If more displays are desired, recompile with "
		<< "a higher value for Xvars::DISPLAYS_MAX." << endl;
	      exit(1);
	    }
	  if (!(xvars.dpy[xvars.dpyMax] = 
		XOpenDisplay(strlen(displayNames[vNum]) ?
			     displayNames[vNum] :(char *) NULL)))
	    {
	      cerr << "Could not open X display " << displayNames[xvars.dpyMax] 
		<< endl;
	      if (strlen(displayNames[xvars.dpyMax]))
		cerr << "Make sure you have used xhost on " 
		  << displayNames[xvars.dpyMax] 
		    << " to allow this machine to connect to it." << endl;
	      exit(1);
	    }

	  // Initialize a bunch of useful X variables.
	  xvars.scr_ptr[xvars.dpyMax] = 
	    DefaultScreenOfDisplay(xvars.dpy[xvars.dpyMax]);
	  xvars.scr_num[xvars.dpyMax] = 
	    DefaultScreen(xvars.dpy[xvars.dpyMax]);
	  xvars.root[xvars.dpyMax] = 
	    RootWindowOfScreen(xvars.scr_ptr[xvars.dpyMax]);
	  xvars.visual[xvars.dpyMax] = 
	    DefaultVisual(xvars.dpy[xvars.dpyMax],xvars.scr_num[xvars.dpyMax]);
	  xvars.depth[xvars.dpyMax] = 
	    DefaultDepthOfScreen(xvars.scr_ptr[xvars.dpyMax]);
	  xvars.cmap[xvars.dpyMax] = 
	    DefaultColormap(xvars.dpy[xvars.dpyMax],xvars.scr_num[xvars.dpyMax]);
	  xvars.white[xvars.dpyMax] = 
	    WhitePixel(xvars.dpy[xvars.dpyMax],xvars.scr_num[xvars.dpyMax]);
	  xvars.black[xvars.dpyMax] = 
	    BlackPixel(xvars.dpy[xvars.dpyMax],xvars.scr_num[xvars.dpyMax]);
	  xvars.font[xvars.dpyMax] = 
	    XLoadQueryFont(xvars.dpy[xvars.dpyMax],FONT_NAME);
	  if (!xvars.font[xvars.dpyMax])
	    {
	      cerr << "Could not load font " << FONT_NAME;
	      if (strlen(displayNames[vNum]))
		cerr << " on " << displayNames[vNum];
	      cerr << endl;
	      exit (1);
	    }

	  // Create graphics context.
	  XGCValues values;
	  values.graphics_exposures = False;
	  values.font = xvars.font[xvars.dpyMax]->fid;
	  values.foreground = xvars.black[xvars.dpyMax];
	  values.background = xvars.white[xvars.dpyMax];
	  values.stipple = 
	    XCreateBitmapFromData(xvars.dpy[xvars.dpyMax],
				  xvars.root[xvars.dpyMax],INSENSITIVE_BITS,
				  INSENSITIVE_WIDTH,INSENSITIVE_HEIGHT);
	  xvars.gc[xvars.dpyMax] = 
	    XCreateGC(xvars.dpy[xvars.dpyMax],xvars.root[xvars.dpyMax],
		      GCGraphicsExposures | GCFont | GCForeground | 
		      GCBackground | GCStipple,
		      &values);

	  // Some colors.
	  xvars.red[xvars.dpyMax] = 
	    xvars.allocNamedColor(xvars.dpyMax,RED_COLOR_NAME,
				  xvars.black[xvars.dpyMax]);
	  xvars.green[xvars.dpyMax] = 
	    xvars.allocNamedColor(xvars.dpyMax,GREEN_COLOR_NAME,
				  xvars.black[xvars.dpyMax]);
      
	  for (int m = 0; m < Xvars::HUMAN_COLORS_NUM; m++)
	    xvars.humanColors[xvars.dpyMax][m] =
	      xvars.allocNamedColor(xvars.dpyMax,Xvars::humanColorNames[m],
				    xvars.black[xvars.dpyMax]);
      
	  
	  // Initialize xdata.  I.e. values local to the Ui.
	  XColor color;
	  color.red = color.green = color.blue = 0;
	  color.flags = DoRed | DoGreen | DoBlue;

	  Pixmap pixmap = 
	    XCreateBitmapFromData(xvars.dpy[xvars.dpyMax],
				  xvars.root[xvars.dpyMax],CURSOR_BITS,
				  CURSOR_WIDTH,CURSOR_HEIGHT);
	  Pixmap mask = 
	    XCreateBitmapFromData(xvars.dpy[xvars.dpyMax],
				  xvars.root[xvars.dpyMax],CURSOR_MASK_BITS,
				  CURSOR_WIDTH,CURSOR_HEIGHT);
	  xdata.arenaCursor[xvars.dpyMax] = 
	    XCreatePixmapCursor(xvars.dpy[xvars.dpyMax],pixmap,mask,
				&color,&color,CURSOR_HOT_X,CURSOR_HOT_Y);
	  XFreePixmap(xvars.dpy[xvars.dpyMax],pixmap);
	  XFreePixmap(xvars.dpy[xvars.dpyMax],mask);
	  
	  xdata.icon[xvars.dpyMax] = 
	    XCreateBitmapFromData(xvars.dpy[xvars.dpyMax],
				  xvars.root[xvars.dpyMax],ICON_BITS,
				  ICON_WIDTH,ICON_HEIGHT);

	  // Maps between displays and viewports.
	  dpyIndex[vNum] = xvars.dpyMax;
	  if (xvars.dpyMax == 0)
	    {
	      assert(vNum == 0);
	      menusNum[vNum] = UI_MENUS_PRIMARY_NUM;
	    }
	  else
	    menusNum[vNum] = UI_MENUS_SECONDARY_NUM;
	  vIndex[xvars.dpyMax][vIndexNum[xvars.dpyMax]] = vNum;
	  vIndexMax[xvars.dpyMax]++;
	  xvars.dpyMax++;
	}
    }
}



void Ui::init_sizes()
{
  // Cache the sizes of various regions in the viewports.
  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    {
      Size quitUnit = 
	ButtonPanel::get_unit(xvars.font[dpyNum],QUIT_LINE_LENGTH);
      Size styleUnit = 
	TogglePanel::get_unit(xvars.font[dpyNum],STYLE_LINE_LENGTH);
      
      roomSize[dpyNum] = world->get_room_size();
      arenaSize[dpyNum].width = roomSize[dpyNum].width + 2 * ARENA_BORDER;
      arenaSize[dpyNum].height = roomSize[dpyNum].height + 2 * ARENA_BORDER;
      statusesSize[dpyNum] = 
	TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH);
      // Minus 1 because health and mass on the same line and plus .5 to
      // leave a little space.
      statusesSize[dpyNum].height = 
	(int) (statusesSize[dpyNum].height * (UI_STATUSES_NUM - .75));

      intelsPlayingSize[dpyNum] = 
	TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH);
      intelsPlayingSize[dpyNum].height *= 2;
      
      menusSize[dpyNum].height = quitUnit.height + styleUnit.height;
      menusSize[dpyNum].width = arenaSize[dpyNum].width
	+ statusesSize[dpyNum].width;
      
      messageBarSize[dpyNum] = TextPanel::get_unit(xvars.font[dpyNum],1,2);
      messageBarSize[dpyNum].width = arenaSize[dpyNum].width 
	+ statusesSize[dpyNum].width;
      
      viewportSize[dpyNum].width = arenaSize[dpyNum].width
	+ statusesSize[dpyNum].width;
      viewportSize[dpyNum].height = menusSize[dpyNum].height 
	+ arenaSize[dpyNum].height + messageBarSize[dpyNum].height;
    }
}



Boolean Ui::viewport_to_loc(int n,const Loc &l) 
{
  Loc old = viewportBoxes[n].loc;
  viewportBoxes[n].loc.r = roomDim.rowMax * (l.r / roomDim.rowMax);
  viewportBoxes[n].loc.c = roomDim.colMax * (l.c / roomDim.colMax);
  
  return !(old == viewportBoxes[n].loc);
}



void Ui::dispatch(int viewportNum,ITcommand command)
{
//  cout << viewportNum << ": command = " << command << endl;

  if (!neverReset)
    {
      if (! intelsSet[viewportNum])
	{  
	  // No associated intel, so scroll with the keyset. 
	  const Dim worldDim = world->get_dim();
	  Boolean changed = True;
	  
	  // Right.
	  switch (command) {
	  case IT_R:
	    if (viewportBoxes[viewportNum].loc.c  < 
		worldDim.colMax - roomDim.colMax / 2)
	      viewportBoxes[viewportNum].loc.c += COL_SHIFT;
	    break;
	    // Down right.
	  case IT_DN_R:
	    if (viewportBoxes[viewportNum].loc.c  < 
		worldDim.colMax - roomDim.colMax / 2)
	      viewportBoxes[viewportNum].loc.c += COL_SHIFT;
	    if (viewportBoxes[viewportNum].loc.r  < 
		worldDim.rowMax - roomDim.rowMax / 2)
	      viewportBoxes[viewportNum].loc.r += ROW_SHIFT;
	    break;
	    // Down.
	  case IT_DN:
	    if (viewportBoxes[viewportNum].loc.r  < 
		worldDim.rowMax - roomDim.rowMax / 2)
	      viewportBoxes[viewportNum].loc.r += ROW_SHIFT;
	    break;
	    // Down left.
	  case IT_DN_L:
	    if (viewportBoxes[viewportNum].loc.r  < 
		worldDim.rowMax - roomDim.rowMax / 2)
	      viewportBoxes[viewportNum].loc.r += ROW_SHIFT;
	    if (viewportBoxes[viewportNum].loc.c  >= 
		-roomDim.colMax / 2)
	      viewportBoxes[viewportNum].loc.c -= COL_SHIFT;
	    break;
	    // Left.
	  case IT_L:
	    if (viewportBoxes[viewportNum].loc.c  >= 
		-roomDim.colMax / 2)
	      viewportBoxes[viewportNum].loc.c -= COL_SHIFT;
	    break;
	    // Up left.
	  case IT_UP_L:
	    if (viewportBoxes[viewportNum].loc.c  >= 
		-roomDim.colMax / 2)
	      viewportBoxes[viewportNum].loc.c -= COL_SHIFT;
	    if (viewportBoxes[viewportNum].loc.r  >= 
		-roomDim.rowMax / 2)
	      viewportBoxes[viewportNum].loc.r -= ROW_SHIFT;
	    break;
	    // Up.
	  case IT_UP:
	    if (viewportBoxes[viewportNum].loc.r  >= 
		-roomDim.rowMax / 2)
	      viewportBoxes[viewportNum].loc.r -= ROW_SHIFT;
	    break;
	    // Up right.
	  case IT_UP_R:
	    if (viewportBoxes[viewportNum].loc.r  >= 
		-roomDim.rowMax / 2)
	      viewportBoxes[viewportNum].loc.r -= ROW_SHIFT;
	    if (viewportBoxes[viewportNum].loc.c  < 
		worldDim.colMax - roomDim.colMax / 2)
	      viewportBoxes[viewportNum].loc.c += COL_SHIFT;
	    break;
	  default:
	    changed = False;
	  }
	  
	  if (changed)
	    redrawAll[viewportNum] = True;
	}
      else 
	// Only give the command to a human.
	if (intels[viewportNum]->is_human())
	  {
	    HumanP human = (HumanP)intels[viewportNum];
	    human->set_command(command);
	  }
    }
}



int Ui::get_viewport_num(int dpyNum,Window window)
{
  for (int v = 0; v < vIndexNum[dpyNum]; v++)
    if (xdata.arenas[vIndex[dpyNum][v]] == window || 
	xdata.toplevels[vIndex[dpyNum][v]] == window)
      return (vIndex[dpyNum][v]);

  return -1;
}



void Ui::create_controls()
{
  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    {
      Size size;
      size.width = xvars.font[dpyNum]->max_bounds.width * CONTROLS_COLS;
      size.height = 
	(xvars.font[dpyNum]->max_bounds.ascent 
	 + xvars.font[dpyNum]->max_bounds.descent) 
	  * CONTROLS_ROWS;

      xdata.controls[dpyNum] = 
	XCreateSimpleWindow(xvars.dpy[dpyNum],xvars.root[dpyNum],0,0,
			    size.width,size.height,1,
			    xvars.black[dpyNum],xvars.white[dpyNum]);

      XSelectInput(xvars.dpy[dpyNum],xdata.controls[dpyNum],ExposureMask);

      XSizeHints size_hints;
      size_hints.flags = PPosition | PSize | PMinSize;
      size_hints.min_width = size.width;
      size_hints.min_height = size.height;

      XTextProperty windowName, iconName;
      char *window_name = "Controls";  // Will appear on window.
      char *icon_name = "Controls";
      assert(XStringListToTextProperty(&window_name,1,&windowName));
      assert(XStringListToTextProperty(&icon_name,1,&iconName)); 
  
      XWMHints wm_hints;
      wm_hints.initial_state = NormalState;
      wm_hints.input = True;
      wm_hints.icon_pixmap = xdata.icon[dpyNum];
      wm_hints.flags = StateHint | IconPixmapHint | InputHint;

      XClassHint class_hints;
      class_hints.res_name = argv[0];
      class_hints.res_class = "XEvil";

      XSetWMProperties(xvars.dpy[dpyNum],xdata.controls[dpyNum],
		       &windowName,&iconName,
		       argv,argc,&size_hints,&wm_hints,&class_hints);
    }
}



void Ui::create_learn_controls()
{
  for (int dpyNum = 0; dpyNum < xvars.dpyMax; dpyNum++)
    {
      Size size = 
	TextPanel::get_unit(xvars.font[dpyNum],
			    LEARN_CONTROLS_COLS,LEARN_CONTROLS_ROWS);
      xdata.learnControls[dpyNum] = 
	XCreateSimpleWindow(xvars.dpy[dpyNum],xvars.root[dpyNum],0,0,
			    size.width,size.height,1,
			    xvars.black[dpyNum],xvars.white[dpyNum]);

      XSelectInput(xvars.dpy[dpyNum],xdata.learnControls[dpyNum],
		   ExposureMask|KeyPressMask);

      XSizeHints size_hints;
      size_hints.flags = PPosition | PSize | PMinSize;
      size_hints.min_width = size.width;
      size_hints.min_height = size.height;

      XTextProperty windowName, iconName;
      char *window_name = "Set Controls";  // Will appear on window.
      char *icon_name = "Set Controls";
      assert(XStringListToTextProperty(&window_name,1,&windowName));
      assert(XStringListToTextProperty(&icon_name,1,&iconName)); 
  
      XWMHints wm_hints;
      wm_hints.initial_state = NormalState;
      wm_hints.input = True;
      wm_hints.icon_pixmap = xdata.icon[dpyNum];
      wm_hints.flags = StateHint | IconPixmapHint | InputHint;

      XClassHint class_hints;
      class_hints.res_name = argv[0];
      class_hints.res_class = "XEvil";

      XSetWMProperties(xvars.dpy[dpyNum],xdata.learnControls[dpyNum],
		       &windowName,&iconName,
		       argv,argc,&size_hints,&wm_hints,&class_hints);

      Pos pos;
      learnControls[dpyNum] = 
	new KeyPressPanel(this,dpyNum,-1,xvars,xdata.learnControls[dpyNum],
			  pos,size,Ui::learn_controls_CB);
      assert(learnControls[dpyNum]);
    }
}



void Ui::create_toplevel(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  xdata.toplevels[viewportNum] = 
    XCreateSimpleWindow(xvars.dpy[dpyNum],xvars.root[dpyNum],0,0,
			viewportSize[dpyNum].width,viewportSize[dpyNum].height,
			0,xvars.black[dpyNum],xvars.white[dpyNum]);

  XSizeHints size_hints;
  size_hints.flags = PPosition | PSize | PMinSize;
  size_hints.min_width = viewportSize[dpyNum].width;
  size_hints.min_height = viewportSize[dpyNum].height;

  XTextProperty windowName, iconName;
  char *window_name = "XEvil";  // Will appear on window.
  char *icon_name = "XEvil";
  assert(XStringListToTextProperty(&window_name,1,&windowName));
  assert(XStringListToTextProperty(&icon_name,1,&iconName)); 
  
  XWMHints wm_hints;
  wm_hints.initial_state = NormalState;
  wm_hints.input = True;
  wm_hints.icon_pixmap = xdata.icon[dpyNum];
  wm_hints.flags = StateHint | IconPixmapHint | InputHint;

  XClassHint class_hints;
  class_hints.res_name = argv[0];
  class_hints.res_class = "XEvil";

  XSetWMProperties(xvars.dpy[dpyNum],xdata.toplevels[viewportNum],
		   &windowName,&iconName,argv,argc,
		   &size_hints,&wm_hints,&class_hints);

  XSelectInput(xvars.dpy[dpyNum],xdata.toplevels[viewportNum],
	       KeyPressMask | KeyReleaseMask);
  
  create_menus(viewportNum);
  create_arena(viewportNum);
  create_statuses(viewportNum);
  create_intels_playing(viewportNum);
  create_message_bar(viewportNum);
  create_levels(viewportNum);

  XMapWindow(xvars.dpy[dpyNum],xdata.arenas[viewportNum]);
  XMapWindow(xvars.dpy[dpyNum],xdata.toplevels[viewportNum]);
}



void Ui::create_menus(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  if (!menusNum[viewportNum])
    return;

  // Compute sizes for all menus.
  Size quitUnit = ButtonPanel::get_unit(xvars.font[dpyNum],QUIT_LINE_LENGTH);
  Size newGameUnit = ButtonPanel::get_unit(xvars.font[dpyNum],NEW_GAME_LINE_LENGTH);
  Size humansNumUnit = 
    WritePanel::get_unit(xvars.font[dpyNum],HUMANS_NUM_LINE_LENGTH);
  Size enemiesNumUnit = 
    WritePanel::get_unit(xvars.font[dpyNum],ENEMIES_NUM_LINE_LENGTH);
  Size enemiesRefillUnit = 
    TogglePanel::get_unit(xvars.font[dpyNum],ENEMIES_REFILL_LINE_LENGTH);
  Size controlsUnit =
    TogglePanel::get_unit(xvars.font[dpyNum],CONTROLS_LINE_LENGTH);
  Size learnControlsUnit =
    TogglePanel::get_unit(xvars.font[dpyNum],LEARN_CONTROLS_LINE_LENGTH);

  Size styleUnit = TogglePanel::get_unit(xvars.font[dpyNum],STYLE_LINE_LENGTH);
  Size scenariosUnit = 
    TogglePanel::get_unit(xvars.font[dpyNum],SCENARIOS_LINE_LENGTH);
  Size levelsUnit = 
    TogglePanel::get_unit(xvars.font[dpyNum],LEVELS_LINE_LENGTH);
  Size killUnit = TogglePanel::get_unit(xvars.font[dpyNum],KILL_LINE_LENGTH);
  Size duelUnit = TogglePanel::get_unit(xvars.font[dpyNum],DUEL_LINE_LENGTH);
  Size extendedUnit = 
    TogglePanel::get_unit(xvars.font[dpyNum],EXTENDED_LINE_LENGTH);
  Size trainingUnit = TogglePanel::get_unit(xvars.font[dpyNum],TRAINING_LINE_LENGTH);
  Size quantaUnit = WritePanel::get_unit(xvars.font[dpyNum],QUANTA_LINE_LENGTH);


  // Create actual menu panels.
  Pos pos(0,0);

  if (menusNum[viewportNum] == UI_MENUS_PRIMARY_NUM)
    {
      assert(viewportNum == 0 && dpyIndex[viewportNum] == 0);

      // Quit button.
      menus[viewportNum][menuQuit] = 
	new ButtonPanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],
			pos,quitUnit,Ui::menu_quit_CB,"Quit");
      assert(menus[viewportNum][menuQuit]);
      pos.x += quitUnit.width;
      
      // New Game button.
      menus[viewportNum][menuNewGame] = 
	new ButtonPanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			newGameUnit,Ui::menu_new_game_CB,"New Game");
      assert(menus[viewportNum][menuNewGame]);
      pos.x += newGameUnit.width;
      
      // Humans WritePanel
      menus[viewportNum][menuHumansNum] = 
	new WritePanel(this,dpyNum,viewportNum,xvars,
		       xdata.toplevels[viewportNum],
		       pos,humansNumUnit,Ui::menu_humans_num_CB,"Humans:");
      assert(menus[viewportNum][menuHumansNum]);
      pos.x += humansNumUnit.width;
      
      // Enemies WritePanel
      menus[viewportNum][menuEnemiesNum] = 
	new WritePanel(this,dpyNum,viewportNum,xvars,
		       xdata.toplevels[viewportNum],pos,
		       enemiesNumUnit,Ui::menu_enemies_num_CB,"Machines:");
      assert(menus[viewportNum][menuEnemiesNum]);
      pos.x += enemiesNumUnit.width;
      
      // Enemies Refill TogglePanel
      menus[viewportNum][menuEnemiesRefill] = 
	new TogglePanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			enemiesRefillUnit,Ui::menu_enemies_refill_CB,
			"Regenerate machines");
      assert(menus[viewportNum][menuEnemiesRefill]);
      pos.x += enemiesRefillUnit.width;
    }
  else
    assert(menusNum[viewportNum] == UI_MENUS_SECONDARY_NUM);
  
  // Learn Controls ButtonPanel
  menus[viewportNum][menuLearnControls] = 
    new TogglePanel(this,dpyNum,viewportNum,xvars,
		    xdata.toplevels[viewportNum],pos,
		    learnControlsUnit,Ui::menu_learn_controls_CB,
		    "Set controls");
  assert(menus[viewportNum][menuLearnControls]);
  pos.x += learnControlsUnit.width;

  // Controls TogglePanel
  menus[viewportNum][menuControls] = 
    new TogglePanel(this,dpyNum,viewportNum,xvars,
		    xdata.toplevels[viewportNum],pos,
		    controlsUnit,Ui::menu_controls_CB,
		    "Show controls");
  assert(menus[viewportNum][menuControls]);
  pos.x += controlsUnit.width;

  
  // Second row.
  pos.x = 0;
  pos.y += quitUnit.height;

  if (menusNum[viewportNum] == UI_MENUS_PRIMARY_NUM)
    {
      // Game stlye header.
      menus[viewportNum][menuStyle] = 
	new TextPanel(this,dpyNum,viewportNum,xvars,
		      xdata.toplevels[viewportNum],
		      pos,styleUnit,"Game style:");
      assert(menus[viewportNum][menuStyle]);
      pos.x += styleUnit.width;
      
      // Scenarios game style.
      menus[viewportNum][menuScenarios] = 
	new TogglePanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			scenariosUnit,Ui::menu_scenarios_CB,
			"Scenarios");
      assert(menus[viewportNum][menuScenarios]);
      pos.x += scenariosUnit.width;
      
      // Levels game style.
      menus[viewportNum][menuLevels] = 
	new TogglePanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			levelsUnit,Ui::menu_levels_CB,
			"Levels");
      assert(menus[viewportNum][menuLevels]);
      pos.x += levelsUnit.width;
      
      // Kill game style.
      menus[viewportNum][menuKill] = 
	new TogglePanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			killUnit,Ui::menu_kill_CB,
			"Kill, Kill, Kill");
      assert(menus[viewportNum][menuKill]);
      pos.x += killUnit.width;
      
      // Duel game style.
      menus[viewportNum][menuDuel] = 
	new TogglePanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			duelUnit,Ui::menu_duel_CB,"Duel");
      assert(menus[viewportNum][menuDuel]);
      pos.x += duelUnit.width;
      
      // Extended duel game style.
      menus[viewportNum][menuExtended] = 
	new TogglePanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			extendedUnit,Ui::menu_extended_CB,"Extended Duel");
      assert(menus[viewportNum][menuExtended]);
      pos.x += extendedUnit.width;
      
      // Training game style.
      menus[viewportNum][menuTraining] = 
	new TogglePanel(this,dpyNum,viewportNum,xvars,
			xdata.toplevels[viewportNum],pos,
			trainingUnit,Ui::menu_training_CB,"Training");
      assert(menus[viewportNum][menuTraining]);
      pos.x += trainingUnit.width;
      
      
      pos.x = viewportSize[dpyNum].width - quantaUnit.width;
      menus[viewportNum][menuQuanta] = 
	new WritePanel(this,dpyNum,viewportNum,xvars,
		       xdata.toplevels[viewportNum],pos,
		       quantaUnit,Ui::menu_quanta_CB,"Speed(ms):");
      assert(menus[viewportNum][menuQuanta]);
    }
  else
    assert(menusNum[viewportNum] == UI_MENUS_SECONDARY_NUM);
}



void Ui::create_arena(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  xdata.arenas[viewportNum] = 
    XCreateSimpleWindow(xvars.dpy[dpyNum],
			xdata.toplevels[viewportNum],
			0,menusSize[dpyNum].height,
			roomSize[dpyNum].width,roomSize[dpyNum].height,
			ARENA_BORDER,xvars.black[dpyNum],
			xvars.white[dpyNum]);
//  XSelectInput(xvars.dpy[dpyNum],xdata.arenas[viewportNum],
//	       ExposureMask | KeyPressMask | KeyReleaseMask | 
//	       ButtonPressMask);
  XSelectInput(xvars.dpy[dpyNum],xdata.arenas[viewportNum],
	       ExposureMask | ButtonPressMask);
}



void Ui::create_statuses(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  Pos pos(arenaSize[dpyNum].width,menusSize[dpyNum].height);
  Size statusUnit;

  for (int n = 0; n < UI_STATUSES_NUM; n++)
    {
      if (n == statusWeapon)
	{
	  statusUnit = 
	    TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH);
	  statuses[viewportNum][n] = 
	    new ButtonPanel(this,dpyNum,viewportNum,xvars,
			    xdata.toplevels[viewportNum],
			    pos,statusUnit,Ui::status_weapon_CB);
	}
      else if (n == statusItem)
	{
	  statusUnit = 
	    TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH);
	  statuses[viewportNum][n] = 
	    new ButtonPanel(this,dpyNum,viewportNum,xvars,
			    xdata.toplevels[viewportNum],
			    pos,statusUnit,Ui::status_item_CB);
	}
      else if (n == statusHealth)
	{
	  statusUnit = 
	    TextPanel::get_unit(xvars.font[dpyNum],
				STATUS_LINE_LENGTH / 2);
	  statuses[viewportNum][n] = 
	    new TextPanel(this,dpyNum,viewportNum,xvars,
			  xdata.toplevels[viewportNum],
			  pos,statusUnit);
	}
      else if (n == statusMass)
	{
	  statusUnit = 
	    TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH);
	  Size otherStatusUnit = 
	    TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH / 2);
	  Pos p(pos.x + otherStatusUnit.width,pos.y);
	  Size s;
	  s.width = statusUnit.width - otherStatusUnit.width;
	  s.height = statusUnit.height;
	  statuses[viewportNum][n] = 
	    new TextPanel(this,dpyNum,viewportNum,xvars,
			  xdata.toplevels[viewportNum],p,s);
	}
      else
	{
	  statusUnit = 
	    TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH);
	  statuses[viewportNum][n] = 
	    new TextPanel(this,dpyNum,viewportNum,xvars,
			  xdata.toplevels[viewportNum],
			  pos,statusUnit);
	}

      assert(statuses[viewportNum][n]);
      if (n != statusHealth)
	pos.y += statusUnit.height;
    }
}



void Ui::create_intels_playing(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  Size intelsPlayingUnit = 
    TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH);

  Pos pos(arenaSize[dpyNum].width,
	  menusSize[dpyNum].height + statusesSize[dpyNum].height);
  humansPlaying[viewportNum] = 
    new TextPanel(this,dpyNum,viewportNum,xvars,
		  xdata.toplevels[viewportNum],
		  pos,intelsPlayingUnit);
  assert(humansPlaying[viewportNum]);

  pos.y += intelsPlayingUnit.height;
  enemiesPlaying[viewportNum] = 
    new TextPanel(this,dpyNum,viewportNum,xvars,
		  xdata.toplevels[viewportNum],
		  pos,intelsPlayingUnit);
  assert(enemiesPlaying[viewportNum]);
}



void Ui::create_message_bar(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  Pos pos(0,menusSize[dpyNum].height + arenaSize[dpyNum].height);
  messageBars[viewportNum] = 
    new TextPanel(this,dpyNum,viewportNum,xvars,
		  xdata.toplevels[viewportNum],
		  pos,messageBarSize[dpyNum]);
  assert(messageBars[viewportNum]);
}



void Ui::create_levels(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  Size unit = TextPanel::get_unit(xvars.font[dpyNum],STATUS_LINE_LENGTH,2);

  Pos pos(arenaSize[dpyNum].width,menusSize[dpyNum].height 
	  + arenaSize[dpyNum].height - unit.height);
  levels[viewportNum] =
    new TextPanel(this,dpyNum,viewportNum,xvars,
		  xdata.toplevels[viewportNum],
		  pos,unit);
  assert(levels[viewportNum]);
}



void Ui::update_statuses(int viewportNum)
{
  int dpyNum = dpyIndex[viewportNum];

  if (intelsSet[viewportNum])
    {
      if (intels[viewportNum]->intel_status_changed())
	{
	  const IntelStatus *status = intels[viewportNum]->get_intel_status();

	  ostrstream name;
	  name << "Name:" << status->name << ends;
	  statuses[viewportNum][statusName]->set_message(name.str());
	  delete name.str();

	  ostrstream className;
	  className << "Class:" << status->className << ends;
	  statuses[viewportNum][statusClassName]->set_message(className.str());
	  delete className.str();
	  
	  ostrstream health;
	  health << "Health:";
	  if (status->health == -1)
	    health << "dead" << ends;
	  else
	    health << status->health << ends; 
	  statuses[viewportNum][statusHealth]->set_message(health.str());
	  delete health.str();

	  ostrstream mass;
	  mass << "Mass:" << status->mass << ends;
	  statuses[viewportNum][statusMass]->set_message(mass.str());
	  delete mass.str();

	  ostrstream weapon;
	  weapon << "Weapon:" << status->weapon;
	  if (status->ammo != PH_AMMO_UNLIMITED)
	    weapon << " (" << status->ammo << ")";
	  weapon << ends;
	  statuses[viewportNum][statusWeapon]->
	    set_foreground(status->weaponReady ? 
			   xvars.green[dpyNum] : xvars.red[dpyNum],False);
	  statuses[viewportNum][statusWeapon]->set_message(weapon.str());
	  delete weapon.str();

	  ostrstream item;
	  item << "Item:" << status->item;
	  if (status->itemClassId != A_None)
	    item << " (" << status->itemCount << ")";
	  item << ends;
	  statuses[viewportNum][statusItem]->set_message(item.str());
	  delete item.str();

	  ostrstream livesHKills;
	  if (settings.style == UIsettings::EXTENDED)
	    // Takes soups into account.
	    livesHKills << "Humans killed:" << 
	      status->humanKills - status->soups << ends;
	  else
	    livesHKills << "Extra lives:" << status->lives << ends;
	  statuses[viewportNum][statusLivesHKills]
	    ->set_message(livesHKills.str());
	  delete livesHKills.str();
	  
	  ostrstream killsMKills;
	  if (settings.style == UIsettings::EXTENDED)
	    killsMKills << "Machines killed:" << status->enemyKills << ends;
	  else
	    killsMKills << "Kills:" 
	      << (status->humanKills + status->enemyKills) << ends;
	  statuses[viewportNum][statusKillsMKills]
	    ->set_message(killsMKills.str());
	  delete killsMKills.str();
	}
    }
}



void Ui::viewport_expose(int viewportNum)
{  
  int dpyNum = dpyIndex[viewportNum];

  if (!cursorDefined[viewportNum])
    {
      XDefineCursor(xvars.dpy[dpyNum],xdata.arenas[viewportNum],
		    xdata.arenaCursor[dpyNum]);
      cursorDefined[viewportNum] = True;
    }
  draw(viewportNum);
}



#define KEY_EQ(key,viewportNum) \
  ((event->xkey.keycode == (keycodes[dpyNum][inputs[viewportNum]])[key][0]) || \
   (event->xkey.keycode == (keycodes[dpyNum][inputs[viewportNum]])[key][1]))
void Ui::viewport_key_press(int dpyNum,XEvent *event)
{     
  assert(!pause);
  
  // Go through all viewports on display, try to match keysym.
  for (int v = 0; v < vIndexNum[dpyNum]; v++)
    {
      int viewportNum = vIndex[dpyNum][v];
      if (inputsSet[viewportNum])
	{
	  Boolean commandSet = True;
	  ITcommand command;
	
	  if (KEY_EQ(IT_CENTER,viewportNum))
	    command = IT_CENTER;
	  else if (KEY_EQ(IT_R,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_R : IT_R;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_R;
	    }
	  else if (KEY_EQ(IT_DN_R,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_DN_R : IT_DN_R;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_DN_R;
	    }
	  else if (KEY_EQ(IT_DN,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_DN : IT_DN;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_DN;
	    }
	  else if (KEY_EQ(IT_DN_L,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_DN_L : IT_DN_L;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_DN_L;
	    }
	  else if (KEY_EQ(IT_L,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_L : IT_L;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_L;
	    }
	  else if (KEY_EQ(IT_UP_L,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_UP_L : IT_UP_L;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_UP_L;
	    }
	  else if (KEY_EQ(IT_UP,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_UP : IT_UP;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_UP;
	    }
	  else if (KEY_EQ(IT_UP_R,viewportNum))
	    {
	      command = weaponKeyDown[viewportNum] ? IT_WEAPON_UP_R : IT_UP_R;
	      weaponCommandDefault[viewportNum] = IT_WEAPON_UP_R;
	    }
	  else if (KEY_EQ(IT_WEAPON_CHANGE,viewportNum))
	    command = IT_WEAPON_CHANGE;
	  else if (KEY_EQ(IT_WEAPON_DROP,viewportNum))
	    command = IT_WEAPON_DROP;
	  else if (KEY_EQ(IT_ITEM_USE,viewportNum))
	    command = IT_ITEM_USE;
	  else if (KEY_EQ(IT_ITEM_CHANGE,viewportNum))
	    command = IT_ITEM_CHANGE;
	  else if (KEY_EQ(IT_ITEM_DROP,viewportNum))
	    command = IT_ITEM_DROP;
	  else 
	    {
	      if (KEY_EQ(IT_WEAPON_CENTER,viewportNum))
		weaponKeyDown[viewportNum] = True;
	      
	      commandSet = False;
	    }

	  if (commandSet) 
	    {
	      // Repeating keys.
	      if (command_repeatable(command))
		commandRepeat[viewportNum] = command;
//	      else
//		commandRepeat[viewportNum] = IT_NO_COMMAND;

	      // Actually send command to human or move viewport.
	      dispatch(viewportNum,command);
	    }
	  else if (event->xkey.keycode 
		   == XKeysymToKeycode(xvars.dpy[dpyNum],XK_0))
	    {
	      settingsChanges |= UIpause;
	      settings.pause = True;
	      otherInput = True;
	    }
	  else
	    otherInput = True;
	}
    }
}



void Ui::viewport_key_release(int dpyNum,XEvent *event)
{     
  // Get keysym.
  KeySym keysym;
  XLookupString(&event->xkey,NULL,0,&keysym,NULL);

  for (int v = 0; v < vIndexNum[dpyNum]; v++)
    {
      int viewportNum = vIndex[dpyNum][v];
      // No longer pressing the weapon_use key.
      if (inputsSet[viewportNum] && KEY_EQ(IT_WEAPON_CENTER,viewportNum))
	{
	  weaponKeyDown[viewportNum] = False;
	  dispatch(viewportNum,weaponCommandDefault[viewportNum]);
	}

      // Cancel keyboard repeating.
      if (KEY_EQ(commandRepeat[v],viewportNum))
	commandRepeat[v] = IT_NO_COMMAND;
    }
}
#undef KEY_EQ



void Ui::viewport_button_press(int viewportNum,XEvent *event)
{
  int dpyNum = dpyIndex[viewportNum]; 

  if (event->xbutton.button == Button1)
    dispatch(viewportNum,
	     Intel::dir_to_command(key_press_to_dir(dpyNum,event)));
  else if (event->xbutton.button == Button2)
    dispatch(viewportNum,
	     Intel::dir_to_command_weapon(key_press_to_dir(dpyNum,event)));
  else if (event->xbutton.button == Button3)
    dispatch(viewportNum,IT_WEAPON_CHANGE);
  else
    otherInput = True;
}



Dir Ui::key_press_to_dir(int dpyNum,XEvent *event)
{
  assert(event->type == ButtonPress);

  if (event->xbutton.x < arenaSize[dpyNum].width * 0.3333)
    {
      if (event->xbutton.y < arenaSize[dpyNum].height * 0.3333)
	return CO_UP_L;
      else if (event->xbutton.y < arenaSize[dpyNum].height * 0.6666)
	return CO_L;
      else
	return CO_DN_L;
    }
  else if (event->xbutton.x < arenaSize[dpyNum].width * .6666)
    {
      if (event->xbutton.y < arenaSize[dpyNum].height * 0.3333)
	return CO_UP;
      else if (event->xbutton.y < arenaSize[dpyNum].height * 0.6666)
	return CO_air;
      else
	return CO_DN;
    }
  else
    {
      if (event->xbutton.y < arenaSize[dpyNum].height * 0.3333)
	return CO_UP_R;
      else if (event->xbutton.y < arenaSize[dpyNum].height * 0.6666)
	return CO_R;
      else
	return CO_DN_R;
    }
}



void Ui::controls_redraw(int dpyNum)
{
  XClearWindow(xvars.dpy[dpyNum],xdata.controls[dpyNum]);

  // Temporary list of keysyms.
  KeySym keymaps[2][UI_KEYS_MAX][2][2];  
  for (int input = 0; input < 2; input++)
    for (int nn = 0; nn < UI_KEYS_MAX; nn++)
      for (int which = 0; which < 2; which++)
	for (int i = 0; i < 2; i++)
	  {
	    unsigned int keycode = keycodes[dpyNum][input][nn][which];
	    keymaps[input][nn][which][i] = 
	      XKeycodeToKeysym(xvars.dpy[dpyNum],keycode,i);
	  }
  
  Size fontSize;
  fontSize.width = xvars.font[dpyNum]->max_bounds.width;
  fontSize.height = xvars.font[dpyNum]->max_bounds.ascent 
    + xvars.font[dpyNum]->max_bounds.descent;
  int p = 0;

  char player[] = 
    "Left Side                             >>>>>> Right Side (DEFAULT) <<<<<<";
  XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],
	      fontSize.width,
	      xvars.font[dpyNum]->max_bounds.ascent 
	      + (p++) * fontSize.height,
	      player,strlen(player));

  char line[] =  
    "------------------------------------------------------------------------------------------";
  XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],
	      fontSize.width,
	      xvars.font[dpyNum]->max_bounds.ascent 
	      + (p++) * fontSize.height,
	      line,strlen(line));

  for (int n = 0; n < UI_KEYS_MAX; n++)
    {
      strstream str0;
      str0 << keysNames[n] << ":  " << XKeysymToString(keymaps[0][n][0][0]);
      if (keymaps[0][n][0][1] && strlen(XKeysymToString(keymaps[0][n][0][1])))
	str0 << ", " << XKeysymToString(keymaps[0][n][0][1]);
      if (keymaps[0][n][0][0] != keymaps[0][n][1][0] && keymaps[0][n][1][0])
	{
	  str0 << ", " << XKeysymToString(keymaps[0][n][1][0]);
	  if (keymaps[0][n][1][1]
	      && strlen(XKeysymToString(keymaps[0][n][1][1])))
	    str0 << ", " << XKeysymToString(keymaps[0][n][1][1]);
	}
      str0 << ends;

      strstream str1;
      str1 << keysNames[n] << ":  " << XKeysymToString(keymaps[1][n][0][0]);
      if (keymaps[1][n][0][1] && strlen(XKeysymToString(keymaps[1][n][0][1])))
	str1 << ", " << XKeysymToString(keymaps[1][n][0][1]);
      if (keymaps[1][n][0][0] != keymaps[1][n][1][0] && keymaps[1][n][1][0])
	{
	  str1 << ", " << XKeysymToString(keymaps[1][n][1][0]);
	  if (keymaps[1][n][1][1]
	      && strlen(XKeysymToString(keymaps[1][n][1][1])))
	    str1 << ", " << XKeysymToString(keymaps[1][n][1][1]);
	}
      str1 << ends;

      XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],fontSize.width,
		  xvars.font[dpyNum]->max_bounds.ascent + p * fontSize.height,
		  str1.str(),strlen(str1.str()));
      XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],
		  fontSize.width * (CONTROLS_COLS / 2 + 1),
		  xvars.font[dpyNum]->max_bounds.ascent + (p++) * fontSize.height,
		  str0.str(),strlen(str0.str()));
      delete str0.str();
      delete str1.str();
    }
  p++;

  char line2[] =  
    "Keyboard controls can be set with the -keys command line option";
  XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],fontSize.width,
	      xvars.font[dpyNum]->max_bounds.ascent + (p++) * fontSize.height,
	      line2,strlen(line2));
  char line3[] =  
    "the [Set controls] button or with the appropriate X resources.";
  XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],fontSize.width,
	      xvars.font[dpyNum]->max_bounds.ascent + (p++) * fontSize.height,
	      line3,strlen(line3));
  p++;

/*  char line4[] =  
    "XEvil copyright (C) 1994 Steve Hardt";
  XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],fontSize.width,
	      xvars.font[dpyNum]->max_bounds.ascent + (p++) * fontSize.height,
	      line4,strlen(line4));
	      
  char line5[] =  
    "hardts@athena.mit.edu hardts@media.mit.edu hardts@r4002.3dem.bioch.bcm.tmc.edu";
  XDrawString(xvars.dpy[dpyNum],xdata.controls[dpyNum],xvars.gc[dpyNum],fontSize.width,
	      xvars.font[dpyNum]->max_bounds.ascent + (p++) * fontSize.height,
	      line5,strlen(line5));
*/
}



char *Ui::keysNames[UI_KEYS_MAX] = 
{
  "center",
  "right",
  "down_right",
  "down",
  "down_left",
  "left",
  "up_left",
  "up",
  "up_right",
  "weapon_use",
  "weapon_change",
  "weapon_drop",
  "item_use",
  "item_change",
  "item_drop",
};




/////////////////////////////// Panel ////////////////////////////////////////
#define PANEL_BORDER 2
#define PANEL_MARGAIN 2


Panel::Panel(UiP u_i,int dpy_num,int vNum,const Xvars &x_vars,Window parent,
	     const Pos &p,const Size &s,unsigned int eventMask) 
: xvars(x_vars)
{
  ui = u_i;
  viewportNum = vNum;
  dpyNum = dpy_num;
//  xvars = &x_vars;
  foreground = xvars.black[dpyNum];
  background = xvars.white[dpyNum];

  size.width = s.width - 2 * PANEL_BORDER;
  size.height = s.height - 2 * PANEL_BORDER;

  window = XCreateSimpleWindow(xvars.dpy[dpyNum],parent,p.x,p.y,
			       size.width,size.height,PANEL_BORDER,
			       xvars.black[dpyNum],xvars.white[dpyNum]);
//  XSelectInput(xvars.dpy[dpyNum],window,
//	       ExposureMask|KeyPressMask|ButtonPressMask);
  XSelectInput(xvars.dpy[dpyNum],window,eventMask);
  XMapWindow(xvars.dpy[dpyNum],window);
}



void Panel::set_foreground(Pixel c,Boolean re_draw)
{
  foreground = c;
  if (re_draw)
    redraw();
}



void Panel::set_background(Pixel c,Boolean re_draw)
{
  background = c;
  if (re_draw)
    redraw();
}



Boolean Panel::process_event(int dpy_num,XEvent *event)
{
  assert(dpyNum == dpy_num);
  if (event->type == Expose && event->xexpose.window == get_window())
    {
      redraw();
      return True;
    }
  
  return False;
}



TextPanel::TextPanel(UiP ui,int dpy_num,int vNum,const Xvars &xv,Window parent,
		     const Pos &p,const Size &s,const char *msg,
		     unsigned int eventMask)
: Panel(ui,dpy_num,vNum,xv,parent,p,s,eventMask)
{
  if (msg != NULL)
    {
      assert(strlen(msg) < UI_STRING_LENGTH);
      strcpy(message,msg);
    }
  else
    strcpy(message,"");
  sensitive = True;
}



void TextPanel::set_message(const char *msg)
{
  if (strlen(msg) >= UI_STRING_LENGTH)
    {
      strncpy(message,msg,UI_STRING_LENGTH-1);
#ifdef PRINT_ERRORS
      cerr << "TextPanel::set_message: " << msg << endl;
#endif
    }
  else
    strcpy(message,msg);
  redraw();
}



Size TextPanel::get_unit(XFontStruct *font,int cols,int rows)
{
  Size fontSize;
  fontSize.width = font->max_bounds.width;
  fontSize.height = font->max_bounds.ascent + font->max_bounds.descent;

  Size ret;
  ret.width = fontSize.width * cols + 2 * PANEL_BORDER + 2 * PANEL_MARGAIN;
  ret.height = fontSize.height * rows + 2 * PANEL_BORDER + 2 * PANEL_MARGAIN;
  return ret;
}



void TextPanel::redraw()
{
  Size size = get_size();
  const Xvars &xvars = get_xvars();
  int dpyNum = get_dpy_num();
  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],get_background());
  XFillRectangle(xvars.dpy[dpyNum],get_window(),xvars.gc[dpyNum],0,0,
		 size.width,size.height);
  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],get_foreground());

  // Draw text, line by line.
  char *start = message;
  char *current = message;
  int lineNo = 0;
  while (True)
    {
      if (*current == '\n' || *current == '\0')
	{
	  XDrawString(xvars.dpy[dpyNum],get_window(),
		      xvars.gc[dpyNum],PANEL_MARGAIN,
		      PANEL_MARGAIN+xvars.font[dpyNum]->max_bounds.ascent +
		      (xvars.font[dpyNum]->max_bounds.ascent + 
		       xvars.font[dpyNum]->max_bounds.descent) * lineNo,
		      start,current - start);
	  start = current + 1;
	  lineNo++;
	  if (*current == '\0')
	    break;
	}
      current++;
    }
      

  if (!sensitive)
    {
      XSetFillStyle(xvars.dpy[dpyNum],xvars.gc[dpyNum],FillStippled);
      XFillRectangle(xvars.dpy[dpyNum],get_window(),xvars.gc[dpyNum],0,0,
		     size.width,size.height);
      XSetFillStyle(xvars.dpy[dpyNum],xvars.gc[dpyNum],FillSolid);
    }
  XSetForeground(xvars.dpy[dpyNum],xvars.gc[dpyNum],xvars.black[dpyNum]);
}



void TextPanel::clear()
{
  const Xvars &xvars = get_xvars();
  int dpyNum = get_dpy_num();
  XClearWindow(xvars.dpy[dpyNum],get_window());
}



void TextPanel::set_sensitive(Boolean s)
{
  if (sensitive != s)
    {
      sensitive = s;
      redraw();
    }
}



WritePanel::WritePanel(UiP ui,int dpy_num,int vNum,const Xvars &xvars,
		       Window parent,const Pos &pos,const Size &size,
		       void (*cb)(UiP,const char *),
		       const char *pmpt,
		       unsigned int eventMask)
: TextPanel(ui,dpy_num,vNum,xvars,parent,pos,size,NULL,eventMask)
{
  assert(strlen(pmpt) < UI_STRING_LENGTH);
  strcpy(prompt,pmpt);
  strcpy(value,"");

  callback = cb;
  active = False;
  update_message();
}



void WritePanel::set_value(const char *val)
{
  assert(strlen(val) < UI_STRING_LENGTH);
  strcpy(value,val);
  update_message();
}



Boolean WritePanel::process_event(int dpy_num,XEvent *event)
{
  int dpyNum = get_dpy_num();
  assert(dpyNum == dpy_num);
  
  if (get_sensitive())
    if (event->xkey.window == get_window())
      {
	if (event->type == KeyPress)
	  {
	    char buffer[KEYSYM_BUFFER];
	    KeySym keysym;
	    int len = 
	      XLookupString(&event->xkey,buffer,KEYSYM_BUFFER,&keysym,NULL);
	    buffer[len] = '\0';
	    
	    if ((keysym >= XK_KP_Space && keysym <= XK_KP_9) ||
		(keysym >= XK_space && keysym <= XK_asciitilde))
	      {
		if (active)
		  {
		    if (strlen(value) + len < UI_STRING_LENGTH)
		      strcat(value,buffer);
		  }
		else
		  {
		    active = True;
		    if (strlen(value) < UI_STRING_LENGTH)
		      strcpy(value,buffer);
		  }
		update_message();
	      }
	    else if (keysym == XK_BackSpace || keysym == XK_Delete)
	      {
		if (active)
		  {
		    if (strlen(value) > 0)
		      value[strlen(value) - 1] = '\0';
		  }
		else
		  {
		    value[0] = '\0';
		    active = True;
		  }
		update_message();
	      }
	    else if (keysym == XK_Return || keysym == XK_KP_Enter 
		     || keysym == XK_Linefeed)
	      {
		if (active)
		  {
		    UiP ui = get_ui();
		    (*callback)(ui,value);
		    active = False;
		  }
		else
		  {
		    value[0] = '\0';
		    active = True;
		  }
		update_message();
	      }
	    
	    return True;
	  }
	else if (event->type == ButtonPress)
	  {
	    if (!active)
	      {
		value[0] = '\0';
		active = True;
	      }
	    update_message();
	    return True;
	  }
      }
  
  return TextPanel::process_event(dpyNum,event);
}



void WritePanel::update_message()
{
  ostrstream tmp;
  if (active)
    tmp << prompt << value << "_" << ends;
  else
    tmp << prompt << value << ends;
  set_message(tmp.str());
  delete tmp.str(); 
}



KeyPressPanel::KeyPressPanel(UiP ui,int dpy_num,int vNum,const Xvars &xvars,
			     Window parent,const Pos &pos,const Size &size,
			     void (*cb)(UiP,KeyPressPanel *,XEvent *),
			     const char *msg,
			     unsigned int eventMask)
     : TextPanel(ui,dpy_num,vNum,xvars,parent,pos,size,msg,eventMask)
{
  callback = cb;
}



Boolean KeyPressPanel::process_event(int dpy_num,XEvent *event)
{
  int dpyNum = get_dpy_num();
  assert(dpyNum == dpy_num);

  if (get_sensitive() && event->type == KeyPress && 
      event->xbutton.window == get_window())
    {
      UiP ui = get_ui();
      (*callback)(ui,this,event);
      return True;
    }
  else
    return TextPanel::process_event(dpyNum,event);
}



ButtonPanel::ButtonPanel(UiP ui,int dpy_num,int vNum,const Xvars &xvars,
			 Window parent,const Pos &pos,const Size &size,
			 void (*cb)(UiP,int,int),
			 const char *msg,
			 unsigned int eventMask)
     : TextPanel(ui,dpy_num,vNum,xvars,parent,pos,size,msg,eventMask)
{
  callback = cb;
}



Boolean ButtonPanel::process_event(int dpyNum,XEvent *event)
{
  if (get_sensitive() && 
      (event->type == ButtonPress) && 
      (event->xbutton.window == get_window()))
    {
      UiP ui = get_ui();
      (*callback)(ui,get_viewport_num(),event->xbutton.button);
      return True;
    }
  else
    return TextPanel::process_event(dpyNum,event);
}



TogglePanel::TogglePanel(UiP ui,int dpy_num,int vNum,const Xvars &xvars,
			 Window parent,const Pos &pos,const Size &size,
			 void (*cb)(UiP,TogglePanel *,Boolean),
			 const char *msg,
			 unsigned int eventMask)
: TextPanel(ui,dpy_num,vNum,xvars,parent,pos,size,msg,eventMask)
{
  set = False;
  callback = cb;
}



void TogglePanel::set_value(Boolean s)
{
  if (s != set)
    {
      Pixel foreground = get_foreground();
      set_foreground(get_background(),False);
      set_background(foreground);
      set = s;
    }
}



Boolean TogglePanel::process_event(int dpyNum,XEvent *event)
{
  if (get_sensitive() && 
      (event->type == ButtonPress) && 
      (event->xbutton.window == get_window()))
    {
      set_value(!set);

      UiP ui = get_ui();
      (*callback)(ui,this,set);
      return True;
    }
  else
    return TextPanel::process_event(dpyNum,event);
}
