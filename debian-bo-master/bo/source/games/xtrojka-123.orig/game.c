/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:	7.ii.1996 	Added update statistics
 *
 * 	This is the game module, doing everything that's not done 
 *	in the trojka core.
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>

#include "game.h"
#include "sh_main.h"
#include "sh_stat.h"
#include "scores.h"
#include "menu.h"

#include "debug.h"
#include "tr_core.h"
#include "xtrojka.h"
#include "screen.h"

extern XtAppContext app_context;

extern Widget main_screen;		/* the mother of all widgets */
extern Widget screen;
extern Widget form;

extern flag is_wizard;

extern GAME_STATE game_state;

extern int starting_speed;
extern int speed;

static XtIntervalId clocker;

flag ignore_clock;
flag remove_clock;

tt_command CMD_DOWN = { tc_c_blockdown, 0 , 0 };
tt_command CMD_LEFT = { tc_c_blockleft, 0 , 0 };
tt_command CMD_RIGHT = { tc_c_blockright, 0 , 0 };
tt_command CMD_DROP = { tc_c_dropblock, 0 , 0 };
tt_command CMD_SPEEDUP = { tc_c_speedup, 0 , 0 };

/*
 *	source
 */
void mainloop(void)
{
/*
 * this is the main loop where the game is played 
 */
	XEvent event;

	DEBUG("game.c", "mainloop")

	set_state(st_idle);
	
	for(;;) {
		XtAppNextEvent(app_context, &event);
		XtDispatchEvent(&event);
	}
}


void resetgame(void)
{
	tt_command CMD_INIT;

	DEBUG("game.c", "resetgame")

	ignore_clock = False;

	CMD_INIT.command = tc_c_init;
	CMD_INIT.param1 = starting_speed;
	CMD_INIT.param2 = is_wizard;
	trojka_api(&CMD_INIT);

	show_score();
	set_speed_item(starting_speed);
	draw_field();
}


void set_state(gs)
GAME_STATE gs;
{
/*
 *	disables/enables menu's event- and update handlers
 *	depending on the game state.
 */
	DEBUG("game.c", "set_state")
	game_state = gs;

	set_menus(gs);
	if(gs == st_playing) {

		remove_clock = False;
		resetgame();

		/*
		 *	start the clocker
		 */
		clocker = XtAppAddTimeOut(app_context, tv_ticks,
			(XtTimerCallbackProc)block_down_intr,
			0);

	}
	else if(gs == st_idle) {

		remove_clock = True;
		draw_title();	
	}
}



void init_event_handlers()
{
	DEBUG("game.c", "init_event_handlers")

	/*
	 *	catch moving main screen
	 */
	XtAddEventHandler(main_screen, StructureNotifyMask, False,
		(XtEventHandler)move_mainwindow_hlr, (Opaque)NULL);

	/*
	 *	catch enter/leave/focus events
	 */
	XtAddEventHandler(screen,
		LeaveWindowMask | EnterWindowMask,
		False,
		(XtEventHandler)std_window_hlr, (Opaque)NULL);

	/*
	 *	catch keyboard events
	 */
	XtAddEventHandler(form, KeyPressMask, False,
			(XtEventHandler)key_pressed_hlr, (Opaque)NULL);

}



void block_down_intr(w, id)
Widget w;
XtIntervalId *id;
{
/*
 *	this function is called every n timer ticks depending
 *	on the game speed. 
 */
	int res;

	DEBUG("game.c", "block_down_intr")

	if(!remove_clock)
		clocker=XtAppAddTimeOut(app_context, tv_ticks,
		(XtTimerCallbackProc)block_down_intr, 0);
	else {
		remove_clock = False;
		return;
	}

	if(ignore_clock)	
		return;

	if((res = trojka_api(&CMD_DOWN)) == tc_res_gameover) {
		gameover();
	}
	else if(res == tc_res_touchdown) {
		show_score();
		update_stat();
	}
}



void key_pressed_hlr(w, unused, ke, continue_to_dispatch)
Widget w;
XtPointer unused;
XKeyPressedEvent *ke;
Boolean *continue_to_dispatch;
{
extern tt_int tv_shape;
/*
 *	This function handles on pressing a key, and acts
 *	depending on the state the game is in.
 */
	Modifiers dum;
	KeySym sym;

	DEBUG("game.c", "game_key_pressed_hlr")

	*continue_to_dispatch = TRUE;

	if(ke->type != KeyPress)
		return;
	 
	XtTranslateKeycode(ke->display, ke->keycode, 0, &dum, &sym);
	
	/*
	 *	act on 'st_playing'
	 */

	if(game_state == st_playing) {

		switch(sym) {

/*
 *	Dirty debug. Get any block you want
 *			case XK_b:
 *
 *				tv_shape = (tv_shape + 1) % tc_blocks;
 *				break;
 */
		
			case XK_h:
			case XK_Left:
				if(ignore_clock)
					return;
				trojka_api(&CMD_LEFT);
				break;

			  case XK_l:
			  case XK_Right:
				if(ignore_clock)
					return;
				trojka_api(&CMD_RIGHT);
				break;

			  case XK_space:
			  case XK_j:
			  case XK_Insert:
			  case XK_Down:
				if(ignore_clock)
					return;
	
				trojka_api(&CMD_DROP);
				break;
		 	 
			  case XK_Up:
			  case XK_k:
				trojka_api(&CMD_SPEEDUP);
				speed = tv_speed;
				set_speed_item(speed);
				break;
		
			  default:
				break;
		}	
	}
	if(game_state == st_idle) {
		/* NOT NECCESARY YET */
		/* MOST ARE COVERED BY ACTIONS */
	}
}


void move_mainwindow_hlr(w, unused, ce, continue_to_dispatch)
Widget w;
XtPointer unused;
XConfigureEvent *ce;
Boolean *continue_to_dispatch;
{
	*continue_to_dispatch = TRUE;
	
	DEBUG("game.c", "move_mainwindow_hlr");
}	
	

	
void gameover_action(w, unused, event, continue_to_dispatch)
Widget w;
XtPointer unused;
XEvent *event;
Boolean *continue_to_dispatch;
{
	DEBUG("game.c", "gameover_action");
	gameover();
}


void gameover(void)
{
	DEBUG("game.c", "gameover")

	remove_clock = True;	

	do_hiscores();

	speed = starting_speed;

	set_state(st_idle);
}


void new_game(void)
{
	DEBUG("game.c", "new_game");

	if(game_state != st_playing)
		set_state(st_playing);
}


void std_window_hlr(w, unused, event, continue_to_dispatch)
Widget w;
XtPointer unused;
XAnyEvent *event;
Boolean *continue_to_dispatch;
{
/*
 *	this function is a callback function. It is called each time
 *	the mouse pointer enters the window
 */
	DEBUG("game.c", "std_window_hlr")
	
	*continue_to_dispatch = TRUE;

	if(game_state != st_playing)
		return;

	switch(event->type) {

		case EnterNotify:
			DEBUG("\tevent_type","EnterNotify");
			ignore_clock = False;
			break;

		case LeaveNotify:
			DEBUG("\tevent_type","LeaveNotify");
			ignore_clock = True;
			break;
		
	}
}




