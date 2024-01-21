/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *	header file for game.c
 */

#ifndef _game_h_
#define _game_h_

#include "xtrojka.h"

/*
 *	function prototypes
 */

void mainloop(void);
void resetgame(void);
void set_state(GAME_STATE);
void init_event_handlers(void);
void block_down_intr(Widget, XtIntervalId *);
void key_pressed_hlr(Widget, XtPointer, XKeyPressedEvent *, Boolean*);
void move_mainwindow_hlr(Widget, XtPointer, XConfigureEvent *, Boolean*);
void gameover_action(Widget, XtPointer, XEvent *, Boolean *);
void gameover(void);
void new_game(void);
void std_window_hlr(Widget, XtPointer, XAnyEvent*, Boolean *);

#endif /* _game_h_ */

