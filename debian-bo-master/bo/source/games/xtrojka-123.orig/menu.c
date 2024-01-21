/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:	
 *
 *	This module is the main menu handler
 */

#include "debug.h"
#include "tr_core.h"
#include "xtrojka.h"
#include "menu.h"
#include "game.h"
#include "sh_main.h"

extern flag is_wizard;
extern flag is_slick;

extern int starting_speed;
extern int speed;

extern GAME_STATE game_state;


void handle_trojkamenu(item)
int item;
{
	DEBUG("menu.c","handle_trojkamenu")

	switch(item) {
		case trojka_Mnewgame:
			new_game();
			break;

		case trojka_Mabortgame:
			gameover();		
			break;

		case trojka_Mwizardmode:
			toggle_wizard();			
			break;

		case trojka_Mslick:
			toggle_slick();
			break;

		case trojka_Mquit:
			quit_appl();
			break;
	}		
}


void handle_speedmenu(item)
int item;
{
	tt_command CMD_SETSPEED;

	DEBUG("menu.c", "handle_speedmenu")

	speed = item + 1;

	CMD_SETSPEED.command = tc_c_setspeed;
	CMD_SETSPEED.param1 = speed;
	trojka_api(&CMD_SETSPEED);		

	set_speed_item(speed);

	starting_speed = speed;
}


void enable_speedmenu(void)
{
	int i;

	DEBUG("menu.c", "enable_speedmenu")

	for(i = 0; i < SPEEDMENU_ITEMS; i++)
		enable(speed_M, i, 1);
}


void disable_speedmenu(speed_item)
int speed_item;
{
	int i;

	DEBUG("menu.c", "disable_speedmenu")

	for(i = 0; i < speed_item; i++) {
		enable(speed_M, i, 0);
	}
	for(i = speed_item; i < SPEEDMENU_ITEMS; i++) {
		enable(speed_M, i, 1);
	}
}


void set_speed_item(the_speed)
int the_speed;
{
	int i;
	int item;

	DEBUG("menu.c", "set_speed_item")
	
	item = the_speed - 1;

	for(i = 0; i < SPEEDMENU_ITEMS; i++)
		check_speed_item(i,0);

	check_speed_item(item,1);

	if(game_state == st_playing)
		disable_speedmenu(item);

	show_speed(the_speed);
}




void set_menus(gs)
GAME_STATE gs;
{
/*
 *	enables/disables menus depending on game state.
 *	TrojkaMenu:
 *		abort game disabled if idle
 *		new game disabled if playing
 *		others always enabled
 *	SpeedMenu:
 *		everything enabled if idle
 *		everything up to currentspeed disabled in playing	
 */
	DEBUG("menu.c", "set_menus")

	if(gs == st_playing) {
		enable(trojka_M, trojka_Mnewgame, 0);
		enable(trojka_M, trojka_Mabortgame, 1);
		if(is_wizard)
			enable(trojka_M, trojka_Mwizardmode, 0);
#ifdef XPM
		enable(trojka_M, trojka_Mslick, 1);
#else
		enable(trojka_M, trojka_Mslick, 0);
#endif
		disable_speedmenu(speed-1);
	} else 
	if(gs == st_idle) {
		speed = starting_speed;
		enable(trojka_M, trojka_Mnewgame, 1);
		enable(trojka_M, trojka_Mabortgame, 0);
		enable(trojka_M, trojka_Mwizardmode, 1);
		enable(trojka_M, trojka_Mslick, 0);

		enable_speedmenu();
	}
}


void toggle_wizard(void)
{
	tt_command SET_WIZARD;

	DEBUG("menu.c", "toggle_wizard");

	if((is_wizard) && (game_state == st_playing))
		return;

	is_wizard = !is_wizard;
	check_wizard_item(is_wizard);

	if((is_wizard) && (game_state == st_playing)) {

		SET_WIZARD.command = tc_c_setwizard;
		trojka_api(&SET_WIZARD);
		enable(trojka_M, trojka_Mwizardmode, 0);
	}
}


void toggle_slick(void)
{
	DEBUG("menu.c", "toggle_slick");

	is_slick = !is_slick;
	check_slick_item(is_slick);

	if(game_state == st_playing)
		draw_field();
}


