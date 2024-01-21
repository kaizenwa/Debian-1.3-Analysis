/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:	27.xii.1995
 *
 *	This module defines the menu information
 */

#ifndef _menu_h_
#define _menu_h_

#include "menu_tool.h"

enum {
	trojka_M = 0,
	speed_M
};

/*
 *	TROJKA MENU DEFINITION
 */

#define TROJKAMENU_ITEMS	7
enum {
	trojka_Mnewgame = 0,
	trojka_Mabortgame,
	/* separator */
	trojka_Mwizardmode = 3,
	trojka_Mslick = 4,
	/* separator */
	trojka_Mquit = 6
};

/*
 *	SPEED MENU DEFINITION
 */

#define SPEEDMENU_ITEMS		9



/*
 *	function prototypes
 */

void handle_trojkamenu(int);
void handle_speedmenu(int);
void enable_speedmenu(void);
void disable_speedmenu(int);
void set_speed_item(int);
void set_menus(GAME_STATE);
void toggle_wizard(void);
void toggle_slick(void);



#endif /* _menu_h_ */
