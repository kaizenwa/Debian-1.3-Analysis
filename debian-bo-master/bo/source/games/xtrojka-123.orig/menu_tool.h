/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	27.xi.1995
 *	modified:	24.xii.1995	major restyle
 *			11.iii.1996	added prototypes
 *
 *
 *	header file for menu_tool.c
 */

#ifndef _menu_tool_h_
#define _menu_tool_h_

#define HOTKEY_LEN		2
#define HOTKEY_WHITE		1
#define ITEM_WHITE		1

#define SEPARATOR		'-'
#define NO_HOT			"_"
#define CHECKMARK		187


typedef struct _ITEM_LIST {
	String	name;
	String	hotkey;
} ITEM_LIST;


/*
 *	this block should be statically defined!
 *	it must be passed in 'client_data' to a callback function
 */
typedef struct _MENU_BLOCK {
	Widget	menu_button;		
	Widget	menu_shell;
	Widget	menu_box;
	int	item_count;
	void	(*std_callback)();
} MENU_BLOCK;


/*
 *	function prototypes
 */

void init_menu_sys(void);
void init_trojkamenu(void);
void init_speedmenu(void);
MENU_BLOCK *create_menu(String, Widget, Widget*, Widget*, void());
void add_menu(MENU_BLOCK*, String, Widget *);
void format_menu(ITEM_LIST*, Widget *, int);
void popup_callback(Widget, XtPointer, XtPointer);
void trojkamenu_select_callback(Widget, XtPointer, XtPointer);
void speedmenu_select_callback(Widget, XtPointer, XtPointer);
void enable(int, int, flag);
void check_wizard_item(flag);
void check_slick_item(flag);
void check_speed_item(int, flag);
void check_item(Widget, flag);
char *format_item(char *, char*, int, flag);


#endif /* _menu_tool_h_ */


