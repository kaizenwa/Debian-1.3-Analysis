/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	27.xi.1995
 *	modified:	24.xii.1995
 *
 *	This module is a low-level menu handler
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>

#include <stdlib.h>

#include "xtrojka.h"
#include "tr_core.h"
#include "menu_tool.h"
#include "menu.h"
#include "debug.h"


extern Widget trojkamenu_but;
extern Widget speedmenu_but;
extern XtAppContext app_context;


Widget trojkamenu_itembox[TROJKAMENU_ITEMS];
Widget trojkamenu_shell, trojkamenu_box;
ITEM_LIST trojkamenu_item[TROJKAMENU_ITEMS] = {
	{ "new_game",		"^N" },
	{ "abort_game",		"^A" },
	{ "-", 			NO_HOT },
	{ "wizard",	 	"^W" },
	{ "slick", 		"^S" },
	{ "-",			NO_HOT },
	{ "quit",		"^Q" }
};


Widget speedmenu_itembox[SPEEDMENU_ITEMS];
Widget speedmenu_shell, speedmenu_box;
ITEM_LIST speedmenu_item[SPEEDMENU_ITEMS] = {
	{ "boring",	NO_HOT },
	{ "",		NO_HOT },
	{ "slow",	NO_HOT },
	{ "",		NO_HOT },
	{ "normal",	NO_HOT },
	{ "",		NO_HOT },
	{ "fast",	NO_HOT },
	{ "",		NO_HOT },
	{ "nerd_speed", NO_HOT }
};

static MENU_BLOCK *trojka_menu;
static MENU_BLOCK *speed_menu;


/*
 *	the functions
 */
void init_menu_sys(void)
{
/*
 *	this function inits the menus' for xtrojka
 */
	int i;
	String label;
	char tmp[255];

	DEBUG("menu_tool.c", "init_menu_sys")

	init_trojkamenu();
	init_speedmenu();

	format_menu(trojkamenu_item, trojkamenu_itembox, TROJKAMENU_ITEMS);
	format_menu(speedmenu_item, speedmenu_itembox, SPEEDMENU_ITEMS);

	/*
	 *	add speed digits to the speed menu
	 */
	for(i = 0; i < SPEEDMENU_ITEMS; i++) {
		XtVaGetValues(speedmenu_itembox[i],
			XtNlabel, &label,
			NULL);
		sprintf(tmp," %d%s", i+1, label);
		XtVaSetValues(speedmenu_itembox[i],
			XtNlabel, tmp,
			NULL);
	}
}



void init_trojkamenu(void)
{
	int i;

	DEBUG("menu_tool.c", "init_trojkamenu");

	/*
	 *	create the menu and add the items to it
	 */	
	trojka_menu = create_menu("trojka",
				trojkamenu_but, 
				&trojkamenu_shell,
				&trojkamenu_box,
				trojkamenu_select_callback);

	for(i = 0; i < TROJKAMENU_ITEMS; i++) {
		add_menu(trojka_menu, trojkamenu_item[i].name,
			&(trojkamenu_itembox[i]));
	}	
}


void init_speedmenu(void)
{
	int i;

	DEBUG("menu_tool.c", "init_speedmenu");

	speed_menu = create_menu("speed",
				speedmenu_but,
				&speedmenu_shell,
				&speedmenu_box,
				speedmenu_select_callback);

	for(i = 0; i < SPEEDMENU_ITEMS; i++) {
		add_menu(speed_menu, speedmenu_item[i].name,
				&(speedmenu_itembox[i]));
	}	
}



MENU_BLOCK *create_menu(name, parent_button, shell, menu_box, std_callback)
String name;
Widget parent_button;
Widget *shell, *menu_box;
void (*std_callback)();
/*
 *	make a menu
 */
{
	char tmp[255];
	char shell_name[255];
	XtTranslations mytrans;
	MENU_BLOCK *mb;

	DEBUG("menu_tool.c", "build_menu");

	if((mb = (MENU_BLOCK*)malloc(sizeof(MENU_BLOCK))) == NULL)
		XtAppError(app_context, "create_menu: No memory for menu");

	sprintf(shell_name,"%s_shell", name);

	*shell = XtCreatePopupShell(
		shell_name,
		overrideShellWidgetClass,
		parent_button,
		NULL,
		0
	);

	sprintf(tmp,"%s_menu", name);
	*menu_box = XtVaCreateManagedWidget(
		tmp,	
		boxWidgetClass,
		*shell,
		XtNhSpace, 0,
		XtNvSpace, 0,
		NULL
	);

	sprintf(tmp,"<EnterWindow>: highlight() \n\
		 <LeaveWindow>: reset() \n\
		 <BtnDown>: set() XtMenuPopup(%s) reset()", shell_name);
	mytrans = XtParseTranslationTable(tmp);
	XtOverrideTranslations(parent_button, mytrans);

	sprintf(tmp, "<BtnUp>: XtMenuPopdown(%s) ", shell_name);
	mytrans = XtParseTranslationTable(tmp);
	XtOverrideTranslations(*shell, mytrans);

	mb->menu_button = parent_button;
	mb->menu_shell = *shell;
	mb->menu_box = *menu_box;
	mb->item_count = 0;
	mb->std_callback = std_callback;

	XtAddCallback(*shell,
		XtNpopupCallback,
		popup_callback,
		mb
	);

	return mb;
}


void add_menu(menu, res_name, widget)
MENU_BLOCK *menu;
String res_name;
Widget *widget;
{
	XtTranslations mytrans;

	DEBUG("menu_tool.c", "add_menu");

	mytrans = XtParseTranslationTable(
		"<EnterWindow>: set() \n\
		 <LeaveWindow>: unset() \n\
		 <BtnDown>: notify() unset()"
	);

	*widget = XtVaCreateManagedWidget(
		res_name, commandWidgetClass,
		menu->menu_box,
		XtNborderWidth, 0,
		NULL
	);

	if(res_name[0] != '-') {
		XtAddCallback(*widget,
			XtNcallback,
			menu->std_callback,
			(XtPointer)menu->item_count);
		XtOverrideTranslations(*widget, mytrans);
	} else
		XtSetSensitive(*widget, False);

	menu->item_count++;
}



void format_menu(item_list, bar_list, items)
ITEM_LIST *item_list;
Widget *bar_list;
int items;
{
	int l, i, max, has_hotkey;
	String resource_label;

	DEBUG("menu_tool.c", "format_menu");
	/*
	 *	find the longest in the menu list
	 */
	max = 0;
	has_hotkey = False;


	for(i = 0; i < items; i++) {
		XtVaGetValues(bar_list[i],
			XtNlabel, &resource_label,
			NULL);
		if((l = strlen(resource_label)) > max)
			max = l;
		if(strcmp(item_list[i].hotkey, NO_HOT))
			has_hotkey = True;	
	}

	for(i = 0; i < items; i++)  {
		XtVaGetValues(bar_list[i],
			XtNlabel, &resource_label,
			NULL);
		XtVaSetValues(bar_list[i],
			XtNlabel, format_item(	resource_label, 
						item_list[i].hotkey,
						max, has_hotkey),
			NULL
		);
	}
}



void popup_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	Position x,y;	
	Dimension height;
	MENU_BLOCK *mb;

	mb = (MENU_BLOCK*)client_data;

	DEBUG("menu_tool.c", "popup_callback")

	XtTranslateCoords(mb->menu_button, (Position)0, (Position)0, &x,&y);
	XtVaGetValues(mb->menu_button, XtNheight, &height, NULL);
	XtVaSetValues(mb->menu_shell, XtNx, x-1, XtNy, y+height, NULL);
}



void trojkamenu_select_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	int item;

	DEBUG("menu_tool.c", "trojkamenu_select_callback")

	XtPopdown(trojka_menu->menu_shell);
	item = (int)client_data;
	handle_trojkamenu(item);
}


void speedmenu_select_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	int item;

	DEBUG("menu_tool.c", "speedmenu_select_callback")

	item = (int)client_data;
	XtPopdown(speedmenu_shell);
	handle_speedmenu(item);
}


void enable(menu, item, enabled)
int menu;
int item;
flag enabled;
{
/*
 * 	enables or disables a menu-item
 */
	DEBUG("menu_tool.c", "enable")

	switch(menu) {
		case trojka_M:
			XtSetSensitive(trojkamenu_itembox[item], enabled);
			break;
		case speed_M:
			XtSetSensitive(speedmenu_itembox[item], enabled);
			break;
	}
}
	

void check_wizard_item(checked) 
flag checked;
{
/*
 *	(un)checks the wizard item in the trojka menu
 */
	DEBUG("menu_tool.c", "check_wizard_item")

	check_item(trojkamenu_itembox[trojka_Mwizardmode], checked);
}


void check_slick_item(checked) 
flag checked;
{
/*
 *	(un)checks the slick item in the trojka menu
 */
	DEBUG("menu_tool.c", "check_slick_item")

	check_item(trojkamenu_itembox[trojka_Mslick], checked);
}

void check_speed_item(speed, on)
int speed;
flag on;
/*
 *	(un)checks the menu item of speed
 */
{
	DEBUG("menu_tool.c", "check_speed_item")

	check_item(speedmenu_itembox[speed], on);
}


void check_item(item, checked)
Widget item;
flag checked;
{
	String label;
	char new_label[255];

	DEBUG("menu_tool.c", "check_item")

	XtVaGetValues(item, XtNlabel, &label, NULL);
	strcpy(new_label, label);
	new_label[0] = checked ? CHECKMARK : ' ';
	XtVaSetValues(item, XtNlabel, new_label, NULL);
}


char *format_item(item, hotkey, swidth, has_hotkey)
char *item;
char *hotkey;
int swidth;
flag has_hotkey;
{
	char format[255];
	static char string[255];
	int i;

	DEBUG("menu_tool.c", "format_item");

	/*
	 *	handle separator bar
	 */

	if(item[0] == SEPARATOR) {

		swidth += (ITEM_WHITE+ (HOTKEY_WHITE * 2)+(HOTKEY_LEN));
		for(i = 0; i < swidth; i++) 
			string[i] = SEPARATOR;

		return string;
	}


	/*
	 *	build formatting string and use it to fill in the itemstring
	 */
	if(has_hotkey) {


		sprintf(format,"%%%ds%%-%ds%%%ds%%%ds%%%ds",
			ITEM_WHITE, swidth, HOTKEY_WHITE, HOTKEY_LEN,
			HOTKEY_WHITE);
		if(strcmp(hotkey, NO_HOT))
			sprintf(string, format, " ", item, " ", 
				hotkey, " ");
		else
			sprintf(string, format, " ", item, " ", 
				" ", " ");
	} else {

		sprintf(format,"%%%ds%%-%ds%%%ds",
			ITEM_WHITE, swidth, ITEM_WHITE);
		sprintf(string, format, " ", item, " ");
	}

	return string;
}

