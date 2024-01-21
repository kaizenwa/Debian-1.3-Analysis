/*
 *                            COPYRIGHT
 *
 *  PCB, interactive printed circuit board design
 *  Copyright (C) 1994,1995,1996 Thomas Nau
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Contact addresses for paper mail and Email:
 *  Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
 *  Thomas.Nau@rz.uni-ulm.de
 *
 */

static	char	*rcsid = "$Id: menu.c,v 143.1 1996/09/16 09:08:44 nau Exp $";

/* initializes menus and handles callbacks
 */

#include <string.h>
#include <ctype.h>
#include <sys/types.h>

#include "global.h"

#include "buffer.h"
#include "data.h"
#include "dialog.h"
#include "draw.h"
#include "mymem.h"
#include "menu.h"
#include "misc.h"
#include "sizedialog.h"

#include <X11/Xaw/Form.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>

/* ---------------------------------------------------------------------------
 * include icon data
 */
#include "check_icon.data"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	CBPOPUP_Display(Widget, XtPointer, XtPointer);
static	void	CBPOPUP_File(Widget, XtPointer, XtPointer);
static	void	CBPOPUP_Buffer(Widget, XtPointer, XtPointer);
static	void	CB_Action(Widget, XtPointer, XtPointer);
static	void	CB_Position(Widget, XtPointer, XtPointer);
static	void	CB_ElementPosition(Widget, XtPointer, XtPointer);
static	void	CB_TextPosition(Widget, XtPointer, XtPointer);
static	void	CB_ObjectPosition(Widget, XtPointer, XtPointer);
static	void	CB_Sizes(Widget, XtPointer, XtPointer);
static	void	CB_About(Widget, XtPointer, XtPointer);
static	void	InitPopupTree(Widget, PopupEntryTypePtr);
static	void	InitPopupMenu(Widget, PopupMenuTypePtr);
static	Widget	InitCommandButton(Widget, CommandButtonTypePtr, Widget, Widget);
static	Boolean	InitCheckPixmap(void);

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Pixmap	Check = BadAlloc;

/* ---------------------------------------------------------------------------
 * file menu button
 */
static	PopupEntryType	FileMenuEntries[] = {
	{ "save", "save layout", CB_Action, "Save,Layout", NULL },
	{ "saveas", "save layout as...", CB_Action, "Save,LayoutAs", NULL },
	{ "load", "load layout", CB_Action, "Load,Layout", NULL },
	{ "loadelement", "load element data to paste-buffer", CB_Action,
		"PasteBuffer,Clear\n"
		"Load,ElementToBuffer", NULL },
	{ "loadlayout", "load layout data to paste-buffer", CB_Action,
		"PasteBuffer,Clear\n"
		"Load,LayoutToBuffer", NULL },
	{ "print", "print layout", CB_Action, "Print", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "save connection data of", NULL, NULL, NULL },
	{ "savesingle", "a single element", CB_ElementPosition,
		"Save,ElementConnections", NULL },
	{ "saveall", "all elements", CB_Action, "Save,AllConnections", NULL },
	{ "saveunused", "unused pins", CB_Action, "Save,AllUnusedPins", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "change name of", NULL, NULL, NULL },
	{ "layoutname", "layout", CB_Action, "ChangeName,Layout", NULL },
	{ "layername", "active layer", CB_Action, "ChangeName,Layer", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "new", "start new layout", CB_Action, "New", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "quit", "quit program", CB_Action, "Quit", NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	FileMenu =
	{ "file", NULL, FileMenuEntries, CBPOPUP_File, NULL, NULL};
static	MenuButtonType	FileMenuButton =
	{ "file", "File", &FileMenu, NULL};

/* ---------------------------------------------------------------------------
 * display menu button
 */
static	PopupEntryType	DisplayMenuEntries[] = {
	{ "redraw", "redraw layout", CB_Action, "Display,Redraw", NULL },
	{ "center", "center layout", CB_Position, "Display,Center", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "displayGrid", "display grid", CB_Action, "Display,Grid", NULL },
	{ "toggleGrid", "use absolute grid", CB_Position,
		"Display,ToggleGrid", NULL },
	{ "toggleAllDirections", "'all-direction' lines", CB_Action,
		"Display,ToggleAllDirections", NULL },
	{ "solderSide", "look at solder-side", CB_Action, "SwapSides", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "grid setting", NULL, NULL, NULL },
	{ "grid05", " 5 mil", CB_Position, "SetValue,Grid,5", NULL },
	{ "grid10", "10 mil", CB_Position, "SetValue,Grid,10", NULL },
	{ "grid20", "20 mil", CB_Position, "SetValue,Grid,20", NULL },
	{ "grid50", "50 mil", CB_Position, "SetValue,Grid,50", NULL },
	{ "grid100", "100 mil", CB_Position, "SetValue,Grid,100", NULL },
	{ "gridInc", "increment by 5", CB_Action, "SetValue,Grid,+5", NULL },
	{ "gridDec", "decrement by 5", CB_Action, "SetValue,Grid,-5", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "zoom setting", NULL, NULL, NULL },
	{ "zoom1", "1 : 1 ", CB_Position, "SetValue,Zoom,0", NULL },
	{ "zoom2", "1 : 2 ", CB_Position, "SetValue,Zoom,1", NULL },
	{ "zoom4", "1 : 4 ", CB_Position, "SetValue,Zoom,2", NULL },
	{ "zoom8", "1 : 8 ", CB_Position, "SetValue,Zoom,3", NULL },
	{ "zoom16", "1 :16 ", CB_Position, "SetValue,Zoom,4", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "displayed element-name", NULL, NULL, NULL },
	{ "description", "description", CB_Action, "Display,Description", NULL },
	{ "onPCB", "name on PCB", CB_Action, "Display,NameOnPCB", NULL },
	{ "value", "value", CB_Action, "Display,Value", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	DisplayMenu =
	{ "display", NULL, DisplayMenuEntries, CBPOPUP_Display, NULL, NULL};
static	MenuButtonType	DisplayMenuButton =
	{ "display", "Display", &DisplayMenu, NULL};

/* ---------------------------------------------------------------------------
 * object menu button
 */
static	PopupEntryType	ObjectMenuEntries[] = {
	{ "header", "elements", NULL, NULL, NULL },
	{ "pinout", "display pinout", CB_ElementPosition, "Display,Pinout", NULL },
	{ "changeName", "change name", CB_ElementPosition,
		"ChangeName,Object", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "text objects", NULL, NULL, NULL },
	{ "edit", "edit", CB_TextPosition, "ChangeName,Object", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "layer-groups", NULL, NULL, NULL },
	{ "lgedit", "edit layer-groups", CB_Action, "EditLayerGroups", NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	ObjectMenu =
	{ "objects",  NULL, ObjectMenuEntries, NULL, NULL, NULL };
static	MenuButtonType	ObjectMenuButton =
	{ "objects", "Objects", &ObjectMenu, NULL};

/* ---------------------------------------------------------------------------
 * selection menu button
 */
static	PopupEntryType	SelectionMenuEntries[] = {
	{ "select", "select all objects", CB_Action, "Select,All", NULL },
	{ "selectconnection", "select all connected objects", CB_Action,
		"Select,Connection", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "unselect", "unselect all objects", CB_Action, "Unselect,All", NULL },
	{ "unselectconnection", "unselect all connected objects", CB_Action,
		"Unselect,Connection", NULL },
#ifdef HAS_REGEX
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "select by name", NULL, NULL, NULL },
	{ "allByName", "all objects", CB_Action, "Select,ObjectByName", NULL },
	{ "elementByName", "elements", CB_Action, "Select,ElementByName", NULL },
	{ "padByName", "pads", CB_Action, "Select,PadByName", NULL },
	{ "pinByName", "pins", CB_Action, "Select,PinByName", NULL },
	{ "textByName", "text objects", CB_Action, "Select,TextByName", NULL },
	{ "viaByName", "vias", CB_Action, "Select,ViaByName", NULL },
#endif
	{ "line", NULL, NULL, NULL, NULL },
	{ "remove", "remove selected objects", CB_Action,
		"RemoveSelected", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "change size of selected objects", NULL, NULL, NULL },
	{ "decrementline", "lines -10 mil", CB_Action,
		"ChangeSize,SelectedLines,-10", NULL },
	{ "incrementline", "lines +10 mil", CB_Action,
		"ChangeSize,SelectedLines,+10", NULL },
	{ "decrementpad", "pads -10 mil", CB_Action,
		"ChangeSize,SelectedPads,-10", NULL },
	{ "incrementpad", "pads +10 mil", CB_Action,
		"ChangeSize,SelectedPads,+10", NULL },
	{ "decrementpin", "pins -10 mil", CB_Action,
		"ChangeSize,SelectedPins,-10", NULL },
	{ "incrementpin", "pins +10 mil", CB_Action,
		"ChangeSize,SelectedPins,+10", NULL },
	{ "decrementtext", "texts -10 mil", CB_Action,
		"ChangeSize,SelectedTexts,-10", NULL },
	{ "incrementtext", "texts +10 mil", CB_Action,
		"ChangeSize,SelectedTexts,+10", NULL },
	{ "decrementvia", "vias -10 mil", CB_Action,
		"ChangeSize,SelectedVias,-10", NULL },
	{ "incrementvia", "vias +10 mil", CB_Action,
		"ChangeSize,SelectedVias,+10", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "change drilling hole of selected objects", NULL, NULL, NULL },
	{ "decrementviahole", "vias -10 mil", CB_Action,
		"Change2ndSize,SelectedVias,-10", NULL },
	{ "incrementviahole", "vias +10 mil", CB_Action,
		"Change2ndSize,SelectedVias,+10", NULL },
	{ "decrementpinhole", "pins -10 mil", CB_Action,
		"Change2ndSize,SelectedPins,-10", NULL },
	{ "incrementpinhole", "pins +10 mil", CB_Action,
		"Change2ndSize,SelectedPins,+10", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "change square-flag of selected objects", NULL, NULL, NULL },
	{ "elementsquare", "elements", CB_Action,
		"ChangeSquare,SelectedElements", NULL },
	{ "pinsquare", "pins", CB_Action,
		"ChangeSquare,SelectedPins", NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	SelectionMenu =
	{ "selection",  NULL, SelectionMenuEntries, NULL, NULL, NULL };
static	MenuButtonType	SelectionMenuButton =
	{ "selection", "Selection", &SelectionMenu, NULL};

/* ---------------------------------------------------------------------------
 * paste buffer menu button
 */
static	PopupEntryType	BufferMenuEntries[] = {
	{ "copy", "copy selection to buffer", CB_Position,
		"PasteBuffer,Clear\n"
		"PasteBuffer,AddSelected\n"
		"Mode,PasteBuffer",
		NULL },
	{ "cut", "cut selection to buffer", CB_Position,
		"PasteBuffer,Clear\n"
		"PasteBuffer,AddSelected\n"
		"RemoveSelected\n"
		"Mode,PasteBuffer",
		NULL },
	{ "paste", "paste buffer to layout", CB_Action, "Mode,PasteBuffer", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "rotate", "rotate buffer 90 deg", CB_Action,
		"Mode,PasteBuffer\n"
		"PasteBuffer,Rotate,1",
		NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "clear", "clear buffer", CB_Action, "PasteBuffer,Clear", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "header", "select current buffer", NULL, NULL, NULL },
	{ "buffer1", "#1", CB_Action, "PasteBuffer,1", NULL },
	{ "buffer2", "#2", CB_Action, "PasteBuffer,2", NULL },
	{ "buffer3", "#3", CB_Action, "PasteBuffer,3", NULL },
	{ "buffer4", "#4", CB_Action, "PasteBuffer,4", NULL },
	{ "buffer5", "#5", CB_Action, "PasteBuffer,5", NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	BufferMenu =
	{ "buffer",  NULL, BufferMenuEntries, CBPOPUP_Buffer, NULL, NULL };
static	MenuButtonType	BufferMenuButton =
	{ "buffer", "Buffer", &BufferMenu, NULL};

/* ---------------------------------------------------------------------------
 * connection menu button
 */
static	PopupEntryType	ConnectionMenuEntries[] = {
	{ "lookup", "lookup connection to object", CB_ObjectPosition,
		"Connection,Find", NULL },
	{ "resetPVP", "reset scanned pads/pins/vias", CB_Action,
		"Connection,ResetPinsViasAndPads\n"
		"Display,Redraw", NULL },
	{ "resetLR", "reset scanned lines/polygons", CB_Action,
		"Connection,ResetLinesAndPolygons\n"
		"Display,Redraw", NULL },
	{ "reset", "reset all connections", CB_Action,
		"Connection,Reset\n"
		"Display,Redraw", NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	ConnectionMenu =
	{ "connections",  NULL, ConnectionMenuEntries, NULL, NULL, NULL };
static	MenuButtonType	ConnectionMenuButton =
	{ "connections", "Connections", &ConnectionMenu, NULL};

/* ---------------------------------------------------------------------------
 * undo menu button
 */
static	PopupEntryType	UndoMenuEntries[] = {
	{ "undo", "undo last destructive operation", CB_Action, "Undo", NULL },
	{ "clear", "clear undo-buffer", CB_Action, "Undo,ClearList", NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	UndoMenu =
	{ "undo",  NULL, UndoMenuEntries, NULL, NULL, NULL };
static	MenuButtonType	UndoMenuButton =
	{ "undo", "Undo", &UndoMenu, NULL};

/* ---------------------------------------------------------------------------
 * groundplane menu button
 */
static	PopupEntryType	GroundplaneMenuEntries[] = {
	{ "add", "add selected", CB_Action, "Groundplane,AddSelected", NULL },
	{ "remove", "remove selected", CB_Action,
		"Groundplane,RemoveSelected", NULL },
	{ "clear", "clear list", CB_Action, "Groundplane,ClearList", NULL },
	{ "line", NULL, NULL, NULL, NULL },
	{ "select", "select members", CB_Action,
		"Groundplane,SelectMembers", NULL },
	{ NULL, NULL, NULL, NULL, NULL }};
static	PopupMenuType	GroundplaneMenu =
	{ "groundplane",  NULL, GroundplaneMenuEntries, NULL, NULL, NULL };
static	MenuButtonType	GroundplaneMenuButton =
	{ "groundplane", "Groundplane", &GroundplaneMenu, NULL};

/* ----------------------------------------------------------------------
 * command buttons
 */
static	CommandButtonType	AboutButton =
	{ "about", "About", CB_About, NULL, NULL },
							SizesButton =
	{ "sizes", "Sizes", CB_Sizes, NULL, NULL };



/* ----------------------------------------------------------------------
 * menu callback interface for actions routines that don't need
 * position information
 *
 * ClientData passes a pointer to a comma seperated list of arguments.
 * The first one determines the action routine to be called, the
 * rest of them are arguments to the action routine
 *
 * if more than one action is to be called a new list is seperated
 * by '\n'
 */
static void CB_Action(Widget W, XtPointer ClientData, XtPointer CallData)
{
	static	char	**array = NULL;
	static	size_t	number = 0;
			int		n;
			char	*copy,
					*current,
					*next,
					*token;

		/* get a copy of the string and split it */
	copy = MyStrdup((char *) ClientData, "CB_CallActionWithoutPosition()");

		/* first loop over all action routines;
		 * strtok cannot be used in nested loops because it saves
		 * a pointer in a private data area which would be corrupted
		 * by th inner loop
		 */
	for (current = copy; current; current = next)
	{
			/* lookup seperating '\n' character;
			 * update pointer if not at the end of the string
			 */
		for (next = current; *next && *next != '\n'; next++);
		if (*next)
		{
			*next = '\0';
			next++;
		}
		else
			next = NULL;
		
		token = strtok(current, ",");
		for (n = 0; token; token = strtok(NULL, ","), n++)
		{
				/* allocate memory if necessary */
			if (n >= number)
			{
				number += 10;
				array = MyRealloc(array, number*sizeof(char *),
					"CB_CallActionWithoutPosition()");
			}
			array[n] = token;
		}
			/* call action routine */
		XtCallActionProc(Output.Output, array[0], NULL, &array[1], n-1);
	}

		/* release memory */
	SaveFree(copy);
}

/* ----------------------------------------------------------------------
 * menu callback interface for misc actions routines that need
 * position information
 */
static void CB_Position(Widget W, XtPointer ClientData, XtPointer CallData)
{
	if (GetPosition("move pointer to the appropriate screen position and press a button"))
		CB_Action(W, ClientData, CallData);
}

/* ----------------------------------------------------------------------
 * menu callback interface for element related actions routines that need
 * position information
 */
static void CB_ElementPosition(Widget W,
	XtPointer ClientData, XtPointer CallData)
{
	if (GetPosition("press a button at the elements location"))
		CB_Action(W, ClientData, CallData);
}

/* ----------------------------------------------------------------------
 * menu callback interface for text related actions routines that need
 * position information
 */
static void CB_TextPosition(Widget W,
	XtPointer ClientData, XtPointer CallData)
{
	if (GetPosition("press a button at the text location"))
		CB_Action(W, ClientData, CallData);
}

/* ----------------------------------------------------------------------
 * menu callback interface for pin/via related actions routines that need
 * position information update
 */
static void CB_ObjectPosition(Widget W,
	XtPointer ClientData, XtPointer CallData)
{
	if (GetPosition("press a button at an 'connecting-objects' location"))
		CB_Action(W, ClientData, CallData);
}

/* ---------------------------------------------------------------------- 
 * called before display menu is popped up
 * used to mark the current grid-mode, zoom value ...
 */
static void CBPOPUP_Display(Widget W, XtPointer ClientData, XtPointer CallData)
{
	RemoveCheckFromMenu(&DisplayMenu);
	XtSetSensitive(XtNameToWidget(DisplayMenu.W, "displayGrid"),
		(TO_SCREEN(PCB->Grid) >= MIN_GRID_DISTANCE));
	if (TEST_FLAG(ABSOLUTEFLAG, PCB))
		CheckEntry(&DisplayMenu, "toggleGrid");
	if (TEST_FLAG(ALLDIRCETIONFLAG, PCB))
		CheckEntry(&DisplayMenu, "toggleAllDirections");
	if (Settings.DrawGrid)
		CheckEntry(&DisplayMenu, "displayGrid");
	if (Settings.ShowSolderSide)
		CheckEntry(&DisplayMenu, "solderSide");
	switch(PCB->Zoom)
	{
		case 0:	CheckEntry(&DisplayMenu, "zoom1"); break;
		case 1:	CheckEntry(&DisplayMenu, "zoom2"); break;
		case 2:	CheckEntry(&DisplayMenu, "zoom4"); break;
		case 3:	CheckEntry(&DisplayMenu, "zoom8"); break;
		case 4:	CheckEntry(&DisplayMenu, "zoom16"); break;
	}
	CheckEntry(&DisplayMenu,
		TEST_FLAG(NAMEONPCBFLAG, PCB) ? "onPCB" :
			TEST_FLAG(DESCRIPTIONFLAG, PCB) ? "description" : "value");
}

/* ---------------------------------------------------------------------- 
 * called before file menu is popped up
 * enables/disables printing
 */
static void CBPOPUP_File(Widget W, XtPointer ClientData, XtPointer CallData)
{
	XtSetSensitive(XtNameToWidget(FileMenu.W, "print"),
		!IsDataEmpty(PCB->Data));
}

/* ---------------------------------------------------------------------- 
 * called before buffer menu is popped up
 */
static void CBPOPUP_Buffer(Widget W, XtPointer ClientData, XtPointer CallData)
{
	char		name[10];

	RemoveCheckFromMenu(&BufferMenu);
	sprintf(name, "buffer%i", Settings.BufferNumber+1);
	CheckEntry(&BufferMenu, name);
}

/* ---------------------------------------------------------------------- 
 * callback routine used by sizes button
 */
static void CB_Sizes(Widget W, XtPointer ClientData, XtPointer CallData)
{
	SizeDialog();
}

/* ---------------------------------------------------------------------- 
 * callback routine used by about button
 */
static void CB_About(Widget W, XtPointer ClientData, XtPointer CallData)
{
	AboutDialog();
}

/* ---------------------------------------------------------------------------
 * remove all 'check' symbols from menu entries
 */
void RemoveCheckFromMenu(PopupMenuTypePtr MenuPtr)
{
	PopupEntryTypePtr	entries = MenuPtr->Entries;

	for (; entries->Name; entries++)
		if (entries->Label)
			XtVaSetValues(entries->W, XtNleftBitmap, None, NULL);
}

/* ---------------------------------------------------------------------------
 * add 'check' symbol to menu entry
 */
void CheckEntry(PopupMenuTypePtr MenuPtr, String WidgetName)
{
	PopupEntryTypePtr	entries = MenuPtr->Entries;

	if (InitCheckPixmap())
		for (; entries->Name; entries++)
			if (entries->Label && !strcmp(entries->Name, (char *) WidgetName))
			{
				XtVaSetValues(entries->W, XtNleftBitmap, Check, NULL);
				return;
			}
}

/* ---------------------------------------------------------------------------
 * initializes a command button
 */
static Widget InitCommandButton(Widget Parent, CommandButtonTypePtr CommandButtonPtr, Widget Top, Widget Left)
{
	CommandButtonPtr->W = XtVaCreateManagedWidget(CommandButtonPtr->Name, commandWidgetClass,
		Parent,
		XtNlabel, CommandButtonPtr->Label,
		XtNfromHoriz, Left,
		XtNfromVert, Top,
		LAYOUT_TOP,
		NULL);
	XtAddCallback(CommandButtonPtr->W, XtNcallback,
		CommandButtonPtr->Callback, CommandButtonPtr->ClientData);

		/* return the created button widget to position some others */
	return(CommandButtonPtr->W);
}

/* ---------------------------------------------------------------------------
 * initializes a menu tree
 * depending on the 'Name' field of the struct either a smeBSB or a
 * smeLine widget is created. If a callback routine is defined for the
 * smeBSB widget it will be registered else the entry will be disabled
 */
static void InitPopupTree(Widget Parent, PopupEntryTypePtr EntryPtr)
{
	for (; EntryPtr->Name; EntryPtr++)
	{
			/* check if it's only a seperator */
		if (EntryPtr->Label)
		{
			EntryPtr->W = XtVaCreateManagedWidget(EntryPtr->Name, smeBSBObjectClass,
				Parent,
				XtNlabel, EntryPtr->Label,
				XtNleftMargin, 12, 
				XtNsensitive, True,
				NULL);
			if (EntryPtr->Callback)
				XtAddCallback(EntryPtr->W, XtNcallback,
					EntryPtr->Callback, (XtPointer) EntryPtr->ClientData);
			else
					/* entry is not selectable */
				XtVaSetValues(EntryPtr->W,
					XtNsensitive, False,
					XtNvertSpace, 60,
					NULL);
		}
		else
			XtVaCreateManagedWidget("menuLine",smeLineObjectClass,Parent,NULL);
	}
}

/* ---------------------------------------------------------------------------
 * initializes one popup menu
 * create a popup shell, add all entries to it and register the popup and
 * popdown callback functions if required
 */
static void InitPopupMenu(Widget Parent, PopupMenuTypePtr MenuPtr)
{
	MenuPtr->W = XtVaCreatePopupShell(MenuPtr->Name, simpleMenuWidgetClass,
		Parent,
		XtNlabel, MenuPtr->Label,
		XtNsensitive, True,
		NULL);
	InitPopupTree(MenuPtr->W, MenuPtr->Entries);

		/* install popup and popdown callbacks */
	if (MenuPtr->CB_Popup)
		XtAddCallback(MenuPtr->W, XtNpopupCallback, MenuPtr->CB_Popup, NULL);
	if (MenuPtr->CB_Popdown)
		XtAddCallback(MenuPtr->W, XtNpopupCallback, MenuPtr->CB_Popdown, NULL);
}

/* ---------------------------------------------------------------------------
 * initializes one menubutton plus it's menu
 * create a menu button widget first than add the popup shell to it
 */
Widget InitMenuButton(Widget Parent,
	MenuButtonTypePtr MenuButtonPtr, Widget Top, Widget Left)
{
	MenuButtonPtr->W = XtVaCreateManagedWidget(MenuButtonPtr->Name, menuButtonWidgetClass,
		Parent,
		XtNlabel, MenuButtonPtr->Label,
		XtNmenuName, MenuButtonPtr->PopupMenu->Name,
		XtNfromHoriz, Left,
		XtNfromVert, Top,
		LAYOUT_TOP,
		NULL);
	InitPopupMenu(MenuButtonPtr->W, MenuButtonPtr->PopupMenu);

		/* return the created button widget to position some others */
	return(MenuButtonPtr->W);
}

/* ---------------------------------------------------------------------------
 * initializes 'check' pixmap if not already done
 */
static Boolean InitCheckPixmap(void)
{
	if (Check == BadAlloc)
		Check = XCreateBitmapFromData(Dpy,
					RootWindowOfScreen(XtScreen(Output.Toplevel)),
					check_icon_bits, check_icon_width, check_icon_height);
	return(Check != BadAlloc ? True : False);
}

/* ---------------------------------------------------------------------------
 * initializes button related menus
 * also initializes control panel
 */
void InitMenu(Widget Parent, Widget Top, Widget Left)
{
	Widget	last;

	last = InitCommandButton(Parent, &AboutButton, Top, Left);
	last = InitMenuButton(Parent, &FileMenuButton, Top, last);
	last = InitMenuButton(Parent, &DisplayMenuButton, Top, last);
	last = InitCommandButton(Parent, &SizesButton, Top, last);
	last = InitMenuButton(Parent, &ObjectMenuButton, Top, last);
	last = InitMenuButton(Parent, &SelectionMenuButton, Top, last);
	last = InitMenuButton(Parent, &BufferMenuButton, Top, last);
	last = InitMenuButton(Parent, &ConnectionMenuButton, Top, last);
	last = InitMenuButton(Parent, &UndoMenuButton, Top, last);
	last = InitMenuButton(Parent, &GroundplaneMenuButton, Top, last);
	Output.Menu = last;
}

