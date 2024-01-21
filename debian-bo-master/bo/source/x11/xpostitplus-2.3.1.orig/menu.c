#ifndef lint
static char	*RCSid = "$Id: menu.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * menu.c - routines to handle the menu.
 *
 * Originally by David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * Modified by
 * Michael J. Hammel (03/01/95)
 * Contractor
 * 1150 Inca St. TH 70
 * Denver, CO 80204
 * mjhammel@csn.org
 *
 * $Log: menu.c,v $
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 *
 * Revision 1.2  90/06/14  11:19:39  davy
 * Ported to X11 Release 4.  Changed to use the SimpleMenu widget instead
 * of a List widget for the menu.
 * 
 * Revision 1.1  90/06/13  09:48:47  davy
 * Initial revision
 */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Shell.h>
#include <stdio.h>

#include "xpostit.h"

static String menustrings[] = {
#define MenuCreate_1p5x2	0
	"Create 1.5x2 Note",
#define MenuCreate_2x3		1
	"Create 2x3 Note",
#define MenuCreate_3x3		2
	"Create 3x3 Note",
#define MenuCreate_3x4		3
	"Create 3x4 Note",
#define MenuCreate_3x5		4
	"Create 3x5 Note",
#define MenuCreate_4x6		5
	"Create 4x6 Note",
#define EndNoteTypes		6
	" ",
#define MenuRaiseAll		7
	"Raise All Notes",
#define MenuLowerAll		8
	"Lower All Notes",
#define MenuSaveAll		9
	"Save All Notes",
#define MenuShowHidden		10
	"Hidden Notes",
#define MenuUnHideAll		11
	"Unhide All Notes",
#define MenuHideAll		12
	"Hide All Notes",
#define MenuCascade		13
	"Cascade Notes",
#define MenuFindANote		14
	"Find A Note",
#define EndNoteFunctions	15
	" ",
#define MenuExit		16
	"Exit",
#define MenuLastEntry		17
	0,
};

Widget			menuwidget, hiddennotes;
extern XtAppContext	appcontext;
static void		HandleMenuSelection();

/*
 * CreateMenuWidget - create the widget used for the menu.
 */
void
CreateMenuWidget()
{
	Arg args[8];
	Widget entry;
	register int i, nargs;
	XtCallbackRec callbacks[2];

	/*
	 * Set the callback.
	 */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(HandleMenuSelection, NULL);

	/*
	 * Create the menu widget.
	 */
	nargs = 0;
	SetArg(XtNmenuOnScreen, True);
	SetArg(XtNlabel, PostItNoteClass);

	/*
	 * The menu is done with a popup shell.
	 */
	menuwidget = XtCreatePopupShell("Menu", simpleMenuWidgetClass,
					toplevel, args, nargs);

	for (i=0; menustrings[i] != NULL; i++)
	{
		switch (i)
		{
			case EndNoteTypes:
			case EndNoteFunctions:
				entry = XtCreateManagedWidget(menustrings[i],
						smeLineObjectClass, menuwidget,
						NULL, 0);
				break;

			default:
				entry = XtCreateManagedWidget(menustrings[i],
						smeBSBObjectClass, menuwidget,
						NULL, 0);
				XtAddCallback(entry, XtNcallback, 
					HandleMenuSelection, (XtPointer)i);
				break;
		}
	}
		
}

/*
 * HandleMenuSelection - callback from menu widget to handle a selection.
 */
static void
HandleMenuSelection(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	/*
	 * Dispatch the command.
	 */
	switch ((int) client_data) {
	case MenuCreate_1p5x2:
		CreateNewNote(PostItNote_1p5x2);
		break;
	case MenuCreate_2x3:
		CreateNewNote(PostItNote_2x3);
		break;
	case MenuCreate_3x3:
		CreateNewNote(PostItNote_3x3);
		break;
	case MenuCreate_3x4:
		CreateNewNote(PostItNote_3x4);
		break;
	case MenuCreate_3x5:
		CreateNewNote(PostItNote_3x5);
		break;
	case MenuCreate_4x6:
		CreateNewNote(PostItNote_4x6);
		break;
	case MenuRaiseAll:
		RaiseAllNotes();
		break;
	case MenuLowerAll:
		LowerAllNotes();
		break;
	case MenuSaveAll:
		SaveAllNotes(True);
		break;
	case MenuShowHidden:
		PopUpList();
		break;
	case MenuUnHideAll:
		UnHideAllNotes();
		break;
	case MenuHideAll:
		HideAllNotes();
		break;
	case MenuCascade:
		CascadeNotes();
		break;
	case MenuFindANote:
		MakeNoteListCB();
		break;
	case MenuExit:
		ByeBye();
		break;
	}
}
