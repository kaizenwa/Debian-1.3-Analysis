/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"
 *
 *	created:	27.xi.1995
 *	modified:
 *
 *	This is action handler
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "actions.h"

#include "xtrojka.h"
#include "debug.h"

extern void quit_appl_action();
extern void gameover_action();
extern void new_game();
extern void toggle_wizard();
extern void toggle_slick();

extern Widget form;

extern XtAppContext app_context;


/*
 * 	actions
 */
XtActionsRec std_actions[] =
{
	{ "aQuitgame", (XtActionProc)quit_appl_action },
	{ "aAbort", (XtActionProc)gameover_action },
	{ "aNew", (XtActionProc)new_game },
	{ "aTogglewizard", (XtActionProc)toggle_wizard },
	{ "aToggleslick", (XtActionProc)toggle_slick }
};


/*
 * 	translations to activate the actions
 *	support both Alt- and Ctrl- key combinations
 */
static String std_trans =
	"Alt<Key>X:	aQuitgame()\n\
	 Alt<Key>Q:	aQuitgame()\n\
	 Alt<Key>A:	aAbort()\n\
	 Alt<Key>N:	aNew()\n\
	 Alt<Key>W:	aTogglewizard()\n\
	 Alt<Key>S:	aToggleslick()\n\
	 Ctrl<Key>X:	aQuitgame()\n\
	 Ctrl<Key>Q:	aQuitgame()\n\
	 Ctrl<Key>A:	aAbort()\n\
	 Ctrl<Key>N:	aNew()\n\
	 Ctrl<Key>W:	aTogglewizard()\n\
	 Ctrl<Key>S:	aToggleslick()";

/* 
 *	functions
 */
void init_actions(void)
{
	DEBUG("actions.c","init_actions")

	XtAppAddActions(app_context, std_actions, XtNumber(std_actions));

	XtOverrideTranslations(form, 
			XtParseTranslationTable(std_trans));
}

