/*
 * File:	searchwin.c
 * Purpose:	Implement the "search and replace" dialog box.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: searchwin.c,v 1.17 1996/12/22 20:06:12 liw Exp $"
 */


#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Toggle.h>

#include <publib.h>

#include "searchwin.h"
#include "cmd.h"
#include "win.h"
#include "x.h"
#include "error.h"


/*
 * Structure:	searchwin
 * Purpose:	Descriptor for the "search and replace" dialog box.
 * Fields:	win	the window this dialog is attached to
 *		shell	the top level widget for the dialog
 *		search_text, replace_text
 *			the asciiTextWidgets that contain the text to
 *			be search and its replacement
 */
struct searchwin {
	struct win *win;
	Widget shell;
	Widget backwards, case_insensitive, regex;
	Widget search_text, replace_text;
	unsigned positioned:1;
};



/*
 * Prototypes for local functions.
 */

static void delete_sw(Widget);
static void cancel_search(Widget, XtPointer, XtPointer);
static void clear_search(Widget, XtPointer, XtPointer);
static void search_next(Widget, XtPointer, XtPointer);
static void replace_and_search(Widget, XtPointer, XtPointer);
static void replace_once(Widget, XtPointer, XtPointer);
static void replace_all(Widget, XtPointer, XtPointer);



/*
A search window looks approximately like this:

Search for:   _____________________________
Replace with: _____________________________

Options: [backward] [case insensitive] [regex]

[Search]  [Replace+search]  [Replace all]  [Reset]  [Dismiss]
*/
int searchwin_create(struct searchwin **sw, struct win *win) {
	Widget form;
	Widget search_label;
	Widget search_button;
	Widget replace_label;
	Widget replace_button;
	Widget replace_once_button;
	Widget replace_all_button;
	Widget cancel_button;
	Widget clear_button;
	Widget options_label;
	XtTranslations trans;
	const int buttondist = 10;

	*sw = malloc(sizeof(struct searchwin));
	if (*sw == NULL) {
		error(win, "out of memory creating search window");
		return -1;
	}

	(*sw)->win = win;
	(*sw)->positioned = 0;

	(*sw)->shell = XtVaCreatePopupShell("searchwin",
		transientShellWidgetClass, win_toplevel(win), 
		XtNallowShellResize, True,
		XtNinput, True,
		NULL);

	form = XtVaCreateManagedWidget(
		"form",
		formWidgetClass,
		(*sw)->shell,
		NULL);

	search_label = XtVaCreateManagedWidget(
		"searchlabel",
		labelWidgetClass,
		form,
		XtNlabel, "Search for:   ",
		XtNborderWidth, 0,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	(*sw)->search_text = XtVaCreateManagedWidget(
		"searchtext",
		asciiTextWidgetClass,
		form,
		XtNeditType, XawtextEdit,
		XtNresizable, True,
		XtNresize, XawtextResizeWidth,
		XtNwidth, 400,
		XtNfromHoriz, search_label,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainRight,
		NULL);

	replace_label = XtVaCreateManagedWidget(
		"replacelabel",
		labelWidgetClass,
		form,
		XtNlabel, "Replace with: ",
		XtNfromVert, (*sw)->search_text,
		XtNborderWidth, 0,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	(*sw)->replace_text = XtVaCreateManagedWidget(
		"replacetext",
		asciiTextWidgetClass,
		form,
		XtNeditType, XawtextEdit,
		XtNresizable, True,
		XtNresize, XawtextResizeWidth,
		XtNwidth, 400,
		XtNfromHoriz, replace_label,
		XtNfromVert, (*sw)->search_text,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainRight,
		NULL);
		
	options_label = XtVaCreateManagedWidget(
		"optionslabel",
		labelWidgetClass,
		form,
		XtNlabel, "Options: ",
		XtNfromVert, (*sw)->replace_text,
		XtNborderWidth, 0,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);
		
	(*sw)->backwards = XtVaCreateManagedWidget(
		"backwardsbutton",
		toggleWidgetClass,
		form,
		XtNstate, False,
		XtNlabel, "Backwards",
		XtNfromVert, (*sw)->replace_text,
		XtNfromHoriz, options_label,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	(*sw)->case_insensitive = XtVaCreateManagedWidget(
		"casebutton",
		toggleWidgetClass,
		form,
		XtNstate, False,
		XtNlabel, "Case insensitive",
		XtNfromVert, (*sw)->replace_text,
		XtNfromHoriz, (*sw)->backwards,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	(*sw)->regex = XtVaCreateManagedWidget(
		"regexbutton",
		toggleWidgetClass,
		form,
		XtNstate, False,
		XtNlabel, "Regex",
		XtNfromVert, (*sw)->replace_text,
		XtNfromHoriz, (*sw)->case_insensitive,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	search_button = XtVaCreateManagedWidget(
		"searchbutton",
		commandWidgetClass,
		form,
		XtNlabel, "Search next",
		XtNfromVert, options_label,
		XtNvertDistance, buttondist,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	replace_button = XtVaCreateManagedWidget(
		"replacebutton",
		commandWidgetClass,
		form,
		XtNlabel, "Replace+search",
		XtNfromVert, options_label,
		XtNvertDistance, buttondist,
		XtNfromHoriz, search_button,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	replace_once_button = XtVaCreateManagedWidget(
		"replaceoncebutton",
		commandWidgetClass,
		form,
		XtNlabel, "Replace this",
		XtNfromVert, options_label,
		XtNvertDistance, buttondist,
		XtNfromHoriz, replace_button,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	replace_all_button = XtVaCreateManagedWidget(
		"replaceallbutton",
		commandWidgetClass,
		form,
		XtNlabel, "Replace all",
		XtNfromVert, options_label,
		XtNvertDistance, buttondist,
		XtNfromHoriz, replace_once_button,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	clear_button = XtVaCreateManagedWidget(
		"clearbutton",
		commandWidgetClass,
		form,
		XtNlabel, "Reset",
		XtNfromVert, options_label,
		XtNvertDistance, buttondist,
		XtNfromHoriz, replace_all_button,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	cancel_button = XtVaCreateManagedWidget(
		"cancelbutton",
		commandWidgetClass,
		form,
		XtNlabel, "Dismiss",
		XtNfromVert, options_label,
		XtNvertDistance, buttondist,
		XtNfromHoriz, clear_button,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	XtAddCallback(search_button, XtNcallback, search_next, win);
	XtAddCallback(replace_button, XtNcallback, replace_and_search, win);
	XtAddCallback(replace_once_button, XtNcallback, replace_once, win);
	XtAddCallback(replace_all_button, XtNcallback, replace_all, win);
	XtAddCallback(clear_button, XtNcallback, clear_search, *sw);
	XtAddCallback(cancel_button, XtNcallback, cancel_search, *sw);

	trans = XtParseTranslationTable(
		"<Key>Return: find(next) \n"
		"<Key>Escape: find(cancel) \n"
		"None<Key>Tab: focus_replace() \n"
		"Shift<Key>Tab: focus_search() \n"
		"Ctrl<Key>R: no-op() \n"
		"Ctrl<Key>S: no-op() \n"
		"<BtnDown>,<BtnUp>: focus_search() \n");
	XtOverrideTranslations((*sw)->search_text, trans);

	trans = XtParseTranslationTable(
		"<Key>Return: find(replace+find) \n"
		"<Key>Escape: find(cancel) \n"
		"None<Key>Tab: focus_search() \n"
		"Shift<Key>Tab: focus_replace() \n"
		"Ctrl<Key>R: no-op() \n"
		"Ctrl<Key>S: no-op() \n"
		"<BtnDown>,<BtnUp>: focus_replace() \n");
	XtOverrideTranslations((*sw)->replace_text, trans);

	XtRealizeWidget((*sw)->shell);
	x_set_wm_protocols((*sw)->shell, delete_sw);

	return 0;
}


/*
 * Function:	searchwin_destroy
 * Purpose:	Destroy a searchwin.
 */
void searchwin_destroy(struct searchwin *sw) {
	XtDestroyWidget(sw->shell);
}


/*
 * Function:	searchwin_focus
 * Purpose:	Set focus inside a searchwin to a given part
 * Arguments:	sw	the search window
 *		part	identifier for the part:
 *			0 = search_text
 *			1 = replace_text
 * Return:	Nothing.
 */
void searchwin_focus(struct searchwin *sw, int part) {
	Widget focus, nonfocus;

	if (part == 0) {
		focus = sw->search_text;
		nonfocus = sw->replace_text;
	} else {
		focus = sw->replace_text;
		nonfocus = sw->search_text;
	}
	XtSetKeyboardFocus(sw->shell, focus);
	XtVaSetValues(focus, XtNdisplayCaret, True, NULL);
	XtVaSetValues(nonfocus, XtNdisplayCaret, False, NULL);
}



/*
 * Function:	searchwin_popup
 * Purpose:	Pop up a "search and replace" dialog box.
 * Arguments:	sw	the "search and replace" dialog box
 * Return:	Nothing.
 */
void searchwin_popup(struct searchwin *sw) {
	Position x, y;
	Dimension width, height;
	Widget top;

	if (!sw->positioned) {
		top = win_toplevel(sw->win);
		XtVaGetValues(top, XtNwidth, &width, XtNheight, &height, NULL);
		XtTranslateCoords(top, (Position) 30, (Position) 30, &x, &y);
		XtVaSetValues(sw->shell, XtNx, x, XtNy, y, NULL);
		sw->positioned = 1;
	}
	searchwin_focus(sw, 0);
	XtPopup(sw->shell, XtGrabNone);
}



/*
 * Function:	searchwin_popdown
 * Purpose:	Close a "search and replace" dialog box.
 * Arguments:	sw	the dialog
 * Return:	Nothing.
 */
void searchwin_popdown(struct searchwin *sw) {
	XtPopdown(sw->shell);
}



/*
 * Function:	searchwin_contains
 * Purpose:	Check whether a dialog contains a widget.
 * Arguments:	sw	the dialog
 *		wid	the widget
 * Return:	True or false.
 */
int searchwin_contains(struct searchwin *sw, Widget wid) {
	return sw->shell == wid || 
		sw->search_text == wid || 
		sw->replace_text == wid;
}



/*
 * Function:	searchwin_search_text
 * Purpose:	Return the current text to be searched.
 * Arguments:	sw	the dialog box
 * Return:	Pointer to the string.
 */
char *searchwin_search_text(struct searchwin *sw) {
	String str;

	XtVaGetValues(sw->search_text, XtNstring, &str, NULL);
	return (char *) str;
}



/*
 * Function:	searchwin_replace_text
 * Purpose:	Return the current replacement text.
 * Arguments:	sw	the dialog box
 * Return:	Pointer to the string.
 */
char *searchwin_replace_text(struct searchwin *sw) {
	String str;

	XtVaGetValues(sw->replace_text, XtNstring, &str, NULL);
	return (char *) str;
}



/*
 * Function:	searchwin_options
 * Purpose:	Return the current sbuf_search options.
 * Arguments:	sw	the dialog box
 * Return:	The sbuf_search options argument.
 */
unsigned long searchwin_options(struct searchwin *sw) {
	unsigned long options;
	Boolean value;

	options = 0;

	XtVaGetValues(sw->backwards, XtNstate, &value, NULL);
	if (value)
		options |= SBUF_BACKWARD;
		
	XtVaGetValues(sw->case_insensitive, XtNstate, &value, NULL);
	if (value)
		options |= SBUF_IGNORE_CASE;

	XtVaGetValues(sw->regex, XtNstate, &value, NULL);
	if (value)
		options |= SBUF_REGEX;

	return options;
}



/*
 * Function:	searchwin_set_search
 * Purpose:	Set the current text to be searched.
 * Arguments:	sw	the dialog box
 *		str	the new pattern
 * Return:	Nothing.
 */
void searchwin_set_search(struct searchwin *sw, char *str) {
	XtVaSetValues(sw->search_text, XtNstring, str, NULL);
}



/*
 * Function:	searchwin_set_replace
 * Purpose:	Set the current replacement text.
 * Arguments:	sw	the dialog box
 *		str	the new text
 * Return:	Nothing.
 */
void searchwin_set_replace(struct searchwin *sw, char *str) {
	XtVaSetValues(sw->replace_text, XtNstring, str, NULL);
}



/***********************************************************************
 * Local functions follow.
 */



/*
 * Function:	delete_sw
 * Purpose:	React to WM_DELETE by closing the dialog box.
 * Arguments:	wid	the widget that got WM_DELETE
 * Return:	Nothing.
 */
static void delete_sw(Widget wid) {
	struct win *win;

	win = win_find(wid);
	if (win != NULL)
		searchwin_popdown(win_searchwin(win));
	else
		error(NULL, "can't find window to close");
}


/*
 * Pop down a search window.
 */
static void cancel_search(Widget w, XtPointer client_data, XtPointer foo) {
	searchwin_popdown((struct searchwin *) client_data);
}


/*
 * Reset search window.
 */
static void clear_search(Widget w, XtPointer client_data, XtPointer foo) {
	struct searchwin *sw = (struct searchwin *) client_data;

	searchwin_set_search(sw, "");
	searchwin_set_replace(sw, "");
	XtVaSetValues(sw->backwards, XtNstate, False, NULL);
	XtVaSetValues(sw->backwards, XtNstate, False, NULL);
	XtVaSetValues(sw->case_insensitive, XtNstate, False, NULL);
	XtVaSetValues(sw->regex, XtNstate, False, NULL);
}


/*
 * Search next.
 */
static void search_next(Widget w, XtPointer client_data, XtPointer foo) {
	(void) cmd_search_next((struct win *) client_data);
	win_update_all();
}


/*
 * Replace this and search next.
 */
static void replace_and_search(Widget w, XtPointer client_data, XtPointer foo) {
	(void) cmd_replace_and_search((struct win *) client_data);
	win_update_all();
}


/*
 * Replace current selection.
 */
static void replace_once(Widget w, XtPointer client_data, XtPointer foo) {
	(void) cmd_replace_once((struct win *) client_data);
	win_update_all();
}


/*
 * Search and replace all.
 */
static void replace_all(Widget w, XtPointer client_data, XtPointer foo) {
	(void) cmd_search_and_replace_all((struct win *) client_data);
	win_update_all();
}
