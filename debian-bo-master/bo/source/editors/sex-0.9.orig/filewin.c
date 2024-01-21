/*
 * File:	filewin.c
 * Purpose:	Implement the "enter filename" dialog box.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: filewin.c,v 1.12 1996/12/22 20:06:11 liw Exp $"
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

#include <publib.h>

#include "filewin.h"
#include "cmd.h"
#include "win.h"
#include "x.h"
#include "error.h"



/*
 * Structure:	filewin
 * Purpose:	Descriptor for the "enter filename" dialog box.
 * Fields:	win	the window this dialog is attached to
 *		shell	the top level widget for the dialog
 *		search_text, replace_text
 *			the asciiTextWidgets that contain the text to
 *			be search and its replacement
 */
struct filewin {
	struct win *win;
	Widget shell;
	Widget filename_text;
	cmd_function *ok_hook;
	unsigned positioned:1;
};



/*
 * Variable:	fw_list
 * Purpose:	List of all existing filewin's.
 * Description:	This array contains pointers to all the filewin's that
 *		currently exist.  It is used to implement WM_DELETE protocol
 *		for filewin's.
 */

static struct dynarr fw_list;
static int init = 0;



/*
 * Prototypes for local functions.
 */

static void delete_fw(Widget);
static void cancel_filename(Widget, XtPointer, XtPointer);
static void ok_filename(Widget, XtPointer, XtPointer);
static void clear_filename(Widget, XtPointer, XtPointer);



/*
A filename window looks approximately like this:

<<<Arbitrary text>>>
Enter filename:   _____________________________

[Ok]  [Clear]  [Cancel]
*/
int filewin_create(struct filewin **fw, struct win *win, char *msgstr,
char *promptstr, cmd_function *ok_hook) {
	Widget form;
	Widget msg;
	Widget label;
	Widget ok_button;
	Widget cancel_button;
	Widget clear_button;
	XtTranslations trans;
	const int buttondist = 10;
	struct filewin **list;

	if (!init) {
		dynarr_init(&fw_list, sizeof(struct filewin *));
		init = 1;
	}

	if (dynarr_resize(&fw_list, fw_list.used + 1) == -1) {
		error(win, "Error: Couldn't create new filewin (out of memory?");
		return -1;
	}

	*fw = malloc(sizeof(struct filewin));
	if (*fw == NULL) {
		error(win, "out of memory creating dialog box");
		return -1;
	}

	list = fw_list.data;
	list[fw_list.used] = *fw;
	++fw_list.used;

	(*fw)->win = win;
	(*fw)->ok_hook = ok_hook;
	(*fw)->positioned = 0;

	(*fw)->shell = XtVaCreatePopupShell("filewin",
		transientShellWidgetClass, win_toplevel(win), 
		XtNallowShellResize, True,
		XtNinput, True,
		NULL);

	form = XtVaCreateManagedWidget(
		"form",
		formWidgetClass,
		(*fw)->shell,
		NULL);

	msg = XtVaCreateManagedWidget(
		"msg",
		labelWidgetClass,
		form,
		XtNlabel, msgstr,
		XtNborderWidth, 0,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	label = XtVaCreateManagedWidget(
		"label",
		labelWidgetClass,
		form,
		XtNlabel, promptstr,
		XtNborderWidth, 0,
		XtNfromVert, msg,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	(*fw)->filename_text = XtVaCreateManagedWidget(
		"filenametext",
		asciiTextWidgetClass,
		form,
		XtNeditType, XawtextEdit,
		XtNresizable, True,
		XtNresize, XawtextResizeWidth,
		XtNwidth, 200,
		XtNfromVert, msg,
		XtNfromHoriz, label,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainRight,
		NULL);

	ok_button = XtVaCreateManagedWidget(
		"ok",
		commandWidgetClass,
		form,
		XtNlabel, "Ok",
		XtNfromVert, label,
		XtNvertDistance, buttondist,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	clear_button = XtVaCreateManagedWidget(
		"clear",
		commandWidgetClass,
		form,
		XtNlabel, "Clear",
		XtNfromVert, label,
		XtNvertDistance, buttondist,
		XtNfromHoriz, ok_button,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	cancel_button = XtVaCreateManagedWidget(
		"cancel",
		commandWidgetClass,
		form,
		XtNlabel, "Cancel",
		XtNfromVert, label,
		XtNvertDistance, buttondist,
		XtNfromHoriz, clear_button,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	XtAddCallback(ok_button, XtNcallback, ok_filename, *fw);
	XtAddCallback(clear_button, XtNcallback, clear_filename, *fw);
	XtAddCallback(cancel_button, XtNcallback, cancel_filename, *fw);

	trans = XtParseTranslationTable(
		"<Key>Return: filename_ok() \n"
		"<Key>Escape: filename_cancel() \n"
		"Ctrl<Key>R: no-op() \n"
		"Ctrl<Key>S: no-op() \n");
	XtOverrideTranslations((*fw)->filename_text, trans);

	XtRealizeWidget((*fw)->shell);
	x_set_wm_protocols((*fw)->shell, delete_fw);
	XtSetKeyboardFocus((*fw)->shell, (*fw)->filename_text);

	return 0;
}



void filewin_destroy(struct filewin *fw) {
	XtDestroyWidget(fw->shell);
}



/*
 * Function:	filewin_set_filename
 * Purpose:	Set the filename in the editing widget.
 * Arguments:	fw	the dialog box
 *		name	the new name
 * Return:	Nothing.
 */
void filewin_set_filename(struct filewin *fw, char *name) {
	XtVaSetValues(fw->filename_text, XtNstring, name, NULL);
}



/*
 * Function:	filewin_popup
 * Purpose:	Pop up a "enter filename" dialog box.
 * Arguments:	fw	the dialog box
 * Return:	Nothing.
 */
void filewin_popup(struct filewin *fw) {
	Position x, y;
	Dimension width, height;
	Widget top;

	if (!fw->positioned) {
		top = win_toplevel(fw->win);
		XtVaGetValues(top, XtNwidth, &width, XtNheight, &height, 
			NULL);
		XtTranslateCoords(top, (Position) 30, (Position) 30, &x, &y);
		XtVaSetValues(fw->shell, XtNx, x, XtNy, y, NULL);
		fw->positioned = 1;
	}

	XtPopup(fw->shell, XtGrabNone);
}



/*
 * Function:	filewin_popdown
 * Purpose:	Close a "enter filename" dialog box.
 * Arguments:	fw	the dialog
 * Return:	Nothing.
 */
void filewin_popdown(struct filewin *fw) {
	XtPopdown(fw->shell);
}



/*
 * Function:	filewin_contains
 * Purpose:	Check whether a dialog contains a widget.
 * Arguments:	sw	the dialog
 *		wid	the widget
 * Return:	True or false.
 */
int filewin_contains(struct filewin *fw, Widget wid) {
	return fw->shell == wid || fw->filename_text == wid;
}



/*
 * Function:	filewin_filename
 * Purpose:	Return the current filename.
 * Arguments:	fw	the dialog box
 * Return:	Pointer to the string.
 */
char *filewin_filename(struct filewin *fw) {
	String str;

	XtVaGetValues(fw->filename_text, XtNstring, &str, NULL);
	return (char *) str;
}



/*
 * Function:	filewin_ok
 * Purpose:	Pop down the dialog and do the "Ok" action.
 * Arguments:	fw	the dialog box
 * Return:	-1 if associated command failed, 0 if it succeeded.
 */
int filewin_ok(struct filewin *fw) {
	filewin_popdown(fw);
	if (fw->ok_hook == NULL)
		return 0;
	return fw->ok_hook(fw->win);
}



/*
 * Function:	filewin_find
 * Purpose:	Return filewin handle given its shell widget.
 * Arguments:	wid	the widget
 * Return:	Pointer to the filewin descriptor, NULL if not found.
 */
struct filewin *filewin_find(Widget wid) {
	int i;
	struct filewin **list;

	list = fw_list.data;
	for (i = 0; i < fw_list.used; ++i)
		if (filewin_contains(list[i], wid))
			return list[i];
	return NULL;
}



/***********************************************************************
 * Local functions follow.
 */



/*
 * Function:	delete_fw
 * Purpose:	React to WM_DELETE by closing the dialog box.
 * Arguments:	wid	the widget that got WM_DELETE
 * Return:	Nothing.
 */
static void delete_fw(Widget wid) {
	struct filewin *fw;

	fw = filewin_find(wid);
	if (fw != NULL)
		filewin_popdown(fw);
}


/*
 * Pop down a search window.
 */
static void cancel_filename(Widget w, XtPointer client_data, XtPointer foo) {
	filewin_popdown((struct filewin *) client_data);
}


/*
 * Done entering the filename.
 */
static void ok_filename(Widget w, XtPointer client_data, XtPointer foo) {
	struct filewin *fw;

	fw = client_data;
	if (fw != NULL)
		filewin_ok(fw);
	win_update_all();
}


/*
 * Clear the filename.
 */
static void clear_filename(Widget w, XtPointer client_data, XtPointer foo) {
	struct filewin *fw;

	fw = client_data;
	if (fw != NULL)
		XtVaSetValues(fw->filename_text, XtNstring, "", NULL);
}
