/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)            | */
/* |                                                                   | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.  There is no           | */
/* | representations about the suitability of this software for        | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.                                              | */
/* |                                                                   | */
/* +-------------------------------------------------------------------+ */

/* $Id: menu.c,v 1.3 1996/04/19 08:53:40 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Form.h>
#include <stdio.h>
#include "menu.h"

#include "bitmaps/checkmark.xbm"
#include "bitmaps/pullright.xbm"

#define MAX_GROUPS	4

typedef struct group_s {
    int size;
    Widget group[16];
    struct group_s *next;
} GroupList;

typedef struct rightlist_s {
    Widget widget;
    Widget shell;
    struct rightlist_s *next;
} RightList;

static GroupList *groupHead = NULL;
static RightList *rightHead = NULL;
static Pixmap checkBitmap = None, pullBitmap = None;

static void pullRightAction(Widget, XEvent *, String *, Cardinal *);

static GroupList *
findGroup(Widget w)
{
    GroupList *cur = groupHead;
    int i;

    if (w == None)
	return NULL;

    while (cur != NULL) {
	for (i = 0; i < cur->size; i++)
	    if (cur->group[i] == w)
		return cur;
	cur = cur->next;
    }
    return NULL;
}

static void 
initMenu(Widget w)
{
    static XtActionsRec act =
    {"popup-menu-pullright", (XtActionProc) pullRightAction};

    if (checkBitmap != None)
	return;

    checkBitmap = XCreateBitmapFromData(XtDisplay(w),
					DefaultRootWindow(XtDisplay(w)),
					(char *) checkmark_bits,
				      checkmark_width, checkmark_height);
    pullBitmap = XCreateBitmapFromData(XtDisplay(w),
				       DefaultRootWindow(XtDisplay(w)),
				       (char *) pullright_bits,
				       pullright_width, pullright_height);

    XtAppAddActions(XtWidgetToApplicationContext(w), &act, 1);
}

static void 
destroy(Widget w, XtPointer data, XtPointer junk)
{
    XtFree((XtPointer) data);
}

static void 
pullRightAction(Widget w, XEvent * event, String * params, Cardinal * nparams)
{
    Dimension width, height;
    int x, y;
    RightList *cur;
    Widget cw = XawSimpleMenuGetActiveEntry(w);

    if (cw == None || event->type != MotionNotify)
	return;

    x = event->xmotion.x;
    y = event->xmotion.y;

    XtVaGetValues(w, XtNwidth, &width, XtNheight, &height, NULL);
    if (x < 0 || x >= width || y < 0 || y >= height)
	return;

    /*
    **  Only the second half of the menu is sensitive to pulls
     */
    if (x < width / 2)
	return;

    for (cur = rightHead; cur != NULL && cur->widget != cw; cur = cur->next);

    if (cur == NULL)
	return;

    x = event->xmotion.x_root - 20;
    y = event->xmotion.y_root - 20;
    XtVaSetValues(cur->shell, XtNx, x, XtNy, y, NULL);

    XtPopup(cur->shell, XtGrabExclusive);
}

static void 
createItem(Widget parent, PaintMenuItem * item,
	   Widget groups[MAX_GROUPS])
{
    int grp = -1;
    Widget entry;

    if (item->name[0] == '\0') {
	entry = XtVaCreateManagedWidget("seperator",
					smeLineObjectClass, parent,
					NULL);
    } else if (item->flags & MF_CHECK) {
	entry = XtVaCreateManagedWidget(item->name,
					smeBSBObjectClass, parent,
				      XtNleftMargin, checkmark_width + 4,
					NULL);
    } else if (item->right != NULL && item->nright != 0) {
	static XtTranslations trans = None, trans2 = None;
	int i;
	RightList *cur;
	String nm;

	if (trans == None) {
	    trans = XtParseTranslationTable(
		      "<BtnMotion>: highlight() popup-menu-pullright()");
	    trans2 = XtParseTranslationTable(
		      "<LeaveWindow>: unhighlight() MenuPopdown()\n"
		      "<BtnUp>: notify() unhighlight() MenuPopdown()\n"
		      "<BtnMotion>: highlight()");
	}
	entry = XtVaCreateManagedWidget(item->name,
					smeBSBObjectClass, parent,
					XtNrightMargin, pullright_width,
					XtNrightBitmap, pullBitmap,
					NULL);

	nm = (String) XtMalloc(strlen(XtName(XtParent(parent))) +
			       strlen(item->name) + 16);
	sprintf(nm, "%s-%s-right", XtName(XtParent(parent)), item->name);

	item->rightShell = XtVaCreatePopupShell(nm,
					   simpleMenuWidgetClass, parent,
						XtNtranslations, trans2,
						NULL);
	XtAddCallback(item->rightShell, XtNdestroyCallback, destroy, (XtPointer) nm);
	XtOverrideTranslations(parent, trans);

	for (i = 0; i < item->nright; i++)
	    createItem(item->rightShell, &item->right[i], groups);

	cur = XtNew(RightList);
	cur->shell = item->rightShell;
	cur->widget = entry;
	cur->next = rightHead;
	rightHead = cur;
    } else {
	entry = XtVaCreateManagedWidget(item->name,
					smeBSBObjectClass, parent,
					NULL);
    }

    if (item->flags & MF_GROUP1)
	grp = 0;
    else if (item->flags & MF_GROUP2)
	grp = 1;
    else if (item->flags & MF_GROUP3)
	grp = 2;
    else if (item->flags & MF_GROUP4)
	grp = 3;

    if (grp != -1) {
	GroupList *c = findGroup(groups[grp]);

	if (c == NULL) {
	    /* XXX GroupList entry leaked */
	    c = XtNew(GroupList);
	    c->next = groupHead;
	    groupHead = c;
	    c->size = 0;
	    groups[grp] = entry;
	}
	c->group[c->size++] = entry;
    }
    if ((item->flags & MF_CHECKON) == MF_CHECKON)
	XtVaSetValues(entry, XtNleftBitmap, checkBitmap, NULL);

    if (item->callback != NULL)
	XtAddCallback(entry, XtNcallback,
		      (XtCallbackProc) item->callback, item->data);
    item->widget = entry;
}

Widget
MenuBarCreate(Widget parent, int nbar, PaintMenuBar bar[])
{
    int list, item;
    Widget button = None, menu;
    Widget prevButton = None;
    char menuPopupName[80];
    int i;
    Widget groups[MAX_GROUPS];

    initMenu(parent);

    /*
    **  If there is more than one entry in this bar
    **    reparent it.
     */
    if (nbar > 1)
	parent = XtVaCreateManagedWidget("menu",
					 formWidgetClass, parent,
					 XtNborderWidth, 0,
					 XtNbottom, XtChainTop,
					 XtNright, XtChainLeft,
					 XtNleft, XtChainLeft,
					 NULL);

    for (list = 0; list < nbar; list++) {
	char *nm;
	strcpy(menuPopupName, bar[list].name);
	strcat(menuPopupName, "Menu");

	nm = XtNewString(menuPopupName);

	button = XtVaCreateManagedWidget(bar[list].name,
					 menuButtonWidgetClass, parent,
					 XtNmenuName, nm,
					 XtNfromHoriz, prevButton,
					 NULL);
	XtAddCallback(button, XtNdestroyCallback, destroy, (XtPointer) nm);
	prevButton = button;

	menu = XtCreatePopupShell(menuPopupName,
				  simpleMenuWidgetClass, button,
				  NULL, 0);

	bar[list].widget = menu;

	for (i = 0; i < MAX_GROUPS; i++)
	    groups[i] = None;

	for (item = 0; item < bar[list].nitems; item++)
	    createItem(menu, &bar[list].items[item], groups);
    }

    return (nbar > 1) ? parent : button;
}

Widget
MenuPopupCreate(Widget parent, int nitems, PaintMenuItem items[])
{
    static XtTranslations trans = None;
    int i;
    Widget menu;
    Widget groups[MAX_GROUPS];

    for (i = 0; i < MAX_GROUPS; i++)
	groups[i] = None;

    initMenu(parent);

    menu = XtCreatePopupShell("popup-menu",
			      simpleMenuWidgetClass, parent,
			      NULL, 0);

    if (trans == None)
	trans = XtParseTranslationTable(
	"<Btn3Down>: XawPositionSimpleMenu(popup-menu) MenuPopup(popup-menu)");

    XtOverrideTranslations(parent, trans);

    for (i = 0; i < nitems; i++)
	createItem(menu, &items[i], groups);

    return menu;
}

void 
MenuCheckItem(Widget w, Boolean flag)
{
    GroupList *c;

    if (w == None)
	return;

    if ((c = findGroup(w)) != NULL) {
	int i;
	for (i = 0; i < c->size; i++)
	    XtVaSetValues(c->group[i], XtNleftBitmap, None, NULL);
    }
    XtVaSetValues(w, XtNleftBitmap, flag ? checkBitmap : None, NULL);
}
