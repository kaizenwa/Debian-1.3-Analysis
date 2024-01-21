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

/* $Id: text.c,v 1.4 1996/04/19 08:52:58 torsten Exp $ */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Text.h>
#include "misc.h"
#include "protocol.h"
#include "text.h"

typedef struct {
    Widget shell, widget, okButton;
    TextPromptInfo *prmps;
    XtCallbackProc okFunc, cancelFunc;
    void *closure;
    char *name;
    Widget form, curFocus;
    int current;
} LocalInfo;

typedef struct tup {
    Widget parent, shell;
    LocalInfo *info;
    char *name;
    struct tup *next;
} textUpList;

static textUpList *head = NULL;

static void 
removeTextUp(char *nm)
{
    textUpList *cur = head, **pp = &head;

    while (cur != NULL && cur->name != nm) {
	pp = &cur->next;
	cur = cur->next;
    }
    if (cur != NULL) {
	*pp = cur->next;
	XtFree((XtPointer) cur);
    }
}

static void 
cancelCallback(Widget junkW, XtPointer lArg, XtPointer junk)
{
    LocalInfo *l = (LocalInfo *) lArg;
    XtCallbackProc uf = l->cancelFunc;
    Widget w = l->widget;
    void *c = l->closure;
    TextPromptInfo *p = l->prmps;

    removeTextUp(l->name);

    XtDestroyWidget(l->shell);
    XtFree((XtPointer) l);

    if (uf != NULL)
	uf(w, c, (XtPointer) p);
}

static void 
okCallback(Widget junkW, XtPointer lArg, XtPointer junk)
{
    LocalInfo *l = (LocalInfo *) lArg;
    XtCallbackProc uf = l->okFunc;
    Widget w = l->widget;
    void *c = l->closure;
    TextPromptInfo *p = l->prmps;
    int i;

    for (i = 0; i < p->nprompt; i++)
	XtVaGetValues(p->prompts[i].w, XtNstring, &p->prompts[i].rstr, NULL);

    if (uf != NULL)
	uf(w, c, (XtPointer) p);

    removeTextUp(l->name);

    XtDestroyWidget(l->shell);
    XtFree((XtPointer) l);
}

static void 
setFocus(LocalInfo * l)
{
    Widget w = l->prmps->prompts[l->current].w;

    if (w != l->curFocus) {
	if (l->curFocus != None)
	    XtVaSetValues(l->curFocus, XtNdisplayCaret, False, NULL);

	XtSetKeyboardFocus(l->form, w);
	XtVaSetValues(w, XtNdisplayCaret, True, NULL);
	l->curFocus = w;
    }
}

static void 
prevField(Widget w, XEvent * event, String * prms, Cardinal * nprms)
{
    Widget shell = GetShell(w);
    textUpList *cur;
    LocalInfo *l;

    for (cur = head; cur != NULL; cur = cur->next)
	if (cur->shell == shell)
	    break;
    if (cur == NULL)
	return;

    l = cur->info;

    if (l->current == 0) {
	l->current = l->prmps->nprompt;
    }
    l->current--;
    setFocus(l);
}
static void 
nextField(Widget w, XEvent * event, String * prms, Cardinal * nprms)
{
    Widget shell = GetShell(w);
    textUpList *cur;
    LocalInfo *l;

    for (cur = head; cur != NULL; cur = cur->next)
	if (cur->shell == shell)
	    break;
    if (cur == NULL)
	return;

    l = cur->info;

    if (l->prmps->nprompt == ++l->current) {
	if (*nprms != 0 && strcmp(prms[0], "True") == 0) {
	    XtCallActionProc(l->okButton, "set", NULL, NULL, 0);
	    XtCallActionProc(l->okButton, "notify", NULL, NULL, 0);
	    /*
	       okCallback(l->okButton, (XtPointer)l, NULL);
	     */
	} else {
	    l->current = 0;
	    setFocus(l);
	}
    } else {
	setFocus(l);
    }
}

static void 
buttonCB(Widget w, LocalInfo * l, XButtonEvent * event, XtPointer junk)
{
    int i;

    for (i = 0; i < l->prmps->nprompt; i++)
	if (l->prmps->prompts[i].w == w)
	    break;
    l->current = i;

    setFocus(l);
}

static void 
textPrompt(Widget w, char *name, TextPromptInfo * prompt,
   XtCallbackProc okProc, XtCallbackProc nokProc, void *data, Pixmap pix)
{
    int i;
    Position x, y;
    static String textAccelerators = "#override\n\
				<Key>Return: set() notify() unset()\n\
				<Key>Linefeed: set() notify() unset()";
    static String textTranslations = "#override\n\
					 Shift<Key>Tab: prev-typin(False)\n\
					 <Key>Tab: next-typin(False)\n\
					 <Key>Return: next-typin(True)\n\
					 <Key>Linefeed: next-typin(True)\n\
					 Ctrl<Key>M: next-typin(True)\n\
					 Ctrl<Key>J: next-typin(True)\n";
    static XtActionsRec act[2] =
    {
	{"next-typin", (XtActionProc) nextField},
	{"prev-typin", (XtActionProc) prevField},
    };
    static XtTranslations trans = None, toglt = None;
    static XtAccelerators accel;
    Widget labelList[64];
    Widget shell, form, title, text = None, last, label;
    Widget okButton, cancelButton, wshell, figure;
    LocalInfo *l;
    textUpList *cur;
    Dimension width, maxWidth = 0;

    for (cur = head; cur != NULL; cur = cur->next) {
	if (strcmp(cur->name, name) == 0 && cur->parent == w) {
	    XtPopup(cur->shell, XtGrabNone);
	    return;
	}
    }

    cur = XtNew(textUpList);
    cur->next = head;
    cur->name = name;
    cur->parent = w;
    head = cur;

    l = XtNew(LocalInfo);
    l->name = cur->name;

    cur->info = l;

    if (trans == None) {
	XtAppAddActions(XtWidgetToApplicationContext(w), act, 2);
	trans = XtParseTranslationTable(textTranslations);
	accel = XtParseAcceleratorTable(textAccelerators);
	toglt = XtParseTranslationTable("<BtnDown>,<BtnUp>: set() notify()");
    }
    wshell = GetShell(w);
    if (!XtIsRealized(wshell))
	wshell = GetToplevel(w);
    XtVaGetValues(wshell, XtNx, &x, XtNy, &y, NULL);

    shell = XtVaCreatePopupShell(name == NULL ? "popup-dialog" : name,
				 transientShellWidgetClass, wshell,
				 XtNx, x + 24,
				 XtNy, y + 24,
				 NULL);
    cur->shell = shell;

    form = XtVaCreateManagedWidget("popup-dialog-form",
				   formWidgetClass, shell,
				   XtNborderWidth, 0,
				   NULL);

    title = XtVaCreateManagedWidget("title",
				    labelWidgetClass, form,
				    XtNlabel, prompt->title,
				    XtNborderWidth, 0,
				    NULL);

    last = title;

    if (pix != None) {
	int wd, h;

	GetPixmapWHD(XtDisplay(w), pix, &wd, &h, NULL);
	figure = XtVaCreateManagedWidget("figure",
					 coreWidgetClass, form,
					 XtNfromVert, last,
					 XtNborderWidth, 4,
			 XtNborderColor, BlackPixelOfScreen(XtScreen(w)),
					 XtNwidth, wd,
					 XtNheight, h,
					 XtNbackgroundPixmap, pix,
					 NULL);
	last = figure;
    }
    for (i = 0; i < prompt->nprompt; i++) {
	label = XtVaCreateManagedWidget("label",
					labelWidgetClass, form,
					XtNfromVert, last,
				     XtNlabel, prompt->prompts[i].prompt,
					XtNborderWidth, 0,
					XtNleft, XtChainLeft,
					XtNright, XtChainLeft,
					NULL);
	labelList[i] = label;
	XtVaGetValues(label, XtNwidth, &width, NULL);
	if (width > maxWidth)
	    maxWidth = width;

	text = XtVaCreateManagedWidget("text",
				       asciiTextWidgetClass, form,
				       XtNfromHoriz, label,
				       XtNfromVert, last,
				       XtNeditType, XawtextEdit,
				       XtNwrap, XawtextWrapNever,
				       XtNresize, XawtextResizeWidth,
				       XtNtranslations, trans,
				       XtNlength, prompt->prompts[i].len,
				       XtNstring, prompt->prompts[i].str,
		       XtNinsertPosition, strlen(prompt->prompts[i].str),
				       XtNleft, XtChainLeft,
				       XtNdisplayCaret, False,
				       NULL);
	XtAddEventHandler(text, ButtonPressMask, False,
			  (XtEventHandler) buttonCB, (XtPointer) l);

	last = text;
	prompt->prompts[i].w = text;
    }
    for (i = 0; i < prompt->nprompt; i++)
	XtVaSetValues(labelList[i], XtNwidth, maxWidth, NULL);

    okButton = XtVaCreateManagedWidget("ok",
				       commandWidgetClass, form,
				       XtNfromVert, last,
				       XtNaccelerators, accel,
				       NULL);
    cancelButton = XtVaCreateManagedWidget("cancel",
					   commandWidgetClass, form,
					   XtNfromVert, last,
					   XtNfromHoriz, okButton,
					   NULL);


    l->widget = w;
    l->curFocus = None;
    l->shell = shell;
    l->prmps = prompt;
    l->closure = data;
    l->okFunc = okProc;
    l->current = 0;
    l->cancelFunc = nokProc;
    l->okButton = okButton;
    l->form = form;

#if 0
    if (prompt->nprompt == 1) {
	XtSetKeyboardFocus(form, text);
	XtInstallAccelerators(text, okButton);
    }
#endif
    setFocus(l);

    XtAddCallback(okButton, XtNcallback, okCallback, (XtPointer) l);
    XtAddCallback(cancelButton, XtNcallback, cancelCallback, (XtPointer) l);
    AddDestroyCallback(shell,
		       (DestroyCallbackFunc) cancelCallback, (XtPointer) l);

    XtPopup(shell, XtGrabNone);
}

void 
TextPrompt(Widget w, char *name, TextPromptInfo * prompt,
	   XtCallbackProc okProc, XtCallbackProc nokProc, void *data)
{
    textPrompt(w, name, prompt, okProc, nokProc, data, None);
}

#if 0
void 
TextPromptPixmap(Widget w, char *name, TextPromptInfo * prompt,
	       XtCallbackProc okProc, XtCallbackProc nokProc, void *data,
		 Pixmap pix)
{
    textPrompt(w, name, prompt, okProc, nokProc, data, pix);
    XFreePixmap(XtDisplay(w), pix);
}

#endif
