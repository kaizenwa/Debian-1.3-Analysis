/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module Xaw-help.c				     */
/*									     */
/*	Online help functions for the Athena Widget interface.		     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifdef ONLINE_HELP
#include "X-sok.h"
#include "Tableau.h"

#include <X11/Shell.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>

static int help_active = 0;
static Widget help, helppaned, helppanel, helptext, helpclose;
extern const char *keyfilename;	/* from X-widget.c */

static void selecttopic(Widget w, XtPointer number, XtPointer garbage) {
    char filename[200];
    const char *s = XtName(w);
    Arg Args[2];
    int i = atoi(s+4);

    sprintf(filename, "%s/%s/%s.help", xsokdir, langdir,
	    i ? rulepool[i-1] : keyfilename);
    XtSetArg(Args[0], XtNstring, filename);
    XtSetArg(Args[1], XtNtype, XawAsciiFile);
    XtSetValues(helptext, Args, 2);
}

void create_help(void) {
    Widget topicsmenu, topicsbutton, w;
    Arg Args[1];
    help         = XtCreatePopupShell("help", transientShellWidgetClass, toplevel, NULL, 0);
    helppaned    = XtCreateManagedWidget("helppaned",	panedWidgetClass,      help,	     NULL, ZERO);
    helppanel 	 = XtCreateManagedWidget("helppanel",	boxWidgetClass,        helppaned,    NULL, ZERO);
    helptext	 = XtCreateManagedWidget("helptext",	asciiTextWidgetClass,  helppaned,    NULL, ZERO);
    XtSetArg(Args[0], XtNmenuName, "topicsmenu");
    topicsbutton = XtCreateManagedWidget("Topic",       menuButtonWidgetClass, helppanel,    Args, 1);
    topicsmenu   = XtCreatePopupShell("topicsmenu",     simpleMenuWidgetClass, topicsbutton, NULL, ZERO);
    helpclose	 = XtCreateManagedWidget("Close Help",	commandWidgetClass,    helppanel,    NULL, ZERO);
    XtAddCallback(helpclose, XtNcallback, popdown_help, NULL);

    XtSetArg(Args[0], XtNlabel, TXT_HELP_KEYS);
    w = XtCreateManagedWidget("Help0", smeBSBObjectClass, topicsmenu, Args, 1);
    XtAddCallback(w, XtNcallback, selecttopic, NULL);
    {   const char **rp;
	for (rp = rulepool; *rp; ++rp) {
	    char n[8], s[40];
	    sprintf(n, "Help%d", rp-rulepool+1);
	    XtSetArg(Args[0], XtNlabel, s);
	    sprintf(s, TXT_HELP_RULES, *rp);
	    w = XtCreateManagedWidget(n, smeBSBObjectClass, topicsmenu, Args, 1);
	    XtAddCallback(w, XtNcallback, selecttopic, NULL);
	}
    }

}

void popup_help(void) {
    if (help_active)
	return;		/* request pending => deny another one */
    help_active = 1;
    XtPopup(help, XtGrabNone);
}

void popdown_help(Widget w, XtPointer a, XtPointer b) {
    if (!help_active)
	return;		/* request pending => deny another one */
    help_active = 0;
    XtPopdown(help);
}

#endif
