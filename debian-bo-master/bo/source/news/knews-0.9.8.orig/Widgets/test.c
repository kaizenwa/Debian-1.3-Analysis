#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Editres.h>
#include "Menu.h"
#include "MenuG.h"
#include "MenuKnapp.h"
#include "MenuShell.h"
#include "PullRight.h"
#include "StringG.h"
#include "ToggleG.h"

#undef	NULL
#define	NULL ((void *)0)

#define MAX_DEPTH	4

static void callback(Widget w, XtPointer client, XtPointer call)
{
    printf("CALLBACK: %s\n", XtName(w));
}

static void create_simple_menu(int depth, char *prefix, Widget parent)
{
    char	buf[16];
    Widget	shell, menu, temp;
    int		i;

    sprintf(buf, "%s_shell", prefix);
    shell = XtCreatePopupShell(buf, menuShellWidgetClass, parent, NULL, 0);
    sprintf(buf, "%s_menu", prefix);
    menu = XtCreateManagedWidget(buf, menuWidgetClass, shell, NULL, 0);

    for (i = 0 ; i < 8 ; i += 2) {
	sprintf(buf, "%s%d", prefix, i);
	temp = MenuCreateGadget(buf, toggleGadgetClass, menu, NULL, 0);
	XtAddCallback(temp, XtNcallback, callback, NULL);

	sprintf(buf, "%s%d", prefix, i + 1);
	if (depth == MAX_DEPTH)
	    temp = MenuCreateGadget(buf, toggleGadgetClass, menu, NULL, 0);
	else {
	    char	m_name[16];
	    Arg		arg;

	    sprintf(m_name, "%s_shell", buf);
	    XtSetArg(arg, XtNmenuName, m_name);
	    temp = MenuCreateGadget(buf, pullRightGadgetClass, menu, &arg, 1);
	    create_simple_menu(depth + 1, buf, shell);
	}
	XtAddCallback(temp, XtNpostPopdownCallback, callback, NULL);
    }
}

int main(int argc, char *argv[])
{
    XtAppContext	app_cont;
    Widget		shell, knapp;
    String		fallbacks[] = {
	"*background:	grey",
	NULL
    };

    shell = XtAppInitialize(&app_cont, "Test", NULL, 0, &argc, argv,
			    fallbacks, NULL, 0);
    if (!shell)
	return 1;

    XtAddEventHandler(shell, (EventMask)0, True, _XEditResCheckMessages, NULL);

    knapp = XtVaCreateManagedWidget("knapp", menuKnappWidgetClass, shell,
				    XtNmenuName, "1_shell", NULL);
    create_simple_menu(1, "1", knapp);

    XtRealizeWidget(shell);
    XtAppMainLoop(app_cont);

    return 0;
}
