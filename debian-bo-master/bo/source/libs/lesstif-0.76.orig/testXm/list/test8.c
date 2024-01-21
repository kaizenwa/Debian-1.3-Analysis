/* test how lesstif handles a list with no elements -- DEFECT 1*/

#include <stdio.h>
#include <Xm/List.h>

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, NULL, NULL);

    listw = XtVaCreateManagedWidget( "test_of_list",
				    xmListWidgetClass, toplevel,
				    NULL );

    XtRealizeWidget(toplevel);

    XtAppMainLoop(app);
    exit(0);
}
