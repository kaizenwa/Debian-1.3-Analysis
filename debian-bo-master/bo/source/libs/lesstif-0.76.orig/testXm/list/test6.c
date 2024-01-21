#include <stdio.h>
#include <Xm/List.h>

char *days[] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
		     "Friday", "Saturday" };

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    XmStringTable str_days;
    int i;
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, NULL, NULL);

    str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
    for(i=0; i<7; ++i)
	str_days[i] = XmStringCreateSimple(days[i]);

    listw = XmCreateScrolledList(toplevel, "test_of_scrolled_list", NULL, 0);

    XtVaSetValues(listw, XmNitemCount, 7, XmNvisibleItemCount, 5,
		  XmNtopItemPosition, 2, XmNitems, str_days, 
                  XmNselectionPolicy, XmBROWSE_SELECT,
                  NULL);

    XtManageChild(listw);

    for(i=0; i<7; ++i)
	XmStringFree(str_days[i]);
    XtFree((XtPointer)str_days);
  
    XtRealizeWidget(toplevel);

    XtAppMainLoop(app);

    exit(0);
}
