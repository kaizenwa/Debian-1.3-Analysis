/*
============
!Resource file: Test-prog
*XmMainWindow.width: 850
*XmMainWindow.height: 330
============
 */

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/RepType.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>

static Widget top;
static XtAppContext app_context;
static Widget main_window;
static Widget menubar;


int
main(int argc, char *argv[]) {
  Widget rc, frame1, label;

  XtSetLanguageProc(NULL, NULL, NULL);
  
  top = XtVaAppInitialize(&app_context,
                          "Test-prog",
                          NULL, 0,
                          &argc, argv,
                          NULL,
                          NULL);
  
  main_window = 
    XtVaCreateManagedWidget("mainwin", xmMainWindowWidgetClass, top, NULL);
  
  XmRepTypeInstallTearOffModelConverter();
  
  menubar = XmCreateMenuBar(main_window, "menubar", NULL, 0);
  XtManageChild(menubar);

  XmMainWindowSetAreas(main_window,menubar,NULL,NULL,NULL,NULL);

  rc = XtVaCreateWidget("rc", xmRowColumnWidgetClass, main_window, NULL);

  XtVaSetValues(main_window, XmNworkWindow, rc, NULL);
  XtManageChild(rc);

  frame1 = XtVaCreateWidget("frame1",
                                   xmFrameWidgetClass,
                                   rc,
                                   NULL);
  XtManageChild(frame1);

  XtRealizeWidget(top);
  
  label = XtVaCreateManagedWidget("Rampaging Bulldozers",
                                         xmLabelWidgetClass,
                                         frame1,
                                         NULL);

  XtAppMainLoop(app_context);
}
