/* test for bulletinboard */
#undef NEED_EDITRES

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/Label.h>
#ifdef NEED_EDITRES
#include <X11/Xmu/Editres.h>
#endif
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Dimension thick = 0;
  Widget c;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmBulletinBoardWidgetClass, 
                                toplevel,
				XmNallowOverlap, False,
				NULL);

  c = XtVaCreateManagedWidget("test1",
			      xmLabelWidgetClass,
			      one,
			      NULL);

  c = XtVaCreateManagedWidget("test2",
			      xmLabelWidgetClass,
			      one,
			      NULL);

#ifdef NEED_EDITRES
  XtAddEventHandler(toplevel, (EventMask)0, True,
                    (XtEventHandler)_XEditResCheckMessages, NULL);
#endif

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNshadowThickness, &thick, NULL);
  printf("shadow thickness: %d\n", thick);

  XtAppMainLoop(app);

  exit(0);
}
