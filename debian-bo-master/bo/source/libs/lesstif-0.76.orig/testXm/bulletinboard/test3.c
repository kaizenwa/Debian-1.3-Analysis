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
  Position x,y;
  Widget c1, c2;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmBulletinBoardWidgetClass, 
                                toplevel,
				XmNwidth, 100, XmNheight, 100,
				XmNallowOverlap, False,
				NULL);

  c1 = XtVaCreateManagedWidget("test1",
			      xmLabelWidgetClass,
			      one,
			      XmNx, 5, XmNy, 5,
			      NULL);

  c2 = XtVaCreateManagedWidget("test2",
			      xmLabelWidgetClass,
			      one,
			      XmNx, 20, XmNy, 20,
			      NULL);

#ifdef NEED_EDITRES
  XtAddEventHandler(toplevel, (EventMask)0, True,
                    (XtEventHandler)_XEditResCheckMessages, NULL);
#endif

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNshadowThickness, &thick, NULL);
  printf("shadow thickness: %d\n", thick);
  XtVaGetValues(c1, XmNx, &x, XmNy, &y, NULL);
  printf("label 1 x,y: %d %d\n", x, y);
  XtVaGetValues(c2, XmNx, &x, XmNy, &y, NULL);
  printf("label 2 x,y: %d %d\n", x, y);

  XtAppMainLoop(app);

  exit(0);
}
