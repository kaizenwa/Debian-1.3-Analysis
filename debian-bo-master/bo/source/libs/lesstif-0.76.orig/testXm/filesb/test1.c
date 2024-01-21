#include <Xm/Xm.h>
#include <Xm/FileSBP.h>
#include <stdio.h>

void
cb(Widget w, XtPointer a, XtPointer b) {
  XmString *st1 = NULL, *st2 = NULL, *st3 = NULL, t1, t2, test;
  int c1 = 0, c2 = 0, c3 = 0, i;
  XmFileSelectionBoxWidget box = (XmFileSelectionBoxWidget)w;
  char *txt;

  XtVaGetValues((Widget)box, XmNdirListItems, &st1, XmNdirListItemCount, &c1, NULL);
  XtVaGetValues((Widget)box, XmNfileListItems, &st2, XmNfileListItemCount, &c2, NULL);
  XtVaGetValues((Widget)box, XmNlistItems, &st3, XmNlistItemCount, &c3, NULL);
  printf("c1 c2 c3 %d %d %08x %p %p %p\n", c1, c2, c3, st1, st2, st3);
  for (i = 0; i < c1; i++) {
	XmStringGetLtoR(st1[i], XmFONTLIST_DEFAULT_TAG, &txt);
	printf("dirList[%d] = '%s'\n", i, txt);
  }
  for (i = 0; i < c2; i++) {
	XmStringGetLtoR(st2[i], XmFONTLIST_DEFAULT_TAG, &txt);
	printf("fileList[%d] = '%s'\n", i, txt);
  }
  for (i = 0; i < c3; i++) {
	XmStringGetLtoR(st3[i], XmFONTLIST_DEFAULT_TAG, &txt);
	printf("listList[%d] = '%s'\n", i, txt);
	fflush(stdout);
  }
  XtVaGetValues((Widget)box, XmNpattern, &t1, XmNtextString, &t2, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("pattern: %s\n", txt);
  XmStringGetLtoR(t2, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("text: %s\n", txt);
  XtVaGetValues((Widget)box, XmNdirectory, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("directory: %s\n", txt);
  XtVaGetValues((Widget)box, XmNdirMask, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirMask: %s\n", txt);
  XtVaGetValues((Widget)box, XmNdirSpec, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirSpec: %s\n", txt);

  test = XmStringCreateSimple("test it out");

  XtVaSetValues((Widget)box, XmNtextString, test, NULL);
  XtVaGetValues((Widget)box, XmNdirSpec, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirSpec: %s\n", txt);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  box = XtVaCreateManagedWidget("Box", xmFileSelectionBoxWidgetClass, toplevel,
				XmNdialogType, XmDIALOG_FILE_SELECTION, NULL);

  XtAddCallback(box, XmNokCallback, cb, NULL);

  XtManageChild(box);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
