#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>
#include <stdio.h>

void cb(Widget w, XtPointer client, XtPointer call)
{
  XmString *st1, *st2, *st3, t1, t2, test;
  int c1, c2, c3, i;
  XmFileSelectionBoxWidget box = (XmFileSelectionBoxWidget)w;
  char *txt;

  XtVaGetValues((Widget)box, XmNdirListItems, &st1, XmNdirListItemCount, &c1, NULL);
  XtVaGetValues((Widget)box, XmNfileListItems, &st2, XmNfileListItemCount, &c2, NULL);
  XtVaGetValues((Widget)box, XmNlistItems, &st3, XmNlistItemCount, &c3, NULL);
  printf("c1 c2 c3 %d %d %d\n", c1, c2, c3);
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
  }
  t2 = NULL;

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNpattern, &t1, XmNtextString, &t2, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("pattern: %s\n", txt);
  if (t2) {
    XmStringGetLtoR(t2, XmFONTLIST_DEFAULT_TAG, &txt);
    printf("text: %s\n", txt);
  } else
    printf("text - problem !\n");

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNdirectory, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("directory: %s\n", txt);

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNdirMask, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirMask: %s\n", txt);

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNdirSpec, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirSpec: %s\n", txt);

  test = XmStringCreateSimple("test it out");

  t1 = NULL; txt = NULL;

  XtVaSetValues((Widget)box, XmNtextString, test, NULL);
  XtVaGetValues((Widget)box, XmNdirSpec, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirSpec: %s\n", txt);
}

void
pushme(Widget w, XtPointer client, XtPointer call) {
  XtManageChild(client);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box, push;


  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  push = XmCreatePushButton(toplevel, "push", NULL, 0);
  XtVaSetValues(push,
		XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

  box = XmCreateFileSelectionDialog(toplevel, "Box", NULL, 0);
  XtAddCallback(box, XmNokCallback, cb, NULL);

  XtAddCallback(push, XmNactivateCallback, pushme, box);

  XtManageChild(push);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
