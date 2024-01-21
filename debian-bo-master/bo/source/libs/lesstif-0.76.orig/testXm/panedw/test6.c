#include <Xm/Xm.h>
#include <Xm/PanedWP.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 
#include <XmI/XmI.h>

Widget toplevel, field, pane, button, button2, button3;

#define PANEBOUND 1

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  int i;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  pane = XtVaCreateManagedWidget("pane", xmPanedWindowWidgetClass,
                                 toplevel, NULL);

  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  pane, 
#if PANEBOUND
				   XmNpaneMinimum, 20,
				   XmNpaneMaximum, 20,
#endif
                                  NULL);  

#if 1
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
                                   pane, 
#if PANEBOUND
				   XmNpaneMinimum, 20,
				   XmNpaneMaximum, 40,
#endif
                                   NULL);

  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  button2 = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass,
                                   pane, 
#if PANEBOUND
				   XmNpaneMinimum, 20,
				   XmNpaneMaximum, 20,
#endif
                                   NULL);

  XtAddCallback(button2, XmNactivateCallback, activate_callback, NULL);

#if 1
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  button3 = XtVaCreateManagedWidget("button3", xmPushButtonWidgetClass,
                                   pane, 
#if PANEBOUND
				   XmNpaneMinimum, 20,
				   XmNpaneMaximum, 40,
#endif
                                   NULL);
  XtAddCallback(button3, XmNactivateCallback, activate_callback, NULL);
#endif

#endif

  XtRealizeWidget(toplevel);

#if 0
  printf("DUMP\n");
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  for (i = 0; i < PW_NumManagedChildren(pane); i++) {
    printf("child: %-8p %-10s ", PW_ManagedChildren(pane)[i],
	   XtName(PW_ManagedChildren(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(PW_ManagedChildren(pane)[i]),
	   PWC_Sash(PW_ManagedChildren(pane)[i]),
	   PWC_PositionIndex(PW_ManagedChildren(pane)[i]),
	   PWC_Position(PW_ManagedChildren(pane)[i]));
  }
  printf("ALL CHILDREN\n");
  for (i = 0; i < MGR_NumChildren(pane); i++) {
    printf("child: %-8p %-10s ", MGR_Children(pane)[i],
	   XtName(MGR_Children(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(MGR_Children(pane)[i]),
	   PWC_Sash(MGR_Children(pane)[i]),
	   PWC_PositionIndex(MGR_Children(pane)[i]),
	   PWC_Position(MGR_Children(pane)[i]));
  }
  printf("\n");

  printf("UNMANAGE\n");
  XtUnmanageChild(button);
  XtUnmanageChild(button2);
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  for (i = 0; i < PW_NumManagedChildren(pane); i++) {
    printf("child: %-8p %-10s ", PW_ManagedChildren(pane)[i],
	   XtName(PW_ManagedChildren(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(PW_ManagedChildren(pane)[i]),
	   PWC_Sash(PW_ManagedChildren(pane)[i]),
	   PWC_PositionIndex(PW_ManagedChildren(pane)[i]),
	   PWC_Position(PW_ManagedChildren(pane)[i]));
  }
  printf("ALL CHILDREN\n");
  for (i = 0; i < MGR_NumChildren(pane); i++) {
    printf("child: %-8p %-10s ", MGR_Children(pane)[i],
	   XtName(MGR_Children(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(MGR_Children(pane)[i]),
	   PWC_Sash(MGR_Children(pane)[i]),
	   PWC_PositionIndex(MGR_Children(pane)[i]),
	   PWC_Position(MGR_Children(pane)[i]));
  }
  printf("\n");

  printf("MANAGE\n");
  XtManageChild(button2);
  printf("sashinc: %d resizeAtRealize: %d\n",
	 PW_IncrementCount(pane), PW_ResizeAtRealize(pane));
  for (i = 0; i < PW_NumManagedChildren(pane); i++) {
    printf("child: %-8p %-10s ", PW_ManagedChildren(pane)[i],
	   XtName(PW_ManagedChildren(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(PW_ManagedChildren(pane)[i]),
	   PWC_Sash(PW_ManagedChildren(pane)[i]),
	   PWC_PositionIndex(PW_ManagedChildren(pane)[i]),
	   PWC_Position(PW_ManagedChildren(pane)[i]));
  }
  printf("ALL CHILDREN\n");
  for (i = 0; i < MGR_NumChildren(pane); i++) {
    printf("child: %-8p %-10s ", MGR_Children(pane)[i],
	   XtName(MGR_Children(pane)[i]));
    printf("sep: %-8p sash: %-8p index: %d position: %d\n",
	   PWC_Separator(MGR_Children(pane)[i]),
	   PWC_Sash(MGR_Children(pane)[i]),
	   PWC_PositionIndex(MGR_Children(pane)[i]),
	   PWC_Position(MGR_Children(pane)[i]));
  }
  printf("\n");
#endif

  XtAppMainLoop(app);

  exit(0);
}

