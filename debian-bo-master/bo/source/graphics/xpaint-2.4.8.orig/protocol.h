/* $Id: protocol.h,v 1.2 1996/04/19 09:18:52 torsten Exp $ */

typedef void (*DestroyCallbackFunc) (Widget, void *, XEvent *);

/* protocol.c */
void AddDestroyCallback(Widget w, DestroyCallbackFunc func, void *data);
void StateSetBusyWatch(Boolean flag);
void StateSetBusy(Boolean flag);
void StateShellBusy(Widget w, Boolean flag);
void StateAddParent(Widget w, Widget parent);
void StateTimeStep(void);
