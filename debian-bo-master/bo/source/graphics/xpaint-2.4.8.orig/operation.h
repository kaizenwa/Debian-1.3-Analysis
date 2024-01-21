/* $Id: operation.h,v 1.1 1996/02/07 10:32:18 torsten Exp $ */

/* operation.c */
void OperationSelectCallAcross(int n);
void OperationSet(String names[], int num);
void OperationSetPaint(Widget paint);
void OperationAddArg(Arg arg);
void OperationInit(Widget toplevel);
#ifdef __IMAGE_H__
Image *OperationIconImage(Widget w, char *name);
#endif
