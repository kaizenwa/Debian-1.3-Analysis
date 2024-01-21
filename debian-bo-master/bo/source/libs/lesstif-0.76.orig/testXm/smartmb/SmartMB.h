/* $Id: SmartMB.h,v 1.1.1.1 1996/08/06 05:52:57 toshok Exp $ */
/*
 * Copyright 1994 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.    John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose. It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * Wolfram Research, Inc be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 *  John L. Cwikla
 *  X Programmer
 *  Wolfram Research Inc.
 *
 *  cwikla@wri.com
*/


#ifndef _SmartMB_h
#define _SmartMB_h

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>

extern WidgetClass xmSmartMessageBoxWidgetClass;

typedef struct _XmSmartMessageBoxRec *XmSmartMessageBoxWidget;
typedef struct _XmSmartMessageBoxConstraintRec *XmSmartMessageBoxConstraint;

#ifndef XmIsSmartMessageBox
#define XmIsSmartMessageBox(a) (XtIsSubclass(a, xmSmartMessageBoxWidgetClass))
#endif

#define XmRSMBChildType "SMBChildType"

#define XmNdata "data"
#define XmCData "Data"

#define XmCHILD_ACTION 1
#define XmCHILD_CONTROL 2
#define XmCHILD_LABEL 3
#define XmCHILD_SEPARATOR 4

#define XmDIALOG_POSITIONING_LEAVE_ALONE 0
#define XmDIALOG_POSITIONING_INITIAL_CENTER 1
#define XmDIALOG_POSITIONING_ALWAYS_CENTER 2
#define XmDIALOG_POSITIONING_DEFAULT_AT_POINTER 3
#define XmDIALOG_POSITIONING_CENTER_AT_POINTER 4

#define XmNdialogPositioning "dialogPositioning"
#define XmCDialogPositioning "DialogPositioning"
#define XmRDialogPositioning "DialogPositioning"

typedef unsigned char DialogPositioning;


#ifdef _NO_PROTO
Widget XmCreateSmartMessageBox();
#else
Widget XmCreateSmartMessageBox(Widget _parent, char *_name, ArgList _warg, Cardinal _numWarg);
#endif /* _NO_PROTO */

#endif /* _SmartMB_h */
