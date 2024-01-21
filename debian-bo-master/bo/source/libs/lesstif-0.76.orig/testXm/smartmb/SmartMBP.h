/* $Id: SmartMBP.h,v 1.1.1.1 1996/08/06 05:52:57 toshok Exp $ */
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

#ifndef _SmartMBP_h
#define _SmartMBP_h

#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>
#include "SmartMB.h"

typedef struct _XmSmartMessageBoxClassPart
{
	int blammo;
} XmSmartMessageBoxClassPart;

typedef struct _XmSmartMessageBoxClassRec
{
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ConstraintClassPart constraint_class;
	XmManagerClassPart manager_class;
	XmBulletinBoardClassPart bulletin_board_class;
	XmSmartMessageBoxClassPart smart_message_box_class;
} XmSmartMessageBoxClassRec, *XmSmartMessageBoxWidgetClass;

extern XmSmartMessageBoxClassRec xwriMessageBoxClassRec;

typedef struct _XmSmartMessageBoxPart
{
	XtPointer data;
	Boolean minimizeButtons;
	unsigned char dialogType;
	DialogPositioning dialogPositioning;
	
	Pixmap pixmap;

	Widget labelW;
	Widget controlW;
	Widget separatorW;
} XmSmartMessageBoxPart;

typedef struct _XmSmartMessageBoxRec
{
	CorePart core;
	CompositePart	 composite;
	ConstraintPart constraint;
	XmManagerPart manager;
	XmBulletinBoardPart bulletin_board;
	XmSmartMessageBoxPart smart_message_box;
} XmSmartMessageBoxRec, *XmSmartMessageBoxPtr;

#ifndef NEED_BBCONSTRAINT
#define NEED_BBCONSTRAINT	1
#endif
#if NEED_BBCONSTRAINT
typedef struct _XmBulletinBoardConstraintPart
{
	char blammo;
} XmBulletinBoardConstraintPart;
#endif /* NEED_BBCONSTRAINT */


typedef struct _XmSmartMessageBoxConstraintPart
{
	unsigned char childType;
} XmSmartMessageBoxConstraintPart;

typedef struct _XmSmartMessageBoxConstraintRec
{
	XmManagerConstraintPart manager;
	XmBulletinBoardConstraintPart bulletin_board;
	XmSmartMessageBoxConstraintPart smart_message_box;
}	XmSmartMessageBoxConstraintRec, *XmSmartMessageBoxConstraintPtr;

#endif /* _SmartMBP_h */
