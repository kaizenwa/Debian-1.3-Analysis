
/*-
# X-BASED MASTERBALL(tm)
#
#  Mball.h
#
###
#
#  Copyright (c) 1994 - 97	David Albert Bagley, bagleyd@bigfoot.com
#
#                   All Rights Reserved
#
#  Permission to use, copy, modify, and distribute this software and
#  its documentation for any purpose and without fee is hereby granted,
#  provided that the above copyright notice appear in all copies and
#  that both that copyright notice and this permission notice appear in
#  supporting documentation, and that the name of the author not be
#  used in advertising or publicity pertaining to distribution of the
#  software without specific, written prior permission.
#
#  This program is distributed in the hope that it will be "playable",
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
*/

/* Public header file for Mball */

#ifndef _XtMball_h
#define _XtMball_h

/***********************************************************************
 *
 * Mball Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNmono "mono"
#define XtNwedges "wedges"
#define XtNrings "rings"
#define XtNorient "orient"
#define XtNpractice "practice"
#define XtNstart "start"
#define XtNpieceBorder "pieceBorder"
#define XtNwedgeColor0 "wedgeColor0"
#define XtNwedgeColor1 "wedgeColor1"
#define XtNwedgeColor2 "wedgeColor2"
#define XtNwedgeColor3 "wedgeColor3"
#define XtNwedgeColor4 "wedgeColor4"
#define XtNwedgeColor5 "wedgeColor5"
#define XtNwedgeColor6 "wedgeColor6"
#define XtNwedgeColor7 "wedgeColor7"
#define XtCUserName "UserName"
#define XtCMono "Mono"
#define XtCWedges "Wedges"
#define XtCRings "Rings"
#define XtCOrient "Orient"
#define XtCPractice "Practice"
#define MBALL_RESTORE (-4)
#define MBALL_RESET (-3)
#define MBALL_AMBIGUOUS (-2)
#define MBALL_ILLEGAL (-1)
#define MBALL_MOVED 0
#define MBALL_CONTROL 1
#define MBALL_SOLVED 2
#define MBALL_PRACTICE 3
#define MBALL_RANDOMIZE 4
#define MBALL_DEC 5
#define MBALL_ORIENT 6
#define MBALL_INC 7
#define MBALL_WEDGE2 8
#define MBALL_WEDGE4 9
#define MBALL_WEDGE6 10
#define MBALL_WEDGE8 11
#define MBALL_COMPUTED 12
#define MBALL_UNDO 13

typedef struct _MballClassRec *MballWidgetClass;
typedef struct _MballRec *MballWidget;

extern WidgetClass mballWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
} mballCallbackStruct;

#endif /* _XtMball_h */
/* DON'T ADD STUFF AFTER THIS #endif */
