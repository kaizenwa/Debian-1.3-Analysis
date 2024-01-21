
/*-
# X-BASED OCTAHEDRON
#
#  Oct.h
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

/* Public header file for Oct */

#ifndef _XtOct_h
#define _XtOct_h

/***********************************************************************
 *
 * Oct Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNmono "mono"
#define XtNsize "size"
#define XtNsticky "sticky"
#define XtNmode "mode"
#define XtNorient "orient"
#define XtNpractice "practice"
#define XtNstart "start"
#define XtNpieceBorder "pieceBorder"
#define XtNfaceColor0 "faceColor0"
#define XtNfaceColor1 "faceColor1"
#define XtNfaceColor2 "faceColor2"
#define XtNfaceColor3 "faceColor3"
#define XtNfaceColor4 "faceColor4"
#define XtNfaceColor5 "faceColor5"
#define XtNfaceColor6 "faceColor6"
#define XtNfaceColor7 "faceColor7"
#define XtCUserName "UserName"
#define XtCMono "Mono"
#define XtCSize "Size"
#define XtCSticky "Sticky"
#define XtCMode "Mode"
#define XtCOrient "Orient"
#define XtCPractice "Practice"
#define OCT_RESTORE (-4)
#define OCT_RESET (-3)
#define OCT_AMBIGUOUS (-2)
#define OCT_ILLEGAL (-1)
#define OCT_MOVED 0
#define OCT_CONTROL 1
#define OCT_SOLVED 2
#define OCT_PRACTICE 3
#define OCT_RANDOMIZE 4
#define OCT_DEC 5
#define OCT_ORIENT 6
#define OCT_INC 7
#define OCT_PERIOD3 8
#define OCT_PERIOD4 9
#define OCT_BOTH 10
#define OCT_STICKY 11
#define OCT_COMPUTED 12
#define OCT_UNDO 13

typedef struct _OctClassRec *OctWidgetClass;
typedef struct _OctRec *OctWidget;

extern WidgetClass octWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
} octCallbackStruct;

#endif /* _XtOct_h */
/* DON'T ADD STUFF AFTER THIS #endif */
