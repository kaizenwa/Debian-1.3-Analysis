
/*-
# X-BASED SKEWB
#
#  Skewb.h
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

/* Public header file for Skewb */

#ifndef _XtSkewb_h
#define _XtSkewb_h

/***********************************************************************
 *
 * Skewb Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNorient "orient"
#define XtNmono "mono"
#define XtNface "face"
#define XtNpos "position"
#define XtNdirection "direction"
#define XtNpractice "practice"
#define XtNstart "start"
#define XtNpieceBorder "pieceBorder"
#define XtNfaceColor0 "faceColor0"
#define XtNfaceColor1 "faceColor1"
#define XtNfaceColor2 "faceColor2"
#define XtNfaceColor3 "faceColor3"
#define XtNfaceColor4 "faceColor4"
#define XtNfaceColor5 "faceColor5"
#define XtCUserName "UserName"
#define XtCOrient "Orient"
#define XtCMono "Mono"
#define XtCFace "Face"
#define XtCPos "Position"
#define XtCDirection "Direction"

#define SKEWB_RESTORE (-4)
#define SKEWB_RESET (-3)
#define SKEWB_ILLEGAL (-1)
#define SKEWB_IGNORE (-1)
#define SKEWB_MOVED 0
#define SKEWB_CONTROL 1
#define SKEWB_SOLVED 2
#define SKEWB_PRACTICE 3
#define SKEWB_RANDOMIZE 4
#define SKEWB_DEC 5
#define SKEWB_ORIENT 6
#define SKEWB_INC 7
#define SKEWB_COMPUTED 8
#define SKEWB_UNDO 9

typedef struct _SkewbClassRec *SkewbWidgetClass;
typedef struct _SkewbRec *SkewbWidget;

extern WidgetClass skewbWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
	int         face, position, direction;
} skewbCallbackStruct;

#endif /* _XtSkewb_h */
/* DON'T ADD STUFF AFTER THIS #endif */
