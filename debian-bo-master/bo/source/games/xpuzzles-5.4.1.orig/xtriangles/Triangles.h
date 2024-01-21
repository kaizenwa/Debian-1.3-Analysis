
/*-
# X-BASED TRIANGLES
#
#  Triangles.h
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

/* Public header file for Triangles */

#ifndef _XtTriangles_h
#define _XtTriangles_h

/***********************************************************************
 *
 * Triangles Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNsize "size"
#define XtNbase "base"
#define XtNstart "start"
#define XtNtileColor "tileColor"
#define XtNtileBorder "tileBorder"
#define XtCUserName "UserName"
#define XtCSize "Size"
#define XtCBase "Base"
#define TRIANGLES_RESTORE (-5)
#define TRIANGLES_RESET (-4)
#define TRIANGLES_BLOCKED (-3)
#define TRIANGLES_SPACE (-2)
#define TRIANGLES_IGNORE (-1)
#define TRIANGLES_MOVED 0
#define TRIANGLES_SOLVED 1
#define TRIANGLES_PRACTICE 2
#define TRIANGLES_RANDOMIZE 3
#define TRIANGLES_DEC 4
#define TRIANGLES_INC 5
#define TRIANGLES_COMPUTED 6
#define TRIANGLES_UNDO 7

typedef struct _TrianglesClassRec *TrianglesWidgetClass;
typedef struct _TrianglesRec *TrianglesWidget;

extern WidgetClass trianglesWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
} trianglesCallbackStruct;

#endif /* _XtTriangles_h */
/* DON'T ADD STUFF AFTER THIS #endif */
