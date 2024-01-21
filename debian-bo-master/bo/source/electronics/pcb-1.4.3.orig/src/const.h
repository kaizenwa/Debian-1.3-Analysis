/*
 *                            COPYRIGHT
 *
 *  PCB, interactive printed circuit board design
 *  Copyright (C) 1994,1995,1996 Thomas Nau
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Contact addresses for paper mail and Email:
 *  Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
 *  Thomas.Nau@rz.uni-ulm.de
 *
 *  RCS: $Id: const.h,v 143.1 1996/09/16 09:08:29 nau Exp $
 */

/* global source constants
 */

#ifndef	__CONST_INCLUDED__
#define	__CONST_INCLUDED__

#include <limits.h>
#include <X11/cursorfont.h>

#include "../globalconst.h"

/* ---------------------------------------------------------------------------
 * the default X cursor shape for the output window
 */
#define	DEFAULT_CURSORSHAPE		XC_crosshair

/* ---------------------------------------------------------------------------
 * the layer-numbers of the two additional special layers
 * 'component' and 'solder'. The offset of MAX_LAYER is not added
 */
#define	SOLDER_LAYER		0
#define	COMPONENT_LAYER		1

/* ---------------------------------------------------------------------------
 * misc constants
 */
#define	MARK_SIZE				50				/* mark size of elements */
#define	UNDO_WARNING_SIZE		(100*1024)		/* warning limit of undo */
#define	USERMEDIANAME			"user defined"	/* label of default media */

/* ---------------------------------------------------------------------------
 * some math constants
 */
#ifndef	M_PI
#define	M_PI					3.14159265358979323846
#endif
#define	M180					(M_PI/180.0)
#define	TAN_22_5_DEGREE_2		0.207106781		/* 0.5*tan(22.5) */
#define	TAN_30_DEGREE			0.577350269		/* tan(30) */
#define	TAN_60_DEGREE			1.732050808		/* tan(60) */

/* ---------------------------------------------------------------------------
 * modes
 */
#define	NO_MODE					0	/* no mode selected */
#define	VIA_MODE				1	/* draw vias */
#define	LINE_MODE				2	/* draw lines */
#define	RECTANGLE_MODE			3	/* create rectangles */
#define	POLYGON_MODE			4	/* draw filled polygons */
#define	PASTEBUFFER_MODE		5	/* paste objects from buffer */
#define	TEXT_MODE				6	/* create text objects */
#define	MIRROR_MODE				101	/* move objects */
#define	ROTATE_MODE				102	/* rotate objects */
#define	REMOVE_MODE				103	/* remove objects */
#define	MOVE_MODE				104	/* move objects */
#define	COPY_MODE				105	/* copy objects */
#define	INSERTPOINT_MODE		106	/* insert point into line/polygon */
#define	RUBBERBANDMOVE_MODE		107	/* move objects and attached lines */

/* ---------------------------------------------------------------------------
 * object flags
 */
#define	OBJ_FLAGS			0x07ef		/* all used flags */
#define	NOFLAG				0x0000
#define	PINFLAG				0x0001		/* is a pin */
#define	VIAFLAG				0x0002		/* is a via */
#define	FOUNDFLAG			0x0004		/* was used by 'FindConnection()' */
#define	MIRRORFLAG			0x0008		/* use mirroring for text */
#define	DISPLAYNAMEFLAG		0x0020		/* display the names of pins/pads */
										/* of an element */
#define	SELECTEDFLAG		0x0040		/* object has been selected */
#define	ONSOLDERFLAG		0x0080		/* element is on bottom side */
#define	SQUAREFLAG			0x0100		/* pin is square, not round */
#define	PINONEFLAG			0x0200		/* marks the first pin of an element */
#define	ONGROUNDPLANEFLAG	0x0400		/* pin/via/pad is on ground plane */

/* ---------------------------------------------------------------------------
 * PCB flags
 */
#define	PCB_FLAGS			0x01e0		/* all used flags */
#define	DESCRIPTIONFLAG		0x0020		/* display description of elements */
#define	NAMEONPCBFLAG		0x0040		/* display name of an element */
#define	ABSOLUTEFLAG		0x0080		/* grid is relative to (0,0) */
#define	ALLDIRCETIONFLAG	0x0100		/* enable 'all-direction' lines */

/* ---------------------------------------------------------------------------
 * object types
 */
#define	NO_TYPE					0x0000		/* no object */
#define	VIA_TYPE				0x0001
#define	ELEMENT_TYPE			0x0002
#define	LINE_TYPE				0x0004
#define	POLYGON_TYPE			0x0008
#define	TEXT_TYPE				0x0010

#define	PIN_TYPE				0x0100		/* objects that are part */
#define	PAD_TYPE				0x0200		/* 'pin' of SMD element */
#define	ELEMENTNAME_TYPE		0x0400		/* of others */
#define	POLYGONPOINT_TYPE		0x0800
#define	LINEPOINT_TYPE			0x1000

#define	ALL_TYPES				-1			/* all bits set */

/* ---------------------------------------------------------------------------
 * define some standard layouts for childs of a form widget
 */
#define	LAYOUT_TOP			XtNleft, XtChainLeft,	\
							XtNright, XtChainLeft,	\
							XtNtop, XtChainTop,		\
							XtNbottom, XtChainTop
#define	LAYOUT_BOTTOM		XtNleft, XtChainLeft,	\
							XtNright, XtChainLeft,	\
							XtNtop, XtChainBottom,	\
							XtNbottom, XtChainBottom
#define	LAYOUT_BOTTOM_RIGHT	XtNleft, XtChainLeft,	\
							XtNright, XtChainRight,	\
							XtNtop, XtChainBottom,	\
							XtNbottom, XtChainBottom
#define	LAYOUT_LEFT			XtNleft, XtChainLeft,	\
							XtNright, XtChainLeft,	\
							XtNtop, XtChainTop,		\
							XtNbottom, XtChainBottom
#define	LAYOUT_NORMAL		XtNleft, XtChainLeft,	\
							XtNright, XtChainRight,	\
							XtNtop, XtChainTop,		\
							XtNbottom, XtChainBottom

#endif
