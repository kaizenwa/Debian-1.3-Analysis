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
 *  RCS: $Id: macro.h,v 143.1 1996/09/16 09:08:43 nau Exp $
 */

/* some commonly used macros not related to a special C-file
 * the file is included by global.h after const.h
 */

#ifndef	__MACRO_INCLUDED__
#define	__MACRO_INCLUDED__

/* ---------------------------------------------------------------------------
 * macros to transform coord systems
 * draw.c uses a different definition of TO_SCREEN
 */
#ifndef	SWAP_IDENT
#define	SWAP_IDENT			Settings.ShowSolderSide
#endif

#define	SWAP_SIGN_X(x)		(x)
#define	SWAP_SIGN_Y(y)		(-(y))
#define	SWAP_ANGLE(a)		(-(a))
#define	SWAP_DELTA(d)		(-(d))
#define	SWAP_X(x)			(SWAP_SIGN_X(x))
#define	SWAP_Y(y)			(PCB->MaxHeight +SWAP_SIGN_Y(y))

#ifndef	TO_SCREEN
#define	TO_SCREEN(x)		((x) >> PCB->Zoom)
#endif

#define	TO_SCREEN_X(x)		TO_SCREEN(SWAP_IDENT ? SWAP_X(x) : (x))
#define	TO_SCREEN_Y(y)		TO_SCREEN(SWAP_IDENT ? SWAP_Y(y) : (y))
#define	TO_SCREEN_ANGLE(a)	(SWAP_IDENT ? SWAP_ANGLE((a)) : (a))
#define	TO_SCREEN_DELTA(d)	(SWAP_IDENT ? SWAP_DELTA((d)) : (d))
#define	TO_SCREEN_SIGN_X(x)	(SWAP_IDENT ? SWAP_SIGN_X(x) : (x))
#define	TO_SCREEN_SIGN_Y(y)	(SWAP_IDENT ? SWAP_SIGN_Y(y) : (y))

#ifndef	TO_PCB
#define	TO_PCB(x)			((x) << PCB->Zoom)
#endif
#define	TO_PCB_X(x)			TO_PCB(x)
#define	TO_PCB_Y(y)			(SWAP_IDENT ? \
								PCB->MaxHeight -TO_PCB(y) : TO_PCB(y))

/* ---------------------------------------------------------------------------
 * misc macros, some might already be defined by <limits.h>
 */
#ifndef	MIN
#define	MIN(a,b)			((a) < (b) ? (a) : (b))
#define	MAX(a,b)			((a) > (b) ? (a) : (b))
#endif

#define	ENTRIES(x)			(sizeof((x))/sizeof((x)[0]))
#define	UNKNOWN(a)			((a) && *(a) ? (a) : "(unknown)")
#define	EMPTY(a)			((a) ? (a) : "")

/* ---------------------------------------------------------------------------
 * returns pointer to current layer or to n-th element on stack
 */
#define	LAYER_ON_STACK(n)	(&PCB->Data->Layer[LayerStack[(n)]])
#define	CURRENT				(LAYER_ON_STACK(0))
#define	INDEXOFCURRENT		LayerStack[0]

/* ---------------------------------------------------------------------------
 * returns the object ID
 */
#define	OBJECT_ID(p)		(((AnyObjectTypePtr) p)->ID)

/* ---------------------------------------------------------------------------
 * access macro for current buffer
 */
#define	PASTEBUFFER			(&Buffers[Settings.BufferNumber][Settings.ShowSolderSide ? 1 : 0])

/* ---------------------------------------------------------------------------
 * some routines for flag setting, clearing, changeing and testing
 */
#define	SET_FLAG(f,p)		((p)->Flags |= (f))
#define	CLEAR_FLAG(f,p)		((p)->Flags &= (~(f)))
#define	TEST_FLAG(f,p)		((p)->Flags & (f))
#define	TOGGLE_FLAG(f,p)	((p)->Flags = TEST_FLAG((f),(p)) ? (p)->Flags & (~(f)) : (p)->Flags | (f))
#define	ASSIGN_FLAG(f,v,p)	((p)->Flags = ((p)->Flags & (~(f))) | ((v) ? (f) : 0))

/* ---------------------------------------------------------------------------
 * access macros for elements name structure
 */
#define	DESCRIPTION_INDEX		0
#define	NAMEONPCB_INDEX		1
#define	VALUE_INDEX			2
#define	NAME_INDEX(p)		(TEST_FLAG(NAMEONPCBFLAG,(p)) ? NAMEONPCB_INDEX :\
								(TEST_FLAG(DESCRIPTIONFLAG, (p)) ?		\
									DESCRIPTION_INDEX : VALUE_INDEX))
#define	ELEMENT_NAME(p,e)	((e)->Name[NAME_INDEX((p))].TextString)
#define	DESCRIPTION_NAME(e)	((e)->Name[DESCRIPTION_INDEX].TextString)
#define	NAMEONPCB_NAME(e)	((e)->Name[NAMEONPCB_INDEX].TextString)
#define	VALUE_NAME(e)		((e)->Name[VALUE_INDEX].TextString)
#define	ELEMENT_TEXT(p,e)	((e)->Name[NAME_INDEX((p))])
#define	DESCRIPTION_TEXT(e)	((e)->Name[DESCRIPTION_INDEX])
#define	NAMEONPCB_TEXT(e)	((e)->Name[NAMEONPCB_INDEX])
#define	VALUE_TEXT(e)		((e)->Name[VALUE_INDEX])

/* ----------------------------------------------------------------------
 * checks for correct X values
 */
#define	VALID_PIXMAP(p)	((p) != BadValue && \
	(p) != BadAlloc && \
	(p) != BadDrawable)

#define	VALID_GC(p)	((p) != BadValue && \
	(p) != BadAlloc && \
	(p) != BadDrawable && \
	(p) != BadFont && \
	(p) != BadMatch && \
	(p) != BadPixmap)

/* ---------------------------------------------------------------------------
 * some loop shortcuts
 * all object loops run backwards to prevent from errors when deleting objects
 *
 * a pointer is created from index addressing because the base pointer
 * may change when new memory is allocated;
 *
 * all data is relativ to an objects name 'top' which can be either
 * PCB or PasteBuffer
 */
#define	VIA_LOOP(top, command)	{ 			\
	Cardinal	n;							\
	PinTypePtr	via;						\
	for (n = (top)->ViaN-1; n != -1; n--)	\
	{										\
		via = &(top)->Via[n];				\
		command;							\
	}}

#define	ELEMENT_LOOP(top, command)	{ 			\
	Cardinal 		n;							\
	ElementTypePtr	element;					\
	for (n = (top)->ElementN-1; n != -1; n--)	\
	{											\
		element = &(top)->Element[n];			\
		command;								\
	}}

#define	ELEMENTTEXT_LOOP(element, command)	{ 	\
	Cardinal	n;								\
	TextTypePtr	text;							\
	for (n = MAX_ELEMENTNAMES-1; n != -1; n--)	\
	{											\
		text = &(element)->Name[n];				\
		command;								\
	}}

#define	ELEMENTNAME_LOOP(element, command)	{ 		\
	Cardinal	n;									\
	char		*textstring;						\
	for (n = MAX_ELEMENTNAMES-1; n != -1; n--)		\
	{												\
		textstring = (element)->Name[n].TextString;	\
		command;									\
	}}

#define	PIN_LOOP(element, command)	{ 			\
	Cardinal	n;								\
	PinTypePtr	pin;							\
	for (n = (element)->PinN-1; n != -1; n--)	\
	{											\
		pin = &(element)->Pin[n];				\
		command;								\
	}}

#define	PAD_LOOP(element, command)	{ 			\
	Cardinal	n;								\
	PadTypePtr	pad;							\
	for (n = (element)->PadN-1; n != -1; n--)	\
	{											\
		pad = &(element)->Pad[n];				\
		command;								\
	}}

#define	ARC_LOOP(element, command)	{ 			\
	Cardinal	n;								\
	ArcTypePtr	arc;							\
	for (n = (element)->ArcN-1; n != -1; n--)	\
	{											\
		arc = &(element)->Arc[n];				\
		command;								\
	}}

#define	ELEMENTLINE_LOOP(element, command)	{ 	\
	Cardinal	n;								\
	LineTypePtr	line;							\
	for (n = (element)->LineN-1; n != -1; n--)	\
	{											\
		line = &(element)->Line[n];				\
		command;								\
	}}

#define	LINE_LOOP(layer, command) {				\
	Cardinal		n;							\
	LineTypePtr		line;						\
	for (n = (layer)->LineN-1; n != -1; n--)	\
	{											\
		line = &(layer)->Line[n];				\
		command;								\
	}}

#define	TEXT_LOOP(layer, command) {				\
	Cardinal		n;							\
	TextTypePtr		text;						\
	for (n = (layer)->TextN-1; n != -1; n--)	\
	{											\
		text = &(layer)->Text[n];				\
		command;								\
	}}

#define	POLYGON_LOOP(layer, command) {			\
	Cardinal		n;							\
	PolygonTypePtr	polygon;					\
	for (n = (layer)->PolygonN-1; n != -1; n--)	\
	{											\
		polygon = &(layer)->Polygon[n];			\
		command;								\
	}}

#define	POLYGONPOINT_LOOP(polygon, command)	{	\
	Cardinal			n;						\
	PointTypePtr	point;					\
	for (n = (polygon)->PointN-1; n != -1; n--)	\
	{											\
		point = &(polygon)->Points[n];			\
		command;								\
	}}

#define	ALLPIN_LOOP(top, command)	{				\
	ELEMENT_LOOP((top), PIN_LOOP(element, command ))\
	}

#define	ALLPAD_LOOP(top, command)	{				\
	ELEMENT_LOOP((top), PAD_LOOP(element, command ))\
	}

#define	ALLLINE_LOOP(top, command)	{				\
	Cardinal		l;								\
	LayerTypePtr	layer = (top)->Layer;			\
	for (l = 0; l < MAX_LAYER; l++, layer++)		\
		LINE_LOOP(layer, command );					\
	}

#define	ALLTEXT_LOOP(top, command)	{				\
	Cardinal		l;								\
	LayerTypePtr	layer = (top)->Layer;			\
	for (l = 0; l < MAX_LAYER; l++, layer++)		\
		TEXT_LOOP(layer, command );					\
	}

#define	ALLPOLYGON_LOOP(top, command)	{			\
	Cardinal		l;								\
	LayerTypePtr	layer = (top)->Layer;			\
	for (l = 0; l < MAX_LAYER; l++, layer++)		\
		POLYGON_LOOP(layer, command );				\
	}

#define	VISIBLELINE_LOOP(top, command)	{			\
	Cardinal		l;								\
	LayerTypePtr	layer = (top)->Layer;			\
	for (l = 0; l < MAX_LAYER; l++, layer++)		\
		if (layer->On)								\
			LINE_LOOP(layer, command );				\
	}

#define	VISIBLETEXT_LOOP(top, command)	{			\
	Cardinal		l;								\
	LayerTypePtr	layer = (top)->Layer;			\
	for (l = 0; l < MAX_LAYER; l++, layer++)		\
		if (layer->On)								\
			TEXT_LOOP(layer, command );				\
	}

#define	VISIBLEPOLYGON_LOOP(top, command)	{		\
	Cardinal		l;								\
	LayerTypePtr	layer = (top)->Layer;			\
	for (l = 0; l < MAX_LAYER; l++, layer++)		\
		if (layer->On)								\
			POLYGON_LOOP(layer, command );			\
	}

#endif

