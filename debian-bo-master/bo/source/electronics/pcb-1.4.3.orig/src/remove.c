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
 */

static	char	*rcsid = "$Id: remove.c,v 143.1 1996/09/16 09:08:54 nau Exp $";

/* functions used to remove vias, pins ...
 */

#include <memory.h>

#include "global.h"

#include "data.h"
#include "draw.h"
#include "error.h"
#include "misc.h"
#include "mymem.h"
#include "remove.h"
#include "search.h"
#include "select.h"
#include "set.h"
#include "undo.h"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	*DestroyVia(PinTypePtr);
static	void	*DestroyLine(LayerTypePtr, LineTypePtr);
static	void	*DestroyText(LayerTypePtr, TextTypePtr);
static	void	*DestroyPolygon(LayerTypePtr, PolygonTypePtr);
static	void	*DestroyElement(ElementTypePtr);
static	void	*RemoveVia(PinTypePtr);
static	void	*DestroyPolygonPoint(LayerTypePtr, PolygonTypePtr,
					PointTypePtr);
static	void	*RemovePolygonPoint(LayerTypePtr, PolygonTypePtr,
					PointTypePtr);

/* ---------------------------------------------------------------------------
 * some local types
 */
static	ObjectFunctionType	RemoveFunctions = {
	RemoveLine,
	RemoveText,
	RemovePolygon,
	RemoveVia,
	RemoveElement,
	NULL,
	NULL,
	NULL,
	NULL,
	RemovePolygonPoint };
static	ObjectFunctionType	DestroyFunctions = {
	DestroyLine,
	DestroyText,
	DestroyPolygon,
	DestroyVia,
	DestroyElement,
	NULL,
	NULL,
	NULL,
	NULL,
	DestroyPolygonPoint };

/* ---------------------------------------------------------------------------
 * remove PCB
 */
void RemovePCB(PCBTypePtr Ptr)
{
	ClearUndoList(True);
	FreePCBMemory(Ptr);
	SaveFree(Ptr);
}

/* ---------------------------------------------------------------------------
 * destroys a via
 */
static void *DestroyVia(PinTypePtr Via)
{
		/* erase from screen and memory */
	if (PCB->ViaOn)
		EraseVia(Via);
	SaveFree(Via->Name);
	*Via = PCB->Data->Via[--PCB->Data->ViaN];
	memset(&PCB->Data->Via[PCB->Data->ViaN], 0, sizeof(PinType));
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * destroys a line from a layer 
 */
static void *DestroyLine(LayerTypePtr Layer, LineTypePtr Line)
{
		/* erase from screen */
	if (Layer->On)
		EraseLine(Line);
	*Line = Layer->Line[--Layer->LineN];
	memset(&Layer->Line[Layer->LineN], 0, sizeof(LineType));
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * destroys a rectangle from a layer
 */
static void *DestroyPolygon(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
		/* erase from screen */
	if (Layer->On)
		ErasePolygon(Polygon);
	FreePolygonMemory(Polygon);
	*Polygon = Layer->Polygon[--Layer->PolygonN];
	memset(&Layer->Polygon[Layer->PolygonN], 0, sizeof(PolygonType));
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * removes a polygon-point from a polygon and destroys the data
 */
static void *DestroyPolygonPoint(LayerTypePtr Layer,
	PolygonTypePtr Polygon, PointTypePtr Point)
{
	return(RemovePolygonPoint(Layer, Polygon, Point));
}

/* ---------------------------------------------------------------------------
 * destroys a text from a layer
 */
static void *DestroyText(LayerTypePtr Layer, TextTypePtr Text)
{
		/* erase from screen */
	if (Layer->On)
		EraseText(Text);
	SaveFree(Text->TextString);
	*Text = Layer->Text[--Layer->TextN];
	memset(&Layer->Text[Layer->TextN], 0, sizeof(TextType));
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * destroys a element
 */
static void *DestroyElement(ElementTypePtr Element)
{
		/* erase from screen */
	if ((PCB->ElementOn || PCB->PinOn) &&
		((TEST_FLAG(ONSOLDERFLAG, Element) != 0) == SWAP_IDENT ||
		PCB->InvisibleObjectsOn))
		EraseElement(Element);
	FreeElementMemory(Element);
	*Element = PCB->Data->Element[--PCB->Data->ElementN];
	memset(&PCB->Data->Element[PCB->Data->ElementN], 0, sizeof(ElementType));
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * removes a via
 */
static void *RemoveVia(PinTypePtr Via)
{
		/* erase from screen and memory */
	if (PCB->ViaOn)
		EraseVia(Via);
	MoveObjectToRemoveUndoList(VIA_TYPE, Via, Via, Via);
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * removes a line from a layer 
 */
void *RemoveLine(LayerTypePtr Layer, LineTypePtr Line)
{
		/* erase from screen */
	if (Layer->On)
		EraseLine(Line);
	MoveObjectToRemoveUndoList(LINE_TYPE, Layer, Line, Line);
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * removes a text from a layer
 */
void *RemoveText(LayerTypePtr Layer, TextTypePtr Text)
{
		/* erase from screen */
	if (Layer->On)
		EraseText(Text);
	MoveObjectToRemoveUndoList(TEXT_TYPE, Layer, Text, Text);
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * removes a polygon from a layer
 */
void *RemovePolygon(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
		/* erase from screen */
	if (Layer->On)
		ErasePolygon(Polygon);
	MoveObjectToRemoveUndoList(POLYGON_TYPE, Layer, Polygon, Polygon);
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * removes a polygon-point from a polygon
 */
static void *RemovePolygonPoint(LayerTypePtr Layer,
	PolygonTypePtr Polygon, PointTypePtr Point)
{
	PointTypePtr	ptr;

		/* insert the polygon-point into the list */
	AddObjectToRemovePointUndoList(POLYGONPOINT_TYPE, Layer, Polygon, Point);

	if (Layer->On)
		ErasePolygon(Polygon);

		/* remove point from list, keep point order */
	for (ptr = Point+1; ptr != &Polygon->Points[Polygon->PointN]; ptr++)
	{
		*Point = *ptr;
		Point = ptr;
	}
	Polygon->PointN--;
	SetPolygonBoundingBox(Polygon);

		/* redraw polygon if necessary */
	if (Layer->On)
		DrawPolygon(Layer, Polygon);
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * removes a element
 */
void *RemoveElement(ElementTypePtr Element)
{
		/* erase from screen */
	if ((PCB->ElementOn || PCB->PinOn) &&
		((TEST_FLAG(ONSOLDERFLAG, Element) != 0) == SWAP_IDENT ||
		PCB->InvisibleObjectsOn))
		EraseElement(Element);
	MoveObjectToRemoveUndoList(ELEMENT_TYPE, Element, Element, Element);
	return(NULL);
}

/* ----------------------------------------------------------------------
 * removes all selected and visible objects
 * returns True if any objects have been removed
 */
Boolean RemoveSelected(void)
{
	if (SelectedOperation(&RemoveFunctions, False))
	{
		IncrementUndoSerialNumber();
		return(True);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * remove object as referred by pointers and type,
 * allocated memory is passed to the 'remove undo' list
 */
void *RemoveObject(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	void	*ptr = ObjectOperation(&RemoveFunctions, Type, Ptr1, Ptr2, Ptr3);

	IncrementUndoSerialNumber();
	return(ptr);
}

/* ---------------------------------------------------------------------------
 * remove object as referred by pointers and type
 * allocated memory is destroyed
 */
void *DestroyObject(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	return(ObjectOperation(&DestroyFunctions, Type, Ptr1, Ptr2, Ptr3));
}
