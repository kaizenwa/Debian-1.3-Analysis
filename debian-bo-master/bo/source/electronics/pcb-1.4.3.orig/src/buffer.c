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

static	char	*rcsid = "$Id: buffer.c,v 143.1 1996/09/16 09:08:27 nau Exp $";

/* functions used by paste- and move/copy buffer
 */

#include <stdlib.h>
#include <memory.h>

#include "global.h"

#include "buffer.h"
#include "copy.h"
#include "create.h"
#include "crosshair.h"
#include "data.h"
#include "mymem.h"
#include "mirror.h"
#include "misc.h"
#include "parse_l.h"
#include "rotate.h"
#include "remove.h"
#include "search.h"
#include "select.h"
#include "set.h"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	*AddViaToBuffer(PinTypePtr);
static	void	*AddLineToBuffer(LayerTypePtr, LineTypePtr);
static	void	*AddTextToBuffer(LayerTypePtr, TextTypePtr);
static	void	*AddPolygonToBuffer(LayerTypePtr, PolygonTypePtr);
static	void	*AddElementToBuffer(ElementTypePtr);
static	void	*MoveViaToBuffer(PinTypePtr);
static	void	*MoveLineToBuffer(LayerTypePtr, LineTypePtr);
static	void	*MoveTextToBuffer(LayerTypePtr, TextTypePtr);
static	void	*MovePolygonToBuffer(LayerTypePtr, PolygonTypePtr);
static	void	*MoveElementToBuffer(ElementTypePtr);

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	DataTypePtr		Dest,
						Source;
static	ObjectFunctionType	AddBufferFunctions = {
			AddLineToBuffer,
			AddTextToBuffer,
			AddPolygonToBuffer,
			AddViaToBuffer,
			AddElementToBuffer,
			NULL,
			NULL,
			NULL,
			NULL,
			NULL },
							MoveBufferFunctions = {
			MoveLineToBuffer,
			MoveTextToBuffer,
			MovePolygonToBuffer,
			MoveViaToBuffer,
			MoveElementToBuffer,
			NULL,
			NULL,
			NULL,
			NULL,
			NULL };

/* ---------------------------------------------------------------------------
 * copies a via to paste buffer
 */
static void *AddViaToBuffer(PinTypePtr Via)
{
	return(CreateNewVia(Dest, Via->X, Via->Y, Via->Thickness,
		Via->DrillingHole, Via->Name, Via->Flags));
}

/* ---------------------------------------------------------------------------
 * copies a line to buffer  
 */
static void *AddLineToBuffer(LayerTypePtr Layer, LineTypePtr Line)
{
	LayerTypePtr	layer = &Dest->Layer[GetLayerNumber(Source, Layer)];
	
	return(CreateNewLineOnLayer(layer, Line->Point1.X, Line->Point1.Y,
		Line->Point2.X, Line->Point2.Y, Line->Thickness, Line->Flags));
}

/* ---------------------------------------------------------------------------
 * copies a text to buffer
 */
static void *AddTextToBuffer(LayerTypePtr Layer, TextTypePtr Text)
{
	LayerTypePtr	layer = &Dest->Layer[GetLayerNumber(Source, Layer)];
	
	return(CreateNewText(layer, &PCB->Font, Text->X, Text->Y,
		Text->Direction, Text->Scale, Text->TextString, Text->Flags));
}

/* ---------------------------------------------------------------------------
 * copies a polygon to buffer
 */
static void *AddPolygonToBuffer(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
	LayerTypePtr	layer = &Dest->Layer[GetLayerNumber(Source, Layer)];
	PolygonTypePtr	polygon;
	
	polygon = GetPolygonMemory(layer);
	CopyPolygonLowLevel(polygon, Polygon);
	return(polygon);
}

/* ---------------------------------------------------------------------------
 * copies a element to buffer
 */
static void *AddElementToBuffer(ElementTypePtr Element)
{
	ElementTypePtr	element;
	
	element = GetElementMemory(Dest);
	CopyElementLowLevel(element, Element);
	return(element);
}

/* ---------------------------------------------------------------------------
 * moves a via to paste buffer without allocating memory for the name
 */
static void *MoveViaToBuffer(PinTypePtr Via)
{
	PinTypePtr	via;

	via = GetViaMemory(Dest);
	*via = *Via;
	*Via = Source->Via[--Source->ViaN];
	memset(&Source->Via[Source->ViaN], 0, sizeof(PinType));
	return(via);
}

/* ---------------------------------------------------------------------------
 * moves a line to buffer  
 */
static void *MoveLineToBuffer(LayerTypePtr Layer, LineTypePtr Line)
{
	LineTypePtr	line;

	line = GetLineMemory(&Dest->Layer[GetLayerNumber(Source, Layer)]);
	*line = *Line;
	*Line = Layer->Line[--Layer->LineN];
	memset(&Layer->Line[Layer->LineN], 0, sizeof(LineType));
	return(line);
}

/* ---------------------------------------------------------------------------
 * moves a text to buffer without allocating memory for the name
 */
static void *MoveTextToBuffer(LayerTypePtr Layer, TextTypePtr Text)
{
	TextTypePtr	text;

	text = GetTextMemory(&Dest->Layer[GetLayerNumber(Source, Layer)]);
	*text = *Text;
	*Text = Layer->Text[--Layer->TextN];
	memset(&Layer->Text[Layer->TextN], 0, sizeof(TextType));
	return(text);
}

/* ---------------------------------------------------------------------------
 * moves a polygon to buffer without allocating memory for the points
 */
static void *MovePolygonToBuffer(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
	PolygonTypePtr	polygon;
	
	polygon = GetPolygonMemory(&Dest->Layer[GetLayerNumber(Source, Layer)]);
	*polygon = *Polygon;
	*Polygon = Layer->Polygon[--Layer->PolygonN];
	memset(&Layer->Polygon[Layer->PolygonN], 0, sizeof(PolygonType));
	return(polygon);
}

/* ---------------------------------------------------------------------------
 * moves a element to buffer without allocating memory for pins/names
 */
static void *MoveElementToBuffer(ElementTypePtr Element)
{
	ElementTypePtr	element;

	element = GetElementMemory(Dest);
	*element = *Element;
	*Element = Source->Element[--Source->ElementN];
	memset(&Source->Element[Source->ElementN], 0, sizeof(ElementType));
	return(element);
}

/* ---------------------------------------------------------------------------
 * calculates the bounding box of the buffer
 */
void SetBufferBoundingBox(BufferTypePtr Buffer)
{
	BoxTypePtr	box = GetDataBoundingBox(Buffer->Data);

	if (box)
		Buffer->BoundingBox = *box;
}

/* ---------------------------------------------------------------------------
 * clears the contents of the paste buffer
 */
void ClearBuffer(BufferTypePtr Buffer)
{
	if (Buffer)
		FreeDataMemory(Buffer->Data);
}

/* ----------------------------------------------------------------------
 * copies all selected and visible objects to the paste buffer
 * returns True if any objects have been removed
 */
void AddSelectedToBuffer(BufferTypePtr Buffer)
{
		/* switch crosshair off because adding objects to the pastebuffer
		 * may change the 'valid' area for the cursor
		 */
	HideCrosshair(True);
	Source = PCB->Data;
	Dest = Buffer->Data;
	SelectedOperation(&AddBufferFunctions, False);

		/* set origin to current position */
	Buffer->X = Crosshair.X;
	Buffer->Y = Crosshair.Y;
	RestoreCrosshair(True);
}

/* ---------------------------------------------------------------------------
 * loads element data from file/library into buffer
 * parse the file with disabled 'PCB mode' (see parser)
 * returns False on error
 * if successful, update some other stuff and reposition the pastebuffer
 */
Boolean LoadElementToBuffer(BufferTypePtr Buffer,
	char *Name, Boolean FromFile)
{
	ElementTypePtr	element;
	Cursor			oldCursor;

		/* change cursor shape to 'watch' and save old setting */
	oldCursor = SetOutputXCursor(XC_watch);
	ClearBuffer(Buffer);
	element = GetElementMemory(Buffer->Data);
	if (!(FromFile ?
		ParseElementFile(element, Name) :
		ParseLibraryEntry(element, Name)))
	{
		SetElementBoundingBox(element);

			/* always add elements using top-side coordinates */
		if (Settings.ShowSolderSide)
			MirrorElementCoordinates(element);

			/* set buffer offset to 'mark' position */
		PASTEBUFFER->X = element->MarkX;
		PASTEBUFFER->Y = element->MarkY;
		SetOutputXCursor(oldCursor);
		return(True);
	}
		/* release memory which might have been aquired */
	ClearBuffer(Buffer);
	SetOutputXCursor(oldCursor);
	return(False);
}

/* ---------------------------------------------------------------------------
 * load PCB into buffer
 * parse the file with enabled 'PCB mode' (see parser)
 * if successful, update some other stuff
 */
Boolean LoadLayoutToBuffer(BufferTypePtr Buffer, char *Filename)
{
	PCBTypePtr	newPCB = CreateNewPCB(False);

		/* new data isn't added to the undo list */
	if (!ParsePCB(newPCB, Filename))
	{
			/* clear data area and replace pointer */
		ClearBuffer(Buffer);
		SaveFree(Buffer->Data);
		Buffer->Data = newPCB->Data;
		newPCB->Data = NULL;
		Buffer->X = newPCB->CursorX;
		Buffer->Y = newPCB->CursorY;
		RemovePCB(newPCB);
		return(True);
	}

		/* release unused memory */
	RemovePCB(newPCB);
	return(False);
}

/* ---------------------------------------------------------------------------
 * rotates the contents of the pastebuffer
 */
void RotateBuffer(BufferTypePtr Buffer, BYTE Number)
{
		/* rotate vias */
	VIA_LOOP(Buffer->Data,
		ROTATE_VIA_LOWLEVEL(via, Buffer->X, Buffer->Y, Number);
	);

		/* elements */
	ELEMENT_LOOP(Buffer->Data,
		RotateElementLowLevel(element, Buffer->X, Buffer->Y, Number);
	);

		/* all layer related objects */
	ALLLINE_LOOP(Buffer->Data,
			RotateLineLowLevel(line, Buffer->X, Buffer->Y, Number);
	);
	ALLTEXT_LOOP(Buffer->Data,
		RotateTextLowLevel(text, Buffer->X, Buffer->Y, Number);
	);
	ALLPOLYGON_LOOP(Buffer->Data,
		RotatePolygonLowLevel(polygon, Buffer->X, Buffer->Y, Number);
	);

		/* finally the origin and the bounding box */
	ROTATE(Buffer->X, Buffer->Y, Buffer->X, Buffer->Y, Number);
	RotateBoxLowLevel(&Buffer->BoundingBox, Buffer->X, Buffer->Y, Number);
}

/* ---------------------------------------------------------------------------
 * initializes the buffers by allocating memory
 */
void InitBuffers(void)
{
	int	i;

	for (i = 0; i < MAX_BUFFER; i++)
	{
		Buffers[i][0].Data = CreateNewBuffer();
		Buffers[i][1].Data = CreateNewBuffer();
	}
}

/* ----------------------------------------------------------------------
 * moves the passed object to the passed buffer and removes it
 * from its original place
 */
void *MoveObjectToBuffer(DataTypePtr Destination, DataTypePtr Src,
	int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
		/* setup local identifiers used by move operations */
	Dest = Destination;
	Source = Src;
	return(ObjectOperation(&MoveBufferFunctions, Type, Ptr1, Ptr2, Ptr3));
}

