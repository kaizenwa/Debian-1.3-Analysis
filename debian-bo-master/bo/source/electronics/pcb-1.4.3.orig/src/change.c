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

static	char	*rcsid = "$Id: change.c,v 143.1 1996/09/16 09:08:28 nau Exp $";

/* functions used to change object properties
 *
 * there is no 'undo' for changing the size of an object because it is
 * easy to recover
 */

#include <stdlib.h>

#include "global.h"

#include "change.h"
#include "control.h"
#include "data.h"
#include "dialog.h"
#include "draw.h"
#include "mymem.h"
#include "misc.h"
#include "search.h"
#include "select.h"
#include "set.h"
#include "undo.h"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	*ChangePinSize(ElementTypePtr, PinTypePtr);
static	void	*ChangePadSize(ElementTypePtr, PadTypePtr);
static	void	*ChangePin2ndSize(ElementTypePtr, PinTypePtr);
static	void	*ChangeViaSize(PinTypePtr);
static	void	*ChangeVia2ndSize(PinTypePtr);
static	void	*ChangeLineSize(LayerTypePtr, LineTypePtr);
static	void	*ChangeTextSize(LayerTypePtr, TextTypePtr);
static	void	*ChangeElementNameSize(ElementTypePtr);
static	void	*ChangePinName(ElementTypePtr, PinTypePtr);
static	void	*ChangePadName(ElementTypePtr, PadTypePtr);
static	void	*ChangeViaName(PinTypePtr);
static	void	*ChangeElementName(ElementTypePtr);
static	void	*ChangeTextName(LayerTypePtr, TextTypePtr);
static	void	*ChangeElementSquare(ElementTypePtr);
static	void	*ChangePinSquare(ElementTypePtr, PinTypePtr);

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	int					Delta;		/* change of size */
static	char				*NewName;	/* new name */
static	ObjectFunctionType	ChangeSizeFunctions = {
	ChangeLineSize,
	ChangeTextSize,
	NULL,
	ChangeViaSize,
	NULL,
	ChangeElementNameSize,
	ChangePinSize,
	ChangePadSize,
	NULL,
	NULL };
static	ObjectFunctionType	Change2ndSizeFunctions = {
	NULL,
	NULL,
	NULL,
	ChangeVia2ndSize,
	NULL,
	NULL,
	ChangePin2ndSize,
	NULL,
	NULL,
	NULL };
static	ObjectFunctionType	ChangeNameFunctions = {
	NULL,
	ChangeTextName,
	NULL,
	ChangeViaName,
	ChangeElementName,
	NULL,
	ChangePinName,
	ChangePadName,
	NULL,
	NULL };
static	ObjectFunctionType	ChangeSquareFunctions = {
	NULL,
	NULL,
	NULL,
	NULL,
	ChangeElementSquare,
	NULL,
	ChangePinSquare,
	NULL,
	NULL,
	NULL };

/* ---------------------------------------------------------------------------
 * changes the size of a via
 * returns TRUE if changed
 */
static void *ChangeViaSize(PinTypePtr Via)
{
	Dimension	value = Via->Thickness +Delta;

	if (value <= MAX_PINORVIASIZE &&
		value >= MIN_PINORVIASIZE &&
		value >= Via->DrillingHole +MIN_PINORVIACOPPER)
	{
		EraseVia(Via);
		Via->Thickness = value;
		DrawVia(Via);
		return(Via);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the drilling hole of a via
 * returns TRUE if changed
 */
static void *ChangeVia2ndSize(PinTypePtr Via)
{
	Dimension	value = Via->DrillingHole +Delta;

	if (value <= MAX_PINORVIASIZE &&
		value >= MIN_PINORVIAHOLE &&
		value <= Via->Thickness -MIN_PINORVIACOPPER)
	{
		EraseVia(Via);
		Via->DrillingHole = value;
		DrawVia(Via);
		return(Via);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the size of a pin
 * returns TRUE if changed
 */
static void *ChangePinSize(ElementTypePtr Element, PinTypePtr Pin)
{
	Dimension	value = Pin->Thickness +Delta;

	Element = Element;		/* get rid of 'unused...' warnings */
	if (value <= MAX_PINORVIASIZE &&
		value >= MIN_PINORVIASIZE &&
		value >= Pin->DrillingHole +MIN_PINORVIACOPPER)
	{
		ErasePin(Pin);
		Pin->Thickness = value;
		DrawPin(Pin);
		return(Pin);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the size of a pad
 * returns TRUE if changed
 */
static void *ChangePadSize(ElementTypePtr Element, PadTypePtr Pad)
{
	Dimension	value = Pad->Thickness +Delta;

	if (value <= MAX_PADSIZE && value >= MIN_PADSIZE)
	{
		ErasePad(Pad);
		Pad->Thickness = value;
		DrawPad(Pad);
		return(Pad);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the drilling hole of a pin
 * returns TRUE if changed
 */
static void *ChangePin2ndSize(ElementTypePtr Element, PinTypePtr Pin)
{
	Dimension	value = Pin->DrillingHole +Delta;

	Element = Element;		/* get rid of 'unused...' warnings */
	if (value <= MAX_PINORVIASIZE &&
		value >= MIN_PINORVIAHOLE &&
		value <= Pin->Thickness -MIN_PINORVIACOPPER)
	{
		ErasePin(Pin);
		Pin->DrillingHole = value;
		DrawPin(Pin);
		return(Pin);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the size of a line
 * returns TRUE if changed
 */
static void *ChangeLineSize(LayerTypePtr Layer, LineTypePtr Line)
{
	Dimension	value = Line->Thickness +Delta;

	if (value <= MAX_LINESIZE && value >= MIN_LINESIZE)
	{
		EraseLine(Line);
		Line->Thickness = value;
		DrawLine(Layer, Line);
		return(Line);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the scaling factor of a text object
 * returns TRUE if changed
 */
static void *ChangeTextSize(LayerTypePtr Layer, TextTypePtr Text)
{
	Dimension	value = Text->Scale +Delta;

	if (value <= MAX_TEXTSCALE && value >= MIN_TEXTSCALE)
	{
		EraseText(Text);
		Text->Scale = value;
		SetTextBoundingBox(&PCB->Font, Text);
		DrawText(Layer, Text);
		return(Text);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the scaling factor of a elementname object
 * returns TRUE if changed
 */
static void *ChangeElementNameSize(ElementTypePtr Element)
{
	Dimension	value = DESCRIPTION_TEXT(Element).Scale +Delta;

	if (value <= MAX_TEXTSCALE && value >= MIN_TEXTSCALE)
	{
		EraseElementName(Element);
		ELEMENTTEXT_LOOP(Element,
			text->Scale = value;
			SetTextBoundingBox(&PCB->Font, text);
		);
		DrawElementName(Element);
		return(Element);
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the name of a via
 */
static void *ChangeViaName(PinTypePtr Via)
{
	char	*old = Via->Name;

	Via->Name = NewName;
	return(old);
}

/* ---------------------------------------------------------------------------
 * changes the name of a pin
 */
static void *ChangePinName(ElementTypePtr Element, PinTypePtr Pin)
{
	char	*old = Pin->Name;

	Element = Element;		/* get rid of 'unused...' warnings */
	Pin->Name = NewName;
	return(old);
}

/* ---------------------------------------------------------------------------
 * changes the name of a pad
 */
static void *ChangePadName(ElementTypePtr Element, PadTypePtr Pad)
{
	char	*old = Pad->Name;

	Element = Element;		/* get rid of 'unused...' warnings */
	Pad->Name = NewName;
	return(old);
}

/* ---------------------------------------------------------------------------
 * changes the layout-name of an element
 */
static void *ChangeElementName(ElementTypePtr Element)
{
	char	*old = ELEMENT_NAME(PCB, Element);

	EraseElementName(Element);
	ELEMENT_NAME(PCB, Element) = NewName;
	SetTextBoundingBox(&PCB->Font, &ELEMENT_TEXT(PCB, Element));
	DrawElementName(Element);
	return(old);
}

/* ---------------------------------------------------------------------------
 * sets data of a text object and calculates bounding box
 * memory must have already been allocated
 * the one for the new string is allocated
 * returns True if the string has been changed
 */
static void *ChangeTextName(LayerTypePtr Layer, TextTypePtr Text)
{
	char	*old = Text->TextString;

	EraseText(Text);
	Text->TextString = NewName;

		/* calculate size of the bounding box */
	SetTextBoundingBox(&PCB->Font, Text);
	DrawText(Layer, Text);
	return(old);
}

/* ---------------------------------------------------------------------------
 * changes the name of a layout; memory has to be already allocated
 */
Boolean ChangeLayoutName(char *Name)
{
	PCB->Name = Name;
	SetStatusLine();
	return(True);
}

/* ---------------------------------------------------------------------------
 * changes the name of a layer; memory has to be already allocated
 */
Boolean ChangeLayerName(LayerTypePtr Layer, char *Name)
{
	CURRENT->Name = Name;
	UpdateControlPanel();
	return(True);
}

/* ---------------------------------------------------------------------------
 * changes the square flag of an elements first pin
 */
static void *ChangeElementSquare(ElementTypePtr Element)
{
	PIN_LOOP(Element,
		if (TEST_FLAG(PINONEFLAG, pin))
			return(ChangePinSquare(Element, pin));
	);
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * changes the square flag of a pin
 */
static void *ChangePinSquare(ElementTypePtr Element, PinTypePtr Pin)
{
	ErasePin(Pin);
	TOGGLE_FLAG(SQUAREFLAG, Pin);
	DrawPin(Pin);
	return(Pin);
}

/* ----------------------------------------------------------------------
 * changes the size of all selected and visible pins
 * returns True if anything has changed
 */
Boolean ChangeSelectedPinSize(Position Difference)
{
	Boolean	change = False;

		/* setup identifiers */
	Delta = Difference;
	if (PCB->PinOn)
		ALLPIN_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, pin))
				change |= (ChangePinSize(element, pin) != NULL);
		);
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the size of all selected and visible pads
 * returns True if anything has changed
 */
Boolean ChangeSelectedPadSize(Position Difference)
{
	Boolean	change = False;

		/* setup identifiers */
	Delta = Difference;
	if (PCB->PinOn)
	{
		ALLPAD_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, pad) &&
				((TEST_FLAG(ONSOLDERFLAG, pad) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn))
				change |= (ChangePadSize(element, pad) != NULL);
		);
	}
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the 2nd size (drilling hole) of all selected and visible pins
 * returns True if anything has changed
 */
Boolean ChangeSelectedPin2ndSize(Position Difference)
{
	Boolean	change = False;

		/* setup identifiers */
	Delta = Difference;
	if (PCB->PinOn)
		ALLPIN_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, pin))
				change |= (ChangePin2ndSize(element, pin) != NULL);
		);
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the size of all selected and visible vias
 * returns True if anything has changed
 */
Boolean ChangeSelectedViaSize(Position Difference)
{
	Boolean	change = False;

		/* setup identifiers */
	Delta = Difference;
	if (PCB->ViaOn)
		VIA_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, via))
				change |= (ChangeViaSize(via) != NULL);
		);
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the 2nd size (drilling hole) of all selected and visible vias
 * returns True if anything has changed
 */
Boolean ChangeSelectedVia2ndSize(Position Difference)
{
	Boolean	change = False;

		/* setup identifiers */
	Delta = Difference;
	if (PCB->ViaOn)
		VIA_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, via))
				change |= (ChangeVia2ndSize(via) != NULL);
		);
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the size of all selected and visible lines
 * returns True if anything has changed
 */
Boolean ChangeSelectedLineSize(Position Difference)
{
	Boolean	change = False;

		/* setup identifiers */
	Delta = Difference;
	VISIBLELINE_LOOP(PCB->Data,
		if (TEST_FLAG(SELECTEDFLAG, line))
			change |= (ChangeLineSize(layer, line) != NULL);
	);
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the size of all selected and visible text objects
 * returns True if anything has changed
 */
Boolean ChangeSelectedTextSize(Position Difference)
{
	Boolean	change = False;

		/* setup identifiers */
	Delta = Difference;
	VISIBLETEXT_LOOP(PCB->Data,
		if (TEST_FLAG(SELECTEDFLAG, text))
			change |= (ChangeTextSize(layer, text) != NULL);
	);
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the square-flag of all selected and visible pins
 * returns True if anything has changed
 */
Boolean ChangeSelectedPinSquare(void)
{
	Boolean	change = False;

	if (PCB->PinOn)
		ALLPIN_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, pin))
			{
				ChangePinSquare(element, pin);
				change = True;
			}
		);
	return(change);
}

/* ----------------------------------------------------------------------
 * changes the square-flag of pin one of all selected and visible elements
 * returns True if anything has changed
 */
Boolean ChangeSelectedElementSquare(void)
{
	Boolean	change = False;

	if (PCB->PinOn)
		ELEMENT_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, element) && element->Pin)
			{
				ChangeElementSquare(element);
				change = True;
			}
		);
	return(change);
}

/* ---------------------------------------------------------------------------
 * changes the size of the passed object
 * Returns True if anything is changed
 */
Boolean ChangeObjectSize(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position Difference)
{
		/* setup identifier */
	Delta = Difference;
	return(ObjectOperation(&ChangeSizeFunctions, Type,Ptr1,Ptr2,Ptr3) != NULL);
}

/* ---------------------------------------------------------------------------
 * changes the 2nd size of the passed object
 * Returns True if anything is changed
 */
Boolean ChangeObject2ndSize(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position Difference)
{
		/* setup identifier */
	Delta = Difference;
	return(ObjectOperation(&Change2ndSizeFunctions,Type,Ptr1,Ptr2,Ptr3) !=NULL);
}

/* ---------------------------------------------------------------------------
 * changes the name of the passed object
 * returns the old name
 *
 * The allocated memory isn't freed because the old string is used
 * by the undo module.
 */
void *ChangeObjectName(int Type, void *Ptr1, void *Ptr2, void *Ptr3, char *Name)
{
		/* setup identifier */
	NewName = Name;
	return(ObjectOperation(&ChangeNameFunctions, Type, Ptr1, Ptr2, Ptr3));
}

/* ---------------------------------------------------------------------------
 * changes the square-flag of the passed object
 * Returns True if anything is changed
 */
Boolean ChangeObjectSquare(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	return(ObjectOperation(&ChangeSquareFunctions, Type,Ptr1,Ptr2,Ptr3) !=NULL);
}

/* ---------------------------------------------------------------------------
 * queries the user for a new object name and changes it
 *
 * The allocated memory isn't freed because the old string is used
 * by the undo module.
 */
void *QueryInputAndChangeObjectName(int Type,
	void *Ptr1, void *Ptr2, void *Ptr3)
{
	char	*name = NULL;

	switch(Type)
	{
		case VIA_TYPE:
			name = GetUserInput("Vianame:", EMPTY(((PinTypePtr) Ptr2)->Name));
			break;

		case PIN_TYPE:
			name = GetUserInput("Pinname:", EMPTY(((PinTypePtr) Ptr2)->Name));
			break;

		case PAD_TYPE:
			name = GetUserInput("Padname:", EMPTY(((PadTypePtr) Ptr2)->Name));
			break;

		case TEXT_TYPE:
			name = GetUserInput("Enter text:",
				EMPTY(((TextTypePtr) Ptr2)->TextString));
			break;

		case ELEMENT_TYPE:
			name = GetUserInput("Elementname:",
				EMPTY(ELEMENT_NAME(PCB, (ElementTypePtr) Ptr2)));
			break;
	}
	if (name)
	{
		char	*old = ChangeObjectName(Type, Ptr1, Ptr2, Ptr3, name);

		AddObjectToChangeNameUndoList(Type, Ptr1, Ptr2, Ptr3, old);
		IncrementUndoSerialNumber();
		return(Ptr3);
	}
	return(NULL);
}
