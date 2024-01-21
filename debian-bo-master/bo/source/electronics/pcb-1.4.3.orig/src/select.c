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

static	char	*rcsid = "$Id: select.c,v 143.1 1996/09/16 09:08:57 nau Exp $";

/* select routines
 */

#include "global.h"

#include "data.h"
#include "draw.h"
#include "error.h"
#include "search.h"
#include "select.h"

#ifdef HAS_REGEX
#ifdef sgi
#include <unistd.h>
#else
#include <sys/types.h>
#include <regex.h>
#endif
#endif

/* ---------------------------------------------------------------------------
 * toggles the selection of any kind of object
 * the different types are defined by search.h
 */
Boolean SelectObject(void)
{
	void	*ptr1, *ptr2, *ptr3;
	Boolean	changed = True;
	
	switch(SearchObjectByPosition(SELECT_TYPES, &ptr1, &ptr2, &ptr3,
		Crosshair.X, Crosshair.Y))
	{
		case VIA_TYPE:
			TOGGLE_FLAG(SELECTEDFLAG, (PinTypePtr) ptr1);
			DrawVia((PinTypePtr) ptr1);
			break;

		case LINE_TYPE:
			TOGGLE_FLAG(SELECTEDFLAG, (LineTypePtr) ptr2);
			DrawLine((LayerTypePtr) ptr1, (LineTypePtr) ptr2);
			break;

		case TEXT_TYPE:
			TOGGLE_FLAG(SELECTEDFLAG, (TextTypePtr) ptr2);
			DrawText((LayerTypePtr) ptr1, (TextTypePtr) ptr2);
			break;

		case POLYGON_TYPE:
			TOGGLE_FLAG(SELECTEDFLAG, (PolygonTypePtr) ptr2);
			DrawPolygon((LayerTypePtr) ptr1, (PolygonTypePtr) ptr2);
			break;

		case PIN_TYPE:
			TOGGLE_FLAG(SELECTEDFLAG, (PinTypePtr) ptr2);
			DrawPin((PinTypePtr) ptr2);
			break;

		case PAD_TYPE:
			TOGGLE_FLAG(SELECTEDFLAG, (PadTypePtr) ptr2);
			DrawPad((PadTypePtr) ptr2);
			break;

		case ELEMENTNAME_TYPE:
		{
			ElementTypePtr	element = (ElementTypePtr) ptr1;

				/* select all names of the element */
			ELEMENTTEXT_LOOP(element, TOGGLE_FLAG(SELECTEDFLAG, text));
			DrawElementName(element);
			break;
		}
			
		case ELEMENT_TYPE:
		{
			ElementTypePtr	element = (ElementTypePtr) ptr1;

				/* select all pins and names of the element */
			PIN_LOOP(element, TOGGLE_FLAG(SELECTEDFLAG, pin););
			PAD_LOOP(element, TOGGLE_FLAG(SELECTEDFLAG, pad););
			ELEMENTTEXT_LOOP(element, TOGGLE_FLAG(SELECTEDFLAG, text));
			TOGGLE_FLAG(SELECTEDFLAG, element);
			if (PCB->ElementOn &&
				((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn))
			if (PCB->ElementOn)
			{
				DrawElementName(element);
				DrawElementPackage(element);
			}
			if (PCB->PinOn)
				DrawElementPinsAndPads(element);
			break;
		}
	}
	return(changed);
}

/* ----------------------------------------------------------------------
 * selects/unselects all visible objects within the passed box
 * Flag determines if the block is to be selected or unselected
 * returns True if the state of any object has changed
 */
Boolean SelectBlock(BoxTypePtr Box, Boolean Flag)
{
	Boolean		changed = False;

		/* check layers */
	VISIBLELINE_LOOP(PCB->Data,
		if (LINE_IN_BOX(line, Box))
		{
			ASSIGN_FLAG(SELECTEDFLAG, Flag, line);
			DrawLine(layer, line);
			changed = True;
		}
	);
	VISIBLETEXT_LOOP(PCB->Data,
		if (TEXT_IN_BOX(text, Box))
		{
			ASSIGN_FLAG(SELECTEDFLAG, Flag, text);
			DrawText(layer, text);
			changed = True;
		}
	);
	VISIBLEPOLYGON_LOOP(PCB->Data,
		if (POLYGON_IN_BOX(polygon, Box))
		{
			ASSIGN_FLAG(SELECTEDFLAG, Flag, polygon);
			DrawPolygon(layer, polygon);
			changed = True;
		}
	);

		/* elements */
	if (PCB->ElementOn && PCB->PinOn)
		ELEMENT_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
				if (ELEMENT_IN_BOX(element, Box))
				{
						/* select all pins and names of element */
					ELEMENTTEXT_LOOP(element,
						ASSIGN_FLAG(SELECTEDFLAG, Flag, text)
					);
					PIN_LOOP(element,
						ASSIGN_FLAG(SELECTEDFLAG, Flag, pin);
					);
					PAD_LOOP(element,
						ASSIGN_FLAG(SELECTEDFLAG, Flag, pad);
					);
					ASSIGN_FLAG(SELECTEDFLAG, Flag, element);
					DrawElement(element);
					changed = True;
				}
		);

		/* element names */
	if (PCB->ElementOn)
		ELEMENT_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
				if (BOX_IN_BOX(&ELEMENT_TEXT(PCB, element).BoundingBox, Box))
				{
						/* select all pins and names of element */
					ELEMENTTEXT_LOOP(element,
						ASSIGN_FLAG(SELECTEDFLAG, Flag, text)
					);
					DrawElementName(element);
					changed = True;
				}
		);

		/* pins */
	if (PCB->PinOn)
	{
		ALLPIN_LOOP(PCB->Data,
			if (VIA_OR_PIN_IN_BOX(pin, Box))
			{
				ASSIGN_FLAG(SELECTEDFLAG, Flag, pin);
				DrawPin(pin);
				changed = True;
			}
		);
		ALLPAD_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, pad) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
				if (PAD_IN_BOX(pad, Box))
				{
					ASSIGN_FLAG(SELECTEDFLAG, Flag, pad);
					DrawPad(pad);
					changed = True;
				}
		);
	}

		/* end with vias */
	if (PCB->ViaOn)
		VIA_LOOP(PCB->Data,
			if (VIA_OR_PIN_IN_BOX(via, Box))
			{
				ASSIGN_FLAG(SELECTEDFLAG, Flag, via);
				DrawVia(via);
				changed = True;
			}
		);

	return(changed);
}

/* ----------------------------------------------------------------------
 * performs several operations on the passed object
 */
void *ObjectOperation(ObjectFunctionTypePtr F,
	int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	switch(Type)
	{
		case LINE_TYPE:
			if (F->Line)
				return(F->Line((LayerTypePtr) Ptr1, (LineTypePtr) Ptr2));
			break;

		case LINEPOINT_TYPE:
			if (F->LinePoint)
				return(F->LinePoint((LayerTypePtr) Ptr1, (LineTypePtr) Ptr2,
					(PointTypePtr) Ptr3));
			break;

		case TEXT_TYPE:
			if (F->Text)
				return(F->Text((LayerTypePtr) Ptr1, (TextTypePtr) Ptr2));
			break;

		case POLYGON_TYPE:
			if (F->Polygon)
				return(F->Polygon((LayerTypePtr) Ptr1, (PolygonTypePtr) Ptr2));
			break;

		case POLYGONPOINT_TYPE:
			if (F->Point)
				return(F->Point((LayerTypePtr) Ptr1, (PolygonTypePtr) Ptr2,
					(PointTypePtr) Ptr3));
			break;

		case VIA_TYPE:
			if (F->Via)
				return(F->Via((PinTypePtr) Ptr1));
			break;

		case ELEMENT_TYPE:
			if (F->Element)
				return(F->Element((ElementTypePtr) Ptr1));
			break;

		case PIN_TYPE:
			if (F->Pin)
				return(F->Pin((ElementTypePtr) Ptr1, (PinTypePtr) Ptr2));
			break;

		case PAD_TYPE:
			if (F->Pad)
				return(F->Pad((ElementTypePtr) Ptr1, (PadTypePtr) Ptr2));
			break;

		case ELEMENTNAME_TYPE:
			if (F->ElementName)
				return(F->ElementName((ElementTypePtr) Ptr1));
			break;
	}
	return(NULL);
}

/* ----------------------------------------------------------------------
 * performs several operations on selected objects which are also visible
 * The lowlevel procdures are passed together with additional information
 * resets the selected flag if requested
 * returns True if anything has changed
 */
Boolean SelectedOperation(ObjectFunctionTypePtr F, Boolean Reset)
{
	Boolean		changed = False;

		/* check lines */
	if (F->Line)
		VISIBLELINE_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, line))
			{
				if (Reset)
					CLEAR_FLAG(SELECTEDFLAG, line);
				F->Line(layer, line);
				changed = True;
			}
		);

		/* check text */
	if (F->Text)
		VISIBLETEXT_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, text))
			{
				if (Reset)
					CLEAR_FLAG(SELECTEDFLAG, text);
				F->Text(layer, text);
				changed = True;
			}
		);

		/* check polygons */
	if (F->Polygon)
		VISIBLEPOLYGON_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, polygon))
			{
				if (Reset)
					CLEAR_FLAG(SELECTEDFLAG, polygon);
				F->Polygon(layer, polygon);
				changed = True;
			}
		);

		/* elements; both text objects share one selection flag */
	if (PCB->ElementOn)
		ELEMENT_LOOP(PCB->Data,
			if (F->Element && TEST_FLAG(SELECTEDFLAG, element))
			{
				if (Reset)
					CLEAR_FLAG(SELECTEDFLAG, element);
				F->Element(element);
				changed = True;
			}
			if (F->ElementName &&
				TEST_FLAG(SELECTEDFLAG, &ELEMENT_TEXT(PCB, element)))
			{
				if (Reset)
					CLEAR_FLAG(SELECTEDFLAG, &ELEMENT_TEXT(PCB, element));
				F->ElementName(element);
				changed = True;
			}
		);
	if (PCB->PinOn && F->Pin)
		ELEMENT_LOOP(PCB->Data,
			PIN_LOOP(element,
				if (TEST_FLAG(SELECTEDFLAG, pin))
				{
					if (Reset)
						CLEAR_FLAG(SELECTEDFLAG, pin);
					F->Pin(element, pin);
					changed = True;
				}
			);
			PAD_LOOP(element,
				if (TEST_FLAG(SELECTEDFLAG, pad))
				{
					if (Reset)
						CLEAR_FLAG(SELECTEDFLAG, pad);
					F->Pad(element, pad);
					changed = True;
				}
			);
		);
		

		/* and vias */
	if (PCB->ViaOn && F->Via)
		VIA_LOOP(PCB->Data,
			if (TEST_FLAG(SELECTEDFLAG, via))
			{
				if (Reset)
					CLEAR_FLAG(SELECTEDFLAG, via);
				F->Via(via);
				changed = True;
			}
		);

	return(changed);
}

/* ----------------------------------------------------------------------
 * selects/unselects all objects which were found during a connection scan
 * Flag determines if they are to be selected or unselected
 * returns True if the state of any object has changed
 *
 * text objects and elements cannot be selected by this routine
 */
Boolean SelectConnection(Boolean Flag)
{
	Boolean		changed = False;

	VISIBLELINE_LOOP(PCB->Data,
		if (TEST_FLAG(FOUNDFLAG, line))
		{
			ASSIGN_FLAG(SELECTEDFLAG, Flag, line);
			DrawLine(layer, line);
			changed = True;
		}
	);
	VISIBLEPOLYGON_LOOP(PCB->Data,
		if (TEST_FLAG(FOUNDFLAG, polygon))
		{
			ASSIGN_FLAG(SELECTEDFLAG, Flag, polygon);
			DrawPolygon(layer, polygon);
			changed = True;
		}
	);

	if (PCB->PinOn)
	{
		ALLPIN_LOOP(PCB->Data,
			if (TEST_FLAG(FOUNDFLAG, pin))
			{
				ASSIGN_FLAG(SELECTEDFLAG, Flag, pin);
				DrawPin(pin);
				changed = True;
			}
		);
		ALLPAD_LOOP(PCB->Data,
			if (TEST_FLAG(FOUNDFLAG, pad))
			{
				ASSIGN_FLAG(SELECTEDFLAG, Flag, pad);
				DrawPad(pad);
				changed = True;
			}
		);
	}

	if (PCB->ViaOn)
		VIA_LOOP(PCB->Data,
			if (TEST_FLAG(FOUNDFLAG, via))
			{
				ASSIGN_FLAG(SELECTEDFLAG, Flag, via);
				DrawVia(via);
				changed = True;
			}
	);

	return(changed);
}

#ifdef HAS_REGEX
/* ---------------------------------------------------------------------------
 * selects objects as defined by Type by name;
 * it's a case insensitive match
 * returns True if any object has been selected
 */
Boolean SelectObjectByName(int Type, char *Pattern)
{
	Boolean		changed = False;
	Cardinal	i;

#if defined(sgi)
#define	REGEXEC(arg)	(re_exec((arg)) == 1)

	char		*compiled;

		/* compile the regular expression */
	if ((compiled = re_comp(Pattern)) != NULL)
	{
		Message("re_comp error: %s\n", compiled);
		return(False);
	}
#else
#define	REGEXEC(arg)	(!regexec(&compiled, (arg), 1, &match, 0))

	int			result;
	regex_t		compiled;
	regmatch_t	match;

		/* compile the regular expression */
	result = regcomp(&compiled, Pattern, REG_EXTENDED | REG_ICASE | REG_NOSUB);
	if (result)
	{
		char	errorstring[128];

		regerror(result, &compiled, errorstring, 128);
		Message("regexp error: %s\n", errorstring);
		regfree(&compiled);
		return(False);
	}
#endif

		/* loop over all visible objects with names */
	if (Type & TEXT_TYPE)
		for (i = 0; i < MAX_LAYER; i++)
		{
			LayerTypePtr	layer = &PCB->Data->Layer[i];
			if (layer->On)
				TEXT_LOOP(layer,
					if (text->TextString && REGEXEC(text->TextString))
					{
						SET_FLAG(SELECTEDFLAG, text);
						DrawText(layer, text);
						changed = True;
					}
				);
		}

	if (PCB->ElementOn && (Type & ELEMENT_TYPE))
		ELEMENT_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
			{
				String	name = ELEMENT_NAME(PCB,element);

				if (name && REGEXEC(name))
				{
					SET_FLAG(SELECTEDFLAG, element);
					DrawElement(element);
					changed = True;
				}
			}
		);
	if (PCB->PinOn && (Type & PIN_TYPE))
		ALLPIN_LOOP(PCB->Data,
			if (pin->Name && REGEXEC(pin->Name))
			{
				SET_FLAG(SELECTEDFLAG, pin);
				DrawPin(pin);
				changed = True;
			}
		);
	if (PCB->PinOn && (Type & PAD_TYPE))
		ALLPAD_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, pad) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
				if (pad->Name && REGEXEC(pad->Name))
				{
					SET_FLAG(SELECTEDFLAG, pad);
					DrawPad(pad);
					changed = True;
				}
		);
	if (PCB->ViaOn && (Type & VIA_TYPE))
		VIA_LOOP(PCB->Data,
			if (via->Name && REGEXEC(via->Name))
			{
				SET_FLAG(SELECTEDFLAG, via);
				DrawVia(via);
				changed = True;
			}
		);
	
#if !defined(sgi)
	regfree(&compiled);
#endif

	return(changed);
}
#endif		/* HAS_REGEX */
