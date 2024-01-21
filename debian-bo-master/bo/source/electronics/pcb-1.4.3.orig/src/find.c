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

static	char	*rcsid = "$Id: find.c,v 143.1 1996/09/16 09:08:38 nau Exp $";

/*
 * short description:
 * - all pin and via pointers are copied to a single array to have linear
 *   access to them. All pointers refer to this list.
 * - two fields, with pointers to line data, are build for each layer
 *   sorted by:
 *     -  the lower x line coordinates in decending order and
 *     -  the higher x line coordinates in asscending order.
 *   Similar fields for pads are created.
 *   They are used to have fewer accesses when looking for line connections.
 * - lists for pins and vias, for lines, pads and for polygons are created.
 *   Every object that has to be checked is added to its list.
 * - there's no 'speed-up' mechanism for polygons because they are not used
 *   as often as lines and are much easier to handle
 * - the maximum distance between line and pin ... would depend on the angle
 *   between them. To speed up computation the limit is set to one half
 *   of the thickness of the objects (cause of square pins).
 *
 * PV:  means pin or via (objects that connect layers)
 * LO:  all non PV objects (layer related objects like lines, polygons, pads)
 *
 * 1. first, the PV at the given coordinates is looked up
 * 2. all LO connections to that PV are looked up next
 * 3. lookup of all LOs connected to LOs from (2).
 *    This step is repeated until no more new connections are found.
 * 4. lookup all PVs connected to the LOs from (2) and (3)
 * 5. start again with (1) for all new PVs from (4)
 *
 * Intersection of line <--> circle:
 * - calculate the signed distance from the line to the center,
 *   return false if abs(distance) > R
 * - get the distance from the line <--> distancevector intersection to
 *   (X1,Y1) in range [0,1], return true if 0 <= distance <= 1
 * - depending on (r > 1.0 or r < 0.0) check the distance of X2,Y2 or X1,Y1
 *   to X,Y
 * - There is a maximum difference between the inner circle
 *   of a PV and the outer one of 8.2% of its radius.
 *   This difference is ignored which means that lines that end
 *   right at the corner of a PV are not recognized.
 *
 * Intersection of line <--> line:
 * - see the description of 'LineLineIntersect()'
 */

/* routines to find connections between pins, vias, lines...
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/times.h>

#include "global.h"

#include "data.h"
#include "dialog.h"
#include "draw.h"
#include "find.h"
#include "mymem.h"
#include "misc.h"
#include "search.h"
#include "set.h"

/* ---------------------------------------------------------------------------
 * some local macros
 */
#define	SEPERATE(FP)							\
	{											\
		int	i;									\
		fputc('#', (FP));						\
		for (i = Settings.CharPerLine; i; i--)	\
			fputc('=', (FP));					\
		fputc('\n', (FP));						\
	}

#define	PADLIST_ENTRY(L,I)	\
	(((PadDataTypePtr *)PadList[(L)].Data)[(I)])

#define	LINELIST_ENTRY(L,I)	\
	(((LineTypePtr *)LineList[(L)].Data)[(I)])

#define	POLYGONLIST_ENTRY(L,I)	\
	(((PolygonTypePtr *)PolygonList[(L)].Data)[(I)])

#define	PVLIST_ENTRY(I)	\
	(((PVDataTypePtr *)PVList.Data)[(I)])

#define	IS_PV_ON_LINE(PV,Line)	\
	((TEST_FLAG(SQUAREFLAG, (PV)) ? \
		IsLineInRectangle( \
			(PV)->X -((PV)->Thickness+1)/2, (PV)->Y -((PV)->Thickness+1)/2, \
			(PV)->X +((PV)->Thickness+1)/2, (PV)->Y +((PV)->Thickness+1)/2, \
			(Line)) : \
		IsPointOnLine((PV)->X,(PV)->Y,(PV)->Thickness/2+1, (Line))))

#define	IS_PV_IN_POLYGON(PV,Polygon)	\
	((TEST_FLAG(SQUAREFLAG, (PV)) ? \
		IsRectangleInPolygon( \
			(PV)->X -((PV)->Thickness+1)/2, (PV)->Y -((PV)->Thickness+1)/2, \
			(PV)->X +((PV)->Thickness+1)/2, (PV)->Y +((PV)->Thickness+1)/2, \
			(Polygon)) : \
		IsPointInPolygon((PV)->X,(PV)->Y,(PV)->Thickness/2+1, (Polygon))))

#define	IS_PV_ON_PAD(PV,Pad)	IS_PV_ON_LINE((PV), (LineTypePtr) (Pad))

#define	ADD_PV_TO_LIST(Ptr)					\
{											\
	SET_FLAG(FOUNDFLAG, (Ptr)->Data);		\
	PVLIST_ENTRY(PVList.Number) = (Ptr);	\
	PVList.Number++;						\
}

#define	ADD_PAD_TO_LIST(L,Ptr)							\
{														\
	SET_FLAG(FOUNDFLAG, (Ptr)->Data);					\
	PADLIST_ENTRY((L),PadList[(L)].Number) = (Ptr);		\
	PadList[(L)].Number++;								\
}

#define	ADD_LINE_TO_LIST(L,Ptr)							\
{														\
	SET_FLAG(FOUNDFLAG, (Ptr));							\
	LINELIST_ENTRY((L),LineList[(L)].Number) = (Ptr);	\
	LineList[(L)].Number++;								\
}

#define	ADD_POLYGON_TO_LIST(L,Ptr)							\
{															\
	SET_FLAG(FOUNDFLAG, (Ptr));								\
	POLYGONLIST_ENTRY((L), PolygonList[(L)].Number) = (Ptr);\
	PolygonList[(L)].Number++;								\
}

/* ---------------------------------------------------------------------------
 * some local types
 *
 * the two 'dummy' structs for PVs and Pads are necessary for creating
 * connection lists which include the elments name
 */
typedef struct
{
	void		**Data;			/* pointer to index data */
	Cardinal	Position,		/* currently used position */
				DrawPosition,
				Number;			/* number of objects in list */
} ListType, *ListTypePtr;

typedef struct					/* holds a copy of all pins and vias */
{								/* plus a pointer to the according element */
	PinTypePtr		Data;
	ElementTypePtr	Element;
} PVDataType, *PVDataTypePtr;

typedef struct					/* holds a copy of all pads of a layer */
{								/* plus a pointer to the according element */
	PadTypePtr		Data;
	ElementTypePtr	Element;
} PadDataType, *PadDataTypePtr;

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Widget			Popup;						/* abort dialog and flag */
static	Boolean			Abort;
static	LineTypePtr		*LineSortedByLowX[MAX_LAYER],	/* sorted array of */
						*LineSortedByHighX[MAX_LAYER];	/* line pointers */
static	PadDataTypePtr	PadData[2];
static	PadDataTypePtr	*PadSortedByLowX[2],
						*PadSortedByHighX[2];
static	PVDataTypePtr	PVSortedByX;				/* same for PV */
static	Cardinal		TotalPV,					/* total number of PV */
						NumberOfPads[2];
static	ListType		LineList[MAX_LAYER],		/* list of objects to */
						PolygonList[MAX_LAYER],
						PadList[2],
						PVList;

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void			CB_Abort(Widget, XtPointer, XtPointer);
static	void			CreateAbortDialog(void);
static	Boolean			CheckAbort(void);
static	int				ComparePadByLowX(const void *, const void *);
static	int				ComparePadByHighX(const void *, const void *);
static	int				CompareLineByLowX(const void *, const void *);
static	int				CompareLineByHighX(const void *, const void *);
static	int				ComparePVByX(const void *, const void *);
static	Cardinal		GetPadByLowX(Position, Cardinal);
static	Cardinal		GetPadByHighX(Position, Cardinal);
static	Cardinal		GetLineByLowX(Position, Cardinal);
static	Cardinal		GetLineByHighX(Position, Cardinal);
static	Cardinal		GetPVByX(Position);
static	void			FreeConnectionLookupMemory(void);
static	void			InitConnectionLookup(void);
static	PadDataTypePtr	*GetIndexOfPads(Position, Position,
							Cardinal, Cardinal *);
static	LineTypePtr		*GetIndexOfLines(Position, Position,
							Cardinal, Cardinal *);
static	PVDataType		*LookupPVByCoordinates(Position, Position);
static	PadDataTypePtr	LookupPadByAddress(PadTypePtr);
static	void			LookupLOConnectionsToPVList(void);
static	void			LookupLOConnectionsToLOList(void);
static	void			LookupPVConnectionsToLOList(void);
static	void			LookupLOConnectionsToLine(LineTypePtr, Cardinal);
static	void			LookupLOConnectionsToPolygon(PolygonTypePtr, Cardinal);
static	Boolean			IsLineInPolygon(LineTypePtr, PolygonTypePtr);
static	Boolean			PrepareNextLoop(FILE *);
static	Boolean			PrintElementConnections(ElementTypePtr, FILE *);
static	Boolean			ListsEmpty(void);
static	void			DoIt(void);
static	void			PrintElementNameList(ElementTypePtr, FILE *);
static	void			PrintConnectionElementName(ElementTypePtr, FILE *);
static	void			PrintConnectionListEntry(char *, ElementTypePtr,
							Boolean, FILE *);
static	void			PrintPadConnections(Cardinal, FILE *, Boolean);
static	void			PrintPinConnections(FILE *, Boolean);
static	Boolean			PrintAndSelectUnusedPinsAndPadsOfElement(ElementTypePtr,
							FILE *);
static	void			DrawNewConnections(void);
static	void			ResetConnections(void);

/* ---------------------------------------------------------------------------
 * some of the 'pad' routines are the same as for lines because the 'pad'
 * struct starts with a line struct. See global.h for details
 */
#define	LookupLOConnectionsToPad(Pad,Group)	\
	LookupLOConnectionsToLine((LineTypePtr) (Pad), (Group))
#define	LinePadIntersect(Line, Pad)			\
	LineLineIntersect((Line), (LineTypePtr) (Pad))
#define	IsPadInPolygon(Pad, Polygon)		\
	IsLineInPolygon((LineTypePtr) (Pad), (Polygon))

/* ---------------------------------------------------------------------------
 * callback for 'AbortDialog' dialog
 */
static void CB_Abort(Widget W, XtPointer ClientData, XtPointer CallData)
{
	Abort = True;
}

/* ---------------------------------------------------------------------------
 * creates an 'Abort' dialog
 */
static void CreateAbortDialog()
{
	static	DialogButtonType	buttons[] = {
		{ "defaultButton", " Abort ", CB_Abort, (XtPointer) NULL, NULL }};

		/* create dialog box */
	Popup = CreateDialogBox("press button to abort 'scanning of connections'",
		buttons, ENTRIES(buttons));
	Abort = False;
	StartDialog(Popup);
}

/* ---------------------------------------------------------------------------
 * dispatches all events and returns the status of
 * 'Abort'
 */
static Boolean CheckAbort(void)
{
	XEvent  event;

	while (XtAppPending(Context))
	{
		XtAppNextEvent(Context, &event);
		XtDispatchEvent(&event);
	}
	return(Abort);
}

/* ---------------------------------------------------------------------------
 * quicksort compare function for pad coordinate X1
 * pad field is sorted in decending order of X1
 */
static int ComparePadByLowX(const void * Index1, const void * Index2)
{
	PadDataTypePtr	ptr1 = *((PadDataTypePtr *) Index1),
					ptr2 = *((PadDataTypePtr *) Index2);

	return((int) (MIN(ptr2->Data->Point1.X, ptr2->Data->Point2.X) -
		MIN(ptr1->Data->Point1.X, ptr1->Data->Point2.X)));
}

/* ---------------------------------------------------------------------------
 * quicksort compare function for pad coordinate X2
 * pad field is sorted in ascending order of X2
 */
static int ComparePadByHighX(const void * Index1, const void * Index2)
{
	PadDataTypePtr	ptr1 = *((PadDataTypePtr *) Index1),
					ptr2 = *((PadDataTypePtr *) Index2);

	return((int) (MAX(ptr1->Data->Point1.X, ptr1->Data->Point2.X) -
		MAX(ptr2->Data->Point1.X, ptr2->Data->Point2.X)));
}

/* ---------------------------------------------------------------------------
 * quicksort compare function for line coordinate X1
 * line field is sorted in decending order of X1
 */
static int CompareLineByLowX(const void * Index1, const void * Index2)
{
	LineTypePtr	ptr1 = *((LineTypePtr *) Index1),
				ptr2 = *((LineTypePtr *) Index2);

	return((int) (MIN(ptr2->Point1.X, ptr2->Point2.X) -MIN(ptr1->Point1.X, ptr1->Point2.X)));
}

/* ---------------------------------------------------------------------------
 * quicksort compare function for line coordinate X2
 * line field is sorted in ascending order of X2
 */
static int CompareLineByHighX(const void * Index1, const void * Index2)
{
	LineTypePtr	ptr1 = *((LineTypePtr *) Index1),
				ptr2 = *((LineTypePtr *) Index2);

	return((int) (MAX(ptr1->Point1.X, ptr1->Point2.X) -MAX(ptr2->Point1.X, ptr2->Point2.X)));
}

/* ---------------------------------------------------------------------------
 * quicksort compare function for pin (via) coordinate X
 * for sorting the PV field in ascending order of X
 */
static int ComparePVByX(const void * Index1, const void * Index2)
{
	PinTypePtr	ptr1 = ((PVDataTypePtr) Index1)->Data,
				ptr2 = ((PVDataTypePtr) Index2)->Data;

	return((int) (ptr1->X -ptr2->X));
}

/* ---------------------------------------------------------------------------
 * returns the minimum index which matches
 * lowX(pad[index]) <= X for all index >= 'found one'
 * the field is sorted in a descending order
 */
static Cardinal GetPadByLowX(Position X, Cardinal Layer)
{
	PadDataTypePtr	*ptr = PadSortedByHighX[Layer];
	int				left = 0,
					right = NumberOfPads[Layer] -1,
					position;

	while (left < right)
	{
		position = (left +right) /2;
		if (MIN(ptr[position]->Data->Point1.X, ptr[position]->Data->Point2.X) <= X)
			right = position;
		else
			left = position+1;
	}
	return((Cardinal) left);
}

/* ---------------------------------------------------------------------------
 * returns the minimum index which matches
 * highX(pad[index]) >= X for all index >= 'found one'
 * the field is sorted in a ascending order
 */
static Cardinal GetPadByHighX(Position X, Cardinal Layer)
{
	PadDataTypePtr	*ptr = PadSortedByHighX[Layer];
	int				left = 0,
					right = NumberOfPads[Layer] -1,
					position;

	while (left < right)
	{
		position = (left +right) /2;
		if (MAX(ptr[position]->Data->Point1.X, ptr[position]->Data->Point2.X) >= X)
			right = position;
		else
			left = position+1;
	}
	return((Cardinal) left);
}

/* ---------------------------------------------------------------------------
 * returns the minimum index which matches
 * lowX(line[index]) <= X for all index >= 'found one'
 * the field is sorted in a descending order
 */
static Cardinal GetLineByLowX(Position X, Cardinal Layer)
{
	LineTypePtr	*ptr = LineSortedByLowX[Layer];
	int			left = 0,
				right = PCB->Data->Layer[Layer].LineN -1,
				position;

	while (left < right)
	{
		position = (left +right) /2;
		if (MIN(ptr[position]->Point1.X, ptr[position]->Point2.X) <= X)
			right = position;
		else
			left = position+1;
	}
	return((Cardinal) left);
}

/* ---------------------------------------------------------------------------
 * returns the minimum index which matches
 * highX(line[index]) >= X for all index >= 'found one'
 * the field is sorted in a ascending order
 */
static Cardinal GetLineByHighX(Position X, Cardinal Layer)
{
	LineTypePtr	*ptr = LineSortedByHighX[Layer];
	int			left = 0,
				right = PCB->Data->Layer[Layer].LineN -1,
				position;

	while (left < right)
	{
		position = (left +right) /2;
		if (MAX(ptr[position]->Point1.X, ptr[position]->Point2.X) >= X)
			right = position;
		else
			left = position+1;
	}
	return((Cardinal) left);
}

/* ---------------------------------------------------------------------------
 * returns the minimum index which matches
 * X(PV[index]) >= X for all index >= 'found one'
 * the field is sorted in a ascending order
 */
static Cardinal GetPVByX(Position X)
{
	int	left = 0,
		right = TotalPV -1,
		position;

	while (left < right)
	{
		position = (left +right) /2;
		if (PVSortedByX[position].Data->X >= X)
			right = position;
		else
			left = position+1;
	}
	return((Cardinal) left);
}

/* ---------------------------------------------------------------------------
 * releases all allocated memory
 */
static void FreeConnectionLookupMemory(void)
{
	Cardinal	i;

	for (i = 0; i < 2; i++)
	{
		MyFree((char **) &PadData[i]);
		MyFree((char **) &PadSortedByLowX[i]);
		MyFree((char **) &PadSortedByHighX[i]);
		MyFree((char **) &PadList[i].Data);
	}
	for (i = 0; i < MAX_LAYER; i++)
	{
		MyFree((char **) &LineSortedByLowX[i]);
		MyFree((char **) &LineSortedByHighX[i]);
		MyFree((char **) &LineList[i].Data);
		MyFree((char **) &PolygonList[i].Data);
	}
	MyFree((char **) &PVSortedByX);
	MyFree((char **) &PVList.Data);
}

/* ---------------------------------------------------------------------------
 * allocates memory for stacks ...
 * initializes index and sorts it by X1 and X2
 */
static void InitConnectionLookup(void)
{
	Cardinal	i,
	pos;

		/* initialize pad data; start by counting the total number
		 * on each of the two possible layers
		 */
	NumberOfPads[COMPONENT_LAYER] = NumberOfPads[SOLDER_LAYER] = 0;
	ALLPAD_LOOP(PCB->Data,
		if (TEST_FLAG(ONSOLDERFLAG, pad))
			NumberOfPads[SOLDER_LAYER]++;
		else
			NumberOfPads[COMPONENT_LAYER]++;
	);
	for (i = 0; i < 2; i++)
	{
		if (NumberOfPads[i])
		{
			int	count = 0;

				/* copy pad and element pointers to a list */
			PadData[i] = (PadDataTypePtr) MyCalloc(NumberOfPads[i],
				sizeof(PadDataType), "InitConnectionLookup()");
			ALLPAD_LOOP(PCB->Data,
				if ((TEST_FLAG(ONSOLDERFLAG, pad) != 0) == (i == SOLDER_LAYER))
				{
					PadData[i][count].Data = pad;
					PadData[i][count].Element = element;
					count++;
				}
			);

				/* create two sorted lists of pointers */
			PadSortedByLowX[i] = (PadDataTypePtr *) MyCalloc(NumberOfPads[i],
				sizeof(PadDataTypePtr), "InitConnectionLookup()");
			PadSortedByHighX[i] = (PadDataTypePtr *) MyCalloc(NumberOfPads[i],
				sizeof(PadDataTypePtr), "InitConnectionLookup()");
			for (count = 0; count < NumberOfPads[i]; count++)
				PadSortedByLowX[i][count] = PadSortedByHighX[i][count] = 
					&PadData[i][count];
			qsort(PadSortedByLowX[i], NumberOfPads[i], sizeof(PadDataTypePtr),
				ComparePadByLowX);
			qsort(PadSortedByHighX[i], NumberOfPads[i], sizeof(PadDataTypePtr),
				ComparePadByHighX);

				/* allocate memory for working list */
			PadList[i].Data = (void **) MyCalloc(NumberOfPads[i],
				sizeof(PadDataTypePtr), "InitConnectionLookup()");
		}

			/* clear some struct members */
		PadList[i].Position = 0;
		PadList[i].DrawPosition = 0;
		PadList[i].Number = 0;
	}

		/* initialize line and polygon data */
	for (i = 0; i < MAX_LAYER; i++)
	{
		LayerTypePtr	layer = &PCB->Data->Layer[i];

		if (layer->LineN)
		{
				/* allocate memory for line pointer lists */
			LineSortedByLowX[i] = (LineTypePtr *) MyCalloc(layer->LineN,
				sizeof(LineTypePtr), "InitConnectionLookup()");
			LineSortedByHighX[i] = (LineTypePtr *) MyCalloc(layer->LineN,
				sizeof(LineTypePtr), "InitConnectionLookup()");
			LineList[i].Data = (void **) MyCalloc(layer->LineN,
				sizeof(LineTypePtr), "InitConnectionLookup()");

				/* copy addresses to arrays and sort them */
			LINE_LOOP(layer,
				LineSortedByLowX[i][n] = LineSortedByHighX[i][n] = line;
			);
			qsort(LineSortedByLowX[i], layer->LineN, sizeof(LineTypePtr),
				CompareLineByLowX);
			qsort(LineSortedByHighX[i], layer->LineN, sizeof(LineTypePtr),
				CompareLineByHighX);
		}

			/* allocate memory for polygon list */
		if (layer->PolygonN)
			PolygonList[i].Data = (void **) MyCalloc(layer->PolygonN,
				sizeof(PolygonTypePtr), "InitConnectionLookup()");

			/* clear some struct members */
		LineList[i].Position = 0;
		LineList[i].DrawPosition = 0;
		LineList[i].Number = 0;
		PolygonList[i].Position = 0;
		PolygonList[i].DrawPosition = 0;
		PolygonList[i].Number = 0;
	}

		/* pin and via data; start with counting their total number,
		 * then allocate memory and copy the data to a tmp field
		 * set number of the according element in tmp field too
		 */
	TotalPV = PCB->Data->ViaN;
	ELEMENT_LOOP(PCB->Data, TotalPV += element->PinN; );
	PVSortedByX = (PVDataTypePtr) MyCalloc(TotalPV, sizeof(PVDataType),
		"InitConnectionLookup()");

		/* copy via data; field is initialized with NULL */
	pos = 0;
	VIA_LOOP(PCB->Data, PVSortedByX[pos++].Data = via; );
	ELEMENT_LOOP(PCB->Data,
		PIN_LOOP(element,
			PVSortedByX[pos].Data = pin;
			PVSortedByX[pos++].Element = element;
		);
	);

		/* sort array by X */
	qsort(PVSortedByX, TotalPV, sizeof(PVDataType), ComparePVByX);

		/* allocate memory for 'new PV to check' list and clear struct */
	PVList.Data = (void **) MyCalloc(TotalPV, sizeof(PVDataTypePtr),
		"InitConnectionLookup()");
	PVList.Position = 0;
	PVList.DrawPosition = 0;
	PVList.Number = 0;
}

/* ---------------------------------------------------------------------------
 * returns a pointer into the sorted list with the highest index and the
 * number of entries left to check
 * All list entries starting from the pointer position to the end
 * may match the specified x coordinate range.
 */
static PadDataTypePtr *GetIndexOfPads(Position Xlow, Position Xhigh,
	Cardinal Layer, Cardinal *Number)
{
	Cardinal	index1,
				index2;

		/* get index of the first pad that may match the coordinates
		 * see GetPadByLowX(), GetPadByHighX()
		 * take the field with less entries to speed up searching
		 */
	index1 = GetPadByLowX(Xhigh, Layer);
	index2 = GetPadByHighX(Xlow, Layer);
	if (index1 > index2)
	{
		*Number = NumberOfPads[Layer] -index1;
		return(&PadSortedByLowX[Layer][index1]);
	}
	*Number = NumberOfPads[Layer] -index2;
	return(&PadSortedByHighX[Layer][index2]);
}

/* ---------------------------------------------------------------------------
 * returns a pointer into the sorted list with the highest index and the
 * number of entries left to check
 * All list entries starting from the pointer position to the end
 * may match the specified x coordinate range.
 */
static LineTypePtr *GetIndexOfLines(Position Xlow, Position Xhigh,
	Cardinal Layer, Cardinal *Number)
{
	Cardinal	index1,
				index2;

		/* get index of the first line that may match the coordinates
		 * see GetLineByLowX(), GetLineByHighX()
		 * take the field with less entries to speed up searching
		 */
	index1 = GetLineByLowX(Xhigh, Layer);
	index2 = GetLineByHighX(Xlow, Layer);
	if (index1 > index2)
	{
		*Number = PCB->Data->Layer[Layer].LineN -index1;
		return(&LineSortedByLowX[Layer][index1]);
	}
	*Number = PCB->Data->Layer[Layer].LineN -index2;
	return(&LineSortedByHighX[Layer][index2]);
}

/* ---------------------------------------------------------------------------
 * finds a pad by it's address in the sorted list
 * A pointer to the list entry or NULL is returned
 */
static PadDataTypePtr LookupPadByAddress(PadTypePtr Pad)
{
	Cardinal		i, layer;
	PadDataTypePtr	ptr;

	layer = TEST_FLAG(ONSOLDERFLAG, Pad) ? SOLDER_LAYER : COMPONENT_LAYER;
	i = NumberOfPads[layer];
	ptr = PadData[layer];
	while (i)
	{
		if (ptr->Data == Pad)
			return(ptr);
		i--;
		ptr++;
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * finds a PV by it's coordinates in the sorted list
 * A pointer to the list entry or NULL is returned
 */
static PVDataTypePtr LookupPVByCoordinates(Position X, Position Y)
{
	Cardinal	i, limit;

		/* return if their are no PVs */
	if (!TotalPV)
		return(NULL);

		/* get absolute lower/upper boundary */
	i = GetPVByX(X -MAX_PINORVIASIZE);
	limit = GetPVByX(X +MAX_PINORVIASIZE +1);
	while (i <= limit)
	{
		PinTypePtr	ptr = PVSortedByX[i].Data;

			/* check which one matches */
		if (abs(ptr->X - X) <= ptr->Thickness/4 &&
			abs(ptr->Y - Y) <= ptr->Thickness/4)
			return(&PVSortedByX[i]);
		i++;
	}
	return(NULL);
}

/* ---------------------------------------------------------------------------
 * checks if a PV is connected to LOs, if it is, the LO is added to
 * the appropriate list and the 'used' flag is set
 */
static void LookupLOConnectionsToPVList(void)
{
	PinTypePtr	pv;
	Cardinal	layer;

		/* loop over all PVs currently on list */
	while (PVList.Position < PVList.Number)
	{
			/* get pointer to data */
		pv = PVLIST_ENTRY(PVList.Position)->Data;

			/* check pads (which are lines) */
		for (layer = 0; layer < 2; layer++)
		{
			Dimension		distance = (MAX_PADSIZE + pv->Thickness) /2;
			PadDataTypePtr	*sortedptr;
			Cardinal		i;

				/* get the lowest data pointer of pads which may have
				 * a connection to the PV ### pad->Point1.X <= pad->Point2.X ###
				 */
			sortedptr = GetIndexOfPads(pv->X -distance, pv->X +distance,
				layer, &i);
			for (; i; i--, sortedptr++)
				if (!TEST_FLAG(FOUNDFLAG, (*sortedptr)->Data) &&
					IS_PV_ON_PAD(pv, (*sortedptr)->Data))
					ADD_PAD_TO_LIST(layer, *sortedptr);
		}

			/* now all lines and polygons of the several layers */
		for (layer = 0; layer < MAX_LAYER; layer++)
		{
			PolygonTypePtr	polygon = PCB->Data->Layer[layer].Polygon;
			Dimension		distance = (MAX_LINESIZE + pv->Thickness) /2;
			LineTypePtr		*sortedptr;
			Cardinal		i;

				/* get the lowest data pointer of lines which may have
				 * a connection to the PV
				 * ### line->Point1.X <= line->Point2.X ###
				 */
			sortedptr = GetIndexOfLines(pv->X -distance, pv->X +distance,
				layer, &i);
			for (; i; i--, sortedptr++)
				if (!TEST_FLAG(FOUNDFLAG, *sortedptr) &&
					IS_PV_ON_LINE(pv, *sortedptr))
					ADD_LINE_TO_LIST(layer, *sortedptr);

				/* check all polygons */
			for (i = 0; i < PCB->Data->Layer[layer].PolygonN; i++, polygon++)
				if (!TEST_FLAG(FOUNDFLAG, polygon) &&
					IS_PV_IN_POLYGON(pv, polygon))
					ADD_POLYGON_TO_LIST(layer, polygon);
		}
		PVList.Position++;
	}
}

/* ---------------------------------------------------------------------------
 * find all connections between LO at the current list position and new LOs
 */
static void LookupLOConnectionsToLOList(void)
{
	Cardinal	i,
				group;
	Cardinal	lineposition[MAX_LAYER],
				polyposition[MAX_LAYER],
				padposition[2];

		/* copy the current LO list positions; the original data is changed
		 * by 'LookupPVConnectionsToLOList()' which has to check the same
		 * list entries plus the new ones
		 */
	for (i = 0; i< MAX_LAYER; i++)
	{
		lineposition[i] = LineList[i].Position;
		polyposition[i] = PolygonList[i].Position;
	}
	for (i = 0; i < 2; i++)
		padposition[i] = PadList[i].Position;
	
		/* loop over all layergroups */
	for (group = 0; group < MAX_LAYER; group++)
	{
		Cardinal	entry;
		Boolean		done;

			/* loop over all new LOs in the list; recurse until no
			 * more new connections in the layergroup were found
			 */
		do
		{
			for (entry = 0; entry < PCB->LayerGroups.Number[group]; entry++)
			{
				Cardinal	layer = PCB->LayerGroups.Entries[group][entry];
				Cardinal	*position;

					/* be aware that the layer number equal MAX_LAYER
					 * and MAX_LAYER+1 have a special meaning for pads
					 */
				if (layer < MAX_LAYER)
				{
						/* try all new lines */
					position = &lineposition[layer];
					for (; *position < LineList[layer].Number; (*position)++)
						LookupLOConnectionsToLine(
							LINELIST_ENTRY(layer, *position), group);

						/* try all new polygons */
					position = &polyposition[layer];
					for (; *position < PolygonList[layer].Number; (*position)++)
						LookupLOConnectionsToPolygon(
							POLYGONLIST_ENTRY(layer, *position), group);
				}
				else
				{
						/* try all new pads */
					layer -= MAX_LAYER;
					position = &padposition[layer];
					for (; *position < PadList[layer].Number; (*position)++)
						LookupLOConnectionsToPad(
							PADLIST_ENTRY(layer, *position)->Data, group);
				}
			}

				/* check if both lists are handled; the second for-loop
				 * (polygons) may have changed the line list
				 */
			done = True;
			for (entry = 0; entry < PCB->LayerGroups.Number[group]; entry++)
			{
				Cardinal	layer = PCB->LayerGroups.Entries[group][entry];

				if (layer < MAX_LAYER)
					done = done &&
						lineposition[layer] >= LineList[layer].Number &&
						polyposition[layer] >= PolygonList[layer].Number;
				else
				{
					layer -= MAX_LAYER;
					done = done && padposition[layer] >= PadList[layer].Number;
				}
			}
		} while (!done);
	}
}

/* ---------------------------------------------------------------------------
 * searches for new PVs that are connected to NEW LOs on the list
 * This routine updates the position counter of the lists too.
 */
static void LookupPVConnectionsToLOList(void)
{
	Cardinal	layer,
				i,
				limit;
	Dimension	distance;

		/* loop over all layers */
	for (layer = 0; layer < MAX_LAYER; layer++)
	{
			/* do nothing if there are no PV's */
		if (!TotalPV)
		{
			LineList[layer].Position = LineList[layer].Number;
			PolygonList[layer].Position = PolygonList[layer].Number;
			continue;
		}

			/* check all lines */
		while (LineList[layer].Position < LineList[layer].Number)
		{
			LineTypePtr	line = LINELIST_ENTRY(layer, LineList[layer].Position);

				/* get the positions in sorted field to speed up searching
				 * ### line->Point1.X <= line->Point2.X ###
				 * the '+1' in the second call of GetPVByX()
				 * makes sure that 'limit' is realy the position outside the
				 * range
				 */
			distance = (MAX_PINORVIASIZE +line->Thickness) /2;
			i = GetPVByX(MIN(line->Point1.X, line->Point2.X) -distance);
			limit = GetPVByX(MAX(line->Point1.X, line->Point2.X) +distance +1);
			for (; i <= limit; i++)
			{
				PinTypePtr	pv = PVSortedByX[i].Data;

				if (!TEST_FLAG(FOUNDFLAG, pv) && IS_PV_ON_LINE(pv, line))
					ADD_PV_TO_LIST(&PVSortedByX[i]);
			}
			LineList[layer].Position++;
		}

			/* now all polygons */
		while (PolygonList[layer].Position < PolygonList[layer].Number)
		{
			PolygonTypePtr	polygon;

			polygon = POLYGONLIST_ENTRY(layer, PolygonList[layer].Position);

				/* get the positions in sorted field to speed up searching */
			distance = MAX_PINORVIASIZE /2;
			i = GetPVByX(polygon->BoundingBox.X1 -distance);
			limit = GetPVByX(polygon->BoundingBox.X2 +distance +1);
			for (; i <= limit; i++)
			{
				PinTypePtr	pv = PVSortedByX[i].Data;
				
				if (!TEST_FLAG(FOUNDFLAG, pv) && IS_PV_IN_POLYGON(pv, polygon))
					ADD_PV_TO_LIST(&PVSortedByX[i]);
			}
			PolygonList[layer].Position++;
		}
	}

		/* loop over all pad-layers */
	for (layer = 0; layer < 2; layer++)
	{
			/* do nothing if there are no PV's */
		if (!TotalPV)
		{
			PadList[layer].Position = PadList[layer].Number;
			continue;
		}

			/* check all pads; for a detailed description see
			 * the handling of lines in this subroutine
			 */
		while (PadList[layer].Position < PadList[layer].Number)
		{
			PadTypePtr	pad;
			
			pad = PADLIST_ENTRY(layer, PadList[layer].Position)->Data;
			distance = (MAX_PINORVIASIZE +pad->Thickness) /2;
			i = GetPVByX(MIN(pad->Point1.X, pad->Point2.X) -distance);
			limit = GetPVByX(MAX(pad->Point1.X, pad->Point2.X) +distance +1);
			for (; i <= limit; i++)
			{
				PinTypePtr	pv = PVSortedByX[i].Data;

				if (!TEST_FLAG(FOUNDFLAG, pv) && IS_PV_ON_PAD(pv, pad))
					ADD_PV_TO_LIST(&PVSortedByX[i]);
			}
			PadList[layer].Position++;
		}
	}
}

/* ---------------------------------------------------------------------------
 * checks if two lines intersect
 * from news FAQ:
 *
 *  Let A,B,C,D be 2-space position vectors.  Then the directed line
 *  segments AB & CD are given by:
 *
 *      AB=A+r(B-A), r in [0,1]
 *      CD=C+s(D-C), s in [0,1]
 *
 *  If AB & CD intersect, then
 *
 *      A+r(B-A)=C+s(D-C), or
 *
 *      XA+r(XB-XA)=XC+s(XD-XC)
 *      YA+r(YB-YA)=YC+s(YD-YC)  for some r,s in [0,1]
 *
 *  Solving the above for r and s yields
 *
 *          (YA-YC)(XD-XC)-(XA-XC)(YD-YC)
 *      r = -----------------------------  (eqn 1)
 *          (XB-XA)(YD-YC)-(YB-YA)(XD-XC)
 *
 *          (YA-YC)(XB-XA)-(XA-XC)(YB-YA)
 *      s = -----------------------------  (eqn 2)
 *          (XB-XA)(YD-YC)-(YB-YA)(XD-XC)
 *
 *  Let I be the position vector of the intersection point, then
 *
 *      I=A+r(B-A) or
 *
 *      XI=XA+r(XB-XA)
 *      YI=YA+r(YB-YA)
 *
 *  By examining the values of r & s, you can also determine some
 *  other limiting conditions:
 *
 *      If 0<=r<=1 & 0<=s<=1, intersection exists
 *          r<0 or r>1 or s<0 or s>1 line segments do not intersect
 *
 *      If the denominator in eqn 1 is zero, AB & CD are parallel
 *      If the numerator in eqn 1 is also zero, AB & CD are coincident
 *
 *  If the intersection point of the 2 lines are needed (lines in this
 *  context mean infinite lines) regardless whether the two line
 *  segments intersect, then
 *
 *      If r>1, I is located on extension of AB
 *      If r<0, I is located on extension of BA
 *      If s>1, I is located on extension of CD
 *      If s<0, I is located on extension of DC
 *
 *  Also note that the denominators of eqn 1 & 2 are identical.
 *
 *
 *
 *
 *
 */
Boolean LineLineIntersect(LineTypePtr Line1, LineTypePtr Line2)
{
	register	float	dx, dy,
						dx1, dy1,
						s,
						r;

		/* setup some constants */
	dx = (float) (Line1->Point2.X -Line1->Point1.X);
	dy = (float) (Line1->Point2.Y -Line1->Point1.Y);
	dx1 = (float) (Line1->Point1.X -Line2->Point1.X);
	dy1 = (float) (Line1->Point1.Y -Line2->Point1.Y);
	s = dy1*dx - dx1*dy;

	r = dx *(float) (Line2->Point2.Y -Line2->Point1.Y) -
		 dy *(float) (Line2->Point2.X -Line2->Point1.X);

		/* handle parallel lines */
	if (r == 0.0)
	{
			/* at least one of the two end points of one segment
			 * has to have a minimum distance to the other segment
			 *
			 * a first quick check is to see if the distance between
			 * the two lines is less then their half total thickness
			 */
		register	float	distance;

		s = s*s /(dx*dx +dy*dy);
		distance = (float) (Line1->Thickness +Line2->Thickness) /2.0;
		distance *= distance;
		if (s > distance)
			return(False);
		return(
			IsPointOnLine(Line1->Point1.X, Line1->Point1.Y,
				Line1->Thickness/2.0, Line2) ||
			IsPointOnLine(Line1->Point2.X, Line1->Point2.Y,
				Line1->Thickness/2.0, Line2) ||
			IsPointOnLine(Line2->Point1.X, Line2->Point1.Y,
				Line2->Thickness/2.0, Line1) ||
			IsPointOnLine(Line2->Point2.X, Line2->Point2.Y,
				Line2->Thickness/2.0, Line1));
	}
	else
	{
		register	float	radius;

		s /= r;
		r = (dy1 *(float) (Line2->Point2.X -Line2->Point1.X) -
			dx1 *(float) (Line2->Point2.Y -Line2->Point1.Y)) /r;

			/* intersection is at least on AB */
		if (r >= 0.0 && r <= 1.0)
		{
			if (s >= 0.0 && s <= 1.0)
				return(True);

				/* intersection on AB and extension of CD */
			return(s < 0.0 ?
				IsPointOnLine(Line2->Point1.X, Line2->Point1.Y, Line2->Thickness/2.0, Line1) :
				IsPointOnLine(Line2->Point2.X, Line2->Point2.Y, Line2->Thickness/2.0, Line1));
		}

			/* intersection is at least on CD */
		if (s >= 0.0 && s <= 1.0)
		{
				/* intersection on CD and extension of AB */
			return(r < 0.0 ?
				IsPointOnLine(Line1->Point1.X, Line1->Point1.Y, Line1->Thickness/2.0, Line2) :
				IsPointOnLine(Line1->Point2.X, Line1->Point2.Y, Line1->Thickness/2.0, Line2));
		}

			/* no intersection of zero-width lines but maybe of thick lines;
			 * we just have two check the distance of the endpoint of the two
			 * segments which are next to the intersection point
			 */
		radius = (float) ((Line1->Thickness +Line2->Thickness) /2 +1);
		radius *= radius;
		if (s < 0.0)
		{
			dx = (float) ((r < 0.0 ? Line1->Point1.X : Line1->Point2.X) -Line2->Point1.X);
			dy = (float) ((r < 0.0 ? Line1->Point1.Y : Line1->Point2.Y) -Line2->Point1.Y);

			return(dx*dx +dy*dy <= radius);
		}
		dx = (float) ((r < 0.0 ? Line1->Point1.X : Line1->Point2.X) -Line2->Point2.X);
		dy = (float) ((r < 0.0 ? Line1->Point1.Y : Line1->Point2.Y) -Line2->Point2.Y);
		return(dx*dx +dy*dy <= radius);
	}
}

/* ---------------------------------------------------------------------------
 * searches all LOs that are connected to the given line on the given
 * layergroup. All found connections are added to the list
 *
 * the notation that is used is:
 * Xij means Xj at line i
 */
static void LookupLOConnectionsToLine(LineTypePtr Line, Cardinal LayerGroup)
{
	Cardinal	entry;
	Position	xlow, xhigh;

		/* the maximum possible distance */
	xlow = MIN(Line->Point1.X, Line->Point2.X) -
		(Line->Thickness +MAX(MAX_PADSIZE, MAX_LINESIZE)) /2;
	xhigh = MAX(Line->Point1.X, Line->Point2.X) +
		(Line->Thickness +MAX(MAX_PADSIZE, MAX_LINESIZE)) /2;

		/* loop over all layers of the group */
	for (entry = 0; entry < PCB->LayerGroups.Number[LayerGroup]; entry++)
	{
		Cardinal		layer,
						i;

		layer = PCB->LayerGroups.Entries[LayerGroup][entry];

			/* handle normal layers */
		if (layer < MAX_LAYER)
		{
			PolygonTypePtr	polygon;
			LineTypePtr		*sortedptr;

				/* get index of the first line that may match the coordinates */
			sortedptr = GetIndexOfLines(xlow, xhigh, layer, &i);

				/* loop till the end of the data is reached
				 * DONT RETRY LINES THAT HAVE BEEN FOUND
				 */
			for (; i; i--, sortedptr++)
				if (!TEST_FLAG(FOUNDFLAG, *sortedptr) &&
					LineLineIntersect(Line, *sortedptr))
					ADD_LINE_TO_LIST(layer, *sortedptr);

				/* now check all polygons */
			i = 0;
			polygon = PCB->Data->Layer[layer].Polygon;
			for (; i < PCB->Data->Layer[layer].PolygonN; i++, polygon++)
				if (!TEST_FLAG(FOUNDFLAG, polygon) &&
					IsLineInPolygon(Line, polygon))
					ADD_POLYGON_TO_LIST(layer, polygon);
		}
		else
		{
				/* handle special 'pad' layers */
			PadDataTypePtr	*sortedptr;

			layer -= MAX_LAYER;
			sortedptr = GetIndexOfPads(xlow, xhigh, layer, &i);
			for (; i; i--, sortedptr++)
				if (!TEST_FLAG(FOUNDFLAG, (*sortedptr)->Data) &&
					LinePadIntersect(Line, (*sortedptr)->Data))
					ADD_PAD_TO_LIST(layer, *sortedptr);
		}
	}
}

/* ---------------------------------------------------------------------------
 * looks up LOs that are connected to the given polygon
 * on the given layergroup. All found connections are added to the list
 */
static void LookupLOConnectionsToPolygon(PolygonTypePtr Polygon,
	Cardinal LayerGroup)
{
	Cardinal	entry;

		/* loop over all layers of the group */
	for (entry = 0; entry < PCB->LayerGroups.Number[LayerGroup]; entry++)
	{
		Cardinal		layer,
						i;

		layer = PCB->LayerGroups.Entries[LayerGroup][entry];

			/* handle normal layers */
		if (layer < MAX_LAYER)
		{
			PolygonTypePtr	polygon;
			LineTypePtr		*sortedptr;

				/* check all polygons */
			polygon = PCB->Data->Layer[layer].Polygon;
			for (i = 0; i < PCB->Data->Layer[layer].PolygonN; i++, polygon++)
				if (!TEST_FLAG(FOUNDFLAG, polygon) &&
					IsPolygonInPolygon(polygon, Polygon))
					ADD_POLYGON_TO_LIST(layer, polygon);

				/* and now check all lines, first reduce the number of lines by
				 * evaluating the coordinates from the sorted lists
				 */
			sortedptr = GetIndexOfLines(Polygon->BoundingBox.X1 -MAX_LINESIZE/2,
				Polygon->BoundingBox.X2 +MAX_LINESIZE/2, layer, &i);

				/* now check all lines that match the condition */
			for (; i; i--, sortedptr++)
				if (!TEST_FLAG(FOUNDFLAG, *sortedptr) &&
					IsLineInPolygon(*sortedptr, Polygon))
					ADD_LINE_TO_LIST(layer, *sortedptr);
		}
		else
		{
				/* handle special 'pad-layers' */
			PadDataTypePtr	*sortedptr;

			layer -= MAX_LAYER;
			sortedptr = GetIndexOfPads(Polygon->BoundingBox.X1 -MAX_PADSIZE/2,
				Polygon->BoundingBox.X2 +MAX_PADSIZE/2, layer, &i);
			for (; i; i--, sortedptr++)
				if (!TEST_FLAG(FOUNDFLAG, (*sortedptr)->Data) &&
					IsPadInPolygon((*sortedptr)->Data, Polygon))
					ADD_PAD_TO_LIST(layer, *sortedptr);
		}
	}
}

/* ---------------------------------------------------------------------------
 * checks if a line has a connection to a polygon
 *
 * - first check if the line can intersect with the polygon by
 *   evaluating the bounding boxes
 * - check the two end points of the line. If none of them matches
 * - check all segments of the polygon against the line.
 */
static Boolean IsLineInPolygon(LineTypePtr Line, PolygonTypePtr Polygon)
{
	Position	minx = MIN(Line->Point1.X, Line->Point2.X) -Line->Thickness,
				maxx = MAX(Line->Point1.X, Line->Point2.X) +Line->Thickness,
				miny = MIN(Line->Point1.Y, Line->Point2.Y) -Line->Thickness,
				maxy = MAX(Line->Point1.Y, Line->Point2.Y) +Line->Thickness;

	if (minx <= Polygon->BoundingBox.X2 && maxx >= Polygon->BoundingBox.X1 &&
		miny <= Polygon->BoundingBox.Y2 && maxy >= Polygon->BoundingBox.Y1)
	{
		LineType	line;

		if (IsPointInPolygon(Line->Point1.X, Line->Point1.Y,
				Line->Thickness/2.0, Polygon) ||
			IsPointInPolygon(Line->Point2.X, Line->Point2.Y,
				Line->Thickness/2.0, Polygon))
			return(True);

			/* check all lines, start with the connection of the first-last
			 * polygon point; POLYGONPOINT_LOOP decrements the pointers !!!
			 */
		line.Point1 = Polygon->Points[0];
		line.Thickness = 0;
		POLYGONPOINT_LOOP(Polygon,
			line.Point2.X = point->X;
			line.Point2.Y = point->Y;
			if (LineLineIntersect(Line, &line))
				return(True);
			line.Point1.X = line.Point2.X;
			line.Point1.Y = line.Point2.Y;
		);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * checks if a polygon has a connection to a second one
 *
 * First check all points out of P1 against P2 and vice versa.
 * If both fail check all lines of P1 agains the ones of P2
 */
Boolean IsPolygonInPolygon(PolygonTypePtr P1, PolygonTypePtr P2)
{
		/* first check if both bounding boxes intersect */
	if (P1->BoundingBox.X1 <= P2->BoundingBox.X2 &&
		P1->BoundingBox.X2 >= P2->BoundingBox.X1 &&
		P1->BoundingBox.Y1 <= P2->BoundingBox.Y2 &&
		P1->BoundingBox.Y2 >= P2->BoundingBox.Y1)
	{
		LineType	line;

		POLYGONPOINT_LOOP(P1,
			if (IsPointInPolygon(point->X, point->Y, 0, P2))
				return(True);
		);
		POLYGONPOINT_LOOP(P2,
			if (IsPointInPolygon(point->X, point->Y, 0, P1))
				return(True);
		);

			/* check all lines of P1 agains P2;
			 * POLYGONPOINT_LOOP decrements the pointer !!!
			 */
		line.Point1.X = P1->Points[0].X;
		line.Point1.Y = P1->Points[1].Y;
		line.Thickness = 0;
		POLYGONPOINT_LOOP(P1,
			line.Point2.X = point->X;
			line.Point2.Y = point->Y;
			if (IsLineInPolygon(&line, P2))
				return(True);
			line.Point1.X = line.Point2.X;
			line.Point1.Y = line.Point2.Y;
		);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * writes the several names of an element to a file
 */
static void PrintElementNameList(ElementTypePtr Element, FILE *FP)
{
	static	DynamicStringType	cname,
								pname,
								vname;

	CreateQuotedString(&cname, EMPTY(DESCRIPTION_NAME(Element)));
	CreateQuotedString(&pname, EMPTY(NAMEONPCB_NAME(Element)));
	CreateQuotedString(&vname, EMPTY(VALUE_NAME(Element)));
	fprintf(FP, "(%s %s %s)\n", cname.Data, pname.Data, vname.Data);
}

/* ---------------------------------------------------------------------------
 * writes the several names of an element to a file
 */
static void PrintConnectionElementName(ElementTypePtr Element, FILE *FP)
{
	fputs("Element", FP);
	PrintElementNameList(Element, FP);
	fputs("{\n", FP);
}

/* ---------------------------------------------------------------------------
 * prints one {pin,pad,via}/element entry of connection lists
 */
static void PrintConnectionListEntry(char *ObjName, ElementTypePtr Element,
	Boolean FirstOne, FILE *FP)
{
	static	DynamicStringType	oname;

	CreateQuotedString(&oname, ObjName);
	if (FirstOne)
		fprintf(FP, "\t%s\n\t{\n", oname.Data);
	else
	{
		fprintf(FP, "\t\t%s ", oname.Data);
		if (Element)
			PrintElementNameList(Element, FP);
		else
			fputs("(__VIA__)\n", FP);
	}
}

/* ---------------------------------------------------------------------------
 * prints all found connections of a pads to file FP
 * the connections are stacked in 'PadList'
 */
static void PrintPadConnections(Cardinal Layer, FILE *FP, Boolean IsFirst)
{
	Cardinal		i;
	PadDataTypePtr	ptr;

	if (!PadList[Layer].Number)
		return;

		/* the starting pad */
	if (IsFirst)
	{
		ptr = PADLIST_ENTRY(Layer, 0);
		PrintConnectionListEntry(UNKNOWN(ptr->Data->Name), NULL, True, FP);
	}	

		/* we maybe have to start with i=1 if we are handling the
		 * starting-pad itself
		 */
	for (i = IsFirst ? 1 : 0; i < PadList[Layer].Number; i++)
	{
		ptr = PADLIST_ENTRY(Layer, i);
		PrintConnectionListEntry(EMPTY(ptr->Data->Name), ptr->Element,
			False, FP);
	}
}

/* ---------------------------------------------------------------------------
 * prints all found connections of a pin to file FP
 * the connections are stacked in 'PVList'
 */
static void PrintPinConnections(FILE *FP, Boolean IsFirst)
{
	Cardinal		i;
	PVDataTypePtr	pv;

	if (!PVList.Number)
		return;

	if (IsFirst)
	{
			/* the starting pin */
		pv = PVLIST_ENTRY(0);
		PrintConnectionListEntry(EMPTY(pv->Data->Name), NULL, True, FP);
	}

		/* we maybe have to start with i=1 if we are handling the
		 * starting-pin itself
		 */
	for (i = IsFirst ? 1 : 0; i < PVList.Number; i++)
	{
			/* get the elements name or assume that its a via */
		pv = PVLIST_ENTRY(i);
		PrintConnectionListEntry(EMPTY(pv->Data->Name), pv->Element, False, FP);
	}
}

/* ---------------------------------------------------------------------------
 * checks if all lists of new objects are handled
 */
static Boolean ListsEmpty(void)
{
	Boolean	empty;
	int		i;

	empty = (PVList.Position >= PVList.Number);
	for (i = 0; i < MAX_LAYER && empty; i++)
		empty = empty &&
			LineList[i].Position >= LineList[i].Number &&
			PolygonList[i].Position >= PolygonList[i].Number;
	return(empty);
}

/* ---------------------------------------------------------------------------
 * loops till no more connections are found 
 */
static void DoIt(void)
{
	do
	{
			/* lookup connections; these are the steps (2) to (4)
			 * from the description
			 */
		LookupLOConnectionsToPVList();
		LookupLOConnectionsToLOList();
		LookupPVConnectionsToLOList();
		DrawNewConnections();
	} while (!ListsEmpty());
}

/* ---------------------------------------------------------------------------
 * prints all unused pins of an element to file FP
 */
static Boolean PrintAndSelectUnusedPinsAndPadsOfElement(ElementTypePtr Element,
	FILE *FP)
{
			Boolean				first = True;
			Cardinal			number;
	static	DynamicStringType	oname;

		/* check all pins in element */
	PIN_LOOP(Element,
		{
			PVDataTypePtr	entry;

				/* lookup pin in list */
			entry = LookupPVByCoordinates(pin->X, pin->Y);

				/* pin might has bee checked before, add to list if not */
			if (!TEST_FLAG(FOUNDFLAG, entry->Data) && FP)
			{
				int	i;

				ADD_PV_TO_LIST(entry);
				DoIt();

				number = PadList[COMPONENT_LAYER].Number +
					PadList[SOLDER_LAYER].Number +
					PVList.Number;

					/* the pin has no connection if it's the only
					 * list entry; don't count vias
					 */
				for (i = 0; i < PVList.Number; i++)
					if (!PVLIST_ENTRY(i)->Element)
						number--;
				if (number == 1)
				{
						/* output of element name if not already done */
					if (first)
					{
						PrintConnectionElementName(Element, FP);
						first = False;
					}

						/* write name to list and draw selected object */
					CreateQuotedString(&oname, EMPTY(entry->Data->Name));
					fprintf(FP, "\t%s\n", oname.Data);
					SET_FLAG(SELECTEDFLAG, entry->Data);
					DrawPin(entry->Data);
				}

					/* reset found objects for the next pin */
				if (PrepareNextLoop(FP))
					return(True);
			}
		}
	);

		/* check all pads in element */
	PAD_LOOP(Element,
		{
			PadDataTypePtr	entry;

				/* lookup pin in list */
			entry = LookupPadByAddress(pad);

				/* pin might has bee checked before, add to list if not */
			if (!TEST_FLAG(FOUNDFLAG, entry->Data) && FP)
			{
				int	i;

				ADD_PAD_TO_LIST(TEST_FLAG(ONSOLDERFLAG, pad) ?
					SOLDER_LAYER : COMPONENT_LAYER, entry);
				DoIt();

				number = PadList[COMPONENT_LAYER].Number +
					PadList[SOLDER_LAYER].Number +
					PVList.Number;

					/* the pin has no connection if it's the only
					 * list entry; don't count vias
					 */
				for (i = 0; i < PVList.Number; i++)
					if (!PVLIST_ENTRY(i)->Element)
						number--;

				if (number == 1)
				{
						/* output of element name if not already done */
					if (first)
					{
						PrintConnectionElementName(Element, FP);
						first = False;
					}

						/* write name to list and draw selected object */
					CreateQuotedString(&oname, EMPTY(entry->Data->Name));
					fprintf(FP, "\t%s\n", oname.Data);
					SET_FLAG(SELECTEDFLAG, entry->Data);
					DrawPad(entry->Data);
				}

					/* reset found objects for the next pin */
				if (PrepareNextLoop(FP))
					return(True);
			}
		}
	);

		/* print seperator if element has unused pins or pads */
	if (!first)
	{
		fputs("}\n\n", FP);
		SEPERATE(FP);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * resets some flags for looking up the next pin/pad
 */
static Boolean PrepareNextLoop(FILE *FP)
{
	Cardinal	layer;

		/* reset found LOs for the next pin */
	for (layer = 0; layer < MAX_LAYER; layer++)
	{
		LineList[layer].Position = LineList[layer].Number = 0;
		PolygonList[layer].Position = PolygonList[layer].Number = 0;
	}

		/* reset found pads */
	for (layer = 0; layer < 2; layer++)
		PadList[layer].Position = PadList[layer].Number = 0;

		/* reset PVs */
	PVList.Number = PVList.Position = 0;

		/* check if abort buttons has been pressed */
	if (CheckAbort())
	{
		if (FP)
			fputs("\n\nABORTED...\n", FP);
		return(True);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * finds all connections to the pins of the passed element.
 * The result is written to filedescriptor 'FP'
 * Returns True if operation was aborted
 */
static Boolean PrintElementConnections(ElementTypePtr Element,
	FILE *FP)
{
	PrintConnectionElementName(Element, FP);

		/* check all pins in element */
	PIN_LOOP(Element,
		{
			PVDataTypePtr	entry;

				/* lookup pin in list */
			entry = LookupPVByCoordinates(pin->X, pin->Y);

				/* pin might have been checked before, add to list if not */
			if (TEST_FLAG(FOUNDFLAG, entry->Data))
			{
				PrintConnectionListEntry(EMPTY(pin->Name), NULL, True, FP);
				fputs("\t\t__CHECKED_BEFORE__\n\t}\n", FP);
				continue;
			}
			ADD_PV_TO_LIST(entry);
			DoIt();

				/* printout all found connections */
			PrintPinConnections(FP, True);
			PrintPadConnections(COMPONENT_LAYER, FP, False);
			PrintPadConnections(SOLDER_LAYER, FP, False);
			fputs("\t}\n", FP);

			if (PrepareNextLoop(FP))
				return(True);
		}
	);

		/* check all pads in element */
	PAD_LOOP(Element,
		{
			PadDataTypePtr	entry;
			Cardinal		layer;

				/* pad might has bee checked before, add to list if not */
			if (TEST_FLAG(FOUNDFLAG, pad))
			{
				PrintConnectionListEntry(EMPTY(pad->Name), NULL, True, FP);
				fputs("\t\t__CHECKED_BEFORE__\n\t}\n", FP);
				continue;
			}
			entry = LookupPadByAddress(pad);
			layer = TEST_FLAG(ONSOLDERFLAG, pad) ? SOLDER_LAYER : COMPONENT_LAYER;
			ADD_PAD_TO_LIST(layer, entry);
			DoIt();

				/* print all found connections */
			PrintPadConnections(layer, FP, True);
			PrintPadConnections(layer == COMPONENT_LAYER ? SOLDER_LAYER : COMPONENT_LAYER,
				FP, False);
			PrintPinConnections(FP, False);
			fputs("\t}\n", FP);

			if (PrepareNextLoop(FP))
				return(True);
		}
	);
	fputs("}\n\n", FP);
	return(False);
}

/* ---------------------------------------------------------------------------
 * draws all new connections which have been found since the
 * routine was called the last time
 */
static void DrawNewConnections(void)
{
	int		i;

		/* decrement 'i' to keep layerstack order */
	for (i = MAX_LAYER-1; i != -1; i--)
	{
		Cardinal	layer = LayerStack[i],
					position;

		if (PCB->Data->Layer[layer].On)
		{
				/* draw all new lines */
			position = LineList[layer].DrawPosition;
			for (; position < LineList[layer].Number; position++)
				DrawLine(&PCB->Data->Layer[layer],
					LINELIST_ENTRY(layer, position));
			LineList[layer].DrawPosition = LineList[layer].Number;
			
				/* draw all new polygons */
			position = PolygonList[layer].DrawPosition;
			for (; position < PolygonList[layer].Number; position++)
				DrawPolygon(&PCB->Data->Layer[layer],
					POLYGONLIST_ENTRY(layer, position));
			PolygonList[layer].DrawPosition = PolygonList[layer].Number;
		}
	}

		/* draw all new pads */
	if (PCB->PinOn)
		for (i = 0; i < 2; i++)
		{
			Cardinal	position = PadList[i].DrawPosition;

			for (; position < PadList[i].Number; position++)
				DrawPad(PADLIST_ENTRY(i, position)->Data);
			PadList[i].DrawPosition = PadList[i].Number;
		}

		/* draw all new PVs; 'PVList' holds a list of pointers to the
		 * sorted array pointers to PV data
		 */
	while (PVList.DrawPosition < PVList.Number)
	{
		PVDataTypePtr	pv = PVLIST_ENTRY(PVList.DrawPosition);

		if (TEST_FLAG(PINFLAG, pv->Data))
		{
			if (PCB->PinOn)
				DrawPin(pv->Data);
		}
		else
			if (PCB->ViaOn)
				DrawVia(pv->Data);
		PVList.DrawPosition++;
	}
}

/* ---------------------------------------------------------------------------
 * find all connections to pins within one element
 */
void LookupElementConnections(ElementTypePtr Element, FILE *FP)
{
		/* reset all currently marked connections */
	CreateAbortDialog();
	ResetConnections();
	InitConnectionLookup();
	PrintElementConnections(Element, FP);
	SetChangedFlag(True);
	EndDialog(Popup);
	if (Settings.RingBellWhenFinished)
		XBell(Dpy, Settings.Volume);
	FreeConnectionLookupMemory();
	RedrawOutput();
}

/* ---------------------------------------------------------------------------
 * find all connections to pins of all element
 */
void LookupConnectionsToAllElements(FILE *FP)
{
		/* reset all currently marked connections */
	CreateAbortDialog();
	ResetConnections();
	InitConnectionLookup();

	ELEMENT_LOOP(PCB->Data,
			/* break if abort dialog returned True */
		if (PrintElementConnections(element, FP))
			break;
		SEPERATE(FP);
		if (Settings.ResetAfterElement && n != 1)
			ResetConnections();
	);
	EndDialog(Popup);
	if (Settings.RingBellWhenFinished)
		XBell(Dpy, Settings.Volume);
	FreeConnectionLookupMemory();
	RedrawOutput();
}

/* ---------------------------------------------------------------------------
 * looks up all connections from the object at the given coordinates
 * the 'FOUNDFLAG' is set for all objects found
 */
void LookupConnection(Position X, Position Y)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* check if there are any pins or vias at that position */
	type = SearchObjectByPosition(LOOKUP_TYPES, &ptr1, &ptr2, &ptr3, X, Y);
	if (type == NO_TYPE)
		return;

	InitConnectionLookup();

		/* now add the object to the appropriate list and start scanning
		 * This is step (1) from the description
		 */
	switch(type)
	{
		case PIN_TYPE:
		case VIA_TYPE:
		{
			PVDataTypePtr	entry;

				/* use center coordinates else LookupPVByCoordinates()
				 * may fail;
				 * bug-fix by Ulrich Pegelow (ulrpeg@bigcomm.gun.de)
				 */
			entry = LookupPVByCoordinates(((PinTypePtr)ptr2)->X,
				((PinTypePtr)ptr2)->Y);
			ADD_PV_TO_LIST(entry);
			break;
		}

		case LINE_TYPE:
		{
			int		layer = GetLayerNumber(PCB->Data, (LayerTypePtr) ptr1);

			ADD_LINE_TO_LIST(layer, (LineTypePtr) ptr2);
			break;
		}

		case POLYGON_TYPE:
		{
			int		layer = GetLayerNumber(PCB->Data, (LayerTypePtr) ptr1);

			ADD_POLYGON_TO_LIST(layer, (PolygonTypePtr) ptr2);
			break;
		}

		case PAD_TYPE:
		{
			PadTypePtr		pad = (PadTypePtr) ptr2;
			PadDataTypePtr	entry;

			entry = LookupPadByAddress(pad);
			ADD_PAD_TO_LIST(
				TEST_FLAG(ONSOLDERFLAG, pad) ? SOLDER_LAYER : COMPONENT_LAYER,
				entry);
			break;
		}

		default:
			return;
	}
	DoIt();

		/* we are done */
	if (Settings.RingBellWhenFinished)
		XBell(Dpy, Settings.Volume);
	FreeConnectionLookupMemory();
}

/* ---------------------------------------------------------------------------
 * find all unused pins of all element
 */
void LookupUnusedPins(FILE *FP)
{
		/* reset all currently marked connections */
	CreateAbortDialog();
	ResetConnections();
	InitConnectionLookup();

	ELEMENT_LOOP(PCB->Data,
			/* break if abort dialog returned True;
			 * passing NULL as filedescriptor discards the normal output
			 */
		if (PrintAndSelectUnusedPinsAndPadsOfElement(element, FP))
			break;
	);
	EndDialog(Popup);

	ResetConnections();
	if (Settings.RingBellWhenFinished)
		XBell(Dpy, Settings.Volume);
	FreeConnectionLookupMemory();
	RedrawOutput();
}

/* ---------------------------------------------------------------------------
 * resets all used flags of pins and vias
 */
void ResetFoundPinsViasAndPads(void)
{
	VIA_LOOP(PCB->Data, CLEAR_FLAG(FOUNDFLAG, via); );
	ELEMENT_LOOP(PCB->Data,
		PIN_LOOP(element, CLEAR_FLAG(FOUNDFLAG, pin); );
		PAD_LOOP(element, CLEAR_FLAG(FOUNDFLAG, pad); );
	);
	SetChangedFlag(True);
}

/* ---------------------------------------------------------------------------
 * resets all used flags of LOs
 */
void ResetFoundLinesAndPolygons(void)
{
	ALLLINE_LOOP(PCB->Data, CLEAR_FLAG(FOUNDFLAG, line); );
	ALLPOLYGON_LOOP(PCB->Data, CLEAR_FLAG(FOUNDFLAG, polygon); );
	SetChangedFlag(True);
}

/* ---------------------------------------------------------------------------
 * resets all found connections
 */
static void ResetConnections(void)
{
	ResetFoundPinsViasAndPads();
	ResetFoundLinesAndPolygons();
	RedrawOutput();
}
