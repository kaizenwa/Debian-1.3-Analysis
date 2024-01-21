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

static	char	*rcsid = "$Id: undo.c,v 143.1 1996/09/16 09:09:00 nau Exp $";

/* functions used to undo operations
 *
 * Description:
 * There are two lists which hold
 *   - information about a command
 *   - data of removed objects
 * Both lists are organized as first-in-last-out which means that the undo
 * list can always use the last entry of the remove list.
 * A serial number is incremented whenever an operation is completed.
 * An operation itself may consist of several basic instructions.
 * E.g.: removing all selected objects is one operation with exactly one
 * serial number even if the remove function is called several times.
 *
 * a lock flag ensures that no infinite loops occure
 */

#include <memory.h>

#include "global.h"

#include "buffer.h"
#include "change.h"
#include "create.h"
#include "data.h"
#include "dialog.h"
#include "draw.h"
#include "error.h"
#include "insert.h"
#include "mymem.h"
#include "misc.h"
#include "move.h"
#include "remove.h"
#include "rotate.h"
#include "search.h"
#include "set.h"
#include "undo.h"

/* ---------------------------------------------------------------------------
 * some local data types
 */
typedef struct					/* information about a change command */
{
	char			*Name;
} ChangeNameType, *ChangeNameTypePtr;

typedef struct					/* information about a move command */
{
	Position		DX,			/* movement vector */
					DY;
} MoveType, *MoveTypePtr;

typedef struct					/* information about removed polygon points */
{
	PointType		Point;		/* data */
	Cardinal		Index;		/* index in a polygons array of points */
} RemovedPointType, *RemovedPointTypePtr;

typedef struct					/* informstion about rotation */
{
	Position		CenterX,	/* center of rotation */
					CenterY;
	BYTE			Steps;		/* number of steps */
} RotateType, *RotateTypePtr;

typedef struct					/* information about moves between layers */
{
	Cardinal		OriginalLayer;	/* the index of the original layer */
} MoveToLayerType, *MoveToLayerTypePtr;

typedef	struct					/* holds information about an operation */
{
	int				Serial,		/* serial number of operation */
					Type,		/* type of operation */
					ID;			/* object ID */
	union						/* some additional information */
	{
		ChangeNameType		ChangeName;
		MoveType			Move;
		RemovedPointType	RemovedPoint;
		RotateType			Rotate;
		MoveToLayerType		MoveToLayer;
	} Data;
} UndoListType, *UndoListTypePtr;

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	DataTypePtr		RemoveList;		/* list of removed objects */
static	UndoListTypePtr	UndoList;		/* list of operations */
static	int				Serial,			/* serial number */
						SavedSerial;
static	size_t			UndoN,			/* number of entries */
						UndoMax;
static	Boolean			Locked = False;	/* do not add entries if */
										/* flag is set; prevents from */
										/* infinite loops */

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	UndoListTypePtr	GetUndoSlot(int, int);
static	void			DrawRecoveredObject(int, void *, void *, void *);
static	Boolean			UndoRotate(UndoListTypePtr);
static	Boolean			UndoChangeName(UndoListTypePtr);
static	Boolean			UndoCopyOrCreate(UndoListTypePtr);
static	Boolean			UndoMove(UndoListTypePtr);
static	Boolean			UndoRemove(UndoListTypePtr);
static	Boolean			UndoRemovePoint(UndoListTypePtr);
static	Boolean			UndoInsertPoint(UndoListTypePtr);
static	Boolean			UndoMoveToLayer(UndoListTypePtr);

/* ---------------------------------------------------------------------------
 * adds a command plus some data to the undo list
 */
static UndoListTypePtr GetUndoSlot(int CommandType, int ID)
{
			UndoListTypePtr	ptr;
	static	size_t			limit = UNDO_WARNING_SIZE;

		/* allocate memory */
	if (UndoN >= UndoMax)
	{
		size_t	size;

		UndoMax += STEP_UNDOLIST;
		size = UndoMax *sizeof(UndoListType);
		UndoList = (UndoListTypePtr) MyRealloc(UndoList, size,
			"AddCommandToUndoList()");
		memset(&UndoList[UndoN], 0, STEP_REMOVELIST *sizeof(UndoListType));

			/* ask user to flush the table because of it's size */
		if (size > limit)
		{
			limit = (size /UNDO_WARNING_SIZE +1) *UNDO_WARNING_SIZE;
			Message("size of 'undo-list' exceeds %li kb\n",
				(long) (size >> 10));
		}
	}

		/* copy typefield and serial number to the list */
	ptr = &UndoList[UndoN++]; 
	ptr->Type = CommandType;
	ptr->ID = ID;
	ptr->Serial = Serial;
	return(ptr);
}

/* ---------------------------------------------------------------------------
 * redraws the recovered object
 */
static void DrawRecoveredObject(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	if (Type & (LINE_TYPE | TEXT_TYPE | POLYGON_TYPE))
	{
		LayerTypePtr	layer;

		layer= &PCB->Data->Layer[GetLayerNumber(RemoveList,(LayerTypePtr)Ptr1)];
		if (layer->On)
			switch(Type)
			{
				case LINE_TYPE:
					DrawLine(layer, (LineTypePtr) Ptr2);
					break;

				case TEXT_TYPE:
					DrawText(layer, (TextTypePtr) Ptr2);
					break;

				case POLYGON_TYPE:
					DrawPolygon(layer, (PolygonTypePtr) Ptr2);
					break;
			}
	}
	else
		switch(Type)
		{
			case ELEMENT_TYPE:
			{
				ElementTypePtr	element = (ElementTypePtr) Ptr2;

				if (PCB->ElementOn &&
					((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
					PCB->InvisibleObjectsOn))
					DrawElement(element);
				break;
			}

			case ELEMENTNAME_TYPE:
			{
				ElementTypePtr	element = (ElementTypePtr) Ptr1;

				if (PCB->ElementOn &&
					((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
					PCB->InvisibleObjectsOn))
					DrawElementName(element);
				break;
			}

			case PIN_TYPE:
				if (PCB->PinOn)
					DrawPin((PinTypePtr) Ptr2);
				break;

			case PAD_TYPE:
				if (PCB->PinOn &&
					((TEST_FLAG(ONSOLDERFLAG, (PadTypePtr) Ptr2) != 0) == SWAP_IDENT ||
					PCB->InvisibleObjectsOn))
					DrawPad((PadTypePtr) Ptr2);
				break;

			case VIA_TYPE:
				if (PCB->ViaOn)
					DrawVia((PinTypePtr) Ptr2);
				break;
		}
}

/* ---------------------------------------------------------------------------
 * recovers an object from a 'rotate' operation
 * returns True if anything has been recovered
 */
static Boolean UndoRotate(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry by it's ID */
	type = SearchObjectByID(PCB->Data, &ptr1, &ptr2, &ptr3, Entry->ID);
	if (type != NO_TYPE)
	{
		RotateObject(type, ptr1, ptr2, ptr3,
			Entry->Data.Rotate.CenterX, Entry->Data.Rotate.CenterY,
			(4-Entry->Data.Rotate.Steps) & 0x03);
		return(True);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * recovers an object from a 'change name' operation
 * returns True if anything has been recovered
 */
static Boolean UndoChangeName(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry by it's ID */
	type = SearchObjectByID(PCB->Data, &ptr1, &ptr2, &ptr3, Entry->ID);
	if (type != NO_TYPE)
	{
		SaveFree(ChangeObjectName(type, ptr1, ptr2, ptr3,
			Entry->Data.ChangeName.Name));
		return(True);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * recovers an object from a 'copy' or 'create' operation
 * returns True if anything has been recovered
 */
static Boolean UndoCopyOrCreate(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry by it's ID */
	type = SearchObjectByID(PCB->Data, &ptr1, &ptr2, &ptr3, Entry->ID);
	if (type != NO_TYPE)
	{
		DestroyObject(type, ptr1, ptr2, ptr3);
		return(True);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * recovers an object from a 'move' operation
 * returns True if anything has been recovered
 */
static Boolean UndoMove(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry by it's ID */
	type = SearchObjectByID(PCB->Data, &ptr1, &ptr2, &ptr3, Entry->ID);
	if (type != NO_TYPE)
	{
		MoveObject(type, ptr1, ptr2, ptr3,
			-Entry->Data.Move.DX, -Entry->Data.Move.DY);
		return(True);
	}
	return(False);
}

/* ----------------------------------------------------------------------
 * recovers an object from a 'remove' operation
 * returns True if anything has been recovered
 */
static Boolean UndoRemove(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry by it's ID */
	type = SearchObjectByID(RemoveList, &ptr1, &ptr2, &ptr3, Entry->ID);
	if (type != NO_TYPE)
	{
		DrawRecoveredObject(type, ptr1, ptr2, ptr3);
		MoveObjectToBuffer(PCB->Data, RemoveList, type, ptr1, ptr2, ptr3);
		return(True);
	}
	return(False);
}

/* ----------------------------------------------------------------------
 * recovers an object from a 'move to another layer' operation
 * returns True if anything has been recovered
 */
static Boolean UndoMoveToLayer(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry by it's ID */
	type = SearchObjectByID(PCB->Data, &ptr1, &ptr2, &ptr3, Entry->ID);
	if (type != NO_TYPE)
	{
		MoveObjectToLayer(type, ptr1, ptr2, ptr3, 
			&PCB->Data->Layer[Entry->Data.MoveToLayer.OriginalLayer]);
		return(True);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * recovers a removed polygon point
 * returns true on success
 */
static Boolean UndoRemovePoint(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry (polygon not point was saved) by it's ID */
	type = SearchObjectByID(PCB->Data, &ptr1, &ptr2, &ptr3, Entry->ID);
	switch(type)
	{
		case POLYGON_TYPE:		/* restore the removed point */
		{
			LayerTypePtr	layer = (LayerTypePtr) ptr1;
			PolygonTypePtr	polygon = (PolygonTypePtr) ptr2;

				/* recover the point */
			if (layer->On)
				ErasePolygon(polygon);
			InsertPointIntoObject(POLYGON_TYPE, layer, polygon,
				&polygon->Points[Entry->Data.RemovedPoint.Index],
				Entry->Data.RemovedPoint.Point.X,
				Entry->Data.RemovedPoint.Point.Y);
			if (layer->On)
				DrawPolygon(layer, polygon);
			return(True);
		}

		default:
			return(False);
	}
}

/* ---------------------------------------------------------------------------
 * recovers an inserted polygon point
 * returns true on success
 */
static Boolean UndoInsertPoint(UndoListTypePtr Entry)
{
	void	*ptr1, *ptr2, *ptr3;
	int		type;

		/* lookup entry by it's ID */
	type = SearchObjectByID(PCB->Data, &ptr1, &ptr2, &ptr3, Entry->ID);
	switch(type)
	{
		case POLYGONPOINT_TYPE:		/* removes an inserted polygon point */
		{
			DestroyObject(POLYGONPOINT_TYPE, (LayerTypePtr) ptr1,
				(PolygonTypePtr) ptr2, (PointTypePtr) ptr3);
			return(True);
		}

		default:
			return(False);
	}
}

/* ---------------------------------------------------------------------------
 * undo of any 'hard to recover' operation
 * supported are MOVE, CREATE, REMOVE and CHANGENAME
 *
 * returns True if anything is has been done
 */
Boolean Undo(void)
{
	UndoListTypePtr	ptr;
	Boolean			changed = False;

	if (!UndoN)
		return(False);

		/* lock undo module to prevent from loops
		 * and loop over all entries with the same serial number
		 */
	LockUndo();
	ptr = &UndoList[UndoN -1];
	Serial = ptr->Serial;
	for (; UndoN && ptr->Serial == Serial; ptr--, UndoN--)
		switch(ptr->Type)
		{
			case UNDO_CHANGENAME:
				changed |= UndoChangeName(ptr);
				break;

			case UNDO_CREATE:
			case UNDO_COPY:
				changed |= UndoCopyOrCreate(ptr);
				break;

			case UNDO_MOVE:
				changed |= UndoMove(ptr);
				break;

			case UNDO_REMOVE:
				changed |= UndoRemove(ptr);
				break;

			case UNDO_REMOVE_POINT:
				changed |= UndoRemovePoint(ptr);
				break;

			case UNDO_INSERT_POINT:
				changed |= UndoInsertPoint(ptr);
				break;

			case UNDO_ROTATE:
				changed |= UndoRotate(ptr);
				break;

			case UNDO_MOVETOLAYER:
				changed |= UndoMoveToLayer(ptr);
		}

		/* release lock */
	UnlockUndo();
	return(changed);
}

/* ---------------------------------------------------------------------------
 * restores the serial number of the undo list
 */
void RestoreUndoSerialNumber(void)
{
	Serial = SavedSerial;
}

/* ---------------------------------------------------------------------------
 * saves the serial number of the undo list
 */
void SaveUndoSerialNumber(void)
{
	SavedSerial = Serial;
}

/* ---------------------------------------------------------------------------
 * increments the serial number of the undo list
 * it's not done automatically because some operations perform more
 * than one request with the same serial #
 */
void IncrementUndoSerialNumber(void)
{
	if (!Locked)
		Serial++;
}

/* ---------------------------------------------------------------------------
 * releases memory of the undo- and remove list
 */
void ClearUndoList(Boolean Force)
{
	UndoListTypePtr		undo;

	if (UndoN && (Force || ConfirmDialog("OK to clear 'undo' buffer ?")))
	{
			/* release memory allocated by objects in undo list */
		for (undo = UndoList; UndoN; undo++, UndoN--)
			if (undo->Type == UNDO_CHANGENAME)
				SaveFree(undo->Data.ChangeName.Name);
		MyFree((char **) &UndoList);
		FreeDataMemory(RemoveList);

			/* reset some counters */
		UndoN = UndoMax = 0;
	}

		/* reset counter in any case */
	Serial = 0;
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of rotated objects
 */
void AddObjectToRotateUndoList(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position CenterX, Position CenterY, BYTE Steps)
{
	UndoListTypePtr	undo;

	if (!Locked)
	{
		undo = GetUndoSlot(UNDO_ROTATE, OBJECT_ID(Ptr3));
		undo->Data.Rotate.CenterX = CenterX;
		undo->Data.Rotate.CenterY = CenterY;
		undo->Data.Rotate.Steps = Steps;
	}
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of removed objects and removes it from
 * the current PCB
 */
void MoveObjectToRemoveUndoList(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	UndoListTypePtr	undo;

	if (!Locked)
	{
		if (!RemoveList)
			RemoveList = CreateNewBuffer();

		undo = GetUndoSlot(UNDO_REMOVE, OBJECT_ID(Ptr3));
		MoveObjectToBuffer(RemoveList, PCB->Data, Type, Ptr1, Ptr2, Ptr3);
	}
}
 
/* ---------------------------------------------------------------------------
 * adds an object to the list of removed polygon/... points
 */
void AddObjectToRemovePointUndoList(int Type,
	void *Ptr1, void *Ptr2, void *Ptr3)
{
	UndoListTypePtr	undo;
	PolygonTypePtr	polygon = (PolygonTypePtr) Ptr2;
	PointTypePtr	pnt = (PointTypePtr) Ptr3;

	if (!Locked)
	{
			/* save the ID of the parent object; else it will be
			 * impossible to recover the point
			 */
		undo = GetUndoSlot(UNDO_REMOVE_POINT, OBJECT_ID(polygon));
		undo->Data.RemovedPoint.Point = *pnt;

		switch(Type)
		{
			case POLYGONPOINT_TYPE:
			{
					/* get index of previous point in array */
				POLYGONPOINT_LOOP(polygon,
					if (point == pnt)
					{
							/* use n-2 because the point isn't removed yet */
						undo->Data.RemovedPoint.Index = (n ?
							n-1 : polygon->PointN-2);
						break;
					}
				);
			}
			break;
		}
	}
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of inserted polygon/... points
 */
void AddObjectToInsertPointUndoList(int Type,
	void *Ptr1, void *Ptr2, void *Ptr3)
{
	UndoListTypePtr	undo;

	if (!Locked)
		undo = GetUndoSlot(UNDO_INSERT_POINT, OBJECT_ID(Ptr3));
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of copied objects
 */
void AddObjectToCopyUndoList(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	UndoListTypePtr	undo;

	if (!Locked)
		undo = GetUndoSlot(UNDO_COPY, OBJECT_ID(Ptr3));
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of moved objects
 */
void AddObjectToMoveUndoList(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position DX, Position DY)
{
	UndoListTypePtr	undo;

	if (!Locked)
	{
		undo = GetUndoSlot(UNDO_MOVE, OBJECT_ID(Ptr3));
		undo->Data.Move.DX = DX;
		undo->Data.Move.DY = DY;
	}
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of objects with changed names
 */
void AddObjectToChangeNameUndoList(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	char *OldName)
{
	UndoListTypePtr	undo;

	if (!Locked)
	{
		undo = GetUndoSlot(UNDO_CHANGENAME, OBJECT_ID(Ptr3));
		undo->Data.ChangeName.Name = OldName;
	}
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of objects with changed names
 */
void AddObjectToMoveToLayerUndoList(int Type, void *Ptr1, void *Ptr2,
	void *Ptr3)
{
	UndoListTypePtr	undo;

	if (!Locked)
	{
		undo = GetUndoSlot(UNDO_MOVETOLAYER, OBJECT_ID(Ptr3));
		undo->Data.MoveToLayer.OriginalLayer =
			GetLayerNumber(PCB->Data, (LayerTypePtr) Ptr1);
	}
}

/* ---------------------------------------------------------------------------
 * adds an object to the list of created objects
 */
void AddObjectToCreateUndoList(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	UndoListTypePtr	undo;

	if (!Locked)
		undo = GetUndoSlot(UNDO_CREATE, OBJECT_ID(Ptr3));
}

/* ---------------------------------------------------------------------------
 * set lock flag
 */
void LockUndo(void)
{
	Locked = True;
}

/* ---------------------------------------------------------------------------
 * reset lock flag
 */
void UnlockUndo(void)
{
	Locked = False;
}
