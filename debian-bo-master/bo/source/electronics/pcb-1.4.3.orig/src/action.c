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

static	char	*rcsid = "$Id: action.c,v 143.1 1996/09/16 09:08:26 nau Exp $";

/* action routines for output window
 */

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <sys/types.h>

#include "global.h"

#include "action.h"
#include "buffer.h"
#include "change.h"
#include "command.h"
#include "control.h"
#include "copy.h"
#include "create.h"
#include "crosshair.h"
#include "data.h"
#include "dialog.h"
#include "draw.h"
#include "error.h"
#include "file.h"
#include "fileselect.h"
#include "find.h"
#include "groundplane.h"
#include "insert.h"
#include "lgdialog.h"
#include "mymem.h"
#include "misc.h"
#include "mirror.h"
#include "move.h"
#include "pinout.h"
#include "polygon.h"
#include "printdialog.h"
#include "remove.h"
#include "rotate.h"
#include "rubberband.h"
#include "search.h"
#include "select.h"
#include "set.h"
#include "undo.h"

#include <X11/cursorfont.h>

/* ---------------------------------------------------------------------------
 * some local types
 */
typedef enum {
	F_AddSelected,
	F_All,
	F_AllConnections,
	F_AllUnusedPins,
	F_Block,
	F_Description,
	F_Center,
	F_Clear,
	F_ClearAndRedraw,
	F_ClearList,
	F_Close,
	F_Connection,
	F_Copy,
	F_ElementByName,
	F_ElementConnections,
	F_ElementToBuffer,
	F_Find,
	F_Grid,
	F_InsertPoint,
	F_Layer,
	F_Layout,
	F_LayoutAs,
	F_LayoutToBuffer,
	F_Line,
	F_LineSize,
	F_Mirror,
	F_Move,
	F_NameOnPCB,
	F_None,
	F_Notify,
	F_Object,
	F_ObjectByName,
	F_PasteBuffer,
	F_PadByName,
	F_PinByName,
	F_PinOrPadName,
	F_Pinout,
	F_Polygon,
	F_PreviousPoint,
	F_Rectangle,
	F_Redraw,
	F_Remove,
	F_RemoveSelected,
	F_Reset,
	F_ResetLinesAndPolygons,
	F_ResetPinsViasAndPads,
	F_Restore,
	F_Rotate,
	F_RubberbandMove,
	F_Save,
	F_SelectedElements,
	F_SelectedLines,
	F_SelectedObjects,
	F_SelectedPads,
	F_SelectedPins,
	F_SelectedTexts,
	F_SelectedVias,
	F_SelectMembers,
	F_Text,
	F_TextByName,
	F_TextScale,
	F_ToggleAllDirections,
	F_ToggleGrid,
	F_ToggleObject,
	F_Via,
	F_ViaByName,
	F_Value,
	F_ViaDrillingHole,
	F_ViaSize,
	F_Zoom
} FunctionID;

typedef struct					/* used to identify subfunctions */
{
	char			*Identifier;
	FunctionID		ID;
} FunctionType, *FunctionTypePtr;

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Boolean			IgnoreMotionEvents = 0;
static	String			MovePointerCommand[] = { "MovePointer", "0", "0" };
static	FunctionType	Functions[] = {
	{ "AddSelected", F_AddSelected },
	{ "All", F_All },
	{ "AllConnections", F_AllConnections },
	{ "AllUnusedPins", F_AllUnusedPins },
	{ "Block", F_Block },
	{ "Description", F_Description },
	{ "Center", F_Center },
	{ "Clear", F_Clear},
	{ "ClearAndRedraw", F_ClearAndRedraw},
	{ "ClearList", F_ClearList },
	{ "Close", F_Close },
	{ "Connection", F_Connection },
	{ "Copy", F_Copy },
	{ "ElementByName", F_ElementByName },
	{ "ElementConnections", F_ElementConnections },
	{ "ElementToBuffer", F_ElementToBuffer },
	{ "Find", F_Find },
	{ "Grid", F_Grid },
	{ "InsertPoint", F_InsertPoint },
	{ "Layer", F_Layer },
	{ "Layout", F_Layout },
	{ "LayoutAs", F_LayoutAs },
	{ "LayoutToBuffer", F_LayoutToBuffer },
	{ "Line", F_Line },
	{ "LineSize", F_LineSize },
	{ "Mirror", F_Mirror },
	{ "Move", F_Move },
	{ "NameOnPCB", F_NameOnPCB },
	{ "None", F_None },
	{ "Notify", F_Notify },
	{ "Object", F_Object },
	{ "ObjectByName", F_ObjectByName },
	{ "PasteBuffer", F_PasteBuffer },
	{ "PadByName", F_PadByName },
	{ "PinByName", F_PinByName },
	{ "PinOrPadName", F_PinOrPadName },
	{ "Pinout", F_Pinout },
	{ "Polygon", F_Polygon },
	{ "PreviousPoint", F_PreviousPoint},
	{ "Rectangle", F_Rectangle },
	{ "Redraw", F_Redraw },
	{ "Remove", F_Remove },
	{ "RemoveSelected", F_RemoveSelected },
	{ "Reset", F_Reset },
	{ "ResetLinesAndPolygons", F_ResetLinesAndPolygons },
	{ "ResetPinsViasAndPads", F_ResetPinsViasAndPads },
	{ "Restore", F_Restore },
	{ "Rotate", F_Rotate },
	{ "RubberbandMove", F_RubberbandMove },
	{ "Save", F_Save },
	{ "SelectedElements", F_SelectedElements },
	{ "SelectedLines", F_SelectedLines },
	{ "SelectedObjects", F_SelectedObjects },
	{ "SelectedPins", F_SelectedPins },
	{ "SelectedPads", F_SelectedPads },
	{ "SelectedTexts", F_SelectedTexts },
	{ "SelectedVias", F_SelectedVias },
	{ "SelectMembers", F_SelectMembers },
	{ "Text", F_Text },
	{ "TextByName", F_TextByName },
	{ "TextScale", F_TextScale },
	{ "ToggleAllDirections", F_ToggleAllDirections },
	{ "ToggleGrid", F_ToggleGrid },
	{ "ToggleObject", F_ToggleObject },
	{ "Value", F_Value },
	{ "Via", F_Via },
	{ "ViaByName", F_ViaByName },
	{ "ViaSize", F_ViaSize },
	{ "ViaDrillingHole", F_ViaDrillingHole },
	{ "Zoom", F_Zoom }};

/* ---------------------------------------------------------------------------
 * some local routines
 */
static	void	WarpPointer(void);
static	int		GetFunctionID(String);
static	void	AdjustAttachedLine(void);
static	void	AdjustAttachedBox(void);
static	void	AdjustAttachedObjects(void);
static	void	NotifyLine(void);
static	void	NotifyBlock(void);
static	void	NotifyMode(void);

/* ---------------------------------------------------------------------------
 * get function ID of passed string
 */
static int GetFunctionID(String Ident)
{
	int		i;

	i = ENTRIES(Functions);
	while (i)
		if (!strcmp(Ident, Functions[--i].Identifier))
			return((int) Functions[i].ID);
	return(-1);
}

/* ---------------------------------------------------------------------------
 * makes the 'marked line' fit into a 45 degree direction
 *
 * directions:
 *
 *           0
 *          7 1
 *         6   2
 *          5 3
 *           4
 */
static void AdjustAttachedLine(void)
{
	Position	dx, dy,
				min;
	BYTE		direction = 0;
	float		m;

		/* I need at least one point */
	if (Crosshair.AttachedLine.State == STATE_FIRST)
		return;

		/* no 45 degree lines required */
	if (TEST_FLAG(ALLDIRCETIONFLAG, PCB))
	{
		Crosshair.AttachedLine.Point2.X = Crosshair.X;
		Crosshair.AttachedLine.Point2.Y = Crosshair.Y;
		return;
	}

		/* first calculate direction of line */
	dx = Crosshair.X -Crosshair.AttachedLine.Point1.X;
	dy = Crosshair.Y -Crosshair.AttachedLine.Point1.Y;
	if (!dx)
	{
		if (!dy)
				/* zero length line, don't draw anything */
			return;
		else
			direction = dy > 0 ? 0 : 4;
	}
	else
	{
		m = (float) dy / (float) dx;
		direction = 2;
		if (m > TAN_30_DEGREE)
			direction = m > TAN_60_DEGREE ? 0 : 1;
		else
			if (m < -TAN_30_DEGREE)
				direction = m < -TAN_60_DEGREE ? 0 : 3;
	}
	if (dx < 0)
		direction += 4;

	dx = abs(dx);
	dy = abs(dy);
	min = MIN(dx, dy);

		/* now set up the second pair of coordinates */
	switch (direction)
	{
		case 0:
		case 4:
			Crosshair.AttachedLine.Point2.X = Crosshair.AttachedLine.Point1.X;
			Crosshair.AttachedLine.Point2.Y = Crosshair.Y;
			break;

		case 2:
		case 6:
			Crosshair.AttachedLine.Point2.X = Crosshair.X;
			Crosshair.AttachedLine.Point2.Y = Crosshair.AttachedLine.Point1.Y;
			break;

		case 1:
			Crosshair.AttachedLine.Point2.X =
				Crosshair.AttachedLine.Point1.X +min;
			Crosshair.AttachedLine.Point2.Y =
				Crosshair.AttachedLine.Point1.Y +min;
			break;

		case 3:
			Crosshair.AttachedLine.Point2.X =
				Crosshair.AttachedLine.Point1.X +min;
			Crosshair.AttachedLine.Point2.Y =
				Crosshair.AttachedLine.Point1.Y -min;
			break;

		case 5:
			Crosshair.AttachedLine.Point2.X =
				Crosshair.AttachedLine.Point1.X -min;
			Crosshair.AttachedLine.Point2.Y =
				Crosshair.AttachedLine.Point1.Y -min;
			break;

		case 7:
			Crosshair.AttachedLine.Point2.X =
				Crosshair.AttachedLine.Point1.X -min;
			Crosshair.AttachedLine.Point2.Y =
				Crosshair.AttachedLine.Point1.Y +min;
			break;
	}
}

/* ---------------------------------------------------------------------------
 * set new coordinates if in 'RECTANGLE' mode
 * the cursor shape is also adjusted
 */
static void AdjustAttachedBox(void)
{
	switch(Crosshair.AttachedBox.State)
	{
		case STATE_SECOND:		/* one corner is selected */
		{
			unsigned int	shape;

				/* update coordinates */
			Crosshair.AttachedBox.Point2.X = Crosshair.X;
			Crosshair.AttachedBox.Point2.Y = Crosshair.Y;

				/* set pointer shape depending on location relative
				 * to first corner
				 */
			if (Crosshair.Y <= Crosshair.AttachedBox.Point1.Y)
				shape = (Crosshair.X >= Crosshair.AttachedBox.Point1.X) ?
					XC_ur_angle : XC_ul_angle;
			else
				shape = (Crosshair.X >= Crosshair.AttachedBox.Point1.X) ?
					XC_lr_angle : XC_ll_angle;
			if (Output.XCursorShape != shape)
				SetOutputXCursor(shape);
			break;
		}

		default:
				/* just reset the cursor shape if necessary */
			if (Output.XCursorShape != XC_ul_angle)
				SetOutputXCursor(XC_ul_angle);
			break;
	}	
}

/* ---------------------------------------------------------------------------
 * adjusts the objects which are to be created like attached lines...
 */
static void AdjustAttachedObjects(void)
{
	switch(Settings.Mode)
	{
			/* update at least an attached block (selection) */
		case NO_MODE:
			if (Crosshair.AttachedBox.State)
			{
				Crosshair.AttachedBox.Point2.X = Crosshair.X;
				Crosshair.AttachedBox.Point2.Y = Crosshair.Y;
			}
			break;

			/* rectangle creation mode */
		case RECTANGLE_MODE:
			AdjustAttachedBox();
			break;

			/* line or polygon creation mode */
		case LINE_MODE:
		case POLYGON_MODE:
			AdjustAttachedLine();
			break;
	}
}
/* ---------------------------------------------------------------------------
 * creates points of a line
 */
static void NotifyLine(void)
{
	switch(Crosshair.AttachedLine.State)
	{
		case STATE_FIRST:			/* first point */
			Crosshair.AttachedLine.State = STATE_SECOND;
			Crosshair.AttachedLine.Point1.X =
				Crosshair.AttachedLine.Point2.X = Crosshair.X;
			Crosshair.AttachedLine.Point1.Y =
				Crosshair.AttachedLine.Point2.Y = Crosshair.Y;
			break;

		default:					/* all following points */
			Crosshair.AttachedLine.State = STATE_THIRD;
			break;
	}
}

/* ---------------------------------------------------------------------------
 * create first or second corner of a marked block
 */
static void NotifyBlock(void)
{
	HideCrosshair(True);
	switch(Crosshair.AttachedBox.State)
	{
		case STATE_FIRST:		/* setup first point */
			Crosshair.AttachedBox.Point1.X =
				Crosshair.AttachedBox.Point2.X = Crosshair.X;
			Crosshair.AttachedBox.Point1.Y =
				Crosshair.AttachedBox.Point2.Y = Crosshair.Y;
			Crosshair.AttachedBox.State = STATE_SECOND;
			break;

		case STATE_SECOND:		/* setup second point */
			Crosshair.AttachedBox.State = STATE_THIRD;
			break;
	}
	RestoreCrosshair(True);
}

/* ---------------------------------------------------------------------------
 * does something appropriate for the current mode setting. This normaly
 * means creation of an object at the current crosshair location.
 *
 * new created objects are added to the undo list
 */
static void NotifyMode(void)
{
	void	*ptr1,
			*ptr2,
			*ptr3;
	int		type;

	switch(Settings.Mode)
	{
		case VIA_MODE:
		{
			PinTypePtr	via;

			if ((via = CreateNewVia(PCB->Data, Crosshair.X, Crosshair.Y,
					Settings.ViaThickness, Settings.ViaDrillingHole,
					NULL, VIAFLAG)) != NULL)
			{
				AddObjectToCreateUndoList(VIA_TYPE, via, via, via);
				IncrementUndoSerialNumber();
				DrawVia(via);
				SetChangedFlag(True);
			}
			break;
		}

		case LINE_MODE:
				/* do update of position */
			NotifyLine();

				/* create line if both ends are determined && length != 0 */
			if (Crosshair.AttachedLine.State == STATE_THIRD &&
				(Crosshair.AttachedLine.Point1.X != Crosshair.AttachedLine.Point2.X ||
				Crosshair.AttachedLine.Point1.Y != Crosshair.AttachedLine.Point2.Y))
			{
				LineTypePtr	line;

				if ((line = CreateNewLineOnLayer(CURRENT,
						Crosshair.AttachedLine.Point1.X,
						Crosshair.AttachedLine.Point1.Y,
						Crosshair.AttachedLine.Point2.X,
						Crosshair.AttachedLine.Point2.Y,
						Settings.LineThickness, NOFLAG)) != NULL)
				{
					AddObjectToCreateUndoList(LINE_TYPE, CURRENT, line, line);
					IncrementUndoSerialNumber();
					DrawLine(CURRENT, line);
					SetChangedFlag(True);
				}

					/* copy the coordinates */
				Crosshair.AttachedLine.Point1.X=Crosshair.AttachedLine.Point2.X;
				Crosshair.AttachedLine.Point1.Y=Crosshair.AttachedLine.Point2.Y;
			}
			break;

		case RECTANGLE_MODE:
				/* do update of position */
			NotifyBlock();

				/* create rectangle if both corners are determined 
				 * and width, height are != 0
				 */
			if (Crosshair.AttachedBox.State == STATE_THIRD &&
				Crosshair.AttachedBox.Point1.X != Crosshair.AttachedBox.Point2.X &&
				Crosshair.AttachedBox.Point1.Y != Crosshair.AttachedBox.Point2.Y)
			{
				PolygonTypePtr	polygon;

				if ((polygon = CreateNewPolygonFromRectangle(CURRENT, 
						Crosshair.AttachedBox.Point1.X,
						Crosshair.AttachedBox.Point1.Y,
						Crosshair.AttachedBox.Point2.X,
						Crosshair.AttachedBox.Point2.Y,
						NOFLAG)) != NULL)
				{
					AddObjectToCreateUndoList(POLYGON_TYPE, CURRENT,
						polygon, polygon);
					IncrementUndoSerialNumber();
					DrawPolygon(CURRENT, polygon);
					SetChangedFlag(True);
				}

					/* reset state to 'first corner' */
				Crosshair.AttachedBox.State = STATE_FIRST;
			}
			break;

		case TEXT_MODE:
		{
			char	*string;

			if ((string = GetUserInput("Enter text:", "")) != NULL)
			{
				TextTypePtr	text;

				if ((text = CreateNewText(CURRENT, &PCB->Font, Crosshair.X,
						Crosshair.Y, 0, Settings.TextScale,
						string, NOFLAG)) != NULL)
				{
					AddObjectToCreateUndoList(TEXT_TYPE, CURRENT, text, text);
					IncrementUndoSerialNumber();
					DrawText(CURRENT, text);
					SetChangedFlag(True);
				}

					/* free memory allocated by GetUserInput() */
				SaveFree(string);
			}
			break;
		}

		case POLYGON_MODE:
		{
			PointTypePtr	points = Crosshair.AttachedPolygon.Points;
			Cardinal		n = Crosshair.AttachedPolygon.PointN;
			
				/* do update of position; use the 'LINE_MODE' mechanism */
			NotifyLine();

				/* check if this is the last point of a polygon */
			if (n >= 3 &&
				points->X == Crosshair.AttachedLine.Point2.X &&
				points->Y == Crosshair.AttachedLine.Point2.Y)
			{
				CopyAttachedPolygonToLayer();
				break;
			}

				/* create new point if it's the first one or if it's
				 * different to the last one
				 */
			if (!n ||
				points[n-1].X != Crosshair.AttachedLine.Point2.X ||
				points[n-1].Y != Crosshair.AttachedLine.Point2.Y)
			{
				CreateNewPointInPolygon(&Crosshair.AttachedPolygon,
					Crosshair.AttachedLine.Point2.X,
					Crosshair.AttachedLine.Point2.Y);

					/* copy the coordinates */
				Crosshair.AttachedLine.Point1.X=Crosshair.AttachedLine.Point2.X;
				Crosshair.AttachedLine.Point1.Y=Crosshair.AttachedLine.Point2.Y;
			}
			break;
		}

		case PASTEBUFFER_MODE:	
			if (CopyPastebufferToLayout(Crosshair.X, Crosshair.Y))
				SetChangedFlag(True);
			break;

		case REMOVE_MODE:
			if ((type = SearchObjectByPosition(REMOVE_TYPES,&ptr1,&ptr2,&ptr3,
				Crosshair.X, Crosshair.Y)) != NO_TYPE)
			{
				RemoveObject(type, ptr1, ptr2, ptr3);
				SetChangedFlag(True);
			}
			break;

		case MIRROR_MODE:
			if ((type = SearchObjectByPosition(MIRROR_TYPES,&ptr1,&ptr2,&ptr3,
				Crosshair.X, Crosshair.Y)) != NO_TYPE)
			{
				MirrorObject(type, ptr1, ptr2, ptr3);
				SetChangedFlag(True);
			}
			break;

		case ROTATE_MODE:
			if ((type = SearchObjectByPosition(ROTATE_TYPES,&ptr1,&ptr2,&ptr3,
				Crosshair.X, Crosshair.Y)) != NO_TYPE)
			{
				RotateObject(type,ptr1,ptr2,ptr3, Crosshair.X, Crosshair.Y, 1);
				SetChangedFlag(True);
			}
			break;

			/* both are almost the same */
		case COPY_MODE:
		case MOVE_MODE:
		case RUBBERBANDMOVE_MODE:
			switch(Crosshair.AttachedObject.State)
			{
					/* first notify, lookup object */
				case STATE_FIRST:
				{
					int	types = (Settings.Mode == COPY_MODE ) ?
									COPY_TYPES : MOVE_TYPES;

					Crosshair.AttachedObject.Type= SearchObjectByPosition(types,
						&Crosshair.AttachedObject.Ptr1,
						&Crosshair.AttachedObject.Ptr2,
						&Crosshair.AttachedObject.Ptr3,
						Crosshair.X, Crosshair.Y);
					if (Crosshair.AttachedObject.Type != NO_TYPE)
					{
						BoxTypePtr	box;

							/* change to next state and save cursor position */
						Crosshair.AttachedObject.X = Crosshair.X;
						Crosshair.AttachedObject.Y = Crosshair.Y;
						Crosshair.AttachedObject.State = STATE_SECOND;

							/* get boundingbox of object and set cursor range */
						box= GetObjectBoundingBox(Crosshair.AttachedObject.Type,
							Crosshair.AttachedObject.Ptr1,
							Crosshair.AttachedObject.Ptr2,
							Crosshair.AttachedObject.Ptr3);
						SetCrosshairRange(Crosshair.X -box->X1,
							Crosshair.Y -box->Y1,
							PCB->MaxWidth -(box->X2 -Crosshair.X),
							PCB->MaxHeight -(box->Y2 -Crosshair.Y));

							/* get all attached objects if necessary */
						if (Settings.Mode == RUBBERBANDMOVE_MODE)
							LookupRubberbandLines(Crosshair.AttachedObject.Type,
								Crosshair.AttachedObject.Ptr1,
								Crosshair.AttachedObject.Ptr2,
								Crosshair.AttachedObject.Ptr3);
						else
							Crosshair.AttachedObject.RubberbandN = 0;
					}
					break;
				}

					/* second notify, move or copy object */
				case STATE_SECOND:
					if (Settings.Mode == COPY_MODE)
						CopyObject(Crosshair.AttachedObject.Type,
							Crosshair.AttachedObject.Ptr1,
							Crosshair.AttachedObject.Ptr2,
							Crosshair.AttachedObject.Ptr3,
							Crosshair.X -Crosshair.AttachedObject.X,
							Crosshair.Y -Crosshair.AttachedObject.Y);
					else
						MoveObjectAndRubberband(Crosshair.AttachedObject.Type,
							Crosshair.AttachedObject.Ptr1,
							Crosshair.AttachedObject.Ptr2,
							Crosshair.AttachedObject.Ptr3,
							Crosshair.X -Crosshair.AttachedObject.X,
							Crosshair.Y -Crosshair.AttachedObject.Y);
					SetChangedFlag(True);

						/* reset identifiers */
					Crosshair.AttachedObject.Type = NO_TYPE;
					Crosshair.AttachedObject.State = STATE_FIRST;
					break;
			}
			break;

			/* insert a point into a polygon/line/... */
		case INSERTPOINT_MODE:
			switch(Crosshair.AttachedObject.State)
			{
					/* first notify, lookup object */
				case STATE_FIRST:
					Crosshair.AttachedObject.Type =
						SearchObjectByPosition(INSERT_TYPES,
						&Crosshair.AttachedObject.Ptr1,
						&Crosshair.AttachedObject.Ptr2,
						&Crosshair.AttachedObject.Ptr3,
						Crosshair.X, Crosshair.Y);

					if (Crosshair.AttachedObject.Type != NO_TYPE)
					{
							/* get starting point of nearest segment */
						if (Crosshair.AttachedObject.Type == POLYGON_TYPE)
						{
							Crosshair.AttachedObject.Ptr3 =
								GetLowestDistancePolygonPoint(
									(PolygonTypePtr) Crosshair.AttachedObject.Ptr2,
								Crosshair.X, Crosshair.Y);
						}
						Crosshair.AttachedObject.State = STATE_SECOND;
					}
					break;

					/* second notify, insert new point into object */
				case STATE_SECOND:
					InsertPointIntoObject(Crosshair.AttachedObject.Type,
						Crosshair.AttachedObject.Ptr1,
						Crosshair.AttachedObject.Ptr2,
						Crosshair.AttachedObject.Ptr3,
						Crosshair.X, Crosshair.Y);
					SetChangedFlag(True);

						/* reset identifiers */
					Crosshair.AttachedObject.Type = NO_TYPE;
					Crosshair.AttachedObject.State = STATE_FIRST;
					break;
			}
			break;
	}
}

/* ---------------------------------------------------------------------------
 * warp pointer to new cursor location
 */
static void WarpPointer(void)
{
	XWarpPointer(Dpy, Output.OutputWindow, Output.OutputWindow,
		0, 0, 0, 0,
		(int) (TO_SCREEN_X(Crosshair.X)), (int) (TO_SCREEN_Y(Crosshair.Y)));

		/* XWarpPointer creates Motion events normally bound to
		 * EventMoveCrosshair.
		 * We don't do any updates when EventMoveCrosshair
		 * is called the next time to prevent from rounding errors
		 */
	IgnoreMotionEvents = True;

		/* update object position and cursor location */
	AdjustAttachedObjects();
	SetCursorStatusLine();
}

/* ---------------------------------------------------------------------------
 * action routine to move to X pointer relative to the current position
 * syntax: MovePointer(deltax,deltay)
 */
void ActionMovePointer(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	Position	dx, dy;

	if (*Num == 2)
	{
		HideCrosshair(False);

		dx = (Position) (atoi(*Params) *PCB->Grid);
		dy = (Position) (atoi(*(Params+1)) *PCB->Grid);
		MoveCrosshairRelative(TO_SCREEN_SIGN_X(dx), TO_SCREEN_SIGN_Y(dy));

			/* adjust pointer */
		WarpPointer();
		RestoreCrosshair(False);
	}
}

/* ---------------------------------------------------------------------------
 * !!! no action routine !!!
 *
 * event handler to set the cursor according to the X pointer position
 * called from inside main.c
 */
void EventMoveCrosshair(XMotionEvent *Event)
{
		/* ignore events that are probably caused by ActionMovePointer */
	if (!IgnoreMotionEvents)
	{
		Window			root, child;
		int				rootX, rootY,
						childX, childY;
		unsigned int	mask;

			/* only handle the event if the pointer really is at
			 * the same position to prevent slow systems from
			 * slow redrawing
			 */
		XQueryPointer(Dpy, Output.OutputWindow, &root, &child,
			&rootX, &rootY, &childX, &childY, &mask);
		if (Event->x == childX && Event->y == childY)
		{
			HideCrosshair(False);

				/* correct the values with zoom factor */
			MoveCrosshairAbsolute(TO_PCB_X(Event->x) +PCB->Grid/2,
				TO_PCB_Y(Event->y) +PCB->Grid/2);

				/* update object position and cursor location */
			AdjustAttachedObjects();
			SetCursorStatusLine();
			RestoreCrosshair(False);
		}
	}
	else
		IgnoreMotionEvents = False;
}

/* ---------------------------------------------------------------------------
 * action routine to change the grid, zoom and sizes
 * the first the type of object and the second is passed to
 * the specific routine
 * the value of the second determines also if it is absolute (without a sign)
 * or relative to the current one (with sign + or -)
 * syntax: SetValue(Grid|Zoom|LineSize|TextScale|ViaDrillingHole|ViaSize, value)
 */
void ActionSetValue(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	Boolean	r;		/* flag for 'relative' value */
	int		value;

	if (*Num == 2)
	{
		HideCrosshair(True);

			/* if the first character is a sign we have to add the
			 * value to the current one
			 */
		r = !isdigit(**(Params+1));
		value = atoi(*(Params+1));
		switch(GetFunctionID(*Params))
		{
			case F_ViaDrillingHole:
				SetViaDrillingHole(r ? value +Settings.ViaDrillingHole : value, False);
				break;

			case F_Grid:
				SetGrid(r ? value +PCB->Grid : value);
				break;

			case F_Zoom:
				SetZoom(r ? value +PCB->Zoom : value);
				break;

			case F_LineSize:
				SetLineSize(r ? value +Settings.LineThickness : value);
				break;

			case F_ViaSize:
				SetViaSize(r ? value +Settings.ViaThickness : value, False);
				break;

			case F_TextScale:
				SetTextScale(r ? value +Settings.TextScale : value);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * quits user input, the translations of the text widget handle it 
 * syntax: FinishInput(OK|Cancel)
 */
void ActionFinishInputDialog(Widget W, XEvent *Event,
	String *Params, Cardinal *Num)
{
	if (*Num == 1)
		FinishInputDialog(!strcmp("OK", *Params));
}

/* ---------------------------------------------------------------------------
 * quits application
 * syntax: Quit()
 */
void ActionQuit(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 0 && (!PCB->Changed || ConfirmDialog("OK to lose data ?")))
		QuitApplication();
}

/* ---------------------------------------------------------------------------
 * searches connections of the pin or via at the cursor position
 * syntax: Connection(Find|ResetLinesAndPolygons|ResetPinsAndVias|Reset)
 */
void ActionConnection(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
			case F_Find:
			{
				Cursor	oldCursor;

				oldCursor = SetOutputXCursor(XC_watch);
				LookupConnection(Crosshair.X, Crosshair.Y);
				SetOutputXCursor(oldCursor);
				break;
			}

			case F_ResetLinesAndPolygons:
				ResetFoundLinesAndPolygons();
				break;

			case F_ResetPinsViasAndPads:
				ResetFoundPinsViasAndPads();
				break;

			case F_Reset:
				ResetFoundPinsViasAndPads();
				ResetFoundLinesAndPolygons();
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * starts input of user commands
 * syntax: Command()
 */
void ActionCommand(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
			char	*command;
	static	char	*previous = NULL;

	if (*Num == 0)
	{
		HideCrosshair(True);
		command = GetUserInput("Enter command:", previous ? previous : "");
		if (command != NULL)
		{
				/* copy new comand line to save buffer */
			if (Settings.SaveLastCommand)
			{
				SaveFree(previous);
				previous = MyStrdup(command, "ActionCommand()");
			}
			ExecuteUserCommand(command);
			SaveFree(command);
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * several display related actions
 * syntax: Display(NameOnPCB|Description|Value)
 *         Display(Center|ClearAndRedraw|Redraw)
 *         Display(Grid|Toggle45Degree|ToggleGrid)
 *         Display(Pinout|PinOrPadName)
 */
void ActionDisplay(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	int		id;

	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(id = GetFunctionID(*Params))
		{
				/* redraw layout with clearing the background */
			case F_ClearAndRedraw:
				ReleaseSaveUnderPixmap();
				ClearAndRedrawOutput();
				break;

				/* redraw layout without clearing the background */
			case F_Redraw:
				ReleaseSaveUnderPixmap();
				RedrawOutput();
				break;

					/* center cursor and move X pointer too */
			case F_Center:
				CenterDisplay(TO_SCREEN_X(Crosshair.X),
					TO_SCREEN_Y(Crosshair.Y));
				XtCallActionProc(Output.Output, MovePointerCommand[0], NULL,
					&MovePointerCommand[1], ENTRIES(MovePointerCommand) -1);
				break;

				/* change the displayed name of elements */
			case F_Value:
			case F_NameOnPCB:
			case F_Description:
				ReleaseSaveUnderPixmap();
				ELEMENT_LOOP(PCB->Data, EraseElementName(element););
				CLEAR_FLAG(DESCRIPTIONFLAG | NAMEONPCBFLAG, PCB);
				switch (id)
				{
					case F_Value:		break;
					case F_NameOnPCB:	SET_FLAG(NAMEONPCBFLAG, PCB); break;
					case F_Description:	SET_FLAG(DESCRIPTIONFLAG, PCB); break;
				}
				ELEMENT_LOOP(PCB->Data, DrawElementName(element););
				break;

				/* toggle line-adjust flag */
			case F_ToggleAllDirections:
				TOGGLE_FLAG(ALLDIRCETIONFLAG, PCB);
				SetStatusLine();
				break;

				/* toggle displaying of absolute-grid */
			case F_ToggleGrid:
				TOGGLE_FLAG(ABSOLUTEFLAG, PCB);
				SetGrid(PCB->Grid);
				break;

				/* toggle displaying of the grid */
			case F_Grid:
				Settings.DrawGrid = !Settings.DrawGrid;
				ReleaseSaveUnderPixmap();
				DrawGrid();
				break;

				/* display the pinout of an element */
			case F_Pinout:
			{
				ElementTypePtr	element;

				if ((SearchObjectByPosition(ELEMENT_TYPE,
						(void **) &element,
						(void **) &element,
						(void **) &element,
						Crosshair.X, Crosshair.Y)) != NO_TYPE)
					PinoutWindow(Output.Toplevel, element);
				break;
			}

				/* toggle displaying of pin/pad/via names */
			case F_PinOrPadName:
			{
				void	*ptr1, *ptr2, *ptr3;

				switch(SearchObjectByPosition(ELEMENT_TYPE|PIN_TYPE|PAD_TYPE,
					(void **) &ptr1, (void **) &ptr2, (void **) &ptr3,
					Crosshair.X, Crosshair.Y))
				{
					case ELEMENT_TYPE:
						PIN_LOOP((ElementTypePtr) ptr1,
							if (TEST_FLAG(DISPLAYNAMEFLAG, pin))
								ErasePinName(pin);
							else
								DrawPinName(pin);
							TOGGLE_FLAG(DISPLAYNAMEFLAG, pin);
						);
						PAD_LOOP((ElementTypePtr) ptr1,
							if (TEST_FLAG(DISPLAYNAMEFLAG, pad))
								ErasePadName(pad);
							else
								DrawPadName(pad);
							TOGGLE_FLAG(DISPLAYNAMEFLAG, pad);
						);
						SetChangedFlag(True);
						break;

					case PIN_TYPE:
						if (TEST_FLAG(DISPLAYNAMEFLAG, (PinTypePtr) ptr2))
							ErasePinName((PinTypePtr) ptr2);
						else
							DrawPinName((PinTypePtr) ptr2);
						TOGGLE_FLAG(DISPLAYNAMEFLAG, (PinTypePtr) ptr2);
						SetChangedFlag(True);
						break;

					case PAD_TYPE:
						if (TEST_FLAG(DISPLAYNAMEFLAG, (PadTypePtr) ptr2))
							ErasePadName((PadTypePtr) ptr2);
						else
							DrawPadName((PadTypePtr) ptr2);
						TOGGLE_FLAG(DISPLAYNAMEFLAG, (PadTypePtr) ptr2);
						SetChangedFlag(True);
						break;
				}
				break;
			}
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * action routine to
 *   set a new mode
 *   save the current one or restore the last saved mode
 *   call an appropriate action for the current mode
 * syntax: Mode(Copy|InsertPoint|Line|Move|None|PasteBuffer|Polygon)
 *         Mode(Remove|Rectangle|Text|Via)
 *         Mode(Notify)
 *         Mode(Save|Restore)
 */
void ActionMode(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	static	int		position = 0;
	static	int		stack[MAX_MODESTACK_DEPTH];

	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
			case F_Copy:			SetMode(COPY_MODE); break;
			case F_InsertPoint:		SetMode(INSERTPOINT_MODE); break;
			case F_Line:			SetMode(LINE_MODE); break;
			case F_Mirror:			SetMode(MIRROR_MODE); break;
			case F_Move:			SetMode(MOVE_MODE); break;
			case F_RubberbandMove:	SetMode(RUBBERBANDMOVE_MODE); break;
			case F_Notify:			NotifyMode(); break;
			case F_PasteBuffer:		SetMode(PASTEBUFFER_MODE); break;
			case F_Polygon:			SetMode(POLYGON_MODE); break;
			case F_Remove:			SetMode(REMOVE_MODE); break;
			case F_Rectangle:		SetMode(RECTANGLE_MODE); break;
			case F_None:			SetMode(NO_MODE); break;
			case F_Rotate:			SetMode(ROTATE_MODE); break;
			case F_Text:			SetMode(TEXT_MODE); break;
			case F_Via:				SetMode(VIA_MODE); break;

			case F_Restore:		/* restore the last saved mode */
				SetMode(position > 0 ? stack[--position] : NO_MODE);
				break;

			case F_Save:		/* save currently selected mode */
				stack[position] = Settings.Mode;
				if (position < MAX_MODESTACK_DEPTH-1)
					position++;
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * action routine to remove objects
 * syntax: RemoveSelected()
 */
void ActionRemoveSelected(Widget W, XEvent *Event,
	String *Params, Cardinal *Num)
{
	if (*Num == 0)
	{
		HideCrosshair(True);
		if (RemoveSelected())
			SetChangedFlag(True);
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * changes the size of objects
 * syntax: ChangeSize(Object, delta)
 *         ChangeSize(SelectedLines|SelectedPins|SelectedVias, delta)
 *         ChangeSize(SelectedPads|SelectedTexts, delta)
 */
void ActionChangeSize(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 2)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
			case F_Object:
			{
				int		type;
				void	*ptr1, *ptr2, *ptr3;

				if ((type = SearchObjectByPosition(CHANGESIZE_TYPES,
					&ptr1, &ptr2, &ptr3, Crosshair.X, Crosshair.Y)) != NO_TYPE)
					if (ChangeObjectSize(type, ptr1, ptr2, ptr3,
						atoi(*(Params+1))))
						SetChangedFlag(True);
				break;
			}

			case F_SelectedVias:
				if (ChangeSelectedViaSize(atoi(*(Params+1))))
					SetChangedFlag(True);
				break;

			case F_SelectedPins:
				if (ChangeSelectedPinSize(atoi(*(Params+1))))
					SetChangedFlag(True);
				break;

			case F_SelectedPads:
				if (ChangeSelectedPadSize(atoi(*(Params+1))))
					SetChangedFlag(True);
				break;

			case F_SelectedLines:
				if (ChangeSelectedLineSize(atoi(*(Params+1))))
					SetChangedFlag(True);
				break;

			case F_SelectedTexts:
				if (ChangeSelectedTextSize(atoi(*(Params+1))))
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * changes the 2nd size of objects (drilling hole)
 * syntax: Change2ndSize(Object, delta)
 *         Change2ndSize(SelectedPins|SelectedVias, delta)
 */
void ActionChange2ndSize(Widget W, XEvent *Event,
	String *Params, Cardinal *Num)
{
	if (*Num == 2)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
			case F_Object:
			{
				int		type;
				void	*ptr1, *ptr2, *ptr3;

				if ((type = SearchObjectByPosition(CHANGE2NDSIZE_TYPES,
					&ptr1, &ptr2, &ptr3, Crosshair.X, Crosshair.Y)) != NO_TYPE)
					if (ChangeObject2ndSize(type,ptr1,ptr2,ptr3,
						atoi(*(Params+1))))
						SetChangedFlag(True);
				break;
			}

			case F_SelectedVias:
				if (ChangeSelectedVia2ndSize(atoi(*(Params+1))))
					SetChangedFlag(True);
				break;

			case F_SelectedPins:
				if (ChangeSelectedPin2ndSize(atoi(*(Params+1))))
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * sets the name of objects
 * syntax: ChangeName(Object)
 *         ChangeName(Layout|Layer)
 */
void ActionChangeName(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	char	*name;

	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
				/* change the name of an object */
			case F_Object:
			{
				int		type;
				void	*ptr1, *ptr2, *ptr3;

				if ((type = SearchObjectByPosition(CHANGENAME_TYPES,
					&ptr1, &ptr2, &ptr3, Crosshair.X, Crosshair.Y)) != NO_TYPE)
					if (QueryInputAndChangeObjectName(type, ptr1, ptr2, ptr3))
						SetChangedFlag(True);
				break;
			}

				/* change the layouts name */
			case F_Layout:
				name= GetUserInput("enter the layouts name:", EMPTY(PCB->Name));
				if (name && ChangeLayoutName(name))
					SetChangedFlag(True);
				break;

				/* change the name of the activ layer */
			case F_Layer:
				name = GetUserInput("enter the layers name:",
					EMPTY(CURRENT->Name));
				if (name && ChangeLayerName(CURRENT, name))
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * changes the square-flag of objects
 * syntax: ChangeSquare(ToggleObject|SelectedElements|SelectedPins)
 */
void ActionChangeSquare(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
			case F_ToggleObject:
			{
				int		type;
				void	*ptr1, *ptr2, *ptr3;

				if ((type = SearchObjectByPosition(CHANGESQUARE_TYPES,
					&ptr1, &ptr2, &ptr3, Crosshair.X, Crosshair.Y)) != NO_TYPE)
					if (ChangeObjectSquare(type, ptr1, ptr2, ptr3))
						SetChangedFlag(True);
				break;
			}

			case F_SelectedElements:
				if (ChangeSelectedElementSquare())
					SetChangedFlag(True);
				break;

			case F_SelectedPins:
				if (ChangeSelectedPinSquare())
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * toggles the selection of the object at the pointer location
 * or sets it if 'All', 'Block' or 'Connection' is passed
 * syntax: Select(ToggleObject)
 *         Select(All|Block|Connection)
 *         Select(ElementByName|ObjectByName|PadByName|PinByName)
 *         Select(TextByName|ViaByName)
 */
void ActionSelect(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 1)
	{
		int		type;

		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
#ifdef HAS_REGEX
				/* select objects by their names */
			case F_ElementByName:	type = ELEMENT_TYPE; goto commonByName;
			case F_ObjectByName:	type = ALL_TYPES; goto commonByName;
			case F_PadByName:		type = PAD_TYPE; goto commonByName;
			case F_PinByName:		type = PIN_TYPE; goto commonByName;
			case F_TextByName:		type = TEXT_TYPE; goto commonByName;
			case F_ViaByName:		type = VIA_TYPE; goto commonByName;

			commonByName:
			{
				char	*pattern;

				if ((pattern = GetUserInput("pattern:", "")) != NULL)
					if (SelectObjectByName(type, pattern))
						SetChangedFlag(True);
				break;
			}
#endif		/* HAS_REGEX */

				/* select a single object */
			case F_ToggleObject:
				if (SelectObject())
					SetChangedFlag(True);
				break;

				/* all objects in block */
			case F_Block:
			{
				BoxType	box;

				box.X1 = MIN(Crosshair.AttachedBox.Point1.X,
					Crosshair.AttachedBox.Point2.X);
				box.Y1 = MIN(Crosshair.AttachedBox.Point1.Y,
					Crosshair.AttachedBox.Point2.Y);
				box.X2 = MAX(Crosshair.AttachedBox.Point1.X,
					Crosshair.AttachedBox.Point2.X);
				box.Y2 = MAX(Crosshair.AttachedBox.Point1.Y,
					Crosshair.AttachedBox.Point2.Y);
				NotifyBlock();
				if (Crosshair.AttachedBox.State == STATE_THIRD &&
					SelectBlock(&box, True))
				{
					SetChangedFlag(True);
					Crosshair.AttachedBox.State = STATE_FIRST;
				}
				break;
			}

				/* select all visible objects */
			case F_All:
			{
				BoxType	box;

				box.X1 = 0;
				box.Y1 = 0;
				box.X2 = PCB->MaxWidth;
				box.Y2 = PCB->MaxHeight;
				if (SelectBlock(&box, True))
					SetChangedFlag(True);
				break;
			}

				/* all found connections */
			case F_Connection:
				if (SelectConnection(True))
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * unselects the object at the pointer location
 * syntax: Unselect(All|Block|Connection)
 */
void ActionUnselect(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
				/* all objects in block */
			case F_Block:
			{
				BoxType	box;

				box.X1 = MIN(Crosshair.AttachedBox.Point1.X,
					Crosshair.AttachedBox.Point2.X);
				box.Y1 = MIN(Crosshair.AttachedBox.Point1.Y,
					Crosshair.AttachedBox.Point2.Y);
				box.X2 = MAX(Crosshair.AttachedBox.Point1.X,
					Crosshair.AttachedBox.Point2.X);
				box.Y2 = MAX(Crosshair.AttachedBox.Point1.Y,
					Crosshair.AttachedBox.Point2.Y);
				NotifyBlock();
				if (Crosshair.AttachedBox.State == STATE_THIRD &&
					SelectBlock(&box, False))
				{
					SetChangedFlag(True);
					Crosshair.AttachedBox.State = STATE_FIRST;
				}
				break;
			}

				/* unselect all visible objects */
			case F_All:
			{
				BoxType	box;

				box.X1 = 0;
				box.Y1 = 0;
				box.X2 = PCB->MaxWidth;
				box.Y2 = PCB->MaxHeight;
				if (SelectBlock(&box, False))
					SetChangedFlag(True);
				break;
			}

				/* all found connections */
			case F_Connection:
				if (SelectConnection(False))
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * saves data to file
 * syntax: Save(Layout|LayoutAs)
 *         Save(AllConnections|AllUnusedPins|ElementConnections)
 */
void ActionSave(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	char	*name;

	if (*Num == 1)
		switch(GetFunctionID(*Params))
		{
				/* save layout; use its original file name
				 * or 'fall thru' to next routine
				 */
			case F_Layout:
				if (PCB->Filename)
				{
					SavePCB(PCB->Filename);
					break;
				}

				/* save data to any file */
			case F_LayoutAs:
				name= FileSelectBox("save layout as:", PCB->Filename,
					Settings.FilePath);
				if (name)
					SavePCB(name);
				break;

				/* save all connections to file */
			case F_AllConnections:
			{
				FILE	*fp;
				Cursor	oldCursor;

				if ((fp = OpenConnectionDataFile()) != NULL)
				{
					oldCursor = SetOutputXCursor(XC_watch);
					LookupConnectionsToAllElements(fp);
						fclose(fp);
						SetOutputXCursor(oldCursor);
						SetChangedFlag(True);
				}
				break;
			}

				/* save all unused pins to file */
			case F_AllUnusedPins:
			{
				FILE	*fp;
				Cursor	oldCursor;

				if ((fp = OpenConnectionDataFile()) != NULL)
				{
					oldCursor = SetOutputXCursor(XC_watch);
					LookupUnusedPins(fp);
					fclose(fp);
					SetOutputXCursor(oldCursor);
					SetChangedFlag(True);
				}
				break;
			}

				/* save all connections to a file */
			case F_ElementConnections:
			{
				ElementTypePtr	element;
				FILE			*fp;
				Cursor			oldCursor;

				if ((SearchObjectByPosition(ELEMENT_TYPE,
						(void **) &element,
						(void **) &element,
						(void **) &element,
						Crosshair.X, Crosshair.Y)) != NO_TYPE)
				{
					if ((fp = OpenConnectionDataFile()) != NULL)
					{
						oldCursor = SetOutputXCursor(XC_watch);
						LookupElementConnections(element, fp);
						fclose(fp);
						SetOutputXCursor(oldCursor);
						SetChangedFlag(True);
					}
				}
				break;
			}
		}
}

/* ---------------------------------------------------------------------------
 * load data
 * syntax: Load(ElementToBuffer|Layout|LayoutToBuffer)
 */
void ActionLoad(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	char	*name;

	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
				/* load element data into buffer */
			case F_ElementToBuffer:
				name = FileSelectBox("load element to buffer:",
					NULL, Settings.ElementPath);
				if (name && LoadElementToBuffer(PASTEBUFFER, name, True))
					SetMode(PASTEBUFFER_MODE);
				break;

				/* load PCB data into buffer */
			case F_LayoutToBuffer:
				name = FileSelectBox("load file to buffer:",
					NULL, Settings.FilePath);
				if (name && LoadLayoutToBuffer(PASTEBUFFER, name))
					SetMode(PASTEBUFFER_MODE);
				break;

				/* load new data */
			case F_Layout:
				name = FileSelectBox("load file:", NULL, Settings.FilePath);
				if (name)
					if (!PCB->Changed ||
						ConfirmDialog("OK to override layout data?"))
						LoadPCB(name);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * print data
 * syntax: Print()
 */
void ActionPrint(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 0)
	{
			/* check if layout is empty */
		if (!IsDataEmpty(PCB->Data))
			PrintDialog();
		else
			Message("can't print empty layout");
	}
}

/* ---------------------------------------------------------------------------
 * starts a new layout
 * syntax: New()
 */
void ActionNew(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	char	*name;

	if (*Num == 0)
	{
		HideCrosshair(True);
		if (!PCB->Changed ||
			ConfirmDialog("OK to clear layout data?"))
		{
			name = GetUserInput("enter the layouts name:", "");
			if (!name)
				return;

				/* do emergency saving
				 * clear the old struct and allocate memory for the new one
				 */
			if (PCB->Changed && Settings.SaveInTMP)
				SaveInTMP();
			RemovePCB(PCB);
			PCB = CreateNewPCB(True);

				/* setup the new name and reset some values to default */
			PCB->Name = name;
			ResetStackAndVisibility();
			CreateDefaultFont();
			SetCrosshairRange(0, 0, PCB->MaxWidth, PCB->MaxHeight);
			UpdateSettingsOnScreen();

				/* reset layout size to resource value */
			XtVaSetValues(Output.Output,
				XtNwidth, TO_SCREEN(PCB->MaxWidth),
				XtNheight, TO_SCREEN(PCB->MaxHeight),
				NULL);

			ClearAndRedrawOutput();
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * swap visible sides
 */
void ActionSwapSides(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	Settings.ShowSolderSide = !Settings.ShowSolderSide;
	CenterDisplay(TO_SCREEN_X(Crosshair.X), TO_SCREEN_Y(Crosshair.Y));

		/* adjust pointer */
	WarpPointer();
	SetStatusLine();
}

/* ---------------------------------------------------------------------------
 * no operation, just for testing purposes
 * syntax: Bell(volume)
 */
void ActionBell(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	XBell(Dpy, *Num == 1 ? atoi(*Params) : Settings.Volume);
}

/* ---------------------------------------------------------------------------
 * paste buffer operations
 * syntax: PasteBuffer(AddSelected|Clear|1..MAX_BUFFER)
 *         PasteBuffer(Rotate, 1..3)
 */
void ActionPasteBuffer(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	HideCrosshair(True);
	if (*Num == 1)
	{
		switch(GetFunctionID(*Params))
		{
				/* clear contents of paste buffer */
			case F_Clear:
				ClearBuffer(PASTEBUFFER);
				break;

				/* copies objects to paste buffer */
			case F_AddSelected:
				AddSelectedToBuffer(PASTEBUFFER);
				break;

				/* set number */
			default:
			{
				int	number = atoi(*Params);

					/* correct number */
				if (number)
					SetBufferNumber(number -1);
			}
		}
	}
	if (*Num == 2)
	{
		switch(GetFunctionID(*Params))
		{
			case F_Rotate:
				if (Settings.Mode == PASTEBUFFER_MODE)
				{
					BYTE	rotate = (BYTE) atoi(*(Params+1));

					RotateBuffer(PASTEBUFFER, rotate);
					SetCrosshairRange(
						PASTEBUFFER->X -PASTEBUFFER->BoundingBox.X1,
						PASTEBUFFER->Y -PASTEBUFFER->BoundingBox.Y1,
						PCB->MaxWidth-
							(PASTEBUFFER->BoundingBox.X2-PASTEBUFFER->X),
						PCB->MaxHeight-
							(PASTEBUFFER->BoundingBox.Y2-PASTEBUFFER->Y));
				}
				break;
		}
	}
	RestoreCrosshair(True);
}

/* ---------------------------------------------------------------------------
 * action routine to 'undo' operations
 * The serial number indicates the operation. This makes it possible
 * to group undo requests.
 * syntax: Undo(ClearList)
 *         Undo()
 */
void ActionUndo(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	HideCrosshair(True);

		/* undo the last destructive operation */
	if (!*Num && Undo())
			SetChangedFlag(True);

	if (*Num == 1)
	{
		switch(GetFunctionID(*Params))
		{
				/* clear 'undo objects' list */
			case F_ClearList:
				ClearUndoList(False);
				break;
		}
	}
	RestoreCrosshair(True);
}

/* ---------------------------------------------------------------------------
 * some polygon related stuff
 * syntax: Polygon(Close|PreviousPoint)
 */
void ActionPolygon(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 1 && Settings.Mode == POLYGON_MODE)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
				/* close open polygon if possible */
			case F_Close:
				ClosePolygon();
				break;

				/* go back to the previous point */
			case F_PreviousPoint:
				GoToPreviousPoint();
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * changes the current drawing-layer
 * syntax: SwitchDrawingLayer()
 */
void ActionSwitchDrawingLayer(Widget W, XEvent *Event,
	String *Params, Cardinal *Num)
{
	int	number;

	if (*Num == 1)
	{
		number = atoi(*Params) -1;
		if (number >= 0 && number < MAX_LAYER)
		{
			ChangeGroupVisibility(number, True, True);
			UpdateControlPanel();
			ClearAndRedrawOutput();
		}
	}
}

/* ---------------------------------------------------------------------------
 * edit layer-groups
 * syntax: EditLayerGroups()
 */
void ActionEditLayerGroups(Widget W, XEvent *Event,
	String *Params, Cardinal *Num)
{
	if (*Num == 0 && LayerGroupDialog())
			ClearAndRedrawOutput();
}

/* ---------------------------------------------------------------------------
 * handles groundplane functions
 * syntax: Groundplane(AddSelected|ClearList|SelectMembers|RemoveSelected)
 */
void ActionGroundplane(Widget W, XEvent *Event, String *Params, Cardinal *Num)
{
	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
				/* add all selected objects to the groundplane */
			case F_AddSelected:
				if (AddSelectedToGroundplane())
					SetChangedFlag(True);
				break;

				/* removes all members from the groundplane list */
			case F_ClearList:
				if (ClearGroundplane())
					SetChangedFlag(True);
				break;

				/* select all objects that are memebers of the groundplane */
			case F_SelectMembers:
				if (SelectGroundplaneMember())
				{
					SetChangedFlag(True);
					RedrawOutput();
				}
				break;

				/* remove all selected objects from the groundplane */
			case F_RemoveSelected:
				if (RemoveSelectedFromGroundplane())
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

/* ---------------------------------------------------------------------------
 * moves objects to the current layer
 * syntax: MoveToCurrentLayer(Object|SelectedObjects)
 */
void ActionMoveToCurrentLayer(Widget W, XEvent *Event,
	String *Params, Cardinal *Num)
{
	if (*Num == 1)
	{
		HideCrosshair(True);
		switch(GetFunctionID(*Params))
		{
			case F_Object:
			{
				int		type;
				void	*ptr1, *ptr2, *ptr3;

				if ((type = SearchObjectByPosition(MOVETOLAYER_TYPES,
					&ptr1, &ptr2, &ptr3, Crosshair.X, Crosshair.Y)) != NO_TYPE)
					if (MoveObjectToLayer(type, ptr1, ptr2, ptr3, CURRENT))
						SetChangedFlag(True);
				break;
			}

			case F_SelectedObjects:
				if (MoveSelectedObjectsToLayer(CURRENT))
					SetChangedFlag(True);
				break;
		}
		RestoreCrosshair(True);
	}
}

