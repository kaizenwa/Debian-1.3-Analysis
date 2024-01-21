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

static	char	*rcsid = "$Id: set.c,v 143.1 1996/09/16 09:08:59 nau Exp $";

/* routines to update widgets and global settings
 * (except output window and dialogs)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "global.h"

#include "buffer.h"
#include "crosshair.h"
#include "control.h"
#include "data.h"
#include "draw.h"
#include "misc.h"
#include "set.h"

#include <X11/cursorfont.h>
#include <X11/Xaw/Form.h>

/* ---------------------------------------------------------------------------
 * output of cursor position
 */
void SetCursorStatusLine(void)
{
	char	text[20];

	sprintf(text, "%-i,%-i", Crosshair.X, Crosshair.Y);
	XtVaSetValues(Output.CursorPosition, XtNlabel, text, NULL);
}

/* ---------------------------------------------------------------------------
 * output of status line
 */
void SetStatusLine(void)
{
	char	text[140];
	int		length;

	sprintf(text, "%c %s,%s,%s, grid=%i, zoom=1:%-i, line=%-i, via=%-i(%-i), text=%i%%, buffer=#%-i, name: ",
		PCB->Changed ? '*' : ' ',
		Settings.ShowSolderSide ? "solder" : "component",
		TEST_FLAG(ABSOLUTEFLAG, PCB) ? "abs" : "rel",
		TEST_FLAG(ALLDIRCETIONFLAG, PCB) ? "all" : "45",
		PCB->Grid,
		(int) (TO_PCB(1)),
		(int) Settings.LineThickness,
		(int) Settings.ViaThickness, (int) Settings.ViaDrillingHole,
		(int) Settings.TextScale,
		Settings.BufferNumber+1);

		/* append the name of the layout */
	length = sizeof(text) -1 -strlen(text);
	strncat(text, UNKNOWN(PCB->Name), length);
	text[sizeof(text) -1] = '\0';
	XtVaSetValues(Output.StatusLine, XtNlabel, text, NULL);
}

/* ---------------------------------------------------------------------------
 * sets cursor grid with respect to grid offset values
 */
void SetGrid(int Grid)
{
	if (Grid >= 1 && Grid <= MAX_GRID)
	{
			/* set offset relative to current location or absolute
			 * to (0,0) depending on resource
			 * DrawGrid() uses XOR mode
			 */
		if (Settings.DrawGrid)
			DrawGrid();
		PCB->GridOffsetX= TEST_FLAG(ABSOLUTEFLAG, PCB) ? 0 : Crosshair.X % Grid;
		PCB->GridOffsetY= TEST_FLAG(ABSOLUTEFLAG, PCB) ? 0 : Crosshair.Y % Grid;
		PCB->Grid = Grid;
		if (Settings.DrawGrid)
			DrawGrid();
		SetStatusLine();
	}
} 

/* ---------------------------------------------------------------------------
* sets new zoom factor, adapts size of viewport and centers the cursor
*/
void SetZoom(int Zoom)
{
	Zoom = MAX(MIN_ZOOM, Zoom);
	Zoom = MIN(MAX_ZOOM, Zoom);

		/* redraw only if zoom has changed */
	if (PCB->Zoom != Zoom)
	{
			/* saved pixmap is invalid */
		ReleaseSaveUnderPixmap();
		PCB->Zoom = Zoom;

			/* recalculate size of viewport */
		XtVaSetValues(Output.Output,
			XtNwidth, TO_SCREEN(PCB->MaxWidth),
			XtNheight, TO_SCREEN(PCB->MaxHeight),
			NULL);

		CenterDisplay(TO_SCREEN_X(Crosshair.X), TO_SCREEN_Y(Crosshair.Y));
	}
		/* always redraw status line (used for init sequence) */
	SetStatusLine();
} 

/* ---------------------------------------------------------------------------
 * sets a new line thickness
 */
void SetLineSize(Dimension Size)
{
	if (Size >= MIN_LINESIZE && Size <= MAX_LINESIZE)
	{
		Settings.LineThickness = Size;
		SetStatusLine();
	}
}

/* ---------------------------------------------------------------------------
 * sets a new via thickness
 */
void SetViaSize(Dimension Size, Boolean Force)
{
	if (Force || (Size <= MAX_PINORVIASIZE &&
		Size >= MIN_PINORVIASIZE &&
		Size >= Settings.ViaDrillingHole +MIN_PINORVIACOPPER))
	{
		Settings.ViaThickness = Size;
		SetStatusLine();
	}
}

/* ---------------------------------------------------------------------------
 * sets a new via drilling hole
 */
void SetViaDrillingHole(Dimension Size, Boolean Force)
{
	if (Force || (Size <= MAX_PINORVIASIZE &&
		Size >= MIN_PINORVIAHOLE &&
		Size <= Settings.ViaThickness -MIN_PINORVIACOPPER))
	{
		Settings.ViaDrillingHole = Size;
		SetStatusLine();
	}
}

/* ---------------------------------------------------------------------------
 * sets a text scaling
 */
void SetTextScale(Dimension Scale)
{
	if (Scale <= MAX_TEXTSCALE && Scale >= MIN_TEXTSCALE)
	{
		Settings.TextScale = Scale;
		SetStatusLine();
	}
}

/* ---------------------------------------------------------------------------
 * sets or resets changed flag and redraws status
 */
void SetChangedFlag(Boolean New)
{
		/* seems that the save output area is invalid now */
	ReleaseSaveUnderPixmap();
	PCB->Changed = New;
	SetStatusLine();
}

/* ---------------------------------------------------------------------------
 * sets a new buffer number
 */
void SetBufferNumber(int Number)
{
	if (Number >= 0 && Number < MAX_BUFFER)
	{
		Settings.BufferNumber = Number;

			/* do an update on the crosshair range */
		if (Settings.Mode == PASTEBUFFER_MODE)
		{
			SetBufferBoundingBox(PASTEBUFFER);
			SetCrosshairRange(
				PASTEBUFFER->X -PASTEBUFFER->BoundingBox.X1,
				PASTEBUFFER->Y -PASTEBUFFER->BoundingBox.Y1,
				PCB->MaxWidth-
					(PASTEBUFFER->BoundingBox.X2 -PASTEBUFFER->X),
				PCB->MaxHeight-
					(PASTEBUFFER->BoundingBox.Y2 -PASTEBUFFER->Y));
		}
		SetStatusLine();
	}
}

/* ---------------------------------------------------------------------------
 * updates all widgets like status, cursor position ... on screen
 */
void UpdateSettingsOnScreen(void)
{
	SetStatusLine();
	SetCursorStatusLine();
	UpdateControlPanel();
}

/* ---------------------------------------------------------------------------
 * set a new mode and update X cursor
 */
void SetMode(int Mode)
{
		/* protect the cursor while changing the mode
		 * perform some additional stuff depending on the new mode
		 * reset 'state' of attached objects
		 */
	HideCrosshair(True);
	Crosshair.AttachedBox.State = STATE_FIRST;
	Crosshair.AttachedLine.State = STATE_FIRST;
	Crosshair.AttachedObject.State = STATE_FIRST;
	Crosshair.AttachedPolygon.PointN = 0;

		/* reset 'valid' cursor area to maximum;
		 * some modes force a change in the 'case' statement
		 */
	SetCrosshairRange(0, 0, PCB->MaxWidth, PCB->MaxHeight);

	Settings.Mode = Mode;
	switch(Mode)
	{
		case NO_MODE:
			SetOutputXCursor(XC_crosshair);
			break;

		case VIA_MODE:
			SetOutputXCursor(XC_arrow);
			break;

		case LINE_MODE:
			Crosshair.AttachedLine.State = STATE_FIRST;
			SetOutputXCursor(XC_pencil);
			break;

		case POLYGON_MODE:
			SetOutputXCursor(XC_sb_up_arrow);
			break;

		case PASTEBUFFER_MODE:
			SetOutputXCursor(XC_hand1);

				/* do an update on the crosshair range */
			SetBufferBoundingBox(PASTEBUFFER);
			SetCrosshairRange(
				PASTEBUFFER->X -PASTEBUFFER->BoundingBox.X1,
				PASTEBUFFER->Y -PASTEBUFFER->BoundingBox.Y1,
				PCB->MaxWidth-
					(PASTEBUFFER->BoundingBox.X2 -PASTEBUFFER->X),
				PCB->MaxHeight-
					(PASTEBUFFER->BoundingBox.Y2 -PASTEBUFFER->Y));
			break;

		case TEXT_MODE:
			SetOutputXCursor(XC_xterm);
			break;

		case RECTANGLE_MODE:
			SetOutputXCursor(XC_ul_angle);
			break;

		case REMOVE_MODE:
			SetOutputXCursor(XC_pirate);
			break;

		case MIRROR_MODE:
			SetOutputXCursor(XC_sb_h_double_arrow);
			break;

		case ROTATE_MODE:
			SetOutputXCursor(XC_exchange);
			break;

		case COPY_MODE:
		case MOVE_MODE:
		case RUBBERBANDMOVE_MODE:
			SetOutputXCursor(XC_crosshair);
			break;

		case INSERTPOINT_MODE:
			SetOutputXCursor(XC_dotbox);
			break;
	}
	UpdateModeSelection();

		/* force a crosshair grid update because the valid range
		 * may have changed
		 */
	MoveCrosshairRelative(0, 0);
	RestoreCrosshair(True);
}

