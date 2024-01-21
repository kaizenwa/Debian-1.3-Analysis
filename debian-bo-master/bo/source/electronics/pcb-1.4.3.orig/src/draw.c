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

static	char	*rcsid = "$Id: draw.c,v 143.1 1996/09/16 09:08:35 nau Exp $";

/* drawing routines
 */

/* ---------------------------------------------------------------------------
 * define TO_SCREEN before macro.h is included from global.h
 */
#define	SWAP_IDENT		SwapOutput
#define TO_SCREEN(a)	((a) >> ZoomValue)

#include "global.h"

#include "crosshair.h"
#include "data.h"
#include "draw.h"
#include "mymem.h"
#include "misc.h"
#include "rotate.h"
#include "select.h"

/* ---------------------------------------------------------------------------
 * some local types
 */
typedef struct
{
	float	X,
			Y;
} FloatPolyType, *FloatPolyTypePtr;

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	int					ZoomValue;		/* zoom, drawable and mirror */
static	Window				DrawingWindow;	/* flag common to all */
static	Boolean				SwapOutput;		/* all drawing routines */
static	XPoint				Outline[MAX_SIZE+1][8];

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	Redraw(Boolean);
static	void	DrawEverything(void);
static	void	DrawLayer(LayerTypePtr);
static	void	InitSpecialPolygon(void);
static	void	DrawSpecialPolygon(GC, Position, Position, XPoint *);
static	void	DrawPinOrViaLowLevel(PinTypePtr);
static	void	DrawPinOrViaNameLowLevel(PinTypePtr);
static	void	DrawPadLowLevel(PadTypePtr);
static	void	DrawPadNameLowLevel(PadTypePtr);
static	void	DrawLineLowLevel(LineTypePtr);
static	void	DrawTextLowLevel(TextTypePtr);
static	void	DrawPolygonLowLevel(PolygonTypePtr);
static	void	DrawArcLowLevel(ArcTypePtr);
static	void	DrawElementPackageLowLevel(ElementTypePtr Element);

/* ---------------------------------------------------------------------------
 * redraws the output area without clearing it
 */
void RedrawOutput(void)
{
	Redraw(False);
}

/* ---------------------------------------------------------------------------
 * redraws the output area after clearing it
 */
void ClearAndRedrawOutput(void)
{
	Redraw(True);
}

/* ---------------------------------------------------------------------- 
 * redraws all the data
 * all necessary sizes are already set by the viewport widget and
 * by the event handlers
 */
static void Redraw(Boolean ClearWindow)
{
	XEvent	event;

		/* check if window already exists */
	if (Output.OutputWindow)
	{
			/* remove all other pending expose events from queue */
		while (XtAppPending(Context))
		{
			XtAppNextEvent(Context, &event);
			if (event.type != Expose ||
				((XExposeEvent *) &event)->window != Output.OutputWindow)
				XtDispatchEvent(&event);
		}

			/* switch off crosshair, change drawing window and redraw
			 * everything
			 */
		HideCrosshair(True);
		SwitchDrawingWindow(PCB->Zoom, Output.OutputWindow,
			Settings.ShowSolderSide);

			/* reset the cursor state because of clearing the background
			 * of the drawing area
			 */
		if (ClearWindow)
		{
			XSetForeground(Dpy, Output.fgGC, Settings.OffLimitColor);
			XFillRectangle(Dpy, DrawingWindow, Output.fgGC,
				0, 0, MAX_COORD, MAX_COORD);
			XFillRectangle(Dpy, DrawingWindow, Output.bgGC,
				0, 0, TO_SCREEN(PCB->MaxWidth), TO_SCREEN(PCB->MaxHeight));
			Crosshair.On = False;
		}

		DrawEverything();

			/* redraw might have come from scrolling the window
			 * so we do an update of the cursor position
			 */
		MoveCrosshairRelative(0, 0);
		RestoreCrosshair(True);
	}
}

/* ----------------------------------------------------------------------
 * setup of zoom and output window for the next drawing operations
 */
void SwitchDrawingWindow(int Zoom, Window OutputWindow, Boolean Swap)
{
	ZoomValue = Zoom;
	DrawingWindow = OutputWindow;
	SwapOutput = Swap;
	XSetFont(Dpy, Output.fgGC, Settings.PinoutFont[Zoom]->fid);
	XSetFont(Dpy, Output.bgGC, Settings.PinoutFont[Zoom]->fid);
	InitSpecialPolygon();
}

/* ---------------------------------------------------------------------------
 * initializes some identifiers for a new zoom factor and redraws whole screen
 */
static void DrawEverything(void)
{
	Cardinal	i;

		/* draw all layers beside the current one */
	for (i = MAX_LAYER-1; i; i--)
		if ((LAYER_ON_STACK(i))->On)
			DrawLayer(LAYER_ON_STACK(i));

		/* draw element packages; pins are drawn by RedrawCurrentLayer();
		 * first draw all 'invisible' then all others to prevent from
		 * hiding the ones on top
		 */
	if (PCB->ElementOn)
		ELEMENT_LOOP(PCB->Data,
			if (PCB->InvisibleObjectsOn ||
				(TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT)
			{
				DrawElementPackage(element);
				DrawElementName(element);
			}
		);

		/* make sure that the current layer is always visible */
	RedrawCurrentLayer();
	if (Settings.DrawGrid)
		DrawGrid();
}

/* ---------------------------------------------------------------------------
 * redraws the current layer as well as pins and vias
 * the later ones are drawn at last to make the drilling hole visible
 */
void RedrawCurrentLayer(void)
{
	HideCrosshair(True);
	DrawLayer(CURRENT);

		/* draw element pins */
	if (PCB->PinOn)
		ELEMENT_LOOP(PCB->Data, DrawElementPinsAndPads(element););

		/* draw vias */
	if (PCB->ViaOn)
		VIA_LOOP(PCB->Data, DrawVia(via););

	RestoreCrosshair(True);
}

/* ---------------------------------------------------------------------------
 * draws one layer
 */
static void DrawLayer(LayerTypePtr Layer)
{
		/* draw all objects by calling their drawing routines */
	LINE_LOOP(Layer, DrawLine(Layer, line););
	TEXT_LOOP(Layer, DrawText(Layer, text););
	POLYGON_LOOP(Layer, DrawPolygon(Layer, polygon););
} 

/* ---------------------------------------------------------------------------
 * initializes some zoom dependend information for pins and lines
 * just to speed up drawing a bit
 */
static void InitSpecialPolygon(void)
{
			int				i, j;
	static	FloatPolyType	p[8] = {{       0.5, -TAN_22_5_DEGREE_2},
									{ TAN_22_5_DEGREE_2,       -0.5},
									{-TAN_22_5_DEGREE_2,       -0.5},
									{      -0.5, -TAN_22_5_DEGREE_2},
									{      -0.5,  TAN_22_5_DEGREE_2},
									{-TAN_22_5_DEGREE_2,        0.5},
									{ TAN_22_5_DEGREE_2,        0.5},
									{       0.5,  TAN_22_5_DEGREE_2}};


		/* loop over maximum number of different sizes */
	for (i = MAX(MAX_PINORVIASIZE, MAX_LINESIZE); i != -1; i--)
		for (j = 0; j < 8; j++)
		{
			Outline[i][j].x = (p[j].X * TO_SCREEN(i));
			Outline[i][j].y = (p[j].Y * TO_SCREEN(i));
		}
}

/* ---------------------------------------------------------------------------
 * draws one polygon
 * x and y are already in display coordinates
 * the points are numbered:
 *
 *          5 --- 6
 *         /       \
 *        4         7
 *        |         |
 *        3         0
 *         \       /
 *          2 --- 1
 *
 */
static void DrawSpecialPolygon(GC DrawGC,
	Position X, Position Y, XPoint *PolyPtr)
{
	int		i;
	XPoint	polygon[8];

		/* add line offset */
	for (i = 0; i < 8; i++)
	{
		polygon[i].x = X+ PolyPtr[i].x;
		polygon[i].y = Y+ PolyPtr[i].y;
	}
	XFillPolygon(Dpy, DrawingWindow, DrawGC,
		polygon, ENTRIES(polygon), Convex, CoordModeOrigin);
}

/* ---------------------------------------------------------------------------
 * lowlevel drawing routine for pins and vias
 */
static void DrawPinOrViaLowLevel(PinTypePtr Ptr)
{
	if (TEST_FLAG(SQUAREFLAG, Ptr))
	{
		Dimension	half = Ptr->Thickness/2;

		XFillRectangle(Dpy, DrawingWindow, Output.fgGC,
			TO_SCREEN(Ptr->X -half), TO_SCREEN(Ptr->Y -half),
			TO_SCREEN(Ptr->Thickness), TO_SCREEN(Ptr->Thickness));
	}
	else
	{
		XSetLineAttributes(Dpy, Output.fgGC,
			TO_SCREEN((Ptr->Thickness -Ptr->DrillingHole) /2),
			LineSolid, CapRound, JoinRound);

			/* transform X11 specific coord system */
		DrawSpecialPolygon(Output.fgGC,
			TO_SCREEN_X(Ptr->X), TO_SCREEN_Y(Ptr->Y),
			&Outline[Ptr->Thickness][0]);
	}

		/* and the drilling hole */
	DrawSpecialPolygon(Output.bgGC,
		TO_SCREEN_X(Ptr->X), TO_SCREEN_Y(Ptr->Y),
		&Outline[Ptr->DrillingHole][0]);
}

/* ---------------------------------------------------------------------------
 * lowlevel drawing routine for pin and via names
 */
static void DrawPinOrViaNameLowLevel(PinTypePtr Ptr)
{
	XDrawString(Dpy, DrawingWindow, Output.fgGC,
		TO_SCREEN_X(Ptr->X -Ptr->Thickness/2 +Settings.PinoutTextOffsetX),
		TO_SCREEN_Y(Ptr->Y -Ptr->Thickness/2 +Settings.PinoutTextOffsetY),
		EMPTY(Ptr->Name),
		MIN(Settings.PinoutNameLength, strlen(EMPTY(Ptr->Name))));
}

/* ---------------------------------------------------------------------------
 * lowlevel drawing routine for pads
 * use 'TEST_FLAG(SQUAREFLAG, Pad) ? CapButt : CapRound' for amrked pads
 */
static void DrawPadLowLevel(PadTypePtr Pad)
{
	XSetLineAttributes(Dpy, Output.fgGC,
		TO_SCREEN(Pad->Thickness), LineSolid, CapRound, JoinRound);
	XDrawLine(Dpy, DrawingWindow, Output.fgGC,
		TO_SCREEN_X(Pad->Point1.X), TO_SCREEN_Y(Pad->Point1.Y),
		TO_SCREEN_X(Pad->Point2.X), TO_SCREEN_Y(Pad->Point2.Y));
}

/* ---------------------------------------------------------------------------
 * lowlevel drawing routine for pad names
 */
static void DrawPadNameLowLevel(PadTypePtr Pad)
{
	XDrawString(Dpy, DrawingWindow, Output.fgGC,
		TO_SCREEN_X((Pad->Point1.X +Pad->Point2.X)/2 +
			Settings.PinoutTextOffsetX),
		TO_SCREEN_Y((Pad->Point1.Y +Pad->Point2.Y)/2 +
			Settings.PinoutTextOffsetY),
		EMPTY(Pad->Name),
		MIN(Settings.PinoutNameLength, strlen(EMPTY(Pad->Name))));
}

/* ---------------------------------------------------------------------------
 * lowlevel drawing routine for lines
 */
static void DrawLineLowLevel(LineTypePtr Line)
{
	XSetLineAttributes(Dpy, Output.fgGC,
		TO_SCREEN(Line->Thickness), LineSolid, CapRound, JoinRound);
	XDrawLine(Dpy, DrawingWindow, Output.fgGC,
		TO_SCREEN_X(Line->Point1.X), TO_SCREEN_Y(Line->Point1.Y),
		TO_SCREEN_X(Line->Point2.X), TO_SCREEN_Y(Line->Point2.Y));
}

/* ---------------------------------------------------------------------------
 * lowlevel drawing routine for text objects
 */
static void DrawTextLowLevel(TextTypePtr Text)
{
	Position		x = 0,
					width;
	unsigned char	*string = (unsigned char *) Text->TextString;
	Cardinal		n;
	FontTypePtr		font = &PCB->Font;

		/* get the center of the text for mirroring */
	width = Text->Direction & 0x01 ? 
				Text->BoundingBox.Y2 -Text->BoundingBox.Y1 :
				Text->BoundingBox.X2 -Text->BoundingBox.X1;
	while (string && *string)
	{
			/* draw lines if symbol is valid and data is present */
		if (*string <= MAX_FONTPOSITION && font->Symbol[*string].Valid)
		{
			LineTypePtr	line = font->Symbol[*string].Line;
			LineType	newline;

			for (n = font->Symbol[*string].LineN; n; n--, line++)
			{
					/* create one line, scale, move, rotate and swap it */
				newline = *line;
				newline.Point1.X = (newline.Point1.X +x) *Text->Scale /100;
				newline.Point1.Y = newline.Point1.Y      *Text->Scale /100;
				newline.Point2.X = (newline.Point2.X +x) *Text->Scale /100;
				newline.Point2.Y = newline.Point2.Y      *Text->Scale /100;
				newline.Thickness = newline.Thickness *Text->Scale /100;

					/* do some mirroring, swaping and rotating */
				if (TEST_FLAG(MIRRORFLAG, Text))
				{
					newline.Point1.X = width -newline.Point1.X;
					newline.Point2.X = width -newline.Point2.X;
				}
				RotateLineLowLevel(&newline, 0, 0, Text->Direction);

					/* the labels of SMD objects on the bottom
					 * side haven't been swapped yet, only their offset
					 */
				if (TEST_FLAG(ONSOLDERFLAG, Text))
				{
					newline.Point1.X = SWAP_SIGN_X(newline.Point1.X);
					newline.Point1.Y = SWAP_SIGN_Y(newline.Point1.Y);
					newline.Point2.X = SWAP_SIGN_X(newline.Point2.X);
					newline.Point2.Y = SWAP_SIGN_Y(newline.Point2.Y);
				}
					/* add offset and draw line */
				newline.Point1.X += Text->X;
				newline.Point1.Y += Text->Y;
				newline.Point2.X += Text->X;
				newline.Point2.Y += Text->Y;
				DrawLineLowLevel(&newline);
			}

				/* move on to next cursor position */
			x += (font->Symbol[*string].Width +font->Symbol[*string].Delta);
		}
		else
		{
				/* the default symbol is a filled box */
			BoxType		defaultsymbol = PCB->Font.DefaultSymbol;
			Position	size = (defaultsymbol.X2 -defaultsymbol.X1) *6/5;

			defaultsymbol.X1 = (defaultsymbol.X1 +x) *Text->Scale /100;
			defaultsymbol.Y1 = defaultsymbol.Y1      *Text->Scale /100;
			defaultsymbol.X2 = (defaultsymbol.X2 +x) *Text->Scale /100;
			defaultsymbol.Y2 = defaultsymbol.Y2      *Text->Scale /100;

				/* do some mirroring ... */
			if (TEST_FLAG(MIRRORFLAG, Text))
			{
				defaultsymbol.X1 = width -defaultsymbol.X1;
				defaultsymbol.X2 = width -defaultsymbol.X2;
			}
			if (TEST_FLAG(ONSOLDERFLAG, Text))
			{
				defaultsymbol.X1 = TO_SCREEN_SIGN_X(defaultsymbol.X1);
				defaultsymbol.Y1 = TO_SCREEN_SIGN_Y(defaultsymbol.Y1);
				defaultsymbol.X2 = TO_SCREEN_SIGN_X(defaultsymbol.X2);
				defaultsymbol.Y2 = TO_SCREEN_SIGN_Y(defaultsymbol.Y2);
			}
			RotateBoxLowLevel(&defaultsymbol, 0, 0, Text->Direction);

				/* add offset and draw box */
			defaultsymbol.X1 += Text->X;
			defaultsymbol.Y1 += Text->Y;
			defaultsymbol.X2 += Text->X;
			defaultsymbol.Y2 += Text->Y;
			XFillRectangle(Dpy, DrawingWindow, Output.fgGC,
				TO_SCREEN_X(defaultsymbol.X1),
				TO_SCREEN_Y(SWAP_IDENT ? defaultsymbol.Y2 : defaultsymbol.Y1),
				TO_SCREEN(abs(defaultsymbol.X2 -defaultsymbol.X1)),
				TO_SCREEN(abs(defaultsymbol.Y2 -defaultsymbol.Y1)));

				/* move on to next cursor position */
			x += size;
		}
		string++;
	}
}

/* ---------------------------------------------------------------------------
 * lowlevel drawing routine for polygons
 */
static void DrawPolygonLowLevel(PolygonTypePtr Polygon)
{
	static	XPoint		*data = NULL;	/* tmp pointer */
	static	Cardinal	max = 0;

		/* allocate memory for data with screen coordinates */
	if (Polygon->PointN > max)
	{
		max = Polygon->PointN;
		data = (XPoint *) MyRealloc(data, max *sizeof(XPoint),
			"DrawPolygonLowLevel()");
	}

		/* copy data to tmp array and convert it to screen coordinates */
	POLYGONPOINT_LOOP(Polygon,
		data[n].x = TO_SCREEN_X(point->X);
		data[n].y = TO_SCREEN_Y(point->Y);
	);
	XFillPolygon(Dpy, DrawingWindow, Output.fgGC,
		data, Polygon->PointN, Complex, CoordModeOrigin);
}

/* ---------------------------------------------------------------------------
 * lowlevel routine to element arcs
 */
static void DrawArcLowLevel(ArcTypePtr Arc)
{
		/* angles have to be converted to X11 notation */
	XSetLineAttributes(Dpy, Output.fgGC,
		TO_SCREEN(Arc->Thickness), LineSolid, CapRound, JoinRound);
	XDrawArc(Dpy, DrawingWindow, Output.fgGC,
		TO_SCREEN_X(Arc->X) -TO_SCREEN(Arc->Width),
		TO_SCREEN_Y(Arc->Y) -TO_SCREEN(Arc->Height),
		TO_SCREEN(2*Arc->Width), TO_SCREEN(2*Arc->Height),
		(TO_SCREEN_ANGLE(Arc->StartAngle) -180) *64,
		TO_SCREEN_DELTA(Arc->Delta) *64);
}

/* ---------------------------------------------------------------------------
 * draws the package of an element
 */
static void DrawElementPackageLowLevel(ElementTypePtr Element)
{
		/* draw lines, arcs, text and pins */
	ELEMENTLINE_LOOP(Element, DrawLineLowLevel(line););
	ARC_LOOP(Element, DrawArcLowLevel(arc););
}

/* ---------------------------------------------------------------------------
 * draw a via object
 */
void DrawVia(PinTypePtr Via)
{
	if (TEST_FLAG(SELECTEDFLAG | FOUNDFLAG | ONGROUNDPLANEFLAG, Via))
	{
		if (TEST_FLAG(SELECTEDFLAG, Via))
			XSetForeground(Dpy, Output.fgGC, PCB->ViaSelectedColor);
		else
			if (TEST_FLAG(FOUNDFLAG, Via))
				XSetForeground(Dpy, Output.fgGC, PCB->ConnectedColor);
			else
				XSetForeground(Dpy, Output.fgGC, PCB->GroundplaneColor);
	}
	else
		XSetForeground(Dpy, Output.fgGC, PCB->ViaColor);
	DrawPinOrViaLowLevel(Via);
	if (TEST_FLAG(DISPLAYNAMEFLAG, Via))
		DrawPinOrViaNameLowLevel(Via);
}

/* ---------------------------------------------------------------------------
 * draws the name of a via
 */
void DrawViaName(PinTypePtr Via)
{
	if (TEST_FLAG(SELECTEDFLAG, Via))
		XSetForeground(Dpy, Output.fgGC, PCB->ViaSelectedColor);
	else
		XSetForeground(Dpy, Output.fgGC, PCB->ViaColor);
	DrawPinOrViaNameLowLevel(Via);
}

/* ---------------------------------------------------------------------------
 * draw a pin object
 */
void DrawPin(PinTypePtr Pin)
{
	if (TEST_FLAG(SELECTEDFLAG | FOUNDFLAG | ONGROUNDPLANEFLAG, Pin))
	{
		if (TEST_FLAG(SELECTEDFLAG, Pin))
			XSetForeground(Dpy, Output.fgGC, PCB->PinSelectedColor);
		else
			if (TEST_FLAG(FOUNDFLAG, Pin))
				XSetForeground(Dpy, Output.fgGC, PCB->ConnectedColor);
			else
				XSetForeground(Dpy, Output.fgGC, PCB->GroundplaneColor);
	}
	else
		XSetForeground(Dpy, Output.fgGC, PCB->PinColor);
	DrawPinOrViaLowLevel(Pin);
	if (TEST_FLAG(DISPLAYNAMEFLAG, Pin))
		DrawPinOrViaNameLowLevel(Pin);
}

/* ---------------------------------------------------------------------------
 * draws the name of a pin
 */
void DrawPinName(PinTypePtr Pin)
{
	if (TEST_FLAG(SELECTEDFLAG, Pin))
		XSetForeground(Dpy, Output.fgGC, PCB->PinSelectedColor);
	else
		XSetForeground(Dpy, Output.fgGC, PCB->PinColor);
	DrawPinOrViaNameLowLevel(Pin);
}

/* ---------------------------------------------------------------------------
 * draw a pad object
 */
void DrawPad(PadTypePtr Pad)
{
	if (TEST_FLAG(SELECTEDFLAG | FOUNDFLAG, Pad))
	{
		if (TEST_FLAG(SELECTEDFLAG, Pad))
			XSetForeground(Dpy, Output.fgGC, PCB->PinSelectedColor);
		else
				XSetForeground(Dpy, Output.fgGC, PCB->ConnectedColor);
	}
	else
		if ((TEST_FLAG(ONSOLDERFLAG, Pad) != 0) == SWAP_IDENT)
			XSetForeground(Dpy, Output.fgGC, PCB->PinColor);
		else
			XSetForeground(Dpy, Output.fgGC, PCB->InvisibleObjectsColor);
	DrawPadLowLevel(Pad);
	if (TEST_FLAG(DISPLAYNAMEFLAG, Pad))
		DrawPadNameLowLevel(Pad);
}

/* ---------------------------------------------------------------------------
 * draws the name of a pad
 */
void DrawPadName(PadTypePtr Pad)
{
	if (TEST_FLAG(SELECTEDFLAG, Pad))
		XSetForeground(Dpy, Output.fgGC, PCB->PinSelectedColor);
	else
		if ((TEST_FLAG(ONSOLDERFLAG, Pad) != 0) == SWAP_IDENT)
			XSetForeground(Dpy, Output.fgGC, PCB->PinColor);
		else
			XSetForeground(Dpy,Output.fgGC, PCB->InvisibleObjectsColor);
	DrawPadNameLowLevel(Pad);
}

/* ---------------------------------------------------------------------------
 * draws a line on a layer
 */
void DrawLine(LayerTypePtr Layer, LineTypePtr Line)
{
	if (TEST_FLAG(SELECTEDFLAG | FOUNDFLAG, Line))
	{
		if (TEST_FLAG(SELECTEDFLAG, Line))
			XSetForeground(Dpy, Output.fgGC, Layer->SelectedColor);
		else
			XSetForeground(Dpy, Output.fgGC, PCB->ConnectedColor);
	}
	else
		XSetForeground(Dpy, Output.fgGC, Layer->Color);
	DrawLineLowLevel(Line);
}

/* ---------------------------------------------------------------------------
 * draws a text on a layer
 */
void DrawText(LayerTypePtr Layer, TextTypePtr Text)
{
	if (TEST_FLAG(SELECTEDFLAG, Text))
		XSetForeground(Dpy, Output.fgGC, Layer->SelectedColor);
	else
		XSetForeground(Dpy, Output.fgGC, Layer->Color);
	DrawTextLowLevel(Text);
}

/* ---------------------------------------------------------------------------
 * draws a polygon on a layer
 */
void DrawPolygon(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
	if (TEST_FLAG(SELECTEDFLAG | FOUNDFLAG, Polygon))
	{
		if (TEST_FLAG(SELECTEDFLAG, Polygon))
			XSetForeground(Dpy, Output.fgGC, Layer->SelectedColor);
		else
			XSetForeground(Dpy, Output.fgGC, PCB->ConnectedColor);
	}
	else
		XSetForeground(Dpy, Output.fgGC, Layer->Color);
	DrawPolygonLowLevel(Polygon);
}

/* ---------------------------------------------------------------------------
 * draws an element
 */
void DrawElement(ElementTypePtr Element)
{
	DrawElementPackage(Element);
	DrawElementName(Element);
	DrawElementPinsAndPads(Element);
} 

/* ---------------------------------------------------------------------------
 * draws the name of an element
 */
void DrawElementName(ElementTypePtr Element)
{
	if (TEST_FLAG(SELECTEDFLAG, &ELEMENT_TEXT(PCB, Element)))
		XSetForeground(Dpy, Output.fgGC, PCB->ElementSelectedColor);
	else
		if ((TEST_FLAG(ONSOLDERFLAG, Element) != 0) == SWAP_IDENT)
			XSetForeground(Dpy, Output.fgGC, PCB->ElementColor);
		else
			XSetForeground(Dpy, Output.fgGC, PCB->InvisibleObjectsColor);
	DrawTextLowLevel(&ELEMENT_TEXT(PCB, Element));
}

/* ---------------------------------------------------------------------------
 * draws the package of an element
 */
void DrawElementPackage(ElementTypePtr Element)
{
		/* set color and draw lines, arcs, text and pins */
	if (TEST_FLAG(SELECTEDFLAG, Element))
		XSetForeground(Dpy, Output.fgGC, PCB->ElementSelectedColor);
	else
		if ((TEST_FLAG(ONSOLDERFLAG, Element) != 0) == SWAP_IDENT)
			XSetForeground(Dpy, Output.fgGC, PCB->ElementColor);
		else
			XSetForeground(Dpy, Output.fgGC, PCB->InvisibleObjectsColor);
	DrawElementPackageLowLevel(Element);
}

/* ---------------------------------------------------------------------------
 * draw pins of an element
 */
void DrawElementPinsAndPads(ElementTypePtr Element)
{
	PAD_LOOP(Element,
		if ((TEST_FLAG(ONSOLDERFLAG, pad) != 0) == SWAP_IDENT ||
			PCB->InvisibleObjectsOn)
			DrawPad(pad);
	);
	PIN_LOOP(Element,
		DrawPin(pin);
	);
}

/* ---------------------------------------------------------------------------
 * erase a via object
 */
void EraseVia(PinTypePtr Via)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawPinOrViaLowLevel(Via);
	if (TEST_FLAG(DISPLAYNAMEFLAG, Via))
		DrawPinOrViaNameLowLevel(Via);
}

/* ---------------------------------------------------------------------------
 * erase a via name
 */
void EraseViaName(PinTypePtr Via)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawPinOrViaNameLowLevel(Via);
}

/* ---------------------------------------------------------------------------
 * erase a pad object
 */
void ErasePad(PadTypePtr Pad)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawPadLowLevel(Pad);
	if (TEST_FLAG(DISPLAYNAMEFLAG, Pad))
		DrawPadNameLowLevel(Pad);
}

/* ---------------------------------------------------------------------------
 * erase a pad name
 */
void ErasePadName(PadTypePtr Pad)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawPadNameLowLevel(Pad);
}

/* ---------------------------------------------------------------------------
 * erase a pin object
 */
void ErasePin(PinTypePtr Pin)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawPinOrViaLowLevel(Pin);
	if (TEST_FLAG(DISPLAYNAMEFLAG, Pin))
		DrawPinOrViaNameLowLevel(Pin);
}

/* ---------------------------------------------------------------------------
 * erase a pin name
 */
void ErasePinName(PinTypePtr Pin)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawPinOrViaNameLowLevel(Pin);
}

/* ---------------------------------------------------------------------------
 * erases a line on a layer
 */
void EraseLine(LineTypePtr Line)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawLineLowLevel(Line);
}

/* ---------------------------------------------------------------------------
 * erases a text on a layer
 */
void EraseText(TextTypePtr Text)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawTextLowLevel(Text);
}

/* ---------------------------------------------------------------------------
 * erases a polygon on a layer
 */
void ErasePolygon(PolygonTypePtr Polygon)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawPolygonLowLevel(Polygon);
}

/* ---------------------------------------------------------------------------
 * erases an element
 */
void EraseElement(ElementTypePtr Element)
{
		/* set color and draw lines, arcs, text and pins */
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	ELEMENTLINE_LOOP(Element, DrawLineLowLevel(line););
	ARC_LOOP(Element, DrawArcLowLevel(arc););
	DrawTextLowLevel(&ELEMENT_TEXT(PCB, Element));
	EraseElementPinsAndPads(Element);
} 

/* ---------------------------------------------------------------------------
 * erases all pins and pads of an element
 */
void EraseElementPinsAndPads(ElementTypePtr Element)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	PIN_LOOP(Element,
		DrawPinOrViaLowLevel(pin);
		if (TEST_FLAG(DISPLAYNAMEFLAG, pin))
			DrawPinOrViaNameLowLevel(pin);
	);
	PAD_LOOP(Element,
		DrawPadLowLevel(pad);
		if (TEST_FLAG(DISPLAYNAMEFLAG, pad))
			DrawPadNameLowLevel(pad);
	);
}

/* ---------------------------------------------------------------------------
 * erases the name of an element
 */
void EraseElementName(ElementTypePtr Element)
{
	XSetForeground(Dpy, Output.fgGC, Settings.bgColor);
	DrawTextLowLevel(&ELEMENT_TEXT(PCB, Element));
}

/* ---------------------------------------------------------------------------
 * draws grid points if the distance is >= MIN_GRID_DISTANCE
 */
void DrawGrid(void)
{
	Position	minx, miny,
				maxx, maxy,
				x, y;

	if (TO_SCREEN(PCB->Grid) >= MIN_GRID_DISTANCE)
	{
		minx = TO_PCB_X(Output.OffsetX);
		miny = TO_PCB_Y(Output.OffsetY);;
		maxx = TO_PCB_X(Output.OffsetX +Output.Width);
		maxy = TO_PCB_Y(Output.OffsetY +Output.Height);
		maxx = MIN((Dimension) maxx, PCB->MaxWidth);
		maxy = MIN((Dimension) maxy, PCB->MaxHeight);
		for (y = miny; y <= maxy; y += PCB->Grid)
			for (x = minx; x <= maxx; x += PCB->Grid)
				XDrawPoint(Dpy, Output.OutputWindow, Output.GridGC,
					TO_SCREEN_X(GRIDFIT_X(x)), TO_SCREEN_Y(GRIDFIT_Y(y)));
	}
}
