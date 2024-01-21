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

static	char	*rcsid = "$Id: control.c,v 143.1 1996/09/16 09:08:30 nau Exp $";

/* control panel
 */

#include <stdio.h>
#include <stdlib.h>

#include "global.h"

#include "control.h"
#include "data.h"
#include "draw.h"
#include "misc.h"
#include "menu.h"
#include "set.h"

#include <X11/Shell.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Toggle.h>

/* ---------------------------------------------------------------------------
 * include bitmap data
 */
#include "mode_icon.data"

/* ---------------------------------------------------------------------------
 * some local defines
 */
#define	BUTTONS_PER_ROW		3	/* number of mode buttons per row */

/* ---------------------------------------------------------------------------
 * some local types
 */
typedef struct					/* description for a single mode button */
{
	char			*Name,		/* the widgets name and label */
					*Label;
	int				Mode;		/* mode ID */
	char			*Bitmap;	/* background bitmap data */
	unsigned int	Width,		/* bitmap size */
					Height;
	Widget			W;
} ModeButtonType, *ModeButtonTypePtr;

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	Widget		AddLabel(String, Widget, Widget, Widget);
static	void		AddDrawingLayerSelection(Widget);
static	void		AddOnOffSelection(Widget);
static	void		UpdateDrawingLayerSelection(void);
static	void		UpdateOnOffSelection(void);
static	Cardinal	GetGroupOfLayer(Cardinal);
static	void		PushOnTopOfLayerStack(int);
static	void		CBPOPUP_SetDrawingLayer(Widget, XtPointer, XtPointer);
static	void		CB_SetOnOff(Widget, XtPointer, XtPointer);
static	void		CB_SetDrawingLayer(Widget, XtPointer, XtPointer);
static	void		CB_SetMode(Widget, XtPointer, XtPointer);

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Widget	OnOffWidgets[MAX_LAYER+4];	/* widgets for on/off selection */

static	PopupEntryType	DrawingLayerMenuEntries[MAX_LAYER+1];
static	PopupMenuType	DrawingLayerMenu =
	{ "drawingLayer", NULL, DrawingLayerMenuEntries,
		CBPOPUP_SetDrawingLayer, NULL, NULL };
static	MenuButtonType	DrawingLayerMenuButton =
	{ "drawingLayer", "DrawingLayer", &DrawingLayerMenu, NULL};

static	ModeButtonType	ModeButtons[] = {
	{ "viaIcon", "via", VIA_MODE, ViaModeIcon_bits,
		ViaModeIcon_width, ViaModeIcon_height, NULL },
	{ "lineIcon", "line", LINE_MODE, LineModeIcon_bits,
		LineModeIcon_width, LineModeIcon_height, NULL },
	{ "rectangleIcon", "rectangle", RECTANGLE_MODE, RectModeIcon_bits,
		RectModeIcon_width, RectModeIcon_height, NULL },
	{ "textIcon", "text", TEXT_MODE, TextModeIcon_bits,
		TextModeIcon_width, TextModeIcon_height, NULL },
	{ "polygonIcon", "polygon", POLYGON_MODE, PolygonModeIcon_bits,
		PolygonModeIcon_width, PolygonModeIcon_height, NULL },
	{ "bufferIcon", "buffer", PASTEBUFFER_MODE, BufferModeIcon_bits,
		BufferModeIcon_width, BufferModeIcon_height, NULL },
	{ "removeIcon", "remove", REMOVE_MODE, RemoveModeIcon_bits,
		RemoveModeIcon_width, RemoveModeIcon_height, NULL },
	{ "mirrorIcon", "mirror", MIRROR_MODE, MirrorModeIcon_bits,
		MirrorModeIcon_width, MirrorModeIcon_height, NULL },
	{ "rotateIcon", "rotate", ROTATE_MODE, RotateModeIcon_bits,
		RotateModeIcon_width, RotateModeIcon_height, NULL },
	{ "insertPointIcon", "insertPoint", INSERTPOINT_MODE,
		InsertPointModeIcon_bits, InsertPointModeIcon_width,
		InsertPointModeIcon_height, NULL }};

/* ---------------------------------------------------------------------------
 * adds a label without border, surrounded by additional space
 * at the specified position
 */
static Widget AddLabel(String Label, Widget Parent, Widget Top, Widget Left)
{
	Widget		label;
	Dimension	height;

	label = XtVaCreateManagedWidget("label", labelWidgetClass,
		Parent,
		XtNlabel, Label,
		XtNfromHoriz, Left,
		XtNfromVert, Top,
		XtNjustify, XtJustifyCenter,
		LAYOUT_TOP,
		NULL);
	XtVaGetValues(label, XtNheight, &height, NULL);
	XtVaSetValues(label, XtNheight, 3*height/2, NULL);
	return(label);
}

/* ---------------------------------------------------------------------------
 * create drawing-layer selection menu
 */
static void AddDrawingLayerSelection(Widget Parent)
{
	static	char	name[MAX_LAYER][20];
			int		i;
			Widget	label;
			Pixel	background;

		/* fill struct and initialize menu button */
	for (i = 0; i < MAX_LAYER; i++)
	{
			/* the widgets name is used for the 'CheckEntry()' funtion
			 * and must be declared static therefore
			 */
		sprintf(name[i], "layer%-i", i+1);
		DrawingLayerMenuEntries[i].Name = name[i];
		DrawingLayerMenuEntries[i].Label = name[i];
		DrawingLayerMenuEntries[i].Callback = CB_SetDrawingLayer;
		DrawingLayerMenuEntries[i].ClientData = (XtPointer) i;
	}

		/* init routine exits on NULL pointer */
	DrawingLayerMenuEntries[i].Name = NULL;
	DrawingLayerMenuEntries[i].Label = NULL;
	label = AddLabel("current", Parent, NULL, NULL);
	InitMenuButton(Parent, &DrawingLayerMenuButton, label, NULL);
	XtVaGetValues(DrawingLayerMenuButton.W,
		XtNbackground, &background,
		NULL);
	XtVaSetValues(DrawingLayerMenuButton.W,
		XtNforeground, background,
		NULL);
}

/* ---------------------------------------------------------------------------
 * adds the layer On/Off selection buttons to our dialog (downwards)
 * the label for the layer buttons are set by UpdateOnOffSelection()
 */
static void AddOnOffSelection(Widget Parent)
{
	static	char	name[MAX_LAYER+4][20];
			Widget	last;
			int		i;
			char	*text;
			Pixel	color;

		/* insert a label at the top */
	last = AddLabel("on/off", Parent, NULL, NULL);

	for (i = 0; i < MAX_LAYER+4; i++)
	{
		switch(i)
		{
			case MAX_LAYER:			/* settings for elements */
				color = PCB->ElementColor;
				text = "pkg. outline";
				strcpy(name[i], "elements");
				break;

			case MAX_LAYER+1:		/* settings for pins */
				color = PCB->PinColor;
				text = "pkg. pins";
				strcpy(name[i], "pins");
				break;

			case MAX_LAYER+2:		/* settings for vias */
				color = PCB->ViaColor;
				text = "vias";
				strcpy(name[i], "vias");
				break;

			case MAX_LAYER+3:		/* settings for invisible objects */
				color = PCB->InvisibleObjectsColor;
				text = "invis. obj.";
				strcpy(name[i], "invisible");
				break;

			default:				/* layers */
				color = PCB->Data->Layer[i].Color;
				text = "";
				sprintf(name[i], "layer%-i", i+1);
				break;
		}

			/* create widget and install callback function */
		OnOffWidgets[i] = XtVaCreateManagedWidget(name[i],toggleWidgetClass,
			Parent,
			XtNlabel, text,
			XtNforeground, color,
			XtNfromHoriz, NULL,
			XtNfromVert, last,
			LAYOUT_TOP,
			NULL);
		last = OnOffWidgets[i];
		XtAddCallback(last, XtNcallback, CB_SetOnOff, (XtPointer) i);
	}
}

/* ---------------------------------------------------------------------------
 * updates the drawing-layer selection menu
 */
static void UpdateDrawingLayerSelection(void)
{
	int		i;

		/* PCB may have changed; we have to make sure that the
		 * labels are set correct
		 */
	for (i = 0; i < MAX_LAYER; i++)
		XtVaSetValues(DrawingLayerMenuEntries[i].W,
			XtNlabel, UNKNOWN(PCB->Data->Layer[i].Name),
			XtNforeground, PCB->Data->Layer[i].Color,
			NULL);

		/* set the label of the menu-nutton itself and reset a flag */
	XtVaSetValues(DrawingLayerMenuButton.W,
		XtNlabel, UNKNOWN(CURRENT->Name),
		XtNbackground, CURRENT->Color,
		NULL);
	RedrawOnEnter = False;
}

/* ---------------------------------------------------------------------------
 * updates the layer On/Off selection buttons
 */
static void UpdateOnOffSelection(void)
{
	int		i;

		/* the buttons for elements, vias and pins never
		 * change their label, so we only check the layer buttons
		 */
	for (i = 0; i < MAX_LAYER; i++)
		XtVaSetValues(OnOffWidgets[i],
			XtNlabel, UNKNOWN(PCB->Data->Layer[i].Name),
			XtNstate, PCB->Data->Layer[i].On,
			NULL);

		/* now set the state of elements, pins and vias */
	XtVaSetValues(OnOffWidgets[i++], XtNstate, PCB->ElementOn, NULL);
	XtVaSetValues(OnOffWidgets[i++], XtNstate, PCB->PinOn, NULL);
	XtVaSetValues(OnOffWidgets[i++], XtNstate, PCB->ViaOn, NULL);
	XtVaSetValues(OnOffWidgets[i++], XtNstate, PCB->InvisibleObjectsOn, NULL);
}

/* ----------------------------------------------------------------------
 * lookup the group to which a layer belongs to
 * returns MAX_LAYER if no group is found
 */
static Cardinal GetGroupOfLayer(Cardinal Layer)
{
	int	group,
		i;

	for (group = 0; group < MAX_LAYER; group++)
		for (i = 0; i < PCB->LayerGroups.Number[group]; i++)
			if (PCB->LayerGroups.Entries[group][i] == Layer)
				return(group);
	return(MAX_LAYER);
}

/* ----------------------------------------------------------------------
 * changes the visibility of all layers in a group
 * returns the number of changed layers
 */
int ChangeGroupVisibility(Cardinal Layer, Boolean On, Boolean ChangeStackOrder)
{
	int		group,
			i,
			changed = 1;	/* at least the current layer changes */

		/* decrement 'i' to keep stack in order of layergroup */
	if ((group = GetGroupOfLayer(Layer)) < MAX_LAYER)
		for (i = PCB->LayerGroups.Number[group]; i;) 
		{
			int	layer = PCB->LayerGroups.Entries[group][--i];

				/* dont count the passed member of the group */
			if (layer != Layer)
			{
				PCB->Data->Layer[layer].On = On;
	
					/* push layer on top of stack if switched on */
				if (On && ChangeStackOrder)
					PushOnTopOfLayerStack(layer);
				changed++;
			}
		}

		/* change at least the passed layer */
	PCB->Data->Layer[Layer].On = On;
	if (On && ChangeStackOrder)
		PushOnTopOfLayerStack(Layer);

		/* update control panel and exit */
	UpdateOnOffSelection();
	return(changed);
}

/* ---------------------------------------------------------------------------
 * move layer (number is passed in) to top of layerstack
 */
static void PushOnTopOfLayerStack(int NewTop)
{
	int		i;

		/* ignore COMPONENT_LAYER and SOLDER_LAYER */
	if (NewTop < MAX_LAYER)
	{
			/* first find position of passed one */
		for (i = 0; i < MAX_LAYER; i++)
			if (LayerStack[i] == NewTop)
				break;

			/* bring this element to the top of the stack */
		for(; i; i--)
			LayerStack[i] = LayerStack[i-1];
		LayerStack[0] = NewTop;
	}
}

/* ---------------------------------------------------------------------------
 * callback routine, called when drawing-layer menu pops up
 */
static void CBPOPUP_SetDrawingLayer(Widget W,
	XtPointer ClientData, XtPointer CallData)
{
	char	name[10];

	RemoveCheckFromMenu(&DrawingLayerMenu);
	sprintf(name, "layer%-i", LayerStack[0]+1);
	CheckEntry(&DrawingLayerMenu, name);
}

/* ---------------------------------------------------------------------------
 * callback routine, called when current layer is changed
 *
 * this routine is either called from one of the toggle widgets or
 * from the output window by using accelerators
 */
static void CB_SetDrawingLayer(Widget W,
	XtPointer ClientData, XtPointer CallData)
{
	ChangeGroupVisibility((int) ClientData, True, True);
	XtVaSetValues(DrawingLayerMenuButton.W,
		XtNlabel, UNKNOWN(CURRENT->Name),
		XtNbackground, CURRENT->Color,
		NULL);
	RedrawOnEnter = True;
}

/* ---------------------------------------------------------------------------
 * callback routine, called when On/Off flag of layer is changed
 */
static void CB_SetOnOff(Widget W, XtPointer ClientData, XtPointer CallData)
{
	Boolean	state;
	int		layer = (int) ClientData;

		/* get new state of widget */
	XtVaGetValues(W, XtNstate, &state, NULL);

		/* set redraw flag if object exists */
	switch(layer)
	{
		case MAX_LAYER:			/* settings for elements */
			PCB->ElementOn = state;
			RedrawOnEnter = (PCB->Data->ElementN != 0);
			break;

		case MAX_LAYER+1:		/* settings for pins */
			PCB->PinOn = state;
			RedrawOnEnter = (PCB->Data->ElementN != 0);
			break;

		case MAX_LAYER+2:		/* settings for vias */
			PCB->ViaOn = state;
			RedrawOnEnter = (PCB->Data->ViaN != 0);
			break;

		case MAX_LAYER+3:		/* settings for invisible objects */
			PCB->InvisibleObjectsOn = state;
			RedrawOnEnter = (PCB->Data->ElementN != 0);
			break;

		default:				/* layers; current one can't be switched off */
		{
			int		i, group;

				/* check if current one is in that group;
				 * if YES, do nothing because it cannot be changed
				 */
			if ((group = GetGroupOfLayer(layer)) < MAX_LAYER)
				for (i = 0; i < PCB->LayerGroups.Number[group]; i++)
					if (PCB->LayerGroups.Entries[group][i] == LayerStack[0])
					{
							/* reset state to 'On' */
						XtVaSetValues(W, XtNstate, True, NULL);
						return;
					}

				/* switch layer group on/off */
			ChangeGroupVisibility(layer, state, False);
			RedrawOnEnter = True;
			break;
		}
	}
}

/* ---------------------------------------------------------------------------
 * initializes dialog box to get settings like
 *   visible layers
 *   drawing layer
 * returns popup shell widget
 */
void InitControlPanel(Widget Parent, Widget Top, Widget Left)
{
	Widget	masterform,
			visible,
			drawing;

	masterform = XtVaCreateManagedWidget("controlMasterForm", formWidgetClass,
		Parent,
		XtNfromHoriz, Left,
		XtNfromVert, Top,
		LAYOUT_TOP,
		NULL);
	visible = XtVaCreateManagedWidget("onOffSelection", formWidgetClass,
		masterform,
		XtNfromVert, NULL,
		XtNfromHoriz, NULL,
		NULL);
	drawing = XtVaCreateManagedWidget("currentSelection", formWidgetClass,
		masterform,
		XtNfromVert, visible,
		XtNfromHoriz, NULL,
		NULL);

		/* we now add the other widgets to the two forms */
	AddOnOffSelection(visible);
	AddDrawingLayerSelection(drawing);
	Output.Control = masterform;
}

/* ---------------------------------------------------------------------------
 * updates label and sizes in control panel
 */
void UpdateControlPanel(void)
{
	UpdateOnOffSelection();
	UpdateDrawingLayerSelection();
}

/* ---------------------------------------------------------------------------
 * callback routine, called when current mode is changed
 */
static void CB_SetMode(Widget W, XtPointer ClientData, XtPointer CallData)
{
	ModeButtonTypePtr	button;
	
		/* the radio group is determined by the widget of button 0 */
	button = (ModeButtonTypePtr) XawToggleGetCurrent(ModeButtons[0].W);
	SetMode(button ? button->Mode : NO_MODE);
}

/* ---------------------------------------------------------------------------
 * initializes mode buttons
 */
Widget InitModeButtons(Widget Parent, Widget Top, Widget Left)
{
	Screen	*screen = XtScreen(Parent);
	int		i;

	for (i = 0; i < ENTRIES(ModeButtons); i++)
	{
		Pixmap	bitmap;

			/* create background pixmap */
		bitmap = XCreatePixmapFromBitmapData(Dpy, XtWindow(Parent),
			ModeButtons[i].Bitmap, ModeButtons[i].Width, ModeButtons[i].Height,
			BlackPixelOfScreen(screen), WhitePixelOfScreen(screen),
			DefaultDepthOfScreen(screen));

			/* create radio button; use label only if pixmap creation failed
			 * layout has to be set here else X11R4 fails
			 */
		if (bitmap != BadAlloc)
			ModeButtons[i].W = XtVaCreateManagedWidget(ModeButtons[i].Name,
				toggleWidgetClass,
				Parent,
				XtNbitmap, bitmap,
				XtNfromHoriz, Left,
				XtNfromVert, Top,
				LAYOUT_TOP,
				NULL);
		else
			ModeButtons[i].W = XtVaCreateManagedWidget(ModeButtons[i].Name,
				toggleWidgetClass,
				Parent,
				XtNlabel, ModeButtons[i].Label,
				XtNfromHoriz, Left,
				XtNfromVert, Top,
				LAYOUT_TOP,
				NULL);

			/* the first entry identifies the radiogroup,
			 * we have to make sure that no NULL is passed to XtNradioData
			 */
		XtVaSetValues(ModeButtons[i].W,
			XtNradioGroup, ModeButtons[0].W,
			XtNradioData, (XtPointer) &ModeButtons[i],
			NULL);
		XtAddCallback(ModeButtons[i].W, XtNcallback,
			CB_SetMode, (XtPointer) &ModeButtons[i]);

			/* update position for next widget */
		if (i % BUTTONS_PER_ROW == BUTTONS_PER_ROW-1)
		{
			Left = NULL;
			Top = ModeButtons[i].W;
		}
		else
			Left = ModeButtons[i].W;
	}
	return(ModeButtons[i-1].W);
}

/* ---------------------------------------------------------------------------
 * updates the mode selection buttons to reflect the currently set mode
 */
void UpdateModeSelection(void)
{
		/* reset all buttons if no mode is selected;
		 * the first buttons widget identifies the radio group whereas
		 * radioData points to the array member itself
		 */
	if (Settings.Mode == NO_MODE)
		XawToggleUnsetCurrent(ModeButtons[0].W);
	else
	{
		int	i;

		for (i = 0; i < ENTRIES(ModeButtons); i++)
			if (Settings.Mode == ModeButtons[i].Mode)
			{
				XawToggleSetCurrent(ModeButtons[0].W,
					(XtPointer) &ModeButtons[i]);
				break;
			}
	}
}
