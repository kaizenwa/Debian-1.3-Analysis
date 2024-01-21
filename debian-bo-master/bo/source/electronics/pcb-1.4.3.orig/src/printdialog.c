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

static	char	*rcsid = "$Id: printdialog.c,v 143.1 1996/09/16 09:08:53 nau Exp $";

/* print dialog routines
 */

#include <stdlib.h>

#include "global.h"

#include "data.h"
#include "dev_ps.h"
#include "dialog.h"
#include "menu.h"
#include "misc.h"
#include "mymem.h"
#include "print.h"
#include "printdialog.h"
#include "printpanner.h"

#include <X11/Shell.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Toggle.h>

/* ---------------------------------------------------------------------------
 * define resource for X11R4 (names have changed from R4 to R5)
 * and scrollbar sizes
 */
#ifndef	XtNtopOfThumb
#define	XtNtopOfThumb	XtNtop
#endif

#define	MIN_SCALE			0.2
#define	MAX_SCALE			5.0
#define	THUMB_LENGTH		10
#define	SCROLLBAR_LENGTH	150


/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Widget				Popup,				/* the popup shells */
							ScrollbarW,			/* scaling scrollbar */
							ColorW,				/* color button */
							InvertW,			/* invert color button */
							ScaleW,				/* scale label */
							PannerW,			/* panner widget */
							FilenameLabelW,		/* a label */
							FilenameW;			/* input line */
static	float				Scale = 1.0;		/* initial scaling factor */
static	int					ReturnCode;			/* returncode of buttons */
static	Boolean				RotateFlag = True,	/* initial rotation flag */
							OutlineFlag = False,/* don't draw board outline.. */
							AlignmentFlag = False,
							DOSFilenames = False;
static	PrintDeviceTypePtr	DeviceSelection = NULL;	/* selcted device */
static	PopupEntryType		DeviceMenuEntries[ENTRIES(PrintingDevice)+1];
static	PopupMenuType		DeviceMenu =
	{ "device", NULL, DeviceMenuEntries, NULL, NULL, NULL };
static	MenuButtonType		DeviceMenuButton =
	{ "device", "Device", &DeviceMenu, NULL };

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	Widget	CreateDeviceMenu(Widget, Widget, Widget);
static	void	CB_Device(Widget, XtPointer, XtPointer);
static	void	UpdateScale(void);
static	void	UpdateScrollbar(float);
static	void	CB_CancelOrOK(Widget, XtPointer, XtPointer);
static	void	CB_Flags(Widget, XtPointer, XtPointer);
static	void	CB_ScrollProc(Widget, XtPointer, XtPointer);
static	void	CB_JumpProc(Widget, XtPointer, XtPointer);
static	void	CB_DOS(Widget, XtPointer, XtPointer);

/* ---------------------------------------------------------------------------
 * creates 'device' menu
 */
static Widget CreateDeviceMenu(Widget Parent, Widget Top, Widget Left)
{
	int		i;

		/* copy media description to menuentry structure */
	for (i = 0; i < ENTRIES(PrintingDevice); i++)
	{
		DeviceMenuEntries[i].Name = PrintingDevice[i].Info->Name;
		DeviceMenuEntries[i].Label = PrintingDevice[i].Info->Name;
		DeviceMenuEntries[i].Callback = CB_Device;
		DeviceMenuEntries[i].ClientData = (char *) PrintingDevice[i].Info;
		DeviceMenuEntries[i].W = NULL;
	}

		/* thanks for the bug-fix Sander (sander@centerline.com) */
	DeviceMenuEntries[i].Name = NULL;
	if (!DeviceSelection)
		DeviceSelection = PrintingDevice[0].Info;
	DeviceMenuButton.Label = DeviceSelection->Name;
	return(InitMenuButton(Parent, &DeviceMenuButton, Top, Left));
}

/* ---------------------------------------------------------------------------
 * callback for device selection (menu)
 */
static void CB_Device(Widget W, XtPointer ClientData, XtPointer CallData)
{
	DeviceSelection = (PrintDeviceTypePtr) ClientData;
	XtVaSetValues(DeviceMenuButton.W,
		XtNlabel, DeviceSelection->Name,
		NULL);

		/* switch off color button if not supported */
	if (!DeviceSelection->HandlesColor)
		XtVaSetValues(ColorW, XtNstate, False, NULL);
	XtVaSetValues(ColorW, XtNsensitive, DeviceSelection->HandlesColor, NULL);

		/* switch off invert color button if not supported */
	if (!DeviceSelection->HandlesInvertion)
		XtVaSetValues(InvertW, XtNstate, False, NULL);
	XtVaSetValues(InvertW, XtNsensitive,
		DeviceSelection->HandlesInvertion, NULL);

		/* unmap panner and medias selector if not supported */
	if (PannerW)
	{
		if (DeviceSelection->HandlesMedia)
			XMapWindow(Dpy, XtWindow(PannerW));
		else
			XUnmapWindow(Dpy, XtWindow(PannerW));
	}
}

/* ---------------------------------------------------------------------------
 * updates scale label
 */
static void UpdateScale(void)
{
	char	s[10];

	sprintf(s, "%4.2f", Scale);
	XtVaSetValues(ScaleW, XtNlabel, s, NULL);
}

/* ---------------------------------------------------------------------------
 * updates the position of the scrollbar thumb
 * the thumb position is passed (range 0..1)
 */
static void UpdateScrollbar(float NewScale)
{
	float	top;

		/* set new scale, ignore all but first digit behind decimal dot */
	NewScale = MIN(NewScale, MAX_SCALE);
	NewScale = MAX(NewScale, MIN_SCALE);
	Scale = ((int) (10.0*NewScale)) /10.0;
	top = (Scale -MIN_SCALE) / (MAX_SCALE -MIN_SCALE);
	top = MIN(top, (1.0 -((float) THUMB_LENGTH /(float) SCROLLBAR_LENGTH)));
	XawScrollbarSetThumb(ScrollbarW, top, -1.0);
	UpdateScale();
	PrintPannerUpdate(Scale, RotateFlag, OutlineFlag, AlignmentFlag);
}

/* ---------------------------------------------------------------------------
 * callback function for OK and cancel button
 */
static void CB_CancelOrOK(Widget W, XtPointer ClientData, XtPointer CallData)
{
	ReturnCode = (int) ClientData;
}

/* ---------------------------------------------------------------------------
 * callback function for the toggle buttons
 * just updates the slider
 */
static void CB_Flags(Widget W, XtPointer ClientData, XtPointer CallData)
{
	Boolean	*ptr = (Boolean *) ClientData;

	XtVaGetValues(W, XtNstate, ptr, NULL);
	PrintPannerUpdate(Scale, RotateFlag, OutlineFlag, AlignmentFlag);
}

/* ---------------------------------------------------------------------------
 * callback function for scrolling
 * see Athena Widget manual for details
 */
static void CB_ScrollProc(Widget W, XtPointer ClientData, XtPointer CallData)
{
	float	top;
	int		delta = (int) CallData;

		/* get thumb postion */
	XtVaGetValues(W, XtNtopOfThumb, &top, NULL);
	top += ((float) delta/ (float) SCROLLBAR_LENGTH);
	top = MIN(1.0, MAX(top, 0.0));
	UpdateScrollbar((float) (MAX_SCALE -MIN_SCALE) *top +(float) MIN_SCALE);
}

/* ---------------------------------------------------------------------------
 * callback function for scrolling
 * see Athena Widget manual for details
 */
static void CB_JumpProc(Widget W, XtPointer ClientData, XtPointer CallData)
{
	float	top = *(float *) CallData;

	UpdateScrollbar((float) (MAX_SCALE -MIN_SCALE) *top +(float) MIN_SCALE);
}

/* ---------------------------------------------------------------------------
 * callback function for the selection of DOS filenames
 */
static void CB_DOS(Widget W, XtPointer ClientData, XtPointer CallData)
{
	XtVaGetValues(W, XtNstate, &DOSFilenames, NULL);
	if (DOSFilenames)
	{
		XUnmapWindow(Dpy, XtWindow(FilenameLabelW));
		XUnmapWindow(Dpy, XtWindow(FilenameW));
	}
	else
	{
		XMapWindow(Dpy, XtWindow(FilenameLabelW));
		XMapWindow(Dpy, XtWindow(FilenameW));
	}
	XtVaSetValues(FilenameW, XtNsensitive, !DOSFilenames, NULL);
}

/* ---------------------------------------------------------------------------
 * print command dialog
 * prints the package layer only if flag is set
 *
 * offset[xy] are relativ to the center of the media and output area
 */
void PrintDialog(void)
{
			Widget				masterform,
								label,
								device,
								rotate,
								mirror,
								outline,
								alignment,
								drillhelper,
								DOSnames;
			MediaTypePtr		media;
			Dimension			width,
								height;
			Position			offsetx,
								offsety;
	static	Boolean				mirrorflag = False,
								invertflag = False,
								colorflag = False,
								drillhelperflag = False;
	static	DialogButtonType	buttons[] = {
		{ "defaultButton", "   OK   ", CB_CancelOrOK,
			(XtPointer) OK_BUTTON, NULL },
		{ "cancelButton", "No/Cancel", CB_CancelOrOK,
			(XtPointer) CANCEL_BUTTON, NULL }};

		/* initialize command if called the first time */
	if (!PCB->PrintFilename)
		PCB->PrintFilename = EvaluateFilename(Settings.PrintFile,
			NULL, PCB->Filename ? PCB->Filename : "noname", NULL);

		/* clear offset --> recenter output */
	offsetx = offsety = 0;

		/* create a popup shell */
	Popup = XtVaCreatePopupShell("popup", transientShellWidgetClass,
		Output.Toplevel,
		XtNallowShellResize, True,
		XtNmappedWhenManaged, False,
		XtNinput, True,
		NULL);

		/* the form that holds everything */
	masterform = XtVaCreateManagedWidget("printMasterForm", formWidgetClass,
		Popup,
		XtNresizable, True,
		NULL);

		/* now all buttons, labels ... */
	label = XtVaCreateManagedWidget("comment", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNlabel, "print settings:",
		NULL);
	label = XtVaCreateManagedWidget("comment", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNlabel, "select device driver:",
		NULL);
	device = CreateDeviceMenu(masterform, label, NULL);
	label = XtVaCreateManagedWidget("comment", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, device,
		XtNlabel, "select rotation, mirroring, colored output...:",
		NULL);
	rotate = XtVaCreateManagedWidget("rotate", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNstate, RotateFlag,
		NULL);
	mirror = XtVaCreateManagedWidget("mirror", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNfromHoriz, rotate,
		XtNstate, mirrorflag,
		NULL);
	ColorW = XtVaCreateManagedWidget("color", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNfromHoriz, mirror,
		XtNstate, colorflag,
		XtNsensitive, DeviceSelection->HandlesColor,
		NULL);
	InvertW = XtVaCreateManagedWidget("invert", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNfromHoriz, ColorW,
		XtNstate, invertflag,
		XtNsensitive, DeviceSelection->HandlesInvertion,
		NULL);
	label = XtVaCreateManagedWidget("comment", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, ColorW,
		XtNlabel, "select outline and alignment targets:",
		NULL);
	outline = XtVaCreateManagedWidget("outline", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNstate, OutlineFlag,
		NULL);
	alignment = XtVaCreateManagedWidget("alignment", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNfromHoriz, outline,
		XtNstate, AlignmentFlag,
		NULL);
	drillhelper = XtVaCreateManagedWidget("drillhelper", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNfromHoriz, alignment,
		XtNstate, drillhelperflag,
		NULL);
	label = XtVaCreateManagedWidget("comment", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, outline,
		XtNlabel, "select output scale here:",
		NULL);
	ScrollbarW = XtVaCreateManagedWidget("scrollbar", scrollbarWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNlength, SCROLLBAR_LENGTH,
		XtNminimumThumb, THUMB_LENGTH,
		XtNorientation, XtorientHorizontal,
		NULL);
	ScaleW = XtVaCreateManagedWidget("scale", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNfromHoriz, ScrollbarW,
		NULL);
	PannerW = PrintPannerCreate(masterform, ScrollbarW, NULL);
	label = XtVaCreateManagedWidget("comment", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, PannerW ? PannerW : ScrollbarW,
		XtNlabel, "select for DOS compatible filenames:",
		NULL);
	DOSnames = XtVaCreateManagedWidget("dosNames", toggleWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, label,
		XtNstate, DOSFilenames,
		XtNlabel, "8.3 names",
		NULL);
	FilenameLabelW = XtVaCreateManagedWidget("comment", labelWidgetClass,
		masterform,
		LAYOUT_TOP,
		XtNfromVert, DOSnames,
		XtNlabel, "enter filename:",
		NULL);
	FilenameW = XtVaCreateManagedWidget("input", asciiTextWidgetClass,
		masterform,
		XtNresizable, True,
		XtNresize, XawtextResizeWidth,
		XtNstring, PCB->PrintFilename,
		XtNwrap, XawtextWrapNever,
		XtNeditType, XawtextEdit,
		XtNfromVert, FilenameLabelW,
		LAYOUT_TOP,
		NULL);

		/* now we add the buttons, the first one is always default */
	AddButtons(masterform, FilenameW, buttons, ENTRIES(buttons));

		/* add callback functions, update panner and thumb position */
	XtAddCallback(rotate, XtNcallback, CB_Flags,(XtPointer) &RotateFlag);
	XtAddCallback(outline, XtNcallback, CB_Flags,(XtPointer) &OutlineFlag);
	XtAddCallback(alignment, XtNcallback, CB_Flags,(XtPointer) &AlignmentFlag);
	XtAddCallback(DOSnames, XtNcallback, CB_DOS, NULL);
	XtAddCallback(ScrollbarW, XtNscrollProc, CB_ScrollProc, NULL);
	XtAddCallback(ScrollbarW, XtNjumpProc, CB_JumpProc, NULL);
	XtAddCallback(ScrollbarW, XtNscrollProc, CB_ScrollProc, NULL);

		/* override default translations and
		 * install accelerators for buttons
		 */
	XtOverrideTranslations(FilenameW,
		XtParseTranslationTable(InputTranslations));
	XtInstallAccelerators(FilenameW, buttons[0].W);
	XtInstallAccelerators(FilenameW, buttons[1].W);

		/* set focus to input line */
	XtSetKeyboardFocus(masterform, FilenameW);

		/* the panner widget has to be realized before the offset can be set;
		 * update sliders and buttons too
		 */
	XtRealizeWidget(Popup);
	CB_DOS(DOSnames, NULL, NULL);
	PrintPannerSetSliderPosition(offsetx, offsety);
	UpdateScale();
	PrintPannerUpdate(Scale, RotateFlag, OutlineFlag, AlignmentFlag);
	UpdateScrollbar(Scale);

		/* now display dialog window and handle input */
	StartDialog(Popup);
	DialogEventLoop(&ReturnCode);

		/* get button settings, offsets and media description */
	XtVaGetValues(rotate, XtNstate, &RotateFlag, NULL);
	XtVaGetValues(mirror, XtNstate, &mirrorflag, NULL);
	XtVaGetValues(drillhelper, XtNstate, &drillhelperflag, NULL);
	XtVaGetValues(ColorW, XtNstate, &colorflag, NULL);
	XtVaGetValues(InvertW, XtNstate, &invertflag, NULL);

		/* get media selection and transform center-based offset
		 * to upper-left corner
		 */
	if (DeviceSelection->HandlesMedia)
	{
		PrintPannerGetSliderPosition(&offsetx, &offsety);
		PrintPannerGetSize(&width, &height);
		media = PrintPannerGetMedia();
		offsetx += ((media->Width -width) /2);
		offsety += ((media->Height -height) /2);
	}
	else
	{
			/* use some defaults if the device doesn't handle media */
		media = NULL;
		offsetx = offsety = 0;
	}

		/* release old command and allocate new string */
	SaveFree(PCB->PrintFilename);
	XtVaGetValues(FilenameW, XtNstring, &PCB->PrintFilename, NULL);
	PCB->PrintFilename = StripWhiteSpaceAndDup(PCB->PrintFilename);

		/* now everything is saved; destroy the dialog */
	EndDialog(Popup);

		/* call the printing routine if everything is OK;
		 * convert offsets to distance from upper-left corner
		 */
	if (ReturnCode == OK_BUTTON && PCB->PrintFilename)
		Print(PCB->PrintFilename, Scale,
			mirrorflag, RotateFlag, colorflag, invertflag,
			OutlineFlag, AlignmentFlag, drillhelperflag, DOSFilenames,
			DeviceSelection, media,
			offsetx, offsety);
}
