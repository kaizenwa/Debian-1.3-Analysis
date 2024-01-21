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

static	char	*rcsid = "$Id: main.c,v 143.1 1996/09/16 09:08:44 nau Exp $";

/* main program, initializes some stuff and handles user input
 */

#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <memory.h>

#include "global.h"

#include "action.h"
#include "buffer.h"
#include "create.h"
#include "control.h"
#include "crosshair.h"
#include "data.h"
#include "draw.h"
#include "error.h"
#include "file.h"
#include "library.h"
#include "menu.h"
#include "misc.h"
#include "mymem.h"
#include "remove.h"
#include "set.h"

#include <X11/cursorfont.h> 
#include <X11/Shell.h> 
#include <X11/Xatom.h> 
#include <X11/Xaw/Command.h> 
#include <X11/Xaw/Form.h> 
#include <X11/Xaw/Label.h> 
#include <X11/Xaw/Scrollbar.h> 
#include <X11/Xaw/Simple.h> 
#include <X11/Xaw/Viewport.h> 

/* ---------------------------------------------------------------------------
 * icon data as created by 'bitmap'
 */
#include "icon.data"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	GetSizeOfDrawingArea(void);
static	void	ViewportEvent(Widget, XtPointer, XEvent *, Boolean *);
static	void	OutputEvent(Widget, XtPointer, XEvent *, Boolean *);
static	void	CB_Backup(XtPointer, XtIntervalId *);
static	void	InitShell(int *, char**);
static	void	InitCursorPosition(Widget, Widget, Widget);
static	void	InitStatusLine(Widget, Widget, Widget);
static	void	InitViewport(Widget, Widget, Widget);
static	void	InitAccelerators(void);
static	void	InitGC(void);
static	void	InitIcon(void);
static	void	InitHandler(void);
static	void	InitWidgets(void);
static	void	InitDeviceDriver(void);

/* ---------------------------------------------------------------------------
 * default translations
 */
static	String			DefaultTranslations = "";

/* ---------------------------------------------------------------------------
 * fallback resources
 */
static	String			Fallback[] = {
	"*font:           -*-courier-bold-r-*-*-14-*-*-*-*-*-*-*",
	"*library.geometry:  750x350",
	"*log.geometry:      300x100",
	"*maxPCBWidth:       20000",
	"*maxPCBHeight:      20000",
	"*outputMasterForm.viewport.width:  900",
	"*outputMasterForm.viewport.height: 600",
	"*pinoutFont0:    -*-courier-bold-r-*-*-24-*-*-*-*-*-*-*",
	"*pinoutFont1:    -*-courier-bold-r-*-*-12-*-*-*-*-*-*-*",
	"*pinoutFont2:    -*-courier-bold-r-*-*-8-*-*-*-*-*-*-*",
	"*pinoutFont3:    -*-courier-bold-r-*-*-4-*-*-*-*-*-*-*",
	"*pinoutFont4:    -*-courier-bold-r-*-*-2-*-*-*-*-*-*-*",
	"*pinoutMasterForm.viewport.width:  200",
	"*pinoutMasterForm.viewport.height: 150",
	"*zoom:                             2",
	NULL };

/* ---------------------------------------------------------------------------
 * resources to query
 * the number of layers has to be adapted here and in 'global.h'
 */
static	XtResource		ToplevelResources[] = {
	{ "absoluteGrid", "AbsoluteGrid", XtRBoolean, sizeof(Boolean),
	  XtOffsetOf(SettingType, AbsoluteGrid), XtRString, "True" },
	{ "alignmentDistance", "AlignmentDistance", XtRDimension, sizeof(Dimension),
	  XtOffsetOf(SettingType, AlignmentDistance), XtRString, "200" },
	{ "allDirectionLines", "AllDirectionLines", XtRBoolean, sizeof(Boolean),
	  XtOffsetOf(SettingType, AllDirectionLines), XtRString, "True" },
	{ "backupInterval", "BackupInterval", XtRInt, sizeof(long),
	  XtOffsetOf(SettingType, BackupInterval), XtRString, "300" },
	{ "charactersPerLine", "CharactersPerLine", XtRInt, sizeof(int),
	  XtOffsetOf(SettingType, CharPerLine), XtRString, "80" },
	{ "connectedColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, ConnectedColor), XtRString, XtDefaultForeground },
	{ "crosshairColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, CrosshairColor), XtRString, XtDefaultForeground },
	{ "elementColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, ElementColor), XtRString, XtDefaultForeground },
	{ "elementSelectedColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, ElementSelectedColor),
	  XtRString, XtDefaultForeground },
	{ "elementCommand", "ElementCommand", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, ElementCommand), XtRString, 
	  "M4PATH=\"%p\";export M4PATH;echo 'include(%f)' | "GNUM4 },
	{ "elementPath", "ElementPath", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, ElementPath), XtRString, PCBLIBDIR },
	{ "fileCommand", "FileCommand", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, FileCommand), XtRString, "cat %f" },
	{ "filePath", "FilePath", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, FilePath), XtRString, "." },
	{ "fontCommand", "FontCommand", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, FontCommand), XtRString, "cat %f" },
	{ "fontFile", "FontFile", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, FontFile), XtRString, FONTFILENAME },
	{ "fontPath", "FontPath", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, FontPath), XtRString, PCBLIBDIR },
	{ "grid", "Grid", XtRInt, sizeof(int),
	  XtOffsetOf(SettingType, Grid), XtRString, "100" },
	{ "gridColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, GridColor), XtRString, XtDefaultForeground },
	{ "groundplaneColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, GroundplaneColor),
	  XtRString, XtDefaultForeground },
	{ "invisibleObjectsColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, InvisibleObjectsColor),
	  XtRString, XtDefaultForeground },

	{ "layerColor1", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[0]), XtRString, XtDefaultForeground },
	{ "layerColor2", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[1]), XtRString, XtDefaultForeground },
	{ "layerColor3", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[2]), XtRString, XtDefaultForeground },
	{ "layerColor4", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[3]), XtRString, XtDefaultForeground },
	{ "layerColor5", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[4]), XtRString, XtDefaultForeground },
	{ "layerColor6", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[5]), XtRString, XtDefaultForeground },
	{ "layerColor7", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[6]), XtRString, XtDefaultForeground },
	{ "layerColor8", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerColor[7]), XtRString, XtDefaultForeground },

	{ "layerName1", "LayerName1", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[0]), XtRString, NULL },
	{ "layerName2", "LayerName2", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[1]), XtRString, NULL },
	{ "layerName3", "LayerName3", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[2]), XtRString, NULL },
	{ "layerName4", "LayerName4", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[3]), XtRString, NULL },
	{ "layerName5", "LayerName5", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[4]), XtRString, NULL },
	{ "layerName6", "LayerName6", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[5]), XtRString, NULL },
	{ "layerName7", "LayerName7", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[6]), XtRString, NULL },
	{ "layerName8", "LayerName8", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, DefaultLayerName[7]), XtRString, NULL },

	{ "layerSelectedColor1", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[0]),
	  XtRString, XtDefaultForeground },
	{ "layerSelectedColor2", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[1]),
	  XtRString, XtDefaultForeground },
	{ "layerSelectedColor3", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[2]),
	  XtRString, XtDefaultForeground },
	{ "layerSelectedColor4", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[3]),
	  XtRString, XtDefaultForeground },
	{ "layerSelectedColor5", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[4]),
	  XtRString, XtDefaultForeground },
	{ "layerSelectedColor6", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[5]),
	  XtRString, XtDefaultForeground },
	{ "layerSelectedColor7", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[6]),
	  XtRString, XtDefaultForeground },
	{ "layerSelectedColor8", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, LayerSelectedColor[7]),
	  XtRString, XtDefaultForeground },

	{ "layerGroups", "LayerGroups", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, Groups), XtRString, "1:2:3:4:5:6:7:8" },
	{ "libraryCommand", "LibraryCommand", XtRString, sizeof(String),
		XtOffsetOf(SettingType, LibraryCommand), XtRString, "cat %f" },
	{ "libraryContentsCommand", "LibraryContentsCommand", XtRString,
		sizeof(String), XtOffsetOf(SettingType, LibraryContentsCommand),
		XtRString, "cat %f" },
	{ "libraryFilename", "LibraryFilename", XtRString, sizeof(String),
		XtOffsetOf(SettingType, LibraryFilename), XtRString, LIBRARYFILENAME },
	{ "libraryPath", "LibraryPath", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, LibraryPath), XtRString, PCBLIBDIR },
	{ "lineThickness", XtCThickness, XtRDimension, sizeof(Dimension),
	  XtOffsetOf(SettingType, LineThickness), XtRString, "10" },
	{ "media", "Media", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, Media), XtRString, DEFAULT_MEDIASIZE },
	{ "offLimitColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, OffLimitColor), XtRString, XtDefaultBackground },
	{ "pinColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, PinColor), XtRString, XtDefaultForeground },
	{ "pinSelectedColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, PinSelectedColor),
	  XtRString, XtDefaultForeground },

	{ "pinoutFont0", "Font", XtRFontStruct, sizeof(XFontStruct *),
	  XtOffsetOf(SettingType, PinoutFont[0]), XtRFontStruct, XtDefaultFont },
	{ "pinoutFont1", "Font", XtRFontStruct, sizeof(XFontStruct *),
	  XtOffsetOf(SettingType, PinoutFont[1]), XtRFontStruct, XtDefaultFont },
	{ "pinoutFont2", "Font", XtRFontStruct, sizeof(XFontStruct *),
	  XtOffsetOf(SettingType, PinoutFont[2]), XtRFontStruct, XtDefaultFont },
	{ "pinoutFont3", "Font", XtRFontStruct, sizeof(XFontStruct *),
	  XtOffsetOf(SettingType, PinoutFont[3]), XtRFontStruct, XtDefaultFont },
	{ "pinoutFont4", "Font", XtRFontStruct, sizeof(XFontStruct *),
	  XtOffsetOf(SettingType, PinoutFont[4]), XtRFontStruct, XtDefaultFont },

	{ "pinoutNameLength", "PinoutNameLength", XtRInt, sizeof(int),
	  XtOffsetOf(SettingType, PinoutNameLength), XtRString, "8" },
	{ "pinoutOffsetX", "PinoutOffsetX", XtRPosition, sizeof(Position),
	  XtOffsetOf(SettingType, PinoutOffsetX), XtRString, "100" },
	{ "pinoutOffsetY", "PinoutOffsetY", XtRPosition, sizeof(Position),
	  XtOffsetOf(SettingType, PinoutOffsetY), XtRString, "100" },
	{ "pinoutTextOffsetX", "PinoutTextOffsetX", XtRPosition, sizeof(Position),
	  XtOffsetOf(SettingType, PinoutTextOffsetX), XtRString, "0" },
	{ "pinoutTextOffsetY", "PinoutTextOffsetY", XtRPosition, sizeof(Position),
	  XtOffsetOf(SettingType, PinoutTextOffsetY), XtRString, "0" },
	{ "pinoutZoom", "PinoutZoom", XtRInt, sizeof(int),
	  XtOffsetOf(SettingType, PinoutZoom), XtRString, "2" },
	{ "printFile", "PrintFile", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, PrintFile), XtRString, "%f.output" },
	{ "raiseLogWindow", "RaiseLogWindow", XtRBoolean, sizeof(Boolean),
	  XtOffsetOf(SettingType, RaiseLogWindow), XtRString, "True" },
	{ "resetAfterElement", "ResetAfterElement", XtRBoolean, sizeof(Boolean),
	  XtOffsetOf(SettingType, ResetAfterElement), XtRString, "False" },
	{ "ringBellWhenFinished", "RingBellWhenFinished",XtRBoolean,sizeof(Boolean),
	  XtOffsetOf(SettingType, RingBellWhenFinished), XtRString, "True" },
	{ "saveCommand", "SaveCommand", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, SaveCommand), XtRString, "cat - > %f" },
	{ "saveInTMP", "SaveInTMP", XtRBoolean, sizeof(Boolean),
	  XtOffsetOf(SettingType, SaveInTMP), XtRString, "True" },
	{ "saveLastCommand", "SaveLastCommand", XtRBoolean, sizeof(Boolean),
	  XtOffsetOf(SettingType, SaveLastCommand), XtRString, "False" },
	{ "size", "Size", XtRString, sizeof(String),
	  XtOffsetOf(SettingType, Size), XtRString, DEFAULT_SIZE },
	{ "textScale", "TextScale", XtRDimension, sizeof(Dimension),
	  XtOffsetOf(SettingType, TextScale), XtRString, "100" },
	{ "useLogWindow", "UseLogWindow", XtRBoolean, sizeof(Boolean),
	  XtOffsetOf(SettingType, UseLogWindow), XtRString, "True" },
	{ "viaColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, ViaColor), XtRString, XtDefaultForeground },
	{ "viaSelectedColor", XtCColor, XtRPixel, sizeof(Pixel),
	  XtOffsetOf(SettingType, ViaSelectedColor),
	  XtRString, XtDefaultForeground },

		/* a default value of '0' will cause InitShell() to
		 * calculate the value from the 'viaThickness' resource
		 */
	{ "viaDrillingHole", "DrillingHole", XtRDimension, sizeof(Dimension),
	  XtOffsetOf(SettingType, ViaDrillingHole), XtRString, "20" },
	{ "viaThickness", XtCThickness, XtRDimension, sizeof(Dimension),
	  XtOffsetOf(SettingType, ViaThickness), XtRString, "40" },
	{ "volume", "Volume", XtRInt, sizeof(int),
	  XtOffsetOf(SettingType, Volume), XtRString, "100" },
	{ "zoom", "Zoom", XtRInt, sizeof(int),
	  XtOffsetOf(SettingType, Zoom), XtRString, "3" }};

/* ---------------------------------------------------------------------------
 * additional command line arguments
 */
static	XrmOptionDescRec	CommandLineOptions[] = {
	{ "-alldirections","allDirectionLines", XrmoptionNoArg, (caddr_t) "True" },
	{ "+alldirections","allDirectionLines", XrmoptionNoArg, (caddr_t) "False" },
	{ "-backup", "backupInterval", XrmoptionSepArg, (caddr_t) NULL },
	{ "-c", "charactersPerLine", XrmoptionSepArg, (caddr_t) NULL },
	{ "-fontfile", "fontFile", XrmoptionSepArg, (caddr_t) NULL },
	{ "-lelement", "elementCommand", XrmoptionSepArg, (caddr_t) NULL },
	{ "-lfile", "fileCommand", XrmoptionSepArg, (caddr_t) NULL },
	{ "-lfont", "fontCommand", XrmoptionSepArg, (caddr_t) NULL },
	{ "-lg", "layerGroups", XrmoptionSepArg, (caddr_t) NULL },
	{ "-libname", "libraryFilename", XrmoptionSepArg, (caddr_t) NULL },
	{ "-libpath", "libraryPath", XrmoptionSepArg, (caddr_t) NULL },
	{ "-llib", "libraryCommand", XrmoptionSepArg, (caddr_t) NULL },
	{ "-llibcont", "libraryContentsCommand", XrmoptionSepArg, (caddr_t) NULL },
	{ "-loggeometry", "log.geometry", XrmoptionSepArg, (caddr_t) NULL },
	{ "-pnl", "pinoutNameLength", XrmoptionSepArg, (caddr_t) NULL },
	{ "-pz", "pinoutZoom", XrmoptionSepArg, (caddr_t) NULL },
	{ "-reset", "resetAfterElement", XrmoptionNoArg, (caddr_t) "True" },
	{ "+reset", "resetAfterElement", XrmoptionNoArg, (caddr_t) "False" },
	{ "-ring", "ringBellWhenFinished", XrmoptionNoArg, (caddr_t) "True" },
	{ "+ring", "ringBellWhenFinished", XrmoptionNoArg, (caddr_t) "False" },
	{ "-s", "saveLastCommand", XrmoptionNoArg, (caddr_t) "True" },
	{ "+s", "saveLastCommand", XrmoptionNoArg, (caddr_t) "False" },
	{ "-save", "saveInTMP", XrmoptionNoArg, (caddr_t) "True" },
	{ "+save", "saveInTMP", XrmoptionNoArg, (caddr_t) "False" },
	{ "-size", "size", XrmoptionSepArg, (caddr_t) NULL },
	{ "-sfile", "saveCommand", XrmoptionSepArg, (caddr_t) NULL },
	{ "-v", "volume", XrmoptionSepArg, (caddr_t) NULL }};

/* ---------------------------------------------------------------------------
 * actions
 */
static	XtActionsRec	Actions[] = {
	{ "MovePointer", ActionMovePointer },
	{ "SetValue", ActionSetValue },
	{ "FinishInputDialog", ActionFinishInputDialog },
	{ "Quit", ActionQuit },
	{ "Connection", ActionConnection },
	{ "Command", ActionCommand },
	{ "Display", ActionDisplay },
	{ "Mode", ActionMode },
	{ "RemoveSelected", ActionRemoveSelected },
	{ "ChangeSize", ActionChangeSize },
	{ "Change2ndSize", ActionChange2ndSize },
	{ "ChangeName", ActionChangeName },
	{ "ChangeSquare", ActionChangeSquare },
	{ "Select", ActionSelect },
	{ "Unselect", ActionUnselect },
	{ "Save", ActionSave },
	{ "Load", ActionLoad },
	{ "Print", ActionPrint },
	{ "New", ActionNew },
	{ "SwapSides", ActionSwapSides },
	{ "Bell", ActionBell },
	{ "PasteBuffer", ActionPasteBuffer },
	{ "Undo", ActionUndo },
	{ "Polygon", ActionPolygon },
	{ "EditLayerGroups", ActionEditLayerGroups },
	{ "Groundplane", ActionGroundplane },
	{ "MoveToCurrentLayer", ActionMoveToCurrentLayer },
	{ "SwitchDrawingLayer", ActionSwitchDrawingLayer }};

/* ----------------------------------------------------------------------
 * sets the size of the drawing area
 * is called by MapNotify and ConfigureNotify events
 *
 * if the function is called during dialog handling (indicated by a valid
 * pixmap) a resize event has occured. This is the only possibility
 * because dialogs are modal to the application. In this case the pixmap
 * is released if the new size is larger than the old one.
 */
static void GetSizeOfDrawingArea(void)
{
	Dimension		width, height,
					scroll_width, scroll_height,
					border_left, border_bottom;
	Position		x, y,
					maxw = TO_SCREEN(PCB->MaxWidth),
					maxh = TO_SCREEN(PCB->MaxHeight);

	XtVaGetValues(Output.Viewport,
		XtNwidth, &width,
		XtNheight, &height,
		NULL);

		/* adjust the size of the statusline */
	XtVaSetValues(Output.StatusLine, XtNwidth, width, NULL);

		/* reduce the viewportsize by the dimensions of the scrollbars */
	XtVaGetValues(Output.ScrollbarBottom,
		XtNheight, &scroll_height,
		XtNborderWidth, &border_bottom,
		NULL);
	XtVaGetValues(Output.ScrollbarLeft,
		XtNwidth, &scroll_width,
		XtNborderWidth, &border_left,
		NULL);

		/* calculate new sizes */
	width -= (scroll_width +border_left);
	height -= (scroll_height +border_bottom);

		/* get origin (!!! offset is <= 0 !!!) */
	XtVaGetValues(Output.Output, XtNx, &x, XtNy, &y, NULL);
	x = -x;
	y = -y;

		/* release pixmap if necessary (size or offset changed) */
	if (width > Output.Width || height > Output.Height ||
		x != Output.OffsetX || y != Output.OffsetY)
		ReleaseSaveUnderPixmap();

		/* limit the viewport size to the drawing area to prevent scrollbars
		 * from growing if the window is resized
		 */
	Output.OffsetX = x;
	Output.OffsetY = y;
	Output.Width = MIN(width, maxw);
	Output.Height = MIN(height, maxh);
	if (width > maxw)
	{
		Output.Width = maxw;
		XtVaSetValues(Output.Output, XtNwidth, Output.Width, NULL);
	}
	else
		Output.Width = width;
	if (height > maxh)
	{
		Output.Height = maxh;
		XtVaSetValues(Output.Output, XtNheight, Output.Height, NULL);
	}
	else
		Output.Height = height;
}

/* ---------------------------------------------------------------------- 
 * handles all events from viewport window
 */
static void ViewportEvent(Widget W,
	XtPointer ClientData, XEvent *Event, Boolean *Flag)
{
	switch(Event->type)
	{
			/* set flag when viewport is completely visible; needed
			 * to make sure that 'SaveOutputWindow()' works correctly.
			 * The viewport event has to be taken because the outputwindow
			 * is always obscured by the viewports clipping window
			 */
		case VisibilityNotify:
			Output.VisibilityOK = 
				(((XVisibilityEvent *) Event)->state == VisibilityUnobscured);
			break;

		case UnmapNotify:
			Output.VisibilityOK = False;
			break;

		case ConfigureNotify:		/* get new size of drawing area */
			GetSizeOfDrawingArea();
			break;
	}
} 

/* ---------------------------------------------------------------------------
 * handles callbacks from timer,
 * used to backup data
 */
static void CB_Backup(XtPointer ClientData, XtIntervalId *ID)
{
	Backup();

		/* restart timer */
	XtAppAddTimeOut(Context, 1000*Settings.BackupInterval, CB_Backup, NULL);
}

/* ---------------------------------------------------------------------- 
 * handles all events from output window
 * the handling of MapNotify and ConfigureNotify could be done much
 * easier with X11R5 and later but...
 *
 * I also had to set a flag to indicate a valid 'enter window' because
 * modal windows may generate 'LeaveNotify' events without 'EnterNotify'.
 * This behavior disturbs the crosshair-visibility management
 */
static void OutputEvent(Widget W,
	XtPointer ClientData, XEvent *Event, Boolean *Flag)
{
			XExposeEvent	*event = (XExposeEvent *) Event;
	static	Boolean			has_entered = False;

	switch(Event->type)
	{
		case Expose:				/* expose event means redraw */
			if (VALID_PIXMAP(PCB->SaveUnder))
			{
					/* make sure that cursor is in the same state
					 * as when a dialog poped up. The state is determined
					 * by the Enter- and LeaveNotify events
					 */
				HideCrosshair(False);
				if (!XCopyArea(Dpy, PCB->SaveUnder, Output.OutputWindow,
					Output.fgGC,
					event->x -Output.OffsetX, event->y -Output.OffsetY,
					event->width, event->height,
					event->x, event->y))
				{
					ClearAndRedrawOutput();
				}
				RestoreCrosshair(False);
			}
			else
				ClearAndRedrawOutput();
			break;

		case EnterNotify:			/* enter output area */
			RestoreCrosshair(False);
			has_entered = True;
			if (RedrawOnEnter)		/* control panel was used -> redraw */
			{
				ReleaseSaveUnderPixmap();
				ClearAndRedrawOutput();
			}
			RedrawOnEnter = False;
			break;

		case LeaveNotify:			/* leave output area */
				/* see also the description at start of this function */
			if (has_entered)
				HideCrosshair(False);
			has_entered = False;	
			break;
			
		case MapNotify:				/* get initial size of drawing area */
		case ConfigureNotify:		/* get position of origin */
			GetSizeOfDrawingArea();
			break;

		case MotionNotify:
			EventMoveCrosshair((XMotionEvent *) Event);
			break;
	}
} 

/* ---------------------------------------------------------------------------
 * init toplevel shell and get application resources
 */
static void InitShell(int *Argc, char **Argv)
{
	int				i, x, y;
	unsigned int	width, height;

		/* init application toplevel window, get resources */
	Output.Toplevel = XtAppInitialize(&Context, "Pcb",
		CommandLineOptions, XtNumber(CommandLineOptions),
		Argc, Argv, Fallback, NULL, 0);
	XtVaSetValues(Output.Toplevel,
		XtNmappedWhenManaged, False,
		NULL);
	Dpy = XtDisplay(Output.Toplevel);

		/* clear structure and get resources */
	memset(&Settings, 0, sizeof(SettingType));
	XtGetApplicationResources(Output.Toplevel, &Settings,
		ToplevelResources, XtNumber(ToplevelResources), NULL, 0);
	if (Settings.LineThickness > MAX_LINESIZE ||
		Settings.LineThickness < MIN_LINESIZE)
		Settings.LineThickness = 10;
	if (Settings.ViaThickness > MAX_PINORVIASIZE ||
		Settings.ViaThickness < MIN_PINORVIASIZE)
		Settings.ViaThickness = 40;
	if (Settings.ViaDrillingHole <= 0)
		Settings.ViaDrillingHole = DEFAULT_DRILLINGHOLE *Settings.ViaThickness /100;

		/* parse geometry string, ignore offsets */
	i = XParseGeometry(Settings.Size, &x, &y, &width, &height);
	if (!(i & WidthValue) || !(i & HeightValue))
	{
			/* failed; use default setting */
		XParseGeometry(DEFAULT_SIZE, &x, &y, &width, &height);
	}
	Settings.MaxWidth = MIN(MAX_COORD, MAX((Dimension) width, MIN_SIZE));
	Settings.MaxHeight = MIN(MAX_COORD, MAX((Dimension) height, MIN_SIZE));
	Settings.Volume = MIN(100, MAX(-100, Settings.Volume));
	ParseGroupString(Settings.Groups, &Settings.LayerGroups);
}

/* ---------------------------------------------------------------------------
 * initializes cursor position
 */
static void InitCursorPosition(Widget Parent, Widget Top, Widget Left)
{
	Output.CursorPosition = XtVaCreateManagedWidget("cursorPosition",
		labelWidgetClass,
		Parent,
		XtNlabel, "",
		XtNfromVert, Top,
		XtNfromHoriz, Left,
		LAYOUT_TOP,
		NULL);
}

/* ---------------------------------------------------------------------------
 * initializes statusline
 */
static void InitStatusLine(Widget Parent, Widget Top, Widget Left)
{
	Output.StatusLine = XtVaCreateManagedWidget("statusLine", labelWidgetClass,
		Parent,
		XtNresizable, True,
		XtNresize, True,
		XtNfromHoriz, Left,
		XtNfromVert, Top,
		LAYOUT_BOTTOM_RIGHT,
		NULL);
}

/* ---------------------------------------------------------------------------
 * initialize output and viewport widget
 */
static void InitViewport(Widget Parent, Widget Top, Widget Left)
{
		/* viewport widget to to handle scrolling */
	Output.Viewport = XtVaCreateManagedWidget("viewport", viewportWidgetClass,
		Parent,
		XtNfromHoriz, Left,
		XtNfromVert, Top,
		LAYOUT_NORMAL,
		XtNallowHoriz, True,
		XtNallowVert, True,
		XtNuseBottom, True,
		XtNforceBars, True,
		NULL);

		/* create simple widget to which I draw to (handled by viewport) */
	Output.Output = XtVaCreateManagedWidget("output", simpleWidgetClass,
		Output.Viewport,
		XtNresizable, True,
		XtNheight, TO_SCREEN(Settings.MaxHeight),
		XtNwidth, TO_SCREEN(Settings.MaxWidth),
		NULL);

		/* add event handlers */
	XtAddEventHandler(Output.Output,
		ExposureMask | LeaveWindowMask | EnterWindowMask |
		StructureNotifyMask | PointerMotionMask,
		False, (XtEventHandler) OutputEvent, NULL);
	XtAddEventHandler(Output.Viewport,
		StructureNotifyMask | VisibilityChangeMask,
		False, (XtEventHandler) ViewportEvent, NULL);
}

/* ---------------------------------------------------------------------------
 * installs accelerators for scrollbars
 */
static void InitAccelerators(void)
{
		/* install accelerators for scrollbars */
	Output.ScrollbarBottom = XtNameToWidget(Output.Viewport, "horizontal");
	Output.ScrollbarLeft = XtNameToWidget(Output.Viewport, "vertical");
	if (Output.ScrollbarBottom != NULL)
		XtInstallAccelerators(Output.Output, Output.ScrollbarBottom);
	if (Output.ScrollbarLeft != NULL)
		XtInstallAccelerators(Output.Output, Output.ScrollbarLeft);
}

/* ----------------------------------------------------------------------
 * initializes icon pixmap
 */
static void InitIcon(void)
{
	Screen	*screen;

		/* initialize icon pixmap */
	screen = XtScreen(Output.Toplevel);
	XtVaSetValues(Output.Toplevel,
		XtNiconPixmap, XCreatePixmapFromBitmapData(Dpy,
			XtWindow(Output.Toplevel), icon_bits, icon_width, icon_height,
			BlackPixelOfScreen(screen), WhitePixelOfScreen(screen),
			DefaultDepthOfScreen(screen)),
		NULL);
}

/* ----------------------------------------------------------------------
 * initializes GC 
 * create a public GC for drawing with background color, font
 * and fill rules for polygons
 * the foreground color is set by some routines in draw.c
 */
static void InitGC(void)
{
	XtVaGetValues(Output.Output, XtNbackground, &Settings.bgColor, NULL);
	Output.fgGC = XCreateGC(Dpy, Output.OutputWindow, 0, NULL);
	Output.bgGC = XCreateGC(Dpy, Output.OutputWindow, 0, NULL);
	Output.GridGC = XCreateGC(Dpy, Output.OutputWindow, 0, NULL);
	if (!VALID_GC((int) Output.fgGC) ||
		!VALID_GC((int) Output.bgGC) ||
		!VALID_GC((int) Output.GridGC))
		MyFatal("can't create default GC\n");

	XSetForeground(Dpy, Output.bgGC, Settings.bgColor);
	XSetFillRule(Dpy, Output.fgGC, WindingRule);
	XSetState(Dpy, Output.GridGC, Settings.GridColor, Settings.bgColor,
		GXinvert, AllPlanes);

		/* dont create graphic expose events from XCopyArea() */
	XSetGraphicsExposures(Dpy, Output.fgGC, False);
}

/* ----------------------------------------------------------------------
 * initialize signal and error handlers
 */
static void InitHandler(void)
{
		/* install a new error handler and catch terminating signals */
	XtAppSetErrorHandler(Context, X11ErrorHandler);

/*
	signal(SIGHUP, CatchSignal);
	signal(SIGQUIT, CatchSignal);
	signal(SIGABRT, CatchSignal);
	signal(SIGSEGV, CatchSignal);
	signal(SIGTERM, CatchSignal);
	signal(SIGINT, CatchSignal);
*/

		/* calling external program by popen() may cause a PIPE signal,
		 * so we ignore it
		 */
	signal(SIGPIPE, SIG_IGN);
}

/* ---------------------------------------------------------------------- 
 * initialize widgets
 */
static void InitWidgets(void)
{
		/* this form widget is a container for most of the other widgets */
	Output.MasterForm = XtVaCreateManagedWidget("masterForm", formWidgetClass,
		Output.Toplevel,
		XtNresizable, True,
		NULL);

		/* init all other widgets as childs of the masterform */
	InitMenu(Output.MasterForm, NULL, NULL);
	InitControlPanel(Output.MasterForm, Output.Menu, NULL);
	InitCursorPosition(Output.MasterForm, NULL, Output.Menu);
	InitViewport(Output.MasterForm, Output.Menu, Output.Control);
	InitStatusLine(Output.MasterForm, Output.Viewport, Output.Control);

		/* set actions and install translations;
		 * the menu must be initialized before the actions are added
		 * for XawPositionSimpleMenu() to be available
		 */
	XtAppAddActions(Context, Actions, XtNumber(Actions));
	XtAugmentTranslations(Output.Toplevel,
		XtParseTranslationTable(DefaultTranslations));

		/* realize the tree, get the IDs of the output window
		 * and initialize some identifiers in draw.c
		 *
		 * destroying and recreating the statusline ensures that it is
		 * resizable even if a geometry is passed to the application
		 */
	XtRealizeWidget(Output.Toplevel);
	XtDestroyWidget(Output.StatusLine);
	InitStatusLine(Output.MasterForm, Output.Viewport, Output.Control);
	Output.OutputWindow = XtWindow(Output.Output);	

	InitAccelerators();
	InitGC();
	InitIcon();
	InitCrosshair();
	InitHandler();
	InitBuffers();
	SetOutputXCursor(XC_crosshair);

		/* set window manager property to get 'delete window' messages */
	WMDeleteWindowAtom = XInternAtom(Dpy, "WM_DELETE_WINDOW", False);
	XSetWMProtocols(Dpy, XtWindow(Output.Toplevel), &WMDeleteWindowAtom, 1);
}

/* ---------------------------------------------------------------------------
 * init device drivers for printing
 */
static void InitDeviceDriver(void)
{
	int	i;

	for (i = 0; i < ENTRIES(PrintingDevice); i++)
		PrintingDevice[i].Info = PrintingDevice[i].Query();
}

/* ---------------------------------------------------------------------- 
 * main program
 */
int main(int argc, char *argv[]) 
{
		/* init application:
		 * - make program name available for error handlers
		 * - evaluate special options
		 * - initialize toplevel shell and resources
		 * - create an empty PCB with default symbols
		 * - initialize all other widgets
		 * - update screen and get size of drawing area
		 * - evaluate command-line arguments
		 * - register 'call on exit()' function
		 */
	if ((Progname = strrchr(*argv, '/')) == NULL)
		Progname = *argv;
	else
		Progname++;

		/* take care of special options */
	if (argc == 2)
	{
		if (!strcmp("-help", argv[1]))
			Usage();
		if (!strcmp("-copyright", argv[1]))
			Copyright();
		if (!strcmp("-version", argv[1]))
		{
			puts(RELEASE);
			exit(0);
		}
	}

	InitShell(&argc, argv);
	InitDeviceDriver();
	PCB = CreateNewPCB(True);
	ResetStackAndVisibility();
	CreateDefaultFont();
	InitWidgets();

		/* update zoom and grid... */
	UpdateSettingsOnScreen();
	GetSizeOfDrawingArea();
	switch(--argc)
	{
		case 0:		/* only program name */
			break;

		case 1:		/* load an initial layout;
					 * abort if filename looks like an option
					 */
			++argv;
			if (**argv == '-')
				Usage();

				/* keep filename even if initial load command failed;
				 * file might not exist
				 */
			if (LoadPCB(*argv))
				PCB->Filename = MyStrdup(*argv, "main()");
			break;

		default:		/* error */
			Usage();
			break;
	}

		/* Register a function to be called when the program terminates.
		 * This makes sure that data is saved even if LEX/YACC routines
		 * abort the program.
		 * If the OS doesn't have at least one of them,
		 * the critical sections will be handled by parse_l.l
		 */
#ifdef HAS_ATEXIT
	atexit(EmergencySave);
#else
#ifdef HAS_ON_EXIT
	on_exit(GlueEmergencySave, NULL);
#endif
#endif

		/* popup main window and initialize error log window
		 * the mode buttons require a 'drawable' so the widgets have to be
		 * realized before calling the init routine.
		 * If the window isn't mapped the statusline won't be resizable
		 * (dont know why)
		 */
	InitModeButtons(Output.MasterForm, Output.Control, NULL);
	XMapWindow(Dpy, XtWindow(Output.Toplevel));
	InitErrorLog();

		/* read the library file and display it if it's not empty */
	if (!ReadLibraryContents() && Library.MenuN)
		InitLibraryWindow(Output.Toplevel);

		/* start backup timer */
	XtAppAddTimeOut(Context, 1000*Settings.BackupInterval, CB_Backup, NULL);

	XtAppMainLoop(Context);
	return(0);
} 
