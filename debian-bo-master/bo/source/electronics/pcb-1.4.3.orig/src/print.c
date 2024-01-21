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

static	char	*rcsid = "$Id: print.c,v 143.1 1996/09/16 09:08:52 nau Exp $";

/* printing routines
 */
#include <math.h>
#include <time.h>

#include "global.h"

#include "data.h"
#include "dev_ps.h"
#include "file.h"
#include "error.h"
#include "misc.h"
#include "print.h"

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	PrintDeviceTypePtr	Device;
static	PrintInitType		DeviceFlags;
static	Boolean				GlobalOutlineFlag,	/* copy of local ident. */
							GlobalAlignmentFlag,
							GlobalDrillHelperFlag,
							GlobalColorFlag,
							GlobalDOSFlag,
							ReplaceOK;			/* ok two overriode all */
												/* print output files */
static	char				*GlobalCommand;

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	SetPrintColor(Pixel);
static	FILE	*OpenPrintFile(char *);
static	int		SetupPrintFile(char *, char *);
static	int		ClosePrintFile(void);
static	int		PrintLayergroups(void);
static	int		PrintSilkscreen(void);
static	int		PrintDrill(void);
static	int		PrintGroundplane(void);
static	int		PrintMask(void);

/* ----------------------------------------------------------------------
 * queries color from X11 database and calls device  command
 * black is default on errors
 */
static void SetPrintColor(Pixel X11Color)
{
	XColor	rgb;
	int		result;

		/* do nothing if no colors are requested */
	if (!GlobalColorFlag)
		return;

		/* query best matching color from X server */
	rgb.pixel = X11Color;
	result = XQueryColor(Dpy,
		DefaultColormapOfScreen(XtScreen(Output.Toplevel)),
		&rgb);

		/* create a PS command to set this color */
	if (result == BadValue || result == BadColor)
		rgb.red = rgb.green = rgb.blue = 0;
	Device->SetColor(rgb);
}

/* ---------------------------------------------------------------------------
 * opens a file for printing
 */
static FILE *OpenPrintFile(char *FileExtention)
{
	char	*filename,
			*completeFilename;
	size_t	length;
	FILE	*fp;

		/* evaluate add extention and suffix to filename */
	if ((filename = ExpandFilename(NULL, GlobalCommand)) == NULL)
		filename = GlobalCommand;
	length = strlen(EMPTY(GlobalCommand)) +1 +
		strlen(FileExtention) +1 +strlen(Device->Suffix) +1;
	completeFilename = MyCalloc(length, sizeof(char), "OpenPrintFile()");
	sprintf(completeFilename, "%s%s%s.%s",
		GlobalDOSFlag ? "" : EMPTY(GlobalCommand),
		GlobalDOSFlag ? "" : "_",
		FileExtention, Device->Suffix);

		/* try to open all the file; if the value of
		 * 'ReplaceOK' is set to 'True' no more confirmation is
		 * requested for the sequence
		 */
	fp = CheckAndOpenFile(completeFilename, !ReplaceOK, True, &ReplaceOK);
	if (fp == NULL)
	{
		OpenErrorMessage(completeFilename);
		SaveFree(completeFilename);
		return(NULL);
	}
	SaveFree(completeFilename);
	return(fp);
}

/* ---------------------------------------------------------------------------
 * setup new print file
 */
static int SetupPrintFile(char *FileExtention, char *Description)
{
	char		*message;

	if ((DeviceFlags.FP = OpenPrintFile(FileExtention)) == NULL)
		return(1);
	if ((message = Device->Init(&DeviceFlags, Description)) != NULL)
	{
		Message(message);
		fclose(DeviceFlags.FP);
		return(1);
	}

		/* draw outline and alignment targets */
	if (GlobalOutlineFlag)
		Device->Outline(0, 0, PCB->MaxWidth, PCB->MaxHeight);
	if (GlobalAlignmentFlag)
		Device->Alignment(DeviceFlags.BoundingBox.X1,
			DeviceFlags.BoundingBox.Y1,
			DeviceFlags.BoundingBox.X2,
			DeviceFlags.BoundingBox.Y2);
	return(0);
}

/* ---------------------------------------------------------------------------
 * closes the printfile and calls the exit routine of the driver
 */
static int ClosePrintFile(void)
{
	Device->Exit();
	return(fclose(DeviceFlags.FP));
}

/* ---------------------------------------------------------------------------
 * prints all layergroups
 * returns != zero on error
 */
static int PrintLayergroups(void)
{
	char		extention[12],
				description[18];
	Cardinal	group,
				entry;
	
	for (group = 0; group < MAX_LAYER+2; group ++)
		if (PCB->LayerGroups.Number[group])
		{
			Cardinal	i;

				/* setup extention and open new file */
			sprintf(extention, "%s%i", GlobalDOSFlag ? "" : "group", group+1);
			sprintf(description, "layergroup #%i", group+1);
			if (SetupPrintFile(extention, description))
				return(1);

				/* print all layers in stack-order;
				 * loop oveal all stackposition anc check each layer
				 * for beeing a member of the group
				 */
			for (i = 0; i < MAX_LAYER; i++)
				for (entry = 0; entry < PCB->LayerGroups.Number[group]; entry++)
				{
					LayerTypePtr	layer;
					Cardinal		number;

					if ((number = PCB->LayerGroups.Entries[group][entry]) != i)
						continue;

						/* OK, found one; print it and break the look */
					layer = &PCB->Data->Layer[number];
					SetPrintColor(layer->Color);
					Device->Layer(layer, group);
					break;
				}

				/* check the component/solder layers for being members
				 * of this group. Print if TRUE
				 */
			for (entry = 0; entry < PCB->LayerGroups.Number[group]; entry++)
			{
				Cardinal	number;

				number = PCB->LayerGroups.Entries[group][entry] -MAX_LAYER;
				switch(number)
				{
					case COMPONENT_LAYER:
					case SOLDER_LAYER:
						SetPrintColor(PCB->PinColor);
						ALLPAD_LOOP(PCB->Data,
							if ((TEST_FLAG(ONSOLDERFLAG, pad) != 0) ==
								(number == SOLDER_LAYER))
								Device->Pad(pad);
						);
						break;

					default:
						break;
				}
			}

				/* now at last all the pins/vias */
			SetPrintColor(PCB->PinColor);
			ALLPIN_LOOP(PCB->Data, Device->PinOrVia(pin););
			SetPrintColor(PCB->ViaColor);
			VIA_LOOP(PCB->Data, Device->PinOrVia(via););

				/* print drill-helper if requested */
			if (GlobalDrillHelperFlag)
			{
				SetPrintColor(PCB->PinColor);
				ALLPIN_LOOP(PCB->Data, Device->DrillHelper(pin););
				SetPrintColor(PCB->ViaColor);
				VIA_LOOP(PCB->Data, Device->DrillHelper(via););
			}

				/* close the device */
			ClosePrintFile();
		}
	return(0);
}

/* ---------------------------------------------------------------------------
 * prints solder and component side
 * - first component followed by solder side
 * - first all element packages, followed by pads and pins
 * returns != zero on error
 */
static int PrintSilkscreen(void)
{
	static	char	*extention[2] = { "componentsilk", "soldersilk" },
					*DOSextention[2] = { "cslk", "sslk" },
					*description[2] = {
						"silkscreen component side",
						"silkscreen solder side" };
			int		i;

		/* loop over both sides, start with component */
	for (i = 0; i < 2; i++)
	{
			/* start with the component side */
		if (SetupPrintFile(GlobalDOSFlag ? DOSextention[i] : extention[i],
			description[i]))
			return(1);

		SetPrintColor(PCB->ElementColor);
		ELEMENT_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, element) == 0) == (i == 0))
				Device->ElementPackage(element);
		);

		SetPrintColor(PCB->PinColor);
		ALLPAD_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, pad) == 0) == (i == 0))
				Device->Pad(pad);
		);
		ALLPIN_LOOP(PCB->Data, Device->PinOrVia(pin); );
		ClosePrintFile();
	}
	return(0);
}

/* ---------------------------------------------------------------------------
 * creates a drill-information file if the device is able to
 * returns != zero on error
 */
static int PrintDrill(void)
{
		/* pass drilling information */
	if (Device->HandlesDrill)
	{
		if (SetupPrintFile(GlobalDOSFlag ? "dril" : "drill",
			"drill information"))
			return(1);

		SetPrintColor(PCB->PinColor);
		ALLPIN_LOOP(PCB->Data, Device->Drill(pin););
		VIA_LOOP(PCB->Data, Device->Drill(via););

			/* close the file */
		ClosePrintFile();
	}
	return(0);
}

/* ---------------------------------------------------------------------------
 * creates a groundplane file if the device is able to
 * returns != zero on error
 */
static int PrintGroundplane(void)
{
	BoxTypePtr	box;

		/* check capability */
	if (Device->HandlesGroundplane)
	{
		if (SetupPrintFile(GlobalDOSFlag ? "gpl" : "groundplane","groundplane"))
			return(1);

			/* uses the size of the other layers */
		box = GetDataBoundingBox(PCB->Data);
		Device->FilledRectangle(box->X1, box->Y1, box->X2, box->Y2);
		SetPrintColor(PCB->PinColor);

			/* first clear the area then draw all pins/vias */
		ALLPIN_LOOP(PCB->Data, Device->ClearPinOrViaOnGroundplane(pin););
		VIA_LOOP(PCB->Data, Device->ClearPinOrViaOnGroundplane(via););
		ALLPIN_LOOP(PCB->Data, Device->PinOrVia(pin););
		VIA_LOOP(PCB->Data, Device->PinOrVia(via););

			/* close the file */
		ClosePrintFile();
	}
	return(0);
}

/* ---------------------------------------------------------------------------
 * prints solder and component side mask
 * returns != zero on error
 */
static int PrintMask(void)
{
	static	char	*extention[2] = { "componentmask", "soldermask" },
					*DOSextention[2] = { "cmsk", "smsk" },
					*description[2] = {
						"solder mask component side",
						"solder mask solder side" };
			int		i;
			BoxTypePtr	box;

		/* check capability */
	if (Device->HandlesMask)
	{
		box = GetDataBoundingBox(PCB->Data);

			/* loop over both sides, start with component */
		for (i = 0; i < 2; i++)
		{
				/* start with the component side */
			if (SetupPrintFile(GlobalDOSFlag ? DOSextention[i] : extention[i],
				description[i]))
				return(1);

			Device->FilledRectangle(box->X1, box->Y1, box->X2, box->Y2);
			SetPrintColor(PCB->PinColor);
			ALLPAD_LOOP(PCB->Data,
				if ((TEST_FLAG(ONSOLDERFLAG, pad) == 0) == (i == 0))
					Device->PadMask(pad);
			);
			ALLPIN_LOOP(PCB->Data, Device->PinOrViaMask(pin););
			VIA_LOOP(PCB->Data, Device->PinOrViaMask(via););

			ClosePrintFile();
		}
	}
	return(0);
}

/* ----------------------------------------------------------------------
 * generates printout
 * mirroring, rotating, scaling and the request for outlines and/or
 * alignment targets were already determined by a dialog
 *
 * output is written either to several files
 * whitespaces have already been removed from 'Command'
 *
 * 'MirrorFlag' has to be negated by the device dependend code if
 * (0,0) is in the lower/left corner for the output device. Some
 * adjustments to the offsets will also be necessary in this case
 *
 * the offsets have to be adjusted if either 'OutlineFlag' or
 * 'AlignmentFlag' is set
 *
 * all offsets are in mil
 */
int Print(char *Command, float Scale,
	Boolean MirrorFlag, Boolean RotateFlag,
	Boolean ColorFlag, Boolean InvertFlag,
	Boolean OutlineFlag, Boolean AlignmentFlag,
	Boolean DrillHelperFlag, Boolean DOSFlag,
	PrintDeviceTypePtr PrintDevice, MediaTypePtr Media,
	Position OffsetX, Position OffsetY)
{
		/* it's not OK to override all files -> user interaction
		 * is required if a file exists
		 */
	ReplaceOK = False;

		/* save pointer... in global identifier */
	Device = PrintDevice;
	GlobalColorFlag = ColorFlag;
	GlobalAlignmentFlag = AlignmentFlag;
	GlobalOutlineFlag = OutlineFlag;
	GlobalDrillHelperFlag = DrillHelperFlag;
	GlobalDOSFlag = DOSFlag;
	GlobalCommand = Command;

		/* set the info struct for the device driver */
	DeviceFlags.SelectedMedia = Media;
	DeviceFlags.MirrorFlag = MirrorFlag;
	DeviceFlags.RotateFlag = RotateFlag;
	DeviceFlags.InvertFlag = InvertFlag;
	DeviceFlags.OffsetX = OffsetX;
	DeviceFlags.OffsetY = OffsetY;
	DeviceFlags.Scale = Scale;

		/* set bounding box coordinates;
		 * the layout has already been checked to be non-empty
		 * selecting outlines or alignment targets causes adjustments
		 */
	if (OutlineFlag)
	{
		DeviceFlags.BoundingBox.X1 = 0;
		DeviceFlags.BoundingBox.Y1 = 0;
		DeviceFlags.BoundingBox.X2 = PCB->MaxWidth;
		DeviceFlags.BoundingBox.Y2 = PCB->MaxHeight;
	}
	else
		DeviceFlags.BoundingBox = *GetDataBoundingBox(PCB->Data);

	if (AlignmentFlag)
	{
		DeviceFlags.BoundingBox.X1 -= Settings.AlignmentDistance;
		DeviceFlags.BoundingBox.Y1 -= Settings.AlignmentDistance;
		DeviceFlags.BoundingBox.X2 += Settings.AlignmentDistance;
		DeviceFlags.BoundingBox.Y2 += Settings.AlignmentDistance;
	}

		/* OK, call all necessary subroutines */
	if (PrintLayergroups() ||
		PrintSilkscreen() ||
		PrintDrill() ||
		PrintGroundplane() ||
		PrintMask())
		return(1);

	return(0);
}
