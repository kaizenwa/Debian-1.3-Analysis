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

static	char	*rcsid = "$Id: misc.c,v 143.1 1996/09/16 09:08:46 nau Exp $";

/* misc functions used by several modules
 */

#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <math.h>
#include <unistd.h>

#include "global.h"

#include "create.h"
#include "crosshair.h"
#include "data.h"
#include "dialog.h"
#include "draw.h"
#include "file.h"
#include "error.h"
#include "mymem.h"
#include "misc.h"
#include "move.h"
#include "remove.h"
#include "rotate.h"
#include "search.h"

#include <X11/cursorfont.h>
#include <X11/Shell.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Simple.h>

/* ---------------------------------------------------------------------------
 * prints copyright information
 */
void Copyright(void)
{
	printf("\n"
		"                COPYRIGHT for %s version %s\n\n"
		"    PCB, interactive printed circuit board design\n"
		"    Copyright (C) 1994,1995,1996 Thomas Nau\n\n"
		"    This program is free software; you can redistribute it and/or modify\n"
		"    it under the terms of the GNU General Public License as published by\n"
		"    the Free Software Foundation; either version 2 of the License, or\n"
		"    (at your option) any later version.\n\n"
		"    This program is distributed in the hope that it will be useful,\n"
		"    but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
		"    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
		"    GNU General Public License for more details.\n\n"
		"    You should have received a copy of the GNU General Public License\n"
		"    along with this program; if not, write to the Free Software\n"
		"    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n\n",
		Progname, RELEASE);
	exit(0);
}

/* ---------------------------------------------------------------------------
 * prints usage message
 */
void Usage(void)
{
	fprintf(stderr, "\nUSAGE: %s [standard X options] [standard options] [layout]\n"
		"or   : %s [standard X options] <exactly one special option>\n\n"
		"standard options are:\n"
		"  -alldirections:           enable 'all-direction' lines\n"
		"  +alldirections:           force 45 degree lines\n"
		"  -backup <seconds>:        time between two backups\n"
		"  -c <number>:              characters per output-line\n"
		"  -fontfile <file>:         read default font from this file\n"
		"  -lelement <command>:      command to copy element files to stdout,\n"
		"                            %%f is set to the filename\n"
		"                            %%p is set to the seachpath\n"
		"  -lfile <command>:         command to copy layout files to stdout,\n"
		"                            %%f is set to the filename\n"
		"                            %%p is set to the seachpath\n"
		"  -lfont <command>:         command to copy font files to stdout,\n"
		"                            %%f is set to the filename\n"
		"                            %%p is set to the seachpath\n"
		"  -lg <layergroups>:        set layergroups of new layouts to this\n"
		"  -libname <file>:          the name of the library\n"
		"  -libpath <path>:          the library search-path\n"
		"  -llib <command>:          command to copy elements from library to stdout,\n"
		"                            %%a is set to 'template value package'\n"
		"                            %%f is set to the filename\n"
		"                            %%p is set to the searchpath\n"
		"  -llibcont <command>:      command to list library contents,\n"
		"                            %%f is set to the filename\n"
		"                            %%p is set to the searchpath\n"
		"  -loggeometry <geometry>:  set the geometry of the logging window\n"
		"  -pnl <value>:             maximum display length of pin names\n"
		"  -pz <value>:              zoom factor for pinout windows\n"
		"  -reset:                   reset connections after each element\n"
		"  +reset:                   negation of '-reset'\n"
		"  -ring:                    ring bell when connection lookup is done\n"
		"  +ring:                    negation of '-r'\n"
		"  -s:                       save the last command entered by user\n"
		"  +s:                       negation of '-s'\n"
		"  -save:                    always save data before it is lost\n"
		"  +save:                    override data if required by command\n"
		"  -sfile <command>:         command to copy stdin to layout file,\n"
		"                            %%f is set to the filename\n"
		"  -size <width>x<height>    size of a layout\n"
		"  -v <value>:               sets the volume of the X speaker\n"
		"special options are:\n"
		"  -copyright:               prints copyright information\n"
		"  -help:                    prints this message\n"
		"  -version:                 prints the current version number\n",
		Progname, Progname);
	exit(1);
}

/* ---------------------------------------------------------------------------
 * sets the bounding box of a polygons
 */
void SetPolygonBoundingBox(PolygonTypePtr Polygon)
{
	Position	minx, miny,
				maxx, maxy;

	minx = miny = MAX_COORD;
	maxx = maxy = 0;
	POLYGONPOINT_LOOP(Polygon,
		minx = MIN(minx, point->X);
		miny = MIN(miny, point->Y);
		maxx = MAX(maxx, point->X);
		maxy = MAX(maxy, point->Y);
	);
	Polygon->BoundingBox.X1 = minx;
	Polygon->BoundingBox.Y1 = miny;
	Polygon->BoundingBox.X2 = maxx;
	Polygon->BoundingBox.Y2 = maxy;
}

/* ---------------------------------------------------------------------------
 * sets the bounding box of an elements
 */
void SetElementBoundingBox(ElementTypePtr Element)
{
	Position	minx, miny,
				maxx, maxy;
	float		fminx, fminy,
				fmaxx, fmaxy;
	int			angle1,
				angle2,
				angle;

		/* first update the text objects */
	ELEMENTTEXT_LOOP(Element, SetTextBoundingBox(&PCB->Font, text));

		/* do not include the elementnames bounding box which
		 * is handles seperatly
		 */
	minx = miny = MAX_COORD;
	maxx = maxy = 0;
	ELEMENTLINE_LOOP(Element,
		minx = MIN(minx, line->Point1.X);
		miny = MIN(miny, line->Point1.Y);
		minx = MIN(minx, line->Point2.X);
		miny = MIN(miny, line->Point2.Y);
		maxx = MAX(maxx, line->Point1.X);
		maxy = MAX(maxy, line->Point1.Y);
		maxx = MAX(maxx, line->Point2.X);
		maxy = MAX(maxy, line->Point2.Y);
	);
	PIN_LOOP(Element,
		minx = MIN(minx, pin->X);
		miny = MIN(miny, pin->Y);
		maxx = MAX(maxx, pin->X);
		maxy = MAX(maxy, pin->Y);
	);
	PAD_LOOP(Element,
		minx = MIN(minx, pad->Point1.X);
		miny = MIN(miny, pad->Point1.Y);
		minx = MIN(minx, pad->Point2.X);
		miny = MIN(miny, pad->Point2.Y);
		maxx = MAX(maxx, pad->Point1.X);
		maxy = MAX(maxy, pad->Point1.Y);
		maxx = MAX(maxx, pad->Point2.X);
		maxy = MAX(maxy, pad->Point2.Y);
	);
	ARC_LOOP(Element,
			/* arc->StartAngle is in [0,360], arc->Delta in [0,360] */
		angle1 = arc->StartAngle;
		angle2 = arc->StartAngle +arc->Delta;

			/* initialize limits */
		fminx = MIN(cos(M180*(float) angle1),cos(M180*(float) angle2));
		fmaxx = MAX(cos(M180*(float) angle1),cos(M180*(float) angle2));
		fminy = MIN(sin(M180*(float) angle1),sin(M180*(float) angle2));
		fmaxy = MAX(sin(M180*(float) angle1),sin(M180*(float) angle2));

			/* loop and check all angles n*180
			 * with angle1 <= a <= angle2
			 */
		for (angle = (angle1 /180 +1)*180; angle < angle2; angle += 180)
		{
			fminx = MIN(cos(M180 *(float) angle), fminx);
			fmaxx = MAX(cos(M180 *(float) angle), fmaxx);
		}

			/* loop and check all angles n*180+90
			 * with angle1 <= a <= angle2
			 */
		for (angle = ((angle1+90)/180)*180+90; angle < angle2; angle += 180)
		{
			fminy = MIN(sin(M180 *(float) angle), fminy);
			fmaxy = MAX(sin(M180 *(float) angle), fmaxy);
		}
		minx = MIN(minx, (int) (fminx *arc->Width) +arc->X);
		miny = MIN(miny, (int) (fminy *arc->Height) +arc->Y);
		maxx = MAX(maxx, (int) (fmaxx *arc->Width) +arc->X);
		maxy = MAX(maxy, (int) (fmaxy *arc->Height) +arc->Y);
	);

	Element->BoundingBox.X1 = minx;
	Element->BoundingBox.Y1 = miny;
	Element->BoundingBox.X2 = maxx;
	Element->BoundingBox.Y2 = maxy;
}

/* ---------------------------------------------------------------------------
 * creates the bounding box of a text object
 */
void SetTextBoundingBox(FontTypePtr FontPtr, TextTypePtr Text)
{
	SymbolTypePtr	symbol = FontPtr->Symbol;
	unsigned char	*s = (unsigned char *) Text->TextString;
	Position		width = 0,
					height = 0;

		/* calculate size of the bounding box */
	for (; s && *s; s++)
		if (*s <= MAX_FONTPOSITION && symbol[*s].Valid)
		{
			width  += symbol[*s].Width +symbol[*s].Delta;
			height = MAX(height, (Position) symbol[*s].Height);
		}
		else
		{
			width += ((FontPtr->DefaultSymbol.X2 -FontPtr->DefaultSymbol.X1) *6/5);
			height = (FontPtr->DefaultSymbol.Y2 -FontPtr->DefaultSymbol.Y1);
		}

		/* scale values and rotate them */
	width = width *Text->Scale /100;
	height = height *Text->Scale /100;

		/* set upper-left and lower-right corner;
		 * swap coordinates if necessary (origin is already in 'swapped')
		 * and rotate box
		 */
	Text->BoundingBox.X1 = Text->X;
	Text->BoundingBox.Y1 = Text->Y;
	if (TEST_FLAG(ONSOLDERFLAG, Text))
	{
		Text->BoundingBox.X2 = Text->BoundingBox.X1 +SWAP_SIGN_X(width);
		Text->BoundingBox.Y2 = Text->BoundingBox.Y1 +SWAP_SIGN_Y(height);
		RotateBoxLowLevel(&Text->BoundingBox,
			Text->X, Text->Y, (4-Text->Direction) & 0x03);
	}
	else
	{
		Text->BoundingBox.X2 = Text->BoundingBox.X1 +width;
		Text->BoundingBox.Y2 = Text->BoundingBox.Y1 +height;
		RotateBoxLowLevel(&Text->BoundingBox,
			Text->X, Text->Y, Text->Direction);
	}
}

/* ---------------------------------------------------------------------------
 * returns True if data area is empty
 */
Boolean IsDataEmpty(DataTypePtr Data)
{
	Boolean		hasNoObjects;
	Cardinal	i;

	hasNoObjects = (Data->ViaN == 0);
	hasNoObjects &= (Data->ElementN == 0);
	for (i = 0; i < MAX_LAYER; i++)
		hasNoObjects = hasNoObjects &&
			Data->Layer[i].LineN == 0 && 
			Data->Layer[i].TextN == 0 &&
			Data->Layer[i].PolygonN == 0;
	return(hasNoObjects);
}

/* ---------------------------------------------------------------------------
 * gets minimum and maximum coordinates
 * returns NULL if layout is empty
 */
BoxTypePtr GetDataBoundingBox(DataTypePtr Data)
{
	static	BoxType	box;

		/* preset identifiers with highest and lowest possible values */
	box.X1 = box.Y1 = MAX_COORD;
	box.X2 = box.Y2 = 0;

		/* now scan for the lowest/highest X and Y coodinate */
	VIA_LOOP(Data,
		box.X1 = MIN(box.X1, via->X -via->Thickness/2);
		box.Y1 = MIN(box.Y1, via->Y -via->Thickness/2);
		box.X2 = MAX(box.X2, via->X +via->Thickness/2);
		box.Y2 = MAX(box.Y2, via->Y +via->Thickness/2);
	);
	ELEMENT_LOOP(Data,
		box.X1 = MIN(box.X1, element->BoundingBox.X1);
		box.Y1 = MIN(box.Y1, element->BoundingBox.Y1);
		box.X2 = MAX(box.X2, element->BoundingBox.X2);
		box.Y2 = MAX(box.Y2, element->BoundingBox.Y2);
		ELEMENTTEXT_LOOP(element,
			box.X1 = MIN(box.X1, text->BoundingBox.X1);
			box.Y1 = MIN(box.Y1, text->BoundingBox.Y1);
			box.X2 = MAX(box.X2, text->BoundingBox.X2);
			box.Y2 = MAX(box.Y2, text->BoundingBox.Y2);
		);
	);
	ALLLINE_LOOP(Data,
		box.X1 = MIN(box.X1, line->Point1.X);
		box.Y1 = MIN(box.Y1, line->Point1.Y);
		box.X1 = MIN(box.X1, line->Point2.X);
		box.Y1 = MIN(box.Y1, line->Point2.Y);
		box.X2 = MAX(box.X2, line->Point1.X);
		box.Y2 = MAX(box.Y2, line->Point1.Y);
		box.X2 = MAX(box.X2, line->Point2.X);
		box.Y2 = MAX(box.Y2, line->Point2.Y);
	);
	ALLTEXT_LOOP(Data,
		box.X1 = MIN(box.X1, text->BoundingBox.X1);
		box.Y1 = MIN(box.Y1, text->BoundingBox.Y1);
		box.X2 = MAX(box.X2, text->BoundingBox.X2);
		box.Y2 = MAX(box.Y2, text->BoundingBox.Y2);
	);
	ALLPOLYGON_LOOP(Data,
		box.X1 = MIN(box.X1, polygon->BoundingBox.X1);
		box.Y1 = MIN(box.Y1, polygon->BoundingBox.Y1);
		box.X2 = MAX(box.X2, polygon->BoundingBox.X2);
		box.Y2 = MAX(box.Y2, polygon->BoundingBox.Y2);
	);
	return(IsDataEmpty(Data) ? NULL : &box);
}

/* ---------------------------------------------------------------------------
 * centers the displayed PCB around the specified point
 * (X,Y) in screen coordinates
 */
void CenterDisplay(Position X, Position Y)
{
	Position	old_x, old_y,			/* output window in viewport */
				new_x, new_y;
	float		top;					/* top of thumb */

		/* save old values (offset of upper/left corner) */
	XtVaGetValues(Output.Output, XtNx, &old_x, XtNy, &old_y, NULL);

		/* move origin half a screen width/height to get display centered */
	X -= (Output.Width /2);
	top = (float) X /(float) TO_SCREEN(PCB->MaxWidth);
	top = MIN(MAX(top, 0.0), 1.0);
	XtCallCallbacks(Output.ScrollbarBottom, XtNjumpProc, (XtPointer) &top);

	Y -= (Output.Height /2);
	top = (float) Y /(float) TO_SCREEN(PCB->MaxHeight);
	top = MIN(MAX(top, 0.0), 1.0);
	XtCallCallbacks(Output.ScrollbarLeft, XtNjumpProc, (XtPointer) &top);

		/* now get the new values (reread cause of rounding) */
	XtVaGetValues(Output.Output, XtNx, &new_x, XtNy, &new_y, NULL);

		/* the scrollbar widget triggers an expose event if (x,y)
		 * has changed. I have to do it if it hasn't changed
		 */
	if (new_x == old_x && new_y == old_y)
		ClearAndRedrawOutput();
}

/* ---------------------------------------------------------------------------
 * transforms symbol coordinates so that the left edge of each symbol
 * is at the zero position. The y coordinates are moved so that min(y) = 0
 * 
 */
void SetFontInfo(FontTypePtr Ptr)
{
	Cardinal		i, j;
	SymbolTypePtr	symbol;
	LineTypePtr		line;
	Position		totalminy = MAX_COORD;

		/* calculate cell with and height (is at least DEFAULT_CELLSIZE)
		 * maximum cell width and height
		 * minimum x and y position of all lines
		 */
	Ptr->MaxWidth = DEFAULT_CELLSIZE;
	Ptr->MaxHeight = DEFAULT_CELLSIZE;
	for (i = 0, symbol = Ptr->Symbol; i <= MAX_FONTPOSITION; i++, symbol++)
	{
		Position		minx, miny,
						maxx, maxy;

			/* next one if the index isn't used or symbol is empty (SPACE) */
		if (!symbol->Valid || !symbol->LineN)
			continue;

		minx = miny = MAX_COORD;
		maxx = maxy = 0;
		for (line = symbol->Line, j = symbol->LineN; j; j--, line++)
		{
			minx = MIN(minx, line->Point1.X);
			miny = MIN(miny, line->Point1.Y);
			minx = MIN(minx, line->Point2.X);
			miny = MIN(miny, line->Point2.Y);
			maxx = MAX(maxx, line->Point1.X);
			maxy = MAX(maxy, line->Point1.Y);
			maxx = MAX(maxx, line->Point2.X);
			maxy = MAX(maxy, line->Point2.Y);
		}

			/* move symbol to left edge */
		for (line = symbol->Line, j = symbol->LineN; j; j--, line++)
			MOVE_LINE_LOWLEVEL(line, -minx, 0);

			/* set symbol bounding box with a minimum cell size of (1,1) */
		symbol->Width = maxx -minx +1;
		symbol->Height = maxy -miny +1;

			/* check total min/max  */
		Ptr->MaxWidth = MAX(Ptr->MaxWidth, symbol->Width);
		Ptr->MaxHeight = MAX(Ptr->MaxHeight, symbol->Height);
		totalminy = MIN(totalminy, miny);
	}

		/* move coordinate system to the upper edge (lowest y on screen) */
	for (i = 0, symbol = Ptr->Symbol; i <= MAX_FONTPOSITION; i++, symbol++)
		if (symbol->Valid)
			for (line = symbol->Line, j = symbol->LineN; j; j--, line++)
				MOVE_LINE_LOWLEVEL(line, 0, -totalminy);

		/* setup the box for the default symbol */
	Ptr->DefaultSymbol.X1 = Ptr->DefaultSymbol.Y1 = 0;
	Ptr->DefaultSymbol.X2 = Ptr->DefaultSymbol.X1 +Ptr->MaxWidth;
	Ptr->DefaultSymbol.Y2 = Ptr->DefaultSymbol.Y1 +Ptr->MaxHeight;
} 

/* ----------------------------------------------------------------------
 * parses the group definition string which is a colon seperated list of
 * comma seperated layer numbers (1,2,b:4,6,8,t)
 */
int ParseGroupString(char *s, LayerGroupTypePtr LayerGroup)
{
	int		group,
			member,
			layer;
	Boolean	c_set = False,	/* flags for the two special layers to */
			s_set = False;	/* provide a default setting for old formats */

		/* clear struct */
	memset(LayerGroup, 0, sizeof (LayerGroupType));

		/* loop over all groups */
	for (group = 0; s && *s && group < MAX_LAYER; group++)
	{
		if (group >= MAX_LAYER)
			goto error;
		while (*s && isspace(*s))
			s++;

			/* loop over all group members */
		for (member = 0; *s; s++)
		{
				/* ignore white spaces and get layernumber */
			while(*s && isspace(*s))
				s++;
			switch (*s)
			{
				case 'c':
				case 'C':	layer = MAX_LAYER +COMPONENT_LAYER;
							c_set = True;
							break;

				case 's':
				case 'S':	layer = MAX_LAYER +SOLDER_LAYER;
							s_set = True;
							break;

				default:	if (!isdigit(*s))
								goto error;
							layer = atoi(s) -1;
							break;
			}
			if (layer > MAX_LAYER+ MAX(SOLDER_LAYER, COMPONENT_LAYER) ||
				member >= MAX_LAYER+1)
				goto error;
			LayerGroup->Entries[group][member++] = layer;
			while (*++s && isdigit(*s));

				/* ignore white spaces and check for seperator */
			while (*s && isspace(*s))
				s++;
			if (!*s || *s == ':')
				break;
			if (*s != ',')
				goto error;
		}
		LayerGroup->Number[group] = member;
		if (*s == ':')
			s++;
	}
	if (!s_set)
		LayerGroup->Entries[SOLDER_LAYER][LayerGroup->Number[SOLDER_LAYER]++] =
			MAX_LAYER +SOLDER_LAYER;
	if (!c_set)
		LayerGroup->Entries[COMPONENT_LAYER][LayerGroup->Number[COMPONENT_LAYER]++] =
			MAX_LAYER +COMPONENT_LAYER;
	return(0);

			/* reset structure on error */
	error:
		memset(LayerGroup, 0, sizeof (LayerGroupType));
		return(1);
}

/* ---------------------------------------------------------------------------
 * quits application
 */
void QuitApplication(void)
{
		/* save data if necessary */
	if (PCB->Changed && Settings.SaveInTMP)
			EmergencySave();
	exit(0);
}

/* ---------------------------------------------------------------------------
 * creates a filename from a template
 * %f is replaced by the filename 
 * %p by the searchpath
 */
char *EvaluateFilename(char *Template, char *Path, char *Filename,
	char *Parameter)
{
	static	DynamicStringType	command;
			char				*p;

	DSClearString(&command);
	for (p = Template; *p; p++)
	{
			/* copy character or add string to command */
		if (*p == '%' && (*(p+1) == 'f' || *(p+1) == 'p' || *(p+1) == 'a'))
			switch(*(++p))
			{
				case 'a': DSAddString(&command, Parameter); break;
				case 'f': DSAddString(&command, Filename); break;
				case 'p': DSAddString(&command, Path); break;
			}
		else
			DSAddCharacter(&command, *p);
	}
	DSAddCharacter(&command, '\0');
	return(MyStrdup(command.Data, "EvaluateFilename()"));
}

/* ---------------------------------------------------------------------------
 * concatenates directory and filename if directory != NULL,
 * expands them with a shell and returns the found name(s) or NULL
 */
char *ExpandFilename(char *Dirname, char *Filename)
{
	static	DynamicStringType	answer;
			char				*command;
			FILE				*pipe;
			int					c;

		/* allocate memory for commandline and build it */
	DSClearString(&answer);
	if (Dirname)
	{
		command = MyCalloc(strlen(Filename) +strlen(Dirname) +7,
			sizeof(char), "ExpandFilename()");
		sprintf(command, "echo %s/%s", Dirname, Filename);
	}
	else
	{
		command = MyCalloc(strlen(Filename) +6, sizeof(char), "Expand()");
		sprintf(command, "echo %s", Filename);
	}

		/* execute it with shell */
	if ((pipe = popen(command, "r")) != NULL)
	{
			/* discard all but the first returned line */
		for(;;)
		{
			if ((c = fgetc(pipe)) == EOF || c == '\n' || c == '\r')
				break;
			else
				DSAddCharacter(&answer, c);
		}

		SaveFree(command);
		return (pclose(pipe) ? NULL : answer.Data);
	}

		/* couldn't be expanded by the shell */
	PopenErrorMessage(command);
	SaveFree(command);
	return(NULL);	
}

/* ---------------------------------------------------------------------------
 * wait for a button of key event in output window and calls the
 * action when a button event has been detected
 * cursor key events are handled
 */
Boolean GetPosition(char *MessageText)
{
	XEvent		event;
	XAnyEvent	*any = (XAnyEvent *) &event;
	KeySym		keysym;
	char		buffer[10];
	Cursor		oldCursor;

	MessagePrompt(MessageText);
	oldCursor = SetOutputXCursor(XC_hand2);

		/* eat up all events that would cause actions to be performed */
	for(;;)
	{
		XtAppNextEvent(Context, &event);
		switch(any->type)
		{
			case KeyRelease:
			case KeyPress:
			case ButtonPress:
					/* first check if we are inside the output window */
				if (any->window != Output.OutputWindow)
					break;

					/* evaluate cursor keys and modifier keys;
					 * dispatch the event if true
					 */
				XLookupString((XKeyEvent *) &event, buffer, sizeof(buffer),
					&keysym, NULL);
				if (IsCursorKey(keysym) || IsModifierKey(keysym))
					XtDispatchEvent(&event);
				else
				{
						/* abort on any other key;
						 * restore cursor and clear message line
						 */
					SetOutputXCursor(oldCursor);
					MessagePrompt(NULL);
					return(any->type == ButtonPress);
				}
				break;

			default:
				XtDispatchEvent(&event);
		}
	}
}

/* ----------------------------------------------------------------------
 * releases tmp pixmap used to save output data
 */
void ReleaseSaveUnderPixmap(void)
{
	if (VALID_PIXMAP(PCB->SaveUnder))
		XFreePixmap(Dpy, PCB->SaveUnder);

		/* mark pixmap unusable */
	PCB->SaveUnder = BadAlloc;
}

/* ----------------------------------------------------------------------
 * saves output window to tmp pixmap to reduce redrawing
 */
void SaveOutputWindow(void)
{
		/* release old pixmap and allocate new one */
	ReleaseSaveUnderPixmap();

		/* create new one only if window isn't obscured */
	if (Output.VisibilityOK)
	{
		PCB->SaveUnder = XCreatePixmap(Dpy, Output.OutputWindow,
			Output.Width, Output.Height,
			DefaultDepthOfScreen(XtScreen(Output.Output)));

			/* make sure that cursor is off */
		if (VALID_PIXMAP(PCB->SaveUnder))
			XCopyArea(Dpy, Output.OutputWindow, PCB->SaveUnder, Output.fgGC,
				Output.OffsetX, Output.OffsetY,
				Output.Width, Output.Height,
				0, 0);
	}
}

/* ---------------------------------------------------------------------------
 * sets the X cursor for the output window (uses cursorfont)
 * XSync() is used to force a cursor change, old memory is released
 */
unsigned int SetOutputXCursor(unsigned int Shape)
{
	unsigned int	oldShape = Output.XCursorShape;
	Cursor			oldCursor = Output.XCursor;

		/* check if window exists to prevent from fatal errors */
	if (Output.OutputWindow)
	{
		Output.XCursorShape = Shape;
		Output.XCursor= XCreateFontCursor(Dpy, Shape);
		XDefineCursor(Dpy, Output.OutputWindow, Output.XCursor);
		XSync(Dpy, False);

			/* release old cursor and return old shape */
		if (oldCursor)
			XFreeCursor(Dpy, oldCursor);
		return(oldShape);
	}
	return(DEFAULT_CURSORSHAPE);
}

/* ---------------------------------------------------------------------------
 * returns the layer number for the passed pointer
 */
int GetLayerNumber(DataTypePtr Data, LayerTypePtr Layer)
{
	int		i;

	for (i = 0; i < MAX_LAYER; i++)
		if (Layer == &Data->Layer[i])
			break;
	return(i);
}

/* ---------------------------------------------------------------------------
 * returns the layergroup number for the passed pointer
 */
int GetLayerGroupNumberByPointer(LayerTypePtr Layer)
{
	return(GetLayerGroupNumberByNumber(GetLayerNumber(PCB->Data, Layer)));
}

/* ---------------------------------------------------------------------------
 * returns the layergroup number for the passed layernumber
 */
int GetLayerGroupNumberByNumber(Cardinal Layer)
{
	int		group,
			entry;

	for (group = 0; group < MAX_LAYER+2; group++)
		for (entry = 0; entry < PCB->LayerGroups.Number[group]; entry++)
			if (PCB->LayerGroups.Entries[group][entry] == Layer)
				return(group);

		/* since every layer belongs to a group it is save to return
		 * the value without boundary checking
		 */
	return(group);
}

/* ---------------------------------------------------------------------------
 * returns a pointer to an objects bounding box;
 * data is valid until the routine is called again
 */
BoxTypePtr GetObjectBoundingBox(int Type, void *Ptr1, void *Ptr2, void *Ptr3)
{
	static	BoxType	box;

	switch(Type)
	{
		case VIA_TYPE:
		{
			PinTypePtr	via = (PinTypePtr) Ptr1;

			box.X1 = via->X -via->Thickness/2;
			box.Y1 = via->Y -via->Thickness/2;
			box.X2 = via->X +via->Thickness/2;
			box.Y2 = via->Y +via->Thickness/2;
			break;
		}

		case LINE_TYPE:
		{
			LineTypePtr	line = (LineTypePtr) Ptr2;

			box.X1 = MIN(line->Point1.X, line->Point2.X);
			box.Y1 = MIN(line->Point1.Y, line->Point2.Y);
			box.X2 = MAX(line->Point1.X, line->Point2.X);
			box.Y2 = MAX(line->Point1.Y, line->Point2.Y);
			break;
		}

		case TEXT_TYPE:
		case ELEMENTNAME_TYPE:
			box = ((TextTypePtr) Ptr2)->BoundingBox;
			break;

		case POLYGON_TYPE:
			box = ((PolygonTypePtr) Ptr2)->BoundingBox;
			break;

		case ELEMENT_TYPE:
			box = ((ElementTypePtr) Ptr1)->BoundingBox;
			break;

		case PAD_TYPE:
		{
			PadTypePtr	pad = (PadTypePtr) Ptr2;

			box.X1 = MIN(pad->Point1.X, pad->Point2.X);
			box.Y1 = MIN(pad->Point1.Y, pad->Point2.Y);
			box.X2 = MAX(pad->Point1.X, pad->Point2.X);
			box.Y2 = MAX(pad->Point1.Y, pad->Point2.Y);
			break;
		}

		case PIN_TYPE:
		{
			PinTypePtr	pin = (PinTypePtr) Ptr2;

			box.X1 = pin->X -pin->Thickness/2;
			box.Y1 = pin->Y -pin->Thickness/2;
			box.X2 = pin->X +pin->Thickness/2;
			box.Y2 = pin->Y +pin->Thickness/2;
			break;
		}

		case LINEPOINT_TYPE:
		{
			PointTypePtr	point = (PointTypePtr) Ptr3;

			box.X1 = box.X2 = point->X;
			box.Y1 = box.Y2 = point->Y;
			break;
		}

		case POLYGONPOINT_TYPE:
		{
			PointTypePtr	point = (PointTypePtr) Ptr3;

			box.X1 = box.X2 = point->X;
			box.Y1 = box.Y2 = point->Y;
			break;
		}

		default:
			return(NULL);
	}
	return(&box);
}

/* ---------------------------------------------------------------------------
 * resets the layerstack setting
 */
void ResetStackAndVisibility(void)
{
	Cardinal	i;

	for (i = 0; i < MAX_LAYER; i++)
	{
		LayerStack[i] = i;
		PCB->Data->Layer[i].On = True;
	}
	PCB->ElementOn = True;
	PCB->InvisibleObjectsOn = True;
	PCB->PinOn = True;
	PCB->ViaOn = True;
}

/* ---------------------------------------------------------------------------
 * changes the maximum size of a layout
 * adjusts the scrollbars
 * releases the saved pixmap if necessary
 * and adjusts the cursor confinement box
 */
void ChangePCBSize(Dimension Width, Dimension Height)
{
		/* force a redraw of the forbidden area */
	ReleaseSaveUnderPixmap();
	PCB->MaxWidth =  Width;
	PCB->MaxHeight = Height;
	Output.Width = TO_SCREEN(Width);
	Output.Height = TO_SCREEN(Height);
	XtVaSetValues(Output.Output,
		XtNwidth, Output.Width,
		XtNheight, Output.Height,
		NULL);

		/* crosshair range is different if pastebuffer-mode
		 * is enabled
		 */
	if (Settings.Mode == PASTEBUFFER_MODE)
		SetCrosshairRange(
			PASTEBUFFER->X -PASTEBUFFER->BoundingBox.X1,
			PASTEBUFFER->Y -PASTEBUFFER->BoundingBox.Y1,
			MAX(0, Width -(PASTEBUFFER->BoundingBox.X2-PASTEBUFFER->X)),
			MAX(0, Height -(PASTEBUFFER->BoundingBox.Y2-PASTEBUFFER->Y)));
	else
		SetCrosshairRange(0, 0, (Position) Width, (Position) Height);
}

/* ----------------------------------------------------------------------
 * returns pointer to current working directory
 */
char *GetWorkingDirectory(void)
{
	static	char	path[MAXPATHLEN+1];

#ifdef SYSV
	return(getcwd(path, MAXPATHLEN));
#else
		/* seems that some BSD releases lack of a prototype for getwd() */
	return((char *) getwd(path));
#endif
}

/* ---------------------------------------------------------------------------
 * writes a string to the passed file pointer
 * some special characters are quoted
 */
void CreateQuotedString(DynamicStringTypePtr DS, char *S)
{
	DSClearString(DS);
	DSAddCharacter(DS, '"');
	while (*S)
	{
		if (*S == '"' || *S == '\\')
			DSAddCharacter(DS, '\\');
		DSAddCharacter(DS, *S++);
	}
	DSAddCharacter(DS, '"');
}

