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

static	char	*rcsid = "$Id: dev_ps.c,v 143.1 1996/09/16 09:08:34 nau Exp $";

/* PostScript device driver
 * code is shared for EPS and PS output
 */
#include <math.h>
#include <time.h>
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include "global.h"

#include "data.h"
#include "dev_ps.h"
#include "error.h"
#include "misc.h"
#include "rotate.h"

/* ---------------------------------------------------------------------------
 * some defines
 */
#define	PS_UNIT		0.072		/* 1 mil in PostScript units */

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	PrintStringArray(char **, int, FILE *);
static	char	*PS_Init(PrintInitTypePtr, char *);
static	char	*EPS_Init(PrintInitTypePtr, char *);
static	char	*PS_EPS_Init(PrintInitTypePtr, char *, Boolean);
static	void	PS_Exit(void);
static	void	EPS_Exit(void);
static	void	PS_EPS_Exit(Boolean);
static	void	PS_SetColor(XColor);
static	void	PS_PrintLayer(LayerTypePtr, int);
static	void	PS_PrintLine(LineTypePtr);
static	void	PS_PrintPolygon(PolygonTypePtr);
static	void	PS_PrintText(TextTypePtr);
static	void	PS_PrintElementPackage(ElementTypePtr);
static	void	PS_PrintPad(PadTypePtr);
static	void	PS_PrintPinOrVia(PinTypePtr);
static	void	PS_PrintClearPinOrViaOnGroundplane(PinTypePtr);
static	void	PS_PrintPadMask(PadTypePtr);
static	void	PS_PrintPinOrViaMask(PinTypePtr);
static	void	PS_FilledRectangle(Position, Position, Position, Position);
static	void	PS_Outline(Position, Position, Position, Position);
static	void	PS_Alignment(Position, Position, Position, Position);
static	void	PS_DrillHelper(PinTypePtr);

/* ----------------------------------------------------------------------
 * some local identifiers
 *
 * PSFunctions is initializes by the standard PostScript code used by PCB.
 * All PS functions are declared in this section:
 *   A:         draws an element arc
 *   Alignment: draws the boards outline
 *   B:         draws a filled box
 *   CLRPA:     clears a pad
 *   CLRPV:     clears a circle
 *   CLRPVSQ:   clears a square
 *   DH:        drill helper, a small circle
 *   FILL:      draws a filled rectangle for the ground plane
 *   L:         draws a line
 *   Outline:   draws the boards outline
 *   P:         draws a filled pin-polygon 
 *   PA:        draws a pad
 *   PO:        draws a filled polygon 
 *   PV:        draws a pin or via
 *   PVSQ:      draws a square pin or via
 * additional information is available from the PS comments.
 *
 * some PS code for arcs comes from the output of 'Fig2Dev'. Thanks to
 * the author
 */
static	PrintDeviceType	PS_QueryConstants = {
			"PostScript",
			"ps",
			PS_Init,
			PS_Exit,
			PS_SetColor,
			NULL,				/* no drill information */
			PS_PrintLayer,
			PS_PrintElementPackage,
			PS_PrintPad,
			PS_PrintPinOrVia,
			PS_PrintPadMask,
			PS_PrintPinOrViaMask,
			PS_PrintClearPinOrViaOnGroundplane,
			PS_FilledRectangle,
			PS_Outline,
			PS_Alignment,
			PS_DrillHelper,
			True,				/* driver can handle color */
			True,				/* and inverted output */
			True,				/* handles ground planes */
			False,				/* handles no drill information */
			True,				/* handles mask layers */
			True },				/* handles different media */

						EPS_QueryConstants = {
			"encapsulated PostScript",
			"eps",
			EPS_Init,
			EPS_Exit,
			PS_SetColor,
			NULL,				/* no drill information */
			PS_PrintLayer,
			PS_PrintElementPackage,
			PS_PrintPad,
			PS_PrintPinOrVia,
			PS_PrintPadMask,
			PS_PrintPinOrViaMask,
			PS_PrintClearPinOrViaOnGroundplane,
			PS_FilledRectangle,
			PS_Outline,
			PS_Alignment,
			PS_DrillHelper,
			True,
			True,
			True,
			False,
			True,
			False};

static	PrintInitType	PS_Flags;

static	char	*PS_Functions[] = {
	"",
	"/PcbDict 200 dict def",
	"PcbDict begin",
	"PcbDict /DictMatrix matrix put",
	"",
	"% some constants",
	"/Black {0.0 mysetgray} def",
	"/White {1.0 mysetgray} def",
	"/TAN {0.207106781} def",
	"/MTAN {-0.207106781} def",
	"",
	"% draw a filled polygon",
	"% get (x,y)... and number of points from stack",
	"/PO {",
	"	/number exch def",
	"	newpath",
	"	moveto",
	"	number 1 sub { lineto } repeat",
	"	closepath fill stroke",
	"} def",
	"",
	"/P {",
	"% draw a pin-polygon,",
	"% get x, y and thickness from stack",
	"	/thickness exch def /y exch def /x exch def",
	"	gsave x y translate thickness thickness scale",
	"	0.5  MTAN",
	"	TAN  -0.5",
	"	MTAN -0.5",
	"	-0.5 MTAN",
	"	-0.5 TAN",
	"	MTAN 0.5",
	"	TAN  0.5",
	"	0.5  TAN",
	"	8 PO grestore",
	"} def",
	"",
	"/PV {",
	"% pin or via, x, y and thickness are on the stack",
	"	/drillinghole exch def /thickness exch def /y exch def /x exch def",
	"	x y thickness P",
	"% draw drilling whole",
	"	gsave White 0 setlinewidth",
	"   newpath x y drillinghole 2 div 0 360 arc closepath fill stroke",
	"   grestore",
	"} def",
	"",
	"/PVSQ {",
	"% square pin or via, x, y and thickness are on the stack",
	"	/drillinghole exch def /thickness exch def /y exch def /x exch def",
	"	newpath x thickness 2 div sub y thickness 2 div sub moveto",
	"	thickness 0 rlineto 0 thickness rlineto",
	"	thickness neg 0 rlineto closepath fill stroke",
	"% draw drilling whole",
	"	gsave White 0 setlinewidth",
	"   newpath x y drillinghole 2 div 0 360 arc closepath fill stroke",
	"   grestore",
	"} def",
	"",
	"/DH {",
	"% drill helpher; x, y, hole, copper-thickness are on stack",
	"	/copper exch def /hole exch def /y exch def /x exch def",
	"   gsave copper setlinewidth",
	"   newpath x y hole copper add 2 div 0 360 arc closepath stroke",
	"   grestore",
	"} def",
	"",
	"/L {",
	"% line, get x1, y1, x2, y2 and thickness from stack",
	"	/thick exch def /y2 exch def /x2 exch def /y1 exch def /x1 exch def",
	"	gsave thick setlinewidth",
	"	x1 y1 moveto x2 y2 lineto stroke",
	"	grestore",
	"} def",
	"",
	"/B {",
	"% filled box, get x1, y1, x2 and y2 from stack",
	"	/y2 exch def /x2 exch def /y1 exch def /x1 exch def",
	"	newpath",
	"	x1 y1 moveto x2 y1 lineto x2 y2 lineto x1 y2 lineto",
	"	closepath fill stroke",
	"} def",
	"",
	"/PA {",
	"% pad, same as line",
	"	L",
	"} def",
	"",
	"/A {",
	"% arc for elements, get x, y, width, height, thickness",
	"% startangle and delta-angle from stack",
	"  /delta exch def /start exch def /thickness exch def",
	"  /height exch def /width exch def /y exch def /x exch def",
	"% draw it",
	"	gsave thickness setlinewidth /save DictMatrix currentmatrix def",
	"% scaling is less then zero because the coord system has to be swapped",
	"	x y translate width neg height scale",
	"	0 0 1 start start delta add arc save setmatrix stroke",
	"	grestore",
	"} def",
	"",
	"/CLRPV {",
	"% clears a pin/via for groundplane; x,y and thickness are on stack",
	"   /thickness exch def /y exch def /x exch def",
	"	gsave White x y thickness P grestore",
	"} def",
	"",
	"/CLRPVSQ {",
	"% clears a square pin, x,y and thickness are on stack",
	"	/thickness exch def /y exch def /x exch def",
	"   gsave White",
	"	newpath x thickness 2 div sub y thickness 2 div sub moveto",
	"	thickness 0 rlineto 0 thickness rlineto",
	"	thickness neg 0 rlineto closepath fill stroke",
	"	grestore",
	"} def",
	"",
	"/CLRPA {",
	"% clears a pad, x1, y1, x2, y2 abd thickness are on stack",
	"	/thick exch def /y2 exch def /x2 exch def /y1 exch def /x1 exch def",
	"	gsave White x1 y1 x2 y2 thick PA grestore",
	"} def",
	"",
	"/FILL {",
	"% draw a filled rectangle for the ground plane",
	"% get x1, y1, x2 and y2 from stack",
	"	/y2 exch def /x2 exch def /y1 exch def /x1 exch def",
	"   gsave 0 setlinewidth",
	"	newpath",
	"	x1 y1 moveto x2 y1 lineto x2 y2 lineto x1 y2 lineto",
	"	closepath fill stroke",
	"   grestore",
	"} def",
	"",
	"/Outline {",
	"% outline, get x1, y1, x2 and y2 from stack",
	"	/y2 exch def /x2 exch def /y1 exch def /x1 exch def",
	"   gsave 0.175 setlinewidth",
	"	newpath",
	"	x1 y1 moveto x2 y1 lineto x2 y2 lineto x1 y2 lineto",
	"	closepath stroke",
	"   grestore",
	"} def",
	"",
	"/Alignment {",
	"% alignment targets, get x1, y1, x2, y2 and distance from stack",
	"	/dis exch def /y2 exch def /x2 exch def /y1 exch def /x1 exch def",
	"   gsave 0.175 setlinewidth",
	"   newpath x1 y1 dis add moveto 0 dis neg rlineto dis 0 rlineto stroke",
	"   newpath x1 y2 dis sub moveto 0 dis rlineto dis 0 rlineto stroke",
	"   newpath x2 y2 dis sub moveto 0 dis rlineto dis neg 0 rlineto stroke",
	"   newpath x2 y1 dis add moveto 0 dis neg rlineto dis neg 0 rlineto stroke",
	"   grestore",
	"} def",
	"" };

static	char	*PS_ColorFunctions[] = {
	"/mysetgray { setgray } def",
	"/mysetrgbcolor { setrgbcolor } def",
	"" };

static	char	*PS_InvertColorFunctions[] = {
	"/mysetgray {neg 1.0 add setgray} def",
	"/mysetrgbcolor {",
	"  /blue exch def /green exch def /red exch def",
	"  1.0 red sub 1.0 green sub 1.0 blue sub setrgbcolor",
	"} def"
	"" };

static	char	*PS_Setup[] = {
	"0.0 setlinewidth",
	"1 setlinecap",
	"Black" };

/* ---------------------------------------------------------------------------
 * prints a string array
 */
static void PrintStringArray(char **Array, int Number, FILE *FP)
{
	for (; Number; Number--, Array++)
		fprintf(FP, "%s\n", *Array);
}

/* ---------------------------------------------------------------------------
 * returns information about the PostScript driver
 */
PrintDeviceTypePtr PS_Query(void)
{
	return(&PS_QueryConstants);
}

/* ---------------------------------------------------------------------------
 * returns information about the encapsulated PostScript driver
 */
PrintDeviceTypePtr EPS_Query(void)
{
	return(&EPS_QueryConstants);
}

/* ----------------------------------------------------------------------
 * call init routine for PostScript output
 */
static char *PS_Init(PrintInitTypePtr Flags, char *Description)
{
	return(PS_EPS_Init(Flags, Description, False));
}

/* ----------------------------------------------------------------------
 * call init routine for encapsulated PostScript output
 */
static char *EPS_Init(PrintInitTypePtr Flags, char *Description)
{
	return(PS_EPS_Init(Flags, Description, True));
}

/* ----------------------------------------------------------------------
 * prints PostScript or enc. PostScript header with function definition
 * info struct is are passed in
 */
static char *PS_EPS_Init(PrintInitTypePtr Flags, char *Description,
	Boolean CreateEPS)
{
			BoxType		box;
			time_t		currenttime;
			float		dx, dy;
	struct	passwd		*pwentry;

		/* save passed-in data */
	PS_Flags = *Flags;
	currenttime = time(NULL);

		/* adjust the 'mirror' flag because (0,0) is in the lower/left
		 * corner which is different to X11
		 */
	if (!Settings.ShowSolderSide)
		PS_Flags.MirrorFlag = !PS_Flags.MirrorFlag;

		/* adjusting the offsets is also necessary because the
		 * passed coordinates are X11 ones (upper/left corner is 0,0)
		 */
	dx = dy = PS_Flags.Scale;
	dx *= (float) (PS_Flags.BoundingBox.X2 -PS_Flags.BoundingBox.X1);
	dy *= (float) (PS_Flags.BoundingBox.Y2 -PS_Flags.BoundingBox.Y1);
	PS_Flags.OffsetY = PS_Flags.SelectedMedia->Height -PS_Flags.OffsetY;
	PS_Flags.OffsetY -= (Dimension) (PS_Flags.RotateFlag ? dx : dy);

		/* create standard PS header */
	if (CreateEPS)
		fputs("%!PS-Adobe-3.0 EPSF-3.0\n", PS_Flags.FP);
	else
		fputs("%!PS-Adobe-3.0\n", PS_Flags.FP);
	fprintf(PS_Flags.FP, "%%%%Title: %s, %s\n", UNKNOWN(PCB->Name),
		UNKNOWN(Description));
	fprintf(PS_Flags.FP, "%%%%Creator: %s "RELEASE"\n", Progname);
	fprintf(PS_Flags.FP, "%%%%CreationDate: %s",
		asctime(localtime(&currenttime)));
	pwentry = getpwuid(getuid());
	fprintf(PS_Flags.FP, "%%%%For: %s (%s)\n", pwentry->pw_name,
		pwentry->pw_gecos);
	fputs("%%LanguageLevel: 1\n", PS_Flags.FP);
	fputs("%%Orientation: Portrait\n", PS_Flags.FP);

		/* - calculate the width and height of the bounding box;
		 * - write bounding box data to file
		 * - rotate it
		 * - transform to PS coordinates#
		 */
	box.X1 = (Position) ((float) PS_Flags.OffsetX * PS_UNIT) -1;
	box.Y1 = (Position) ((float) PS_Flags.OffsetY * PS_UNIT) -1;
	if (!PS_Flags.RotateFlag)
	{
		box.X2= (Position)((dx +PS_Flags.OffsetX) *PS_UNIT) +1;
		box.Y2= (Position)((dy +PS_Flags.OffsetY) *PS_UNIT) +1;
	}
	else
	{
		box.X2= (Position)((dy +PS_Flags.OffsetX) *PS_UNIT) +1;
		box.Y2= (Position)((dx +PS_Flags.OffsetY) *PS_UNIT) +1;
	}

		/* print it if encapsulated PostScript has been requested */
	if (CreateEPS)
	{
		fprintf(PS_Flags.FP, "%%%%BoundingBox: %i %i %i %i\n",
			(int) box.X1, (int) box.Y1, (int) box.X2, (int) box.Y2);
		fputs("%%Pages: 0\n", PS_Flags.FP);
	}
	else
	{
		fputs("%%Pages: 1\n", PS_Flags.FP);
		fputs("%%PageOrder: Ascend\n", PS_Flags.FP);
	}

		/* OK, continue with structured comments */
	fprintf(PS_Flags.FP, "%%%%IncludeFeature: *PageSize %s\n",
		PS_Flags.SelectedMedia->Name);
	fputs("%%EndComments\n", PS_Flags.FP);
	fputs("%%BeginProlog\n", PS_Flags.FP);
	PrintStringArray(PS_Functions, ENTRIES(PS_Functions), PS_Flags.FP);
	if (PS_Flags.InvertFlag)
		PrintStringArray(PS_InvertColorFunctions,
			ENTRIES(PS_InvertColorFunctions), PS_Flags.FP);
	else
		PrintStringArray(PS_ColorFunctions,
			ENTRIES(PS_ColorFunctions), PS_Flags.FP);
	fputs("%%EndProlog\n", PS_Flags.FP);
	fputs("%%BeginDefaults\n", PS_Flags.FP);
	fputs("%%EndDefaults\n", PS_Flags.FP);
	fputs("%%BeginSetup\n", PS_Flags.FP);
	if (!CreateEPS && strcmp(PS_Flags.SelectedMedia->Name, USERMEDIANAME))
		fprintf(PS_Flags.FP, "%s\n", PS_Flags.SelectedMedia->Name);
	PrintStringArray(PS_Setup, ENTRIES(PS_Setup), PS_Flags.FP);
	fputs("%%EndSetup\n", PS_Flags.FP);

	if (!CreateEPS)
	{
		fputs("%%Page: 1 1\n", PS_Flags.FP);
		fputs("%%BeginPageSetup\n", PS_Flags.FP);
		fputs("%%EndPageSetup\n", PS_Flags.FP);
	}

		/* clear the area */
	fputs("gsave White newpath\n", PS_Flags.FP);
	fprintf(PS_Flags.FP,
		"%i %i moveto %i %i lineto %i %i lineto %i %i lineto\n",
		(int) box.X1, (int) box.Y1,
		(int) box.X2, (int) box.Y1,
		(int) box.X2, (int) box.Y2,
		(int) box.X1, (int) box.Y2);
	fputs("closepath fill stroke grestore\n", PS_Flags.FP);

		/* add information about layout size, offset ... */
	fprintf(PS_Flags.FP, "%% PCBMIN(%d,%d), PCBMAX(%d,%d)\n",
		(int) PS_Flags.BoundingBox.X1, (int) PS_Flags.BoundingBox.Y1,
		(int) PS_Flags.BoundingBox.X2, (int) PS_Flags.BoundingBox.Y2);
	fprintf(PS_Flags.FP, "%% PCBOFFSET(%d,%d), PCBSCALE(%.5f)\n",
		PS_Flags.OffsetX, PS_Flags.OffsetY, PS_Flags.Scale);
	fputs("% PCBSTARTDATA --- do not remove ---\n", PS_Flags.FP);
	fputs("gsave\n", PS_Flags.FP);

		/* now insert transformation commands (reverse order):
		 * - move upper/left edge of layout to (0,0)
		 * - mirror it to transform X to PostScript coordinates
		 * - move to (0,0) again
		 * - if rotation is required, rotate and move to (0,0)
		 * - apply user scaling
		 * - move to new offset
		 * - scale to PostScript (72 dots per inch)
		 */ 
	fprintf(PS_Flags.FP, "%.5f %.5f scale\n",
		PS_UNIT, PS_UNIT);
	fprintf(PS_Flags.FP, "%i %i translate\n",
		PS_Flags.OffsetX, PS_Flags.OffsetY);
	fprintf(PS_Flags.FP, "%.3f %.3f scale\n",
		PS_Flags.Scale, PS_Flags.Scale);
	if (PS_Flags.RotateFlag)
	{
		fprintf(PS_Flags.FP, "%i 0 translate\n",
			(int) PS_Flags.BoundingBox.Y2 -PS_Flags.BoundingBox.Y1);
		fputs("90 rotate\n", PS_Flags.FP);
	}
	if (PS_Flags.MirrorFlag)
	{
		fprintf(PS_Flags.FP, "0 %i translate\n",
			(int) (PS_Flags.BoundingBox.Y2 -PS_Flags.BoundingBox.Y1));
		fputs("1 -1 scale\n", PS_Flags.FP);
	}
	fprintf(PS_Flags.FP, "%i %i translate\n", -PS_Flags.BoundingBox.X1,
		-PS_Flags.BoundingBox.Y1);
	return(NULL);
}

/* ----------------------------------------------------------------------
 * call exit routine for PostScript output
 */
static void PS_Exit(void)
{
	PS_EPS_Exit(False);
}

/* ----------------------------------------------------------------------
 * call exit routine for encapsulated PostScript output
 */
static void EPS_Exit(void)
{
	PS_EPS_Exit(True);
}

/* ---------------------------------------------------------------------------
 * exit code for this driver is empty
 */
static void PS_EPS_Exit(Boolean CreateEPS)
{
		/* print trailing commands */
	fputs("grestore\n", PS_Flags.FP);
	fputs("% PCBENDDATA --- do not remove ---\n", PS_Flags.FP);
	if (!CreateEPS)
		fputs("showpage\n", PS_Flags.FP);
	fputs("%%EOF\n", PS_Flags.FP);
}

/* ----------------------------------------------------------------------
 * prints layer data
 */
static void PS_PrintLayer(LayerTypePtr Layer, int GroupNumber)
{
	fprintf(PS_Flags.FP, "%% LayerGroup: %i, Layer: \"%s\" (#%i)\n",
		GroupNumber, UNKNOWN(Layer->Name), GetLayerNumber(PCB->Data, Layer));
	LINE_LOOP(Layer, PS_PrintLine(line));
	TEXT_LOOP(Layer, PS_PrintText(text));
	POLYGON_LOOP(Layer, PS_PrintPolygon(polygon));
}

/* ----------------------------------------------------------------------
 * prints a line
 */
static void PS_PrintLine(LineTypePtr Line)
{
	fprintf(PS_Flags.FP, "%d %d %d %d %d L\n",
		(int) Line->Point1.X, (int) Line->Point1.Y,
		(int) Line->Point2.X, (int) Line->Point2.Y,
		(int) Line->Thickness);
}

/* ---------------------------------------------------------------------------
 * prints a filled polygon
 */
static void PS_PrintPolygon(PolygonTypePtr Ptr)
{
	int i = 0;

	POLYGONPOINT_LOOP(Ptr,
		{
			if (i++ % 9 == 8)
				fputc('\n', PS_Flags.FP);
			fprintf(PS_Flags.FP, "%i %i ",
				(int) point->X, (int) point->Y);
		}
	);
	fprintf(PS_Flags.FP, "%d PO\n", Ptr->PointN);
}

/* ----------------------------------------------------------------------
 * prints a text
 * the routine is identical to DrawText() in module draw.c except
 * that DrawLine() and DrawRectangle() are replaced by their corresponding
 * printing routines
 */
static void PS_PrintText(TextTypePtr Text)
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
				PS_PrintLine(&newline);
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
			RotateBoxLowLevel(&defaultsymbol, 0, 0, Text->Direction);

				/* add offset and draw box */
			defaultsymbol.X1 += Text->X;
			defaultsymbol.Y1 += Text->Y;
			defaultsymbol.X2 += Text->X;
			defaultsymbol.Y2 += Text->Y;
			fprintf(PS_Flags.FP, "%d %d %d %d B\n",
				(int) defaultsymbol.X1, (int) defaultsymbol.Y1,
				(int) defaultsymbol.X2, (int) defaultsymbol.Y2);

				/* move on to next cursor position */
			x += size;
		}
		string++;
	}
}

/* ----------------------------------------------------------------------
 * prints package outline
 */
static void PS_PrintElementPackage(ElementTypePtr Element)
{
	ELEMENTLINE_LOOP(Element, PS_PrintLine(line););
	ARC_LOOP(Element, 
		fprintf(PS_Flags.FP, "%d %d %d %d %d %d %d A\n",
			(int) arc->X, (int) arc->Y,
			(int) arc->Width, (int) arc->Height,
			(int) arc->Thickness,
			arc->StartAngle,
			arc->Delta);
	);
	PS_PrintText(&ELEMENT_TEXT(PCB, Element));
}

/* ----------------------------------------------------------------------
 * prints a pad
 */
static void PS_PrintPad(PadTypePtr Pad)
{
	fprintf(PS_Flags.FP, "%d %d %d %d %d PA\n",
		(int) Pad->Point1.X, (int) Pad->Point1.Y,
		(int) Pad->Point2.X, (int) Pad->Point2.Y,
		(int) Pad->Thickness);
}

/* ----------------------------------------------------------------------
 * prints a via or pin
 */
static void PS_PrintPinOrVia(PinTypePtr Ptr)
{
	fprintf(PS_Flags.FP, "%d %d %d %d %s\n",
		(int) Ptr->X, (int) Ptr->Y,
		(int) Ptr->Thickness, (int) Ptr->DrillingHole,
		TEST_FLAG(SQUAREFLAG, Ptr) ? "PVSQ" : "PV");
}

/* ----------------------------------------------------------------------
 * clears the area around a via or pin on the groundplane
 */
static void PS_PrintClearPinOrViaOnGroundplane(PinTypePtr Ptr)
{
	fprintf(PS_Flags.FP, "%d %d %d %s\n",
		(int) Ptr->X, (int) Ptr->Y,
		TEST_FLAG(ONGROUNDPLANEFLAG, Ptr) ?
			(int) Ptr->DrillingHole :
			(int) Ptr->Thickness +2 *GROUNDPLANEFRAME,
		TEST_FLAG(SQUAREFLAG, Ptr) ? "CLRPVSQ" : "CLRPV");
}

/* ----------------------------------------------------------------------
 * prints a pad mask
 */
static void PS_PrintPadMask(PadTypePtr Pad)
{
	fprintf(PS_Flags.FP, "%d %d %d %d %d CLRPA\n",
		(int) Pad->Point1.X, (int) Pad->Point1.Y,
		(int) Pad->Point2.X, (int) Pad->Point2.Y,
		(int) Pad->Thickness);
}

/* ----------------------------------------------------------------------
 * prints a via or pin mask
 */
static void PS_PrintPinOrViaMask(PinTypePtr Ptr)
{
	fprintf(PS_Flags.FP, "%d %d %d %s\n",
		(int) Ptr->X, (int) Ptr->Y,
		(int) Ptr->Thickness,
		TEST_FLAG(SQUAREFLAG, Ptr) ? "CLRPVSQ" : "CLRPV");
}

/* ---------------------------------------------------------------------------
 * draws a filled rectangle for the ground plane
 */
static void PS_FilledRectangle(Position X1, Position Y1,
	Position X2, Position Y2)
{
	fprintf(PS_Flags.FP, "%d %d %d %d FILL\n",
		(int) X1, (int) Y1, (int) X2, (int) Y2);
}

/* ---------------------------------------------------------------------------
 * draw the outlines of a layout;
 * the upper/left and lower/right corner are passed
 */
static void PS_Outline(Position X1, Position Y1, Position X2, Position Y2)
{
	fprintf(PS_Flags.FP, "%d %d %d %d Outline\n",
		(int) X1, (int) Y1, (int) X2, (int) Y2);
}

/* ---------------------------------------------------------------------------
 * draw the alignment targets;
 * the upper/left and lower/right corner are passed
 */
static void PS_Alignment(Position X1, Position Y1, Position X2, Position Y2)
{
	fprintf(PS_Flags.FP, "%d %d %d %d %d Alignment\n",
		(int) X1, (int) Y1, (int) X2, (int) Y2,
		(int) Settings.AlignmentDistance);
}

/* ----------------------------------------------------------------------
 * prints a via or pin
 */
static void PS_DrillHelper(PinTypePtr Ptr)
{
	if (Ptr->DrillingHole >= 4*MIN_PINORVIAHOLE)
		fprintf(PS_Flags.FP, "%d %d %d %d DH\n",
			(int) Ptr->X, (int) Ptr->Y,
			(int) 2*MIN_PINORVIAHOLE, (int) MIN_PINORVIAHOLE);
}

/* ----------------------------------------------------------------------
 * queries color from X11 database and generates PostScript command
 * set PostScript identifier to the calculated value or to black by 
 * default
 */
static void PS_SetColor(XColor RGB)
{
	fprintf(PS_Flags.FP,
		"/Color {%.3f %.3f %.3f mysetrgbcolor} def Color\n",
		(float) RGB.red /65535.0,
		(float) RGB.green /65535.0,
		(float) RGB.blue /65535.0);
}
