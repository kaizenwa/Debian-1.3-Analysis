%{
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

static	char	*rcsid = "$Id: parse_y.y,v 143.1 1996/09/16 09:08:49 nau Exp $";

/* grammar to parse ASCII input of PCB description
 */

#include "global.h"
#include "create.h"
#include "data.h"
#include "error.h"
#include "mymem.h"
#include "misc.h"
#include "parse_l.h"
#include "remove.h"

static	LayerTypePtr	Layer;
static	PolygonTypePtr	Polygon;
static	SymbolTypePtr	Symbol;
static	Boolean			LayerFlag[MAX_LAYER];

extern	char			*yytext;		/* defined by LEX */
extern	PCBTypePtr		yyPCB;
extern	DataTypePtr		yyData;
extern	ElementTypePtr	yyElement;
extern	FontTypePtr		yyFont;
extern	int				yylineno;		/* linenumber */
extern	char			*yyfilename;	/* in this file */

%}

%union									/* define YYSTACK type */
{
	unsigned	number;
	char		*string;
}

%token	<number>	NUMBER CHAR_CONST	/* line thickness, coordinates ... */
%token	<string>	STRING				/* element names ... */

%token	T_PCB T_LAYER T_VIA T_LINE T_RECTANGLE T_TEXT T_ELEMENTLINE
%token	T_ELEMENT T_PIN T_PAD T_GRID T_FLAGS T_SYMBOL T_SYMBOLLINE T_CURSOR
%token	T_ELEMENTARC T_MARK T_GROUPS T_POLYGON

%type	<number>	symbolid

%%

parse
		: parsepcb
		| parseelement
		| parsefont
		| error { YYABORT; }
		;
		
parsepcb
		:	{
					/* reset flags for 'used layers';
					 * init font and data pointers
					 */
				int	i;

				if (!yyPCB)
				{
					Message("illegal fileformat\n");
					YYABORT;
				}
				for (i = 0; i < MAX_LAYER; i++)
					LayerFlag[i] = False;
				yyFont = &yyPCB->Font;
				yyData = yyPCB->Data;
			}
		  pcbname 
		  pcbgrid
		  pcbcursor
		  pcbflags
		  pcbgroups
		  pcbfont
		  pcbdata
		;

pcbfont
		: parsefont
		|
		;

parsefont
		:
			{
					/* mark all symbols invalid */
				int	i;

				if (!yyFont)
				{
					Message("illegal fileformat\n");
					YYABORT;
				}
				yyFont->Valid = False;
				for (i = 0; i <= MAX_FONTPOSITION; i++)
					yyFont->Symbol[i].Valid = False;
			}
		  symbols
			{
				yyFont->Valid = True;
		  		SetFontInfo(yyFont);
			}
		;

parseelement:
			{
				if (!yyElement)
				{
					Message("illegal fileformat\n");
					YYABORT;
				}
			}
		  element
		;

pcbname
		: T_PCB '(' STRING ')'
			{
				yyPCB->Name = $3;
				yyPCB->MaxWidth = MAX_COORD;
				yyPCB->MaxHeight = MAX_COORD;
			}
		| T_PCB '(' STRING NUMBER NUMBER ')'
			{
				yyPCB->Name = $3;
				yyPCB->MaxWidth = $4;
				yyPCB->MaxHeight = $5;
			}
		;	

pcbgrid
		: T_GRID '(' NUMBER NUMBER NUMBER ')'
			{
				yyPCB->Grid = $3;
				yyPCB->GridOffsetX = $4;
				yyPCB->GridOffsetY = $5;
			}
		|
		;

pcbcursor
		: T_CURSOR '(' NUMBER NUMBER NUMBER ')'
			{
				yyPCB->CursorX = $3;
				yyPCB->CursorY = $4;
				yyPCB->Zoom = $5;
			}
		|
		;

pcbflags
		: T_FLAGS '(' NUMBER ')'
			{
				yyPCB->Flags = $3 & PCB_FLAGS;
			}
		|
		;

pcbgroups
		: T_GROUPS '(' STRING ')'
			{
				if (ParseGroupString($3, &yyPCB->LayerGroups))
				{
					Message("illegal layer-group string\n");
					YYABORT;
				}
			}
		|
		;

pcbdata
		: pcbdefinitions
		|
		;

pcbdefinitions
		: pcbdefinition
		| pcbdefinitions pcbdefinition
		;

pcbdefinition
		: via
		| layer
		|
			{
					/* clear pointer to force memory allocation by 
					 * the appropriate subroutine
					 */
				yyElement = NULL;
			}
		  element
		| error { YYABORT; }
		;

via
		: via_newformat
		| via_oldformat
		;

via_newformat
			/* x, y, thickness, drilling-hole, name, flags */
		: T_VIA '(' NUMBER NUMBER NUMBER NUMBER STRING NUMBER ')'
			{
				CreateNewVia(yyData, $3, $4, $5, $6, $7,
					($8 & OBJ_FLAGS) | VIAFLAG);
				SaveFree($7);
			}
		;

via_oldformat
			/* old format: x, y, thickness, name, flags */
		: T_VIA '(' NUMBER NUMBER NUMBER STRING NUMBER ')'
			{
				Dimension	hole = ($5 *DEFAULT_DRILLINGHOLE)/100;

					/* make sure that there's enough copper left */
				if ($5 -hole < MIN_PINORVIACOPPER && 
					$5 > MIN_PINORVIACOPPER)
					hole = $5 -MIN_PINORVIACOPPER;

				CreateNewVia(yyData, $3, $4, $5, hole, $6,
					($7 & OBJ_FLAGS) | VIAFLAG);
				SaveFree($6);
			}
		;

layer
			/* name */
		: T_LAYER '(' NUMBER STRING ')' '('
			{
				if ($3 <= 0 || $3 > MAX_LAYER)
				{
					yyerror("Layernumber out of range");
					YYABORT;
				}
				if (LayerFlag[$3-1])
				{
					yyerror("Layernumber used twice");
					YYABORT;
				}
				Layer = &yyData->Layer[$3-1];

					/* memory for name is alread allocated */
				Layer->Name = $4;
				LayerFlag[$3-1] = True;
			}
		  layerdata ')'
		;

layerdata
		: layerdefinitions
		|
		;

layerdefinitions
		: layerdefinition
		| layerdefinitions layerdefinition
		;

layerdefinition
			/* x1, y1, x2, y2, thickness, flags */
		: T_LINE '(' NUMBER NUMBER NUMBER NUMBER NUMBER NUMBER ')'
			{
				CreateNewLineOnLayer(Layer, $3, $4, $5, $6, $7, $8 & OBJ_FLAGS);
			}
			/* x1, y1, x2, y2, flags */
		| T_RECTANGLE '(' NUMBER NUMBER NUMBER NUMBER NUMBER ')'
			{
				CreateNewPolygonFromRectangle(Layer,
					$3, $4, $3+$5, $4+$6, $7 & OBJ_FLAGS);
			}
		| text_newformat
		| text_oldformat
			/* flags are passed in */
		| T_POLYGON '(' NUMBER ')' '('
			{
				Polygon = CreateNewPolygon(Layer, $3 & OBJ_FLAGS);
			}
		  polygonpoints ')'
		  	{
					/* ignore junk */
				if (Polygon->PointN >= 3)
					SetPolygonBoundingBox(Polygon);
				else
				{
					Message("WARNING parsing file '%s'\n"
						"    line:        %i\n"
						"    description: 'ignored polygon (< 3 points)'\n",
						yyfilename, yylineno);
					DestroyObject(POLYGON_TYPE, Layer, Polygon, Polygon);
				}
			}
		;

text_oldformat
			/* x, y, direction, text, flags */
		: T_TEXT '(' NUMBER NUMBER NUMBER STRING NUMBER ')'
			{
					/* use a default scale of 100% */
				CreateNewText(Layer,yyFont,$3, $4, $5, 100, $6, $7 & OBJ_FLAGS);
				SaveFree($6);
			}
		;

text_newformat
			/* x, y, direction, scale, text, flags */
		: T_TEXT '(' NUMBER NUMBER NUMBER NUMBER STRING NUMBER ')'
			{
				CreateNewText(Layer,yyFont, $3, $4, $5, $6, $7, $8 & OBJ_FLAGS);
				SaveFree($7);
			}
		;

polygonpoints
		: polygonpoint
		| polygonpoints polygonpoint
		;

polygonpoint
			/* xcoord ycoord */
		: '(' NUMBER NUMBER ')'
			{
				CreateNewPointInPolygon(Polygon, $2, $3);
			}
		;

element
		: element_oldformat
		| element_1.3.4_format
		| element_newformat
		;

element_oldformat
			/* element_flags, description, pcb-name,
			 * text_x, text_y, text_direction, text_scale, text_flags
			 */
		: T_ELEMENT '(' STRING STRING NUMBER NUMBER NUMBER ')' '('
			{
				yyElement = CreateNewElement(yyData, yyElement, yyFont, 0x0000,
					$3, $4, NULL, $5, $6, $7, 100, 0x0000);
				SaveFree($3);
				SaveFree($4);
			}
		  elementdefinitions ')'
			{
				SetElementBoundingBox(yyElement);
			}
		;

element_1.3.4_format
			/* element_flags, description, pcb-name,
			 * text_x, text_y, text_direction, text_scale, text_flags
			 */
		: T_ELEMENT '(' NUMBER STRING STRING NUMBER NUMBER NUMBER NUMBER NUMBER ')' '('
			{
				yyElement = CreateNewElement(yyData, yyElement, yyFont, $3,
					$4, $5, NULL, $6, $7, $8, $9, $10);
				SaveFree($4);
				SaveFree($5);
			}
		  elementdefinitions ')'
			{
				SetElementBoundingBox(yyElement);
			}
		;

element_newformat
			/* element_flags, description, pcb-name, value, 
			 * text_x, text_y, text_direction, text_scale, text_flags
			 */
		: T_ELEMENT '(' NUMBER STRING STRING STRING NUMBER NUMBER NUMBER NUMBER NUMBER ')' '('
			{
				yyElement = CreateNewElement(yyData, yyElement, yyFont, $3,
					$4, $5, $6, $7, $8, $9, $10, $11);
				SaveFree($4);
				SaveFree($5);
				SaveFree($6);
			}
		  elementdefinitions ')'
			{
				SetElementBoundingBox(yyElement);
			}
		;

elementdefinitions
		: elementdefinition
		| elementdefinitions elementdefinition
		;

elementdefinition
		: pin_newformat
		| pin_oldformat
		| pad
			/* x1, y1, x2, y2, thickness */
		| T_ELEMENTLINE '(' NUMBER NUMBER NUMBER NUMBER NUMBER ')'
			{
				CreateNewLineInElement(yyElement, $3, $4, $5, $6, $7);
			}
			/* x, y, width, height, startangle, anglediff, thickness */
		| T_ELEMENTARC '(' NUMBER NUMBER NUMBER NUMBER NUMBER NUMBER NUMBER ')'
			{
				CreateNewArcInElement(yyElement, $3, $4, $5, $6, $7, $8, $9);
			}
			/* x, y position */
		| T_MARK '(' NUMBER NUMBER ')'
			{
				yyElement->MarkX = $3;
				yyElement->MarkY = $4;
			}
		;

pin_newformat
			/* x, y, thickness, drilling hole, name, flags */
		: T_PIN '(' NUMBER NUMBER NUMBER NUMBER STRING NUMBER ')'
			{
				CreateNewPin(yyElement, $3, $4, $5, $6, $7,
					($8 & OBJ_FLAGS) | PINFLAG);
				SaveFree($7);
			}
		;

pin_oldformat
			/* old format: x, y, thickness, name, flags
			 * drilling hole is 40% of the diameter
			 */
		: T_PIN '(' NUMBER NUMBER NUMBER STRING NUMBER ')'
			{
				Dimension	hole = ($5 *DEFAULT_DRILLINGHOLE)/100;

					/* make sure that there's enough copper left */
				if ($5 -hole < MIN_PINORVIACOPPER && 
					$5 > MIN_PINORVIACOPPER)
					hole = $5 -MIN_PINORVIACOPPER;

				CreateNewPin(yyElement, $3, $4, $5, hole, $6,
					($7 & OBJ_FLAGS) | PINFLAG);
				SaveFree($6);
			}
		;

pad
			/* x1, y1, x2, y2, thickness, name and flags */
		: T_PAD '(' NUMBER NUMBER NUMBER NUMBER NUMBER STRING NUMBER ')'
			{
				CreateNewPad(yyElement,$3,$4,$5,$6,$7,$8, ($9 & OBJ_FLAGS));
			}
		;

symbols
		: symbol
		| symbols symbol
		;

symbol
		: T_SYMBOL '(' symbolid NUMBER ')' '('
			{
				if ($3 <= 0 || $3 > MAX_FONTPOSITION)
				{
					yyerror("fontposition out of range");
					YYABORT;
				}
				Symbol = &yyFont->Symbol[$3];
				if (Symbol->Valid)
				{
					yyerror("symbol ID used twice");
					YYABORT;
				}
				Symbol->Valid = True;
				Symbol->Delta = $4;
			}
		  symboldata ')'
		;

symbolid
		: NUMBER
		| CHAR_CONST
		;

symboldata
		: symboldefinitions
		|
		;

symboldefinitions
		: symboldefinition
		| symboldefinitions symboldefinition
		;

symboldefinition
			/* x1, y1, x2, y2, thickness */
		: T_SYMBOLLINE '(' NUMBER NUMBER NUMBER NUMBER NUMBER ')'
			{
				CreateNewLineInSymbol(Symbol, $3, $4, $5, $6, $7);
			}
		;

%%

/* ---------------------------------------------------------------------------
 * error routine called by parser library
 */
int yyerror(s)
char *s;
{
	Message("ERROR parsing file '%s'\n"
		"    line:        %i\n"
		"    description: '%s'\n",
		yyfilename, yylineno, s);
	return(0);
}

