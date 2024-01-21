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
 *  RCS: $Id: global.h,v 143.1 1996/09/16 09:08:39 nau Exp $
 */

/* definition of types
 */

#ifndef	__GLOBAL_INCLUDED__
#define	__GLOBAL_INCLUDED__

#include "const.h"
#include "macro.h"

#include <stdio.h>
#include <X11/Xmd.h>
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>

/* ---------------------------------------------------------------------------
 * do not change the following definition even if it's not very nice.
 * It allows us too cast PadTypePtr to LineTypePtr and access the pads
 * coordinates as a line. The background is to have only one set of funtions
 * for searching connections
 */
#define	LINESTRUCT				\
	int				ID,			\
					Flags;		\
	PointType		Point1,		\
					Point2;		\
	Dimension		Thickness;

/* ---------------------------------------------------------------------------
 * some useful values of our widgets
 */
typedef struct						/* holds information about output window */
{
	Widget			Toplevel,		/* toplevel widget */
					StatusLine,		/* label widget of statusline */
					CursorPosition,	/* current cursor position */
					ScrollbarLeft,	/* viewport scrollbars */
					ScrollbarBottom,
					Control,		/* control widget */
					Menu,			/* popup menue */
					Message,		/* fields for user message */
					MasterForm,		/* the widgets thats hols all the others */
					Viewport,		/* viewport widget to scoll output */
					Output;			/* simple widget for drawing */
	Window			OutputWindow;	/* window ID of simple widget */
	GC				bgGC,			/* background and foreground; */
					fgGC,			/* changed from some routines */
					GridGC;			/* for the grid */
	Position		OffsetX,		/* origin offset */
					OffsetY;
	Dimension		Width,			/* sizes of output window */
					Height;
	Cursor			XCursor;		/* used X cursor */
	unsigned int	XCursorShape;	/* and its shape (cursorfont) */
	Boolean			VisibilityOK;	/* output is completely visible */
} OutputType, *OutputTypePtr;

/* ----------------------------------------------------------------------
 * layer group. A layer group identifies layers which are always switched
 * on/off together.
 */
typedef struct
{
	Cardinal		Number[MAX_LAYER+2],	/* number of entries per groups */
					Entries[MAX_LAYER+2][MAX_LAYER+2];
} LayerGroupType, *LayerGroupTypePtr;

typedef struct						/* a bounding box */
{
	Position		X1, Y1,			/* upper left */
					X2, Y2;			/* and lower right corner */
} BoxType, *BoxTypePtr;

/* ---------------------------------------------------------------------------
 * the basic type supported by PCB
 * there are only four combination of coordinates supported:
 * line straight up, right and up, straight right and down and right
 * check 'draw.c' and 'create.c' for details
 */
typedef struct						/* all objects start with an ID */
{
	int				ID;
} AnyObjectType, *AnyObjectTypePtr;

typedef struct						/* a line/polygon point */
{
	int				ID;
	Position		X,
					Y;
} PointType, *PointTypePtr;

typedef struct						/* holds information about one line */
{
	LINESTRUCT
} LineType, *LineTypePtr;

typedef struct
{
	int				ID,
					Flags;
	Position		X,				/* origin */
					Y;
	BoxType			BoundingBox;
	BYTE			Direction;
	Dimension		Scale;			/* text scaling in percent */
	char			*TextString;	/* string */
} TextType, *TextTypePtr;

typedef struct						/* holds information about a polygon */
{
	int				ID,
					Flags;
	BoxType			BoundingBox;
	Cardinal		PointN,			/* number of points in polygon */
					PointMax;		/* max number from malloc() */
	PointTypePtr	Points;			/* data */
} PolygonType, *PolygonTypePtr;

typedef struct						/* holds information about one layer */
{
	char			*Name;			/* layer name */
	Cardinal		LineN,			/* number of lines */
					TextN,			/* labels */
					PolygonN,		/* and polygons */
					LineMax,		/* max number from malloc() */
					TextMax,
					PolygonMax;
	LineTypePtr		Line;			/* pointer to additional structures */
	TextTypePtr		Text;
	PolygonTypePtr	Polygon;
	Boolean			On;				/* visible flag */
	Pixel			Color,			/* color */
					SelectedColor;
} LayerType, *LayerTypePtr;

typedef struct						/* a SMD pad */
{
	LINESTRUCT
	char			*Name;			/* 'Line' */
} PadType, *PadTypePtr;

typedef struct
{
	int				ID,
					Flags;
	Position		X,				/* center and diameter */
					Y;
	Dimension		Thickness,
					DrillingHole;
	char			*Name;				
} PinType, *PinTypePtr;

typedef struct						/* used for arcs */
{
	Position		X,				/* center coordinates */
					Y;
	Dimension		Width,			/* length of axis */
					Height,
					Thickness;
	int				StartAngle,		/* the two limiting angles in degrees */
					Delta;	
} ArcType, *ArcTypePtr;

typedef struct
{
	int				ID,
					Flags;
	TextType		Name[MAX_ELEMENTNAMES];	/* the elements names; */
											/* description text */
											/* name on PCB second, */
											/* value third */
											/* see macro.h */
	Position		MarkX,					/* position mark */
					MarkY;
	BoxType			BoundingBox;
	Cardinal		PinN,		/* number of pins, lines and arcs */
					PinMax,
					PadN,
					PadMax,
					LineN,
					LineMax,
					ArcN,
					ArcMax;
	PinTypePtr		Pin;		/* pin description */
	PadTypePtr		Pad;		/* pad description of SMD components */
	LineTypePtr		Line;
	ArcTypePtr		Arc;
} ElementType, *ElementTypePtr;

/* ---------------------------------------------------------------------------
 * symbol and font related stuff
 */
typedef struct						/* a single symbol */
{
	LineTypePtr		Line;
	Boolean			Valid;
	Cardinal		LineN,			/* number of lines */
					LineMax;
	Dimension		Width,			/* size of cell */
					Height,
					Delta;			/* distance to next symbol in 0.001'' */
} SymbolType, *SymbolTypePtr;

typedef struct						/* complete set of symbols */
{
	Dimension		MaxHeight,		/* maximum cell width and height */
					MaxWidth;
	BoxType			DefaultSymbol;	/* the default symbol is a filled box */
	SymbolType		Symbol[MAX_FONTPOSITION+1];
	Boolean			Valid;
} FontType, *FontTypePtr;

typedef struct				/* holds all objects */
{
	Cardinal		ViaN,			/* number of vias */
					ViaMax,			/* max number from malloc() */
					ElementN,		/* and elements */
					ElementMax;		/* max number from malloc() */
	PinTypePtr		Via;			/* pointer to object data */
	ElementTypePtr	Element;
	LayerType		Layer[MAX_LAYER];
} DataType, *DataTypePtr;

typedef struct				/* holds information about board layout */
{							/* most of the information is saved with layout */
							/* and initialized from resources when */
							/* a new board design is started */
							/* this struct is also used for the remove list */
							/* and for buffer handling */
	int				ID,				/* see macro.h */
					Flags;
	char			*Name,			/* name of board */
					*Filename,		/* name of file (from load) */
					*PrintFilename;	/* from print dialog */
	Boolean			Changed,		/* layout has been changed */
					ViaOn,			/* visibility flags */
					ElementOn,
					InvisibleObjectsOn,
					PinOn;
	Pixel			ViaColor,		/* some colors */
					ViaSelectedColor,
					PinColor,
					PinSelectedColor,
					ElementColor,
					InvisibleObjectsColor,
					ElementSelectedColor,
					ConnectedColor,
					GroundplaneColor;
	int				Zoom,			/* zoom factor (factor = 1 << zoom) */
					CursorX,		/* cursor position as saved with layout */
					CursorY,
					Grid,			/* used grid with offsets */
					GridOffsetX,	/* as saves with layout */
					GridOffsetY;
	Dimension		MaxWidth,		/* allowed size */
					MaxHeight;
	FontType		Font;
	LayerGroupType	LayerGroups;
	Pixmap			SaveUnder;		/* saves data during dialog handling */
	DataTypePtr		Data;
} PCBType, *PCBTypePtr;

typedef struct						/* information about the paste buffer */
{
	Position		X,				/* offset */
					Y;
	BoxType			BoundingBox;
	DataTypePtr		Data;			/* data; not all members of PCBType */
									/* are used */
} BufferType, *BufferTypePtr;

/* ---------------------------------------------------------------------------
 * some types for cursor drawing, setting of block and lines
 * as well as for merging of elements
 */
typedef struct						/* rubberband lines for element moves */
{
	LayerTypePtr	Layer;			/* layer that holds the line */
	LineTypePtr		Line;			/* the line itself */
	PointTypePtr	MovedPoint;		/* and finally the point */
} RubberbandType, *RubberbandTypePtr;

typedef struct						/* current marked line */
{
	PointType		Point1,			/* start- and end-position */
					Point2;
	int				State;
} AttachedLineType, *AttachedLineTypePtr;

typedef struct						/* currently marked block */
{
	PointType		Point1,			/* start- and end-position */
					Point2;
	int				State;
} AttachedBoxType, *AttachedBoxTypePtr;

typedef struct						/* currently attached object */
{
	Position			X,			/* saved position when MOVE_MODE */
						Y;			/* was entered */
	BoxType				BoundingBox;
	int					Type,		/* object type */
						State;
	void				*Ptr1,		/* three pointers to data, see */
						*Ptr2,		/* search.c */
						*Ptr3;
	Cardinal			RubberbandN,/* number of lines in array */
						RubberbandMax;
	RubberbandTypePtr	Rubberband;
} AttachedObjectType, *AttachedObjectTypePtr;

typedef struct						/* holds cursor information */
{
	GC					GC,			/* GC for cursor drawing */
						AttachGC;	/* and for displaying buffer contents */
	Position			X,			/* position in PCB coordinates */
						Y,
						MinX,		/* lowest and highest coordinates */
						MinY,
						MaxX,
						MaxY;
	Boolean				On;				/* flag for 'is visible' */
	AttachedLineType	AttachedLine;	/* data of new lines... */
	AttachedBoxType		AttachedBox;
	PolygonType			AttachedPolygon;
	AttachedObjectType	AttachedObject;	/* data of attached objects */
} CrosshairType, *CrosshairTypePtr;

/* ---------------------------------------------------------------------------
 * our resources
 * most of them is used as default when a new design is started
 */
typedef struct						/* some resources... */
{
	Pixel			bgColor,		/* background and cursor color ... */
					CrosshairColor,	/* different object colors */
					ViaColor,
					ViaSelectedColor,
					PinColor,
					PinSelectedColor,
					ElementColor,
					InvisibleObjectsColor,
					ElementSelectedColor,
					ConnectedColor,
					OffLimitColor,
					GridColor,
					LayerColor[MAX_LAYER],
					LayerSelectedColor[MAX_LAYER],
					GroundplaneColor;
	Dimension		ViaThickness,	/* some preset values */
					ViaDrillingHole,
					LineThickness,
					MaxWidth,		/* maximum size of a layout */
					MaxHeight,
					TextScale,		/* text scaling in % */
					AlignmentDistance;
	int				Grid,			/* grid 0.001'' */
					Zoom,			/* number of shift operations for zooming */
					PinoutZoom,		/* same for pinout windows */
					PinoutNameLength,	/* max displayed length of a pinname */
					Volume,			/* the speakers volume -100..100 */
					CharPerLine,	/* width of an output line in characters */
					Mode,			/* currently active mode */
					BufferNumber;	/* number of the current buffer */
	long			BackupInterval;	/* time between two backups in seconds */
	String			DefaultLayerName[MAX_LAYER],
					FontCommand,	/* commands for file loading... */
					FileCommand,
					ElementCommand,
					PrintFile,
					LibraryCommand,
					LibraryContentsCommand,
					SaveCommand,
					LibraryFilename,
					FontFile,		/* name of default font file */
					Groups,			/* string with layergroups */
					FilePath,		/* colon seperated search paths */
					FontPath,
					ElementPath,
					LibraryPath,
					Size,			/* geometry string for size */
					Media;			/* type of output media */
	Position		PinoutOffsetX,	/* offset of origin */
					PinoutOffsetY,
					PinoutTextOffsetX,	/* offset of text from pin center */
					PinoutTextOffsetY;
	LayerGroupType	LayerGroups;	/* default layer groups */
	Boolean			AbsoluteGrid,	/* grid is relative to (0,0) */
					UseLogWindow,	/* use log window instead of dialog box */
					RaiseLogWindow,	/* raise log window if iconified */
					ShowSolderSide,	/* mirror output */
					SaveLastCommand,/* save the last command entered by user */
					SaveInTMP,				/* always save data in /tmp */
					DrawGrid,				/* draw grid points */
					AllDirectionLines,		/* enable lines to all directions */
					ResetAfterElement,		/* reset connections after */
											/* each element */
					RingBellWhenFinished;	/* flag if a signal should be */
											/* produced when searching of */
											/* connections is done */
	XFontStruct		*PinoutFont[MAX_ZOOM+1];/* font ID used for pin names */
} SettingType, *SettingTypePtr;

/* ----------------------------------------------------------------------
 * pointer to low-level copy, move and rotate functions
 */
typedef struct
{
	void	*(*Line)(LayerTypePtr, LineTypePtr);
	void	*(*Text)(LayerTypePtr, TextTypePtr);
	void	*(*Polygon)(LayerTypePtr, PolygonTypePtr);
	void	*(*Via)(PinTypePtr);
	void	*(*Element)(ElementTypePtr);
	void	*(*ElementName)(ElementTypePtr);
	void	*(*Pin)(ElementTypePtr, PinTypePtr);
	void	*(*Pad)(ElementTypePtr, PadTypePtr);
	void	*(*LinePoint)(LayerTypePtr, LineTypePtr, PointTypePtr);
	void	*(*Point)(LayerTypePtr, PolygonTypePtr, PointTypePtr);
} ObjectFunctionType, *ObjectFunctionTypePtr;

/* ---------------------------------------------------------------------------
 * structure used by device drivers
 */
typedef struct					/* media description */
{
	String		Name;			/* name and size (in mil) */
	Dimension	Width,
				Height;
	Position	MarginX,
				MarginY;
} MediaType, *MediaTypePtr;

typedef struct						/* needs and abilities of a driver */
{
	MediaTypePtr	SelectedMedia;	/* media to be used */
	FILE			*FP;			/* file pointers */
	Boolean			MirrorFlag,		/* several flags */
					RotateFlag,
					InvertFlag;
	Position		OffsetX,		/* offset from lower/left corner */
					OffsetY;
	float			Scale;			/* scaleing */
	BoxType			BoundingBox;	/* bounding box of output */
} PrintInitType, *PrintInitTypePtr;

typedef struct						/* functions of a driver */
{
	char		*Name,								/* name of driver */
				*Suffix;							/* filename suffix */
	char		*(*Init)(PrintInitTypePtr, char *);	/* initializes driver */
	void		(*Exit)(void);						/* exit code */
	void		(*SetColor)(XColor);				/* set color */
	void		(*Drill)(PinTypePtr);				/* drilling information */
	void		(*Layer)(LayerTypePtr, int);		/* printing functions */
	void		(*ElementPackage)(ElementTypePtr);
	void		(*Pad)(PadTypePtr);
	void		(*PinOrVia)(PinTypePtr);
	void		(*PadMask)(PadTypePtr);
	void		(*PinOrViaMask)(PinTypePtr);
	void		(*ClearPinOrViaOnGroundplane)(PinTypePtr);
	void		(*FilledRectangle)(Position, Position, Position, Position);
	void		(*Outline)(Position, Position, Position, Position);
	void		(*Alignment)(Position, Position, Position, Position);
	void		(*DrillHelper)(PinTypePtr);
	Boolean		HandlesColor,				/* colored output */
				HandlesInvertion,			/* inverted output */
				HandlesGroundplane,			/* able to create ground plane */
				HandlesDrill,				/* able to produce drill info */
				HandlesMask,				/* able to produce mask layers */
				HandlesMedia;				/* able to handle different media */
} PrintDeviceType, *PrintDeviceTypePtr;

typedef struct
{
	PrintDeviceTypePtr	(*Query)(void);		/* query function */
	PrintDeviceTypePtr	Info;				/* data returned by Query() */
} DeviceInfoType, DeviceInfoTypePtr;

/* ---------------------------------------------------------------------------
 * structure used by library routines
 */
typedef struct
{
	char	*ListEntry;			/* the string for the selection box */
	char	*AllocatedMemory,	/* pointer to allocated memory; all others */
								/* point to parts of the string */
			*Template,			/* m4 template name */
			*Package,			/* package */
			*Value,				/* the value field */
			*Description;		/* some descritional text */
} LibraryEntryType, *LibraryEntryTypePtr;

typedef struct
{
	char				*Name;		/* name of the menue entry */
	Cardinal			EntryN,		/* number of objects */
						EntryMax;	/* number of reserved memory locations */
	LibraryEntryTypePtr	Entry;		/* the entries */
} LibraryMenuType, *LibraryMenuTypePtr;

typedef struct
{
	Cardinal			MenuN,
						MenuMax;
	LibraryMenuTypePtr	Menu;
} LibraryType, *LibraryTypePtr;

#endif
