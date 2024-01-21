/*********************************************************************************/
/* This file is  part of Xcolorsel, a multipurpose viewer for X11 rgb.txt files. */
/*                     Copyright (C) 1993, 1994 Michael Weller                   */
/*                                                                               */
/* This program is free software; you can redistribute it and/or modify it under */
/* the terms of the GNU General Public License as published be the Free Software */
/* Foundation;  either version 2 of the License,  or (at your opinion) any later */
/* version.                                                                      */
/*                                                                               */
/* This program is  distributed in the hope  that it will be useful, but WITHOUT */
/* ANY WARRANTY; without even the  implied warranty of  MERCHANBILITY or FITNESS */
/* FOR  A  PARTICULAR  PURPOSE.   See the  GNU General  Public License  for more */
/* details.                                                                      */
/*                                                                               */
/* You should have received a copy  of the GNU General Public License along with */
/* this program; if not,  write to the  Free Software Foundation, Inc., 675 Mass */
/* Ave, Cambridge, MA 02139, USA.                                                */
/*                                                                               */
/* The author can be  reached by means of email  as eowmob@exp-math.uni-essen.de */
/* or  eowmob@pollux.exp-math.uni-essen.de   or   mat42b@vm.hrz.uni-essen.de  or */
/* mat42b@de0hrz1a.bitnet.  Or as:  Michael Weller,  Heiderhoefen 116b,  D 46049 */
/* Oberhausen, Germany.                                                          */
/*                                                                               */
/* Part of the files are derived  from the Template Widget of the Athena Widgets */
/* as published  by the  Massachusetts Institute of  Technology.  Actually these */
/* files do not contain any code  by the M.I.T  but only  variable declarations. */
/* Nevertheless these parts of these files are still distributed under the terms */
/* of the M.I.T copyright which are here repeated:                               */
/*                                                                               */
/* Copyright    Massachusetts Institute of Technology   1987, 1988               */
/*                                                                               */
/* Permission to use, copy,  modify, distribute,  and sell this software and its */
/* documentation for  any purpose is hereby  granted without fee,  provided that */
/* the  above  copyright  notice  appear  in  all  copies  and  that  both  that */
/* copyright   notice   and   this  permission   notice  appear  in   supporting */
/* documentation,  and that the  name of M.I.T.  not  be used  in advertising or */
/* publicity  pertaining  to distribution  of  the  software  without  specific, */
/* written  prior  permission.    M.I.T.  makes  no  representations  about  the */
/* suitability  of  this  software  for  any  purpose.   It is provided  "as is" */
/* without express or implied warranty.                                          */
/*                                                                               */
/* M.I.T. DISCLAIMS ALL  WARRANTIES WITH REGARD  TO THIS SOFTWARE, INCLUDING ALL */
/* IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS,  IN NO EVENT SHALL M.I.T. */
/* BE LIABLE FOR ANY SPECIAL,  INDIRECT OR CONSEQUENTIAL  DAMAGES OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE,  DATA OR PROFITS, WHETHER IN AN ACTION */
/* OF CONTRACT,  NEGLIGENCE  OR  OTHER  TORTIOUS  ACTION,  ARISING OUT OF  OR IN */
/* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                      */
/*********************************************************************************/

/*****************************************************************************************
* This defines a RgbText Widget. This is a subclass of an AsciiTextWidget. It displays *
* the contents of an rgb.txt in a standard read-only AsciiTextWidget together with       *
* little tiles of color probes. Since it needs to allocate ReadWrite Colorcells at the   *
* Xserver often, this is done at initialization time. So a maximum number of displayed   *
* lines is needed. This is written in the max resource for correct interaction with      *
* constraint widgets.                                                                    *
* If the maximum Row number is exceeded, incorrect color values will be displayed.       *
*                                                                                        *
*****************************************************************************************/

/* This was inspirated from: */

/* $XConsortium: Template.h,v 1.5 90/12/19 18:46:00 converse Exp $ */

/* Copyright    Massachusetts Institute of Technology   1987, 1988
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */


/* Additional Resources compared to the AsciiTextWidget are:

Name		Class		RepType		Default
----		-----		-------		-------

maxRows		MaxRows		Dimension	30	This is the number of writeable
							colorcells this widget allocates!
							Use with care!
rows		Rows		Dimension	10	This is converted to pixels and
							returned as the preferred
							height of this widget
columns		Columns		Dimension	30	Same as above but for columns.		
file		File		String		"/usr/lib/X11/rgb.txt" The file to display
tileWidth	TileWidth	Dimension	3	The width of the displayed colortiles
							measured in space characters
grabCursor	Cursor		Cursor		None	The cursor to use for grabbing of
							colors.
maxRed		MaxRed		Short		0	|The Maximum of these values and
maxGreen	MaxGreen	Short		0	|the values in file is scaled to the
maxBlue		MaxBlue		Short		0	|highest R/G/B-value on the screen (65535).
maxIntensity	MaxIntensity	Short		255	Same as above, but applies to all R/G/B values.
							Note that since scaling from zero is impossible
							this value has to be at least 1.
breakPercentage	BreakPercentage Short		9500	The maximum percentage a color may differ
							from the selected color to be still found.
displayFormat	DisplayFormat	String		input	The format in which the colors are printed.
							Allowed values are:
							rgb4, rgb8, rgb12, rgb16, rgb, rgbi, ciexyz,
							cieuvy, ciexyy, cielab, cieluv,
							tekhvc, #rgb, #rrggbb, #rrrgggbbb, #rrrrggggbbbb,
							input (These are the dezimal values out of file).
							(The value is not case sensitiv. However it is
							return be getvalue in same case as it was set)
realized	Realized	Boolean		True	Show values the Xserver can actual display
							instead of the theoretic values.
							Has no effect for displayFormat: input
comma		Comma		Boolean		False	Colorvalues are seperated by ", " instead of /
							Has no effect for displayFormat: #rgb,...
							Also C-style numberformats are used.

string, type, editType are forced to the needed values.

No resizes are made when rows or columns is changed!
*/


#ifndef _RgbText_h
#define _RgbText_h

#include <X11/Xaw/AsciiText.h>

#define XtNmaxRows	"maxRows"
#define XtNrows		"rows"
#define XtNcolumns	"columns"
#define XtNtileWidth	"tileWidth"
#define XtNmaxRed	"maxRed"
#define XtNmaxGreen	"maxGreen"
#define XtNmaxBlue	"maxBlue"
#define XtNmaxIntensity	"maxIntensity"
#define XtNbreakPercentage	"breakPercentage"
#define XtNgrabCursor	"grabCursor"
#define XtNdisplayFormat "displayFormat"
#define XtNrealized	"realized"
#define	XtNcomma	"comma"

#define XtCMaxRows	"MaxRows"
#define XtCRows		"Rows"
#define XtCColumns	"Columns"
#define XtCTileWidth	"TileWidth"
#define XtCMaxRed	"MaxRed"
#define XtCMaxGreen	"MaxGreen"
#define XtCMaxBlue	"MaxBlue"
#define XtCMaxIntensity	"MaxIntensity"
#define XtCBreakPercentage	"BreakPercentage"
#define XtCDisplayFormat "DisplayFormat"
#define XtCRealized	"Realized"
#define XtCComma	"Comma"

/* All Values in Pixels! */

typedef struct	{
		Cardinal	offset;		/* Number of selected hit (1..numhits),
						   0 if no hit exists. */
						/* (One is best hit) */
		Cardinal	numhits;	/* number of all hits */
		Cardinal	numexact;	/* number of exact hits */
		Dimension	percentage;	/* Exactness of hit in percents*100
						   (for example 4235 = 42.35% )	*/
		String 		repres;		/* Pointer to READ-ONLY! ascii repres of
						   searched color */
		}	RgbTextAnswer;
typedef void (* RgbTextAnswerProc)(Widget w, XColor *color, RgbTextAnswer *answer);

extern String GetRgbFile(Widget w); /* get the pointer to the name of the file that was read in
					(read only) valid until widget is destroyed, NULL is
					returned if none is defined */
extern Cardinal GetNumEntries(Widget w); /* get the number of lines read in */
extern Cardinal GetNumDifferent(Widget w); /* number of different colors defined */
extern Cardinal GetNumRealisable(Widget w); /* get the number of different colors when realised
					       on the screen */
extern Dimension GetMaxPixels(Widget w);	/* get the maximum height allowed in pixels. */
extern String GetColorName(Widget w); /* Gets the name of the Color in the
					 line in which the selection starts,
					 or if there is no selection in which
					 the cursor (caret) is positioned.
					 If there is no caret shown, NULL is
					 returned. The pointer points to read only
					 data!!! It is valid until a new rgb-file via
					 set-value file is set or the widget is deleted. */
extern int GetColorSpec(Widget w, XColor *color);
					 /* Same as above, but returns the color itself.
					 The index value is set to 0; 
					 flags to DoRed|DoGreen|DoBlue;
					 If 0 is returned, there is no color
					 selected and black was returned. */
extern void FindColor(Widget w, XColor *color, RgbTextAnswer *answer);
					/* if color == NULL, the offset-th hit is found. If
					   color != NULL then the given color is searched.
					   In both cases all fields of answer are set to the
					   actual values. */
extern void GrabColor(Widget w, RgbTextAnswerProc answer_proc);
					/* same as FindColor, but a pointer grab is done, and
					   the user can select any point on the screen, which is
					   then given to FindColor. FindColor's answer is then
					   returned by calling answer_proc(w,color,answer).
					   The grabbed color is copied to color (with flags set
					   to DoRed|DoBlue|DoGreen). answer and color are returned
					   in read-only buffers, that are changed in the next
					   call to GrabColor on this widget.
					   These buffers remain valid and are the same until
					   the rgbTextWidget is destroyed.
					   */
	

/* Miscanellous */

typedef struct _RgbTextClassRec *	RgbTextWidgetClass;
typedef struct _RgbTextRec *     	RgbTextWidget;

extern WidgetClass rgbTextWidgetClass;
 
#endif /* _RgbText_h */
