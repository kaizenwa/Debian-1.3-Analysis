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

/******************************************************************************************
* This defines an RgbSrc Object. This is a subclass of an AsciiSrc. It is designed to     *
* to be used by an RgbText Object. The only difference to an AsciiSrc is a special scan   *
* function.                                                                               *
******************************************************************************************/

/* This was inspirated from: */


/* $XConsortium: Template.c,v 1.4 91/02/17 16:18:42 converse Exp $ */

/* Copyright	Massachusetts Institute of Technology	1987, 1988
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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "RgbSrcP.h"

/* No resources */

/* No actions */

/* Both are in RgbText! */

/* No actions no translations */

static void initialize(Widget request, Widget new, ArgList args, Cardinal *numargs);
static XawTextPosition myscan(Widget w, XawTextPosition position, XawTextScanType type,
		XawTextScanDirection dir, int count, Boolean include);

RgbSrcClassRec rgbSrcClassRec = {
  { /* object fields */
    /* ======        */ 
    /* superclass		*/	(WidgetClass) &asciiSrcClassRec,
    /* class_name		*/	"RgbSrc",
    /* widget_size		*/	sizeof(RgbSrcRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	NULL,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	NULL,
    /* num_resources		*/	0,
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	0,
    /* compress_exposure	*/	0,
    /* compress_enterleave	*/	0,
    /* visible_interest		*/	0,
    /* destroy			*/	NULL,
    /* resize			*/	NULL,
    /* expose			*/	NULL,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	NULL,
    /* display_accelerator	*/	NULL,
    /* extension		*/	NULL
  },
  { /* text_src fields */
    /* Read			*/	XtInheritRead,
    /* Replace			*/	XtInheritReplace,
    /* Scan			*/	(_XawTextPositionFunc)myscan,
    /* Search			*/	XtInheritSearch,
    /* SetSelection		*/	XtInheritSetSelection,
    /* ConvertSelection		*/	XtInheritConvertSelection,
  },
  { /* ascii_src fields */
    /* empty			*/	0
  },
  { /* rgb_src fields */
    /* empty			*/	0
  }
};

WidgetClass rgbSrcObjectClass = (WidgetClass)&rgbSrcClassRec;

#define rgbparent(widget)	(*(RgbTextRec *)XtParent(widget))
#define myself(widget)		(*(RgbSrcRec *)(widget))

static void initialize(Widget request, Widget new, ArgList args, Cardinal *numargs)
{
XtCheckSubclass(XtParent(new),rgbTextWidgetClass,
"rgbSrc: Didn't I say that my parent has to be a subclass of RgbText?\n");
}

static XawTextPosition myscan(Widget w, XawTextPosition position, XawTextScanType type,
		XawTextScanDirection dir, int count, Boolean include)
{
char *ptr,*start,*end;
XawTextPosition offset;
if(type!=XawstWhiteSpace)
	return (*(((AsciiSrcClassRec *)(rgbSrcClassRec.object_class.superclass))->text_src_class.Scan))
		(w, position, type, dir, count, include); 
start=rgbparent(w).rgb_text.asciirep;
while(count--)
	{
	/* find position in line: */
	ptr=start+position;
	while(ptr>start)
		{
		if(*(ptr-1)=='\n')break;
		ptr--;
		}
	offset=position-(ptr-start);
	if(dir==XawsdLeft)
		{
		if(offset<rgbparent(w).rgb_text.startofrgb)
			position=0;
		else if(offset<=rgbparent(w).rgb_text.endofrgb)
			{
			position=rgbparent(w).rgb_text.startofrgb;
			if(include==True)
				position=0;
			}
		else if(offset<=rgbparent(w).rgb_text.endofrgb+3)
			{
			position=rgbparent(w).rgb_text.endofrgb;
			position=0;
			}
		else 	{
			position=rgbparent(w).rgb_text.endofrgb+3;
			if(include==True)
				position=rgbparent(w).rgb_text.endofrgb;
			}
		}
	else	{
		end=strchr(ptr,'\n');
		if(!end)end=strchr(ptr,0);
		if(offset>=rgbparent(w).rgb_text.endofrgb+3)
			{
			position=(end-ptr);
			if(include==True)
				position++;
			}
		else if(offset>=rgbparent(w).rgb_text.endofrgb)
			{
			position=rgbparent(w).rgb_text.endofrgb+3;
			position=0;
			}
		else if(offset>=rgbparent(w).rgb_text.startofrgb)
			{
			position=rgbparent(w).rgb_text.endofrgb;
			if(include==True)
				position=rgbparent(w).rgb_text.endofrgb+3;
			}
		else	position=0;
		}
	position=((ptr+position)-start);
	}
/*printf("%lu\n",(unsigned long)position);*/
return position;
}
