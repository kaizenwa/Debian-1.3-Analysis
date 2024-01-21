/*                   -*-Mode: c; c-file-style: "GNU"-*-              */
/**
 *
 * $Id: TextOut.c,v 1.22 1997/01/07 02:35:42 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static char rcsid[] = "$Id: TextOut.c,v 1.22 1997/01/07 02:35:42 miers Exp $";

#define	I18N

#include <LTconfig.h>
#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/TextOutP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBarP.h>
#include <XmI/XmI.h>
#include <X11/Xfuncs.h>
#include <limits.h>		/* for INT_MAX */
#ifdef HAVE_STRING_H
#include <string.h>
#define ANSI_STRING
#else
#include <strings.h>
#endif
#include <stdlib.h>

#include <XmI/DebugUtil.h>

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

#ifndef _XmMin 
#define _XmMin(a,b)  ((a) < (b)) ? (a) : (b)
#endif
#ifndef _XmMax 
#define _XmMax(a,b)  ((a) > (b)) ? (a) : (b)
#endif
static void Draw(XmTextWidget w, LineNum num, XmTextPosition start, XmTextPosition end, XmHighlightMode highlight);

#define Offset(field) XtOffsetOf(XmTextInnerRec, inner.field)

/* Resources for the TextInner class */
static XtResource output_resources[] =
{
  {
    XmNblinkRate, XmCBlinkRate, XmRInt,
    sizeof(int), Offset(out.blinkrate),
    XmRImmediate, (XtPointer) 500
  },
  {
    XmNcolumns, XmCColumns, XmRShort,
    sizeof(short), Offset(out.columns),
    XmRImmediate, (XtPointer) 20
  },
  {
    XmNcursorPositionVisible, XmCCursorPositionVisible, XmRBoolean,
    sizeof(Boolean), Offset(out.cursor_position_visible),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNfontList, XmCFontList, XmRFontList,
    sizeof(XmFontList), Offset(out.fontlist),
    XmRImmediate, (XtPointer) XmUNSPECIFIED
  },
  {
    XmNresizeHeight, XmCResizeHeight, XmRBoolean,
    sizeof(Boolean), Offset(out.resizeheight),
    XmRImmediate, (XtPointer) False
  },
  {
    XmNresizeWidth, XmCResizeWidth, XmRBoolean,
    sizeof(Boolean), Offset(out.resizewidth),
    XmRImmediate, (XtPointer) False
  },
  {
    XmNrows, XmCRows, XmRShort,
    sizeof(short), Offset(out.rows),
    XmRImmediate, (XtPointer) 1
  },
  {
    XmNwordWrap, XmCWordWrap, XmRBoolean,
    sizeof(Boolean), Offset(out.wordwrap),
    XmRImmediate, (XtPointer) False
  },

    /* ScrolledText resources */
  {
    XmNscrollHorizontal, XmCScroll, XmRBoolean,
    sizeof(Boolean), Offset(out.scrollhorizontal),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNscrollLeftSide, XmCScrollSide, XmRBoolean,
    sizeof(Boolean), Offset(out.scrollleftside),
    XmRImmediate, (XtPointer) False
  },
  {
    XmNscrollTopSide, XmCScrollSide, XmRBoolean,
    sizeof(Boolean), Offset(out.scrolltopside),
    XmRImmediate, (XtPointer) False
  },
  {
    XmNscrollVertical, XmCScroll, XmRBoolean,
    sizeof(Boolean), Offset(out.scrollvertical),
    XmRImmediate, (XtPointer) True
  },  
};
/*
 * Common initialization procedures -------------------------------------------
 */

static void
FontInit(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);
  XmFontListEntry entry = NULL;
  XFontStruct *fs;
  int i;

  DBGW("FontInit");

  for (i=0; Out_FontList(o)[i].tag != NULL; i++) {
    if (!strcmp(XmFONTLIST_DEFAULT_TAG, Out_FontList(o)[i].tag)) {
      entry = &Out_FontList(o)[i];
      break;
    }
  }
  if (!entry)
    Out_FontList(o) = entry = _XmFontListCreateDefault(XtDisplay((Widget)w));

  fs = (XFontStruct *) entry->font;
  Out_Font(o) = fs;
  Out_FontHeight(o) = Out_FontAscent(o) + Out_FontDescent(o);
  Out_FontAverageWidth(o) = (fs->max_bounds.width + fs->min_bounds.width) / 2;
  Out_TabWidth(o)=8*Out_FontAverageWidth(o);
  if (XdbInDebug(__FILE__, (Widget)w)) {
    XdbDebug(__FILE__, (Widget)w,
      "font: ascent=%d descent=%d height=%d avewidth=%d\n",
      Out_FontAscent(o), Out_FontDescent(o),
      Out_FontHeight(o), Out_FontAverageWidth(o));
  }
}


static void
GCInitialize(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);
  XGCValues values;
  XtGCMask mask, dynamic, dontcare;

/*
 * DrawGC
 *      Need to change :        ClipRectangles
 */
  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillSolid;
  values.font = Out_Font(o)->fid;
  values.foreground = Prim_Foreground(w);
  values.background = XtBackground(w);
  values.clip_x_origin = 0;
  values.clip_y_origin = 0;
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCFont | GCForeground | GCBackground;
  dynamic = GCClipMask | GCClipXOrigin | GCClipYOrigin | GCForeground | GCBackground;
  dontcare = 0;
/*   Out_DrawGC(o) = XtAllocateGC((Widget) w, 0, mask, &values, dynamic, dontcare); */
  Out_DrawGC(o) = XCreateGC(XtDisplay(w), XtWindow(w), mask, &values);
  Out_DrawGCInverted(o) = False;

/*
 * Cursor GC
 *      Need to change :        Tile/Stipple Origin, Stipple
 */
  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillStippled;
  values.foreground = Prim_Foreground(w);
  values.background = XtBackground(w);
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
  dynamic = GCTileStipXOrigin | GCTileStipYOrigin | GCStipple;
  dontcare = 0;
/*   Out_CursorGC(o) = XtAllocateGC((Widget) w, 0, mask, &values, dynamic, dontcare); */
  Out_CursorGC(o) = XCreateGC(XtDisplay(w), XtWindow(w), mask, &values);

/*
 * CopyGC
 *      At least this one is not changed anywhere :-)
 */
  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillSolid;
  values.foreground = XtBackground(w);
  values.background = Prim_Foreground(w);
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
  Out_CopyGC(o) = XtGetGC((Widget) w, mask, &values);
}

static void
GCPixmap(XmTextWidget tw)
{
  OutputData o=Text_OutputData(tw);
  Widget w = (Widget) tw;
  static char dots[] = {2, 1, 1};
  GC gc;
  int width, width_erase, height;
  Display *display;
  XGCValues values;
  XtGCMask mask;

  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillSolid;
  values.foreground = 0;
  values.background = 0;

  display = XtDisplay(w);
  width = 5;
  height = Out_FontHeight(o);
  width_erase = 2 * Out_Font(o)->max_bounds.width;

  if (Out_CursorIBeam(o))
    XFreePixmap(XtDisplay(w), Out_CursorIBeam(o));
  Out_CursorIBeam(o) = (Pixmap) NULL;
  if (Out_CursorStipple(o))
    XFreePixmap(XtDisplay(w), Out_CursorStipple(o));
  Out_CursorStipple(o) = (Pixmap) NULL;
  if (Out_CursorSave(o))
    XFreePixmap(XtDisplay(w), Out_CursorSave(o));
  Out_CursorSave(o) = (Pixmap) NULL;
  Out_CursorSaveValid(o) = False;

  if (height > 0) {
    Out_CursorIBeam(o) = XCreatePixmap(display,
				       RootWindowOfScreen(XtScreen(w)),
				       width, height, 1);
    Out_CursorStipple(o) = XCreatePixmap(display,
					 RootWindowOfScreen(XtScreen(w)),
					 width, height, 1);
    Out_CursorSave(o) = XCreatePixmap(display,
				      RootWindowOfScreen(XtScreen(w)),
				      width_erase, height, w->core.depth);
    values.line_style = LineSolid;
    values.line_width = 0;
    values.fill_style = FillSolid;
    values.foreground = 0;
    values.background = 0;
    mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
    gc = XCreateGC(display, Out_CursorIBeam(o), mask, &values);
    XFillRectangle(display, Out_CursorIBeam(o), gc, 0, 0, width, height);
    XFillRectangle(display, Out_CursorStipple(o), gc, 0, 0, width, height);
    XSetForeground(display, gc, 1);
    XDrawLine(display, Out_CursorIBeam(o), gc, 2, 0, 2, height - 2);
    XDrawLine(display, Out_CursorIBeam(o), gc, 0, 0, 4, 0);
    XDrawLine(display, Out_CursorIBeam(o), gc, 0, height - 2, 4, height - 2);
    
    XSetLineAttributes(display, gc, 0, LineOnOffDash, CapRound, JoinRound);
    XSetDashes(display, gc, 0, &dots[1], (int) dots[0]);
    XDrawLine(display, Out_CursorStipple(o), gc, 2, 0, 2, height - 2);
    XDrawLine(display, Out_CursorStipple(o), gc, 1, 0, 3, 0);
    XDrawLine(display, Out_CursorStipple(o), gc, 1, height - 2, 3, height - 2);
    
    XFreeGC(display, gc);
  }
}

static void
GCClip(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);
  XRectangle clip;

  clip.x = 0;
  clip.y = 0;
  clip.width = XtWidth(w)-
    2 * Prim_ShadowThickness(w) - 2 * Prim_HighlightThickness(w) -
    2 * Text_MarginWidth(w);
  clip.height = XtHeight(w)-
    2 * Prim_ShadowThickness(w) - 2 * Prim_HighlightThickness(w) -
    2 * Text_MarginHeight(w);

/* These calls change the GCs */
  XSetClipRectangles(XtDisplay((Widget) w), Out_DrawGC(o),
    Out_XDraw(o), Out_YDraw(o) - Out_FontAscent(o), &clip, 1, Unsorted);

  GCPixmap(w);
}


static void
SizeRecalc(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);

  Out_XDraw(o) = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
    Text_MarginWidth(w);

    /* Y offset for a row R is calculated as Out_YDraw(o) - R*fontHeight,
       and I add FontAscent here because y positions for fonts are
       measured from the baseline of the font. */
  Out_YDraw(o) = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
    Text_MarginHeight(w) + Out_FontAscent(o);
  
  Out_Columns(o) = (XtWidth(w)
    - 2 * Prim_ShadowThickness(w)
    - 2 * Text_MarginWidth(w)) / Out_FontAverageWidth(o);

  if (Text_EditMode(w)==XmSINGLE_LINE_EDIT)
    Out_Rows(o)=1;
  else
    Out_Rows(o) = (XtHeight(w)
      - 2 * Prim_ShadowThickness(w)
      - 2 * Text_MarginHeight(w)) / Out_FontHeight(o);
}




/* Text Drawing functions ------------------------------------------------*/

static void
DrawAll(XmTextWidget w)
{
  int i;
  
  for (i = 0; i < Text_LineCount(w); i++) {
    Line line=&Text_Line(w)[i];
    Line next=&Text_Line(w)[i+1];
    
    Draw(w, i, line->start, next->start-1, 0);
    
    if (XdbInDebug(__FILE__, (Widget)w)) {
      XdbDebug(__FILE__, (Widget)w,
        "DrawAll: line[%d]: start=%d end=%d\n",
        i, line->start, next->start-1);
    }
  }
}




/* Utilities ------------------------------------------------------------- */


#define Out_NextTabStop(o,x) (Out_TabWidth(o)*((x+Out_TabWidth(o))/Out_TabWidth(o)))
  
/* XTextWidth clone, but taking TABS into account.
 * FIXME: Must start from col 0 (or a tab boundary) for tab position to be
 *        accurate.
 */
int
_XmTextNextX(XmTextWidget w,int x,char *ptr, int len)
{
  OutputData o=Text_OutputData(w);
  XFontStruct *fs=Out_Font(o);
  
  while (len>0) {
    char c;
    
    c=*ptr;
    if (c=='\t') {
      x=Out_NextTabStop(o,x);
    }
    else {
      if (c<fs->min_char_or_byte2 || c>fs->max_char_or_byte2)
        c=fs->default_char;
      if (fs->per_char) {
        c-=fs->min_char_or_byte2;
        x+=fs->per_char[c].width;
      }
      else
        x+=fs->max_bounds.width;
    }
    
    ptr++;
    len--;
  }
  
  return x;
}

static Dimension
FontTextWidth(XmTextWidget w,XmTextPosition pos, XmTextPosition end)
{
  XmTextBlockRec block;
  Dimension width;
  
  if (Text_Source(w) == NULL) {
	_XmWarning((Widget)w, "FontTextWidth: no source\n");
	return 1;
  }

  (*Text_Source(w)->ReadSource)(Text_Source(w),pos,end,&block);
  width=_XmTextNextX(w,0,block.ptr,block.length);
  XtFree(block.ptr);
  return width;
}

static XmTextPosition
XToPos(XmTextWidget w, LineNum row, int x)
{
  OutputData o=Text_OutputData(w);
  Line line=&Text_Line(w)[row];
  Line next=&Text_Line(w)[row+1];
  XmTextPosition start,end,pos;
  XmTextBlockRec block;
  
  XdbDebug(__FILE__, (Widget)w, "XToPos(row %d, x %d) TotalLines %d\n",
	row, x, Text_TotalLines(w));

  /* Just guessing - Is this right ?? Danny */
  if (row >= Text_TotalLines(w)) {
	next=&Text_Line(w)[Text_TotalLines(w)-1];
	end=next->start-1;
	if (end < 0)
	    end = 0;
	return end;
  }

  start=line->start;
  if (next->past_end)	/* There's nothing there ... */
#if 1
	end = start;
#else
	end = Text_xxx(w) - start;	/* Mixing widget and output info here ... */
#endif
  else
	end=next->start-1;

    /* x is now the width from the start of the line to the pixel pos */
  x -= Out_XDraw(o) - Out_XOffset(o);
  
  (*Text_Source(w)->ReadSource)(Text_Source(w),start,end,&block);

/* check if the cursor is before the 1st character */
  if (x <= 0) {
    pos = start;
  }

/* OK, how 'bout after the last character */
  else if (x > _XmTextNextX(w,0,block.ptr,block.length)) {
    pos = end;
  }

/* must be in between somewhere... */
  else {
    int tot,count,i;
    
    tot = 0;
    count=end-start;
    pos = -1;
    for (i = 0; i < count; i++) {
      tot = _XmTextNextX(w,tot,&block.ptr[i],1);
      if (x < tot) {
	pos = start + i;
	break;
      }
    }
    if (pos < 0)
      pos = end;
  }

  XtFree(block.ptr);

  if (XdbInDebug(__FILE__, (Widget)w)) {
    XdbDebug(__FILE__, (Widget)w,
      "XToPos: x=%d start=%d end=%d pos=%d\n",x,start,end,pos);
  }
  
  return pos;
}


static LineNum
YToLineNum(XmTextWidget w, Position y)
{
  OutputData o=Text_OutputData(w);
  LineNum line;
  
  line = (LineNum)((y - Prim_ShadowThickness(w) - Prim_HighlightThickness(w) -
    Text_MarginHeight(w)) / Out_FontHeight(o));

  if (line>=Text_LineCount(w)) line=Text_LineCount(w)-1;
  
  if (XdbInDebug(__FILE__, (Widget)w)) {
    XdbDebug(__FILE__, (Widget)w,
      "YToLineNum: y=%d line=%d\n",y,line);
  }
  
  return line;
}

static void
DrawText(XmTextWidget w, int x, int y, char *ptr, int len,
  XmHighlightMode highlight)
{
  OutputData o=Text_OutputData(w);
  int xoff,xstart=x,xend;

  xoff = Out_XDraw(o) - Out_XOffset(o);
  if (highlight==XmHIGHLIGHT_SELECTED && !Out_DrawGCInverted(o)) {
    XSetForeground(XtDisplay(w),Out_DrawGC(o),XtBackground(w));
    XSetBackground(XtDisplay(w),Out_DrawGC(o),Prim_Foreground(w));
    Out_DrawGCInverted(o)=True;
  }
  else if (Out_DrawGCInverted(o)) {
    XSetForeground(XtDisplay(w),Out_DrawGC(o),Prim_Foreground(w));
    XSetBackground(XtDisplay(w),Out_DrawGC(o),XtBackground(w));
    Out_DrawGCInverted(o)=False;
  }
  
  while (len>0) {
    char *start;
    Dimension piecewidth;
    int count;
  
    start=ptr;
    count=0;
    while (len>0 && *ptr && *ptr!='\t') ptr++,count++,len--;
    if (count>0) {
      piecewidth=_XmTextNextX(w,0,start,count);
      XDrawImageString(XtDisplay(w), XtWindow(w), Out_DrawGC(o),
        x + xoff, y, start, count);
      if (XdbInDebug(__FILE__, (Widget)w)) {
        char temp[256];
        
        strncpy(temp,start,count);
        temp[count]='\0';
        XdbDebug(__FILE__, (Widget)w, "Draw: x=%d y=%d -->%s<--\n",x,y,temp);
      }
    
      x+=piecewidth;
    }
    if (len>0 && *ptr=='\t') {
      x=Out_NextTabStop(o,x);
      ptr++,len--;
    }
  }
  xend=x;
  
  if (highlight==XmHIGHLIGHT_SECONDARY_SELECTED)
    XDrawLine(XtDisplay(w), XtWindow(w), Out_DrawGC(o),
      xoff+xstart, y+1, xoff+xend, y+1);
}

static void
ChangeHOffset(XmTextWidget w, int offset)
{ 
  OutputData o = Text_OutputData(w);
  if (Out_XOffset(o) == offset)
    return;
  Out_XOffset(o) = offset;
  Out_CursorSaveValid(o) = False;
  (*Text_Output(w)->expose)((Widget) w, NULL, (Region) NULL);

}


void
_XmRedisplayHBar(XmTextWidget w, int offset)
{
  OutputData o = Text_OutputData(w);
  Dimension displayWidth, maxWidth;
  int oldMin, oldMax, oldSize, oldValue;
  int newValue;
  int i;

 if (Out_Hbar(o) == NULL)
   return;

  ChangeHOffset(w, offset);
  if (!Out_ScrollHorizontal(o))
    return;
  maxWidth = 0;
  for(i = 0; i < Out_Rows(o); i++)
    if (maxWidth < Text_Line(w)[i].extra->width)
      maxWidth = Text_Line(w)[i].extra->width;
  displayWidth  = XtWidth(w) - 2*(Prim_HighlightThickness(w) + 
    Prim_ShadowThickness(w) + Text_MarginWidth(w));
  if (maxWidth < displayWidth)
    maxWidth = displayWidth;
  Out_ScrollWidth(o) = maxWidth;
  XtVaGetValues(Out_Hbar(o),
		XmNmaximum, &oldMax,
		XmNminimum, &oldMin,
		XmNsliderSize, &oldSize,
		XmNvalue, &oldValue,
		NULL);
  newValue = _XmMin(Out_XOffset(o), maxWidth - displayWidth);
  if (oldMax != maxWidth 
      || oldMin != 0
      || oldSize != displayWidth
      || oldValue != newValue)
    XtVaSetValues(Out_Hbar(o),
		  XmNmaximum, maxWidth,
		  XmNminimum, 0,
		  XmNsliderSize, displayWidth,
		  XmNvalue, newValue,
		  NULL);
    XdbDebug(__FILE__, (Widget) w,
	     "_XmRedisplayHBar: oldmax=%d newmax=%d oldsize=%d newsize=%d\n",
	     oldMax, maxWidth, oldSize, displayWidth);
    XdbDebug(__FILE__, (Widget) w,
	     "_XmRedisplayHBar: oldvalue=%d newvalue=%d\n",
	     oldValue, newValue);

}
void
_XmChangeVSB(XmTextWidget w, XmTextPosition pos)
{
  OutputData o=Text_OutputData(w);
  int top_index;
  int oldMin, oldMax, oldSize, oldValue, newMax;
  top_index = _XmTextGetTableIndex(w, Text_TopPos(w));
  XtVaGetValues(Out_Vbar(o),
		XmNmaximum, &oldMax,
		XmNminimum, &oldMin,
		XmNsliderSize, &oldSize,
		XmNvalue, &oldValue,
		NULL);
  newMax = _XmMax(Text_TotalLines(w),Out_Rows(o));
  if (oldMax != newMax
      || oldMin != 0
      || oldSize != Out_Rows(o)
      || oldValue != top_index)
    XtVaSetValues(Out_Vbar(o),
		  XmNmaximum, newMax,
		  XmNminimum, 0,
		  XmNsliderSize, Out_Rows(o),
		  XmNvalue, top_index,
		  NULL);
    XdbDebug(__FILE__, (Widget) w,
	     "_XmChangeVSB: oldmax=%d newmax=%d oldsize=%d newsize=%d\n",
	     oldMax, Text_TotalLines(w), oldSize, Text_LineCount(w));
    XdbDebug(__FILE__, (Widget) w,
	     "_XmChangeVSB: oldvalue=%d newvalue=%d\n",
	     oldValue, top_index);

}
/* Cursor functions -------------------------------------------------------- */

static void
CursorInit(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);
  
  Out_CursorIBeam(o) = (Pixmap) NULL;
  Out_CursorStipple(o) = (Pixmap) NULL;
  Out_CursorSave(o) = (Pixmap) NULL;

  Out_CursorPositionVisible(o) = Out_CursorSaveValid(o) = False;
}

static void
CursorSet(XmTextWidget w, Boolean focus)
{
  OutputData o=Text_OutputData(w);
  
  Out_HasFocus(o) = focus;

    /* These change the GC !! (Danny) */
  if (focus)
    XSetStipple(XtDisplay((Widget) w), Out_CursorGC(o), Out_CursorIBeam(o));
  else
    XSetStipple(XtDisplay((Widget) w), Out_CursorGC(o), Out_CursorStipple(o));
}

static void
CursorSaveUnderIBeam(XmTextWidget w, Position x, Position y)
{
  OutputData o = Text_OutputData(w);

  if (! XtIsRealized((Widget)w))
        return;
    /* Make sure that the text is drawn before saving the cursor */
  XFlush(XtDisplay((Widget) w));

    /* save the area under the cursor */
  XFillRectangle(XtDisplay((Widget) w), Out_CursorSave(o), Out_CopyGC(o),
    0, 0, 5, Out_FontHeight(o));
  XCopyArea(XtDisplay((Widget) w), XtWindow((Widget) w), Out_CursorSave(o),
    Out_CursorGC(o),
    x - 2, y,
    5, Out_FontHeight(o), 0, 0);

  Out_CursorSaveValid(o) = True;
}

static void
CursorRestoreUnderIBeam(XmTextWidget w, Position x, Position y)
{
  OutputData o=Text_OutputData(w);
  
  if (! XtIsRealized((Widget)w))
        return;
    /* Make sure that the text is drawn before erasing the cursor */
  XFlush(XtDisplay((Widget) w));
  
  XCopyArea(XtDisplay((Widget) w), Out_CursorSave(o), XtWindow((Widget) w),
    Out_CursorGC(o), 0, 0, 5, Out_FontHeight(o),
    x - 2, y);
  Out_CursorSaveValid(o) = False;
}

static void
CursorDrawIBeam(XmTextWidget w, int x, int y)
{
  OutputData o = Text_OutputData(w);
  
    /* save the area under the cursor */
  CursorSaveUnderIBeam(w, x, y);

  /* Bail if the GC's don't exist yet. */
  if (! XtIsRealized((Widget)w))
        return;

  /* These change the GC !! (Danny) */
  XSetTSOrigin(XtDisplay((Widget) w), Out_CursorGC(o),
    x - 2, y);
  XFillRectangle(XtDisplay((Widget) w), XtWindow((Widget) w), Out_CursorGC(o),
    x - 2, y, 5, Out_FontHeight(o));
}

static void
CursorDraw(XmTextWidget w)
{
  OutputData o = Text_OutputData(w);

  if (Out_CursorPositionVisible(o)) {
    CursorDrawIBeam(w, Out_CursorX(o), Out_CursorY(o));
    Out_BlinkOn(o) = True;
  }
}

static void
CursorErase(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);
  
  if (Out_CursorSaveValid(o)) {
    CursorRestoreUnderIBeam(w, Out_CursorX(o), Out_CursorY(o));
  }
  Out_BlinkOn(o) = False;
}

/* Callbacks ---------------------------------------------------------------*/

static void
HandleTimer(XtPointer client_data, XtIntervalId* timer) 
{
  XmTextWidget w = (XmTextWidget) client_data;
  OutputData o=Text_OutputData(w);
  Out_BlinkState(o) =  Out_BlinkState(o) == on ? off: on;
  (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w),
					Out_BlinkState(o));

    Out_TimerId(o) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget) w),
				     Out_BlinkRate(o), HandleTimer, 
				     (XtPointer) w);
}

static void
HandleHBar(Widget sw, XtPointer client_data, XtPointer call_data)     
{
  XmTextWidget w = (XmTextWidget) client_data;
  OutputData o=Text_OutputData(w);
  XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct*) call_data;
  Dimension displayWidth, newOffset = 0;

  switch(cbs->reason) {
  case XmCR_DECREMENT:
    XdbDebug(__FILE__, (Widget) w, 
	     "HandleHbar reason=XmCR_DECREMENT value= %d\n", cbs->value);
    newOffset = Out_XOffset(o) > Out_FontAverageWidth(o) ?
       Out_XOffset(o) - Out_FontAverageWidth(o) : 0;
    break;
  case XmCR_DRAG:
    XdbDebug(__FILE__, (Widget) w, 
	     "HandleHbar reason=XmCR_DRAG value=%d\n", cbs->value);
    newOffset = cbs->value/Out_FontAverageWidth(o)*Out_FontAverageWidth(o);
    break;
  case XmCR_INCREMENT:
    XdbDebug(__FILE__, (Widget) w, 
	     "HandleHbar reason=XmCR_INCREMENT value=%d\n", cbs->value);
    displayWidth  = XtWidth(w) - 2*(Prim_HighlightThickness(w) + 
				    Prim_ShadowThickness(w) + Text_MarginWidth(w));

    if (Out_XOffset(o) < Out_ScrollWidth(o) - displayWidth)
      newOffset = Out_XOffset(o) + Out_FontAverageWidth(o);
    break;
  case XmCR_PAGE_DECREMENT:
    XdbDebug(__FILE__, (Widget) w, 
	     "HandleHbar reason=XmCR_PAGE_DECREMENT value=%d\n",cbs->value );
    /* FIXME How mutch does we have to move on page_decrement ? */
    newOffset = Out_XOffset(o) > Out_FontAverageWidth(o) ?
      Out_XOffset(o) - Out_FontAverageWidth(o) : 0;
    break;
  case XmCR_PAGE_INCREMENT:
    XdbDebug(__FILE__, (Widget) w, 
	     "HandleHbar reason=XmCR_PAGE_INCREMENT value=%d\n",cbs->value );
    /* FIXME How mutch does we have to move on page_increment ? */
    displayWidth  = XtWidth(w) - 2*(Prim_HighlightThickness(w) + 
				    Prim_ShadowThickness(w) + Text_MarginWidth(w));
    
    if (Out_XOffset(o) < Out_ScrollWidth(o) - displayWidth -
	Out_FontAverageWidth(o))
      newOffset = Out_XOffset(o) + Out_FontAverageWidth(o);
    break;
  case XmCR_TO_BOTTOM:
    XdbDebug(__FILE__, (Widget) w,
	     "HandleHbar reason=XmCR_TO_BOTTOM value=%d pixel=%d\n",
	     cbs->value, cbs->pixel );
    newOffset = 0;
    break;
  case XmCR_TO_TOP:
    XdbDebug(__FILE__, (Widget) w, 
	     "HandleHbar reason=XmCR_TO_TOP value=%d pixel=%d\n",
	     cbs->value, cbs->pixel);
    displayWidth  = XtWidth(w) - 2*(Prim_HighlightThickness(w) + 
				    Prim_ShadowThickness(w) + Text_MarginWidth(w));
    newOffset = Out_ScrollWidth(o) - displayWidth;
    break;
  case XmCR_VALUE_CHANGED:
    XdbDebug(__FILE__, (Widget) w, 
	     "HandleHbar reason=XmCR_VALUE_CHANGED value=%d\n", cbs->value);
    newOffset = cbs->value/Out_FontAverageWidth(o)*Out_FontAverageWidth(o);
    break;
  default:
    newOffset = Out_XOffset(o);
    break;
  }
  ChangeHOffset(w, newOffset);
  XtVaSetValues(Out_Hbar(o),
		XmNvalue, Out_XOffset(o),
		NULL);
}

void
HandleVBar(Widget sw, XtPointer client_data, XtPointer call_data)
{
  XmTextWidget w = (XmTextWidget) client_data;
  OutputData o=Text_OutputData(w);
  XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct*) call_data;
  Dimension pos = 0;

  switch(cbs->reason) {
  case XmCR_DECREMENT:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmCR_DECREMENT\n");
    XmTextScroll((Widget) w, -1);
    pos =  _XmTextGetTableIndex(w, Text_TopPos(w));
    break;
  case XmCR_DRAG:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmCR_DRAG value=%d\n",
	     cbs->value);
    pos = cbs->value;
    Text_TopPos(w) = Text_LineTable(w)[pos].start_pos;
    break;
  case XmCR_INCREMENT:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmCR_INCREMENT\n");
    XmTextScroll((Widget) w, 1);
    pos =  _XmTextGetTableIndex(w, Text_TopPos(w));
    break;
  case XmCR_PAGE_DECREMENT:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmCR_PAGE_INCREMENT\n");
    XmTextScroll((Widget) w, -1);
    pos =  _XmTextGetTableIndex(w, Text_TopPos(w));
    break;
  case XmCR_PAGE_INCREMENT:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmCR_PAGE_INCREMENT\n");
    break;
    XmTextScroll((Widget) w, 1);
    pos =  _XmTextGetTableIndex(w, Text_TopPos(w));
  case XmCR_TO_BOTTOM:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmCR_TO_BOTTOM\n");
    pos = 0;
    Text_TopPos(w) = Text_FirstPos(w);
    break;
  case XmCR_TO_TOP:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmCR_TO_TOP\n");
    pos =  _XmTextGetTableIndex(w, Text_TopPos(w)) - Text_LineCount(w) + 1;
    break;
  case XmCR_VALUE_CHANGED:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=XmVALUE_CHANGED\n");
    pos = cbs->value;
    Text_TopPos(w) = Text_LineTable(w)[pos].start_pos;
    break;
  default:
    XdbDebug(__FILE__, (Widget) w, "HandleVbar reason=UNKNOWN\n %d",cbs->reason);
    break;
  }
  XtVaSetValues(Out_Vbar(o),
		XmNvalue, pos,
		NULL);
  _XmRedisplayHBar(w, Out_XOffset(o));
  Text_NeedsRefigureLines(w) = True;
  (*w->core.widget_class->core_class.expose)((Widget) w, NULL, (Region) NULL);

}


static void
HandleFocusEvents(Widget aw, XtPointer client_data, XtPointer call_data)
{
  XmAnyCallbackStruct *call_struct = (XmAnyCallbackStruct *) call_data;
  XmTextWidget w = (XmTextWidget) aw;
  OutputData o=Text_OutputData(w);

  XdbDebug(__FILE__, (Widget) w, "HandleFocusEvent reason=%s\n",
	   call_struct->reason == XmCR_FOCUS ? "XmCR_FOCUS": 
	   "XmCR_LOSING_FOCUS");
  if (call_struct->reason == XmCR_FOCUS) {
    Out_HasFocus(o) = True;
    Out_BlinkState(o) = on;
    XSetStipple(XtDisplay(aw), Out_CursorGC(o), Out_CursorIBeam(o));
    if(Out_BlinkRate(o) > 0)
      Out_TimerId(o) = XtAppAddTimeOut(XtWidgetToApplicationContext(aw),
				       Out_BlinkRate(o), HandleTimer, 
				       (XtPointer) w);
  }
  else {
    Out_HasFocus(o) = False;
    XSetStipple(XtDisplay(aw), Out_CursorGC(o), Out_CursorStipple(o));
    if(Out_TimerId(o))
      XtRemoveTimeOut(Out_TimerId(o));
  }
  (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), on);
}



/* Output Methods --------------------------------------------------------- */

static XmTextPosition
XYToPos(XmTextWidget w, Position x, Position y)
{
  LineNum row=YToLineNum(w,y);

  return XToPos(w,row,x);
}


static Boolean
PosToXY(XmTextWidget w, XmTextPosition pos, Position * x, Position * y)
{
  OutputData o=Text_OutputData(w);
  int i;
  
  if (pos<Text_TopPos(w)) return False;
  
  for (i=0; i<Text_LineCount(w); i++) {
    Line line=&Text_Line(w)[i];
    Line next=&Text_Line(w)[i+1];

    if (pos>=line->start && pos <next->start) {
      *y = Out_YDraw(o) + i*Out_FontHeight(o);
      *x = Out_XDraw(o) - Out_XOffset(o) + FontTextWidth(w,line->start, pos);
      return True;
    }
  }
  
  return False;
}

static void
MakePositionVisible(XmTextWidget w, XmTextPosition pos)
{
  OutputData o=Text_OutputData(w);
  unsigned int line,newTopLine;
  int start, offset;
  Dimension xpos,last_xpos;
#if 0
  XdbDebug(__FILE__, (Widget) w, "MakePositionVisible pos=%d\n", pos);
#endif
  line = _XmTextGetTableIndex(w, pos);
  if (line < Text_TopLine(w))
    newTopLine = line;
  else if (line > Text_TopLine(w) + Text_LineCount(w) - 1)
    newTopLine = line - Text_LineCount(w) + 1;
  else
    newTopLine = Text_TopLine(w);
  if (newTopLine != Text_TopLine(w)) {
    Text_TopPos(w) = Text_LineTable(w)[newTopLine].start_pos;
    Text_NeedsRedisplay(w) = True;
    Text_NeedsRefigureLines(w) = True;
  }
  start = Text_LineTable(w)[line].start_pos;
  xpos = FontTextWidth(w, start, pos);
  last_xpos = XtWidth(w) - 2*(Prim_HighlightThickness(w) + 
			      Prim_ShadowThickness(w) + 
			      Text_MarginWidth(w));
  if (xpos - Out_XOffset(o) > last_xpos)
    offset = xpos - last_xpos;
  else if (xpos < Out_XOffset(o))
    offset =  xpos;
  else
    offset = Out_XOffset(o);
  
  /* FIXME should Out_ScrollHorizontal and Out_ScrollVertical be False if
   * Text is not child of scrolledwindow
   */
  if (offset != Out_XOffset(o)) {
    if(Out_ScrollHorizontal(o) && Out_Hbar(o))
      _XmRedisplayHBar(w, offset);
    else 
      ChangeHOffset(w, offset);
  }
  if(Out_ScrollVertical(o) && Out_Vbar(o))
    _XmChangeVSB(w, pos);
}


static Boolean
MoveLines(XmTextWidget w, LineNum from, LineNum to, LineNum dest)
{
  return False;
}

static Boolean
MeasureLine(XmTextWidget w, LineNum num, XmTextPosition pos,
  XmTextPosition * next, LineTableExtraRec ** extra)
{
  OutputData o=Text_OutputData(w);
  Dimension width = 0;
  XmTextPosition start, end;
  LineTableExtra e;

  start = pos < Text_LastPos(w) ? pos: Text_LastPos(w);
  if (pos < Text_LastPos(w))
    end = (*Text_Source(w)->Scan)(Text_Source(w), pos,
      XmSELECT_LINE, XmsdRight, -1, False);
  else
    end = Text_LastPos(w);
  if (next) {
    if (end >= Text_LastPos(w))
      *next = PASTENDPOS;
    else
      *next = end + 1;
    width = FontTextWidth(w, start, end);
    
    if (extra) {
      e = (LineTableExtra)XtMalloc(sizeof(LineTableExtraRec));
      e->width = width;
      e->wrappedbychar = False;
      
      *extra = e;
    }
  }
  
#if 0
/* This is incredibly verbose; besides it seems to work. */
  if (XdbInDebug(__FILE__, (Widget) w)) {
    if (next)
      XdbDebug(__FILE__, (Widget) w, 
	       "MeasureLine: line=%d pos=%d next=%d width=%d\n",
	       num, pos, *next, width);
    else
      XdbDebug(__FILE__, (Widget) w,
	       "MeasureLine: line=%d pos=%d next=NULL width=%d\n",
	       num, pos, width);
  }
#endif
  if (num <= Out_Rows(o) - 1)
    return True;
  else {
    _XmRedisplayHBar(w, Out_XOffset(o));
    return False;
  }
}


static void
DrawInsertionPoint(XmTextWidget w, XmTextPosition pos, OnOrOff state)
{
  OutputData o=Text_OutputData(w);
  Position x,y;

#if 0
  XdbDebug(__FILE__, (Widget)w, "Output DrawInsertionPoint CursorSave %sValid\n",
	Out_CursorSaveValid(o) ? "" : "not ");
#endif

  if (! XtIsRealized((Widget)w))
	return;

  /* erase previously visible cursor */
  if (Out_CursorPositionVisible(o))
    CursorErase(w);
  if (state == on) {  
    /* draw new cursor */
    Out_CursorPositionVisible(o) = (*Text_Output(w)->PosToXY)(w,pos,&x,&y);
    if (Out_CursorPositionVisible(o)) {
      Out_CursorX(o) = x;
      Out_CursorY(o) = y - Out_FontAscent(o) + 1;
#if 1
	if (! Out_CursorSaveValid(o)) {
		CursorSaveUnderIBeam(w, Out_CursorX(o), Out_CursorY(o));
	}
#endif
      CursorDraw(w);
    }
  }
#if 0
  if (XdbInDebug(__FILE__, (Widget)w)) {
    XdbDebug(__FILE__, (Widget)w,
      "DrawInsertionPoint: x=%d pos=%d state=%s\n",
      Out_CursorX(o), pos, (state==on?"on":"off"));
  }
#endif
}


static void
Draw(XmTextWidget w, LineNum num, XmTextPosition start, XmTextPosition end,
  XmHighlightMode highlight)
{
  OutputData o=Text_OutputData(w);
  XmTextBlockRec block;
  Line line=&Text_Line(w)[num];

  if (end > Text_LastPos(w)) {
    end = Text_LastPos(w);
    if (line->start > end)
      return;
  }
  (*Text_Source(w)->ReadSource)(Text_Source(w),line->start,end,&block);
  if (block.length > 0) {
    int x,y;
    char *ptr;
    int len;
    
    ptr=block.ptr+start-line->start;
    len=end-start;
    if (start>line->start)
      x= _XmTextNextX(w,0,block.ptr,start-line->start);
    else
      x= 0;
    
    y = Out_YDraw(o) + num*Out_FontHeight(o);

    DrawText(w,x,y,ptr,len,highlight);
  }
  XtFree(block.ptr);
}


static void
OutputInvalidate(XmTextWidget w, XmTextPosition start, XmTextPosition end,
  long delta)
{
	OutputData o = Text_OutputData(w);
	Out_CursorSaveValid(o) = False;
}

static void
GetPreferredSize(Widget aw, Dimension * width, Dimension * height)
{
  XmTextWidget w = (XmTextWidget) aw;
  OutputData o=Text_OutputData(w);
  
  *width = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
    2 * Text_MarginWidth(w) +
    Out_Columns(o) * Out_FontAverageWidth(o));

  *height = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
    2 * Text_MarginHeight(w) +
    Out_Rows(o) * Out_FontHeight(o));
}

static void
OutputGetValues(Widget aw, ArgList args, Cardinal num_args)
{
}

static Boolean
OutputSetValues(Widget old, Widget request, Widget new,
  ArgList args, Cardinal * num_args)
{
  XdbDebug(__FILE__, new, "OutputSetValues\n");

  return False;
}

static void
destroy(Widget aw)
{
  XmTextWidget w = (XmTextWidget) aw;
  OutputData o=Text_OutputData(w);

  DBGW("destroy");

  if (o && Out_TimerId(o))
    XtRemoveTimeOut(Out_TimerId(o));

  XtReleaseGC((Widget) w, Out_DrawGC(o));
  XtReleaseGC((Widget) w, Out_CursorGC(o));
  XtReleaseGC((Widget) w, Out_CopyGC(o));
  if (Out_CursorIBeam(o))
    XFreePixmap(XtDisplay((Widget)w), Out_CursorIBeam(o));
  if (Out_CursorStipple(o))
    XFreePixmap(XtDisplay((Widget)w), Out_CursorStipple(o));
  if (Out_CursorSave(o))
    XFreePixmap(XtDisplay((Widget)w), Out_CursorSave(o));

  XtFree((char *)Text_Output(w));
}

static void
OutputExpose(Widget aw, XEvent * event, Region region)
{
  XmTextWidget w = (XmTextWidget) aw;

  XdbDebug(__FILE__, aw, "OutputExpose\n");

  XClearArea(XtDisplay(w), XtWindow(w),
    Prim_HighlightThickness(w) + Prim_ShadowThickness(w),
    Prim_HighlightThickness(w) + Prim_ShadowThickness(w),
    XtWidth(w) - 2 * (Prim_HighlightThickness(w) + Prim_ShadowThickness(w)),
    XtHeight(w) - 2 * (Prim_HighlightThickness(w) + Prim_ShadowThickness(w)),
    False);
  DrawAll(w);
  _XmDrawShadows(XtDisplay(w),
		 XtWindow(w),
		 Prim_TopShadowGC(w),
		 Prim_BottomShadowGC(w),
		 Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		 XtWidth(w) - 2 * Prim_HighlightThickness(w),
		 XtHeight(w) - 2 * Prim_HighlightThickness(w),
		 Prim_ShadowThickness(w),
		 XmSHADOW_IN);
  (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), on);
}

static void
OutputRealize(Widget aw, XtValueMask * value_mask, XSetWindowAttributes * attributes)
{
  XmTextWidget w = (XmTextWidget) aw;

  XdbDebug(__FILE__, aw, "OutputRealize\n");

  GCInitialize(w);
  GCClip(w);
  CursorSet(w,False);
#if 0
  CursorSaveUnderIBeam(w, 0, 0);
#endif
}

static void
resize(Widget aw, Boolean state)
{
  XmTextWidget w = (XmTextWidget) aw;
  OutputData o=Text_OutputData(w);
  
  SizeRecalc(w);

  /* Only adjust the GCs if they have already been created! */
  if (Out_DrawGC(o))
    GCClip(w);
}




OutputRec outputRec =
{
    /* _OutputDataRec            */ NULL,
    /* XYToPosProc               */ XYToPos,
    /* PosToXYProc               */ PosToXY,
    /* MeasureLineProc           */ MeasureLine,
    /* DrawProc                  */ Draw,
    /* DrawInsertionPointProc    */ DrawInsertionPoint,
    /* MakePositionVisibleProc   */ MakePositionVisible,
    /* MoveLinesProc             */ MoveLines,
    /* InvalidateProc            */ OutputInvalidate,
    /* GetPreferredSizeProc      */ GetPreferredSize,
    /* GetValuesProc             */ OutputGetValues,
    /* SetValuesProc             */ OutputSetValues,
    /* XmRealizeOutProc          */ OutputRealize,
    /* XtWidgetProc              */ destroy,
    /* XmResizeFlagProc          */ resize,
    /* XtExposeProc              */ OutputExpose
};

void
_XmTextOutputCreate(Widget aw, ArgList args, Cardinal num_args)
{
  XmTextWidget w = (XmTextWidget) aw;
  OutputData o;
  XmTextInnerWidget iw = (XmTextInnerWidget) w->text.inner_widget;
  Dimension width,height;

  DBGW("_XmTextOutputCreate");

  Text_Output(w) = (Output) XtMalloc(sizeof(OutputRec));
  bcopy(&outputRec, Text_Output(w), sizeof(OutputRec));

  Text_OutputData(w) = o = &iw->inner.out; 
  XtGetSubresources (aw, iw, 
		     aw->core.name, 
		     aw->core.widget_class->core_class.class_name,
		     output_resources,
		     XtNumber(output_resources),
		     args, num_args);
  Out_XOffset(o)=0;
  XtAddCallback(aw, XmNfocusCallback,
		HandleFocusEvents, 
		(XtPointer)w);
  XtAddCallback(aw, XmNlosingFocusCallback,
		HandleFocusEvents, 
		(XtPointer)w);
  CursorInit(w);
  
  /* Use DrawGC as a flag to see if the GCs have been created */
  Out_DrawGC(o) = NULL;


  /* Get the font information */
  if (Out_FontList(o) == (XmFontList) XmUNSPECIFIED || !Out_FontList(o)) {
    Out_FontList(o) = _XmGetDefaultFontList((Widget) w, XmTEXT_FONTLIST);
    Out_FontListCreated(o) = False;	/* we didn't create this, so don't free it */
  }
  else
    Out_FontListCreated(o) = True;	/* we created this, so free it when the time comes */

  FontInit(w);

    /* Check window dimensions */
  if (XtWidth(w) == (Dimension) 0)
    if (Out_Columns(o)<=0) Out_Columns(o) = 20;
  else
    Out_Columns(o) = (XtWidth(w)
      - 2 * Prim_ShadowThickness(w)
      - 2 * Text_MarginWidth(w)) / Out_FontAverageWidth(o);

  if (XtHeight(w) == (Dimension) 0)
    if (Out_Rows(o)<=0) Out_Rows(o) = 10;
  else
    Out_Rows(o) = (XtHeight(w)
      - 2 * Prim_ShadowThickness(w)
      - 2 * Text_MarginHeight(w)) / Out_FontHeight(o);
  if (Text_EditMode(w)==XmSINGLE_LINE_EDIT) Out_Rows(o)=1;

  (*Text_Output(w)->GetPreferredSize)(aw,&width,&height);
  XtWidth(w) = width;
  XtHeight(w) = height;
  
  SizeRecalc(w);

  /* Create scrollbars if child of scrolledwindow */

  if (XmIsScrolledWindow(XtParent(w))) {
    Widget sw = XtParent(w);
    Boolean scrollingPolicy;

    XtVaGetValues(sw,
		  XmNscrollingPolicy, &scrollingPolicy,
		  NULL);

    if (scrollingPolicy == XmAUTOMATIC) {
      Out_ScrollVertical(o) = False;
      Out_ScrollHorizontal(o) = False;
    }

    if (Out_ScrollHorizontal(o)) {

      if (SW_HSB(sw) != NULL)
	Out_Hbar(o) = (Widget)SW_HSB(sw);
      else
        Out_Hbar(o) = XtVaCreateWidget("HorScrollBar",
				       xmScrollBarWidgetClass, 
				       sw,
				       XmNorientation,XmHORIZONTAL,  
				       NULL);
      
      XtAddCallback((Widget)Out_Hbar(o), XmNdecrementCallback,
		    HandleHBar, 
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Hbar(o), XmNdragCallback,
		    HandleHBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Hbar(o), XmNincrementCallback,
		    HandleHBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Hbar(o), XmNpageDecrementCallback,
		    HandleHBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Hbar(o), XmNpageIncrementCallback,
		    HandleHBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Hbar(o), XmNtoBottomCallback,
		    HandleHBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Hbar(o), XmNtoTopCallback,
		    HandleHBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Hbar(o), XmNvalueChangedCallback,
		    HandleHBar,
		    (XtPointer)w);
      XtManageChild(Out_Hbar(o));
    }
    else
      Out_Hbar(o) = NULL;

    if (Out_ScrollVertical(o) && Text_EditMode(w) != XmSINGLE_LINE_EDIT) {
      if (SW_VSB(sw) != NULL)
	Out_Vbar(o) = (Widget)SW_VSB(sw);
      else
        Out_Vbar(o) = XtVaCreateWidget("VertScrollBar",
				       xmScrollBarWidgetClass,
				       sw,
				       NULL);
      XtAddCallback((Widget)Out_Vbar(o), XmNdecrementCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Vbar(o), XmNdragCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Vbar(o), XmNincrementCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Vbar(o), XmNpageDecrementCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Vbar(o), XmNpageIncrementCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Vbar(o), XmNtoBottomCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Vbar(o), XmNtoTopCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtAddCallback((Widget)Out_Vbar(o), XmNvalueChangedCallback,
		    HandleVBar,
		    (XtPointer)w);
      XtManageChild(Out_Vbar(o));
    }
    else
      Out_Vbar(o) = NULL;
    XmScrolledWindowSetAreas (sw, Out_Hbar(o), Out_Vbar(o), aw); 
    if (Out_ScrollLeftSide(o) && Out_ScrollTopSide(o))
      XtVaSetValues(sw,
		    XmNscrollBarPlacement, XmTOP_LEFT,
		    NULL);
    else if (!Out_ScrollLeftSide(o) && Out_ScrollTopSide(o))
      XtVaSetValues(sw,
		    XmNscrollBarPlacement, XmTOP_RIGHT,
		    NULL);
    else if (Out_ScrollLeftSide(o) && !Out_ScrollTopSide(o))
      XtVaSetValues(sw,
		    XmNscrollBarPlacement, XmBOTTOM_LEFT,
		    NULL);
    else if (!Out_ScrollLeftSide(o) && !Out_ScrollTopSide(o))
      XtVaSetValues(sw,
		    XmNscrollBarPlacement, XmBOTTOM_RIGHT,
		    NULL);
  }

}



/*
 * Quasi-Public functions -----------------------------------------------------
 */


void
_XmTextDrawDestination(XmTextWidget widget)
{
}

void
_XmTextClearDestination(XmTextWidget widget, Boolean ignore_sens)
{
}

void
_XmTextDestinationVisible(Widget w, Boolean turn_on)
{
}

void
_XmTextChangeBlinkBehavior(XmTextWidget widget, Boolean newvalue)
{
}

void
_XmTextAdjustGC(XmTextWidget w)
{
}

Boolean
_XmTextShouldWordWrap(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);

  return o->wordwrap;
}

Boolean
_XmTextScrollable(XmTextWidget w)
{
  OutputData o=Text_OutputData(w);

  return o->scrollvertical;
}

void
_XmTextOutputGetSecResData(XmSecondaryResourceData * secResDataRtn)
{
}

int
_XmTextGetNumberLines(XmTextWidget w)
{
  return w->text.total_lines;
}

Boolean
_XmTextGetDisplayRect(Widget w, XRectangle * display_rect)
{
  return False;
}

void
_XmTextMarginsProc(Widget w, XmBaselineMargins * margins_rec)
{
}

void
_XmTextChangeHOffset(XmTextWidget widget, int length)
{
}

void
_XmTextToggleCursorGC(Widget widget)
{
}

void
_XmTextFreeContextData(Widget w, XtPointer clientData, XtPointer callData)
{
}

void
_XmTextResetClipOrigin(XmTextWidget tw, XmTextPosition position,
  Boolean clip_mask_reset)
{
}

XmTextPosition
_XmTextFindLineEnd(XmTextWidget widget, XmTextPosition position,
  LineTableExtra * extra)
{
  return 0;
}

void
_XmTextMovingCursorPosition(XmTextWidget tw, XmTextPosition position)
{
  OutputData o=Text_OutputData(tw);
  (*Text_Output(tw)->DrawInsertionPoint)(tw, position, Out_BlinkState(o));
}

Boolean
_XmTextGetBaselines(Widget widget, Dimension ** baselines, int *line_count)
{
  return False;
}












