/**
 *
 * $Id: TextStrSo.c,v 1.16 1996/12/19 06:57:27 u27113 Exp $
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

static char rcsid[] = "$Id: TextStrSo.c,v 1.16 1996/12/19 06:57:27 u27113 Exp $";

#include <LTconfig.h>
#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <X11/Xfuncs.h>
#ifdef HAVE_STRING_H
#include <string.h>
#define ANSI_STRING
#else
#include <strings.h>
#endif
#include <stdlib.h>

#include <XmI/DebugUtil.h>

#define SOURCE_ALLOC_SIZE 256

void _XmTextInvalidate(XmTextWidget w, XmTextPosition position, 
		       XmTextPosition topos, long delta);

static char *
StrPtr(XmSourceData d, XmTextPosition pos)
{
  char *ptr;
  
  ptr=d->ptr + pos;
  return ptr;
}

static char *
StrNext(XmSourceData d, XmTextPosition pos)
{
  return d->ptr + pos+1;
}  

static char *
StrPrev(XmSourceData d, XmTextPosition pos)
{
  if (pos>0) return d->ptr + pos+1;
  else return NULL;
}  

/* AddWidget ----------------------------------------------------------------
 
   Method that adds a widget to the source structure so that this new widget
   will be updated as the text in the source changes.
*/

static void
AddWidget(XmTextSource source, XmTextWidget w)
{
  XmSourceData d;

  d=source->data;
  d->numwidgets++;
  d->widgets=(XmTextWidget *)XtRealloc((char *)d->widgets,
    sizeof(XmTextWidget *)*d->numwidgets);
    
  d->widgets[d->numwidgets-1]=w;

  if (d->maxallowed==0) { /* 1st time adding a widget! */
    d->maxallowed=Text_MaxLength(w);

    if (d->value == (char *) XmUNSPECIFIED || d->value == NULL) {
      d->value=XtNewString("");
      d->old_length=0;
    }
    else {
      d->value=XtNewString(d->value);
      d->old_length= (d->value) ? strlen(d->value) : 0;
    }

    d->maxlength=d->old_length+SOURCE_ALLOC_SIZE;
    d->ptr=XtMalloc(d->maxlength+1);
    d->length=d->old_length;
    strcpy(d->ptr,d->value);

    d->gap_start=d->ptr;
    d->gap_end=d->ptr;
  }
  Text_LastPos(w)=d->length;
}

/* RemoveWidget -------------------------------------------------------------

   Removes a widget from a source, but DOES NOT delete the source if removing
   the last widget.
*/
static void
RemoveWidget(XmTextSource source, XmTextWidget w)
{
  XmSourceData d;
  XmTextWidget *get,*store;
  int i,found=0;
  
  d=source->data;
  get=store=d->widgets;
  for (i=0; i<d->numwidgets; i++) {
    if (*get!=w) {
      *store=*get;
      store++;
    }
    else {
      found++;
    }
    
    get++;
  }
  d->numwidgets-=found;
}

/* CountLines ---------------------------------------------------------------

   Counts the number of lines...  Slightly more tricky than at first glance,
   but only slightly.  Stuff having to do with " \n" having 2 lines.  Think
   about it.
   */
static int
CountLines(XmTextSource source, XmTextPosition start, unsigned long len)
{
  XmSourceData d;
  char *ptr;
  int count;
  
  d=source->data;
  ptr=StrPtr(d,start);
  count=1;
  while (len-- > 0) {
    if (*ptr=='\n') count++;
  }
  
  return count;
}

/* ReadSource ---------------------------------------------------------------

   Copies the text in the source to a text block.
   */
static XmTextPosition
ReadSource(XmTextSource source,XmTextPosition pos,XmTextPosition last,
  XmTextBlock block)
{
  XmSourceData d;
  int len;
  
  d=source->data;

  if (((unsigned)last) > ((unsigned)d->length))
	last = d->length;

  if (pos>last) {
    int swap;

    swap=pos;
    pos=last;
    last=swap;
  }
  
  len=last-pos;
  block->length=len;
  block->ptr=XtMalloc(len+1);
  block->format=XmFMT_8_BIT;
  if (len>0) strncpy(block->ptr,&d->ptr[pos],len);
  block->ptr[len]='\0';
  
  return pos;
}

/* Replace ------------------------------------------------------------------

   (And sundry functions)

   Replace text in the source with something else.
   */

static void
CheckSize(XmSourceData d,int len)
{
  if (d->length + len > d->maxlength) {
    int i;
    
    i = SOURCE_ALLOC_SIZE;
    if (i < len)
      i = len;
    d->maxlength += i + 1;
    d->ptr = XtRealloc(d->ptr, d->maxlength);
    
    DBG("*source->CheckSize: Alloced new space\n");
  }
}

static void
Insert(XmSourceData d, XmTextPosition start, char *ptr, int len)
{
  int i;
  
#ifdef	DO_SANITY
/* Sanity check */
  if (start < 0) start = 0;
#endif

  for (i = d->length - 1; i >= start; i--)
    d->ptr[i + len] = d->ptr[i];
  strncpy(&d->ptr[start], ptr, len);
  d->length+=len;
}

static XmTextStatus
Replace(XmTextWidget w,XEvent *ev,
  XmTextPosition *startret,XmTextPosition *endret,
  XmTextBlock block,Boolean call_callback)
{
  XmSourceData d;
  int i;
  XmTextPosition start,end;

  d=Text_Source(w)->data;
  start=*startret;
  end=*endret;
  _XmTextDisableRedisplay(w, True);
  if (start>end) {
    int swap;

    swap=start;
    start=end;
    end=swap;
  }
  if (end>d->length) end=d->length;
  if (start>d->length) return EditError;
  if (d->length+(end-start)+block->length > d->maxallowed) return EditError;

  if (end>start) { /* we need to delete some stuff */
    char *dest,*source,*last;

    dest=d->ptr+start;
    source=d->ptr+end;
    last=d->ptr+d->length;
    while (source<last)
      *dest++=*source++;

    d->length-=end-start;
  }

  if (block && block->length>0) {
    CheckSize(d,block->length);
    Insert(d,start,block->ptr,block->length);
  }
  
  for (i=0; i<d->numwidgets; i++) {
    XmTextVerifyCallbackStruct cbs;
    XmTextWidget w;
    
    w=d->widgets[i];
    Text_LastPos(w)=d->length;
    if (call_callback) {
      if (Text_ValueChangedCallback(w)) {
        cbs.reason = XmCR_VALUE_CHANGED;
        cbs.currInsert = cbs.newInsert = start;
        cbs.startPos = cbs.endPos = start;
        cbs.text = block;
        XtCallCallbacks((Widget)w, XmNvalueChangedCallback, &cbs);
      }
    }
  }

  _XmTextUpdateLineTable((Widget) w, start, end, block, True);
  _XmTextInvalidate(w, start, end, block->length - end + start);
  _XmTextEnableRedisplay(w);
  return EditDone;
}

/* Scan ---------------------------------------------------------------------

   Search the source for something: (either forwards or backwards)

     XmSELECT_POSITION
     XmSELECT_WHITESPACE
     XmSELECT_WORD
     XmSELECT_LINE
     XmSELECT_ALL
     XmSELECT_PARAGRAPH
*/
#define InWhiteSpace(a) (a==' ' || a=='\t' || a=='\n')
#define InWord(a) (!InWhiteSpace(a))

static XmTextPosition
Scan(XmTextSource source, XmTextPosition pos,XmTextScanType type,
  XmTextScanDirection dir,int count,Boolean inc)
{
  XmSourceData d=source->data;
  Boolean found=False;
  char *ptr;
  
  if (pos>d->length) pos=d->length;
  if (count<0) count=d->length;
  if (dir==XmsdLeft) {
    while (count>0 && pos>0) {
      ptr=StrPtr(d,pos-1);
      switch (type) {
        case XmSELECT_WHITESPACE:
          if (!InWhiteSpace(*ptr)) found=True;
          break;
        case XmSELECT_WORD:
          if (!InWord(*ptr)) found=True;
          break;
        case XmSELECT_LINE:
          if (*ptr=='\n') found=True;
          break;
        case XmSELECT_PARAGRAPH:
          if (*ptr=='\n') found=True;
          break;          
        default:
          found=True;
          break;
      }
      if (found) {
        return pos;
      }
      
      pos--;
      count--;
    }
  }
  else {
    while (count>0 && pos<d->length) {
      ptr=StrPtr(d,pos);
      switch (type) {
        case XmSELECT_WHITESPACE:
          if (!InWhiteSpace(*ptr)) found=True;
          break;
        case XmSELECT_WORD:
          if (!InWord(*ptr)) found=True;
          break;
        case XmSELECT_LINE:
          if (*ptr=='\n') found=True;
          break;
        case XmSELECT_PARAGRAPH:
          if (*ptr=='\n') found=True;
          break;          
        default:
          found=True;
          break;
      }
      if (found) {
        return pos;
      }
      
      pos++;
      count--;
    }
  }
  
  return pos;
}

static Boolean
GetSelection(XmTextSource source, XmTextPosition *left,
  XmTextPosition *right)
{
    return False;
}

static void
SetSelection(XmTextSource source, XmTextPosition left,
  XmTextPosition right, Time time)
{
}

XmTextSourceRec sourceRec = {
    /* _XmSourceDataRec      */ NULL,
    /* AddWidgetProc         */ AddWidget,
    /* CountLinesProc        */ CountLines,
    /* RemoveWidgetProc      */ RemoveWidget,
    /* ReadProc              */ ReadSource,
    /* ReplaceProc           */ Replace,
    /* ScanProc              */ Scan,
    /* GetSelectionProc      */ GetSelection,
    /* SetSelectionProc      */ SetSelection
};

XmTextSource
_XmStringSourceCreate(char *value, Boolean is_wchar)
{
  XmTextSource source;
  XmSourceData d;
  

  source = (XmTextSource) XtMalloc(sizeof(XmTextSourceRec));
  bcopy(&sourceRec, source, sizeof(XmTextSourceRec));

  d=(XmSourceData) XtMalloc(sizeof(XmSourceDataRec));
  d->source=source;
  d->widgets=NULL;
  d->numwidgets=0;
  d->value=value;
  d->ptr=d->gap_start=d->gap_end=NULL;
  d->length=d->old_length=d->maxlength=d->maxallowed=0;
  d->hasselection=False;
  
  source->data=d;
  
  return source;
}



/*
 * Quasi-Public functions -----------------------------------------------------
 */


void
_XmStringSourceDestroy(XmTextSource source)
{
  XmSourceData d = source->data;

  if (d->old_length>0) XtFree(d->value);
  if (d->length>0) XtFree(d->ptr);
  XtFree((char *)d);
  XtFree((char *)source);
}

char *
_XmStringSourceGetValue(XmTextSource source, Boolean want_wchar)
{
  XmSourceData d=source->data;
  char *txt;
  
  txt=XtMalloc(d->length+1);

  strncpy(txt, d->ptr, d->length);
  txt[d->length] = '\0';

  XdbDebug(__FILE__, NULL, "_XmStringSourceGetValue => %p '%s'\n", txt, txt);

  return txt;
}

void
_XmStringSourceSetValue(XmTextWidget w, char *value)
{
  XmTextSource source=Text_Source(w);
  XmSourceData d = source->data;
  int len;

  len = value ? strlen(value) : 0;
  d->length=0;
  d->gap_start=d->ptr;
  d->gap_end=d->ptr;
  CheckSize(d,len);

  if (len)
	Insert(d,0,value,len);  
}

Boolean
_XmStringSourceHasSelection(XmTextSource source)
{
  return False;
}

Boolean
_XmStringSourceGetEditable(XmTextSource source)
{
  XmSourceData d;

  d=source->data;
  return d->editable;
}

void
_XmStringSourceSetEditable(XmTextSource source, Boolean editable)
{
  XmSourceData d;

  d=source->data;
  d->editable=editable;
}

int
_XmStringSourceGetMaxLength(XmTextSource source)
{
  XmSourceData d;

  d=source->data;
  return d->maxlength;
}

void
_XmStringSourceSetMaxLength(XmTextSource source, int max)
{
  XmSourceData d;

  d=source->data;
  if (max>d->length) {
    XmTextBlockRec block;

    block.ptr="";
    block.length=0;
    block.format=XmFMT_8_BIT;
/*     (*source->Replace)(source, NULL, NULL, max, d->length, &block, True); */
  }
  d->maxallowed=max;
}

char *
_XmStringSourceGetString(XmTextWidget w,
			 XmTextPosition from,
			 XmTextPosition to,
			 Boolean want_wchar)
{
  return NULL;
}


Boolean
_XmTextFindStringBackwards(Widget w,
			   XmTextPosition start,
			   char *search_string,
			   XmTextPosition *position)
{
  return False;
}

Boolean
_XmTextFindStringForwards(Widget w,
			  XmTextPosition start,
			  char *search_string,
			  XmTextPosition *position)
{
  return False;
}

Boolean
_XmStringSourceFindString(Widget w,
			  XmTextPosition start,
			  char *string,
			  XmTextPosition *position)
{
  return False;
}

void
_XmStringSourceSetGappedBuffer(XmSourceData data, XmTextPosition position)
{
}

Boolean
_XmTextModifyVerify(XmTextWidget initiator,
		    XEvent *event,
		    XmTextPosition *start,
		    XmTextPosition *end,
		    XmTextPosition *cursorPos,
		    XmTextBlock block,
		    XmTextBlock newblock,
		    Boolean *freeBlock)
{
  return False;
}

