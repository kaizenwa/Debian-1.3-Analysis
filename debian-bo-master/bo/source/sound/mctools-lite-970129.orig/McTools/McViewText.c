/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.  If
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
*/


#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xatom.h>
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McDebug.h"
#include "McSlider.h"
#include "McViewText.h"
#include "McSelection.h"
#include "McBuffer.h"

/**************************************************************************/

#define VIEWTEXT ((McViewText *)(gadget->specialInfo))

static void ViewTextCleanup(McGadget *gadget);
static int  ViewTextEvent(McGadget *gadget, XEvent *event);
static void RemakePointers(McViewText *viewtext);
static void viewtext_slider_update(McGadget *gadget, McViewText *viewtext);
static int ViewTextKey(McGadget *gadget, XKeyEvent *event);

/**************************************************************************/

McSpecialInfo *McCreateViewText(unsigned int flags, XFontStruct *font,
				unsigned char *txt) {
  McViewText *viewtext = (McViewText *) calloc(sizeof(McViewText), 1);
  viewtext->flags = flags;
  viewtext->text = txt;
  viewtext->now.rowstart = viewtext->now.colstart = -1;
  viewtext->now.rowend   = viewtext->now.colend   = -1;
  viewtext->now.rowclick = viewtext->now.colclick = -1;
  viewtext->specialInfo.updateProc = McViewTextUpdate;
  viewtext->specialInfo.eventProc = ViewTextEvent;
  viewtext->specialInfo.keyboardProc = ViewTextKey;
  viewtext->specialInfo.cleanupProc = ViewTextCleanup;
  viewtext->font=font;

  RemakePointers(viewtext);

  return (McSpecialInfo *)viewtext;
}

static void ViewTextCleanup(McGadget *gadget) {
  McViewText *viewtext = (McViewText *)gadget->specialInfo;
  if (viewtext->lines) free(viewtext->lines);
  free(gadget->specialInfo);
}

/**************************************************************************/

static void RemakePointers(McViewText *viewtext) {
  int direction, ascent, descent;
  XCharStruct overall;
  unsigned char *match, *text=viewtext->text;
  int linemax, len;

  if (viewtext->lines && viewtext->lines->text==viewtext->text) return;

  if (viewtext->lines) free(viewtext->lines);

  viewtext->linecnt=viewtext->maxwidth=viewtext->maxheight=0;
  viewtext->lines=NULL;

  if (!viewtext->text) return;

  linemax=32;
  viewtext->lines=malloc(linemax*sizeof(McViewLine));

  while(*text) {

    match=text;
    while(*text && (*text!='\n')) text++;
    len=text-match;
    if (text[-1]=='\r') len--;
    if (*text) text++;

    XTextExtents(viewtext->font, match, len,
		 &direction, &ascent, &descent, &overall);

    viewtext->lines[viewtext->linecnt].text=match;
    viewtext->lines[viewtext->linecnt].len=len;
    viewtext->lines[viewtext->linecnt].width=overall.width;
    viewtext->lines[viewtext->linecnt].height=ascent+descent;
    viewtext->linecnt++;

    viewtext->maxheight+=ascent+descent;
    if (overall.width>viewtext->maxwidth) viewtext->maxwidth=overall.width;

    if (viewtext->linecnt>=linemax) {
      linemax<<=1;
      viewtext->lines=realloc(viewtext->lines, linemax*sizeof(McViewLine));
    }
  }
}  

/**************************************************************************/

void McViewTextSetText(McGadget *gadget, unsigned char *text) {
  McViewText *viewtext = (McViewText *)gadget->specialInfo;
  viewtext->text=text;
  RemakePointers(viewtext);
  McViewTextUpdate(gadget, 0, 1);
}

/**************************************************************************/

void McViewTextUpdate(McGadget *gadget, int busy, int all) {
  McViewText *viewtext = (McViewText *)gadget->specialInfo;
  McViewLine *lines = viewtext->lines;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  GC gc1, gc3, gcx;
  int y, maxy, row, col, height;
  XRectangle rect;
  unsigned char *text; int len;
  int direction, ascent, descent;
  XCharStruct overall;

  rect.x=BW; rect.y=BW;
  rect.width=gadget->width-BW-BW; rect.height=gadget->height-BW-BW;

  if (gadget->flags & GAD_SELECTED) {
    gc1=app->gc[GC_SELECTED_BITMAP];
    gc3=app->gc[GC_SELECTED];
  } else {
    gc1=app->gc[GC_NORMAL];
    gc3=app->gc[GC_CLEAR];
  }
  XSetFont(app->display, gc1, viewtext->font->fid);
  XSetFont(app->display, gc3, viewtext->font->fid);

  if (!(viewtext->firstrow)) {
    XFillRectangle(app->display, gadget->win, gc3,
		   BW, 1, gadget->width-BW-BW, BW-1);
  }

  gcx=app->gc[GC_BRIGHT];

  row=0;
  for(y=0; y<=viewtext->firstrow; y+=lines[row-1].height) row++;

  row--;
  y=BW-(viewtext->firstrow - (y-lines[row].height));

  col=viewtext->firstcol;
  maxy=gadget->height-BW;

  while((y<maxy) && (row<viewtext->linecnt)) {
    height=lines[row].height;
    text=lines[row].text; len=lines[row].len;

    if ((row>viewtext->now.rowstart) && (row<viewtext->now.rowend)) {
      if (col<BW)
	XFillRectangle(app->display, gadget->win, gc1, 0, y, BW-col, height);
      XTextExtents(viewtext->font, text, len,
		   &direction,&ascent,&descent,&overall);
      XDrawImageString(app->display, gadget->win, gc3,
		  BW-col, y+viewtext->font->ascent,
		  text, len);
      XFillRectangle(app->display, gadget->win, gc1,
		     BW+overall.width-col, y, -1, height);
    } else {
      int start, end, startx;

      if (col<BW)
	XFillRectangle(app->display, gadget->win, gc3, 0, y, BW-col, height);
      XTextExtents(viewtext->font, text, len,
		   &direction,&ascent,&descent,&overall);
      XDrawImageString(app->display, gadget->win, gc1,
		  BW-col, y+viewtext->font->ascent,
		  text, len);
      XFillRectangle(app->display, gadget->win, gc3,
		     BW+overall.width-col, y, -1, height);

      if (row==viewtext->now.rowend) {
	end=viewtext->now.colend;
	if (end>len) {
	  XFillRectangle(app->display, gadget->win, gc1,
			 BW-col+lines[row].width, y,
			 col+gadget->width, height);
	  end--;
	} else {
	  end++;
	}
      } else {
	end=0;
      }

      if (row==viewtext->now.rowstart) {
	start=viewtext->now.colstart;
	XTextExtents(viewtext->font, text, start,
		     &direction,&ascent,&descent,&overall);
	startx=overall.width; 

	if (row!=viewtext->now.rowend) {
	  XFillRectangle(app->display, gadget->win, gc1,
			 BW-col+lines[row].width, y,
			 col+gadget->width, height);
	  end=len;
	}
      } else {
	start=startx=0;
      }

      if ((end-start>0) && len) {
	XDrawImageString(app->display, gadget->win, gc3,
			 BW-col+startx, y+viewtext->font->ascent,
			 text+start, end-start);
      }
    }
    row++;
    y+=height;
  }

  if (y<=maxy) {
    XFillRectangle(app->display, gadget->win, gc3,
		   BW, y, gadget->width-BW-BW, maxy-y+1);
  }

  if (y<maxy)
    XFillRectangle(app->display, gadget->win, gc3,
		   BW, y, gadget->width-BW-BW, maxy-y);

  XSetFont(app->display, gc1, app->defaultFont->fid);
  XSetFont(app->display, gc3, app->defaultFont->fid);

  viewtext_slider_update(gadget, viewtext);

  McDrawFocusMark(gadget,
		  (gadget->flags&GAD_SELECTED)?COL_SELECTED:COL_BACKGROUND);
}

/**************************************************************************/

static int SetCursor(McGadget *gadget, int mx, int my) {
  int direction, ascent, descent;
  XCharStruct overall;
  int row, col, y, len, of;
  McViewText *viewtext;
  McViewLine *lines;
  unsigned char *data;

  /* Inside the gadget? */
  mx-=BW; if (mx<0) mx=0;
  my-=BW; if (my<0) my=0;
  if (mx>gadget->width-BW-BW)  mx=gadget->width-BW-BW;
  if (my>gadget->height-BW-BW) my=gadget->height-BW-BW;

  viewtext = (McViewText *)gadget->specialInfo;
  lines = viewtext->lines;

  /* Search for the first row being displayed */
  row=0;
  for(y=0; y<=viewtext->firstrow; y+=lines[row].height) row++;
  row--;
  my+=lines[row].height+viewtext->firstrow-y;

  /* Now search the row the pointer is in */
  for(y=0; y<=my; y+=lines[row].height) row++;
  row--;

  /* valid? */
  if ((row<0) || (row>=viewtext->linecnt)) return 1;

  /* Which column?
   * Since the characters (may) have different widths, we perform a binary
   * search to find the actual position.
   */
  mx+=viewtext->firstcol-(viewtext->font->max_bounds.width/2);
  data=lines[row].text;
  of=len=lines[row].len;
  col=0;
  if (of) {
    while(of) {
      XTextExtents(viewtext->font, data, of,
		   &direction, &ascent, &descent, &overall);
      if (mx>=overall.width) {
	data+=of;
	mx-=overall.width;
	col+=of;
	if (col>=len) { col=len+1; break; }
      }
      if (of&1 && of>1) of++;
      of=(of>>1);
    }
  } else {
    if (mx>0) col=1;
  }

  memcpy(&viewtext->last, &viewtext->now, sizeof(viewtext->now));

  /* phew... yeshar'click? ---> */
  if (viewtext->now.rowclick<0) {
    viewtext->now.rowclick=viewtext->now.rowstart=viewtext->now.rowend=row;
    viewtext->now.colclick=viewtext->now.colstart=viewtext->now.colend=col;
  } else {
    if (row<viewtext->now.rowclick) {
      viewtext->now.rowstart=row;
      viewtext->now.rowend=viewtext->now.rowclick;
      viewtext->now.colstart=col;
      viewtext->now.colend=viewtext->now.colclick;
    } else if (row>viewtext->now.rowclick) {
      viewtext->now.rowstart=viewtext->now.rowclick;
      viewtext->now.rowend=row;
      viewtext->now.colstart=viewtext->now.colclick;
      viewtext->now.colend=col;
    } else {
      viewtext->now.rowstart=viewtext->now.rowclick;
      viewtext->now.rowend=viewtext->now.rowclick;
      if (col<viewtext->now.colclick) viewtext->now.colstart=col;
      else viewtext->now.colstart=viewtext->now.colclick;
      if (col>viewtext->now.colclick) viewtext->now.colend=col;
      else viewtext->now.colend=viewtext->now.colclick;
    }
  }

#if 0
  printf("m:%d/%d  start:%d/%d  click:%d/%d  end:%d/%d\n", row, col,
	 viewtext->now.rowstart,viewtext->now.colstart,
	 viewtext->now.rowclick,viewtext->now.colclick,
	 viewtext->now.rowend,viewtext->now.colend);
#endif
  if (memcmp(&viewtext->last, &viewtext->now, sizeof(viewtext->now)))
    McViewTextUpdate(gadget, 0, 0);

  return 1;
}

/**************************************************************************/

static void MakeSelection(McGadget *gadget) {
  McViewText *viewtext = (McViewText *)gadget->specialInfo;
  McViewLine *lines=viewtext->lines;
  int start,len,nl,i;
  McBuffer buffer;

  if (viewtext->now.colstart<0) return;

  memset(&buffer, 0, sizeof(McBuffer));

  start=viewtext->now.colstart; nl=0;
  if (viewtext->now.rowstart==viewtext->now.rowend) {
    len=viewtext->now.colend-start+1;
    if (start+len>lines[viewtext->now.rowstart].len) {
      if (start+len>lines[viewtext->now.rowstart].len+1) nl=1;
      len=lines[viewtext->now.rowstart].len-start;
    }
    if (len>0)
      mbwrite(&lines[viewtext->now.rowstart].text[start], 1, len, &buffer);
    if (nl) mbputc('\n', &buffer);
  } else {
    len=lines[viewtext->now.rowstart].len-start;
    if (len>0)
      mbwrite(&lines[viewtext->now.rowstart].text[start], 1, len, &buffer);
    mbputc('\n', &buffer);
    for(i=viewtext->now.rowstart+1; i<viewtext->now.rowend; i++) {
      mbwrite(lines[i].text, 1, lines[i].len, &buffer);
      mbputc('\n', &buffer);
    }
    len=viewtext->now.colend+1; nl=0;
    if (len>lines[viewtext->now.rowend].len) {
      if (len>lines[viewtext->now.rowend].len+1) nl=1;
      len=lines[viewtext->now.rowend].len;
    }
    if (len>0) mbwrite(lines[viewtext->now.rowend].text, 1, len, &buffer);
    if (nl) mbputc('\n', &buffer);
  }

  if (buffer.len) McSetNewSelection(gadget, buffer.data, buffer.len);

  free(buffer.data);
}

static void ClearSelection(McGadget *gadget) {
  McViewText *viewtext = (McViewText *)gadget->specialInfo;
  viewtext->now.rowstart = viewtext->now.colstart = -1;
  viewtext->now.rowend   = viewtext->now.colend   = -1;
  viewtext->now.rowclick = viewtext->now.colclick = -1;
  McViewTextUpdate(gadget, 0, 0);
}

/**************************************************************************/

static int ViewTextEvent(McGadget *gadget, XEvent *event) {
  McViewText *viewtext = (McViewText *)gadget->specialInfo;
  McWindow *mcw=gadget->mcw;

  switch(event->type) {
  case Expose:
    return 0;

  case SelectionClear:
    if (gadget==mcw->selectionOwner) {
      McClearSelection(mcw);
      ClearSelection(gadget);
      return 1;
    }
    return 0;

  case ButtonPress:
    if (event->xbutton.button!=1) return 0;
    if (McInRectangle(event->xbutton.x, event->xbutton.y,
		      0, 0, gadget->width, gadget->height)) {
      if (!(event->xbutton.state&ShiftMask))
	viewtext->now.rowclick=viewtext->now.colclick=-1;
      gadget->flags|=GAD_PRESSED;
      McMoveGadgetToStart(gadget);
      McSetFocus(gadget);
      return SetCursor(gadget, event->xbutton.x, event->xbutton.y);
    }
    return 0;

  case MotionNotify:
    if (!(gadget->flags&GAD_PRESSED)) return 0;
    SetCursor(gadget, event->xmotion.x, event->xmotion.y);
    return 1;

  case ButtonRelease:
    if (event->xbutton.button!=1) return 0;
    if (!(gadget->flags&GAD_PRESSED)) return 0;
    gadget->flags&=~GAD_PRESSED;
    SetCursor(gadget, event->xbutton.x, event->xbutton.y);
    MakeSelection(gadget);
    return 1;
  }
  return 0;
}

/**************************************************************************/

static int ViewTextKey(McGadget *gadget, XKeyEvent *event) {
  int up=0, maxh, maxw;
  McWindow *mcw=gadget->mcw;
  McViewText *viewtext = (McViewText *)gadget->specialInfo;

  maxh=viewtext->maxheight-(gadget->height-BW-BW);
  maxw=viewtext->maxwidth-(gadget->width-BW-BW);

  switch((int)mcw->keysym) {
  case XK_Home:
    if (viewtext->firstrow || viewtext->firstcol) {
      viewtext->firstrow=0;
      viewtext->firstcol=0;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_End:
    if (viewtext->firstrow<maxh) {
      viewtext->firstrow=maxh;
      viewtext->firstcol=0;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Prior:
    if (viewtext->firstrow>0) {
      viewtext->firstrow-=
	(((gadget->height-BW-BW)/viewtext->lines[0].height)-1)*
	viewtext->lines[0].height;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Next:
    if (viewtext->firstrow<maxh) {
      viewtext->firstrow+=
	(((gadget->height-BW-BW)/viewtext->lines[0].height)-1)*
	viewtext->lines[0].height;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Up:
    if (viewtext->firstrow>0) {
      viewtext->firstrow-=viewtext->lines[0].height;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Down:
    if (viewtext->firstrow<maxh) {
      viewtext->firstrow+=viewtext->lines[0].height;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Left:
    if (viewtext->firstcol>0) {
      viewtext->firstcol-=maxw/20;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Right:
    if (viewtext->firstcol<maxw) {
      viewtext->firstcol+=maxw/20;
      up=2;
    } else {
      up=1;
    }
    break;

  }

  if (up&2) {
    if (viewtext->firstrow>maxh) viewtext->firstrow=maxh;
    if (viewtext->firstrow<0) viewtext->firstrow=0;
    if (viewtext->firstcol>maxw) viewtext->firstcol=maxw;
    if (viewtext->firstcol<0) viewtext->firstcol=0;
    McViewTextUpdate(gadget, 0, 1);
  }

  return !!up;
}

/***************************************************************************/

McGadget *MakeViewText(McWindow *mcw, int x, int y, int w, int h,
		       XFontStruct *font, char *text) {
  McGadget *gadget;

  if (w<=0) w=mcw->w-(BW<<2);
  if (h<=0) h=mcw->h-(BW<<2);
  if (x==0) x=(mcw->w>>1)-(w>>1);	/* Center */
  if (y==0) y=(mcw->h>>1)-(h>>1);
  if (x<0) x=mcw->w+x-w-BW-BW-BW;	/* Right justify */
  if (y<0) y=mcw->h+y-h-BW-BW-BW;

  gadget = McCreateGadget(mcw, GAD_3D|GAD_H3D|GAD_ACTIVE,
			  VIEWTEXTGADGET, x, y, w, h);
  gadget->specialInfo = McCreateViewText(0, font, text);
  return gadget;
}

/**************************************************************************/

static void slider_ver_proc(McGadget *gadget);
static void slider_hor_proc(McGadget *gadget);

void McViewTextBindSliders(McGadget *txtgad,
			   McGadget *sliderhor, McGadget *sliderver) {
  McViewText *viewtext = (McViewText *)txtgad->specialInfo;
  viewtext->sliderhor = sliderhor;
  viewtext->sliderver = sliderver;
  if (sliderhor) {
    sliderhor->callbackUp = sliderhor->callbackDown = slider_hor_proc;
    sliderhor->customData = txtgad;
  }
  if (sliderver) {
    sliderver->callbackUp = sliderver->callbackDown = slider_ver_proc;
    sliderver->customData = txtgad;
  }
}

static void viewtext_slider_update(McGadget *gadget, McViewText *viewtext) {
  McGadget *slidergad;

  if ((slidergad=viewtext->sliderhor)) {
    McSlider *slider = (McSlider *)slidergad->specialInfo;
    int max = viewtext->maxwidth - (gadget->width-BW-BW);
    if (max<0) max=0;
    slider->leftValue=slider->rightValue=viewtext->firstcol;
    McSliderSetProps(slidergad, max, viewtext->maxwidth);
    McGadgetUpdate(slidergad);
  }

  if ((slidergad=viewtext->sliderver)) {
    McSlider *slider = (McSlider *)slidergad->specialInfo;
    int max = viewtext->maxheight - (gadget->height-BW-BW);
    if (max<0) max=0;
    slider->leftValue=slider->rightValue=max-viewtext->firstrow;
    McSliderSetProps(slidergad, max, viewtext->maxheight);
    McGadgetUpdate(slidergad);
  }
}

static void slider_hor_proc(McGadget *gadget) {
  McSlider *slider = (McSlider *)gadget->specialInfo;
  McGadget *txtgad = (McGadget *)gadget->customData;
  McViewText *viewtext = (McViewText *)txtgad->specialInfo;
  int col = slider->leftValue;

  if (viewtext->firstcol!=col) {
    viewtext->firstcol = col;
    McViewTextUpdate(txtgad, 0, 1);
    McSetFocus(txtgad);
  }
}

static void slider_ver_proc(McGadget *gadget) {
  McSlider *slider = (McSlider *)gadget->specialInfo;
  McGadget *txtgad = (McGadget *)gadget->customData;
  McViewText *viewtext = (McViewText *)txtgad->specialInfo;
  int row = slider->maxValue-slider->leftValue;

  if (viewtext->firstrow!=row) {
    viewtext->firstrow = row;
    McViewTextUpdate(txtgad, 0, 1);
    McSetFocus(txtgad);
  }
}





