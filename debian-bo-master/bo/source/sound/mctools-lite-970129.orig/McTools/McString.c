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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xatom.h>
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McString.h"
#include "McDebug.h"
#include "McSelection.h"

#define STRING ((McString *)(gadget->specialInfo))

static int McStringEvent(McGadget *gadget, XEvent *event);
static void McStringUpdate(McGadget *gadget, int busy, int all);
static int McStringKey(McGadget *gadget, XKeyEvent *event);
static int process_control(McGadget *gadget, unsigned char key, Time ctrltme);
static int add_text(McGadget *gadget, unsigned char *buf,int l);
static void add_selection_text(McGadget *gadget, unsigned char *str,
			       unsigned long len);
static void SetCursor(McGadget *gadget, int from, int to);
static void McStringCleanup(McGadget *gadget);
static void GadgetChanged(McGadget *gadget);

McSpecialInfo *McCreateString(unsigned int flags, XFontStruct *font,
			      unsigned char *text, int bufsize) {
  int		direction, ascent, descent;
  XCharStruct	overall;

  McString *string = (McString *) calloc(sizeof(McString), 1);
  string->flags = flags;
  string->specialInfo.updateProc = McStringUpdate;
  string->specialInfo.eventProc = McStringEvent;
  string->specialInfo.keyboardProc = McStringKey;
  string->specialInfo.cleanupProc = McStringCleanup;
  string->specialInfo.flags = GSP_FOCUSUPDATE;
  string->font = font;
  string->buf = (unsigned char *)calloc(bufsize, 1);
  if (!text) text="";
  strncpy(string->buf, text, bufsize-1);
  string->bufsize = bufsize;
  string->len = string->cursor = string->cursor_click =
    string->cursor_end = strlen(text);
  string->oldcursor = string->oldlen = string->oldcursor_end = -1;
  string->offset = 0;

  XTextExtents(string->font, " ", 1, &direction, &ascent, &descent, &overall);
  string->cursorWidth=overall.width;

  return (McSpecialInfo *)string;
}

static void McStringCleanup(McGadget *gadget) {
  McString *string = (McString *)gadget->specialInfo;
  if (string->buf)
    free(string->buf);
  free(string);
}

void McStringSetText(McGadget *gadget, unsigned char *text) {
  McString *string = (McString *)gadget->specialInfo;

  if (text) strncpy(string->buf, text, string->bufsize-1);
  string->len=strlen(string->buf);
  gadget->flags&=~GAD_CHANGED;
  SetCursor(gadget, string->len, -1);
  McStringUpdate(gadget, 0, 1);
}

/*
 * Hmmm... should I have another set of 3 GC's, which violates the rule
 * of having as few GC's as possible, or should I always change the
 * clipmasks for those GC's, violating the rule of having as less
 * traffic as possible...
 */
static void McStringUpdate(McGadget *gadget, int busy, int all) {
  int		direction, ascent, descent;
  XCharStruct	overall;
  XRectangle	rect[1];
  McString *string = (McString *)gadget->specialInfo;
  int xx,x,y,w,gw,offs,max,bgcol;
  GC gc, gc1, gc2, gc3;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;

  if (gadget->flags & GAD_SELECTED) {
    gc1=app->gc[GC_SELECTED];
    gc2=app->gc[GC_NORMAL];
    gc3=app->gc[GC_SELECTED_BITMAP];
    bgcol=COL_SELECTED;
  } else {
    gc1=app->gc[GC_CLEAR];		/* Background, selected text */
    gc2=app->gc[GC_SELECTED];
    gc3=app->gc[GC_NORMAL];
    bgcol=COL_BACKGROUND;
  }

  gc=XCreateGC(app->display, gadget->win, 0, NULL);
  XCopyGC(app->display, gc1, ~0, gc);
  XSetFont(app->display, gc, string->font->fid);
  gc1=gc;
  gc=XCreateGC(app->display, gadget->win, 0, NULL);
  XCopyGC(app->display, gc2, ~0, gc);
  XSetFont(app->display, gc, string->font->fid);
  gc2=gc;
  gc=XCreateGC(app->display, gadget->win, 0, NULL);
  XCopyGC(app->display, gc3, ~0, gc);
  XSetFont(app->display, gc, string->font->fid);
  gc3=gc;

  if (app->style) {
    XSetForeground(app->display, gc1, app->colors[COL_BRIGHT]);
    XSetBackground(app->display, gc1, app->colors[COL_FOREGROUND]);
    XSetBackground(app->display, gc3, app->colors[COL_BRIGHT]);
    bgcol=COL_BRIGHT;
  }

  offs=string->offset;

  XTextExtents(string->font, string->buf, string->cursor, &direction, &ascent,
	       &descent, &overall);
  x = BW - string->offset + overall.width;
  gw=gadget->width>>2;
  if (x>=(max=(gadget->width-BW-BW-BW))) {
				    /* Double check, to avoid lock-up when */
				    /* the gadget is too small. */
    while (x>=max) { string->offset+=gw; x-=gw; }
  } else {
    max=BW;
    while (x<max) { string->offset-=gw; x+=gw; }
  }
  if (string->offset<0) string->offset=0;

  if ((string->offset!=offs) || (string->oldlen!=string->len)) {
    all=1;
    string->oldlen=string->len;
  }

  x = BW - string->offset;
  y = ((gadget->height-descent-ascent)>>1);
  rect[0].x=BW; rect[0].y=BW;
  rect[0].width=gadget->width-BW-BW;
  rect[0].height=gadget->height-BW-BW;

  if (all) {
    XFillRectangle(app->display, gadget->win, gc1, 0, 0, -1, y);
    XFillRectangle(app->display, gadget->win, gc1, 0, y+descent+ascent,-1,-1);

    if (string->offset<=0)
      XFillRectangle(app->display, gadget->win, gc1, 0, y, BW, ascent+descent);
    XTextExtents(string->font, string->buf, string->len, &direction, &ascent,
		 &descent, &overall);
    XDrawImageString(app->display, gadget->win, gc3,
		     x, y+ascent, string->buf, string->len);

    w = gadget->width - overall.width - BW + string->offset;
    if (w>=0)
      XFillRectangle(app->display, gadget->win, gc1, x+overall.width,
		     y, w, ascent+descent);
  }

  if (string->flags & CURSOR_VISIBLE) {
    XTextExtents(string->font, string->buf, string->oldcursor,
		 &direction, &ascent, &descent, &overall);

    xx=x+overall.width;

    if (string->oldcursor>=string->len) {
      XFillRectangle(app->display, gadget->win, gc1,
		     xx,y,string->cursorWidth,descent+ascent);
    } else {
      XDrawImageString(app->display, gadget->win, gc3,
		       xx, y+ascent, &string->buf[string->oldcursor],
		       string->oldcursor_end-string->oldcursor+1);
    }
    string->flags&=~CURSOR_VISIBLE;
  }

  if (mcw->keyboardFocus==gadget) {
    XTextExtents(string->font, string->buf, string->cursor,
		 &direction, &ascent, &descent, &overall);
    xx=x+overall.width;
    if (string->cursor>=string->len) {
      if (mcw->window_has_focus) {
	XFillRectangle(app->display, gadget->win, gc2,
		       xx,y,string->cursorWidth,descent+ascent);
      } else {
	XDrawRectangle(app->display, gadget->win, gc2,
		       xx,y,string->cursorWidth-1,descent+ascent-1);
      }
    } else {
      if (mcw->window_has_focus) {
	XDrawImageString(app->display, gadget->win, gc1,
			 xx, y+ascent, &string->buf[string->cursor],
			 string->cursor_end-string->cursor+1);
      } else {
	XTextExtents(string->font, string->buf+string->cursor,
		     string->cursor_end-string->cursor+1,
		     &direction, &ascent, &descent, &overall);
	XDrawRectangle(app->display, gadget->win, gc2,
		       xx,y,overall.width-1,descent+ascent-1);
      }
    }
    string->flags|=CURSOR_VISIBLE;
  }

  string->oldcursor=string->cursor;
  string->oldcursor_end=string->cursor_end;

  XFreeGC(app->display, gc1);
  XFreeGC(app->display, gc2);
  XFreeGC(app->display, gc3);

  McDrawFocusMark(gadget, bgcol);
}

static int McFindPos(McGadget *gadget, int x, int y) {
  int		direction, ascent, descent;
  XCharStruct	overall;
  McString *string = (McString *)gadget->specialInfo;
  unsigned char *p=string->buf;
  int pos=0,of;

  if ((x=x+string->offset-BW)<=0) return 0;
  if (!(of=string->len)) return 0;

  /*
   * Since the characters have different widths, we perform a binary search
   * to find the actual position.
   * This could also be done by calculating the sizes for each char, using
   * XFontstructs, but I don't like to do that now... :)
   */
  while(of) {
    XTextExtents(string->font, p, of,&direction,&ascent,&descent,&overall);
    if (x>=overall.width) {
      p+=of;
      x-=overall.width;
      pos+=of;
      if (pos>=string->len) return string->len;
    }
    if (of&1 && of>1) of++;
    of=(of>>1);
  }
  return pos;
}

static void SetCursor(McGadget *gadget, int from, int to) {
  McString *string = (McString *)gadget->specialInfo;
  string->cursor=from;
  if (to<0) {
    string->cursor_click=to=from;
  }
  string->cursor_end=to;
}

static void McSetPos(McGadget *gadget, int x, int y) {
  McString *string = (McString *)gadget->specialInfo;
  int i=McFindPos(gadget, x, y);
  if (string->cursor!=i) {
    SetCursor(gadget,i,-1);
    McGadgetUpdate(gadget);
  }
}

static int McStringEvent(McGadget *gadget, XEvent *event) {
  McString *string = (McString *)gadget->specialInfo;
  unsigned char *slct;
  int i;
  int wanted;
  McWindow *mcw=gadget->mcw;

  if (event->type == Expose) return 0;

  switch (event->type) {
    case SelectionClear:
    if (gadget==mcw->selectionOwner) {
      McClearSelection(mcw);
      SetCursor(gadget, string->cursor, -1);
      McGadgetUpdate(gadget);
      return 1;
    }
    return 0;
  }

  if (mcw->keyboardFocus==gadget) {

    switch (event->type) {
    case SelectionNotify:
      if (event->xselection.property==None) return 0;
      wanted=string->bufsize-string->len+3;
      slct=McReadSelection(gadget, &wanted);
      add_selection_text(gadget, slct, wanted);
      return 1;

    case ButtonPress:
      if ((event->xbutton.button!=1) && (event->xbutton.button!=2)) return 0;
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	switch (event->xbutton.button) {
	case 1:
	  gadget->flags|=GAD_PRESSED;
	  McSetPos(gadget, event->xbutton.x,event->xbutton.y);
	  string->cursor_click=string->cursor;
	  break;
	case 2:
	  McSetPos(gadget, event->xbutton.x,event->xbutton.y);
	  string->cursor_click=string->cursor;
	  wanted=string->bufsize-string->len+3;
	  slct=McQuerySelection(gadget, event->xbutton.time, &wanted);
	  add_selection_text(gadget, slct, wanted);
	  break;
	}
	return 1;
      }
      return 0;

    case ButtonRelease:
      if ((event->xbutton.button!=1) && (event->xbutton.button!=2)) return 0;
      if (gadget->flags & GAD_PRESSED) {
	gadget->flags&=~GAD_PRESSED;
	if (string->cursor != string->cursor_end) {
	  int len;
	  if ((len=string->cursor_end-string->cursor+1)>0) {
	    if (!McSetNewSelection(gadget, &string->buf[string->cursor], len))
	      SetCursor(gadget, string->cursor_click, -1);
	  }
	}	    
	return 1;
      }
      return 0;

    case MotionNotify:
      i=McFindPos(gadget, event->xmotion.x,event->xmotion.y);
      if (gadget->flags & GAD_PRESSED) {
	if (i>=string->cursor_click) {
	  SetCursor(gadget, string->cursor_click, i);
	  McGadgetUpdate(gadget);
	  return 1;
	}
	if (i<string->cursor_click) {
	  SetCursor(gadget, i, string->cursor_click);
	  McGadgetUpdate(gadget);
	  return 1;
	}
	return 1;
      }
      return 0;
    default:
      return 0;
    }
  } else {
    switch (event->type) {
    case ButtonPress:
      if ((event->xbutton.button!=1) && (event->xbutton.button!=2)) return 0;
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	gadget->flags|=GAD_PRESSED;
	McSetFocus(gadget);
	switch (event->xbutton.button) {
	case 1:
	  SetCursor(gadget, McFindPos(gadget, event->xbutton.x,
					  event->xbutton.y),-1);
	break;
      case 2:
	  SetCursor(gadget, McFindPos(gadget, event->xbutton.x,
					  event->xbutton.y),-1);
	  wanted=string->bufsize-string->len+3;
	  slct=McQuerySelection(gadget, event->xbutton.time, &wanted);
	  add_selection_text(gadget, slct, wanted);
	  break;
	}
	McGadgetUpdate(gadget);
	return 1;
      }
      return 0;

    case ButtonRelease:
      if ((event->xbutton.button!=1) && (event->xbutton.button!=2)) return 0;
      gadget->flags&=~GAD_PRESSED;

    case MotionNotify:
      return 0;
    }
  }
  return 0;
}

static void add_selection_text(McGadget *gadget,
			       unsigned char *str, unsigned long len) {
  if (!str) return;
  if (add_text(gadget, str, len))
    McGadgetUpdate(gadget);
  else
    XBell(gadget->mcw->app->display, 0);
  XFree(str);
}

static int add_text(McGadget *gadget, unsigned char *buf, int l) {
  register int i,n;
  register unsigned char *ps,*pd;
  McString *string = (McString *)gadget->specialInfo;

  n=l;
  while(--n>=0) {
    if (buf[n]<32) {
      for (i=n;i<l;i++) buf[i]=buf[i+1];
      l--;
    }
  }

  if (!l) return 0;
  if (string->len+l>=string->bufsize) return 0;

  i=string->len-string->cursor;
  ps=&string->buf[string->len];
  pd=&string->buf[string->len+l];
  while(i--) *--pd=*--ps; /* As long as there are only few bytes,
			   * this should be faster than memcpy, until
			   * someone writes an assembler memcpy */
  
  i=l;
  ps=buf;
  pd=&string->buf[string->cursor];
  while(i--) *pd++=*ps++;

  string->len+=l;
  SetCursor(gadget,string->cursor+l,-1);
  string->buf[string->len]=0;
  GadgetChanged(gadget);
  return 1;
}

static int McStringKey(McGadget *gadget, XKeyEvent *event) {
  int result=0;
  Time ctrltme=event->time;
  McWindow *mcw=gadget->mcw;

  switch((int)mcw->keysym) {
  case XK_BackSpace:
  case XK_Linefeed:
  case XK_Clear:
  case XK_Return:
  case XK_Pause:
  case XK_Scroll_Lock:
  case XK_Escape:
  case XK_Delete:
    result=process_control(gadget, mcw->keysym & 0xFF, ctrltme);
    break;

  case XK_KP_Enter:
    result=process_control(gadget, 13, ctrltme); /* C-M */
    break;

  case XK_Insert:
    result=process_control(gadget, 15, ctrltme); /* C-O */
    break;

  case XK_Begin:
    result=process_control(gadget,   1, ctrltme); /* C-A */
    break;

  case XK_Home:
    result=process_control(gadget, 188, ctrltme); /* M-< */
    break;

  case XK_End:
    result=process_control(gadget, 190, ctrltme); /* M-> */
    break;

  case XK_Up:
    result=process_control(gadget, 16, ctrltme); /* C-P */
    break;

  case XK_Down:
    result=process_control(gadget, 14, ctrltme); /* C-N */
    break;

  case XK_Left:
    result=process_control(gadget, 2, ctrltme); /* C-B */
    break;

  case XK_Right:
    result=process_control(gadget, 6, ctrltme); /* C-F */
    break;

  default:
    if ((mcw->keycnt==1) && ((mcw->keys[0])<32)) {
      result=process_control(gadget, mcw->keys[0], ctrltme);
      break;
    }
    if (add_text(gadget, mcw->keys, mcw->keycnt)) result|=2;
  }

  if (result&2) McGadgetUpdate(gadget);

  return !!result;
}

static int process_control(McGadget *gadget, unsigned char key, Time ctrltme) {
  register int i;
  register unsigned char *ps,*pd,*slct;
  McString *string = (McString *)gadget->specialInfo;
  int wanted;

  switch(key){
  case 188: /* M-< */
  case 1: /* C-A -- beginning-of-line */
    if (string->cursor) {
      SetCursor(gadget,0,-1);
      return 2;
    }
    return 1;

  case 2: /* C-B -- backward-char */
    if (string->cursor) {
      SetCursor(gadget,string->cursor-1,-1);
      return 2;
    }
    return 1;

  case 4: /* C-D -- delete-char */
    if (string->cursor>=string->len) return 1;
    if (!string->len) return 1;
    pd=&string->buf[string->cursor];
    ps=pd+1;
    i=string->len-string->cursor;
    while(i--) *pd++=*ps++;
    string->len--;
    GadgetChanged(gadget);
    return 2;

  case 190: /* M-> */
  case 5: /* C-E -- end-of-line */
    if (string->cursor!=string->len) {
      SetCursor(gadget,string->len,-1);
      return 2;
    }
    return 1;

  case 6: /* C-F -- forward-char */
    if (string->cursor<string->len) {
      SetCursor(gadget,string->cursor+1,-1);
      return 2;
    }
    return 1;

  case 255: /* DEL -- backward-delete-char */
  case 8: /* C-H -- backward-delete-char */
    if (!string->cursor) return 1;
    if (!string->len) return 1;
    ps=&string->buf[string->cursor];
    pd=ps-1;
    i=string->len-string->cursor+1;
    while(i--) *pd++=*ps++;
    string->len--;
    GadgetChanged(gadget);
    SetCursor(gadget,string->cursor-1,-1);
    return 2;

  case 13: /* C-M */
    if (gadget->callbackDown) {
      gadget->flags&=~GAD_CHANGED;
      (*gadget->callbackDown)(gadget);
    } else if (gadget->callbackUp) {
      gadget->flags&=~GAD_CHANGED;
      (*gadget->callbackUp)(gadget);
    } else return 0;
    return 1;

  case 11: /* C-K -- kill-line */
    if (string->cursor>=string->len) return 1;
    if (!string->len) return 1;
    McSetNewSelection(gadget, &string->buf[string->cursor],
		      string->len-string->cursor);
    string->len=string->cursor;
    string->buf[string->len]=0;
    GadgetChanged(gadget);
    return 2;

  case 12: /* C-L -- recenter */
    McGadgetRedraw(gadget);
    return 2;

  case 25: /* C-Y -- yank in current selection */
    wanted=string->bufsize-string->len+3;
    slct=McQuerySelection(gadget, ctrltme, &wanted);
    add_selection_text(gadget, slct, wanted);
    return 2;

  case 15: /* C-O -- insert */
    return 1;

  case 17: /* C-Q -- quote */
    return 1;

  default:
    return 0;
  }
}

static void GadgetChanged(McGadget *gadget) {
  McString *string = (McString *)gadget->specialInfo;
  gadget->flags|=GAD_CHANGED;
  if (string->callbackChange)
    (*(string->callbackChange))(gadget);
}

/****************************************************************************/

McGadget *MakeString(McWindow *mcw, int x, int y, int w, int h,
		     int id, string_t label,
		     void (*callbackUp)(struct McGadget *),
		     void (*callbackDown)(struct McGadget *)) {
  McGadget *gadget;

  if (x<0) x=mcw->w+x-w;
  if (y<0) x=mcw->h+y-h;
  if (!label) label="";

  gadget = McCreateGadget(mcw, GAD_H3D|GAD_3D|GAD_SELECTED|GAD_ACTIVE,
			  STRINGGADGET, x, y, w, h);
  gadget->specialInfo = McCreateString(0, mcw->app->defaultFont, label, 255);
  gadget->callbackUp = callbackUp;
  gadget->callbackDown = callbackDown;
  gadget->id = id;
  return gadget;
}

