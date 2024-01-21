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


#include "McApp.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "McText.h"

McText *McCreateText(McWindow *mcw, const string_t text, GC gc,
		     XFontStruct *font, int x, int y) {

  McText *txt = (McText *) calloc(sizeof(McText), 1);

  if (!gc) gc=mcw->app->gc[GC_NORMAL];
  if (!font) font=mcw->app->defaultFont;

  txt->x = x;
  txt->y = y;
  txt->text = NULL;
  txt->font = font;
  txt->next = NULL;
  McChangeText(mcw, txt, text, gc);
  return txt;
}

void McChangeText(McWindow *mcw, McText *txt, const string_t text, GC gc) {
  int		direction, ascent, descent;
  XCharStruct	overall;

  XTextExtents(txt->font, text, strlen(text), &direction, &ascent,
	       &descent, &overall);

  txt->width = overall.width;
  txt->height = ascent + descent + 1;
  if (text) {
    if (txt->text)
      free(txt->text);
    txt->text = strdup(text);
  }
  if (gc) txt->gc = gc;
}

void McFreeText(McText *txt) {
  if (txt->text)
    free(txt->text);
  free(txt);
}

void McWriteText(McWindow *mcw, Window win, McText *txt, GC gc, int x, int y) {
  GC ggc;
  if (!win) win=mcw->clientwin;
  while(txt) {
    if (gc) ggc=gc; else ggc=txt->gc;
    XSetFont(mcw->app->display, ggc, txt->font->fid);
    XDrawImageString(mcw->app->display, win, gc?gc:txt->gc,
		     txt->x+x, txt->y+y+txt->font->ascent,
		     txt->text,strlen(txt->text));
    txt=txt->next;
  }
}

int McCalculateText(XFontStruct *font, unsigned char *text,
		    int *lines, int *maxwidth, McLine *line) {
  int len, i, height;
  unsigned char *begin, *end, *chp;
  int direction, ascent, descent, ch, maxw;
  int orientation, offset, maxoffset;
  XCharStruct overall;

  *lines=0; orientation=MCTXT_CENTERED; maxoffset=0;
  begin=text;
  while(*begin) {
    offset=0;
    end=begin;
    while((*end) && (*end!='\n')) end++;
    len=end-begin;
    if (*begin=='&') {
      switch(begin[1]) {
      case 'c':
	orientation=MCTXT_CENTERED;
	begin+=2;
	break;
      case 'l':
	orientation=MCTXT_FLUSHLEFT;
	begin+=2;
	break;
      case 'r':
	orientation=MCTXT_FLUSHRIGHT;
	begin+=2;
	break;
      case 'L':
	orientation=MCTXT_FLUSHLEFT;
	begin+=2;
	ch=*begin++;
	if (len && (chp=memchr(begin, ch, len)) && (chp>begin)) {
	  XTextExtents(font, begin, chp-begin, &direction,
		       &ascent, &descent, &overall);
	  offset=overall.width;
	}
	break;

      case '&':
	begin++;
	break;
      default:
	break;
      }
    }

    XTextExtents(font, begin, len, &direction, &ascent, &descent, &overall);

    /* Split text into several lines if maxwidth is set */
    while ((*maxwidth) && (overall.width>*maxwidth)) {
      end--;
      while((end>begin) && (*end!=32) && (*end!=9)) end--;
      if (end<=begin) { /* Split failed, text too long */
	end=begin+len;
	break;
      }
      len=end-begin;
      XTextExtents(font, begin, len, &direction, &ascent, &descent, &overall);
    }

    if (offset>maxoffset) maxoffset=offset;
    line[*lines].width=overall.width;
    line[*lines].len=len;
    line[*lines].offset=offset;
    line[*lines].data=begin;
    line[*lines].orientation=orientation;
    (*lines)++;
    if (!*end) break;
    *end=0;
    begin=end+1;
  }

  maxw=0; height=0;
  for (i=0; i<*lines; i++) {
    line[i].offset=maxoffset-line[i].offset;
    line[i].width+=line[i].offset;
    line[i].height=ascent+descent+2;
    if (line[i].width>maxw) maxw=line[i].width;
    height+=line[i].height;
  }

  if (*maxwidth==0) *maxwidth=maxw;
  return height;
}

