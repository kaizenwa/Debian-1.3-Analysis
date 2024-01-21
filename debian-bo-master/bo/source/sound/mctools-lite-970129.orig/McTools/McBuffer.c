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

#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>

#include "McBuffer.h"

static McBuffer *make_room(McBuffer *buffer, int len) {
  while (buffer->len+len+4>=buffer->size) {
    if (!(buffer->data=realloc(buffer->data, buffer->size+1024))) {
      buffer->len=buffer->size=0;
      return NULL;
    }
    buffer->size+=1024;
  }
  return buffer;
}

int mbprintf(McBuffer *buffer, const char *fmt, ...) {
  int result;
  va_list args;

  if (!make_room(buffer, 1024)) return 0;
  va_start(args, fmt);
  result=vsprintf(&buffer->data[buffer->len], fmt, args);
  va_end(args);

  buffer->len+=result;

  return result;
}

int mbputs(const unsigned char *str, McBuffer *buffer) {
  int len=strlen(str);
  if (!make_room(buffer, len)) return EOF;
  strcpy(&buffer->data[buffer->len], str);
  buffer->len+=len;
  return len;
}

int mbputc(int ch, McBuffer *buffer) {
  if (!make_room(buffer, 1)) return EOF;
  buffer->data[buffer->len++]=ch;
  return ch;
}

int mbwrite(void *ptr, size_t size, size_t nmemb, McBuffer *buffer) {
  size*=nmemb;
  if (size) {
    if (!make_room(buffer, size)) return 0;
    memcpy(&buffer->data[buffer->len], ptr, size);
    buffer->len+=size;
  }
  return nmemb;
}


