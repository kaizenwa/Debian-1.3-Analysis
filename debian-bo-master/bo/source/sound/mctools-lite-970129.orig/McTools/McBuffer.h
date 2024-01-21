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

#ifndef _McBuffer_h_
#define _McBuffer_h_

typedef struct McBuffer {
  unsigned char *data;
  int len;
  int size;
} McBuffer;

extern int mbprintf(McBuffer *buffer, const char *fmt, ...);
extern int mbputs(const unsigned char *str, McBuffer *buffer);
extern int mbputc(int ch, McBuffer *buffer);
extern int mbwrite(void *ptr, size_t size, size_t nmemb, McBuffer *buffer);

#endif


