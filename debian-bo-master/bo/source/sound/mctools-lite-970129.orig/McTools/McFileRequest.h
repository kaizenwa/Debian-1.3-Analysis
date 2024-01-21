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

#ifndef _McFileRequest_h_
#define _McFileRequest_h_

struct DirEntry;
struct McApp;
struct McWindow;
struct McGadget;

#define MCF_POPUP	1
#define MCF_MUST_EXIST	2

typedef struct McFileRequest {
  struct McApp *app;
  struct McWindow *mcw;
  struct McGadget *Gad[12];
  const unsigned char **dirinfo;
  struct DirEntry *dirlist;
  XFontStruct *listfont;
  unsigned char *oldfile;
  unsigned char *format;
  short formatlen, namelen;
  int newpath, newfile, flags;
  int (*callback)(unsigned char *,unsigned char *,unsigned char *);
} McFileRequest;

extern McFileRequest *McCreateFileRequest(McApp *, int popup,
					unsigned char *title,
					unsigned char *pattern,
					unsigned char *path,
					unsigned char *file,
					unsigned char *openmsg,
					unsigned char *format,
					int (*callback)(unsigned char *path,
							unsigned char *file,
							unsigned char *mask));

extern void McRemoveFileRequest(McFileRequest **req);

#endif


