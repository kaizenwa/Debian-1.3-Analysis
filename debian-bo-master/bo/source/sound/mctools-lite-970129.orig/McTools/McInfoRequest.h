/*
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

#ifndef _McInfoRequest_h_
#define _McInfoRequest_h_

struct McApp;
struct McWindow;

typedef struct McInfoRequest {
  struct McApp *app;
  struct McWindow *mcw;
  void (*callback)(int yes, void *customData);
  void *customData;
} McInfoRequest;

extern McInfoRequest *McCreateInfoRequest(struct McApp *_app,
					  const unsigned char *title,
					  const unsigned char *text,
					  const unsigned char *yes,
					  const unsigned char *middle,
					  const unsigned char *no,
					  int width,
					  void (*callback)(int, void *));
extern void McInfoRequestNewText(McInfoRequest *req, unsigned char *text);
extern void McRemoveInfoRequest(McInfoRequest **req);

/* height not used yet, pass as zero! */
extern McInfoRequest *McSizedError(struct McApp *_app, int width, int height,
				   const char *fmt, ...);
extern McInfoRequest *McSizedInfo(struct McApp *_app,int width, int height,
				  const char *fmt, ...);
extern void McSync(struct McApp *_app);

#define McError(a,f,arg...) McSizedError(a, 0, 0, f, ## arg)
#define McInfo(a,f,arg...)  McSizedInfo(a, 0, 0, f, ## arg)

#endif


