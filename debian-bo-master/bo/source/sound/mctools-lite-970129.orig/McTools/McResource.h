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

#ifndef _McResource_h_
#define _McResource_h_

extern Display *McParseOpenDisp(McApp *, int *argc, char *argv[],
				string_t class,
				XrmOptionDescRec *DescTable, int DescSize);

extern string_t McFindResource(McApp *, const string_t name, int where);
extern string_t McGetResource(McApp *, string_t name);
extern int McGetSwitch(McApp *, string_t name);
extern int McTestSwitch(string_t str);
extern int McGetProfileInt(struct McApp *,
			   const string_t section,
			   const string_t name,
			   int def);
extern const string_t McGetProfileString(struct McApp *,
					 const string_t section,
					 const string_t name,
					 const string_t def);

extern void McPutResource(McApp *,
			  const string_t name, const string_t value);
extern void McPutSwitch(McApp *, const string_t name, int bool);
extern void McWriteProfileInt(struct McApp *,
			      const string_t section,
			      const string_t name,
			      int value);
extern void McWriteProfileString(struct McApp *,
				 const string_t section,
				 const string_t name,
				 const string_t str);

extern void McWriteResources(McApp *);

extern void McSetHints(McWindow *mcw, string_t title,
		       int argc, char *argv[],
		       XSizeHints *sizehints, XWMHints *wm_hints);
extern string_t *McSaveCmdline(int argc, string_t argv[]);
extern void McReadStandards(McApp *);
void McSetTitle(McWindow *mcw, string_t title);

#endif /* _McResource_h_ */


