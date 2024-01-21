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

#ifdef DEBUG_CODE
extern void McDebug(McApp *, int id);
extern char *McEventName(int event);
extern char *McKeysymName(KeySym key);
extern void vmstat(void);
extern void McDump(unsigned char *adr, int size, int showadr);
extern unsigned char *McEscapeString(unsigned char *buf, unsigned char *str);
extern void McMstats(struct McApp *);
#ifdef HAVE_MSTATS
void McPrintMemUsed(void);
#endif
#endif
