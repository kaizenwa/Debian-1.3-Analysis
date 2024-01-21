/*
** xalloc.h           Memory allocation routines.
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/
#ifndef __XALLOC_H__
#define __XALLOC_H__

#define xalloc  _nis_xalloc
#define xfree   _nis_xfree
#define xstrdup _nis_xstrdup
#define xdup    _nis_xdup

extern void *xalloc(void **buf, int len, int size);
extern void xfree(void **buf);
extern char *xstrdup(char *str);
extern void *xdup(void *buf, int len);

#define Xalloc(bufp, len) xalloc((void **) (bufp), (len), sizeof(**(bufp)))
#define Xnew(bufp)        Xalloc(&bufp, 1)

#endif
