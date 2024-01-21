/**
 *
 * $Id: uil.h,v 1.4 1996/11/07 23:02:50 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 
#ifndef _UIL_H
#define _UIL_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	char Name[256];
	int lineno;
} FileData;

typedef struct {
  unsigned int width, height;
  char *ColorTable;
#ifdef __cplusplus
const
#endif
  char *data;
} PixmapType;

typedef struct {
  unsigned int width, height;
  int x_hot, y_hot;
  char *data;
} BitMapType;

#ifdef __cplusplus
}
#endif

enum { UID_COMPOUND_STRING, UID_ARGS, UID_INTEGER, UID_CALLBACK, UID_CREATE};

#if 0
static const char UID_COMPOUND_STRING = 1;
static const char UID_ARGS = 2;
static const char UID_INTEGER = 3;
static const char UID_CALLBACK = 4;
static const char UID_CREATE = 5;
#endif

#endif





