/**
 *
 * $Id: misc.h,v 1.4 1996/11/07 23:02:49 miers Exp $
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

/*
static char *rcsid = "$Id: misc.h,v 1.4 1996/11/07 23:02:49 miers Exp $";
*/
#ifndef _MISC_H
#define _MISC_H

#ifdef __cplusplus
extern "C" {
#endif
#define MAX_STACK 20
extern int StackPointer;
extern char *Stack[MAX_STACK];
void yyerror(char *);
void Exit(int, char *,char *, ...);
void Warn(int, char *,char *, ...);

char *pop();
void push(char *s);
char *Store(char *s);
void GetArgValues(int index, char **theString, char *Access);

#ifdef __cplusplus
}
#endif

#define LOC __LINE__,__FILE__
extern int errno;

#endif








