/**
 *
 * $Id: misc.c,v 1.5 1996/11/07 23:02:48 miers Exp $
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
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "misc.h"

const int BUFFER_SIZE = 256;

char *Store(char *s)
{
  int len = strlen(s);
  static char *buffer = NULL;
  static int SpaceAvailable = 0;
  char *ReturnValue; 

  if (len >= SpaceAvailable){
    int BufferSize = BUFFER_SIZE;
    if (len > BUFFER_SIZE)
      BufferSize = len+1;
    buffer = (char *) malloc(BufferSize * sizeof(char));
    if (NULL == buffer) {
      Exit(LOC, "can't alloc memory\n");
    }
    SpaceAvailable = BufferSize;
  }

  ReturnValue = buffer;

  while(*s)
    *buffer++ = *s++;
  *buffer++ = 0;
  SpaceAvailable -= (len+1);
  return ReturnValue;
} 

int StackPointer = 0;
char *Stack[MAX_STACK];

void push(char *s)
{
  Stack[StackPointer] = s;
  StackPointer++;
  if (StackPointer >= MAX_STACK){
    Exit(LOC,"STACK OVERFLOW");
  }
}

char *
pop()
{
  StackPointer--;
  if (StackPointer < 0){
    Exit(LOC,"STACK UNDERFLOW\n");
  }
  return Stack[StackPointer];
}

void Exit(int line, char *file, char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  fprintf(stderr,"%s:%d:",file,line);
  vfprintf(stderr,fmt,ap);
  va_end(ap);
  abort();  /* Get a dump so we can see the stack frame, variables, etc... */
}

void Warn(int line, char *file, char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  fprintf(stderr,"%s:%d:",file,line);
  vfprintf(stderr,fmt,ap);
  va_end(ap);
  return;
}









