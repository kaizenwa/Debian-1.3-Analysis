/*******************************************************************
 *
 *  TTMemory.c                                               1.0
 *
 *    Memory management component (body).
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  NOTE:
 *
 *    The FreeType library uses a simple and fast growing heap
 *    known as a 'Font Pool' to manage all its data. This version
 *    does not manage multiple pools (a feature that may reveal
 *    useful for a FreeType based font server to be able to manage
 *    several open fonts).
 *
 ******************************************************************/

#include "ttmemory.h"

#define MARK_MAGIC  0xBabe0007

  char*  buffer_base;
  Long   buffer_size;
  Long   buffer_top;

  void Init_FontPool( void* buffer, long size )
  {
    buffer_base = (char*) buffer;
    buffer_size = size;
    buffer_top  = 0;
  }


  Bool Alloc( long size, void* *pointer )
  {
    Long l;

    l = buffer_top + (( size+3 ) & -4);

    if ( l >= buffer_size )
      return FAILURE;

    *pointer   = (void*)(buffer_base + buffer_top);
    buffer_top = l;

    return SUCCESS;
  }


  void Mark( PMarkRecord  mark )
  {
    mark->magic = MARK_MAGIC;
    mark->top   = buffer_top;
  }


  Bool Release( PMarkRecord mark )
  {
    if ( mark->magic == MARK_MAGIC )
    {
      mark->magic = 0;
      buffer_top  = mark->top;
      mark->top   = -1;
      return SUCCESS;
    }
    else
      return FAILURE;
  }
