/*******************************************************************
 *
 *  TTMemory.h                                               1.0
 *
 *    Memory management component (specification).
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

#ifndef TTMEMORY_H
#define TTMEMORY_H

#include "tttypes.h"

  struct _TMarkRecord
  {
    Long  magic;
    Int   top;
  };
  typedef struct _TMarkRecord  TMarkRecord;
  typedef TMarkRecord         *PMarkRecord;

  void Init_FontPool( void* buffer, long size );
  /* Initializes the Font Pool on a given memory block */

#define ALLOC(size,target)  Alloc( size, (void**)&(target) )

  Bool Alloc( long size, void* *pointer );
  /* Allocates a chunk of 'size' bytes from the Font Pool.       */
  /* 'pointer' holds the address of the allocated block on exit. */
  /* Returns 1 on success, 0 if no memory is left.               */

  void Mark( PMarkRecord mark );
  /* Mark the current Font Pool state into the 'mark' record. */

  Bool Release( PMarkRecord mark );
  /* Recover the Font Pool to a previous state saved by 'Mark'. */
  /* Returns 1 on success, 0 if release is incoherent.          */
  /* NOTE : A Release destroys the 'mark'!!                     */

#endif /* TTMEMORY_H */
