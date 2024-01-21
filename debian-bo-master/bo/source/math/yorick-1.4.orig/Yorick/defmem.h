/*
    DEFMEM.H
    Declare structures and functions for memory management.

    $Id: defmem.h,v 1.1 1993/08/27 18:32:09 munro Exp $

    Ymalloc, Yfree, Yrealloc declared, plus a generic block allocator
    package.

    The generic block allocator provides a much faster, less fragmenting
    alternative to malloc/free for cases in which many small units
    of memory are to be allocated and freed.  The strategy is to
    allocated a large block of units and maintain a list of free units;
    when the free list becomes empty, a new large block is allocated.
    Each unit must have alignment at least as strict as void*, and arrays
    of units are not supported.  (The surest way to set the proper unit
    size is to use sizeof(union{your unit; void *dummy}).)

    To use the package, you would generally declare a static MemoryBlock,
    initialized with 0 pointers and the unitSize and blockSize you desire.
    The unitSize may not change once a block has been allocated, but a
    change in blockSize is benign and will take effect the next time a
    large block is allocated.
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#ifndef DEFMEM_H
#define DEFMEM_H

/* The low-level memory manager used by Yorick must have the same linkage
   as the <stdlib.h> malloc, free, and realloc of standard ANSI C, as set
   forth in 2nd ed K&R, section B5, *EXCEPT* I assume the size_t type is
   long, to avoid having to actually include <stdlib.h>, which is missing
   on Suns.  Yorick always refers to Ymalloc, Yfree, and Yrealloc, to
   simplify changing memory managers, should this become necessary.
   The Yorick memory management functions have the following sematics
   for 0-pointers:
   (1) Ymalloc(0) gives a non-0 pointer, same as Ymalloc(1)
   (2) Yfree(0) is a no-op
   (3) Yrealloc(0, size) is same as Ymalloc(size)
   (4) Ymalloc and Yrealloc call YError on failure rather than returning 0
 */
extern void *Ymalloc(long size);
extern void Yfree(void *p);
extern void *Yrealloc(void *p, long size);
extern void YError(const char *msg);   /* also declared in ydata.h */

typedef struct MemoryBlock MemoryBlock;
struct MemoryBlock {
  void *freeList;   /* allocate new block when reaches 0 */
  void *blockList;  /* list of blocks kept using first unit of each block */
  long unitSize;    /* byte size of units, multiple of sizeof(void**) */
  long blockSize;   /* multiple of unit size, at least 2*unitSize */
};

extern void *NextUnit(MemoryBlock *block);       /* ptr= malloc(unitSize) */
extern void FreeUnit(MemoryBlock *block, void *ptr);         /* free(ptr) */
extern void FreeAllUnits(MemoryBlock *block);

#endif
