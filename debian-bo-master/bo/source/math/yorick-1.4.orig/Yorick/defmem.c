/*
    DEFMEM.C
    Implement functions for memory management.

    $Id: defmem.c,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "defmem.h"

/* Don't bother to try to include <stdlib.h>; it might not exist.  */
extern void *malloc(long size);
extern void free(void *p);
extern void *realloc(void *p, long size);

extern long y_net_malloc;  /* simplest possible bug detectors */
extern long y_net_blocks;

long y_net_malloc= 0;
long y_net_blocks= 0;

#ifdef DEBUG
static void *DebugMalloc(long size);
static void *DebugRealloc(void *p, long new);
static int DebugMCheck(void *p, int blow);

static void *DebugMalloc(long size)
{
  void *p;
  long *fill, *fmax, orig;
  if (size<=0) size= 1;
  orig= size;
  size= (((size-1)>>3) + 4)<<3;
  p= malloc(size);
  if (p) {
    fill= p;
    fmax= (long *)((char *)p + size);
    *fill++= orig;
    while (fill<fmax) *(fill++)= -1;
    p= (char *)p + 16;
  }
  return p;
}

static void *DebugRealloc(void *p, long new)
{
  long *fill= (long *)((char *)p - 16);
  long size= ((((*fill)-1)>>3) + 4)<<3;
  long orig;
  DebugMCheck(p, 1);
  p= fill;
  if (new<=0) new= 1;
  orig= new;
  new= (((new-1)>>3) + 4)<<3;
  p= realloc(p, new);
  if (p) {
    long *fmax= (long *)((char *)p + new);
    fill= p;
    *fill= orig;
    if (new > size) {
      fill= (long *)((char *)p + size);
      while (fill<fmax) *(fill++)= -1;
    } else {
      char *past= (char *)p + 16 + orig;
      while (past<(char *)fmax) *(past++)= '\xff';
    }
    p= (char *)p + 16;
  }
  return p;
}

static int DebugMCheck(void *p, int blow)
{
  if (p) {
    long size, orig, *fill;
    char *past, *pmax;
    past= (char *)p - 16;
    fill= (long *)past;
    orig= *fill++;
    size= (((orig-1)>>3) + 4)<<3;
    while (fill<(long *)p) {
      if (*fill++ != -1) {
	if (blow) YError("****DEBUG**** memory underrun (Yfree/Yrealloc)");
	return -1;
      }
    }
    p= past;
    pmax= past + size;
    past+= 16+orig;
    while (past<pmax) {
      if (*past++ != '\xff') {
	if (blow) YError("****DEBUG**** memory overrun (Yfree/Yrealloc)");
	return 1;
      }
    }
  }
  return 0;
}
#endif

void *Ymalloc(long size)
{
#ifndef DEBUG
  void *p= malloc(size>0? size : 1L);
#else
  void *p= DebugMalloc(size);
#endif
  if (!p) YError("****SEVERE**** memory manager failed (Ymalloc)");
  y_net_malloc++;
  return p;
}

void Yfree(void *p)
{
#ifndef DEBUG
  if (p) { free(p); y_net_malloc--; }
#else
  if (p) {
    DebugMCheck(p, 1);
    p= (char *)p - 16;
    free(p);
    y_net_malloc--;
  }
#endif
}

void *Yrealloc(void *p, long size)
{
#ifndef DEBUG
  if (p) p= realloc(p, size>0? size : 1L);
  else { p= malloc(size>0? size : 1L); if (p) y_net_malloc++; }
#else
  if (p) p= DebugRealloc(p, size);
  else { p= DebugMalloc(size); if (p) y_net_malloc++; }
#endif
  if (!p) YError("****SEVERE**** memory manager failed (Yrealloc)");
  return p;
}

static void *NextBlock(MemoryBlock *block)
{
  long unitSize= block->unitSize;
  long n= block->blockSize;
  char *newUnit= Ymalloc(n+unitSize);  /* extra unit for block list */
  void *newBlock;

  /* Add to blockList (1st unit of each block reserved for this). */
  *((void **)newUnit)= block->blockList;
  block->blockList= newUnit;

  /* Usable units begin with 2nd in block, this will be returned. */
  newUnit+= unitSize;
  newBlock= newUnit;

  /* Free list begins with 3rd unit in block and continues to end.
     Initialize the block as a chain of free units. */
  n-= unitSize;
  newUnit+= unitSize;
  block->freeList= newUnit;
  while ((n-=unitSize)>0) {
    /* smallest legal n=2*unitSize barely fails to reach this point */
    *((void **)newUnit)= newUnit+unitSize;
    newUnit+= unitSize;
  }
  *((void **)newUnit)= 0;

  return newBlock;
}

void *NextUnit(MemoryBlock *block)
{
  void *nextFree= block->freeList;
  y_net_blocks++;
  if (nextFree) {
    /* (void**)<->(void*) typecast in next line is equivalent to
       list->next if list is a struct List {struct List *next;}... */
    block->freeList= *(void **)nextFree;
    return nextFree;
  } else {
    return NextBlock(block);
  }
}

void FreeUnit(MemoryBlock *block, void *ptr)
{
  *((void**)ptr)= block->freeList;
  block->freeList= ptr;
  y_net_blocks--;
}

void FreeAllUnits(MemoryBlock *block)
{
  void *blockList, *next= block->blockList;
  while ((blockList=next)) {
    next= *(void**)blockList;
    Yfree(blockList);
  }
}
