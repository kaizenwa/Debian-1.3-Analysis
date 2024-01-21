/*
    DEFSTR.C
    Define functions for string manipulation.

    $Id: defstr.c,v 1.1 1993/08/27 18:32:09 munro Exp $

    Implement StrCpy, StrCat, StrAlloc, StrFree functions using 8 block
    allocators.  This should greatly speed up memory allocate/free
    for short to medium length strings.  The gotcha is that after
    once a string is allocated using StrCpy or StrAlloc, StrFree only
    returns its space for use by other StrCpy or StrAlloc strings,
    NOT to the general memory pool.
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "defstr.h"
#include "defmem.h"

#define N_STRING_BLOCKS 12
extern MemoryBlock y_StringBlocks[N_STRING_BLOCKS];
MemoryBlock y_StringBlocks[N_STRING_BLOCKS]= {
  {0, 0,  2*sizeof(void *), 128* 2*sizeof(void *)},
  {0, 0,  4*sizeof(void *), 64*  4*sizeof(void *)},
  {0, 0,  6*sizeof(void *), 42*  6*sizeof(void *)},
  {0, 0,  8*sizeof(void *), 32*  8*sizeof(void *)},
  {0, 0, 10*sizeof(void *), 25* 10*sizeof(void *)},
  {0, 0, 12*sizeof(void *), 20* 12*sizeof(void *)},
  {0, 0, 14*sizeof(void *), 18* 14*sizeof(void *)},
  {0, 0, 16*sizeof(void *), 16* 16*sizeof(void *)},
  {0, 0, 18*sizeof(void *), 14* 18*sizeof(void *)},
  {0, 0, 20*sizeof(void *), 12* 20*sizeof(void *)},
  {0, 0, 22*sizeof(void *), 10* 22*sizeof(void *)},
  {0, 0, 24*sizeof(void *), 10* 24*sizeof(void *)}
};

char *StrCpy(const char *s)
{
  if (s) {
    char *t= StrAlloc(strlen(s));
    return strcpy(t, s);
  } else {
    return 0;
  }
}

char *StrNCpy(const char *s, long n)
{
  if (s && n>=0) {
    char *t= StrAlloc(n);
    strncpy(t, s, n);
    t[n]= '\0';
    return t;
  } else {
    return 0;
  }
}

char *StrCat(const char *s, const char *t)
{
  if (s) {
    if (t) {
      long ls= strlen(s);
      char *u= StrAlloc(ls+strlen(t));
      strcpy(u, s);
      return strcat(u, t);
    } else {
      return StrCpy(s);
    }
  } else {
    return StrCpy(t);
  }
}

char *StrNCat(const char *s, const char *t, long n)
{
  if (t && n>=0) {
    char *u;
    if (s) {
      u= StrAlloc(strlen(s)+n);
      strcpy(u, s);
    } else {
      u= StrAlloc(n);
    }
    return strncat(u, t, n);
  } else {
    return StrCpy(s);
  }
}

char *StrAlloc(long n)
{
  char *t, tag;
  n+= 1;  /* block marker + trailing \0 minus 1 */
  if (n >= N_STRING_BLOCKS*2*sizeof(void *)) {
    t= (char *)Ymalloc(n+1);
    tag= N_STRING_BLOCKS;
  } else {
    /* All machines I know use 4 or 8 byte void*, so shoot me... */
    if (sizeof(void *) == 4) tag= n>>3;
    else if (sizeof(void *) == 8) tag= n>>4;
    else tag= n%(2*sizeof(void *));
    t= NextUnit(&y_StringBlocks[(int)tag]);
  }
  *t++= tag;
  *t= '\0';
  return t;
}

void StrFree(char *s)
{
  if (s) {
    unsigned char *t= (unsigned char *)s-1;
    if (*t >= N_STRING_BLOCKS) Yfree(t);
    else FreeUnit(&y_StringBlocks[*t], t);
  }
}
