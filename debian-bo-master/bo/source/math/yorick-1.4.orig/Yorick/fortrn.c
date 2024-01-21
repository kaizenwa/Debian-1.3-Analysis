/*
   FORTRN.C
   Yorick routines callable from FORTRAN.
   Includes C-callable versions of a simple memory management package
   which is safe against asynchronous interrupts.

   $Id: fortrn.c,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "defmem.h"
#include "yasync.h"

/* Routines to allocate Cray-style pointers:
   memreal8 -- allocate 8 byte reals (C doubles)
   memreal4 -- allocate 4 byte reals (C floats -- may be 8 bytes)
   meminteg -- allocate integers (C longs)
   membyte  -- allocate characters (C chars)
   memfree  -- free any of above

   memwork  -- call prior to a sequence of allocation calls to remember
               the addresses returned.  If fblowup is called, or if
	       memwork is called again before everything has been
	       freed with memfree since the previous memwork call,
	       the previous arrays will be freed.  Using this requires
	       some care, but it allows the workspace you allocate to
	       be made "safe" against asynchronous interrupts of your
	       FORTRAN code.
	       If you use memwork, memfree will be most efficient if
	       called in reverse order from the allocations.

   Routine to longjmp out of an error condition:
   fblowup  -- YError (actually YAsyncError) for FORTRAN

   Routine to print a warning (use this sparingly if at all):
   fwhine   -- YWarning for FORTRAN
 */
/* The C-callable versions of these routies are:
      extern void *YAsyncBytes(long n)
      extern void YAsyncFree(void *ptr)
      extern void YAsyncWork(void)
      extern void YAsyncError(const char *msg)
      extern void YWarning(const char *msg)

   declared in yasync.h
 */

/* FORTRAN routine names may be forced to upper case, forced to lower case,
   and terminated with an underscore or not.  */
#ifdef f_linkage
#define FORTNAME(x, x_, X, X_) x
#else
#ifdef f_linkage_
#define FORTNAME(x, x_, X, X_) x_
#else
#ifdef F_LINKAGE
#define FORTNAME(x, x_, X, X_) X
#else
#ifdef F_LINKAGE_
#define FORTNAME(x, x_, X, X_) X_
#else
This horrendous syntax error is caused by your failure to define a
FORTRAN linkage I have been trained to understand.
#endif
#endif
#endif
#endif

extern void *FORTNAME(memreal8, memreal8_, MEMREAL8, MEMREAL8_)(long *n);
extern void *FORTNAME(memreal4, memreal4_, MEMREAL4, MEMREAL4_)(long *n);
extern void *FORTNAME(meminteg, meminteg_, MEMINTEG, MEMINTEG_)(long *n);
extern void *FORTNAME(membyte, membyte_, MEMBYTE, MEMBYTE_)(long *n);
extern void FORTNAME(memfree, memfree_, MEMFREE, MEMFREE_)(void *ptr);
extern void FORTNAME(memwork, memwork_, MEMWORK, MEMWORK_)(void);

#ifndef F_STRING_SWAP
#define F_STRING_ARG(msg, len) (char *msg, long len)
#else
#define F_STRING_ARG(msg, len) (long len, char *msg)
#endif

extern void FORTNAME(fblowup, fblowup_, FBLOWUP, FBLOWUP_)
     F_STRING_ARG(msg, len);

extern void FORTNAME(fwhine, fwhine_, FWHINE, FWHINE_)
     F_STRING_ARG(msg, len);

static void **memlist= 0;
static long nlist= 0;
static int keeplist= 0;
static void ClearList(void);
static void *ListAppend(void *);
static void CopyFortranString(char *src, char *dst, long len);

void FORTNAME(fblowup, fblowup_, FBLOWUP, FBLOWUP_)
     F_STRING_ARG(msg, len)
{
  char mess[128];
  if (len>127) len= 127;
  CopyFortranString(msg, mess, len);
  YAsyncError(mess);
}

void FORTNAME(fwhine, fwhine_, FWHINE, FWHINE_)
     F_STRING_ARG(msg, len)
{
  char mess[128];
  if (len>127) len= 127;
  CopyFortranString(msg, mess, len);
  YWarning(mess);
}

void YAsyncError(const char *msg)
{
  ClearList();
  if (*msg) YError(msg);
  else YError("<YAsyncError or FBLOWUP called>");
}

static void CopyFortranString(char *src, char *dst, long len)
{
  char *last= 0;
  while ((len--)>0) {
    *dst= *src++;
    if (*dst!=' ' && *dst!='\t') last= dst;
    dst++;
  }
  if (last) last[1]= '\0';
  else *dst= '\0';
}

static void ClearList(void)
{
  void **list= memlist;
  while (nlist>0) Yfree(memlist[--nlist]);
  memlist= 0;
  if (list) Yfree(list);
  keeplist= 0;
}

static void *ListAppend(void *ptr)
{
  if (!(nlist&0x1f)) Yrealloc(memlist, nlist+32);
  return memlist[nlist++]= ptr;
}

void FORTNAME(memwork, memwork_, MEMWORK, MEMWORK_)(void)
{
  YAsyncWork();
}

void YAsyncWork(void)
{
  if (keeplist) ClearList();
  keeplist= 1;
}

void FORTNAME(memfree, memfree_, MEMFREE, MEMFREE_)(void *ptr)
{
  YAsyncFree(ptr);
}

void YAsyncFree(void *ptr)
{
  if (keeplist) {
    long i;
    int flag= 0;
    for (i=nlist-1 ; i>=0 ; i--) {
      if (!memlist[i]) continue;
      if (memlist[i]==ptr) {
	memlist[i]= 0;
	if (flag) break;
	flag|= 1;
      } else {
	if (flag&1) break;
	flag|= 2;
      }
    }
    if (i<0) { nlist= 0; ClearList(); }
    Yfree(ptr);
  } else {
    Yfree(ptr);
    if (memlist) ClearList();
  }
}

void *FORTNAME(memreal8, memreal8_, MEMREAL8, MEMREAL8_)(long *n)
{
  return YAsyncBytes(sizeof(double)*n[0]);
}

void *FORTNAME(memreal4, memreal4_, MEMREAL4, MEMREAL4_)(long *n)
{
  return YAsyncBytes(sizeof(float)*n[0]);
}

void *FORTNAME(meminteg, meminteg_, MEMINTEG, MEMINTEG_)(long *n)
{
  return YAsyncBytes(sizeof(long)*n[0]);
}

void *FORTNAME(membyte, membyte_, MEMBYTE, MEMBYTE_)(long *n)
{
  return YAsyncBytes(n[0]);
}

void *YAsyncBytes(long n)
{
  void *ptr= Ymalloc(n);
  return keeplist? ListAppend(ptr) : ptr;
}
