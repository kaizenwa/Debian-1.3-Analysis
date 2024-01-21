/*
    DEFSTR.H
    Declare functions for string manipulation.

    $Id: defstr.h,v 1.1 1993/08/27 18:32:09 munro Exp $

    Duplicates declarations in ANSI <string.h> header (which is not
    present on Suns), and adds custom functions StrSave, StrAlloc, and
    StrFree which make use of the block allocator defined in defmem.c.
    (Note that size_t is assumed to be long here...)
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#ifndef DEFSTR_H
#define DEFSTR_H

/* first come function definitions from <string.h> */

#ifdef STDC_HEADERS
#include <string.h>

#else

#ifndef SIZE_T_TYPE
#define SIZE_T_TYPE unsigned long
#endif

#ifdef BSD_STRCHR
#define strchr index
#define strrchr indexr
#endif

extern char *strcpy(char *s, const char *t);
extern char *strncpy(char *s, const char *t, SIZE_T_TYPE n);
extern char *strcat(char *s, const char *t);
extern char *strncat(char *s, const char *t, SIZE_T_TYPE n);
extern int strcmp(const char *s, const char *t);
extern int strncmp(const char *s, const char *t, SIZE_T_TYPE n);
extern char *strchr(const char *s, int c);
extern char *strrchr(const char *s, int c);
extern SIZE_T_TYPE strspn(const char *s, const char *t);
extern SIZE_T_TYPE strcspn(const char *s, const char *t);
extern char *strpbrk(const char *s, const char *t);
extern char *strstr(const char *s, const char *t);
extern SIZE_T_TYPE strlen(const char *s);
extern char *strerror(int n);
extern char *strtok(char *s, const char *t);

extern void *memcpy(void *s, const void *t, SIZE_T_TYPE n);
extern void *memmove(void *s, const void *t, SIZE_T_TYPE n);
extern int memcmp(const void *s, const void *t, SIZE_T_TYPE n);
extern void *memchr(const void *s, int c, SIZE_T_TYPE n);
extern void *memset(void *s, int c, SIZE_T_TYPE n);
#endif

/* next come string functions from <stdlib.h> */

extern long strtol(const char *s, char **endp, int base);
extern double strtod(const char *s, char **endp);

/* Yorick specific higher-level string functions properly handle
   0 char*s, and automatically do appropriate memory management.
   Memory management uses fast block allocators for <64 character
   strings.  */

extern char *StrAlloc(long n);          /* returns * to n+1 new chars */
extern void StrFree(char *s);           /* releases StrSave/Alloc string */

/* The Cpy operations return 0 iff s==0 */
extern char *StrCpy(const char *s);
extern char *StrNCpy(const char *s, long n);
/* The Cat operations ALWAYS return a copy -- 0 iff both s==0 && t==0.
   If t==0, same as StrCpy(s); if s==0, StrCpy(t) or StrNCpy(t, n).  */
extern char *StrCat(const char *s, const char *t);
extern char *StrNCat(const char *s, const char *t, long n);

#endif
