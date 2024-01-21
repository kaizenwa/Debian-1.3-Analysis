/***
 * CopyPolicy: GNU Public License 2 applies
 * Copyright (C) by Heiko Eissfeldt
 *
 * uti.h - C common use utility definitions
 *
 * Purpose:
 *   Defines symbols for PASCAL like boolean expressions and other
 *   miscellaneous types including portable data types
 *   [ANSI/System V]
 *
 *****************************************************************************/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef _UTI_DEFINED
#define or ||
#define and &&
#define not !
typedef int bool;
typedef char* string;
typedef unsigned char byte;

#if defined (MSDOS) && !defined (__STDC__)
#define __STDC__
#endif

#ifndef VOID

#define VOID void
#define FAR 
#define NEAR 
#define PASCAL 

typedef int BOOL;
typedef u_int16_t WORD;
typedef u_int32_t DWORD;
typedef long LONG;
typedef unsigned char BYTE;
typedef char *PSTR;

#define LOBYTE(w) ((BYTE)(w))
#define HIBYTE(w) ((BYTE)(((WORD)(w) >> 8) & 0xFF))
#define LOWORD(l) ((WORD)(l))
#define HIWORD(l) ((WORD)(((DWORD)(l) >> 16) & 0xFFFF))

#endif

#define EXPORT _far _pascal _export

#define LONGPTR(ptr) (LONG)(BYTE FAR *)(ptr)
#define LONGPROC(proc) (LONG)(VOID FAR *)(proc)

#define _UTI_DEFINED
#endif
