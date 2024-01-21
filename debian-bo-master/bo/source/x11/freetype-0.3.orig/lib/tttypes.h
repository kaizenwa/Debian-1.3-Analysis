/*******************************************************************
 *
 *  TTTypes.h
 *
 *    Freetype engine's common types specification
 *    (this spec has no associated body).
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  NOTE : 
 *
 *   All these declarations are library internals, and *not* part
 *   of the high-level interface. See also 'freetype.h'.
 *
 ******************************************************************/

#ifndef TTTYPES_H
#define TTTYPES_H

#include "freetype.h"
#include "ttconfig.h"

#ifdef DEBUG
#include <stdlib.h>
#include <stdio.h>
#endif

  typedef unsigned char    Byte;
  /* Byte is not defined in C */

  typedef unsigned short   UShort;
  typedef signed   short   Short;

  typedef unsigned long    ULong;
  typedef signed   long    Long;

  typedef long             Fixed;   /* signed fixed 16.16 float */

  typedef int              Int;

  typedef long             Integer;

  /* Simple access types : pointers and tables */

  typedef  Byte*     PByte;
  typedef  UShort*   PUShort;
  typedef  Short*    PShort;
  typedef  ULong*    PULong;
  typedef  Long*     PLong;

  typedef  Fixed*    PFixed;

  typedef  Int*      PInt;

  typedef void*      Pointer;

  struct _TPointRec
  {
    Int  x, y, flag;
  };
  typedef struct _TPointRec  TPointRec;

  struct _TPoint
  {
    int         glyphNum;    /* current point's glyph number */
    TT_Vector   V;           /* current pixel position       */
    Byte        touch;       /* touch flag                   */
  };
  typedef struct _TPoint  TPoint;

  typedef TPointRec *PPoints;

  typedef TPoint     TTTZone[1024];
  typedef TPoint    *PTTZone;

#ifndef Bool
  typedef int        Bool;
  /* No real booleans in C */
#endif

#ifndef TRUE
#define TRUE  1
#endif

#ifndef FALSE
#define FALSE  0
#endif


  typedef long      *PStorage;

/* Rounding mode constants */

#define TT_Round_Off              0
#define TT_Round_To_Half_Grid     1
#define TT_Round_To_Grid          2
#define TT_Round_To_Double_Grid   3
#define TT_Round_Up_To_Grid       4
#define TT_Round_Down_To_Grid     5
#define TT_Round_Super            6

/* Touch flag masks */

#define TT_Flag_On_Curve      1
#define TT_Flag_Touched_X     2
#define TT_Flag_Touched_Y     4
#define TT_Flag_Touched_Both  6

/* Error management constants :) */

#define SUCCESS  1
#define FAILURE  0

#endif /* TTTYPES_H */

