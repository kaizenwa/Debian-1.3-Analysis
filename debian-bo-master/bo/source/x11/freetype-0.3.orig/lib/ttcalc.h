/*******************************************************************
 *
 *  TTCalc.h
 *
 *    Arithmetic and Vectorial Computations (specification).
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 ******************************************************************/

#ifndef TTCALC_H
#define TTCALC_H

#include "tttypes.h"

  /* IntN types :                                                       */
  /*                                                                    */
  /*   Used to garantee the size of some specific integers              */
  /*                                                                    */
  /*  Of course, they are equivalent to Short, UShort, Long, etc ..     */
  /*  but parts of this unit could be used by different programs.       */
  /*                                                                    */

  typedef  signed short    Int16;
  typedef  signed long     Int32;

  typedef  unsigned short  Word16;
  typedef  unsigned long   Word32;

  struct _Int64
  {
    Word32   lo;
    Word32   hi;
  };

  typedef struct _Int64    Int64;


#if FT_BYTE_ORDER == FT_LITTLE_ENDIAN

  void Do16 ( unsigned short *S );
  void Do32 ( unsigned long  *L );

  void Do16s( unsigned short *S, Int cnt );
  void Do32s( unsigned long  *L, Int cnt );

#else

#define Do16
#define Do32

#define Do16s
#define Do32s

#endif

  Int32 MulDiv( Int32 A, Int32 B, Int32 C );

  void Add64( Int64* x, Int64* y, Int64* z );
  void Sub64( Int64* x, Int64* y, Int64* z );

  void MulTo64( Int32 x, Int32 y, Int64 *z );

  Int32 Div64by32( Int64* x, Int32 y );

  Int  Order64( Int64* z );
  Int  Order32( Int32  z );

  Int32 Sqrt32( Int32  l );
  Int32 Sqrt64( Int64 *l );

  Bool  MulVec( Int64          *L,   /* This is a 48 bits integer  */
                TT_UnitVector   F,   /* Freedom vector             */
                TT_UnitVector   P,   /* Projection vector          */
                TT_Vector      *R ); /* Result vector              */

  TT_F26Dot6  Norm( TT_F26Dot6 x, TT_F26Dot6 y );

  TT_F2Dot14  UnitNorm( TT_UnitVector v );

  TT_F26Dot6  Dot( TT_Vector *u, TT_Vector *v );

  TT_F26Dot6  Project( TT_Vector *v, TT_UnitVector u );

#endif
