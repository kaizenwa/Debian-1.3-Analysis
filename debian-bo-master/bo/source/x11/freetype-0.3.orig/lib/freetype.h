/*******************************************************************
 *
 *  FreeType.h
 *
 *    High-level interface specification.
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  Notes:
 *
 *    This is the only file that should be included by client            
 *    application sources for the final release. All other types
 *    and functions defined in the "tt*.h" files are library  
 *    internals, and should not be included (except of course
 *    during development, as now).
 *
 *    FreeType is still in alpha, thus the empty current interface.
 *
 ******************************************************************/

#ifndef FREETYPE_H
#define FREETYPE_H

  typedef  signed long    TT_Fixed;   /* Signed Fixed 16.16 Float */

  typedef  signed short   TT_FWord;   /* Distance in FUnits */

  typedef  unsigned short TT_UFWord;  /* Unsigned distance */

  typedef  signed short   TT_F2Dot14; /* signed fixed float 2.14 used for */
                                      /* unary vectors, with layout :     */
                                      /*                                  */
                                      /*  s : 1  -- sign bit              */
                                      /*  m : 1  -- mantissa bit          */
                                      /*  f : 14 -- unsigned fractional   */
                                      /*                                  */
                                      /*  's:m' is the 2-bit signed int   */
                                      /*  value to which the positive     */
                                      /*  fractional part should be       */
                                      /*  added.                          */
                                      /*                                  */

  struct _TT_UnitVector   /* guess what .. ? */
  { 
    TT_F2Dot14  x;
    TT_F2Dot14  y;
  };
  typedef struct _TT_UnitVector  TT_UnitVector;

  typedef signed long  TT_F26Dot6;  /* 26.6 fixed float, used for glyph pts */
                                    /* pixel coordinates                    */

  struct _TT_Vector      /* Simple vector type */
  {
    TT_F26Dot6  x;
    TT_F26Dot6  y;
  };
  typedef struct _TT_Vector  TT_Vector;
  
  typedef TT_Vector     TT_VecTable[10];
  typedef TT_Vector    *TT_PVecTable;

  typedef unsigned char   TT_TouchTable[10];
  typedef unsigned char  *TT_PTouchTable;

  struct _TT_VecRecord
  {
    int             n;     /* number of points in zone    */
    TT_PVecTable    org;   /* original points coordinates */
    TT_PVecTable    cur;   /* current points coordinates  */
    TT_PTouchTable  touch; /* current touch flags         */
  };
  typedef struct _TT_VecRecord  TT_VecRecord;

  /* This type defining a set of glyph points will be used to represent */
  /* each zone ( regular and twilight ) during instructions decoding    */

  struct _TT_Contour
  {
    int   first;  /* index of first contour point in regular zone */
    int   last;   /* index of last contour point in regular zone  */
  };
  typedef struct _TT_Contour    TT_Contour;

  typedef TT_Contour   TT_ContourTable[10];
  typedef TT_Contour  *TT_PContourTable;

  struct _TT_ContourRecord
  {
    int                n;  /* contours number */
    TT_PContourTable   c;  /* contours table  */
  };
  typedef struct _TT_ContourRecord   TT_ContourRecord;
  /* This type is used to define the contours of each glyph */

#endif /* FREETYPE_H */

