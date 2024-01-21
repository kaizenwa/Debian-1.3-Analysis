/*******************************************************************
 *
 *  TTRaster.h                                                 1.1
 *
 *    The FreeType rasterizer (body).
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  NOTES:
 *
 *   The rasterizer is able to work with a block of memory given
 *   by a client application called the 'Render Pool'.            
 *
 *   Now supports gray-level rendering.
 *
 ******************************************************************/

#include "ttraster.h"
#include "tttypes.h"
#include <string.h>

/* Bug fixes:                                                             */
/*                                                                        */
/*   11/16/96 : Added first grayscale support.                            */
/*   11/ 8/96 : David: Fixed 'Curve_To' from access bug.                  */
/*                                                                        */
/*                                                                        */

/* To Do:                                                                 */
/*                                                                        */
/* - It might be more interesting to use linked lists for trace arrays    */
/*   (solves the storage nightmare).                                      */
/*                                                                        */
/* - Add an HLine for flat segments located on a scanline. This is a      */
/*   delicate point relative to grayscaling.                              */
/*                                                                        */
/* - Add an additional horizontal sweep to take care of drop-out          */
/*   controls. A delicate issue too...                                    */
/*                                                                        */

#ifdef SECURE
#include "ttcalc.h"
#else
#define  MulDiv(a,b,c)  ( (a)*(b)/(c) )
#endif

#ifdef DEBUG3
  extern char* Vio;  /* A pointer to VRAM or display buffer */
#endif

#ifndef NULL
#define NULL  (void*)0
#endif

#define  MaxBezier     32   /* The maximum number of Bezier subdivisions */

#define  Precision_Bits 6

#define  Precision      64L /* 6bit precision, as defined in the   */
                            /* TrueType specifications             */

#define  Precision2     32L /* Half of Precision                   */

#define  Floor(x)    ( (x) & -Precision )
#define  Ceiling(x)  ( ((x)+Precision-1) & -Precision )
#define  Trunc(x)    ( (signed long)(x) >> Precision_Bits )
#define  Frac(x)     ( (x) & (Precision-1) )
/* NOTE: These operations are only valid on 2's complement processors */

  Int  Raster_Error = 0;

#ifdef DEBUG3
#define DEBUG3_PSET        Pset()
#else
#define DEBUG3_PSET  
#endif

  const unsigned char    LMask[8] = 
        { 0xFF, 0x7F, 0x3F, 0x1F, 0x0F, 0x07, 0x03, 0x01 };
  /* Left fill bitmask */

  const unsigned char    RMask[8] =
        { 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE, 0xFF };
  /* Right fill bitmask */

  enum _TStates { Unknown, Ascending, Descending, Flat };
  typedef enum _TStates  TStates;
  /* States of each line, arc and profile */

  struct _TProfile;

  typedef struct _TProfile  TProfile;
  typedef TProfile         *PProfile;

  struct _TTraceElement
  {
    PProfile  profile;  /* profile pointer                                 */
    Long      x;        /* profile's current x coordinate in current sweep */
  };
  typedef struct _TTraceElement  TTraceElement;

  typedef TTraceElement   TTraceArray[128];
  typedef TTraceElement  *PTraceArray;

  struct _TTraceRec
  {
    Int          n;    /* number of profiles in array     */
    PTraceArray  t;    /* array of (profile,x-coordinate) */
  };
  typedef struct _TTraceRec  TTraceRec;
  typedef TTraceRec         *PTraceRec;

  struct _TProfile
  {                                                                     
    Int        flow;        /* Profile orientation : Asc/Descending     */
    Int        height;      /* profile's height in scanlines            */
    Int        start;       /* profile's start scanline                 */
    ULong      offset;      /* offset of profile's data in render pool  */
    PProfile   link;        /* link to next profile                     */
    Int        index;       /* index of profile's entry in trace table  */
    Int        count_lines; /* count of lines having to be drawn        */
    Int        start_line;  /* lines to be rendered before this profile */
    PTraceRec  trace;       /* pointer to profile's current trace table */
  };

  struct _TBand
  {
    Int   y_min;   /* band's minimum */
    Int   y_max;   /* band's maximum */
  };
  typedef struct _TBand  TBand;

#define AlignProfileSize  ( (sizeof(TProfile)+3)/4 )
#define AlignTraceSize    ( (sizeof(TTraceRec)+3)/4 )

  PProfile  cProfile;   /* current profile                     */
  PProfile  fProfile;   /* head of linked list of profiles     */
  PProfile  oProfile;   /* 'old' profile, used to update links */
  PProfile  gProfile;   /* last profile in case of impact      */

  Int       nProfs;     /* current number of profiles */

  TStates    state;       /* rendering state */

  Bool      fresh;      /* signals a fresh new profile whose 'start' field */
                        /* must be completed                               */

  Bool      joint;      /* signals that le last arc ended exactly on a     */
                        /* scanline. Allows removal of doublets            */

  PStorage  buff    = (PStorage)0;  /* The profiles buffer          */
  Int       maxBuff = 0;            /* Profiles buffer size         */
  Int       profCur = 0;            /* Current cursor in buffer     */

  TRasterBlock  target;  /* description of target bit/pixmap */

  PByte         bTarget; /* target bit/pixmap */

  TBand     band_stack[16];  /* band stack used for sub-banding */
  Int       band_top;        /* band stack top                  */

  TTraceRec trace_left;  /* left  ( ascending ) trace table  */
  TTraceRec trace_right; /* right ( descending ) trace table */

  Int       traceOfs;    /* current offset in bitmap                     */
  Int       debugOfs;    /* current screen offset during debug rendering */

  struct { Long x, y; }  Arcs[ 2*MaxBezier+1 ];  /* The Bezier stack */

  Int       curArc;      /* Bezier stack top */

  PStorage  xCoord;  /* current xCoord table */
  PStorage  yCoord;  /* current yCoord table */

  PByte     flags;   /* current flags table    */
  PShort    outs;    /* current outlines table */

  Int       nPoints;   /* number of points in current glyph   */
  Int       nContours; /* number of contours in current glyph */

  Long      LastX, LastY, MinY, MaxY;

  Byte      dropOutControl;  /* current drop_out control method */

  char      gray_palette[5]; /* Palette of gray levels used for render */

  Short     count_table[256]; /* Look-up table used to quickly count */
                              /* set bits in a gray 2x2 cell         */

  Byte*     gray_lines; /* Intermediate table used to render the graylevels */
  Int       gray_width; /* width in bytes of one gray_line. Each gray pixel */
                        /* takes 2 bits long there.                         */
                        /* The gray_lines must hold 2 lines, thus with size */
                        /* in bytes of at least 'gray_width*2'              */

#ifdef DEBUG3

  /************************************************}
  {*                                              *}
  {* Pset:                                        *}
  {*                                              *}
  {*  Used for debugging only. Plots a point      *}
  {*  in VRAM during render (not after)...        *}
  {*                                              *}
  {* NOTE:  This procedure relies on the value    *}
  {*        of cProfile->start, which may not     *}
  {*        be set when Pset is called sometimes  *}
  {*        This will usually result in a dot     *}
  {*        plotted on the first screen scanline  *}
  {*        (far away its original position).     *}
  {*                                              *}
  {*        This "bug" reflects nothing wrong     *}
  {*        in the current implementation, and    *}
  {*        the bitmap is rendered correctly,     *}
  {*        so don't panic if you see 'flying'    *}
  {*        dots in debugging mode.               *}
  {*                                              *}
  {************************************************/

  void Pset()
  {
    Long  o;
    Long  x;

    x = buff[profCur];

    switch (cProfile->flow)
    {
      case TT_Flow_Up: o = Vio_ScanLineWidth*
                           (profCur-cProfile->offset+cProfile->start) +
                           ( x / (Precision*8) );
                       break;

      case TT_Flow_Down: o = Vio_ScanLineWidth*
                              (cProfile->start-profCur+cProfile->offset) +
                           ( x / (Precision*8) );
                       break;
    }
    if ( o > 0 )
      Vio[o] |= (unsigned)0x80 >> ( (x/Precision) & 7 );
  }


  void Clear_Band( Int y1, Int y2 )
  {
    memset( Vio + y1*Vio_ScanLineWidth, (y2-y1+1)*Vio_ScanLineWidth, 0 );
  }

#endif /* DEBUG3 */

  /************************************************}
  {*                                              *}
  {* Init_Profile : init the profiles list.       *}
  {*                                              *}
  {************************************************/

  void Init_Profile()
  {
    cProfile         = (PProfile)( buff + profCur );
    cProfile->offset = profCur;
    nProfs           = 0;
  }

  /************************************************}
  {*                                              *}
  {* New_Profile :                                *}
  {*                                              *}
  {*  Creates a new profile in the list           *}
  {*                                              *}
  {************************************************/

  Bool New_Profile( TStates  aState )
  {
    if ( !fProfile )
    {
      cProfile  = (PProfile)( buff + profCur );
      fProfile  = cProfile;
      profCur  += AlignProfileSize;
    }

    if ( profCur >= maxBuff )
    {
      Raster_Error = Raster_Err_Overflow;
      return FAILURE;
    }

    switch (aState)
    {
      case Ascending : cProfile->flow = TT_Flow_Up;
                       break;

      case Descending: cProfile->flow = TT_Flow_Down;
                       break;

      default: Raster_Error = Raster_Err_Invalid;
               return FAILURE;
    }

    cProfile->start   = 0;
    cProfile->height  = 0;
    cProfile->offset  = profCur;
    cProfile->link    = (PProfile)0;

    if ( !gProfile ) gProfile = cProfile;

    state = aState;
    fresh = TRUE;
    joint = FALSE;

    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* End_Profile :                                *}
  {*                                              *}
  {*  Finalizes the current profile               *}
  {*                                              *}
  {************************************************/

  Bool  End_Profile()
  {
    Int h;

    h = profCur - cProfile->offset;

    if ( h < 0 )
    {
      Raster_Error = Raster_Err_Neg_Height;
      return FAILURE;
    }

    if ( h > 0 )
    {
      cProfile->height = h;
      cProfile         = (PProfile)(buff + profCur );
      profCur         += AlignProfileSize;
      cProfile->height = 0;
      cProfile->offset = profCur;
      nProfs++;
    }

    if ( profCur >= maxBuff )
    {
      Raster_Error = Raster_Err_Overflow;
      return FAILURE;
    }

    joint = FALSE;

    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Finalize_Profile_Table :                     *}
  {*                                              *}
  {*  Adjusts links in the profile list.          *}
  {*                                              *}
  {************************************************/

  void Finalize_Profile_Table()
  {
    Int     n;
    PProfile p;

    n = nProfs;
    if ( n > 1 )
    {
      p = fProfile;
      while ( n > 1 )
      {
        p->link = (PProfile)(buff + p->offset + p->height);
        p       = p->link;
        n--;
      }
      p->link = NULL;
    }
    else
      fProfile = NULL;
  }

  /************************************************}
  {*                                              *}
  {* Split_Bezier :                               *}
  {*                                              *}
  {*   Sudivizes one Bezier arc into two joint    *}
  {*   subarcs in the Bezier stack.               *}
  {*                                              *}
  {************************************************/

  void Split_Bezier()
  {
    Arcs[curArc+4].x = Arcs[curArc+2].x;
    Arcs[curArc+4].y = Arcs[curArc+2].y;

    Arcs[curArc+3].x = ( Arcs[curArc+2].x + Arcs[curArc+1].x ) / 2;
    Arcs[curArc+3].y = ( Arcs[curArc+2].y + Arcs[curArc+1].y ) / 2;

    Arcs[curArc+1].x = ( Arcs[ curArc ].x + Arcs[curArc+1].x ) / 2;
    Arcs[curArc+1].y = ( Arcs[ curArc ].y + Arcs[curArc+1].y ) / 2;

    Arcs[curArc+2].x = ( Arcs[curArc+3].x + Arcs[curArc+1].x ) / 2;
    Arcs[curArc+2].y = ( Arcs[curArc+3].y + Arcs[curArc+1].y ) / 2;

    /*

    **** Version discarded due to a gcc 2.7.2 compiler bug !! *****
    **** the following sequence was miscompiled with -O2 !!   *****
    **** no problem whatsoever with gcc 2.6.3                 *****
    ****                                                      *****

    Long x1, y1, x2, y2;

    x1 = Arcs[curArc+2].x;    y1 = Arcs[curArc+2].y;
    x2 = Arcs[ curArc ].x;    y2 = Arcs[ curArc ].y;

    Arcs[curArc+4].x = x1;    Arcs[curArc+4].y = y1;

    x1 += Arcs[curArc+1].x;   y1 += Arcs[curArc+1].y;
    x2 += Arcs[curArc+1].x;   y2 += Arcs[curArc+1].y;

    x1 = x1/2; x2 = x2/2;
    y1 = y1/2; y2 = y2/2;

    Arcs[curArc+3].x = x1;    Arcs[curArc+3].y = y1;
    Arcs[curArc+1].x = x2;    Arcs[curArc+1].y = y2;

    Arcs[curArc+2].x = (x1+x2)/2;
    Arcs[curArc+2].y = (y1+y2)/2;

    */

    curArc += 2;
  }

  /************************************************}
  {*                                              *}
  {* Push_Bezier :                                *}
  {*                                              *}
  {*   Pushes a new Bezier arc on top of the      *}
  {*   Bezier stack.                              *}
  {*                                              *}
  {************************************************/

  void Push_Bezier( Long x1, Long y1, Long x2, Long y2, Long x3, Long y3 ) 
  {
    curArc = 0;
    Arcs[curArc+2].x = x1;  Arcs[curArc+2].y = y1;
    Arcs[curArc+1].x = x2;  Arcs[curArc+1].y = y2;
    Arcs[ curArc ].x = x3;  Arcs[ curArc ].y = y3;
  }

  /************************************************}
  {*                                              *}
  {* Line_Up                                      *}
  {*                                              *}
  {*  Computes the x coordinates of an ascending  *}
  {*  segment and stores them in the profiles     *}
  {*  buffer.                                     *}
  {*                                              *}
  {************************************************/

  Bool  Line_Up( Long x1, Long y1, Long x2, Long y2 )
  {
    Long  Dx, Dy;
    Int   e1, e2, f1, f2, size;
    Long  Ix, Rx, Ax;

    Dx = x2-x1; Dy = y2-y1;

    if ( Dy <= 0 || y2 < MinY || y1 > MaxY ) return SUCCESS;

    if ( y1 < MinY )
    {
      x1 += MulDiv( Dx, MinY-y1, Dy );
      e1  = Trunc(MinY);
      f1  = 0;
    }
    else
    {
      e1 = Trunc(y1);
      f1 = Frac(y1);
    }

    if ( y2 > MaxY )
    {
      x2 += MulDiv( Dx, MaxY-y2, Dy );
      e2  = Trunc(MaxY);
      f2  = 0;
    }
    else
    {
      e2 = Trunc(y2);
      f2 = Frac(y2);
    }

    if ( f1 > 0 )
      if ( e1 == e2 ) return SUCCESS;
      else
      {
        x1 += MulDiv( Dx, Precision-f1, Dy );
        e1 += 1;
      }
    else
      if ( joint ) { profCur--; joint = FALSE; }

    if ( f2 > 0 ) x2 += MulDiv( Dx, -f2, Dy );
    else
      joint = TRUE;

    if ( fresh )
    {
      cProfile->start = e1;
      fresh           = FALSE;
    }

    if ( Dx > 0 )
    {
      Ix = (Precision*Dx) / Dy;
      Rx = (Precision*Dx) % Dy;
      Ax = 0;
      Dx = 1;
    }
    else
    {
      Ix = -((Precision*-Dx) / Dy );
      Rx =   (Precision*-Dx) % Dy;
      Ax =  0;
      Dx = -1;
    }

    size = e2-e1+1;
    if ( profCur + size >= maxBuff )
    {
      Raster_Error = Raster_Err_Overflow;
      return FAILURE;
    }

    do
    {
      buff[profCur] = x1;  DEBUG3_PSET;
      profCur++;

      x1 += Ix;
      Ax += Rx;
      if ( Ax >= Dy ) { Ax -= Dy; x1 += Dx; }
      e1 ++;
    }
    while ( e1 <= e2 );
    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Line_Down                                    *}
  {*                                              *}
  {*  Computes the x coordinates of a descending  *}
  {*  segment and stores them in the profiles     *}
  {*  buffer.                                     *}
  {*                                              *}
  {************************************************/

  Bool  Line_Down( Long x1, Long y1, Long x2, Long y2 )
  {
    Long  Dx, Dy;
    Int   e1, e2, f1, f2, size;
    Long  Ix, Rx, Ax;

    Dx = x2-x1; Dy = y2-y1;

    if ( Dy >= 0 || y1 < MinY || y2 > MaxY ) return SUCCESS;

    if ( y1 > MaxY )
    {
      x1 += MulDiv( Dx, MaxY-y1, Dy );
      e1  = Trunc(MaxY);
      f1  = 0;
    }
    else
    {
      e1 = Trunc(y1);
      f1 = Frac(y1);
    }

    if ( y2 < MinY )
    {
      x2 += MulDiv( Dx, MinY-y2, Dy );
      e2  = Trunc(MinY);
      f2  = 0;
    }
    else
    {
      e2 = Trunc(y2);
      f2 = Frac(y2);
    }

    if ( f1 > 0 ) x1 += MulDiv( Dx, -f1, Dy );
    else
      if ( joint ) { profCur--; joint = FALSE; }

    if ( f2 > 0 )
      if ( e1 == e2 ) return SUCCESS;
      else
      {
        x2 += MulDiv( Dx, Precision-f2, Dy );
        e2 += 1;
      }
    else
      joint = TRUE;

    if ( fresh )
    {
      cProfile->start = e1;
      fresh          = FALSE;
    }

    if ( Dx > 0 )
    {
      Ix = (Precision*Dx) / -Dy;
      Rx = (Precision*Dx) % -Dy;
      Ax = 0;
      Dx = 1;
    }
    else
    {
      Ix = -((Precision*-Dx) / -Dy );
      Rx = (Precision*-Dx) % -Dy;
      Ax = 0;
      Dx = -1;
    }

    Dy = -Dy;

    size = e1-e2+1;
    if ( profCur + size >= maxBuff )
    {
      Raster_Error = Raster_Err_Overflow;
      return FAILURE;
    }

    do
    {
      buff[profCur] = x1;  DEBUG3_PSET;
      profCur++;

      x1 += Ix;
      Ax += Rx;
      if ( Ax >= Dy ) { Ax -= Dy; x1 += Dx; }
      e1 --;
    }
    while ( e1 >= e2 );

    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Bezier_Up                                    *}
  {*                                              *}
  {*  computes the horiz.coordinates of an        *}
  {*  ascending Bezier arc and stores them in     *}
  {*  the profiles buffer.                        *}
  {*                                              *}
  {*  Applies only to the arc on top of the stack *}
  {*  The arc is popped before exit.              *}
  {*                                              *}
  {************************************************/

  Bool Bezier_Up()
  {
    Long x1, y1, x2, y2, e, e2, e0;
    Int  debArc, f1;

    y1 = Arcs[curArc+2].y;
    y2 = Arcs[curArc].y;

    if ( y2 < MinY || y1 > MaxY )
    {
      curArc -= 2;
      return SUCCESS;
    }
    
    e2 = Floor(y2);

    if ( e2 > MaxY ) e2 = MaxY;

    e0 = MinY;

    if ( y1 < MinY ) e = MinY;
    else
    {
      e  = Ceiling(y1);
      f1 = Frac(y1);
      e0 = e;

      if ( f1 == 0 )
      {
        if ( joint ) { profCur--; joint = FALSE; }

        buff[profCur] = Arcs[curArc+2].x;  DEBUG3_PSET;
        profCur++;

        e += Precision;
      }
    }

    if ( fresh )
    {
      cProfile->start = Trunc(e0);
      fresh = FALSE;
    }

    /* Table overflow ? */
    if ( profCur + Trunc(e2-e) + 1 >= maxBuff )
    {
      Raster_Error = Raster_Err_Overflow;
      return FAILURE;
    }

    debArc = curArc;

    while ( curArc >= debArc && e <= e2 )
    {
      joint = FALSE;

      y2 = Arcs[curArc].y;

      if ( y2 == e )
      {
        joint = TRUE;
        buff[profCur] = Arcs[curArc].x;  DEBUG3_PSET;
        profCur++;

        e += Precision;
        curArc -= 2;
      }
      else
        if ( y2 < e ) curArc -= 2;
       else
       {
         y1 = Arcs[curArc+2].y;
         if ( y2-y1 < Precision2 )
         {
           x1 = Arcs[curArc+2].x;
           x2 = Arcs[ curArc ].x;
           buff[profCur] = x1 + MulDiv( x2-x1, e-y1, y2-y1 ); DEBUG3_PSET;
           profCur++;

           curArc -= 2;
           e += Precision;
         }
         else
           Split_Bezier();
       }
    }

    curArc = debArc-2;
    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Bezier_Down                                  *}
  {*                                              *}
  {*  computes the horiz.coordinates of a         *}
  {*  descending Bezier arc and stores them in    *}
  {*  the profiles buffer.                        *}
  {*                                              *}
  {*  Applies only to the arc on top of the stack *}
  {*  The arc is popped before exit.              *}
  {*                                              *}
  {************************************************/

  Bool Bezier_Down()
  {
    Long x1, y1, x2, y2, e, e2, e0;
    Int  debArc, f1;

    y1 = Arcs[curArc+2].y;
    y2 = Arcs[ curArc ].y;

    if ( y1 < MinY || y2 > MaxY )
    {
      curArc -= 2;
      return SUCCESS;
    }
    
    e2 = Ceiling(y2);

    if ( e2 < MinY ) e2 = MinY;

    e0 = MaxY;

    if ( y1 > MaxY ) e = MaxY;
    else
    {
      e  = Floor(y1);
      f1 = Frac(y1);
      e0 = e;

      if ( f1 == 0 )
      {
        if ( joint ) { profCur--; joint = FALSE; }

        buff[profCur] = Arcs[curArc+2].x;  DEBUG3_PSET;
        profCur++;

        e -= Precision;
      }
    }

    if ( fresh )
    {
      cProfile->start = Trunc(e0);
      fresh = FALSE;
    }

    /* Table overflow ? */
    if ( profCur + Trunc(e-e2) + 1 >= maxBuff )
    {
      Raster_Error = Raster_Err_Overflow;
      return FAILURE;
    }

    debArc = curArc;

    while ( curArc >= debArc && e >= e2 )
    {
      joint = FALSE;

      y2 = Arcs[curArc].y;

      if ( y2 == e )
      {
        joint = TRUE;
        buff[profCur] = Arcs[curArc].x;  DEBUG3_PSET;
        profCur++;

        e -= Precision;
        curArc -= 2;
      }
      else
        if ( y2 > e ) curArc -= 2;
       else
       {
         y1 = Arcs[curArc+2].y;
         if ( y1-y2 < Precision2 )
         {
           x1 = Arcs[curArc+2].x;
           x2 = Arcs[ curArc ].x;

           buff[profCur] = x1 + MulDiv( x2-x1, e-y1, y2-y1 ); DEBUG3_PSET;
           profCur++;

           curArc -= 2;
           e -= Precision;
         }
         else
           Split_Bezier();
       }
    }

    curArc = debArc-2;
    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Line_To                                      *}
  {*                                              *}
  {*  Injects a line during profile rendering.    *}
  {*                                              *}
  {************************************************/
  
  Bool Line_To( Long x, Long y )
  {
    switch (state)
    {
      case Unknown: if ( y > LastY )
                        {
                         if ( !New_Profile( Ascending ) ) return FAILURE;
                        }
                        else
                        {
                          if ( y < LastY )
                            if ( !New_Profile( Descending ) ) return FAILURE;
                        }
                        break;

      case Ascending: if ( y < LastY )
                      {
                        if ( !End_Profile() ||
                             !New_Profile( Descending ) ) return FAILURE;
                      }
                      break;

      case Descending: if ( y > LastY )
                       {
                         if ( !End_Profile() ||
                              !New_Profile( Ascending ) ) return FAILURE;
                       }
                       break;
      default: ;
    }

    switch (state)
    {
      case Ascending : if ( !Line_Up  ( LastX, LastY, x, y )) return FAILURE;
                       break;

      case Descending: if ( !Line_Down( LastX, LastY, x, y )) return FAILURE;
                       break;
      default: ;
    }

   LastX = x;
   LastY = y;

   return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* BezierTo                                     *}
  {*                                              *}
  {*  Injects a Bezier arc during profile render  *}
  {*                                              *}
  {************************************************/

  Bool Bezier_To( Long x, Long y, Long cx, Long cy )
  {
    Long    y1, y2, y3, x3;
    TStates  state_bez;

    Push_Bezier( LastX, LastY, cx, cy, x, y );

    do
    {
      y1 = Arcs[curArc+2].y;
      y2 = Arcs[curArc+1].y;
      y3 = Arcs[ curArc ].y;
      x3 = Arcs[ curArc ].x;

      if ( y1 == y2 )
      {
        if ( y2 == y3 ) state_bez = Flat;
        else
        if ( y2 > y3 )  state_bez = Descending;
        else
                        state_bez = Ascending;
      }
      else
      if ( y1 > y2 )
      {
        if ( y2 >= y3 ) state_bez = Descending;
        else
                        state_bez = Unknown;
      }
      else
        if ( y2 <= y3 ) state_bez = Ascending;
        else
                        state_bez = Unknown;

      switch (state_bez)
      {
        case Flat: curArc -= 2;
                   break;

        case Unknown: Split_Bezier();
                      break;

        default: if ( state != state_bez )
                 {
                   if ( state != Unknown )
                     if ( !End_Profile() ) return FAILURE;

                   if ( !New_Profile( state_bez ) ) return FAILURE;
                 }
                 switch (state)
                 {
                   case Ascending : if ( !Bezier_Up  () ) return FAILURE;
                                    break;

                   case Descending: if ( !Bezier_Down() ) return FAILURE;
                                    break;
                   default: ;
                 }
      }
    }
    while ( curArc >= 0 );

    LastX = x3;
    LastY = y3;

    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Curve_To                                     *}
  {*                                              *}
  {*   Injects several Beziers in a row.          *}
  {*                                              *}
  {************************************************/

  Bool Curve_To( Long x, Long y, Int firstCtrl, Int lastCtrl )
  {
    Long  xz, yz, cx, cy;

    xz = xCoord[firstCtrl];
    yz = yCoord[firstCtrl];

    firstCtrl++;

    while ( firstCtrl <= lastCtrl )
    {
      cx = ( xz + xCoord[firstCtrl] )/2;
      cy = ( yz + yCoord[firstCtrl] )/2;

      if ( !Bezier_To( cx, cy, xz, yz ) ) return FAILURE;

      xz = xCoord[firstCtrl];
      yz = yCoord[firstCtrl];

      firstCtrl++;
    }

    return Bezier_To( x, y, xz, yz );
  }

 /************************************************}
 {*                                              *}
 {* Convert_Glyph                                *}
 {*                                              *}
 {*  Converts a glyph into a set of profiles.    *}
 {*                                              *}
 {************************************************/
 
 Bool Convert_Glyph( PStorage _xCoord, PStorage _yCoord )
 {
   Int   i, j, first, last, start;

   j        = 0;
   nProfs   = 0;
   fProfile = NULL;
   joint    = FALSE;
   fresh    = FALSE;
   last     = 0;

   xCoord = _xCoord;
   yCoord = _yCoord;

   Init_Profile();

   for ( i = 0; i < nContours; i++ )
   {
     state    = Unknown;
     first    = j;
     LastX    = xCoord[j];
     LastY    = yCoord[j];
     start    = 0;
     gProfile = NULL;

     j++;

     while ( j <= outs[i] )
     {
       if ( (flags[j] & 1) == 0 )   /* OFF Curve */

         if ( start == 0 )
         {
           start = j;
           last  = j;
         }
         else
           last++;

       else                         /* ON Curve */
         if ( start != 0 )
         {
           if ( !Curve_To( xCoord[j], yCoord[j], start, last ) )
             return FAILURE;
           start = 0;
         }
         else
           if ( !Line_To( xCoord[j], yCoord[j] ) )
             return FAILURE;

       j++;
     }

     if ( start != 0 )
     {
       if ( !Curve_To( xCoord[first], yCoord[first], start, last ) )
         return FAILURE;
     }
     else
       if ( !Line_To( xCoord[first], yCoord[first] ) )
         return FAILURE;

     /* We must now see if the extreme arcs join or not */

     if ( ( Frac(LastY) == 0   &&
            LastY >= MinY      &&
            LastY <= MaxY ) )

       if ( gProfile && gProfile->flow == cProfile->flow )
         profCur--;
       /* Note that gProfile can be nil if the contour was too small */
       /* to be drawn                                               */

     if ( !End_Profile() ) return FAILURE;
   }

   Finalize_Profile_Table();

   return SUCCESS;
 }

  /************************************************}
  {*                                              *}
  {* Draw_Horizontal_Spans :                      *}
  {*                                              *}
  {*  Draws the glyph with the help of the        *}
  {*  profile table generated by 'Convert_Glyph'  *}
  {*                                              *}
  {************************************************/

  /* Ins_New : Insert new profile to trace table */

  void Ins_New( PTraceRec  trace, PProfile  profile, Long  x )
  {
    int          i, j, n;
    PTraceArray  t;

    i = 0;
    t = trace->t;
    n = trace->n;

    while ( i < n && t[i].x <= x ) i++;

    if ( i < n )
      for ( j = n-1; j >= i; j-- )
      {
        memcpy( t + j+1, t + j, sizeof( *t ) );
        t[j+1].profile->index = j+1;
      }

    t[i].profile   = profile;
    t[i].x         = x;
    profile->index = i;

    trace->n ++;
  }

  /* Del_Old : remove a profile from a trace table */

  void Del_Old( PTraceRec  trace, Int index )
  {
    int         i, n;
    PTraceArray t;

    n = trace->n;
    t = trace->t;

    t[index].profile->index = -1;

    for ( i = index; i < n-1; i++ )
    {
      memcpy( t+i, t+i+1, sizeof(*t) );
      t[i].profile->index = i;
    }
    trace->n --;
  }

  /* Sort : sort a trace table. Ugly bubble sort, but I had not */
  /*        the time to optimize it ..                          */
  /*        and anyway, the list is only sorted when profiles   */
  /*        cross, are born or die..                            */

  void Sort( PTraceRec  trace )
  {
    Int  i, j, n;
    Long k, l;

    PTraceArray t;
    PProfile    q;

    t = trace->t;
    n = trace->n;

    k = t[0].x;
    for ( i = 1; i > n; i++ )
    {
      l = t[i].x;

      if ( k > l )
      {
        for ( i = 1; i < n; i++ )
          for ( j = i; j >= 1; j-- )
          {
            if ( t[j].x < t[j-1].x )
            {
              k        = t[j-1].x;
              t[j-1].x = t[ j ].x;
              t[ j ].x = k;

              q             = t[j-1].profile;
              t[j-1].profile = t[ j ].profile;
              t[ j ].profile = q;

              t[j-1].profile->index = j-1;
              q->index             = j;
            }
          }
        return;
      }
      else
        k = l;
    }
  }


  Bool Draw_Horizontal_Spans()
  {
    Int  y, i, j;

    PProfile p, q;

    Int  min_y, max_y;

    Long  e1, e2, x1, x2;

    Int   c1, c2, f1, f2;

    /* We first find the Y extrema */

    p     = fProfile;
    max_y = Trunc(MinY);
    min_y = Trunc(MaxY);

    while ( p )
    {
      switch (p->flow)
      {
        case TT_Flow_Up: if ( min_y > p->start )
                           min_y = p->start;

                         if ( max_y < p->start + p->height - 1 )
                           max_y = p->start + p->height -1;

                         p->start_line = p->start;
                         p->index      = -1;
                         p->trace      = &trace_left;
                         break;

        case TT_Flow_Down: if ( min_y > p->start - p->height + 1 )
                             min_y = p->start - p->height + 1;

                           if ( max_y < p->start )
                             max_y = p->start;

                           p->start_line = p->start - p->height + 1;
                           p->offset    += p->height-1;
                           p->index      = -1;
                           p->trace      = &trace_right;
                           break;

        default: Raster_Error = Raster_Err_Invalid;
                 return FAILURE;
      }

      p = p->link;
    }

    /* We compute the 'distance to minimum' for each profile */

   p = fProfile;

   while ( p )
   {
     p->count_lines = p->start_line - min_y + 1;
     p              = p->link;
   }

   /* A little setup */

   traceOfs = target.cols      * min_y;
#ifdef DEBUG3
   debugOfs = Vio_ScanLineWidt * min_y;
#endif
   trace_right.n = 0;
   trace_right.n = 0;

   /* Let's do it ! */

   for ( y = min_y; y <= max_y; y++ )
   {
     p = fProfile;

     while (p)
     {
       if ( p->count_lines > 0 )
       {
         p->count_lines --;
         if ( p->count_lines == 0 )
         {
           Ins_New( p->trace, p, buff[p->offset] );
           p->offset += p->flow;
           p->height --;
         }
       }
       else
       if ( p->count_lines == 0 )
       {
         p->trace->t[p->index].x = buff[p->offset];
         p->offset += p->flow;
         p->height --;
       }

       p = p->link;
     }

     /* Let's sort now */

     Sort( &trace_left  );
     Sort( &trace_right );

     /* Let's then trace */

     i = 0;

     while ( i < trace_left.n )
     {
       x1 =  trace_left.t[i].x;
       x2 = trace_right.t[i].x;

#ifdef REVERSE
       if ( x1 > x2 )
       {
         e1 = x1;
         x1 = x2;
         x2 = e1;
       }
#endif

       e1 = Ceiling(x1);
       e2 = Floor(x2);

       /* Drop-out control */

       if ( e1 > e2 )
         if ( e1 == e2+Precision )
           switch (dropOutControl)
           {
             case TT_DropOut_Control_Simple :

                   e2 = e1;
                   break;

             case TT_DropOut_Control_Complex :

                    p =  trace_left.t[i].profile;
                    q = trace_right.t[i].profile;

                    if ( p->height <= 0 || q->height <= 0 )
                      goto No_Draw;

                    if ( y <= p->start_line || y <= q->start_line )
                      goto No_Draw;

                    e2 = e1;
                    break;

              default:
                    goto No_Draw;
           }

       e1 = Trunc(e1);
       e2 = Trunc(e2);

       /* The following typecast (signed long) is due to the */
       /* poor arithmetic parser of Borland C++ 3.1          */

       if ( e2 >= 0 && e1 < (signed long) target.width )
       {
         if ( e1 < 0 ) e1 = 0;
         if ( e2 >= target.width ) e2 = target.width-1;

         c1 = e1 >> 3;
         c2 = e2 >> 3;

         f1 = e1 & 7;
         f2 = e2 & 7;

         j = traceOfs + c1;

         if ( c1 == c2 )
           bTarget[j] |= ( LMask[f1] & RMask[f2] );
         else
         {
           bTarget[j] |= LMask[f1];
           
           if ( c2 > c1+1 )
             memset( bTarget+j+1, 0xFF, c2-c1-1 );
           
           j += c2-c1;

           bTarget[j] |= RMask[f2];
         }

#ifdef DEBUG3

         j = debugOfs + c1;

         if ( c1 == c2 )
           Vio[j] |= ( LMask[f1] & RMask[f2] );
         else
         {
           Vio[j] |= LMask[f1];

           if ( c2 > c1+1 )
             memset( Vio+j+1, 0x55, c2-c1-1 );

           j += c2-c1;

           Vio[j] |= RMask[f2];
         }
#endif /* DEBUG3 */

       }

    No_Draw:

       i++;
     }

     traceOfs += target.cols;
#ifdef DEBUG3
     debugOfs += Vio_ScanLineWidth;
#endif
     p = fProfile;

     while ( p )
     {
       if ( p->count_lines == 0 && p->height == 0 )
       {
         Del_Old( p->trace, p->index );
         p->height      = -1;
         p->count_lines = -1;
       }
       p = p->link;
     }
   }

   return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Draw_Gray_Horizontal_Spans :                 *}
  {*                                              *}
  {*  Draws the glyph with the help of the        *}
  {*  profile table generated by 'Convert_Glyph'  *}
  {*                                              *}
  {* NOTE : This procedure differs only slightly  *}
  {*        from Draw_Horizontal_Spans. It has    *}
  {*        not been "incorporated" yet for       *}
  {*        debugging purpose.                    *}
  {*                                              *}
  {************************************************/

  Bool Draw_Gray_Horizontal_Spans()
  {
    Int  y, i, j;

    PProfile p, q;

    Int  min_y, max_y;

    Int  min_x, max_x;

    Long  e1, e2, x1, x2;

    Int   c1, c2, f1, f2;

    Int   traceG;

    /* We first find the Y extrema */

    p     = fProfile;
    max_y = Trunc(MinY);
    min_y = Trunc(MaxY);

    /* Graylevel render */
    memset( gray_lines, 0, gray_width*2 );

    min_x = target.cols;
    max_x = 0;
    /* ---------------- */

    while ( p )
    {
      switch (p->flow)
      {
        case TT_Flow_Up: if ( min_y > p->start )
                           min_y = p->start;

                         if ( max_y < p->start + p->height - 1 )
                           max_y = p->start + p->height -1;

                         p->start_line = p->start;
                         p->index      = -1;
                         p->trace      = &trace_left;
                         break;

        case TT_Flow_Down: if ( min_y > p->start - p->height + 1 )
                             min_y = p->start - p->height + 1;

                           if ( max_y < p->start )
                             max_y = p->start;

                           p->start_line = p->start - p->height + 1;
                           p->offset    += p->height-1;
                           p->index      = -1;
                           p->trace      = &trace_right;
                           break;

        default: Raster_Error = Raster_Err_Invalid;
                 return FAILURE;
      }

      p = p->link;
    }

   /* Graylevel render  */
   min_y = min_y & -2;
   max_y = ( max_y + 3 ) & -2;
   /* ----------------- */

    /* We compute the 'distance to minimum' for each profile */

   p = fProfile;

   while ( p )
   {
     p->count_lines = p->start_line - min_y + 1;
     p              = p->link;
   }

   /* A little setup */

   traceOfs = target.cols * (min_y/2);
/* debugOfs =          80 * min_y; */
   traceG   = 0;

   trace_right.n = 0;
   trace_right.n = 0;

   /* Let's do it ! */

   for ( y = min_y; y <= max_y; y++ )
   {
     p = fProfile;

     while (p)
     {
       if ( p->count_lines > 0 )
       {
         p->count_lines --;
         if ( p->count_lines == 0 )
         {
           Ins_New( p->trace, p, buff[p->offset] );
           p->offset += p->flow;
           p->height --;
         }
       }
       else
       if ( p->count_lines == 0 )
       {
         p->trace->t[p->index].x = buff[p->offset];
         p->offset += p->flow;
         p->height --;
       }

       p = p->link;
     }

     /* Let's sort now */

     Sort( &trace_left  );
     Sort( &trace_right );

     /* Let's then trace */

     i = 0;

     while ( i < trace_left.n )
     {
       x1 =  trace_left.t[i].x;
       x2 = trace_right.t[i].x;

#ifdef REVERSE
       if ( x1 > x2 )
       {
         e1 = x1;
         x1 = x2;
         x2 = e1;
       }
#endif

       e1 = Ceiling(x1);
       e2 = Floor(x2);

       /* Drop-out control */

       if ( e1 > e2 )
         if ( e1 == e2+Precision )
           switch (dropOutControl)
           {
             case TT_DropOut_Control_Simple :

                   e2 = e1;
                   break;

             case TT_DropOut_Control_Complex :

                    p =  trace_left.t[i].profile;
                    q = trace_right.t[i].profile;

                    if ( p->height <= 0 || q->height <= 0 )
                      goto No_Draw;

                    if ( y <= p->start_line || y <= q->start_line )
                      goto No_Draw;

                    e2 = e1;
                    break;

              default:
                    goto No_Draw;
           }

       e1 = Trunc(e1);
       e2 = Trunc(e2);

       /* The following typecast (signed long) is due to the */
       /* poor arithmetic parser of Borland C++ 3.1          */

       if ( e2 >= 0 && e1 < (signed long) target.width )
       {
         if ( e1 < 0 ) e1 = 0;
         if ( e2 >= target.width ) e2 = target.width-1;

         /* Graylevel render */
         c1 = e1 >> 3;
         c2 = e2 >> 3;
         /* ---------------- */

         if ( min_x > c1 ) min_x = c1;
         if ( max_x < c2 ) max_x = c2;

         f1 = e1 & 7;
         f2 = e2 & 7;

         j = traceG + c1;

         if ( c1 == c2 )
           gray_lines[j] |= ( LMask[f1] & RMask[f2] );
         else
         {
           gray_lines[j] |= LMask[f1];
           
           if ( c2 > c1+1 )
             memset( gray_lines+j+1, 0xFF, c2-c1-1 );
           
           j += c2-c1;

           gray_lines[j] |= RMask[f2];
         }
/*
#ifdef DEBUG3

         j = debugOfs + c1;

         if ( c1 == c2 )
           Vio[j] |= ( LMask[f1] & RMask[f2] );
         else
         {
           Vio[j] |= LMask[f1];

           if ( c2 > c1+1 )
             memset( Vio+j+1, 0x55, c2-c1-1 );

           j += c2-c1;

           Vio[j] |= RMask[f2];
         }
#endif
*/
       }

    No_Draw:

       i++;
     }

/*   traceOfs += target.cols; */
/*   debugOfs += 80; */

     traceG += gray_width;

     if ( traceG > gray_width )
     {
       j = traceOfs + min_x*4;

       if ( max_x >= 0 )
       {

         if ( max_x >= target.cols/4-1 ) max_x = target.cols/4-1;
         if ( min_x < 0 ) min_x = 0;
  
          for ( c1 = min_x; c1 <= max_x; c1++ )
          {
            c2 = count_table[ gray_lines[c1           ] ] + 
                 count_table[ gray_lines[c1+gray_width] ];
  
            if (c2)
            {
              bTarget[j++] |= gray_palette[ (unsigned)(c2 & 0xF000) >> 12 ];
              bTarget[j++] |= gray_palette[ (unsigned)(c2 & 0x0F00) >>  8 ];
              bTarget[j++] |= gray_palette[ (unsigned)(c2 & 0x00F0) >>  4 ];
              bTarget[j++] |= gray_palette[ (unsigned)(c2 & 0x000F)       ];
  
              gray_lines[c1           ] = 0;
              gray_lines[c1+gray_width] = 0;
            }
            else
              j += 4;
          }
        }

        traceG = 0;
        traceOfs += target.cols;

        min_x =  target.cols;
        max_x = -target.cols;
     }

     p = fProfile;

     while ( p )
     {
       if ( p->count_lines == 0 && p->height == 0 )
       {
         Del_Old( p->trace, p->index );
         p->height      = -1;
         p->count_lines = -1;
       }
       p = p->link;
     }
   }

   return SUCCESS;
  }


  /************************************************}
  {*                                              *}
  {* Render_Glyph :                               *}
  {*                                              *}
  {*  Render a glyph whose description is in      *}
  {*  the 'AGlyph' variable.                      *}
  {*                                              *}
  {************************************************/

  int Render_Glyph( TGlyphRecord *AGlyph )
  {
    Int      i, j, k;
    Int      profIni;

    if ( !buff )
    {
      Raster_Error = Raster_Err_Not_Ini;
      return FAILURE;
    }

    outs      = (PShort) AGlyph->outStarts;
    flags     = (PByte)  AGlyph->flag;
    nPoints   = AGlyph->points;
    nContours = AGlyph->outlines;

    i = 64*sizeof( TTraceRec );

    profCur = ( 2*i+3 )/4;

    /* XXX : Right now, we allocate two 64-elements trace arrays */
    /*       from the render pool. This limits us to glyphs of   */
    /*       less than 64 contours and wastes a significant      */
    /*       amount of memory. Moreover, the cross-referenced    */
    /*       pointer in the profiles and trace elements makes    */
    /*       sorting a heavy task ( see Ins_New and Del_Old      */
    /*       above ), though it happens quite rarely. Using      */
    /*       linked list from within the profiles headers        */
    /*       would solve the storage problem, and may result     */
    /*       in a faster rendering. This has to be tested. Later */
    /*       of course..                                         */

    trace_left.t  = (PTraceArray)buff;
    trace_right.t = (PTraceArray)(buff+(i+3)/4);

    profIni = profCur;

    band_top            = 0;
    band_stack[0].y_min = 0;
    band_stack[0].y_max = target.rows-1;

    bTarget = ( unsigned char*)target.bitmap;

    while ( band_top >= 0 )
    {
      MaxY = band_stack[band_top].y_max * Precision;
      MinY = band_stack[band_top].y_min * Precision;

      profCur = profIni;
      
      if ( !Convert_Glyph( AGlyph->xCoord, AGlyph->yCoord ) )
      {
        /* sub-banding */

#ifdef DEBUG3
        Clear_Band( Trunc(MinY), Trunc(MaxY) );
#endif /* DEBUG3 */

       i = band_stack[band_top].y_min;
       j = band_stack[band_top].y_max;

       k = ( i+j )/2;

       if ( band_top >= 8 || k <= i )
       {
         band_top     = -1;
         Raster_Error = Raster_Err_Invalid;
         return FAILURE;
       }

       band_stack[band_top+1].y_min = k;
       band_stack[band_top+1].y_max = j;

       band_stack[band_top].y_max = k-1;

       band_top++;
      }
      else
      {
        if ( fProfile )
          if ( !Draw_Horizontal_Spans() ) return FAILURE;
        band_top --;
      }
    }

    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* Render_Gray_Glyph :                          *}
  {*                                              *}
  {*  Gray renders a glyph whose description is   *}
  {*  in the 'AGlyph' variable.                   *}
  {*                                              *}
  {************************************************/

  int Render_Gray_Glyph( TGlyphRecord *AGlyph,
                         char*         palette )
  {
    Int      i, j, k;
    Int      profIni;

    if ( !buff )
    {
      Raster_Error = Raster_Err_Not_Ini;
      return FAILURE;
    }

    if (palette) memcpy( gray_palette, palette, 5 );

    outs      = (PShort) AGlyph->outStarts;
    flags     = (PByte)  AGlyph->flag;
    nPoints   = AGlyph->points;
    nContours = AGlyph->outlines;

    i = 64*sizeof( TTraceRec );

    profCur = ( 2*i+3 )/4;

    trace_left.t  = (PTraceArray)buff;
    trace_right.t = (PTraceArray)(buff+(i+3)/4);

    profIni = profCur;

    band_top            = 0;
    band_stack[0].y_min = 0;
    band_stack[0].y_max = 2*target.rows-1;

    bTarget = ( unsigned char*)target.bitmap;

    while ( band_top >= 0 )
    {
      MaxY = band_stack[band_top].y_max * Precision;
      MinY = band_stack[band_top].y_min * Precision;

      profCur = profIni;

      if ( !Convert_Glyph( AGlyph->xCoord, AGlyph->yCoord ) )
      {
        /* sub-banding */

#ifdef DEBUG3
        Clear_Band( Trunc(MinY), Trunc(MaxY) );
#endif /* DEBUG3 */

       i = band_stack[band_top].y_min;
       j = band_stack[band_top].y_max;

       k = ( i+j )/2;

       if ( band_top >= 8 || k <= i )
       {
         band_top     = -1;
         Raster_Error = Raster_Err_Invalid;
         return FAILURE;
       }

       band_stack[band_top+1].y_min = k;
       band_stack[band_top+1].y_max = j;

       band_stack[band_top].y_max = k-1;

       band_top++;
      }
      else
      {
        if ( fProfile )
          if ( !Draw_Gray_Horizontal_Spans() ) return FAILURE;
        band_top --;
      }
    }

    return SUCCESS;
  }

  /************************************************}
  {*                                              *}
  {* InitRasterizer                               *}
  {*                                              *}
  {*  Raster Initialization.                      *}
  {*  Get the bitmap description and render pool  *}
  {*  addresses.                                  *}
  {*                                              *}
  {************************************************/

  int  InitRasterizer( TRasterBlock*  rasterBlock,
                       long*          profBuffer,
                       long           profSize,
                       char*          grayBuffer,
                       int            grayLength )
  {
    int i, l, j, c;

    if ( !profBuffer || profSize < 0 )
      return FAILURE;

    /* XXX We should also check the contents of the passed raster block */

    buff    = profBuffer;
    maxBuff = (profSize/4)-AlignProfileSize;
    memcpy( &target, rasterBlock, sizeof(target) );

    gray_lines = (Byte *) grayBuffer;
    gray_width = grayLength/4;

    /* Initialization of Count_Table */

    for ( i = 0; i < 256; i++ )
    {
      l = 0;
      j = i;
      for ( c = 0; c < 4; c++ )
      {
        l <<= 4;

        if ( j & 0x80 ) l++;
        if ( j & 0x40 ) l++;

        j = ( j << 2 ) & 0xFF;
      }

      count_table[i] = l;
    }

    /* default gray_palette takes the gray levels of the standard VGA */
    /* 256 colors mode                                                */

    gray_palette[0] = 0;
    gray_palette[1] = 23;
    gray_palette[2] = 27;
    gray_palette[3] = 29;
    gray_palette[4] = 31;

    dropOutControl = 2;
    Raster_Error   = Raster_Err_None;
    return SUCCESS;
  }
