/*******************************************************************
 *
 *  TTCalc.c
 *
 *    Arithmetic and Vectorial Computations (body).
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

#include "ttcalc.h"
#include "tterror.h"
#include "tttables.h"

#define Sign_Bit 0x80000000

/* The following macro is used to test the sign of a 2's complement */
/* value during computations                                        */

#ifndef ONE_COMPLEMENT
#define Test_Neg(x) ((signed)(x)<0)
#else
#define Test_Neg(x) ((x) & Sign_Bit)
#endif

  const long Roots[63] =
  {
               1,    1,    2,     3,     4,     5,     8,    11,
              16,   22,   32,    45,    64,    90,   128,   181,
             256,  362,  512,   724,  1024,  1448,  2048,  2896,
            4096, 5892, 8192, 11585, 16384, 23170, 32768, 46340,

              65536,   92681,  131072,   185363,   262144,   370727,
             524288,  741455, 1048576,  1482910,  2097152,  2965820,
            4194304, 5931641, 8388608, 11863283, 16777216, 23726566,

              33554432,   47453132,   67108864,   94906265,
             134217728,  189812531,  268435456,  379625062,
             536870912,  759250125, 1073741824, 1518500250,
            2147483647
  };

#if FT_BYTE_ORDER == FT_LITTLE_ENDIAN

  void Do16( unsigned short *S )
  {
    *S = ( (*S >> 8) | (*S << 8) );
  }



  void Do32( unsigned long *L )
  {
    *L = (   (*L >> 24) |
           ( (*L >>  8) & 0x0000FF00 ) |
           ( (*L <<  8) & 0x00FF0000 ) |
             (*L << 24) );
  }


  void Do16s( unsigned short *S, Int cnt )
  {
    while ( cnt > 0 )
    {
      *S = (*S >> 8) | (*S << 8);
      S++;
      cnt--;
    }
  }


  void Do32s( unsigned long *L, Int cnt )
  {
    while ( cnt > 0 )
    {
      *L = (   (*L >> 24) |
             ( (*L >>  8) & 0x0000FF00 ) |
             ( (*L <<  8) & 0x00FF0000 ) |
               (*L << 24) );
      L++;
      cnt--;
    }
  }

#endif /* LITTLE_ENDIAN */

  Int32 MulDiv( Int32 a, Int32 b, Int32 c )
  {
    /* Here's the inline assembly to use on Intel processors */
    /*                                                       */
    /* mov ecx, dword ptr [ C ]                              */
    /* mov ebx, dword ptr [ B ]                              */
    /* mov eax, dword ptr [ A ]                              */
    /* idiv ecx                                              */
    /*                           result in 'eax'             */
    /*                                                       */
    /* no exception ( divide0/overflow ) handling !!         */

    /* A well optimizing compiler could do 'return (A*B)/C' directly */

    /* We could do a "MulTo64(a,b,&z); return Div64by32( &z, c );" but */
    /* it is faster to compute the result directly. It's only a        */
    /* slightly modified division algorithm..                          */

    Int32 s1, s2, q, r, ar, aq;
    Int   i;

    s1 = a & Sign_Bit; if (s1) a = -a;
    s2 = b & Sign_Bit; if (s2) b = -b;

    s1 = s1^s2;
    s2 = c & Sign_Bit; if (s2) c = -c;
    s1 = s1^s2;

    if ( s1 )
      s2 = 0x80000001;
    else
      s2 = 0x7FFFFFFF;

    if ( c == 0 ) return s2;  /* Divide by zero --> Overflow */

    /* Useful Shortcut, most of the calculations fall there */

    if ( (( a | b ) >> 16) == 0 ) /* 32 bits are enough !! */
    {
      q = a*b/c;
      if (s1) q = -q;
      return q;
    }

    /* Otherwise, we really need those 64 bits !! */

    r  = 0;
    q  = 0;
    ar = a % c;
    aq = a / c;

    for ( i = 0; i < 32; i++ )
    {
      r <<= 1;
      q <<= 1;
      if ( b >> 31 )
      {
        r += ar;
        q += aq;

#ifdef BOUND_CALC
          if (Test_Neg(q)) return s2;   /* Overflow check */
#endif
      }
      b <<= 1;

      while ( (signed)(r-c) >= 0 )
      {
        r = r-c;
        q++;

#ifdef BOUND_CALC
          if (Test_Neg(q)) return s2;  /* Overflow check */
#endif
      }
    }

    if (s1) q = -q;

    return q;
  }


  void Neg64( Int64 *x )
  {
    /* Remember that -(0x80000000) == 0x80000000 with 2-complement !! */
    /* We take care of that here                                      */

    x->hi ^= 0xFFFFFFFF;
    x->lo ^= 0xFFFFFFFF;
    x->lo++;
    if (!x->lo)
    {
      x->hi++;
      if ( (signed)x->hi == Sign_Bit )  /* Check -MaxInt32-1 */
      {
        x->lo--;
        x->hi--;  /* We return 0x7FFFFFFF !! */
      }
    }
  }


  void Add64( Int64 *x, Int64 *y, Int64 *z )
  {
    /* Here's the inline assembly to use on Intel processors */
    /*                                                       */
    /* mov eax, dword ptr [ x ]                              */
    /* mov edx, dword ptr [x+4]                              */
    /* add eax, dword ptr [ y ]                              */
    /* adc edx, dword ptr [y+4]                              */
    /* mov dword ptr [ z ], eax                              */
    /* mov dword ptr [z+4], edx                              */
    /*                                                       */

    register Word32 lo, hi;

    hi = x->hi + y->hi;
    lo = x->lo + y->lo;

    if ( y->lo )
      if ( x->lo >= (unsigned)-y->lo ) hi++;

    z->lo = lo;
    z->hi = hi;
  }



  void Sub64( Int64 *x, Int64 *y, Int64 *z )
  {
    /* Here's the inline assembly to use on Intel processors */
    /*                                                       */
    /* mov eax, dword ptr [ x ]                              */
    /* mov edx, dword ptr [x+4]                              */
    /* sub eax, dword ptr [ y ]                              */
    /* sbb edx, dword ptr [y+4]                              */
    /* mov dword ptr [ z ], eax                              */
    /* mov dword ptr [z+4], edx                              */
    /*                                                       */

    register Word32 lo, hi;

    hi = x->hi - y->hi;
    lo = x->lo - y->lo;

    if ( x->lo < y->lo ) hi--;

    z->lo = lo;
    z->hi = hi;
  }



  void MulTo64( Int32 x, Int32 y, Int64* z )
  {
    /* Here's the inline assembly to use on Intel processors */
    /*                                                       */
    /* mov eax, dword ptr [ x ]                              */
    /* mov ebx, dword ptr [ y ]                              */
    /* imul ebx                                              */
    /* mov dword ptr [ z ], eax                              */
    /* mov dword ptr [z+4], edx                              */
    /*                                                       */

    Int32  s1, s2;
    Word32 lo1, hi1, lo2, hi2, lo, hi, i1, i2;

    /* The following is based on the formula : */
    /*
    /   ( al + T.ah )*( bl + T.bh ) = al.bl + T.(al.bh + ah.bl) + T.T.(ah.bh)
    /
    /   where T = 1 << 16.
    /
    /   Gosh !! That's Ugly !!
    /                            */

    s1 = x & Sign_Bit;
    s2 = y & Sign_Bit;

    if (s1) x = -x;
    if (s2) y = -y;

    lo1 = x & 0x0000FFFF;  hi1 = x >> 16;
    lo2 = y & 0x0000FFFF;  hi2 = y >> 16;

    lo = lo1*lo2;
    i1 = lo1*hi2;
    i2 = lo2*hi1;
    hi = hi1*hi2;

    /* Check carry overflow of i1+i2 */

    if ( i2 )
    {
      if ( i1 >= (unsigned)-i2 ) hi += 1 << 16;
      i1 += i2;
    }

    i2 = i1 >> 16;
    i1 = i1 << 16;

    /* Check carry overflow of i1+lo */
    if ( i1 )
    {
      if ( lo >= (unsigned)-i1 ) hi++;
      lo += i1;
    }

    hi += i2;

    z->lo = lo;
    z->hi = hi;

    if ( s1^s2 ) Neg64(z);
  }



  Int32 Div64by32( Int64* x, Int32 y )
  {
    /* Here's the inline assembly to use on Intel processors */
    /*                                                       */
    /* mov ebx, [ x  ]                                       */
    /* mov eax, dword ptr [ ebx ] ; x->lo                    */
    /* mov edx, dword ptr [ebx+4] ; y->lo                    */
    /* mov ebx, [ y ]                                        */
    /* idiv ebx                                              */
    /*                                                       */

    /*  We're going to do the division bit by bit            */
    /*  ourselves, as it seems to be the easiest way         */
    /*  ( maybe not the fastest, though .. )                 */
    /*                                                       */

    Word32 s1, s2, q, r, i, lo;

    s1 = x->hi & Sign_Bit;
    s2 = y     & Sign_Bit;

    if ( s1 ) Neg64(x);
    if ( s2 ) y = -y;

    s1 = s1^s2;
    if (s1)
      s2 = 0x80000001;  /* MinInt32 */    /* Overflow result */
    else                                  /* in s2           */
      s2 = 0x7FFFFFFF;  /* MaxInt32 */

    /* Shortcut */

    if ( x->hi == 0 )
      return x->lo / y;

    r  = x->hi;
    lo = x->lo;

    if ( r >= y ) 
      return s2;  /* Return Max/Min Int32 if divide overflow */
                  /* This includes division by zero !!       */
    q = 0;

    for ( i = 0; i < 16; i++ )
    {
      r <<= 1;
      q <<= 1;
      r  |= lo >> 31;

      if ( !Test_Neg(r-y) )
      {
        r  = r-y;
        q |= 1;
      };
      lo <<= 1;
    }

    if ( s1 )
      q = -q;

    return q;
  }



  Int  Order64( Int64* z )
  {
    Word32 i;
    int j;

    if ( z->hi )
    {
      i = z->hi;
      j = 16;
    }
    else
    {
      i = z->lo;
      j = 0;
    }

    while ( i > 0 ) { i >>= 1; j++; }

    return j-1;
  }



  Int Order32( Int32 z )
  {
    int j;

    j = 0;
    while ( z ) { z = (unsigned)z >> 1; j++; }

    return j-1;
  }



  Int32 Sqrt32( Int32 l )
  {
    Int32 r, s;

    if ( l <= 0 ) return 0;
    if ( l == 1 ) return 1;

    r = Roots[ Order32(l) ];

    do
    {
      s = r;
      r = ( r + l/r ) >> 1;
    }
    while ( r > s || r*r > l );

    return r;
  }



  Int32 Sqrt64( Int64* l )
  {
    Int64 l2;
    Int32 r, s;

    if ( Test_Neg(l->hi) ) return 0;
    s = Order64( l );
    if ( s == 0 ) return 1;
    r = Roots[s];
    MulTo64( r, r, &l2 );

    while ( r > s || l2.hi & Sign_Bit )
    {
      s = r;
      r = ( r + Div64by32( l, r ) ) >> 1;
      MulTo64( r, r,   &l2 );
      Sub64  ( l, &l2, &l2 );
    }
    return r;
  }



  /*******************************************************)
  (* Special routine used to compute point displacements *)
  (*                                                     *)
  (* This routine could be optimized to inline assembly  *)
  (* for those of you concerned with speed               */

  Bool  MulVec( Int64          *L,   /* This is a 48 bits integer  */
                TT_UnitVector   F,   /* Freedom vector             */
                TT_UnitVector   P,   /* Projection vector          */
                TT_Vector      *R )  /* Result vector              */
  {
    Int32 b;
    Int64 t, t2;

    b = (Int32)F.x * (Int32)P.x + (Int32)F.y * (Int32)P.y;
    if ( b == 0 )
    {
      R->x = 0;
      R->y = 0;
      return FAILURE;
    }

    MulTo64( L->lo, F.x, &t  );
    MulTo64( L->hi, F.x, &t2 );

    t.hi += t2.lo;

    R->x = Div64by32( &t, b );  /* Rx = L*Fx/B */

    MulTo64( L->lo, F.y, &t  );
    MulTo64( L->hi, F.y, &t2 );
    t.hi += t2.lo;

    R->y = Div64by32( &t, b );  /* Ry = L*Fy/B */

    return SUCCESS;
  }
               


  TT_F26Dot6 Norm( TT_F26Dot6 x, TT_F26Dot6 y )
  {
    Int64 t1, t2;
    MulTo64( x, x, &t1 );
    MulTo64( y, y, &t2 );
    Add64( &t1, &t2, &t1 );
    return Sqrt64( &t1 );
  }



  TT_F2Dot14 UnitNorm( TT_UnitVector v )
  {
    Int32 x, y;

    x = (Int32)v.x;
    y = (Int32)v.y;

    return Sqrt32( x*x + y*y );
  }



  TT_F26Dot6 Dot( TT_Vector* u, TT_Vector* v )
  {
    Int64 t1, t2;

    MulTo64( u->x, v->x, &t1 );
    MulTo64( u->y, v->y, &t2 );
    Add64( &t1, &t2, &t1 );
    return Div64by32( &t1, 64 );
  }



  TT_F26Dot6 Project( TT_Vector *v, TT_UnitVector u )
  {
    Int64 t1, t2;

    MulTo64( v->x, u.x, &t1 );
    MulTo64( v->y, u.y, &t2 );
    Add64( &t1, &t2, &t1 );
    return Div64by32( &t1, 0x4000 );
  }



  Bool MoveVec1( TT_Vector* v, TT_F26Dot6 h )
  {
    Int64     t;
    TT_Vector r;

    MulTo64( h, 0x4000, &t );

    if ( MulVec( &t, Gs.freeVector, Gs.projVector, &r ) )
    {
      v->x += r.x;
      v->y += r.y;
      return SUCCESS;
    }

    Error = TT_Err_Divide_By_Zero;
    return FAILURE;
  }
