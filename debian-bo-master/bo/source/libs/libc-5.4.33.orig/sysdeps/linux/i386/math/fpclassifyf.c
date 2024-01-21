/* Copyright (C) 1993  Olaf Flebbe
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */


/* YES: DO NOT INCLUDE ANY HEADER FILES.
   TYPE CONVERTING, YUCK! */

/* 
   THIS FUNCTION IS STILL EXPERIMENTAL AND/OR OBSOLETE
   Olaf Flebbe 1/96
*/

#define FP_NAN       1
#define FP_INFINITE  2
#define FP_NORMAL    3
#define FP_SUBNORMAL 4
#define FP_ZERO      5

#ifdef __STDC__
int fpclassifyf  ( unsigned int );
int fpclassifyd  ( unsigned int, unsigned int );
int fpclassifyl  ( unsigned int, unsigned int, unsigned int );
#else
int fpclassifyf  ();
int fpclassifyd  ();
int fpclassifyl  ();
#endif

int fpclassifyf( unsigned int x)
{
  unsigned int y;

  x <<= 1;
  y = x & 0xff000000;
  x = x & 0x00ffffff;

  if (y == 0xff000000) {
    /* i.e. NaN or Inf */
    if (x) 
      return FP_NAN;
    else
      return FP_INFINITE;
  } else if (y) 
    return FP_NORMAL;
  else if (x)
    return FP_SUBNORMAL;
  else
    return FP_ZERO;
}

int fpclassifyd( unsigned int x2, unsigned int x1)
{
  unsigned int y;

  y  = x1 & 0x7ff00000;
  x1 = (x1 & 0x000fffff)| x2;

  if (y == 0x7ff00000) {
    /* i.e. NaN or Inf */
    if (x1) 
      return FP_NAN;
    else
      return FP_INFINITE;
  } else if (y) 
    return FP_NORMAL;
  else if (x1)
    return FP_SUBNORMAL;
  else
    return FP_ZERO;
}

int fpclassifyl( unsigned int x3, unsigned int x2,
		  unsigned int x1)
{
  unsigned int y;

  y  = x1 & 0x00007fff;
  x1 = (x2 & 0x7fffffff) | x3;

  if (y == 0x00007fff) {
    /* i.e. NaN or Inf */
    if (x1) 
      return FP_NAN;
    else
      return FP_INFINITE;
  } else if (y) 
    return FP_NORMAL;
  else if (x1)
    return FP_SUBNORMAL; /* WHAT ABOUT PSEUDODENORMALS ???????? */
  else
    return FP_ZERO;
}

