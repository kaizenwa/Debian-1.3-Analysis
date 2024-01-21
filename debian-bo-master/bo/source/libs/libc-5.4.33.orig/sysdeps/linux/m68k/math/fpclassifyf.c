/* Copyright (C) 1993  Olaf Flebbe
   Changed by Andreas Schwab for m68k
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

#define FP_NAN       1
#define FP_INFINITE  2
#define FP_NORMAL    3
#define FP_SUBNORMAL 4
#define FP_ZERO      5

int fpclassifyf( unsigned int x) {
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

int fpclassifyd( unsigned int x1, unsigned int x2) {
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

int fpclassifyl( unsigned int x1, unsigned int x2,
		  unsigned int x3) {
  unsigned int y;

  y  = x1 & 0x7fff0000;
  x1 = (x2 & 0x7fffffff) | x3;

  if (y == 0x7fff0000) {
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

