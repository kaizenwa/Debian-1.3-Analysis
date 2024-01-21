/*
    XBlockOut a 3D Tetris

    Copyright (C) 1992,1993,1994  Thierry EXCOFFIER

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Contact: Thierry.EXCOFFIER@ligia.univ-lyon1.fr
*/
#ifndef R0

#define R0

#ifdef HAVE_PROTOTYPES

#define R1(A1) A1
#define R2(A1,A2) A1,A2
#define R3(A1,A2,A3) A1,A2,A3
#define R4(A1,A2,A3,A4) A1,A2,A3,A4
#define R5(A1,A2,A3,A4,A5) A1,A2,A3,A4,A5
#define R6(A1,A2,A3,A4,A5,A6) A1,A2,A3,A4,A5,A6
#define R7(A1,A2,A3,A4,A5,A6,A7) A1,A2,A3,A4,A5,A6,A7
#define R8(A1,A2,A3,A4,A5,A6,A7,A8) A1,A2,A3,A4,A5,A6,A7,A8
#define R9(A1,A2,A3,A4,A5,A6,A7,A8,A9) A1,A2,A3,A4,A5,A6,A7,A8,A9
#define R10(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) A1,A2,A3,A4,A5,A6,A7,A8,A9,A10
#define R11(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11

#else

#define R1(A1)
#define R2(A1,A2)
#define R3(A1,A2,A3)
#define R4(A1,A2,A3,A4)
#define R5(A1,A2,A3,A4,A5)
#define R6(A1,A2,A3,A4,A5,A6)
#define R7(A1,A2,A3,A4,A5,A6,A7)
#define R8(A1,A2,A3,A4,A5,A6,A7,A8)
#define R9(A1,A2,A3,A4,A5,A6,A7,A8,A9)
#define R10(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)
#define R11(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)

#endif

#endif
