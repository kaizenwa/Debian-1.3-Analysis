/* ########################################################################

			     main_buffer.h

   File: main_buffer.h
   Path: /home/fournigault/c/X11/xcoral-2.31/main_buffer.h
   Description: 
   Created: Fri Jan 27 11:16:57 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:16:58 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#ifndef _MAIN_BUFFER_H_
#define _MAIN_BUFFER_H_

#include <stdio.h>
#include "proto_decl.h"

#define SIZEOF_BUFFER	50000
#define PSIZE		0x2000
#define NEW		0
#define INSERT		1

typedef struct _Buf {
	char *top;	/* Debut du buffer */
	char *l_cur;	/* debut du trou (left cursor) */
	char *r_cur;	/* fin du trou (right cursor) */
	char *bottom;	/* fin du buffer */
} Buf;

FCT (void, ClearBuffer, (Buf *buf) );
FCT (void, DeleteBuffer, (Buf *buf) );
FCT (char *, DeleteLines, (Buf *buf, int n, int *len, int *dn) );
FCT (void, DeleteNchar, (Buf *buf, int n) );
FCT (char *, GetBackwardLine, (Buf *buf, int n, int *len) );
FCT (Buf *, GetBuffer, (unsigned int size) );
FCT (int, GetCurrentChar, (Buf *buf, char *c) );
FCT (char *, GetCurrentLine, (Buf *buf, int *len) );
FCT (char *, GetForwardLine, (Buf *buf, int n, int *len) );
FCT (int, GetNcFromLeft, (Buf *buf) );
FCT (int, GetNcFromRight, (Buf *buf) );
FCT (int, GetNewLine, (char *t, int len) );
FCT (int, GetNumberOfLineInBuf, (Buf *buf) );
FCT (void, GetPrevChar, (Buf *buf, char *c) );
FCT (void, HoleToLeft, (Buf *buf) );
FCT (void, HoleToRight, (Buf *buf) );
FCT (void, InsertNchar, (Buf *buf, char *s, int n) );
FCT (int, LoadFileInBuffer, (Buf *buf, FILE *fd, int len, int flag) );
FCT (int, MoveHole, (Buf *buf, int n) );
FCT (int, MoveToLine, (Buf *buf, int n) );
FCT (int, WriteCurrentFile, (Buf *buf, FILE *fd) );

#define TopBuf(buf)	(buf -> top)
#define BottomBuf(buf)	(buf -> bottom)
/*
#define RightBuf(buf)	((buf -> r_cur == buf -> bottom) ? buf -> bottom : buf -> r_cur + 1)
#define LeftBuf(buf)	((buf -> l_cur == buf -> top) ? buf -> top : buf -> l_cur - 1)
*/

#define RightBuf(buf)	(buf -> r_cur + 1)
#define LeftBuf(buf)	(buf -> l_cur - 1)

#endif /* _MAIN_BUFFER_H_ */
