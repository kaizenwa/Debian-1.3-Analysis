/* Copyright (C) 1992, the Florida State University
   Distributed by the Florida State University under the terms of the
   GNU Library General Public License.

This file is part of Pthreads.

Pthreads is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation (version 2).

Pthreads is distributed "AS IS" in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with Pthreads; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge,
MA 02139, USA.

Report problems and direct all questions to:

  pthreads-bugs@ada.cs.fsu.edu

  @(#)pthread_setjmp.h	2.5 4/12/95

*/

#ifndef _pthread_pthread_setjmp_h
#define _pthread_pthread_setmp_h

#include <setjmp.h>

#if defined(ASM_SETJMP) || !defined(C_CONTEXT_SWITCH)

/*
 * for this version, the index of JB_SP must be even !!!
 * This way, we can speed up the context switch (using std).
 */
#define JB_SVMASK 3
#define JB_SP     0
#define JB_PC     1
#define JB_MASK   4

#else !defined(ASM_SETJMP) && defined(C_CONTEXT_SWITCH)
#ifdef SOLARIS

#define JB_SVMASK 0
#define JB_SP     1
#define JB_PC     2
#define JB_MASK   12

#else !SOLARIS
#ifdef SVR4

#include <setjmp.h> /* We hope that this defines JB_xxx */

#else !SVR4

#define JB_SVMASK 0
#define JB_SP     3
#define JB_PC     4
#define JB_MASK   2

#endif !SVR4
#endif !SOLARIS
#endif  !defined(ASM_SETJMP) && defined(C_CONTEXT_SWITCH)

#endif /*!_pthread_setjmp_internals_h*/
