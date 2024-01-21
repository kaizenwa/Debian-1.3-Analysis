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

  @(#)config_header.c	2.5%4/12/95
*/

/*
 * configuration header file to identify compile options
 */

#ifndef C_INTERFACE_NP
#define C_INTERFACE_NP
#endif

#ifndef SRP_NP
#define SRP_NP
#endif

#ifndef DRAFT5_NP
#define DRAFT5_NP
#endif

#ifndef SIGNAL_STACK_NP
#define SIGNAL_STACK_NP
#endif

#ifndef STACK_CHECK_NP
#define STACK_CHECK_NP
#endif

#ifndef ASM_SETJMP_NP
#define ASM_SETJMP_NP
#endif

