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

  @(#)get_offsets.c	2.5%4/12/95
*/

#if defined(LOCORE) || defined(_ASM)
#define sp_offset 0
#define pc_offset 4
#define thread_errno 80
#define stack_base 88
#define state 92
#define nscp 532
#define mask 556
#define pending 560
#define pthread_self 0
#define is_in_kernel 4
#define is_updating_timer 8
#define state_change 12
#define new_signals 16
#define pending_signals 20
#define all_signals 24
#define no_signals 28
#define cantmask 32
#define process_stack_base 36
#define ready 40
#define ready_head 40
#define sched 636
#define TV_SEC 80
#define TV_NSEC 84
#define sc_mask 4
#define sc_sp 8
#define sc_pc 12
#define jmp_svmask 12
#define jmp_pc 4
#define jmp_sp 0
#define jmp_mask 16
#define mutex_queue 0
#define mutex_lock  8
#define mutex_owner 12
#define mutex_protocol 24
#define cleanup_size 12
#define sigset_t_size 4
#endif defined(LOCORE) || defined(_ASM)
