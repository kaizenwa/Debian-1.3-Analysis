/* Copyright (C) 1992 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <sysdeps/linux/sysdep.h>

#define ENTRY(name)                                                           \
  .globl _##name##;							      \
  .align 4;								      \
  _##name##:

#define _HASH  #
#define PSEUDO(name, syscall_name, args)                                      \
  .text;								      \
  ENTRY (name)                                                                \
    PUSH_##args 							      \
    moveq _HASH SYS_##syscall_name, d0;					      \
    MOVE_##args 							      \
    trap  _HASH 0;							      \
    tstl  d0;  								      \
    bpl   Lexit;							      \
    negl  d0;								      \
    movel d0,_errno;							      \
    moveq _HASH -1,d0;							      \
   Lexit:								      \
    POP_##args

/* Linux takes system call arguments in registers:
	1: d1
	2: d2
	3: d3
	4: d4
	5: d5
 */

#define PUSH_0	/* No arguments to push.  */
#define PUSH_1	/* no need to restore d1  */
#define PUSH_2	movel d2,sp@-;
#define PUSH_3	movml d2-d3,sp@-;
#define PUSH_4	movml d2-d4,sp@-;
#define PUSH_5	movml d2-d5,sp@-;

#define MOVE_0	/* No arguments to move.  */

#define MOVE_1	movl sp@(4),d1;
#define MOVE_2	movml sp@(8),d1-d2;
#define MOVE_3	movml sp@(12),d1-d3;
#define MOVE_4	movml sp@(16),d1-d4;
#define MOVE_5	movml sp@(20),d1-d5;

#define POP_0	/* No arguments to pop.  */
#define POP_1	/* didn't save d1        */
#define POP_2	movel sp@+,d2;
#define POP_3	movml sp@+,d2-d3;
#define POP_4	movml sp@+,d2-d4;
#define POP_5	movml sp@+,d2-d5;

#define ret rts
/* Linux doesn't use it. */
#if 0
#define r0	d0
#define r1	d1
#define MOVE(x,y)       movel x , y
#endif
