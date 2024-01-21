/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
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

#include <sys/syscall.h>

/* Not that using a `PASTE' macro loses.  */
#ifdef	__STDC__

#ifdef __ELF__
/* Use the regular ELF conventions about underscores, and provide the
   weak symbol, as required */
#define	SYSCALL__(name,args)	PSEUDO (__##name, name, args) \
.weak name;  \
  name = __##name; \
.type name,@function; \
.type __##name,@function; \
.L__##name##end: .size __##name,.L__##name##end - __##name

#define	SYSCALL(name,args)	PSEUDO (name, name, args) \
.type name,@function; \
.L_##name##end: .size name,.L_##name##end - name

#else /* __ELF__ */

/* Regular a.out definition */
#define	SYSCALL__(name,args)	PSEUDO (__##name, name, args)
#define	SYSCALL(name,args)	PSEUDO (name, name, args)

#endif /* __ELF__ */

#else  /* __STDC__ */

#define	SYSCALL__(name,args)	PSEUDO (__/**/name, name, args)
#define	SYSCALL(name,args)	PSEUDO (name, name, args)

#endif /* __STDC__ */

/* Machine-dependent sysdep.h files are expected to define the macro
   PSEUDO (function_name, syscall_name) to emit assembly code to define the
   C-callable function FUNCTION_NAME to do system call SYSCALL_NAME.
   r0 and r1 are the system call outputs.  movl should be defined as
   an instruction such that "movl r1, r0" works.  ret should be defined
   as the return instruction.  */


#if !defined(HAVE_GNU_LD) && !defined (__ELF__)
#define	 ___errno	_errno
#endif

#define	HAVE_SYSCALLS
