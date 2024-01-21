/*
 * Copyright (c) 1993 Eric Youngdale, Peter MacDonald, David Engel
 * and Hongjiu Lu.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. The name of the above contributors may not be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/* Notice of general intent:
 *
 * The linux operating system generally contains large amounts of code
 * that fall under the GNU General Public License, or GPL for short.
 * This file contains source code that by it's very nature would always
 * be linked with an application program, and because of this a GPL type
 * of copyright on this file would place restrictions upon the
 * distribution of binary-only commercial software.  Since the goal of the
 * Linux project as a whole is not to discourage the development and
 * distribution of commercial software for Linux, this file has been placed
 * under a more relaxed BSD-style of copyright.
 *
 * It is the general understanding of the above contributors that a
 * program executable linked to a library containing code that falls
 * under the GPL or GLPL style of license is not subject to the terms of
 * the GPL or GLPL license if the program executable(s) that are supplied
 * are linked to a shared library form of the GPL or GLPL library, and as long
 * as the form of the shared library is such that it is possible for
 * the end user to modify and rebuild the library and use it in
 * conjunction with the program executable.
 */

#ifdef __ELF__
#define MAIN main
#define EXIT exit
#define LIBC_INIT __libc_init
#define SETLOCALE setlocale
#define SETFPUCW __setfpucw
#define FPU_CONTROL __fpu_control
#define ENVIRON __environ
#define BRK_ADDR ___brk_addr
#ifdef __i486__
#define ALIGN_TEXT	.align	16,0x90
#else
#define ALIGN_TEXT	.align	4,0x90
#endif
#define ALIGN_DATA	.align	4
#else
#define MAIN _main
#define EXIT _exit
#define LIBC_INIT ___libc_init
#define SETLOCALE _setlocale
#define SETFPUCW ___setfpucw
#define FPU_CONTROL ___fpu_control
#define ENVIRON ___environ
#define BRK_ADDR ____brk_addr
#ifdef __i486__
#define ALIGN_TEXT	.align	4,0x90
#else
#define ALIGN_TEXT	.align	2,0x90
#endif
#define ALIGN_DATA	.align	2
#endif

	.file "crt0.S"

#if defined(__ELF__) && defined(CALL_DEFAULT_LOCALE)
	.section .rodata
	ALIGN_DATA
	.type ___null_string,@object
	.size ___null_string,4
___null_string:
	.long .L_shared_dummy__
.L_shared_dummy__:
	.asciz ""
#endif

	.text
	ALIGN_TEXT
	.globl	___crt_dummy__
___crt_dummy__:
#ifndef __ELF__
__entry:
/*
 * The first thing we do is try to load the shared library. If that
 * fails, it won't return.
 */
	call ___load_shared_libraries
#else
/* ELF stuff here */
.globl _start
.type _start,@function
_start:
	/* First locate the start of the environment variables */
	popl %ecx
	movl %esp,%ebx  /* Points to the arguments */
	movl %esp,%eax /* Same here */
	movl %ecx,%edx
	addl %edx,%edx
	addl %edx,%edx
	addl %edx,%eax
	addl $4,%eax	

	/* Set up an invalid (NULL return address, NULL frame pointer)
	   callers stack frame so anybody unrolling the stack knows where
	   to stop */
	xorl %ebp,%ebp /* NULL */
	pushl %ebp /* callers %cs */
	pushl %ebp /* callers %eip (return address) */
	pushl %ebp /* callers %ebp (frame pointer) */
	movl %esp,%ebp /* mark callers stack frame as invalid */

	pushl %eax  /* Environment pointer */
	push %ebx   /* Argument pointer */
	pushl %ecx /* And the argument count */
#ifndef __SVR4_I386_ABI_L1__
/*
 * Make sure we are not using iBCS2 personality. (i.e. force linux).
 */
	movl $136,%eax
	movl $0,%ebx
	int  $0x80
#endif
#endif

#if 0
/*
 *	This is important, and was missing from the new version...
 */
	movl $45,%eax
	movl $0,%ebx
	int  $0x80
	movl %eax,BRK_ADDR
#endif
/*
 *	ok, set up the  ___environ and call _main
 */	
	movl 8(%esp),%eax
	movl %eax,ENVIRON
	movzwl FPU_CONTROL,%eax
	pushl %eax
	call SETFPUCW
	addl $4,%esp
/* Some functions may be needed. */
	call LIBC_INIT
#ifdef CALL_DEFAULT_LOCALE
/* Set up the default locale */
	movl ___null_string,%eax
	pushl %eax
	pushl $DEFAULT_LOCALE
	call SETLOCALE
	addl $8,%esp
#endif
#ifdef ELF_INIT_FINI
	pushl $_fini
	call atexit
	addl $4,%esp
	call _init
#endif
	call MAIN
	pushl %eax
	call EXIT
/*
 *	Just in case _exit fails... We use int $0x80 for __exit().
 */
	popl %ebx
	ALIGN_TEXT
done:
	movl $1,%eax
#ifdef	__SVR4_I386_ABI_L1__
	lcall,$0x7,$0
#else
	int $0x80
#endif
	jmp done
	ALIGN_TEXT

#ifdef __ELF__
	.size	_start,.-_start
#if 1
	.globl __environ
	.data
	.type __environ,@object
	ALIGN_DATA
__environ:
	.long 0
	.size __environ,4
	.weak environ
	environ = __environ
#endif
#else
	.text
	ALIGN_TEXT
#ifdef CALL_DEFAULT_LOCALE
___null_string:
	.long ___shared_dummy__
#endif
___shared_dummy__:
	.asciz ""
	.stabs "___SHARED_LIBRARIES__",25,0,0,___shared_dummy__

	.data
	ALIGN_DATA
___shared_dummy1__:
	.long 0xfeeb1ed3  /* Magic number used by DLL code to make sure this
			     is a real list */

	.stabs "__SHARABLE_CONFLICTS__",25,0,0,___shared_dummy1__
#endif
