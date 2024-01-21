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
/* Changes by Tristan Gingold 
   * does not handle the shared libraries
   * call __chkr_init_chkr before main
 */
/* Avoid bad compilation... */
#ifdef __CHECKER__
#error "Do NOT compile chkrcrt0.S with checkergcc"
#endif

	.file "chkrcrt0.S"
	.stabs "/libc-linux/sysdeps/linux/i386/crt/",100,0,0,__entry
	.stabs "gccchkrcrt0.S",100,0,0,__entry
	.stabd 68,0,1
	.text
	.globl	___crt_dummy__
___crt_dummy__:
__entry:
/*
 *	This is important, and was missing from the new version...
 */
	movl $45,%eax
	movl $0,%ebx
	int  $0x80
	movl %eax,____brk_addr

/*
 *	ok, set up the 387 flags and ___environ.
 */	
	fldcw init_cw
	movl 8(%esp),%eax
	movl %eax,___environ
/*
 * The next thing we do is initialize Checker.
 * Because 387 flags are set, Checker can use the FPU.
 */
	pushl $0	/* libs */
	pushl $0	/* nlibs */
	pushl $1	/* linked */
	call ____chkr_init_chkr
	addl $12, %esp
/*
 * Now we can call __setfpucw and _main
 */
	movzwl ___fpu_control,%eax
/*	call chkr_0_5_2_4_chkr		*//* for the pushl */
	pushl %eax
	call ___setfpucw
	addl $4,%esp
/* Some functions may be needed. */
	call ___libc_init
	call _chkr$main
/*	call chkr_0_5_2_4_chkr		*//* for pushl */
	pushl %eax
	call _exit
/*
 *	Just in case _exit fails... We use int $0x80 for __exit().
 */
	popl %ebx
done:
	movl $1,%eax
	int $0x80
	jmp done

	.stabd 68,0,2
	.stabs "__entry:F39",36,0,1,__entry

#ifdef __i486__
	.align  4,0x90
#else
	.align  2,0x90
#endif
init_cw:
	.word 0x1372

	.text
	.align 4, 0x90
___shared_dummy__:
	.asciz ""
	.stabs "___SHARED_LIBRARIES__",25,0,0,___shared_dummy__

	.data
	.align 2
___shared_dummy1__:
	.long 0xfeeb1ed3  /* Magic number used by DLL code to make sure this
			     is a real list */

	.stabs "__SHARABLE_CONFLICTS__",25,0,0,___shared_dummy1__
