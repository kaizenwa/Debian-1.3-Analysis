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
 * 2. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Eric Youngdale,
 *      Peter MacDonald, David Engel and Hongjiu Lu.
 * 3. The name of the above contributors may not be
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

	.file "nullpagecrt0.S"

	.text
__entry:
	jmp __entry1
	.align 12, 0x0
__entry1:
/* Modifications by Tristan Gingold 1994.
 * Unmap the first page.  All memory access to the first page will be
 *  trapped. (SIGSEGV)
 * The next 3 instructions could be done just before 'jmp __entry1'.
 * Not tested: the behavior with the function unexec of emacs.
 * 
 * This crt0 uses more than one page of memory.  However, since 3 kb
 *  contains only null bytes, they are not written on the disk.
 */
	movl $91,%eax
	xorl %ebx,%ebx
	movl $4096,%ecx
	int $0x80

/*
 * The first thing we do is try to load the shared library. If that
 * fails, it won't return.
 */
	call ___load_shared_libraries
/*
 *	This is important, and was missing from the new version...
 */
	movl $45,%eax
	movl $0,%ebx
	int  $0x80
	movl %eax,____brk_addr
/*
 *	ok, set up the  ___environ and call _main
 */	
	movl 8(%esp),%eax
	movl %eax,___environ
	movzwl ___fpu_control,%eax
	pushl %eax
	call ___setfpucw
	addl $4,%esp
/* Some functions may be needed. */
	call ___libc_init
	call _main
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

#ifdef __i486__
	.align  4,0x90
#else
	.align  2,0x90
#endif
___shared_dummy__:
	.asciz ""
	.stabs "___SHARED_LIBRARIES__",25,0,0,___shared_dummy__

	.data
#ifdef __i486__
	.align  4
#else
	.align  2
#endif
___shared_dummy1__:
	.long 0xfeeb1ed3  /* Magic number used by DLL code to make sure this
			     is a real list */

	.stabs "__SHARABLE_CONFLICTS__",25,0,0,___shared_dummy1__
