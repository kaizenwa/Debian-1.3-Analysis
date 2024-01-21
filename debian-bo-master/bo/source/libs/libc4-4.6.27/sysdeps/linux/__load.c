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

#include <unistd.h>
#include <syscall.h>
#include <stdarg.h>
#include <sharedlib.h>
#include <errno.h>
#include <sys/mman.h>
#include "config.h"
#include "fixups.h"

static int errno = 0;

static
_syscall1(int,exit,int,code)

static
_syscall1(int,uselib,const char *,filename)

static
_syscall3(ssize_t,write,int,fd,const void *,buf,size_t,count)

static
_syscall2(int,munmap,caddr_t,addr,size_t,len)

extern struct libentry * __SHARED_LIBRARIES__[];
extern struct fixuplist  _SHARABLE_CONFLICTS__;

#if 0
/* This change seems not correct. H.J. */

#undef data_set_element
#include <gnu-stabs.h>

static const char const __shared_dummy__ = 0;

/* Magic number used by DLL code to make sure this is a real list. */
static unsigned int __shared_dummy1__ = 0xfeeb1ed3;

data_set_element (__SHARED_LIBRARIES__, __shared_dummy__);
data_set_element (_SHARABLE_CONFLICTS__, __shared_dummy1__);
#endif

void
__load_shared_libraries (int argc, char **argv, char **envp)
{
    if (__SHARED_LIBRARIES__[2])
    {
	unsigned mapinfo[2];
	/* this must be volatile to work with -O, GCC bug? */
	volatile loadptr loader = (loadptr)LDSO_ADDR;

#ifdef LDSO_IMAGE1
	if (uselib(LDSO_IMAGE) && uselib(LDSO_IMAGE1))
#else
	if (uselib(LDSO_IMAGE))
#endif
	{
	    char *p = argv[0];
#ifdef LDSO_IMAGE1
	    char errmsg[] = ": can't load dynamic linker '"
		LDSO_IMAGE
		" nor "
		LDSO_IMAGE1
		"'\n";
#else
	    char errmsg[] = ": can't load dynamic linker '" LDSO_IMAGE "'\n";
#endif

	    if (p)
	    {
		while (*p) p++;
		write(2, argv[0], p-argv[0]);
	    }

	    write(2, errmsg, sizeof errmsg - 1);
	    while (1) exit(128);
	}

	loader((argc <= 0) ? FUNC_LDD : FUNC_LINK, mapinfo, argv[0],
	       envp, __SHARED_LIBRARIES__, &_SHARABLE_CONFLICTS__);

	munmap((caddr_t)mapinfo[0], mapinfo[1]);
    }
    else 
    {
	char msg[] = "\tstatically linked\n";

	if (argc<=0)
		write (2, msg, sizeof msg - 1);
    }
    if (argc <= 0)
while (1) exit(0);
}
