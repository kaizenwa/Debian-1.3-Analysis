/*
 * Copyright (c) 1994 Eric Youngdale, Peter MacDonald, David Engel,
 * Hongjiu Lu and Mitch D'Souza.
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
void ___chkr_init_chkr(int linked, int nlibs, char **libs,
		  int argc, char *argv[], char *argp[]);
		  
#if defined(MDCHECKER) && !defined(PLCHECKER)
#include "checker.h"
#include <syscall.h>
#include <stdarg.h>
#include <sharedlib.h>
#include <errno.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <a.out.h>

#define LDSO_ENTRY (0x62f00000 + 32)
#define FUNC_LINK_AND_CALLBACK 3
#define LDSO_IMAGE "/lib/ld.so"

static int errno = 0;
static char *argv0;
static int callback_done = 0;
typedef void (*loadptr)(int func, ...);
static char **my_environ;
extern char **__environ;

static
_syscall1(int,uselib,const char *,filename)

extern struct libentry * __SHARED_LIBRARIES__[];
extern struct fixuplist  _SHARABLE_CONFLICTS__;

/* callback is called by ld.so and get infos about the shared libraries. */
/* TODO: check there are no conflicts with reserved memory */
void
callback (int ver, int nlibs, char **libs, int nmods, char **mods)
{
 if (ver != 1)
   {
     chkr_printf("Checker: Bad version of ld.so\n");
     chkr_abort();
   }
 
 /* This is needed because __chkr_init_chkr use getenv */
 __environ = my_environ;
 
 /* Initialize Checker */
 ___chkr_init_chkr(1, nlibs, libs, 1, &argv0, __environ);
 callback_done = 1;	/* Has been called */
}

/* This is the first function called by crt0.S, to load the shared libs */
void
__load_shared_libraries (int argc, char **argv, char **envp)
{
  my_environ = envp;
  argv0 = argv[0];
  
  if (__SHARED_LIBRARIES__[2])
    {
      unsigned mapinfo[2];
      /* this must be volatile to work with -O, GCC bug? */
      volatile loadptr loader = (loadptr)LDSO_ENTRY;

      if (uselib(LDSO_IMAGE))
        {
	  chkr_printf("%s: can't load dynamic linker'" LDSO_IMAGE "'\n", argv[0]);
	    
	  while (1) 
	    chkr_abort();
	}
      loader(FUNC_LINK_AND_CALLBACK, mapinfo, argv[0], envp,
	     __SHARED_LIBRARIES__, &_SHARABLE_CONFLICTS__, &callback);
      munmap((caddr_t)mapinfo[0], mapinfo[1]);
      if (!callback_done)
        {
          chkr_printf("Checker fails to get info about the shared libraries.\n");
          chkr_abort();
        }
    }
    
  /* This must be done at the end */
  __environ = my_environ;
}
#endif

#ifdef PLCHECKER
#include "checker.h"
#include <fcntl.h>
#include <a.out.h>

extern char **__environ;

/* This is automatically called by preload */
void
_preload_main_(int ver, int nlibs, char **libs,
                 int nmod, char **mods, char *argv0, char **env)
{
  if (ver != 1)
    {
      chkr_printf("Bad version of ld.so\n");
      chkr_abort();
    }
    
  /* Set environ: */
  __environ = env;
  
  /* Initialize Checker */
  ___chkr_init_chkr(0, nlibs, libs, 1, &argv0, env);
}  
#endif
