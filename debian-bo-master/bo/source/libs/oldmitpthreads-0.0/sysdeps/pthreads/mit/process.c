/* ==== process.c ============================================================
 * Copyright (c) 1994 by Chris Provenzano, proven@mit.edu
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *  This product includes software developed by Chris Provenzano.
 * 4. The name of Chris Provenzano may not be used to endorse or promote 
 *	  products derived from this software without specific prior written
 *	  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRIS PROVENZANO ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL CHRIS PROVENZANO BE LIABLE FOR ANY 
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
 * SUCH DAMAGE.
 *
 * Description : Process functions (fork, exec, ...).
 *
 *  1.23 94/04/18 proven
 *      -Started coding this file.
 */

#include <sys/types.h>
#include <pthread.h>
#include <unistd.h>

#ifndef lint
static const char rcsid[] = "$Id: process.c,v 1.3 1996/10/04 22:53:36 hjl Exp $";
#endif

extern void    fd_kern_fork         (void);
extern pid_t   machdep_sys_fork     (void);
extern void    pthread_sched_resume (void);
extern void    machdep_stop_timer   (struct timespec *);
extern int     fd_kern_exec         (const int);
extern int     machdep_sys_execve   (char *, char * const *, char * const *);


/* ==========================================================================
 * fork()
 *
 * This function requires a sig_prevent()/sig_check_and_resume() for the
 * parent. The child never unlocks.
 */
pid_t fork()
{
	pid_t ret;

	pthread_sched_prevent();

	fd_kern_fork();
	if ((ret = machdep_sys_fork())) { /* Parent or error */
		pthread_sched_resume();
	} else { /* Child */
		machdep_unset_thread_timer(NULL);
		machdep_stop_timer(NULL);
		fork_lock++;
	}
	return(ret);
}

/* ==========================================================================
 * execve()
 *
 * This function requires a sig_prevent()/sig_check_and_resume() if one
 * hasn't been done in the fork routine. Normally machdep_sys_execve()
 * should never return.
 */
int execve(const char *name, char * const *argv, char * const *envp) 
{
	int ret;

	if (!fork_lock) {
		pthread_sched_prevent();
		fd_kern_exec(0);
		ret = machdep_sys_execve(name, argv, envp);
		pthread_sched_resume();
	} else {
		fd_kern_exec(1);
		ret = machdep_sys_execve(name, argv, envp);
	}
	return(ret);
}

#if defined(__ELF__) || defined(__GNU_LIBRARY__)
#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (fork, __fork);
elf_alias (execve, __execve);
#endif
#endif
