/*
 *  linux/ibcs/bsdsignal.c
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: bsdsignal.c,v 1.8 1995/09/14 11:26:56 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/bsdsignal.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/stddef.h>
#include <linux/unistd.h>
#include <linux/ptrace.h>
#include <linux/config.h>
#include <linux/fcntl.h>
#include <linux/personality.h>

#include <asm/system.h>
#include <linux/fs.h>
#include <linux/sys.h>

#include <ibcs/signal.h>

#include <ibcs/map.h>
#include <ibcs/bsd.h>
#include <ibcs/ibcs.h>

#include <signal.h>


#define _S(nr) (1<<((nr)-1))

#define _BLOCKABLE (~(_S(BSD_SIGKILL) | _S(BSD_SIGSTOP)))


int
bsd_sigaction(int bsd_signum, const struct bsd_sigaction *action,
	struct bsd_sigaction *oldaction)
{
	struct bsd_sigaction new_sa, old_sa;
	int signum;
	struct sigaction *p;

	if (bsd_signum >= NSIGNALS)
		return -EINVAL;
#ifdef INIT_MM
	signum = current->exec_domain->signal_map[bsd_signum];
#else
	signum = signal_map_to_linux[current->personality & PER_MASK][bsd_signum];
#endif
	if (signum<1 || signum>32 || signum==SIGKILL || signum==SIGSTOP)
		return -EINVAL;

	p = signum - 1 + current->SIGACTION;
	if (action) {
		memcpy_fromfs(&new_sa, action, sizeof(struct bsd_sigaction));
		new_sa.sa_mask |= _S(bsd_signum);
		new_sa.sa_mask &= _BLOCKABLE;
		if (TASK_SIZE <= (unsigned long) new_sa.sa_handler)
			return -EFAULT;
	}
	if (oldaction) {
		if (!verify_area(VERIFY_WRITE, oldaction, sizeof(struct bsd_sigaction))) {
		  old_sa.sa_handler = p->sa_handler;
#ifdef INIT_MM
		  old_sa.sa_mask = map_bitvec(p->sa_mask,
			current->exec_domain->signal_invmap);
#else
		  old_sa.sa_mask = map_bitvec(p->sa_mask,
			signal_map_from_linux[current->personality & PER_MASK]);
#endif
		  old_sa.sa_flags = 0;
		  if(p->sa_flags & SA_NOCLDSTOP)
			old_sa.sa_flags |= BSD_SA_NOCLDSTOP;
		  if(p->sa_flags & SA_STACK)
			old_sa.sa_flags |= BSD_SA_ONSTACK;
		  if(p->sa_flags & SA_RESTART)
			old_sa.sa_flags |= BSD_SA_RESTART;
		  memcpy_tofs(oldaction, &old_sa, sizeof(struct bsd_sigaction));
		      };
	}
	if (action) {
		p->sa_restorer = NULL;
	  	p->sa_handler = new_sa.sa_handler;
#ifdef INIT_MM
		p->sa_mask = map_bitvec(new_sa.sa_mask,
			current->exec_domain->signal_map);
#else
		p->sa_mask = map_bitvec(new_sa.sa_mask,
			signal_map_to_linux[current->personality & PER_MASK]);
#endif
		p->sa_flags = 0;
		if (new_sa.sa_flags & BSD_SA_NOCLDSTOP)
			p->sa_flags |= SA_NOCLDSTOP;
		if (new_sa.sa_flags & BSD_SA_ONSTACK)
			p->sa_flags |= SA_STACK;
		if (new_sa.sa_flags & BSD_SA_RESTART)
			p->sa_flags |= SA_RESTART;

		/* Check for pending signals. */
		if (p->sa_handler == SIG_IGN) {
			if (signum == SIGCHLD)
				return 0;
			current->signal &= ~_S(signum);
			return 0;
		}
		if (p->sa_handler == SIG_DFL) {
			if (signum != SIGCONT
			&& signum != SIGCHLD
			&& signum != SIGWINCH)
				return 0;
			current->signal &= ~_S(signum);
			return 0;
		}	
	}
	return 0;
}


/* BSD passes the pointer to the new set to the library function but
 * replaces it with the actual signal set before handing off to the
 * syscall. Although the pointer to the old set is still in the stack
 * frame during the syscall the syscall returns the old set in eax
 * and the library code does the save if necessary.
 */
int
bsd_sigprocmask(int how, sigset_t bsdnset, sigset_t *bsdoset)
{
	sigset_t new_set, old_set;

#ifdef INIT_MM
	old_set = map_bitvec(current->blocked,
			current->exec_domain->signal_invmap),
#else
	old_set = map_bitvec(current->blocked,
			signal_map_from_linux[current->personality & PER_MASK]),
#endif

#ifdef INIT_MM
	new_set = map_bitvec(bsdnset,
		current->exec_domain->signal_map);
#else
	new_set = map_bitvec(bsdnset,
		signal_map_to_linux[current->personality & PER_MASK]);
#endif

	switch (how) {
		case 1: /* SIGBLOCK */
			current->blocked |= new_set;
			break;
		case 2: /* SIGUNBLOCK */
			current->blocked &= ~new_set;
			break;
		case 3: /* SIGSETMASK */
			current->blocked = new_set;
			break;
		default:
			return -EINVAL;
	}

	return old_set;
}


/* Although the stack frame contains the pointer to where the set should
 * be stored BSD returns the set in eax and the library code does the
 * store.
 */
int
bsd_sigpending(sigset_t *set)
{
#ifdef INIT_MM
	return map_bitvec(current->blocked & current->signal,
			current->exec_domain->signal_invmap);
#else
	return map_bitvec(current->blocked & current->signal,
			signal_map_from_linux[current->personality & PER_MASK]);
#endif
}
