/*
 *  linux/ibcs/signal.c
 *
 *  This module does not go through the normal processing routines for
 *  ibcs. The reason for this is that for most events, the return is a
 *  procedure address for the previous setting. This procedure address
 *  may be negative which is not an error. Therefore, the return processing
 *  for standard functions is skipped by declaring this routine as a "special"
 *  module for the decoder and dealing with the register settings directly.
 *
 *  Please consider this closely if you plan on changing this mode.
 *  -- Al Longyear
 *
 * $Id: signal.c,v 1.24 1996/07/26 12:03:36 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/signal.c,v $
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
#include <linux/fcntl.h>
#include <linux/personality.h>

#include <asm/system.h>
#include <linux/fs.h>
#include <linux/sys.h>

#include <ibcs/ibcs.h>
#include <ibcs/xnx.h>
#include <ibcs/abi4.h>
#include <ibcs/map.h>

#include <signal.h>

#define SIG_HOLD	((__sighandler_t)2)	/* hold signal */

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif

#ifdef __cplusplus
extern "C"
#endif

#include <ibcs/signal.h>


typedef void (*pfn) (void);     /* Completion function */

/*
 *  Parameters to the signal functions have a common stack frame. This
 *  defines the stack frame.
 */

#define SIGNAL_NUMBER    get_syscall_parameter (regs, 0)
#define HIDDEN_PARAM     (SIGNAL_NUMBER & ~0xFF)
#define SECOND_PARAM     get_syscall_parameter (regs, 1)
#ifdef __sparc__
#define THIRD_PARAM      get_syscall_parameter (regs, 2)
#else /* __sparc__ */
#define THIRD_PARAM      ((unsigned long) regs->edx)
#endif /* __sparc__ */

/* Return a mask that includes SIG only.  */
#define __sigmask(sig)	(1 << ((sig) - 1))

/*
 *  Defines for the other signal processing routines
 */

#define __sigemptyset(set)	((*(set) = 0L), 0)
#define __sigfillset(set)       ((*(set) = -1L), 0)
#define __sigaddset(set, sig)   ((*(set) |= __sigmask (sig)), 0)
#define __sigdelset(set, sig)   ((*(set) &= ~__sigmask (sig)), 0)
#define __sigismember(set, sig) ((*(set) & __sigmask (sig)) ? 1 : 0)

#define sigaddset	__sigaddset
#define sigdelset	__sigdelset
#define sigismember	__sigismember
#define sigemptyset	__sigemptyset
#define sigfillset	__sigfillset

#define TO_KERNEL(save)      \
	save = get_fs ();    \
	set_fs (get_ds ())

#define FROM_KERNEL(save)    \
	set_fs (save)

#define _S(nr) (1<<((nr)-1))

#define _BLOCKABLE (~(_S(IBCS_SIGKILL) | _S(IBCS_SIGSTOP)))


#ifndef INIT_MM
/*
 * Translate the signal number from Linux to iBCS. (Only used in the
 * 1.0 kernel patches to build the iBCS signal stack frame).
 */
int ibcs_lmapsig(int sig)
{
	if ((unsigned int)sig >= NSIGNALS)
		return -1;
	return signal_map_from_linux[current->personality & PER_MASK][sig];
}
#endif /* INIT_MM */

/*
 *  Translate the signal number to the corresponding item for Linux.
 */
static int ibcs_mapsig(int sig)
{

	if ((unsigned int) sig >= NSIGNALS)
		return -1;
#ifdef INIT_MM
	return current->exec_domain->signal_map[sig];
#else
	return signal_map_to_linux[current->personality & PER_MASK][sig];
#endif
}

inline int ibcs_signo (struct pt_regs *regs, int *sig)
{
	int    value = ibcs_mapsig(SIGNAL_NUMBER & 0xFF);

	if (value == -1) {
		set_error (regs, iABI_errors (EINVAL));
		return 0;
	}

	*sig = value;
	return 1;
}


/*
 *  Process the signal() function from iBCS
 *
 *  This version appeared in "Advanced Programming in the Unix Environment"
 *  by W. Richard Stevens, page 298.
 */

void ibcs_sig_handler (struct pt_regs * regs, int sig,
			__sighandler_t handler, int oneshot)
{
	struct sigaction act, oact;
	int	      answer;
	int	      old_fs;

	sigemptyset (&act.sa_mask);
	act.sa_restorer = NULL;
	act.sa_handler = handler;
	act.sa_flags   = 0;

	if (oneshot)
		act.sa_flags = SA_ONESHOT | SA_NOMASK;
	else
		act.sa_flags = 0;

	TO_KERNEL (old_fs);
	answer = SYS(sigaction) (sig, &act, &oact);
	FROM_KERNEL (old_fs);

	if (answer < 0) {
		set_error (regs, iABI_errors (-answer));
	} else
		set_result (regs, (int) oact.sa_handler);
}

/*
 *  Process the signal() function from iBCS
 */
int ibcs_signal (struct pt_regs * regs)
{
	__sighandler_t   vec;
	int	      sig;

	if (ibcs_signo (regs, &sig)) {
		vec = (__sighandler_t) SECOND_PARAM;
		ibcs_sig_handler (regs, sig, vec, 1);
	}
	return 0;
}

/*
 *      Process the iBCS sigset function.
 *
 *      This is basically the same as the signal() routine with the exception
 *      that it will accept a SIG_HOLD parameter.
 *
 *      A SIG_HOLD will defer the processing of the signal until a sigrelse()
 *      function is called.
 */
int ibcs_sigset (struct pt_regs * regs)
{
	sigset_t	 newmask, oldmask;
	__sighandler_t   vec;
	int	      sig, answer;
	int	      old_fs;

	if (ibcs_signo (regs, &sig)) {
		vec = (__sighandler_t) SECOND_PARAM;
		if (vec != SIG_HOLD) {
			sigemptyset (&newmask);
			sigaddset  (&newmask, sig);
			current->blocked &= ~newmask;
			ibcs_sig_handler (regs, sig, vec, 0);
		} else {
/*
 *      Process the hold function
 */
			sigemptyset (&newmask);
			sigaddset  (&newmask, sig);

			TO_KERNEL (old_fs);
			answer = SYS(sigprocmask) (SIG_BLOCK,
						  &newmask,
						  &oldmask);
			FROM_KERNEL (old_fs);

			if (answer < 0) {
				set_error (regs, iABI_errors (-answer));
			}
		}
	}
	return 0;
}

/*
 *      Process the iBCS sighold function.
 *
 *      Suspend the signal from future recognition.
 */
void ibcs_sighold (struct pt_regs * regs)
{
	sigset_t   newmask, oldmask;
	int	sig, answer;
	int	old_fs;

	if (!ibcs_signo (regs, &sig))
		return;

	sigemptyset (&newmask);
	sigaddset  (&newmask, sig);

	TO_KERNEL (old_fs);
	answer = SYS(sigprocmask) (SIG_BLOCK, &newmask, &oldmask);
	FROM_KERNEL (old_fs);

	if (answer < 0) {
		set_error (regs, iABI_errors (-answer));
	}
}

/*
 *      Process the iBCS sigrelse.
 *
 *      Re-enable the signal processing from a previously suspended
 *      signal. This may have been done by calling the sighold() function
 *      or a longjmp() during the signal processing routine. If you do a
 *      longjmp() function then it is expected that you will call sigrelse
 *      before going on with the program.
 */
void ibcs_sigrelse (struct pt_regs * regs)
{
	sigset_t   newmask, oldmask;
	int	sig, answer;
	int	old_fs;

	if (!ibcs_signo (regs, &sig))
		return;

	sigemptyset (&newmask);
	sigaddset   (&newmask, sig);

	TO_KERNEL (old_fs);
	answer = SYS(sigprocmask) (SIG_UNBLOCK, &newmask, &oldmask);
	FROM_KERNEL (old_fs);

	if (answer < 0) {
		set_error (regs, iABI_errors (-answer));
	}
}

/*
 *      Process the iBCS sigignore
 *
 *      This is basically a signal (...,SIG_IGN) call.
 */

void ibcs_sigignore (struct pt_regs * regs)
{
	struct sigaction act, oact;
	int	      sig, answer;
	int	      old_fs;

	if (!ibcs_signo (regs, &sig))
		return;

	sigemptyset (&act.sa_mask);

	act.sa_restorer = NULL;
	act.sa_handler = SIG_IGN;
	act.sa_flags   = 0;

	TO_KERNEL (old_fs);
	answer = SYS(sigaction) (sig, &act, &oact);
	FROM_KERNEL (old_fs);

	if (answer < 0) {
		set_error (regs, iABI_errors (-answer));
	}
}

/*
 *      Process the iBCS sigpause
 *
 *      Wait for the signal indicated to arrive before resuming the
 *      processing. I do not know if the signal is processed first using
 *      the normal event processing before the return. If someone can
 *      shed some light on this then please correct this code. I block
 *      the signal and look for it to show up in the pending list.
 */

void ibcs_sigpause (struct pt_regs * regs)
{
	sigset_t   newset;
	int	sig, answer;

#ifdef __sparc__
	printk(KERN_ERR "Sparc/iBCS: sigpause not yet implemented\n");
#else
	if (!ibcs_signo(regs, &sig))
		return;

	sigfillset(&newset);
	sigdelset(&newset, sig);
	answer = SYS(sigsuspend)(0, current->blocked,
			newset, regs->esi, regs->edi,
			regs->ebp, regs->eax,
			regs->ds, regs->es,
			regs->fs, regs->gs, regs->orig_eax,
			regs->eip, regs->cs, regs->eflags,
			regs->esp, regs->ss);

	if (answer < 0) {
		regs->eax = iABI_errors(-answer);
		regs->eflags |= 1;
	}
#endif
}

/*
 *  This is the service routine for the syscall #48 (signal funcs).
 *
 *   Examine the request code and branch on the request to the appropriate
 *   function.
 */

int ibcs_sigfunc (struct pt_regs * regs)
{
	int sig_type = (int) HIDDEN_PARAM;

#ifdef IBCS_TRACE
	if ((ibcs_trace & (TRACE_SIGNAL | TRACE_SIGNAL_F))
	|| ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS2 sig%s(%ld, 0x%08lx, 0x%08lx)\n",
			sig_type == 0 ? "nal"
			: (sig_type == 0x100 ? "set"
			: (sig_type == 0x200 ? "hold"
			: (sig_type == 0x400 ? "relse"
			: (sig_type == 0x800 ? "ignore"
			: (sig_type == 0x1000 ? "pause"
			: "???" ))))),
			SIGNAL_NUMBER & 0xff, SECOND_PARAM, THIRD_PARAM);
	}
#endif

#ifdef __sparc__
	set_result (regs, 0);
#else /* __sparc__ */
	regs->eflags &= ~1;
	regs->eax     = 0;
#endif /* __sparc__ */
	switch (sig_type) {
	case 0x0000:
		ibcs_signal (regs);
		break;

	case 0x0100:
		ibcs_sigset (regs);
		break;

	case 0x0200:
		ibcs_sighold (regs);
		break;
		
	case 0x0400:
		ibcs_sigrelse (regs);
		break;

	case 0x0800:
		ibcs_sigignore (regs);
		break;

	case 0x1000:
		ibcs_sigpause (regs);
		break;

	default:
		set_error (regs, EINVAL);

#ifdef IBCS_TRACE
		if ((ibcs_trace & (TRACE_SIGNAL | TRACE_SIGNAL_F))
		|| ibcs_func_p->trace)
		       printk (KERN_ERR "iBCS2 sigfunc(%x, %ld, %lx, %lx) unsupported\n",
			       sig_type,
			       SIGNAL_NUMBER,
			       SECOND_PARAM,
			       THIRD_PARAM);
#endif
		return 0;
	}

#ifdef IBCS_TRACE
	if ((ibcs_trace & (TRACE_SIGNAL | TRACE_SIGNAL_F))
	|| ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS2 returns %ld\n", get_result (regs));
	}
#endif
	return 0;
}

int ibcs_kill(int pid, int sig) {
	int outsig = ibcs_mapsig(sig & 0xFF);

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_SIGNAL) || ibcs_func_p->trace)
		printk (KERN_DEBUG "ibcs_kill:	insig (%d)	outsig(%d) \n"
			, sig & 0xFF, outsig);
#endif
	if (outsig < 0) {
		return -EINVAL;
	}
	return SYS(kill) (pid, outsig);
}


/* This function is used to handle the sigaction call from SVr4 binaries.
   If anyone else uses this, this function needs to be modified since the
   order and size of the ibcs_sigaction structure is different in ibcs
   and the SVr4 ABI */


asmlinkage int abi_sigaction(int abi_signum, const struct abi_sigaction * action,
	struct abi_sigaction * oldaction)
{
	struct abi_sigaction new_sa, old_sa;
	int signum;
	struct sigaction *p;

	signum = ibcs_mapsig(abi_signum);
	if (signum<1 || signum>32 || signum==SIGKILL || signum==SIGSTOP)
		return -EINVAL;
	p = signum - 1 + current->SIGACTION;
	if (action) {
		memcpy_fromfs(&new_sa, action, sizeof(struct abi_sigaction));
		if (new_sa.sa_flags & ABI_SA_NODEFER)
			new_sa.sa_mask = 0;
		else {
			new_sa.sa_mask |= _S(abi_signum);
			new_sa.sa_mask &= _BLOCKABLE;
		}
		if (TASK_SIZE <= (unsigned long) new_sa.sa_handler)
			return -EFAULT;
	}
	if (oldaction) {
		if (!verify_area(VERIFY_WRITE,oldaction, sizeof(struct abi_sigaction))) {
		  old_sa.sa_handler = p->sa_handler;
#ifdef INIT_MM
		  old_sa.sa_mask = map_bitvec(p->sa_mask,
			current->exec_domain->signal_invmap);
#else
		  old_sa.sa_mask = map_bitvec(p->sa_mask,
			signal_map_from_linux[current->personality & PER_MASK]);
#endif
		  old_sa.sa_flags = 0;
		  if(p->sa_flags & SA_STACK)
			old_sa.sa_flags |= ABI_SA_ONSTACK;
		  if(p->sa_flags & SA_RESTART)
			old_sa.sa_flags |= ABI_SA_RESTART;
		  if(p->sa_flags & SA_NOMASK)
			old_sa.sa_flags |= ABI_SA_NODEFER;
		  if(p->sa_flags & SA_ONESHOT)
			old_sa.sa_flags |= ABI_SA_RESETHAND;
		  if(p->sa_flags & SA_NOCLDSTOP)
			old_sa.sa_flags |= ABI_SA_NOCLDSTOP;
		  memcpy_tofs(oldaction, &old_sa, sizeof(struct abi_sigaction));
		      };
	}
	if (action) {
	        /* The internal format of the sigaction structure is
		   different, so we cannot simply copy the structure. */
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
		if(new_sa.sa_flags & ABI_SA_ONSTACK)
			p->sa_flags |= SA_STACK;
		if(new_sa.sa_flags & ABI_SA_RESTART)
			p->sa_flags |= SA_RESTART;
		if(new_sa.sa_flags & ABI_SA_NODEFER)
			p->sa_flags |= SA_NOMASK;
		if(new_sa.sa_flags & ABI_SA_RESETHAND)
			p->sa_flags |= SA_ONESHOT;
		if(new_sa.sa_flags & ABI_SA_NOCLDSTOP)
			p->sa_flags |= SA_NOCLDSTOP;

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

asmlinkage int sco_sigaction(int sco_signum, const struct sco_sigaction * action,
	struct sco_sigaction * oldaction)
{
	struct sco_sigaction new_sa, old_sa;
	int signum;
	struct sigaction *p;

	signum = ibcs_mapsig(sco_signum);
	if (signum<1 || signum>32 || signum==SIGKILL || signum==SIGSTOP)
		return -EINVAL;
	p = signum - 1 + current->SIGACTION;
	if (action) {
		memcpy_fromfs(&new_sa, action, sizeof(struct sco_sigaction));
		new_sa.sa_mask |= _S(sco_signum);
		new_sa.sa_mask &= _BLOCKABLE;
		if (TASK_SIZE <= (unsigned long) new_sa.sa_handler)
			return -EFAULT;
	}
	if (oldaction) {
		if (!verify_area(VERIFY_WRITE, oldaction, sizeof(struct sco_sigaction))) {
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
			old_sa.sa_flags |= SCO_SA_NOCLDSTOP;
		  memcpy_tofs(oldaction, &old_sa, sizeof(struct sco_sigaction));
		      };
	}
	if (action) {
	        /* The internal format of the sigaction structure is
		   different, so we cannot simply copy the structure. */
		p->sa_restorer = NULL;
	  	p->sa_handler = new_sa.sa_handler;
#ifdef INIT_MM
		  p->sa_mask = map_bitvec(new_sa.sa_mask,
			current->exec_domain->signal_map);
#else
		  p->sa_mask = map_bitvec(new_sa.sa_mask,
			signal_map_to_linux[current->personality & PER_MASK]);
#endif
		p->sa_flags = SA_NOMASK;
		if (new_sa.sa_flags & SCO_SA_NOCLDSTOP)
			p->sa_flags |= SA_NOCLDSTOP;

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

static short int howcnv[] = {SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK, SIG_SETMASK};

asmlinkage int
abi_sigprocmask(int how, sigset_t *abinset, sigset_t *abioset)
{
	sigset_t new_set, *nset, old_set, *oset;
	int old_fs, error;

	nset = oset = NULL;

	if (abinset) {
		new_set = get_fs_long((unsigned long *)abinset);
#ifdef INIT_MM
		new_set = map_bitvec(new_set,
			current->exec_domain->signal_map);
#else
		new_set = map_bitvec(new_set,
			signal_map_to_linux[current->personality & PER_MASK]);
#endif
		nset = &new_set;
	}
	if (abioset)
		oset = &old_set;

	old_fs = get_fs();
	set_fs(get_ds());
	error = SYS(sigprocmask)(howcnv[how], nset, oset);
	set_fs(old_fs);

	if (!error && abioset) {
#ifdef INIT_MM
		old_set = map_bitvec(old_set,
			current->exec_domain->signal_invmap);
#else
		old_set = map_bitvec(old_set,
			signal_map_from_linux[current->personality & PER_MASK]);
#endif
		put_fs_long(old_set, (unsigned long *)abioset);
	}

	return error;
}

#ifndef __sparc__
int abi_sigsuspend(struct pt_regs * regs)
{
	sigset_t * set;
	unsigned long newset, oldset;
	int error;

#if defined(EMU_BSD) && defined(PER_BSD)
	if (current->personality == PER_BSD) {
		oldset = get_syscall_parameter (regs, 0);
	} else
#endif
	{
		set = (sigset_t *)get_syscall_parameter (regs, 0);
		if ((error = verify_area(VERIFY_READ, set, sizeof(sigset_t))))
			return error;
		oldset = get_fs_long ((unsigned long *) set);
	}
#ifdef INIT_MM
	newset = map_bitvec(oldset,
		current->exec_domain->signal_map);
#else
	newset = map_bitvec(oldset,
		signal_map_to_linux[current->personality & PER_MASK]);
#endif

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_SIGNAL) || ibcs_func_p->trace)
		printk("iBCS: sigsuspend oldset, newset = %lx %lx\n",
			oldset, newset);
#endif
	{
#if 0
	    extern do_sigpause(unsigned int, struct pt_regs *);
	    return do_sigpause(newset, regs);
#endif
	}
	return SYS(sigsuspend)(0, oldset,
			newset, regs->esi, regs->edi,
			regs->ebp, regs->eax,
			regs->ds, regs->es,
			regs->fs, regs->gs, regs->orig_eax,
			regs->eip, regs->cs, regs->eflags,
			regs->esp, regs->ss);
}
#endif /* __sparc__ */
