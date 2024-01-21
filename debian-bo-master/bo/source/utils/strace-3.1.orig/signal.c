/*
 * Copyright (c) 1991, 1992 Paul Kranenburg <pk@cs.few.eur.nl>
 * Copyright (c) 1993 Branko Lankester <branko@hacktic.nl>
 * Copyright (c) 1993, 1994, 1995, 1996 Rick Sladkey <jrs@world.std.com>
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
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *	$Id: signal.c,v 2.37 1996/05/23 05:12:47 jrs Exp $
 */

#include "defs.h"

#include <signal.h>
#include <sys/user.h>
#include <fcntl.h>

#ifdef SVR4
#include <sys/ucontext.h>
#endif /* SVR4 */

#ifdef LINUX
#ifdef HAVE_ASM_SIGCONTEXT_H
#include <asm/sigcontext.h>
#else /* !HAVE_ASM_SIGCONTEXT_H */
#ifdef I386
struct sigcontext_struct {
	unsigned short gs, __gsh;
	unsigned short fs, __fsh;
	unsigned short es, __esh;
	unsigned short ds, __dsh;
	unsigned long edi;
	unsigned long esi;
	unsigned long ebp;
	unsigned long esp;
	unsigned long ebx;
	unsigned long edx;
	unsigned long ecx;
	unsigned long eax;
	unsigned long trapno;
	unsigned long err;
	unsigned long eip;
	unsigned short cs, __csh;
	unsigned long eflags;
	unsigned long esp_at_signal;
	unsigned short ss, __ssh;
	unsigned long i387;
	unsigned long oldmask;
	unsigned long cr2;
};
#else /* !I386 */
#ifdef M68K
struct sigcontext_struct
{
	unsigned long sc_mask;
	unsigned long sc_usp;
	unsigned long sc_d0;
	unsigned long sc_d1;
	unsigned long sc_a0;
	unsigned long sc_a1;
	unsigned short sc_sr;
	unsigned long sc_pc;
	unsigned short sc_formatvec;
};
#endif /* M68K */
#endif /* !I386 */
#endif /* !HAVE_ASM_SIGCONTEXT_H */
#endif /* LINUX */

char *signalent0[] = {
#include "signalent.h"
};
int nsignals0 = sizeof signalent0 / sizeof signalent0[0];

#if SUPPORTED_PERSONALITIES >= 2
char *signalent0[] = {
#include "signalent1.h"
};
int nsignals1 = sizeof signalent1 / sizeof signalent1[0];
#endif /* SUPPORTED_PERSONALITIES >= 2 */

char **signalent;
int nsignals;

#ifdef SUNOS4

static struct xlat sigvec_flags[] = {
	{ SV_ONSTACK,	"SV_ONSTACK"	},
	{ SV_INTERRUPT,	"SV_INTERRUPT"	},
	{ SV_RESETHAND,	"SV_RESETHAND"	},
	{ SA_NOCLDSTOP,	"SA_NOCLDSTOP"	},
	{ 0,		NULL		},
};

#endif /* SUNOS4 */

#ifdef HAVE_SIGACTION

static struct xlat sigact_flags[] = {
#ifdef SA_STACK
	{ SA_STACK,	"SA_STACK"	},
#endif
#ifdef SA_RESTART
	{ SA_RESTART,	"SA_RESTART"	},
#endif
#ifdef SA_INTERRUPT
	{ SA_INTERRUPT,	"SA_INTERRUPT"	},
#endif
#ifdef SA_NOMASK
	{ SA_NOMASK,	"SA_NOMASK"	},
#endif
#ifdef SA_ONESHOT
	{ SA_ONESHOT,	"SA_ONESHOT"	},
#endif
#ifdef SA_SIGINFO
	{ SA_SIGINFO,	"SA_SIGINFO"	},
#endif
#ifdef SA_RESETHAND
	{ SA_RESETHAND,	"SA_RESETHAND"	},
#endif
#ifdef SA_ONSTACK
	{ SA_ONSTACK,	"SA_ONSTACK"	},
#endif
#ifdef SA_NODEFER
	{ SA_NODEFER,	"SA_NODEFER"	},
#endif
#ifdef SA_NOCLDSTOP
	{ SA_NOCLDSTOP,	"SA_NOCLDSTOP"	},
#endif
#ifdef SA_NOCLDWAIT
	{ SA_NOCLDWAIT,	"SA_NOCLDWAIT"	},
#endif
#ifdef _SA_BSDCALL
	{ _SA_BSDCALL,	"_SA_BSDCALL"	},
#endif
	{ 0,		NULL		},
};

static struct xlat sigprocmaskcmds[] = {
	{ SIG_BLOCK,	"SIG_BLOCK"	},
	{ SIG_UNBLOCK,	"SIG_UNBLOCK"	},
	{ SIG_SETMASK,	"SIG_SETMASK"	},
#ifdef SIG_SETMASK32
	{ SIG_SETMASK32,"SIG_SETMASK32"	},
#endif
	{ 0,		NULL		},
};

#endif /* HAVE_SIGACTION */


static char *
sprintsigmask(s, mask)
char *s;
sigset_t mask;
{
	int i, nsigs;
	char *format;
	static char outstr[256];

	strcpy(outstr, s);
	s = outstr + strlen(outstr);
	nsigs = 0;
	for (i = 1; i <= NSIG; i++) {
		if (sigismember(&mask, i) == 1)
			nsigs++;
	}
	if (nsigs >= NSIG * 2 / 3) {
		*s++ = '~';
		for (i = 1; i <= NSIG; i++) {
			switch (sigismember(&mask, i)) {
			case 1:
				sigdelset(&mask, i);
				break;
			case 0:
				sigaddset(&mask, i);
				break;
			}
		}
	}
	format = "%s";
	*s++ = '[';
	for (i = 1; i <= NSIG; i++) {
		if (sigismember(&mask, i) == 1) {
			sprintf(s, format, signalent[i] + 3); s += strlen(s);
			format = " %s";
		}
	}
	*s++ = ']';
	*s = '\0';
	return outstr;
}

static void
printsigmask(mask)
sigset_t mask;
{
	tprintf("%s", sprintsigmask("", mask));
}

void
printsignal(nr)
int nr;
{
	if (nr > 0 && nr < nsignals)
		tprintf("%s", signalent[nr]);
	else
		tprintf("%d", nr);
}

/*
 * Check process TCP for the disposition of signal SIG.
 * Return 1 if the process would somehow manage to  survive signal SIG,
 * else return 0.  This routine will never be called with SIGKILL.
 */
int
sigishandled(tcp, sig)
struct tcb *tcp;
int sig;
{
#ifdef LINUX
	int sfd;
	char sname[32];
	char buf[1024];
	char *s;
	int i;
	int signalled, blocked, ignored, caught;

	/* This is incredibly costly but it's worth it. */
	sprintf(sname, "/proc/%d/stat", tcp->pid);
	if ((sfd = open(sname, O_RDONLY)) == -1) {
		perror(sname);
		return 1;
	}
	i = read(sfd, buf, 1024);
	buf[i] = '\0';
	close(sfd);
	/*
	 * Skip the extraneous fields. This loses if the
	 * command name has any spaces in it.  So be it.
	 */
	for (i = 0, s = buf; i < 30; i++) {
		while (*++s != ' ') {
			if (!*s)
				break;
		}
	}
	if (sscanf(s, "%d%d%d%d",
		   &signalled, &blocked, &ignored, &caught) != 4) {
		fprintf(stderr, "/proc/pid/stat format error\n");
		return 1;
	}
#ifdef DEBUG
	fprintf(stderr, "sigs: %08x %08x %08x %08x\n",
		signalled, blocked, ignored, caught);
#endif
	if ((ignored & sigmask(sig)) || (caught & sigmask(sig)))
		return 1;
#endif /* LINUX */

#ifdef SUNOS4
	void (*u_signal)();

	if (upeek(tcp->pid, uoff(u_signal[0]) + sig*sizeof(u_signal),
	    (long *) &u_signal) < 0) {
		return 0;
	}
	if (u_signal != SIG_DFL)
		return 1;
#endif /* SUNOS4 */

#ifdef SVR4
	/*
	 * Since procfs doesn't interfere with wait I think it is safe
	 * to punt on this question.  If not, the information is there.
	 */
	return 1;
#else /* !SVR4 */
	switch (sig) {
	case SIGCONT:
	case SIGSTOP:
	case SIGTSTP:
	case SIGTTIN:
	case SIGTTOU:
	case SIGCHLD:
	case SIGIO:
#if defined(SIGURG) && SIGURG != SIGIO
	case SIGURG:
#endif
	case SIGWINCH:
		/* Gloria Gaynor says ... */
		return 1;
	default:
		break;
	}
	return 0;
#endif /* !SVR4 */
}

#ifdef SUNOS4

int
sys_sigvec(tcp)
struct tcb *tcp;
{
	struct sigvec sv;
	long addr;

	if (entering(tcp)) {
		printsignal(tcp->u_arg[0]);
		tprintf(", ");
		addr = tcp->u_arg[1];
	} else {
		addr = tcp->u_arg[2];
	}
	if (addr == 0)
		tprintf("NULL");
	else if (!verbose(tcp))
		tprintf("%#lx", addr);
	else if (umove(tcp, addr, &sv) < 0)
		tprintf("{...}");
	else {
		switch ((int) sv.sv_handler) {
		case (int) SIG_ERR:
			tprintf("{SIG_ERR}");
			break;
		case (int) SIG_DFL:
			tprintf("{SIG_DFL}");
			break;
		case (int) SIG_IGN:
			if (tcp->u_arg[0] == SIGTRAP) {
				tcp->flags |= TCB_SIGTRAPPED;
				kill(tcp->pid, SIGSTOP);
			}
			tprintf("{SIG_IGN}");
			break;
		case (int) SIG_HOLD:
			if (tcp->u_arg[0] == SIGTRAP) {
				tcp->flags |= TCB_SIGTRAPPED;
				kill(tcp->pid, SIGSTOP);
			}
			tprintf("SIG_HOLD");
			break;
		default:
			if (tcp->u_arg[0] == SIGTRAP) {
				tcp->flags |= TCB_SIGTRAPPED;
				kill(tcp->pid, SIGSTOP);
			}
			tprintf("{%#lx, ", (unsigned long) sv.sv_handler);
			printsigmask(sv.sv_mask);
			tprintf(", ");
			if (!printflags(sigvec_flags, sv.sv_flags))
				tprintf("0");
			tprintf("}");
		}
	}
	if (entering(tcp))
		tprintf(", ");
	return 0;
}

int
sys_sigpause(tcp)
struct tcb *tcp;
{
	if (entering(tcp))
		printsigmask(tcp->u_arg[0]);
	return 0;
}

int
sys_sigstack(tcp)
struct tcb *tcp;
{
	struct sigstack ss;
	long addr;

	if (entering(tcp))
		addr = tcp->u_arg[0];
	else
		addr = tcp->u_arg[1];
	if (addr == 0)
		tprintf("NULL");
	else if (umove(tcp, addr, &ss) < 0)
		tprintf("%#lx", addr);
	else {
		tprintf("{ss_sp %#lx ", (unsigned long) ss.ss_sp);
		tprintf("ss_onstack %s}", ss.ss_onstack ? "YES" : "NO");
	}
	if (entering(tcp))
		tprintf(", ");
	return 0;
}

int
sys_sigcleanup(tcp)
struct tcb *tcp;
{
	return 0;
}

#endif /* SUNOS4 */

#ifndef SVR4

int
sys_sigsetmask(tcp)
struct tcb *tcp;
{
	if (entering(tcp)) {
		printsigmask(tcp->u_arg[0]);
		if ((tcp->u_arg[0] & sigmask(SIGTRAP))) {
			/* Mark attempt to block SIGTRAP */
			tcp->flags |= TCB_SIGTRAPPED;
			/* Send unblockable signal */
			kill(tcp->pid, SIGSTOP);
		}
	}
	else if (!syserror(tcp)) {
		tcp->auxstr = sprintsigmask("old mask ", tcp->u_rval);
		return RVAL_HEX | RVAL_STR;
	}
	return 0;
}

int
sys_sigblock(tcp)
struct tcb *tcp;
{
	return sys_sigsetmask(tcp);
}

#endif /* !SVR4 */

#ifdef HAVE_SIGACTION

int
sys_sigaction(tcp)
struct tcb *tcp;
{
	struct sigaction sa;
	long addr;

	if (entering(tcp)) {
		printsignal(tcp->u_arg[0]);
		tprintf(", ");
		addr = tcp->u_arg[1];
	} else
		addr = tcp->u_arg[2];
	if (addr == 0)
		tprintf("NULL");
	else if (!verbose(tcp))
		tprintf("%#lx", addr);
	else if (umove(tcp, addr, &sa) < 0)
		tprintf("{...}");
	else {
		switch ((long) sa.sa_handler) {
		case (long) SIG_ERR:
			tprintf("{SIG_ERR}");
			break;
		case (long) SIG_DFL:
			tprintf("{SIG_DFL}");
			break;
		case (long) SIG_IGN:
#ifndef SVR4
			if (tcp->u_arg[0] == SIGTRAP) {
				tcp->flags |= TCB_SIGTRAPPED;
				kill(tcp->pid, SIGSTOP);
			}
#endif /* !SVR4 */
			tprintf("{SIG_IGN}");
			break;
		default:
#ifndef SVR4
			if (tcp->u_arg[0] == SIGTRAP) {
				tcp->flags |= TCB_SIGTRAPPED;
				kill(tcp->pid, SIGSTOP);
			}
#endif /* !SVR4 */
			tprintf("{%#lx, ", (long) sa.sa_handler);
			printsigmask(sa.sa_mask);
			tprintf(", ");
			if (!printflags(sigact_flags, sa.sa_flags))
				tprintf("0");
			tprintf("}");
		}
	}
	if (entering(tcp))
		tprintf(", ");
	return 0;
}

int
sys_signal(tcp)
struct tcb *tcp;
{
	if (entering(tcp)) {
		printsignal(tcp->u_arg[0]);
		switch (tcp->u_arg[1]) {
		case (int) SIG_ERR:
			tprintf("SIG_ERR");
			break;
		case (int) SIG_DFL:
			tprintf("SIG_DFL");
			break;
		case (int) SIG_IGN:
#ifndef SVR4
			if (tcp->u_arg[0] == SIGTRAP) {
				tcp->flags |= TCB_SIGTRAPPED;
				kill(tcp->pid, SIGSTOP);
			}
#endif /* !SVR4 */
			tprintf("SIG_IGN");
			break;
		default:
#ifndef SVR4
			if (tcp->u_arg[0] == SIGTRAP) {
				tcp->flags |= TCB_SIGTRAPPED;
				kill(tcp->pid, SIGSTOP);
			}
#endif /* !SVR4 */
			tprintf("%#lx", tcp->u_arg[1]);
		}
	}
	return 0;
}

#endif /* HAVE_SIGACTION */

#ifdef LINUX

int
sys_sigreturn(tcp)
struct tcb *tcp;
{
#ifdef I386
	long esp;
	struct sigcontext_struct sc;

	if (entering(tcp)) {
		tcp->u_arg[0] = 0;
		if (upeek(tcp->pid, 4*UESP, &esp) < 0)
			return 0;
		if (umove(tcp, esp, &sc) < 0)
			return 0;
		tcp->u_arg[0] = 1;
		tcp->u_arg[1] = sc.oldmask;
	}
	else {
		tcp->u_rval = tcp->u_error = 0;
		if (tcp->u_arg[0] == 0)
			return 0;
		tcp->auxstr = sprintsigmask("mask now ", tcp->u_arg[1]);
		return RVAL_NONE | RVAL_STR;
	}
	return 0;
#else /* !I386 */
#ifdef M68K
	long usp;
	struct sigcontext_struct sc;

	if (entering(tcp)) {
		tcp->u_arg[0] = 0;
		if (upeek(tcp->pid, 4*PT_USP, &usp) < 0)
			return 0;
		if (umove(tcp, usp, &sc) < 0)
			return 0;
		tcp->u_arg[0] = 1;
		tcp->u_arg[1] = sc.sc_mask;
	}
	else {
		tcp->u_rval = tcp->u_error = 0;
		if (tcp->u_arg[0] == 0)
			return 0;
		tcp->auxstr = sprintsigmask("mask now ", tcp->u_arg[1]);
		return RVAL_NONE | RVAL_STR;
	}
	return 0;
#else /* !M68K */
#ifdef ALPHA
	long fp;
# if __GNU_LIBRARY__ < 5
	struct sigcontext_struct sc;
# else
	struct sigcontext sc;
# endif

	if (entering(tcp)) {
		tcp->u_arg[0] = 0;
		if (upeek(tcp->pid, REG_FP, &fp) < 0)
			return 0;
		if (umove(tcp, fp, &sc) < 0)
			return 0;
		tcp->u_arg[0] = 1;
		tcp->u_arg[1] = sc.sc_mask;
	}
	else {
		tcp->u_rval = tcp->u_error = 0;
		if (tcp->u_arg[0] == 0)
			return 0;
		tcp->auxstr = sprintsigmask("mask now ", tcp->u_arg[1]);
		return RVAL_NONE | RVAL_STR;
	}
	return 0;
#endif /* ALPHA */
#endif /* !M68K */
#endif /* !I386 */
}

int
sys_siggetmask(tcp)
struct tcb *tcp;
{
	if (exiting(tcp)) {
		tcp->auxstr = sprintsigmask("mask ", tcp->u_rval);
	}
	return RVAL_HEX | RVAL_STR;
}

int
sys_sigsuspend(tcp)
struct tcb *tcp;
{
	if (entering(tcp)) {
#if 0
		/* first two are not really arguments, but print them anyway */
		/* nevermind, they are an anachronism now, too bad... */
		tprintf("%d, %#x, ", tcp->u_arg[0], tcp->u_arg[1]);
#endif
		printsigmask(tcp->u_arg[2]);
	}
	return 0;
}

#endif /* LINUX */

#ifdef SVR4

int
sys_sigsuspend(tcp)
struct tcb *tcp;
{
	sigset_t sigset;

	if (entering(tcp)) {
		if (umove(tcp, tcp->u_arg[0], &sigset) < 0)
			tprintf("[?]");
		else
			printsigmask(sigset);
	}
	return 0;
}
static struct xlat ucontext_flags[] = {
	{ UC_SIGMASK,	"UC_SIGMASK"	},
	{ UC_STACK,	"UC_STACK"	},
	{ UC_CPU,	"UC_CPU"	},
#ifdef UC_FPU
	{ UC_FPU,	"UC_FPU"	},
#endif
#ifdef UC_INTR
	{ UC_INTR,	"UC_INTR"	},
#endif
	{ 0,		NULL		},
};

static struct xlat sigaltstack_flags[] = {
	{ SS_ONSTACK,	"SS_ONSTACK"	},
	{ SS_DISABLE,	"SS_DISABLE"	},
	{ 0,		NULL		},
};

static void
printcontext(tcp, ucp)
struct tcb *tcp;
ucontext_t *ucp;
{
	tprintf("{");
	if (!abbrev(tcp)) {
		tprintf("uc_flags=");
		if (!printflags(ucontext_flags, ucp->uc_flags))
			tprintf("0");
		tprintf(", uc_link=%#lx, ", (unsigned long) ucp->uc_link);
	}
	tprintf("uc_sigmask=");
	printsigmask(ucp->uc_sigmask);
	if (!abbrev(tcp)) {
		tprintf(", uc_stack={ss_sp=%#lx, ss_size=%d, ss_flags=",
			(unsigned long) ucp->uc_stack.ss_sp,
			ucp->uc_stack.ss_size);
		if (!printflags(sigaltstack_flags, ucp->uc_stack.ss_flags))
			tprintf("0");
		tprintf("}");
	}
	tprintf(", ...}");
}

int
sys_getcontext(tcp)
struct tcb *tcp;
{
	ucontext_t uc;

	if (entering(tcp)) {
		if (!tcp->u_arg[0])
			tprintf("NULL");
		else if (umove(tcp, tcp->u_arg[0], &uc) < 0)
			tprintf("{...}");
		else
			printcontext(tcp, &uc);
	}
	return 0;
}

int
sys_setcontext(tcp)
struct tcb *tcp;
{
	ucontext_t uc;

	if (entering(tcp)) {
		if (!tcp->u_arg[0])
			tprintf("NULL");
		else if (umove(tcp, tcp->u_arg[0], &uc) < 0)
			tprintf("{...}");
		else
			printcontext(tcp, &uc);
	}
	else {
		tcp->u_rval = tcp->u_error = 0;
		if (tcp->u_arg[0] == 0)
			return 0;
		return RVAL_NONE;
	}
	return 0;
}

#endif /* SVR4 */

#ifdef HAVE_SIGACTION

int
sys_sigprocmask(tcp)
struct tcb *tcp;
{
#ifdef ALPHA
	if (entering(tcp)) {
		printxval(sigprocmaskcmds, tcp->u_arg[0], "SIG_???");
		tprintf(", ");
		printsigmask(tcp->u_arg[1]);
	}
	else if (!syserror(tcp)) {
		tcp->auxstr = sprintsigmask("old mask ", tcp->u_rval);
		return RVAL_HEX | RVAL_STR;
	}
#else /* !ALPHA */
	sigset_t sigset;

	if (entering(tcp)) {
#ifdef SVR4
		if (tcp->u_arg[0] == 0)
			tprintf("0");
		else
#endif /* SVR4 */
		printxval(sigprocmaskcmds, tcp->u_arg[0], "SIG_???");
		tprintf(", ");
		if (!tcp->u_arg[1])
			tprintf("NULL, ");
		else if (umove(tcp, tcp->u_arg[1], &sigset) < 0)
			tprintf("%#lx, ", tcp->u_arg[1]);
		else {
			printsigmask(sigset);
			tprintf(", ");
		}
	}
	else {
		if (!tcp->u_arg[2])
			tprintf("NULL");
		else if (syserror(tcp))
			tprintf("%#lx", tcp->u_arg[2]);
		else if (umove(tcp, tcp->u_arg[2], &sigset) < 0)
			tprintf("[?]");
		else
			printsigmask(sigset);
	}
#endif /* !ALPHA */
	return 0;
}

#endif /* HAVE_SIGACTION */

int
sys_kill(tcp)
struct tcb *tcp;
{
	if (entering(tcp)) {
		long sig = tcp->u_arg[1];

		if (sig >= 0 && sig < NSIG)
			tprintf("%ld, %s", tcp->u_arg[0], signalent[sig]);
		else
			tprintf("%ld, %ld", tcp->u_arg[0], sig);
	}
	return 0;
}

int
sys_killpg(tcp)
struct tcb *tcp;
{
	return sys_kill(tcp);
}

int
sys_sigpending(tcp)
struct tcb *tcp;
{
	sigset_t sigset;

	if (exiting(tcp)) {
		if (syserror(tcp))
			tprintf("%#lx", tcp->u_arg[0]);
		else if (umove(tcp, tcp->u_arg[0], &sigset) < 0)
			tprintf("[?]");
		else
			printsigmask(sigset);
	}
	return 0;
}
